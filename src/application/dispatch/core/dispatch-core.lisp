(in-package #:cl-tmux)

;;;; Declarative command dispatch — macros, helpers, and core dispatch logic.
;;;;
;;;; This file contains:
;;;;   - Cyclic navigator macro (next-cyclic, prev-cyclic)
;;;;   - Overlay-rendering helpers (show-built-overlay, %overlayf, etc.)
;;;;   - with-target-session / with-target-context target-binding macros
;;;;   - Active-pane/window guard macros (with-active-pane, with-active-window)
;;;;   - Focus-transition macro (%with-window-focus-transition)
;;;;   - Directional pane/window/copy-mode helpers used by dispatch-handlers.lisp
;;;;
;;;; Target-string resolution helpers (%resolve-target-window-pane and friends)
;;;; live in dispatch-core-targets.lisp.
;;;;
;;;; Hook-dispatch helpers (%derive-hook-session, run-command-hooks, etc.)
;;;; live in dispatch-core-hooks.lisp.
;;;;
;;;; Window/pane/split command factories (%cmd-new-window, %cmd-split, etc.)
;;;; live in dispatch-core-window-cmds.lisp.
;;;;
;;;; Focus event delivery helpers live in dispatch-core-focus.lisp.
;;;;
;;;; The actual command handler rules live in dispatch-handlers.lisp.

;;; ── Cyclic navigation macro ─────────────────────────────────────────────────
;;;
;;; next-cyclic and prev-cyclic are the same modular-arithmetic pattern with
;;; the step direction as the only difference.  A Prolog-like fact table:
;;;   navigate(next, List, Current) :- idx + 1.
;;;   navigate(prev, List, Current) :- idx - 1.

(defmacro define-cyclic-navigators (&rest specs)
  "Build cyclic list navigator functions from a declarative step table.
   Each SPEC is (name step docstring)."
  `(progn
     ,@(mapcar
        (lambda (spec)
          (destructuring-bind (name step docstring) spec
            `(defun ,name (list current)
               ,docstring
               (let ((idx (or (position current list) 0)))
                 (nth (mod (+ idx ,step) (length list)) list)))))
        specs)))

(define-cyclic-navigators
  (next-cyclic  1  "Element after CURRENT in LIST, wrapping around.")
  (prev-cyclic -1  "Element before CURRENT in LIST, wrapping around."))

;;; ── Active-pane access macro ─────────────────────────────────────────────────
;;;
;;; The pattern (let ((ap (session-active-pane session))) (when ap body))
;;; appears in %active-screen and %forward-octets.
;;; with-active-pane names the intent directly.

(defmacro show-built-overlay ((stream) &body body)
  "Show an overlay whose text is built by BODY writing to STREAM."
  `(show-overlay (with-output-to-string (,stream) ,@body)))

(defun %overlay-lines-string (lines &optional (empty ""))
  "Render LINES as newline-separated overlay text, or EMPTY when no lines exist."
  (if lines
      (with-output-to-string (s)
        (loop for line in lines
              for first = t then nil
              do (unless first
                   (terpri s))
                 (princ line s)))
      empty))

(defun %overlayf (control &rest args)
  "Render a formatted one-line overlay from CONTROL and ARGS."
  (show-overlay (apply #'format nil control args)))

(defmacro with-overlay-on-error ((op-label) &body body)
  "Run BODY, reporting any ERROR as an overlay tagged with OP-LABEL.
   Standardises the 'run body, surface exceptions via an overlay message
   naming the failing operation' pattern shared by buffer file I/O and
   hook-command dispatch, instead of each call site re-deriving its own
   handler-case/%overlayf wrapper."
  `(handler-case (progn ,@body)
     (error (e) (%overlayf "~A error: ~A" ,op-label e))))

(defun %flag-value (flags char)
  "Return the value associated with CHAR in FLAGS, or NIL when absent."
  (cdr (assoc char flags)))

(defun %flag-present-p (flags char)
  "Return true when FLAGS contains an entry for CHAR, even if its value is NIL."
  (not (null (assoc char flags))))

(defmacro with-target-session ((target-session target-str session
                               &key message (on-missing :skip))
                               &body body)
  "Bind TARGET-SESSION from TARGET-STR or SESSION.
   When TARGET-STR is missing, run BODY with SESSION.
   When TARGET-STR is present but unresolved, either skip BODY, show MESSAGE, or
   run BODY against SESSION depending on ON-MISSING (:skip, :error, :current)."
  (let ((target-str-var (gensym "TARGET-STR-"))
        (resolved-var (gensym "TARGET-SESSION-"))
        (session-var (gensym "SESSION-")))
    `(let* ((,session-var ,session)
            (,target-str-var ,target-str)
            (,resolved-var (and ,target-str-var
                                (find-session-by-target *server-sessions*
                                                        ,target-str-var)))
            (,target-session (or ,resolved-var ,session-var)))
       (cond
         ((or (null ,target-str-var) ,resolved-var)
          ,@body)
         ((eq ,on-missing :current)
          ,@body)
         ((eq ,on-missing :error)
          (progn
            ,(when message
               `(%overlayf ,message ,target-str-var))
            nil))
         (t nil)))))

(defmacro with-target-context ((target-session target-window target-pane session target-str)
                               &body body)
  "Bind TARGET-SESSION, TARGET-WINDOW, and TARGET-PANE from TARGET-STR or SESSION."
  `(multiple-value-bind (,target-session ,target-window ,target-pane)
       (resolve-target-context *server-sessions* ,session ,target-str)
     ,@body))

(defmacro with-active-pane ((pane-var session) &body body)
  "Bind PANE-VAR to SESSION's active pane and evaluate BODY.
   Returns NIL when no active pane is present (no-op guard)."
  `(let ((,pane-var (session-active-pane ,session)))
     (when ,pane-var ,@body)))

;;; -- Kill-result helper -------------------------------------------------------
;;;
;;; Both :kill-pane and :kill-window check the result and set *running* nil on
;;; :quit.  %handle-kill-result centralises that one-liner.

(defun %handle-kill-result (result)
  "Set *running* nil when RESULT is :quit, then return RESULT."
  (when (eq result :quit) (setf *running* nil))
  result)

;;; -- Active-window guard macro ------------------------------------------------
;;;
;;; Several handlers obtain the active window and do nothing when it is NIL.
;;; with-active-window names that guard directly.

(defmacro with-active-window ((win-var session) &body body)
  "Bind WIN-VAR to SESSION's active window and evaluate BODY only when present."
  `(let ((,win-var (session-active-window ,session)))
     (when ,win-var ,@body)))

;;; -- Focus transition helper --------------------------------------------------
;;;
;;; `%cmd-cycle-window` uses this macro before dispatch-core-focus.lisp is loaded,
;;; so the definition has to live here to be available at compile time.

(defmacro %with-window-focus-transition ((session) &body body)
  "Run BODY (which may change SESSION's active window by any means) and then
   deliver focus-out to the previously active window's pane and focus-in to the
   newly active window's pane. Captures the active window/pane BEFORE BODY and
   diffs AFTER, so it works for direct session-select-window calls and for
   lookup-based switches (select-window-by-number, find-window) alike. Returns
   BODY's primary value."
  (let ((sess (gensym "SESSION")) (old-win (gensym "OLD-WIN"))
        (old-pane (gensym "OLD-PANE")) (new-win (gensym "NEW-WIN")))
    `(let* ((,sess     ,session)
            (,old-win  (session-active-window ,sess))
            (,old-pane (and ,old-win (window-active-pane ,old-win))))
       (prog1 (progn ,@body)
         (let ((,new-win (session-active-window ,sess)))
           (unless (eq ,old-win ,new-win)
             (%notify-pane-focus ,old-pane nil)
             (%notify-pane-focus (and ,new-win (window-active-pane ,new-win)) t)
             ;; tmux's session-window-changed event hook: the active window changed.
             ;; (run-hooks fires both the add-hook and set-hook registries.)
             (cl-tmux/hooks:run-hooks cl-tmux/hooks:+hook-session-window-changed+ ,sess)))))))

;;; -- Active window/pane pair helper -----------------------------------------
;;;
;;; Several handlers need both the active window and its active pane so they can
;;; resolve optional targets relative to the current focused location.

(defun %active-window-pane (session)
  "Return SESSION's active window and its active pane as two values."
  (let ((win (session-active-window session)))
    (values win (and win (window-active-pane win)))))

;;; -- Swap-active-pane helper --------------------------------------------------
;;;
;;; :swap-pane-forward and :swap-pane-backward share the same shape.

(defun %swap-active-pane (session direction)
  "Swap the active pane of SESSION in DIRECTION (:left or :right).
   Pops zoom first."
  (with-active-window (win session)
    (%pane-navigation-unzoom win)
    (swap-pane win direction)))

;;; -- Resize-active-window-pane helper ------------------------------------------
;;;
;;; :resize-left/-right/-up/-down share the same shape: resize the active
;;; pane of SESSION's active window in DIRECTION.

(defun %resize-active-window-pane (session direction)
  "Resize the active pane of SESSION's active window in DIRECTION."
  (resize-pane (session-active-window session) direction))

;;; -- Private command helpers ------------------------------------------------

(defun %active-screen (session)
  "Return SESSION's active-pane screen, or NIL when there is no active pane."
  (with-active-pane (ap session)
    (pane-screen ap)))

;;; -- %copy-mode-active-p ---------------------------------------------------
;;;
;;; Internal predicate — not part of the public API.  Tests access it via the
;;; double-colon path.  The % prefix signals that callers outside this file
;;; should treat it as an implementation detail.

(defun %copy-mode-active-p (session)
  "Return T when the active pane's screen is in copy mode."
  (multiple-value-bind (_win ap) (%active-window-pane session)
    (and ap
         (screen-copy-mode-p (pane-screen ap)))))

;;; -- Directional pane selection helper ------------------------------------
;;;
;;; The four :select-pane-left/right/up/down handlers share the same shape:
;;; obtain the active window and pane, then walk to the neighbor in DIRECTION.

(defun %select-pane-in-direction (session direction)
  "Select the pane adjacent to the active pane in DIRECTION.
   Pops zoom first; a zoomed window's single-leaf tree would otherwise have no
   neighbours at all."
  (multiple-value-bind (win ap) (%active-window-pane session)
    (when (and win ap)
      (%pane-navigation-unzoom win)
      (let ((nb (pane-neighbor win ap direction)))
        (when nb (%select-pane-with-focus win nb))))))

;;; -- Named-layout application helper --------------------------------------
;;;
;;; The three :select-layout-* handlers share the same shape: apply a named
;;; layout to the active window and recompute geometry.

(defun %apply-named-layout-to-session (session layout-name)
  "Apply LAYOUT-NAME to SESSION's active window and reassign geometry.
   Reads the main-pane-width/-height and other-pane-width/-height options here (the
   cl-tmux layer, above options) and threads them into apply-named-layout (model
   layer, below options) so main-horizontal / main-vertical honour their configured
   main / other pane sizes."
  (let ((win (session-active-window session)))
    (when win
      (cl-tmux/model:apply-named-layout
       win layout-name
       (or (cl-tmux/options:get-option "main-pane-width") 80)
       (or (cl-tmux/options:get-option "main-pane-height") 24)
       (or (cl-tmux/options:get-option "other-pane-width") 0)
       (or (cl-tmux/options:get-option "other-pane-height") 0))
      (%assign-window-tree win (window-width win) (window-height win)))))

;;; -- Copy-mode dispatch helper --------------------------------------------
;;;
;;; The copy-mode command handlers share the pattern:
;;; obtain the active screen and invoke a copy-mode function when present.
;;; NOTE: This helper guards only on the presence of an active screen, not
;;; on whether copy mode is currently on.  The caller is responsible for
;;; gating on copy-mode state when required.

(defun %copy-mode-call (session fn)
  "Call FN on SESSION's active screen when one exists.
   Does NOT check whether copy mode is currently active; the caller must
   guard on that separately if needed."
  (let ((screen (%active-screen session)))
    (when screen (funcall fn screen))))

;;; -- Window list formatter ------------------------------------------------

(defun %format-window-list (session)
  "Return a formatted string listing all windows in SESSION.
   Format: INDEX: NAME (WxH) [active marker]
   INDEX is the window's stored id, not its 0-based list position."
  (let* ((win  (session-active-window session))
         (wins (session-windows session)))
    (with-output-to-string (s)
      (dolist (w wins)
        (format s "~A~A: ~A (~Dx~D) [~D pane~:P]~A~%"
                (if (eq w win) "*" " ")
                (window-id w)
                (window-name w)
                (window-width w)
                (window-height w)
                (length (window-panes w))
                (if (eq w win) " [active]" ""))))))
