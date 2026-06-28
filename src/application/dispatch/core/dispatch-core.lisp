(in-package #:cl-tmux)

;;;; Declarative command dispatch — macros, helpers, and core dispatch logic.
;;;;
;;;; This file contains:
;;;;   - Cyclic navigator macro (next-cyclic, prev-cyclic)
;;;;   - Active-pane/window guard macros (with-active-pane, with-active-window)
;;;;   - Private command helper functions (%cmd-new-window, %cmd-cycle-window, etc.)
;;;;   - Copy-mode key override macro + table
;;;;   - Session/window/menu format helpers
;;;;   - New-session factory
;;;;   - Named-command table macro + table
;;;;   - dispatch-prefix-command entry point
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

(defun %flag-value (flags char)
  "Return the value associated with CHAR in FLAGS, or NIL when absent."
  (cdr (assoc char flags)))

(defun %flag-present-p (flags char)
  "Return true when FLAGS contains CHAR."
  (not (null (%flag-value flags char))))

(defun %resolve-target-window-pane (session target-str current-window current-pane)
  "Resolve TARGET-STR to a window/pane pair.
   When TARGET-STR is absent, return CURRENT-WINDOW and CURRENT-PANE.
   When TARGET-STR names a window but not a pane, return that window's active pane."
  (if target-str
      (multiple-value-bind (target-session target-window target-pane)
          (resolve-target *server-sessions* target-str
                          :current-session session
                          :current-window current-window
                          :current-pane current-pane)
        (declare (ignore target-session))
        (when target-window
          (values target-window
                  (or (and target-pane
                           (member target-pane (window-panes target-window))
                           target-pane)
                      (window-active-pane target-window)))))
      (values current-window current-pane)))

(defun %resolve-target-session-window (session target-str current-window current-pane)
  "Resolve TARGET-STR to a session/window pair.
   When TARGET-STR is absent, return SESSION and CURRENT-WINDOW.
   When TARGET-STR names a pane, return its window."
  (if target-str
      (multiple-value-bind (target-session target-window target-pane)
          (resolve-target *server-sessions* target-str
                          :current-session session
                          :current-window current-window
                          :current-pane current-pane)
        (declare (ignore target-pane))
        (when target-window
          (values target-session target-window)))
      (values session current-window)))

(defun %resolve-window-target-or-active (session target-str)
  "Return TARGET-STR's window or SESSION's active window when TARGET-STR is absent."
  (or (and target-str (%resolve-window-target session target-str))
      (session-active-window session)))

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
  "Swap the active pane of SESSION in DIRECTION (:left or :right)."
  (with-active-window (win session)
    (swap-pane win direction)))

;;; -- Private command helpers ------------------------------------------------

(defun %derive-hook-session (target)
  "Resolve a hook TARGET — a session, window, or pane — to its owning session, so
   command hooks (which run against a session) can fire from any run-hooks call
   regardless of what object the firing point had.  NIL when unresolvable."
  (cond
    ((null target) nil)
    ((cl-tmux/model::session-p target) target)
    ((cl-tmux/model::window-p  target) (%session-of-window target))
    ((cl-tmux/model::pane-p    target) (%session-of-pane   target))
    (t nil)))

(defun %dispatch-hook-entry (session entry)
  "Dispatch a single hook ENTRY against SESSION.
   STRING entries are run as command lines via %run-command-line; errors are
   reported as an overlay instead of being silently swallowed.
   KEYWORD entries dispatch directly via dispatch-command."
  (cond
    ((stringp entry)
     (handler-case (%run-command-line session entry)
       (error (condition)
         (%overlayf "hook error: ~A" condition))))
    ((keywordp entry)
     (dispatch-command session entry 0))))

(defun run-command-hooks (event-name target)
  "Dispatch every command registered for hook EVENT-NAME against the session
   derived from TARGET (a session/window/pane).  String hooks (from set-hook
   in .tmux.conf) run via %run-command-line for format expansion; keyword
   hooks (programmatic set-command-hook calls) dispatch directly."
  (let ((session (%derive-hook-session target)))
    (when session
      (dolist (entry (cl-tmux/hooks:command-hooks event-name))
        (%dispatch-hook-entry session entry)))))

;; Install run-command-hooks as the command-hook runner so lower layers
;; (cl-tmux/commands kill-pane / kill-window) can fire command hooks too.
(setf cl-tmux/hooks:*command-hook-runner* #'run-command-hooks)

(defun %compute-window-base-index (prev-win &key at-index after-current before-current)
  "Return the base window-id to use when inserting a new window.
   AT-INDEX overrides everything when it is an integer.
   AFTER-CURRENT inserts after PREV-WIN's id (adds 1).
   BEFORE-CURRENT inserts at PREV-WIN's id (pushes existing windows right).
   Otherwise uses the configured base-index option, defaulting to 0."
  (cond
    ((and at-index (integerp at-index)) at-index)
    ((and after-current prev-win) (1+ (window-id prev-win)))
    ((and before-current prev-win) (window-id prev-win))
    (t (or (cl-tmux/options:get-option "base-index") 0))))

(defun %cmd-new-window (session &key name start-dir detach at-index after-current
                                     before-current)
  "Create a new window in SESSION and start a reader thread for it.
   NAME: window name (defaults to shell basename).
   START-DIR: start directory for the new pane's shell.
   DETACH: when T, do not make the new window active.
   AT-INDEX: when an integer, try to assign that specific window id.
   AFTER-CURRENT: when T, insert after the current window's id.
   BEFORE-CURRENT: when T, insert at (before) the current window's id.
   Returns the new window."
  (let* ((rows     (- *term-rows* *status-height*))
         (cols     *term-cols*)
         (win-name (or name (cl-tmux/model::%shell-basename)))
         (prev-win (session-active-window session))
         (base     (%compute-window-base-index prev-win
                                               :at-index      at-index
                                               :after-current  after-current
                                               :before-current before-current))
         (win      (session-new-window session win-name rows cols base start-dir)))
    (start-reader-thread (window-active-pane win))
    (cl-tmux/hooks:run-hooks cl-tmux/hooks:+hook-after-new-window+ win)
    (when (and detach prev-win)
      (session-select-window session prev-win))
    win))

(defun %cmd-cycle-window (session cycler)
  "Switch the active window using CYCLER (next-cyclic or prev-cyclic)."
  (let ((w (funcall cycler
                    (session-windows session)
                    (session-active-window session))))
    (when w
      (%with-window-focus-transition (session)
        (session-select-window session w)))))

(defun %cmd-cycle-pane (session cycler)
  "Switch the active pane within the active window using CYCLER."
  (let* ((win   (session-active-window session))
         (panes (window-panes win))
         (next  (funcall cycler panes (window-active-pane win))))
    (when next (%select-pane-with-focus win next))))

(defun %cmd-cycle-session (session cycler)
  "Switch to the adjacent session using CYCLER (next-cyclic or prev-cyclic).
   No-op when SESSION is the only session or the cycler wraps back to it."
  (let* ((sessions (mapcar #'cdr *server-sessions*))
         (target   (and sessions (funcall cycler sessions session))))
    (when (and target (not (eq target session)))
      (%switch-to-session target))))

(defun %cmd-split (session orient
                   &key no-focus size start-dir before full input-only input-bytes)
  "Split the active pane of SESSION's active window in tree ORIENT (:h left/right,
   :v top/bottom).  Returns NIL when the pane is too small and no shell is forked.
   NO-FOCUS T skips focus change.  SIZE hints the new pane's extent.
   BEFORE T inserts the new pane before the active pane (split-window -b).
   FULL T spans the whole window (split-window -f).
   INPUT-ONLY T creates a no-PTY pane and feeds INPUT-BYTES into its screen.
   START-DIR: when non-NIL, the new pane's shell starts in that directory."
  (let* ((win (session-active-window session))
         (new (window-split session win orient :no-focus no-focus :size size
                                           :start-dir start-dir :before before
                                           :full full :input-only input-only
                                           :input-bytes input-bytes)))
    (when new
      (when (> (pane-fd new) 0)
        (start-reader-thread new))
      (cl-tmux/hooks:run-hooks cl-tmux/hooks:+hook-after-split-window+ new)
      ;; A split creates a new pane — fire after-new-pane too (was defined but
      ;; never fired).
      (cl-tmux/hooks:run-hooks cl-tmux/hooks:+hook-after-new-pane+ new))
    new))

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
  "Select the pane adjacent to the active pane in DIRECTION."
  (multiple-value-bind (win ap) (%active-window-pane session)
    (when (and win ap)
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
