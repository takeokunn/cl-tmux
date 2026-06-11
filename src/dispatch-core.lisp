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

;;; -- Swap-active-pane helper --------------------------------------------------
;;;
;;; :swap-pane-forward and :swap-pane-backward share the same shape.

(defun %swap-active-pane (session direction)
  "Swap the active pane of SESSION in DIRECTION (:left or :right)."
  (with-active-window (win session)
    (swap-pane win direction)))

;;; -- Focus event delivery (?1004) -------------------------------------------
;;;
;;; When a pane's application has enabled focus events, switching the active pane
;;; must deliver ESC[O (focus lost) to the pane being left and ESC[I (focus
;;; gained) to the pane being entered.  focus-event-report (terminal layer) owns
;;; the byte sequence; here we perform the PTY write.  Both are guarded by a live
;;; fd, so panes without a PTY (fd <= 0, e.g. in tests) are a harmless no-op.

(defun %session-of-window (win)
  "The session in *server-sessions* whose window list contains WIN, or NIL.
   Lets chokepoints that only have a window (e.g. %select-pane-with-focus) fire
   .tmux.conf set-hook command hooks, which run-command-hooks dispatches against a
   session."
  (and win (loop for (nil . sess) in *server-sessions*
                 when (member win (session-windows sess)) return sess)))

(defun %session-of-pane (pane)
  "The session in *server-sessions* one of whose windows contains PANE, or NIL.
   Lets %notify-pane-focus fire .tmux.conf set-hook command hooks from a pane."
  (and pane (loop for (nil . sess) in *server-sessions*
                  when (loop for w in (session-windows sess)
                             thereis (member pane (window-panes w)))
                    return sess)))

(defun %notify-pane-focus (pane focused-p)
  "Notify PANE of a focus change: fire the pane-focus-in / pane-focus-out hook
   (independent of ?1004), then send the application its focus-tracking report
   (ESC[I gained / ESC[O lost) when it enabled focus events and PANE has a live
   PTY.  A safe no-op when PANE is NIL."
  (when pane
    ;; Hook fires on every focus transition, regardless of whether the app
    ;; enabled ?1004 focus reporting (matches tmux's pane-focus-in/out hooks).
    ;; run-hooks fires both the add-hook and (via the pane's session) set-hook.
    (cl-tmux/hooks:run-hooks (if focused-p
                                 cl-tmux/hooks:+hook-pane-focus-in+
                                 cl-tmux/hooks:+hook-pane-focus-out+)
                             pane))
  (when (and pane (> (pane-fd pane) 0))
    (let ((seq (cl-tmux/terminal/actions:focus-event-report
                (pane-screen pane) focused-p)))
      (when seq
        (pty-write (pane-fd pane) (babel:string-to-octets seq :encoding :utf-8))))))

(defun %select-pane-with-focus (win new-pane)
  "Make NEW-PANE the active pane of WIN, delivering focus-out to the previously
   active pane and focus-in to NEW-PANE (for panes that enabled ?1004).  Used by
   every interactive pane-switch path so focus tracking stays transparent."
  (let ((old (window-active-pane win)))
    (window-select-pane win new-pane)
    (unless (eq old new-pane)
      (%notify-pane-focus old nil)
      (%notify-pane-focus new-pane t)
      ;; tmux's window-pane-changed event hook: WIN's active pane changed.
      ;; (run-hooks fires both registries, deriving the session from WIN.)
      (cl-tmux/hooks:run-hooks cl-tmux/hooks:+hook-window-pane-changed+ win))))

(defmacro %with-window-focus-transition ((session) &body body)
  "Run BODY (which may change SESSION's active window by any means) and then
   deliver focus-out to the previously active window's pane and focus-in to the
   newly active window's pane.  Captures the active window/pane BEFORE BODY and
   diffs AFTER, so it works for direct session-select-window calls and for
   lookup-based switches (select-window-by-number, find-window) alike.  Returns
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

(defun run-command-hooks (event-name target)
  "Dispatch every command registered for hook EVENT-NAME (via the `set-hook`
   directive) against the session derived from TARGET (a session/window/pane).
   A no-op when no command hooks are set or no session can be derived.
   Hooks may be stored as keywords (legacy) OR as strings (from set-hook in
   .tmux.conf, e.g. 'display-message #{session_name}').  String hooks are
   run via %run-command-line so format expansion and argument parsing work."
  (let ((session (%derive-hook-session target)))
    (when session
      (dolist (entry (cl-tmux/hooks:command-hooks event-name))
        (cond
          ((stringp entry)
           ;; String hook from set-hook directive: run as a command line with full
           ;; format expansion and argument parsing.
           (ignore-errors (%run-command-line session entry)))
          ((keywordp entry)
           ;; Keyword hook from programmatic add-hook or legacy set-command-hook.
           (dispatch-command session entry 0)))))))

;; Install run-command-hooks as the command-hook runner so lower layers
;; (cl-tmux/commands kill-pane / kill-window) can fire command hooks too.
(setf cl-tmux/hooks:*command-hook-runner* #'run-command-hooks)

(defun %cmd-new-window (session &key name start-dir detach at-index after-current)
  "Create a new window in SESSION and start a reader thread for it.
   NAME: window name (defaults to shell basename).
   START-DIR: start directory for the new pane's shell.
   DETACH: when T, do not make the new window active.
   AT-INDEX: when an integer, try to assign that specific window id.
   AFTER-CURRENT: when T, insert after the current window's id.
   Returns the new window."
  (let* ((rows (- *term-rows* *status-height*))
         (cols *term-cols*)
         (win-name (or name (cl-tmux/model::%shell-basename)))
         (prev-win (session-active-window session))
         ;; Determine base-index for id assignment.
         (base (cond
                 ((and at-index (integerp at-index)) at-index)
                 ((and after-current prev-win)
                  (1+ (window-id prev-win)))
                 (t (or (cl-tmux/options:get-option "base-index") 0))))
         (win  (session-new-window session win-name rows cols base start-dir)))
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

(defun %cmd-split (session orient &key no-focus size start-dir before full)
  "Split the active pane of SESSION's active window in tree ORIENT (:h left/right,
   :v top/bottom).  Returns NIL when the pane is too small and no shell is forked.
   NO-FOCUS T skips focus change.  SIZE hints the new pane's extent.
   BEFORE T inserts the new pane before the active pane (split-window -b).
   FULL T spans the whole window (split-window -f).
   START-DIR: when non-NIL, the new pane's shell starts in that directory."
  (let* ((win (session-active-window session))
         (new (window-split win orient :no-focus no-focus :size size
                                       :start-dir start-dir :before before
                                       :full full)))
    (when new
      (start-reader-thread new)
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
  (let* ((win (session-active-window session))
         (ap  (and win (window-active-pane win))))
    (and ap
         (screen-copy-mode-p (pane-screen ap)))))

;;; -- Directional pane selection helper ------------------------------------
;;;
;;; The four :select-pane-left/right/up/down handlers share the same shape:
;;; obtain the active window and pane, then walk to the neighbor in DIRECTION.

(defun %select-pane-in-direction (session direction)
  "Select the pane adjacent to the active pane in DIRECTION."
  (let* ((win (session-active-window session))
         (ap  (and win (window-active-pane win))))
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

