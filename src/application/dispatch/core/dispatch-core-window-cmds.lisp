(in-package #:cl-tmux)

;;;; Window/pane/split command factories.
;;;;
;;;; These build the %cmd-* helpers behind the :new-window, :next-window,
;;;; :prev-window, :next-pane, :prev-pane, :split-horizontal, and
;;;; :split-vertical dispatch-handlers.lisp entries.

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
