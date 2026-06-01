(in-package #:cl-tmux/commands)

;;; High-level tmux commands that operate on the session/window/pane model.
;;; Each exported function is the CL analogue of a tmux command-line command.

;;; ── Kill ───────────────────────────────────────────────────────────────────

(defun kill-pane (session &optional pane)
  "Close PANE (default: active pane of SESSION).
   Sends SIGHUP to its child process and closes the PTY fd.
   Removes the pane from the window's split tree, collapsing its parent so the
   sibling reclaims the freed rectangle.  If the owning window becomes empty,
   also calls KILL-WINDOW.
   Returns :quit if no windows remain, nil otherwise."
  (let* ((win    (session-active-window session))
         (target (or pane (window-active-pane win))))
    (when target
      (ignore-errors (pty-close (pane-fd target) (pane-pid target))))
    (let ((survivor (window-remove-pane win target)))
      (if (null (window-panes win))
          (kill-window session win)
          (progn
            (window-select-pane win (or survivor (first (window-panes win))))
            nil)))))

(defun kill-window (session &optional window)
  "Destroy WINDOW (default: active window of SESSION).
   Kills all panes in it and removes the window from SESSION.
   Returns :quit if no windows remain, NIL otherwise."
  (let* ((target    (or window (session-active-window session)))
         (remaining (remove target (session-windows session))))
    (dolist (pane (window-panes target))
      (ignore-errors (pty-close (pane-fd pane) (pane-pid pane))))
    (setf (session-windows session) remaining)
    (unless remaining (return-from kill-window :quit))
    (when (eq (session-active-window session) target)
      (session-select-window session (first remaining)))
    nil))

;;; ── Rename ─────────────────────────────────────────────────────────────────

(defun rename-window (window name)
  "Set WINDOW's name to NAME."
  (when window
    (setf (window-name window) name)))

;;; ── Window selection ───────────────────────────────────────────────────────

(defun select-window-by-number (session n)
  "Select the Nth window (0-based) of SESSION if it exists."
  (let ((win (nth n (session-windows session))))
    (when win
      (session-select-window session win))))

;;; ── Pane resize ────────────────────────────────────────────────────────────

(defun resize-pane (window direction &optional (amount 5))
  "Resize the active pane via the split tree. Returns the active pane on success, NIL otherwise."
  (when (and window (window-tree window))
    (window-resize-active window direction amount)))

;;; ── Copy mode transitions ──────────────────────────────────────────────────
;;;
;;; Enter and exit are symmetric facts:
;;;   copy_mode(enter, Screen) :- copy_mode_p(Screen) := true,  offset := 0.
;;;   copy_mode(exit,  Screen) :- copy_mode_p(Screen) := false, offset := 0.

(defmacro define-copy-mode-transitions (&rest specs)
  "Build copy-mode transition functions from a Prolog-like fact table.
   Each SPEC is (name active-p docstring): active-p is T or NIL."
  `(progn
     ,@(mapcar (lambda (spec)
                 (destructuring-bind (name active-p docstring) spec
                   `(defun ,name (screen)
                      ,docstring
                      (setf (screen-copy-mode-p screen) ,active-p
                            (screen-copy-offset  screen) 0))))
               specs)))

(define-copy-mode-transitions
  (copy-mode-enter t
   "Enter copy/scroll mode on SCREEN: freeze the viewport at the live position.")
  (copy-mode-exit nil
   "Exit copy mode: resume live PTY output display."))

(defun copy-mode-scroll (screen delta)
  "Adjust SCREEN's copy-offset by DELTA lines.
   Positive DELTA scrolls back toward older output; negative scrolls forward.
   Clamped to [0, (length scrollback)]. Marks the screen dirty."
  (when (screen-copy-mode-p screen)
    (let ((max-offset (length (screen-scrollback screen))))
      (setf (screen-copy-offset screen)
            (max 0 (min max-offset (+ (screen-copy-offset screen) delta))))
      (setf (screen-dirty-p screen) t))))
