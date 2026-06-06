(in-package #:cl-tmux/commands)

;;; High-level tmux commands that operate on the session/window/pane model.
;;; Each exported function is the CL analogue of a tmux command-line command.

;;; Core tmux commands: kill, rename, select, resize

;;; ── Kill ───────────────────────────────────────────────────────────────────
;;;
;;; kill_pane(Session)  :- close_pty(Pane), remove_pane(Window, Pane),
;;;                         (empty(Window) -> kill_window(Session, Window) ; true).
;;; kill_window(Session, Window) :- forall(pane(P, Window), close_pty(P)),
;;;                                  remove_window(Session, Window),
;;;                                  (empty(Session) -> quit ; select_next(Session)).

(defun kill-pane (session &optional pane)
  "Close PANE (default: active pane of SESSION).
   Sends SIGHUP to its child process and closes the PTY fd.
   Removes the pane from the window's split tree, collapsing its parent so the
   sibling reclaims the freed rectangle.  If the owning window becomes empty,
   also calls KILL-WINDOW.
   Only re-selects a new active pane when the killed pane was the active one.
   Returns :quit if no windows remain, nil otherwise."
  (let* ((win        (session-active-window session))
         (target     (or pane (window-active-pane win)))
         (was-active (eq target (window-active-pane win))))
    (when target
      (ignore-errors (pty-close (pane-fd target) (pane-pid target))))
    (let ((survivor (window-remove-pane win target)))
      (run-hooks +hook-after-kill-pane+ target)
      (run-command-hooks-via-runner +hook-after-kill-pane+ session)
      (if (null (window-panes win))
          (kill-window session win)
          (progn
            (when was-active
              (let* ((remaining (window-panes win))
                     (last-act  (window-last-active win))
                     (chosen    (or (and last-act (find last-act remaining))
                                    survivor
                                    (first remaining))))
                (window-select-pane win chosen)))
            nil)))))

(defun %nearest-window (windows killed-id)
  "Return the window from WINDOWS whose id is numerically closest to KILLED-ID.
   When two windows are equidistant, the one with the larger id (next neighbour)
   is preferred.  Falls back to (first windows) when the list is empty."
  (reduce (lambda (best w)
            (let ((d-best (abs (- killed-id (window-id best))))
                  (d-w    (abs (- killed-id (window-id w)))))
              (cond ((< d-w d-best) w)
                    ((and (= d-w d-best) (> (window-id w) killed-id)) w)
                    (t best))))
          (rest windows)
          :initial-value (first windows)))

(defun %maybe-renumber-windows (session)
  "If the 'renumber-windows' option is set, renumber all windows in SESSION
   starting from the 'base-index' option value, preserving their current order."
  (when (cl-tmux/options:get-option "renumber-windows")
    (let ((base (or (cl-tmux/options:get-option "base-index") 0)))
      (loop for win in (session-windows session)
            for i from base
            do (setf (window-id win) i)))))

(defun kill-window (session &optional window)
  "Destroy WINDOW (default: active window of SESSION).
   Kills all panes in it and removes the window from SESSION.
   After killing the active window, selects the numerically nearest remaining
   window (next higher id if available, otherwise next lower).
   If 'renumber-windows' is set, renumbers the remaining windows starting from
   the 'base-index' option.
   Returns :quit if no windows remain, NIL otherwise."
  (let* ((target    (or window (session-active-window session)))
         (killed-id (window-id target))
         (remaining (remove target (session-windows session))))
    (dolist (pane (window-panes target))
      (ignore-errors (pty-close (pane-fd pane) (pane-pid pane))))
    (setf (session-windows session) remaining)
    (run-hooks +hook-after-kill-window+ target)
    (run-command-hooks-via-runner +hook-after-kill-window+ session)
    (unless remaining (return-from kill-window :quit))
    (when (eq (session-active-window session) target)
      (session-select-window session (%nearest-window remaining killed-id)))
    (%maybe-renumber-windows session)
    nil))

;;; ── Rename / Select ────────────────────────────────────────────────────────
;;;
;;; rename_window(Window, Name)   :- set(window-name, Name), run_hooks(after-rename-window).
;;; rename_session(Session, Name) :- nonempty(Name), set(session-name, Name).
;;; select_window(Session, N)     :- nth(N, windows(Session), W), activate(W).

(defun rename-window (window name)
  "Set WINDOW's name to NAME.  Empty NAME is a no-op, matching tmux behaviour."
  (when (and window name (not (string= name "")))
    (setf (window-name window) name)
    (run-hooks +hook-after-rename-window+ window name)))

(defun rename-session (session name)
  "Set SESSION's name to NAME."
  (when (and session name (not (string= name "")))
    (setf (session-name session) name)))

(defun select-window-by-number (session n)
  "Select the window in SESSION whose window-id equals N.
   The lookup is by stored window-id, not by list position, so the digit pressed
   matches the window label even after kills leave gaps in the list."
  (let ((win (find n (session-windows session) :key #'window-id)))
    (when win
      (session-select-window session win))))

;;; ── Resize ─────────────────────────────────────────────────────────────────
;;;
;;; resize_pane(Window, Dir, Amount) :- active_pane(Window, P),
;;;                                     adjust_split_tree(Window, P, Dir, Amount).

(defun resize-pane (window direction &optional (amount 5))
  "Resize the active pane via the split tree. Returns the active pane on success, NIL otherwise."
  (when (and window (window-tree window))
    (window-resize-active window direction amount)))
