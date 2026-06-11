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

(defun %previous-window-by-index (windows killed-id)
  "tmux session_previous: the window in WINDOWS (the non-empty remaining list)
   with the greatest id strictly LESS than KILLED-ID, wrapping to the greatest id
   overall when none is lower."
  (flet ((max-by-id (ws)
           (reduce (lambda (a b) (if (> (window-id b) (window-id a)) b a)) ws)))
    (let ((lower (remove-if-not (lambda (w) (< (window-id w) killed-id)) windows)))
      (max-by-id (or lower windows)))))

(defun %mru-window (windows)
  "The unambiguously most-recently-active window in WINDOWS — the one with the
   strictly greatest POSITIVE last-active-time (the top of tmux's lastw stack).
   NIL when no window has been focused (all timestamps 0) or the greatest time is
   tied, i.e. there is no unambiguous last-used window (tmux's lastw is empty)."
  (let* ((sorted (sort (copy-list windows) #'> :key #'window-last-active-time))
         (top    (first sorted))
         (next   (second sorted)))
    (when (and top
               (> (window-last-active-time top) 0)
               (or (null next)
                   (> (window-last-active-time top) (window-last-active-time next))))
      top)))

(defun %window-after-kill (windows killed-id)
  "Choose the window to activate after the current window (KILLED-ID) is killed,
   matching tmux's session_detach reselection order: the last-used (MRU) window
   first (session_last), else the previous window by index — wrapping to the
   highest id — (session_previous).  tmux's session_next (next-by-index) fallback
   is unreachable here because session_previous always succeeds on a non-empty
   list (it wraps).  WINDOWS is the remaining-windows list; returns NIL only when
   WINDOWS is empty."
  (when windows
    (or (%mru-window windows)
        (%previous-window-by-index windows killed-id))))

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
   After killing the active window, reselects like tmux session_detach: the
   last-used (MRU) window first, otherwise the previous window by index (wrapping
   to the highest id) — see %window-after-kill.
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
    (unless remaining (return-from kill-window :quit))
    (when (eq (session-active-window session) target)
      (session-select-window session (%window-after-kill remaining killed-id)))
    (%maybe-renumber-windows session)
    nil))

;;; ── Rename / Select ────────────────────────────────────────────────────────
;;;
;;; rename_window(Window, Name)   :- set(window-name, Name), run_hooks(after-rename-window).
;;; rename_session(Session, Name) :- nonempty(Name), set(session-name, Name).
;;; select_window(Session, N)     :- nth(N, windows(Session), W), activate(W).

(defun rename-window (window name &key (disable-automatic-rename t))
  "Set WINDOW's name to NAME.  Empty NAME is a no-op, matching tmux behaviour.
   By default (a MANUAL rename) disables automatic-rename, signalling user
   ownership so subsequent foreground-process changes do not overwrite the name
   (re-enable with `set-option -w automatic-rename on`).  The AUTOMATIC-rename
   path passes :DISABLE-AUTOMATIC-RENAME NIL so repeated title-driven renames keep
   working — otherwise auto-rename would fire only once.  Fires after-rename-window
   and window-renamed in both cases."
  (when (and window name (not (string= name "")))
    (setf (window-name window) name)
    (when disable-automatic-rename
      (setf (window-automatic-rename-p window) nil))
    (run-hooks +hook-after-rename-window+ window name)
    (run-hooks +hook-window-renamed+ window name)))

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
    (prog1 (window-resize-active window direction amount)
      (run-hooks +hook-after-resize-pane+ window))))
