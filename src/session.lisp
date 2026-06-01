(in-package #:cl-tmux/model)

;;; ── Session ID counter ──────────────────────────────────────────────────────

(defparameter *session-id-counter* 0
  "Auto-increment counter for session IDs. Each new session gets (incf *session-id-counter*).")

;;; ── Session ────────────────────────────────────────────────────────────────

(defstruct session
  "Top-level container: a named set of windows with one active."
  (id          0   :type fixnum)
  (name        ""  :type string)
  (windows     nil :type list)
  (active      nil)
  (last-active 0   :type integer)   ; universal-time of last access; updated on touch
  (clients     nil :type list)      ; list of connected client descriptors
  (locked-p    nil :type boolean)   ; T when lock-session has been called
  (group       nil))                ; NIL or group-id (string/integer); sessions in same group share windows

(defun session-active-window (session)
  (or (session-active session)
      (first (session-windows session))))

(defun session-select-window (session window)
  (setf (session-active session) window)
  (when window
    (setf (window-last-active-time window) (get-universal-time))))

(defun session-active-pane (session)
  (let ((w (session-active-window session)))
    (when w (window-active-pane w))))

;;; ── Full-screen window factory ──────────────────────────────────────────────
;;;
;;; Data/logic separation:
;;;   %attach-full-screen-pane  — window data setup (PTY pane → tree leaf)
;;;   session-new-window        — session attachment (window → session list)

(defun %attach-full-screen-pane (window rows cols)
  "Fork a shell and install it as WINDOW's sole full-screen leaf pane."
  (let ((pane (%fork-pane 1 0 0 cols rows)))
    (setf (window-panes  window) (list pane)
          (window-active window) pane
          (window-tree   window) (make-layout-leaf pane))))

(defun %next-window-id (session &optional (base-index 0))
  "Return the smallest integer >= BASE-INDEX not already used by any window in SESSION.
   BASE-INDEX defaults to 0 (tmux default)."
  (let ((used (mapcar #'window-id (session-windows session))))
    (loop for i from base-index
          unless (member i used) return i)))

(defun session-new-window (session name rows cols &optional (base-index 0))
  "Create a new window with one full-screen pane, attach it to SESSION.
   The new window receives the lowest free id >= BASE-INDEX (default 0).
   The window list is kept sorted by window-id after insertion."
  (let* ((new-id (%next-window-id session base-index))
         (win (make-window :id new-id :name name :width cols :height rows)))
    (%attach-full-screen-pane win rows cols)
    (setf (session-windows session)
          (sort (cons win (session-windows session)) #'< :key #'window-id))
    (setf (session-active session) win)
    win))

;;; ── Global state & initialisation ─────────────────────────────────────────

(defun session-touch (session)
  "Update SESSION's last-active timestamp to the current universal time."
  (setf (session-last-active session) (get-universal-time))
  session)

(defun %shell-basename ()
  "Return the basename component of *default-shell*, or \"window\" as fallback."
  (let* ((shell (or *default-shell* "window"))
         (slash-pos (position #\/ shell :from-end t)))
    (if slash-pos
        (subseq shell (1+ slash-pos))
        shell)))

(defun create-initial-session (rows cols)
  "Bootstrap: one session, one window, one full-screen pane.
   The first window index is base-index (default 0) and its name is the
   shell basename (e.g. \"bash\", \"zsh\")."
  (let ((session   (make-session :id (incf *session-id-counter*)
                                 :name "0"
                                 :last-active (get-universal-time)))
        (pane-rows (- rows *status-height*)))
    (session-new-window session (%shell-basename) pane-rows cols)
    session))

(defun all-panes (session)
  "Flat list of every pane across all windows of SESSION."
  (loop for w in (session-windows session)
        nconc (copy-list (window-panes w))))

;;; ── Window reordering ────────────────────────────────────────────────────────

(defun session-move-window (session window target-index)
  "Move WINDOW to TARGET-INDEX (0-based) in SESSION's window list.
   Clamps TARGET-INDEX to valid range. Returns the updated window list."
  (let* ((wins    (session-windows session))
         (n       (length wins))
         (src-idx (position window wins)))
    (when src-idx
      (let* ((dst (max 0 (min (1- n) target-index)))
             (without (append (subseq wins 0 src-idx)
                              (subseq wins (1+ src-idx))))
             (before  (subseq without 0 dst))
             (after   (subseq without dst)))
        (setf (session-windows session) (append before (list window) after)))))
  (session-windows session))

(defun session-swap-windows (session index-a index-b)
  "Exchange the windows at INDEX-A and INDEX-B in SESSION's window list.
   Indices are 0-based. No-op when indices are equal or out of range."
  (let* ((wins (session-windows session))
         (n    (length wins)))
    (when (and (/= index-a index-b)
               (< -1 index-a n)
               (< -1 index-b n))
      (let ((new-wins (copy-list wins)))
        (rotatef (nth index-a new-wins) (nth index-b new-wins))
        (setf (session-windows session) new-wins))))
  (session-windows session))

(defun session-last-window (session)
  "Return the window with the second-highest last-active-time (i.e. the
   previously active window), or NIL when only one window exists."
  (let* ((wins (session-windows session))
         (sorted (sort (copy-list wins) #'>
                       :key #'window-last-active-time)))
    (second sorted)))

;;; ── update-environment support ──────────────────────────────────────────────

(defparameter *update-environment*
  '("DISPLAY" "SSH_AUTH_SOCK" "SSH_CONNECTION" "XAUTHORITY")
  "List of environment variable names to propagate into new panes.
   Mirrors tmux's update-environment server option.")

(defun get-update-environment-vars ()
  "Return an alist of (name . value) for each variable in *UPDATE-ENVIRONMENT*
   that is set in the current process environment.  Unset variables are omitted."
  (loop for name in *update-environment*
        for value = (sb-ext:posix-getenv name)
        when value collect (cons name value)))
