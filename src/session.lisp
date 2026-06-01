(in-package #:cl-tmux/model)

;;; ── Session ────────────────────────────────────────────────────────────────

(defstruct session
  "Top-level container: a named set of windows with one active."
  (id      0   :type fixnum)
  (name    ""  :type string)
  (windows nil :type list)
  (active  nil))

(defun session-active-window (session)
  (or (session-active session)
      (first (session-windows session))))

(defun session-select-window (session window)
  (setf (session-active session) window))

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

(defun session-new-window (session name rows cols)
  "Create a new window with one full-screen pane, attach it to SESSION."
  (let ((win (make-window :id (1+ (length (session-windows session)))
                          :name name :width cols :height rows)))
    (%attach-full-screen-pane win rows cols)
    (setf (session-windows session) (append (session-windows session) (list win))
          (session-active  session) win)
    win))

;;; ── Global state & initialisation ─────────────────────────────────────────

(defun create-initial-session (rows cols)
  "Bootstrap: one session, one window, one full-screen pane."
  (let ((session   (make-session :id 1 :name "0"))
        (pane-rows (- rows *status-height*)))
    (session-new-window session "1" pane-rows cols)
    session))

(defun all-panes (session)
  "Flat list of every pane across all windows of SESSION."
  (loop for w in (session-windows session)
        nconc (copy-list (window-panes w))))
