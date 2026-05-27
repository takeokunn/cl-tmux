(in-package #:cl-tmux/model)

;;;; Session → Window → Pane data model.
;;;;
;;;; A session owns an ordered list of windows.  Each window contains one or
;;;; more panes arranged side-by-side (vertical split) or top-to-bottom
;;;; (horizontal split).  Every pane owns a PTY file descriptor, a child PID,
;;;; and a virtual screen (terminal emulator state).

;;; ── Pane ───────────────────────────────────────────────────────────────────

(defstruct pane
  "One terminal pane: a PTY fd + virtual screen + position within its window."
  (id     0   :type fixnum)
  ;; Origin (column, row) within the window, 0-based
  (x      0   :type fixnum)
  (y      0   :type fixnum)
  (width  80  :type fixnum)
  (height 24  :type fixnum)
  ;; PTY master fd and child process PID
  (fd     -1  :type fixnum)
  (pid    -1  :type fixnum)
  ;; Virtual terminal emulator state
  (screen nil))

(defun pane-feed (pane bytes)
  "Feed raw PTY bytes into the pane's screen, holding the screen lock."
  (let ((screen (pane-screen pane)))
    (with-lock-held ((screen-lock screen))
      (screen-process-bytes screen bytes))))

;;; ── Window ─────────────────────────────────────────────────────────────────

(defstruct window
  "A named collection of panes with one active (focused) pane."
  (id      0    :type fixnum)
  (name    ""   :type string)
  (width   80   :type fixnum)
  (height  24   :type fixnum)
  (panes   nil  :type list)   ; ordered list of all panes
  (active  nil))              ; the currently focused pane

(defun window-active-pane (window)
  (or (window-active window)
      (first (window-panes window))))

(defun window-select-pane (window pane)
  (setf (window-active window) pane))

(defun window-split (window direction)
  "Add a new pane to WINDOW by splitting the available space.
   DIRECTION :horizontal → top/bottom stacking; :vertical → side by side.
   Resizes existing panes, forks a shell for the new pane, and returns it."
  (let* ((rows (window-height window))
         (cols (window-width  window))
         (existing (window-panes window))
         (n        (1+ (length existing)))
         (layouts  (divide-window direction n rows cols))
         (new-pane nil))
    ;; Reposition and resize existing panes according to the new layout.
    (loop for pane in existing
          for layout in layouts
          do (destructuring-bind (px py pw ph) layout
               (setf (pane-x pane) px (pane-y pane) py
                     (pane-width pane) pw (pane-height pane) ph)
               (set-pty-size (pane-fd pane) ph pw)))
    ;; Create the new pane using the last layout slot.
    (destructuring-bind (px py pw ph) (car (last layouts))
      (multiple-value-bind (fd pid)
          (forkpty-with-shell ph pw)
        (setf new-pane
              (make-pane :id n
                         :x px :y py :width pw :height ph
                         :fd fd :pid pid
                         :screen (make-screen pw ph)))))
    (setf (window-panes  window) (append existing (list new-pane))
          (window-active window) new-pane)
    new-pane))

;;; Divide a rows×cols area into N equal slots.
;;; Returns a list of (x y width height) plists.
(defun divide-window (direction n rows cols)
  (case direction
    (:horizontal
     (let ((h (floor rows n)))
       (loop for i below n collect (list 0 (* i h) cols h))))
    (:vertical
     (let ((w (floor cols n)))
       (loop for i below n collect (list (* i w) 0 w rows))))
    (otherwise
     (list (list 0 0 cols rows)))))

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

(defun session-new-window (session name rows cols)
  "Create a new window with one full-size pane, add it to SESSION.
   Pane height is (rows − *status-height*) to leave room for the status bar."
  (let* ((id  (1+ (length (session-windows session))))
         (win (make-window :id id :name name
                           :width cols :height rows)))
    (multiple-value-bind (fd pid)
        (forkpty-with-shell rows cols)
      (let ((pane (make-pane :id 1 :x 0 :y 0 :width cols :height rows
                             :fd fd :pid pid
                             :screen (make-screen cols rows))))
        (setf (window-panes  win) (list pane)
              (window-active win) pane)))
    (setf (session-windows session)
          (append (session-windows session) (list win)))
    (setf (session-active session) win)
    win))

;;; ── Global state & initialisation ─────────────────────────────────────────

(defvar *current-session* nil)

(defun create-initial-session (rows cols)
  "Bootstrap: one session, one window, one full-screen pane."
  (let ((session (make-session :id 1 :name "0"))
        (pane-rows (- rows *status-height*)))
    (session-new-window session "1" pane-rows cols)
    (setf *current-session* session)
    session))

(defun all-panes (session)
  "Flat list of every pane across all windows of SESSION."
  (loop for w in (session-windows session)
        nconc (copy-list (window-panes w))))
