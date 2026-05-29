(in-package #:cl-tmux/model)

;;; ── Pane ───────────────────────────────────────────────────────────────────

(defstruct pane
  "One terminal pane: a PTY fd + virtual screen + position within its window."
  (id     0   :type fixnum)
  (x      0   :type fixnum)
  (y      0   :type fixnum)
  (width  80  :type fixnum)
  (height 24  :type fixnum)
  (fd     -1  :type fixnum)
  (pid    -1  :type fixnum)
  (screen nil))

(defun pane-feed (pane bytes)
  "Feed raw PTY bytes into PANE's screen, holding the screen lock."
  (let ((screen (pane-screen pane)))
    (with-lock-held ((screen-lock screen))
      (screen-process-bytes screen bytes))))

(defun pane-reposition (pane x y width height)
  "Move and resize PANE to X,Y with WIDTH x HEIGHT.
   Resizes the underlying PTY and virtual screen."
  (setf (pane-x pane)      x
        (pane-y pane)      y
        (pane-width  pane) width
        (pane-height pane) height)
  (set-pty-size (pane-fd pane) height width)
  (let ((screen (pane-screen pane)))
    (with-lock-held ((screen-lock screen))
      (screen-resize screen width height))))

;;; ── Window ─────────────────────────────────────────────────────────────────

(defstruct window
  "A named collection of panes with one active (focused) pane."
  (id      0   :type fixnum)
  (name    ""  :type string)
  (width   80  :type fixnum)
  (height  24  :type fixnum)
  (panes   nil :type list)
  (active  nil)
  (layout  nil))

(defun window-active-pane (window)
  (or (window-active window)
      (first (window-panes window))))

(defun window-select-pane (window pane)
  (setf (window-active window) pane))

(defun window-split (window direction)
  "Add a new pane to WINDOW by splitting the available space in DIRECTION.
   :horizontal stacks panes top/bottom; :vertical places them side by side.
   Returns the new pane."
  (let* ((rows     (window-height window))
         (cols     (window-width  window))
         (existing (window-panes  window))
         (n        (1+ (length existing)))
         (layouts  (divide-window direction n rows cols))
         (new-pane nil))
    (loop for pane in existing
          for layout in layouts
          do (destructuring-bind (px py pw ph) layout
               (pane-reposition pane px py pw ph)))
    (destructuring-bind (px py pw ph) (car (last layouts))
      (multiple-value-bind (fd pid)
          (forkpty-with-shell ph pw)
        (setf new-pane
              (make-pane :id n
                         :x px :y py :width pw :height ph
                         :fd fd :pid pid
                         :screen (make-screen pw ph)))))
    (setf (window-panes  window) (append existing (list new-pane))
          (window-active window) new-pane
          (window-layout window) direction)
    new-pane))

(defun window-relayout (window rows cols)
  "Re-fit WINDOW's panes into ROWS x COLS, preserving the split layout."
  (setf (window-width  window) cols
        (window-height window) rows)
  (let* ((panes   (window-panes window))
         (n       (length panes))
         (layouts (if (window-layout window)
                      (divide-window (window-layout window) n rows cols)
                      (list (list 0 0 cols rows)))))
    (loop for pane in panes
          for layout in layouts
          do (destructuring-bind (px py pw ph) layout
               (pane-reposition pane px py pw ph)))))

(defun ensure-window-fits (window rows cols)
  "Relayout WINDOW only when its stored size differs from ROWS x COLS."
  (when (or (/= (window-width  window) cols)
            (/= (window-height window) rows))
    (window-relayout window rows cols)))

(defun divide-window (direction n rows cols)
  "Divide ROWS x COLS into N layout slots for DIRECTION (:vertical/:horizontal).
   Reserves one row/column between adjacent panes for a separator.
   Returns a list of (x y width height)."
  (case direction
    (:vertical
     (let* ((avail (- cols (1- n)))
            (w     (max 1 (floor avail n))))
       (loop for i below n
             for x = (* i (1+ w))
             collect (list x 0
                           (if (= i (1- n)) (max 1 (- cols x)) w)
                           rows))))
    (:horizontal
     (let* ((avail (- rows (1- n)))
            (h     (max 1 (floor avail n))))
       (loop for i below n
             for y = (* i (1+ h))
             collect (list 0 y cols
                           (if (= i (1- n)) (max 1 (- rows y)) h)))))
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
  "Create a new window with one full-size pane and add it to SESSION."
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
