(in-package #:cl-tmux/terminal)

;;;; VT100 / ANSI terminal emulator.
;;;;
;;;; Maintains a grid of character cells and a parser state machine.
;;;; screen-process-bytes is the main entry point; call it with raw PTY output.
;;;; The renderer reads the resulting grid through screen-cell.

;;; ── Cell ───────────────────────────────────────────────────────────────────

;; Attribute bits
(defconstant +attr-bold+    #b001)
(defconstant +attr-dim+     #b010)
(defconstant +attr-reverse+ #b100)

(defstruct cell
  "One character position on the virtual screen."
  (char  #\Space :type character)
  (fg    7       :type (unsigned-byte 8))   ; 0-15 ANSI colour; 7 = default fg
  (bg    0       :type (unsigned-byte 8))   ; 0-15 ANSI colour; 0 = default bg
  (attrs 0       :type (unsigned-byte 8)))  ; bit-field: bold, dim, reverse

(defun blank-cell ()
  (make-cell))

;;; ── Screen ─────────────────────────────────────────────────────────────────

(defstruct (screen (:constructor %make-screen))
  "Virtual terminal screen: cursor, cell grid, and VT100 parser state."
  ;; Geometry
  (width    80 :type fixnum)
  (height   24 :type fixnum)
  ;; Row-major grid: index = y*width + x
  (cells    #() :type simple-vector)
  ;; Cursor
  (cx 0 :type fixnum)
  (cy 0 :type fixnum)
  ;; Current SGR state stamped on newly written cells
  (cur-fg    7 :type (unsigned-byte 8))
  (cur-bg    0 :type (unsigned-byte 8))
  (cur-attrs 0 :type (unsigned-byte 8))
  ;; Scroll region (inclusive 0-based row indices)
  (scroll-top    0  :type fixnum)
  (scroll-bottom 23 :type fixnum)
  ;; Parser state machine
  (state       :ground :type keyword)
  (params      nil)       ; accumulated CSI parameter list (reversed while building)
  (cur-param   nil)       ; integer currently being assembled digit by digit
  (intermediate nil)      ; e.g. #\? for DEC private sequences
  ;; Dirty flag: set whenever a cell changes; cleared by renderer after paint
  (dirty-p t :type boolean)
  ;; Lock for thread safety (renderer ↔ PTY-reader threads)
  (lock (make-lock "screen")))

(defun make-screen (width height)
  "Create a blank screen of given dimensions."
  (let* ((n     (* width height))
         (cells (make-array n :initial-element nil)))
    (dotimes (i n) (setf (aref cells i) (blank-cell)))
    (%make-screen :width  width
                  :height height
                  :cells  cells
                  :scroll-bottom (1- height))))

;;; Exported cursor accessors (the struct slots use cx/cy internally)
(defun screen-cursor-x (s) (screen-cx s))
(defun screen-cursor-y (s) (screen-cy s))

;;; ── Grid helpers ───────────────────────────────────────────────────────────

(defun screen-cell (screen x y)
  "Return the cell at column X, row Y."
  (aref (screen-cells screen)
        (+ (* y (screen-width screen)) x)))

(defun (setf screen-cell) (cell screen x y)
  (setf (aref (screen-cells screen)
              (+ (* y (screen-width screen)) x))
        cell)
  (setf (screen-dirty-p screen) t))

(defun screen-clear-dirty (screen)
  (setf (screen-dirty-p screen) nil))

(defun clamp (v lo hi) (max lo (min hi v)))

(defun set-cursor (s x y)
  (setf (screen-cx s) (clamp x 0 (1- (screen-width  s)))
        (screen-cy s) (clamp y 0 (1- (screen-height s)))))

;;; ── Character writing ──────────────────────────────────────────────────────

(defun write-char-at-cursor (screen ch)
  "Write CH at the cursor, advancing it (and scrolling if necessary)."
  (let ((x (screen-cx screen))
        (y (screen-cy screen)))
    (setf (screen-cell screen x y)
          (make-cell :char  ch
                     :fg    (screen-cur-fg    screen)
                     :bg    (screen-cur-bg    screen)
                     :attrs (screen-cur-attrs screen)))
    (let ((nx (1+ x)))
      (cond
        ((< nx (screen-width screen))
         (setf (screen-cx screen) nx))
        (t
         (setf (screen-cx screen) 0)
         (cursor-down/scroll screen))))))

(defun cursor-down/scroll (screen)
  "Move cursor down one line, scrolling when at the bottom of the scroll region."
  (if (< (screen-cy screen) (screen-scroll-bottom screen))
      (incf (screen-cy screen))
      (scroll-up-one screen)))

;;; ── Scroll region ──────────────────────────────────────────────────────────

(defun scroll-up-one (screen)
  "Scroll the scroll region up one line; new bottom line is blank."
  (let ((top    (screen-scroll-top    screen))
        (bottom (screen-scroll-bottom screen))
        (w      (screen-width         screen)))
    (loop for row from top below bottom
          do (loop for col below w
                   do (setf (screen-cell screen col row)
                            (screen-cell screen col (1+ row)))))
    (loop for col below w
          do (setf (screen-cell screen col bottom) (blank-cell)))))

(defun scroll-down-one (screen)
  "Scroll the scroll region down one line; new top line is blank."
  (let ((top    (screen-scroll-top    screen))
        (bottom (screen-scroll-bottom screen))
        (w      (screen-width         screen)))
    (loop for row from bottom above top
          do (loop for col below w
                   do (setf (screen-cell screen col row)
                            (screen-cell screen col (1- row)))))
    (loop for col below w
          do (setf (screen-cell screen col top) (blank-cell)))))

;;; ── Erase helpers ──────────────────────────────────────────────────────────

(defun erase-region (screen x0 y0 x1 y1)
  (loop for y from y0 to y1
        do (let ((bx (if (= y y0) x0 0))
                 (ex (if (= y y1) x1 (1- (screen-width screen)))))
             (loop for x from bx to ex
                   do (setf (screen-cell screen x y) (blank-cell))))))

(defun erase-display (screen mode)
  (let ((cx (screen-cx screen)) (cy (screen-cy screen))
        (w  (screen-width  screen))
        (h  (screen-height screen)))
    (case mode
      (0 (erase-region screen cx cy (1- w) cy)
         (when (< (1+ cy) h) (erase-region screen 0 (1+ cy) (1- w) (1- h))))
      (1 (when (> cy 0) (erase-region screen 0 0 (1- w) (1- cy)))
         (erase-region screen 0 cy cx cy))
      (2 (erase-region screen 0 0 (1- w) (1- h))))))

(defun erase-line (screen mode)
  (let ((cx (screen-cx screen)) (cy (screen-cy screen))
        (w  (screen-width  screen)))
    (case mode
      (0 (erase-region screen cx  cy (1- w) cy))
      (1 (erase-region screen 0   cy cx     cy))
      (2 (erase-region screen 0   cy (1- w) cy)))))

;;; ── SGR (Select Graphic Rendition) ────────────────────────────────────────

(defun apply-sgr (screen params)
  (when (null params) (setf params '(0)))
  (dolist (p params)
    (cond
      ((= p  0) (setf (screen-cur-fg    screen) 7
                      (screen-cur-bg    screen) 0
                      (screen-cur-attrs screen) 0))
      ((= p  1) (setf (screen-cur-attrs screen)
                      (logior (screen-cur-attrs screen) +attr-bold+)))
      ((= p  2) (setf (screen-cur-attrs screen)
                      (logior (screen-cur-attrs screen) +attr-dim+)))
      ((= p  7) (setf (screen-cur-attrs screen)
                      (logior (screen-cur-attrs screen) +attr-reverse+)))
      ((= p 22) (setf (screen-cur-attrs screen)
                      (logand (screen-cur-attrs screen)
                              (lognot (logior +attr-bold+ +attr-dim+)))))
      ((= p 27) (setf (screen-cur-attrs screen)
                      (logand (screen-cur-attrs screen) (lognot +attr-reverse+))))
      ((<= 30 p 37) (setf (screen-cur-fg screen) (- p 30)))
      ((= p  39)    (setf (screen-cur-fg screen) 7))
      ((<= 40 p 47) (setf (screen-cur-bg screen) (- p 40)))
      ((= p  49)    (setf (screen-cur-bg screen) 0))
      ((<= 90 p 97)   (setf (screen-cur-fg screen) (+ 8 (- p  90))))
      ((<= 100 p 107) (setf (screen-cur-bg screen) (+ 8 (- p 100)))))))

;;; ── CSI sequence dispatch ──────────────────────────────────────────────────

(defun execute-csi (screen final intermed params)
  (let ((p1 (or (first  params) 0))
        (p2 (or (second params) 0)))
    (flet ((p1* () (max 1 p1))
           (p2* () (max 1 p2)))
      (if (eql intermed #\?)
          ;; DEC private sequences — minimal support
          (case final
            (#\h nil)   ; e.g. ?25h show cursor, ?1049h alt-screen — no-op
            (#\l nil))  ; e.g. ?25l hide cursor, ?1049l — no-op
          ;; Standard ANSI sequences
          (case final
            (#\A (set-cursor screen (screen-cx screen) (- (screen-cy screen) (p1*))))
            (#\B (set-cursor screen (screen-cx screen) (+ (screen-cy screen) (p1*))))
            (#\C (set-cursor screen (+ (screen-cx screen) (p1*)) (screen-cy screen)))
            (#\D (set-cursor screen (- (screen-cx screen) (p1*)) (screen-cy screen)))
            (#\E (set-cursor screen 0 (+ (screen-cy screen) (p1*))))
            (#\F (set-cursor screen 0 (- (screen-cy screen) (p1*))))
            (#\G (set-cursor screen (1- (p1*)) (screen-cy screen)))
            (#\H (set-cursor screen (1- (p2*)) (1- (p1*))))   ; row;col, 1-based
            (#\f (set-cursor screen (1- (p2*)) (1- (p1*))))
            (#\J (erase-display screen p1))
            (#\K (erase-line    screen p1))
            (#\L (dotimes (_ (p1*)) (scroll-down-one screen)))
            (#\M (dotimes (_ (p1*)) (scroll-up-one   screen)))
            (#\P ; delete chars right of cursor
             (let ((cx (screen-cx screen)) (cy (screen-cy screen))
                   (w  (screen-width screen)))
               (loop for x from cx to (- w (p1*) 1)
                     do (setf (screen-cell screen x cy)
                              (screen-cell screen (+ x (p1*)) cy)))
               (loop for x from (max cx (- w (p1*))) to (1- w)
                     do (setf (screen-cell screen x cy) (blank-cell)))))
            (#\S (dotimes (_ (p1*)) (scroll-up-one   screen)))
            (#\T (dotimes (_ (p1*)) (scroll-down-one screen)))
            (#\d (set-cursor screen (screen-cx screen) (1- (p1*))))
            (#\m (apply-sgr screen params))
            (#\r ; DECSTBM — set scroll region
             (let ((top    (max 0 (1- (p1*))))
                   (bottom (min (1- (screen-height screen)) (1- (max 1 p2)))))
               (when (< top bottom)
                 (setf (screen-scroll-top    screen) top
                       (screen-scroll-bottom screen) bottom)
                 (set-cursor screen 0 0))))
            (otherwise nil))))))

;;; ── Main entry point: feed raw PTY bytes ──────────────────────────────────

(defun screen-process-bytes (screen bytes &key (start 0) (end (length bytes)))
  "Update SCREEN by processing raw PTY output BYTES[START..END)."
  (loop for i from start below end
        for b = (aref bytes i)
        do (ecase (screen-state screen)

             (:ground
              (cond
                ((= b #x1B) (setf (screen-state screen) :escape))
                ((= b #x0D) (setf (screen-cx screen) 0))
                ((= b #x0A) (cursor-down/scroll screen))
                ((= b #x0B) (cursor-down/scroll screen))
                ((= b #x0C) (cursor-down/scroll screen))
                ((= b #x08) (when (> (screen-cx screen) 0)
                               (decf (screen-cx screen))))
                ((= b #x09) (let ((nx (* 8 (ceiling (1+ (screen-cx screen)) 8))))
                               (set-cursor screen (min nx (1- (screen-width screen)))
                                           (screen-cy screen))))
                ((= b #x07) nil)                    ; BEL — ignore
                ((= b #x7F) nil)                    ; DEL — ignore
                ((and (>= b #x20) (< b #x7F))
                 (write-char-at-cursor screen (code-char b)))
                ((>= b #xC0)                        ; UTF-8 multi-byte lead
                 (write-char-at-cursor screen #\?))))  ; replace; continuations skipped

             (:escape
              (cond
                ((= b #x5B) ; ESC [ → CSI
                 (setf (screen-state screen)       :csi
                       (screen-params screen)       '()
                       (screen-cur-param screen)    nil
                       (screen-intermediate screen) nil))
                ((= b #x5D) ; ESC ] → OSC
                 (setf (screen-state screen) :osc))
                ((= b #x4D) ; ESC M → reverse index
                 (if (= (screen-cy screen) (screen-scroll-top screen))
                     (scroll-down-one screen)
                     (decf (screen-cy screen)))
                 (setf (screen-state screen) :ground))
                ((= b #x63) ; ESC c → RIS (full reset)
                 (erase-region screen 0 0
                               (1- (screen-width screen))
                               (1- (screen-height screen)))
                 (set-cursor screen 0 0)
                 (setf (screen-cur-fg    screen) 7
                       (screen-cur-bg    screen) 0
                       (screen-cur-attrs screen) 0
                       (screen-scroll-top    screen) 0
                       (screen-scroll-bottom screen) (1- (screen-height screen))
                       (screen-state screen) :ground))
                (t (setf (screen-state screen) :ground))))

             (:csi
              (cond
                ((and (>= b #x30) (<= b #x39)) ; digit 0-9
                 (setf (screen-cur-param screen)
                       (+ (* (or (screen-cur-param screen) 0) 10)
                          (- b #x30))))
                ((= b #x3B) ; ; separator
                 (push (or (screen-cur-param screen) 0) (screen-params screen))
                 (setf (screen-cur-param screen) nil))
                ((= b #x3F) (setf (screen-intermediate screen) #\?))
                ((= b #x3E) (setf (screen-intermediate screen) #\>))
                ((and (>= b #x40) (<= b #x7E)) ; final byte
                 (when (screen-cur-param screen)
                   (push (screen-cur-param screen) (screen-params screen)))
                 (execute-csi screen (code-char b)
                              (screen-intermediate screen)
                              (nreverse (screen-params screen)))
                 (setf (screen-state screen) :ground))
                (t (setf (screen-state screen) :ground)))) ; malformed → bail

             (:osc
              ;; Discard OSC payload; terminated by BEL or ESC \.
              (when (= b #x07)
                (setf (screen-state screen) :ground))))))
