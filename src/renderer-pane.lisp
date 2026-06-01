(in-package #:cl-tmux/renderer)

;;;; Pane and border rendering.
;;;;
;;;; Depends on the ANSI escape-code primitives from renderer-format.lisp
;;;; (loaded first in the same package) and the layout/model structures from
;;;; cl-tmux/model.

;;; ── Pane ────────────────────────────────────────────────────────────────────

;;; ── Clock-mode ASCII digit font (3 rows tall) ───────────────────────────────
;;;
;;; Each digit is represented as a list of 3 strings, each 3 chars wide.
;;; Spacing between digits is 1 space.  The colon separator is 1 char wide.

(defparameter *clock-digits*
  '(("███" "█ █" "███")   ; 0
    ("  █" "  █" "  █")   ; 1
    ("██ " " ██" " ██")   ; 2 — corrected
    ("██ " " █ " "███")   ; 3 — corrected
    ("█ █" "███" "  █")   ; 4
    (" ██" "██ " "███")   ; 5 — corrected
    (" █ " "███" "███")   ; 6
    ("███" "  █" "  █")   ; 7
    ("███" "███" "███")   ; 8
    ("███" "███" "  █"))  ; 9
  "3-row ASCII digit font. Each entry is (row0 row1 row2) for a 3-wide glyph.")

(defun %clock-digit-rows (digit)
  "Return the 3 display rows for DIGIT (0–9) from *clock-digits*."
  (nth digit *clock-digits*))

(defun draw-clock-to-screen (stream ox oy pw ph)
  "Render the current time HH:MM as 3-row ASCII digits centred in the pane
   at terminal offset (OX, OY), clipping to the pane rectangle (PW x PH).
   Only renders if the pane is at least 13 columns wide and 3 rows tall."
  (when (and (>= pw 13) (>= ph 3))
    (multiple-value-bind (sec min hour) (get-decoded-time)
      (declare (ignore sec))
      ;; Format: two digits, colon, two digits = 3+1+3+1+3+1+3 = 15 chars
      ;; But we use simple 3-char digits + 1-char separators:
      ;; D D : D D = 3+1+3+1+1+1+3+1+3 = 17 chars; trim to 13 "HH:MM" minimal.
      ;; Represent as list of (rows . char-sequence) for each position.
      (let* ((h0 (floor hour 10))
             (h1 (mod   hour 10))
             (m0 (floor min  10))
             (m1 (mod   min  10))
             ;; Build row strings for the 3 display rows
             (sep-rows '("   " " █ " "   "))  ; colon separator rows
             (rows (loop for row-idx from 0 below 3
                         collect (concatenate 'string
                                   (nth row-idx (%clock-digit-rows h0))
                                   " "
                                   (nth row-idx (%clock-digit-rows h1))
                                   (nth row-idx sep-rows)
                                   (nth row-idx (%clock-digit-rows m0))
                                   " "
                                   (nth row-idx (%clock-digit-rows m1)))))
             ;; Centre within the pane
             (clock-w (length (first rows)))
             (clock-h 3)
             (start-col (max 0 (floor (- pw clock-w) 2)))
             (start-row (max 0 (floor (- ph clock-h) 2))))
        ;; Blue background, bright cyan text for clock face
        (format stream "~C[44;96m" +esc+)
        (loop for row-str in rows
              for roff from 0 do
          (let ((term-row (+ oy start-row roff))
                (term-col (+ ox start-col)))
            (move-to stream term-row term-col)
            (write-string (subseq row-str 0 (min (length row-str) pw)) stream)))
        (reset-attrs stream)))))

(defun render-pane (stream pane)
  "Draw the pane's screen into the real terminal at the pane's (x, y) offset.
   When *clock-mode-pane-id* matches (pane-id pane), draw a clock overlay."
  (let* ((screen (pane-screen pane))
         (pw     (pane-width   pane))
         (ph     (pane-height  pane))
         (ox     (pane-x      pane))
         (oy     (pane-y      pane)))
    (with-lock-held ((screen-lock screen))
      ;; Hoist selection boundary computation outside the cell loop so it is
      ;; computed once per frame instead of once per cell (~1920 times).
      (let* ((sel-active (and (screen-copy-selecting screen)
                              (consp (screen-copy-mark   screen))
                              (consp (screen-copy-cursor screen))))
             (sel-start-r 0) (sel-end-r 0) (sel-start-c 0) (sel-end-c 0))
        (when sel-active
          (let* ((mark   (screen-copy-mark   screen))
                 (cursor (screen-copy-cursor screen))
                 (mr (car mark))   (mc (cdr mark))
                 (cr (car cursor)) (cc (cdr cursor))
                 ;; mark/cursor are live-grid rows (0..height-1).
                 ;; Viewport row = live-grid row + copy-offset, so add the offset
                 ;; here so that the in-sel check below uses viewport coordinates,
                 ;; matching the row variable in the render loop.
                 (offset (screen-copy-offset screen)))
            (setf sel-start-r (+ (min mr cr) offset)
                  sel-end-r   (+ (max mr cr) offset)
                  sel-start-c (if (< mr cr) mc (if (> mr cr) cc (min mc cc)))
                  sel-end-c   (if (< mr cr) cc (if (> mr cr) mc (max mc cc))))))
        (let ((prev-fg -1) (prev-bg -1) (prev-attrs -1))
          (loop for row below ph do
            (move-to stream (+ oy row) ox)
            (loop for col below pw
                  for cell  = (screen-display-cell screen col row)
                  ;; A continuation cell (width 0) is the right half of a
                  ;; double-width glyph the terminal already drew — emit nothing.
                  unless (zerop (cell-width cell))
                    do (let* ((fg    (cell-fg    cell))
                              (bg    (cell-bg    cell))
                              (in-sel (and sel-active
                                           (cond
                                             ((= sel-start-r sel-end-r row)
                                              (and (<= sel-start-c col) (< col sel-end-c)))
                                             ((= row sel-start-r) (>= col sel-start-c))
                                             ((= row sel-end-r)   (< col sel-end-c))
                                             (t (and (> row sel-start-r)
                                                     (< row sel-end-r))))))
                              (attrs (if in-sel
                                         (logxor (cell-attrs cell) cl-tmux/terminal/types:+attr-reverse+)
                                         (cell-attrs cell))))
                         (unless (and (= fg prev-fg) (= bg prev-bg) (= attrs prev-attrs))
                           (render-cell-attrs stream fg bg attrs)
                           (setf prev-fg fg prev-bg bg prev-attrs attrs))
                         (write-char (cell-char cell) stream))))))
      (screen-clear-dirty screen))
    ;; Clock-mode overlay: draw a digital clock if this pane is the clock pane.
    (when (eql cl-tmux::*clock-mode-pane-id* (pane-id pane))
      (draw-clock-to-screen stream ox oy pw ph))))

;;; ── Split-tree separators ───────────────────────────────────────────────────

(defun layout-subtree-rect (node)
  "Bounding rectangle of NODE's leaves as a plist (:x :y :w :h), derived from the
   already-laid-out pane geometry."
  (let ((panes (layout-leaves node)))
    (let ((min-x (reduce #'min panes :key #'pane-x))
          (min-y (reduce #'min panes :key #'pane-y))
          (max-x (reduce #'max panes :key (lambda (p) (+ (pane-x p) (pane-width p)))))
          (max-y (reduce #'max panes :key (lambda (p) (+ (pane-y p) (pane-height p))))))
      (list :x min-x :y min-y :w (- max-x min-x) :h (- max-y min-y)))))

(defun subtree-contains-p (node pane)
  "True when PANE is a leaf of NODE's subtree."
  (and pane (member pane (layout-leaves node))))

;;; ── Border style SGR helpers ────────────────────────────────────────────────

(defun %apply-border-style (stream style-string activep)
  "Emit the SGR code(s) for a pane border.
   ACTIVEP selects the active vs. inactive style option string.
   Supported format: \"default\" → reset, \"fg=COLOR\" → foreground colour only."
  (declare (ignore activep))
  (cond
    ((or (null style-string)
         (string-equal style-string "default"))
     (reset-attrs stream))
    ((and (>= (length style-string) 3)
          (string-equal (subseq style-string 0 3) "fg="))
     (let ((color-name (subseq style-string 3)))
       (reset-attrs stream)
       ;; Map named ANSI colours and the "green" shorthand used as default.
       (let ((code (cond
                     ((string-equal color-name "black")   30)
                     ((string-equal color-name "red")     31)
                     ((string-equal color-name "green")   32)
                     ((string-equal color-name "yellow")  33)
                     ((string-equal color-name "blue")    34)
                     ((string-equal color-name "magenta") 35)
                     ((string-equal color-name "cyan")    36)
                     ((string-equal color-name "white")   37)
                     (t nil))))
         (when code
           (format stream "~C[~Dm" +esc+ code)))))
    (t (reset-attrs stream))))

;;; ── Separator renderers (data layer — what each orientation draws) ──────────

(defun %render-h-separator (stream node active-pane terminal-cols)
  "Draw the │ column between the left and right children of an :h split.
   Applies the pane-border-style / pane-active-border-style option."
  (let* ((a          (layout-split-first  node))
         (b          (layout-split-second node))
         (rect       (layout-subtree-rect a))
         (border-col (+ (getf rect :x) (getf rect :w)))
         (activep    (or (subtree-contains-p a active-pane)
                         (subtree-contains-p b active-pane)))
         (style      (if activep
                         (cl-tmux/options:get-option "pane-active-border-style" "fg=green")
                         (cl-tmux/options:get-option "pane-border-style" "default"))))
    (when (< border-col terminal-cols)
      (%apply-border-style stream style activep)
      (loop for row from (getf rect :y) below (+ (getf rect :y) (getf rect :h))
            do (move-to stream row border-col)
               (write-char #\│ stream))
      (reset-attrs stream))))

(defun %render-v-separator (stream node terminal-cols)
  "Draw the ─ row between the top and bottom children of a :v split."
  (let* ((rect       (layout-subtree-rect (layout-split-first node)))
         (border-row (+ (getf rect :y) (getf rect :h)))
         (x          (getf rect :x))
         (w          (min (getf rect :w) (- terminal-cols x))))
    (reset-attrs stream)
    (move-to stream border-row x)
    (loop repeat (max 0 w) do (write-char #\─ stream))))

;;; ── Tree border walk (logic layer) ──────────────────────────────────────────

(defun render-tree-borders (stream node active-pane terminal-cols)
  "Walk the split-tree NODE, drawing one separator per internal split node.
   :h (left|right) splits draw │ bars; :v (top/bottom) splits draw ─ bars.
   Recurses into both children after drawing the parent separator."
  (when (layout-split-p node)
    (ecase (layout-split-orientation node)
      (:h (%render-h-separator stream node active-pane terminal-cols))
      (:v (%render-v-separator stream node terminal-cols)))
    (render-tree-borders stream (layout-split-first  node) active-pane terminal-cols)
    (render-tree-borders stream (layout-split-second node) active-pane terminal-cols)))
