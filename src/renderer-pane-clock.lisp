(in-package #:cl-tmux/renderer)

;;; Clock-mode and display-panes big-digit rendering.
;;;
;;; `renderer-compose.lisp` consumes `%draw-pane-number-to-screen`, so this file
;;; is loaded from `renderer-pane.lisp` before the compose layer is compiled.

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

(defun %clock-display-hour (hour)
  "Convert HOUR (0–23) to the displayed hour per the clock-mode-style option:
   24 (default) → unchanged; 12 → a 12-hour clock (0 → 12, 13–23 → 1–11)."
  (if (eql 12 (cl-tmux/options:get-option "clock-mode-style" 24))
      (let ((h (mod hour 12))) (if (zerop h) 12 h))
      hour))

(defun %clock-face-sgr ()
  "SGR parameter string for the clock face, from the clock-mode-colour option
   (a foreground colour name mapped to its SGR code; falls back to bright cyan
   when the name is unknown)."
  (format nil "~D" (or (%border-color-sgr
                        (cl-tmux/options:get-option "clock-mode-colour" "blue"))
                       96)))

;;; +min-clock-width+ : minimum terminal columns required to render the HH:MM clock.
;;; The clock occupies 3-char digits with 1-char spacing: 3+1+3+1+1+1+3+1+3 = 17 chars
;;; rendered, but the minimal HH:MM glyph block is 13 columns wide (two 3-wide digits,
;;; one 1-wide colon, two 1-wide spaces, two more 3-wide digits, no trailing space).
(defconstant +min-clock-width+ 13
  "Minimum pane width in columns required to render the clock-mode HH:MM display.")

(defun %center-coord (total size)
  "Return the column/row offset to center SIZE within TOTAL (clamped to 0)."
  (max 0 (floor (- total size) 2)))

(defun %emit-sgr (stream code)
  "Emit an ANSI SGR escape sequence (ESC[CODEm) to STREAM.
   CODE may be an integer or a string (e.g. \"44;96\" for compound SGR parameters).
   A no-op when CODE is NIL — allows callers to pass optional style codes directly."
  (when code (format stream "~C[~Am" +esc+ code)))

(defun %blit-rows (stream rows ox oy start-row start-col max-width)
  "Write ROWS (a list of strings) at terminal position (OX+START-COL, OY+START-ROW),
   clipping each to MAX-WIDTH columns."
  (loop for row-str in rows
        for roff from 0 do
    (move-to stream (+ oy start-row roff) (+ ox start-col))
    (write-string (subseq row-str 0 (min (length row-str) max-width)) stream)))

(defun draw-clock-to-screen (stream ox oy pw ph)
  "Render the current time HH:MM as 3-row ASCII digits centred in the pane
   at terminal offset (OX, OY), clipping to the pane rectangle (PW x PH).
   Honours clock-mode-style (12/24-hour) and clock-mode-colour (digit colour).
   Only renders if the pane is at least +MIN-CLOCK-WIDTH+ columns wide and 3 rows tall."
  (when (and (>= pw +min-clock-width+) (>= ph 3))
    (multiple-value-bind (sec min hour) (get-decoded-time)
      (declare (ignore sec))
      (setf hour (%clock-display-hour hour))
      ;; Format: two digits, colon, two digits = 3+1+3+1+3+1+3 = 15 chars
      ;; But we use simple 3-char digits + 1-char separators:
      ;; D D : D D = 3+1+3+1+1+1+3+1+3 = 17 chars; trim to 13 "HH:MM" minimal.
      ;; Represent as list of (rows . char-sequence) for each position.
      (let* ((h0 (floor hour 10))
             (h1 (mod   hour 10))
             (m0 (floor min  10))
             (m1 (mod   min  10))
             ;; Colon glyph rows: centre row lights a block, top/bottom are blank.
             (colon-separator-rows '("   " " █ " "   "))
             ;; Build row strings for the 3 display rows
             (rows (loop for row-idx from 0 below 3
                         collect (concatenate 'string
                                   (nth row-idx (%clock-digit-rows h0))
                                   " "
                                   (nth row-idx (%clock-digit-rows h1))
                                   (nth row-idx colon-separator-rows)
                                   (nth row-idx (%clock-digit-rows m0))
                                   " "
                                   (nth row-idx (%clock-digit-rows m1)))))
             ;; Centre within the pane
             (clock-w (length (first rows)))
             (clock-h 3)
             (start-col (%center-coord pw clock-w))
             (start-row (%center-coord ph clock-h)))
        ;; Blue background, bright cyan text for clock face.
        ;; Use a named constant consistent with +sgr-default-status+ (44;97) but with
        ;; bright cyan (96) instead of bright white to distinguish the clock from the bar.
        (%emit-sgr stream (%clock-face-sgr))
        (%blit-rows stream rows ox oy start-row start-col pw)
        (reset-attrs stream)))))

(defun %draw-pane-number-to-screen (stream ox oy pw ph number active-p)
  "Draw NUMBER (a pane index) as centred 3-row big digits in the pane at terminal
   offset (OX,OY), clipped to the pane rectangle (PW x PH).  Coloured by
   display-panes-active-colour when ACTIVE-P, else display-panes-colour (a colour
   name → fg SGR; fallback blue 34).  This is tmux's display-panes (C-b q) display;
   reuses the clock's big-digit glyphs.  Renders only if the pane is ≥ 3x3."
  (when (and (>= pw 3) (>= ph 3))
    (let* ((digits    (map 'list #'digit-char-p (princ-to-string number)))
           (rows      (loop for row-idx below 3
                            collect (format nil "~{~A~^ ~}"
                                            (mapcar (lambda (d)
                                                      (nth row-idx (%clock-digit-rows d)))
                                                    digits))))
           (num-w     (length (first rows)))
           (start-col (%center-coord pw num-w))
           (start-row (%center-coord ph 3))
           (colour    (if active-p
                          (cl-tmux/options:get-option "display-panes-active-colour" "red")
                          (cl-tmux/options:get-option "display-panes-colour" "blue")))
           (sgr       (or (%border-color-sgr colour) 34)))
      (%emit-sgr stream sgr)
      (%blit-rows stream rows ox oy start-row start-col pw)
      (reset-attrs stream))))
