(in-package #:cl-tmux/commands)

;;; ── Copy-mode word / WORD navigation ───────────────────────────────────────
;;;
;;; This file contains the word-motion core shared by vi w/b/e and W/B/E.

;;; ── Row-character helper ─────────────────────────────────────────────────────

(defun %copy-mode-row-chars (screen row)
  "Return a simple-vector of characters on ROW of SCREEN in viewport projection.
   Uses the scrollback offset so word navigation works correctly in copy mode.
   Returns a simple-vector for O(1) indexed access in word-motion loops."
  (let* ((width  (screen-width screen))
         (result (make-array width :element-type 'character)))
    (dotimes (col width result)
      (setf (aref result col)
            (cell-char (screen-display-cell screen col row))))))

(defun %word-separator-p (ch)
  "Return T when CH is a word separator according to the 'word-separators' option.
   Default separators: space, hyphen, underscore, at-sign."
  (let ((seps (or (cl-tmux/options:get-option "word-separators") " -_@")))
    (find ch seps :test #'char=)))

(defun %space-separator-p (ch)
  "Return T when CH is whitespace.  Used by the WORD-motion commands (vi W/B/E),
   which treat a WORD as a run of non-blank characters separated only by spaces —
   independent of the 'word-separators' option that drives w/b/e."
  (or (char= ch #\Space) (char= ch #\Tab)))

(defun %copy-mode-word-bounds (chars col max-col sep-pred)
  "Return (values start-col end-col) for the word or separator cell at COL.
   CHARS is a simple-vector of row characters.  MAX-COL is the last valid index.
   SEP-PRED classifies separator characters."
  (if (funcall sep-pred (aref chars col))
      (values col col)
      (let ((start col)
            (end   col))
        (loop while (and (> start 0)
                         (not (funcall sep-pred (aref chars (1- start)))))
              do (decf start))
        (loop while (and (< end max-col)
                         (not (funcall sep-pred (aref chars (1+ end)))))
              do (incf end))
        (values start end))))

(defun %copy-mode-word-at-cursor (screen)
  "Return the word under the copy-mode cursor as a string.
   The boundary rules match copy-mode-select-word:
   - on a word character, expand to the surrounding word bounds;
   - on a separator character, return the single cell under the cursor.
   Returns NIL when copy mode is inactive or the screen has no usable cursor."
  (when (screen-copy-mode-p screen)
    (let* ((cur (or (screen-copy-cursor screen)
                    (cons (1- (screen-height screen)) 0)))
             (w   (screen-width screen))
             (max-col (1- w))
             (row (max 0 (min (1- (screen-height screen)) (car cur))))
             (col (max 0 (min max-col (cdr cur))))
             (chars (%copy-mode-row-chars screen row)))
        (multiple-value-bind (start end)
            (%copy-mode-word-bounds chars col max-col #'%word-separator-p)
          (coerce (subseq chars start (1+ end)) 'string)))))

(defun %copy-mode-select-word-bounds (screen row col)
  "Return (values start-col end-col) for the word or separator cell at ROW/COL.
   On a separator, both values are COL so callers can select the single cell."
  (let* ((width   (screen-width screen))
         (max-col (1- width))
         (chars   (%copy-mode-row-chars screen row)))
    (%copy-mode-word-bounds chars col max-col #'%word-separator-p)))

(defun copy-mode-select-word (screen)
  "Select the word under the copy-mode cursor (tmux copy-mode `select-word`).
   Word characters are defined by the same `word-separators` option used by the
   w/b/e word-motion commands (via %word-separator-p), so selection is
   consistent with word navigation.  The mark is placed on the first word
   character and the cursor on the column just past the last word character so
   that %selection-text extracts exactly the word (the single-row selection
   reads columns [start-col, end-col) exclusively).  When the cursor is not on a
   word character, only the single cell under the cursor is selected.  Marks the
   screen dirty.  No-op when copy mode is inactive."
  (when (screen-copy-mode-p screen)
    (let* ((cur (or (screen-copy-cursor screen)
                    (cons (1- (screen-height screen)) 0)))
           (w   (screen-width screen))
           (max-row (1- (screen-height screen)))
           ;; Clamp row and col to the current grid so cell reads never escape.
           (row (max 0 (min max-row (car cur))))
           (col (max 0 (min (1- w) (cdr cur)))))
      (multiple-value-bind (start end)
          (%copy-mode-select-word-bounds screen row col)
        (setf (screen-copy-mark   screen) (cons row start)
              ;; Exclusive end may reach width so the last word cell is kept.
              (screen-copy-cursor screen) (cons row (min w (1+ end)))))
      (setf (screen-copy-mark-offset screen) (screen-copy-offset screen)
            (screen-copy-selecting   screen) t
            (screen-dirty-p          screen) t))))

(defmacro with-copy-mode-dirty (screen &body body)
  "Execute BODY only when SCREEN is in copy mode; mark the screen dirty on exit."
  `(when (screen-copy-mode-p ,screen)
     ,@body
     (setf (screen-dirty-p ,screen) t)))

;;; ── Multi-line word-navigation helpers ──────────────────────────────────────
;;;
;;; Real tmux copy-mode `w`/`b`/`e` (and W/B/E) cross line boundaries.  Three
;;; private helpers implement the scans parameterised on a separator predicate;
;;; the public defuns are thin wrappers that supply the right predicate.
;;;
;;; When a forward scan exhausts the current row it calls %scroll-down-one-line
;;; to advance to the next row.  The "saved cursor" idiom detects the no-op
;;; case (already at the bottom of history) and stops rather than looping.
;;; Backward wrapping mirrors the same pattern via %scroll-up-one-line.

(defun %word-forward-impl (screen sep-pred)
  "Move the cursor forward to the start of the next word, crossing lines.
   SEP-PRED classifies separator characters."
  (with-copy-mode-dirty screen
    (let* ((row   (car (screen-copy-cursor screen)))
           (col   (cdr (screen-copy-cursor screen)))
           (width (screen-width screen))
           (chars (%copy-mode-row-chars screen row)))
      ;; Skip over the current word characters (non-separator run).
      (loop while (and (< col width)
                       (not (funcall sep-pred (aref chars col))))
            do (incf col))
      ;; Skip over separators to reach the next word start.
      (loop while (and (< col width)
                       (funcall sep-pred (aref chars col)))
            do (incf col))
      ;; If the scan fell off the end of the row, wrap down to BOL of next row.
      (if (>= col width)
          (let ((saved (screen-copy-cursor screen)))
            (%scroll-down-one-line screen row 0 (screen-height screen))
            ;; If scroll was a no-op (bottom of history), stay at last col.
            (when (equal saved (screen-copy-cursor screen))
              (setf (screen-copy-cursor screen) (cons row (1- width)))))
          (setf (screen-copy-cursor screen) (cons row col))))))

(defun %word-backward-impl (screen sep-pred)
  "Move the cursor backward to the start of the current/previous word, crossing lines.
   SEP-PRED classifies separator characters."
  (with-copy-mode-dirty screen
    (let* ((row     (car (screen-copy-cursor screen)))
           (col     (cdr (screen-copy-cursor screen)))
           (max-off (length (screen-scrollback screen))))
      ;; At BOL: wrap to EOL of the previous row before scanning.
      (when (= col 0)
        (let ((saved (screen-copy-cursor screen)))
          (%scroll-up-one-line screen row (1- (screen-width screen)) max-off)
          (unless (equal saved (screen-copy-cursor screen))
            (let ((cur (screen-copy-cursor screen)))
              (setf row (car cur)
                    col (cdr cur))))))
      ;; Scan backward over separators then over word characters.
      (let ((chars (%copy-mode-row-chars screen row)))
        (loop while (and (> col 0) (funcall sep-pred (aref chars (1- col))))
              do (decf col))
        (loop while (and (> col 0) (not (funcall sep-pred (aref chars (1- col)))))
              do (decf col))
        (setf (screen-copy-cursor screen) (cons row (max 0 col)))))))

(defun %word-end-impl (screen sep-pred)
  "Move the cursor to the last character of the current/next word, crossing lines.
   SEP-PRED classifies separator characters."
  (with-copy-mode-dirty screen
    (let* ((row   (car (screen-copy-cursor screen)))
           (col   (cdr (screen-copy-cursor screen)))
           (width (screen-width screen))
           (chars (%copy-mode-row-chars screen row)))
      ;; If at the last char of a word, step once to cross into separator territory.
      (when (and (< col (1- width))
                 (not (funcall sep-pred (aref chars col)))
                 (funcall sep-pred (aref chars (1+ col))))
        (incf col))
      ;; Skip separators; wrap to the next row when the current row is exhausted.
      (loop
        (loop while (and (< col width) (funcall sep-pred (aref chars col)))
              do (incf col))
        (when (< col width) (return))
        ;; Fell off EOL during separator scan — try to wrap down.
        (let ((saved (screen-copy-cursor screen)))
          (%scroll-down-one-line screen row 0 (screen-height screen))
          (if (equal saved (screen-copy-cursor screen))
              (return)  ; at history bottom, stop
              (let ((cur (screen-copy-cursor screen)))
                (setf row (car cur)
                      col (cdr cur)
                      chars (%copy-mode-row-chars screen row))))))
      ;; Advance to the last character of the word.
      (loop while (and (< col (1- width))
                       (not (funcall sep-pred (aref chars (1+ col)))))
            do (incf col))
      (setf (screen-copy-cursor screen) (cons row (min (1- width) col))))))

;;; Public API: word (w/b/e) and WORD (W/B/E) motions.

(defmacro define-word-motion-suite (prefix sep-pred forward-name backward-name end-name)
  "Generate three word-motion functions sharing SEP-PRED: FORWARD-NAME, BACKWARD-NAME, END-NAME."
  (declare (ignore prefix))
  `(progn
     (defun ,forward-name  (screen) (%word-forward-impl  screen ,sep-pred))
     (defun ,backward-name (screen) (%word-backward-impl screen ,sep-pred))
     (defun ,end-name      (screen) (%word-end-impl      screen ,sep-pred))))

;;; word motion (vi w/b/e): punctuation-delimited word.
(define-word-motion-suite word #'%word-separator-p
  copy-mode-word-forward copy-mode-word-backward copy-mode-word-end)

;;; WORD motion (vi W/B/E): blank-delimited — a WORD spans punctuation, stops only at spaces.
(define-word-motion-suite space #'%space-separator-p
  copy-mode-space-forward copy-mode-space-backward copy-mode-space-end)
