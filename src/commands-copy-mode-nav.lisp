(in-package #:cl-tmux/commands)

;;; ── Copy-mode word / line / screen navigation ───────────────────────────────
;;;
;;; This file contains word-motion (w/b/e), line-motion (0/$), screen-position
;;; jumps (H/M/L), and page-scroll commands.  It depends on the viewport and
;;; cursor primitives in commands-copy-mode.lisp.

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
  (when (screen-copy-mode-p screen)
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
          (setf (screen-copy-cursor screen) (cons row col)))
      (setf (screen-dirty-p screen) t))))

(defun %word-backward-impl (screen sep-pred)
  "Move the cursor backward to the start of the current/previous word, crossing lines.
   SEP-PRED classifies separator characters."
  (when (screen-copy-mode-p screen)
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
        (setf (screen-copy-cursor screen) (cons row (max 0 col))
              (screen-dirty-p screen) t)))))

(defun %word-end-impl (screen sep-pred)
  "Move the cursor to the last character of the current/next word, crossing lines.
   SEP-PRED classifies separator characters."
  (when (screen-copy-mode-p screen)
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
      (setf (screen-copy-cursor screen) (cons row (min (1- width) col))
            (screen-dirty-p screen) t))))

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

;;; ── Line start / end ─────────────────────────────────────────────────────────
;;;
;;; define-line-jump-commands: prolog-style table for commands that jump the
;;; cursor column while keeping the current row.  COL-EXPR has ROW and SCREEN
;;; bound (ROW from (car (screen-copy-cursor screen)), SCREEN is the argument).

(defmacro define-line-jump-commands (&rest specs)
  "Generate copy-mode column-jump functions from a declarative (name doc col-expr) table.
   COL-EXPR may reference SCREEN and the current ROW."
  `(progn
     ,@(mapcar (lambda (spec)
                 (destructuring-bind (name doc col-expr) spec
                   `(defun ,name (screen)
                      ,doc
                      (when (screen-copy-mode-p screen)
                        (let ((row (car (screen-copy-cursor screen))))
                          (declare (ignorable row))
                          (setf (screen-copy-cursor screen)
                                (cons row ,col-expr)
                                (screen-dirty-p screen) t))))))
               specs)))

(define-line-jump-commands
  (copy-mode-line-start
   "Move cursor to column 0 of the current row (vi 0)."
   0)
  (copy-mode-line-end
   "Move cursor to the last column of the current row (vi $)."
   (1- (screen-width screen)))
  (copy-mode-back-to-indentation
   "Move cursor to the first non-blank character of the current row (vi ^).
    Distinct from line-start: on an indented line ^ stops at the indent.
    Falls back to column 0 when the row is entirely blank."
   (or (position-if-not #'%space-separator-p (%copy-mode-row-chars screen row)) 0)))

;;; ── Declarative cursor-jump and scroll-wrapper macro tables ─────────────────
;;;
;;; define-copy-mode-cursor-jump: prolog-style facts table for commands that
;;; jump the cursor row to a fixed or computed row expression, keeping column.
;;;
;;;   (define-copy-mode-cursor-jump (name docstring row-expr) ...)
;;;
;;; Each fact expands to a defun with the guard (when (screen-copy-mode-p screen)).
;;;
;;; define-copy-mode-scroll-commands: prolog-style facts table for commands that
;;; delegate to copy-mode-scroll with a fixed delta expression.
;;;
;;;   (define-copy-mode-scroll-commands (name docstring delta-expr) ...)

(defmacro define-copy-mode-cursor-jump (&rest rules)
  "Generate one copy-mode cursor-jump defun per rule.
   Each RULE is (function-name docstring row-expression).
   The generated function jumps the cursor to ROW-EXPR, keeping the current
   cursor column unchanged, and marks the screen dirty."
  `(progn
     ,@(mapcar (lambda (rule)
                 (destructuring-bind (name docstring row-expr) rule
                   `(defun ,name (screen)
                      ,docstring
                      (when (screen-copy-mode-p screen)
                        (let ((new-row ,row-expr)
                              (cur-col (cdr (screen-copy-cursor screen))))
                          (setf (screen-copy-cursor screen) (cons new-row cur-col)
                                (screen-dirty-p screen) t))))))
               rules)))

(defmacro define-copy-mode-scroll-commands (&rest rules)
  "Generate one copy-mode scroll-wrapper defun per rule.
   Each RULE is (function-name docstring delta-expression).
   The generated function guards on copy-mode-p and delegates to copy-mode-scroll."
  `(progn
     ,@(mapcar (lambda (rule)
                 (destructuring-bind (name docstring delta-expr) rule
                   `(defun ,name (screen)
                      ,docstring
                      (when (screen-copy-mode-p screen)
                        (copy-mode-scroll screen ,delta-expr)))))
               rules)))

(define-copy-mode-cursor-jump
  (copy-mode-high
   "Move cursor to row 0 (top of viewport), keeping column."
   0)
  (copy-mode-middle
   "Move cursor to the middle row of the viewport, keeping column."
   (floor (screen-height screen) 2))
  (copy-mode-low
   "Move cursor to the last row of the viewport (height-1), keeping column."
   (1- (screen-height screen))))

(define-copy-mode-scroll-commands
  (copy-mode-page-up
   "Scroll the viewport back by one full screen height."
   (screen-height screen))
  (copy-mode-page-down
   "Scroll the viewport forward by one full screen height."
   (- (screen-height screen)))
  (copy-mode-half-page-up
   "Scroll the viewport back by half a screen height."
   (floor (screen-height screen) 2))
  (copy-mode-half-page-down
   "Scroll the viewport forward by half a screen height."
   (- (floor (screen-height screen) 2)))
  (copy-mode-scroll-up-line
   "Scroll the viewport back by 1 line (cursor stays fixed when possible)."
   1)
  (copy-mode-scroll-down-line
   "Scroll the viewport forward by 1 line (cursor stays fixed when possible)."
   -1)
  (copy-mode-top
   "Jump to the oldest scrollback line (maximum scroll-back offset)."
   +scroll-to-oldest+)
  (copy-mode-bottom
   "Jump to the live view bottom (scroll-offset = 0)."
   +scroll-to-newest+))

;;; ── Scroll-middle (vi z) ─────────────────────────────────────────────────────

(defun copy-mode-scroll-middle (screen)
  "Scroll the viewport so the cursor row is centered (tmux copy-mode-vi z).
   Adjusts the copy-offset so the current cursor row appears at the middle of
   the viewport, then moves the cursor row to that center row.  If the history
   limit prevents full centering the offset is clamped and the cursor is placed
   at the achievable nearest-center row."
  (when (screen-copy-mode-p screen)
    (let* ((row     (car (screen-copy-cursor screen)))
           (h       (screen-height screen))
           (sb-n    (length (screen-scrollback screen)))
           (center  (floor h 2))
           (offset  (screen-copy-offset screen))
           (new-off (max 0 (min sb-n (+ offset (- center row)))))
           (delta   (- new-off offset))
           (new-row (max 0 (min (1- h) (+ row delta)))))
      (setf (screen-copy-offset screen) new-off
            (screen-copy-cursor screen) (cons new-row (cdr (screen-copy-cursor screen)))
            (screen-dirty-p screen) t))))

;;; ── Paragraph motion (vi { and }) ───────────────────────────────────────────
;;;
;;; Real tmux's copy-mode `{` / `}` jump to the nearest blank line above / below
;;; the cursor (a "paragraph boundary").  A blank line is a viewport row where
;;; every cell is a space character.  If no blank line is found in the direction
;;; of travel, the cursor moves to the top / bottom of the virtual history.

(defun %copy-mode-row-blank-p (screen vrow)
  "Return T if VROW (virtual row, 0 = oldest scrollback) is entirely blank.
   Uses the same virtual-to-display mapping as %extract-vrow-chars."
  (let* ((sb    (screen-scrollback screen))
         (sb-n  (length sb))
         (width (screen-width screen)))
    (loop for col from 0 below width
          for ch = (if (< vrow sb-n)
                       (let ((vec (nth (- sb-n 1 vrow) sb)))
                         (if (and vec (< col (length vec)))
                             (cell-char (aref vec col))
                             #\Space))
                       (cell-char (screen-cell screen col (- vrow sb-n))))
          always (or (char= ch #\Space) (char= ch (code-char 0))))))

(defun %cursor-vrow (screen)
  "Return the virtual row of the current copy-mode cursor."
  (let ((row    (car (screen-copy-cursor screen)))
        (offset (screen-copy-offset screen))
        (sb-n   (length (screen-scrollback screen))))
    (+ sb-n row (- offset))))

(defun %set-cursor-vrow (screen vrow)
  "Move the copy-mode cursor to VROW (virtual row), adjusting the viewport
   offset as needed so the cursor row lands in [0, height-1].  Clamps to the
   valid virtual-row range [0, sb-n+height-1].
   Formula: viewport_row = vrow - sb-n + offset."
  (let* ((sb-n    (length (screen-scrollback screen)))
         (h       (screen-height screen))
         (total   (+ sb-n h))
         (clamped (max 0 (min (1- total) vrow)))
         (offset  (screen-copy-offset screen))
         (col     (cdr (screen-copy-cursor screen)))
         ;; viewport row of CLAMPED at the current offset
         (nat-row (+ clamped (- sb-n) offset)))
    (if (and (>= nat-row 0) (< nat-row h))
        ;; Target is already visible — just move the cursor
        (setf (screen-copy-cursor screen) (cons nat-row col))
        ;; Target is outside the viewport — scroll to center it
        (let* ((desired  (floor h 2))
               ;; new-off satisfies: clamped - sb-n + new-off = desired
               (new-off  (max 0 (min sb-n (+ desired sb-n (- clamped)))))
               ;; actual cursor row with the clamped offset
               (new-row  (max 0 (min (1- h) (+ clamped (- sb-n) new-off)))))
          (setf (screen-copy-offset screen) new-off
                (screen-copy-cursor screen)  (cons new-row col))))
    (setf (screen-dirty-p screen) t)))

(defun copy-mode-previous-paragraph (screen)
  "Jump to the nearest blank-line paragraph boundary above (vi {)."
  (when (screen-copy-mode-p screen)
    (let* ((cur-vrow (%cursor-vrow screen))
           (target   nil))
      ;; Walk upward from cur-vrow - 1, skipping any blanks immediately above,
      ;; then stopping at the first blank line found.
      (loop for vrow downfrom (1- cur-vrow) to 0
            do (when (%copy-mode-row-blank-p screen vrow)
                 (setf target vrow)
                 (return)))
      (%set-cursor-vrow screen (or target 0)))))

(defun copy-mode-next-paragraph (screen)
  "Jump to the nearest blank-line paragraph boundary below (vi })."
  (when (screen-copy-mode-p screen)
    (let* ((sb-n     (length (screen-scrollback screen)))
           (h        (screen-height screen))
           (total    (+ sb-n h))
           (cur-vrow (%cursor-vrow screen))
           (target   nil))
      (loop for vrow from (1+ cur-vrow) below total
            do (when (%copy-mode-row-blank-p screen vrow)
                 (setf target vrow)
                 (return)))
      (%set-cursor-vrow screen (or target (1- total))))))

;;; ── Copy-mode selection: line-select (V) ────────────────────────────────────

(defun copy-mode-begin-line-selection (screen)
  "Begin a full-line selection at the current row (tmux V binding).
   Sets copy-line-selection-p and activates the selection."
  (when (screen-copy-mode-p screen)
    (let* ((cur    (or (screen-copy-cursor screen) (cons 0 0)))
           (row    (car cur))
           ;; Mark at col 0, cursor at col width-1 to select full row.
           (mark   (cons row 0))
           (cursor (cons row (1- (screen-width screen)))))
      (setf (screen-copy-mark             screen) mark
            (screen-copy-mark-offset      screen) (screen-copy-offset screen)
            (screen-copy-cursor           screen) cursor
            (screen-copy-selecting        screen) t
            (screen-copy-line-selection-p screen) t
            (screen-dirty-p               screen) t))))

;;; ── Copy-mode yank variants (D and Y) ───────────────────────────────────────

(defun %copy-row-range-to-paste-buffer (screen row from-col to-col)
  "Extract characters from SCREEN at ROW between FROM-COL (inclusive) and
   TO-COL (exclusive), right-trim trailing spaces, and push to the paste buffer.
   Does nothing when the trimmed result is empty."
  (let ((trimmed (string-right-trim " " (%extract-row-chars screen row from-col to-col))))
    (when (plusp (length trimmed))
      (cl-tmux/buffer:add-paste-buffer trimmed))))

;;; ── Line jump-to-char (vi f/F/t/T) ──────────────────────────────────────────
;;;
;;; f<ch> — jump forward on the current line to the next occurrence of CH.
;;; F<ch> — jump backward on the current line to the previous occurrence.
;;; t<ch> — like f but land one column before the target (till).
;;; T<ch> — like F but land one column after the target (till backward).
;;; ;     — repeat the last jump with the same direction, char, and mode.
;;; ,     — reverse the last jump.
;;;
;;; The last jump is stored in the dynamic variable *copy-mode-last-jump*,
;;; a list (direction char till-p) where direction is :forward or :backward.

(defvar *copy-mode-last-jump* nil
  "Most recent jump-to-char state: (direction char till-p).
   NIL when no jump has been performed yet.")

(defun %copy-mode-jump (screen direction char till-p)
  "Move the cursor on the current line to the nearest CHAR in DIRECTION.
   TILL-P: if T, stop one column before (forward) or after (backward) the match.
   Records the jump in *copy-mode-last-jump* for ; / , repeat.
   Returns T if a match was found."
  (when (screen-copy-mode-p screen)
    (setf *copy-mode-last-jump* (list direction char till-p))
    (let* ((row   (car (screen-copy-cursor screen)))
           (col   (cdr (screen-copy-cursor screen)))
           (chars (%copy-mode-row-chars screen row))
           (w     (length chars)))
      (if (eq direction :forward)
          (loop for c from (1+ col) below w
                when (char= (aref chars c) char)
                  do (setf (cdr (screen-copy-cursor screen))
                           (if till-p (max col (1- c)) c))
                     (return t))
          (loop for c downfrom (1- col) to 0
                when (char= (aref chars c) char)
                  do (setf (cdr (screen-copy-cursor screen))
                           (if till-p (min (1- w) (1+ c)) c))
                     (return t))))))

(defun copy-mode-jump-forward (screen char)
  "Jump to the next occurrence of CHAR on the current line (vi f<char>)."
  (%copy-mode-jump screen :forward char nil))

(defun copy-mode-jump-backward (screen char)
  "Jump to the previous occurrence of CHAR on the current line (vi F<char>)."
  (%copy-mode-jump screen :backward char nil))

(defun copy-mode-jump-to (screen char)
  "Jump to just before the next occurrence of CHAR on the line (vi t<char>)."
  (%copy-mode-jump screen :forward char t))

(defun copy-mode-jump-to-backward (screen char)
  "Jump to just after the previous occurrence of CHAR on the line (vi T<char>)."
  (%copy-mode-jump screen :backward char t))

(defun copy-mode-jump-again (screen)
  "Repeat the last jump-to-char with the same direction, char, and mode (vi ;)."
  (when *copy-mode-last-jump*
    (destructuring-bind (dir ch till) *copy-mode-last-jump*
      ;; jump-again resets the stored jump so it stays at the original params
      (let ((*copy-mode-last-jump* *copy-mode-last-jump*))
        (%copy-mode-jump screen dir ch till)))))

(defun copy-mode-jump-reverse (screen)
  "Reverse the last jump-to-char (vi ,): same char, opposite direction."
  (when *copy-mode-last-jump*
    (destructuring-bind (dir ch till) *copy-mode-last-jump*
      (let ((*copy-mode-last-jump* *copy-mode-last-jump*))
        (%copy-mode-jump screen (if (eq dir :forward) :backward :forward) ch till)))))

(defun copy-mode-goto-line (screen line-number)
  "Jump to LINE-NUMBER (1-based: 1 = oldest scrollback row) in copy mode.
   Out-of-range values are clamped to [1, total-rows]."
  (when (and (screen-copy-mode-p screen)
             (integerp line-number)
             (> line-number 0))
    (%set-cursor-vrow screen (1- line-number))))

(defun copy-mode-copy-end-of-line (screen)
  "Copy from the current cursor column to the end of the line, then exit copy mode."
  (when (screen-copy-mode-p screen)
    (let* ((row (car (screen-copy-cursor screen)))
           (col (cdr (screen-copy-cursor screen)))
           (w   (screen-width screen)))
      (%copy-row-range-to-paste-buffer screen row col w))
    (copy-mode-exit screen)))

(defun copy-mode-copy-line (screen)
  "Copy the full current line (all columns), then exit copy mode."
  (when (screen-copy-mode-p screen)
    (let* ((row (car (screen-copy-cursor screen)))
           (w   (screen-width screen)))
      (%copy-row-range-to-paste-buffer screen row 0 w))
    (copy-mode-exit screen)))
