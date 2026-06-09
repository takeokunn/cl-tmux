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

;;; ── Declarative word-navigation macro table ─────────────────────────────────
;;;
;;; define-copy-mode-word-command: Prolog-style facts table for word-motion
;;; commands.  All three share the same outer structure:
;;;   (when copy-mode-p) guard → let* decomposing cursor → scan body → setf cursor dirty-p.
;;; Only the scan body (the inner loop sequence) differs between commands.
;;;
;;;   (define-copy-mode-word-command (name docstring clamp-expr &rest scan-body) ...)
;;;
;;; CLAMP-EXPR wraps new-col in the final setf (e.g. (min (1- width) new-col) or
;;; (max 0 new-col)).  SCAN-BODY is a sequence of forms executed between the
;;; initial let* binding and the final setf.

(defmacro define-copy-mode-word-command (&rest rules)
  "Generate one copy-mode word-navigation defun per rule.
   Each RULE is (function-name docstring clamp-expr scan-body...).
   CLAMP-EXPR is the column expression for the final cursor position;
   SCAN-BODY are the loop forms that advance/retreat new-col.
   The generated function guards on copy-mode-p and marks the screen dirty."
  `(progn
     ,@(mapcar (lambda (rule)
                 (destructuring-bind (name docstring clamp-expr &rest scan-body) rule
                   `(defun ,name (screen)
                      ,docstring
                      (when (screen-copy-mode-p screen)
                        (let* ((row     (car (screen-copy-cursor screen)))
                               (col     (cdr (screen-copy-cursor screen)))
                               (width   (screen-width screen))
                               (chars   (%copy-mode-row-chars screen row))
                               (new-col col))
                          (declare (ignorable width))
                          ,@scan-body
                          (setf (screen-copy-cursor screen) (cons row ,clamp-expr)
                                (screen-dirty-p screen) t))))))
               rules)))

(define-copy-mode-word-command
  (copy-mode-word-forward
   "Move cursor forward to the start of the next word.
    Word boundaries are defined by the 'word-separators' option (default: space, -, _, @)."
   (min (1- width) new-col)
   ;; Step over the current word characters (non-separator run).
   (loop while (and (< new-col width)
                    (not (%word-separator-p (aref chars new-col))))
         do (incf new-col))
   ;; Step over the trailing separators to reach the next word start.
   (loop while (and (< new-col width)
                    (%word-separator-p (aref chars new-col)))
         do (incf new-col)))

  (copy-mode-word-backward
   "Move cursor backward to the start of the previous or current word."
   (max 0 new-col)
   ;; Step back over any leading separators.
   (loop while (and (> new-col 0)
                    (%word-separator-p (aref chars (1- new-col))))
         do (decf new-col))
   ;; Step back over word characters to reach the word start.
   (loop while (and (> new-col 0)
                    (not (%word-separator-p (aref chars (1- new-col)))))
         do (decf new-col)))

  (copy-mode-word-end
   "Move cursor to the last character of the current or next word."
   (min (1- width) new-col)
   ;; If already at end of a word, cross the boundary into the next word.
   (when (and (< new-col (1- width))
              (not (%word-separator-p (aref chars new-col)))
              (%word-separator-p (aref chars (1+ new-col))))
     (incf new-col))
   ;; Skip over separators to reach the next word.
   (loop while (and (< new-col (1- width))
                    (%word-separator-p (aref chars new-col)))
         do (incf new-col))
   ;; Advance to the last character of the word.
   (loop while (and (< new-col (1- width))
                    (not (%word-separator-p (aref chars (1+ new-col)))))
         do (incf new-col))))

;;; WORD motion (vi W/B/E): identical scans but blank-delimited (%space-separator-p)
;;; rather than word-separators — a WORD spans punctuation, stopping only at spaces.
(define-copy-mode-word-command
  (copy-mode-space-forward
   "Move cursor forward to the start of the next WORD (whitespace-delimited; vi W)."
   (min (1- width) new-col)
   (loop while (and (< new-col width)
                    (not (%space-separator-p (aref chars new-col))))
         do (incf new-col))
   (loop while (and (< new-col width)
                    (%space-separator-p (aref chars new-col)))
         do (incf new-col)))

  (copy-mode-space-backward
   "Move cursor backward to the start of the previous/current WORD (vi B)."
   (max 0 new-col)
   (loop while (and (> new-col 0)
                    (%space-separator-p (aref chars (1- new-col))))
         do (decf new-col))
   (loop while (and (> new-col 0)
                    (not (%space-separator-p (aref chars (1- new-col)))))
         do (decf new-col)))

  (copy-mode-space-end
   "Move cursor to the last character of the current/next WORD (vi E)."
   (min (1- width) new-col)
   (when (and (< new-col (1- width))
              (not (%space-separator-p (aref chars new-col)))
              (%space-separator-p (aref chars (1+ new-col))))
     (incf new-col))
   (loop while (and (< new-col (1- width))
                    (%space-separator-p (aref chars new-col)))
         do (incf new-col))
   (loop while (and (< new-col (1- width))
                    (not (%space-separator-p (aref chars (1+ new-col)))))
         do (incf new-col))))

;;; ── Line start / end ─────────────────────────────────────────────────────────

(defun copy-mode-line-start (screen)
  "Move cursor to column 0 of the current row."
  (when (screen-copy-mode-p screen)
    (let ((row (car (screen-copy-cursor screen))))
      (setf (screen-copy-cursor screen) (cons row 0)
            (screen-dirty-p screen) t))))

(defun copy-mode-line-end (screen)
  "Move cursor to the last column of the current row."
  (when (screen-copy-mode-p screen)
    (let ((row (car (screen-copy-cursor screen))))
      (setf (screen-copy-cursor screen) (cons row (1- (screen-width screen)))
            (screen-dirty-p screen) t))))

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
