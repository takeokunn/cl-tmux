(in-package #:cl-tmux/commands)

;;; ── Copy mode ──────────────────────────────────────────────────────────────
;;;
;;; copy_mode(enter, Screen) :- set(copy-mode-p, true), set(copy-offset, 0).
;;; copy_mode(exit, Screen)  :- set(copy-mode-p, false), set(copy-offset, 0).
;;; copy_mode(scroll, Screen, Delta)      :- copy-mode-p(Screen),
;;;                                          new_offset(clamp(offset+Delta, 0, len(scrollback))),
;;;                                          scroll_cursor_into_view(Screen).
;;; copy_mode(move_cursor, Screen, Dir)  :- copy-mode-p(Screen),
;;;                                          move_cursor_one(Screen, Dir),
;;;                                          scroll_to_ensure_visible(Screen).
;;; copy_mode(begin_selection, Screen) :- copy-mode-p(Screen),
;;;                                       set(mark, cursor), set(selecting, true).
;;; copy_mode(cancel, Screen) :- set(mark, nil), set(cursor, nil), set(selecting, false).
;;; copy_mode(yank, Screen)   :- selection_text(Screen, T), add_paste_buffer(T),
;;;                               copy_mode(cancel, Screen), copy_mode(exit, Screen).

(defun copy-mode-enter (screen)
  "Enter copy/scroll mode on SCREEN: freeze the viewport at the live position.
   The copy-mode cursor is placed at the bottom-left of the viewport so that
   the first navigation key moves it naturally upward toward older content."
  (setf (screen-copy-mode-p   screen) t
        (screen-copy-offset    screen) 0
        (screen-copy-mark      screen) nil
        ;; Start cursor at bottom-left of the visible viewport (real tmux behaviour).
        (screen-copy-cursor    screen) (cons (1- (screen-height screen)) 0)
        (screen-copy-selecting screen) nil))

(defun copy-mode-exit (screen)
  "Exit copy mode: resume live PTY output display."
  (setf (screen-copy-mode-p   screen) nil
        (screen-copy-offset    screen) 0
        (screen-copy-mark      screen) nil
        (screen-copy-cursor    screen) nil
        (screen-copy-selecting screen) nil))

(defun %copy-mode-clamp-cursor (screen)
  "Clamp the copy-mode cursor row into [0, height-1] and col into [0, width-1].
   Called after the viewport offset changes so the cursor stays visible."
  (let ((cur (screen-copy-cursor screen)))
    (when cur
      (let ((row (max 0 (min (1- (screen-height screen)) (car cur))))
            (col (max 0 (min (1- (screen-width  screen)) (cdr cur)))))
        (setf (screen-copy-cursor screen) (cons row col))))))

(defun copy-mode-scroll (screen delta)
  "Scroll SCREEN's viewport by DELTA lines (positive = older, negative = newer).
   The copy-mode cursor is clamped to remain within the visible viewport.
   This is the raw viewport-jump path used by Page-Up/Down, mouse wheel, g/G.
   Arrow-key and j/k navigation goes through COPY-MODE-MOVE-CURSOR instead."
  (when (screen-copy-mode-p screen)
    (let ((max-offset (length (screen-scrollback screen))))
      (setf (screen-copy-offset screen)
            (max 0 (min max-offset (+ (screen-copy-offset screen) delta))))
      (%copy-mode-clamp-cursor screen)
      (setf (screen-dirty-p screen) t))))

(defun copy-mode-move-cursor (screen direction)
  "Move SCREEN's copy-mode cursor in DIRECTION (:left :right :up :down).
   Initializes the cursor to bottom-left of the viewport if not yet set.
   For :up/:down the cursor moves one line at a time; when it would leave
   the visible viewport (row < 0 or row >= height) the viewport offset is
   adjusted instead so the cursor stays at the top or bottom edge.
   Marks the screen dirty."
  (when (screen-copy-mode-p screen)
    (let* ((h          (screen-height screen))
           (w          (screen-width  screen))
           (cur        (or (screen-copy-cursor screen) (cons (1- h) 0)))
           (row        (car cur))
           (col        (cdr cur))
           (max-offset (length (screen-scrollback screen))))
      (flet ((%scroll-up-one-line ()
               ;; Three cases: viewport-interior, scroll-and-clamp, already-at-limit.
               (let ((new-row (1- row)))
                 (cond
                   ((>= new-row 0)
                    ;; Cursor still within viewport — move it up.
                    (setf (screen-copy-cursor screen) (cons new-row col)))
                   ((< (screen-copy-offset screen) max-offset)
                    ;; Cursor at top edge — scroll viewport back (older), hold cursor at row 0.
                    (incf (screen-copy-offset screen))
                    (setf (screen-copy-cursor screen) (cons 0 col)))
                   ;; Already at the oldest scrollback line — do not move.
                   (t nil))))
             (%scroll-down-one-line ()
               ;; Three cases: viewport-interior, scroll-and-clamp, already-at-limit.
               (let ((new-row (1+ row)))
                 (cond
                   ((< new-row h)
                    ;; Cursor still within viewport — move it down.
                    (setf (screen-copy-cursor screen) (cons new-row col)))
                   ((> (screen-copy-offset screen) 0)
                    ;; Cursor at bottom edge — scroll viewport forward (newer), hold cursor at h-1.
                    (decf (screen-copy-offset screen))
                    (setf (screen-copy-cursor screen) (cons (1- h) col)))
                   ;; Already at live view bottom — do not move.
                   (t nil)))))
        (ecase direction
          (:left  (setf (screen-copy-cursor screen) (cons row (max 0      (1- col)))))
          (:right (setf (screen-copy-cursor screen) (cons row (min (1- w) (1+ col)))))
          (:up    (%scroll-up-one-line))
          (:down  (%scroll-down-one-line)))
        ;; When selecting, ensure mark is placed if not yet set.
        (when (and (screen-copy-selecting screen) (null (screen-copy-mark screen)))
          (setf (screen-copy-mark screen) (screen-copy-cursor screen)))
        (setf (screen-dirty-p screen) t)))))

(defun copy-mode-begin-selection (screen)
  "Begin a text selection at the current copy-mode cursor position."
  (when (screen-copy-mode-p screen)
    (let ((cur (or (screen-copy-cursor screen) (cons 0 0))))
      (setf (screen-copy-mark      screen) cur
            (screen-copy-cursor    screen) cur
            (screen-copy-selecting screen) t
            (screen-dirty-p        screen) t))))

(defun copy-mode-cancel-selection (screen)
  "Cancel any active copy-mode selection."
  (setf (screen-copy-mark      screen) nil
        (screen-copy-cursor    screen) nil
        (screen-copy-selecting screen) nil
        (screen-dirty-p        screen) t))

;;; %selection-bounds extracts the canonical (start-row end-row start-col end-col)
;;; rectangle from the mark and cursor positions — independent of which end the
;;; user anchored first.  %selection-text builds the string from that rectangle.
;;; Both are private (percent-prefixed) and independently testable.

(defun %selection-bounds (screen)
  "Return (values start-r end-r start-c end-c) for the current copy-mode
   selection in SCREEN, normalising mark and cursor order.
   Assumes mark and cursor are already set."
  (let* ((mark   (screen-copy-mark   screen))
         (cursor (screen-copy-cursor screen))
         (mr (car mark))   (mc (cdr mark))
         (cr (car cursor)) (cc (cdr cursor))
         (start-r (min mr cr))
         (end-r   (max mr cr))
         (start-c (if (< mr cr) mc (if (> mr cr) cc (min mc cc))))
         (end-c   (if (< mr cr) cc (if (> mr cr) mc (max mc cc)))))
    (values start-r end-r start-c end-c)))

(defun %selection-text (screen)
  "Compute the text selected by copy-mode in SCREEN.
   Returns a string, or NIL when no valid selection exists.
   Intermediate rows (not the last) are right-trimmed of trailing spaces."
  (unless (and (screen-copy-selecting screen)
               (screen-copy-mark   screen)
               (screen-copy-cursor screen))
    (return-from %selection-text nil))
  (multiple-value-bind (start-r end-r start-c end-c)
      (%selection-bounds screen)
    (let* ((w    (screen-width screen))
           (text (with-output-to-string (out)
                   (loop for row from start-r to end-r do
                     (let ((c0 (if (= row start-r) start-c 0))
                           (c1 (if (= row end-r)   end-c   w)))
                       (let ((row-str (with-output-to-string (rs)
                                        (loop for col from c0 below c1 do
                                          (write-char (cell-char (screen-cell screen col row)) rs)))))
                         ;; Trim trailing spaces from intermediate rows.
                         (write-string (if (< row end-r)
                                           (string-right-trim " " row-str)
                                           row-str)
                                       out))
                       (when (< row end-r) (write-char #\Newline out)))))))
      (if (plusp (length text)) text nil))))

(defun copy-mode-yank (screen)
  "Copy selected text to paste buffer and exit copy mode."
  (let ((text (%selection-text screen)))
    (when (and text (plusp (length text)))
      (cl-tmux/buffer:add-paste-buffer text)))
  (copy-mode-cancel-selection screen)
  (copy-mode-exit screen))

;;; ── Copy-mode navigation (word / line / screen jumps) ───────────────────────
;;;
;;; copy_mode_word_forward(Screen)  :- skip_spaces_right, advance to next non-space.
;;; copy_mode_word_backward(Screen) :- skip_spaces_left, retreat to prev word start.
;;; copy_mode_word_end(Screen)      :- advance to last char of current/next word.
;;; copy_mode_line_start(Screen)    :- set cursor-x = 0.
;;; copy_mode_line_end(Screen)      :- set cursor-x = width-1.
;;; copy_mode_top(Screen)           :- jump to top of scrollback.
;;; copy_mode_bottom(Screen)        :- jump to live view bottom.
;;; copy_mode_high/middle/low(Screen) :- cursor to row 0 / mid / last.
;;; copy_mode_page_up/down(Screen)  :- scroll by screen-height lines.
;;; copy_mode_half_page_up/down     :- scroll by floor(screen-height/2) lines.
;;; copy_mode_scroll_up/down_line   :- scroll 1 line keeping cursor fixed if possible.

(defun %copy-mode-row-cells (screen row)
  "Return a list of characters on ROW of SCREEN in viewport projection.
   Uses the scrollback offset so word navigation works correctly in copy mode."
  (loop for col from 0 below (screen-width screen)
        collect (cell-char (screen-display-cell screen col row))))

(defun copy-mode-word-forward (screen)
  "Move cursor forward to the start of the next word (non-space run).
   A word is any run of non-space characters.  Space is #\\Space."
  (when (screen-copy-mode-p screen)
    (let* ((row  (car (screen-copy-cursor screen)))
           (col  (cdr (screen-copy-cursor screen)))
           (w    (screen-width screen))
           (chars (%copy-mode-row-cells screen row))
           (new-col col))
      ;; Step over the current word
      (loop while (and (< new-col w)
                       (char/= (nth new-col chars) #\Space))
            do (incf new-col))
      ;; Step over spaces
      (loop while (and (< new-col w)
                       (char= (nth new-col chars) #\Space))
            do (incf new-col))
      (setf (screen-copy-cursor screen) (cons row (min (1- w) new-col)))
      (setf (screen-dirty-p screen) t))))

(defun copy-mode-word-backward (screen)
  "Move cursor backward to the start of the previous or current word."
  (when (screen-copy-mode-p screen)
    (let* ((row  (car (screen-copy-cursor screen)))
           (col  (cdr (screen-copy-cursor screen)))
           (chars (%copy-mode-row-cells screen row))
           (new-col col))
      ;; Step back over spaces
      (loop while (and (> new-col 0)
                       (char= (nth (1- new-col) chars) #\Space))
            do (decf new-col))
      ;; Step back over word characters
      (loop while (and (> new-col 0)
                       (char/= (nth (1- new-col) chars) #\Space))
            do (decf new-col))
      (setf (screen-copy-cursor screen) (cons row (max 0 new-col)))
      (setf (screen-dirty-p screen) t))))

(defun copy-mode-word-end (screen)
  "Move cursor to the last character of the current or next word."
  (when (screen-copy-mode-p screen)
    (let* ((row  (car (screen-copy-cursor screen)))
           (col  (cdr (screen-copy-cursor screen)))
           (w    (screen-width screen))
           (chars (%copy-mode-row-cells screen row))
           (new-col col))
      ;; If already at end of a word, skip one space to enter the next word
      (when (and (< new-col (1- w))
                 (char/= (nth new-col chars) #\Space)
                 (char= (nth (1+ new-col) chars) #\Space))
        (incf new-col))
      ;; Skip over spaces
      (loop while (and (< new-col (1- w))
                       (char= (nth new-col chars) #\Space))
            do (incf new-col))
      ;; Advance to end of word
      (loop while (and (< new-col (1- w))
                       (char/= (nth (1+ new-col) chars) #\Space))
            do (incf new-col))
      (setf (screen-copy-cursor screen) (cons row (min (1- w) new-col)))
      (setf (screen-dirty-p screen) t))))

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
   most-positive-fixnum)
  (copy-mode-bottom
   "Jump to the live view bottom (scroll-offset = 0)."
   (- most-positive-fixnum)))

;;; ── Copy-mode selection: line-select (V) ────────────────────────────────────

(defun copy-mode-begin-line-selection (screen)
  "Begin a full-line selection at the current row (tmux V binding).
   Sets copy-line-selection-p and activates the selection."
  (when (screen-copy-mode-p screen)
    (let* ((cur (or (screen-copy-cursor screen) (cons 0 0)))
           (row (car cur))
           ;; Mark at col 0, cursor at col width-1 to select full row
           (mark   (cons row 0))
           (cursor (cons row (1- (screen-width screen)))))
      (setf (screen-copy-mark           screen) mark
            (screen-copy-cursor         screen) cursor
            (screen-copy-selecting      screen) t
            (screen-copy-line-selection-p screen) t
            (screen-dirty-p             screen) t))))

;;; ── Copy-mode yank variants (D and Y) ───────────────────────────────────────

(defun %copy-row-range-to-paste-buffer (screen row from-col to-col)
  "Extract characters from SCREEN at ROW between FROM-COL (inclusive) and
   TO-COL (exclusive), right-trim trailing spaces, and push to the paste buffer.
   Does nothing when the trimmed result is empty.
   Separates data extraction from the dispatch logic in the D and Y commands."
  (let* ((text (with-output-to-string (out)
                 (loop for col from from-col below to-col do
                   (write-char (cell-char (screen-cell screen col row)) out))))
         (trimmed (string-right-trim " " text)))
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

;;; ── Copy-mode search ────────────────────────────────────────────────────────
;;;
;;; copy_mode_search_forward(Screen, Term) :- scan rows from cursor downward.
;;; copy_mode_search_backward(Screen, Term) :- scan rows from cursor upward.
;;; copy_mode_search_next(Screen) :- repeat last search forward.
;;; copy_mode_search_prev(Screen) :- repeat last search backward.

(defun %copy-mode-row-string (screen row)
  "Return the string content of ROW in the visible viewport."
  (with-output-to-string (out)
    (loop for col from 0 below (screen-width screen) do
      (write-char (cell-char (screen-display-cell screen col row)) out))))

(defun %copy-mode-find-forward (screen term start-row start-col)
  "Scan forward from START-ROW/START-COL for TERM.
   Returns (values row col) of the first match, or (values nil nil)."
  (let ((h (screen-height screen)))
    (loop for row from start-row below h do
      (let* ((row-str  (%copy-mode-row-string screen row))
             (from-col (if (= row start-row) start-col 0))
             (pos      (search term row-str :start2 from-col)))
        (when pos
          (return-from %copy-mode-find-forward (values row pos)))))
    (values nil nil)))

(defun %copy-mode-find-backward (screen term start-row start-col)
  "Scan backward from START-ROW/START-COL for TERM.
   Returns (values row col) of the last match before cursor, or (values nil nil)."
  (loop for row from start-row downto 0 do
    (let* ((row-str  (%copy-mode-row-string screen row))
           (end-col  (if (= row start-row) start-col (length row-str)))
           (pos      (and (> end-col 0)
                          (loop for i from (1- end-col) downto 0
                                when (and (<= (+ i (length term)) (length row-str))
                                          (string= term (subseq row-str i (+ i (length term)))))
                                  return i))))
      (when pos
        (return-from %copy-mode-find-backward (values row pos)))))
  (values nil nil))

(defun copy-mode-search-forward (screen term)
  "Search forward from the current cursor for TERM.
   Saves TERM for n/N repeats.  Moves cursor to the first match."
  (when (and (screen-copy-mode-p screen) term (plusp (length term)))
    (setf (screen-copy-search-term screen) term)
    (let* ((cur (or (screen-copy-cursor screen) (cons 0 0)))
           (row (car cur))
           (col (1+ (cdr cur))))   ; start one past current to advance
      (multiple-value-bind (found-row found-col)
          (%copy-mode-find-forward screen term row col)
        (when found-row
          (setf (screen-copy-cursor screen) (cons found-row found-col)
                (screen-dirty-p screen) t))))))

(defun copy-mode-search-backward (screen term)
  "Search backward from the current cursor for TERM.
   Saves TERM for n/N repeats.  Moves cursor to the first match going back."
  (when (and (screen-copy-mode-p screen) term (plusp (length term)))
    (setf (screen-copy-search-term screen) term)
    (let* ((cur (or (screen-copy-cursor screen) (cons 0 0)))
           (row (car cur))
           (col (cdr cur)))
      (multiple-value-bind (found-row found-col)
          (%copy-mode-find-backward screen term row col)
        (when found-row
          (setf (screen-copy-cursor screen) (cons found-row found-col)
                (screen-dirty-p screen) t))))))

(defun copy-mode-search-next (screen)
  "Repeat the last search in the forward direction."
  (when (screen-copy-mode-p screen)
    (let ((term (screen-copy-search-term screen)))
      (when term
        (copy-mode-search-forward screen term)))))

(defun copy-mode-search-prev (screen)
  "Repeat the last search in the backward direction."
  (when (screen-copy-mode-p screen)
    (let ((term (screen-copy-search-term screen)))
      (when term
        (copy-mode-search-backward screen term)))))
