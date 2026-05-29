(in-package #:cl-tmux/terminal/actions)

;;;; Pure terminal action functions — cursor movement, erase, scroll, and
;;;; character writing.  No parser logic or CSI/SGR dispatch lives here.

;;; ── Cursor movement ────────────────────────────────────────────────────────

(defun set-cursor (screen x y)
  "Move the cursor to (X, Y), clamping both coordinates into bounds."
  (setf (screen-cx screen) (clamp x 0 (1- (screen-width  screen)))
        (screen-cy screen) (clamp y 0 (1- (screen-height screen)))))

(defun cursor-up (screen n)
  "Move the cursor up N rows, clamping to the scroll-top boundary."
  (setf (screen-cy screen)
        (max (screen-scroll-top screen)
             (- (screen-cy screen) n))))

(defun cursor-down (screen n)
  "Move the cursor down N rows, clamping to the scroll-bottom boundary."
  (setf (screen-cy screen)
        (min (screen-scroll-bottom screen)
             (+ (screen-cy screen) n))))

(defun cursor-right (screen n)
  "Move the cursor right N columns, clamping to width-1."
  (setf (screen-cx screen)
        (min (1- (screen-width screen))
             (+ (screen-cx screen) n))))

(defun cursor-left (screen n)
  "Move the cursor left N columns, clamping to column 0."
  (setf (screen-cx screen)
        (max 0 (- (screen-cx screen) n))))

(defun scroll-up-one (screen)
  "Scroll the scroll region up one line; new bottom line is blank.
   A copy of the displaced top row is pushed onto the scrollback buffer.
   The scrollback is capped at 1000 entries."
  (let ((top    (screen-scroll-top    screen))
        (bottom (screen-scroll-bottom screen))
        (w      (screen-width         screen)))
    ;; Save the top row of the scroll region into the scrollback buffer.
    (let ((saved-row (make-array w)))
      (dotimes (col w)
        (setf (aref saved-row col) (screen-cell screen col top)))
      (push saved-row (screen-scrollback screen))
      (when (> (length (screen-scrollback screen)) 1000)
        (setf (screen-scrollback screen)
              (butlast (screen-scrollback screen)))))
    ;; Shift rows up within the scroll region.
    (loop for row from top below bottom
          do (loop for col below w
                   do (setf (screen-cell screen col row)
                            (screen-cell screen col (1+ row)))))
    ;; Clear the newly exposed bottom row.
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

(defun cursor-down/scroll (screen)
  "Move cursor down one line, scrolling when at the bottom of the scroll region.
   This is the internal helper used by write-char-at-cursor and the LF handler."
  (if (< (screen-cy screen) (screen-scroll-bottom screen))
      (incf (screen-cy screen))
      (scroll-up-one screen)))

(defun cursor-lf (screen)
  "Line feed: move cursor down, scrolling the scroll region if at the bottom."
  (cursor-down/scroll screen))

(defun cursor-ht (screen)
  "Horizontal tab: advance cursor to the next 8-column tab stop."
  (let ((nx (* 8 (ceiling (1+ (screen-cx screen)) 8))))
    (setf (screen-cx screen)
          (min nx (1- (screen-width screen))))))

(defun cursor-bs (screen)
  "Backspace: move cursor left one column if not already at column 0."
  (when (> (screen-cx screen) 0)
    (decf (screen-cx screen))))

(defun cursor-ri (screen)
  "Reverse index (ESC M): move cursor up one line, scrolling down if at top."
  (if (= (screen-cy screen) (screen-scroll-top screen))
      (scroll-down-one screen)
      (decf (screen-cy screen))))

(defun cursor-cr (screen)
  "Carriage return: move the cursor to column 0."
  (setf (screen-cx screen) 0))

;;; ── Character writing ──────────────────────────────────────────────────────

(defun %advance-cursor (screen n)
  "Advance the cursor N columns, wrapping to the next line (and scrolling the
   scroll region if needed) when it would pass the right edge."
  (let ((nx (+ (screen-cx screen) n)))
    (cond
      ((< nx (screen-width screen)) (setf (screen-cx screen) nx))
      (t (setf (screen-cx screen) 0)
         (cursor-down/scroll screen)))))

(defun write-char-at-cursor (screen ch)
  "Write CH at the cursor, then advance.  Double-width (CJK) characters occupy
   a lead cell plus a continuation placeholder and advance the cursor by two;
   a wide char that will not fit at the right edge wraps to the next line first."
  (let ((w  (char-width ch))
        (fg (screen-cur-fg    screen))
        (bg (screen-cur-bg    screen))
        (at (screen-cur-attrs screen)))
    ;; A double-width char straddling the right edge wraps, leaving the final
    ;; column blank.
    (when (and (= w 2) (>= (1+ (screen-cx screen)) (screen-width screen)))
      (setf (screen-cell screen (screen-cx screen) (screen-cy screen)) (blank-cell))
      (setf (screen-cx screen) 0)
      (cursor-down/scroll screen))
    (let ((x (screen-cx screen))
          (y (screen-cy screen)))
      (setf (screen-cell screen x y)
            (make-cell :char ch :fg fg :bg bg :attrs at :width w))
      ;; Continuation placeholder for the wide char's second column.
      (when (and (= w 2) (< (1+ x) (screen-width screen)))
        (setf (screen-cell screen (1+ x) y)
              (make-cell :char #\Space :fg fg :bg bg :attrs at :width 0)))
      (%advance-cursor screen w))))

(defun write-codepoint (screen cp)
  "Write Unicode code point CP at the cursor, converting it via SAFE-CODE-CHAR."
  (write-char-at-cursor screen (safe-code-char cp)))

;;; ── Erase ──────────────────────────────────────────────────────────────────

(defun erase-region (screen x0 y0 x1 y1)
  "Erase all cells from (X0,Y0) to (X1,Y1) inclusive, treating the range as
   a linear span across rows."
  (loop for y from y0 to y1
        do (let ((bx (if (= y y0) x0 0))
                 (ex (if (= y y1) x1 (1- (screen-width screen)))))
             (loop for x from bx to ex
                   do (setf (screen-cell screen x y) (blank-cell))))))

(defun erase-display (screen mode)
  "Erase part or all of the display.
   MODE 0: from cursor to end of screen.
   MODE 1: from start of screen to cursor (inclusive).
   MODE 2: entire screen.
   MODE 3: entire screen + scrollback (treated same as 2 here)."
  (let ((cx (screen-cx screen)) (cy (screen-cy screen))
        (w  (screen-width  screen))
        (h  (screen-height screen)))
    (case mode
      (0 (erase-region screen cx cy (1- w) cy)
         (when (< (1+ cy) h)
           (erase-region screen 0 (1+ cy) (1- w) (1- h))))
      (1 (when (> cy 0)
           (erase-region screen 0 0 (1- w) (1- cy)))
         (erase-region screen 0 cy cx cy))
      ((2 3)
       (erase-region screen 0 0 (1- w) (1- h))
       (when (= mode 3)
         (setf (screen-scrollback screen) nil))))))

(defun erase-line (screen mode)
  "Erase part or all of the current line.
   MODE 0: from cursor to end of line.
   MODE 1: from start of line to cursor (inclusive).
   MODE 2: entire line."
  (let ((cx (screen-cx screen)) (cy (screen-cy screen))
        (w  (screen-width  screen)))
    (case mode
      (0 (erase-region screen cx  cy (1- w) cy))
      (1 (erase-region screen 0   cy cx     cy))
      (2 (erase-region screen 0   cy (1- w) cy)))))

;;; ── Delete / insert characters ─────────────────────────────────────────────

(defun delete-chars (screen n)
  "DCH — delete N characters at the cursor, shifting remaining chars left.
   The vacated cells at the end of the line are filled with blanks."
  (let ((cx (screen-cx screen))
        (cy (screen-cy screen))
        (w  (screen-width screen)))
    (loop for x from cx to (- w n 1)
          do (setf (screen-cell screen x cy)
                   (screen-cell screen (+ x n) cy)))
    (loop for x from (max cx (- w n)) to (1- w)
          do (setf (screen-cell screen x cy) (blank-cell)))))

(defun insert-chars (screen n)
  "ICH — insert N blank characters at the cursor, pushing existing chars right.
   Characters shifted past the right margin are lost."
  (let ((cx (screen-cx screen))
        (cy (screen-cy screen))
        (w  (screen-width screen)))
    (loop for x from (1- w) downto (+ cx n)
          do (setf (screen-cell screen x cy)
                   (screen-cell screen (- x n) cy)))
    (loop for x from cx to (min (1- w) (+ cx n -1))
          do (setf (screen-cell screen x cy) (blank-cell)))))

;;; ── Insert / delete lines (cursor-relative, within the scroll region) ───────

(defun insert-lines (screen n)
  "IL — insert N blank lines at the cursor row, pushing lower lines down within
   [cursor-row, scroll-bottom].  Lines pushed past the bottom are discarded."
  (let* ((top    (screen-cy screen))
         (bottom (screen-scroll-bottom screen))
         (w      (screen-width screen)))
    (when (<= top bottom)
      (let ((count (min n (- bottom top -1))))
        (loop for row from bottom downto (+ top count)
              do (loop for col below w
                       do (setf (screen-cell screen col row)
                                (screen-cell screen col (- row count)))))
        (loop for row from top to (+ top count -1)
              do (loop for col below w
                       do (setf (screen-cell screen col row) (blank-cell))))))))

(defun delete-lines (screen n)
  "DL — delete N lines at the cursor row, pulling lower lines up within
   [cursor-row, scroll-bottom].  Lines exposed at the bottom become blank."
  (let* ((top    (screen-cy screen))
         (bottom (screen-scroll-bottom screen))
         (w      (screen-width screen)))
    (when (<= top bottom)
      (let ((count (min n (- bottom top -1))))
        (loop for row from top to (- bottom count)
              do (loop for col below w
                       do (setf (screen-cell screen col row)
                                (screen-cell screen col (+ row count)))))
        (loop for row from (+ (- bottom count) 1) to bottom
              do (loop for col below w
                       do (setf (screen-cell screen col row) (blank-cell))))))))

;;; ── Scroll region ──────────────────────────────────────────────────────────

(defun decstbm (screen top bottom)
  "DECSTBM — set the vertical scroll region.
   TOP and BOTTOM are 0-based inclusive row indices.  The cursor is homed
   to (0,0) after a valid set."
  (let ((clamped-top    (max 0 top))
        (clamped-bottom (min (1- (screen-height screen)) bottom)))
    (when (< clamped-top clamped-bottom)
      (setf (screen-scroll-top    screen) clamped-top
            (screen-scroll-bottom screen) clamped-bottom)
      (set-cursor screen 0 0))))

;;; ── DEC private mode (alternate screen) ───────────────────────────────────

(defun dec-pm-set (screen params)
  "Handle DEC private mode set sequences (?XXXh).
   Param 1049 enters the alternate screen: the current cell grid and cursor
   are saved, and the screen is replaced with a fresh blank grid.  A repeated
   ?1049h is a no-op so the saved primary screen is never clobbered."
  (dolist (param params)
    (case param
      (1049
       (unless (screen-alt-cells screen)
         ;; Save current grid and cursor.
         (setf (screen-alt-cells screen) (copy-seq (screen-cells screen))
               (screen-alt-cx    screen) (screen-cx screen)
               (screen-alt-cy    screen) (screen-cy screen))
         ;; Replace with a fresh blank grid and home the cursor.
         (let* ((w (screen-width  screen))
                (h (screen-height screen))
                (n (* w h))
                (new-cells (make-array n :initial-element nil)))
           (dotimes (i n) (setf (aref new-cells i) (blank-cell)))
           (setf (screen-cells screen) new-cells))
         (set-cursor screen 0 0)))
      ;; Other DEC modes are accepted silently.
      (otherwise nil))))

(defun dec-pm-reset (screen params)
  "Handle DEC private mode reset sequences (?XXXl).
   Param 1049 exits the alternate screen: restores the previously saved
   cell grid and cursor.  If no saved grid exists, clears the screen."
  (dolist (param params)
    (case param
      (1049
       (if (screen-alt-cells screen)
           ;; Restore the saved normal-screen grid and cursor.
           (setf (screen-cells screen) (screen-alt-cells screen)
                 (screen-cx    screen) (screen-alt-cx    screen)
                 (screen-cy    screen) (screen-alt-cy    screen)
                 (screen-alt-cells screen) nil)
           ;; No saved grid: fall back to clearing the current screen.
           (erase-display screen 2))
       (setf (screen-dirty-p screen) t))
      (otherwise nil))))

;;; ── DECSC / DECRC (cursor save & restore) ──────────────────────────────────

(defun save-cursor (screen)
  "DECSC (ESC 7): save the cursor position and current SGR state."
  (setf (screen-saved-cursor screen)
        (list (screen-cx screen) (screen-cy screen)
              (screen-cur-fg screen) (screen-cur-bg screen)
              (screen-cur-attrs screen))))

(defun restore-cursor (screen)
  "DECRC (ESC 8): restore the cursor position and SGR state saved by DECSC.
   With nothing previously saved, home the cursor and reset SGR (VT100 default)."
  (let ((saved (screen-saved-cursor screen)))
    (if saved
        (destructuring-bind (cx cy fg bg attrs) saved
          (set-cursor screen cx cy)
          (setf (screen-cur-fg    screen) fg
                (screen-cur-bg    screen) bg
                (screen-cur-attrs screen) attrs))
        (progn
          (set-cursor screen 0 0)
          (setf (screen-cur-fg screen) 7
                (screen-cur-bg screen) 0
                (screen-cur-attrs screen) 0)))))

;;; ── Full reset ─────────────────────────────────────────────────────────────

(defun ris-action (screen)
  "RIS — ESC c: hard terminal reset.
   Clears the entire cell grid, homes the cursor, resets all SGR attributes,
   and restores the scroll region to the full screen height."
  (erase-region screen 0 0
                (1- (screen-width  screen))
                (1- (screen-height screen)))
  (set-cursor screen 0 0)
  (setf (screen-cur-fg    screen) 7
        (screen-cur-bg    screen) 0
        (screen-cur-attrs screen) 0
        (screen-scroll-top    screen) 0
        (screen-scroll-bottom screen) (1- (screen-height screen))))

;;; ── Display projection (copy-mode scrollback) ──────────────────────────────

(defparameter *display-blank-cell* (blank-cell)
  "Shared immutable blank cell returned for out-of-range display lookups.
   Cells are never mutated in place (only replaced via (setf screen-cell)),
   so sharing one instance for read-only miss results is safe and avoids
   per-cell allocation during copy-mode scrollback redraws.")

(defun screen-display-cell (screen col row)
  "Cell shown at viewport position (COL, ROW) for the current scroll state.

   With copy mode off or copy-offset 0 this is simply the live grid cell.
   When scrolled back by N = copy-offset lines, the viewport shifts up into
   history: the top N rows are filled from the scrollback buffer (which stores
   displaced rows newest-first) and the live grid is pushed down by N rows.

   Scrollback rows saved at a different width are clamped/padded with blanks, so
   the projection is always safe to index at any (COL, ROW) within the pane."
  (let ((offset (if (screen-copy-mode-p screen) (screen-copy-offset screen) 0)))
    (cond
      ;; Top OFFSET rows come from history: display row R ↦ scrollback[OFFSET-1-R].
      ((< row offset)
       (let* ((vec (nth (- offset 1 row) (screen-scrollback screen))))
         (if (and vec (< col (length vec)))
             (aref vec col)
             *display-blank-cell*)))
      ;; Remaining rows come from the live grid, shifted down by OFFSET.
      (t
       (let ((live-row (- row offset)))
         (if (< live-row (screen-height screen))
             (screen-cell screen col live-row)
             *display-blank-cell*))))))
