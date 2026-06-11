(in-package #:cl-tmux/commands)

;;; Named scroll-extent sentinels — used by copy-mode-top and copy-mode-bottom.
(defconstant +scroll-to-oldest+ most-positive-fixnum
  "Sentinel delta that clamps to the maximum scrollback offset (oldest content).")
(defconstant +scroll-to-newest+ (- most-positive-fixnum)
  "Sentinel delta that clamps to offset 0 (live view / newest content).")

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

(defun copy-mode-enter (screen &key scroll-to-top exit-on-bottom)
  "Enter copy/scroll mode on SCREEN: freeze the viewport at the live position.
   The copy-mode cursor is placed at the bottom-left of the viewport so that
   the first navigation key moves it naturally upward toward older content.
   SCROLL-TO-TOP T pre-scrolls to the oldest scrollback content (copy-mode -u).
   EXIT-ON-BOTTOM T (copy-mode -e) auto-exits copy mode when the viewport is
   scrolled back down to the live bottom (offset 0)."
  (setf (screen-copy-mode-p        screen) t
        (screen-copy-mark          screen) nil
        (screen-copy-mark-offset   screen) 0
        (screen-copy-selecting     screen) nil
        (screen-copy-exit-on-bottom screen) (and exit-on-bottom t))
  (if scroll-to-top
      ;; copy-mode -u: scroll to oldest content (max offset), cursor at top-left.
      (let ((max-offset (length (screen-scrollback screen))))
        (setf (screen-copy-offset screen) max-offset
              (screen-copy-cursor screen) (cons 0 0)))
      ;; Normal entry: live view, cursor at bottom-left.
      (setf (screen-copy-offset screen) 0
            (screen-copy-cursor screen) (cons (1- (screen-height screen)) 0))))

(defun copy-mode-exit (screen)
  "Exit copy mode: resume live PTY output display."
  (setf (screen-copy-mode-p        screen) nil
        (screen-copy-offset         screen) 0
        (screen-copy-mark           screen) nil
        (screen-copy-mark-offset    screen) 0
        (screen-copy-cursor         screen) nil
        (screen-copy-selecting      screen) nil
        (screen-copy-line-selection-p screen) nil
        (screen-copy-rect-select-p  screen) nil
        (screen-copy-exit-on-bottom screen) nil))

(defun %copy-mode-clamp-cursor (screen)
  "Clamp the copy-mode cursor row into [0, height-1] and col into [0, width-1].
   Called after the viewport offset changes so the cursor stays visible.
   Operates on the cursor cons directly; no-op when cursor is NIL."
  (let ((cursor (screen-copy-cursor screen)))
    (when cursor
      (let ((row (max 0 (min (1- (screen-height screen)) (car cursor))))
            (col (max 0 (min (1- (screen-width  screen)) (cdr cursor)))))
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
      (setf (screen-dirty-p screen) t)
      ;; copy-mode -e: auto-exit when scrolled back down to the live bottom.
      ;; Only triggers on a downward (newer) scroll that reaches offset 0.
      (when (and (screen-copy-exit-on-bottom screen)
                 (< delta 0)
                 (zerop (screen-copy-offset screen)))
        (copy-mode-exit screen)))))

;;; These top-level helpers are called from copy-mode-move-cursor for :up / :down.
;;; Keeping them at top level eliminates the flet nesting and makes each path
;;; independently readable.

(defun %scroll-up-one-line (screen row col max-offset)
  "Move cursor up one line in copy-mode viewport for SCREEN.
   ROW/COL is the current cursor position; MAX-OFFSET is the scrollback length.
   When the cursor is at row 0 and scrollback remains, scrolls the viewport
   back one line, keeping the cursor pinned at row 0.
   At the oldest scrollback line the call is a no-op."
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

(defun %scroll-down-one-line (screen row col h)
  "Move cursor down one line in copy-mode viewport for SCREEN.
   ROW/COL is the current cursor position; H is the viewport height.
   When the cursor is at row H-1 and offset > 0, scrolls the viewport
   forward one line, keeping the cursor pinned at row H-1.
   At the live view bottom the call is a no-op."
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
      (t nil))))

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
      (ecase direction
        (:left  (setf (screen-copy-cursor screen) (cons row (max 0      (1- col)))))
        ;; While selecting, the cursor is the EXCLUSIVE selection end, so it may
        ;; advance to W (one past the last column) to let the selection include
        ;; the rightmost cell — matching select-word.  Plain navigation caps at
        ;; W-1 (the last visible column).
        (:right (let ((right-bound (if (screen-copy-selecting screen) w (1- w))))
                  (setf (screen-copy-cursor screen)
                        (cons row (min right-bound (1+ col))))))
        (:up    (%scroll-up-one-line   screen row col max-offset))
        (:down  (%scroll-down-one-line screen row col h)))
      ;; When selecting, ensure mark is placed if not yet set.
      (when (and (screen-copy-selecting screen) (null (screen-copy-mark screen)))
        (setf (screen-copy-mark screen) (screen-copy-cursor screen)))
      (setf (screen-dirty-p screen) t))))

(defun copy-mode-set-cursor (screen row col)
  "Set the copy-mode cursor to ROW, COL, clamping both to the screen bounds.
   No-op when copy mode is not active."
  (when (screen-copy-mode-p screen)
    (let ((clamped-row (max 0 (min (1- (screen-height screen)) row)))
          (clamped-col (max 0 (min (1- (screen-width  screen)) col))))
      (setf (screen-copy-cursor screen) (cons clamped-row clamped-col)
            (screen-dirty-p screen) t))))

(defun copy-mode-begin-selection (screen)
  "Begin a text selection at the current copy-mode cursor position."
  (when (screen-copy-mode-p screen)
    (let ((cur (or (screen-copy-cursor screen) (cons 0 0))))
      (setf (screen-copy-mark        screen) cur
            (screen-copy-mark-offset screen) (screen-copy-offset screen)
            (screen-copy-cursor      screen) cur
            (screen-copy-selecting   screen) t
            (screen-dirty-p          screen) t))))

(defun copy-mode-set-mark (screen)
  "Set the copy-mode mark at the current cursor position without starting a selection.
   Implements tmux's `set-mark` send-keys -X command: the mark can later be
   jumped to with `jump-to-mark` without having gone through begin-selection.
   No-op when copy mode is inactive or there is no cursor."
  (when (and (screen-copy-mode-p screen) (screen-copy-cursor screen))
    (setf (screen-copy-mark        screen) (screen-copy-cursor screen)
          (screen-copy-mark-offset screen) (screen-copy-offset screen)
          (screen-dirty-p          screen) t)))

(defun copy-mode-other-end (screen)
  "Swap the two ends of the active selection (tmux copy-mode `other-end`, vi `o`).
   The moving end (cursor) and the anchored end (mark) exchange places so the
   user can extend the selection from the opposite side.  No-op (returns NIL)
   when copy mode is inactive, no selection is in progress, or either end is
   unset.  Marks the screen dirty when a swap occurs."
  (when (and (screen-copy-mode-p screen)
             (screen-copy-selecting screen)
             (screen-copy-mark   screen)
             (screen-copy-cursor screen))
    ;; After swapping, the new mark is the old cursor; it was at the current offset.
    (rotatef (screen-copy-cursor screen) (screen-copy-mark screen))
    (setf (screen-copy-mark-offset screen) (screen-copy-offset screen)
          (screen-dirty-p          screen) t)))

(defun copy-mode-jump-to-mark (screen)
  "Move the copy-mode cursor to the selection mark without swapping (tmux
   `jump-to-mark`).  When a selection is active, teleports the cursor to the
   anchor position so the user can re-examine the start of a selection.  No-op
   when copy mode is inactive or no mark has been set."
  (when (and (screen-copy-mode-p screen)
             (screen-copy-mark screen))
    (let ((mark        (screen-copy-mark screen))
          (mark-offset (screen-copy-mark-offset screen))
          (cur-offset  (screen-copy-offset screen)))
      ;; If the mark was set at a different viewport offset, adjust the cursor
      ;; row to account for the difference so it points to the same virtual row.
      (let* ((row-delta (- mark-offset cur-offset))
             (raw-row   (+ (car mark) row-delta))
             (h         (screen-height screen))
             (clamped   (max 0 (min (1- h) raw-row))))
        (setf (screen-copy-cursor screen) (cons clamped (cdr mark))
              (screen-dirty-p     screen) t)))))

(defun copy-mode-clear-selection (screen)
  "Clear the active selection without leaving copy mode (tmux `clear-selection`,
   the default copy-mode-vi Escape binding).  Drops the mark and the in-progress
   selection / line / rectangle flags, but — unlike copy-mode-cancel-selection,
   which is the full exit-path reset — KEEPS the cursor and scroll position so the
   user stays put in copy mode.  No-op unless copy mode is active with a selection
   or mark; marks the screen dirty when it clears."
  (when (and (screen-copy-mode-p screen)
             (or (screen-copy-selecting screen) (screen-copy-mark screen)))
    (setf (screen-copy-selecting        screen) nil
          (screen-copy-mark             screen) nil
          (screen-copy-mark-offset      screen) 0
          (screen-copy-line-selection-p screen) nil
          (screen-copy-rect-select-p    screen) nil
          (screen-dirty-p               screen) t)))

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
           (max-col (1- w))
           ;; Clamp BOTH row and col to the (possibly shrunk) grid bounds so the
           ;; cell reads below can never go out of range.
           (row (max 0 (min (1- (screen-height screen)) (car cur))))
           (col (max 0 (min max-col (cdr cur))))
           ;; Read through screen-display-cell (viewport-projected via copy-offset)
           ;; so word detection is correct when copy mode is scrolled back —
           ;; consistent with %copy-mode-row-chars (word navigation) and with the
           ;; viewport rows stored in screen-copy-cursor/-mark.
           (char-at (lambda (c)
                      (cell-char (screen-display-cell screen c row)))))
      (if (%word-separator-p (funcall char-at col))
          ;; Cursor not on a word char: select the single cell under it.  The
          ;; cursor's EXCLUSIVE end may reach width (one past the last column) so
          ;; %selection-text captures the rightmost cell; cap at width, never
          ;; max-col, to avoid dropping a cell at the right edge.
          (setf (screen-copy-mark   screen) (cons row col)
                (screen-copy-cursor screen) (cons row (min w (1+ col))))
          ;; On a word char: expand left to the word start and right past its end.
          (let ((start col)
                (end   col))
            (loop while (and (> start 0)
                             (not (%word-separator-p (funcall char-at (1- start)))))
                  do (decf start))
            (loop while (and (< end max-col)
                             (not (%word-separator-p (funcall char-at (1+ end)))))
                  do (incf end))
            (setf (screen-copy-mark   screen) (cons row start)
                  ;; Exclusive end may reach width so the last word char is kept.
                  (screen-copy-cursor screen) (cons row (min w (1+ end))))))
      (setf (screen-copy-mark-offset screen) (screen-copy-offset screen)
            (screen-copy-selecting   screen) t
            (screen-dirty-p          screen) t))))

(defun copy-mode-cancel-selection (screen)
  "Cancel any active copy-mode selection."
  (setf (screen-copy-mark           screen) nil
        (screen-copy-mark-offset    screen) 0
        (screen-copy-cursor         screen) nil
        (screen-copy-selecting      screen) nil
        (screen-copy-line-selection-p screen) nil
        (screen-copy-rect-select-p  screen) nil
        (screen-dirty-p             screen) t))

;;; %selection-bounds extracts the canonical (start-row end-row start-col end-col)
;;; rectangle from the mark and cursor positions — independent of which end the
;;; user anchored first.  %selection-text builds the string from that rectangle.
;;; Both are private (percent-prefixed) and independently testable.

(defun %selection-bounds (screen)
  "Return (values start-vrow end-vrow start-col end-col) for the current copy-mode
   selection in SCREEN.  Rows are VIRTUAL (0 = oldest scrollback, increasing toward
   the live grid bottom) so the selection is invariant to subsequent viewport scrolling.
   Assumes mark and cursor are already set."
  (let* ((sb-n        (length (screen-scrollback screen)))
         (mark        (screen-copy-mark   screen))
         (cursor      (screen-copy-cursor screen))
         (mark-offset (screen-copy-mark-offset screen))
         (cur-offset  (screen-copy-offset screen))
         ;; Convert viewport rows to virtual rows using the offset in effect at placement.
         (mark-vrow   (+ sb-n (car mark)   (- mark-offset)))
         (cur-vrow    (+ sb-n (car cursor) (- cur-offset)))
         (mark-col    (cdr mark))
         (cursor-col  (cdr cursor))
         (start-vrow  (min mark-vrow cur-vrow))
         (end-vrow    (max mark-vrow cur-vrow))
         (start-col   (cond ((< mark-vrow cur-vrow) mark-col)
                            ((> mark-vrow cur-vrow) cursor-col)
                            (t                      (min mark-col cursor-col))))
         (end-col     (cond ((< mark-vrow cur-vrow) cursor-col)
                            ((> mark-vrow cur-vrow) mark-col)
                            (t                      (max mark-col cursor-col)))))
    (values start-vrow end-vrow start-col end-col)))

;;; %extract-row-chars reads characters from a rectangular range as a string.
;;; It accepts either a virtual row (via %copy-mode-virtual-row-string, used by
;;; the selection path where %selection-bounds now returns virtual rows) or a
;;; viewport row (used by %copy-row-range-to-paste-buffer in the nav module).
;;; The selection path uses the virtual-row overload; nav uses viewport overload.
;;; Pure data extraction — no I/O side effects.

(defun %extract-vrow-chars (screen vrow from-col to-col)
  "Return a string of characters from SCREEN at VIRTUAL row VROW (0=oldest
   scrollback, increasing toward live grid), columns FROM-COL to TO-COL (exclusive).
   Inlines the virtual-row lookup so this file has no forward-reference to
   commands-copy-mode-search.  Pure data extraction."
  (if (>= from-col to-col)
      ""
      (let* ((sb    (screen-scrollback screen))
             (sb-n  (length sb))
             (n     (- to-col from-col))
             (result (make-array n :element-type 'character :initial-element #\Space)))
        (dotimes (i n result)
          (let ((col (+ from-col i)))
            (setf (char result i)
                  (if (< vrow sb-n)
                      ;; Scrollback: vrow 0 = oldest = nth(sb-n-1), newest = nth(0).
                      (let ((vec (nth (- sb-n 1 vrow) sb)))
                        (if (and vec (< col (length vec)))
                            (cell-char (aref vec col))
                            #\Space))
                      ;; Live grid row.
                      (cell-char (screen-cell screen col (- vrow sb-n))))))))))

(defun %extract-row-chars (screen row from-col to-col)
  "Return a string of characters from SCREEN at viewport ROW, columns FROM-COL to
   TO-COL (exclusive).  Reads through screen-display-cell so the projection honours
   the copy-mode scroll offset.  ROW is a VIEWPORT row (0-based, same units as
   screen-copy-cursor when copy-offset is 0).  Used by %copy-row-range-to-paste-buffer.
   The selection path uses %extract-vrow-chars instead.  Pure data extraction."
  (let* ((n      (- to-col from-col))
         (result (make-string n)))
    (dotimes (i n result)
      (setf (char result i)
            (cell-char (screen-display-cell screen (+ from-col i) row))))))

(defun %selection-text (screen)
  "Compute the text selected by copy-mode in SCREEN.
   Returns a string, or NIL when no valid selection exists.
   Intermediate rows (not the last) are right-trimmed of trailing spaces.
   Pure data extraction: no lock held, no I/O."
  (unless (and (screen-copy-selecting screen)
               (screen-copy-mark   screen)
               (screen-copy-cursor screen))
    (return-from %selection-text nil))
  (multiple-value-bind (start-vrow end-vrow start-col end-col)
      (%selection-bounds screen)
    (let* ((w    (screen-width screen))
           (text (with-output-to-string (out)
                   (loop for vrow from start-vrow to end-vrow do
                     (let* ((col-from (if (= vrow start-vrow) start-col 0))
                            (col-to   (if (= vrow end-vrow)   end-col   w))
                            (row-str  (%extract-vrow-chars screen vrow col-from col-to)))
                       ;; Trim trailing spaces from intermediate rows.
                       (write-string (if (< vrow end-vrow)
                                         (string-right-trim " " row-str)
                                         row-str)
                                     out)
                       (when (< vrow end-vrow)
                         (write-char #\Newline out)))))))
      (if (plusp (length text)) text nil))))

