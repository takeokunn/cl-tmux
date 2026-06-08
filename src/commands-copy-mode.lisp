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
        (:right (setf (screen-copy-cursor screen) (cons row (min (1- w) (1+ col)))))
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
      (setf (screen-copy-mark      screen) cur
            (screen-copy-cursor    screen) cur
            (screen-copy-selecting screen) t
            (screen-dirty-p        screen) t))))

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
    (rotatef (screen-copy-cursor screen) (screen-copy-mark screen))
    (setf (screen-dirty-p screen) t)))

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
      (setf (screen-copy-selecting screen) t
            (screen-dirty-p        screen) t))))

(defun copy-mode-cancel-selection (screen)
  "Cancel any active copy-mode selection."
  (setf (screen-copy-mark           screen) nil
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
  "Return (values start-row end-row start-col end-col) for the current copy-mode
   selection in SCREEN, normalising mark and cursor order so start <= end.
   Assumes mark and cursor are already set."
  (let* ((mark       (screen-copy-mark   screen))
         (cursor     (screen-copy-cursor screen))
         (mark-row   (car mark))
         (mark-col   (cdr mark))
         (cursor-row (car cursor))
         (cursor-col (cdr cursor))
         (start-row  (min mark-row cursor-row))
         (end-row    (max mark-row cursor-row))
         (start-col  (cond ((< mark-row cursor-row) mark-col)
                           ((> mark-row cursor-row) cursor-col)
                           (t                       (min mark-col cursor-col))))
         (end-col    (cond ((< mark-row cursor-row) cursor-col)
                           ((> mark-row cursor-row) mark-col)
                           (t                       (max mark-col cursor-col)))))
    (values start-row end-row start-col end-col)))

;;; %extract-row-chars reads characters from a rectangular range as a string.
;;; Pure data extraction — no I/O side effects.

(defun %extract-row-chars (screen row from-col to-col)
  "Return a string of characters from SCREEN at viewport ROW, columns FROM-COL to
   TO-COL (exclusive).  Reads through screen-display-cell so the projection honours
   the copy-mode scroll offset: when scrolled back, rows above the offset come from
   the scrollback buffer.  ROW is a viewport row (matching screen-copy-cursor/-mark),
   NOT a raw live-grid row — this is what makes a selection yanked while scrolled
   back return the text the user actually sees.  Pure data extraction."
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
  (multiple-value-bind (start-row end-row start-col end-col)
      (%selection-bounds screen)
    (let* ((w    (screen-width screen))
           (text (with-output-to-string (out)
                   (loop for row from start-row to end-row do
                     (let* ((col-from (if (= row start-row) start-col 0))
                            (col-to   (if (= row end-row)   end-col   w))
                            (row-str  (%extract-row-chars screen row col-from col-to)))
                       ;; Trim trailing spaces from intermediate rows.
                       (write-string (if (< row end-row)
                                         (string-right-trim " " row-str)
                                         row-str)
                                     out)
                       (when (< row end-row)
                         (write-char #\Newline out)))))))
      (if (plusp (length text)) text nil))))

;;; ── Rectangle selection text ────────────────────────────────────────────────
;;;
;;; When rectangle select is active (screen-copy-rect-select-p), each row in
;;; the selection range is read between the same left and right column bounds
;;; (the canonical column range derived from mark and cursor column positions).
;;; Rows are joined with newlines; trailing spaces within each row are trimmed.

(defun %rectangle-selection-text (screen)
  "Compute the rectangle-selected text for SCREEN.
   Returns a string, or NIL when no valid selection exists.
   In rectangle mode each row between start-row and end-row is extracted
   between fixed column bounds (min/max of mark-col and cursor-col)."
  (unless (and (screen-copy-selecting screen)
               (screen-copy-mark   screen)
               (screen-copy-cursor screen))
    (return-from %rectangle-selection-text nil))
  (multiple-value-bind (start-row end-row start-col end-col)
      (%selection-bounds screen)
    (let* ((text (with-output-to-string (out)
                   (loop for row from start-row to end-row do
                     (let* ((row-str  (%extract-row-chars screen row start-col end-col))
                            (trimmed  (string-right-trim " " row-str)))
                       (write-string trimmed out)
                       (when (< row end-row)
                         (write-char #\Newline out)))))))
      (if (plusp (length text)) text nil))))

;;; ── copy-pipe helper ─────────────────────────────────────────────────────────
;;;
;;; When the "copy-command" option is set to a non-empty string, the yank text
;;; is also piped to that shell command via uiop:run-program.  Errors are
;;; silently swallowed so a misconfigured copy-command does not crash the session.

(defconstant +copy-command-timeout+ 30
  "Maximum seconds to wait for a copy-command subprocess before giving up.")

(defun %run-shell-cmd-with-input (command text)
  "Pipe TEXT as stdin to COMMAND (a shell string), bounded by +copy-command-timeout+.
   Errors are silently swallowed so a misconfigured command does not crash the session."
  (ignore-errors
    (bt:with-timeout (+copy-command-timeout+)
      (uiop:run-program (list "/bin/sh" "-c" command)
                        :input (make-string-input-stream text)
                        :ignore-error-status t))))

(defun %run-copy-command (text)
  "Pipe TEXT to the shell command stored in the \"copy-command\" option.
   No-op when the option is empty or TEXT is NIL/empty.
   The subprocess is bounded by +copy-command-timeout+ seconds so a hanging
   copy-command does not block the event loop indefinitely."
  (when (and text (plusp (length text)))
    (let ((cmd (ignore-errors (cl-tmux/options:get-option "copy-command"))))
      (when (and (stringp cmd) (plusp (length cmd)))
        (%run-shell-cmd-with-input cmd text)))))

(defun copy-mode-yank (screen)
  "Copy selected text to paste buffer (and pipe via copy-command if configured),
   then exit copy mode.  In rectangle-select mode the rectangular region is used."
  (let ((text (if (screen-copy-rect-select-p screen)
                  (%rectangle-selection-text screen)
                  (%selection-text screen))))
    (when (and text (plusp (length text)))
      (cl-tmux/buffer:add-paste-buffer text)
      (%run-copy-command text)))
  (copy-mode-cancel-selection screen)
  (copy-mode-exit screen))

;;; ── Rectangle-select toggle ─────────────────────────────────────────────────

(defun copy-mode-toggle-rectangle (screen)
  "Toggle rectangle-select mode for SCREEN.
   When toggled on, yank uses the rectangular region instead of stream selection.
   Marks the screen dirty."
  (when (screen-copy-mode-p screen)
    (setf (screen-copy-rect-select-p screen)
          (not (screen-copy-rect-select-p screen))
          (screen-dirty-p screen) t)))

;;; ── Append selection ────────────────────────────────────────────────────────
;;;
;;; append-selection appends the current selection to the *most recent* paste
;;; buffer entry (if one exists) instead of pushing a new entry.  If the paste
;;; buffer is empty, it behaves like a normal yank.

(defun copy-mode-append-selection (screen)
  "Append selected text to the most recent paste buffer entry, then exit copy mode.
   If the paste buffer is empty the selection is pushed as a new entry.
   Rectangle-select mode is honoured."
  (when (screen-copy-mode-p screen)
    (let ((text (if (screen-copy-rect-select-p screen)
                    (%rectangle-selection-text screen)
                    (%selection-text screen))))
      (when (and text (plusp (length text)))
        (let ((existing (cl-tmux/buffer:get-paste-buffer 0)))
          (if existing
              ;; Replace the most recent entry with old + new text.
              (progn
                (cl-tmux/buffer:delete-paste-buffer 0)
                (cl-tmux/buffer:add-paste-buffer (concatenate 'string existing text)))
              (cl-tmux/buffer:add-paste-buffer text)))
        (%run-copy-command text)))
    (copy-mode-cancel-selection screen)
    (copy-mode-exit screen)))

;;; ── copy-pipe (yank + pipe) ─────────────────────────────────────────────────
;;;
;;; copy-mode-copy-pipe is the direct implementation of tmux's copy-pipe-and-cancel:
;;; it places the selection text into the paste buffer AND pipes it to CMD.
;;; CMD overrides the "copy-command" option for this single invocation.

(defun %resolve-copy-pipe-cmd (cmd)
  "Return the effective shell command string for copy-pipe.
   If CMD is a non-empty string, use it directly.
   Otherwise fall back to the \"copy-command\" global option.
   Returns NIL when neither source yields a usable command."
  (if (and (stringp cmd) (plusp (length cmd)))
      cmd
      (let ((option-cmd (ignore-errors (cl-tmux/options:get-option "copy-command"))))
        (when (and (stringp option-cmd) (plusp (length option-cmd)))
          option-cmd))))

(defun copy-mode-copy-pipe (screen cmd)
  "Yank selected text to the paste buffer and pipe it to CMD (a shell string).
   If CMD is empty or NIL the global \"copy-command\" option is used.
   Exits copy mode after yanking."
  (when (screen-copy-mode-p screen)
    (let ((text (if (screen-copy-rect-select-p screen)
                    (%rectangle-selection-text screen)
                    (%selection-text screen))))
      (when (and text (plusp (length text)))
        (cl-tmux/buffer:add-paste-buffer text)
        (let ((effective-cmd (%resolve-copy-pipe-cmd cmd)))
          (when effective-cmd
            (%run-shell-cmd-with-input effective-cmd text)))))
    (copy-mode-cancel-selection screen)
    (copy-mode-exit screen)))

;;; Navigation (word/line/screen jumps) and search are in separate files:
;;;   commands-copy-mode-nav.lisp    — word-forward/backward/end, line-start/end,
;;;                                    cursor-jump macros, page/half-page scroll,
;;;                                    begin-line-selection, copy-end-of-line, copy-line
;;;   commands-copy-mode-search.lisp — %copy-mode-row-string, find-forward/backward,
;;;                                    search-forward/backward, search-next/prev
