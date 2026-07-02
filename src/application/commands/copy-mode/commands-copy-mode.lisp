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
        (screen-copy-exit-on-bottom screen) nil
        (screen-copy-mode-entered-by-mouse-p screen) nil))

(defun %clamp-row-col (screen row col)
  "Return (cons clamped-row clamped-col) with row in [0, height-1] and col in [0, width-1]."
  (cons (max 0 (min (1- (screen-height screen)) row))
        (max 0 (min (1- (screen-width  screen)) col))))

(defun %copy-mode-clamp-cursor (screen)
  "Clamp the copy-mode cursor row into [0, height-1] and col into [0, width-1].
   Called after the viewport offset changes so the cursor stays visible.
   Operates on the cursor cons directly; no-op when cursor is NIL."
  (let ((cursor (screen-copy-cursor screen)))
    (when cursor
      (setf (screen-copy-cursor screen)
            (%clamp-row-col screen (car cursor) (cdr cursor))))))

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

;;; ── send-keys -X *-and-cancel / selection-mode / scroll-to-mouse ─────────────
;;;
;;; These are real tmux window-copy commands (window-copy.c) that scroll/move and
;;; then exit copy mode when the live bottom (offset 0) is reached.

(defun %copy-mode-with-cancel-at-bottom (screen action &optional extra-guard)
  "Run ACTION (a thunk of no arguments) while SCREEN is in copy mode, then exit
   copy mode once the viewport has reached the live bottom (scroll-offset 0).
   EXTRA-GUARD, when supplied, is a thunk of no arguments called AFTER ACTION
   runs whose result is ANDed into the exit decision — used by callers that
   must also confirm ACTION left some other state (e.g. the cursor) unchanged.
   This is the shared skeleton behind the tmux send-keys -X *-and-cancel family
   (window-copy.c): perform a scroll/move, then auto-exit at the live bottom."
  (when (screen-copy-mode-p screen)
    (funcall action)
    (when (and (zerop (screen-copy-offset screen))
               (or (null extra-guard) (funcall extra-guard)))
      (copy-mode-exit screen))))

(defun copy-mode-scroll-down-and-cancel (screen)
  "send-keys -X scroll-down-and-cancel: scroll the viewport down one line, then
   exit copy mode when the live bottom (scroll-offset 0) is reached."
  (%copy-mode-with-cancel-at-bottom
   screen (lambda () (copy-mode-scroll screen -1))))

(defun copy-mode-page-down-and-cancel (screen)
  "send-keys -X page-down-and-cancel: scroll one full page down, then exit copy
   mode when the live bottom is reached."
  (%copy-mode-with-cancel-at-bottom
   screen (lambda () (copy-mode-scroll screen (- (screen-height screen))))))

(defun copy-mode-cursor-down-and-cancel (screen)
  "send-keys -X cursor-down-and-cancel: move the cursor down; exit copy mode when
   the cursor is already at the bottom of the history (it cannot move and the
   viewport is at the live bottom)."
  (let ((before     (copy-tree (screen-copy-cursor screen)))
        (before-off (screen-copy-offset screen)))
    (%copy-mode-with-cancel-at-bottom
     screen
     (lambda () (copy-mode-move-cursor screen :down))
     (lambda () (and (equal before (screen-copy-cursor screen))
                      (zerop before-off))))))

(defun copy-mode-selection-mode (screen mode)
  "send-keys -X selection-mode <char|word|line>: set the selection granularity.
   line begins a line selection, word selects the current word, anything else
   (the default char) begins a character selection."
  (when (screen-copy-mode-p screen)
    (cond
      ((string-equal mode "line") (copy-mode-begin-line-selection screen))
      ((string-equal mode "word") (copy-mode-select-word screen))
      (t                          (copy-mode-begin-selection screen)))))

(defun copy-mode-scroll-to-mouse (screen)
  "send-keys -X scroll-to-mouse: scroll the copy-mode viewport toward the mouse
   drag position.  cl-tmux performs mouse-drag scrolling in the event layer, so
   this scriptable form is accepted and refreshes the viewport."
  (when (screen-copy-mode-p screen)
    (setf (screen-dirty-p screen) t)))

;;; These top-level helpers are called from copy-mode-move-cursor for :up / :down.
;;; Keeping them at top level makes each path independently readable.

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

(defun %copy-mode-move-left (screen row col)
  (setf (screen-copy-cursor screen) (cons row (max 0 (1- col)))))

(defun %copy-mode-move-right (screen row col w selecting)
  ;; Selecting uses an exclusive end, so allow W to include the rightmost cell.
  (let ((right-bound (if selecting w (1- w))))
    (setf (screen-copy-cursor screen)
          (cons row (min right-bound (1+ col))))))

(defun %copy-mode-ensure-selection-mark (screen)
  (when (and (screen-copy-selecting screen) (null (screen-copy-mark screen)))
    (setf (screen-copy-mark screen) (screen-copy-cursor screen))))

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
        (:left  (%copy-mode-move-left screen row col))
        (:right (%copy-mode-move-right screen row col w (screen-copy-selecting screen)))
        (:up    (%scroll-up-one-line   screen row col max-offset))
        (:down  (%scroll-down-one-line screen row col h)))
      (%copy-mode-ensure-selection-mark screen)
      (setf (screen-dirty-p screen) t))))

(defun copy-mode-set-cursor (screen row col)
  "Set the copy-mode cursor to ROW, COL, clamping both to the screen bounds.
   No-op when copy mode is not active."
  (when (screen-copy-mode-p screen)
    (setf (screen-copy-cursor screen) (%clamp-row-col screen row col)
          (screen-dirty-p screen) t)))

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

(defun %reset-selection-fields (screen)
  "Clear all selection state fields on SCREEN (selecting, mark, line/rect flags) and
   mark dirty.  Does NOT clear the cursor — callers that need that do so separately."
  (setf (screen-copy-selecting        screen) nil
        (screen-copy-mark             screen) nil
        (screen-copy-mark-offset      screen) 0
        (screen-copy-line-selection-p screen) nil
        (screen-copy-rect-select-p    screen) nil
        (screen-dirty-p               screen) t))

(defun copy-mode-clear-selection (screen)
  "Clear the active selection without leaving copy mode (tmux `clear-selection`,
   the default copy-mode-vi Escape binding).  Drops the mark and the in-progress
   selection / line / rectangle flags, but — unlike copy-mode-cancel-selection,
   which is the full exit-path reset — KEEPS the cursor and scroll position so the
   user stays put in copy mode.  No-op unless copy mode is active with a selection
   or mark; marks the screen dirty when it clears."
  (when (and (screen-copy-mode-p screen)
             (or (screen-copy-selecting screen) (screen-copy-mark screen)))
    (%reset-selection-fields screen)))

(defun copy-mode-cancel-selection (screen)
  "Cancel any active copy-mode selection."
  (setf (screen-copy-cursor screen) nil)
  (%reset-selection-fields screen))
