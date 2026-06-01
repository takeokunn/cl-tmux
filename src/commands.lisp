(in-package #:cl-tmux/commands)

;;; High-level tmux commands that operate on the session/window/pane model.
;;; Each exported function is the CL analogue of a tmux command-line command.

;;; ── Kill ───────────────────────────────────────────────────────────────────
;;;
;;; kill_pane(Session)  :- close_pty(Pane), remove_pane(Window, Pane),
;;;                         (empty(Window) -> kill_window(Session, Window) ; true).
;;; kill_window(Session, Window) :- forall(pane(P, Window), close_pty(P)),
;;;                                  remove_window(Session, Window),
;;;                                  (empty(Session) -> quit ; select_next(Session)).

(defun kill-pane (session &optional pane)
  "Close PANE (default: active pane of SESSION).
   Sends SIGHUP to its child process and closes the PTY fd.
   Removes the pane from the window's split tree, collapsing its parent so the
   sibling reclaims the freed rectangle.  If the owning window becomes empty,
   also calls KILL-WINDOW.
   Only re-selects a new active pane when the killed pane was the active one.
   Returns :quit if no windows remain, nil otherwise."
  (let* ((win        (session-active-window session))
         (target     (or pane (window-active-pane win)))
         (was-active (eq target (window-active-pane win))))
    (when target
      (ignore-errors (pty-close (pane-fd target) (pane-pid target))))
    (let ((survivor (window-remove-pane win target)))
      (run-hooks +hook-after-kill-pane+ target)
      (if (null (window-panes win))
          (kill-window session win)
          (progn
            (when was-active
              (let* ((remaining (window-panes win))
                     (last-act  (window-last-active win))
                     (chosen    (or (and last-act (find last-act remaining))
                                    survivor
                                    (first remaining))))
                (window-select-pane win chosen)))
            nil)))))

(defun %nearest-window (windows killed-id)
  "Return the window from WINDOWS whose id is numerically closest to KILLED-ID.
   When two windows are equidistant, the one with the larger id (next neighbour)
   is preferred.  Falls back to (first windows) when the list is empty."
  (reduce (lambda (best w)
            (let ((d-best (abs (- killed-id (window-id best))))
                  (d-w    (abs (- killed-id (window-id w)))))
              (cond ((< d-w d-best) w)
                    ((and (= d-w d-best) (> (window-id w) killed-id)) w)
                    (t best))))
          (rest windows)
          :initial-value (first windows)))

(defun kill-window (session &optional window)
  "Destroy WINDOW (default: active window of SESSION).
   Kills all panes in it and removes the window from SESSION.
   After killing the active window, selects the numerically nearest remaining
   window (next higher id if available, otherwise next lower).
   Returns :quit if no windows remain, NIL otherwise."
  (let* ((target    (or window (session-active-window session)))
         (killed-id (window-id target))
         (remaining (remove target (session-windows session))))
    (dolist (pane (window-panes target))
      (ignore-errors (pty-close (pane-fd pane) (pane-pid pane))))
    (setf (session-windows session) remaining)
    (run-hooks +hook-after-kill-window+ target)
    (unless remaining (return-from kill-window :quit))
    (when (eq (session-active-window session) target)
      (session-select-window session (%nearest-window remaining killed-id)))
    nil))

;;; ── Rename / Select ────────────────────────────────────────────────────────
;;;
;;; rename_window(Window, Name)   :- set(window-name, Name), run_hooks(after-rename-window).
;;; rename_session(Session, Name) :- nonempty(Name), set(session-name, Name).
;;; select_window(Session, N)     :- nth(N, windows(Session), W), activate(W).

(defun rename-window (window name)
  "Set WINDOW's name to NAME.  Empty NAME is a no-op, matching tmux behaviour."
  (when (and window name (not (string= name "")))
    (setf (window-name window) name)
    (run-hooks +hook-after-rename-window+ window name)))

(defun rename-session (session name)
  "Set SESSION's name to NAME."
  (when (and session name (not (string= name "")))
    (setf (session-name session) name)))

(defun select-window-by-number (session n)
  "Select the window in SESSION whose stored id equals N.
   Looks up by window-id, not by 0-based list position, so the digit pressed
   always matches the window label even after kills leave gaps in the list."
  (let ((win (find n (session-windows session) :key #'window-id)))
    (when win
      (session-select-window session win))))

;;; ── Resize ─────────────────────────────────────────────────────────────────
;;;
;;; resize_pane(Window, Dir, Amount) :- active_pane(Window, P),
;;;                                     adjust_split_tree(Window, P, Dir, Amount).

(defun resize-pane (window direction &optional (amount 5))
  "Resize the active pane via the split tree. Returns the active pane on success, NIL otherwise."
  (when (and window (window-tree window))
    (window-resize-active window direction amount)))

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
    (let* ((h   (screen-height screen))
           (w   (screen-width  screen))
           (cur (or (screen-copy-cursor screen) (cons (1- h) 0)))
           (row (car cur))
           (col (cdr cur))
           (max-offset (length (screen-scrollback screen))))
      (ecase direction
        (:left
         (setf (screen-copy-cursor screen) (cons row (max 0 (1- col)))))
        (:right
         (setf (screen-copy-cursor screen) (cons row (min (1- w) (1+ col)))))
        (:up
         (let ((new-row (1- row)))
           (cond
             ;; Cursor still within viewport — just move it up
             ((>= new-row 0)
              (setf (screen-copy-cursor screen) (cons new-row col)))
             ;; Cursor at top edge — scroll viewport back (older), hold cursor at row 0
             ((< (screen-copy-offset screen) max-offset)
              (incf (screen-copy-offset screen))
              (setf (screen-copy-cursor screen) (cons 0 col)))
             ;; Already at the oldest scrollback line — do not move
             (t nil))))
        (:down
         (let ((new-row (1+ row)))
           (cond
             ;; Cursor still within viewport — just move it down
             ((< new-row h)
              (setf (screen-copy-cursor screen) (cons new-row col)))
             ;; Cursor at bottom edge — scroll viewport forward (newer), hold cursor at h-1
             ((> (screen-copy-offset screen) 0)
              (decf (screen-copy-offset screen))
              (setf (screen-copy-cursor screen) (cons (1- h) col)))
             ;; Already at live view bottom — do not move
             (t nil)))))
      ;; When selecting, ensure mark is placed if not yet set
      (when (and (screen-copy-selecting screen) (null (screen-copy-mark screen)))
        (setf (screen-copy-mark screen) (screen-copy-cursor screen)))
      (setf (screen-dirty-p screen) t))))

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

(defun copy-mode-top (screen)
  "Jump to the oldest scrollback line (maximum scroll-back offset)."
  (when (screen-copy-mode-p screen)
    (copy-mode-scroll screen most-positive-fixnum)))

(defun copy-mode-bottom (screen)
  "Jump to the live view bottom (scroll-offset = 0)."
  (when (screen-copy-mode-p screen)
    (copy-mode-scroll screen (- most-positive-fixnum))))

(defun copy-mode-high (screen)
  "Move cursor to row 0 (top of viewport), keeping column."
  (when (screen-copy-mode-p screen)
    (let ((col (cdr (screen-copy-cursor screen))))
      (setf (screen-copy-cursor screen) (cons 0 col)
            (screen-dirty-p screen) t))))

(defun copy-mode-middle (screen)
  "Move cursor to the middle row of the viewport, keeping column."
  (when (screen-copy-mode-p screen)
    (let* ((col  (cdr (screen-copy-cursor screen)))
           (mid  (floor (screen-height screen) 2)))
      (setf (screen-copy-cursor screen) (cons mid col)
            (screen-dirty-p screen) t))))

(defun copy-mode-low (screen)
  "Move cursor to the last row of the viewport (height-1), keeping column."
  (when (screen-copy-mode-p screen)
    (let* ((col  (cdr (screen-copy-cursor screen)))
           (last (1- (screen-height screen))))
      (setf (screen-copy-cursor screen) (cons last col)
            (screen-dirty-p screen) t))))

(defun copy-mode-page-up (screen)
  "Scroll the viewport back by one full screen height."
  (when (screen-copy-mode-p screen)
    (copy-mode-scroll screen (screen-height screen))))

(defun copy-mode-page-down (screen)
  "Scroll the viewport forward by one full screen height."
  (when (screen-copy-mode-p screen)
    (copy-mode-scroll screen (- (screen-height screen)))))

(defun copy-mode-half-page-up (screen)
  "Scroll the viewport back by half a screen height."
  (when (screen-copy-mode-p screen)
    (copy-mode-scroll screen (floor (screen-height screen) 2))))

(defun copy-mode-half-page-down (screen)
  "Scroll the viewport forward by half a screen height."
  (when (screen-copy-mode-p screen)
    (copy-mode-scroll screen (- (floor (screen-height screen) 2)))))

(defun copy-mode-scroll-up-line (screen)
  "Scroll the viewport back by 1 line (cursor stays fixed when possible)."
  (when (screen-copy-mode-p screen)
    (copy-mode-scroll screen 1)))

(defun copy-mode-scroll-down-line (screen)
  "Scroll the viewport forward by 1 line (cursor stays fixed when possible)."
  (when (screen-copy-mode-p screen)
    (copy-mode-scroll screen -1)))

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

(defun copy-mode-copy-end-of-line (screen)
  "Copy from the current cursor column to the end of the line, then exit copy mode."
  (when (screen-copy-mode-p screen)
    (let* ((row (car (screen-copy-cursor screen)))
           (col (cdr (screen-copy-cursor screen)))
           (w   (screen-width screen))
           (text (with-output-to-string (out)
                   (loop for c from col below w do
                     (write-char (cell-char (screen-cell screen c row)) out)))))
      (let ((trimmed (string-right-trim " " text)))
        (when (plusp (length trimmed))
          (cl-tmux/buffer:add-paste-buffer trimmed))))
    (copy-mode-exit screen)))

(defun copy-mode-copy-line (screen)
  "Copy the full current line (all columns), then exit copy mode."
  (when (screen-copy-mode-p screen)
    (let* ((row  (car (screen-copy-cursor screen)))
           (w    (screen-width screen))
           (text (with-output-to-string (out)
                   (loop for c from 0 below w do
                     (write-char (cell-char (screen-cell screen c row)) out)))))
      (let ((trimmed (string-right-trim " " text)))
        (when (plusp (length trimmed))
          (cl-tmux/buffer:add-paste-buffer trimmed))))
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

;;; ── Pane operations ────────────────────────────────────────────────────────
;;;
;;; swap_pane(Window, Dir)   :- active(Window, AP), neighbor(AP, Dir, Other),
;;;                              swap_positions(AP, Other), swap_list_order(AP, Other).
;;; capture_pane(Pane, Opts) :- lock(screen(Pane)),
;;;                              (scrollback(Opts) -> emit_scrollback ; true),
;;;                              emit_visible_rows.

(defun swap-pane (window direction)
  "Swap the active pane with the next (:right) or previous (:left) pane in WINDOW.
   Swaps the panes in the panes list, reassigns positions, and relayouts."
  (let* ((panes (window-panes window))
         (ap    (window-active-pane window))
         (idx   (position ap panes))
         (n     (length panes)))
    (when (> n 1)
      (let* ((other-idx (ecase direction
                          (:right (mod (1+ idx) n))
                          (:left  (mod (1- idx) n))))
             (other (nth other-idx panes))
             (new-panes (copy-list panes)))
        (setf (nth idx new-panes) other
              (nth other-idx new-panes) ap
              (window-panes window) new-panes)
        ;; Swap x/y/width/height between the two panes
        (let ((ax (pane-x ap)) (ay (pane-y ap)) (aw (pane-width ap)) (ah (pane-height ap)))
          (pane-reposition ap (pane-x other) (pane-y other) (pane-width other) (pane-height other))
          (pane-reposition other ax ay aw ah))
        ap))))

(defun capture-pane (pane &key (include-scrollback nil))
  "Dump the visible content of PANE as a string.
   When INCLUDE-SCROLLBACK is T, also include scrollback history above the visible area."
  (let ((screen (pane-screen pane)))
    (with-lock-held ((screen-lock screen))
      (with-output-to-string (out)
        (when include-scrollback
          (dolist (row (reverse (screen-scrollback screen)))
            (dotimes (i (length row))
              (write-char (cell-char (aref row i)) out))
            (terpri out)))
        (dotimes (row (screen-height screen))
          (dotimes (col (screen-width screen))
            (write-char (cell-char (screen-cell screen col row)) out))
          (terpri out))))))

;;; ── break-pane ─────────────────────────────────────────────────────────────
;;;
;;; break_pane(Session) :-
;;;   active_window(Session, Win),
;;;   active_pane(Win, Pane),
;;;   (sole_pane(Win) -> no_op ; true),
;;;   remove_pane(Win, Pane),
;;;   new_window(Session, NewWin),
;;;   set_sole_pane(NewWin, Pane),
;;;   select_window(Session, NewWin).

(defun break-pane (session)
  "Remove the active pane from its window and place it as the sole pane
   of a new window.  When the source window has only one pane, break-pane
   is a no-op (nothing to break out).  Returns the new window, or NIL."
  (let* ((src-win (session-active-window session))
         (pane    (and src-win (window-active-pane src-win))))
    (unless (and src-win pane) (return-from break-pane nil))
    ;; Must have at least 2 panes to break one out.
    (when (< (length (window-panes src-win)) 2)
      (return-from break-pane nil))
    ;; Remove pane from its current window (collapses the tree).
    (window-remove-pane src-win pane)
    ;; After removal, re-select a pane in the source window.
    (when (window-panes src-win)
      (window-select-pane src-win (first (window-panes src-win))))
    ;; Create a new window with the pane as the sole full-screen occupant.
    ;; Use the lowest free window id (same rule as session-new-window).
    (let* ((rows   (window-height src-win))
           (cols   (window-width  src-win))
           (new-id (%next-window-id session))
           (name   (%shell-basename))
           (new-win (make-window :id new-id :name name :width cols :height rows)))
      ;; Install the pane as the sole leaf in the new window's tree.
      (setf (window-panes new-win) (list pane)
            (window-tree  new-win) (make-layout-leaf pane))
      (window-select-pane new-win pane)
      ;; Reposition the pane to fill the new window.
      (pane-reposition pane 0 0 cols rows)
      ;; Attach the new window to the session, keeping list sorted by id.
      (setf (session-windows session)
            (sort (cons new-win (session-windows session)) #'< :key #'window-id))
      (session-select-window session new-win)
      (run-hooks +hook-after-new-window+ new-win)
      new-win)))

;;; ── join-pane / move-pane ───────────────────────────────────────────────────
;;;
;;; join_pane(Session, SrcWin, SrcPane, DstWin, Dir) :-
;;;   remove_pane(SrcWin, SrcPane),
;;;   (empty(SrcWin) -> kill_window(Session, SrcWin) ; true),
;;;   insert_by_split(DstWin, SrcPane, Dir).

(defun join-pane (session src-window src-pane dst-window direction)
  "Move SRC-PANE from SRC-WINDOW into DST-WINDOW as a split in DIRECTION.
   DIRECTION is :h (left/right) or :v (top/bottom).
   If SRC-WINDOW becomes empty after removal, it is killed.
   Returns SRC-PANE on success, NIL on failure."
  (unless (and src-window src-pane dst-window) (return-from join-pane nil))
  ;; Remove from source window.
  (window-remove-pane src-window src-pane)
  ;; Kill src window if now empty.
  (when (null (window-panes src-window))
    (let ((remaining (remove src-window (session-windows session))))
      (setf (session-windows session) remaining)
      (when (eq (session-active-window session) src-window)
        (session-select-window session (first remaining)))))
  ;; Insert into dst window as a split on the active pane.
  (let* ((active (window-active-pane dst-window))
         (tree   (window-tree dst-window)))
    (unless (and active tree) (return-from join-pane nil))
    (let ((active-leaf (layout-find-leaf tree active)))
      (unless active-leaf (return-from join-pane nil))
      ;; Compute geometry for the joined pane.
      (multiple-value-bind (px py pw ph) (split-child-geometry active direction)
        ;; Reposition the incoming pane to the new geometry.
        (pane-reposition src-pane px py pw ph)
        ;; Wire into the tree: replace the active leaf with a split.
        (let ((new-split (make-layout-split direction active-leaf
                                            (make-layout-leaf src-pane) 1/2)))
          (%replace-in-tree dst-window active-leaf new-split)
          (setf (window-panes dst-window)
                (layout-leaves (window-tree dst-window)))
          (window-relayout dst-window (window-height dst-window) (window-width dst-window))
          src-pane)))))

;;; ── pipe-pane ───────────────────────────────────────────────────────────────
;;;
;;; pipe_pane(Pane, Cmd) :-
;;;   (existing_pipe(Pane) -> close_pipe(Pane) ; true),
;;;   (Cmd \= nil -> open_pipe(Pane, Cmd) ; true).

(defun pipe-pane-open (pane command)
  "Tee PANE's PTY output to a pipe connected to COMMAND.
   If PANE already has an open pipe, it is closed first.
   Returns the pipe write-fd on success, NIL on failure."
  ;; Close any existing pipe.
  (when (pane-pipe-fd pane)
    (pipe-pane-close pane))
  ;; Open a new pipe to the command.
  (handler-case
      (let* ((shell cl-tmux/config:*default-shell*)
             (proc  (uiop:launch-program (list shell "-c" command)
                                         :input :stream :output nil
                                         :error-output nil))
             (stream (uiop:process-info-input proc)))
        (setf (pane-pipe-fd pane) stream)
        stream)
    (error () nil)))

(defun pipe-pane-close (pane)
  "Close PANE's output pipe if one is open."
  (when (pane-pipe-fd pane)
    (ignore-errors (close (pane-pipe-fd pane)))
    (setf (pane-pipe-fd pane) nil)))

(defun pipe-pane-write (pane bytes)
  "Write BYTES to PANE's output pipe if one is active.
   Silently ignores write errors (pipe may have closed on the other end)."
  (when (pane-pipe-fd pane)
    (ignore-errors
      (write-sequence bytes (pane-pipe-fd pane))
      (force-output (pane-pipe-fd pane)))))

;;; ── send-keys-to-pane ───────────────────────────────────────────────────────
;;;
;;; send_keys_to_pane(Pane, String) :-
;;;   pane_fd(Pane, Fd),
;;;   Fd > -1,
;;;   forall(char(Ch, String), write_byte(Fd, Ch)).

(defun send-keys-to-pane (pane string)
  "Write each character of STRING as a UTF-8 byte sequence to PANE's PTY fd.
   Silently ignores the write when PANE has no open PTY (fd <= -1)."
  (when (and pane (> (pane-fd pane) -1))
    (let ((bytes (babel:string-to-octets string :encoding :utf-8)))
      (pty-write (pane-fd pane) bytes))))

;;; ── Shell ──────────────────────────────────────────────────────────────────
;;;
;;; run_shell(cmd)            :- subprocess(cmd, timeout=30, output=string).
;;; if_shell(cmd, then, else) :- subprocess(cmd), exit_code=0 -> then ; else.
;;;
;;; Both run-shell and if-shell accept an optional :timeout keyword (seconds).
;;; The foreground (synchronous) paths honour the timeout via a bordeaux-threads
;;; helper; background tasks are fire-and-forget.
;;;
;;; uiop:run-program is used instead of sb-ext:run-program so the code is
;;; portable across all ASDF-supported implementations.
;;;
;;; if-shell is exported and wired to the :if-shell dispatch key in dispatch.lisp
;;; so it is reachable from the prefix-key handler.

(defun %run-with-timeout (thunk timeout-seconds)
  "Run THUNK in a fresh thread; join it up to TIMEOUT-SECONDS.
   Returns (funcall thunk) result or NIL if the timeout expires."
  (handler-case
      (bt:with-timeout (timeout-seconds)
        (funcall thunk))
    (bt:timeout () nil)))

(defun run-shell (command &key background (timeout 30))
  "Run COMMAND in a subshell.  Returns the output string (stdout) when BACKGROUND
   is nil, or T immediately when BACKGROUND is T.
   Uses *default-shell* for the shell binary.
   TIMEOUT (seconds, default 30) limits how long a synchronous command may run;
   when the limit is exceeded NIL is returned."
  (let ((shell (or *default-shell* "/bin/sh")))
    (if background
        (progn
          (bt:make-thread
            (lambda ()
              (uiop:run-program (list shell "-c" command)
                                :output nil :ignore-error-status t))
            :name "shell-bg")
          t)
        (%run-with-timeout
          (lambda ()
            (uiop:run-program (list shell "-c" command)
                              :output :string :ignore-error-status t))
          timeout))))

(defun if-shell (command then-fn &optional else-fn &key (timeout 30))
  "Run COMMAND; call THEN-FN if exit code is 0, ELSE-FN otherwise.
   THEN-FN and ELSE-FN are zero-argument functions.
   TIMEOUT (seconds, default 30) limits how long the command may run;
   when the limit is exceeded ELSE-FN is called."
  (let* ((shell (or *default-shell* "/bin/sh"))
         (exit-code (%run-with-timeout
                      (lambda ()
                        (multiple-value-bind (_ __ code)
                            (uiop:run-program (list shell "-c" command)
                                              :output nil :ignore-error-status t)
                          (declare (ignore _ __))
                          code))
                      timeout)))
    (if (and exit-code (zerop exit-code))
        (when then-fn (funcall then-fn))
        (when else-fn (funcall else-fn)))))
