(in-package #:cl-tmux/commands)

;;; ── Pane operations ────────────────────────────────────────────────────────
;;;
;;; swap_pane(Window, Dir)   :- active(Window, AP), neighbor(AP, Dir, Other),
;;;                              swap_positions(AP, Other), swap_list_order(AP, Other).
;;; capture_pane(Pane, Opts) :- lock(screen(Pane)),
;;;                              (scrollback(Opts) -> emit_scrollback ; true),
;;;                              emit_visible_rows.

(defun %swap-pane-geometry (active-pane other)
  "Exchange the screen positions of two panes ACTIVE-PANE and OTHER in-place.
   Updates both pane structs so the renderer sees the swapped layout immediately."
  (let ((saved-x      (pane-x      active-pane))
        (saved-y      (pane-y      active-pane))
        (saved-width  (pane-width  active-pane))
        (saved-height (pane-height active-pane)))
    (pane-reposition active-pane
                     (pane-x other) (pane-y other)
                     (pane-width other) (pane-height other))
    (pane-reposition other saved-x saved-y saved-width saved-height)))

(defun swap-two-panes (window pane-a pane-b)
  "Swap PANE-A and PANE-B within WINDOW: exchange both their list positions and
   their screen geometry, so the renderer sees the swap immediately.  No-op
   (returns NIL) when either pane is missing, they are the same, or either is not
   in WINDOW.  Does NOT change which pane is active.  Returns PANE-A on success."
  (let* ((panes (window-panes window))
         (ia    (and pane-a (position pane-a panes)))
         (ib    (and pane-b (position pane-b panes))))
    (when (and ia ib (/= ia ib))
      (let ((new-panes (copy-list panes)))
        (setf (nth ia new-panes) pane-b
              (nth ib new-panes) pane-a
              (window-panes window) new-panes))
      (%swap-pane-geometry pane-a pane-b)
      pane-a)))

(defun swap-pane (window direction)
  "Swap the active pane with an adjacent pane in WINDOW.
   DIRECTION:
     :right / :forward  — next in panes list (wraps around)
     :left  / :backward — previous in panes list (wraps around)
     :up    — spatially adjacent pane above (via pane-neighbor)
     :down  — spatially adjacent pane below (via pane-neighbor)
   Swaps both list order and screen geometry (via swap-two-panes)."
  (let* ((panes (window-panes window))
         (ap    (window-active-pane window))
         (idx   (position ap panes))
         (n     (length panes)))
    (when (> n 1)
      (let ((other
             (ecase direction
               ((:right :forward)
                (nth (mod (1+ idx) n) panes))
               ((:left :backward)
                (nth (mod (1- idx) n) panes))
               (:up   (pane-neighbor window ap :up))
               (:down (pane-neighbor window ap :down)))))
        (when other
          (swap-two-panes window ap other))))))

;;; ── capture-pane -e: reconstruct SGR escapes from cell attributes ───────────
;;;
;;; A self-contained cell→SGR encoder (the commands layer must not depend on the
;;; renderer).  capture-pane -e emits these so a captured buffer keeps its colours
;;; when re-displayed (e.g. the `capture-pane -ep` idiom, or session-restore tools).

;;; Cell attribute bit → SGR code mapping.
;;; Bit 0 = bold (SGR 1), bit 1 = dim (SGR 2), bit 2 = reverse (SGR 7),
;;; bit 3 = underline (SGR 4), bit 4 = blink (SGR 5), bit 5 = italic (SGR 3),
;;; bit 6 = conceal (SGR 8), bit 7 = strikethrough (SGR 9).
;;; Mirrors the renderer's cell-attr table — changes to that table must be
;;; reflected here.
(defparameter *capture-sgr-attr-codes*
  '((0 . 1) (1 . 2) (2 . 7) (3 . 4) (4 . 5) (5 . 3) (6 . 8) (7 . 9))
  "Cell attribute bit → SGR code: bold/dim/reverse/underline/blink/italic/
   conceal/strikethrough (mirrors the renderer's cell-attr table).")

;;; Bit 24 of a cell color value indicates a 24-bit true-colour (0xRRGGBB packed
;;; into the low 24 bits).  Values below this threshold use the 256-colour palette
;;; or the 16 standard/bright colours.
(defconstant +true-color-bit+ #x1000000
  "Flag bit in a cell color integer indicating a 24-bit true-colour value.
   When set, the low 24 bits encode R (bits 23-16), G (bits 15-8), B (bits 7-0).")

(defun %capture-color-sgr (color is-bg)
  "SGR parameter fragment (a string) for a cell COLOR value; IS-BG selects the
   background variant.  Handles 0-7 (standard), 8-15 (bright), 16-255 (256-colour)
   and +true-color-bit+ true-colour, matching the cell colour encoding."
  (cond
    ;; Branch 1: 24-bit true-colour — +true-color-bit+ set, RGB in low 24 bits.
    ((>= color +true-color-bit+)
     (format nil "~D;2;~D;~D;~D" (if is-bg 48 38)
             (ldb (byte 8 16) color) (ldb (byte 8 8) color) (ldb (byte 8 0) color)))
    ;; Branch 2: standard ANSI colours 0-7 (30-37 fg, 40-47 bg).
    ((<= 0 color 7)   (format nil "~D" (+ color (if is-bg 40 30))))
    ;; Branch 3: bright colours 8-15 (90-97 fg, 100-107 bg).
    ((<= 8 color 15)  (format nil "~D" (+ (- color 8) (if is-bg 100 90))))
    ;; Branch 4: 256-colour palette (SGR 38;5;N or 48;5;N).
    (t                (format nil "~D;5;~D" (if is-bg 48 38) color))))

(defun %capture-cell-sgr (fg bg attrs)
  "Full SGR escape (reset + this cell's attributes and colours) for capture -e."
  (with-output-to-string (s)
    (format s "~C[0" #\Escape)            ; reset baseline, then re-apply
    (loop for (bit . code) in *capture-sgr-attr-codes*
          when (logbitp bit attrs) do (format s ";~D" code))
    (format s ";~A;~A" (%capture-color-sgr fg nil) (%capture-color-sgr bg t))
    (write-char #\m s)))

(defun %row-content-width (cell-at full-width trim)
  "The number of columns to emit for a row of FULL-WIDTH cells (CELL-AT: col → cell).
   When TRIM is true (capture-pane default), trailing blank cells (space character)
   are dropped — tmux strips trailing whitespace from each captured line.  When TRIM
   is NIL (capture-pane -J), the full width is kept so trailing spaces are preserved.
   An all-blank row trims to width 0 (an empty captured line)."
  (if (not trim)
      full-width
      (loop for col from (1- full-width) downto 0
            unless (char= (cell-char (funcall cell-at col)) #\Space)
              return (1+ col)
            finally (return 0))))

(defun %cells-to-sgr-string (cell-at width)
  "Build a row string with SGR escapes from WIDTH cells, CELL-AT being a function
   col → cell.  Emits a full SGR sequence whenever the colour/attrs change, then
   the character, and a trailing reset.  Shared by visible and scrollback rows.
   WIDTH is the already-trimmed content width (see %row-content-width); a zero
   width yields the empty string (no stray reset on a blank line)."
  (if (zerop width)
      ""
      (with-output-to-string (out)
        (let ((prev nil))
          (dotimes (col width)
            (let* ((cell (funcall cell-at col))
                   (key  (list (cell-fg cell) (cell-bg cell) (cell-attrs cell))))
              (unless (equal key prev)
                (write-string (%capture-cell-sgr (cell-fg cell) (cell-bg cell) (cell-attrs cell))
                              out)
                (setf prev key))
              (write-char (cell-char cell) out)))
          (format out "~C[0m" #\Escape)))))

(defun %build-row-string-sgr (cell-at full-width &optional (trim t))
  "Build a SGR-attributed row string from CELL-AT over %row-content-width columns.
   Parallel to %build-row-string for the escapes=t case."
  (%cells-to-sgr-string cell-at (%row-content-width cell-at full-width trim)))

(defun %screen-row-string-sgr (screen row &optional (trim t))
  "Visible-row string with SGR escapes (capture-pane -e)."
  (%build-row-string-sgr (lambda (col) (screen-cell screen col row)) (screen-width screen) trim))

(defun %scrollback-row-string-sgr (cell-vector &optional (trim t))
  "Scrollback-row string with SGR escapes (capture-pane -e)."
  (%build-row-string-sgr (lambda (col) (aref cell-vector col)) (length cell-vector) trim))

(defun %build-row-string (cell-at full-width trim)
  "Build a plain string from CELL-AT over width computed by %row-content-width."
  (let* ((width  (%row-content-width cell-at full-width trim))
         (result (make-string width)))
    (dotimes (col width result)
      (setf (char result col) (cell-char (funcall cell-at col))))))

(defun %screen-row-string (screen row &optional (trim t))
  "Return a string representing ROW in SCREEN's visible grid, character per cell.
   When TRIM (the capture-pane default), trailing blank cells are dropped; when
   NIL (capture-pane -J) the full width is kept.  Pure data-to-string conversion."
  (%build-row-string (lambda (col) (screen-cell screen col row)) (screen-width screen) trim))

(defun %scrollback-row-string (cell-vector &optional (trim t))
  "Return a string of characters from a scrollback row CELL-VECTOR (a simple-vector
   of cells).  TRIM drops trailing blank cells (capture-pane default)."
  (%build-row-string (lambda (col) (aref cell-vector col)) (length cell-vector) trim))

(defun capture-pane (pane &key (include-scrollback nil) (escapes nil) (join nil)
                               (preserve-trailing nil))
  "Dump the visible content of PANE as a string.
   When INCLUDE-SCROLLBACK is T, also include scrollback history above the visible area.
   When ESCAPES is T (capture-pane -e), each row is rendered with SGR escape
   sequences so colours/attributes are preserved; otherwise plain characters.
   When JOIN is T (capture-pane -J), trailing spaces on each line are PRESERVED and
   VISIBLE lines that wrapped at the right margin are rejoined into one logical
   line (no newline at the wrap boundary), using the screen's per-row wrap flags.
   When PRESERVE-TRAILING is T (capture-pane -N), trailing spaces are PRESERVED but
   wrapped lines are NOT joined — the difference from -J.  Either flag disables the
   default trailing-whitespace trimming; only JOIN rejoins wrapped rows.
   Otherwise — tmux's default — trailing whitespace is stripped and every row ends
   with a newline.  (Scrollback rows carry no wrap flag, so -J does not join across
   the scrollback/visible boundary or within scrollback.)
   The screen lock is held only for snapshot extraction; string rendering happens
   outside the lock so renderer threads are not blocked during I/O."
  (let ((screen (pane-screen pane))
        ;; Default trims trailing spaces; -J (join) and -N (preserve-trailing) both
        ;; keep them — only -J additionally rejoins wrapped rows.
        (trim   (not (or join preserve-trailing))))
    ;; Snapshot pure data under lock (I/O-synchronisation concern).
    (let ((scrollback-snapshot nil)
          (visible-rows nil)
          (wrapped-flags nil))
      (with-lock-held ((screen-lock screen))
        (when include-scrollback
          (setf scrollback-snapshot (reverse (screen-scrollback screen))))
        (setf visible-rows
              (loop for row from 0 below (screen-height screen)
                    collect (if escapes
                                (%screen-row-string-sgr screen row trim)
                                (%screen-row-string screen row trim))))
        (when join
          (setf wrapped-flags
                (loop for row from 0 below (screen-height screen)
                      collect (cl-tmux/terminal/types:%line-wrapped-p screen row)))))
      ;; Render to string outside the lock (pure I/O).
      (with-output-to-string (out)
        (dolist (row-cells scrollback-snapshot)
          (write-string (if escapes
                            (%scrollback-row-string-sgr row-cells trim)
                            (%scrollback-row-string row-cells trim))
                        out)
          (terpri out))
        (if join
            ;; -J: suppress the newline between a wrapped row and its continuation.
            (loop for rows on visible-rows
                  for wrapped in wrapped-flags
                  do (write-string (first rows) out)
                     (unless (and wrapped (rest rows)) (terpri out)))
            (dolist (row-str visible-rows)
              (write-string row-str out)
              (terpri out)))))))

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

(defun break-pane (session &key src-window pane name (select t))
  "Remove PANE from SRC-WINDOW and place it as the sole pane of a new window.
   SRC-WINDOW defaults to the session's active window and PANE to that window's
   active pane (the no-argument interactive behaviour).  NAME, when given, names
   the new window (break-pane -n); otherwise the shell basename is used.  When
   SELECT is true (default) the session switches to the new window; NIL leaves the
   current window active (break-pane -d).  When the source window has only one
   pane, break-pane is a no-op.  Returns the new window, or NIL."
  (let* ((src-win (or src-window (session-active-window session)))
         (pane    (or pane (and src-win (window-active-pane src-win)))))
    (when (and src-win pane (>= (length (window-panes src-win)) 2))
      ;; Remove pane from its current window (collapses the tree).
    (window-remove-pane src-win pane)
    ;; After removal, re-select a pane in the source window.
    (when (window-panes src-win)
      (window-select-pane src-win (first (window-panes src-win))))
    ;; Create a new window with the pane as the sole full-screen occupant.
    ;; Use the lowest free window id (same rule as session-new-window).
    (let* ((rows    (window-height src-win))
           (cols    (window-width  src-win))
           (new-id  (cl-tmux/model::%next-window-id session))
           (wname   (or name (cl-tmux/model::%shell-basename)))
           (new-win (make-window :id new-id :name wname :width cols :height rows)))
      ;; Install the pane as the sole leaf in the new window's tree.
      (setf (window-panes new-win) (list pane)
            (window-tree  new-win) (make-layout-leaf pane)
            (pane-window  pane)    new-win)
      (window-select-pane new-win pane)
      ;; Reposition the pane to fill the new window.
      (pane-reposition pane 0 0 cols rows)
      ;; Attach the new window to the session via the model-layer helper.
      (session-insert-window session new-win)
      (when select (session-select-window session new-win))
      (run-hooks +hook-after-new-window+ new-win)
      new-win))))

;;; ── join-pane / move-pane ───────────────────────────────────────────────────
;;;
;;; join_pane(Session, SrcWin, SrcPane, DstWin, Dir) :-
;;;   remove_pane(SrcWin, SrcPane),
;;;   (empty(SrcWin) -> kill_window(Session, SrcWin) ; true),
;;;   insert_by_split(DstWin, SrcPane, Dir).

(defun %join-pane-kill-empty-src (session src-window)
  "Remove SRC-WINDOW from SESSION when it has no panes remaining.
   Switches the active window to the first surviving window if needed."
  (when (null (window-panes src-window))
    (let ((remaining (remove src-window (session-windows session))))
      (setf (session-windows session) remaining)
      (when (eq (session-active-window session) src-window)
        (session-select-window session (first remaining))))))

(defun %join-pane-insert-into-dst (src-pane dst-window direction)
  "Insert SRC-PANE into DST-WINDOW as a DIRECTION split.
   Returns SRC-PANE on success, NIL when the destination has no active leaf."
  (let* ((active      (window-active-pane dst-window))
         (tree        (window-tree dst-window))
         (active-leaf (and active tree (layout-find-leaf tree active))))
    (when active-leaf
      (multiple-value-bind (px py pw ph)
          (split-child-geometry active direction)
        (pane-reposition src-pane px py pw ph)
        (let ((new-split (make-layout-split direction active-leaf
                                            (make-layout-leaf src-pane) 1/2)))
          (cl-tmux/model::%replace-in-tree dst-window active-leaf new-split)
          (setf (window-panes dst-window)
                (layout-leaves (window-tree dst-window))
                (pane-window src-pane) dst-window)
          (window-relayout dst-window
                           (window-height dst-window)
                           (window-width  dst-window))
          src-pane)))))

(defun join-pane (session src-window src-pane dst-window direction)
  "Move SRC-PANE from SRC-WINDOW into DST-WINDOW as a split in DIRECTION.
   DIRECTION is :h (left/right) or :v (top/bottom).
   If SRC-WINDOW becomes empty after removal, it is killed.
   Returns SRC-PANE on success, NIL on failure."
  (when (and src-window src-pane dst-window)
    (window-remove-pane src-window src-pane)
    (%join-pane-kill-empty-src session src-window)
    (%join-pane-insert-into-dst src-pane dst-window direction)))

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

