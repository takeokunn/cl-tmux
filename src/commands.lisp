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
    (unless (> n 1) (return-from swap-pane nil))
    (let ((other
           (ecase direction
             ((:right :forward)
              (nth (mod (1+ idx) n) panes))
             ((:left :backward)
              (nth (mod (1- idx) n) panes))
             (:up   (pane-neighbor window ap :up))
             (:down (pane-neighbor window ap :down)))))
      (when other
        (swap-two-panes window ap other)))))

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
;;; reflected here.  Using eval-when + defconstant avoids the list non-EQL
;;; redefinition error while communicating immutability to readers.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +capture-sgr-attr-codes+
    (if (boundp '+capture-sgr-attr-codes+)
        (symbol-value '+capture-sgr-attr-codes+)
        '((0 . 1) (1 . 2) (2 . 7) (3 . 4) (4 . 5) (5 . 3) (6 . 8) (7 . 9)))
    "Cell attribute bit → SGR code: bold/dim/reverse/underline/blink/italic/
   conceal/strikethrough (mirrors the renderer's cell-attr table)."))

(defun %capture-color-sgr (color is-bg)
  "SGR parameter fragment (a string) for a cell COLOR value; IS-BG selects the
   background variant.  Handles 0-7 (standard), 8-15 (bright), 16-255 (256-colour)
   and bit-24 true-colour, matching the cell colour encoding."
  (cond
    ((>= color #x1000000)                 ; true-colour: bit 24 set, RGB in low 24
     (format nil "~D;2;~D;~D;~D" (if is-bg 48 38)
             (ldb (byte 8 16) color) (ldb (byte 8 8) color) (ldb (byte 8 0) color)))
    ((<= 0 color 7)   (format nil "~D" (+ color (if is-bg 40 30))))
    ((<= 8 color 15)  (format nil "~D" (+ (- color 8) (if is-bg 100 90))))
    (t                (format nil "~D;5;~D" (if is-bg 48 38) color))))

(defun %capture-cell-sgr (fg bg attrs)
  "Full SGR escape (reset + this cell's attributes and colours) for capture -e."
  (with-output-to-string (s)
    (format s "~C[0" #\Escape)            ; reset baseline, then re-apply
    (loop for (bit . code) in +capture-sgr-attr-codes+
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

(defun %screen-row-string-sgr (screen row &optional (trim t))
  "Visible-row string with SGR escapes (capture-pane -e)."
  (let ((cell-at (lambda (col) (screen-cell screen col row))))
    (%cells-to-sgr-string cell-at
                          (%row-content-width cell-at (screen-width screen) trim))))

(defun %scrollback-row-string-sgr (cell-vector &optional (trim t))
  "Scrollback-row string with SGR escapes (capture-pane -e)."
  (let ((cell-at (lambda (col) (aref cell-vector col))))
    (%cells-to-sgr-string cell-at
                          (%row-content-width cell-at (length cell-vector) trim))))

(defun %screen-row-string (screen row &optional (trim t))
  "Return a string representing ROW in SCREEN's visible grid, character per cell.
   When TRIM (the capture-pane default), trailing blank cells are dropped; when
   NIL (capture-pane -J) the full width is kept.  Pure data-to-string conversion."
  (let* ((cell-at (lambda (col) (screen-cell screen col row)))
         (width   (%row-content-width cell-at (screen-width screen) trim))
         (result  (make-string width)))
    (dotimes (col width result)
      (setf (char result col) (cell-char (funcall cell-at col))))))

(defun %scrollback-row-string (cell-vector &optional (trim t))
  "Return a string of characters from a scrollback row CELL-VECTOR (a simple-vector
   of cells).  TRIM drops trailing blank cells (capture-pane default)."
  (let* ((cell-at (lambda (col) (aref cell-vector col)))
         (n       (%row-content-width cell-at (length cell-vector) trim))
         (result  (make-string n)))
    (dotimes (i n result)
      (setf (char result i) (cell-char (funcall cell-at i))))))

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
    (let* ((rows    (window-height src-win))
           (cols    (window-width  src-win))
           (new-id  (cl-tmux/model::%next-window-id session))
           (wname   (or name (cl-tmux/model::%shell-basename)))
           (new-win (make-window :id new-id :name wname :width cols :height rows)))
      ;; Install the pane as the sole leaf in the new window's tree.
      (setf (window-panes new-win) (list pane)
            (window-tree  new-win) (make-layout-leaf pane))
      (window-select-pane new-win pane)
      ;; Reposition the pane to fill the new window.
      (pane-reposition pane 0 0 cols rows)
      ;; Attach the new window to the session via the model-layer helper.
      (session-insert-window session new-win)
      (when select (session-select-window session new-win))
      (run-hooks +hook-after-new-window+ new-win)
      new-win)))

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
                (layout-leaves (window-tree dst-window)))
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

;;; ── send-keys key-name translation ──────────────────────────────────────────
;;;
;;; tmux's send-keys interprets arguments that name a key (Enter, Tab, Up, C-c,
;;; M-x, F5, ...) and sends that key's byte sequence rather than the literal
;;; text.  *send-key-names* is the named-key table; C-<x> (control) and M-<x>
;;; (meta/alt) are handled algorithmically.  Escape sequences use the normal
;;; (non-application) xterm encodings, matching what send-keys emits by default.

;; String constants are not EQL-able, so DEFCONSTANT causes SBCL to signal a
;; redefinition error every time the fasl is loaded.  DEFVAR avoids the check.
(defvar +escape-string+ (load-time-value (string (code-char 27)) t)
  "The ESC character (ASCII 27) as a single-character string.")

(defun %escape-sequence (&rest tail)
  "Build a string beginning with ESC followed by TAIL strings concatenated."
  (apply #'concatenate 'string +escape-string+ tail))

(defparameter *send-key-names*
  (list
   ;; whitespace / control
   (cons "Enter"  (string #\Return)) (cons "C-m" (string #\Return))
   (cons "Tab"    (string #\Tab))    (cons "C-i" (string #\Tab))
   (cons "Space"  " ")
   (cons "Escape" +escape-string+)   (cons "Esc" +escape-string+)
   (cons "BSpace" (string (code-char 127)))
   (cons "BTab"   (%escape-sequence "[Z"))
   ;; arrows (normal cursor mode)
   (cons "Up"     (%escape-sequence "[A")) (cons "Down"  (%escape-sequence "[B"))
   (cons "Right"  (%escape-sequence "[C")) (cons "Left"  (%escape-sequence "[D"))
   ;; navigation block
   (cons "Home"     (%escape-sequence "[H")) (cons "End"      (%escape-sequence "[F"))
   (cons "PageUp"   (%escape-sequence "[5~")) (cons "PPage"   (%escape-sequence "[5~"))
   (cons "PageDown" (%escape-sequence "[6~")) (cons "NPage"   (%escape-sequence "[6~"))
   (cons "Insert"   (%escape-sequence "[2~")) (cons "IC"      (%escape-sequence "[2~"))
   (cons "Delete"   (%escape-sequence "[3~")) (cons "DC"      (%escape-sequence "[3~"))
   ;; function keys
   (cons "F1"  (%escape-sequence "OP"))   (cons "F2"  (%escape-sequence "OQ"))
   (cons "F3"  (%escape-sequence "OR"))   (cons "F4"  (%escape-sequence "OS"))
   (cons "F5"  (%escape-sequence "[15~")) (cons "F6"  (%escape-sequence "[17~"))
   (cons "F7"  (%escape-sequence "[18~")) (cons "F8"  (%escape-sequence "[19~"))
   (cons "F9"  (%escape-sequence "[20~")) (cons "F10" (%escape-sequence "[21~"))
   (cons "F11" (%escape-sequence "[23~")) (cons "F12" (%escape-sequence "[24~")))
  "Alist mapping tmux key-name strings to their literal byte sequence (as a
   string whose char-codes are the bytes — all < 128).")

(defparameter *modified-send-keys*
  '(;; letter-final keys → ESC [ 1 ; <mod> <final>
    ("Up" :letter #\A) ("Down" :letter #\B) ("Right" :letter #\C) ("Left" :letter #\D)
    ("Home" :letter #\H) ("End" :letter #\F)
    ("F1" :letter #\P) ("F2" :letter #\Q) ("F3" :letter #\R) ("F4" :letter #\S)
    ;; tilde keys → ESC [ <param> ; <mod> ~
    ("F5" :tilde 15) ("F6" :tilde 17) ("F7" :tilde 18) ("F8" :tilde 19)
    ("F9" :tilde 20) ("F10" :tilde 21) ("F11" :tilde 23) ("F12" :tilde 24)
    ("PageUp" :tilde 5) ("PPage" :tilde 5) ("PageDown" :tilde 6) ("NPage" :tilde 6)
    ("Insert" :tilde 2) ("IC" :tilde 2) ("Delete" :tilde 3) ("DC" :tilde 3))
  "Base special keys that take a CSI modifier, with the byte-sequence shape used
   when a modifier is present.  :letter keys encode as ESC [ 1 ; <mod> <final>;
   :tilde keys as ESC [ <param> ; <mod> ~.  The inverse of the event loop's
   modifier decoding, so send-keys C-Up round-trips with `bind -n C-Up`.")

(defun %split-key-modifiers (name)
  "Strip leading C-/M-/S- modifier prefixes from NAME.  Returns (values MOD-VALUE
   BASE): MOD-VALUE is the CSI modifier code (1 + Shift + 2·Alt + 4·Ctrl), 1 when
   no modifier prefix is present; BASE is the remaining key name."
  (let ((bits 0) (i 0) (len (length name)))
    (loop while (and (<= (+ i 2) len) (char= (char name (1+ i)) #\-))
          for m = (char-upcase (char name i))
          do (case m
               (#\C (setf bits (logior bits 4)))
               (#\M (setf bits (logior bits 2)))
               (#\S (setf bits (logior bits 1)))
               (otherwise (return)))
             (incf i 2))
    (values (1+ bits) (subseq name i))))

(defun %modified-special-key-string (name)
  "Escape string for a modified special key NAME (C-Up → ESC[1;5A, S-F5 →
   ESC[15;2~, C-M-Left → ESC[1;7D), or NIL when NAME is not a modified special
   key.  Modifiers map through %split-key-modifiers; the base must be a key in
   *modified-send-keys* and at least one modifier must be present."
  (multiple-value-bind (mod-value base) (%split-key-modifiers name)
    (when (> mod-value 1)
      (let ((entry (assoc base *modified-send-keys* :test #'string=)))
        (when entry
          (%escape-sequence
           (ecase (second entry)
             (:letter (format nil "[1;~D~C" mod-value (third entry)))
             (:tilde  (format nil "[~D;~D~~" (third entry) mod-value)))))))))

(defun %key-name-to-bytes (name)
  "Return the octet vector for a tmux key NAME (Enter, Tab, Up, C-c, M-x, F5,
   C-Up, S-F5...), or NIL when NAME is not a recognised key.
   C-<char> → the control byte (logand char #x1f); M-<char> → ESC then <char>;
   <mods>-<special> → the modified CSI sequence (see %modified-special-key-string)."
  (let ((entry    (assoc name *send-key-names* :test #'string=))
        (modified (%modified-special-key-string name)))
    (cond
      (entry
       (babel:string-to-octets (cdr entry) :encoding :utf-8))
      ;; Modified special key (C-Up, S-F5, C-M-Left) before the C-/M-<char> paths.
      (modified
       (babel:string-to-octets modified :encoding :utf-8))
      ;; C-<char>: control byte.  C-a..C-z → 1..26, C-@ → 0, C-[ → 27, ...
      ((and (= (length name) 3) (string= (subseq name 0 2) "C-"))
       (make-array 1 :element-type '(unsigned-byte 8)
                     :initial-element (logand (char-code (char-upcase (char name 2)))
                                              #x1f)))
      ;; M-<char>: ESC followed by the character (Alt/Meta).
      ((and (= (length name) 3) (string= (subseq name 0 2) "M-"))
       (babel:string-to-octets
        (concatenate 'string (string (code-char 27)) (subseq name 2))
        :encoding :utf-8))
      (t nil))))

;;; ── Command-string tokeniser ────────────────────────────────────────────────
;;;
;;; tmux command arguments are split shell-style: whitespace separates arguments,
;;; '...' is a literal span, "..." allows backslash escapes, and a bare \\ escapes
;;; the next character.  Adjacent spans join into one argument (foo"bar baz" →
;;; foobar baz).  This is the shared lexer behind multi-argument commands such as
;;; send-keys (and, in future, display-message / if-shell).

(defun %consume-single-quoted (string start length accumulator)
  "Consume a single-quoted literal span from STRING beginning at START.
   Writes characters into ACCUMULATOR stream up to the closing quote.
   Returns the index after the closing quote (or LENGTH when unterminated)."
  (let ((index (1+ start)))         ; skip the opening quote
    (loop while (and (< index length)
                     (char/= (char string index) #\'))
          do (write-char (char string index) accumulator)
             (incf index))
    (if (< index length) (1+ index) index))) ; skip closing quote when present

(defun %consume-double-quoted (string start length accumulator)
  "Consume a double-quoted span from STRING beginning at START.
   Inside double quotes a backslash followed by any character is an escape:
   only the escaped character is written.  Other characters are written verbatim.
   Returns the index after the closing quote (or LENGTH when unterminated)."
  (let ((index (1+ start)))         ; skip the opening quote
    (loop while (and (< index length)
                     (char/= (char string index) #\"))
          do (if (and (char= (char string index) #\\) (< (1+ index) length))
                 (progn (write-char (char string (1+ index)) accumulator)
                        (incf index 2))
                 (progn (write-char (char string index) accumulator)
                        (incf index))))
    (if (< index length) (1+ index) index))) ; skip closing quote when present

(defun tokenize-command-string (string)
  "Split STRING into a list of argument strings, shell-style.
   Whitespace separates arguments; '...' is a literal span; \"...\" allows \\
   escapes; a bare \\ escapes the next character; adjacent spans concatenate.
   Unterminated quotes are tolerated (consumed to end of string).  An explicitly
   quoted empty token (e.g. '') yields an empty-string argument."
  (let ((arguments   nil)
        (accumulator (make-string-output-stream))
        (in-arg      nil)
        (index       0)
        (length      (length string)))
    (flet ((flush-argument ()
             (when in-arg
               (push (get-output-stream-string accumulator) arguments)
               (setf in-arg nil))))
      (loop while (< index length)
            for character = (char string index)
            do (cond
                 ((member character '(#\Space #\Tab))
                  (flush-argument)
                  (incf index))
                 ((char= character #\')
                  (setf in-arg t
                        index (%consume-single-quoted string index length accumulator)))
                 ((char= character #\")
                  (setf in-arg t
                        index (%consume-double-quoted string index length accumulator)))
                 ((and (char= character #\\) (< (1+ index) length))
                  (setf in-arg t)
                  (write-char (char string (1+ index)) accumulator)
                  (incf index 2))
                 (t
                  (setf in-arg t)
                  (write-char character accumulator)
                  (incf index))))
      (flush-argument)
      (nreverse arguments))))

(defun %translate-send-keys (string)
  "Bytes that send-keys should write for the argument string STRING.  STRING is
   tokenised shell-style; each argument naming a tmux key (Enter, C-c, Up, F5,
   M-x, ...) contributes that key's byte sequence and every other argument
   contributes its literal UTF-8 bytes.  Matches tmux: spaces separate arguments
   unless quoted — `send-keys echo hi` sends \"echohi\", whereas
   `send-keys \"echo hi\" Enter` sends \"echo hi\" then CR."
  (let ((args (tokenize-command-string string)))
    (if (null args)
        (babel:string-to-octets string :encoding :utf-8)
        (apply #'concatenate '(vector (unsigned-byte 8))
               (mapcar (lambda (arg)
                         (or (%key-name-to-bytes arg)
                             (babel:string-to-octets arg :encoding :utf-8)))
                       args)))))

;;; ── send-keys-to-pane ───────────────────────────────────────────────────────

(defun send-keys-to-pane (pane string &key literal)
  "Write STRING to PANE's PTY.  STRING is parsed as send-keys arguments: each
   argument naming a tmux key (Enter, Tab, C-c, Up, F5, M-x, ...) is translated
   to its byte sequence, and other arguments are sent as literal UTF-8 text.
   When LITERAL is true (send-keys -l), STRING is written as raw UTF-8 bytes
   with NO key-name interpretation.
   No-op when PANE has no open PTY (fd <= -1)."
  (when (and pane (> (pane-fd pane) -1))
    (pty-write (pane-fd pane)
               (if literal
                   (babel:string-to-octets string :encoding :utf-8)
                   (%translate-send-keys string)))))

;;; ── Shell ──────────────────────────────────────────────────────────────────
;;;
;;; run_shell(cmd)            :- subprocess(cmd, timeout=30, output=string).
;;; if_shell(cmd, then, else) :- subprocess(cmd), exit_code=0 -> then ; else.
;;;
;;; Both run-shell and if-shell accept a :timeout keyword (seconds, default 30).
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

(defmacro with-shell-timeout ((shell-var timeout) &body body)
  "Bind SHELL-VAR to the active shell binary and run BODY with a TIMEOUT (seconds).
   TIMEOUT is evaluated at macro-expansion call time and passed directly to
   %RUN-WITH-TIMEOUT.  Returns the result of BODY or NIL when the timeout fires."
  `(%run-with-timeout
     (lambda ()
       (let ((,shell-var (or *default-shell* "/bin/sh")))
         ,@body))
     ,timeout))

(defun run-shell (command &key background (timeout 30))
  "Run COMMAND in a subshell.  Returns the output string (stdout) when BACKGROUND
   is nil, or T immediately when BACKGROUND is T.
   Uses *default-shell* for the shell binary.
   TIMEOUT (seconds, default 30) limits how long a synchronous command may run;
   when the limit is exceeded NIL is returned."
  (if background
      (progn
        ;; Deliberate no-timeout policy: background shell commands are fire-and-forget.
        ;; The caller requested asynchronous execution and does not need the result.
        ;; If a bounded background job is needed, the caller should wrap in bt:with-timeout.
        (bt:make-thread
          (lambda ()
            (let ((shell (or *default-shell* "/bin/sh")))
              (uiop:run-program (list shell "-c" command)
                                :output nil :ignore-error-status t)))
          :name "shell-bg")
        t)
      (with-shell-timeout (shell timeout)
        (uiop:run-program (list shell "-c" command)
                          :output :string :ignore-error-status t))))

(defun if-shell (command then-fn &key else-fn (timeout 30))
  "Run COMMAND; call THEN-FN if exit code is 0, ELSE-FN otherwise.
   THEN-FN and ELSE-FN are zero-argument functions (keyword arguments).
   TIMEOUT (seconds, default 30) limits how long the command may run;
   when the limit is exceeded ELSE-FN is called."
  (let ((exit-code
          (with-shell-timeout (shell timeout)
            (multiple-value-bind (output error-output code)
                (uiop:run-program (list shell "-c" command)
                                  :output nil :ignore-error-status t)
              (declare (ignore output error-output))
              code))))
    (if (and exit-code (zerop exit-code))
        (when then-fn (funcall then-fn))
        (when else-fn (funcall else-fn)))))
