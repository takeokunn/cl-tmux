(in-package #:cl-tmux/commands)

;;; ── capture-pane -e: reconstruct SGR escapes from cell attributes ───────────
;;;
;;; A self-contained cell→SGR encoder (the commands layer must not depend on the
;;; renderer).  capture-pane -e emits these so a captured buffer keeps its colours
;;; when re-displayed (e.g. the `capture-pane -ep` idiom, or session-restore tools).

;;; Cell attribute bit → SGR code mapping.
;;; Mirrors the renderer's cell-attr table — changes to that table must be
;;; reflected here.
(defconstant +attr-bit-bold+          0 "Cell attribute bit for bold (SGR 1).")
(defconstant +attr-bit-dim+           1 "Cell attribute bit for dim (SGR 2).")
(defconstant +attr-bit-reverse+       2 "Cell attribute bit for reverse video (SGR 7).")
(defconstant +attr-bit-underline+     3 "Cell attribute bit for underline (SGR 4).")
(defconstant +attr-bit-blink+         4 "Cell attribute bit for blink (SGR 5).")
(defconstant +attr-bit-italic+        5 "Cell attribute bit for italic (SGR 3).")
(defconstant +attr-bit-conceal+       6 "Cell attribute bit for conceal (SGR 8).")
(defconstant +attr-bit-strikethrough+ 7 "Cell attribute bit for strikethrough (SGR 9).")

(defparameter *capture-sgr-attr-codes*
  `((,+attr-bit-bold+          . 1)
    (,+attr-bit-dim+           . 2)
    (,+attr-bit-reverse+       . 7)
    (,+attr-bit-underline+     . 4)
    (,+attr-bit-blink+         . 5)
    (,+attr-bit-italic+        . 3)
    (,+attr-bit-conceal+       . 8)
    (,+attr-bit-strikethrough+ . 9))
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
    ;; Branch 0: terminal default colour (SGR 39 fg / 49 bg).
    ((= color cl-tmux/terminal/types:+default-color+) (if is-bg "49" "39"))
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

(defstruct (capture-pane-snapshot
            (:constructor %make-capture-pane-snapshot
                (&key scrollback-rows scrollback-wrapped visible-rows
                      wrapped-flags)))
  "Pure snapshot data captured from a screen under lock and rendered later."
  scrollback-rows
  scrollback-wrapped
  visible-rows
  wrapped-flags)

(defun %capture-pane-row-string (screen row escapes trim)
  "Return one visible row from SCREEN as either plain text or SGR-attributed text."
  (if escapes
      (%screen-row-string-sgr screen row trim)
      (%screen-row-string screen row trim)))

(defun %capture-pane-scrollback-row-string (row-cells escapes trim)
  "Return one scrollback row as either plain text or SGR-attributed text."
  (if escapes
      (%scrollback-row-string-sgr row-cells trim)
      (%scrollback-row-string row-cells trim)))

(defun %capture-pane-snapshot (screen include-scrollback escapes join trim)
  "Collect all capture-pane source data from SCREEN while holding its lock."
  (with-lock-held ((screen-lock screen))
    (%make-capture-pane-snapshot
     :scrollback-rows (when include-scrollback
                        (reverse (screen-scrollback screen)))
     :scrollback-wrapped (when (and include-scrollback join)
                           (reverse (screen-scrollback-wrapped screen)))
     :visible-rows (loop for row from 0 below (screen-height screen)
                         collect (%capture-pane-row-string screen row escapes trim))
     :wrapped-flags (when join
                      (loop for row from 0 below (screen-height screen)
                            collect (cl-tmux/terminal/types:%line-wrapped-p screen row))))))

(defun %emit-capture-pane-snapshot (snapshot join escapes trim)
  "Render a SNAPSHOT to a string outside the screen lock."
  (with-output-to-string (out)
    ;; -J joins wrapped SCROLLBACK rows too (the flag travels with the row
    ;; when it scrolls into history); the last history row may continue into
    ;; the first visible row.  Screens whose scrollback was built without
    ;; flags (tests) fall back to un-joined emission via the NIL default.
    (let ((wflags (capture-pane-snapshot-scrollback-wrapped snapshot)))
      (dolist (row-cells (capture-pane-snapshot-scrollback-rows snapshot))
        (write-string (%capture-pane-scrollback-row-string row-cells escapes trim) out)
        (unless (and join (pop wflags))
          (terpri out))))
    (if join
        ;; -J: suppress the newline between a wrapped row and its continuation.
        (loop for rows on (capture-pane-snapshot-visible-rows snapshot)
              for wrapped in (capture-pane-snapshot-wrapped-flags snapshot)
              do (write-string (first rows) out)
                 (unless (and wrapped (rest rows)) (terpri out)))
        (dolist (row-str (capture-pane-snapshot-visible-rows snapshot))
          (write-string row-str out)
          (terpri out)))))

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
    (%emit-capture-pane-snapshot
     (%capture-pane-snapshot screen include-scrollback escapes join trim)
     join escapes trim)))
