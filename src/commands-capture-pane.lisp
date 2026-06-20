(in-package #:cl-tmux/commands)

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

(defstruct (capture-pane-snapshot
            (:constructor %make-capture-pane-snapshot
                (&key scrollback-rows visible-rows wrapped-flags)))
  "Pure snapshot data captured from a screen under lock and rendered later."
  scrollback-rows
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
     :visible-rows (loop for row from 0 below (screen-height screen)
                         collect (%capture-pane-row-string screen row escapes trim))
     :wrapped-flags (when join
                      (loop for row from 0 below (screen-height screen)
                            collect (cl-tmux/terminal/types:%line-wrapped-p screen row))))))

(defun %emit-capture-pane-snapshot (snapshot join escapes trim)
  "Render a SNAPSHOT to a string outside the screen lock."
  (with-output-to-string (out)
    (dolist (row-cells (capture-pane-snapshot-scrollback-rows snapshot))
      (write-string (%capture-pane-scrollback-row-string row-cells escapes trim) out)
      (terpri out))
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

;;; ── pipe-pane ───────────────────────────────────────────────────────────────
;;;
;;; pipe_pane(Pane, Cmd) :-
;;;   (existing_pipe(Pane) -> close_pipe(Pane) ; true),
;;;   (Cmd \= nil -> open_pipe(Pane, Cmd) ; true).

(defconstant +pipe-pane-close-timeout+ 1
  "Seconds to wait for a pipe-pane subprocess to exit after stdin closes.")

(defconstant +pipe-pane-open-timeout+ 1
  "Seconds to wait for launching the pipe-pane subprocess.")

(defun %pipe-pane-copy-output (pane output-stream)
  "Copy OUTPUT-STREAM from the command back into PANE's PTY."
  (unwind-protect
      (handler-case
          (let ((buffer (make-string 4096)))
            (loop
              for count = (read-sequence buffer output-stream)
              while (plusp count) do
                (ignore-errors
                  (pty-write (pane-fd pane) (subseq buffer 0 count)))))
        (end-of-file () nil)
        (error () nil))
    (ignore-errors (close output-stream))))

(defun %pipe-pane-start-output-thread (pane output-stream)
  "Start the background copier for command stdout into PANE."
  (bt:make-thread (lambda () (%pipe-pane-copy-output pane output-stream))
                  :name (format nil "pipe-pane-output-~D" (pane-id pane))))

(defun %pipe-pane-reset (pane)
  "Clear all pipe-pane state slots on PANE."
  (setf (pane-pipe-fd pane) nil
        (pane-pipe-output-stream pane) nil
        (pane-pipe-output-thread pane) nil
        (pane-pipe-process pane) nil))

(defun pipe-pane-open (pane command &key
                            (pane-output-to-command-p t)
                            (command-output-to-pane-p nil))
  "Connect PANE and COMMAND with pipe-pane direction flags.
   PANE-OUTPUT-TO-COMMAND-P routes pane output to the command's stdin.
   COMMAND-OUTPUT-TO-PANE-P routes command stdout back into the pane.
   Returns a non-NIL stream or process handle on success, NIL on failure."
  ;; Close any existing pipe in either direction.
  (when (pane-pipe-active-p pane)
    (pipe-pane-close pane))
  (let ((proc nil)
        (input-stream nil)
        (output-stream nil)
        (output-thread nil))
    (handler-case
        (bt:with-timeout (+pipe-pane-open-timeout+)
          (let* ((shell (or cl-tmux/config:*default-shell* "/bin/sh"))
                 (new-proc
                   (uiop:launch-program (list shell "-c" command)
                                       :input (if pane-output-to-command-p :stream nil)
                                       :output (if command-output-to-pane-p :stream nil)
                                       :error-output nil))
                 (new-input (and pane-output-to-command-p
                                 (uiop:process-info-input new-proc)))
                 (new-output (and command-output-to-pane-p
                                  (uiop:process-info-output new-proc))))
            (setf proc new-proc
                  input-stream new-input
                  output-stream new-output
                  (pane-pipe-fd pane) input-stream
                  (pane-pipe-output-stream pane) output-stream
                  (pane-pipe-process pane) proc)
            (when output-stream
              (setf output-thread
                    (%pipe-pane-start-output-thread pane output-stream)
                    (pane-pipe-output-thread pane) output-thread))
            (or input-stream output-stream proc t)))
      (bt:timeout ()
        (%pipe-pane-cleanup pane
                            :input-stream input-stream
                            :output-stream output-stream
                            :output-thread output-thread
                            :process proc)
        nil)
      (error ()
        (%pipe-pane-cleanup pane
                            :input-stream input-stream
                            :output-stream output-stream
                            :output-thread output-thread
                            :process proc)
        nil))))

(defun %wait-pipe-process (process)
  "Return true when PROCESS exits before the pipe-pane close timeout."
  (when process
    (handler-case
        (progn
          (bt:with-timeout (+pipe-pane-close-timeout+)
            (uiop:wait-process process))
          t)
      (bt:timeout () nil)
      (error () nil))))

(defun %terminate-pipe-process (process)
  "Reap a pipe-pane subprocess, terminating it only if it ignores stdin EOF."
  (when (and process (not (%wait-pipe-process process)))
    (ignore-errors
      (when (uiop:process-alive-p process)
        (uiop:terminate-process process)))
    (%wait-pipe-process process)))

(defun %pipe-pane-cleanup (pane &key input-stream output-stream output-thread process)
  "Best-effort cleanup for pipe-pane resources, then reset PANE."
  (when input-stream
    (ignore-errors (close input-stream)))
  (when output-stream
    (ignore-errors (close output-stream)))
  (ignore-errors (%terminate-pipe-process process))
  (when output-thread
    (ignore-errors
      (cl-tmux::%join-thread-with-timeout output-thread
                                          +pipe-pane-close-timeout+)))
  (%pipe-pane-reset pane))

(defun pipe-pane-close (pane)
  "Close PANE's output pipe if one is open."
  (%pipe-pane-cleanup pane
                      :input-stream (pane-pipe-fd pane)
                      :output-stream (pane-pipe-output-stream pane)
                      :output-thread (pane-pipe-output-thread pane)
                      :process (pane-pipe-process pane)))

(defun pipe-pane-write (pane bytes)
  "Write BYTES to PANE's output pipe if one is active.
   Silently ignores write errors (pipe may have closed on the other end)."
  (when (pane-pipe-fd pane)
    (ignore-errors
      (write-sequence bytes (pane-pipe-fd pane))
      (force-output (pane-pipe-fd pane)))))
