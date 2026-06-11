(in-package #:cl-tmux/renderer)

;;;; Pane and border rendering.
;;;;
;;;; Depends on the ANSI escape-code primitives from renderer-format.lisp
;;;; (loaded first in the same package) and the layout/model structures from
;;;; cl-tmux/model.

;;; Forward-declare the cl-tmux special variable defined in runtime.lisp so
;;; SBCL does not warn about an unknown special during compilation of this file.
(declaim (special cl-tmux::*clock-mode-pane-id*))

;;; ── Pane ────────────────────────────────────────────────────────────────────

(defun in-selection-p (row col sel-start-r sel-end-r sel-start-c sel-end-c &optional rect-p)
  "Return T when (ROW, COL) falls within the selection.
   RECT-P non-nil: rectangle mode — any cell in [start-r..end-r] x [start-c..end-c).
   Default (character mode): the standard stream-of-characters selection logic."
  (if rect-p
      (and (<= sel-start-r row sel-end-r)
           (<= sel-start-c col)
           (< col sel-end-c))
      (cond
        ((= sel-start-r sel-end-r row)
         (and (<= sel-start-c col) (< col sel-end-c)))
        ((= row sel-start-r) (>= col sel-start-c))
        ((= row sel-end-r)   (< col sel-end-c))
        (t (and (> row sel-start-r) (< row sel-end-r))))))

;;; ── Clock-mode ASCII digit font (3 rows tall) ───────────────────────────────
;;;
;;; Each digit is represented as a list of 3 strings, each 3 chars wide.
;;; Spacing between digits is 1 space.  The colon separator is 1 char wide.

(defparameter *clock-digits*
  '(("███" "█ █" "███")   ; 0
    ("  █" "  █" "  █")   ; 1
    ("██ " " ██" " ██")   ; 2 — corrected
    ("██ " " █ " "███")   ; 3 — corrected
    ("█ █" "███" "  █")   ; 4
    (" ██" "██ " "███")   ; 5 — corrected
    (" █ " "███" "███")   ; 6
    ("███" "  █" "  █")   ; 7
    ("███" "███" "███")   ; 8
    ("███" "███" "  █"))  ; 9
  "3-row ASCII digit font. Each entry is (row0 row1 row2) for a 3-wide glyph.")

(defun %clock-digit-rows (digit)
  "Return the 3 display rows for DIGIT (0–9) from *clock-digits*."
  (nth digit *clock-digits*))

(defun %clock-display-hour (hour)
  "Convert HOUR (0–23) to the displayed hour per the clock-mode-style option:
   24 (default) → unchanged; 12 → a 12-hour clock (0 → 12, 13–23 → 1–11)."
  (if (eql 12 (cl-tmux/options:get-option "clock-mode-style" 24))
      (let ((h (mod hour 12))) (if (zerop h) 12 h))
      hour))

(defun %clock-face-sgr ()
  "SGR parameter string for the clock face, from the clock-mode-colour option
   (a foreground colour name mapped to its SGR code; falls back to bright cyan
   when the name is unknown)."
  (format nil "~D" (or (%border-color-sgr
                        (cl-tmux/options:get-option "clock-mode-colour" "blue"))
                       96)))

;;; +min-clock-width+ : minimum terminal columns required to render the HH:MM clock.
;;; The clock occupies 3-char digits with 1-char spacing: 3+1+3+1+1+1+3+1+3 = 17 chars
;;; rendered, but the minimal HH:MM glyph block is 13 columns wide (two 3-wide digits,
;;; one 1-wide colon, two 1-wide spaces, two more 3-wide digits, no trailing space).
(defconstant +min-clock-width+ 13
  "Minimum pane width in columns required to render the clock-mode HH:MM display.")

(defun draw-clock-to-screen (stream ox oy pw ph)
  "Render the current time HH:MM as 3-row ASCII digits centred in the pane
   at terminal offset (OX, OY), clipping to the pane rectangle (PW x PH).
   Honours clock-mode-style (12/24-hour) and clock-mode-colour (digit colour).
   Only renders if the pane is at least +MIN-CLOCK-WIDTH+ columns wide and 3 rows tall."
  (when (and (>= pw +min-clock-width+) (>= ph 3))
    (multiple-value-bind (sec min hour) (get-decoded-time)
      (declare (ignore sec))
      (setf hour (%clock-display-hour hour))
      ;; Format: two digits, colon, two digits = 3+1+3+1+3+1+3 = 15 chars
      ;; But we use simple 3-char digits + 1-char separators:
      ;; D D : D D = 3+1+3+1+1+1+3+1+3 = 17 chars; trim to 13 "HH:MM" minimal.
      ;; Represent as list of (rows . char-sequence) for each position.
      (let* ((h0 (floor hour 10))
             (h1 (mod   hour 10))
             (m0 (floor min  10))
             (m1 (mod   min  10))
             ;; Colon glyph rows: centre row lights a block, top/bottom are blank.
             (colon-separator-rows '("   " " █ " "   "))
             ;; Build row strings for the 3 display rows
             (rows (loop for row-idx from 0 below 3
                         collect (concatenate 'string
                                   (nth row-idx (%clock-digit-rows h0))
                                   " "
                                   (nth row-idx (%clock-digit-rows h1))
                                   (nth row-idx colon-separator-rows)
                                   (nth row-idx (%clock-digit-rows m0))
                                   " "
                                   (nth row-idx (%clock-digit-rows m1)))))
             ;; Centre within the pane
             (clock-w (length (first rows)))
             (clock-h 3)
             (start-col (max 0 (floor (- pw clock-w) 2)))
             (start-row (max 0 (floor (- ph clock-h) 2))))
        ;; Blue background, bright cyan text for clock face.
        ;; Use a named constant consistent with +sgr-default-status+ (44;97) but with
        ;; bright cyan (96) instead of bright white to distinguish the clock from the bar.
        (format stream "~C[~Am" +esc+ (%clock-face-sgr))
        (loop for row-str in rows
              for roff from 0 do
          (let ((term-row (+ oy start-row roff))
                (term-col (+ ox start-col)))
            (move-to stream term-row term-col)
            (write-string (subseq row-str 0 (min (length row-str) pw)) stream)))
        (reset-attrs stream)))))

(defun %draw-pane-number-to-screen (stream ox oy pw ph number active-p)
  "Draw NUMBER (a pane index) as centred 3-row big digits in the pane at terminal
   offset (OX,OY), clipped to the pane rectangle (PW x PH).  Coloured by
   display-panes-active-colour when ACTIVE-P, else display-panes-colour (a colour
   name → fg SGR; fallback blue 34).  This is tmux's display-panes (C-b q) display;
   reuses the clock's big-digit glyphs.  Renders only if the pane is ≥ 3x3."
  (when (and (>= pw 3) (>= ph 3))
    (let* ((digits    (map 'list #'digit-char-p (princ-to-string number)))
           (rows      (loop for row-idx below 3
                            collect (format nil "~{~A~^ ~}"
                                            (mapcar (lambda (d)
                                                      (nth row-idx (%clock-digit-rows d)))
                                                    digits))))
           (num-w     (length (first rows)))
           (start-col (max 0 (floor (- pw num-w) 2)))
           (start-row (max 0 (floor (- ph 3) 2)))
           (colour    (if active-p
                          (cl-tmux/options:get-option "display-panes-active-colour" "red")
                          (cl-tmux/options:get-option "display-panes-colour" "blue")))
           (sgr       (or (%border-color-sgr colour) 34)))
      (format stream "~C[~Dm" +esc+ sgr)
      (loop for row-str in rows
            for roff from 0 do
        (move-to stream (+ oy start-row roff) (+ ox start-col))
        (write-string (subseq row-str 0 (min (length row-str) pw)) stream))
      (reset-attrs stream))))

;;; ── Copy-mode search-match highlighting ─────────────────────────────────────
;;;
;;; While copy mode has an active search term, every match in the visible viewport
;;; is overdrawn in copy-mode-match-style (the match under the cursor in
;;; copy-mode-current-match-style).  A POST-PASS re-emits the matched cells with a
;;; raw style SGR (via style-to-sgr) — so it needs no cell-colour-model conversion
;;; and never touches the hot per-cell render loop.

(defun %screen-row-display-string (screen row)
  "The visible (offset-aware) content of ROW as a string."
  (with-output-to-string (s)
    (dotimes (col (screen-width screen))
      (write-char (cell-char (screen-display-cell screen col row)) s))))

(defun %all-match-ranges (term row-str)
  "All (START . END) column ranges in ROW-STR matching TERM: regex via cl-ppcre,
   literal-substring fallback when TERM is not a valid regex.  Zero-width matches
   are skipped."
  (if (ignore-errors (cl-ppcre:create-scanner term))
      (loop for (s e) on (cl-ppcre:all-matches term row-str) by #'cddr
            when (and e (> e s)) collect (cons s e))
      (loop with tlen = (length term) and start = 0
            for pos = (search term row-str :start2 start)
            while pos
            collect (cons pos (+ pos tlen))
            do (setf start (+ pos (max 1 tlen))))))

(defun %copy-match-sgr (option-name default)
  "SGR string for the copy-mode-(current-)match-style OPTION-NAME, or NIL when the
   option is empty.  Parses the tmux style string via the renderer style pipeline."
  (let ((style (cl-tmux/options:get-option option-name default)))
    (when (and style (plusp (length style)))
      (style-to-sgr (parse-style-string style)))))

(defun %render-copy-search-matches (buffer pane)
  "When PANE's screen is in copy mode with an active search term, overdraw each
   matching span in copy-mode-match-style — the span under the copy cursor in
   copy-mode-current-match-style — over the already-rendered pane content."
  (let ((screen (pane-screen pane)))
    (when (and screen (screen-copy-mode-p screen))
      (let ((term (screen-copy-search-term screen)))
        (when (and term (plusp (length term)))
          (let* ((match-sgr   (%copy-match-sgr "copy-mode-match-style" "bg=green"))
                 (current-sgr  (%copy-match-sgr "copy-mode-current-match-style"
                                                "bg=magenta"))
                 (cursor       (screen-copy-cursor screen))
                 (cur-row      (and (consp cursor) (car cursor)))
                 (cur-col      (and (consp cursor) (cdr cursor)))
                 (ox           (pane-x pane))
                 (oy           (pane-y pane))
                 (w            (screen-width screen)))
            (when match-sgr
              (dotimes (row (screen-height screen))
                (let ((row-str (%screen-row-display-string screen row)))
                  (dolist (range (%all-match-ranges term row-str))
                    (let* ((start (car range))
                           (end   (min (cdr range) w))
                           (current-p (and (eql cur-row row) cur-col
                                           (<= start cur-col) (< cur-col end)))
                           (sgr   (if current-p (or current-sgr match-sgr) match-sgr)))
                      (move-to buffer (+ oy row) (+ ox start))
                      (format buffer "~C[~Am" +esc+ sgr)
                      (write-string (subseq row-str start end) buffer)
                      (reset-attrs buffer))))))))))))

;;; ── Selection bounds computation ──────────────────────────────────────────────

(defun %compute-selection-bounds (screen)
  "Compute normalised selection boundary coordinates for SCREEN's copy-mode selection.
   Returns (values sel-active sel-start-row sel-end-row sel-start-col sel-end-col sel-rect-p).
   sel-active is NIL when the selection prerequisites (selecting flag, mark, cursor)
   are not all present.  Rows are VIEWPORT rows (0..height-1) so in-selection-p and
   screen-display-cell work directly.  The computation uses virtual rows internally so
   a selection started before scrolling highlights the correct cells after scrolling.
   sel-rect-p is T when rectangle-select mode is active (screen-copy-rect-select-p)."
  (if (and (screen-copy-selecting screen)
           (consp (screen-copy-mark   screen))
           (consp (screen-copy-cursor screen)))
      (let* ((mark        (screen-copy-mark   screen))
             (cursor      (screen-copy-cursor screen))
             (mark-col    (cdr mark))
             (cursor-col  (cdr cursor))
             (sb-n        (length (screen-scrollback screen)))
             (mark-offset (screen-copy-mark-offset screen))
             (cur-offset  (screen-copy-offset screen))
             (h           (screen-height screen))
             ;; Convert viewport rows (stored at their respective offsets) to virtual rows.
             (mark-vrow   (+ sb-n (car mark)   (- mark-offset)))
             (cur-vrow    (+ sb-n (car cursor) (- cur-offset)))
             ;; Convert virtual rows back to viewport rows at the CURRENT offset,
             ;; clamped so that off-screen anchors highlight the nearest edge.
             (start-vrow  (min mark-vrow cur-vrow))
             (end-vrow    (max mark-vrow cur-vrow))
             (start-vp    (max 0 (min (1- h) (+ start-vrow cur-offset (- sb-n)))))
             (end-vp      (max 0 (min (1- h) (+ end-vrow   cur-offset (- sb-n)))))
             (rect-p      (screen-copy-rect-select-p screen))
             ;; Column orientation: which virtual end is topmost?
             (mark-is-start (< mark-vrow cur-vrow))
             (mark-is-end   (> mark-vrow cur-vrow)))
        (values t
                start-vp
                end-vp
                ;; Rectangle mode: uniform column range (min..max+1 exclusive).
                ;; Character mode: start-col tracks the topmost-virtual end's column.
                (if rect-p
                    (min mark-col cursor-col)
                    (if mark-is-start mark-col
                        (if mark-is-end cursor-col (min mark-col cursor-col))))
                (if rect-p
                    (1+ (max mark-col cursor-col))
                    (if mark-is-start cursor-col
                        (if mark-is-end mark-col (max mark-col cursor-col))))
                rect-p))
      (values nil 0 0 0 0 nil)))

;;; ── Per-row cell rendering ───────────────────────────────────────────────────

(defun %render-cell-row (stream screen pane-col-count row
                         sel-active sel-start-row sel-end-row sel-start-col sel-end-col
                         sel-rect-p
                         prev-fg-cell prev-bg-cell prev-attrs-cell prev-attrs2-cell
                         prev-ul-color-cell prev-hyperlink-cell
                         def-fg def-bg mode-style-fg mode-style-bg)
  "Render one row of cells to STREAM, highlighting selected cells.
   PREV-FG-CELL / PREV-BG-CELL / PREV-ATTRS-CELL / PREV-ATTRS2-CELL /
   PREV-UL-COLOR-CELL are single-element lists used as mutable registers for SGR
   change-detection across the row.
   DEF-FG / DEF-BG (or NIL) are the pane's window-style default colours: a cell
   carrying the model default (fg=7 / bg=0) is recoloured to them, which dims an
   inactive pane / highlights the active one.
   MODE-STYLE-FG / MODE-STYLE-BG (or NIL) are the mode-style selection colours:
   when either is set, selected cells take those colours instead of the
   reverse-video default, so `set -g mode-style bg=colour…` works.
   Returns nothing; updates the prev-* registers as a side-effect."
  (loop with rev-screen = (and (screen-reverse-screen screen)
                               cl-tmux/terminal/types:+attr-reverse+)
        for col below pane-col-count
        for cell = (screen-display-cell screen col row)
        ;; A continuation cell (width 0) is the right half of a double-width
        ;; glyph the terminal already drew — emit nothing.
        unless (zerop (cell-width cell))
          do (let* ((raw-fg (cell-fg cell))
                    (raw-bg (cell-bg cell))
                    ;; window-style recolours only cells left at the model
                    ;; defaults (fg=7, bg=0); explicit colours are preserved.
                    (base-fg (if (and def-fg (= raw-fg 7)) def-fg raw-fg))
                    (base-bg (if (and def-bg (= raw-bg 0)) def-bg raw-bg))
                    (in-sel (and sel-active
                                 (in-selection-p row col
                                                 sel-start-row sel-end-row
                                                 sel-start-col sel-end-col
                                                 sel-rect-p)))
                    ;; A colour-based mode-style highlights the selection with its
                    ;; own fg/bg; otherwise selection falls back to reverse-video.
                    (mode-style-colour (and in-sel (or mode-style-fg mode-style-bg)))
                    (fg    (if mode-style-colour (or mode-style-fg base-fg) base-fg))
                    (bg    (if mode-style-colour (or mode-style-bg base-bg) base-bg))
                    ;; DECSCNM (reverse-video screen) and the selection highlight
                    ;; both toggle reverse; XOR both so a cell that is reverse for
                    ;; two reasons renders normal (correct double-reverse).
                    (attrs (logxor (cell-attrs cell)
                                   (if (and in-sel (not mode-style-colour))
                                       cl-tmux/terminal/types:+attr-reverse+ 0)
                                   (or rev-screen 0)))
                    ;; Extended attributes and underline colour pass through without
                    ;; modification (no selection / DECSCNM involvement).
                    (attrs2   (cell-attrs2    cell))
                    (ul-color (cell-ul-color  cell)))
               (unless (and (= fg       (car prev-fg-cell))
                            (= bg       (car prev-bg-cell))
                            (= attrs    (car prev-attrs-cell))
                            (= attrs2   (car prev-attrs2-cell))
                            (= ul-color (car prev-ul-color-cell)))
                 (render-cell-attrs stream fg bg attrs attrs2 ul-color)
                 (setf (car prev-fg-cell)       fg
                       (car prev-bg-cell)       bg
                       (car prev-attrs-cell)    attrs
                       (car prev-attrs2-cell)   attrs2
                       (car prev-ul-color-cell) ul-color))
               ;; OSC 8 hyperlink: re-emit when entering/leaving/changing a link
               ;; span so the outer terminal makes those cells clickable.
               (let ((hl (cell-hyperlink cell)))
                 (unless (equal hl (car prev-hyperlink-cell))
                   (write-string (format nil "~C]8;;~@[~A~]~C\\" #\Escape hl #\Escape)
                                 stream)
                   (setf (car prev-hyperlink-cell) hl)))
               (write-char (cell-char cell) stream)
               ;; Unicode combining characters (zero-width marks) follow the base char.
               (dolist (ch (cell-combining cell))
                 (write-char ch stream)))))

(defun render-pane (stream pane)
  "Draw the pane's screen into the real terminal at the pane's (x, y) offset.
   When *clock-mode-pane-id* matches (pane-id pane), draw a clock overlay."
  (let* ((screen     (pane-screen  pane))
         (pane-width  (pane-width  pane))
         (pane-height (pane-height pane))
         (origin-x    (pane-x     pane))
         (origin-y    (pane-y     pane)))
    ;; window-style / window-active-style: the active pane uses window-active-style,
    ;; every other pane window-style.  Empty (the default) → NIL defaults → no
    ;; recolouring, so panes render exactly as before unless the user opts in.
    (multiple-value-bind (def-fg def-bg)
        (let* ((win      (pane-window pane))
               (active-p (and win (eq pane (window-active-pane win))))
               (style    (cl-tmux/options:get-option-for-pane
                          (if active-p "window-active-style" "window-style")
                          pane)))
          (%window-style-default-colors style))
     (multiple-value-bind (mode-style-fg mode-style-bg)
         ;; mode-style selection colours (a colour-based value overrides the
         ;; reverse-video default highlight); "reverse" → NIL/NIL → reverse path.
         (%window-style-default-colors
          (cl-tmux/options:get-option "mode-style" "reverse"))
      (with-lock-held ((screen-lock screen))
        ;; Hoist selection boundary computation outside the cell loop so it is
        ;; computed once per frame instead of once per cell (~1920 times).
        (multiple-value-bind (sel-active sel-start-row sel-end-row sel-start-col sel-end-col
                              sel-rect-p)
            (%compute-selection-bounds screen)
          ;; Use single-element lists as mutable SGR-state registers so
          ;; %render-cell-row can update them without returning multiple values.
          (let ((prev-fg-cell       (list -1))
                (prev-bg-cell       (list -1))
                (prev-attrs-cell    (list -1))
                (prev-attrs2-cell   (list -1))  ; extended attrs: double-underline/overline
                (prev-ul-color-cell (list -1))  ; underline colour (SGR 58)
                ;; OSC 8 hyperlink register (NIL = no open link), threaded across
                ;; rows so a link spanning rows stays open.
                (prev-hyperlink-cell (list nil)))
            (loop for row below pane-height do
              (move-to stream (+ origin-y row) origin-x)
              (%render-cell-row stream screen pane-width row
                                sel-active sel-start-row sel-end-row
                                sel-start-col sel-end-col
                                sel-rect-p
                                prev-fg-cell prev-bg-cell prev-attrs-cell
                                prev-attrs2-cell prev-ul-color-cell
                                prev-hyperlink-cell
                                def-fg def-bg mode-style-fg mode-style-bg))
            ;; Close any hyperlink still open at the end of the pane (OSC 8 ; ;).
            (when (car prev-hyperlink-cell)
              (write-string (format nil "~C]8;;~C\\" #\Escape #\Escape) stream))))
        (screen-clear-dirty screen))))
    ;; Clock-mode overlay: draw a digital clock if this pane is the clock pane.
    (when (eql cl-tmux::*clock-mode-pane-id* (pane-id pane))
      (draw-clock-to-screen stream origin-x origin-y pane-width pane-height))))

