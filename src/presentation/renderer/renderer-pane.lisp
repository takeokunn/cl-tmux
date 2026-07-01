(in-package #:cl-tmux/renderer)

;;;; Pane and border rendering.
;;;;
;;;; Depends on the ANSI escape-code primitives from renderer-format.lisp
;;;; (loaded first in the same package) and the layout/model structures from
;;;; cl-tmux/model.

;;; Forward-declare the cl-tmux special variable defined in runtime.lisp so
;;; SBCL does not warn about an unknown special during compilation of this file.
(declaim (special cl-tmux::*clock-mode-pane-id*))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (let* ((source (or *load-truename* *compile-file-truename*))
         (base (and source
                    (make-pathname :name nil :type nil :defaults source))))
    (dolist (name '("renderer-pane-selection.lisp"
                    "renderer-pane-clock.lisp"
                    "renderer-pane-copy-mode.lisp"
                    "renderer-pane-search.lisp"))
      (let ((path (and base (merge-pathnames name base))))
        (when (and path (probe-file path))
          (load path))))))

;;; ── Per-row cell rendering ───────────────────────────────────────────────────

(defstruct (sgr-register (:conc-name sgr-reg-))
  "Mutable SGR state registers threaded across cells (and rows for hyperlinks).
   Tracks the last-emitted attribute values so redundant SGR sequences are suppressed."
  (fg        -1  :type fixnum)
  (bg        -1  :type fixnum)
  (attrs     -1  :type fixnum)
  (attrs2    -1  :type fixnum)
  (ul-color  -1  :type fixnum)
  (hyperlink nil))

(defun %pane-cell-in-selection-p (row col sel-active sel-start-row sel-end-row
                                  sel-start-col sel-end-col sel-rect-p)
  "True when cell (ROW, COL) falls inside the active copy-mode selection.
   Always false when SEL-ACTIVE is NIL."
  (and sel-active
       (in-selection-p row col sel-start-row sel-end-row
                       sel-start-col sel-end-col sel-rect-p)))

(defun %emit-cell-sgr-if-changed (stream sgr-reg fg bg attrs attrs2 ul-color)
  "Emit an SGR sequence for FG/BG/ATTRS/ATTRS2/UL-COLOR to STREAM, but only when
   they differ from the last-emitted values recorded in SGR-REG.  Updates SGR-REG
   to the new values as a side-effect.  Suppressing redundant SGR sequences keeps
   the per-frame output small when neighbouring cells share the same style."
  (unless (and (= fg       (sgr-reg-fg       sgr-reg))
               (= bg       (sgr-reg-bg       sgr-reg))
               (= attrs    (sgr-reg-attrs    sgr-reg))
               (= attrs2   (sgr-reg-attrs2   sgr-reg))
               (= ul-color (sgr-reg-ul-color sgr-reg)))
    (render-cell-attrs stream fg bg attrs attrs2 ul-color)
    (setf (sgr-reg-fg       sgr-reg) fg
          (sgr-reg-bg       sgr-reg) bg
          (sgr-reg-attrs    sgr-reg) attrs
          (sgr-reg-attrs2   sgr-reg) attrs2
          (sgr-reg-ul-color sgr-reg) ul-color)))

(defun %emit-cell-hyperlink-if-changed (stream sgr-reg hyperlink)
  "Emit an OSC 8 hyperlink-start/stop escape to STREAM when HYPERLINK differs
   from the link recorded in SGR-REG (entering, leaving, or switching a link
   span).  Updates SGR-REG to the new value as a side-effect."
  (unless (equal hyperlink (sgr-reg-hyperlink sgr-reg))
    (write-string (format nil "~C]8;;~@[~A~]~C\\" #\Escape hyperlink #\Escape) stream)
    (setf (sgr-reg-hyperlink sgr-reg) hyperlink)))

(defun %render-cell (stream cell row col sgr-reg rev-screen
                     def-fg def-bg selection-style-fg selection-style-bg
                     mark-style-fg mark-style-bg
                     sel-active sel-start-row sel-end-row sel-start-col sel-end-col
                     sel-rect-p sel-mark-row sel-mark-col)
  "Resolve CELL's colours through the base → mark → selection → attrs pipeline
   and render it to STREAM, threading the last-emitted SGR state through SGR-REG."
  (let* ((in-sel (%pane-cell-in-selection-p row col sel-active sel-start-row sel-end-row
                                            sel-start-col sel-end-col sel-rect-p)))
    (multiple-value-bind (base-fg base-bg) (%pane-cell-base-colors cell def-fg def-bg)
      (multiple-value-bind (mark-col-p mark-fg mark-bg)
          (%pane-cell-mark-colors row col sel-mark-row sel-mark-col
                                  mark-style-fg mark-style-bg base-fg base-bg)
        (multiple-value-bind (sel-fg sel-bg selection-style-colour)
            (%pane-cell-selection-colors in-sel selection-style-fg selection-style-bg
                                         base-fg base-bg)
          (let* ((fg (if mark-col-p mark-fg sel-fg))
                 (bg (if mark-col-p mark-bg sel-bg))
                 ;; DECSCNM (reverse-video screen) and the selection highlight both
                 ;; toggle reverse; XOR both so a cell that is reverse for two
                 ;; reasons renders normal (correct double-reverse).
                 (attrs (%pane-cell-attrs cell in-sel selection-style-colour mark-col-p rev-screen))
                 ;; Extended attributes and underline colour pass through without
                 ;; modification (no selection / DECSCNM involvement).
                 (attrs2   (cell-attrs2 cell))
                 (ul-color (cell-ul-color cell)))
            (%emit-cell-sgr-if-changed stream sgr-reg fg bg attrs attrs2 ul-color)
            (%emit-cell-hyperlink-if-changed stream sgr-reg (cell-hyperlink cell))
            (write-char (cell-char cell) stream)
            ;; Unicode combining characters (zero-width marks) follow the base char.
            (dolist (ch (cell-combining cell))
              (write-char ch stream))))))))

(defun %render-cell-row (stream screen pane-col-count row
                         sel-active sel-start-row sel-end-row sel-start-col sel-end-col
                         sel-rect-p
                         sel-mark-row sel-mark-col
                         sgr-reg
                         def-fg def-bg selection-style-fg selection-style-bg
                         mark-style-fg mark-style-bg)
  "Render one row of cells to STREAM, highlighting selected cells.
   SGR-REG is a sgr-register struct used as mutable state across rows (last-emitted
   fg/bg/attrs/ul-color/hyperlink); the caller initialises it once and threads it
   across all rows in the pane.
   DEF-FG / DEF-BG (or NIL) are the pane's window-style default colours.
   SELECTION-STYLE-FG / SELECTION-STYLE-BG (or NIL) are the copy-mode selection
   colours.
   MARK-STYLE-FG / MARK-STYLE-BG (or NIL) are the copy-mode mark-row colours.
  Returns nothing; mutates SGR-REG as a side-effect."
  (loop with rev-screen = (and (screen-reverse-screen screen)
                               cl-tmux/terminal/types:+attr-reverse+)
        for col below pane-col-count
        for cell = (screen-display-cell screen col row)
        ;; A continuation cell (width 0) is the right half of a double-width
        ;; glyph the terminal already drew — emit nothing.
        unless (zerop (cell-width cell))
          do (%render-cell stream cell row col sgr-reg rev-screen
                           def-fg def-bg selection-style-fg selection-style-bg
                           mark-style-fg mark-style-bg
                           sel-active sel-start-row sel-end-row sel-start-col sel-end-col
                           sel-rect-p sel-mark-row sel-mark-col)))

(defun %pane-cell-base-colors (cell def-fg def-bg)
  "Substitute the pane's window-style default colours DEF-FG / DEF-BG only for
   cells whose colour is the terminal-default sentinel (+default-color+).  Cells
   carrying an explicit palette index (including 7=white / 0=black) or true-colour
   are left untouched, matching tmux's COLOUR_DEFAULT-gated window-style recolour."
  (let ((raw-fg (cell-fg cell))
        (raw-bg (cell-bg cell)))
    (values (if (and def-fg (= raw-fg cl-tmux/terminal/types:+default-color+)) def-fg raw-fg)
            (if (and def-bg (= raw-bg cl-tmux/terminal/types:+default-color+)) def-bg raw-bg))))

(defun %pane-cell-mark-colors (row col sel-mark-row sel-mark-col
                               mark-style-fg mark-style-bg
                               base-fg base-bg)
  (let* ((mark-row-p (and sel-mark-row (= row sel-mark-row)))
         (mark-col-p (and mark-row-p sel-mark-col (= col sel-mark-col)))
         (mark-style-active (and mark-row-p (or mark-style-fg mark-style-bg)))
         (mark-fg (if mark-style-active (or mark-style-fg base-fg) base-fg))
         (mark-bg (if mark-style-active (or mark-style-bg base-bg) base-bg)))
    (values mark-col-p mark-fg mark-bg)))

(defun %pane-cell-selection-colors (in-sel selection-style-fg selection-style-bg
                                    base-fg base-bg)
  (let ((selection-style-colour (and in-sel
                                     (or selection-style-fg selection-style-bg))))
    (values (if selection-style-colour
                (or selection-style-fg base-fg)
                base-fg)
            (if selection-style-colour
                (or selection-style-bg base-bg)
                base-bg)
            selection-style-colour)))

(defun %pane-cell-attrs (cell in-sel selection-style-colour mark-col-p rev-screen)
  (let ((attrs (logxor (cell-attrs cell)
                       (if (and in-sel (not selection-style-colour) (not mark-col-p))
                           cl-tmux/terminal/types:+attr-reverse+ 0)
                       (or rev-screen 0))))
    (if mark-col-p
        (logior attrs cl-tmux/terminal/types:+attr-reverse+)
        attrs)))

(defstruct (pane-style-colours (:conc-name pane-style-))
  "Resolved fg/bg cell-colour numbers (each NIL or a cell-colour integer)
   consumed by %render-pane-body, bundled so render-pane can pass them around
   as one value instead of six."
  def-fg def-bg
  selection-fg selection-bg
  mark-fg mark-bg)

(defun %resolve-pane-style-colours (pane)
  "Resolve PANE's window-style default colours and the copy-mode selection/mark
   colours into a PANE-STYLE-COLOURS struct.
   window-style / window-active-style: the active pane uses window-active-style,
   every other pane window-style.  Empty (the default) → NIL defaults → no
   recolouring, so panes render exactly as before unless the user opts in.
   copy-mode-selection-style overrides the default reverse-video selection
   highlight when it provides colours; copy-mode-mark-style colours the marked row."
  (let* ((pane-win      (pane-window pane))
         (pane-active-p (and pane-win (eq pane (window-active-pane pane-win))))
         (window-style  (cl-tmux/options:get-option-for-pane
                         (if pane-active-p "window-active-style" "window-style")
                         pane)))
    (multiple-value-bind (def-fg def-bg) (%window-style-default-colors window-style)
      (multiple-value-bind (selection-fg selection-bg)
          (%window-style-default-colors
           (cl-tmux/options:get-option "copy-mode-selection-style" "reverse"))
        (multiple-value-bind (mark-fg mark-bg)
            (%window-style-default-colors
             (cl-tmux/options:get-option "copy-mode-mark-style" "bg=red,fg=black"))
          (make-pane-style-colours
           :def-fg def-fg :def-bg def-bg
           :selection-fg selection-fg :selection-bg selection-bg
           :mark-fg mark-fg :mark-bg mark-bg))))))

(defun %render-pane-body (stream screen pane-height
                          line-number-gutter-width content-origin-x content-width
                          origin-x origin-y
                          line-number-base-style line-number-current-style line-number-mode
                          colours)
  "Render every row of PANE's screen content (and line-number gutter, when
   active) to STREAM under the screen lock, then clear the screen's dirty flag.
   COLOURS is the PANE-STYLE-COLOURS struct from %resolve-pane-style-colours."
  (with-lock-held ((screen-lock screen))
    ;; Hoist selection boundary computation outside the cell loop so it is
    ;; computed once per frame instead of once per cell (~1920 times).
    (multiple-value-bind (sel-active sel-start-row sel-end-row sel-start-col sel-end-col
                          sel-rect-p sel-mark-row sel-mark-col)
        (%compute-selection-bounds screen)
      ;; sgr-register bundles the mutable last-emitted SGR state so
      ;; %render-cell-row can detect and suppress redundant attribute sequences.
      (let ((sgr-reg (make-sgr-register)))
        (loop for row below pane-height do
          (when (plusp line-number-gutter-width)
            (%render-copy-mode-line-number-row stream screen row origin-x origin-y
                                               line-number-gutter-width
                                               line-number-base-style
                                               line-number-current-style
                                               line-number-mode))
          (when (plusp content-width)
            (move-to stream (+ origin-y row) content-origin-x)
            (%render-cell-row stream screen content-width row
                              sel-active sel-start-row sel-end-row
                              sel-start-col sel-end-col
                              sel-rect-p
                              sel-mark-row sel-mark-col
                              sgr-reg
                              (pane-style-def-fg colours) (pane-style-def-bg colours)
                              (pane-style-selection-fg colours) (pane-style-selection-bg colours)
                              (pane-style-mark-fg colours) (pane-style-mark-bg colours))))
        ;; Close any hyperlink still open at the end of the pane (OSC 8 ; ;).
        (when (sgr-reg-hyperlink sgr-reg)
          (write-string (format nil "~C]8;;~C\\" #\Escape #\Escape) stream)))))
  (screen-clear-dirty screen))

(defun render-pane (stream session pane)
  "Draw the pane's screen into the real terminal at the pane's (x, y) offset.
   When *clock-mode-pane-id* matches (pane-id pane), draw a clock overlay."
  (let* ((screen      (pane-screen  pane))
         (pane-width  (pane-width  pane))
         (pane-height (pane-height pane))
         (origin-x    (pane-x     pane))
         (origin-y    (pane-y     pane))
         (line-number-mode (%copy-mode-line-number-mode))
         (line-number-base-style
           (%copy-mode-line-number-style-spec
            (cl-tmux/options:get-option "copy-mode-line-number-style" "")))
         (line-number-current-style
           (%copy-mode-line-number-style-spec
            (cl-tmux/options:get-option "copy-mode-current-line-number-style" ""))))
    (multiple-value-bind (line-number-gutter-width content-origin-x content-width)
        (%copy-mode-pane-geometry screen origin-x pane-height pane-width)
      (%render-pane-body stream screen pane-height
                         line-number-gutter-width content-origin-x content-width
                         origin-x origin-y
                         line-number-base-style line-number-current-style line-number-mode
                         (%resolve-pane-style-colours pane))
      ;; Copy-mode overlay is rendered as a right-aligned slice so it does not
      ;; repaint the whole pane row.
      (%render-copy-mode-position-overlay stream session pane
                                          content-origin-x origin-y content-width)
      ;; Clock-mode overlay: draw a digital clock if this pane is the clock pane.
      (when (eql cl-tmux::*clock-mode-pane-id* (pane-id pane))
        (draw-clock-to-screen stream content-origin-x origin-y content-width pane-height)))))
