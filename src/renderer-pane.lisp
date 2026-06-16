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

(defun %render-cell-row (stream screen pane-col-count row
                         sel-active sel-start-row sel-end-row sel-start-col sel-end-col
                         sel-rect-p
                         sgr-reg
                         def-fg def-bg mode-style-fg mode-style-bg)
  "Render one row of cells to STREAM, highlighting selected cells.
   SGR-REG is a sgr-register struct used as mutable state across rows (last-emitted
   fg/bg/attrs/ul-color/hyperlink); the caller initialises it once and threads it
   across all rows in the pane.
   DEF-FG / DEF-BG (or NIL) are the pane's window-style default colours.
   MODE-STYLE-FG / MODE-STYLE-BG (or NIL) are the mode-style selection colours.
   Returns nothing; mutates SGR-REG as a side-effect."
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
                       (sgr-reg-ul-color sgr-reg) ul-color))
               ;; OSC 8 hyperlink: re-emit when entering/leaving/changing a link
               ;; span so the outer terminal makes those cells clickable.
               (let ((hl (cell-hyperlink cell)))
                 (unless (equal hl (sgr-reg-hyperlink sgr-reg))
                   (write-string (format nil "~C]8;;~@[~A~]~C\\" #\Escape hl #\Escape)
                                 stream)
                   (setf (sgr-reg-hyperlink sgr-reg) hl)))
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
          ;; sgr-register bundles the mutable last-emitted SGR state so
          ;; %render-cell-row can detect and suppress redundant attribute sequences.
          (let ((sgr-reg (make-sgr-register)))
            (loop for row below pane-height do
              (move-to stream (+ origin-y row) origin-x)
              (%render-cell-row stream screen pane-width row
                                sel-active sel-start-row sel-end-row
                                sel-start-col sel-end-col
                                sel-rect-p
                                sgr-reg
                                def-fg def-bg mode-style-fg mode-style-bg))
            ;; Close any hyperlink still open at the end of the pane (OSC 8 ; ;).
            (when (sgr-reg-hyperlink sgr-reg)
              (write-string (format nil "~C]8;;~C\\" #\Escape #\Escape) stream))))
        (screen-clear-dirty screen))))
    ;; Clock-mode overlay: draw a digital clock if this pane is the clock pane.
    (when (eql cl-tmux::*clock-mode-pane-id* (pane-id pane))
      (draw-clock-to-screen stream origin-x origin-y pane-width pane-height))))
