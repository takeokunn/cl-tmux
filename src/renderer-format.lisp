(in-package #:cl-tmux/renderer)

;;;; ANSI escape-code primitives — pure data layer.
;;;;
;;;; All functions here write escape sequences to a stream; they do not touch
;;;; any model or terminal state.

(defconstant +esc+ #\Escape)

;;; ── Cursor positioning ──────────────────────────────────────────────────────

(defun move-to (stream row col)
  "ESC[row;colH — cursor absolute position, 1-based."
  (format stream "~C[~D;~DH" +esc+ (1+ row) (1+ col)))

;;; ── Colour SGR emission helpers (Prolog-like fact table) ────────────────────
;;;
;;; Color encoding (matches cell.fg / cell.bg in cell.lisp):
;;;   0-7   : standard ANSI                → SGR 30-37 / 40-47
;;;   8-15  : bright ANSI                  → SGR 90-97 / 100-107
;;;   16-255: 256-color palette            → SGR 38;5;N / 48;5;N
;;;   bit 24 set (#x1000000+): true-color  → SGR 38;2;R;G;B / 48;2;R;G;B
;;;
;;; emit_colour(fg/bg, 0-7)       :- standard_colour_code(fg/bg, n).
;;; emit_colour(fg/bg, 8-15)      :- bright_colour_code(fg/bg, n).
;;; emit_colour(fg/bg, 16-255)    :- palette_256(fg/bg, n).
;;; emit_colour(fg/bg, 0x1RRGGBB) :- true_colour(fg/bg, r, g, b).

;;; %EMIT-FG and %EMIT-BG are inlined: render-pane calls render-cell-attrs up
;;; to ~1920 times per frame; eliminating the two call frames is measurable.
(declaim (inline %emit-fg %emit-bg))

(defmacro define-colour-emitters (&rest specs)
  "Build %EMIT-FG and %EMIT-BG from a declarative spec table.
   Each SPEC is (name label std-base bright-base palette-prefix tc-prefix default-val).
   bright-base is std-base + 60 (i.e. 30+60=90 for fg, 40+60=100 for bg), offset
   by -8 so that (+ bright-base palette-index) yields the target SGR code directly
   for indices 8-15:  82+8=90, 82+15=97 for fg; 92+8=100, 92+15=107 for bg."
  `(progn
     ,@(mapcar
        (lambda (spec)
          (destructuring-bind (name label std-base bright-base palette-prefix tc-prefix default-val) spec
            `(defun ,name (stream n)
               ,(format nil
                  "Emit the ANSI SGR ~A colour code for value N to STREAM.~%~
                   N < 0: emit nothing (out-of-range / no-colour sentinel for tests).~%~
                   0-7:   standard colours   → ;~D-~D~%~
                   8-15:  bright colours     → ;~D-~D~%~
                   16-255: 256-colour palette → ;~A;N~%~
                   bit 24 set (#x1000000+): true-color → ;~A;R;G;B"
                  label std-base (+ std-base 7) bright-base (+ bright-base 7)
                  palette-prefix tc-prefix)
               ;; The (>= n 0) guard handles callers (e.g. unit tests) that pass -1
               ;; as a "no colour" sentinel.  Normal callers always pass (unsigned-byte 25)
               ;; values from cell-fg / cell-bg, which are always non-negative.
               (when (>= n 0)
                 (cond
                   ;; True-color: bit 24 set → #x1RRGGBB
                   ((logbitp 24 n)
                    (let* ((rgb (logand n #xFFFFFF))
                           (r (ash rgb -16))
                           (g (logand (ash rgb -8) #xFF))
                           (b (logand rgb #xFF)))
                      (format stream ";~A;~D;~D;~D" ,tc-prefix r g b)))
                   ((<= 0    n  7)   (format stream ";~D"      (+ ,std-base    n)))
                   ((<= 8    n 15)   (format stream ";~D"      (+ ,bright-base n)))
                   ((<= 16   n 255)  (format stream ";~A;~D"   ,palette-prefix n))
                   ;; Defensive fallback for values 256-#xFFFFFF without bit 24.
                   ;; Unreachable via apply-sgr (palette clamped to 255, true-color
                   ;; sets bit 24). Emits the default-colour reset code.
                   (t                (format stream ";~D"       ,default-val)))))))
        specs)))

(define-colour-emitters
  ;;        name       label         std  bright  256-pfx  tc-pfx  default
  (%emit-fg "foreground"              30    82    "38;5"   "38;2"   39)
  (%emit-bg "background"              40    92    "48;5"   "48;2"   49))

;;; ── Attribute rendering ─────────────────────────────────────────────────────
;;;
;;; define-cell-attr-renderer is a Prolog-like rule table:
;;;   render_attr(bold, stream)      :- write(stream, ";1").
;;;   render_attr(italic, stream)    :- write(stream, ";3").
;;;   ...

(defmacro define-cell-attr-renderer (&rest bit-rules)
  "Build RENDER-CELL-ATTRS from a declarative table of (bit-index sgr-code) entries.
   Attribute bits are checked in order and the corresponding SGR code is emitted."
  `(defun render-cell-attrs (stream fg bg attrs)
     "Emit an SGR escape sequence resetting then applying FG, BG, and ATTRS to STREAM."
     (format stream "~C[0" +esc+)
     ,@(mapcar (lambda (rule)
                 `(when (logbitp ,(first rule) attrs)
                    (write-string ,(format nil ";~D" (second rule)) stream)))
               bit-rules)
     (%emit-fg stream fg)
     (%emit-bg stream bg)
     (write-char #\m stream)))

(define-cell-attr-renderer
  (0 1)    ; bold          → SGR 1
  (1 2)    ; dim           → SGR 2
  (2 7)    ; reverse       → SGR 7
  (3 4)    ; underline     → SGR 4
  (4 5)    ; blink         → SGR 5
  (5 3)    ; italic        → SGR 3
  (6 8)    ; conceal       → SGR 8
  (7 9))   ; strikethrough → SGR 9

;;; ── Cursor visibility ───────────────────────────────────────────────────────

(defun cursor-invisible (stream)
  (format stream "~C[?25l" +esc+))

(defun cursor-visible (stream)
  (format stream "~C[?25h" +esc+))

;;; ── Attribute reset ─────────────────────────────────────────────────────────

(defun reset-attrs (stream)
  (format stream "~C[0m" +esc+))
