(in-package #:cl-tmux/test)

;;;; Escape-code format primitive tests.
;;;;
;;;; Covers: ANSI escape-code helpers in src/renderer-format.lisp:
;;;;   +esc+, move-to, define-cell-attr-renderer, cursor-invisible/visible, reset-attrs

(def-suite renderer-suite :description "Escape-code renderer")
(in-suite renderer-suite)

;;; ── Helper ──────────────────────────────────────────────────────────────────

(defun cell-attrs-string (fg bg attrs &optional (attrs2 0) (ul-color 0))
  (with-output-to-string (s)
    (cl-tmux/renderer::render-cell-attrs s fg bg attrs attrs2 ul-color)))

;;; ── move-to (1-based conversion) ────────────────────────────────────────────

(test move-to-is-one-based
  "move-to converts 0-based row/col arguments to 1-based ESC[row;colH sequences."
  (is (string= (format nil "~C[1;1H" #\Escape)
               (with-output-to-string (s)
                 (cl-tmux/renderer::move-to s 0 0)))
      "move-to 0,0 should emit ESC[1;1H")
  (is (string= (format nil "~C[3;5H" #\Escape)
               (with-output-to-string (s)
                 (cl-tmux/renderer::move-to s 2 4)))
      "move-to 2,4 should emit ESC[3;5H"))

;;; ── render-cell-attrs (SGR codes) ───────────────────────────────────────────

(test render-cell-attrs-basic-color-table
  "render-cell-attrs emits the correct SGR code for standard fg, bg, and bright fg."
  (dolist (c '((1 0 ";31" "fg 1 → ;31 (red)")
               (0 2 ";42" "bg 2 → ;42 (green)")
               (9 0 ";91" "bright fg 9 → ;91")))
    (destructuring-bind (fg bg expected desc) c
      (let ((out (cell-attrs-string fg bg 0)))
        (is (search expected out) "~A (got ~S)" desc out)))))

(test render-cell-attrs-frame
  "render-cell-attrs always wraps its SGR codes in a leading ESC[0 reset and a trailing m."
  (let ((out (cell-attrs-string 1 2 1)))
    (is (eql 0 (search (format nil "~C[0" #\Escape) out))
        "render-cell-attrs should start with ESC[0 (got ~S)" out)
    (is (char= #\m (char out (1- (length out))))
        "render-cell-attrs should end with m (got ~S)" out)))

(test render-cell-attrs-default-color-omitted
  "fg/bg outside 0..15 emit no colour code — only the leading reset remains."
  (let ((out (cell-attrs-string -1 -1 0)))
    (is (string= (format nil "~C[0m" #\Escape) out)
        "out-of-range fg/bg should omit colour codes (got ~S)" out)))

;;; ── cursor-invisible / cursor-visible ───────────────────────────────────────

(test cursor-visibility-sequences-table
  "cursor-invisible emits ESC[?25l; cursor-visible emits ESC[?25h."
  (dolist (c '((cl-tmux/renderer::cursor-invisible "?25l" "cursor-invisible → ESC[?25l")
               (cl-tmux/renderer::cursor-visible   "?25h" "cursor-visible → ESC[?25h")))
    (destructuring-bind (fn suffix desc) c
      (let ((out (with-output-to-string (s) (funcall fn s))))
        (is (string= (format nil "~C[~A" #\Escape suffix) out)
            "~A (got ~S)" desc out)))))

;;; ── reset-attrs ─────────────────────────────────────────────────────────────

(test reset-attrs-emits-sgr-zero-m
  "reset-attrs writes ESC[0m to the stream."
  (let ((s (make-string-output-stream)))
    (cl-tmux/renderer::reset-attrs s)
    (is (string= (format nil "~C[0m" #\Escape)
                 (get-output-stream-string s)))))

(test define-cell-attr-renderer-macro-is-defined
  "define-cell-attr-renderer is a defined macro."
  (is (macro-function 'cl-tmux/renderer::define-cell-attr-renderer)))

;;; ── render-cell-attrs attribute-bit table ───────────────────────────────────
;;;
;;; All single-bit attribute flags: each row is (bit-value SGR-search-string label).

(test render-cell-attrs-attribute-bits
  "Each attrs bit-flag causes render-cell-attrs to include the correct SGR code.
   Rows: (attrs-value expected-sgr-substring label)."
  (dolist (c '((1   ";1"  "bold (bit0)")
               (2   ";2"  "dim (bit1)")
               (4   ";7"  "reverse (bit2)")
               (8   ";4"  "underline (bit3)")
               (16  ";5"  "blink (bit4)")
               (32  ";3"  "italic (bit5)")
               (64  ";8"  "conceal (bit6)")
               (128 ";9"  "strikethrough (bit7)")))
    (destructuring-bind (attrs sgr label) c
      (let ((out (cell-attrs-string 0 0 attrs)))
        (is (search sgr out)
            "~A must emit ~S (got ~S)" label sgr out)))))

;;; ── Extended colour emit paths ───────────────────────────────────────────────

(test render-cell-attrs-256color-table
  "256-color palette indices emit the correct extended SGR sequences."
  (dolist (c '((200 0   ";38;5;200" "fg 200 → ;38;5;200")
               (0   42  ";48;5;42"  "bg 42 → ;48;5;42")))
    (destructuring-bind (fg bg expected desc) c
      (let ((out (cell-attrs-string fg bg 0)))
        (is (search expected out) "~A (got ~S)" desc out)))))

(test render-cell-attrs-truecolor-table
  "True-color values emit the correct 38;2;R;G;B / 48;2;R;G;B SGR sequences."
  (dolist (c (list (list (logior #x1000000 (ash 255 16) (ash 128 8)   0) 0 ";38;2;255;128;0"   "truecolor fg → ;38;2;255;128;0")
                   (list 0 (logior #x1000000 (ash 0   16) (ash 128 8) 255) ";48;2;0;128;255"   "truecolor bg → ;48;2;0;128;255")))
    (destructuring-bind (fg bg expected desc) c
      (let ((out (cell-attrs-string fg bg 0)))
        (is (search expected out) "~A (got ~S)" desc out)))))

;;; ── %split-style-tokens ─────────────────────────────────────────────────────

(test split-style-tokens-table
  "%split-style-tokens splits on commas; single-token and empty-string edge cases."
  (dolist (c '(("bold"                 ("bold")                       "single token")
               ("fg=red,bold,underline" ("fg=red" "bold" "underline") "comma-split")
               (""                     ("")                           "empty → one empty string")))
    (destructuring-bind (input expected desc) c
      (is (equal expected (cl-tmux/renderer::%split-style-tokens input)) "~A" desc))))

;;; ── %dispatch-style-token ───────────────────────────────────────────────────

(test dispatch-style-token-sets-attr-table
  "%dispatch-style-token sets the correct plist key for bold, reverse, and underline."
  (dolist (c '(("bold"      :bold      "bold sets :bold T")
               ("reverse"   :reverse   "reverse sets :reverse T")
               ("underline" :underline "underline sets :underline T")))
    (destructuring-bind (token key desc) c
      (let ((cell (list nil)))
        (is-true (cl-tmux/renderer::%dispatch-style-token token cell) "~A: returns T" desc)
        (is-true (getf (car cell) key) "~A: plist key" desc)))))

(test dispatch-style-token-nobold
  "%dispatch-style-token 'nobold' sets :bold NIL in result-cell."
  (let ((cell (list (list :bold t))))
    (cl-tmux/renderer::%dispatch-style-token "nobold" cell)
    (is (null (getf (car cell) :bold))
        ":bold must be NIL after 'nobold' dispatch")))

(test dispatch-style-token-unknown-returns-nil
  "%dispatch-style-token returns NIL for an unknown token."
  (let ((cell (list nil)))
    (is (null (cl-tmux/renderer::%dispatch-style-token "completely-unknown" cell))
        "%dispatch-style-token must return NIL for unknown tokens")))

;;; ── %emit-style-attrs ───────────────────────────────────────────────────────

(test emit-style-attrs-bold
  "%emit-style-attrs with :bold T pushes \"1\" onto parts."
  (let ((parts (cl-tmux/renderer::%emit-style-attrs '(:bold t) nil)))
    (is (member "1" parts :test #'string=)
        ":bold T must push \"1\" into parts")))

(test emit-style-attrs-reverse-and-underline
  "%emit-style-attrs pushes codes for all set attributes."
  (let ((parts (cl-tmux/renderer::%emit-style-attrs '(:reverse t :underline t) nil)))
    (is (member "7" parts :test #'string=) ":reverse T must push \"7\"")
    (is (member "4" parts :test #'string=) ":underline T must push \"4\"")))

(test emit-style-attrs-empty-style-returns-nil-parts
  "%emit-style-attrs with an empty style plist returns the unchanged parts."
  (let ((parts (cl-tmux/renderer::%emit-style-attrs nil nil)))
    (is (null parts) "empty style must leave parts as NIL")))

;;; ── %border-color-sgr ───────────────────────────────────────────────────────

(test border-color-sgr-table
  "%border-color-sgr maps known colour names to SGR codes; nil for unknown; case-insensitive."
  (dolist (c '(("green"    32  "green → 32")
               ("red"      31  "red → 31")
               ("Blue"     34  "mixed-case Blue → 34")
               ("notacolor" nil "unknown → nil")))
    (destructuring-bind (color expected desc) c
      (is (equal expected (cl-tmux/renderer::%border-color-sgr color)) "~A" desc))))

;;; ── %color-name-to-sgr-number ───────────────────────────────────────────────

(test color-name-to-sgr-number-table
  "%color-name-to-sgr-number maps named colours, colour-N, default, and unknown correctly
   for both fg (is-bg NIL) and bg (is-bg T)."
  (dolist (c '(("red"      nil "31")
               ("red"      t   "41")
               ("colour4"  nil "38;5;4")
               ("colour4"  t   "48;5;4")
               ("default"  nil "39")
               ("default"  t   "49")
               ("notacolor" nil "39")
               ("notacolor" t   "49")))
    (destructuring-bind (color is-bg expected) c
      (is (string= expected
                   (cl-tmux/renderer::%color-name-to-sgr-number color is-bg))
          "%color-name-to-sgr-number ~S is-bg=~S must produce ~S"
          color is-bg expected))))

;;; ── %status-sgr-from-style ───────────────────────────────────────────────────

(test status-sgr-from-style-default-table
  "%status-sgr-from-style returns the default blue-on-white SGR for nil and empty string."
  (dolist (c '((nil "nil arg → default SGR")
               (""  "empty string → default SGR")))
    (destructuring-bind (arg desc) c
      (is (string= "44;97" (cl-tmux/renderer::%status-sgr-from-style arg)) "~A" desc))))

(test status-sgr-from-style-bold
  "%status-sgr-from-style with 'bold' includes SGR code 1."
  (let ((sgr (cl-tmux/renderer::%status-sgr-from-style "bold")))
    (is (search "1" sgr)
        "%status-sgr-from-style 'bold' must include \"1\" (got ~S)" sgr)))

;;; ── %effective-status-style ─────────────────────────────────────────────────

(test effective-status-style-empty-when-nothing-set
  "%effective-status-style is empty when status-style is not set."
  (with-isolated-config
    (is (string= "" (cl-tmux/renderer::%effective-status-style))
        "no status-style set must yield the empty string")))

(test effective-status-style-returns-status-style
  "%effective-status-style returns the status-style option value directly."
  (with-isolated-config
    (cl-tmux/options:set-option "status-style" "fg=white,bg=blue,bold")
    (let ((eff (cl-tmux/renderer::%effective-status-style)))
      (is (search "bold" eff)       "status-style bold preserved (got ~S)" eff)
      (is (search "fg=white" eff)   "status-style fg=white (got ~S)" eff)
      (is (search "bg=blue" eff)    "status-style bg=blue (got ~S)" eff))))

;;; ── set-cursor-shape ─────────────────────────────────────────────────────────

(test set-cursor-shape-table
  "set-cursor-shape emits the DECSCUSR sequence ESC[N q for each shape number."
  (dolist (c '((2 "2 q" "shape 2 (steady block)")
               (1 "1 q" "shape 1 (blinking block)")))
    (destructuring-bind (shape suffix desc) c
      (let ((out (with-output-to-string (s) (cl-tmux/renderer::set-cursor-shape s shape))))
        (is (search (format nil "~C[~A" #\Escape suffix) out) "~A (got ~S)" desc out)))))

;;; ── %emit-fg / %emit-bg palette boundaries ────────────────────────────────────

(test emit-fg-bg-palette-boundary-table
  "Verifies fg/bg SGR emission at standard-colour, bright, and 256-colour boundaries.
   Each row is (fg bg expected-sgr-substring description)."
  (dolist (c '((7   0   ";37"      "fg 7 (white)")
               (0   0   ";40"      "bg 0 (black); fg=0 also emits ;30 but ;40 is present")
               (8   0   ";90"      "fg 8 (first bright)")
               (15  0   ";97"      "fg 15 (last bright)")
               (16  0   ";38;5;16" "fg 16 (first 256-colour)")
               (255 0   ";38;5;255" "fg 255 (last 256-colour)")
               (0   16  ";48;5;16" "bg 16 (first 256-colour)")
               (0   255 ";48;5;255" "bg 255 (last 256-colour)")))
    (destructuring-bind (fg bg expected desc) c
      (let ((out (cell-attrs-string fg bg 0)))
        (is (search expected out)
            "~A must emit ~S (got ~S)" desc expected out)))))

;;; ── render-cell-attrs all attributes table ───────────────────────────────────
