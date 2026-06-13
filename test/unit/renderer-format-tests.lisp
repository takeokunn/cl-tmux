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
  (is (string= (format nil "~C[1;1H" #\Escape)
               (with-output-to-string (s)
                 (cl-tmux/renderer::move-to s 0 0)))
      "move-to 0,0 should emit ESC[1;1H")
  (is (string= (format nil "~C[3;5H" #\Escape)
               (with-output-to-string (s)
                 (cl-tmux/renderer::move-to s 2 4)))
      "move-to 2,4 should emit ESC[3;5H"))

;;; ── render-cell-attrs (SGR codes) ───────────────────────────────────────────

(test render-cell-attrs-foreground
  (let ((out (cell-attrs-string 1 0 0)))
    (is (search ";31" out) "fg 1 should emit ;31 (got ~S)" out)))

(test render-cell-attrs-background
  (let ((out (cell-attrs-string 0 2 0)))
    (is (search ";42" out) "bg 2 should emit ;42 (got ~S)" out)))

(test render-cell-attrs-bright-foreground
  (let ((out (cell-attrs-string 9 0 0)))      ; bright fg uses 82+fg => 91
    (is (search ";91" out) "bright fg 9 should emit ;91 (got ~S)" out)))

(test render-cell-attrs-frame
  (let ((out (cell-attrs-string 1 2 1)))
    (is (eql 0 (search (format nil "~C[0" #\Escape) out))
        "render-cell-attrs should start with ESC[0 (got ~S)" out)
    (is (char= #\m (char out (1- (length out))))
        "render-cell-attrs should end with m (got ~S)" out)))

(test render-cell-attrs-default-color-omitted
  ;; fg/bg outside 0..15 emit no colour code — only the leading reset remains.
  (let ((out (cell-attrs-string -1 -1 0)))
    (is (string= (format nil "~C[0m" #\Escape) out)
        "out-of-range fg/bg should omit colour codes (got ~S)" out)))

;;; ── cursor-invisible / cursor-visible ───────────────────────────────────────

(test cursor-invisible-emits-hide-sequence
  (let ((out (with-output-to-string (s)
               (cl-tmux/renderer::cursor-invisible s))))
    (is (string= (format nil "~C[?25l" #\Escape) out)
        "cursor-invisible should emit ESC[?25l (got ~S)" out)))

(test cursor-visible-emits-show-sequence
  (let ((out (with-output-to-string (s)
               (cl-tmux/renderer::cursor-visible s))))
    (is (string= (format nil "~C[?25h" #\Escape) out)
        "cursor-visible should emit ESC[?25h (got ~S)" out)))

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

(test render-cell-attrs-256color-fg
  "fg index 200 (256-color palette) emits the sequence ;38;5;200 in the SGR string."
  (let ((out (cell-attrs-string 200 0 0)))
    (is (search ";38;5;200" out)
        "256-color fg 200 must emit ;38;5;200 (got ~S)" out)))

(test render-cell-attrs-256color-bg
  "bg index 42 (256-color palette) emits the sequence ;48;5;42 in the SGR string."
  (let ((out (cell-attrs-string 0 42 0)))
    (is (search ";48;5;42" out)
        "256-color bg 42 must emit ;48;5;42 (got ~S)" out)))

(test render-cell-attrs-truecolor-fg
  "True-color fg #x1FF8000 (R=255, G=128, B=0) emits ;38;2;255;128;0."
  (let* ((fg  (logior #x1000000 (ash 255 16) (ash 128 8) 0))
         (out (cell-attrs-string fg 0 0)))
    (is (search ";38;2;255;128;0" out)
        "truecolor fg must emit ;38;2;255;128;0 (got ~S)" out)))

(test render-cell-attrs-truecolor-bg
  "True-color bg #x10080FF (R=0, G=128, B=255) emits ;48;2;0;128;255."
  (let* ((bg  (logior #x1000000 (ash 0 16) (ash 128 8) 255))
         (out (cell-attrs-string 0 bg 0)))
    (is (search ";48;2;0;128;255" out)
        "truecolor bg must emit ;48;2;0;128;255 (got ~S)" out)))

;;; ── %split-style-tokens ─────────────────────────────────────────────────────

(test split-style-tokens-single-token
  "%split-style-tokens with a single token returns a one-element list."
  (is (equal '("bold") (cl-tmux/renderer::%split-style-tokens "bold"))
      "single token must return one-element list"))

(test split-style-tokens-multiple-tokens
  "%split-style-tokens splits on commas."
  (is (equal '("fg=red" "bold" "underline")
             (cl-tmux/renderer::%split-style-tokens "fg=red,bold,underline"))
      "multiple tokens must be split on commas"))

(test split-style-tokens-empty-string
  "%split-style-tokens with an empty string returns a list with one empty string."
  (let ((result (cl-tmux/renderer::%split-style-tokens "")))
    (is (= 1 (length result)) "empty string produces one element")
    (is (string= "" (first result)) "that element must be the empty string")))

;;; ── %dispatch-style-token ───────────────────────────────────────────────────

(test dispatch-style-token-bold
  "%dispatch-style-token 'bold' sets :bold T in result-cell."
  (let ((cell (list nil)))
    (is-true (cl-tmux/renderer::%dispatch-style-token "bold" cell)
             "%dispatch-style-token must return T on match")
    (is-true (getf (car cell) :bold)
             ":bold must be T after dispatch")))

(test dispatch-style-token-nobold
  "%dispatch-style-token 'nobold' sets :bold NIL in result-cell."
  (let ((cell (list (list :bold t))))
    (cl-tmux/renderer::%dispatch-style-token "nobold" cell)
    (is (null (getf (car cell) :bold))
        ":bold must be NIL after 'nobold' dispatch")))

(test dispatch-style-token-reverse
  "%dispatch-style-token 'reverse' sets :reverse T."
  (let ((cell (list nil)))
    (cl-tmux/renderer::%dispatch-style-token "reverse" cell)
    (is-true (getf (car cell) :reverse) ":reverse must be T")))

(test dispatch-style-token-underline
  "%dispatch-style-token 'underline' sets :underline T."
  (let ((cell (list nil)))
    (cl-tmux/renderer::%dispatch-style-token "underline" cell)
    (is-true (getf (car cell) :underline) ":underline must be T")))

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

(test border-color-sgr-known-color
  "%border-color-sgr returns the integer SGR code for a known colour name."
  (is (= 32 (cl-tmux/renderer::%border-color-sgr "green"))
      "%border-color-sgr 'green' must return 32")
  (is (= 31 (cl-tmux/renderer::%border-color-sgr "red"))
      "%border-color-sgr 'red' must return 31"))

(test border-color-sgr-unknown-returns-nil
  "%border-color-sgr returns NIL for an unrecognised colour name."
  (is (null (cl-tmux/renderer::%border-color-sgr "notacolor"))
      "%border-color-sgr must return NIL for unknown colour"))

(test border-color-sgr-case-insensitive
  "%border-color-sgr accepts mixed-case colour names."
  (is (= 34 (cl-tmux/renderer::%border-color-sgr "Blue"))
      "%border-color-sgr 'Blue' must return 34"))

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

(test status-sgr-from-style-nil-returns-default
  "%status-sgr-from-style with NIL returns the default blue-on-white SGR."
  (is (string= "44;97" (cl-tmux/renderer::%status-sgr-from-style nil))
      "%status-sgr-from-style nil must return \"44;97\""))

(test status-sgr-from-style-empty-returns-default
  "%status-sgr-from-style with empty string returns the default SGR."
  (is (string= "44;97" (cl-tmux/renderer::%status-sgr-from-style ""))
      "%status-sgr-from-style \"\" must return \"44;97\""))

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

(test set-cursor-shape-emits-decscusr
  "set-cursor-shape emits the DECSCUSR sequence ESC[Nq to the stream."
  (let ((out (with-output-to-string (s)
               (cl-tmux/renderer::set-cursor-shape s 2))))
    (is (search (format nil "~C[2 q" #\Escape) out)
        "set-cursor-shape 2 must emit ESC[2 q (got ~S)" out)))

(test set-cursor-shape-block-cursor
  "set-cursor-shape with shape 1 emits ESC[1 q (blinking block)."
  (let ((out (with-output-to-string (s)
               (cl-tmux/renderer::set-cursor-shape s 1))))
    (is (search (format nil "~C[1 q" #\Escape) out)
        "set-cursor-shape 1 must emit ESC[1 q (got ~S)" out)))

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
