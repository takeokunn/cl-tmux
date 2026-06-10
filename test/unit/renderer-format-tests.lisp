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

(test render-cell-attrs-bold
  (let ((out (cell-attrs-string 0 0 1)))      ; bit0 = bold
    (is (search ";1" out) "bold (attrs bit0) should emit ;1 (got ~S)" out)))

(test render-cell-attrs-reverse
  (let ((out (cell-attrs-string 0 0 4)))      ; bit2 = reverse video
    (is (search ";7" out) "reverse (attrs bit2) should emit ;7 (got ~S)" out)))

(test render-cell-attrs-bright-foreground
  (let ((out (cell-attrs-string 9 0 0)))      ; bright fg uses 82+fg => 91
    (is (search ";91" out) "bright fg 9 should emit ;91 (got ~S)" out)))

(test render-cell-attrs-frame
  (let ((out (cell-attrs-string 1 2 1)))
    (is (eql 0 (search (format nil "~C[0" #\Escape) out))
        "render-cell-attrs should start with ESC[0 (got ~S)" out)
    (is (char= #\m (char out (1- (length out))))
        "render-cell-attrs should end with m (got ~S)" out)))

(test render-cell-attrs-dim
  (let ((out (cell-attrs-string 7 0 2)))      ; bit1 = dim
    (is (search ";2" out) "dim (attrs bit1) should emit ;2 (got ~S)" out)))

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

(test render-cell-attrs-underline
  "Underline attribute (bit 3 = #x08 = +attr-underline+) emits ;4."
  (let ((out (cell-attrs-string 0 0 8)))     ; bit3 = underline
    (is (search ";4" out) "underline (attrs bit3) should emit ;4 (got ~S)" out)))

(test render-cell-attrs-blink
  "Blink attribute (bit 4 = #x10 = +attr-blink+) emits ;5."
  (let ((out (cell-attrs-string 0 0 16)))    ; bit4 = blink
    (is (search ";5" out) "blink (attrs bit4) should emit ;5 (got ~S)" out)))

(test define-cell-attr-renderer-macro-is-defined
  "define-cell-attr-renderer is a defined macro."
  (is (macro-function 'cl-tmux/renderer::define-cell-attr-renderer)))

;;; ── New attribute bits: emit path ───────────────────────────────────────────

(test render-cell-attrs-italic
  "Italic attribute (bit 5 = #x20 = +attr-italic+) emits ;3 in the SGR string."
  (let ((out (cell-attrs-string 0 0 32)))    ; bit5 = #b00100000 = 32
    (is (search ";3" out) "italic (attrs bit5) must emit ;3 (got ~S)" out)))

(test render-cell-attrs-conceal
  "Conceal attribute (bit 6 = #x40 = +attr-conceal+) emits ;8."
  (let ((out (cell-attrs-string 0 0 64)))    ; bit6 = #b01000000 = 64
    (is (search ";8" out) "conceal (attrs bit6) must emit ;8 (got ~S)" out)))

(test render-cell-attrs-strikethrough
  "Strikethrough attribute (bit 7 = #x80 = +attr-strikethrough+) emits ;9."
  (let ((out (cell-attrs-string 0 0 128)))   ; bit7 = #b10000000 = 128
    (is (search ";9" out) "strikethrough (attrs bit7) must emit ;9 (got ~S)" out)))

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

(test color-name-to-sgr-number-fg-named
  "%color-name-to-sgr-number with is-bg NIL returns foreground SGR fragment."
  (is (string= "31" (cl-tmux/renderer::%color-name-to-sgr-number "red" nil))
      "fg 'red' must produce \"31\""))

(test color-name-to-sgr-number-bg-named
  "%color-name-to-sgr-number with is-bg T returns background SGR fragment."
  (is (string= "41" (cl-tmux/renderer::%color-name-to-sgr-number "red" t))
      "bg 'red' must produce \"41\""))

(test color-name-to-sgr-number-colour-n-fg
  "%color-name-to-sgr-number for 'colour4' with is-bg NIL returns '38;5;4'."
  (is (string= "38;5;4" (cl-tmux/renderer::%color-name-to-sgr-number "colour4" nil))
      "fg 'colour4' must produce \"38;5;4\""))

(test color-name-to-sgr-number-colour-n-bg
  "%color-name-to-sgr-number for 'colour4' with is-bg T returns '48;5;4'."
  (is (string= "48;5;4" (cl-tmux/renderer::%color-name-to-sgr-number "colour4" t))
      "bg 'colour4' must produce \"48;5;4\""))

(test color-name-to-sgr-number-default-fg
  "%color-name-to-sgr-number for 'default' with is-bg NIL returns '39'."
  (is (string= "39" (cl-tmux/renderer::%color-name-to-sgr-number "default" nil))
      "fg 'default' must produce \"39\""))

(test color-name-to-sgr-number-default-bg
  "%color-name-to-sgr-number for 'default' with is-bg T returns '49'."
  (is (string= "49" (cl-tmux/renderer::%color-name-to-sgr-number "default" t))
      "bg 'default' must produce \"49\""))

(test color-name-to-sgr-number-unknown-fg
  "%color-name-to-sgr-number for unknown colour with is-bg NIL returns '39'."
  (is (string= "39" (cl-tmux/renderer::%color-name-to-sgr-number "notacolor" nil))
      "fg unknown must fall back to \"39\""))

(test color-name-to-sgr-number-unknown-bg
  "%color-name-to-sgr-number for unknown colour with is-bg T returns '49'."
  (is (string= "49" (cl-tmux/renderer::%color-name-to-sgr-number "notacolor" t))
      "bg unknown must fall back to \"49\""))

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

;;; ── %effective-status-style (deprecated status-fg/bg/attr fold-in) ───────────

(test effective-status-style-empty-when-nothing-set
  "%effective-status-style is empty when neither status-style nor the deprecated
   status-fg/bg/attr options are set."
  (with-isolated-config
    (is (string= "" (cl-tmux/renderer::%effective-status-style))
        "no style options set must yield the empty string")))

(test effective-status-style-folds-deprecated-bg-alone
  "An old .tmux.conf setting only status-bg (no status-style) folds to bg=<colour>."
  (with-isolated-config
    (cl-tmux/options:set-option "status-bg" "black")
    (is (string= "bg=black" (cl-tmux/renderer::%effective-status-style))
        "status-bg alone must fold to \"bg=black\"")))

(test effective-status-style-folds-deprecated-into-status-style
  "%effective-status-style appends the deprecated status-fg/bg/attr options to the
   modern status-style base so both are honoured."
  (with-isolated-config
    (cl-tmux/options:set-option "status-style" "bold")
    (cl-tmux/options:set-option "status-fg" "white")
    (cl-tmux/options:set-option "status-bg" "blue")
    (cl-tmux/options:set-option "status-attr" "underscore")
    (let ((eff (cl-tmux/renderer::%effective-status-style)))
      (is (search "bold" eff)       "status-style base preserved (got ~S)" eff)
      (is (search "fg=white" eff)   "status-fg folded in (got ~S)" eff)
      (is (search "bg=blue" eff)    "status-bg folded in (got ~S)" eff)
      (is (search "underscore" eff) "status-attr folded in (got ~S)" eff))))

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

(test emit-fg-palette-lower-boundary
  "%emit-fg with fg index 16 (first 256-color palette entry) emits ;38;5;16."
  (let ((out (cell-attrs-string 16 0 0)))
    (is (search ";38;5;16" out)
        "fg 16 must emit ;38;5;16 (got ~S)" out)))

(test emit-fg-palette-upper-boundary
  "%emit-fg with fg index 255 (last 256-color palette entry) emits ;38;5;255."
  (let ((out (cell-attrs-string 255 0 0)))
    (is (search ";38;5;255" out)
        "fg 255 must emit ;38;5;255 (got ~S)" out)))

(test emit-bg-palette-lower-boundary
  "%emit-bg with bg index 16 (first 256-color palette entry) emits ;48;5;16."
  (let ((out (cell-attrs-string 0 16 0)))
    (is (search ";48;5;16" out)
        "bg 16 must emit ;48;5;16 (got ~S)" out)))

(test emit-bg-palette-upper-boundary
  "%emit-bg with bg index 255 (last 256-color palette entry) emits ;48;5;255."
  (let ((out (cell-attrs-string 0 255 0)))
    (is (search ";48;5;255" out)
        "bg 255 must emit ;48;5;255 (got ~S)" out)))

(test emit-fg-bright-lower-boundary
  "%emit-fg with fg index 8 (first bright colour) emits ;90."
  (let ((out (cell-attrs-string 8 0 0)))
    (is (search ";90" out)
        "fg 8 must emit ;90 (got ~S)" out)))

(test emit-fg-bright-upper-boundary
  "%emit-fg with fg index 15 (last bright colour) emits ;97."
  (let ((out (cell-attrs-string 15 0 0)))
    (is (search ";97" out)
        "fg 15 must emit ;97 (got ~S)" out)))

(test emit-bg-standard-color-0
  "%emit-bg with bg index 0 (black) emits ;40."
  (let ((out (cell-attrs-string 0 0 0)))
    ;; fg=0 emits ;30, bg=0 emits ;40 — both present in a single reset+emit
    (is (search ";40" out)
        "bg 0 must emit ;40 (got ~S)" out)))

(test emit-fg-standard-color-7
  "%emit-fg with fg index 7 (white) emits ;37."
  (let ((out (cell-attrs-string 7 0 0)))
    (is (search ";37" out)
        "fg 7 must emit ;37 (got ~S)" out)))

;;; ── render-cell-attrs all attributes table ───────────────────────────────────

(test render-cell-attrs-all-attributes-table
  "Table-driven test: each attribute bit produces the expected SGR code."
  (let ((cases
         ;; (attr-bits expected-sgr-substr description)
         `((1   ";1"  "bold")
           (2   ";2"  "dim")
           (4   ";7"  "reverse")
           (8   ";4"  "underline")
           (16  ";5"  "blink")
           (32  ";3"  "italic")
           (64  ";8"  "conceal")
           (128 ";9"  "strikethrough"))))
    (dolist (c cases)
      (destructuring-bind (attrs expected desc) c
        (let ((out (cell-attrs-string 0 0 attrs)))
          (is (search expected out)
              "~A (attrs ~D) must emit ~S (got ~S)" desc attrs expected out))))))

;;; ── attrs2: double-underline and overline ────────────────────────────────────

(test render-cell-attrs2-double-underline
  "attrs2 bit 0 (+attr2-double-underline+) emits ;21 in the SGR string."
  (let ((out (cell-attrs-string 0 0 0 1)))    ; attrs2 bit0 = double-underline
    (is (search ";21" out) "double-underline (attrs2 bit0) must emit ;21 (got ~S)" out)))

(test render-cell-attrs2-overline
  "attrs2 bit 1 (+attr2-overline+) emits ;53 in the SGR string."
  (let ((out (cell-attrs-string 0 0 0 2)))    ; attrs2 bit1 = overline
    (is (search ";53" out) "overline (attrs2 bit1) must emit ;53 (got ~S)" out)))

(test render-cell-attrs2-double-underline-and-overline
  "attrs2 with both bits set emits both ;21 and ;53."
  (let ((out (cell-attrs-string 0 0 0 3)))    ; attrs2 bits 0+1
    (is (search ";21" out) "double-underline must be in combined attrs2 output (got ~S)" out)
    (is (search ";53" out) "overline must be in combined attrs2 output (got ~S)" out)))

(test render-cell-attrs2-zero-emits-nothing-extra
  "attrs2 = 0 does not emit ;21 or ;53."
  (let ((out (cell-attrs-string 0 0 0 0)))
    (is (not (search ";21" out)) "no double-underline when attrs2=0 (got ~S)" out)
    (is (not (search ";53" out)) "no overline when attrs2=0 (got ~S)" out)))

;;; ── ul-color: underline colour (SGR 58) ─────────────────────────────────────

(test render-cell-attrs-ul-color-zero-emits-nothing
  "ul-color = 0 (default) does not emit any ;58 sequence."
  (let ((out (cell-attrs-string 0 0 0 0 0)))
    (is (not (search ";58" out)) "ul-color=0 must not emit ;58 (got ~S)" out)))

(test render-cell-attrs-ul-color-palette
  "ul-color = 200 emits ;58;5;200."
  (let ((out (cell-attrs-string 0 0 0 0 200)))
    (is (search ";58;5;200" out) "palette ul-color 200 must emit ;58;5;200 (got ~S)" out)))

(test render-cell-attrs-ul-color-truecolor
  "ul-color true-color (R=255 G=0 B=128) emits ;58;2;255;0;128."
  (let* ((ul-color (logior #x1000000 (ash 255 16) (ash 0 8) 128))
         (out (cell-attrs-string 0 0 0 0 ul-color)))
    (is (search ";58;2;255;0;128" out)
        "truecolor ul-color must emit ;58;2;255;0;128 (got ~S)" out)))

;;; ── move-to additional positions ─────────────────────────────────────────────

(test move-to-large-coordinates
  "move-to with large row and col values produces the correct 1-based sequence."
  (is (string= (format nil "~C[100;200H" #\Escape)
               (with-output-to-string (s)
                 (cl-tmux/renderer::move-to s 99 199)))
      "move-to 99,199 should emit ESC[100;200H"))

;;; ── %dispatch-style-token remaining tokens ───────────────────────────────────

(test dispatch-style-token-dim
  "%dispatch-style-token 'dim' sets :dim T."
  (let ((cell (list nil)))
    (is-true (cl-tmux/renderer::%dispatch-style-token "dim" cell)
             "%dispatch-style-token must return T for 'dim'")
    (is-true (getf (car cell) :dim) ":dim must be T after dispatch")))

(test dispatch-style-token-italics
  "%dispatch-style-token 'italics' sets :italics T."
  (let ((cell (list nil)))
    (cl-tmux/renderer::%dispatch-style-token "italics" cell)
    (is-true (getf (car cell) :italics) ":italics must be T after dispatch")))

(test dispatch-style-token-blink
  "%dispatch-style-token 'blink' sets :blink T."
  (let ((cell (list nil)))
    (cl-tmux/renderer::%dispatch-style-token "blink" cell)
    (is-true (getf (car cell) :blink) ":blink must be T after dispatch")))

(test dispatch-style-token-conceal
  "%dispatch-style-token 'conceal' sets :conceal T."
  (let ((cell (list nil)))
    (cl-tmux/renderer::%dispatch-style-token "conceal" cell)
    (is-true (getf (car cell) :conceal) ":conceal must be T after dispatch")))

(test dispatch-style-token-strikethrough
  "%dispatch-style-token 'strikethrough' sets :strikethrough T."
  (let ((cell (list nil)))
    (cl-tmux/renderer::%dispatch-style-token "strikethrough" cell)
    (is-true (getf (car cell) :strikethrough) ":strikethrough must be T after dispatch")))

(test dispatch-style-token-nodim
  "%dispatch-style-token 'nodim' sets :dim NIL."
  (let ((cell (list (list :dim t))))
    (cl-tmux/renderer::%dispatch-style-token "nodim" cell)
    (is (null (getf (car cell) :dim)) ":dim must be NIL after 'nodim'")))

(test dispatch-style-token-noreverse
  "%dispatch-style-token 'noreverse' sets :reverse NIL."
  (let ((cell (list (list :reverse t))))
    (cl-tmux/renderer::%dispatch-style-token "noreverse" cell)
    (is (null (getf (car cell) :reverse)) ":reverse must be NIL after 'noreverse'")))

(test dispatch-style-token-nounderline
  "%dispatch-style-token 'nounderline' sets :underline NIL."
  (let ((cell (list (list :underline t))))
    (cl-tmux/renderer::%dispatch-style-token "nounderline" cell)
    (is (null (getf (car cell) :underline)) ":underline must be NIL after 'nounderline'")))

(test dispatch-style-token-noitalics
  "%dispatch-style-token 'noitalics' sets :italics NIL."
  (let ((cell (list (list :italics t))))
    (cl-tmux/renderer::%dispatch-style-token "noitalics" cell)
    (is (null (getf (car cell) :italics)) ":italics must be NIL after 'noitalics'")))

;;; ── %emit-style-attrs remaining attributes ───────────────────────────────────

(test emit-style-attrs-all-attributes-table
  "%emit-style-attrs table: each attribute key produces the expected SGR code string."
  (let ((cases '((:bold          "1")
                 (:dim           "2")
                 (:italics       "3")
                 (:underline     "4")
                 (:blink         "5")
                 (:reverse       "7")
                 (:conceal       "8")
                 (:strikethrough "9"))))
    (dolist (c cases)
      (destructuring-bind (key code) c
        (let ((parts (cl-tmux/renderer::%emit-style-attrs (list key t) nil)))
          (is (member code parts :test #'string=)
              "%emit-style-attrs ~S must push ~S (got ~S)" key code parts))))))

;;; ── parse-style-string remaining attributes ──────────────────────────────────

(test parse-style-string-dim
  "parse-style-string parses 'dim' into :dim T."
  (let ((p (cl-tmux/renderer:parse-style-string "dim")))
    (is (getf p :dim) "parse-style-string dim must set :dim T")))

(test parse-style-string-italics
  "parse-style-string parses 'italics' into :italics T."
  (let ((p (cl-tmux/renderer:parse-style-string "italics")))
    (is (getf p :italics) "parse-style-string italics must set :italics T")))

(test parse-style-string-blink
  "parse-style-string parses 'blink' into :blink T."
  (let ((p (cl-tmux/renderer:parse-style-string "blink")))
    (is (getf p :blink) "parse-style-string blink must set :blink T")))

(test parse-style-string-conceal
  "parse-style-string parses 'conceal' into :conceal T."
  (let ((p (cl-tmux/renderer:parse-style-string "conceal")))
    (is (getf p :conceal) "parse-style-string conceal must set :conceal T")))

(test parse-style-string-strikethrough
  "parse-style-string parses 'strikethrough' into :strikethrough T."
  (let ((p (cl-tmux/renderer:parse-style-string "strikethrough")))
    (is (getf p :strikethrough)
        "parse-style-string strikethrough must set :strikethrough T")))

(test parse-style-string-fg-and-bg-combined
  "parse-style-string parses both fg= and bg= in a combined style string."
  (let ((p (cl-tmux/renderer:parse-style-string "fg=green,bg=blue")))
    (is (string= "green" (getf p :fg)) ":fg must be 'green' (got ~S)" (getf p :fg))
    (is (string= "blue"  (getf p :bg)) ":bg must be 'blue' (got ~S)"  (getf p :bg))))

(test parse-style-string-whitespace-trimmed-around-tokens
  "parse-style-string trims whitespace from each token before dispatching."
  (let ((p (cl-tmux/renderer:parse-style-string " bold , reverse ")))
    (is (getf p :bold)    ":bold must be T after whitespace trimming")
    (is (getf p :reverse) ":reverse must be T after whitespace trimming")))

;;; ── style-to-sgr remaining attributes ───────────────────────────────────────

(test style-to-sgr-dim
  "style-to-sgr with :dim T includes SGR code 2."
  (let ((sgr (cl-tmux/renderer:style-to-sgr '(:dim t))))
    (is (search "2" sgr) "style-to-sgr :dim must include \"2\" (got ~S)" sgr)))

(test style-to-sgr-underline
  "style-to-sgr with :underline T includes SGR code 4."
  (let ((sgr (cl-tmux/renderer:style-to-sgr '(:underline t))))
    (is (search "4" sgr) "style-to-sgr :underline must include \"4\" (got ~S)" sgr)))

(test style-to-sgr-italics
  "style-to-sgr with :italics T includes SGR code 3."
  (let ((sgr (cl-tmux/renderer:style-to-sgr '(:italics t))))
    (is (search "3" sgr) "style-to-sgr :italics must include \"3\" (got ~S)" sgr)))

(test style-to-sgr-fg-colour-n
  "style-to-sgr with :fg \"colour200\" includes 38;5;200."
  (let ((sgr (cl-tmux/renderer:style-to-sgr '(:fg "colour200"))))
    (is (search "38;5;200" sgr)
        "style-to-sgr :fg colour200 must include 38;5;200 (got ~S)" sgr)))

(test style-to-sgr-empty-plist-returns-default
  "style-to-sgr with an empty plist (no keys set) returns the default SGR."
  (is (string= "44;97" (cl-tmux/renderer:style-to-sgr '()))
      "style-to-sgr empty plist must return default \"44;97\""))

;;; ── %color-name-to-sgr-number brightblack / brightwhite ─────────────────────

(test color-name-to-sgr-number-brightblack-fg
  "%color-name-to-sgr-number for 'brightblack' (fg) returns the bright ANSI code."
  (is (string= "90" (cl-tmux/renderer::%color-name-to-sgr-number "brightblack" nil))
      "fg 'brightblack' must produce \"90\""))

(test color-name-to-sgr-number-brightwhite-bg
  "%color-name-to-sgr-number for 'brightwhite' (bg) returns the bright background code."
  (is (string= "107" (cl-tmux/renderer::%color-name-to-sgr-number "brightwhite" t))
      "bg 'brightwhite' must produce \"107\""))

;;; ── %border-color-sgr all named colours table ────────────────────────────────

(test border-color-sgr-all-named-colors
  "%border-color-sgr returns the expected SGR code for each named colour."
  (let ((cases '(("black"   . 30) ("red"     . 31) ("green"  . 32)
                 ("yellow"  . 33) ("blue"    . 34) ("magenta". 35)
                 ("cyan"    . 36) ("white"   . 37))))
    (dolist (c cases)
      (is (= (cdr c) (cl-tmux/renderer::%border-color-sgr (car c)))
          "%border-color-sgr ~S must return ~D" (car c) (cdr c)))))
