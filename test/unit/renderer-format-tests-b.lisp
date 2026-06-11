(in-package #:cl-tmux/test)

;;;; renderer-format tests — part B: render-cell-attrs all-attributes table,
;;;; attrs2 double-underline/overline, ul-color, move-to, dispatch-style-token remaining,
;;;; emit-style-attrs remaining, parse-style-string, style-to-sgr, border-color table,
;;;; dispatch-border-charset padded/none.

(in-suite renderer-suite)

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

;;; ── %dispatch-border-charset padded/none styles ──────────────────────────────
;;;
;;; The 'padded' and 'none' styles both return all-space characters.
;;; These were previously untested (only rounded/double/heavy/single/simple covered).

(test dispatch-border-charset-padded-all-spaces
  "%dispatch-border-charset 'padded' returns all space characters."
  (multiple-value-bind (tl tr bl br h v)
      (cl-tmux/renderer::%dispatch-border-charset "padded")
    (is (char= #\Space tl) "padded tl must be space")
    (is (char= #\Space tr) "padded tr must be space")
    (is (char= #\Space bl) "padded bl must be space")
    (is (char= #\Space br) "padded br must be space")
    (is (char= #\Space h)  "padded h must be space")
    (is (char= #\Space v)  "padded v must be space")))

(test dispatch-border-charset-none-all-spaces
  "%dispatch-border-charset 'none' returns all space characters."
  (multiple-value-bind (tl tr bl br h v)
      (cl-tmux/renderer::%dispatch-border-charset "none")
    (is (char= #\Space tl) "none tl must be space")
    (is (char= #\Space tr) "none tr must be space")
    (is (char= #\Space bl) "none bl must be space")
    (is (char= #\Space br) "none br must be space")
    (is (char= #\Space h)  "none h must be space")
    (is (char= #\Space v)  "none v must be space")))

(test dispatch-border-charset-unknown-falls-back-to-single
  "%dispatch-border-charset with an unknown style returns the single-line characters."
  (multiple-value-bind (tl tr bl br h v)
      (cl-tmux/renderer::%dispatch-border-charset "this-is-unknown")
    (is (char= #\┌ tl) "unknown style tl falls back to ┌")
    (is (char= #\┐ tr) "unknown style tr falls back to ┐")
    (is (char= #\└ bl) "unknown style bl falls back to └")
    (is (char= #\┘ br) "unknown style br falls back to ┘")
    (is (char= #\─ h)  "unknown style h falls back to ─")
    (is (char= #\│ v)  "unknown style v falls back to │")))
