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

(test render-cell-attrs2-single-bit-table
  "Each attrs2 bit alone emits the correct SGR code."
  (dolist (c '((1 ";21" "double-underline (attrs2 bit0) → ;21")
               (2 ";53" "overline (attrs2 bit1) → ;53")))
    (destructuring-bind (attrs2 expected desc) c
      (let ((out (cell-attrs-string 0 0 0 attrs2)))
        (is (search expected out) "~A (got ~S)" desc out)))))

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

(test render-cell-attrs-ul-color-table
  "ul-color: 0 emits nothing; palette 200 emits ;58;5;200; truecolor emits ;58;2;255;0;128."
  (dolist (row (list (list 0                                              nil             "ul-color=0 must not emit ;58")
                     (list 200                                            ";58;5;200"     "palette ul-color 200 must emit ;58;5;200")
                     (list (logior #x1000000 (ash 255 16) (ash 0 8) 128) ";58;2;255;0;128" "truecolor must emit ;58;2;255;0;128")))
    (destructuring-bind (ul-color expected-sub desc) row
      (let ((out (cell-attrs-string 0 0 0 0 ul-color)))
        (if expected-sub
            (is (search expected-sub out) "~A (got ~S)" desc out)
            (is (not (search ";58" out)) "~A (got ~S)" desc out))))))

;;; ── move-to additional positions ─────────────────────────────────────────────

(test move-to-large-coordinates
  "move-to with large row and col values produces the correct 1-based sequence."
  (is (string= (format nil "~C[100;200H" #\Escape)
               (with-output-to-string (s)
                 (cl-tmux/renderer::move-to s 99 199)))
      "move-to 99,199 should emit ESC[100;200H"))

;;; ── %dispatch-style-token remaining tokens ───────────────────────────────────

(test dispatch-style-token-extra-attrs-table
  "%dispatch-style-token sets the correct plist key for dim, italics, blink, conceal, strikethrough."
  (dolist (c '(("dim"           :dim)
               ("italics"       :italics)
               ("blink"         :blink)
               ("conceal"       :conceal)
               ("strikethrough" :strikethrough)))
    (destructuring-bind (token key) c
      (let ((cell (list nil)))
        (is-true (cl-tmux/renderer::%dispatch-style-token token cell) "~A: returns T" token)
        (is-true (getf (car cell) key) "~A: attr set" token)))))

(test dispatch-style-token-no-attrs-table
  "%dispatch-style-token clears the correct plist key for each 'no*' token."
  (dolist (c '(("nodim"       :dim)
               ("noreverse"   :reverse)
               ("nounderline" :underline)
               ("noitalics"   :italics)))
    (destructuring-bind (token key) c
      (let ((cell (list (list key t))))
        (cl-tmux/renderer::%dispatch-style-token token cell)
        (is (null (getf (car cell) key)) "~A: ~S must be NIL" token key)))))

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

(test parse-style-string-attributes-table
  "parse-style-string correctly maps each attribute name to its plist key."
  (let ((cases '(("dim"           . :dim)
                 ("italics"       . :italics)
                 ("blink"         . :blink)
                 ("conceal"       . :conceal)
                 ("strikethrough" . :strikethrough))))
    (dolist (c cases)
      (let ((p (cl-tmux/renderer:parse-style-string (car c))))
        (is (getf p (cdr c))
            "parse-style-string ~S must set ~S T" (car c) (cdr c))))))

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

(test style-to-sgr-attributes-table
  "style-to-sgr emits the correct SGR code for each boolean attribute."
  (let ((cases '((:dim       "2")
                 (:underline "4")
                 (:italics   "3"))))
    (dolist (c cases)
      (destructuring-bind (key code) c
        (let ((sgr (cl-tmux/renderer:style-to-sgr (list key t))))
          (is (search code sgr)
              "style-to-sgr ~S must include ~S (got ~S)" key code sgr))))))

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

(test color-name-to-sgr-number-bright-table
  "%color-name-to-sgr-number maps bright colour names to correct fg/bg codes."
  (dolist (c '(("brightblack" nil "90"  "fg brightblack → 90")
               ("brightwhite" t   "107" "bg brightwhite → 107")))
    (destructuring-bind (color is-bg expected desc) c
      (is (string= expected (cl-tmux/renderer::%color-name-to-sgr-number color is-bg))
          "~A" desc))))

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

(test dispatch-border-charset-all-spaces-table
  "%dispatch-border-charset 'padded' and 'none' both return all space characters."
  (dolist (style '("padded" "none"))
    (multiple-value-bind (tl tr bl br h v)
        (cl-tmux/renderer::%dispatch-border-charset style)
      (is (char= #\Space tl) "~A tl must be space" style)
      (is (char= #\Space tr) "~A tr must be space" style)
      (is (char= #\Space bl) "~A bl must be space" style)
      (is (char= #\Space br) "~A br must be space" style)
      (is (char= #\Space h)  "~A h must be space" style)
      (is (char= #\Space v)  "~A v must be space" style))))

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

;;; ── %border-charset-for / %popup-border-charset (option-driven lookup) ──────

(test border-charset-for-reads-named-option
  "%border-charset-for reads OPTION-NAME and dispatches on its value."
  (with-isolated-options ("popup-border-lines" "double")
    (multiple-value-bind (tl tr bl br h v)
        (cl-tmux/renderer::%border-charset-for "popup-border-lines")
      (is (char= #\╔ tl) "double popup-border-lines tl must be ╔")
      (is (char= #\╗ tr) "double popup-border-lines tr must be ╗")
      (is (char= #\╚ bl) "double popup-border-lines bl must be ╚")
      (is (char= #\╝ br) "double popup-border-lines br must be ╝")
      (is (char= #\═ h)  "double popup-border-lines h must be ═")
      (is (char= #\║ v)  "double popup-border-lines v must be ║"))))

(test border-charset-for-defaults-to-single-when-unset
  "%border-charset-for falls back to single-line glyphs when the option is unset."
  (with-isolated-config
    (multiple-value-bind (tl tr bl br h v)
        (cl-tmux/renderer::%border-charset-for "menu-border-lines")
      (is (char= #\┌ tl) "unset menu-border-lines tl must be ┌")
      (is (char= #\┐ tr) "unset menu-border-lines tr must be ┐")
      (is (char= #\└ bl) "unset menu-border-lines bl must be └")
      (is (char= #\┘ br) "unset menu-border-lines br must be ┘")
      (is (char= #\─ h)  "unset menu-border-lines h must be ─")
      (is (char= #\│ v)  "unset menu-border-lines v must be │"))))

(test popup-border-charset-delegates-to-popup-border-lines-option
  "%popup-border-charset returns the charset for the popup-border-lines option
   specifically (not menu-border-lines or any other *-border-lines option)."
  (with-isolated-options ("popup-border-lines" "heavy")
    (multiple-value-bind (tl tr bl br h v)
        (cl-tmux/renderer::%popup-border-charset)
      (is (char= #\┏ tl) "heavy popup-border-lines tl must be ┏")
      (is (char= #\┓ tr) "heavy popup-border-lines tr must be ┓")
      (is (char= #\┗ bl) "heavy popup-border-lines bl must be ┗")
      (is (char= #\┛ br) "heavy popup-border-lines br must be ┛")
      (is (char= #\━ h)  "heavy popup-border-lines h must be ━")
      (is (char= #\┃ v)  "heavy popup-border-lines v must be ┃"))))

;;; ── %center-coord (box/clock centring) ───────────────────────────────────────

(test center-coord-table
  "%center-coord returns floor((total-size)/2), clamped to 0 when size >= total."
  (dolist (c '((80 20 30 "80 wide, size 20 -> offset 30")
               (10 10  0 "size equals total -> offset 0")
               (10 20  0 "size larger than total -> clamped to 0")
               (81 40 20 "odd total centres via floor")))
    (destructuring-bind (total size expected desc) c
      (is (= expected (cl-tmux/renderer::%center-coord total size)) "~A" desc))))

;;; ── %emit-sgr (raw SGR code emission) ────────────────────────────────────────

(test emit-sgr-writes-escape-sequence-for-code
  "%emit-sgr writes ESC[CODEm for an integer or string code."
  (is (string= (format nil "~C[44m" #\Escape)
               (with-output-to-string (s) (cl-tmux/renderer::%emit-sgr s 44)))
      "integer code 44 must emit ESC[44m")
  (is (string= (format nil "~C[44;97m" #\Escape)
               (with-output-to-string (s) (cl-tmux/renderer::%emit-sgr s "44;97")))
      "compound string code must emit verbatim inside ESC[...m"))

(test emit-sgr-nil-code-is-a-no-op
  "%emit-sgr with a NIL code writes nothing to the stream."
  (is (string= "" (with-output-to-string (s) (cl-tmux/renderer::%emit-sgr s nil)))
      "NIL code must emit no output"))

;;; ── %classify-color-name (colour-name classification) ────────────────────────

(test classify-color-name-table
  "%classify-color-name returns (values KIND PAYLOAD) for colourN, default, named,
   and unrecognised colour names."
  (dolist (c '(("colour200" :colour-n 200 "colourN → :colour-n with parsed integer")
               ("default"   :default  nil "\"default\" → :default nil")
               ("red"       :named    31  "named colour → :named with fg SGR code")
               ("bogus"     nil       nil "unrecognised name → nil nil")))
    (destructuring-bind (name expected-kind expected-payload desc) c
      (multiple-value-bind (kind payload) (cl-tmux/renderer::%classify-color-name name)
        (is (eql expected-kind kind) "~A: kind" desc)
        (is (eql expected-payload payload) "~A: payload" desc)))))

(test classify-color-name-colour-n-unparseable-suffix
  "%classify-color-name with a non-numeric colourN suffix returns :colour-n with a
   NIL payload (junk-allowed parse failure)."
  (multiple-value-bind (kind payload) (cl-tmux/renderer::%classify-color-name "colourxyz")
    (is (eql :colour-n kind) "colourxyz kind must be :colour-n")
    (is (null payload) "colourxyz payload must be NIL (unparseable)")))
