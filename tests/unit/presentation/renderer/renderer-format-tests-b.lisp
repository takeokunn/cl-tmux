(in-package #:cl-tmux/test)

;;;; renderer-format tests — part B: render-cell-attrs all-attributes table,
;;;; attrs2 double-underline/overline, ul-color, move-to, dispatch-style-token remaining,
;;;; emit-style-attrs remaining, parse-style-string, style-to-sgr, border-color table,
;;;; dispatch-border-charset padded/none.

(describe "renderer-suite"

  ;; Table-driven test: each attribute bit produces the expected SGR code.
  (it "render-cell-attrs-all-attributes-table"
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
          (declare (ignore desc))
          (let ((out (cell-attrs-string 0 0 attrs)))
            (expect (search expected out)))))))

  ;;; ── attrs2: double-underline and overline ────────────────────────────────────

  ;; Each attrs2 bit alone emits the correct SGR code.
  (it "render-cell-attrs2-single-bit-table"
    (dolist (c '((1 ";21" "double-underline (attrs2 bit0) → ;21")
                 (2 ";53" "overline (attrs2 bit1) → ;53")))
      (destructuring-bind (attrs2 expected desc) c
        (declare (ignore desc))
        (let ((out (cell-attrs-string 0 0 0 attrs2)))
          (expect (search expected out))))))

  ;; attrs2 with both bits set emits both ;21 and ;53.
  (it "render-cell-attrs2-double-underline-and-overline"
    (let ((out (cell-attrs-string 0 0 0 3)))    ; attrs2 bits 0+1
      (expect (search ";21" out))
      (expect (search ";53" out))))

  ;; attrs2 = 0 does not emit ;21 or ;53.
  (it "render-cell-attrs2-zero-emits-nothing-extra"
    (let ((out (cell-attrs-string 0 0 0 0)))
      (expect (not (search ";21" out)))
      (expect (not (search ";53" out)))))

  ;;; ── ul-color: underline colour (SGR 58) ─────────────────────────────────────

  ;; ul-color: 0 emits nothing; palette 200 emits ;58;5;200; truecolor emits ;58;2;255;0;128.
  (it "render-cell-attrs-ul-color-table"
    (dolist (row (list (list 0                                              nil             "ul-color=0 must not emit ;58")
                       (list 200                                            ";58;5;200"     "palette ul-color 200 must emit ;58;5;200")
                       (list (logior #x1000000 (ash 255 16) (ash 0 8) 128) ";58;2;255;0;128" "truecolor must emit ;58;2;255;0;128")))
      (destructuring-bind (ul-color expected-sub desc) row
        (declare (ignore desc))
        (let ((out (cell-attrs-string 0 0 0 0 ul-color)))
          (if expected-sub
              (expect (search expected-sub out))
              (expect (not (search ";58" out))))))))

  ;;; ── move-to additional positions ─────────────────────────────────────────────

  ;; move-to with large row and col values produces the correct 1-based sequence.
  (it "move-to-large-coordinates"
    (expect (string= (format nil "~C[100;200H" #\Escape)
                     (with-output-to-string (s)
                       (cl-tmux/renderer::move-to s 99 199)))))

  ;;; ── %dispatch-style-token remaining tokens ───────────────────────────────────

  ;; %dispatch-style-token sets the correct plist key for dim, italics, blink, conceal, strikethrough.
  (it "dispatch-style-token-extra-attrs-table"
    (dolist (c '(("dim"           :dim)
                 ("italics"       :italics)
                 ("blink"         :blink)
                 ("conceal"       :conceal)
                 ("strikethrough" :strikethrough)))
      (destructuring-bind (token key) c
        (let ((cell (list nil)))
          (expect (cl-tmux/renderer::%dispatch-style-token token cell) :to-be-truthy)
          (expect (getf (car cell) key) :to-be-truthy)))))

  ;; %dispatch-style-token clears the correct plist key for each 'no*' token.
  (it "dispatch-style-token-no-attrs-table"
    (dolist (c '(("nodim"       :dim)
                 ("noreverse"   :reverse)
                 ("nounderline" :underline)
                 ("noitalics"   :italics)))
      (destructuring-bind (token key) c
        (let ((cell (list (list key t))))
          (cl-tmux/renderer::%dispatch-style-token token cell)
          (expect (null (getf (car cell) key)))))))

  ;;; ── %emit-style-attrs remaining attributes ───────────────────────────────────

  ;; %emit-style-attrs table: each attribute key produces the expected SGR code string.
  (it "emit-style-attrs-all-attributes-table"
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
            (expect (member code parts :test #'string=)))))))

  ;;; ── parse-style-string remaining attributes ──────────────────────────────────

  ;; parse-style-string correctly maps each attribute name to its plist key.
  (it "parse-style-string-attributes-table"
    (let ((cases '(("dim"           . :dim)
                   ("italics"       . :italics)
                   ("blink"         . :blink)
                   ("conceal"       . :conceal)
                   ("strikethrough" . :strikethrough))))
      (dolist (c cases)
        (let ((p (cl-tmux/renderer:parse-style-string (car c))))
          (expect (getf p (cdr c)))))))

  ;; parse-style-string parses both fg= and bg= in a combined style string.
  (it "parse-style-string-fg-and-bg-combined"
    (let ((p (cl-tmux/renderer:parse-style-string "fg=green,bg=blue")))
      (expect (string= "green" (getf p :fg)))
      (expect (string= "blue"  (getf p :bg)))))

  ;; parse-style-string trims whitespace from each token before dispatching.
  (it "parse-style-string-whitespace-trimmed-around-tokens"
    (let ((p (cl-tmux/renderer:parse-style-string " bold , reverse ")))
      (expect (getf p :bold))
      (expect (getf p :reverse))))

  ;;; ── style-to-sgr remaining attributes ───────────────────────────────────────

  ;; style-to-sgr emits the correct SGR code for each boolean attribute.
  (it "style-to-sgr-attributes-table"
    (let ((cases '((:dim       "2")
                   (:underline "4")
                   (:italics   "3"))))
      (dolist (c cases)
        (destructuring-bind (key code) c
          (let ((sgr (cl-tmux/renderer:style-to-sgr (list key t))))
            (expect (search code sgr)))))))

  ;; style-to-sgr with :fg "colour200" includes 38;5;200.
  (it "style-to-sgr-fg-colour-n"
    (let ((sgr (cl-tmux/renderer:style-to-sgr '(:fg "colour200"))))
      (expect (search "38;5;200" sgr))))

  ;; style-to-sgr with an empty plist (no keys set) returns the default SGR.
  (it "style-to-sgr-empty-plist-returns-default"
    (expect (string= "44;97" (cl-tmux/renderer:style-to-sgr '()))))

  ;;; ── %color-name-to-sgr-number brightblack / brightwhite ─────────────────────

  ;; %color-name-to-sgr-number maps bright colour names to correct fg/bg codes.
  (it "color-name-to-sgr-number-bright-table"
    (dolist (c '(("brightblack" nil "90"  "fg brightblack → 90")
                 ("brightwhite" t   "107" "bg brightwhite → 107")))
      (destructuring-bind (color is-bg expected desc) c
        (declare (ignore desc))
        (expect (string= expected (cl-tmux/renderer::%color-name-to-sgr-number color is-bg))))))

  ;;; ── %border-color-sgr all named colours table ────────────────────────────────

  ;; %border-color-sgr returns the expected SGR code for each named colour.
  (it "border-color-sgr-all-named-colors"
    (let ((cases '(("black"   . 30) ("red"     . 31) ("green"  . 32)
                   ("yellow"  . 33) ("blue"    . 34) ("magenta". 35)
                   ("cyan"    . 36) ("white"   . 37))))
      (dolist (c cases)
        (expect (= (cdr c) (cl-tmux/renderer::%border-color-sgr (car c)))))))

  ;;; ── %dispatch-border-charset padded/none styles ──────────────────────────────
  ;;;
  ;;; The 'padded' and 'none' styles both return all-space characters.
  ;;; These were previously untested (only rounded/double/heavy/single/simple covered).

  ;; %dispatch-border-charset 'padded' and 'none' both return all space characters.
  (it "dispatch-border-charset-all-spaces-table"
    (dolist (style '("padded" "none"))
      (multiple-value-bind (tl tr bl br h v)
          (cl-tmux/renderer::%dispatch-border-charset style)
        (expect (char= #\Space tl))
        (expect (char= #\Space tr))
        (expect (char= #\Space bl))
        (expect (char= #\Space br))
        (expect (char= #\Space h))
        (expect (char= #\Space v)))))

  ;; %dispatch-border-charset with an unknown style returns the single-line characters.
  (it "dispatch-border-charset-unknown-falls-back-to-single"
    (multiple-value-bind (tl tr bl br h v)
        (cl-tmux/renderer::%dispatch-border-charset "this-is-unknown")
      (expect (char= #\┌ tl))
      (expect (char= #\┐ tr))
      (expect (char= #\└ bl))
      (expect (char= #\┘ br))
      (expect (char= #\─ h))
      (expect (char= #\│ v))))

  ;;; ── %border-charset-for / %popup-border-charset (option-driven lookup) ──────

  ;; %border-charset-for reads OPTION-NAME and dispatches on its value.
  (it "border-charset-for-reads-named-option"
    (with-isolated-options ("popup-border-lines" "double")
      (multiple-value-bind (tl tr bl br h v)
          (cl-tmux/renderer::%border-charset-for "popup-border-lines")
        (expect (char= #\╔ tl))
        (expect (char= #\╗ tr))
        (expect (char= #\╚ bl))
        (expect (char= #\╝ br))
        (expect (char= #\═ h))
        (expect (char= #\║ v)))))

  ;; %border-charset-for falls back to single-line glyphs when the option is unset.
  (it "border-charset-for-defaults-to-single-when-unset"
    (with-isolated-config
      (multiple-value-bind (tl tr bl br h v)
          (cl-tmux/renderer::%border-charset-for "menu-border-lines")
        (expect (char= #\┌ tl))
        (expect (char= #\┐ tr))
        (expect (char= #\└ bl))
        (expect (char= #\┘ br))
        (expect (char= #\─ h))
        (expect (char= #\│ v)))))

  ;; %popup-border-charset returns the charset for the popup-border-lines option
  ;; specifically (not menu-border-lines or any other *-border-lines option).
  (it "popup-border-charset-delegates-to-popup-border-lines-option"
    (with-isolated-options ("popup-border-lines" "heavy")
      (multiple-value-bind (tl tr bl br h v)
          (cl-tmux/renderer::%popup-border-charset)
        (expect (char= #\┏ tl))
        (expect (char= #\┓ tr))
        (expect (char= #\┗ bl))
        (expect (char= #\┛ br))
        (expect (char= #\━ h))
        (expect (char= #\┃ v)))))

  ;;; ── %center-coord (box/clock centring) ───────────────────────────────────────

  ;; %center-coord returns floor((total-size)/2), clamped to 0 when size >= total.
  (it "center-coord-table"
    (dolist (c '((80 20 30 "80 wide, size 20 -> offset 30")
                 (10 10  0 "size equals total -> offset 0")
                 (10 20  0 "size larger than total -> clamped to 0")
                 (81 40 20 "odd total centres via floor")))
      (destructuring-bind (total size expected desc) c
        (declare (ignore desc))
        (expect (= expected (cl-tmux/renderer::%center-coord total size))))))

  ;;; ── %emit-sgr (raw SGR code emission) ────────────────────────────────────────

  ;; %emit-sgr writes ESC[CODEm for an integer or string code.
  (it "emit-sgr-writes-escape-sequence-for-code"
    (expect (string= (format nil "~C[44m" #\Escape)
                     (with-output-to-string (s) (cl-tmux/renderer::%emit-sgr s 44))))
    (expect (string= (format nil "~C[44;97m" #\Escape)
                     (with-output-to-string (s) (cl-tmux/renderer::%emit-sgr s "44;97")))))

  ;; %emit-sgr with a NIL code writes nothing to the stream.
  (it "emit-sgr-nil-code-is-a-no-op"
    (expect (string= "" (with-output-to-string (s) (cl-tmux/renderer::%emit-sgr s nil)))))

  ;;; ── %classify-color-name (colour-name classification) ────────────────────────

  ;; %classify-color-name returns (values KIND PAYLOAD) for colourN, default, named,
  ;; and unrecognised colour names.
  (it "classify-color-name-table"
    (dolist (c '(("colour200" :colour-n 200 "colourN → :colour-n with parsed integer")
                 ("default"   :default  nil "\"default\" → :default nil")
                 ("red"       :named    31  "named colour → :named with fg SGR code")
                 ("bogus"     nil       nil "unrecognised name → nil nil")))
      (destructuring-bind (name expected-kind expected-payload desc) c
        (declare (ignore desc))
        (multiple-value-bind (kind payload) (cl-tmux/renderer::%classify-color-name name)
          (expect (eql expected-kind kind))
          (expect (eql expected-payload payload))))))

  ;; %classify-color-name with a non-numeric colourN suffix returns :colour-n with a
  ;; NIL payload (junk-allowed parse failure).
  (it "classify-color-name-colour-n-unparseable-suffix"
    (multiple-value-bind (kind payload) (cl-tmux/renderer::%classify-color-name "colourxyz")
      (expect (eql :colour-n kind))
      (expect (null payload)))))
