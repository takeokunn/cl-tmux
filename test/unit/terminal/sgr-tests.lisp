(in-package #:cl-tmux/test)

;;;; SGR tests (src/terminal/sgr.lisp).
;;;; Tests: sgr suite.

;;; ── SUITE: sgr ──────────────────────────────────────────────────────────────

(def-suite sgr
  :description "Select Graphic Rendition — colour and attribute codes"
  :in terminal-suite)
(in-suite sgr)

(test sgr-foreground-table
  "Standard foreground SGR codes 31-37 set fg indices 1-7."
  (loop for code from 31 to 37
        for expected-fg from 1 to 7
        do (with-screen (s 10 2)
             (feed s (esc "[~DmX" code))
             (is (= expected-fg (fg-at s 0 0))
                 "SGR ~D: expected fg ~D got ~D"
                 code expected-fg (fg-at s 0 0)))))

(test sgr-background-table
  "Standard background SGR codes 41-47 set bg indices 1-7."
  (loop for code from 41 to 47
        for expected-bg from 1 to 7
        do (with-screen (s 10 2)
             (feed s (esc "[~DmX" code))
             (is (= expected-bg (bg-at s 0 0))
                 "SGR ~D: expected bg ~D got ~D"
                 code expected-bg (bg-at s 0 0)))))

(test sgr-bright-foreground-table
  "Bright foreground SGR codes 90-97 set fg indices 8-15."
  (loop for code from 90 to 97
        for expected-fg from 8 to 15
        do (with-screen (s 10 2)
             (feed s (esc "[~DmX" code))
             (is (= expected-fg (fg-at s 0 0))
                 "SGR ~D: expected fg ~D got ~D"
                 code expected-fg (fg-at s 0 0)))))

(test sgr-bold
  "SGR 1 sets the bold attribute bit."
  (with-screen (s 10 2)
    (feed s (esc "[1mB"))
    (is (logbitp 0 (attrs-at s 0 0)) "bold bit not set")))

(test sgr-dim
  "SGR 2 sets the dim attribute bit."
  (with-screen (s 10 2)
    (feed s (esc "[2mD"))
    (is (not (zerop (logand (attrs-at s 0 0) #b010))) "dim bit not set")))

(test sgr-reverse
  "SGR 7 sets the reverse-video attribute bit."
  (with-screen (s 10 2)
    (feed s (esc "[7mR"))
    (is (not (zerop (logand (attrs-at s 0 0) #b100))) "reverse bit not set")))

(test sgr-reset
  "SGR 0 after setting colours and bold restores defaults on the next cell."
  (with-screen (s 10 2)
    (feed s (esc "[31;1mX"))
    (feed s (esc "[0mY"))
    (check-cell s 1 0 :fg 7 :bg 0 :attrs 0)))

(test sgr-underline
  "SGR 4 sets the underline attribute bit (bit 3 = #x08)."
  (with-screen (s 10 2)
    (feed s (esc "[4mU"))
    (is (logbitp 3 (attrs-at s 0 0)) "underline bit (3) must be set after SGR 4")))

(test sgr-blink
  "SGR 5 sets the blink attribute bit (bit 4 = #x10)."
  (with-screen (s 10 2)
    (feed s (esc "[5mB"))
    (is (logbitp 4 (attrs-at s 0 0)) "blink bit (4) must be set after SGR 5")))

(test sgr-default-fg-39
  "SGR 39 resets the foreground colour to the default (7) without touching bg or attrs."
  (with-screen (s 10 2)
    (feed s (esc "[31m"))    ; fg → 1 (red)
    (feed s (esc "[39mX"))   ; fg → 7 (default)
    (check-sgr-state s :fg 7 :bg 0 :attrs 0)
    (is (= 7 (fg-at s 0 0)) "cell fg must be default (7) after SGR 39")))

(test sgr-default-bg-49
  "SGR 49 resets the background colour to the default (0) without touching fg or attrs."
  (with-screen (s 10 2)
    (feed s (esc "[42m"))    ; bg → 2 (green)
    (feed s (esc "[49mX"))   ; bg → 0 (default)
    (check-sgr-state s :fg 7 :bg 0 :attrs 0)
    (is (= 0 (bg-at s 0 0)) "cell bg must be default (0) after SGR 49")))

(test sgr-bold-dim-off-22
  "SGR 22 clears both the bold bit (0) and the dim bit (1)."
  (with-screen (s 10 2)
    (feed s (esc "[1;2m"))   ; bold + dim on
    (feed s (esc "[22mX"))   ; both off
    (is (zerop (logand (attrs-at s 0 0) #b011))
        "SGR 22 must clear both bold (bit0) and dim (bit1)")))

(test sgr-compound
  "ESC[1;31;42m sets bold, fg=1, bg=2 simultaneously."
  (with-screen (s 10 2)
    (feed s (esc "[1;31;42mX"))
    (is (= 1 (fg-at s 0 0))   "fg expected 1")
    (is (= 2 (bg-at s 0 0))   "bg expected 2")
    (is (logbitp 0 (attrs-at s 0 0)) "bold bit not set")))

(test sgr-bright-red
  "ESC[91m sets fg=9 (bright red)."
  (with-screen (s 10 2)
    (feed s (esc "[91mR"))
    (is (= 9 (fg-at s 0 0)) "expected fg 9 (bright red)")))

(test sgr-italic-sets-italic-bit-not-dim
  "SGR 3 sets the italic attribute bit (5) and must NOT set the dim bit (1)."
  (with-screen (s 10 2)
    (feed s (esc "[3mX"))
    (is (logbitp 5 (attrs-at s 0 0)) "italic bit (5) must be set after SGR 3")
    (is-false (logbitp 1 (attrs-at s 0 0))
              "dim bit (1) must NOT be set by SGR 3 (regression: was incorrectly aliased to dim)")))

(test sgr-italic-off-23
  "SGR 23 clears the italic bit (5) without touching other attributes."
  (with-screen (s 10 2)
    (feed s (esc "[3;1mX"))  ; italic + bold on
    (feed s (esc "[23mY"))   ; italic off
    (is-false (logbitp 5 (attrs-at s 1 0)) "italic bit (5) must be cleared by SGR 23")
    (is       (logbitp 0 (attrs-at s 1 0)) "bold bit (0) must remain after SGR 23")))

(test sgr-conceal-sets-bit
  "SGR 8 sets the conceal attribute bit (6)."
  (with-screen (s 10 2)
    (feed s (esc "[8mX"))
    (is (logbitp 6 (attrs-at s 0 0)) "conceal bit (6) must be set after SGR 8")))

(test sgr-conceal-off-28
  "SGR 28 clears the conceal bit (6)."
  (with-screen (s 10 2)
    (feed s (esc "[8mX"))
    (feed s (esc "[28mY"))
    (is-false (logbitp 6 (attrs-at s 1 0)) "conceal bit (6) must be cleared by SGR 28")))

(test sgr-strikethrough-sets-bit
  "SGR 9 sets the strikethrough attribute bit (7)."
  (with-screen (s 10 2)
    (feed s (esc "[9mX"))
    (is (logbitp 7 (attrs-at s 0 0)) "strikethrough bit (7) must be set after SGR 9")))

(test sgr-strikethrough-off-29
  "SGR 29 clears the strikethrough bit (7)."
  (with-screen (s 10 2)
    (feed s (esc "[9mX"))
    (feed s (esc "[29mY"))
    (is-false (logbitp 7 (attrs-at s 1 0)) "strikethrough bit (7) must be cleared by SGR 29")))

(test sgr-256color-fg
  "SGR 38;5;N sets the fg to the 256-color palette index N."
  (with-screen (s 10 2)
    (cl-tmux/terminal/sgr:apply-sgr s '(38 5 200))
    (is (= 200 (cl-tmux/terminal/types:screen-cur-fg s))
        "apply-sgr 38;5;200 must set cur-fg to 200")))

(test sgr-256color-bg
  "SGR 48;5;N sets the bg to the 256-color palette index N."
  (with-screen (s 10 2)
    (cl-tmux/terminal/sgr:apply-sgr s '(48 5 42))
    (is (= 42 (cl-tmux/terminal/types:screen-cur-bg s))
        "apply-sgr 48;5;42 must set cur-bg to 42")))

(test sgr-truecolor-fg
  "SGR 38;2;R;G;B sets fg to the true-color encoding (bit 24 set, bits 0-23 = RGB)."
  (with-screen (s 10 2)
    (cl-tmux/terminal/sgr:apply-sgr s '(38 2 255 128 0))
    (let ((expected (logior #x1000000 (ash 255 16) (ash 128 8) 0)))
      (is (= expected (cl-tmux/terminal/types:screen-cur-fg s))
          "apply-sgr 38;2;255;128;0 must encode #x1FF8000 in cur-fg"))))

(test sgr-truecolor-bg
  "SGR 48;2;R;G;B sets bg to the true-color encoding."
  (with-screen (s 10 2)
    (cl-tmux/terminal/sgr:apply-sgr s '(48 2 0 128 255))
    (let ((expected (logior #x1000000 (ash 0 16) (ash 128 8) 255)))
      (is (= expected (cl-tmux/terminal/types:screen-cur-bg s))
          "apply-sgr 48;2;0;128;255 must encode true-color in cur-bg"))))

(test sgr-256color-fg-via-emulator
  "ESC[38;5;200m fed through the emulator sets fg=200 on the next written cell."
  (with-screen (s 10 2)
    (feed s (esc "[38;5;200mX"))
    (is (= 200 (fg-at s 0 0)) "256-color fg=200 must be stored in cell after ESC[38;5;200m")))

(test sgr-256color-bg-via-emulator
  "ESC[48;5;42m fed through the emulator sets bg=42 on the next written cell."
  (with-screen (s 10 2)
    (feed s (esc "[48;5;42mX"))
    (is (= 42 (bg-at s 0 0)) "256-color bg=42 must be stored in cell after ESC[48;5;42m")))

(test sgr-truecolor-black
  "SGR 38;2;0;0;0 encodes true-black: bit 24 set, R=G=B=0."
  (with-screen (s 10 2)
    (cl-tmux/terminal/sgr:apply-sgr s '(38 2 0 0 0))
    (is (= #x1000000 (cl-tmux/terminal/types:screen-cur-fg s))
        "true-black must be #x1000000 (bit 24 set, RGB=0)")))

(test sgr-reset-clears-new-attrs
  "SGR 0 after setting italic, conceal, and strikethrough zeroes all attr bits."
  (with-screen (s 10 2)
    (feed s (esc "[3;8;9mX"))    ; italic + conceal + strikethrough on
    (feed s (esc "[0mY"))        ; SGR reset
    (check-cell s 1 0 :fg 7 :bg 0 :attrs 0)))

(test sgr-22-does-not-clear-italic
  "SGR 22 (bold+dim off) must NOT clear the italic bit (5)."
  (with-screen (s 10 2)
    (feed s (esc "[1;2;3mX"))    ; bold + dim + italic on
    (feed s (esc "[22mY"))       ; bold + dim off
    (is (logbitp 5 (attrs-at s 1 0))
        "italic bit (5) must survive SGR 22")))

;;; ── SUITE: direct-action-sgr ─────────────────────────────────────────────────
;;;
;;; These tests call apply-sgr directly rather than through screen-process-bytes,
;;; targeting edge cases that the CSI/parser path may not hit explicitly.

(def-suite direct-action-sgr
  :description "Direct calls to apply-sgr"
  :in terminal-suite)
(in-suite direct-action-sgr)

(test apply-sgr-directly-updates-screen-attributes
  "apply-sgr called directly updates the screen's current SGR state."
  (with-screen (s 10 5)
    ;; SGR 31 = foreground red (index 1)
    (cl-tmux/terminal/sgr:apply-sgr s '(31))
    (is (= 1 (cl-tmux/terminal/types:screen-cur-fg s))
        "apply-sgr 31 must set cur-fg to 1 (red)")
    ;; SGR 0 = reset
    (cl-tmux/terminal/sgr:apply-sgr s '(0))
    (is (= 7 (cl-tmux/terminal/types:screen-cur-fg s))
        "apply-sgr 0 must reset cur-fg to 7 (default)")
    ;; Empty params = implicit reset
    (cl-tmux/terminal/sgr:apply-sgr s '(42))      ; bg green
    (cl-tmux/terminal/sgr:apply-sgr s nil)         ; empty = reset
    (is (= 0 (cl-tmux/terminal/types:screen-cur-bg s))
        "apply-sgr nil (empty) must reset cur-bg to 0")))

(test sgr-reset-sgr-helper
  "reset-sgr helper sets fg=7, bg=0, attrs=0 directly."
  (with-screen (s 10 2)
    (cl-tmux/terminal/sgr:apply-sgr s '(31 42 1))   ; fg=1, bg=2, bold
    (cl-tmux/terminal/sgr::reset-sgr s)
    (check-sgr-state s :fg 7 :bg 0 :attrs 0)))

(test sgr-attr-on-helper
  "attr-on adds a single attribute bit without clearing others."
  (with-screen (s 10 2)
    (cl-tmux/terminal/sgr::attr-on s cl-tmux/terminal/types:+attr-bold+)
    (cl-tmux/terminal/sgr::attr-on s cl-tmux/terminal/types:+attr-underline+)
    (is (logbitp 0 (cl-tmux/terminal/types:screen-cur-attrs s)) "bold must be on")
    (is (logbitp 3 (cl-tmux/terminal/types:screen-cur-attrs s)) "underline must be on")))

(test sgr-attr-off-helper
  "attr-off clears a single attribute bit without touching others."
  (with-screen (s 10 2)
    (cl-tmux/terminal/sgr::attr-on s cl-tmux/terminal/types:+attr-bold+)
    (cl-tmux/terminal/sgr::attr-on s cl-tmux/terminal/types:+attr-dim+)
    (cl-tmux/terminal/sgr::attr-off s cl-tmux/terminal/types:+attr-dim+)
    (is      (logbitp 0 (cl-tmux/terminal/types:screen-cur-attrs s)) "bold must remain")
    (is-false (logbitp 1 (cl-tmux/terminal/types:screen-cur-attrs s)) "dim must be cleared")))

;;; ── SGR 21 double-underline ───────────────────────────────────────────────────

(def-suite sgr-extended
  :description "Extended SGR attributes: double-underline, overline, underline-color"
  :in terminal-suite)
(in-suite sgr-extended)

(test sgr-21-double-underline
  "SGR 21 sets the +attr2-double-underline+ bit in cur-attrs2."
  (with-screen (s 10 2)
    (feed s (esc "[21mX"))
    (is (not (zerop (logand (cl-tmux/terminal/types:screen-cur-attrs2 s)
                            cl-tmux/terminal/types:+attr2-double-underline+)))
        "double-underline bit must be set in cur-attrs2 after SGR 21")))

(test sgr-21-double-underline-cleared-by-24
  "SGR 24 clears both the underline bit and the double-underline bit."
  (with-screen (s 10 2)
    (feed s (esc "[4;21mX"))   ; underline + double-underline on
    (feed s (esc "[24mY"))     ; underline off
    (is-false (logbitp 3 (cl-tmux/terminal/types:screen-cur-attrs s))
              "underline bit must be cleared by SGR 24")
    (is (zerop (logand (cl-tmux/terminal/types:screen-cur-attrs2 s)
                       cl-tmux/terminal/types:+attr2-double-underline+))
        "double-underline bit must be cleared by SGR 24")))

(test sgr-53-overline-sets-bit
  "SGR 53 sets the +attr2-overline+ bit in cur-attrs2."
  (with-screen (s 10 2)
    (feed s (esc "[53mX"))
    (is (not (zerop (logand (cl-tmux/terminal/types:screen-cur-attrs2 s)
                            cl-tmux/terminal/types:+attr2-overline+)))
        "overline bit must be set in cur-attrs2 after SGR 53")))

(test sgr-55-overline-off
  "SGR 55 clears the +attr2-overline+ bit in cur-attrs2."
  (with-screen (s 10 2)
    (feed s (esc "[53mX"))    ; overline on
    (feed s (esc "[55mY"))    ; overline off
    (is (zerop (logand (cl-tmux/terminal/types:screen-cur-attrs2 s)
                       cl-tmux/terminal/types:+attr2-overline+))
        "overline bit must be cleared by SGR 55")))

(test sgr-58-underline-color-256
  "SGR 58;5;42 sets cur-ul-color to 42 (256-color palette index)."
  (with-screen (s 10 2)
    (cl-tmux/terminal/sgr:apply-sgr s '(58 5 42))
    (is (= 42 (cl-tmux/terminal/types:screen-cur-ul-color s))
        "cur-ul-color must be 42 after SGR 58;5;42")))

(test sgr-59-resets-underline-color
  "SGR 59 resets the underline color to default (0)."
  (with-screen (s 10 2)
    (cl-tmux/terminal/sgr:apply-sgr s '(58 5 42))
    (cl-tmux/terminal/sgr:apply-sgr s '(59))
    (is (= 0 (cl-tmux/terminal/types:screen-cur-ul-color s))
        "cur-ul-color must be 0 after SGR 59")))
