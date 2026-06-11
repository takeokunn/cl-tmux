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

;;; ── colon-delimited (ISO 8613-6) SGR ─────────────────────────────────────────
;;;
;;; Modern apps (neovim, many TUIs) emit true-colour as 38:2:R:G:B and 256-colour
;;; as 38:5:N with COLON separators, optionally with a colourspace-id field
;;; (38:2:cs:R:G:B) which may be empty (38:2::R:G:B).  The parser groups a colon
;;; parameter into a list so apply-sgr applies it (rather than dropping it).

(test sgr-colon-group-direct-truecolor
  "apply-sgr with a colon group (a list) sets the true-colour encoding."
  (with-screen (s 10 2)
    (cl-tmux/terminal/sgr:apply-sgr s '((38 2 255 128 0)))
    (is (= (logior #x1000000 (ash 255 16) (ash 128 8) 0)
           (cl-tmux/terminal/types:screen-cur-fg s))
        "colon group (38 2 255 128 0) must encode #x1FF8000 in cur-fg")))

(test sgr-colon-truecolor-fg-via-emulator
  "ESC[38:2:255:128:0m (ISO 8613-6 colon true-colour) sets fg like the ; form."
  (with-screen (s 10 2)
    (feed s (esc "[38:2:255:128:0mX"))
    (is (= (logior #x1000000 (ash 255 16) (ash 128 8) 0) (fg-at s 0 0))
        "colon true-colour must encode #x1FF8000 in the cell fg")))

(test sgr-colon-truecolor-empty-colorspace
  "ESC[38:2::255:128:0m (empty colourspace-id field) still applies the RGB —
   the RGB are taken as the last three sub-parameters, skipping the empty field."
  (with-screen (s 10 2)
    (feed s (esc "[38:2::255:128:0mX"))
    (is (= (logior #x1000000 (ash 255 16) (ash 128 8) 0) (fg-at s 0 0))
        "empty-colourspace colon true-colour must still encode the RGB")))

(test sgr-colon-truecolor-explicit-colorspace
  "ESC[38:2:1:255:128:0m (explicit colourspace-id 1) skips the CS field, applies RGB."
  (with-screen (s 10 2)
    (feed s (esc "[38:2:1:255:128:0mX"))
    (is (= (logior #x1000000 (ash 255 16) (ash 128 8) 0) (fg-at s 0 0))
        "explicit-colourspace colon true-colour must skip CS and encode the RGB")))

(test sgr-colon-256color-via-emulator
  "ESC[38:5:200m (colon 256-colour) sets fg=200."
  (with-screen (s 10 2)
    (feed s (esc "[38:5:200mX"))
    (is (= 200 (fg-at s 0 0)) "colon 256-colour must set fg=200")))

(test sgr-colon-truecolor-bg-via-emulator
  "ESC[48:2:0:128:255m sets the background true-colour."
  (with-screen (s 10 2)
    (feed s (esc "[48:2:0:128:255mX"))
    (is (= (logior #x1000000 (ash 0 16) (ash 128 8) 255) (bg-at s 0 0))
        "colon true-colour must encode the bg")))

(test sgr-colon-mixed-with-semicolon-params
  "ESC[1;38:2:255:0:0m — a colon group amid ;-params: bold AND true-red fg."
  (with-screen (s 10 2)
    (feed s (esc "[1;38:2:255:0:0mX"))
    (is (= (logior #x1000000 (ash 255 16)) (fg-at s 0 0))
        "fg must be true red despite the leading bold ;-param")
    (is-true (logbitp 0 (attrs-at s 0 0)) "bold bit (0) must also be set")))

(test sgr-colon-undercurl-applies-underline
  "A (4 3) colon group (undercurl) applies underline (4) — same pen attrs as SGR 4."
  (with-screen (s 10 2)
    (cl-tmux/terminal/sgr:apply-sgr s '(4))
    (let ((plain-underline (cl-tmux/terminal/types:screen-cur-attrs s)))
      (cl-tmux/terminal/sgr:apply-sgr s '(0))           ; reset pen
      (cl-tmux/terminal/sgr:apply-sgr s '((4 3)))       ; undercurl colon group
      (is (= plain-underline (cl-tmux/terminal/types:screen-cur-attrs s))
          "(4 3) must set the same attrs as plain SGR 4"))))

;;; ── %pen-to-sgr-params (inverse SGR, for DECRQSS) ────────────────────────────

(test pen-to-sgr-params-reset
  "A default pen (fg 7, bg 0, no attrs) reconstructs to just \"0\"."
  (is (string= "0" (cl-tmux/terminal/sgr:%pen-to-sgr-params 7 0 0 0))
      "default pen → \"0\""))

(test pen-to-sgr-params-bold-red
  "Bold (attr bit 0) + red fg (1) reconstructs to \"0;1;31\"."
  (is (string= "0;1;31" (cl-tmux/terminal/sgr:%pen-to-sgr-params 1 0 1 0))
      "bold red → \"0;1;31\""))

(test pen-to-sgr-params-truecolor-fg
  "A true-colour fg reconstructs to 0;38;2;R;G;B."
  (is (string= "0;38;2;255;128;0"
               (cl-tmux/terminal/sgr:%pen-to-sgr-params
                (logior #x1000000 (ash 255 16) (ash 128 8) 0) 0 0 0))
      "truecolor fg → 0;38;2;255;128;0"))

(test pen-to-sgr-params-bright-bg
  "A bright bg (index 12) reconstructs to 0;104; default fg (7) is omitted."
  (is (string= "0;104" (cl-tmux/terminal/sgr:%pen-to-sgr-params 7 12 0 0))
      "bright bg 12 → \"0;104\""))

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
