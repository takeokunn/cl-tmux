(in-package #:cl-tmux/test)

;;;; sgr tests — part B: direct-action-sgr suite, sgr-extended (double-underline,
;;;; overline, ul-color), extra SGR codes, direct-dispatch helpers, truecolor edge cases,
;;;; define-sgr-rules macro, consume-256-color-param.

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

(test sgr-reset-sgr-pen-helper
  "reset-sgr-pen sets fg=7, bg=0, attrs=0 directly."
  (with-screen (s 10 2)
    (cl-tmux/terminal/sgr:apply-sgr s '(31 42 1))   ; fg=1, bg=2, bold
    (cl-tmux/terminal/types:reset-sgr-pen s)
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

;;; ── SGR codes not yet exercised ───────────────────────────────────────────────
;;;
;;; Coverage gaps identified by source audit:
;;;   SGR 0  (standard fg 30 / bg 40 for black)
;;;   SGR 6  (rapid-blink, maps to blink bit)
;;;   SGR 25 (blink off)
;;;   SGR 27 (reverse off)
;;;   SGR 51/52 (framed/encircled, silently accepted)
;;;   Bright background 100-107
;;;   %dispatch-sgr-code direct call
;;;   attr2-on / attr2-off direct calls

(in-suite sgr)

(test sgr-black-foreground-30
  "SGR 30 sets the foreground to index 0 (black)."
  (with-screen (s 10 2)
    (feed s (esc "[30mX"))
    (is (= 0 (fg-at s 0 0)) "SGR 30 must set fg to 0 (black)")))

(test sgr-black-background-40
  "SGR 40 sets the background to index 0 (black)."
  (with-screen (s 10 2)
    (feed s (esc "[40mX"))
    (is (= 0 (bg-at s 0 0)) "SGR 40 must set bg to 0 (black)")))

(test sgr-rapid-blink-6-sets-blink-bit
  "SGR 6 (rapid blink) maps to the same blink bit as SGR 5."
  (with-screen (s 10 2)
    (feed s (esc "[6mB"))
    (is (logbitp 4 (attrs-at s 0 0)) "rapid-blink (SGR 6) must set the blink bit (4)")))

(test sgr-blink-off-25
  "SGR 25 clears the blink attribute bit (4)."
  (with-screen (s 10 2)
    (feed s (esc "[5mB"))   ; blink on
    (feed s (esc "[25mX"))  ; blink off
    (is-false (logbitp 4 (attrs-at s 1 0))
              "blink bit (4) must be cleared by SGR 25")))

(test sgr-reverse-off-27
  "SGR 27 clears the reverse-video attribute bit."
  (with-screen (s 10 2)
    (feed s (esc "[7mR"))   ; reverse on
    (feed s (esc "[27mX"))  ; reverse off
    (is (zerop (logand (attrs-at s 1 0) #b100))
        "reverse bit must be cleared by SGR 27")))

(test sgr-framed-51-accepted-silently
  "SGR 51 (framed) is accepted without error and does not alter standard attrs."
  (with-screen (s 10 2)
    (finishes (feed s (esc "[51mX")))
    ;; No standard attribute bit should be set by SGR 51.
    (is (zerop (logand (attrs-at s 0 0) #b1111111))
        "SGR 51 must not set any standard attribute bits")))

(test sgr-encircled-52-accepted-silently
  "SGR 52 (encircled) is accepted without error and does not alter standard attrs."
  (with-screen (s 10 2)
    (finishes (feed s (esc "[52mX")))
    (is (zerop (logand (attrs-at s 0 0) #b1111111))
        "SGR 52 must not set any standard attribute bits")))

(test sgr-bright-background-table
  "Bright background SGR codes 100-107 set bg indices 8-15."
  (loop for code from 100 to 107
        for expected-bg from 8 to 15
        do (with-screen (s 10 2)
             (feed s (esc "[~DmX" code))
             (is (= expected-bg (bg-at s 0 0))
                 "SGR ~D: expected bg ~D got ~D"
                 code expected-bg (bg-at s 0 0)))))

;;; ── direct-action-sgr additional ─────────────────────────────────────────────

(in-suite direct-action-sgr)

(test dispatch-sgr-code-directly-foreground
  "%dispatch-sgr-code sets foreground directly when called with code 31."
  (with-screen (s 10 2)
    (cl-tmux/terminal/sgr:%dispatch-sgr-code s 31)
    (is (= 1 (cl-tmux/terminal/types:screen-cur-fg s))
        "%dispatch-sgr-code 31 must set cur-fg to 1 (red)")))

(test dispatch-sgr-code-directly-background
  "%dispatch-sgr-code sets background directly when called with code 42."
  (with-screen (s 10 2)
    (cl-tmux/terminal/sgr:%dispatch-sgr-code s 42)
    (is (= 2 (cl-tmux/terminal/types:screen-cur-bg s))
        "%dispatch-sgr-code 42 must set cur-bg to 2 (green)")))

(test dispatch-sgr-code-unknown-is-noop
  "%dispatch-sgr-code silently ignores unrecognized SGR codes."
  (with-screen (s 10 2)
    (finishes (cl-tmux/terminal/sgr:%dispatch-sgr-code s 999))
    ;; SGR state should remain at default after an unknown code.
    (check-sgr-state s :fg 7 :bg 0 :attrs 0)))

(test attr2-on-and-off-helpers
  "attr2-on sets a bit in cur-attrs2; attr2-off clears it without touching others."
  (with-screen (s 10 2)
    (cl-tmux/terminal/sgr::attr2-on s cl-tmux/terminal/types:+attr2-overline+)
    (cl-tmux/terminal/sgr::attr2-on s cl-tmux/terminal/types:+attr2-double-underline+)
    (is (not (zerop (logand (cl-tmux/terminal/types:screen-cur-attrs2 s)
                            cl-tmux/terminal/types:+attr2-overline+)))
        "attr2-on must set overline bit")
    (is (not (zerop (logand (cl-tmux/terminal/types:screen-cur-attrs2 s)
                            cl-tmux/terminal/types:+attr2-double-underline+)))
        "attr2-on must set double-underline bit")
    ;; Now clear only overline.
    (cl-tmux/terminal/sgr::attr2-off s cl-tmux/terminal/types:+attr2-overline+)
    (is (zerop (logand (cl-tmux/terminal/types:screen-cur-attrs2 s)
                       cl-tmux/terminal/types:+attr2-overline+))
        "attr2-off must clear overline bit")
    (is (not (zerop (logand (cl-tmux/terminal/types:screen-cur-attrs2 s)
                            cl-tmux/terminal/types:+attr2-double-underline+)))
        "attr2-off for overline must leave double-underline untouched")))

;;; ── SGR truecolor edge cases ─────────────────────────────────────────────────

(in-suite sgr-extended)

(test sgr-truecolor-underline-color
  "SGR 58;2;R;G;B sets cur-ul-color to the true-color encoding."
  (with-screen (s 10 2)
    (cl-tmux/terminal/sgr:apply-sgr s '(58 2 255 0 128))
    (let ((expected (logior #x1000000 (ash 255 16) (ash 0 8) 128)))
      (is (= expected (cl-tmux/terminal/types:screen-cur-ul-color s))
          "apply-sgr 58;2;255;0;128 must encode true-color in cur-ul-color"))))

;;; ── Coverage gap: define-sgr-rules macro ─────────────────────────────────────
;;;
;;; Audit finding: define-sgr-rules was not tested as a macro in isolation.
;;; The generated %dispatch-sgr-code now also carries a docstring; verify it.

(in-suite direct-action-sgr)

(test define-sgr-rules-macro-is-defined
  "define-sgr-rules is a defined macro in the sgr package."
  (is (macro-function 'cl-tmux/terminal/sgr::define-sgr-rules)
      "define-sgr-rules must be a macro"))

(test dispatch-sgr-code-has-docstring
  "%dispatch-sgr-code (exported) has a non-empty docstring injected by the macro."
  (let ((doc (documentation 'cl-tmux/terminal/sgr:%dispatch-sgr-code 'function)))
    (is (and (stringp doc) (plusp (length doc)))
        "%dispatch-sgr-code must have a non-empty docstring")))

;;; ── Coverage gap: %consume-256-color-param direct test ───────────────────────
;;;
;;; The %consume-256-color-param helper was extracted from apply-sgr to eliminate
;;; code duplication across the 38/48/58 256-color arms.

(test consume-256-color-param-sets-fg-and-advances
  "%consume-256-color-param stores the clamped index via SETTER and returns the
   tail after the three consumed elements."
  (with-screen (s 10 2)
    ;; Simulate the 38;5;42 arm: parameter-tail = (38 5 42 99)
    (let* ((parameter-tail '(38 5 42 99))
           (tail (cl-tmux/terminal/sgr::%consume-256-color-param
                  s #'(setf cl-tmux/terminal/types:screen-cur-fg) parameter-tail)))
      (is (= 42 (cl-tmux/terminal/types:screen-cur-fg s))
          "%consume-256-color-param must store index 42 via the setter")
      (is (equal '(99) tail)
          "%consume-256-color-param must return the tail after the 3 consumed elements"))))

(test consume-256-color-param-clamps-to-255
  "%consume-256-color-param clamps an out-of-range index (> 255) to 255."
  (with-screen (s 10 2)
    (cl-tmux/terminal/sgr::%consume-256-color-param
     s #'(setf cl-tmux/terminal/types:screen-cur-fg) '(38 5 300))
    (is (= 255 (cl-tmux/terminal/types:screen-cur-fg s))
        "index 300 must be clamped to 255")))
