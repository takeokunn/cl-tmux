(in-package #:cl-tmux/test)

;;;; sgr tests — part B: direct-action-sgr suite, sgr-extended (double-underline,
;;;; overline, ul-color), extra SGR codes, direct-dispatch helpers, truecolor edge cases,
;;;; define-sgr-rules macro, consume-256-color-param.

;;; ── SUITE: direct-action-sgr ─────────────────────────────────────────────────
;;;
;;; These tests call apply-sgr directly rather than through screen-process-bytes,
;;; targeting edge cases that the CSI/parser path may not hit explicitly.

(describe "terminal-suite/direct-action-sgr"

  ;; apply-sgr called directly updates the screen's current SGR state.
  (it "apply-sgr-directly-updates-screen-attributes"
    (with-screen (s 10 5)
      ;; SGR 31 = foreground red (index 1)
      (cl-tmux/terminal/sgr:apply-sgr s '(31))
      (expect (= 1 (cl-tmux/terminal/types:screen-cur-fg s)))
      ;; SGR 0 = reset
      (cl-tmux/terminal/sgr:apply-sgr s '(0))
      (expect (= cl-tmux/terminal/types:+default-color+ (cl-tmux/terminal/types:screen-cur-fg s)))
      ;; Empty params = implicit reset
      (cl-tmux/terminal/sgr:apply-sgr s '(42))      ; bg green
      (cl-tmux/terminal/sgr:apply-sgr s nil)         ; empty = reset
      (expect (= cl-tmux/terminal/types:+default-color+ (cl-tmux/terminal/types:screen-cur-bg s)))))

  ;; SGR 39 sets cur-fg to the +default-color+ sentinel (not palette 7).
  (it "apply-sgr-39-sets-default-sentinel"
    (with-screen (s 10 2)
      (cl-tmux/terminal/sgr:apply-sgr s '(31))   ; red first
      (cl-tmux/terminal/sgr:apply-sgr s '(39))   ; default fg
      (expect (= cl-tmux/terminal/types:+default-color+
             (cl-tmux/terminal/types:screen-cur-fg s)))))

  ;; SGR 49 sets cur-bg to the +default-color+ sentinel (not palette 0).
  (it "apply-sgr-49-sets-default-sentinel"
    (with-screen (s 10 2)
      (cl-tmux/terminal/sgr:apply-sgr s '(42))   ; green bg first
      (cl-tmux/terminal/sgr:apply-sgr s '(49))   ; default bg
      (expect (= cl-tmux/terminal/types:+default-color+
             (cl-tmux/terminal/types:screen-cur-bg s)))))

  ;; reset-sgr-pen sets fg=7, bg=0, attrs=0 directly.
  (it "sgr-reset-sgr-pen-helper"
    (with-screen (s 10 2)
      (cl-tmux/terminal/sgr:apply-sgr s '(31 42 1))   ; fg=1, bg=2, bold
      (cl-tmux/terminal/types:reset-sgr-pen s)
      (check-sgr-state s :fg cl-tmux/terminal/types:+default-color+ :bg cl-tmux/terminal/types:+default-color+ :attrs 0)))

  ;; attr-on adds a single attribute bit without clearing others.
  (it "sgr-attr-on-helper"
    (with-screen (s 10 2)
      (cl-tmux/terminal/sgr::attr-on s cl-tmux/terminal/types:+attr-bold+)
      (cl-tmux/terminal/sgr::attr-on s cl-tmux/terminal/types:+attr-underline+)
      (expect (logbitp 0 (cl-tmux/terminal/types:screen-cur-attrs s)))
      (expect (logbitp 3 (cl-tmux/terminal/types:screen-cur-attrs s)))))

  ;; attr-off clears a single attribute bit without touching others.
  (it "sgr-attr-off-helper"
    (with-screen (s 10 2)
      (cl-tmux/terminal/sgr::attr-on s cl-tmux/terminal/types:+attr-bold+)
      (cl-tmux/terminal/sgr::attr-on s cl-tmux/terminal/types:+attr-dim+)
      (cl-tmux/terminal/sgr::attr-off s cl-tmux/terminal/types:+attr-dim+)
      (expect (logbitp 0 (cl-tmux/terminal/types:screen-cur-attrs s)))
      (expect (logbitp 1 (cl-tmux/terminal/types:screen-cur-attrs s)) :to-be-falsy))))

;;; ── SGR 21 double-underline ───────────────────────────────────────────────────

(describe "terminal-suite/sgr-extended"

  ;; SGR 21 sets the +attr2-double-underline+ bit in cur-attrs2.
  (it "sgr-21-double-underline"
    (with-screen (s 10 2)
      (feed s (esc "[21mX"))
      (expect (not (zerop (logand (cl-tmux/terminal/types:screen-cur-attrs2 s)
                              cl-tmux/terminal/types:+attr2-double-underline+))))))

  ;; SGR 24 clears both the underline bit and the double-underline bit.
  (it "sgr-21-double-underline-cleared-by-24"
    (with-screen (s 10 2)
      (feed s (esc "[4;21mX"))   ; underline + double-underline on
      (feed s (esc "[24mY"))     ; underline off
      (expect (logbitp 3 (cl-tmux/terminal/types:screen-cur-attrs s)) :to-be-falsy)
      (expect (zerop (logand (cl-tmux/terminal/types:screen-cur-attrs2 s)
                         cl-tmux/terminal/types:+attr2-double-underline+)))))

  ;; SGR 53 sets the +attr2-overline+ bit in cur-attrs2; SGR 55 clears it.
  (it "sgr-overline-on-and-off"
    (with-screen (s 10 2)
      (feed s (esc "[53mX"))
      (expect (not (zerop (logand (cl-tmux/terminal/types:screen-cur-attrs2 s)
                              cl-tmux/terminal/types:+attr2-overline+))))
      (feed s (esc "[55mY"))
      (expect (zerop (logand (cl-tmux/terminal/types:screen-cur-attrs2 s)
                         cl-tmux/terminal/types:+attr2-overline+)))))

  ;; SGR 58;5;42 sets cur-ul-color to 42; SGR 59 resets it to 0.
  (it "sgr-underline-color-set-and-reset"
    (with-screen (s 10 2)
      (cl-tmux/terminal/sgr:apply-sgr s '(58 5 42))
      (expect (= 42 (cl-tmux/terminal/types:screen-cur-ul-color s)))
      (cl-tmux/terminal/sgr:apply-sgr s '(59))
      (expect (= 0 (cl-tmux/terminal/types:screen-cur-ul-color s))))))

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

(describe "terminal-suite/sgr"

  ;; SGR 30 sets foreground to index 0 (black); SGR 40 sets background to index 0 (black).
  (it "sgr-black-fg-and-bg-table"
    (dolist (row (list (list "[30mX" #'fg-at "SGR 30 must set fg to 0 (black)")
                       (list "[40mX" #'bg-at "SGR 40 must set bg to 0 (black)")))
      (destructuring-bind (seq accessor desc) row
        (declare (ignore desc))
        (with-screen (s 10 2)
          (feed s (esc seq))
          (expect (= 0 (funcall accessor s 0 0)))))))

  ;; SGR 6 (rapid blink) maps to the same blink bit as SGR 5.
  (it "sgr-rapid-blink-6-sets-blink-bit"
    (with-screen (s 10 2)
      (feed s (esc "[6mB"))
      (expect (logbitp 4 (attrs-at s 0 0)))))

  ;; SGR 25 clears the blink attribute bit (4).
  (it "sgr-blink-off-25"
    (with-screen (s 10 2)
      (feed s (esc "[5mB"))   ; blink on
      (feed s (esc "[25mX"))  ; blink off
      (expect (logbitp 4 (attrs-at s 1 0)) :to-be-falsy)))

  ;; SGR 27 clears the reverse-video attribute bit.
  (it "sgr-reverse-off-27"
    (with-screen (s 10 2)
      (feed s (esc "[7mR"))   ; reverse on
      (feed s (esc "[27mX"))  ; reverse off
      (expect (zerop (logand (attrs-at s 1 0) #b100)))))

  ;; SGR 51 (framed) and SGR 52 (encircled) are accepted without error and do not alter standard attrs.
  (it "sgr-framed-encircled-accepted-silently-table"
    (dolist (row '((51 "SGR 51 (framed)")
                   (52 "SGR 52 (encircled)")))
      (destructuring-bind (code desc) row
        (declare (ignore desc))
        (with-screen (s 10 2)
          (finishes (feed s (esc "[~DmX" code)))
          (expect (zerop (logand (attrs-at s 0 0) #b1111111)))))))

  ;; Bright background SGR codes 100-107 set bg indices 8-15.
  (it "sgr-bright-background-table"
    (loop for code from 100 to 107
          for expected-bg from 8 to 15
          do (with-screen (s 10 2)
               (feed s (esc "[~DmX" code))
               (expect (= expected-bg (bg-at s 0 0)))))))

;;; ── direct-action-sgr additional ─────────────────────────────────────────────

(describe "terminal-suite/direct-action-sgr"

  ;; %dispatch-sgr-code sets cur-fg or cur-bg directly by SGR code.
  (it "dispatch-sgr-code-directly-table"
    (dolist (row (list (list 31 1 #'cl-tmux/terminal/types:screen-cur-fg "31 → cur-fg=1 (red)")
                       (list 42 2 #'cl-tmux/terminal/types:screen-cur-bg "42 → cur-bg=2 (green)")))
      (destructuring-bind (code expected accessor desc) row
        (declare (ignore desc))
        (with-screen (s 10 2)
          (cl-tmux/terminal/sgr:%dispatch-sgr-code s code)
          (expect (= expected (funcall accessor s)))))))

  ;; %dispatch-sgr-code silently ignores unrecognized SGR codes.
  (it "dispatch-sgr-code-unknown-is-noop"
    (with-screen (s 10 2)
      (finishes (cl-tmux/terminal/sgr:%dispatch-sgr-code s 999))
      ;; SGR state should remain at default after an unknown code.
      (check-sgr-state s :fg cl-tmux/terminal/types:+default-color+ :bg cl-tmux/terminal/types:+default-color+ :attrs 0)))

  ;; attr2-on sets a bit in cur-attrs2; attr2-off clears it without touching others.
  (it "attr2-on-and-off-helpers"
    (with-screen (s 10 2)
      (cl-tmux/terminal/sgr::attr2-on s cl-tmux/terminal/types:+attr2-overline+)
      (cl-tmux/terminal/sgr::attr2-on s cl-tmux/terminal/types:+attr2-double-underline+)
      (expect (not (zerop (logand (cl-tmux/terminal/types:screen-cur-attrs2 s)
                              cl-tmux/terminal/types:+attr2-overline+))))
      (expect (not (zerop (logand (cl-tmux/terminal/types:screen-cur-attrs2 s)
                              cl-tmux/terminal/types:+attr2-double-underline+))))
      ;; Now clear only overline.
      (cl-tmux/terminal/sgr::attr2-off s cl-tmux/terminal/types:+attr2-overline+)
      (expect (zerop (logand (cl-tmux/terminal/types:screen-cur-attrs2 s)
                         cl-tmux/terminal/types:+attr2-overline+)))
      (expect (not (zerop (logand (cl-tmux/terminal/types:screen-cur-attrs2 s)
                              cl-tmux/terminal/types:+attr2-double-underline+)))))))

;;; ── SGR truecolor edge cases ─────────────────────────────────────────────────

(describe "terminal-suite/sgr-extended"

  ;; SGR 58;2;R;G;B sets cur-ul-color to the true-color encoding.
  (it "sgr-truecolor-underline-color"
    (with-screen (s 10 2)
      (cl-tmux/terminal/sgr:apply-sgr s '(58 2 255 0 128))
      (let ((expected (logior #x1000000 (ash 255 16) (ash 0 8) 128)))
        (expect (= expected (cl-tmux/terminal/types:screen-cur-ul-color s)))))))

;;; ── Coverage gap: define-sgr-rules macro ─────────────────────────────────────
;;;
;;; Audit finding: define-sgr-rules was not tested as a macro in isolation.
;;; The generated %dispatch-sgr-code now also carries a docstring; verify it.

(describe "terminal-suite/direct-action-sgr"

  ;; define-sgr-rules is a defined macro in the sgr package.
  (it "define-sgr-rules-macro-is-defined"
    (expect (macro-function 'cl-tmux/terminal/sgr::define-sgr-rules)))

  ;; %dispatch-sgr-code (exported) has a non-empty docstring injected by the macro.
  (it "dispatch-sgr-code-has-docstring"
    (let ((doc (documentation 'cl-tmux/terminal/sgr:%dispatch-sgr-code 'function)))
      (expect (and (stringp doc) (plusp (length doc))))))

  ;;; ── Coverage gap: %consume-256-color-param direct test ───────────────────────
  ;;;
  ;;; The %consume-256-color-param helper was extracted from apply-sgr to eliminate
  ;;; code duplication across the 38/48/58 256-color arms.

  ;; %consume-256-color-param stores the clamped index via SETTER and returns the
  ;; tail after the three consumed elements.
  (it "consume-256-color-param-sets-fg-and-advances"
    (with-screen (s 10 2)
      ;; Simulate the 38;5;42 arm: parameter-tail = (38 5 42 99)
      (let* ((parameter-tail '(38 5 42 99))
             (tail (cl-tmux/terminal/sgr::%consume-256-color-param
                    s #'(setf cl-tmux/terminal/types:screen-cur-fg) parameter-tail)))
        (expect (= 42 (cl-tmux/terminal/types:screen-cur-fg s)))
        (expect (equal '(99) tail)))))

  ;; %consume-256-color-param clamps an out-of-range index (> 255) to 255.
  (it "consume-256-color-param-clamps-to-255"
    (with-screen (s 10 2)
      (cl-tmux/terminal/sgr::%consume-256-color-param
       s #'(setf cl-tmux/terminal/types:screen-cur-fg) '(38 5 300))
      (expect (= 255 (cl-tmux/terminal/types:screen-cur-fg s)))))

  ;;; ── Coverage gap: %encode-truecolor-rgb, %apply-sgr-group, %apply-sgr-color-arm ──
  ;;;
  ;;; %apply-sgr-group (colon-delimited groups) and %apply-sgr-color-arm
  ;;; (semicolon-protocol arms) were previously exercised only indirectly via
  ;;; apply-sgr/feed integration tests, so a future edit that let the two
  ;;; parallel implementations diverge would not have been caught by a direct
  ;;; unit test.  %encode-truecolor-rgb is the shared true-colour arithmetic
  ;;; helper both implementations now delegate to.

  ;; %encode-truecolor-rgb clamps each channel to 0-255 and encodes #x1RRGGBB.
  (it "encode-truecolor-rgb-clamps-and-encodes"
    (expect (= (logior #x1000000 (ash 255 16) (ash 128 8) 0)
           (cl-tmux/terminal/sgr::%encode-truecolor-rgb 255 128 0)))
    (expect (= (logior #x1000000 (ash 255 16) (ash 0 8) 255)
           (cl-tmux/terminal/sgr::%encode-truecolor-rgb 300 -5 999)))
    (expect (= #x1000000 (cl-tmux/terminal/sgr::%encode-truecolor-rgb nil nil nil))))

  ;; %apply-sgr-group applies a (38 2 R G B) colon group as a true-colour fg.
  (it "apply-sgr-group-truecolor-sets-fg"
    (with-screen (s 10 2)
      (cl-tmux/terminal/sgr::%apply-sgr-group
       s (list 38 2 255 128 0))
      (expect (= (logior #x1000000 (ash 255 16) (ash 128 8) 0)
             (cl-tmux/terminal/types:screen-cur-fg s)))))

  ;; %apply-sgr-group takes the LAST three values as R G B, skipping an optional
  ;; leading colourspace-id field, e.g. (38 2 1 255 128 0).
  (it "apply-sgr-group-truecolor-skips-colourspace-field"
    (with-screen (s 10 2)
      (cl-tmux/terminal/sgr::%apply-sgr-group
       s (list 38 2 1 255 128 0))
      (expect (= (logior #x1000000 (ash 255 16) (ash 128 8) 0)
             (cl-tmux/terminal/types:screen-cur-fg s)))))

  ;; %apply-sgr-group applies a (48 5 N) colon group as a 256-colour bg.
  (it "apply-sgr-group-256color-sets-bg"
    (with-screen (s 10 2)
      (cl-tmux/terminal/sgr::%apply-sgr-group s (list 48 5 200))
      (expect (= 200 (cl-tmux/terminal/types:screen-cur-bg s)))))

  ;; %apply-sgr-group with a non-colour lead (e.g. undercurl (4 3)) dispatches
  ;; the lead as a plain SGR code.
  (it "apply-sgr-group-plain-code-fallback"
    (with-screen (s 10 2)
      (cl-tmux/terminal/sgr::%apply-sgr-group s (list 4 3))
      (expect (logbitp 3 (cl-tmux/terminal/types:screen-cur-attrs s)))))

  ;; %apply-sgr-color-arm consumes a 38;5;N semicolon arm and returns the tail
  ;; after the three consumed elements.
  (it "apply-sgr-color-arm-256color-advances-tail"
    (with-screen (s 10 2)
      (let ((tail (cl-tmux/terminal/sgr::%apply-sgr-color-arm s '(38 5 200 1))))
        (expect (= 200 (cl-tmux/terminal/types:screen-cur-fg s)))
        (expect (equal '(1) tail)))))

  ;; %apply-sgr-color-arm consumes a 48;2;R;G;B semicolon arm and returns the
  ;; tail after the five consumed elements.
  (it "apply-sgr-color-arm-truecolor-advances-tail"
    (with-screen (s 10 2)
      (let ((tail (cl-tmux/terminal/sgr::%apply-sgr-color-arm
                   s '(48 2 0 128 255 7))))
        (expect (= (logior #x1000000 (ash 0 16) (ash 128 8) 255)
               (cl-tmux/terminal/types:screen-cur-bg s)))
        (expect (equal '(7) tail)))))

  ;; %apply-sgr-color-arm with a malformed/incomplete arm (e.g. bare 38 with no
  ;; kind) falls back to dispatching the lead as a plain SGR code.
  (it "apply-sgr-color-arm-malformed-falls-back-to-plain-code"
    (with-screen (s 10 2)
      (let ((tail (cl-tmux/terminal/sgr::%apply-sgr-color-arm s '(38))))
        ;; 38 is not a recognized plain SGR code, so this is a no-op dispatch;
        ;; the important behaviour under test is the returned tail.
        (expect (equal '() tail))))))
