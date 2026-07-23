(in-package #:cl-tmux/test)

;;;; Screen tests — part IV: boolean-slot macro, title/cwd/pending-wrap,
;;;; focus-events, and G0/G1/active-g charset state.
;;;;
;;;; The define-boolean-slot-tests macro eliminates the repeated
;;;; defaults-NIL / enable-sequence / disable-sequence triple for boolean
;;;; screen slots. Later screen test files may depend on this macro, so this
;;;; file must remain before those files in the ASDF load order.

;;; ── Boolean-slot test macro ───────────────────────────────────────────────
;;;
;;; Each generated triple tests:
;;;   1. The slot defaults to NIL on a fresh screen.
;;;   2. A specific escape sequence sets it to T.
;;;   3. A complementary sequence clears it to NIL.

(defmacro define-boolean-slot-tests
    (slot-accessor suite-name enable-sequence disable-sequence
     &key (suite-description (symbol-name suite-name))
          (parent-suite 'terminal-suite))
  "Generate a describe block with three cl-weave tests for a boolean screen slot.

   SLOT-ACCESSOR    — accessor symbol (e.g. cl-tmux/terminal/types:screen-insert-mode)
   SUITE-NAME       — unquoted symbol naming the describe block
   ENABLE-SEQUENCE  — form that feeds the enabling sequence to screen variable S
   DISABLE-SEQUENCE — form that feeds the disabling sequence to screen variable S"
  (declare (ignore suite-description))
  (let* ((name (symbol-name slot-accessor))
         (default-test  (string-downcase (format nil "~A-DEFAULTS-FALSE" name)))
         (enabled-test  (string-downcase (format nil "~A-ENABLED-BY-SEQUENCE" name)))
         (disabled-test (string-downcase (format nil "~A-DISABLED-BY-SEQUENCE" name))))
    `(describe ,(format nil "~A/~A" (string-downcase (symbol-name parent-suite))
                                    (string-downcase (symbol-name suite-name)))
       (it ,default-test
         (with-screen (s 10 5)
           (expect (,slot-accessor s) :to-be-falsy)))
       (it ,enabled-test
         (with-screen (s 10 5)
           ,enable-sequence
           (expect (,slot-accessor s) :to-be-truthy)))
       (it ,disabled-test
         (with-screen (s 10 5)
           ,enable-sequence
           ,disable-sequence
           (expect (,slot-accessor s) :to-be-falsy))))))

;;; ── SUITE: screen-title-stack ────────────────────────────────────────────────
;;;
;;; XTPUSHTITLE / XTPOPTITLE: a stack of saved title strings, bounded to
;;; +title-stack-max-depth+ = 8 entries.

(describe "terminal-suite/title-stack-suite"

  ;; screen-title-stack is NIL on a fresh screen.
  (it "screen-title-stack-defaults-nil"
    (with-screen (s 10 5)
      (expect (null (cl-tmux/terminal/types:screen-title-stack s)))))

  ;; ESC[>0t pushes the current title; ESC[<0t pops and restores it.
  (it "screen-title-stack-push-pop-via-sequences"
    (with-screen (s 10 5)
      (feed s (format nil "~C]2;MyTitle~C\\" #\Escape #\Escape))
      (expect (string= "MyTitle" (cl-tmux/terminal/types:screen-title s)))
      (feed s (esc "[>0t"))
      (expect (not (null (cl-tmux/terminal/types:screen-title-stack s))))
      (feed s (format nil "~C]2;NewTitle~C\\" #\Escape #\Escape))
      (expect (string= "NewTitle" (cl-tmux/terminal/types:screen-title s)))
      (feed s (esc "[<0t"))
      (expect (string= "MyTitle" (cl-tmux/terminal/types:screen-title s)))))

  ;; Pushing beyond +title-stack-max-depth+ does not grow the stack beyond the limit.
  (it "screen-title-stack-depth-limit"
    (with-screen (s 10 5)
      (dotimes (_ (+ cl-tmux/terminal/types:+title-stack-max-depth+ 2))
        (feed s (esc "[>0t")))
      (expect (<= (length (cl-tmux/terminal/types:screen-title-stack s))
                  cl-tmux/terminal/types:+title-stack-max-depth+)))))

;;; ── SUITE: screen-cwd ────────────────────────────────────────────────────────

(describe "terminal-suite/screen-cwd-suite"

  ;; screen-cwd is the empty string on a fresh screen.
  (it "screen-cwd-defaults-empty-string"
    (with-screen (s 10 5)
      (expect (string= "" (cl-tmux/terminal/types:screen-cwd s)))))

  ;; screen-cwd can be set to an arbitrary string via setf.
  (it "screen-cwd-can-be-set-directly"
    (with-screen (s 10 5)
      (setf (cl-tmux/terminal/types:screen-cwd s) "/home/user/project")
      (expect (string= "/home/user/project" (cl-tmux/terminal/types:screen-cwd s)))))

  ;; OSC 7 ; file://host/path sets screen-cwd to a non-empty value.
  (it "screen-cwd-updated-by-osc7"
    (with-screen (s 20 5)
      (feed s (format nil "~C]7;file://localhost/tmp/foo~C\\" #\Escape #\Escape))
      (expect (string/= "" (cl-tmux/terminal/types:screen-cwd s))))))

;;; ── SUITE: screen-pending-wrap ───────────────────────────────────────────────

(describe "terminal-suite/pending-wrap-suite"

  ;; screen-pending-wrap is NIL on a fresh screen.
  (it "screen-pending-wrap-defaults-false"
    (with-screen (s 10 5)
      (expect (cl-tmux/terminal/types:screen-pending-wrap s) :to-be-falsy)))

  ;; Writing a character into the last column with autowrap sets pending-wrap.
  (it "screen-pending-wrap-set-when-cursor-at-last-column"
    (with-screen (s 3 2)
      (feed s "abc")
      (expect (cl-tmux/terminal/types:screen-pending-wrap s) :to-be-truthy)))

  ;; pending-wrap is cleared when the next character triggers an actual wrap.
  (it "screen-pending-wrap-cleared-on-wrap"
    (with-screen (s 3 2)
      (feed s "abc")
      (expect (cl-tmux/terminal/types:screen-pending-wrap s) :to-be-truthy)
      (feed s "d")
      (expect (cl-tmux/terminal/types:screen-pending-wrap s) :to-be-falsy)))

  ;; Any explicit cursor movement (CR) clears pending-wrap.
  (it "screen-pending-wrap-cleared-by-cursor-move"
    (with-screen (s 3 2)
      (feed s "abc")
      (feed s (string #\Return))
      (expect (cl-tmux/terminal/types:screen-pending-wrap s) :to-be-falsy))))

;;; ── SUITE: screen-focus-events (using define-boolean-slot-tests) ─────────────

(define-boolean-slot-tests
  cl-tmux/terminal/types:screen-focus-events
  focus-events-suite
  (feed s (esc "[?1004h"))   ; ?1004h enables focus event reporting
  (feed s (esc "[?1004l"))   ; ?1004l disables focus event reporting
  :suite-description "screen-focus-events: defaults NIL, ?1004h enables, ?1004l disables")

;;; ── SUITE: G0/G1 charset designation and SO/SI ───────────────────────────────

(describe "terminal-suite/g0-g1-charset-suite"

  ;; screen-g0-charset defaults to :ascii on a fresh screen.
  (it "screen-g0-charset-defaults-ascii"
    (with-screen (s 10 5)
      (expect (eq :ascii (cl-tmux/terminal/types:screen-g0-charset s)))))

  ;; screen-g1-charset defaults to :ascii on a fresh screen.
  (it "screen-g1-charset-defaults-ascii"
    (with-screen (s 10 5)
      (expect (eq :ascii (cl-tmux/terminal/types:screen-g1-charset s)))))

  ;; screen-active-g defaults to :g0 on a fresh screen.
  (it "screen-active-g-defaults-g0"
    (with-screen (s 10 5)
      (expect (eq :g0 (cl-tmux/terminal/types:screen-active-g s)))))

  ;; ESC ( 0 designates G0 as DEC special graphics.
  (it "screen-g0-charset-designated-by-esc-paren-0"
    (with-screen (s 10 5)
      (feed s (esc "(0"))
      (expect (eq :dec-graphics (cl-tmux/terminal/types:screen-g0-charset s)))))

  ;; ESC ) 0 designates G1 as DEC special graphics.
  (it "screen-g1-charset-designated-by-esc-paren-0"
    (with-screen (s 10 5)
      (feed s (esc ")0"))
      (expect (eq :dec-graphics (cl-tmux/terminal/types:screen-g1-charset s)))))

  ;; SO (0x0E) selects G1; SI (0x0F) selects G0.
  (it "screen-active-g-toggled-by-so-si"
    (with-screen (s 10 5)
      (screen-process-bytes s (make-array 1 :element-type '(unsigned-byte 8)
                                            :initial-contents '(#x0E)))
      (expect (eq :g1 (cl-tmux/terminal/types:screen-active-g s)))
      (screen-process-bytes s (make-array 1 :element-type '(unsigned-byte 8)
                                            :initial-contents '(#x0F)))
      (expect (eq :g0 (cl-tmux/terminal/types:screen-active-g s))))))
