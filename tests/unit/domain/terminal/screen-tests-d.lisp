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
  "Generate a def-suite + three fiveam tests for a boolean screen slot.

   SLOT-ACCESSOR    — accessor symbol (e.g. cl-tmux/terminal/types:screen-insert-mode)
   SUITE-NAME       — unquoted symbol naming the def-suite
   ENABLE-SEQUENCE  — form that feeds the enabling sequence to screen variable S
   DISABLE-SEQUENCE — form that feeds the disabling sequence to screen variable S"
  (let* ((name (symbol-name slot-accessor))
         (default-test  (intern (format nil "~A-DEFAULTS-FALSE" name)))
         (enabled-test  (intern (format nil "~A-ENABLED-BY-SEQUENCE" name)))
         (disabled-test (intern (format nil "~A-DISABLED-BY-SEQUENCE" name))))
    `(progn
       (def-suite ,suite-name
         :description ,suite-description
         :in ,parent-suite)
       (in-suite ,suite-name)
       (test ,default-test
         ,(format nil "~A is NIL on a fresh screen." name)
         (with-screen (s 10 5)
           (is-false (,slot-accessor s)
                     ,(format nil "~A must be NIL initially" name))))
       (test ,enabled-test
         ,(format nil "~A is T after the enable sequence." name)
         (with-screen (s 10 5)
           ,enable-sequence
           (is-true (,slot-accessor s)
                    ,(format nil "~A must be T after enable sequence" name))))
       (test ,disabled-test
         ,(format nil "~A is NIL after the disable sequence." name)
         (with-screen (s 10 5)
           ,enable-sequence
           ,disable-sequence
           (is-false (,slot-accessor s)
                     ,(format nil "~A must be NIL after disable sequence" name)))))))

;;; ── SUITE: screen-title-stack ────────────────────────────────────────────────
;;;
;;; XTPUSHTITLE / XTPOPTITLE: a stack of saved title strings, bounded to
;;; +title-stack-max-depth+ = 8 entries.

(def-suite title-stack-suite
  :description "screen-title-stack slot: defaults, push, pop, depth limit"
  :in terminal-suite)
(in-suite title-stack-suite)

(test screen-title-stack-defaults-nil
  "screen-title-stack is NIL on a fresh screen."
  (with-screen (s 10 5)
    (is (null (cl-tmux/terminal/types:screen-title-stack s))
        "title-stack must be NIL initially")))

(test screen-title-stack-push-pop-via-sequences
  "ESC[>0t pushes the current title; ESC[<0t pops and restores it."
  (with-screen (s 10 5)
    (feed s (format nil "~C]2;MyTitle~C\\" #\Escape #\Escape))
    (is (string= "MyTitle" (cl-tmux/terminal/types:screen-title s))
        "pre-condition: title must be MyTitle")
    (feed s (esc "[>0t"))
    (is (not (null (cl-tmux/terminal/types:screen-title-stack s)))
        "title-stack must be non-NIL after push")
    (feed s (format nil "~C]2;NewTitle~C\\" #\Escape #\Escape))
    (is (string= "NewTitle" (cl-tmux/terminal/types:screen-title s))
        "title must change to NewTitle after OSC 2")
    (feed s (esc "[<0t"))
    (is (string= "MyTitle" (cl-tmux/terminal/types:screen-title s))
        "title must be restored to MyTitle after pop")))

(test screen-title-stack-depth-limit
  "Pushing beyond +title-stack-max-depth+ does not grow the stack beyond the limit."
  (with-screen (s 10 5)
    (dotimes (_ (+ cl-tmux/terminal/types:+title-stack-max-depth+ 2))
      (feed s (esc "[>0t")))
    (is (<= (length (cl-tmux/terminal/types:screen-title-stack s))
            cl-tmux/terminal/types:+title-stack-max-depth+)
        "title-stack must never exceed +title-stack-max-depth+ entries")))

;;; ── SUITE: screen-cwd ────────────────────────────────────────────────────────

(def-suite screen-cwd-suite
  :description "screen-cwd slot: default empty string and OSC 7 update"
  :in terminal-suite)
(in-suite screen-cwd-suite)

(test screen-cwd-defaults-empty-string
  "screen-cwd is the empty string on a fresh screen."
  (with-screen (s 10 5)
    (is (string= "" (cl-tmux/terminal/types:screen-cwd s))
        "cwd must be empty string initially")))

(test screen-cwd-can-be-set-directly
  "screen-cwd can be set to an arbitrary string via setf."
  (with-screen (s 10 5)
    (setf (cl-tmux/terminal/types:screen-cwd s) "/home/user/project")
    (is (string= "/home/user/project" (cl-tmux/terminal/types:screen-cwd s))
        "cwd must hold the value after setf")))

(test screen-cwd-updated-by-osc7
  "OSC 7 ; file://host/path sets screen-cwd to a non-empty value."
  (with-screen (s 20 5)
    (feed s (format nil "~C]7;file://localhost/tmp/foo~C\\" #\Escape #\Escape))
    (is (string/= "" (cl-tmux/terminal/types:screen-cwd s))
        "cwd must be non-empty after OSC 7 ; file://host/path")))

;;; ── SUITE: screen-pending-wrap ───────────────────────────────────────────────

(def-suite pending-wrap-suite
  :description "screen-pending-wrap slot: default, set, clear"
  :in terminal-suite)
(in-suite pending-wrap-suite)

(test screen-pending-wrap-defaults-false
  "screen-pending-wrap is NIL on a fresh screen."
  (with-screen (s 10 5)
    (is-false (cl-tmux/terminal/types:screen-pending-wrap s)
              "pending-wrap must be NIL initially")))

(test screen-pending-wrap-set-when-cursor-at-last-column
  "Writing a character into the last column with autowrap sets pending-wrap."
  (with-screen (s 3 2)
    (feed s "abc")
    (is-true (cl-tmux/terminal/types:screen-pending-wrap s)
             "pending-wrap must be T after filling the last column with autowrap on")))

(test screen-pending-wrap-cleared-on-wrap
  "pending-wrap is cleared when the next character triggers an actual wrap."
  (with-screen (s 3 2)
    (feed s "abc")
    (is-true (cl-tmux/terminal/types:screen-pending-wrap s) "pre-condition")
    (feed s "d")
    (is-false (cl-tmux/terminal/types:screen-pending-wrap s)
              "pending-wrap must be cleared after a character is written following a wrap")))

(test screen-pending-wrap-cleared-by-cursor-move
  "Any explicit cursor movement (CR) clears pending-wrap."
  (with-screen (s 3 2)
    (feed s "abc")
    (feed s (string #\Return))
    (is-false (cl-tmux/terminal/types:screen-pending-wrap s)
              "pending-wrap must be cleared by CR")))

;;; ── SUITE: screen-focus-events (using define-boolean-slot-tests) ─────────────

(define-boolean-slot-tests
  cl-tmux/terminal/types:screen-focus-events
  focus-events-suite
  (feed s (esc "[?1004h"))   ; ?1004h enables focus event reporting
  (feed s (esc "[?1004l"))   ; ?1004l disables focus event reporting
  :suite-description "screen-focus-events: defaults NIL, ?1004h enables, ?1004l disables")

;;; ── SUITE: G0/G1 charset designation and SO/SI ───────────────────────────────

(def-suite g0-g1-charset-suite
  :description "screen-g0-charset, screen-g1-charset, screen-active-g: defaults and sequences"
  :in terminal-suite)
(in-suite g0-g1-charset-suite)

(test screen-g0-charset-defaults-ascii
  "screen-g0-charset defaults to :ascii on a fresh screen."
  (with-screen (s 10 5)
    (is (eq :ascii (cl-tmux/terminal/types:screen-g0-charset s))
        "g0-charset must default to :ascii")))

(test screen-g1-charset-defaults-ascii
  "screen-g1-charset defaults to :ascii on a fresh screen."
  (with-screen (s 10 5)
    (is (eq :ascii (cl-tmux/terminal/types:screen-g1-charset s))
        "g1-charset must default to :ascii")))

(test screen-active-g-defaults-g0
  "screen-active-g defaults to :g0 on a fresh screen."
  (with-screen (s 10 5)
    (is (eq :g0 (cl-tmux/terminal/types:screen-active-g s))
        "active-g must default to :g0")))

(test screen-g0-charset-designated-by-esc-paren-0
  "ESC ( 0 designates G0 as DEC special graphics."
  (with-screen (s 10 5)
    (feed s (esc "(0"))
    (is (eq :dec-graphics (cl-tmux/terminal/types:screen-g0-charset s))
        "g0-charset must be :dec-graphics after ESC(0")))

(test screen-g1-charset-designated-by-esc-paren-0
  "ESC ) 0 designates G1 as DEC special graphics."
  (with-screen (s 10 5)
    (feed s (esc ")0"))
    (is (eq :dec-graphics (cl-tmux/terminal/types:screen-g1-charset s))
        "g1-charset must be :dec-graphics after ESC)0")))

(test screen-active-g-toggled-by-so-si
  "SO (0x0E) selects G1; SI (0x0F) selects G0."
  (with-screen (s 10 5)
    (screen-process-bytes s (make-array 1 :element-type '(unsigned-byte 8)
                                          :initial-contents '(#x0E)))
    (is (eq :g1 (cl-tmux/terminal/types:screen-active-g s))
        "active-g must be :g1 after SO (0x0E)")
    (screen-process-bytes s (make-array 1 :element-type '(unsigned-byte 8)
                                          :initial-contents '(#x0F)))
    (is (eq :g0 (cl-tmux/terminal/types:screen-active-g s))
        "active-g must be :g0 after SI (0x0F)")))
