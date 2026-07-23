(in-package #:cl-tmux/test)

;;;; config directive suite, bindable commands, basic apply/set directives — part I

;;; Import the config-directives symbols we need

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(cl-tmux/config:lookup-key-binding
            cl-tmux/config:*default-shell*
            cl-tmux/config:*status-height*
            cl-tmux/config:key-table-bind
            cl-tmux/config:apply-config-directive
            cl-tmux/config:load-config-from-string
            cl-tmux/config:load-config-from-stream
            cl-tmux/config:config-file-path
            cl-tmux/config:load-config-file)))

;;; NOTE: with-isolated-key-tables and with-temp-config-file are defined in
;;; tests/helpers-overlay-assertions.lisp so all test suites can reuse them.

(describe "config-directives-suite"

  ;;; *bindable-commands* invariant

  ;; *bindable-commands* must exclude copy-mode-internal commands.
  (it "bindable-commands-excludes-copy-mode-internals"
    (expect (null (intersection '(:copy-mode-exit :copy-mode-up :copy-mode-down)
                                cl-tmux/config::*bindable-commands*)))
    (dolist (cmd '(:copy-mode-exit :copy-mode-up :copy-mode-down))
      (expect (not (member cmd cl-tmux/config::*bindable-commands*))))
    (expect (member :new-window cl-tmux/config::*bindable-commands*)))

  ;;; apply-config-directive

  ;; apply-config-directive for a valid bind returns T and binds the char.
  (it "apply-directive-bind-returns-t"
    (with-isolated-config
      (assert-config-directive-applied '("bind" "z" "new-window")
                                       "valid bind directive")
      (expect (eq :new-window (lookup-key-binding #\z)))))

  ;; apply-config-directive for an unknown command returns NIL and changes nothing.
  (it "apply-directive-unknown-returns-nil"
    (with-isolated-config
      (let* ((tbl (cl-tmux/config:ensure-key-table "prefix"))
             (count-before (hash-table-count tbl))
             (shell-before    *default-shell*)
             (height-before   *status-height*))
        (assert-config-directive-rejected '("bogus" "x")
                                          "an unknown command")
        (expect (= count-before (hash-table-count tbl)))
        (expect (equal shell-before *default-shell*))
        (expect (eql height-before *status-height*)))))

  ;;; set [-g|-a|...] name value  — flag handling (the canonical .tmux.conf form)

  ;; 'set-option -g status off' applies (3 tokens) — previously the fixed-arity table
  ;; silently dropped it.  Sets 'status', not an option named '-g'.
  (it "apply-set-directive-global-flag"
    (with-isolated-options ()
      (assert-set-directive-option-state '("set-option" "-g" "status" "off")
                                         "status" "off"
                                         :context "set-option -g status off")
      (expect (null (cl-tmux/options:get-option "-g")))))

  ;; 'set-option -ag <name> <value>' appends to the option's current value.
  (it "apply-set-directive-append-flag"
    (with-isolated-options ("status-left" "A")
      (assert-set-directive-option-state '("set-option" "-ag" "status-left" "B")
                                         "status-left" "AB"
                                         :context "set-option -ag")))

  ;; 'set-option -u <name>' removes the option from the current scope.
  (it "apply-set-directive-unset-flag"
    (with-isolated-options ("status-left" "keep-me")
      (assert-set-directive-option-state '("set-option" "-u" "status-left")
                                         "status-left" nil
                                         :context "set-option -u"
                                         :present-p nil)))

  ;; Plain 'set name value' (no flags) still flows through the normal directive
  ;; table and applies unchanged.
  (it "apply-set-directive-plain-unaffected"
    (with-isolated-options ()
      (assert-set-directive-option-state '("set-option" "status" "off")
                                         "status" "off"
                                         :context "plain set")))

  ;;; set mouse — *mouse-reporting-hook* side effect

  ;; 'set-option -g mouse on'/'off' invokes *mouse-reporting-hook* with T/NIL so the
  ;; renderer layer can enable/disable mouse reporting without config depending
  ;; on it directly.
  (it "set-mouse-invokes-mouse-reporting-hook"
    (with-isolated-config
      (let ((calls nil))
        (let ((cl-tmux/config:*mouse-reporting-hook*
                (lambda (on-p) (push on-p calls))))
          (assert-config-directive-applied '("set-option" "-g" "mouse" "on")
                                           "set-option -g mouse on")
          (assert-config-directive-applied '("set-option" "-g" "mouse" "off")
                                           "set-option -g mouse off")
          (expect (equal '(nil t) calls))))))

  ;; 'set-option -g mouse on' is safe when *mouse-reporting-hook* is unset (NIL).
  (it "set-mouse-with-no-hook-does-not-signal"
    (with-isolated-config
      (let ((cl-tmux/config:*mouse-reporting-hook* nil))
        (finishes
          (assert-config-directive-applied '("set-option" "-g" "mouse" "on")
                                           "set-option -g mouse on with no hook")))))

  ;;; set-shell / set-status-height directives

  ;; set-shell sets *default-shell*; set-status-height sets *status-height*.
  (it "set-shell-and-status-height-directives"
    (with-isolated-config
      (assert-config-directive-applied '("set-shell" "/usr/bin/zsh")
                                       "set-shell directive")
      (expect (string= "/usr/bin/zsh" *default-shell*))
      (assert-config-directive-applied '("set-status-height" "2")
                                       "set-status-height directive")
      (expect (= 2 *status-height*))))

  ;;; bind/unbind/set: arity and validity table

  ;; Every malformed or unknown directive returns NIL without mutating state.
  (it "invalid-directive-cases-return-nil"
    (with-isolated-config
      ;; NOTE: ("bind" "z" "new-window" "x") is no longer here — a bind with extra
      ;; tokens is now a valid arg-taking binding (key → command line), covered by
      ;; bind-key-to-command-line-stores-token-list.
      (dolist (tokens '(("bind")
                        ("bind" "z" "bogus-command")
                        ("unbind")
                        ("unbind" "z" "extra")
                        ("set-shell")
                        ("set-status-height")
                        ("totally-unknown" "arg")))
        (assert-config-directive-rejected tokens
                                          (format nil "~S" tokens)))))

  ;;; set-status-height: tolerant parsing

  ;; Non-integer or non-positive set-status-height values return NIL and do not signal.
  (it "set-status-height-noninteger-is-tolerated"
    (with-isolated-config
      (let ((before *status-height*))
        (assert-config-directive-safe-nil '("set-status-height" "abc")
                                          "set-status-height with a non-integer value")
        (expect (eql before *status-height*))
        (assert-config-directive-safe-nil '("set-status-height" "0")
                                          "set-status-height with a non-positive value (0)")
        (expect (eql before *status-height*))))))
