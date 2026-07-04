(in-package #:cl-tmux/test)

;;;; config directive suite, bindable commands, basic apply/set directives — part I

(def-suite config-directives-suite :description "Config file directive parsing")
(in-suite config-directives-suite)

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

;;; *bindable-commands* invariant

(test bindable-commands-excludes-copy-mode-internals
  "*bindable-commands* must exclude copy-mode-internal commands."
  (is (null (intersection '(:copy-mode-exit :copy-mode-up :copy-mode-down)
                          cl-tmux/config::*bindable-commands*))
      "copy-mode-internal commands must not be user-bindable, found ~A"
      (intersection '(:copy-mode-exit :copy-mode-up :copy-mode-down)
                    cl-tmux/config::*bindable-commands*))
  (dolist (cmd '(:copy-mode-exit :copy-mode-up :copy-mode-down))
    (is (not (member cmd cl-tmux/config::*bindable-commands*))
        "~A must not be a user-bindable command" cmd))
  (is (member :new-window cl-tmux/config::*bindable-commands*)
      ":new-window must remain a user-bindable command"))

;;; apply-config-directive

(test apply-directive-bind-returns-t
  "apply-config-directive for a valid bind returns T and binds the char."
  (with-isolated-config
    (assert-config-directive-applied '("bind" "z" "new-window")
                                     "valid bind directive")
    (is (eq :new-window (lookup-key-binding #\z))
        "#\\z should be bound to :new-window after the bind directive")))

(test apply-directive-unknown-returns-nil
  "apply-config-directive for an unknown command returns NIL and changes nothing."
  (with-isolated-config
    (let* ((tbl (cl-tmux/config:ensure-key-table "prefix"))
           (count-before (hash-table-count tbl))
           (shell-before    *default-shell*)
           (height-before   *status-height*))
      (assert-config-directive-rejected '("bogus" "x")
                                        "an unknown command")
      (is (= count-before (hash-table-count tbl))
          "prefix key-table must be unchanged by an unknown directive")
      (is (equal shell-before *default-shell*)
          "*default-shell* must be unchanged by an unknown directive")
      (is (eql height-before *status-height*)
          "*status-height* must be unchanged by an unknown directive"))))

;;; set [-g|-a|...] name value  — flag handling (the canonical .tmux.conf form)

(test apply-set-directive-global-flag
  "'set -g status off' applies (3 tokens) — previously the fixed-arity table
   silently dropped it.  Sets 'status', not an option named '-g'."
  (with-isolated-options ()
    (assert-set-directive-option-state '("set" "-g" "status" "off")
                                       "status" "off"
                                       :context "set -g status off")
    (is (null (cl-tmux/options:get-option "-g"))
        "must NOT create an option literally named '-g'")))

(test apply-set-directive-append-flag
  "'set -ag <name> <value>' appends to the option's current value."
  (with-isolated-options ("status-left" "A")
    (assert-set-directive-option-state '("set" "-ag" "status-left" "B")
                                       "status-left" "AB"
                                       :context "set -ag")))

(test apply-set-directive-unset-flag
  "'set -u <name>' removes the option from the current scope."
  (with-isolated-options ("status-left" "keep-me")
    (assert-set-directive-option-state '("set" "-u" "status-left")
                                       "status-left" nil
                                       :context "set -u"
                                       :present-p nil)))

(test apply-set-directive-plain-unaffected
  "Plain 'set name value' (no flags) still flows through the normal directive
   table and applies unchanged."
  (with-isolated-options ()
    (assert-set-directive-option-state '("set" "status" "off")
                                       "status" "off"
                                       :context "plain set")))

;;; set mouse — *mouse-reporting-hook* side effect

(test set-mouse-invokes-mouse-reporting-hook
  "'set -g mouse on'/'off' invokes *mouse-reporting-hook* with T/NIL so the
   renderer layer can enable/disable mouse reporting without config depending
   on it directly."
  (with-isolated-config
    (let ((calls nil))
      (let ((cl-tmux/config:*mouse-reporting-hook*
              (lambda (on-p) (push on-p calls))))
        (assert-config-directive-applied '("set" "-g" "mouse" "on")
                                         "set -g mouse on")
        (assert-config-directive-applied '("set" "-g" "mouse" "off")
                                         "set -g mouse off")
        (is (equal '(nil t) calls)
            "the hook must be called with T then NIL, got ~A" calls)))))

(test set-mouse-with-no-hook-does-not-signal
  "'set -g mouse on' is safe when *mouse-reporting-hook* is unset (NIL)."
  (with-isolated-config
    (let ((cl-tmux/config:*mouse-reporting-hook* nil))
      (finishes
        (assert-config-directive-applied '("set" "-g" "mouse" "on")
                                         "set -g mouse on with no hook")))))

;;; set-shell / set-status-height directives

(test set-shell-and-status-height-directives
  "set-shell sets *default-shell*; set-status-height sets *status-height*."
  (with-isolated-config
    (assert-config-directive-applied '("set-shell" "/usr/bin/zsh")
                                     "set-shell directive")
    (is (string= "/usr/bin/zsh" *default-shell*)
        "*default-shell* should be /usr/bin/zsh, got ~A" *default-shell*)
    (assert-config-directive-applied '("set-status-height" "2")
                                     "set-status-height directive")
    (is (= 2 *status-height*)
        "*status-height* should be 2, got ~A" *status-height*)))

;;; bind/unbind/set: arity and validity table

(test invalid-directive-cases-return-nil
  "Every malformed or unknown directive returns NIL without mutating state."
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

(test set-status-height-noninteger-is-tolerated
  "Non-integer or non-positive set-status-height values return NIL and do not signal."
  (with-isolated-config
    (let ((before *status-height*))
      (assert-config-directive-safe-nil '("set-status-height" "abc")
                                        "set-status-height with a non-integer value")
      (is (eql before *status-height*)
          "*status-height* should be unchanged, got ~A" *status-height*)
      (assert-config-directive-safe-nil '("set-status-height" "0")
                                        "set-status-height with a non-positive value (0)")
      (is (eql before *status-height*)
          "*status-height* should be unchanged, got ~A" *status-height*))))
