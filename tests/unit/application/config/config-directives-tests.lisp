(in-package #:cl-tmux/test)

;;;; bindable-commands, apply-config-directive, set flags, bind/unbind, load-config-from-stream — part I

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

;;; Test isolation helpers

(defun config-path (override xdg home)
  "Namestring of the resolved config path for the given env values + HOME
   (HOME a directory pathname)."
  (namestring (cl-tmux/config::%config-path-from override xdg home)))

;;; NOTE: with-isolated-key-tables and with-temp-config-file are defined in
;;; tests/helpers.lisp so all test suites can reuse them.

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

;;; bind key to a command LINE (arg-taking key bindings)

(test bind-key-to-command-line-stores-token-list
  "'bind X display-message hello' binds X to the command token list, applies (T),
   and list-keys shows the reconstructed command line."
  (with-isolated-key-tables
    (assert-config-directive-applied '("bind" "X" "display-message" "hello")
                                     "multi-token bind")
    (is (equal '("display-message" "hello") (lookup-key-binding #\X))
        "binding value must be the command token list")
    (is (search "display-message hello" (cl-tmux/config:describe-key-bindings))
        "list-keys must show the reconstructed command line")))

(test bind-key-single-keyword-still-binds
  "'bind z new-window' still binds the key to the :new-window keyword (existing
   single-command behaviour is preserved)."
  (with-isolated-key-tables
    (assert-config-directive-applied '("bind" "z" "new-window")
                                     "single known-command bind")
    (is (eq :new-window (lookup-key-binding #\z))
        "a single known command binds to its keyword")))

(test bind-key-single-unknown-command-rejected
  "'bind z totally-bogus-xyz' (one unknown command word) is still rejected (NIL),
   and the unknown command is not stored (z keeps its default keyword binding)."
  (with-isolated-key-tables
    (assert-config-directive-rejected '("bind" "z" "totally-bogus-xyz")
                                      "an unknown single command")
    ;; z is bound to a built-in (zoom) by default; the rejected bind must not have
    ;; replaced it with the bogus command token-list.
    (is (not (equal '("totally-bogus-xyz") (lookup-key-binding #\z)))
        "the unknown command must not be stored as a binding")))

;;; load-config-from-string

(test load-from-string-counts-and-applies
  "load-config-from-string ignores comments/blanks and applies real directives."
  (with-isolated-config
    (let ((applied (load-config-from-string
                    (format nil "# a comment~%~%bind z new-window~%set-status-height 5~%"))))
      (is (= 2 applied)
          "exactly 2 directives should be applied, got ~A" applied)
      (is (eq :new-window (lookup-key-binding #\z))
          "#\\z should be bound to :new-window")
      (is (= 5 *status-height*)
          "*status-height* should be 5, got ~A" *status-height*))))

(test load-realistic-config-applies-all-directives
  "A realistic multi-option .tmux.conf loads end-to-end: the wired options, the
   clustered -ga/-gw flags, bind -n, and a prefix change all take effect.  Guards
   the whole `.tmux.conf completely` path against regressions."
  (with-isolated-config
    (let ((applied
            (load-config-from-string
             (format nil "~
# realistic config~%~
set -g status-position top~%~
set -g status-left-style fg=red~%~
set -gw monitor-bell off~%~
set -g alternate-screen off~%~
set -ga @x a~%~
set -ga @x b~%~
bind -n F1 next-window~%~
set -g prefix C-a~%"))))
      (is (= 8 applied) "all 8 directives applied (comment/blank skipped), got ~A" applied)
      (is (string= "top" (cl-tmux/options:get-option "status-position"))
          "status-position took effect")
      (is (string= "fg=red" (cl-tmux/options:get-option "status-left-style"))
          "status-left-style took effect")
      (is (null (cl-tmux/options:get-option "monitor-bell"))
          "clustered -gw set monitor-bell off")
      (is (null (cl-tmux/options:get-option "alternate-screen"))
          "alternate-screen set off")
      (is (string= "ab" (cl-tmux/options:get-option "@x"))
          "clustered -ga appended a then b → \"ab\"")
      (is (eq :next-window
              (cl-tmux/config:key-table-command
               (cl-tmux/config:key-table-lookup "root" "F1")))
          "bind -n F1 bound F1 in the root table")
      (is (= 1 cl-tmux/config:*prefix-key-code*)
          "set -g prefix C-a changed the prefix to C-a (byte 1)"))))

(test load-from-string-multichar-and-quote-key
  "A single-char quote key parses as the character."
  (with-isolated-config
    (let ((applied (load-config-from-string
                    (format nil "bind \" split-horizontal~%bind n split-vertical~%"))))
      (is (= 2 applied)
          "both bind directives should be applied, got ~A" applied)
      (is (eq :split-horizontal (lookup-key-binding #\"))
          "the single-char token should bind the double-quote character")
      (is (eq :split-vertical (lookup-key-binding #\n))
          "#\\n should be re-bound to :split-vertical"))))

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

;;; config-file-path precedence (pure: %config-path-from)

(test config-path-table
  "%config-path-from: override wins; XDG used when set; ~/.config fallback; empty = unset."
  (dolist (c '(("/custom/my.conf" "/x/cfg"  #p"/home/u/" "/custom/my.conf"                  "explicit override wins")
               (nil               "/x/cfg"  #p"/home/u/" "/x/cfg/cl-tmux/cl-tmux.conf"      "XDG set")
               (nil               "/x/cfg/" #p"/home/u/" "/x/cfg/cl-tmux/cl-tmux.conf"      "XDG trailing slash")
               (nil               nil       #p"/home/u/" "/home/u/.config/cl-tmux/cl-tmux.conf" "no XDG fallback")
               (""                ""        #p"/home/u/" "/home/u/.config/cl-tmux/cl-tmux.conf" "empty env = unset")))
    (destructuring-bind (override xdg home expected desc) c
      (is (string= expected (config-path override xdg home)) "~A" desc))))

;;; load-config-file

(test load-config-file-missing-returns-nil
  "load-config-file on a non-existent path returns NIL."
  (with-isolated-config
    (is (null (load-config-file #p"/nonexistent/cl-tmux-xyz.conf"))
        "loading a non-existent config file should return NIL")))

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

;;; unbind directive

(test apply-directive-unbind-removes-binding
  "unbind removes an existing binding and returns T."
  (with-isolated-config
    (is (eq :new-window (lookup-key-binding #\c))
        "#\\c should be bound to :new-window before unbind")
    (assert-config-directive-applied '("unbind" "c")
                                     "a valid unbind directive")
    (is (null (lookup-key-binding #\c))
        "#\\c should be unbound after the unbind directive")))

(test apply-directive-unbind-a-clears-prefix-table
  "unbind -a removes every binding in the prefix table (the real tmux whole-table
   unbind, previously mis-parsed as a key literally named \"-a\")."
  (with-isolated-config
    (is (not (null (lookup-key-binding #\c))) "prefix has bindings before unbind -a")
    (assert-config-directive-applied '("unbind" "-a")
                                     "unbind -a")
    (is (null (lookup-key-binding #\c))
        "after unbind -a the prefix table must be empty")))

(test apply-directive-unbind-a-T-clears-named-table
  "unbind -a -T <table> clears only that named table."
  (with-isolated-config
    (apply-config-directive '("bind" "-T" "mytable" "x" "new-window"))
    (is (not (null (cl-tmux/config:key-table-lookup "mytable" #\x)))
        "mytable has the x binding before unbind -a -T")
    (assert-config-directive-applied '("unbind" "-a" "-T" "mytable")
                                     "unbind -a -T mytable")
    (is (null (cl-tmux/config:key-table-lookup "mytable" #\x))
        "mytable must be empty after unbind -a -T mytable")))

(test apply-directive-unbind-a-n-clears-root-table
  "unbind -a -n clears the root (no-prefix) key table."
  (with-isolated-config
    (apply-config-directive '("bind" "-n" "F1" "new-window"))
    (is (not (null (cl-tmux/config:key-table-lookup "root" "F1")))
        "root has the F1 binding before unbind -a -n")
    (assert-config-directive-applied '("unbind" "-a" "-n")
                                     "unbind -a -n")
    (is (null (cl-tmux/config:key-table-lookup "root" "F1"))
        "root table must be empty after unbind -a -n")))

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

;;; multi-character key tokens

(test bind-multichar-key-token
  "A multi-character key token (M-z) is stored as the string itself."
  (with-isolated-config
    (let ((applied (load-config-from-string "bind M-z next-window")))
      (is (= 1 applied)
          "exactly 1 directive should be applied, got ~A" applied)
      (is (eq :next-window (lookup-key-binding "M-z"))
          "the multi-char token M-z should bind the string key M-z")
      (is (eq :zoom-toggle (lookup-key-binding #\z))
          "the single character #\\z must still be :zoom-toggle"))))

;;; bind -N "note" (tmux 3.1+ key-binding description)

(test bind-with-note-flag-binds-correctly
  "bind -N \"note\" x next-window skips the -N note and binds x to :next-window
   (not the note string as a bogus command)."
  (with-isolated-config
    (let ((applied (load-config-from-string "bind -N \"Go to next window\" x next-window")))
      (is (= 1 applied) "the bind must apply as exactly one directive")
      (is (eq :next-window (lookup-key-binding #\x))
          "x must bind :next-window with the -N note skipped"))))

(test bind-note-flag-combined-with-n-and-r
  "bind -N works alongside other flags in any order: -n (root) and -r (repeat)."
  (with-isolated-config
    (is (= 1 (load-config-from-string "bind -n -N \"root note\" y next-window")))
    (is (eq :next-window (cl-tmux/config:key-table-command
                          (cl-tmux/config:key-table-lookup "root" #\y)))
        "bind -n -N must bind y in the root table")
    (is (= 1 (load-config-from-string "bind -r -N \"repeat note\" z next-window")))
    (let ((entry (cl-tmux/config:key-table-lookup "prefix" #\z)))
      (is (eq :next-window (cl-tmux/config:key-table-command entry))
          "bind -r -N must still bind z")
      (is (cl-tmux/config:key-table-repeatable-p entry)
          "the -r flag must survive alongside -N"))))

(test bind-note-flag-multi-command-block
  "bind -N \"note\" key { cmd1 ; cmd2 } combines a note with a brace block."
  (with-isolated-config
    (load-config-from-string "bind -N \"reload\" R { split-window ; next-window }")
    (let* ((entry (cl-tmux/config:key-table-lookup "prefix" #\R))
           (cmd   (cl-tmux/config:key-table-command entry)))
      (is (and (consp cmd) (eq :sequence (first cmd)))
          "a -N note plus a brace block must store a :sequence")
      (is (= 2 (length (rest cmd))) "both inner commands must be captured"))))

(test bind-note-flag-note-storage
  "bind -N stores the note on the binding; binding without -N has NIL note."
  (with-isolated-config
    (load-config-from-string "bind -N \"Go to next window\" x next-window")
    (let ((entry (cl-tmux/config:key-table-lookup "prefix" #\x)))
      (is (eq :next-window (cl-tmux/config:key-table-command entry)))
      (is (string= "Go to next window" (cl-tmux/config:key-table-note entry))
          "the -N note must be stored on the binding")))
  (with-isolated-config
    (load-config-from-string "bind y next-window")
    (is (null (cl-tmux/config:key-table-note
               (cl-tmux/config:key-table-lookup "prefix" #\y)))
        "no -N flag → NIL note")))

(test describe-key-bindings-surfaces-note
  "list-keys / describe-key-bindings renders the -N note in -N \"...\" form for a
   noted binding, and emits no -N marker for un-noted bindings."
  (with-isolated-config
    (load-config-from-string "bind -N \"reload config\" R source-file ~/.tmux.conf")
    (let ((output (cl-tmux/config:describe-key-bindings-for-table "prefix")))
      (is (search "reload config" output)
          "the note text must appear in the list-keys output")
      (is (search "-N \"reload config\"" output)
          "the note is rendered in -N \"...\" form"))))

;;; brace-block command syntax (tmux 3.x): bind r { cmd1 ; cmd2 }

(test strip-brace-block-unwraps-braces
  "%strip-brace-block returns the inner tokens of a { ... } block and leaves a
   non-block token list unchanged."
  (is (equal '("a" "b")
             (cl-tmux/config::%strip-brace-block '("{" "a" "b" "}"))))
  (is (equal '("a" "b")
             (cl-tmux/config::%strip-brace-block '("a" "b")))
      "a non-block list passes through untouched")
  (is (null (cl-tmux/config::%strip-brace-block '("{" "}")))
      "an empty block yields no inner tokens"))

(test line-brace-delta-counts-unquoted-braces
  "%line-brace-delta nets '{' against '}' and ignores braces inside quotes."
  (dolist (c '((1  "bind r {"                    "open brace is +1")
               (-1 "}"                           "close brace is -1")
               (0  "bind r { next-window }"      "balanced block nets 0")
               (0  "display \"a { b }\""         "braces inside a double-quoted string are ignored")))
    (destructuring-bind (expected input desc) c
      (is (= expected (cl-tmux/config::%line-brace-delta input)) "~A" desc))))

(test bind-single-line-brace-block-single-command
  "bind r { next-window } binds the single inner command as a keyword."
  (with-isolated-config
    (let ((applied (load-config-from-string "bind r { next-window }")))
      (is (= 1 applied))
      (is (eq :next-window (lookup-key-binding #\r))
          "C-b r must be bound to :next-window via the brace block"))))

(test bind-single-line-brace-block-multi-command
  "bind r { split-window ; next-window } stores a :sequence of both commands."
  (with-isolated-config
    (load-config-from-string "bind r { split-window ; next-window }")
    (let* ((entry (cl-tmux/config:key-table-lookup "prefix" #\r))
           (cmd   (cl-tmux/config:key-table-command entry)))
      (is (consp cmd) "command must be a list")
      (is (eq :sequence (first cmd)) "must be a :sequence")
      (is (= 2 (length (rest cmd))) "the sequence must hold two commands"))))

(test bind-empty-brace-block-rejected
  "bind r { } is not a valid binding (no command) and must apply nothing."
  (with-isolated-config
    (let ((applied (load-config-from-string "bind r { }")))
      (is (= 0 applied) "an empty brace block applies no directive"))))

(test bind-multiline-brace-block-joined-and-applied
  "A multi-line { ... } block (tmux 3.x style) is joined into one directive and
   stored as a :sequence — the inner newlines act as command separators."
  (with-isolated-config
    (let ((applied (load-config-from-string
                    (format nil "bind r {~%  split-window~%  next-window~%}~%"))))
      (is (= 1 applied) "the whole block counts as exactly one directive")
      (let* ((entry (cl-tmux/config:key-table-lookup "prefix" #\r))
             (cmd   (cl-tmux/config:key-table-command entry)))
        (is (and (consp cmd) (eq :sequence (first cmd)))
            "multi-line block must store a :sequence")
        (is (= 2 (length (rest cmd)))
            "both inner commands must be captured")))))

;;; top-level `;' command-sequence separator on ordinary config lines (tmux parity)

(test apply-config-line-splits-top-level-semicolons
  "apply-config-line splits a top-level unescaped `;' into separate command
   sequences and applies each segment in order (tmux command-sequence parity)."
  (with-isolated-options ("status" "on" "status-style" "")
    (is (eq t (cl-tmux/config::apply-config-line
               "set -g status off ; set -g status-style bg=red"))
        "a multi-segment line returns T when at least one segment applied")
    (is (string= "off" (cl-tmux/options:get-option "status"))
        "the first segment (set -g status off) must be applied")
    (is (string= "bg=red" (cl-tmux/options:get-option "status-style"))
        "the second segment (set -g status-style bg=red) must be applied")))

(test apply-config-line-ignores-empty-semicolon-segments
  "apply-config-line discards empty segments produced by doubled/trailing `;'.
   The separators are whitespace-delimited `;' tokens (an adjacent `;;' is one
   token the tokenizer does not split — a known limitation)."
  (with-isolated-options ("status" "on" "status-style" "")
    (is (eq t (cl-tmux/config::apply-config-line
               "set -g status off ; ; set -g status-style bg=red ;"))
        "doubled and trailing `;' produce empty segments that are skipped")
    (is (string= "off" (cl-tmux/options:get-option "status")))
    (is (string= "bg=red" (cl-tmux/options:get-option "status-style")))))

(test apply-config-line-bind-escaped-semicolon-stays-a-sequence
  "Regression: `bind r source-file ... \\; display ...' keeps its `\\;'-joined body
   as one bind sequence.  apply-config-line must NOT pre-split a bind line — bind
   owns its `;' tokens and invokes %split-on-semicolons itself."
  (with-isolated-config
    (is (eq t (cl-tmux/config::apply-config-line
               "bind r source-file ~/.tmux.conf \\; display-message Reloaded"))
        "the whole bind line is applied as a single directive")
    (let* ((entry (cl-tmux/config:key-table-lookup "prefix" #\r))
           (cmd   (cl-tmux/config:key-table-command entry)))
      (is (and (consp cmd) (eq :sequence (first cmd)))
          "the binding must store a :sequence, not a pre-split single command")
      (is (= 2 (length (rest cmd)))
          "both source-file and display-message must be in the sequence"))))

(test multiline-brace-block-does-not-corrupt-following-directive
  "After a multi-line brace block the reader resumes cleanly: a directive on the
   line after the closing brace is still applied."
  (with-isolated-config
    (let ((applied (load-config-from-string
                    (format nil "bind r {~%  next-window~%}~%bind x last-window~%"))))
      (is (= 2 applied) "block + following bind = 2 directives")
      (is (eq :last-window (lookup-key-binding #\x))
          "the directive after the block must still bind"))))

;;; load-config-from-stream

(test load-config-from-stream-applies
  "load-config-from-stream ignores comments and applies the real directives."
  (with-isolated-config
    (let ((applied (with-input-from-string
                       (s (format nil "# leading comment~%bind z next-window~%set-status-height 4~%"))
                     (load-config-from-stream s))))
      (is (= 2 applied)
          "exactly 2 directives should be applied, got ~A" applied)
      (is (eq :next-window (lookup-key-binding #\z))
          "#\\z should be bound to :next-window after the stream directives")
      (is (= 4 *status-height*)
          "*status-height* should be 4, got ~A" *status-height*))))

;;; ── %normalize-key-alias / *key-name-aliases* (navigation-key spellings) ────
;;;
;;; PPage/PgUp/NPage/PgDn/IC/DC are alternate spellings tmux accepts for the
;;; canonical navigation-key names the event loop emits (see %csi-tilde-key-name).
;;; %parse-key-token consults these via %normalize-key-alias so `bind -n PPage`
;;; resolves to the same binding as `bind -n PageUp`.

(test normalize-key-alias-table
  "%normalize-key-alias maps tmux's alternate navigation-key spellings to the
   canonical name the event loop emits, case-insensitively; unknown tokens
   return NIL so the caller falls through to the verbatim token."
  (dolist (row '(("PPage"  "PageUp"   "PPage aliases PageUp")
                 ("PgUp"   "PageUp"   "PgUp aliases PageUp")
                 ("NPage"  "PageDown" "NPage aliases PageDown")
                 ("PgDn"   "PageDown" "PgDn aliases PageDown")
                 ("IC"     "Insert"   "IC aliases Insert")
                 ("DC"     "Delete"   "DC aliases Delete")
                 ("ppage"  "PageUp"   "alias lookup is case-insensitive")
                 ("PageUp" nil        "the canonical name itself is not an alias")
                 ("Up"     nil        "an unrelated key name returns NIL")))
    (destructuring-bind (input expected desc) row
      (is (equal expected (cl-tmux/config::%normalize-key-alias input)) "~A" desc))))

(test bind-navigation-key-alias-resolves-canonical-binding
  "bind -n PPage <cmd> and bind -n PageUp <cmd> store under the SAME canonical
   key, so either spelling in a .tmux.conf reaches the binding the event loop
   looks up."
  (with-isolated-config
    (cl-tmux/config:apply-config-directive '("bind" "-n" "PPage" "next-window"))
    (let ((entry (cl-tmux/config:key-table-lookup "root" "PageUp")))
      (is (eq :next-window (cl-tmux/config:key-table-command entry))
          "PPage must bind under the canonical \"PageUp\" key"))))

;;; ── %parse-control-char (config-tokenizer.lisp) ──────────────────────────────
;;;
;;; Returns the control CHARACTER (not the byte %prefix-control-byte returns)
;;; for the part of a token after a "C-" prefix; used by %parse-key-token.

(test parse-control-char-table
  "%parse-control-char maps C-a..C-z to ^A..^Z, C-Space/C-@ to NUL, the
   bracket/backslash/caret/underscore control keys to their control chars, and
   any other input to NIL."
  (dolist (row (list (list "Space" (code-char 0)  "C-Space -> NUL")
                     (list "a"     (code-char 1)  "C-a -> ^A")
                     (list "z"     (code-char 26) "C-z -> ^Z")
                     (list "A"     (code-char 1)  "C-A -> ^A (case-insensitive)")
                     (list "@"     (code-char 0)  "C-@ -> NUL")
                     (list "["     (code-char 27) "C-[ -> ESC")
                     (list "\\"    (code-char 28) "C-\\ -> FS")
                     (list "]"     (code-char 29) "C-] -> GS")
                     (list "^"     (code-char 30) "C-^ -> RS")
                     (list "_"     (code-char 31) "C-_ -> US")
                     (list "1"     nil            "C-1 is not a controllable key")
                     (list "ab"    nil            "multi-char rest is not a single key")))
    (destructuring-bind (input expected desc) row
      (is (equal expected (cl-tmux/config::%parse-control-char input)) "~A" desc))))

;;; ── %canonical-command-name / %known-command-name-p (config-commands.lisp) ──

(test canonical-command-name-table
  "%canonical-command-name is identity: command aliases are not a compatibility
   layer, so canonical names and shorthand spellings pass through unchanged."
  (dolist (row '(("neww"        "neww"           "neww stays unresolved")
                 ("splitw"      "splitw"         "splitw stays unresolved")
                 ("NEWW"        "NEWW"           "case is not alias-normalized")
                 ("killp"       "killp"          "killp stays unresolved")
                 ("new-window"  "new-window"      "a canonical name passes through unchanged")
                 ("bogus-xyz"   "bogus-xyz"       "an unrecognised name passes through unchanged")))
    (destructuring-bind (input expected desc) row
      (is (string= expected (cl-tmux/config::%canonical-command-name input)) "~A" desc))))

(test known-command-name-p-table
  "%known-command-name-p accepts bindable keywords and known canonical names; it
   rejects tmux short aliases and genuine typos."
  (dolist (row '(("new-window"        t   "a bindable keyword name is known")
                 ("neww"              nil "a tmux short alias is rejected")
                 ("previous-window"   t   "an arg-only canonical name is known")
                 ("breakp"            nil "break-pane's alias is rejected")
                 ("totally-bogus-xyz" nil "a genuine typo is not known")
                 (""                  nil "the empty string is not known")))
    (destructuring-bind (input expected desc) row
      (is (eq expected (and (cl-tmux/config::%known-command-name-p input) t)) "~A" desc))))

;;; ── key-label (config-listing.lisp) ──────────────────────────────────────────

(test key-label-table
  "key-label renders a character key as a one-character string and passes a
   string key (named keys like \"Up\"/\"C-Right\") through unchanged."
  (dolist (row (list (list #\c      "c"       "a character key becomes a 1-char string")
                     (list #\%      "%"       "a punctuation character key becomes a 1-char string")
                     (list "Up"     "Up"      "a string key passes through unchanged")
                     (list "C-Right" "C-Right" "a multi-char named key passes through unchanged")))
    (destructuring-bind (input expected desc) row
      (is (string= expected (cl-tmux/config::key-label input)) "~A" desc))))
