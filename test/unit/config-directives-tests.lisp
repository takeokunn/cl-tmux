(in-package #:cl-tmux/test)

;;;; bindable-commands, apply-config-directive, set flags, bind/unbind, load-config-from-stream — part I

(def-suite config-directives-suite :description "Config file directive parsing")
(in-suite config-directives-suite)

;;; Import the config-directives symbols we need

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(cl-tmux/config:lookup-key-binding
             cl-tmux/config:*default-shell*
            cl-tmux/config:*status-height*
            cl-tmux/config:set-key-binding
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
;;; test/helpers.lisp so all test suites can reuse them.

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
    (is (eq t (apply-config-directive '("bind" "z" "new-window")))
        "valid bind directive should return T")
    (is (eq :new-window (lookup-key-binding #\z))
        "#\\z should be bound to :new-window after the bind directive")))

(test apply-directive-unknown-returns-nil
  "apply-config-directive for an unknown command returns NIL and changes nothing."
  (with-isolated-config
    (let* ((tbl (cl-tmux/config:ensure-key-table "prefix"))
           (count-before (hash-table-count tbl))
           (shell-before    *default-shell*)
           (height-before   *status-height*))
      (is (null (apply-config-directive '("bogus" "x")))
          "an unknown command should return NIL")
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
    (is (eq t (apply-config-directive '("set" "-g" "status" "off")))
        "set -g status off must return T (applied)")
    (is (string= "off" (cl-tmux/options:get-option "status"))
        "status stores the choice string \"off\" (status-line-count maps it to 0)")
    (is (null (cl-tmux/options:get-option "-g"))
        "must NOT create an option literally named '-g'")))

(test apply-set-directive-append-flag
  "'set -ag <name> <value>' appends to the option's current value."
  (with-isolated-options ("status-left" "A")
    (is (eq t (apply-config-directive '("set" "-ag" "status-left" "B")))
        "set -ag must return T")
    (is (string= "AB" (cl-tmux/options:get-option "status-left"))
        "set -ag must append B to the existing A")))

(test apply-set-directive-plain-unaffected
  "Plain 'set name value' (no flags) still flows through the normal directive
   table and applies unchanged."
  (with-isolated-options ()
    (is (eq t (apply-config-directive '("set" "status" "off")))
        "plain set must still return T")
    (is (string= "off" (cl-tmux/options:get-option "status"))
        "plain set status off stores the choice string \"off\"")))

;;; bind key to a command LINE (arg-taking key bindings)

(test bind-key-to-command-line-stores-token-list
  "'bind X display-message hello' binds X to the command token list, applies (T),
   and list-keys shows the reconstructed command line."
  (with-isolated-key-tables
    (is (eq t (apply-config-directive '("bind" "X" "display-message" "hello")))
        "multi-token bind must return T")
    (is (equal '("display-message" "hello") (lookup-key-binding #\X))
        "binding value must be the command token list")
    (is (search "display-message hello" (cl-tmux/config:describe-key-bindings))
        "list-keys must show the reconstructed command line")))

(test bind-key-single-keyword-still-binds
  "'bind z new-window' still binds the key to the :new-window keyword (existing
   single-command behaviour is preserved)."
  (with-isolated-key-tables
    (is (eq t (apply-config-directive '("bind" "z" "new-window")))
        "single known-command bind must return T")
    (is (eq :new-window (lookup-key-binding #\z))
        "a single known command binds to its keyword")))

(test bind-key-single-unknown-command-rejected
  "'bind z totally-bogus-xyz' (one unknown command word) is still rejected (NIL),
   and the unknown command is not stored (z keeps its default keyword binding)."
  (with-isolated-key-tables
    (is (null (apply-config-directive '("bind" "z" "totally-bogus-xyz")))
        "an unknown single command must be rejected")
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
    (is (eq t (apply-config-directive '("set-shell" "/usr/bin/zsh")))
        "set-shell directive should return T")
    (is (string= "/usr/bin/zsh" *default-shell*)
        "*default-shell* should be /usr/bin/zsh, got ~A" *default-shell*)
    (is (eq t (apply-config-directive '("set-status-height" "2")))
        "set-status-height directive should return T")
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
      (is (null (apply-config-directive tokens))
          "~S should return NIL" tokens))))

;;; unbind directive

(test apply-directive-unbind-removes-binding
  "unbind removes an existing binding and returns T."
  (with-isolated-config
    (is (eq :new-window (lookup-key-binding #\c))
        "#\\c should be bound to :new-window before unbind")
    (is (eq t (apply-config-directive '("unbind" "c")))
        "a valid unbind directive should return T")
    (is (null (lookup-key-binding #\c))
        "#\\c should be unbound after the unbind directive")))

(test apply-directive-unbind-a-clears-prefix-table
  "unbind -a removes every binding in the prefix table (the real tmux whole-table
   unbind, previously mis-parsed as a key literally named \"-a\")."
  (with-isolated-config
    (is (not (null (lookup-key-binding #\c))) "prefix has bindings before unbind -a")
    (is (eq t (apply-config-directive '("unbind" "-a")))
        "unbind -a must return T")
    (is (null (lookup-key-binding #\c))
        "after unbind -a the prefix table must be empty")))

(test apply-directive-unbind-a-T-clears-named-table
  "unbind -a -T <table> clears only that named table."
  (with-isolated-config
    (apply-config-directive '("bind" "-T" "mytable" "x" "new-window"))
    (is (not (null (cl-tmux/config:key-table-lookup "mytable" #\x)))
        "mytable has the x binding before unbind -a -T")
    (is (eq t (apply-config-directive '("unbind" "-a" "-T" "mytable")))
        "unbind -a -T mytable must return T")
    (is (null (cl-tmux/config:key-table-lookup "mytable" #\x))
        "mytable must be empty after unbind -a -T mytable")))

(test apply-directive-unbind-a-n-clears-root-table
  "unbind -a -n clears the root (no-prefix) key table."
  (with-isolated-config
    (apply-config-directive '("bind" "-n" "F1" "new-window"))
    (is (not (null (cl-tmux/config:key-table-lookup "root" "F1")))
        "root has the F1 binding before unbind -a -n")
    (is (eq t (apply-config-directive '("unbind" "-a" "-n")))
        "unbind -a -n must return T")
    (is (null (cl-tmux/config:key-table-lookup "root" "F1"))
        "root table must be empty after unbind -a -n")))

;;; set-status-height: tolerant parsing

(test set-status-height-noninteger-is-tolerated
  "Non-integer or non-positive set-status-height values return NIL and do not signal."
  (with-isolated-config
    (let ((before *status-height*))
      (is (null (handler-case (apply-config-directive '("set-status-height" "abc"))
                  (error (e)
                    (fail "set-status-height with non-integer must not signal, got ~A" e)
                    :signaled)))
          "set-status-height with a non-integer value should return NIL")
      (is (eql before *status-height*)
          "*status-height* should be unchanged, got ~A" *status-height*)
      (is (null (handler-case (apply-config-directive '("set-status-height" "0"))
                  (error (e)
                    (fail "set-status-height with 0 must not signal, got ~A" e)
                    :signaled)))
          "set-status-height with a non-positive value (0) should return NIL")
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
