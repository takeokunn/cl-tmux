(in-package #:cl-tmux/test)

;;;; Config-file directive parsing tests.
;;;;
;;;; These tests cover the config-directives layer:
;;;;   * %config-tokens, %parse-key-token
;;;;   * *bindable-commands*, %command-keyword
;;;;   * apply-config-directive, apply-config-line
;;;;   * load-config-from-stream, load-config-from-string
;;;;   * %config-path-from, config-file-path, load-config-file

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
  (is (not (member :copy-mode-exit cl-tmux/config::*bindable-commands*))
      ":copy-mode-exit must not be a user-bindable command")
  (is (not (member :copy-mode-up cl-tmux/config::*bindable-commands*))
      ":copy-mode-up must not be a user-bindable command")
  (is (not (member :copy-mode-down cl-tmux/config::*bindable-commands*))
      ":copy-mode-down must not be a user-bindable command")
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

(test config-path-explicit-override-wins
  "$CL_TMUX_CONF takes precedence over XDG and the default."
  (is (equal #p"/custom/my.conf"
             (cl-tmux/config::%config-path-from "/custom/my.conf" "/x/cfg" #p"/home/u/"))
      "an explicit override must be used verbatim"))

(test config-path-honors-xdg-config-home
  "Without an override, $XDG_CONFIG_HOME/cl-tmux/cl-tmux.conf is used."
  (is (string= "/x/cfg/cl-tmux/cl-tmux.conf"
               (config-path nil "/x/cfg" #p"/home/u/")))
  (is (string= "/x/cfg/cl-tmux/cl-tmux.conf"
               (config-path nil "/x/cfg/" #p"/home/u/"))
      "a trailing slash on XDG_CONFIG_HOME must not double up"))

(test config-path-defaults-to-dot-config
  "With neither override nor XDG set, the path defaults under ~/.config."
  (is (string= "/home/u/.config/cl-tmux/cl-tmux.conf"
               (config-path nil nil #p"/home/u/"))))

(test config-path-empty-env-is-unset
  "Empty-string env values are treated as unset."
  (is (string= "/home/u/.config/cl-tmux/cl-tmux.conf"
               (config-path "" "" #p"/home/u/"))))

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

(test bind-note-flag-stores-note-on-binding
  "bind -N \"note\" key cmd stores the note, retrievable via key-table-note."
  (with-isolated-config
    (load-config-from-string "bind -N \"Go to next window\" x next-window")
    (let ((entry (cl-tmux/config:key-table-lookup "prefix" #\x)))
      (is (eq :next-window (cl-tmux/config:key-table-command entry)))
      (is (string= "Go to next window" (cl-tmux/config:key-table-note entry))
          "the -N note must be stored on the binding"))))

(test bind-without-note-has-nil-note
  "A binding made without -N has a NIL note."
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
  (is (= 1  (cl-tmux/config::%line-brace-delta "bind r {")))
  (is (= -1 (cl-tmux/config::%line-brace-delta "}")))
  (is (= 0  (cl-tmux/config::%line-brace-delta "bind r { next-window }")))
  (is (= 0  (cl-tmux/config::%line-brace-delta "display \"a { b }\""))
      "braces inside a double-quoted string are ignored"))

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

;;; load-config-file on a real temp file

(test load-config-file-existing-temp-file
  "load-config-file applies an existing file's directives and returns the count."
  (with-isolated-config
    (with-temp-config-file (p "bind z next-window" "set-status-height 3")
      (is (= 2 (load-config-file p))
          "load-config-file should apply and count both directives")
      (is (eq :next-window (lookup-key-binding #\z))
          "#\\z should be bound to :next-window after loading the temp file")
      (is (= 3 *status-height*)
          "*status-height* should be 3 after loading the temp file, got ~A"
          *status-height*))))

;;; %command-keyword: resolution and non-interning contract

(test command-keyword-does-not-intern-unknown
  "%command-keyword must NOT intern an unknown command name into :keyword."
  (let ((name "CL-TMUX-NONEXISTENT-COMMAND-DO-NOT-INTERN-ME"))
    (is (null (find-symbol name :keyword))
        "precondition: ~A must not be interned in :keyword before the call" name)
    (is (null (cl-tmux/config::%command-keyword name))
        "an unknown command name must resolve to NIL")
    (is (null (find-symbol name :keyword))
        "%command-keyword must not have interned ~A into the keyword package"
        name)))

(test command-keyword-returns-bindable-keyword
  "%command-keyword returns the command keyword for a recognized bindable name."
  (is (eq :new-window (cl-tmux/config::%command-keyword "new-window"))
      "new-window should resolve to :new-window")
  (is (eq :new-window (cl-tmux/config::%command-keyword "NEW-WINDOW"))
      "resolution should be case-insensitive")
  (is (eq :split-horizontal (cl-tmux/config::%command-keyword "split-horizontal"))
      "split-horizontal should resolve to :split-horizontal"))

(test command-keyword-resolves-tmux-aliases
  "%command-keyword resolves tmux command-name aliases (full names whose keyword
   differs, plus short forms) to the canonical bindable keyword."
  (is (eq :prev-window      (cl-tmux/config::%command-keyword "previous-window")))
  (is (eq :copy-mode-enter  (cl-tmux/config::%command-keyword "copy-mode")))
  (is (eq :swap-pane-forward (cl-tmux/config::%command-keyword "swap-pane")))
  (is (eq :detach           (cl-tmux/config::%command-keyword "detach-client")))
  (is (eq :show-window-options (cl-tmux/config::%command-keyword "showw")))
  (is (eq :prev-window      (cl-tmux/config::%command-keyword "PREVIOUS-WINDOW"))
      "alias resolution must be case-insensitive"))

(test command-keyword-resolves-standard-tmux-abbreviations
  "%command-keyword resolves the standard tmux command abbreviations (man tmux
   ALIASES) for arg-less bindable commands to their canonical keyword."
  (is (eq :break-pane    (cl-tmux/config::%command-keyword "breakp")))
  (is (eq :kill-pane     (cl-tmux/config::%command-keyword "killp")))
  (is (eq :next-window   (cl-tmux/config::%command-keyword "next")))
  (is (eq :prev-window   (cl-tmux/config::%command-keyword "prev")))
  (is (eq :last-window   (cl-tmux/config::%command-keyword "last")))
  (is (eq :display-panes (cl-tmux/config::%command-keyword "displayp")))
  (is (eq :rotate-window (cl-tmux/config::%command-keyword "rotatew"))))

(test bind-tmux-abbreviation-fires
  "bind b breakp binds :break-pane via the abbreviation, and an unknown abbrev
   is still rejected."
  (with-isolated-config
    (is (= 1 (load-config-from-string "bind b breakp")))
    (is (eq :break-pane (lookup-key-binding #\b))
        "abbrev breakp must bind :break-pane")
    (is (= 0 (load-config-from-string "bind Q definitely-not-a-command"))
        "an unknown abbreviation must still be rejected")))

(test command-name-aliases-target-bindable-keywords
  "Every alias value must be a member of *bindable-commands* (else the bind would
   resolve to a keyword the dispatcher rejects)."
  (dolist (pair cl-tmux/config::*command-name-aliases*)
    (is (member (cdr pair) cl-tmux/config::*bindable-commands*)
        "alias ~A -> ~A must target a bindable keyword" (car pair) (cdr pair))))

(test bind-tmux-alias-name-fires
  "bind x previous-window binds the canonical :prev-window keyword via the alias."
  (with-isolated-config
    (let ((applied (load-config-from-string "bind x previous-window")))
      (is (= 1 applied))
      (is (eq :prev-window (lookup-key-binding #\x))
          "previous-window alias must bind :prev-window"))))

(test command-keyword-rejects-non-bindable-keyword
  "%command-keyword returns NIL for interned-but-non-bindable keywords."
  (let ((kw (intern "COPY-MODE-EXIT" :keyword)))
    (is (eq :copy-mode-exit kw)
        "the keyword :copy-mode-exit should be interned")
    (is (not (member :copy-mode-exit cl-tmux/config::*bindable-commands*))
        "precondition: :copy-mode-exit must not be in *bindable-commands*")
    (is (null (cl-tmux/config::%command-keyword "copy-mode-exit"))
        "%command-keyword must reject an interned-but-non-bindable keyword"))
  (let ((cl-tmux/config::*bindable-commands* '(:detach)))
    (is (null (cl-tmux/config::%command-keyword "new-window"))
        "with *bindable-commands* not containing :new-window, NIL is returned")
    (is (eq :detach (cl-tmux/config::%command-keyword "detach"))
        "a name still present in the rebound *bindable-commands* resolves")))

;;; %config-tokens (tokenizer)

(test config-tokens-splits-on-whitespace
  "%config-tokens splits a line into whitespace-separated tokens."
  (is (equal '("bind" "c" "new-window")
             (cl-tmux/config::%config-tokens "bind c new-window")))
  (is (equal '("set-shell" "/bin/bash")
             (cl-tmux/config::%config-tokens "  set-shell  /bin/bash  ")))
  (is (null (cl-tmux/config::%config-tokens ""))
      "empty string yields no tokens")
  (is (null (cl-tmux/config::%config-tokens "   "))
      "whitespace-only string yields no tokens"))

;;; %parse-key-token

(test parse-key-token-single-char-returns-char
  "%parse-key-token returns a character for a 1-char token."
  (is (char= #\c   (cl-tmux/config::%parse-key-token "c")))
  (is (char= #\%   (cl-tmux/config::%parse-key-token "%")))
  (is (char= #\"   (cl-tmux/config::%parse-key-token "\""))))

(test parse-key-token-multi-char-returns-string
  "%parse-key-token returns the string itself for tokens longer than 1 char."
  (is (string= "M-1" (cl-tmux/config::%parse-key-token "M-1")))
  (is (string= "F1"  (cl-tmux/config::%parse-key-token "F1"))))

(test parse-key-token-control-letters-return-control-char
  "%parse-key-token converts C-<letter> to its control character so the binding
   matches the byte the event loop sees: C-a→^A(1), C-z→^Z(26), C-b→^B(2)."
  (is (char= (code-char 1)  (cl-tmux/config::%parse-key-token "C-a")) "C-a → ^A (1)")
  (is (char= (code-char 26) (cl-tmux/config::%parse-key-token "C-z")) "C-z → ^Z (26)")
  (is (char= (code-char 2)  (cl-tmux/config::%parse-key-token "C-b")) "C-b → ^B (2)")
  ;; Case-insensitive: C-A is the same control char as C-a.
  (is (char= (code-char 1)  (cl-tmux/config::%parse-key-token "C-A")) "C-A → ^A (1)"))

(test parse-key-token-control-space-and-at-return-nul
  "C-Space and C-@ both map to the NUL control character (byte 0)."
  (is (char= (code-char 0) (cl-tmux/config::%parse-key-token "C-Space")) "C-Space → NUL")
  (is (char= (code-char 0) (cl-tmux/config::%parse-key-token "C-@")) "C-@ → NUL"))

(test parse-key-token-control-punctuation-return-control-char
  "C-[ C-\\ C-] map to control bytes 27/28/29."
  (is (char= (code-char 27) (cl-tmux/config::%parse-key-token "C-[")) "C-[ → ESC (27)")
  (is (char= (code-char 28) (cl-tmux/config::%parse-key-token "C-\\")) "C-\\ → FS (28)")
  (is (char= (code-char 29) (cl-tmux/config::%parse-key-token "C-]")) "C-] → GS (29)"))

(test parse-key-token-control-modifier-arrow-stays-string
  "C-<named-key> (e.g. C-Left) that the event loop encodes as a multi-byte
   sequence is kept as the string for the deferred modifier-key path."
  (is (string= "C-Left"  (cl-tmux/config::%parse-key-token "C-Left")))
  (is (string= "C-Up"    (cl-tmux/config::%parse-key-token "C-Up"))))

(test bind-control-letter-fires-via-control-char
  "bind C-a <cmd> binds the control character ^A (byte 1) so a real Ctrl-a
   keypress (which the event loop reads as byte 1) resolves to the command."
  (with-isolated-config
    (cl-tmux/config:apply-config-directive '("bind" "C-a" "next-window"))
    (is (eq :next-window (lookup-key-binding (code-char 1)))
        "C-a must bind ^A (byte 1) in the prefix table")))

(test bind-modifier-arrow-stores-canonical-string-key
  "bind C-Up <cmd> stores the command under the string key \"C-Up\" in the
   prefix table — the canonical key name the event loop reconstructs from the
   ESC [ 1 ; 5 A wire sequence.  (Without this, modifier+arrow binds were dead.)"
  (with-isolated-config
    (cl-tmux/config:apply-config-directive '("bind" "C-Up" "next-window"))
    (let ((entry (cl-tmux/config:key-table-lookup "prefix" "C-Up")))
      (is (eq :next-window (cl-tmux/config:key-table-command entry))
          "C-Up must bind under string key \"C-Up\" in the prefix table"))))

(test bind-n-modifier-arrow-stores-in-root-table
  "bind -n M-Left <cmd> stores under string key \"M-Left\" in the ROOT table so
   a bare (no-prefix) Alt+Left fires it."
  (with-isolated-config
    (cl-tmux/config:apply-config-directive '("bind" "-n" "M-Left" "next-window"))
    (let ((entry (cl-tmux/config:key-table-lookup "root" "M-Left")))
      (is (eq :next-window (cl-tmux/config:key-table-command entry))
          "M-Left must bind under string key \"M-Left\" in the root table"))))

(test bind-plain-arrow-stores-canonical-string-key
  "bind Up <cmd> stores under string key \"Up\" in the prefix table, matching
   the name reconstructed from the ESC [ A wire sequence."
  (with-isolated-config
    (cl-tmux/config:apply-config-directive '("bind" "Up" "next-window"))
    (let ((entry (cl-tmux/config:key-table-lookup "prefix" "Up")))
      (is (eq :next-window (cl-tmux/config:key-table-command entry))
          "Up must bind under string key \"Up\" in the prefix table"))))

(test bind-n-meta-key-stores-in-root-table
  "bind -n M-h <cmd> stores under string key \"M-h\" in the ROOT table so a bare
   (no-prefix) Alt+h, which arrives as ESC h, fires it."
  (with-isolated-config
    (cl-tmux/config:apply-config-directive '("bind" "-n" "M-h" "next-window"))
    (let ((entry (cl-tmux/config:key-table-lookup "root" "M-h")))
      (is (eq :next-window (cl-tmux/config:key-table-command entry))
          "M-h must bind under string key \"M-h\" in the root table"))))

;;; status option: off / on / line-count parsing → *status-height*

(test set-status-numeric-shows-bar
  "`set -g status 2` reserves two status rows (multi-line); the count is clamped
   to tmux's maximum of 5."
  (with-isolated-config
    (cl-tmux/config:apply-config-directive '("set" "-g" "status" "2"))
    (is (= 2 cl-tmux/config:*status-height*)
        "status 2 must reserve 2 rows")
    (cl-tmux/config:apply-config-directive '("set" "-g" "status" "5"))
    (is (= 5 cl-tmux/config:*status-height*) "status 5 → 5 rows")
    (cl-tmux/config:apply-config-directive '("set" "-g" "status" "9"))
    (is (= 5 cl-tmux/config:*status-height*) "status 9 → clamped to 5 rows")))

(test set-status-off-hides-bar
  "`set -g status off` (and 0/false) hides the status bar (height 0)."
  (with-isolated-config
    (cl-tmux/config:apply-config-directive '("set" "-g" "status" "off"))
    (is (= 0 cl-tmux/config:*status-height*) "status off → height 0")
    (cl-tmux/config:apply-config-directive '("set" "-g" "status" "0"))
    (is (= 0 cl-tmux/config:*status-height*) "status 0 → height 0")))

(test set-status-on-shows-bar
  "`set -g status on` shows the status bar (height 1)."
  (with-isolated-config
    (cl-tmux/config:apply-config-directive '("set" "-g" "status" "off"))
    (cl-tmux/config:apply-config-directive '("set" "-g" "status" "on"))
    (is (= 1 cl-tmux/config:*status-height*) "status on → height 1")))

;;; apply-config-line

(test apply-config-line-applies-valid-directives
  "apply-config-line applies a directive line and returns T."
  (with-isolated-config
    (is (eq t (cl-tmux/config::apply-config-line "bind z new-window"))
        "valid directive line should return T")
    (is (eq :new-window (lookup-key-binding #\z)))))

(test apply-config-line-ignores-blank-and-comments
  "apply-config-line returns NIL for blank lines and # comments."
  (is (null (cl-tmux/config::apply-config-line ""))
      "blank line should return NIL")
  (is (null (cl-tmux/config::apply-config-line "   "))
      "whitespace-only line should return NIL")
  (is (null (cl-tmux/config::apply-config-line "# this is a comment"))
      "comment line should return NIL"))

(test define-config-directives-macro-is-defined
  "define-config-directives is a defined macro."
  (is (macro-function 'cl-tmux/config::define-config-directives)))

(test define-key-directive-handlers-macro-is-defined
  "define-key-directive-handlers is a defined macro."
  (is (macro-function 'cl-tmux/config::define-key-directive-handlers)))

(test env-set-p-correctly-classifies-strings
  "%env-set-p returns T for non-empty strings and NIL for nil or empty strings."
  (is-true  (cl-tmux/config::%env-set-p "/some/path")  "non-empty string is set")
  (is-true  (cl-tmux/config::%env-set-p "x")           "single-char string is set")
  (is-false (cl-tmux/config::%env-set-p nil)            "nil is not set")
  (is-false (cl-tmux/config::%env-set-p "")             "empty string is not set"))

;;; Tokenizer with quote/escape support

(test config-tokenizer-quoted-double-quotes
  "%config-tokens with a double-quoted string produces a single token preserving spaces."
  (let ((tokens (cl-tmux/config::%config-tokens "bind n \"foo bar\"")))
    (is (equal '("bind" "n" "foo bar") tokens)
        "double-quoted string must yield a single token with spaces: got ~S" tokens)))

(test config-tokenizer-single-quotes
  "%config-tokens with a single-quoted string produces a single token."
  (let ((tokens (cl-tmux/config::%config-tokens "set-shell '/usr/bin/my shell'")))
    (is (= 2 (length tokens))
        "single-quoted path must produce 2 tokens, got ~D: ~S" (length tokens) tokens)
    (is (string= "/usr/bin/my shell" (second tokens))
        "second token must be the single-quoted value, got ~S" (second tokens))))

(test config-tokenizer-backslash-escape
  "%config-tokens: backslash outside quotes escapes the next character."
  ;; The Lisp literal "foo\\ bar" is the 8-char string  foo\ bar  (with a real
  ;; backslash); that is what the tokenizer must collapse to "foo bar".
  (let ((tokens (cl-tmux/config::%config-tokens "foo\\ bar")))
    (is (= 1 (length tokens))
        "backslash-escaped space must yield a single token, got ~S" tokens)
    (is (string= "foo bar" (first tokens))
        "token must be foo bar after backslash-space, got ~S" (first tokens))))

(test config-tokenizer-empty-double-quotes
  "%config-tokens: empty double-quotes produces an empty string token."
  (let ((tokens (cl-tmux/config::%config-tokens "cmd \"\"")))
    (is (= 2 (length tokens))
        "empty double-quotes must yield 2 tokens, got ~S" tokens)
    (is (string= "" (second tokens))
        "second token must be the empty string, got ~S" (second tokens))))

(test config-tokenizer-mixed
  "%config-tokens: mix of plain tokens, quoted tokens, and backslash escapes."
  ;; "a \"b c\" d\\ e" is the literal  a "b c" d\ e  — quoted token preserves the
  ;; inner space; the backslash-space escapes to keep "d e" as one token.
  (let ((tokens (cl-tmux/config::%config-tokens "a \"b c\" d\\ e")))
    (is (= 3 (length tokens))
        "must have 3 tokens, got ~S" tokens)
    (is (string= "a" (first tokens)) "first token is a")
    (is (string= "b c" (second tokens)) "second token is b c")
    (is (string= "d e" (third tokens)) "third token is d e (backslash-space)")))

;;; bind-key with flags

(test bind-key-no-prefix-n-flag
  "bind -n binds in the root key-table (no prefix required)."
  (with-isolated-key-tables
    (apply-config-directive '("bind" "-n" "C" "new-window"))
    (let ((entry (cl-tmux/config:key-table-lookup "root" #\C)))
      (is (not (null entry))
          "bind -n must add a binding to the root table")
      (is (eq :new-window (cl-tmux/config:key-table-command entry))
          "root binding must be :new-window"))))

(test bind-key-repeatable-r-flag
  "bind -r marks the binding as repeatable."
  (with-isolated-key-tables
    (apply-config-directive '("bind" "-r" "H" "resize-left"))
    (let ((entry (cl-tmux/config:key-table-lookup "prefix" #\H)))
      (is (not (null entry))
          "bind -r must add a binding to the prefix table")
      (is (cl-tmux/config:key-table-repeatable-p entry)
          "binding must be marked repeatable with -r flag"))))

(test bind-key-custom-table-T-flag
  "bind -T table-name binds in the named key-table."
  (with-isolated-key-tables
    (apply-config-directive '("bind" "-T" "copy-mode" "q" "copy-mode-enter"))
    (let ((entry (cl-tmux/config:key-table-lookup "copy-mode" #\q)))
      (is (not (null entry))
          "bind -T copy-mode must add a binding to the copy-mode table")
      (is (eq :copy-mode-enter (cl-tmux/config:key-table-command entry))
          "copy-mode binding must be :copy-mode-enter"))))

(test bind-key-simple-also-updates-key-table
  "Simple bind (no flags) also updates the prefix key-table."
  (with-isolated-key-tables
    (apply-config-directive '("bind" "z" "new-window"))
    (let ((entry (cl-tmux/config:key-table-lookup "prefix" #\z)))
      (is (not (null entry))
          "simple bind must also add to the prefix key-table")
      (is (eq :new-window (cl-tmux/config:key-table-command entry))
          "prefix binding must be :new-window"))))

;;; bind-key directive alias
;;;
;;; NOTE: The six individual set-option alias tests (set-directive-stores-option,
;;; setw-directive-stores-option, etc.) have been removed.  The table-driven test
;;; set-option-directive-aliases-table-driven (below) supersedes them all.

(test bind-key-alias-accepted
  "bind-key is accepted as an alias for bind and creates a prefix binding."
  (with-isolated-key-tables
    (is (eq t (apply-config-directive '("bind-key" "z" "new-window")))
        "bind-key directive should return T")
    (is (eq :new-window (lookup-key-binding #\z))
        "#\\z should be bound to :new-window via bind-key")))

(test bind-key-alias-n-flag
  "bind-key -n binds in the root table (no prefix required)."
  (with-isolated-key-tables
    (apply-config-directive '("bind-key" "-n" "F" "new-window"))
    (let ((entry (cl-tmux/config:key-table-lookup "root" #\F)))
      (is (not (null entry))
          "bind-key -n must add a binding to the root table")
      (is (eq :new-window (cl-tmux/config:key-table-command entry))
          "root table binding must be :new-window"))))

;;; unbind-key directive alias

(test unbind-key-alias-removes-binding
  "unbind-key is accepted as an alias for unbind and removes a binding."
  (with-isolated-config
    (is (eq :new-window (lookup-key-binding #\c))
        "#\\c should be bound to :new-window before unbind-key")
    (is (eq t (apply-config-directive '("unbind-key" "c")))
        "a valid unbind-key directive should return T")
    (is (null (lookup-key-binding #\c))
        "#\\c should be unbound after the unbind-key directive")))

;;; unbind with -n flag

(test unbind-with-n-flag-removes-root-binding
  "unbind -n removes a binding from the root table."
  (with-isolated-key-tables
    (apply-config-directive '("bind" "-n" "X" "new-window"))
    (let ((entry (cl-tmux/config:key-table-lookup "root" #\X)))
      (is (not (null entry)) "binding must exist before unbind -n"))
    (is (eq t (apply-config-directive '("unbind" "-n" "X")))
        "unbind -n must return T")
    (is (null (cl-tmux/config:key-table-lookup "root" #\X))
        "root binding must be removed after unbind -n")))

;;; unbind with -T flag

(test unbind-with-T-flag-removes-named-table-binding
  "unbind -T copy-mode removes a binding from the named table."
  (with-isolated-key-tables
    (apply-config-directive '("bind" "-T" "copy-mode" "q" "copy-mode-enter"))
    (let ((entry (cl-tmux/config:key-table-lookup "copy-mode" #\q)))
      (is (not (null entry)) "binding must exist before unbind -T"))
    (is (eq t (apply-config-directive '("unbind" "-T" "copy-mode" "q")))
        "unbind -T copy-mode must return T")
    (is (null (cl-tmux/config:key-table-lookup "copy-mode" #\q))
        "copy-mode binding must be removed after unbind -T")))

;;; %whitespace-p
;;; NOTE: set-option-directive-stores-option, set-window-option-directive-stores-option,
;;; sets-directive-stores-option, and set-session-option-directive-stores-option have
;;; been removed — all superseded by set-option-directive-aliases-table-driven (below).

(test whitespace-p-recognizes-space-and-tab
  "%whitespace-p returns T for space and tab, NIL for other chars."
  (is-true  (cl-tmux/config::%whitespace-p #\Space) "space is whitespace")
  (is-true  (cl-tmux/config::%whitespace-p #\Tab)   "tab is whitespace")
  (is-false (cl-tmux/config::%whitespace-p #\a)     "letter is not whitespace")
  (is-false (cl-tmux/config::%whitespace-p #\Newline) "newline is not whitespace"))

;;; %parse-bind-key-args edge cases

(test parse-bind-key-args-empty-returns-nil
  "%parse-bind-key-args with empty args list returns NIL."
  (is (null (cl-tmux/config::%parse-bind-key-args '()))
      "empty args must return NIL"))

(test parse-bind-key-args-T-flag-missing-table-returns-nil
  "%parse-bind-key-args with -T and no table name returns NIL."
  (is (null (cl-tmux/config::%parse-bind-key-args '("-T")))
      "-T with no following table name must return NIL"))

(test parse-bind-key-args-unknown-command-returns-nil
  "%parse-bind-key-args with an unknown command returns NIL."
  (is (null (cl-tmux/config::%parse-bind-key-args '("z" "unknown-bogus-command")))
      "unknown command must cause %parse-bind-key-args to return NIL"))

(test parse-bind-key-args-n-and-r-flags-combined
  "%parse-bind-key-args with -n -r binds in root table with repeatable."
  (multiple-value-bind (table key kw repeatable)
      (cl-tmux/config::%parse-bind-key-args '("-n" "-r" "z" "new-window"))
    (is (string= "root" table) "table must be root for -n flag")
    (is (char= #\z key)        "key must be #\\z")
    (is (eq :new-window kw)    "command must be :new-window")
    (is-true repeatable        "repeatable must be T for -r flag")))

;;; %parse-unbind-key-args edge cases

(test parse-unbind-key-args-empty-returns-nil-nil
  "%parse-unbind-key-args with empty args returns (values nil nil)."
  (multiple-value-bind (table key)
      (cl-tmux/config::%parse-unbind-key-args '())
    (is (null table) "table must be NIL for empty args")
    (is (null key)   "key must be NIL for empty args")))

(test parse-unbind-key-args-extra-arg-returns-nil-nil
  "%parse-unbind-key-args with extra trailing arg returns (values nil nil)."
  (multiple-value-bind (table key)
      (cl-tmux/config::%parse-unbind-key-args '("z" "extra"))
    (is (null table) "table must be NIL when extra arg present")
    (is (null key)   "key must be NIL when extra arg present")))

(test parse-unbind-key-args-T-flag-missing-table-returns-nil
  "%parse-unbind-key-args with -T and no table name returns (values nil nil)."
  (multiple-value-bind (table key)
      (cl-tmux/config::%parse-unbind-key-args '("-T"))
    (is (null table) "table must be NIL when -T has no table name")
    (is (null key)   "key must be NIL when -T has no table name")))

;;; Backslash-escape edge case: backslash at end of string

(test config-tokenizer-backslash-at-end-of-string
  "%config-tokens: backslash at the end of string does not signal an error."
  (finishes (cl-tmux/config::%config-tokens "token\\")))

;;; load-config-from-string: all-blank and all-comment input

(test load-from-string-blank-input-returns-zero
  "load-config-from-string with only blanks and comments returns 0."
  (with-isolated-config
    (let ((applied (load-config-from-string
                    (format nil "# comment~%~%   ~%# another~%"))))
      (is (= 0 applied)
          "blank/comment-only input must apply 0 directives, got ~A" applied))))

;;; bind -r -n combined (order-independent flags)

(test bind-key-r-then-n-flag
  "bind -r -n binds in root table and marks repeatable (flag order insensitive)."
  (with-isolated-key-tables
    (apply-config-directive '("bind" "-r" "-n" "G" "new-window"))
    (let ((entry (cl-tmux/config:key-table-lookup "root" #\G)))
      (is (not (null entry))
          "bind -r -n must add a binding to the root table")
      (is (eq :new-window (cl-tmux/config:key-table-command entry))
          "root table binding must be :new-window")
      (is (cl-tmux/config:key-table-repeatable-p entry)
          "binding must be repeatable when -r flag is present"))))

