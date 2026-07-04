(in-package #:cl-tmux/test)

;;;; config directive tests — bind, unbind, notes, brace blocks, and sequences

(in-suite config-directives-suite)

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
        "no -N flag -> NIL note")))

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
   stored as a :sequence -- the inner newlines act as command separators."
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
               "set-option -g status off ; set-option -g status-style bg=red"))
        "a multi-segment line returns T when at least one segment applied")
    (is (string= "off" (cl-tmux/options:get-option "status"))
        "the first segment (set-option -g status off) must be applied")
    (is (string= "bg=red" (cl-tmux/options:get-option "status-style"))
        "the second segment (set-option -g status-style bg=red) must be applied")))

(test apply-config-line-ignores-empty-semicolon-segments
  "apply-config-line discards empty segments produced by doubled/trailing `;'.
   The separators are whitespace-delimited `;' tokens (an adjacent `;;' is one
   token the tokenizer does not split -- a known limitation)."
  (with-isolated-options ("status" "on" "status-style" "")
    (is (eq t (cl-tmux/config::apply-config-line
               "set-option -g status off ; ; set-option -g status-style bg=red ;"))
        "doubled and trailing `;' produce empty segments that are skipped")
    (is (string= "off" (cl-tmux/options:get-option "status")))
    (is (string= "bg=red" (cl-tmux/options:get-option "status-style")))))

(test apply-config-line-bind-escaped-semicolon-stays-a-sequence
  "Regression: `bind r source-file ... \\; display ...' keeps its `\\;'-joined body
   as one bind sequence.  apply-config-line must NOT pre-split a bind line -- bind
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
