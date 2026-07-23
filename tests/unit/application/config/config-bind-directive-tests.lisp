(in-package #:cl-tmux/test)

;;;; config directive tests — bind, unbind, notes, brace blocks, and sequences

(describe "config-directives-suite"

  ;;; bind key to a command LINE (arg-taking key bindings)

  ;; 'bind X display-message hello' binds X to the command token list, applies (T),
  ;; and list-keys shows the reconstructed command line.
  (it "bind-key-to-command-line-stores-token-list"
    (with-isolated-key-tables
      (assert-config-directive-applied '("bind" "X" "display-message" "hello")
                                       "multi-token bind")
      (expect (equal '("display-message" "hello") (lookup-key-binding #\X)))
      (expect (search "display-message hello" (cl-tmux/config:describe-key-bindings)))))

  ;; 'bind z new-window' still binds the key to the :new-window keyword (existing
  ;; single-command behaviour is preserved).
  (it "bind-key-single-keyword-still-binds"
    (with-isolated-key-tables
      (assert-config-directive-applied '("bind" "z" "new-window")
                                       "single known-command bind")
      (expect (eq :new-window (lookup-key-binding #\z)))))

  ;; 'bind z totally-bogus-xyz' (one unknown command word) is still rejected (NIL),
  ;; and the unknown command is not stored (z keeps its default keyword binding).
  (it "bind-key-single-unknown-command-rejected"
    (with-isolated-key-tables
      (assert-config-directive-rejected '("bind" "z" "totally-bogus-xyz")
                                        "an unknown single command")
      ;; z is bound to a built-in (zoom) by default; the rejected bind must not have
      ;; replaced it with the bogus command token-list.
      (expect (not (equal '("totally-bogus-xyz") (lookup-key-binding #\z))))))

  ;;; unbind directive

  ;; unbind removes an existing binding and returns T.
  (it "apply-directive-unbind-removes-binding"
    (with-isolated-config
      (expect (eq :new-window (lookup-key-binding #\c)))
      (assert-config-directive-applied '("unbind" "c")
                                       "a valid unbind directive")
      (expect (null (lookup-key-binding #\c)))))

  ;; unbind -a removes every binding in the prefix table (the real tmux whole-table
  ;; unbind, previously mis-parsed as a key literally named "-a").
  (it "apply-directive-unbind-a-clears-prefix-table"
    (with-isolated-config
      (expect (not (null (lookup-key-binding #\c))))
      (assert-config-directive-applied '("unbind" "-a")
                                       "unbind -a")
      (expect (null (lookup-key-binding #\c)))))

  ;; unbind -a -T <table> clears only that named table.
  (it "apply-directive-unbind-a-T-clears-named-table"
    (with-isolated-config
      (apply-config-directive '("bind" "-T" "mytable" "x" "new-window"))
      (expect (not (null (cl-tmux/config:key-table-lookup "mytable" #\x))))
      (assert-config-directive-applied '("unbind" "-a" "-T" "mytable")
                                       "unbind -a -T mytable")
      (expect (null (cl-tmux/config:key-table-lookup "mytable" #\x)))))

  ;; unbind -a -n clears the root (no-prefix) key table.
  (it "apply-directive-unbind-a-n-clears-root-table"
    (with-isolated-config
      (apply-config-directive '("bind" "-n" "F1" "new-window"))
      (expect (not (null (cl-tmux/config:key-table-lookup "root" "F1"))))
      (assert-config-directive-applied '("unbind" "-a" "-n")
                                       "unbind -a -n")
      (expect (null (cl-tmux/config:key-table-lookup "root" "F1")))))

  ;;; multi-character key tokens

  ;; A multi-character key token (M-z) is stored as the string itself.
  (it "bind-multichar-key-token"
    (with-isolated-config
      (let ((applied (load-config-from-string "bind M-z next-window")))
        (expect (= 1 applied))
        (expect (eq :next-window (lookup-key-binding "M-z")))
        (expect (eq :zoom-toggle (lookup-key-binding #\z))))))

  ;;; bind -N "note" (tmux 3.1+ key-binding description)

  ;; bind -N "note" x next-window skips the -N note and binds x to :next-window
  ;; (not the note string as a bogus command).
  (it "bind-with-note-flag-binds-correctly"
    (with-isolated-config
      (let ((applied (load-config-from-string "bind -N \"Go to next window\" x next-window")))
        (expect (= 1 applied))
        (expect (eq :next-window (lookup-key-binding #\x))))))

  ;; bind -N works alongside other flags in any order: -n (root) and -r (repeat).
  (it "bind-note-flag-combined-with-n-and-r"
    (with-isolated-config
      (expect (= 1 (load-config-from-string "bind -n -N \"root note\" y next-window")))
      (expect (eq :next-window (cl-tmux/config:key-table-command
                                (cl-tmux/config:key-table-lookup "root" #\y))))
      (expect (= 1 (load-config-from-string "bind -r -N \"repeat note\" z next-window")))
      (let ((entry (cl-tmux/config:key-table-lookup "prefix" #\z)))
        (expect (eq :next-window (cl-tmux/config:key-table-command entry)))
        (expect (cl-tmux/config:key-table-repeatable-p entry)))))

  ;; bind -N "note" key { cmd1 ; cmd2 } combines a note with a brace block.
  (it "bind-note-flag-multi-command-block"
    (with-isolated-config
      (load-config-from-string "bind -N \"reload\" R { split-window ; next-window }")
      (let* ((entry (cl-tmux/config:key-table-lookup "prefix" #\R))
             (cmd   (cl-tmux/config:key-table-command entry)))
        (expect (and (consp cmd) (eq :sequence (first cmd))))
        (expect (= 2 (length (rest cmd)))))))

  ;; bind -N stores the note on the binding; binding without -N has NIL note.
  (it "bind-note-flag-note-storage"
    (with-isolated-config
      (load-config-from-string "bind -N \"Go to next window\" x next-window")
      (let ((entry (cl-tmux/config:key-table-lookup "prefix" #\x)))
        (expect (eq :next-window (cl-tmux/config:key-table-command entry)))
        (expect (string= "Go to next window" (cl-tmux/config:key-table-note entry)))))
    (with-isolated-config
      (load-config-from-string "bind y next-window")
      (expect (null (cl-tmux/config:key-table-note
                     (cl-tmux/config:key-table-lookup "prefix" #\y))))))

  ;; list-keys / describe-key-bindings renders the -N note in -N "..." form for a
  ;; noted binding, and emits no -N marker for un-noted bindings.
  (it "describe-key-bindings-surfaces-note"
    (with-isolated-config
      (load-config-from-string "bind -N \"reload config\" R source-file ~/.tmux.conf")
      (let ((output (cl-tmux/config:describe-key-bindings-for-table "prefix")))
        (expect (search "reload config" output))
        (expect (search "-N \"reload config\"" output)))))

  ;;; brace-block command syntax (tmux 3.x): bind r { cmd1 ; cmd2 }

  ;; %strip-brace-block returns the inner tokens of a { ... } block and leaves a
  ;; non-block token list unchanged.
  (it "strip-brace-block-unwraps-braces"
    (expect (equal '("a" "b")
                   (cl-tmux/config::%strip-brace-block '("{" "a" "b" "}"))))
    (expect (equal '("a" "b")
                   (cl-tmux/config::%strip-brace-block '("a" "b"))))
    (expect (null (cl-tmux/config::%strip-brace-block '("{" "}")))))

  ;; %line-brace-delta nets '{' against '}' and ignores braces inside quotes.
  (it "line-brace-delta-counts-unquoted-braces"
    (dolist (c '((1  "bind r {"                    "open brace is +1")
                 (-1 "}"                           "close brace is -1")
                 (0  "bind r { next-window }"      "balanced block nets 0")
                 (0  "display \"a { b }\""         "braces inside a double-quoted string are ignored")))
      (destructuring-bind (expected input desc) c
        (declare (ignore desc))
        (expect (= expected (cl-tmux/config::%line-brace-delta input))))))

  ;; bind r { next-window } binds the single inner command as a keyword.
  (it "bind-single-line-brace-block-single-command"
    (with-isolated-config
      (let ((applied (load-config-from-string "bind r { next-window }")))
        (expect (= 1 applied))
        (expect (eq :next-window (lookup-key-binding #\r))))))

  ;; bind r { split-window ; next-window } stores a :sequence of both commands.
  (it "bind-single-line-brace-block-multi-command"
    (with-isolated-config
      (load-config-from-string "bind r { split-window ; next-window }")
      (let* ((entry (cl-tmux/config:key-table-lookup "prefix" #\r))
             (cmd   (cl-tmux/config:key-table-command entry)))
        (expect (consp cmd))
        (expect (eq :sequence (first cmd)))
        (expect (= 2 (length (rest cmd)))))))

  ;; bind r { } is not a valid binding (no command) and must apply nothing.
  (it "bind-empty-brace-block-rejected"
    (with-isolated-config
      (let ((applied (load-config-from-string "bind r { }")))
        (expect (= 0 applied)))))

  ;; A multi-line { ... } block (tmux 3.x style) is joined into one directive and
  ;; stored as a :sequence -- the inner newlines act as command separators.
  (it "bind-multiline-brace-block-joined-and-applied"
    (with-isolated-config
      (let ((applied (load-config-from-string
                      (format nil "bind r {~%  split-window~%  next-window~%}~%"))))
        (expect (= 1 applied))
        (let* ((entry (cl-tmux/config:key-table-lookup "prefix" #\r))
               (cmd   (cl-tmux/config:key-table-command entry)))
          (expect (and (consp cmd) (eq :sequence (first cmd))))
          (expect (= 2 (length (rest cmd))))))))

  ;;; top-level `;' command-sequence separator on ordinary config lines (tmux parity)

  ;; apply-config-line splits a top-level unescaped `;' into separate command
  ;; sequences and applies each segment in order (tmux command-sequence parity).
  (it "apply-config-line-splits-top-level-semicolons"
    (with-isolated-options ("status" "on" "status-style" "")
      (expect (eq t (cl-tmux/config::apply-config-line
                     "set-option -g status off ; set-option -g status-style bg=red")))
      (expect (string= "off" (cl-tmux/options:get-option "status")))
      (expect (string= "bg=red" (cl-tmux/options:get-option "status-style")))))

  ;; apply-config-line discards empty segments produced by doubled/trailing `;'.
  ;; The separators are whitespace-delimited `;' tokens (an adjacent `;;' is one
  ;; token the tokenizer does not split -- a known limitation).
  (it "apply-config-line-ignores-empty-semicolon-segments"
    (with-isolated-options ("status" "on" "status-style" "")
      (expect (eq t (cl-tmux/config::apply-config-line
                     "set-option -g status off ; ; set-option -g status-style bg=red ;")))
      (expect (string= "off" (cl-tmux/options:get-option "status")))
      (expect (string= "bg=red" (cl-tmux/options:get-option "status-style")))))

  ;; Regression: `bind r source-file ... \; display ...' keeps its `\;'-joined body
  ;; as one bind sequence.  apply-config-line must NOT pre-split a bind line -- bind
  ;; owns its `;' tokens and invokes %split-on-semicolons itself.
  (it "apply-config-line-bind-escaped-semicolon-stays-a-sequence"
    (with-isolated-config
      (expect (eq t (cl-tmux/config::apply-config-line
                     "bind r source-file ~/.tmux.conf \\; display-message Reloaded")))
      (let* ((entry (cl-tmux/config:key-table-lookup "prefix" #\r))
             (cmd   (cl-tmux/config:key-table-command entry)))
        (expect (and (consp cmd) (eq :sequence (first cmd))))
        (expect (= 2 (length (rest cmd)))))))

  ;; After a multi-line brace block the reader resumes cleanly: a directive on the
  ;; line after the closing brace is still applied.
  (it "multiline-brace-block-does-not-corrupt-following-directive"
    (with-isolated-config
      (let ((applied (load-config-from-string
                      (format nil "bind r {~%  next-window~%}~%bind x last-window~%"))))
        (expect (= 2 applied))
        (expect (eq :last-window (lookup-key-binding #\x)))))))
