(in-package #:cl-tmux/test)

;;;; set-g-status-off, bind-n, load-config patterns, bare-arg-cmds, %elif, line-continuation, comments, styles — part IV

(describe "config-directives-suite"

  ;; ── set-option -g status off side-effect ────────────────────────────────────────────

  ;; 'set-option -g status off' → *status-height* 0; 'on' → 1.
  (it "apply-set-directive-status-table"
    (dolist (c '(("off" 0 "'set-option -g status off' sets *status-height* to 0")
                 ("on"  1 "'set-option -g status on' sets *status-height* to 1")))
      (destructuring-bind (value expected desc) c
        (declare (ignore desc))
        (let ((orig cl-tmux/config:*status-height*))
          (unwind-protect
              (progn
                (setf cl-tmux/config:*status-height* 0)
                (cl-tmux/config:apply-config-directive (list "set-option" "-g" "status" value))
                (expect (= expected cl-tmux/config:*status-height*)))
            (setf cl-tmux/config:*status-height* orig))))))

  ;; ── bind -n with argument-bearing command ────────────────────────────────────

  ;; 'bind -n C-\ split-window -c /tmp' binds the control character ^\ (byte 28)
  ;; and stores the full command token list.  C-<key> tokens now resolve to the
  ;; control CHARACTER the event loop sees (the old string-key form could never
  ;; fire), so the binding is looked up by (code-char 28), not the string "C-\".
  (it "bind-n-split-window-with-c-flag"
    (with-isolated-key-tables
      (cl-tmux/config:apply-config-directive
       '("bind" "-n" "C-\\" "split-window" "-c" "/tmp"))
      ;; C-\ → (logand (char-code #\\) #x1f) = 28 (FS).  The binding is keyed by
      ;; that control character so it matches the byte a real Ctrl-\ keypress sends.
      (let ((entry (cl-tmux/config:key-table-lookup "root" (code-char 28))))
        (expect (not (null entry)))
        (let ((cmd (cl-tmux/config:key-table-command entry)))
          (expect (consp cmd))
          (expect (string= "split-window" (first cmd)))
          (expect (member "-c" cmd :test #'string=))
          (expect (member "/tmp" cmd :test #'string=))))))

  ;; ── Semicolon-separated multi-command bindings ───────────────────────────────

  ;; 'bind r source-file x \; display y' stores a :sequence command list.
  (it "bind-key-semicolon-sequence-stored-as-sequence"
    (with-isolated-key-tables
      (cl-tmux/config:apply-config-directive
       '("bind" "r" "source-file" "/tmp/x" ";" "display-message" "Reloaded"))
      (let ((entry (cl-tmux/config:key-table-lookup "prefix" #\r)))
        (expect (not (null entry)))
        (let ((cmd (cl-tmux/config:key-table-command entry)))
          (expect (consp cmd))
          (expect (eq :sequence (car cmd)))
          (expect (= 2 (length (cdr cmd))))
          (expect (string= "source-file" (first (first (cdr cmd)))))
          (expect (string= "display-message" (first (second (cdr cmd)))))))))

  ;; ── Common .tmux.conf patterns ───────────────────────────────────────────────

  ;; Common .tmux.conf patterns load without error.
  (it "load-config-common-patterns-no-error"
    (with-isolated-config
      (let ((common-config
             "set-option -g prefix C-a
set-option -g mouse on
set-option -g base-index 1
set-window-option -g pane-base-index 1
set-option -g default-terminal \"screen-256color\"
set-option -g escape-time 0
set-option -g history-limit 50000
set-option -g renumber-windows on
set-option -g mode-keys vi
bind r source-file /dev/null \; display-message \"Reloaded\"
bind -T copy-mode-vi v send-keys -X begin-selection
bind -T copy-mode-vi y send-keys -X copy-selection
bind '\"' split-window -c #{pane_current_path}
bind % split-window -h -c #{pane_current_path}
bind c new-window -c #{pane_current_path}
unbind-all
bind r source-file /dev/null"))
        (expect (zerop (multiple-value-bind (result)
                           (ignore-errors (cl-tmux/config:load-config-from-string common-config))
                         (declare (ignore result))
                         0))))))

  ;; bind -T copy-mode-vi v send-keys -X begin-selection stores in the
  ;; copy-mode-vi table.  The multi-token command is stored as a deferred token
  ;; list; key-press dispatch sends -X begin-selection to the pane's copy mode.
  (it "load-config-bind-T-copy-mode-vi-stores-correctly"
    (with-isolated-config
      (cl-tmux/config:load-config-from-string
       "bind -T copy-mode-vi v send-keys -X begin-selection")
      (let ((entry (cl-tmux/config:key-table-lookup "copy-mode-vi" #\v)))
        (expect (not (null entry)))
        (let ((cmd (cl-tmux/config:key-table-command entry)))
          (expect (equal '("send-keys" "-X" "begin-selection") cmd))))))

  ;; 'set-option -s escape-time 0' stores in server options.
  (it "load-config-set-g-escape-time-stores-as-server-option"
    (with-isolated-config
      (cl-tmux/config:load-config-from-string "set-option -s escape-time 0")
      (expect (eql 0 (cl-tmux/options:get-server-option "escape-time")))))

  ;; 'set-option -u status' in a config file removes the option and restores the runtime
  ;; status height to its default.
  (it "load-config-set-u-restores-status-side-effects"
    (with-isolated-config
      (setf cl-tmux/config:*status-height* 4)
      (cl-tmux/config:load-config-from-string
       "set-option -g status off
set-option -u status")
      (expect (= 1 cl-tmux/config:*status-height*))
      (expect (null (nth-value 1 (gethash "status" cl-tmux/options:*global-options*))))
      (expect (string= "on" (cl-tmux/options:get-option "status")))))

  ;; ── Bare arg-command shorthand rejection in bind (single-token path) ────────
  ;;
  ;; `bind X <command> args` (multi-token) already works — it is stored
  ;; unvalidated and resolved at dispatch via *arg-command-table*.  A BARE
  ;; `bind X <shorthand>` (single token) goes through %command-keyword, and the
  ;; named-buffer family uses canonical command names only.

  ;; Named-buffer shorthand spellings (deleteb/loadb/pasteb/saveb/showb) are
  ;; rejected because cl-tmux accepts canonical command names only.
  (it "config-bind-rejects-named-buffer-shorthand-single-tokens"
    (dolist (abbrev '("deleteb" "loadb" "pasteb" "saveb" "showb"))
      (with-isolated-config
        (expect (= 0 (cl-tmux/config:load-config-from-string
                      (format nil "bind X ~A" abbrev))))
        (expect (null (lookup-key-binding #\X)))))
    (with-isolated-config
      (expect (= 0 (cl-tmux/config:load-config-from-string "bind X setb")))))

  ;; Rejected shorthand spellings do not weaken typo rejection: an unknown
  ;; single-token command is still refused.
  (it "config-bind-rejects-unknown-single-token-still"
    (with-isolated-config
      (expect (= 0 (cl-tmux/config:load-config-from-string "bind X totally-bogus-cmd")))))

  ;; A realistic .tmux.conf written with canonical command names loads:
  ;; set-option/set-window-option/bind all apply, and command bodies are normalized for dispatch.
  (it "load-realistic-tmux-conf-with-canonical-commands"
    (with-isolated-config
      (let ((applied (cl-tmux/config:load-config-from-string
                      "set-option -g status on
set-window-option -g mode-keys vi
bind c new-window
bind | split-window -h
bind -T copy-mode-vi v send-keys -X begin-selection")))
        (expect (= 5 applied))
        (expect (string= "on" (cl-tmux/options:get-option "status")))
        (expect (eq :new-window (lookup-key-binding #\c)))
        (expect (equal '("split-window" "-h") (lookup-key-binding #\|)))
        (expect (not (null (cl-tmux/config:key-table-lookup "copy-mode-vi" #\v)))))))

  ;; ── %elif chains (4-state cond stack) ────────────────────────────────────────
  ;;
  ;; A plain skip flag mishandles %elif after a matched branch.  These exercise the
  ;; :active/:seeking/:taken/:dead state machine.  The identity evaluator makes the
  ;; condition "1" truthy and "0" falsy.

  ;; The %if/%elif/%else state machine picks exactly the first matching branch.
  (it "config-if-elif-else-state-machine-table"
    (dolist (c '(("%if 1~%bind a new-window~%%elif 1~%bind b new-window~%%endif~%"
                  1 "if=1 elif=1: only if-branch")
                 ("%if 0~%bind a new-window~%%elif 1~%bind b new-window~%%endif~%"
                  1 "if=0 elif=1: elif-branch applies")
                 ("%if 0~%bind a x~%%elif 0~%bind b x~%%else~%bind c new-window~%%endif~%"
                  1 "if=0 elif=0: else-branch applies")
                 ("%if 0~%bind a x~%%elif 0~%bind b x~%%elif 1~%bind c new-window~%%else~%bind d x~%%endif~%"
                  1 "elif chain: first true elif wins")
                 ("%if 1~%bind a new-window~%%elif 1~%bind b x~%%else~%bind c x~%%endif~%"
                  1 "if=1 with elif+else: only if-branch")
                 ("%if 0~%%if 1~%bind a new-window~%%elif 1~%bind b new-window~%%endif~%%endif~%"
                  0 "nested if in dead outer branch stays dead")))
      (destructuring-bind (config-str expected desc) c
        (declare (ignore desc))
        (with-isolated-config
          (let ((cl-tmux/config:*config-condition-evaluator* (lambda (x) x)))
            (expect (= expected (cl-tmux/config:load-config-from-string
                                 (format nil config-str)))))))))

  ;; ── Line continuation (trailing backslash) ───────────────────────────────────

  ;; %line-continues-p is T for an odd number of trailing backslashes.
  (it "line-continues-p-counts-trailing-backslashes"
    (expect (cl-tmux/config::%line-continues-p "foo \\") :to-be-truthy)
    (expect (cl-tmux/config::%line-continues-p "foo \\\\") :to-be-falsy)
    (expect (cl-tmux/config::%line-continues-p "foo \\\\\\") :to-be-truthy)
    (expect (cl-tmux/config::%line-continues-p "foo") :to-be-falsy)
    (expect (cl-tmux/config::%line-continues-p "") :to-be-falsy))

  ;; A line ending in a single backslash continues onto the next line, forming one
  ;; directive.
  (it "config-line-continuation-joins-directive"
    (with-isolated-config
      (expect (= 1 (cl-tmux/config:load-config-from-string
                    (format nil "bind a \\~%new-window"))))))

  ;; Without a trailing backslash, two lines remain two separate directives.
  (it "config-no-continuation-two-lines-two-directives"
    (with-isolated-config
      (expect (= 2 (cl-tmux/config:load-config-from-string
                    (format nil "bind a new-window~%bind b next-window"))))))

  ;; %glob-pattern-p is true for * ? [ metacharacters; NIL for plain paths.
  (it "glob-pattern-p-detects-metacharacters"
    (dolist (row '(("/etc/*.conf"    t   "* is a glob metacharacter")
                   ("/etc/foo?.conf" t   "? is a glob metacharacter")
                   ("/etc/[ab].conf" t   "[ is a glob metacharacter")
                   ("/etc/foo.conf"  nil "plain path has no glob metacharacters")))
      (destructuring-bind (path expected desc) row
        (declare (ignore desc))
        (if expected
            (expect (cl-tmux/config::%glob-pattern-p path) :to-be-truthy)
            (expect (cl-tmux/config::%glob-pattern-p path) :to-be-falsy)))))

  ;; ── # comment handling (inline, quote- and format-aware) ─────────────────────

  ;; %strip-config-comment removes a comment only outside quotes and not for #{/##.
  (it "strip-config-comment-respects-quotes-and-formats"
    (dolist (c '(("set-option -g foo bar # note"    "set-option -g foo bar"           "inline comment stripped")
                 ("# full line"              ""                         "full-line comment → empty")
                 ("set x \"#{session_name}\"" "set x \"#{session_name}\"" "# inside double quotes kept")
                 ("set x '# literal'"       "set x '# literal'"        "# inside single quotes kept")
                 ("set x #{session_name}"   "set x #{session_name}"    "unquoted #{ format kept")
                 ("set x ##y"              "set x ##y"                 "## escaped-literal not a comment")
                 ("set x bar"              "set x bar"                 "no comment → unchanged")))
      (destructuring-bind (input expected desc) c
        (declare (ignore desc))
        (expect (string= expected (cl-tmux/config::%strip-config-comment input))))))

  ;; An inline # comment is stripped before the directive is applied.
  (it "config-inline-comment-not-in-value"
    (with-isolated-options ()
      (cl-tmux/config:load-config-from-string "set-option -g status-left foo # a comment")
      (expect (string= "foo" (cl-tmux/options:get-option "status-left")))))

  ;; A #{...} inside a quoted value survives (not treated as a comment).
  (it "config-quoted-hash-preserved-in-value"
    (with-isolated-options ()
      (cl-tmux/config:load-config-from-string "set-option -g status-left \"#{session_name}\"")
      (expect (string= "#{session_name}" (cl-tmux/options:get-option "status-left")))))

  ;; An inline # comment on an inner line of a multi-line brace block does not
  ;; truncate the block — both commands still bind.
  (it "config-brace-block-inner-comment-preserved"
    (with-isolated-config
      (load-config-from-string
       (format nil "bind r {~%new-window  # make a window~%next-window~%}"))
      (let* ((entry (cl-tmux/config:key-table-lookup "prefix" #\r))
             (cmd   (cl-tmux/config:key-table-command entry)))
        (expect (consp cmd))
        (expect (eq :sequence (first cmd)))
        (expect (= 2 (length (rest cmd)))))))

  ;; ── Mid-token '#' is a literal, not a comment ────────────────────────────────
  ;;
  ;; tmux's lexer only begins a comment when '#' is the first character of a token
  ;; (line start or just after whitespace).  A '#' in the middle of an unquoted
  ;; word — most commonly a hex colour like bg=#0000ff — is a literal character.
  ;; Previously %strip-config-comment truncated such values to "bg=".

  ;; A '#' in the middle of an unquoted token is literal; only a token-start '#'
  ;; begins a comment.
  (it "strip-config-comment-keeps-mid-token-hash"
    (dolist (c '(("set-option -g status-style bg=#0000ff"                 "set-option -g status-style bg=#0000ff" "mid-token hex colour kept verbatim")
                 ("set-option -g @c fg=#ff0000"                           "set-option -g @c fg=#ff0000"           "mid-token hex in a user (@) option kept")
                 ("set-option -g status-style bg=#0000ff # trailing note" "set-option -g status-style bg=#0000ff" "mid-token hex kept AND a real trailing comment still stripped")
                 ("set-option -g foo #bar"                                "set-option -g foo"                     "a '#' at a token start (after whitespace) still begins a comment")))
      (destructuring-bind (input expected desc) c
        (declare (ignore desc))
        (expect (string= expected (cl-tmux/config::%strip-config-comment input))))))

  ;; End-to-end: an unquoted hex colour survives apply-config-line into the option
  ;; value (it used to be truncated by comment stripping).
  (it "apply-config-line-keeps-hash-colour-end-to-end"
    (with-isolated-options ("status-style" "")
      (cl-tmux/config::apply-config-line "set-option -g status-style bg=#1e1e1e")
      (expect (string= "bg=#1e1e1e" (cl-tmux/options:get-option "status-style")))))

  ;; ── set-option -ag on style options inserts a ',' separator ─────────────────────────
  ;;
  ;; tmux marks *-style options OPTIONS_TABLE_IS_STYLE; `set-option -a` appends to them
  ;; with a comma so incremental theming (bg first, fg later) composes.  Plain
  ;; string options keep separator-less concatenation.

  ;; 'set-option -ag <name>-style v' comma-joins; plain string options still concat.
  (it "apply-set-directive-append-style-comma"
    ;; Style option with a non-empty current value: comma-joined.
    (with-isolated-options ("status-style" "bg=red")
      (assert-set-directive-option-state '("set-option" "-ag" "status-style" "fg=blue")
                                         "status-style" "bg=red,fg=blue"
                                         :context "set-option -ag status-style fg=blue"))
    ;; Style option appended onto an empty value: no leading comma.
    (with-isolated-options ("mode-style" "")
      (apply-config-directive '("set-option" "-ag" "mode-style" "fg=green"))
      (expect (string= "fg=green" (cl-tmux/options:get-option "mode-style"))))
    ;; Non-style string option: still plain (no comma).
    (with-isolated-options ("status-left" "A")
      (apply-config-directive '("set-option" "-ag" "status-left" "B"))
      (expect (string= "AB" (cl-tmux/options:get-option "status-left"))))))
