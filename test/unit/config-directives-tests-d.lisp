(in-package #:cl-tmux/test)

;;;; set-g-status-off, bind-key-n, load-config patterns, bare-arg-cmds, %elif, line-continuation, source-file-glob, if-shell — part IV

(in-suite config-directives-suite)

;;; ── set -g status off side-effect ────────────────────────────────────────────

(test apply-set-directive-status-table
  "'set -g status off' → *status-height* 0; 'on' → 1."
  (dolist (c '(("off" 0 "'set -g status off' sets *status-height* to 0")
               ("on"  1 "'set -g status on' sets *status-height* to 1")))
    (destructuring-bind (value expected desc) c
      (let ((orig cl-tmux/config:*status-height*))
        (unwind-protect
            (progn
              (setf cl-tmux/config:*status-height* 0)
              (cl-tmux/config:apply-config-directive (list "set" "-g" "status" value))
              (is (= expected cl-tmux/config:*status-height*) "~A" desc))
          (setf cl-tmux/config:*status-height* orig))))))

;;; ── bind -n with argument-bearing command ────────────────────────────────────

(test bind-key-n-split-window-with-c-flag
  "'bind -n C-\\ split-window -c /tmp' binds the control character ^\\ (byte 28)
   and stores the full command token list.  C-<key> tokens now resolve to the
   control CHARACTER the event loop sees (the old string-key form could never
   fire), so the binding is looked up by (code-char 28), not the string \"C-\\\"."
  (with-isolated-key-tables
    (cl-tmux/config:apply-config-directive
     '("bind" "-n" "C-\\" "split-window" "-c" "/tmp"))
    ;; C-\ → (logand (char-code #\\) #x1f) = 28 (FS).  The binding is keyed by
    ;; that control character so it matches the byte a real Ctrl-\ keypress sends.
    (let ((entry (cl-tmux/config:key-table-lookup "root" (code-char 28))))
      (is (not (null entry)) "C-\\ must be bound (as control char 28) in root table")
      (let ((cmd (cl-tmux/config:key-table-command entry)))
        (is (consp cmd) "command for multi-token bind must be a token list")
        (is (string= "split-window" (first cmd)) "first token must be split-window")
        (is (member "-c" cmd :test #'string=) "token list must include -c flag")
        (is (member "/tmp" cmd :test #'string=) "token list must include /tmp")))))

;;; ── Semicolon-separated multi-command bindings ───────────────────────────────

(test bind-key-semicolon-sequence-stored-as-sequence
  "'bind r source-file x \; display y' stores a :sequence command list."
  (with-isolated-key-tables
    (cl-tmux/config:apply-config-directive
     '("bind" "r" "source-file" "/tmp/x" ";" "display-message" "Reloaded"))
    (let ((entry (cl-tmux/config:key-table-lookup "prefix" #\r)))
      (is (not (null entry)) "#\\r must be bound in prefix table")
      (let ((cmd (cl-tmux/config:key-table-command entry)))
        (is (consp cmd) "command must be a cons")
        (is (eq :sequence (car cmd)) "first element must be :sequence")
        (is (= 2 (length (cdr cmd))) ":sequence must have 2 sub-command lists")
        (is (string= "source-file" (first (first (cdr cmd))))
            "first sub-command must start with source-file")
        (is (string= "display-message" (first (second (cdr cmd))))
            "second sub-command must start with display-message")))))

;;; ── Common .tmux.conf patterns ───────────────────────────────────────────────

(test load-config-common-patterns-no-error
  "Common .tmux.conf patterns load without error."
  (with-isolated-config
    (let ((common-config
           "set -g prefix C-a
set -g mouse on
set -g base-index 1
setw -g pane-base-index 1
set -g default-terminal \"screen-256color\"
set -g escape-time 0
set -g history-limit 50000
set -g renumber-windows on
set -g mode-keys vi
bind r source-file /dev/null \; display-message \"Reloaded\"
bind-key -T copy-mode-vi v send-keys -X begin-selection
bind-key -T copy-mode-vi y send-keys -X copy-selection-and-cancel
bind '\"' split-window -c #{pane_current_path}
bind % split-window -h -c #{pane_current_path}
bind c new-window -c #{pane_current_path}
unbind-all
bind-key r source-file /dev/null"))
      (is (zerop (multiple-value-bind (result)
                     (ignore-errors (cl-tmux/config:load-config-from-string common-config))
                   (declare (ignore result))
                   0))
          "common .tmux.conf patterns must load without signaling conditions"))))

(test load-config-bind-T-copy-mode-vi-stores-correctly
  "bind -T copy-mode-vi v send-keys -X begin-selection stores in copy-mode-vi table."
  (with-isolated-config
    (cl-tmux/config:load-config-from-string
     "bind-key -T copy-mode-vi v send-keys -X begin-selection")
    (let ((entry (cl-tmux/config:key-table-lookup "copy-mode-vi" #\v)))
      (is (not (null entry)) "copy-mode-vi must have 'v' binding after load")
      (let ((cmd (cl-tmux/config:key-table-command entry)))
        (is (consp cmd) "command must be a token list")
        (is (string= "send-keys" (first cmd)) "first token must be send-keys")))))

(test load-config-set-g-escape-time-stores-as-server-option
  "'set -s escape-time 0' stores in server options."
  (with-isolated-config
    (cl-tmux/config:load-config-from-string "set -s escape-time 0")
    (is (eql 0 (cl-tmux/options:get-server-option "escape-time"))
        "escape-time must be 0 after 'set -s escape-time 0'")))

;;; ── Bare arg-command shorthand rejection in bind (single-token path) ────────
;;;
;;; `bind X <command> args` (multi-token) already works — it is stored
;;; unvalidated and resolved at dispatch via *arg-command-table*.  A BARE
;;; `bind X <shorthand>` (single token) goes through %command-keyword, and we no
;;; longer keep backward-compat shorthands for the named-buffer family.

(test config-bind-rejects-named-buffer-shorthand-single-tokens
  "Bare named-buffer shorthands are rejected in the single-token bind path."
  (dolist (abbrev '("deleteb" "loadb" "pasteb" "saveb" "setb" "showb"))
    (with-isolated-config
      (is (= 0 (cl-tmux/config:load-config-from-string
                (format nil "bind X ~A" abbrev)))
          "bind X ~A must be rejected (0 applied); the shorthand is unsupported"
          abbrev))))

(test config-bind-rejects-unknown-single-token-still
  "The abbreviation aliases do not weaken typo rejection: an unknown single-token
   command is still refused."
  (with-isolated-config
    (is (= 0 (cl-tmux/config:load-config-from-string "bind X totally-bogus-cmd"))
        "an unknown bare command must still be rejected (0 applied)")))

;;; ── %elif chains (4-state cond stack) ────────────────────────────────────────
;;;
;;; A plain skip flag mishandles %elif after a matched branch.  These exercise the
;;; :active/:seeking/:taken/:dead state machine.  The identity evaluator makes the
;;; condition "1" truthy and "0" falsy.

(test config-if-elif-else-state-machine-table
  "The %if/%elif/%else state machine picks exactly the first matching branch."
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
      (with-isolated-config
        (let ((cl-tmux/config:*config-condition-evaluator* (lambda (x) x)))
          (is (= expected (cl-tmux/config:load-config-from-string
                           (format nil config-str)))
              "~A" desc))))))

;;; ── Line continuation (trailing backslash) ───────────────────────────────────

(test line-continues-p-counts-trailing-backslashes
  "%line-continues-p is T for an odd number of trailing backslashes."
  (is-true  (cl-tmux/config::%line-continues-p "foo \\"))
  (is-false (cl-tmux/config::%line-continues-p "foo \\\\"))
  (is-true  (cl-tmux/config::%line-continues-p "foo \\\\\\"))
  (is-false (cl-tmux/config::%line-continues-p "foo"))
  (is-false (cl-tmux/config::%line-continues-p "")))

(test config-line-continuation-joins-directive
  "A line ending in a single backslash continues onto the next line, forming one
   directive."
  (with-isolated-config
    (is (= 1 (cl-tmux/config:load-config-from-string
              (format nil "bind a \\~%new-window")))
        "the continued line is applied as a single bind directive")))

(test config-no-continuation-two-lines-two-directives
  "Without a trailing backslash, two lines remain two separate directives."
  (with-isolated-config
    (is (= 2 (cl-tmux/config:load-config-from-string
              (format nil "bind a new-window~%bind b next-window")))
        "two independent binds apply as two directives")))

;;; ── source-file: -q flags, glob patterns, multiple paths ─────────────────────

(test glob-expand-passthrough-non-glob
  "%glob-expand returns a non-glob path unchanged as a one-element list."
  (is (equal '("/etc/foo.conf") (cl-tmux/config::%glob-expand "/etc/foo.conf"))))

(test glob-expand-empty-for-no-matches
  "%glob-expand returns NIL for a glob that matches nothing."
  (is (null (cl-tmux/config::%glob-expand "/nonexistent-cl-tmux-xyz-dir/*.conf"))))

(test source-files-skips-flags-and-tolerates-missing
  "source-files skips -q/-n/-v flags and ignores missing files (returns T, no error)."
  (is (eq t (cl-tmux/config:source-files '("-q" "/no/such/cl-tmux-file.conf")))))

(test source-files-glob-expands-and-loads-matching-files
  "source-file with a glob loads every matching file; %glob-expand finds them."
  (let ((dir (uiop:ensure-directory-pathname
              (merge-pathnames "cl-tmux-glob-test/" (uiop:temporary-directory)))))
    (ensure-directories-exist dir)
    (unwind-protect
         (progn
           (with-open-file (f (merge-pathnames "a.conf" dir)
                              :direction :output :if-exists :supersede)
             (write-line "# cl-tmux glob test (no global mutation)" f))
           (with-open-file (f (merge-pathnames "b.conf" dir)
                              :direction :output :if-exists :supersede)
             (write-line "# cl-tmux glob test (no global mutation)" f))
           (let ((matches (cl-tmux/config::%glob-expand
                           (namestring (merge-pathnames "*.conf" dir)))))
             (is (= 2 (length matches)) "glob matched both .conf files (got ~A)" matches)
             (is (eq t (cl-tmux/config:source-files
                        (list (namestring (merge-pathnames "*.conf" dir)))))
                 "source-files loads the globbed files without error")))
      (ignore-errors (uiop:delete-directory-tree dir :validate t)))))

;;; ── # comment handling (inline, quote- and format-aware) ─────────────────────

(test strip-config-comment-respects-quotes-and-formats
  "%strip-config-comment removes a comment only outside quotes and not for #{/##."
  (dolist (c '(("set -g foo bar # note"    "set -g foo bar"           "inline comment stripped")
               ("# full line"              ""                         "full-line comment → empty")
               ("set x \"#{session_name}\"" "set x \"#{session_name}\"" "# inside double quotes kept")
               ("set x '# literal'"       "set x '# literal'"        "# inside single quotes kept")
               ("set x #{session_name}"   "set x #{session_name}"    "unquoted #{ format kept")
               ("set x ##y"              "set x ##y"                 "## escaped-literal not a comment")
               ("set x bar"              "set x bar"                 "no comment → unchanged")))
    (destructuring-bind (input expected desc) c
      (is (string= expected (cl-tmux/config::%strip-config-comment input)) "~A" desc))))

(test config-inline-comment-not-in-value
  "An inline # comment is stripped before the directive is applied."
  (with-isolated-options ()
    (cl-tmux/config:load-config-from-string "set -g status-left foo # a comment")
    (is (string= "foo" (cl-tmux/options:get-option "status-left"))
        "the comment must not leak into the option value")))

(test config-quoted-hash-preserved-in-value
  "A #{...} inside a quoted value survives (not treated as a comment)."
  (with-isolated-options ()
    (cl-tmux/config:load-config-from-string "set -g status-left \"#{session_name}\"")
    (is (string= "#{session_name}" (cl-tmux/options:get-option "status-left"))
        "the #{...} format must survive as the option value")))

(test config-brace-block-inner-comment-preserved
  "An inline # comment on an inner line of a multi-line brace block does not
   truncate the block — both commands still bind."
  (with-isolated-config
    (load-config-from-string
     (format nil "bind r {~%new-window  # make a window~%next-window~%}"))
    (let* ((entry (cl-tmux/config:key-table-lookup "prefix" #\r))
           (cmd   (cl-tmux/config:key-table-command entry)))
      (is (consp cmd) "command must be a list")
      (is (eq :sequence (first cmd)) "must be a :sequence")
      (is (= 2 (length (rest cmd)))
          "the sequence must hold both commands despite the inner comment (got ~S)"
          cmd))))

;;; ── if-shell config directive: -F format conditions + flag stripping ─────────

(test if-shell-directive-branch-selection-table
  "%apply-if-shell-directive selects THEN or ELSE based on condition, flags stripped."
  (dolist (c '((("true" "set -g status-left THEN" "set -g status-left ELSE")
                "THEN" "exit-0 shell runs THEN")
               (("-F" "1" "set -g status-left THEN" "set -g status-left ELSE")
                "THEN" "-F truthy runs THEN")
               (("-F" "0" "set -g status-left THEN" "set -g status-left ELSE")
                "ELSE" "-F zero runs ELSE")
               (("-F" "#{==:a,a}" "set -g status-left THEN" "set -g status-left ELSE")
                "THEN" "format #{==:a,a} → 1 runs THEN")
               (("-b" "true" "set -g status-left THEN" "set -g status-left ELSE")
                "THEN" "-b stripped, shell condition runs THEN")))
    (destructuring-bind (args expected desc) c
      (with-isolated-config
        (cl-tmux/config::%apply-if-shell-directive "if-shell" args)
        (is (string= expected (cl-tmux/options:get-option "status-left"))
            "~A" desc)))))

(test if-shell-format-true-p-rules
  "%if-shell-format-true-p: empty and \"0\" are false; other text is true."
  (is-true  (cl-tmux/config::%if-shell-format-true-p "1"))
  (is-true  (cl-tmux/config::%if-shell-format-true-p "yes"))
  (is-false (cl-tmux/config::%if-shell-format-true-p "0"))
  (is-false (cl-tmux/config::%if-shell-format-true-p "")))

;;; ── if-shell brace-block then/else bodies (tmux 3.x { ... } syntax) ──────────

(test if-shell-directive-F-brace-table
  "if-shell -F with brace-block bodies: truthy runs THEN block, falsy runs ELSE block."
  (dolist (c '(("1" "THEN" "truthy -F runs THEN brace block")
               ("0" "ELSE" "falsy -F runs ELSE brace block")))
    (destructuring-bind (cond expected desc) c
      (with-isolated-config
        (cl-tmux/config::%apply-if-shell-directive
         "if-shell" (list "-F" cond
                          "{" "set" "-g" "status-left" "THEN" "}"
                          "{" "set" "-g" "status-left" "ELSE" "}"))
        (is (string= expected (cl-tmux/options:get-option "status-left"))
            "~A" desc)))))

(test if-shell-directive-F-brace-multi-command
  "A brace THEN block with multiple ;-separated commands runs ALL of them."
  (with-isolated-config
    (cl-tmux/config::%apply-if-shell-directive
     "if-shell" '("-F" "1" "{" "set" "-g" "status-left" "A" ";"
                  "set" "-g" "status-right" "B" "}"))
    (is (string= "A" (cl-tmux/options:get-option "status-left")) "first cmd ran")
    (is (string= "B" (cl-tmux/options:get-option "status-right")) "second cmd ran")))

(test if-shell-directive-F-brace-no-else-is-safe
  "if-shell -F 0 with only a THEN brace block (no else) runs nothing — no error."
  (with-isolated-config
    (cl-tmux/config::%apply-if-shell-directive
     "if-shell" '("-F" "0" "{" "set" "-g" "status-left" "THEN" "}"))
    (is (not (string= "THEN" (cl-tmux/options:get-option "status-left")))
        "false condition + no else block → the THEN block does NOT run")))

(test take-brace-or-command-splits-block-and-bare
  "%take-brace-or-command: a { ... ; ... } block → inner command lists + rest; a
   bare token → one re-tokenised command + rest."
  (multiple-value-bind (cmds rest)
      (cl-tmux/config::%take-brace-or-command '("{" "a" "b" ";" "c" "d" "}" "tail"))
    (is (equal '(("a" "b") ("c" "d")) cmds) "two ;-separated commands extracted")
    (is (equal '("tail") rest) "rest is the tokens after the closing brace"))
  (multiple-value-bind (cmds rest)
      (cl-tmux/config::%take-brace-or-command '("set -g x Y" "more"))
    (is (equal '(("set" "-g" "x" "Y")) cmds) "bare token re-tokenised into one command")
    (is (equal '("more") rest) "rest is the remaining tokens")))

;;; ── run-shell -C : run a tmux command, not a shell command ───────────────────

(test run-shell-C-table
  "run-shell -C executes the tmux command and reports handled."
  (dolist (c '(("run-shell" "status-left"  "FOO" "run-shell -C")
               ("run-shell" "status-right" "BAR" "run-shell -C")))
    (destructuring-bind (verb option value desc) c
      (with-isolated-config
        (let ((handled (cl-tmux/config::%apply-run-shell-directive
                        verb (list "-C" (format nil "set -g ~A ~A" option value)))))
          (is (eq t handled) "~A: must report handled" desc)
          (is (string= value (cl-tmux/options:get-option option))
              "~A: must set option" desc))))))

;;; ── Mid-token '#' is a literal, not a comment ────────────────────────────────
;;;
;;; tmux's lexer only begins a comment when '#' is the first character of a token
;;; (line start or just after whitespace).  A '#' in the middle of an unquoted
;;; word — most commonly a hex colour like bg=#0000ff — is a literal character.
;;; Previously %strip-config-comment truncated such values to "bg=".

(test strip-config-comment-keeps-mid-token-hash
  "A '#' in the middle of an unquoted token is literal; only a token-start '#'
   begins a comment."
  (dolist (c '(("set -g status-style bg=#0000ff"                 "set -g status-style bg=#0000ff" "mid-token hex colour kept verbatim")
               ("set -g @c fg=#ff0000"                           "set -g @c fg=#ff0000"           "mid-token hex in a user (@) option kept")
               ("set -g status-style bg=#0000ff # trailing note" "set -g status-style bg=#0000ff" "mid-token hex kept AND a real trailing comment still stripped")
               ("set -g foo #bar"                                "set -g foo"                     "a '#' at a token start (after whitespace) still begins a comment")))
    (destructuring-bind (input expected desc) c
      (is (string= expected (cl-tmux/config::%strip-config-comment input)) "~A" desc))))

(test apply-config-line-keeps-hash-colour-end-to-end
  "End-to-end: an unquoted hex colour survives apply-config-line into the option
   value (it used to be truncated by comment stripping)."
  (with-isolated-options ("status-style" "")
    (cl-tmux/config::apply-config-line "set -g status-style bg=#1e1e1e")
    (is (string= "bg=#1e1e1e" (cl-tmux/options:get-option "status-style"))
        "hex colour reaches the option store intact")))

;;; ── set -ag on style options inserts a ',' separator ─────────────────────────
;;;
;;; tmux marks *-style options OPTIONS_TABLE_IS_STYLE; `set -a` appends to them
;;; with a comma so incremental theming (bg first, fg later) composes.  Plain
;;; string options keep separator-less concatenation.

(test apply-set-directive-append-style-comma
  "'set -ag <name>-style v' comma-joins; plain string options still concat."
  ;; Style option with a non-empty current value: comma-joined.
  (with-isolated-options ("status-style" "bg=red")
    (is (eq t (apply-config-directive '("set" "-ag" "status-style" "fg=blue")))
        "set -ag must return T")
    (is (string= "bg=red,fg=blue" (cl-tmux/options:get-option "status-style"))
        "style append must insert a ',' separator"))
  ;; Style option appended onto an empty value: no leading comma.
  (with-isolated-options ("mode-style" "")
    (apply-config-directive '("set" "-ag" "mode-style" "fg=green"))
    (is (string= "fg=green" (cl-tmux/options:get-option "mode-style"))
        "appending onto an empty style value adds no stray comma"))
  ;; Non-style string option: still plain (no comma).
  (with-isolated-options ("status-left" "A")
    (apply-config-directive '("set" "-ag" "status-left" "B"))
    (is (string= "AB" (cl-tmux/options:get-option "status-left"))
        "non-style option keeps separator-less concatenation")))
