(in-package #:cl-tmux/test)

;;;; config directive tests — if-shell

(in-suite config-directives-suite)

;;; ── if-shell config directive: -F format conditions + flag stripping ─────────

(test if-shell-directive-branch-selection-table
  "%apply-if-shell-directive selects THEN or ELSE based on condition, flags stripped."
  (dolist (c '((("true" "set-option -g status-left THEN" "set-option -g status-left ELSE")
                "THEN" "exit-0 shell runs THEN")
               (("-F" "1" "set-option -g status-left THEN" "set-option -g status-left ELSE")
                "THEN" "-F truthy runs THEN")
               (("-F" "0" "set-option -g status-left THEN" "set-option -g status-left ELSE")
                "ELSE" "-F zero runs ELSE")
               (("-F" "#{==:a,a}" "set-option -g status-left THEN" "set-option -g status-left ELSE")
                "THEN" "format #{==:a,a} → 1 runs THEN")
               (("-b" "true" "set-option -g status-left THEN" "set-option -g status-left ELSE")
                "THEN" "-b stripped, shell condition runs THEN")))
    (destructuring-bind (args expected desc) c
      (with-isolated-config
        (cl-tmux/config::%apply-if-shell-directive "if-shell" args)
        (is (string= expected (cl-tmux/options:get-option "status-left"))
            "~A" desc)))))

(test if-shell-format-true-p-rules
  "%if-shell-format-true-p: empty and \"0\" are false; other text is true."
  (dolist (row '(("1"   t   "non-zero string is true")
                 ("yes" t   "non-empty non-zero string is true")
                 ("0"   nil "\"0\" is false")
                 (""    nil "empty string is false")))
    (destructuring-bind (input expected desc) row
      (if expected
          (is-true  (cl-tmux/config::%if-shell-format-true-p input) desc)
          (is-false (cl-tmux/config::%if-shell-format-true-p input) desc)))))

;;; ── if-shell brace-block then/else bodies (tmux 3.x { ... } syntax) ──────────

(test if-shell-directive-F-brace-table
  "if-shell -F with brace-block bodies: truthy runs THEN block, falsy runs ELSE block."
  (dolist (c '(("1" "THEN" "truthy -F runs THEN brace block")
               ("0" "ELSE" "falsy -F runs ELSE brace block")))
    (destructuring-bind (cond expected desc) c
      (with-isolated-config
        (cl-tmux/config::%apply-if-shell-directive
         "if-shell" (list "-F" cond
                          "{" "set-option" "-g" "status-left" "THEN" "}"
                          "{" "set-option" "-g" "status-left" "ELSE" "}"))
        (is (string= expected (cl-tmux/options:get-option "status-left"))
            "~A" desc)))))

(test if-shell-directive-F-brace-multi-command
  "A brace THEN block with multiple ;-separated commands runs ALL of them."
  (with-isolated-config
    (cl-tmux/config::%apply-if-shell-directive
     "if-shell" '("-F" "1" "{" "set-option" "-g" "status-left" "A" ";"
                  "set-option" "-g" "status-right" "B" "}"))
    (is (string= "A" (cl-tmux/options:get-option "status-left")) "first cmd ran")
    (is (string= "B" (cl-tmux/options:get-option "status-right")) "second cmd ran")))

(test if-shell-directive-F-brace-no-else-is-safe
  "if-shell -F 0 with only a THEN brace block (no else) runs nothing — no error."
  (with-isolated-config
    (cl-tmux/config::%apply-if-shell-directive
     "if-shell" '("-F" "0" "{" "set-option" "-g" "status-left" "THEN" "}"))
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
      (cl-tmux/config::%take-brace-or-command '("set-option -g x Y" "more"))
    (is (equal '(("set-option" "-g" "x" "Y")) cmds) "bare token re-tokenised into one command")
    (is (equal '("more") rest) "rest is the remaining tokens")))
