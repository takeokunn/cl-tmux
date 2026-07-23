(in-package #:cl-tmux/test)

;;;; config directive tests — if-shell

(describe "config-directives-suite"

  ;;; ── if-shell config directive: -F format conditions + flag stripping ─────────

  ;; %apply-if-shell-directive selects THEN or ELSE based on condition, flags stripped.
  (it "if-shell-directive-branch-selection-table"
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
        (declare (ignore desc))
        (with-isolated-config
          (cl-tmux/config::%apply-if-shell-directive "if-shell" args)
          (expect (string= expected (cl-tmux/options:get-option "status-left")))))))

  ;; %if-shell-format-true-p: empty and "0" are false; other text is true.
  (it "if-shell-format-true-p-rules"
    (dolist (row '(("1"   t   "non-zero string is true")
                   ("yes" t   "non-empty non-zero string is true")
                   ("0"   nil "\"0\" is false")
                   (""    nil "empty string is false")))
      (destructuring-bind (input expected desc) row
        (declare (ignore desc))
        (if expected
            (expect (cl-tmux/config::%if-shell-format-true-p input) :to-be-truthy)
            (expect (cl-tmux/config::%if-shell-format-true-p input) :to-be-falsy)))))

  ;;; ── if-shell brace-block then/else bodies (tmux 3.x { ... } syntax) ──────────

  ;; if-shell -F with brace-block bodies: truthy runs THEN block, falsy runs ELSE block.
  (it "if-shell-directive-F-brace-table"
    (dolist (c '(("1" "THEN" "truthy -F runs THEN brace block")
                 ("0" "ELSE" "falsy -F runs ELSE brace block")))
      (destructuring-bind (cond expected desc) c
        (declare (ignore desc))
        (with-isolated-config
          (cl-tmux/config::%apply-if-shell-directive
           "if-shell" (list "-F" cond
                            "{" "set-option" "-g" "status-left" "THEN" "}"
                            "{" "set-option" "-g" "status-left" "ELSE" "}"))
          (expect (string= expected (cl-tmux/options:get-option "status-left")))))))

  ;; A brace THEN block with multiple ;-separated commands runs ALL of them.
  (it "if-shell-directive-F-brace-multi-command"
    (with-isolated-config
      (cl-tmux/config::%apply-if-shell-directive
       "if-shell" '("-F" "1" "{" "set-option" "-g" "status-left" "A" ";"
                    "set-option" "-g" "status-right" "B" "}"))
      (expect (string= "A" (cl-tmux/options:get-option "status-left")))
      (expect (string= "B" (cl-tmux/options:get-option "status-right")))))

  ;; if-shell -F 0 with only a THEN brace block (no else) runs nothing — no error.
  (it "if-shell-directive-F-brace-no-else-is-safe"
    (with-isolated-config
      (cl-tmux/config::%apply-if-shell-directive
       "if-shell" '("-F" "0" "{" "set-option" "-g" "status-left" "THEN" "}"))
      (expect (not (string= "THEN" (cl-tmux/options:get-option "status-left"))))))

  ;; %take-brace-or-command: a { ... ; ... } block → inner command lists + rest; a
  ;; bare token → one re-tokenised command + rest.
  (it "take-brace-or-command-splits-block-and-bare"
    (multiple-value-bind (cmds rest)
        (cl-tmux/config::%take-brace-or-command '("{" "a" "b" ";" "c" "d" "}" "tail"))
      (expect (equal '(("a" "b") ("c" "d")) cmds))
      (expect (equal '("tail") rest)))
    (multiple-value-bind (cmds rest)
        (cl-tmux/config::%take-brace-or-command '("set-option -g x Y" "more"))
      (expect (equal '(("set-option" "-g" "x" "Y")) cmds))
      (expect (equal '("more") rest)))))
