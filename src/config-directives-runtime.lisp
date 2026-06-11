(in-package #:cl-tmux/config)

;;; -- Runtime directive handlers: set-environment, if-shell, run-shell, source ----
;;;
;;; %apply-set-environment-directive, %apply-if-shell-directive,
;;; %apply-command-alias-directive, %apply-run-shell-directive,
;;; %glob-expand, source-files, %apply-source-file-directive.

;;; ── set-environment flag handling (set-environment -r VAR) ──────────────────
;;;
;;; The fixed-arity table handles only `set-environment VAR VALUE` (2 args).
;;; The `-r` form (unset) passes 2 args: "-r" and VAR, which the fixed-arity
;;; table rejects because arg[0] ≠ a variable name.  This handler intercepts
;;; the unset form before the fixed-arity table gets a chance to reject it.

(defun %apply-set-environment-directive (cmd args)
  "Handle 'set-environment [-g] [-u|-r] VAR [VALUE]' config directives.
   -u unsets the variable (tmux's unset flag); -r is accepted as a synonym for
   unset (cl-tmux has no separate update-environment list to remove from).
   -g is accepted and ignored (global scope is the only scope supported).
   Returns T when handled, NIL otherwise."
  (when (member cmd '("set-environment" "setenv") :test #'string=)
    (let* (;; Consume optional flags: -g (global, default), -u/-r (unset).
           (remove-p   nil)
           (remaining  args))
      (loop while (and remaining
                       (let ((tok (first remaining)))
                         (and (>= (length tok) 2) (char= (char tok 0) #\-))))
            do (let ((tok (pop remaining)))
                 (when (or (find #\u tok) (find #\r tok)) (setf remove-p t))))
      (let ((var-name  (first remaining))
            (var-value (second remaining)))
        (when var-name
          (if remove-p
              ;; Unset: lazy lookup so SB-POSIX need not be loaded before cl-tmux.
              (let ((fn (%config-posix-fn "UNSETENV")))
                (when fn (ignore-errors (funcall fn var-name))))
              ;; Set: value required for non-remove form.
              (when var-value
                (%config-setenv var-name var-value)))
          t)))))

;;; ── if-shell config-time conditional ────────────────────────────────────────
;;;
;;; tmux's `if-shell` can appear as a standalone directive in .tmux.conf:
;;;   if-shell 'uname | grep -q Darwin' 'set -g prefix C-a' 'set -g prefix C-b'
;;; It runs the condition via /bin/sh, then applies THEN-CMD or ELSE-CMD.
;;; This is different from the run-time :if-shell dispatch (which is interactive).

(defun %if-shell-format-true-p (condition)
  "tmux -F truthiness for if-shell: expand CONDITION as a format and treat an
   empty result or \"0\" as false, anything else as true.  A NIL context is used
   (config-time has no pane); global-scoped formats still resolve."
  (let ((result (ignore-errors (cl-tmux/format:expand-format condition nil))))
    (and result
         (not (string= result ""))
         (not (string= result "0")))))

(defun %take-brace-or-command (tokens)
  "Consume one command UNIT from the front of TOKENS (an if-shell then/else body).
   A unit is either a brace block { ... } — its inner tokens are split into command
   token-lists via %split-on-semicolons (depth-tracked for nesting; a missing close
   brace is tolerated, end-of-list closes) — or a single bare token, a complete
   quoted command string re-tokenised into one command.  Returns
   (values COMMAND-TOKEN-LISTS REST), each list ready for apply-config-directive."
  (cond
    ((null tokens) (values nil nil))
    ((string= (first tokens) "{")
     (let ((depth 1) (inner '()) (rest (rest tokens)))
       (loop for tok = (pop rest)
             while tok do
               (cond ((string= tok "{") (incf depth) (push tok inner))
                     ((string= tok "}") (decf depth)
                      (if (zerop depth) (return) (push tok inner)))
                     (t (push tok inner))))
       (values (%split-on-semicolons (nreverse inner)) rest)))
    (t
     (values (list (%config-tokens (first tokens))) (rest tokens)))))

(defun %apply-if-shell-directive (cmd args)
  "Handle 'if-shell [-bF] [-t target] CONDITION THEN-CMD [ELSE-CMD]' directives.
   Without -F, CONDITION is a shell command (exit 0 = true).  With -F, CONDITION
   is a format string (true unless it expands to empty or \"0\").  -b (background)
   and -t target are accepted and ignored at config time.  Returns T when CMD is
   if-shell/if (handled), NIL otherwise."
  (when (member cmd '("if-shell" "if") :test #'string=)
    (let ((format-mode nil)
          (remaining   args))
      ;; Consume leading flag tokens (clusters like -bF are allowed; -t takes the
      ;; next token).  Stop at the first non-flag token — the CONDITION.
      (loop while (and remaining
                       (let ((tok (first remaining)))
                         (and (> (length tok) 1) (char= (char tok 0) #\-))))
            do (let ((tok (pop remaining)))
                 (cond
                   ((string= tok "-t") (when remaining (pop remaining)))
                   (t (when (find #\F tok) (setf format-mode t))))))
      (when (>= (length remaining) 2)
        (let* ((condition (first remaining))
               (truthy-p  (if format-mode
                              (%if-shell-format-true-p condition)
                              ;; Run the condition shell command; treat any error
                              ;; (including a timeout signal from UIOP) as non-zero
                              ;; (falsy).  :timeout 30 guards against a hanging command
                              ;; blocking config loading indefinitely.
                              (handler-case
                                  (eql 0 (nth-value 2
                                           (uiop:run-program
                                            (list "/bin/sh" "-c" condition)
                                            :ignore-error-status t :timeout 30)))
                                (error () nil)))))
          ;; THEN/ELSE bodies are each either a brace block { ... } (tmux 3.x) or a
          ;; single quoted command token.  %take-brace-or-command consumes one unit
          ;; and returns its command(s) as ready-to-apply token-lists.
          (multiple-value-bind (then-cmds after-then)
              (%take-brace-or-command (rest remaining))
            (let ((else-cmds (and after-then
                                  (nth-value 0 (%take-brace-or-command after-then)))))
              (dolist (line (if truthy-p then-cmds else-cmds))
                (apply-config-directive line))))))
      t)))

;;; ── command-alias array syntax handling ─────────────────────────────────────
;;;
;;; tmux stores command aliases as an array option in .tmux.conf:
;;;   set -s command-alias[0] e='new-window -n'
;;; The option name carries the index (`command-alias[0]`).  After %strip-set-
;;; flags the positionals look like: ("command-alias[0]" "e=new-window -n").
;;; This function detects that pattern and routes it to the alias registry.

(defun %apply-command-alias-directive (name value)
  "If NAME looks like 'command-alias[N]', parse VALUE as 'alias=expansion'
   and register the alias.  Returns T when handled, NIL otherwise."
  (when (and (>= (length name) 13)
             (string= (subseq name 0 13) "command-alias"))
    (let ((eq-pos (position #\= value)))
      (when eq-pos
        (cl-tmux/options:register-command-alias
         (subseq value 0 eq-pos)
         (subseq value (1+ eq-pos)))
        t))))

;;; ── run-shell / run flag handling (run-shell -b/-t/-d/-C 'cmd') ──────────────
;;;
;;; The fixed-arity table only matches the bare 1-arg form `run-shell 'cmd'`, so
;;; the common real-world `run-shell -b 'cmd'` / `run -b '~/.tmux/...'` forms
;;; (with leading flags) silently failed.  This handler strips leading flags
;;; before the fixed-arity table and runs whatever shell command remains.

(defun %apply-run-shell-directive (cmd args)
  "Handle 'run-shell [-b] [-C] [-t target] [-d delay] shell-command' directives
   (alias 'run').  Consumes leading flags:
     -b           run in background (boolean; we run synchronously regardless)
     -C           run a tmux command instead of a shell command (boolean)
     -t <target>  target pane (takes the next token as its value)
     -d <delay>   delay (takes the next token as its value)
   Unknown leading -X flags: a single bare flag token is skipped to stay
   tolerant.  Stops at the first non-flag token; that token plus any remaining
   tokens (joined by spaces) form the shell command.
   Returns T when CMD is run-shell/run (handled), NIL otherwise."
  (when (member cmd '("run-shell" "run") :test #'string=)
    (let ((tmux-command-p nil)
          (remaining      args))
      ;; Consume leading flag tokens.
      (loop while (and remaining
                       (let ((tok (first remaining)))
                         (and (>= (length tok) 1) (char= (char tok 0) #\-))))
            do (let ((tok (pop remaining)))
                 (cond
                   ((string= tok "-C") (setf tmux-command-p t))
                   ((string= tok "-b")) ; background flag, no argument
                   ((or (string= tok "-t") (string= tok "-d"))
                    ;; These flags take the next token as their value.
                    (when remaining (pop remaining)))
                   ;; Unknown bare -X flag: skip the single flag token only.
                   (t nil))))
      ;; Remaining tokens (joined) form the shell command.
      (let ((command (when remaining
                       (format nil "~{~A~^ ~}" remaining))))
        (cond
          ;; No command after flags: a flag-only invocation is a no-op but handled.
          ((null command) t)
          ;; -C: the argument is a tmux command, not a shell command — run it
          ;; through the config dispatcher (same path if-shell uses for its
          ;; then/else commands).  e.g. `run-shell -C 'display-message hi'`.
          (tmux-command-p
           (ignore-errors (apply-config-directive (%config-tokens command)))
           t)
          ;; Shell command: run it the same way the fixed-arity entries do.
          (t
           (let ((expanded (%expand-leading-tilde command)))
             ;; :timeout 30 guards against a hanging run-shell blocking config loading.
             ;; handler-case makes a timeout signal (UIOP:SUBPROCESS-ERROR or similar)
             ;; explicit: the command is abandoned and loading continues rather than
             ;; silently treating the timeout as a non-zero exit.
             (handler-case
                 (uiop:run-program (list "/bin/sh" "-c" expanded)
                                   :ignore-error-status t :timeout 30)
               (error () nil)))
           t))))))

(defun %glob-expand (path)
  "Expand a shell glob PATH (one containing * ? or [) to the sorted namestrings of
   the matching regular files.  A path with no glob metacharacters is returned
   unchanged as a one-element list, so a plain (possibly missing) path still
   reaches load-config-file."
  (if (find-if (lambda (c) (member c '(#\* #\? #\[) :test #'char=)) path)
      (sort (loop for p in (ignore-errors (directory (pathname path)))
                  unless (ignore-errors (uiop:directory-pathname-p p))
                    collect (namestring p))
            #'string<)
      (list path)))

(defun %parse-source-file-flags (args)
  "Parse the leading -Fnqv flags of source-file.  Returns
   (values PARSE-ONLY-P QUIET-P VERBOSE-P FORMAT-P POSITIONALS).  Clustered flags
   (e.g. -qn) are supported; scanning stops at the first non-flag token (a path)."
  (let ((parse-only nil) (quiet nil) (verbose nil) (format-p nil) (rest args))
    (loop while (and rest
                     (let ((tok (first rest)))
                       (and (> (length tok) 1) (char= (char tok 0) #\-))))
          do (let ((tok (pop rest)))
               (when (find #\n tok) (setf parse-only t))
               (when (find #\q tok) (setf quiet t))
               (when (find #\v tok) (setf verbose t))
               (when (find #\F tok) (setf format-p t))))
    (values parse-only quiet verbose format-p rest)))

(defun %parse-config-file-only (file)
  "Parse-only loader for `source-file -n`: read FILE and tokenise each logical line
   (honouring # comments) WITHOUT applying any directive, so NO command executes —
   tmux's CMD_PARSE_PARSEONLY syntax check.  Errors are swallowed (lenient, like
   load-config-file)."
  (ignore-errors
    (with-open-file (in file :if-does-not-exist nil)
      (when in
        (loop for line = (read-line in nil nil)
              while line
              do (%config-tokens (%strip-config-comment line)))))))

(defun source-files (args)
  "Implement `source-file [-Fnqv] path...`: for each non-flag PATH, optionally
   expand it as a format string (-F), then expand a leading ~ and shell globs
   (* ? []), and load every matching config file.  With -n (parse-only), each file
   is read and tokenised but NO command runs (tmux's CMD_PARSE_PARSEONLY syntax
   check).  Errors (missing file, parse failure) are always swallowed, so cl-tmux is
   effectively -q whether or not -q is given; -v is accepted (no client sink to echo
   to).  Returns T."
  (multiple-value-bind (parse-only quiet verbose format-p positionals)
      (%parse-source-file-flags args)
    (declare (ignore quiet verbose))
    (dolist (raw positionals)
      (let ((path (if format-p
                      (or (ignore-errors (cl-tmux/format:expand-format raw nil)) raw)
                      raw)))
        (when (plusp (length path))
          (dolist (file (%glob-expand (%expand-leading-tilde path)))
            (if parse-only
                (%parse-config-file-only file)
                (ignore-errors (load-config-file file))))))))
  t)

(defun %apply-source-file-directive (cmd args)
  "Intercept source-file / source: -q/-n/-v flags, glob patterns, and multiple
   paths (the fixed-arity directive table only handled a single bare path).
   Returns T when CMD is a source verb, else NIL."
  (when (member cmd '("source-file" "source") :test #'string=)
    (source-files args)))

