(in-package #:cl-tmux/config)

;;; -- Runtime directive handlers: set-environment, if-shell, run-shell, source ----
;;;
;;; %apply-set-environment-directive, %apply-if-shell-directive,
;;; %apply-run-shell-directive,
;;; %glob-expand, source-files, %apply-source-file-directive.

;;; ── set-environment flag handling (set-environment -r VAR) ──────────────────
;;;
;;; The fixed-arity table handles only `set-environment VAR VALUE` (2 args).
;;; The `-r` form (unset) passes 2 args: "-r" and VAR, which the fixed-arity
;;; table rejects because arg[0] ≠ a variable name.  This handler intercepts
;;; the unset form before the fixed-arity table gets a chance to reject it.

(defconstant +config-shell-command-timeout+ 30
  "Seconds to allow config-time shell directives to run.")

(defun %run-config-shell-command (command &key combine-stderr directory)
  "Run COMMAND through /bin/sh while loading config, with a bounded lifetime."
  (uiop:run-program (list "/bin/sh" "-c" command)
                    :output :string
                    :error-output (when combine-stderr :output)
                    :ignore-error-status t
                    :timeout +config-shell-command-timeout+
                    :directory directory))

(defun %run-config-shell-command-safe (command &key combine-stderr directory delay)
  "Run COMMAND via %run-config-shell-command, treating any timeout/error signal
   (SERIOUS-CONDITION, wider than ERROR so it also covers UIOP timeout signals
   such as uiop:subprocess-error) as an abandoned, falsy result instead of
   letting it propagate — so config loading cannot hang or abort indefinitely.
   Returns the same (values stdout stderr exit-code) as %run-config-shell-command
   on success, or NIL on a signalled condition."
  (handler-case
      (progn
        (when (and delay (plusp delay))
          (sleep delay))
        (%run-config-shell-command command
                                   :combine-stderr combine-stderr
                                   :directory directory))
    (serious-condition () nil)))

(defun %parse-set-environment-flags (args)
  "Parse the [-g] [-u|-r] [-t target] flags of a set-environment directive.
   Returns (values remaining-tokens remove-p global-p target-p target-name)."
  (let ((remove-p nil) (global-p nil) (target-p nil) (target-name nil))
    (let ((remaining
            (%consuming-flags (args tok rest)
              ((member tok '("-u" "-r") :test #'string=)
               (setf remove-p t))
              ((string= tok "-g")
               (setf global-p t))
              ((string= tok "-t")
               (setf target-p t
                     target-name (first rest))
               (when rest (setf rest (cdr rest)))))))
      (values remaining remove-p global-p target-p target-name))))

(defun %apply-set-environment-to-session (target-name remove-p var-name var-value)
  "Apply a `set-environment -t TARGET-NAME` directive to the named session's
   environment overlay.  Returns T when a matching session and VAR-NAME exist."
  (let ((session (and target-name (cl-tmux::server-find-session target-name))))
    (when (and session var-name)
      (if remove-p
          (cl-tmux/model:session-unset-environment session var-name)
          (when var-value
            (cl-tmux/model:session-set-environment session var-name var-value)))
      t)))

(defun %apply-set-environment-to-process (remove-p var-name var-value)
  "Apply a global `set-environment` directive to the real process environment.
   Returns T when VAR-NAME is present."
  (when var-name
    (if remove-p
        ;; Unset: lazy lookup so SB-POSIX need not be loaded before cl-tmux.
        (let ((fn (%config-posix-fn "UNSETENV")))
          (when fn (ignore-errors (funcall fn var-name))))
        ;; Set: value required for non-remove form.
        (when var-value
          (%config-setenv var-name var-value)))
    t))

(defun %apply-set-environment-directive (cmd args)
  "Handle 'set-environment [-g] [-u|-r] [-t target] VAR [VALUE]' config directives.
   -u unsets the variable (tmux's unset flag); -r is accepted as a synonym for
   unset (cl-tmux has no separate update-environment list to remove from).
   -g mutates the process environment, while -t TARGET mutates the named session
   overlay.  -g and -t are mutually exclusive.  Returns T when handled, NIL
   otherwise."
  (when (string= cmd "set-environment")
    (multiple-value-bind (remaining remove-p global-p target-p target-name)
        (%parse-set-environment-flags args)
      (let ((var-name (first remaining)) (var-value (second remaining)))
        (cond
          ((and global-p target-p) nil)
          (target-p (%apply-set-environment-to-session
                     target-name remove-p var-name var-value))
          (t (%apply-set-environment-to-process remove-p var-name var-value)))))))

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
    (and result (not (member result '("" "0") :test #'string=)))))

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
   if-shell (handled), NIL otherwise."
  (when (member cmd '("if-shell" "if") :test #'string=)
    (let ((format-mode nil)
          (remaining   args))
      ;; Consume leading flag tokens (clusters like -bF are allowed; -t takes the
      ;; next token).  Stop at the first non-flag token — the CONDITION.
      (setf remaining
            (%consuming-flags (remaining tok rest)
              ((string= tok "-t") (when rest (setf rest (cdr rest))))
              (t (when (%flag-token-contains-any-p tok '(#\F))
                   (setf format-mode t)))))
      (when (>= (length remaining) 2)
        (let* ((condition (first remaining))
               (truthy-p  (if format-mode
                              (%if-shell-format-true-p condition)
                              ;; Run the condition shell command; treat any condition
                              ;; (including a timeout signal from UIOP) as non-zero
                              ;; (falsy), so config loading cannot hang indefinitely.
                              (eql 0 (nth-value 2
                                       (%run-config-shell-command-safe condition))))))
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

;;; ── run-shell / run flag handling (run-shell -b/-t/-d/-C 'cmd') ──────────────
;;;
;;; The fixed-arity table only matches the bare 1-arg form `run-shell 'cmd'`, so
;;; the common real-world `run-shell -b 'cmd'` / `run -b '~/.tmux/...'` forms
;;; (with leading flags) silently failed.  This handler strips leading flags
;;; before the fixed-arity table and runs whatever shell command remains.

(defun %parse-run-shell-delay (value)
  "Parse run-shell -d VALUE using the same integer-only shape as runtime dispatch."
  (and value
       (max 0 (or (%config-parse-integer-or-nil value :junk-allowed t) 0))))

(defun %run-shell-flag-cluster-p (token)
  "True when TOKEN is a cluster containing only no-argument run-shell flags."
  (and (> (length token) 2)
       (char= (char token 0) #\-)
       (loop for index from 1 below (length token)
             always (member (char token index) '(#\b #\C #\E) :test #'char=))))

(defun %apply-run-shell-flag-character (flag)
  "Return the parser state assignment represented by no-argument FLAG."
  (case flag
    (#\b :background)
    (#\C :tmux-command)
    (#\E :combine-stderr)
    (otherwise nil)))

(defun %parse-run-shell-directive-args (args)
  "Parse config-time run-shell ARGS.
Returns (values remaining background-p tmux-command-p combine-stderr-p
start-directory delay invalid-p)."
  (let ((remaining args)
        (background-p nil)
        (tmux-command-p nil)
        (combine-stderr-p nil)
        (start-directory nil)
        (delay nil)
        (invalid-p nil))
    (labels ((apply-state (state)
               (case state
                 (:background (setf background-p t))
                 (:tmux-command (setf tmux-command-p t))
                 (:combine-stderr (setf combine-stderr-p t)))))
      (loop while (and remaining (%leading-flag-token-p (first remaining)))
            for token = (pop remaining)
            do (cond
                 ((%run-shell-flag-cluster-p token)
                  (loop for index from 1 below (length token)
                        do (apply-state
                            (%apply-run-shell-flag-character
                             (char token index)))))
                 ((string= token "-b") (setf background-p t))
                 ((string= token "-C") (setf tmux-command-p t))
                 ((string= token "-E") (setf combine-stderr-p t))
                 ((string= token "-c")
                  (if remaining
                      (setf start-directory (pop remaining))
                      (setf invalid-p t)))
                 ((string= token "-d")
                  (if remaining
                      (setf delay (%parse-run-shell-delay (pop remaining)))
                      (setf invalid-p t)))
                 ((string= token "-t")
                  (if remaining
                      (pop remaining)
                      (setf invalid-p t)))
                 (t
                  (setf invalid-p t)))
            when invalid-p
              do (return)))
    (values remaining background-p tmux-command-p combine-stderr-p
            start-directory delay invalid-p)))

(defun %run-config-shell-command-background (command &key combine-stderr directory delay)
  "Run config COMMAND asynchronously and report the directive as handled."
  (bt:make-thread
   (lambda ()
     (%run-config-shell-command-safe command
                                     :combine-stderr combine-stderr
                                     :directory directory
                                     :delay delay))
   :name "cl-tmux config run-shell")
  t)

(defun %apply-run-shell-tmux-command (command &key background delay)
  "Apply a run-shell -C COMMAND, optionally in the background after DELAY."
  (flet ((apply-command ()
           (when (and delay (plusp delay))
             (sleep delay))
           (ignore-errors (apply-config-directive (%config-tokens command)))))
    (if background
        (progn
          (bt:make-thread #'apply-command
                          :name "cl-tmux config run-shell -C")
          t)
        (progn
          (apply-command)
          t))))

(defun %apply-run-shell-directive (cmd args)
  "Handle 'run-shell [-bCE] [-c start-directory] [-d delay] [-t target] shell-command' directives
   Consumes leading flags:
     -b           run in background
     -C           run a tmux command instead of a shell command (boolean)
     -E           combine stderr with stdout
     -c <path>    run shell command in start-directory
     -t <target>  target pane (takes the next token as its value)
     -d <delay>   delay (takes the next token as its value)
   Stops at the first non-flag token; that token plus any remaining tokens
   (joined by spaces) form the shell command.
   Returns T when CMD is run-shell/run and the form is handled, NIL otherwise."
  (when (member cmd '("run-shell" "run") :test #'string=)
    (multiple-value-bind (remaining background-p tmux-command-p combine-stderr-p
                          start-directory delay invalid-p)
        (%parse-run-shell-directive-args args)
      (when invalid-p
        (return-from %apply-run-shell-directive nil))
      ;; Remaining tokens (joined) form the shell command.
      (let ((command (%join-config-tokens remaining)))
        (cond
          ;; No command after flags: a flag-only invocation is a no-op but handled.
          ((null command) t)
          ;; -C: the argument is a tmux command, not a shell command — run it
          ;; through the config dispatcher (same path if-shell uses for its
          ;; then/else commands).  e.g. `run-shell -C 'display-message hi'`.
          (tmux-command-p
           (%apply-run-shell-tmux-command command
                                          :background background-p
                                          :delay delay))
          ;; Shell command: run it the same way the fixed-arity entries do.
          (t
           ;; A timeout signal (UIOP:SUBPROCESS-ERROR or similar) is abandoned via
           ;; %run-config-shell-command-safe rather than silently treated as a
           ;; non-zero exit, and loading continues.
           (let ((expanded-command (%expand-leading-tilde command))
                 (expanded-directory (and start-directory
                                          (%expand-leading-tilde start-directory))))
             (if background-p
                 (%run-config-shell-command-background
                  expanded-command
                  :combine-stderr combine-stderr-p
                  :directory expanded-directory
                  :delay delay)
                 (%run-config-shell-command-safe
                  expanded-command
                  :combine-stderr combine-stderr-p
                  :directory expanded-directory
                  :delay delay)))
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

(defun %glob-pattern-p (path)
  "True when PATH contains a shell glob metacharacter (* ? or [), i.e. it is a
   pattern rather than a literal path.  Mirrors the detection in %glob-expand."
  (find-if (lambda (c) (member c '(#\* #\? #\[) :test #'char=)) path))

(defun %source-file-report-missing (path)
  "Surface tmux's source-file diagnostic for a missing file / no-glob-match
   (cmd-source-file.c reports `cmdq_error(item, \"%s: %s\", strerror(ENOENT), path)`).
   Routed through the runtime *message-log* sink (the same channel display-message
   uses) so it appears in the show-messages overlay.  Resolved at call time, so the
   config package's load-order independence from cl-tmux is preserved."
  ;; add-message-log lives in the cl-tmux runtime package, which depends on this
  ;; config package — so resolve it at RUNTIME via find-symbol (the same trick
  ;; #{session_count} uses) to avoid a compile-time circular dependency.
  (ignore-errors
    (let ((fn (find-symbol "ADD-MESSAGE-LOG" "CL-TMUX")))
      (when (and fn (fboundp fn))
        (funcall fn (format nil "No such file or directory: ~A" path))))))

(defun %parse-source-file-flags (args)
  "Parse the leading -Fnqv flags and -t target of source-file.  Returns
   (values PARSE-ONLY-P QUIET-P VERBOSE-P FORMAT-P POSITIONALS).  Clustered flags
   (e.g. -qn) are supported; scanning stops at the first non-flag token (a path)."
  (let ((parse-only nil) (quiet nil) (verbose nil) (format-p nil) (rest args))
      (setf rest
          (%consume-leading-flag-tokens
           rest
           (lambda (tok rest)
             (when (%flag-token-contains-any-p tok '(#\n)) (setf parse-only t))
             (when (%flag-token-contains-any-p tok '(#\q)) (setf quiet t))
             (when (%flag-token-contains-any-p tok '(#\v)) (setf verbose t))
             (when (%flag-token-contains-any-p tok '(#\F)) (setf format-p t))
             (let ((target-pos (position #\t tok)))
               (if target-pos
                   (progn
                     (when (and (= target-pos (1- (length tok))) rest)
                       (setf rest (cdr rest)))
                     (values rest nil))
                   (values rest t))))))
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

(defun %resolve-source-file-path (raw format-p)
  "Resolve one source-file positional RAW to its on-disk path: when FORMAT-P
   (-F flag), expand RAW as a format string first (falling back to RAW itself on
   expansion failure), then expand a leading ~ into $HOME.  Returns the resolved
   path string."
  (%expand-leading-tilde
   (if format-p
       (or (ignore-errors (cl-tmux/format:expand-format raw nil)) raw)
       raw)))

(defun %source-file-glob-matches (expanded quiet)
  "Expand EXPANDED (a resolved source-file path, possibly a glob) to its matching
   files via %glob-expand.  When EXPANDED is a glob pattern that matched nothing,
   report tmux's GLOB_NOMATCH diagnostic unless QUIET.  Returns the list of
   matching file namestrings (possibly empty)."
  (let ((matches (%glob-expand expanded)))
    (when (and (%glob-pattern-p expanded) (null matches) (not quiet))
      (%source-file-report-missing expanded))
    matches))

(defun %load-or-parse-source-file (file parse-only quiet)
  "Apply one matched source-file FILE: tokenise-only (tmux's CMD_PARSE_PARSEONLY)
   when PARSE-ONLY, otherwise load and execute it via load-config-file.  A load
   that finds no file (load-config-file returns NIL) reports tmux's 'No such file
   or directory' diagnostic unless QUIET."
  (if (probe-file file)
      (progn
        (if parse-only
            (%parse-config-file-only file)
            (ignore-errors (load-config-file file)))
        t)
      (progn
        (unless quiet
          (%source-file-report-missing file))
        nil)))

(defun source-files (args)
  "Implement `source-file [-Fnqv] [-t target-pane] path...`: for each non-flag PATH, optionally
   expand it as a format string (-F), then expand a leading ~ and shell globs
   (* ? []), and load every matching config file.  With -n (parse-only), each file
   is read and tokenised but NO command runs (tmux's CMD_PARSE_PARSEONLY syntax
   check).  Missing files and unmatched globs report tmux's diagnostic and make the
   command fail unless -q is given; -v is accepted (no client sink to echo to)."
  (multiple-value-bind (parse-only quiet verbose format-p positionals)
      (%parse-source-file-flags args)
    (declare (ignore verbose))
    (let ((ok t))
      (dolist (raw positionals ok)
        (let ((path (%resolve-source-file-path raw format-p)))
          (when (plusp (length path))
            (let ((matches (%source-file-glob-matches path quiet)))
              (if matches
                  (dolist (file matches)
                    (unless (or (%load-or-parse-source-file file parse-only quiet)
                                quiet)
                      (setf ok nil)))
                  (unless quiet
                    (setf ok nil))))))))))

(defun %apply-source-file-directive (cmd args)
  "Intercept source-file: -q/-n/-v flags, glob patterns, and multiple paths
   (the fixed-arity directive table only handled a single bare path).  Returns T
   when CMD is source-file, else NIL."
  (when (member cmd '("source-file" "source") :test #'string=)
    (source-files args)))
