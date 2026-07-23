(in-package #:cl-tmux)

;;; -- Shell execution commands ------------------------------------------------
;;;
;;; run-shell, if-shell.

(defun %run-shell-overlay-text (output)
  "Return the overlay text for RUN-SHELL OUTPUT."
  (or (and output (plusp (length output)) output)
      "(run-shell: no output)"))

(defun %cmd-run-shell-arg (session args)
  "run-shell [-bCE] [-c start-dir] command:
   run COMMAND in a shell and show the output.
   -b: run in background (fire-and-forget, no output shown).
   -C executes COMMAND as a tmux command instead of a shell command.
   -E redirects stderr to stdout for displayed shell output.
   -c start-dir: run COMMAND with start-dir as the subprocess directory."
  (with-command-input (flags positionals args "c"
                             :allowed-flags '(#\b #\C #\E #\c)
                             :message "run-shell: unsupported argument")
    (let* ((command (format nil "~{~A~^ ~}" positionals))
           (start-directory (%expand-start-dir session (%flag-value flags #\c))))
      (when (plusp (length command))
        (cond
          ((%run-shell-tmux-command-p flags)
           (%run-command-line session command))
          ((%run-shell-background-p flags)
           (run-shell command :background t
                              :combine-stderr (%run-shell-combine-stderr-p flags)
                              :start-directory start-directory))
          (t
           (let ((output (run-shell command
                                    :combine-stderr
                                    (%run-shell-combine-stderr-p flags)
                                    :start-directory start-directory)))
             (show-overlay (%run-shell-overlay-text output)))))))))

(defun %if-shell-run-branch (session then-str else-str truthy-p)
  "Run the THEN-STR or ELSE-STR command line for IF-SHELL depending on TRUTHY-P."
  (if truthy-p
      (when then-str (%run-command-line session then-str))
      (when else-str (%run-command-line session else-str))))

(defun %if-shell-format-result-truthy-p (result)
  "Treat a formatted IF-SHELL result as truthy when it is neither empty nor 0."
  (not (member result '("" "0") :test #'string=)))

(defmacro define-flag-predicates (&rest specs)
  "Define a boolean %FLAG-PRESENT-P wrapper predicate for each SPEC, a
   (name flag-char docstring) triple."
  `(progn
     ,@(mapcar (lambda (spec)
                 (destructuring-bind (name flag-char docstring) spec
                   `(defun ,name (flags) ,docstring (%flag-present-p flags ,flag-char))))
               specs)))

(define-flag-predicates
  (%run-shell-background-p #\b
   "True when RUN-SHELL was called with the background flag.")
  (%run-shell-tmux-command-p #\C
   "True when RUN-SHELL should route COMMAND through tmux instead of the shell.")
  (%run-shell-combine-stderr-p #\E
   "True when RUN-SHELL should redirect stderr into displayed stdout.")
  (%if-shell-format-p #\F
   "True when IF-SHELL should expand its condition as a format string."))

(defun %cmd-if-shell-format-arg (session target-session target-window target-pane
                                  cond-str then-str else-str)
  "Handle IF-SHELL when -F is present by expanding the condition as a format."
  (let* ((ctx    (cl-tmux/format:format-context-from-session
                  target-session target-window target-pane))
         (result (cl-tmux/format:expand-format cond-str ctx)))
    (%if-shell-run-branch session then-str else-str
                          (%if-shell-format-result-truthy-p result))))

(defun %cmd-if-shell-shell-arg (session cond-str then-str else-str)
  "Handle IF-SHELL without -F by delegating to the shell exit status."
  (if-shell cond-str
            (lambda () (when then-str (%run-command-line session then-str)))
            :else-fn (lambda () (when else-str (%run-command-line session else-str)))))

(defun %cmd-if-shell-arg (session args)
  "if-shell [-bF] [-t target-pane] condition [then-cmd] [else-cmd]: conditional command execution.
   -F: treat condition as a format string (#{var}) instead of a shell command.
   -b, -t: supported flags.
   Without -F: runs condition as shell; exit 0 = truthy."
  (with-command-input (flags positionals args "t"
                             :allowed-flags '(#\b #\F #\t)
                             :max-positionals 3
                             :message "if-shell: unsupported argument")
    (let* ((format-p (%if-shell-format-p flags))
           (target-str (%flag-value flags #\t))
           (cond-str (first positionals))
           (then-str (second positionals))
           (else-str (third positionals)))
      (when cond-str
        (with-target-context (target-session target-window target-pane session target-str)
          (if format-p
              (%cmd-if-shell-format-arg session target-session target-window target-pane
                                        cond-str then-str else-str)
              (%cmd-if-shell-shell-arg session cond-str then-str else-str)))))))
