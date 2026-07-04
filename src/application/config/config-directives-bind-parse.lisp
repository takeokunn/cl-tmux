(in-package #:cl-tmux/config)

;;;; bind directive argument parsing.

;;; ── bind-key flag parsing ────────────────────────────────────────────────
;;;
;;; %parse-bind-key-args handles the optional flags before key and command:
;;;   bind [-n] [-r] [-T table] [-N note] key command
;;; Returns (values table key command repeatable note) or NIL on parse failure.

(defun %resolve-single-command-binding (table key-token tokens repeatable note)
  "Resolve a single-command token list TOKENS into the command form stored in the
   key table.  For a single-word token list, tries to map to a directly-bindable
   keyword first; falls back to a canonical command token list for known command
   names; rejects aliases and typos at load time (returning NIL).
   For a multi-word token list, the first token must be a known canonical command.
   Returns (values table key-token command repeatable note) or NIL."
  (if (= (length tokens) 1)
      ;; Single word: try keyword dispatch, then canonical token list, then reject.
      (let ((keyword (%command-keyword (first tokens))))
        (cond
          (keyword
           (values table key-token keyword repeatable note))
          ;; A recognised canonical command: store the single-token command list.
          ((%known-command-name-p (first tokens))
           (values table key-token tokens repeatable note))
          ;; Alias or typo: reject at load time.
          (t nil)))
      ;; Multi-token: store only when the command name is canonical.
      (when (%known-command-name-p (first tokens))
        (values table key-token tokens repeatable note))))

(defun %parse-bind-key-flags (args)
  "Consume the leading -n/-r/-T/-N bind-directive flags from ARGS.
   Returns (values remaining-tokens table repeatable note), or NIL when a
   value-taking flag (-T or -N) is missing its argument.
   TABLE defaults to +TABLE-PREFIX+; REPEATABLE and NOTE default to NIL."
  (let ((table      +table-prefix+)
        (repeatable nil)
        (note       nil)
        (missing-arg-p nil))
    ;; Consume all leading flag tokens with %consuming-flags, collecting -n/-r/-T/-N.
    ;; Stops at the first token that does not start with '-' (the key name).
    ;; The lambda body mutates its local REST (returned as first value) so that
    ;; %consume-leading-flag-tokens advances past value-args consumed by -T/-N.
    (let ((remaining
           (%consuming-flags (args tok rest)
             ((string= tok "-n")
              (setf table +table-root+))
             ((string= tok "-r")
              (setf repeatable t))
             ((string= tok "-T")
              ;; -T requires a following table name argument; bail on missing arg.
              (if rest
                  (setf table (pop rest))
                  (setf missing-arg-p t)))
             ;; -N "note": tmux 3.1+ key-binding description.  Must be consumed here
             ;; so the key name is not mis-read as "-N" nor the note as the command.
             ((string= tok "-N")
              (if rest
                  (setf note (pop rest))
                  (setf missing-arg-p t))))))
      (unless missing-arg-p
        (values remaining table repeatable note)))))

(defun %resolve-bind-key-command (table key-token remaining repeatable note)
  "Resolve REMAINING (the tokens after key/table/flag parsing: KEY CMD-TOKEN...)
   into a bind-key binding.  Returns (values table key command repeatable note),
   or NIL when REMAINING has no command, is an empty brace block, or names an
   unrecognised single-word command."
  (when remaining
    (let* ((cmd-tokens (%strip-brace-block (rest remaining)))
           ;; Split on ";" tokens to support multi-command sequences:
           ;;   bind r source-file ~/.tmux.conf \; display "Reloaded!"
           ;; — or: bind r { source-file ~/.tmux.conf ; display "Reloaded!" }
           (sequences  (%split-on-semicolons cmd-tokens)))
      ;; An empty block (`bind r { }`) leaves no command — reject it.
      (when cmd-tokens
        (if (= (length sequences) 1)
            ;; Single command: delegate to %resolve-single-command-binding.
            (%resolve-single-command-binding table key-token (first sequences)
                                             repeatable note)
            ;; Multiple commands: store as :sequence list of token lists.
            (values table key-token (cons :sequence sequences) repeatable note))))))

(defun %parse-bind-key-args (args)
  "Parse the ARGS list for a bind directive (excludes the \"bind\" verb itself).
   Returns (values table key command repeatable note) where TABLE is +TABLE-PREFIX+
   by default and NOTE is the -N description string (or NIL), or NIL when ARGS do
   not form a valid binding.

   Flags consumed:
     -n          Use the root key table instead of the prefix table.
     -r          Mark the binding as repeatable (no prefix needed after first press).
     -T <table>  Bind in the named key table TABLE.
     -N <note>   Attach a human-readable description to the binding (list-keys)."
  (multiple-value-bind (remaining table repeatable note) (%parse-bind-key-flags args)
    ;; After flags: remaining = (key cmd-token...) — need key + at least one cmd.
    (when (and remaining (rest remaining))
      (%resolve-bind-key-command table (%parse-key-token (first remaining))
                                 remaining repeatable note))))
