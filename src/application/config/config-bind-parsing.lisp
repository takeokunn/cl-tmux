(in-package #:cl-tmux/config)

;;;; bind/unbind directive parsing and dispatch.
;;;;
;;;; Relies on the generic directive-dispatch machinery in
;;;; config-directives-macro.lisp (loaded before this file).

;;; ── bind-key flag parsing ────────────────────────────────────────────────
;;;
;;; %parse-bind-key-args handles the optional flags before key and command:
;;;   bind [-n] [-r] [-T table] [-N note] key command
;;; Returns (values table key command repeatable note) or NIL on parse failure.

(defun %resolve-single-command-binding (table key-token tokens repeatable note)
  "Resolve a single-command token list TOKENS into the command form stored in the
   key table.  For a single-word token list, tries to map to a directly-bindable
   keyword first; falls back to the alias-aware token list for known command names;
   rejects typos at load time (returning NIL), matching tmux's parse-time validation.
   For a multi-word token list the whole list is stored.
   Returns (values table key-token command repeatable note) or NIL."
  (if (= (length tokens) 1)
      ;; Single word: try keyword dispatch, then alias-aware token list, then reject.
      (let ((keyword (%command-keyword (first tokens))))
        (cond
          (keyword
           (values table key-token keyword repeatable note))
          ;; A recognised command (canonical or tmux alias): store the single-token
          ;; command list, resolved by the alias-aware dispatch at key-press.
          ((%known-command-name-p (first tokens))
           (values table key-token tokens repeatable note))
          ;; Genuine typo: reject at load time, matching tmux.
          (t nil)))
      ;; Multi-token: store as token list.
      (values table key-token tokens repeatable note)))

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

;;; ── Semicolon-sequence splitter ──────────────────────────────────────────
;;;
;;; tmux bind directives support ";" (from "\;" in the config line) as a
;;; command separator: bind r source-file ~/.tmux.conf \; display "Reloaded!"
;;; %split-on-semicolons splits a flat token list on ";" tokens,
;;; removing empty segments, yielding a list of per-command token lists.

(defun %strip-brace-block (tokens)
  "When TOKENS form a `{ ... }` block — first token \"{\" and last token \"}\" —
   return the inner tokens; otherwise return TOKENS unchanged.  This lets the
   tmux 3.x brace form `bind r { cmd1 ; cmd2 }` reuse %split-on-semicolons
   exactly like the older `bind r cmd1 \\; cmd2` form.  An empty block `{ }`
   yields NIL (no commands)."
  (if (and (cdr tokens)
           (string= (first tokens) "{")
           (string= (first (last tokens)) "}"))
      (butlast (rest tokens))
      tokens))

(defun %split-on-semicolons (tokens)
  "Split TOKENS on \";\" tokens, returning a list of per-command token lists.
   Empty segments (consecutive semicolons or trailing) are discarded.
   When no semicolons are present, returns (list tokens) unchanged."
  (let ((result  '())
        (current '()))
    (dolist (tok tokens)
      (if (string= tok ";")
          (progn (when current (push (nreverse current) result))
                 (setf current '()))
          (push tok current)))
    (when current (push (nreverse current) result))
    (if result (nreverse result) (list tokens))))

;;; ── unbind-key flag parsing ──────────────────────────────────────────────
;;;
;;; %parse-unbind-key-args handles optional [-n] [-T table] flags before the key.
;;; Returns (values table key) or (values nil nil) on parse failure.

(defun %parse-unbind-key-args (args)
  "Parse the ARGS list for an unbind directive (excludes the verb itself).
   Returns (values TABLE KEY ALL-P): TABLE is +TABLE-PREFIX+ by default, -n selects
   +TABLE-ROOT+, -T <table> a named table, and -a marks 'unbind every key in the
   table' (KEY is then NIL — the real tmux `unbind -a [-T table]` form).  Returns
   (values nil nil nil) on parse failure."
  (let ((table     +table-prefix+)
        (all-p     nil)
        (remaining args))
    (loop
      (cond
        ((null remaining)
         ;; End of args: valid only when -a was given (whole-table unbind).
         (return (if all-p (values table nil t) (values nil nil nil))))
        ((string= (first remaining) "-n")
         (setf table     +table-root+
               remaining (rest remaining)))
        ((string= (first remaining) "-a")
         (setf all-p     t
               remaining (rest remaining)))
        ((string= (first remaining) "-q")
         ;; -q: quiet — suppress "no such key" errors.  cl-tmux's unbind is
         ;; already silent on a missing key, so -q is accepted and skipped.
         (setf remaining (rest remaining)))
        ((string= (first remaining) "-T")
         (setf remaining (rest remaining))
         (when (null remaining) (return (values nil nil nil)))
         (setf table (pop remaining)))
        (t
         (unless (= (length remaining) 1) (return (values nil nil nil)))
         (return (values table (%parse-key-token (first remaining)) all-p)))))))

;;; ── Declarative bind/unbind verb dispatch ────────────────────────────────

(defmacro define-key-directive-handlers (&rest rules)
  "Build %APPLY-KEY-DIRECTIVE from a declarative table of verb RULES.
   Each RULE is (VERBS &body BODY) where VERBS is a list of verb strings
   and BODY is evaluated with CMD and ARGS in scope."
  `(defun %apply-key-directive (cmd args)
     "Dispatch a bind/unbind directive.  Returns T on success, NIL on failure."
     (cond
       ,@(mapcar
          (lambda (rule)
            (destructuring-bind (verbs &body body) rule
              `((member cmd ',verbs :test #'string=)
                ,@body)))
          rules)
       (t nil))))

(define-key-directive-handlers
  (("bind" "bind-key")
   (multiple-value-bind (table key command repeatable note)
       (%parse-bind-key-args args)
     (when command
       ;; COMMAND is a keyword (built-in) or a token list (`bind key cmd args`).
       ;; NOTE is the optional -N description, surfaced by list-keys.
       (key-table-bind table key command :repeatable repeatable :note note)
       t)))
  (("unbind" "unbind-key")
   (multiple-value-bind (table key all-p)
       (%parse-unbind-key-args args)
     (cond
       ;; -a: clear every binding in TABLE (the real tmux `unbind -a [-T t]`).
       (all-p
        (let ((inner (gethash table *key-tables*)))
          (when inner (clrhash inner)))
        t)
       ((and table key)
        (key-table-unbind table key)
        t)
       (t nil))))
  ;; unbind-all [-T table]: clear all bindings in a key-table (default: prefix).
  ;; -T specifies the table; without -T the prefix table is cleared.
  (("unbind-all")
   (let* ((t-pos  (position "-T" args :test #'string=))
          (table  (if (and t-pos (nth (1+ t-pos) args))
                      (nth (1+ t-pos) args)
                      +table-prefix+))
          (inner  (gethash table *key-tables*)))
     (when inner (clrhash inner))
     t)))
