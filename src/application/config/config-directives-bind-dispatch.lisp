(in-package #:cl-tmux/config)

;;;; bind/unbind directive dispatch.

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
  (("bind")
   (multiple-value-bind (table key command repeatable note)
       (%parse-bind-key-args args)
     (when command
       ;; COMMAND is a keyword (built-in) or a token list (`bind key cmd args`).
       ;; NOTE is the optional -N description, surfaced by list-keys.
       (key-table-bind table key command :repeatable repeatable :note note)
       t)))
  (("unbind")
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
