;;;; Key-binding rulebase: the Prolog program and generic query helpers.
;;;;
;;;; The rulebase has two layers:
;;;;
;;;;   FACTS  (dynamic, one per live binding)
;;;;     (binding TABLE KEY COMMAND)   a key in TABLE runs COMMAND
;;;;     (repeatable TABLE KEY)        the binding stays in the prefix table
;;;;     (note TABLE KEY NOTE)         a `bind -N' description string
;;;;
;;;;   RULES  (static, the reasoning)
;;;;     conflict/5           same key, two tables, different commands
;;;;     shadows-root/2       a non-root binding whose key is also bound in root
;;;;     repeatable-command/1 a command reachable through a repeatable binding
;;;;
;;;; TABLE is a table-name string, KEY is a character or key-name string, and
;;;; COMMAND is whatever the store holds: a keyword (`:new-window'), or a
;;;; parsed command list (`("resize-pane" "-L" "5")').  All are ground Prolog
;;;; terms — `cl-prolog:unify' compares constants with `equal', so characters,
;;;; strings, keywords, and command lists all unify by value.

(in-package #:cl-tmux/reasoning)

(defun %reasoning-rules ()
  "Return the static rule clauses as fresh `cl-prolog' clause values.

The inequality goals reference `cl-prolog:\\=' explicitly: builtin goals
dispatch on symbol identity, so a same-named symbol from this package would
raise an existence error instead of resolving to the engine's builtin."
  (let ((root cl-tmux/config:+table-root+))
    (list
     ;; A key is in conflict when two distinct tables bind it to distinct
     ;; commands.  Solutions are symmetric (t1/t2 swap); callers dedupe.
     (cl-prolog:make-clause
      '(conflict ?key ?table-1 ?command-1 ?table-2 ?command-2)
      (list '(binding ?table-1 ?key ?command-1)
            '(binding ?table-2 ?key ?command-2)
            (list '|\\=| '?table-1 '?table-2)
            (list '|\\=| '?command-1 '?command-2)))
     ;; A non-root binding shadows root when its key is also bound in root.
     (cl-prolog:make-clause
      (list 'shadows-root '?table '?key)
      (list '(binding ?table ?key ?command)
            (list '|\\=| '?table root)
            (list 'binding root '?key '?root-command)))
     ;; A command is repeatable when some repeatable binding runs it.
     (cl-prolog:make-clause
      '(repeatable-command ?command)
      (list '(binding ?table ?key ?command)
            '(repeatable ?table ?key))))))

(defun %fact-clauses (facts)
  "Translate FACTS (a list of binding plists) into fact clauses."
  (let ((clauses '()))
    (dolist (fact facts (nreverse clauses))
      (destructuring-bind (&key table key command repeatable note) fact
        (push (cl-prolog:make-clause (list 'binding table key command)) clauses)
        (when repeatable
          (push (cl-prolog:make-clause (list 'repeatable table key)) clauses))
        (when note
          (push (cl-prolog:make-clause (list 'note table key note)) clauses))))))

(defun build-key-rulebase (facts)
  "Return a `cl-prolog' rulebase for FACTS plus the static reasoning rules.

FACTS is a list of plists as produced by `snapshot-key-bindings'.  The result
is an ordinary rulebase; query it with the helpers below or with any
`cl-prolog' entry point (`query-prolog', `prolog-succeeds-p', …)."
  (cl-prolog:make-rulebase
   :clauses (append (%fact-clauses facts) (%reasoning-rules))))

;;; ── Query helpers ─────────────────────────────────────────────────────────
;;;
;;; Each helper turns raw solution lists into ordinary Lisp values so callers
;;; never touch the engine's binding alists.  COMMAND values may be keywords,
;;; strings, or command lists, so dedup/sort use `equal' and a printed key.

(defun key-command (rulebase table key)
  "Return (values COMMAND FOUND-P) for KEY in TABLE, or (values NIL NIL)."
  (cl-prolog:with-prolog-query (?command)
      (rulebase (list 'binding table key '?command))
    (return-from key-command (values ?command t)))
  (values nil nil))

(defun keys-running (rulebase command)
  "Return a list of (TABLE . KEY) conses whose binding runs COMMAND."
  (let ((out '()))
    (dolist (solution (cl-prolog:query-prolog
                       rulebase (list 'binding '?table '?key command))
                      (nreverse out))
      (push (cons (cl-prolog:solution-binding '?table solution)
                  (cl-prolog:solution-binding '?key solution))
            out))))

(defun repeatable-commands (rulebase)
  "Return the distinct commands reachable through a repeatable binding."
  (let ((out '()))
    (dolist (solution (cl-prolog:query-prolog rulebase '(repeatable-command ?command)))
      (pushnew (cl-prolog:solution-binding '?command solution) out :test #'equal))
    (sort out #'string< :key #'prin1-to-string)))

(defun binding-conflicts (rulebase)
  "Return the distinct key conflicts as a list of plists.

Each entry is (:key KEY :tables (T1 T2) :commands (C1 C2)) with the table pair
canonicalized so the symmetric (t1/t2) solutions collapse to one row."
  (let ((seen (make-hash-table :test #'equal))
        (out '()))
    (dolist (solution (cl-prolog:query-prolog
                       rulebase '(conflict ?key ?table-1 ?command-1 ?table-2 ?command-2))
                      (nreverse out))
      (let* ((key (cl-prolog:solution-binding '?key solution))
             (table-1 (cl-prolog:solution-binding '?table-1 solution))
             (table-2 (cl-prolog:solution-binding '?table-2 solution))
             (command-1 (cl-prolog:solution-binding '?command-1 solution))
             (command-2 (cl-prolog:solution-binding '?command-2 solution))
             ;; Order the pair by printed table name for a stable identity.
             (swap (string> (prin1-to-string table-1) (prin1-to-string table-2)))
             (low-table (if swap table-2 table-1))
             (high-table (if swap table-1 table-2))
             (low-command (if swap command-2 command-1))
             (high-command (if swap command-1 command-2))
             (identity (list key low-table high-table)))
        (unless (gethash identity seen)
          (setf (gethash identity seen) t)
          (push (list :key key
                      :tables (list low-table high-table)
                      :commands (list low-command high-command))
                out))))))

(defun shadowing-bindings (rulebase)
  "Return (TABLE . KEY) conses for non-root bindings whose key is also in root."
  (let ((out '()))
    (dolist (solution (cl-prolog:query-prolog rulebase '(shadows-root ?table ?key))
                      (nreverse out))
      (pushnew (cons (cl-prolog:solution-binding '?table solution)
                     (cl-prolog:solution-binding '?key solution))
               out :test #'equal))))
