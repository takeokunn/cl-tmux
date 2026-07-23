;;;; Command-metadata reasoning: a second cold-path Prolog read-model.
;;;;
;;;; Projects cl-tmux's canonical command table (`*command-usage-table*',
;;;; name → getopt usage string) into a rulebase so callers can ask relational
;;;; questions the flat table cannot answer directly: which commands accept a
;;;; given flag, which take no arguments, which flags a command supports.
;;;;
;;;;   FACTS
;;;;     (command NAME)              NAME is a canonical command
;;;;     (usage NAME FLAGS-STRING)   its raw usage string
;;;;     (accepts-flag NAME FLAG)    NAME accepts single-letter FLAG (a string)
;;;;   RULES
;;;;     (scriptable NAME)           :- (usage NAME "")   — accepts no arguments
;;;;     (flag-shared FLAG A B)      :- two distinct commands both accept FLAG
;;;;
;;;; This is strictly cold-path (introspection / validation), never the hot
;;;; dispatch loop.

(in-package #:cl-tmux/reasoning)

(defun %parse-usage-flags (usage)
  "Extract the single-character option flags declared in a getopt USAGE string.

Flags appear as `-` immediately after `[` or a space, followed by one or more
alphanumeric letters (clustered `[-dErx]` or valued `[-t target]` forms).
Returns a de-duplicated list of one-character strings, in first-seen order."
  (let ((flags '())
        (index 0)
        (length (length usage)))
    (loop while (< index length) do
      (let ((dash (position #\- usage :start index)))
        (cond
          ((null dash) (setf index length))
          ((and (plusp dash)
                (member (char usage (1- dash)) '(#\[ #\Space)))
           (let ((cursor (1+ dash)))
             (loop while (and (< cursor length)
                              (alphanumericp (char usage cursor)))
                   do (pushnew (string (char usage cursor)) flags :test #'string=)
                      (incf cursor))
             (setf index (max cursor (1+ dash)))))
          (t (setf index (1+ dash))))))
    (nreverse flags)))

(defun command-usage-facts ()
  "Return the canonical command table as (NAME . USAGE) pairs.

Reads the internal `*command-usage-table*'; a cold-path introspection use."
  (copy-alist (symbol-value (find-symbol "*COMMAND-USAGE-TABLE*" :cl-tmux))))

(defun %command-rules ()
  "Static rule clauses for the command-metadata rulebase."
  (list
   (cl-prolog:make-clause '(scriptable ?name)
                          (list '(usage ?name "")))
   (cl-prolog:make-clause '(flag-shared ?flag ?a ?b)
                          (list '(accepts-flag ?a ?flag)
                                '(accepts-flag ?b ?flag)
                                (list '|\\=| '?a '?b)))))

(defun build-command-rulebase (usage-pairs)
  "Build a cl-prolog rulebase from USAGE-PAIRS (a NAME . USAGE alist)."
  (let ((clauses '()))
    (dolist (pair usage-pairs)
      (destructuring-bind (name . usage) pair
        (push (cl-prolog:make-clause (list 'command name)) clauses)
        (push (cl-prolog:make-clause (list 'usage name usage)) clauses)
        (dolist (flag (%parse-usage-flags usage))
          (push (cl-prolog:make-clause (list 'accepts-flag name flag)) clauses))))
    (cl-prolog:make-rulebase
     :clauses (append (nreverse clauses) (%command-rules)))))

(defun current-command-rulebase ()
  "Build a command-metadata rulebase from the live canonical command table."
  (build-command-rulebase (command-usage-facts)))

;;; ── Query helpers ─────────────────────────────────────────────────────────

(defun command-accepts-flag-p (rulebase name flag)
  "True when command NAME accepts single-character FLAG (a one-char string)."
  (cl-prolog:prolog-succeeds-p rulebase (list 'accepts-flag name flag)))

(defun commands-with-flag (rulebase flag)
  "Return the canonical command names that accept FLAG, sorted."
  (let ((out '()))
    (dolist (solution (cl-prolog:query-prolog rulebase (list 'accepts-flag '?name flag)))
      (pushnew (cl-prolog:solution-binding '?name solution) out :test #'equal))
    (sort out #'string<)))

(defun flags-of-command (rulebase name)
  "Return the flags command NAME accepts, sorted."
  (let ((out '()))
    (dolist (solution (cl-prolog:query-prolog rulebase (list 'accepts-flag name '?flag)))
      (pushnew (cl-prolog:solution-binding '?flag solution) out :test #'equal))
    (sort out #'string<)))

(defun scriptable-commands (rulebase)
  "Return the canonical commands that take no arguments, sorted."
  (let ((out '()))
    (dolist (solution (cl-prolog:query-prolog rulebase '(scriptable ?name)))
      (pushnew (cl-prolog:solution-binding '?name solution) out :test #'equal))
    (sort out #'string<)))
