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
                          (list '(usage ?name "")))))

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

;;; NOTE: these three deliberately do NOT use cl-prolog's SETOF/3, despite it
;;; looking like a natural fit — command/flag names in this domain are raw
;;; Lisp strings, and cl-prolog's standard-order-of-terms comparator (which
;;; SETOF needs internally to sort/dedup) has no case for STRINGP; it signals
;;; "Not a Prolog term" as soon as it must order two distinct string
;;; solutions. Verified experimentally against the live command table (which
;;; has far more than one distinct command/flag, so the bug reliably
;;; triggers). See the parallel note on REPEATABLE-COMMANDS in
;;; key-rulebase.lisp for the same finding in the other reasoning domain.
;;; They use FINDALL/3 (via %FINDALL, defined in key-rulebase.lisp) instead —
;;; it does not sort/dedup internally, so the STRINGP limitation above does
;;; not apply; dedup/sort stay explicit Lisp-level steps here.

(defun commands-with-flag (rulebase flag)
  "Return the canonical command names that accept FLAG, sorted."
  (sort (remove-duplicates (%findall rulebase '?name (list 'accepts-flag '?name flag))
                           :test #'equal :from-end t)
        #'string<))

(defun flags-of-command (rulebase name)
  "Return the flags command NAME accepts, sorted."
  (sort (remove-duplicates (%findall rulebase '?flag (list 'accepts-flag name '?flag))
                           :test #'equal :from-end t)
        #'string<))

(defun scriptable-commands (rulebase)
  "Return the canonical commands that take no arguments, sorted."
  (sort (remove-duplicates (%findall rulebase '?name '(scriptable ?name))
                           :test #'equal :from-end t)
        #'string<))
