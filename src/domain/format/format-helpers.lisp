(in-package #:cl-tmux/format)

;;; -- Pure data helpers + format shorthand/arithmetic tables -----------------
;;;
;;; This file contains the purely functional utilities and data-table macros
;;; used by the rest of the format engine.  Nothing here calls expand-format,
;;; %expand-step, or the brace expansion functions; it is safe to load first.

;;; ── Pure data helpers ────────────────────────────────────────────────────────

(defun %lookup (context key)
  "Retrieve KEY from the plist CONTEXT.
   When not found in CONTEXT, falls back to *global-options* so that user-defined
   options (#{@my-var}) and any registered tmux option (#{word_separators}) work,
   then to the process environment — tmux's format_find checks the environment
   for unresolved names, which is what makes config variable assignments
   (NAME=value lines) usable as #{NAME}.
   The option fallback uses the hyphenated name %variable-to-keyword produces;
   the environment fallback restores the underscores (env names are
   conventionally UPPER_SNAKE, matching the upcased keyword symbol-name).
   Returns an empty string when absent everywhere."
  (let ((val (getf context key)))
    (if val
        (princ-to-string val)
        ;; The keyword's symbol-name is already the hyphenated option name
        ;; (e.g. WORD-SEPARATORS from word_separators, or @MY-VAR from @my-var).
        ;; Lowercasing it gives the option-registry key directly.
        (let* ((opt-name (string-downcase (symbol-name key)))
               (opt-val  (cl-tmux/options:get-option opt-name nil)))
          (cond
            (opt-val (princ-to-string opt-val))
            (t (or (sb-ext:posix-getenv
                    (substitute #\_ #\- (symbol-name key)))
                   "")))))))

(defun %variable-to-keyword (name)
  "Convert a variable name string to a context keyword.
   Underscores → hyphens, then upcase and intern in the KEYWORD package."
  (intern (string-upcase (substitute #\- #\_ name)) :keyword))

(defun %truthy-p (str)
  "T when STR is truthy, matching tmux's format_true: any non-empty string is
   truthy EXCEPT the single character \"0\".  Note this means \"false\", \"00\",
   \"0.0\" and \"-0\" are all TRUTHY in tmux — only the empty string and exactly
   \"0\" are false."
  (and (plusp (length str))
       (not (string= str "0"))))

(defun %top-level-char (content start target)
  "Index of the next TARGET character in CONTENT at/after START that is NOT
   inside a nested #{...}, or NIL.  A TARGET occurrence inside a nested format
   belongs to it, not the caller's splitter."
  (let ((depth 0) (i start) (n (length content)))
    (loop while (< i n)
          do (let ((c (char content i)))
               (cond
                 ((and (char= c #\#) (< (1+ i) n) (char= (char content (1+ i)) #\{))
                  (incf depth) (incf i 2))
                 ((and (char= c #\}) (plusp depth)) (decf depth) (incf i))
                 ((and (char= c target) (zerop depth)) (return i))
                 (t (incf i))))
          finally (return nil))))

(defun %top-level-comma (content start)
  "Index of the next comma in CONTENT at/after START that is NOT inside a nested
   #{...}, or NIL.  Commas inside a nested format belong to it, not the splitter."
  (%top-level-char content start #\,))

(defun %top-level-pipe (content start)
  "Index of the next '|' in CONTENT at/after START that is NOT inside a nested
   #{...}, or NIL.  Pipes inside a nested format (e.g. a nested #{e|..}) belong
   to it, not the field splitter."
  (%top-level-char content start #\|))

(defun %split-two (rest)
  "Split REST on the first top-level comma into (values first second).
   When no comma is present, SECOND defaults to the empty string."
  (let ((comma (%top-level-comma rest 0)))
    (if comma
        (values (subseq rest 0 comma) (subseq rest (1+ comma)))
        (values rest ""))))

(defun %split-active-inactive (rest)
  "Split REST on the first top-level comma into (values active-fmt inactive-fmt).
   When no comma is present, INACTIVE defaults to ACTIVE (both use the same format)."
  (let ((comma (%top-level-comma rest 0)))
    (if comma
        (values (subseq rest 0 comma) (subseq rest (1+ comma)))
        (values rest rest))))

(defun %split-conditional (content)
  "Split CONTENT (text after '?') into (values cond true-branch false-branch).
   Splits on TOP-LEVEL commas only, so a comma inside a nested #{...} (e.g. the
   condition #{==:#{x},y}) stays part of that nested format."
  (let ((comma1 (%top-level-comma content 0)))
    (if (null comma1)
        (values content "" "")
        (let* ((cond-str (subseq content 0 comma1))
               (comma2   (%top-level-comma content (1+ comma1))))
          (if (null comma2)
              (values cond-str (subseq content (1+ comma1)) "")
              (values cond-str
                      (subseq content (1+ comma1) comma2)
                      (subseq content (1+ comma2))))))))

;;; ── Shorthand character table (data layer) ───────────────────────────────────
;;;
;;; Prolog-like fact table — each row is one format shorthand:
;;;   format_char(#\S) :- lookup(:session-name).
;;;   format_char(#\I) :- lookup(:window-index).
;;;   format_char(#\W) :- lookup(:window-name).
;;;   format_char(#\P) :- lookup(:pane-index).
;;;   format_char(#\H) :- lookup(:hostname).
;;;   format_char(#\#) :- write(#\#).        -- literal hash

(defmacro define-format-shorthands (&rest specs)
  "Build %EXPAND-SHORTHAND from a declarative (char context-key) fact table.
   Returns T when CH is a known shorthand (so the caller can advance by 2),
   NIL when unknown."
  `(defun %expand-shorthand (ch context out)
     "Expand single-character shorthand CH to OUT via CONTEXT lookup.
      Returns T on match, NIL when CH is not a recognized shorthand."
     (case ch
       ,@(mapcar (lambda (spec)
                   (destructuring-bind (char key) spec
                     `(,char (write-string (%lookup context ,key) out) t)))
                 specs)
       (#\# (write-char #\# out) t)
       (otherwise nil))))

(define-format-shorthands
  (#\S :session-name)
  (#\I :window-index)
  (#\W :window-name)
  (#\P :pane-index)
  (#\H :hostname))

;;; ── Float operand parsing for #{e|...|f|...} ────────────────────────────────

(defun %parse-double (string)
  "Parse the leading numeric prefix of STRING as a double-float, mirroring
   strtod's lenient behaviour: leading sign, digits, a decimal point, and an
   exponent are consumed; trailing junk is ignored.  Returns 0.0d0 when no
   number is present.  Never signals."
  (if (not (stringp string))
      0.0d0
      (let* ((s (string-trim '(#\Space #\Tab) string))
             (n (length s))
             (i 0))
        ;; optional leading sign
        (when (and (< i n) (member (char s i) '(#\+ #\-) :test #'char=))
          (incf i))
        (let ((digits-start i) (seen-dot nil) (seen-exp nil))
          (loop while (< i n)
                for c = (char s i)
                do (cond
                     ((digit-char-p c) (incf i))
                     ((and (char= c #\.) (not seen-dot) (not seen-exp))
                      (setf seen-dot t) (incf i))
                     ((and (member c '(#\e #\E) :test #'char=)
                           (not seen-exp) (> i digits-start))
                      (setf seen-exp t) (incf i)
                      (when (and (< i n)
                                 (member (char s i) '(#\+ #\-) :test #'char=))
                        (incf i)))
                     (t (return))))
          (let ((token (subseq s 0 i)))
            (if (and (plusp (length token))
                     (some #'digit-char-p token))
                (handler-case
                    (let ((*read-eval* nil)
                          (*read-default-float-format* 'double-float))
                      (let ((v (read-from-string token nil 0.0d0)))
                        (if (realp v) (coerce v 'double-float) 0.0d0)))
                  (error () 0.0d0))
                0.0d0))))))

;;; ── Arithmetic operator dispatch table (Prolog-like fact table) ─────────────
;;;
;;; define-arithmetic-op-table builds %dispatch-arithmetic-op from a declarative
;;; (op-string expr) fact table, following the define-csi-rules / define-strftime-code-table
;;; pattern.  A and B are double-float operands (already cast to integer values in
;;; integer mode); USE-FP selects float vs. truncating division.  Arithmetic rules
;;; return a number; comparison rules return the string \"1\" or \"0\".

(defmacro define-arithmetic-op-table (&rest rules)
  "Build %DISPATCH-ARITHMETIC-OP from a declarative (op-string expr) fact table.
   Each EXPR is evaluated with double-float variables A and B and the boolean
   USE-FP in scope.  Arithmetic EXPRs return a number; comparison EXPRs return a
   \"1\"/\"0\" string.  Division and modulo guard against a zero divisor.
   Returns the result, or NIL when OP-STRING is not recognised."
  `(defun %dispatch-arithmetic-op (op a b use-fp)
     "Evaluate operator OP on double-float operands A and B (USE-FP selects float
      vs. truncating semantics).  Returns a number for arithmetic operators, a
      \"1\"/\"0\" string for comparisons, or NIL when OP is not recognised."
     (declare (ignorable use-fp))
     (cond
       ,@(mapcar (lambda (rule)
                   (destructuring-bind (op-string expr) rule
                     `((string= op ,op-string) ,expr)))
                 rules)
       (t nil))))

(define-arithmetic-op-table
  ("+"  (+ a b))
  ("-"  (- a b))
  ("*"  (* a b))
  ("/"  (if (zerop b) 0 (if use-fp (/ a b) (truncate a b))))
  ("%"  (if (zerop b) 0 (if use-fp (rem a b) (truncate (rem a b)))))
  ("==" (if (< (abs (- a b)) 1d-9) "1" "0"))
  ("!=" (if (> (abs (- a b)) 1d-9) "1" "0"))
  ("<"  (if (< a b) "1" "0"))
  (">"  (if (> a b) "1" "0"))
  ("<=" (if (<= a b) "1" "0"))
  (">=" (if (>= a b) "1" "0")))

(defun %format-arith-result (result prec)
  "Render a numeric arithmetic RESULT to a string with PREC decimal places,
   mirroring tmux's xasprintf(\"%.*f\", prec, result).  PREC 0 yields a bare
   integer (e.g. \"3\", matching tmux's %.0f); PREC > 0 yields fixed decimals
   (e.g. \"16.5000\").  Comparison operators already return their \"1\"/\"0\"
   string and bypass this formatter."
  (if (<= prec 0)
      (format nil "~D" (truncate result))
      (format nil "~,vF" prec (coerce result 'double-float))))
