(in-package #:cl-tmux/format)

;;; -- Comparison and logical format operators ---------------------------------
;;;
;;; Backs tmux's bare comparison operators (#{==:a,b}, #{!=:a,b}, #{<:a,b}, ...)
;;; and logical operators (#{||:a,b}, #{&&:a,b}).  Both split their REST on the
;;; first top-level comma and expand each side as a format string before
;;; comparing/combining.

(defun %comparison-op-p (mod)
  "True when MOD is a recognised comparison operator (==, !=, <, >, <=, >=)."
  (member mod '("==" "!=" "<" ">" "<=" ">=") :test #'string=))

(defun %logical-op-p (mod)
  "True when MOD is a logical operator (|| or &&)."
  (member mod '("||" "&&") :test #'string=))

(defun %split-and-expand (rest context)
  "Split REST on the first top-level comma and expand both halves as format strings.
   Returns (values expanded-lhs expanded-rhs).  Backs binary operator dispatch."
  (multiple-value-bind (lhs rhs) (%split-two rest)
    (values (expand-format lhs context)
            (expand-format rhs context))))

(defun %bit01 (truth)
  "Return \"1\" when TRUTH is non-nil, otherwise \"0\"."
  (if truth "1" "0"))

(defun %apply-comparison (op rest context)
  "Evaluate a comparison: ==/!= are string (in)equality; </>/<=/>= compare the
   sides LEXICOGRAPHICALLY (strcmp), matching tmux's bare comparison operators in
   format.c (which use strcmp, not a numeric compare).  Split REST on the first
   TOP-LEVEL comma, expand BOTH sides as formats, and return \"1\"/\"0\".  Sides
   are expanded (a bare word is literal, #{...} expands), so #{==:#{host},server}
   compares the host value to the literal \"server\" and #{<:10,9} is \"1\" because
   \"10\" sorts before \"9\".  (Numeric comparison is a distinct tmux feature gated
   behind the #{e|...} expression modifier, not these bare operators.)"
  (multiple-value-bind (a b) (%split-and-expand rest context)
    (cond
      ((string= op "==") (%bit01 (string= a b)))
      ((string= op "!=") (%bit01 (string/= a b)))
      (t
       ;; CL string</string>/string<=/string>= return a mismatch index (non-nil =
       ;; true) on success and NIL on failure, so %bit01 treats them correctly.
       (%bit01 (cond
                 ((string= op "<")  (string<  a b))
                 ((string= op ">")  (string>  a b))
                 ((string= op "<=") (string<= a b))
                 ((string= op ">=") (string>= a b))
                 (t nil)))))))

(defun %apply-logical (op rest context)
  "Evaluate a logical #{||:a,b} / #{&&:a,b}.  Split REST on the first TOP-LEVEL
   comma, expand BOTH operands as format strings, then test each for truthiness
   (non-empty and not \"0\").  || returns \"1\" when either operand is truthy;
   && returns \"1\" only when both are.  Mirrors tmux's logical format operators,
   commonly nested inside a conditional: #{?#{||:#{a},#{b}},yes,no}."
  (multiple-value-bind (a b) (%split-and-expand rest context)
    (if (string= op "||")
        (if (or (%truthy-p a) (%truthy-p b)) "1" "0")
        (if (and (%truthy-p a) (%truthy-p b)) "1" "0"))))
