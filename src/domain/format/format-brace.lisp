(in-package #:cl-tmux/format)

;;;; Core #{...} modifier, operator, conditional, and value expansion.

(defun %prefixed-mod-p (mod letter)
  "T when MOD is exactly LETTER or LETTER followed by '/flags' — the shared
   shape of the m[/r][/i]: and C[/r][/i]: modifier tokens."
  (and (plusp (length mod))
       (char= (char mod 0) letter)
       (or (= (length mod) 1) (char= (char mod 1) #\/))))

(defun %expand-timestamp-modifier (rest context out)
  "#{t:VARIABLE} formats a positive integer CL universal-time from VARIABLE.
   Missing, empty, or non-timestamp values expand to the empty string."
  (let* ((looked-up (%lookup context (%variable-to-keyword rest)))
         (ts        (and (stringp looked-up) (plusp (length looked-up))
                         (cl-tmux::%parse-integer-or-nil looked-up :junk-allowed t))))
    (when (and ts (plusp ts))
      (write-string (%strftime-format-at "" ts) out))))

(defun %expand-match-modifier (mod rest context out)
  "#{m:pat,str} / #{m/r:..} / #{m/ri:..} — glob or regex match → \"1\" or \"0\"."
  (let* ((comma    (%top-level-comma rest 0))
         (pat-str  (expand-format (if comma (subseq rest 0 comma)      rest) context))
         (test-str (expand-format (if comma (subseq rest (1+ comma)) "") context))
         (regex-p  (and (> (length mod) 1) (find #\r mod :start 1)))
         (ci-p     (and (> (length mod) 1) (find #\i mod :start 1))))
    (write-string (if (if regex-p
                          (%regex-match-p pat-str test-str ci-p)
                          (%glob-match-p pat-str test-str))
                      "1" "0")
                  out)))

(defun %expand-charcode-modifier (rest context out)
  "#{a:N} — character whose code is N (bare literal or nested #{...} operand)."
  (let* ((n-str (if (search "#{" rest) (expand-format rest context) rest))
         (code  (cl-tmux::%parse-integer-or-nil n-str :junk-allowed t))
         (ch    (and code (<= 0 code (1- char-code-limit))
                     (ignore-errors (code-char code)))))
    (when ch (write-string (string ch) out))))

(defun %expand-value-modifier (mod rest content context out)
  "Fallback arm: value modifier (b, d, U, L, n, =N, pN, s///) or plain context lookup.
   CONTENT is the full brace content (MOD + ':' + REST), used to attempt a plain
   context lookup when MOD is unrecognised."
  (let* ((value    (%resolve-format-value rest context))
         (modified (%apply-format-modifier mod value)))
    (write-string
     (or modified (%lookup context (%variable-to-keyword content)))
     out)))

(defun %expand-brace-modifier (mod rest content context out)
  "Dispatch #{MOD:REST} — the 9-way modifier/operator expansion.
   CONTENT is the full brace content (MOD + ':' + REST), used by the value-modifier
   fallback to attempt a plain context lookup when MOD is unrecognised.
   Prolog-style fact table (first matching clause wins):
     comparison ops → %apply-comparison  (==, !=, <, >, <=, >=)
     logical ops    → %apply-logical     (||, &&)
     W / S / P      → window / session / pane iteration
     t              → %expand-timestamp-modifier
     m[/r][/i]:    → %expand-match-modifier    → \"1\" or \"0\"
     a:             → %expand-charcode-modifier
     C[/r][/i]:    → %format-content-search    → line-number string
     l              → literal: emit REST unexpanded (#{l:#{x}} → \"#{x}\")
     fallback       → %expand-value-modifier   (b, d, U, L, n, =N, pN, s///) or plain lookup"
  (cond
    ((%comparison-op-p mod)
     (write-string (%apply-comparison mod rest context) out))
    ((%logical-op-p mod)
     (write-string (%apply-logical mod rest context) out))
    ((string= mod "W")
     (write-string (%expand-window-iteration rest context) out))
    ((string= mod "S")
     (write-string (%expand-session-iteration rest context) out))
    ((string= mod "P")
     (write-string (%expand-pane-iteration rest context) out))
    ((string= mod "t")
     (%expand-timestamp-modifier rest context out))
    ((%prefixed-mod-p mod #\m)
     (%expand-match-modifier mod rest context out))
    ((string= mod "a")
     (%expand-charcode-modifier rest context out))
    ((%prefixed-mod-p mod #\C)
     (write-string (%format-content-search mod rest context) out))
    ((string= mod "l")
     (write-string rest out))
    (t
     (%expand-value-modifier mod rest content context out))))

(defun %arithmetic-brace-fields (content pipe2)
  "Return (values ARGS FLAGS) — the top-level '|'-separated fields of CONTENT
   after the OP field (which ends at PIPE2).  The LAST field is ARGS (the A,B
   operands); every earlier field is a flag (f and/or a precision digit-string)."
  (let ((fields '()) (i (1+ pipe2)))
    (loop for p = (%top-level-pipe content i)
          do (if p
                 (progn (push (subseq content i p) fields) (setf i (1+ p)))
                 (progn (push (subseq content i) fields) (return))))
    (setf fields (nreverse fields))
    (values (car (last fields)) (butlast fields))))

(defun %arithmetic-brace-precision (flags use-fp)
  "Explicit digit-string flag wins; else 2 when F flag is present, else 0."
  (let ((prec-s (find-if (lambda (f)
                           (and (plusp (length f)) (every #'digit-char-p f)))
                         flags)))
    (cond (prec-s (parse-integer prec-s))
          (use-fp 2)
          (t 0))))

(defun %arithmetic-brace-operand (str use-fp)
  "Coerce STR (already format-expanded) into a double-float operand, truncating
   to an integer value first unless USE-FP requests fractional precision."
  (if use-fp
      (%parse-double str)
      (coerce (truncate (%parse-double str)) 'double-float)))

(defun %expand-arithmetic-brace (content context out)
  "Expand #{e|OP|[f|][PREC|]A,B} — arithmetic and comparison — into OUT.
   OP        : + - * / %, or == != < > <= >=.
   f flag    : optional second field; operands parsed as doubles.
   PREC      : optional precision field (digits); default 0 integer,
               2 when f is present.  Last field is the A,B operands.
   CONTENT is the full brace content (already known to start with \"e|\")."
  (let* ((pipe2 (%top-level-pipe content 2))
         (op    (and pipe2 (subseq content 2 pipe2))))
    (when (and op (plusp (length op)))
      (multiple-value-bind (args flags) (%arithmetic-brace-fields content pipe2)
        (let* ((use-fp (member "f" flags :test #'string=))
               (prec   (%arithmetic-brace-precision flags use-fp))
               (comma  (%top-level-comma args 0))
               (a-str  (expand-format (if comma (subseq args 0 comma) args) context))
               (b-str  (expand-format (if comma (subseq args (1+ comma)) "0") context))
               (a      (%arithmetic-brace-operand a-str use-fp))
               (b      (%arithmetic-brace-operand b-str use-fp))
               (result (%dispatch-arithmetic-op op a b (and use-fp t))))
          (when result
            (write-string (if (stringp result) result (%format-arith-result result prec))
                          out)))))))

(defun %expand-brace (template start context out)
  "Expand #{...} content starting at START (just past the '{').
   Writes to OUT and returns the index just past the closing '}'.
   Emits '#' literally when no closing brace is found.  Nested #{...} inside the
   content is supported via balanced-brace matching + recursive expansion."
  (let ((close (%matching-close-brace template start)))
    (if (null close)
        (progn (write-char #\# out) (1- start))   ; no close: treat # literally
        (let ((content (subseq template start close)))
          (cond
            ((and (>= (length content) 3)
                  (char= (char content 0) #\e)
                  (char= (char content 1) #\|))
             (%expand-arithmetic-brace content context out))
            ((and (plusp (length content)) (char= (char content 0) #\?))
             (multiple-value-bind (cond-str true-str false-str)
                 (%split-conditional (subseq content 1))
               (let ((resolved (if (search "#{" cond-str)
                                   (expand-format cond-str context)
                                   (let ((ctx-val (getf context (%variable-to-keyword cond-str))))
                                     (if ctx-val (princ-to-string ctx-val) cond-str)))))
                 (write-string
                  (expand-format (if (%truthy-p resolved) true-str false-str) context)
                  out))))
            ((find #\: content)
             (let* ((colon (position #\: content))
                    (mod   (subseq content 0 colon))
                    (rest  (subseq content (1+ colon))))
               (%expand-brace-modifier mod rest content context out)))
            (t (write-string (%lookup context (%variable-to-keyword content)) out)))
          (1+ close)))))
