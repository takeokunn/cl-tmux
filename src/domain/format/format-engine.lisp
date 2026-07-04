(in-package #:cl-tmux/format)

;;;; Core brace expander (%expand-brace), bracket/paren expanders,
;;;;  CPS character processor, and expand-format public entry point.

(defconstant +format-shell-command-timeout+ 2
  "Seconds to allow #(shell-command) format expansion commands to run.")

(defun %run-format-shell-command (command)
  "Run COMMAND for #(shell-command) expansion and return stdout or the empty string."
  (handler-case
      (uiop:run-program (list "/bin/sh" "-c" command)
                        :output :string
                        :ignore-error-status t
                        :timeout +format-shell-command-timeout+)
    (error () "")))

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
    ;; comparison operators: #{==:a,b} #{!=:a,b} #{<:a,b} #{>:..} #{<=:..} #{>=:..}
    ((%comparison-op-p mod)
     (write-string (%apply-comparison mod rest context) out))
    ;; logical operators: #{||:a,b} (either truthy) #{&&:a,b} (both)
    ((%logical-op-p mod)
     (write-string (%apply-logical mod rest context) out))
    ;; #{W:active,inactive} — window list; joined by window-status-separator
    ((string= mod "W")
     (write-string (%expand-window-iteration rest context) out))
    ;; #{S:active,inactive} — session list (no auto-separator)
    ((string= mod "S")
     (write-string (%expand-session-iteration rest context) out))
    ;; #{P:active,inactive} — pane list (no auto-separator)
    ((string= mod "P")
     (write-string (%expand-pane-iteration rest context) out))
    ;; #{t:...} - stored-timestamp lookup
    ((string= mod "t")
     (%expand-timestamp-modifier rest context out))
    ;; #{m:pat,str} / #{m/r:..} / #{m/ri:..} — glob or regex match → "1" / "0"
    ((%prefixed-mod-p mod #\m)
     (%expand-match-modifier mod rest context out))
    ;; #{a:N} — character whose code is N (bare literal or nested #{...} operand)
    ((string= mod "a")
     (%expand-charcode-modifier rest context out))
    ;; #{C:term} / #{C/r:..} / #{C/ri:..} — pane content search → line number
    ((%prefixed-mod-p mod #\C)
     (write-string (%format-content-search mod rest context) out))
    ;; #{l:rest} — literal: emit REST exactly, WITHOUT resolving/expanding it.
    ;; tmux's FORMAT_LITERAL modifier, e.g. #{l:#{pane_in_mode}} → "#{pane_in_mode}".
    ;; Must precede the fallback (which would otherwise expand REST as an operand).
    ((string= mod "l")
     (write-string rest out))
    ;; Fallback: value modifier (b, d, U, L, n, =N, pN, s///) or plain context lookup.
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

(defun %matching-close-brace (template start)
  "Index of the } that closes the #{ whose content begins at START, accounting
   for nested #{...}.  Returns NIL when there is no matching close brace.
   For brace-free content this is just the first }, so non-nested formats are
   delimited exactly as before."
  (let ((depth 1) (i start) (n (length template)))
    (loop while (< i n)
          for c = (char template i)
          if (and (char= c #\#) (< (1+ i) n) (char= (char template (1+ i)) #\{))
            do (progn (incf depth) (incf i 2))
          else if (char= c #\})
            do (progn (decf depth)
                      (if (zerop depth) (return i) (incf i)))
          else do (incf i))))

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
            ;; #{e|OP|[f|][PREC|]A,B} — arithmetic and comparison; delegate to
            ;; %expand-arithmetic-brace.  Checked before the colon branch
            ;; (no colon in the e|..| syntax).
            ((and (>= (length content) 3)
                  (char= (char content 0) #\e)
                  (char= (char content 1) #\|))
             (%expand-arithmetic-brace content context out))
            ;; #{?cond,true,false} — conditional.
            ;; cond-str is looked up in context first; a context hit uses the looked-up
            ;; value so #{?window_active,YES,NO} works alongside #{?1,yes,no}.
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
            ;; #{MOD:rest} — modifier or operator; delegate to %expand-brace-modifier.
            ;; Checked AFTER the conditional (whose ?-branches may contain ':').
            ((find #\: content)
             (let* ((colon (position #\: content))
                    (mod   (subseq content 0 colon))
                    (rest  (subseq content (1+ colon))))
               (%expand-brace-modifier mod rest content context out)))
            ;; #{variable} — context lookup
            (t (write-string (%lookup context (%variable-to-keyword content)) out)))
          (1+ close)))))

(defun %expand-bracket (template start out)
  "Consume #[attrs] style directive starting at START (just past the '[').
   In real tmux these become SGR sequences; here we pass them through literally
   so the renderer can recognise and convert them (or ignore them safely).
   Writes the full #[...] literally and returns the index just past ']'.
   Emits '#' literally when no closing bracket is found."
  (let ((close (position #\] template :start start)))
    (if (null close)
        (progn (write-char #\# out) (1- start))
        (progn
          (write-char #\# out) (write-char #\[ out)
          (write-string (subseq template start close) out)
          (write-char #\] out)
          (1+ close)))))

(defun %expand-paren (template start out)
  "Expand #(shell-cmd) starting at START (just past the '(').
   Runs the command via uiop:run-program and writes its stdout to OUT.
   Returns the index just past the closing ')'.
   On any error (no closing paren, command failure) returns safely without
   crashing: missing ')' emits '#' literally; command errors emit empty string."
  (let ((close (position #\) template :start start)))
    (if (null close)
        (progn (write-char #\# out) (1- start))
        (let ((cmd (subseq template start close)))
          (let ((result (%run-format-shell-command cmd)))
            ;; Strip a single trailing newline (shell commands usually add one)
            (write-string
             (if (and (plusp (length result))
                      (char= (char result (1- (length result))) #\Newline))
                 (subseq result 0 (1- (length result)))
                 result)
             out))
          (1+ close)))))

;;; ── CPS-style character processor ───────────────────────────────────────────
;;;
;;; %expand-step processes template[I] and returns the NEXT index.
;;; It is the kernel of the CPS loop in expand-format.
;;;
;;; Prolog reading of each cond clause:
;;;   expand_step(#,{,...}, ctx, out) :- expand_brace(ctx, out).
;;;   expand_step(#,[,...}, ctx, out) :- expand_bracket(out).
;;;   expand_step(#,(,...}, ctx, out) :- expand_paren(out).
;;;   expand_step(#,X,     ctx, out) :- shorthand(X, ctx, out).
;;;   expand_step(#,?,     _,   out) :- write(#), write(?).   % unknown
;;;   expand_step(%,X,     _,   out) :- strftime_letter(X), expand_strftime(%X).
;;;   expand_step(Ch,      _,   out) :- write(Ch).            % plain char

(defun %expand-step (template i context out)
  "Process TEMPLATE[I] and return the index of the next character to process."
  (declare (type string template) (type fixnum i))
  (let ((ch (char template i)))
    (cond
      ;; Format specifier: '#' followed by another character
      ((and (char= ch #\#) (< (1+ i) (length template)))
       (let ((next (char template (1+ i))))
         (cond
           ((char= next #\{) (%expand-brace   template (+ i 2) context out))
           ((char= next #\[) (%expand-bracket template (+ i 2) out))
           ((char= next #\() (%expand-paren   template (+ i 2) out))
           ((%expand-shorthand next context out) (+ i 2))
           ;; Unknown specifier: emit both characters literally
           (t (write-char #\# out) (write-char next out) (+ i 2)))))
      ;; Bare strftime code: %X where X is a recognised strftime letter.
      ;; Real tmux passes status strings through strftime() before #{} expansion.
      ;; Handling inline keeps the expansion composable and avoids a pre-pass.
      ((and (char= ch #\%)
            (< (1+ i) (length template))
            (%strftime-letter-p (char template (1+ i))))
       (write-string (%strftime-format (format nil "%~C" (char template (1+ i)))) out)
       (+ i 2))
      ;; Plain character: pass through unchanged
      (t (write-char ch out) (+ i 1)))))

;;; ── Public entry point ───────────────────────────────────────────────────────

(defun expand-format (template context)
  "Expand TEMPLATE using CONTEXT (a plist of keyword→value pairs).
   Processes one character position at a time via %expand-step (CPS-like):
   each call returns the next index, making the loop a pure iteration over steps.

   Supported specifiers:  #S #I #W #P #H ##  #{var}  #{?c,t,f}  #[sgr]  #(cmd)
                          #{t:var} (timestamp)  #{=N:var} #{=-N:var} (truncate)
                          #{pN:var} #{p-N:var} (pad)  #{b:var} #{d:var} (path)
                          #{U:var} #{L:var} (case)  #{n:var} (length)
                          #{l:var} (literal — emit operand unexpanded)
                          #{s/PAT/REP/[i]:var} (substitute)"
  (with-output-to-string (out)
    (loop for i = 0 then (%expand-step template i context out)
          while (< i (length template)))))

(defun expand-format-safe (template context &optional (fallback template))
  "Like EXPAND-FORMAT, but returns FALLBACK (default: TEMPLATE unexpanded)
   instead of signalling when expansion errors.  Consolidates the
   handler-case-around-expand-format shape duplicated across the renderer
   and dispatch layers."
  (handler-case (expand-format template context)
    (error () fallback)))
