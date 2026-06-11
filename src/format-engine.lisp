(in-package #:cl-tmux/format)

;;;; Core brace expander (%expand-brace), bracket/paren expanders,
;;;;  CPS character processor, and expand-format public entry point.

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
            ;; #{?cond,true,false} — conditional
            ;; cond-str may be a context variable name ("window_active") or a
            ;; literal value ("1", "0", "").  Perform a context lookup first: if
            ;; the keyword key is present in the context plist, use the looked-up
            ;; value; otherwise treat cond-str as the literal condition.
            ;; This makes #{?window_active,YES,NO} work for real-world use-cases
            ;; while preserving #{?1,yes,no} / #{?0,yes,no} / #{?,yes,no}.
            ;; #{e|OP|A,B} — arithmetic: OP is +,-,*,/,% (integer).
            ;; No colon — checked before the colon branch.
            ((and (>= (length content) 3)
                  (char= (char content 0) #\e)
                  (char= (char content 1) #\|))
             (let* ((pipe2 (position #\| content :start 2))
                    (op    (and pipe2 (subseq content 2 pipe2)))
                    (args  (and pipe2 (subseq content (1+ pipe2)))))
               (if (and op args)
                   (let* ((comma (and args (%top-level-comma args 0)))
                          (a-str (expand-format (if comma (subseq args 0 comma) args) context))
                          (b-str (expand-format (if comma (subseq args (1+ comma)) "0") context))
                          (a     (or (parse-integer a-str :junk-allowed t) 0))
                          (b     (or (parse-integer b-str :junk-allowed t) 0))
                          (result (%dispatch-arithmetic-op op a b)))
                     (when result (write-string (format nil "~D" result) out)))
                   (write-string "" out))))
            ((and (plusp (length content)) (char= (char content 0) #\?))
             (multiple-value-bind (cond-str true-str false-str)
                 (%split-conditional (subseq content 1))
               (let ((resolved
                       (if (search "#{" cond-str)
                           (expand-format cond-str context)   ; nested format cond
                           (let ((ctx-val (getf context
                                                 (%variable-to-keyword cond-str))))
                             (if ctx-val (princ-to-string ctx-val) cond-str)))))
                 ;; Expand the chosen branch too — literals pass through unchanged,
                 ;; nested #{...} expands.  (A comma INSIDE a nested branch is not
                 ;; supported: %split-conditional splits naively on commas.)
                 (write-string (expand-format
                                (if (%truthy-p resolved) true-str false-str)
                                context)
                               out))))
            ;; #{mod:...} — modifier applied to a looked-up variable or format string.
            ;; Checked AFTER the conditional (whose ?-branches may contain ':'),
            ;; and falls back to a plain lookup when MOD is unrecognised.
            ((find #\: content)
             (let* ((colon (position #\: content))
                    (mod   (subseq content 0 colon))
                    (rest  (subseq content (1+ colon))))
               (cond
                 ;; comparison operators: #{==:a,b} #{!=:a,b} #{<:a,b} #{>:..} #{<=:..} #{>=:..}
                 ((%comparison-op-p mod)
                  (write-string (%apply-comparison mod rest context) out))
                 ;; logical operators: #{||:a,b} (either truthy) #{&&:a,b} (both)
                 ((%logical-op-p mod)
                  (write-string (%apply-logical mod rest context) out))
                 ;; #{W:active,inactive} — iterate the session's windows, applying
                 ;; ACTIVE to the current window and INACTIVE to the rest, joined by
                 ;; window-status-separator.  The building block for custom window
                 ;; lists (and, later, status-format[0]).
                 ((string= mod "W")
                  (write-string (%expand-window-iteration rest context) out))
                 ;; #{S:active,inactive} — iterate every server session (analog of
                 ;; W:); #{P:active,inactive} — iterate the current window's panes.
                 ;; Both concatenate without an auto-separator (tmux S:/P: behaviour).
                 ((string= mod "S")
                  (write-string (%expand-session-iteration rest context) out))
                 ((string= mod "P")
                  (write-string (%expand-pane-iteration rest context) out))
                 ;; #{t:...} — timestamp / strftime modifier.  We first look REST
                 ;; up as a context variable: if it resolves to a positive integer
                 ;; (a CL universal-time, e.g. #{t:session_last_attached}) we format
                 ;; THAT timestamp with the default format — tmux's #{t:VARIABLE}
                 ;; semantics.  Otherwise REST is treated as a strftime format
                 ;; string applied to the CURRENT time (e.g. #{t:%H:%M}), which also
                 ;; preserves literal pass-through and the empty-REST default.
                 ((string= mod "t")
                  (let* ((looked-up (%lookup context (%variable-to-keyword rest)))
                         (ts        (and (stringp looked-up)
                                         (plusp (length looked-up))
                                         (parse-integer looked-up :junk-allowed t))))
                    (if (and ts (plusp ts))
                        (write-string (%strftime-format-at "" ts) out)
                        (write-string (%strftime-format rest) out))))
                 ;; #{m:pattern,string} — glob match; returns "1" (match) or "0".
                 ;; Split REST on the first TOP-LEVEL comma: left = pattern, right = string.
                 ;; Both sides are expanded as format strings before matching.
                 ;; #{m:pat,str} fnmatch glob; #{m/r:pat,str} regular expression
                 ;; (with /ri for case-insensitive).  Both split REST on the first
                 ;; top-level comma and expand each side before matching.
                 ((and (plusp (length mod))
                       (char= (char mod 0) #\m)
                       (or (= (length mod) 1) (char= (char mod 1) #\/)))
                  (let* ((comma (%top-level-comma rest 0))
                         (pat-str  (expand-format
                                    (if comma (subseq rest 0 comma) rest) context))
                         (test-str (expand-format
                                    (if comma (subseq rest (1+ comma)) "") context))
                         (regex-p  (and (> (length mod) 1) (find #\r mod :start 1)))
                         (ci-p     (and (> (length mod) 1) (find #\i mod :start 1)))
                         (matched  (if regex-p
                                       (%regex-match-p pat-str test-str ci-p)
                                       (%glob-match-p pat-str test-str))))
                    (write-string (if matched "1" "0") out)))
                 ;; #{a:N} — the single character whose character code is N.
                 ;; A nested #{...} operand is expanded first so #{a:#{var}} works,
                 ;; but a bare literal like 35 is parsed directly (NOT looked up as a
                 ;; variable, which would treat "35" as a context var name and fail).
                 ;; The parsed code is range-checked against char-code-limit; an
                 ;; invalid code (nil, negative, or out of range) yields the empty
                 ;; string.  Special-cased before %resolve-format-value's modifier
                 ;; handling because REST is a NUMBER, like #{t:...} and #{m:...} above.
                 ((string= mod "a")
                  (let* ((n-str (if (search "#{" rest)
                                    (expand-format rest context)
                                    rest))
                         (code  (parse-integer n-str :junk-allowed t))
                         (ch    (and code (<= 0 code (1- char-code-limit))
                                     (ignore-errors (code-char code)))))
                    (when ch (write-string (string ch) out))))
                 ;; #{C:term} — search the visible pane content for TERM and write
                 ;; the 1-based line number of the first match (or "0").  #{C/r:}
                 ;; treats TERM as a regex, #{C/i:} folds case (same flag grammar
                 ;; as #{m/r:}).  Detected like the m: branch: MOD starts with C
                 ;; and is either bare "C" or "C/...".
                 ((and (plusp (length mod))
                       (char= (char mod 0) #\C)
                       (or (= (length mod) 1) (char= (char mod 1) #\/)))
                  (write-string (%format-content-search mod rest context) out))
                 ;; value modifiers (=N, b, d, U, L, l, pN, s///) on a resolved operand
                 (t
                  (let* ((value    (%resolve-format-value rest context))
                         (modified (%apply-format-modifier mod value)))
                    (write-string
                     (or modified (%lookup context (%variable-to-keyword content)))
                     out))))))
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
          (let ((result
                  (handler-case
                      (uiop:run-program (list "/bin/sh" "-c" cmd)
                                        :output :string
                                        :ignore-error-status t
                                        :timeout 2)
                    (error () ""))))
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
                          #{t:fmt} (strftime)  #{=N:var} #{=-N:var} (truncate)
                          #{pN:var} #{p-N:var} (pad)  #{b:var} #{d:var} (path)
                          #{U:var} #{L:var} #{l:var} (case/length)
                          #{s/PAT/REP/[i]:var} (substitute)"
  (with-output-to-string (out)
    (loop for i = 0 then (%expand-step template i context out)
          while (< i (length template)))))

