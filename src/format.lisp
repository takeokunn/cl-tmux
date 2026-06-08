(in-package #:cl-tmux/format)

;;;; tmux-style format string expansion.
;;;;
;;;; expand-format TEMPLATE CONTEXT → string
;;;;
;;;; Architecture (data / logic separation):
;;;;   DATA   — define-format-shorthands table maps chars to context keys
;;;;   LOGIC  — %expand-step: one character → next index (CPS-like)
;;;;   EFFECT — expand-format: loop over %expand-step, collect output

;;; ── Pure data helpers ────────────────────────────────────────────────────────

(defun %lookup (context key)
  "Retrieve KEY from the plist CONTEXT.
   When not found in CONTEXT, falls back to *global-options* so that user-defined
   options (#{@my-var}) and any registered tmux option (#{word_separators}) work.
   The fallback uses the hyphenated option name that %variable-to-keyword produces
   (underscores in the template map to hyphens in the keyword and option registry).
   Returns an empty string when absent everywhere."
  (let ((val (getf context key)))
    (if val
        (princ-to-string val)
        ;; The keyword's symbol-name is already the hyphenated option name
        ;; (e.g. WORD-SEPARATORS from word_separators, or @MY-VAR from @my-var).
        ;; Lowercasing it gives the option-registry key directly.
        (let* ((opt-name (string-downcase (symbol-name key)))
               (opt-val  (cl-tmux/options:get-option opt-name nil)))
          (if opt-val (princ-to-string opt-val) "")))))

(defun %variable-to-keyword (name)
  "Convert a variable name string to a context keyword.
   Underscores → hyphens, then upcase and intern in the KEYWORD package."
  (intern (string-upcase (substitute #\- #\_ name)) :keyword))

(defun %truthy-p (str)
  "T when STR is truthy: non-empty, not \"0\", not \"false\"."
  (and (plusp (length str))
       (not (string= str "0"))
       (not (string-equal str "false"))))

(defun %top-level-comma (content start)
  "Index of the next comma in CONTENT at/after START that is NOT inside a nested
   #{...}, or NIL.  Commas inside a nested format belong to it, not the splitter."
  (let ((depth 0) (i start) (n (length content)))
    (loop while (< i n) do
      (let ((c (char content i)))
        (cond
          ((and (char= c #\#) (< (1+ i) n) (char= (char content (1+ i)) #\{))
           (incf depth) (incf i 2))
          ((and (char= c #\}) (plusp depth)) (decf depth) (incf i))
          ((and (char= c #\,) (zerop depth)) (return-from %top-level-comma i))
          (t (incf i)))))
    nil))

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

;;; ── Brace and bracket form handlers (logic layer) ────────────────────────────
;;;
;;; These return the NEXT index to process (CPS convention: each step tells
;;; the caller where to resume).

;;; ── Format modifiers (#{mod:variable}) ──────────────────────────────────────
;;;
;;; tmux lets a #{...} expression carry a modifier before a colon:
;;;   #{=20:window_name}  truncate to the first 20 chars
;;;   #{=-20:window_name} truncate to the last 20 chars
;;;   #{b:pane_current_path} basename (final path component)
;;;   #{d:pane_current_path} dirname  (everything before the final component)
;;;   #{s/foo/bar/:window_name} substitute foo→bar (append i for case-insensitive)
;;; We support that flat (single-modifier, non-nested) subset.  The variable
;;; part is resolved through the normal context lookup before the modifier runs.

(defun %path-basename (path)
  "Final path component of PATH (C basename semantics): trailing slashes are
   stripped first.  \"/a/b/c\" → \"c\", \"/a/b/\" → \"b\", \"foo\" → \"foo\", \"/\" → \"/\"."
  (let* ((trimmed (string-right-trim "/" path))
         (slash   (position #\/ trimmed :from-end t)))
    (cond ((string= trimmed "") "/")
          (slash (subseq trimmed (1+ slash)))
          (t trimmed))))

(defun %path-dirname (path)
  "Directory part of PATH (C dirname semantics).  \"/a/b/c\" → \"/a/b\",
   \"/foo\" → \"/\", \"foo\" → \".\"."
  (let* ((trimmed (string-right-trim "/" path))
         (slash   (position #\/ trimmed :from-end t)))
    (cond ((null slash) ".")
          ((zerop slash) "/")
          (t (subseq trimmed 0 slash)))))

(defun %truncate-spec (mod)
  "Parse a =N / =-N truncation modifier MOD into its integer N (negative N keeps
   the tail), or NIL when MOD is not a truncation spec."
  (when (and (>= (length mod) 2) (char= (char mod 0) #\=))
    (parse-integer mod :start 1 :junk-allowed t)))

(defun %parse-substitute-spec (mod)
  "Parse a substitution modifier s<d>PAT<d>REP<d>[flags] into (values PAT REP
   flags), or NIL when MOD is not a substitution.  <d> is the single delimiter
   character immediately after the leading 's' (usually '/').  Note: PAT and REP
   must not contain the outer #{...} colon separator (flat parsing)."
  (when (and (>= (length mod) 4) (char= (char mod 0) #\s))
    (let* ((delim (char mod 1))
           (p1    (position delim mod :start 2))
           (p2    (and p1 (position delim mod :start (1+ p1)))))
      (when (and p1 p2)
        (values (subseq mod 2 p1)          ; PAT
                (subseq mod (1+ p1) p2)     ; REP
                (subseq mod (1+ p2)))))))   ; flags after the final delimiter

(defun %string-replace-all (string pat replacement &optional ignore-case)
  "Replace every occurrence of PAT in STRING with REPLACEMENT, left to right.
   IGNORE-CASE T matches case-insensitively.  STRING is returned unchanged when
   PAT is empty (avoids an infinite loop)."
  (if (zerop (length pat))
      string
      (let ((test (if ignore-case #'char-equal #'char=)))
        (with-output-to-string (out)
          (let ((start 0))
            (loop
              (let ((pos (search pat string :start2 start :test test)))
                (cond
                  (pos (write-string string out :start start :end pos)
                       (write-string replacement out)
                       (setf start (+ pos (length pat))))
                  (t   (write-string string out :start start)
                       (return))))))))))

;;; ── Glob pattern matching (#{m:pattern,string}) ─────────────────────────────
;;;
;;; tmux's #{m:pattern,string} checks whether STRING matches PATTERN using
;;; Unix shell glob rules: * matches any sequence, ? matches any single char,
;;; and [...] matches a character class.  We implement the first two here
;;; (sufficient for 95% of real configs; [...] is left as a literal match).

(defun %glob-match-p (pattern string &key (start-p 0) (start-s 0))
  "Return T when STRING matches the shell glob PATTERN.
   Supported metacharacters: * (any sequence), ? (any one character).
   Case-sensitive.  Uses simple recursive backtracking."
  (let ((np (length pattern)) (ns (length string)))
    (loop
      (cond
        ((= start-p np) (return (= start-s ns)))
        ((char= (char pattern start-p) #\*)
         ;; Skip consecutive *s
         (loop while (and (< start-p np) (char= (char pattern start-p) #\*))
               do (incf start-p))
         (when (= start-p np) (return t))         ; trailing * matches rest
         ;; Try matching rest of pattern at each position in remaining string
         (loop for i from start-s to ns
               when (%glob-match-p pattern string :start-p start-p :start-s i)
                 do (return-from %glob-match-p t))
         (return nil))
        ((= start-s ns) (return nil))
        ((or (char= (char pattern start-p) #\?)
             (char= (char pattern start-p) (char string start-s)))
         (incf start-p) (incf start-s))
        (t (return nil))))))

(defun %apply-pad-modifier (mod value)
  "Apply a pN / p-N pad modifier to VALUE.  Returns a padded string or NIL.
   Positive N left-aligns VALUE in a field of N chars (space-fill on the right).
   Negative N right-aligns VALUE in a field of ABS(N) chars (space-fill on the left)."
  (when (and (>= (length mod) 2) (char= (char mod 0) #\p))
    (let ((n (parse-integer mod :start 1 :junk-allowed t)))
      (when n
        (let* ((abs-n (abs n))
               (len   (length value)))
          (if (>= len abs-n)
              value
              (if (>= n 0)
                  (concatenate 'string value
                               (make-string (- abs-n len) :initial-element #\Space))
                  (concatenate 'string
                               (make-string (- abs-n len) :initial-element #\Space)
                               value))))))))

;;; — Format modifier dispatch table (Prolog-like fact table) —————————————————
;;;
;;; define-format-modifier-table builds %dispatch-format-modifier from a
;;; declarative (modifier-string expr) fact table, following the same
;;; define-csi-rules / define-style-token-table pattern used elsewhere.
;;;
;;; The heterogeneous fallback cases (pN, s///, =N) are not in the table because
;;; their matching is prefix-based rather than exact.  They are handled by the
;;; caller (%apply-format-modifier) after the table returns NIL.

(defmacro define-format-modifier-table (&rest rules)
  "Build %DISPATCH-FORMAT-MODIFIER from a declarative (modifier-string expr) fact table.
   EXPR receives the implicit variable VALUE (the already-resolved string) and
   returns the transformed string.  The generated function returns NIL when MOD
   does not match any entry."
  `(defun %dispatch-format-modifier (mod value)
     "Apply the exact-match format modifier MOD to VALUE.
      Returns the transformed string, or NIL when MOD is not in the table."
     (cond
       ,@(mapcar (lambda (rule)
                   (destructuring-bind (modifier-string expr) rule
                     `((string= mod ,modifier-string) ,expr)))
                 rules)
       (t nil))))

(define-format-modifier-table
  ("b" (%path-basename value))
  ("d" (%path-dirname  value))
  ("U" (string-upcase   value))
  ("L" (string-downcase value))
  ("l" (format nil "~D" (length value)))
  ;; #{E:var}: expand the VALUE of var as another format string.
  ;; This enables double-expansion: #{E:status-left} looks up status-left's
  ;; value and then expands any #{...} in it, matching real tmux 3.x behaviour.
  ;; The context is empty here — expand-format's global-options fallback handles
  ;; format vars that expand to registered options (which is the main use case).
  ("E" (expand-format value nil)))

(defun %apply-format-modifier (mod value)
  "Apply the format modifier MOD to the already-resolved string VALUE.
   Returns the transformed string, or NIL when MOD is not a recognised modifier
   (so the caller can fall back to a plain variable lookup).
   Supported: b (basename), d (dirname), U (uppercase), L (lowercase), l (length),
              =N / =-N (truncate), pN / p-N (pad to width), s/PAT/REP/[i]."
  (or (%dispatch-format-modifier mod value)
      (%apply-pad-modifier mod value)
      (multiple-value-bind (pat rep flags) (%parse-substitute-spec mod)
        (if pat
            (%string-replace-all value pat rep (and (find #\i flags) t))
            (let ((n (%truncate-spec mod)))
              (cond
                ((null n) nil)
                ((>= n 0) (if (> (length value) n) (subseq value 0 n) value))
                (t (let ((keep (min (length value) (- n))))
                     (subseq value (- (length value) keep))))))))))

(defun %matching-close-brace (template start)
  "Index of the } that closes the #{ whose content begins at START, accounting
   for nested #{...}.  Returns NIL when there is no matching close brace.
   For brace-free content this is just the first }, so non-nested formats are
   delimited exactly as before."
  (let ((depth 1) (i start) (n (length template)))
    (loop while (< i n) do
      (let ((c (char template i)))
        (cond
          ((and (char= c #\#) (< (1+ i) n) (char= (char template (1+ i)) #\{))
           (incf depth) (incf i 2))
          ((char= c #\})
           (decf depth)
           (when (zerop depth) (return-from %matching-close-brace i))
           (incf i))
          (t (incf i)))))
    nil))

(defun %resolve-format-value (s context)
  "Resolve S to a value for a modifier operand.
   • When S contains #{...} it is expanded as a format string.
   • When S contains ':' (but no #{) it is treated as a chained modifier
     expression, e.g. 'd:pane_current_path' → expand-format '#{d:pane_current_path}'.
     This gives modifier chaining for free: #{b:d:pane_current_path} resolves
     the inner #{d:pane_current_path} first, then applies b.
   • Otherwise it is looked up as a single context variable name."
  (cond
    ((search "#{" s) (expand-format s context))
    ((find #\: s)
     (expand-format (concatenate 'string "#{" s "}") context))
    (t (%lookup context (%variable-to-keyword s)))))

(defun %comparison-op-p (mod)
  "True when MOD is a recognised comparison operator (==, !=, <, >, <=, >=)."
  (member mod '("==" "!=" "<" ">" "<=" ">=") :test #'string=))

(defun %apply-comparison (op rest context)
  "Evaluate a comparison: ==/!= are string (in)equality; </>/<=/>= compare the
   sides numerically (a non-numeric side parses as 0).  Split REST on the first
   TOP-LEVEL comma, expand BOTH sides as formats, and return \"1\"/\"0\".  Sides
   are expanded (a bare word is literal, #{...} expands), so #{==:#{host},server}
   compares the host value to the literal \"server\" and #{>:#{client_width},100}
   compares numerically."
  (let* ((comma (%top-level-comma rest 0))
         (a     (expand-format (if comma (subseq rest 0 comma) rest) context))
         (b     (expand-format (if comma (subseq rest (1+ comma)) "") context)))
    (flet ((bit01 (truth) (if truth "1" "0")))
      (cond
        ((string= op "==") (bit01 (string= a b)))
        ((string= op "!=") (bit01 (not (string= a b))))
        (t
         (let ((na (or (parse-integer a :junk-allowed t) 0))
               (nb (or (parse-integer b :junk-allowed t) 0)))
           (bit01 (cond
                    ((string= op "<")  (<  na nb))
                    ((string= op ">")  (>  na nb))
                    ((string= op "<=") (<= na nb))
                    ((string= op ">=") (>= na nb))
                    (t nil)))))))))

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
               (if (and op args (member op '("+" "-" "*" "/" "%") :test #'string=))
                   (let* ((comma (%top-level-comma args 0))
                          (a-str (expand-format (if comma (subseq args 0 comma) args) context))
                          (b-str (expand-format (if comma (subseq args (1+ comma)) "0") context))
                          (a     (or (parse-integer a-str :junk-allowed t) 0))
                          (b     (or (parse-integer b-str :junk-allowed t) 0))
                          (result (cond
                                    ((string= op "+") (+ a b))
                                    ((string= op "-") (- a b))
                                    ((string= op "*") (* a b))
                                    ((string= op "/") (if (zerop b) 0 (truncate a b)))
                                    ((string= op "%") (if (zerop b) 0 (rem a b)))
                                    (t 0))))
                     (write-string (format nil "~D" result) out))
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
                 ;; #{t:strftime_format} — format current time; REST is the strftime format
                 ;; string (e.g. "%H:%M"), NOT a variable name.  Special case before
                 ;; %resolve-format-value so we do not look up "%H:%M" in the context.
                 ((string= mod "t")
                  (write-string (%strftime-format rest) out))
                 ;; #{m:pattern,string} — glob match; returns "1" (match) or "0".
                 ;; Split REST on the first TOP-LEVEL comma: left = pattern, right = string.
                 ;; Both sides are expanded as format strings before matching.
                 ((string= mod "m")
                  (let* ((comma (%top-level-comma rest 0))
                         (pat-str  (expand-format
                                    (if comma (subseq rest 0 comma) rest) context))
                         (test-str (expand-format
                                    (if comma (subseq rest (1+ comma)) "") context)))
                    (write-string (if (%glob-match-p pat-str test-str) "1" "0") out)))
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

(defun %strftime-letter-p (ch)
  "Return T when CH is a single-character strftime code recognised by %strftime-format."
  (and (characterp ch)
       (member ch '(#\Y #\y #\m #\d #\e #\H #\M #\S #\I #\p #\P
                    #\A #\a #\B #\b #\T #\R #\F #\j #\Z #\%)
                :test #'char=)))

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

;;; ── Strftime support (#{t:format}) ──────────────────────────────────────────
;;;
;;; #{t:fmt} formats the CURRENT local time using strftime-style codes in FMT.
;;; Common codes: %Y (year) %m (month) %d (day) %H (hour) %M (min) %S (sec)
;;;               %T (HH:MM:SS) %R (HH:MM) %F (YYYY-MM-DD) %% (literal %)
;;; FMT is the REST part of the #{t:...} expression (after the first colon),
;;; so #{t:%H:%M} gives the current time as "15:30" without a variable lookup.

(defconstant +weekday-names+
    (if (boundp '+weekday-names+)
        (symbol-value '+weekday-names+)
        #("Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday" "Sunday"))
  "Full weekday names indexed 0=Monday..6=Sunday (CL decode-universal-time convention).")

(defconstant +weekday-abbrevs+
    (if (boundp '+weekday-abbrevs+)
        (symbol-value '+weekday-abbrevs+)
        #("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))
  "Three-letter weekday abbreviations indexed 0=Monday..6=Sunday.")

(defconstant +month-names+
    (if (boundp '+month-names+)
        (symbol-value '+month-names+)
        #("January" "February" "March" "April" "May" "June"
          "July" "August" "September" "October" "November" "December"))
  "Full month names indexed 0=January..11=December.")

(defconstant +month-abbrevs+
    (if (boundp '+month-abbrevs+)
        (symbol-value '+month-abbrevs+)
        #("Jan" "Feb" "Mar" "Apr" "May" "Jun"
          "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))
  "Three-letter month abbreviations indexed 0=January..11=December.")

(defun %days-in-month (month year)
  "Return the number of days in MONTH (1-12) of YEAR."
  (case month
    ((1 3 5 7 8 10 12) 31)
    ((4 6 9 11) 30)
    (2 (if (or (and (zerop (mod year 4)) (not (zerop (mod year 100))))
               (zerop (mod year 400))) 29 28))
    (otherwise 30)))

;;; — strftime dispatch table (Prolog-like fact table) ———————————————————————
;;;
;;; define-strftime-code-table builds %dispatch-strftime-code from a declarative
;;; (code-char &rest body) fact table, following the define-csi-rules pattern.
;;; The BODY forms receive the closed-over variables sec/min/hour/day/month/year/weekday
;;; from the enclosing let* in %strftime-format and write to OUT.

(defmacro define-strftime-code-table (&rest rules)
  "Build %DISPATCH-STRFTIME-CODE from a declarative (code-char &rest body) fact table.
   The generated function writes the appropriate output for CODE-CHAR to OUT,
   using the time variables (SEC MIN HOUR DAY MONTH YEAR WEEKDAY) in scope.
   Returns T when CODE-CHAR is recognised, NIL otherwise."
  `(defun %dispatch-strftime-code (code out sec min hour day month year weekday)
     "Write the strftime expansion for CODE-CHAR to OUT.  Returns T on match, NIL otherwise."
     (case code
       ,@(mapcar (lambda (rule)
                   `(,(first rule) ,@(rest rule) t))
                 rules)
       (otherwise nil))))

(define-strftime-code-table
  (#\Y (format out "~4,'0D" year))
  (#\y (format out "~2,'0D" (mod year 100)))
  (#\m (format out "~2,'0D" month))
  (#\d (format out "~2,'0D" day))
  (#\e (format out "~2D" day))
  (#\H (format out "~2,'0D" hour))
  (#\M (format out "~2,'0D" min))
  (#\S (format out "~2,'0D" sec))
  ;; 12-hour clock: 0 o'clock maps to 12
  (#\I (format out "~2,'0D" (let ((h (mod hour 12))) (if (zerop h) 12 h))))
  (#\p (write-string (if (< hour 12) "AM" "PM") out))
  (#\P (write-string (if (< hour 12) "am" "pm") out))
  ;; Weekday arrays indexed 0=Monday (CL decode-universal-time convention)
  (#\A (write-string (aref +weekday-names+  weekday) out))
  (#\a (write-string (aref +weekday-abbrevs+ weekday) out))
  (#\B (write-string (aref +month-names+  (1- month)) out))
  (#\b (write-string (aref +month-abbrevs+ (1- month)) out))
  (#\T (format out "~2,'0D:~2,'0D:~2,'0D" hour min sec))
  (#\R (format out "~2,'0D:~2,'0D" hour min))
  (#\F (format out "~4,'0D-~2,'0D-~2,'0D" year month day))
  (#\j (let ((day-of-year (loop for m from 1 below month
                                sum (%days-in-month m year))))
         (format out "~3,'0D" (+ day-of-year day))))
  (#\Z (write-string "UTC" out))
  (#\% (write-char #\% out)))

(defun %strftime-format (fmt)
  "Format the current local time using strftime-style codes in FMT.
   Supported: %Y %y %m %d %e %H %M %S %I %p %P %A %a %B %b %T %R %F %j %Z %%.
   Unknown codes are kept literally (% + code char).
   Empty FMT uses the default '%a %b %e %H:%M:%S %Z %Y'."
  (multiple-value-bind (sec min hour day month year weekday dst tz)
      (get-decoded-time)
    (declare (ignore dst tz))
    (when (zerop (length fmt))
      (setf fmt "%a %b %e %H:%M:%S %Z %Y"))
    (with-output-to-string (out)
      (let ((fmt-index 0)
            (fmt-length (length fmt)))
        (loop while (< fmt-index fmt-length) do
          (let ((current-char (char fmt fmt-index)))
            (cond
              ((and (char= current-char #\%)
                    (< (1+ fmt-index) fmt-length))
               (let ((code-char (char fmt (1+ fmt-index))))
                 (incf fmt-index 2)
                 (unless (%dispatch-strftime-code
                          code-char out sec min hour day month year weekday)
                   ;; Unknown code: emit literally as %X
                   (write-char #\% out)
                   (write-char code-char out))))
              (t
               (write-char current-char out)
               (incf fmt-index)))))))))

;;; ── Context builder ─────────────────────────────────────────────────────────

(defun %current-time-string ()
  "Return HH:MM string from the system clock."
  (multiple-value-bind (sec min hour) (get-decoded-time)
    (declare (ignore sec))
    (format nil "~2,'0D:~2,'0D" hour min)))

(defun %short-hostname (h)
  "Return the hostname up to the first dot, or the full string if no dot."
  (subseq h 0 (or (position #\. h) (length h))))

;;; ── #{pane_current_command} via pgrep/ps ────────────────────────────────────
;;;
;;; The foreground command of a pane's PTY is the youngest child of the shell
;;; process (pane-pid).  pgrep -P <pid> lists children; ps -o comm= formats
;;; the name.  Results are cached per (pid . cache-time) to avoid spawning
;;; two subprocesses on every render cycle.

(defvar *pane-command-cache* (make-hash-table :test #'eql)
  "pid → (universal-time . command-name) TTL cache for #{pane_current_command}.")

(defconstant +pane-command-cache-ttl+ 2
  "Seconds before #{pane_current_command} is re-queried from the OS.")

(defun %fetch-pane-command (pid)
  "Query the OS for the foreground command of PID's terminal.
   Uses pgrep -P to find the first child process, then ps -o comm= for its name.
   Returns a command name string, or NIL on failure."
  (handler-case
      (let ((child-out (string-trim " \t\n\r"
                          (uiop:run-program
                           (list "pgrep" "-P" (format nil "~D" pid))
                           :output :string :ignore-error-status t
                           :timeout 1))))
        (when (plusp (length child-out))
          ;; pgrep returns one PID per line; take the first
          (let ((first-cpid (string-trim " \t\r"
                              (first (uiop:split-string child-out
                                                        :separator '(#\Newline))))))
            (when (and (plusp (length first-cpid))
                       (every #'digit-char-p first-cpid))
              (let ((name (string-trim " \t\n\r"
                            (uiop:run-program
                             (list "ps" "-o" "comm=" "-p" first-cpid)
                             :output :string :ignore-error-status t
                             :timeout 1))))
                (when (plusp (length name)) name))))))
    (error () nil)))

(defun %pane-cwd-from-os (pane)
  "Query the OS for the current working directory of PANE's shell process.
   On Linux reads /proc/PID/cwd; on macOS uses lsof -p PID -a -d cwd.
   Returns a path string, or empty string on failure."
  (let ((pid (and pane (cl-tmux/model:pane-pid pane))))
    (unless (and pid (> pid 0)) (return-from %pane-cwd-from-os ""))
    ;; Linux: /proc/PID/cwd is a symlink to the cwd.
    (let ((proc-path (format nil "/proc/~D/cwd" pid)))
      (when (probe-file proc-path)
        (let ((cwd (handler-case
                       (string-trim " \t\n\r"
                                    (uiop:run-program
                                     (list "readlink" proc-path)
                                     :output :string :ignore-error-status t
                                     :timeout 1))
                     (error () ""))))
          (when (plusp (length cwd)) (return-from %pane-cwd-from-os cwd)))))
    ;; macOS: lsof reports the cwd as file descriptor 'cwd'.
    ;; Try both full path (/usr/sbin/lsof) and bare name in case PATH varies.
    (handler-case
        (let* ((lsof  (or (and (probe-file "/usr/sbin/lsof") "/usr/sbin/lsof")
                          "lsof"))
               (out (string-trim " \t\n\r"
                                (uiop:run-program
                                 (list lsof "-p" (format nil "~D" pid)
                                       "-a" "-d" "cwd" "-Fn")
                                 :output :string :ignore-error-status t
                                 :timeout 2))))
          ;; lsof -Fn prints "nPATH" lines; find the one starting with "n/"
          (dolist (line (uiop:split-string out :separator '(#\Newline)) "")
            (when (and (> (length line) 1) (char= (char line 0) #\n))
              (let ((path (subseq line 1)))
                (when (plusp (length path))
                  (return-from %pane-cwd-from-os path))))))
      (error () ""))))

(defun %pane-current-command (pane)
  "Return the foreground command name for PANE's PTY, using a TTL cache.
   Falls back to the shell basename when OS introspection is unavailable."
  (let ((pid (and pane (cl-tmux/model:pane-pid pane))))
    (if (and pid (> pid 0))
        (let* ((cached (gethash pid *pane-command-cache*))
               (now    (get-universal-time))
               (stale  (or (null cached)
                           (> (- now (car cached)) +pane-command-cache-ttl+))))
          (if stale
              (let ((cmd (or (%fetch-pane-command pid)
                             (cl-tmux/model::%shell-basename))))
                (setf (gethash pid *pane-command-cache*) (cons now cmd))
                cmd)
              (cdr cached)))
        (cl-tmux/model::%shell-basename))))

(defun format-context-from-session (session window pane
                                    &key (client-width 0) (client-height 0)
                                         (client-tty ""))
  "Build a context plist for EXPAND-FORMAT from SESSION, WINDOW, and PANE.
   Any of SESSION, WINDOW, PANE may be NIL; missing slots default to safe
   empty values.

   Optional keyword arguments supply client dimensions and tty path:
     :CLIENT-WIDTH   — terminal width reported to the client (default 0)
     :CLIENT-HEIGHT  — terminal height reported to the client (default 0)
     :CLIENT-TTY     — path to the client tty device (default \"\")

   Keys returned:
     :session-name :window-index :window-name :window-count :session-windows
     :window-active :window-flags :window-raw-flags :window-panes :pane-index :pane-title
     :pane-id :pane-width :pane-height :pane-pid :pane-left :pane-top :pane-active
     :hostname :host :host-short :time
     :client-width :client-height :client-tty"
  ;; session-active-window is the session's current window — distinct from
  ;; the WINDOW argument which is the window whose context we are building.
  ;; Naming it explicitly avoids confusion when both appear in the same binding.
  (let* ((session-name    (if session (cl-tmux/model:session-name session) ""))
         (session-wins    (if session (cl-tmux/model:session-windows session) nil))
         (session-active-window (if session (cl-tmux/model:session-active-window session) nil))
         (window-count    (length session-wins))
         ;; #{window_index}: the window's numeric id (respects base-index).
         (window-index    (if window (cl-tmux/model:window-id window) 0))
         (window-name     (if window (cl-tmux/model:window-name window) ""))
         (window-active   (if (and window session-active-window
                                   (eq window session-active-window)) "1" "0"))
         ;; #{window_raw_flags}: composite flag string (*=active, -=last, Z=zoomed),
         ;; "" when no flags apply (no single-space padding fallback).
         (window-raw-flags
          (let ((flags ""))
            (when window
              ;; * = current/active window
              (when (and session-active-window (eq window session-active-window))
                (setf flags (concatenate 'string flags "*")))
              ;; - = last window (was previously active and has a positive last-active-time)
              (when (and session
                         (not (eq window session-active-window))
                         ;; Only mark as last if the window has actually been active before
                         (> (cl-tmux/model:window-last-active-time window) 0)
                         (eq window (cl-tmux/model:session-last-window session)))
                (setf flags (concatenate 'string flags "-")))
              ;; Z = zoomed
              (when (cl-tmux/model:window-zoom-p window)
                (setf flags (concatenate 'string flags "Z"))))
            flags))
         ;; #{window_flags}: same as raw flags but padded to a single space when empty.
         (window-flags
          (if (zerop (length window-raw-flags)) " " window-raw-flags))
         ;; #{window_zoomed_flag}: "Z" when the window is zoomed, else " ".
         (window-zoomed-flag (if (and window (cl-tmux/model:window-zoom-p window)) "Z" " "))
         (window-panes    (if window (cl-tmux/model:window-panes window) nil))
         ;; #{pane_index}: the pane's numeric id (respects pane-base-index).
         (pane-index      (if pane (cl-tmux/model:pane-id pane) 0))
         ;; pane-title: prefer the explicit pane-title slot; fall back to the
         ;; screen-title set via OSC 0/2 when the pane has a live screen.
         (pane-title      (cond
                            ((null pane) "")
                            ((and (plusp (length (cl-tmux/model:pane-title pane))))
                             (cl-tmux/model:pane-title pane))
                            ((cl-tmux/model:pane-screen pane)
                             (cl-tmux/terminal:screen-title
                              (cl-tmux/model:pane-screen pane)))
                            (t "")))
         ;; #{pane_current_path}: OSC 7 cwd reported by the shell.
         ;; Falls back to OS proc query (lsof on macOS, /proc on Linux) when
         ;; the shell has not reported its cwd via OSC 7.
         (pane-current-path (let* ((scr (and pane (cl-tmux/model:pane-screen pane)))
                                   (osc-cwd (and scr (cl-tmux/terminal:screen-cwd scr))))
                              (if (and osc-cwd (plusp (length osc-cwd)))
                                  osc-cwd
                                  (%pane-cwd-from-os pane))))
         ;; #{pane_current_command}: foreground process name (via pgrep/ps, TTL-cached).
         (pane-current-command (%pane-current-command pane))
         ;; #{cursor_x} / #{cursor_y}: cursor position in the active pane screen.
         (pane-scr        (and pane (cl-tmux/model:pane-screen pane)))
         (cursor-x        (if pane-scr (cl-tmux/terminal:screen-cursor-x pane-scr) 0))
         (cursor-y        (if pane-scr (cl-tmux/terminal:screen-cursor-y pane-scr) 0))
         ;; #{pane_in_mode}: "1" when pane is in copy mode, else "0".
         (pane-in-mode    (if (and pane-scr (cl-tmux/terminal:screen-copy-mode-p pane-scr))
                              "1" "0"))
         ;; #{window_layout}: tmux layout string (checksum,geometry).
         (window-layout   (or (and window (cl-tmux/model:layout->string window)) ""))
         ;; #{pane_synchronized}: "1" when synchronize-panes option is on, else "0".
         (pane-synchronized (if (cl-tmux/options:get-option "synchronize-panes")
                                "1" "0"))
         ;; #{window_activity_flag}: "#" when the window has unseen activity
         ;; (monitor-activity was triggered).  Cleared when the window is focused.
         (window-activity-flag
          (if (and window (cl-tmux/model:window-activity-flag window)) "#" " "))
         ;; #{window_silence_flag}: "~" when monitor-silence threshold exceeded.
         (window-silence-flag
          (if (and window (cl-tmux/model:window-silence-flag window)) "~" " "))
         ;; #{window_start_flag} / #{window_end_flag}: "1" for first/last window
         ;; in the session list.  Used by themes for list-end decorators.
         (window-start-flag
          (if (and window session-wins (eq window (first session-wins))) "1" "0"))
         (window-end-flag
          (if (and window session-wins (eq window (car (last session-wins)))) "1" "0"))
         ;; #{window_bell_flag}: "!" when any pane in the window has a pending bell.
         ;; Used by status themes to show an alert indicator in the window list.
         (window-bell-flag
          (if (and window
                   (some (lambda (p)
                           (let ((scr (cl-tmux/model:pane-screen p)))
                             (and scr (cl-tmux/terminal:screen-bell-pending scr))))
                         (cl-tmux/model:window-panes window)))
              "!"
              " "))
         (hostname        (machine-instance))
         (time-str        (%current-time-string))
         (host-short      (%short-hostname hostname))
         ;; Environment variables available as format variables.
         ;; These allow theme files to detect the outer terminal (iTerm2, kitty, etc.)
         ;; and adjust rendering accordingly — same set as %if condition context.
         (term-program    (or (ignore-errors (sb-ext:posix-getenv "TERM_PROGRAM")) ""))
         (colorterm       (or (ignore-errors (sb-ext:posix-getenv "COLORTERM")) "")))
    (list :session-name  session-name
          ;; #{session_id}: numeric session identifier.
          :session-id    (if session (cl-tmux/model:session-id session) 0)
          :window-index  window-index
          ;; #{window_id}: numeric window identifier (window-id slot).
          :window-id     (if window (cl-tmux/model:window-id window) 0)
          :window-name   window-name
          :window-count  window-count
          ;; #{session_windows}: tmux's name for the window count.
          :session-windows window-count
          :window-active window-active
          :window-flags  window-flags
          ;; #{window_raw_flags}: same flags but "" (not " ") when empty.
          :window-raw-flags window-raw-flags
          ;; #{window_zoomed_flag}: "Z" when the active pane is zoomed.
          :window-zoomed-flag window-zoomed-flag
          ;; #{window_panes}: number of panes in this window.
          :window-panes  (length window-panes)
          ;; #{window_layout}: layout serialization string.
          :window-layout window-layout
          :pane-index    pane-index
          :pane-title    pane-title
          ;; #{pane_current_path}: OSC 7 cwd reported by the shell.
          :pane-current-path pane-current-path
          ;; Structural pane variables, all pure functions of the pane struct.
          :pane-id       (if pane (cl-tmux/model:pane-id     pane) 0)
          :pane-width    (if pane (cl-tmux/model:pane-width  pane) 0)
          :pane-height   (if pane (cl-tmux/model:pane-height pane) 0)
          :pane-pid      (if pane (cl-tmux/model:pane-pid    pane) 0)
          :pane-left     (if pane (cl-tmux/model:pane-x      pane) 0)
          :pane-top      (if pane (cl-tmux/model:pane-y      pane) 0)
          ;; #{pane_active}: "1" when PANE is its window's active pane, else "0".
          :pane-active   (if (and pane window
                                  (eq pane (cl-tmux/model:window-active-pane window)))
                             "1" "0")
          ;; #{cursor_x} / #{cursor_y}: 0-based cursor position.
          :cursor-x      cursor-x
          :cursor-y      cursor-y
          ;; #{pane_in_mode}: "1" when copy mode active, else "0".
          :pane-in-mode  pane-in-mode
          ;; #{pane_current_command}: foreground process name (TTL-cached via pgrep/ps).
          :pane-current-command pane-current-command
          :hostname      hostname
          :host          hostname
          :host-short    host-short
          :time          time-str
          :client-width  client-width
          :client-height client-height
          :client-tty    client-tty
          ;; #{version}: cl-tmux version string (matches tmux 3.x format for compat).
          :version       "3.5"
          ;; #{session_attached}: "1" when clients are attached, else "0".
          :session-attached (if (and session
                                     (cl-tmux/model:session-clients session))
                                "1" "0")
          ;; #{server_pid}: PID of the cl-tmux server process (via sb-posix when available).
          :server-pid    (let ((getpid (ignore-errors (find-symbol "GETPID" "SB-POSIX"))))
                           (if getpid
                               (format nil "~D" (ignore-errors (funcall getpid)))
                               "0"))
          ;; #{session_last_attached}: universal-time of last access.
          :session-last-attached (if session
                                     (format nil "~D"
                                             (cl-tmux/model:session-last-active session))
                                     "0")
          ;; #{window_last_flag}: "*" when window was last active, else " ".
          ;; #{window_flag} is the active indicator ("*" = active, "-" = last, " " = other).
          :window-last-flag " "
          ;; #{pane_format}: always "1" in context (we have a pane).
          :pane-format (if pane "1" "0")
          ;; #{window_format}: always "1" in context.
          :window-format (if window "1" "0")
          ;; #{pane_synchronized}: reflects synchronize-panes option.
          :pane-synchronized pane-synchronized
          ;; #{window_bell_flag}: "!" when a pane in the window has a pending bell.
          :window-bell-flag window-bell-flag
          ;; #{window_activity_flag}: "#" when monitor-activity was triggered.
          :window-activity-flag window-activity-flag
          ;; #{window_silence_flag}: "~" when monitor-silence threshold exceeded.
          :window-silence-flag window-silence-flag
          ;; #{window_start_flag} / #{window_end_flag}: first/last in session list.
          :window-start-flag window-start-flag
          :window-end-flag   window-end-flag
          ;; #{window_number}: deprecated alias for #{window_index}.
          :window-number window-index
          ;; #{scroll_position}: scrollback offset in copy mode, else "".
          :scroll-position (if (and pane-scr (cl-tmux/terminal:screen-copy-mode-p pane-scr))
                               (format nil "~D" (cl-tmux/terminal:screen-copy-offset pane-scr))
                               "")
          ;; #{selection_active}: "1" when copy mode has an active selection.
          :selection-active (if (and pane-scr
                                     (cl-tmux/terminal:screen-copy-mode-p pane-scr)
                                     (cl-tmux/terminal:screen-copy-selecting pane-scr))
                                "1" "0")
          ;; #{pane_marked}: "1" when the pane is marked, else "0".
          :pane-marked (if (and pane (cl-tmux/model:pane-marked pane)) "1" "0")
          ;; #{pane_input_off}: "1" when pane input is disabled (select-pane -d).
          :pane-input-off (if (and pane (cl-tmux/model:pane-input-disabled pane)) "1" "0")
          ;; #{pane_dead}: "1" when the pane's PTY has closed (remain-on-exit case).
          ;; A pane is dead when its fd is closed (fd <= 0) but it still exists.
          :pane-dead   (if (and pane (<= (cl-tmux/model:pane-fd pane) 0)) "1" "0")
          ;; #{session_count}: total number of sessions in *server-sessions*.
          ;; Accessed via qualified name because *server-sessions* lives in cl-tmux.
          ;; Falls back to 1 (this session) when the registry is empty or unbound.
          :session-count (format nil "~D"
                                 (max 1 (ignore-errors
                                          (length (symbol-value
                                                   (find-symbol "*SERVER-SESSIONS*"
                                                                "CL-TMUX"))))))
          ;; #{session_group}: session group identifier (empty string when not grouped).
          :session-group (if (and session (cl-tmux/model:session-group session))
                             (format nil "~A" (cl-tmux/model:session-group session))
                             "")
          ;; #{pane_mode}: mode name when the pane is in a special mode.
          ;; "copy-mode" when in copy mode, "" otherwise.
          :pane-mode   (if (and pane-scr (cl-tmux/terminal:screen-copy-mode-p pane-scr))
                           "copy-mode" "")
          ;; Environment variables for terminal detection in themes.
          :term-program term-program
          :colorterm    colorterm
          ;; #{client_prefix}: "1" when the prefix key has been pressed and we're
          ;; waiting for the next key; "0" otherwise.  Used by prefix-highlight plugins.
          ;; Reads *prefix-active* from events-loop.lisp (accessed via qualified name).
          :client-prefix (if (ignore-errors
                               (symbol-value
                                (find-symbol "*PREFIX-ACTIVE*" "CL-TMUX")))
                             "1" "0")
          ;; #{client_last_session}: name of the previously active session.
          ;; Used by some plugins to show a "back" indicator.
          :client-last-session ""
          ;; #{window_visible_layout}: layout string for the visible portion.
          ;; Same as #{window_layout} in our implementation.
          :window-visible-layout (or (and window (cl-tmux/model:layout->string window)) "")
          ;; #{session_path}: initial working directory for the session.
          :session-path (ignore-errors (sb-posix:getcwd))
          ;; #{history_size}: number of lines in the active pane's scrollback.
          :history-size (format nil "~D"
                                (if pane-scr
                                    (length (cl-tmux/terminal:screen-scrollback pane-scr))
                                    0))
          ;; #{history_limit}: configured history limit.
          :history-limit (format nil "~D"
                                 (or (cl-tmux/options:get-option "history-limit") 2000))
          ;; #{window_last_flag}: "1" when this is the last (previously active) window.
          :window-last-flag (if (and window session
                                     (eq window (cl-tmux/model:session-last-window session)))
                                "1" "0"))))

(defun format-context-from-window (session window
                                   &key (client-width 0) (client-height 0)
                                        (client-tty ""))
  "Build a context plist for per-window format strings (e.g. window-status-format).
   Like FORMAT-CONTEXT-FROM-SESSION but specialised for a single window.
   Any argument may be NIL.

   Keys: :session-name :window-index :window-name :window-count
         :window-active :window-flags :window-raw-flags :pane-index :pane-title
         :hostname :time :host :host-short
         :client-width :client-height :client-tty"
  (format-context-from-session session window
                               (when window
                                 (first (cl-tmux/model:window-panes window)))
                               :client-width  client-width
                               :client-height client-height
                               :client-tty    client-tty))
