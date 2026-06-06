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
  "Retrieve KEY from the plist CONTEXT, returning an empty string when absent."
  (let ((val (getf context key)))
    (if val (princ-to-string val) "")))

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

(defun %apply-format-modifier (mod value)
  "Apply the format modifier MOD to the already-resolved string VALUE.
   Returns the transformed string, or NIL when MOD is not a recognised modifier
   (so the caller can fall back to a plain variable lookup).
   Supported: b (basename), d (dirname), U (uppercase), L (lowercase), l (length),
              =N / =-N (truncate), pN / p-N (pad to width), s/PAT/REP/[i]."
  (cond
    ((string= mod "b") (%path-basename value))
    ((string= mod "d") (%path-dirname value))
    ((string= mod "U") (string-upcase value))
    ((string= mod "L") (string-downcase value))
    ((string= mod "l") (format nil "~D" (length value)))
    ;; pN / p-N — pad VALUE to ABS(N) characters.
    ;; Positive N: left-align, space-fill on the right.
    ;; Negative N: right-align, space-fill on the left.
    ((and (>= (length mod) 2) (char= (char mod 0) #\p))
     (let ((n (parse-integer mod :start 1 :junk-allowed t)))
       (when n
         (if (>= n 0)
             (let ((len (length value)))
               (if (>= len n)
                   value
                   (concatenate 'string value
                                (make-string (- n len) :initial-element #\Space))))
             (let* ((abs-n (- n))
                    (len   (length value)))
               (if (>= len abs-n)
                   value
                   (concatenate 'string
                                (make-string (- abs-n len) :initial-element #\Space)
                                value)))))))
    (t (multiple-value-bind (pat rep flags) (%parse-substitute-spec mod)
         (if pat
             (%string-replace-all value pat rep (and (find #\i flags) t))
             (let ((n (%truncate-spec mod)))
               (cond
                 ((null n) nil)
                 ((>= n 0) (if (> (length value) n) (subseq value 0 n) value))
                 (t (let ((keep (min (length value) (- n))))
                      (subseq value (- (length value) keep)))))))))))

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
  "Resolve S to a value for a modifier operand: when S contains a nested #{...} it
   is expanded as a format; otherwise it is looked up as a single context variable
   name.  So #{=10:window_name} looks up window_name, and #{=10:#{window_name}}
   expands the nested form first."
  (if (search "#{" s)
      (expand-format s context)
      (%lookup context (%variable-to-keyword s))))

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
                                        :ignore-error-status t)
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

(defparameter +%weekday-names+
  #("Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday" "Sunday"))
(defparameter +%weekday-abbrevs+
  #("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))
(defparameter +%month-names+
  #("January" "February" "March" "April" "May" "June"
    "July" "August" "September" "October" "November" "December"))
(defparameter +%month-abbrevs+
  #("Jan" "Feb" "Mar" "Apr" "May" "Jun"
    "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))

(defun %days-in-month (month year)
  "Return the number of days in MONTH (1-12) of YEAR."
  (case month
    ((1 3 5 7 8 10 12) 31)
    ((4 6 9 11) 30)
    (2 (if (or (and (zerop (mod year 4)) (not (zerop (mod year 100))))
               (zerop (mod year 400))) 29 28))
    (otherwise 30)))

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
    ;; CL decode-universal-time: weekday 0=Monday..6=Sunday.
    ;; Arrays +%weekday-names+ / +%weekday-abbrevs+ use the same 0=Monday order,
    ;; so we index by WEEKDAY directly for %A/%a.
    (with-output-to-string (out)
      (let ((i 0) (n (length fmt)))
        (loop while (< i n) do
          (let ((c (char fmt i)))
            (if (and (char= c #\%) (< (1+ i) n))
                (let ((code (char fmt (1+ i))))
                  (incf i 2)
                  (case code
                    (#\Y (format out "~4,'0D" year))
                    (#\y (format out "~2,'0D" (mod year 100)))
                    (#\m (format out "~2,'0D" month))
                    (#\d (format out "~2,'0D" day))
                    (#\e (format out "~2D" day))
                    (#\H (format out "~2,'0D" hour))
                    (#\M (format out "~2,'0D" min))
                    (#\S (format out "~2,'0D" sec))
                    (#\I (format out "~2,'0D"
                                 (let ((h (mod hour 12))) (if (zerop h) 12 h))))
                    (#\p (write-string (if (< hour 12) "AM" "PM") out))
                    (#\P (write-string (if (< hour 12) "am" "pm") out))
                    ;; Arrays indexed 0=Monday (CL convention), matching weekday.
                    (#\A (write-string (aref +%weekday-names+ weekday) out))
                    (#\a (write-string (aref +%weekday-abbrevs+ weekday) out))
                    (#\B (write-string (aref +%month-names+ (1- month)) out))
                    (#\b (write-string (aref +%month-abbrevs+ (1- month)) out))
                    (#\T (format out "~2,'0D:~2,'0D:~2,'0D" hour min sec))
                    (#\R (format out "~2,'0D:~2,'0D" hour min))
                    (#\F (format out "~4,'0D-~2,'0D-~2,'0D" year month day))
                    (#\j (let ((yday (loop for m from 1 below month
                                          sum (%days-in-month m year))))
                           (format out "~3,'0D" (+ yday day))))
                    (#\Z (write-string "UTC" out))
                    (#\% (write-char #\% out))
                    (otherwise
                     (write-char #\% out)
                     (write-char code out))))
                (progn (write-char c out) (incf i)))))))))

;;; ── Context builder ─────────────────────────────────────────────────────────

(defun %current-time-string ()
  "Return HH:MM string from the system clock."
  (multiple-value-bind (sec min hour) (get-decoded-time)
    (declare (ignore sec))
    (format nil "~2,'0D:~2,'0D" hour min)))

(defun %short-hostname (h)
  "Return the hostname up to the first dot, or the full string if no dot."
  (subseq h 0 (or (position #\. h) (length h))))

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
     :window-active :window-flags :window-panes :pane-index :pane-title
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
         (window-index    (if (and window session-wins)
                              (let ((pos (position window session-wins)))
                                (if pos (1+ pos) 0))
                              0))
         (window-name     (if window (cl-tmux/model:window-name window) ""))
         (window-active   (if (and window session-active-window
                                   (eq window session-active-window)) "1" "0"))
         (window-flags    (if (and window session-active-window
                                   (eq window session-active-window)) "*" " "))
         (window-panes    (if window (cl-tmux/model:window-panes window) nil))
         (pane-index      (if (and pane window-panes)
                              (let ((pos (position pane window-panes)))
                                (if pos (1+ pos) 0))
                              0))
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
         ;; #{pane_current_path}: the OSC 7 cwd reported by the pane's shell.
         (pane-current-path (let ((scr (and pane (cl-tmux/model:pane-screen pane))))
                              (if scr (cl-tmux/terminal:screen-cwd scr) "")))
         ;; #{cursor_x} / #{cursor_y}: cursor position in the active pane screen.
         (pane-scr        (and pane (cl-tmux/model:pane-screen pane)))
         (cursor-x        (if pane-scr (cl-tmux/terminal:screen-cursor-x pane-scr) 0))
         (cursor-y        (if pane-scr (cl-tmux/terminal:screen-cursor-y pane-scr) 0))
         ;; #{pane_in_mode}: "1" when pane is in copy mode, else "0".
         (pane-in-mode    (if (and pane-scr (cl-tmux/terminal:screen-copy-mode-p pane-scr))
                              "1" "0"))
         ;; #{window_layout}: tmux layout string (checksum,geometry).
         (window-layout   (or (and window (cl-tmux/model:layout->string window)) ""))
         (hostname        (machine-instance))
         (time-str        (%current-time-string))
         (host-short      (%short-hostname hostname)))
    (list :session-name  session-name
          :window-index  window-index
          :window-name   window-name
          :window-count  window-count
          ;; #{session_windows}: tmux's name for the window count.
          :session-windows window-count
          :window-active window-active
          :window-flags  window-flags
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
          :hostname      hostname
          :host          hostname
          :host-short    host-short
          :time          time-str
          :client-width  client-width
          :client-height client-height
          :client-tty    client-tty)))

(defun format-context-from-window (session window
                                   &key (client-width 0) (client-height 0)
                                        (client-tty ""))
  "Build a context plist for per-window format strings (e.g. window-status-format).
   Like FORMAT-CONTEXT-FROM-SESSION but specialised for a single window.
   Any argument may be NIL.

   Keys: :session-name :window-index :window-name :window-count
         :window-active :window-flags :pane-index :pane-title
         :hostname :time :host :host-short
         :client-width :client-height :client-tty"
  (format-context-from-session session window
                               (when window
                                 (first (cl-tmux/model:window-panes window)))
                               :client-width  client-width
                               :client-height client-height
                               :client-tty    client-tty))
