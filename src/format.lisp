(in-package #:cl-tmux/format)

;;; -- Format string expansion engine -----------------------------------------
;;;
;;; Brace and bracket form handlers, modifier application, iteration expansion,
;;; and the CPS-style expand-step / expand-format entry point.
;;;
;;; Data helpers (pure functions + shorthand/arithmetic tables) live in
;;; format-helpers.lisp.  Strftime support lives in format-strftime.lisp.
;;; Both are loaded before this file.

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
;;;   #{a:35}             the character whose code is 35 ('#')
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
         (when (loop for i from start-s to ns
                     thereis (%glob-match-p pattern string :start-p start-p :start-s i))
           (return t))
         (return nil))
        ((= start-s ns) (return nil))
        ((or (char= (char pattern start-p) #\?)
             (char= (char pattern start-p) (char string start-s)))
         (incf start-p) (incf start-s))
        (t (return nil))))))

(defun %regex-match-p (pattern string &optional ignore-case)
  "Return T when STRING matches the regular expression PATTERN (via cl-ppcre).
   IGNORE-CASE T compiles the pattern case-insensitively.  This backs the tmux
   #{m/r:pattern,string} match modifier.  A malformed PATTERN yields NIL (no
   match) rather than signaling — invalid regexes never break format expansion."
  (handler-case
      (let ((scanner (cl-ppcre:create-scanner pattern
                                              :case-insensitive-mode ignore-case)))
        (and (cl-ppcre:scan scanner string) t))
    (error () nil)))

(defun %pane-visible-lines (pane)
  "The visible (non-scrollback) rows of PANE's screen as a list of strings, top
   to bottom, with trailing spaces trimmed — the per-line content tmux's #{C:}
   search runs against.  Returns NIL when PANE has no live screen.  Read lazily
   (only when a #{C:} modifier actually fires), so non-search formats pay nothing."
  (let ((scr (and pane (cl-tmux/model:pane-screen pane))))
    (when scr
      (let ((w (cl-tmux/terminal:screen-width  scr))
            (h (cl-tmux/terminal:screen-height scr)))
        (loop for y below h
              collect (string-right-trim
                       '(#\Space)
                       (with-output-to-string (s)
                         (dotimes (x w)
                           (write-char (cl-tmux/terminal:cell-char
                                        (cl-tmux/terminal:screen-cell scr x y))
                                       s)))))))))

(defun %content-search-match-p (term line regex-p ci-p)
  "Does LINE match the #{C:} search TERM?  Mirrors tmux's window_pane_search:
   non-regex wraps TERM as the glob *TERM* and fnmatches the whole line (the
   stars turn %glob-match-p's anchored match into a contains-with-globbing
   search); regex scans LINE for TERM.  CI-P folds case on both branches."
  (if regex-p
      (%regex-match-p term line ci-p)
      (let ((pat (concatenate 'string "*" term "*")))
        (if ci-p
            (%glob-match-p (string-downcase pat) (string-downcase line))
            (%glob-match-p pat line)))))

(defun %format-content-search (mod rest context)
  "Evaluate a #{C[/r][/i]:term} content-search modifier.  TERM (REST) is first
   expanded as a format string, then matched against the visible content of the
   context pane line by line; returns the 1-based line number of the first match
   as a string, or \"0\" when there is no match (or no pane).  MOD is the modifier
   token (C, C/r, C/i, C/ri); r selects regex, i case-insensitivity — the same
   flag syntax as #{m/r:} and tmux's format_search."
  (let* ((term    (expand-format rest context))
         (regex-p (and (> (length mod) 1) (find #\r mod :start 1)))
         (ci-p    (and (> (length mod) 1) (find #\i mod :start 1)))
         (lines   (%pane-visible-lines (getf context :%c-search-pane))))
    (or (loop for line in lines
              for n from 1
              when (%content-search-match-p term line regex-p ci-p)
                do (return (format nil "~D" n)))
        "0")))

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
  ;; #{q:var}: backslash-quote shell-special characters (e.g. embedding a path).
  ("q" (%quote-format-value value))
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
    (loop while (< i n)
          for c = (char template i)
          if (and (char= c #\#) (< (1+ i) n) (char= (char template (1+ i)) #\{))
            do (progn (incf depth) (incf i 2))
          else if (char= c #\})
            do (progn (decf depth)
                      (if (zerop depth) (return i) (incf i)))
          else do (incf i))))

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
  (multiple-value-bind (lhs rhs) (%split-two rest)
    (let ((a (expand-format lhs context))
          (b (expand-format rhs context)))
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
                      (t nil))))))))))

(defun %logical-op-p (mod)
  "True when MOD is a logical operator (|| or &&)."
  (member mod '("||" "&&") :test #'string=))

(defun %apply-logical (op rest context)
  "Evaluate a logical #{||:a,b} / #{&&:a,b}.  Split REST on the first TOP-LEVEL
   comma, expand BOTH operands as format strings, then test each for truthiness
   (non-empty and not \"0\").  || returns \"1\" when either operand is truthy;
   && returns \"1\" only when both are.  Mirrors tmux's logical format operators,
   commonly nested inside a conditional: #{?#{||:#{a},#{b}},yes,no}."
  (multiple-value-bind (lhs rhs) (%split-two rest)
    (let ((a (%truthy-p (expand-format lhs context)))
          (b (%truthy-p (expand-format rhs context))))
      (if (string= op "||")
          (if (or a b) "1" "0")
          (if (and a b) "1" "0")))))

(defun %quote-format-value (value)
  "Backslash-escape characters in VALUE that are special to the shell, matching
   tmux's #{q:...} modifier — used to embed a value (e.g. a path) safely inside a
   shell command such as run-shell or if-shell."
  (with-output-to-string (s)
    (loop for ch across value do
      (when (member ch '(#\Space #\Tab #\Newline #\" #\' #\\ #\; #\& #\|
                         #\$ #\` #\( #\) #\< #\> #\* #\? #\[ #\] #\{ #\} #\~ #\#)
                    :test #'char=)
        (write-char #\\ s))
      (write-char ch s))))

(defun %iterate-fmt (items active-item active-fmt inactive-fmt context-fn &optional separator)
  "Iterate ITEMS: format each with ACTIVE-FMT when it is EQ to ACTIVE-ITEM, else
   INACTIVE-FMT.  CONTEXT-FN is called with each item to produce its format context.
   SEPARATOR is written between items when non-NIL."
  (with-output-to-string (s)
    (loop for item in items
          for first = t then nil
          do (when (and separator (not first)) (write-string separator s))
             (write-string
              (expand-format (if (eq item active-item) active-fmt inactive-fmt)
                             (funcall context-fn item))
              s))))

(defun %expand-window-iteration (rest context)
  "Expand a #{W:ACTIVE,INACTIVE} window-list modifier.  Iterates the windows of
   the context's session: the current window is formatted with ACTIVE, the others
   with INACTIVE.  Results are joined with the window-status-separator option.
   Returns \"\" when there is no session."
  (let ((session (getf context :%session)))
    (if (null session)
        ""
        (multiple-value-bind (active-fmt inactive-fmt) (%split-active-inactive rest)
          (%iterate-fmt
           (cl-tmux/model:session-windows session)
           (cl-tmux/model:session-active-window session)
           active-fmt inactive-fmt
           (lambda (win)
             (format-context-from-session session win (cl-tmux/model:window-active-pane win)))
           (or (cl-tmux/options:get-option "window-status-separator") " "))))))

(defun %all-server-sessions ()
  "The list of live session objects from cl-tmux's *server-sessions* registry,
   read by runtime symbol lookup to avoid a compile-time dependency on the umbrella
   package (the same indirection #{session_count} uses).  NIL when empty/unbound."
  (ignore-errors
    (mapcar #'cdr (symbol-value (find-symbol "*SERVER-SESSIONS*" "CL-TMUX")))))

(defun %expand-session-iteration (rest context)
  "Expand a #{S:ACTIVE,INACTIVE} session-list modifier.  Iterates every server
   session: the context's current session is formatted with ACTIVE, the others with
   INACTIVE.  Results are concatenated without separator.  Falls back to the single
   context session when the registry is empty."
  (multiple-value-bind (active-fmt inactive-fmt) (%split-active-inactive rest)
    (let* ((cur-session (getf context :%session))
           (sessions    (or (%all-server-sessions)
                            (and cur-session (list cur-session)))))
      (%iterate-fmt
       sessions cur-session active-fmt inactive-fmt
       (lambda (sess)
         (let* ((win  (cl-tmux/model:session-active-window sess))
                (pane (and win (cl-tmux/model:window-active-pane win))))
           (format-context-from-session sess win pane)))))))

(defun %expand-pane-iteration (rest context)
  "Expand a #{P:ACTIVE,INACTIVE} pane-list modifier.  Iterates the panes of the
   context's current window: the active pane is formatted with ACTIVE, the others
   with INACTIVE.  Results are concatenated without separator.  Returns \"\" when
   there is no window."
  (let* ((session (getf context :%session))
         (window  (and session (cl-tmux/model:session-active-window session))))
    (if (null window)
        ""
        (multiple-value-bind (active-fmt inactive-fmt) (%split-active-inactive rest)
          (%iterate-fmt
           (cl-tmux/model:window-panes window)
           (cl-tmux/model:window-active-pane window)
           active-fmt inactive-fmt
           (lambda (pane) (format-context-from-session session window pane)))))))



