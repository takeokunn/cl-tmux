(in-package #:cl-tmux/format)

;;; -- Format value-modifiers (#{mod:variable}) --------------------------------
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
;;;
;;; Loaded after format-helpers.lisp / format-strftime.lisp and before
;;; format-engine.lisp, which calls %apply-format-modifier and
;;; %resolve-format-value.

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
    (cl-tmux::%parse-integer-or-nil mod :start 1 :junk-allowed t)))

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

(defun %regex-replace-all (string pat replacement &optional ignore-case)
  "Replace every regex match of PAT in STRING with REPLACEMENT (via cl-ppcre),
   mirroring tmux's #{s/PAT/REP/[i]:var} (regsub with REG_EXTENDED).  PAT is an
   extended regular expression; REPLACEMENT supports \\N backreferences.
   IGNORE-CASE T compiles PAT case-insensitively.  An empty PAT returns STRING
   unchanged (matches the literal-era behaviour and avoids per-position inserts).
   A malformed PAT degrades gracefully by returning STRING unchanged — invalid
   regexes never break format expansion (mirrors %regex-match-p)."
  (if (zerop (length pat))
      string
      (handler-case
          (let ((scanner (cl-ppcre:create-scanner
                          pat :case-insensitive-mode ignore-case)))
            (cl-ppcre:regex-replace-all scanner string replacement))
        (error () string))))

(defun %apply-pad-modifier (mod value)
  "Apply a pN / p-N pad modifier to VALUE.  Returns a padded string or NIL.
   Positive N left-aligns VALUE in a field of N chars (space-fill on the right).
   Negative N right-aligns VALUE in a field of ABS(N) chars (space-fill on the left)."
  (when (and (>= (length mod) 2) (char= (char mod 0) #\p))
    (let ((n (cl-tmux::%parse-integer-or-nil mod :start 1 :junk-allowed t)))
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
  ;; #{n:var}: length (character count) of the resolved value, as a decimal string.
  ;; (tmux's FORMAT_LENGTH modifier.)  The literal modifier #{l:...} is NOT in this
  ;; table because it must bypass operand resolution; it is handled directly in
  ;; %expand-brace-modifier (format-engine.lisp).
  ("n" (format nil "~D" (length value)))
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
   Supported: b (basename), d (dirname), U (uppercase), L (lowercase), n (length),
              =N / =-N (truncate), pN / p-N (pad to width), s/PAT/REP/[i].
   (l (literal) is handled in %expand-brace-modifier, not here, since it must
    bypass operand resolution.)"
  (or (%dispatch-format-modifier mod value)
      (%apply-pad-modifier mod value)
      (multiple-value-bind (pat rep flags) (%parse-substitute-spec mod)
        (if pat
            (%regex-replace-all value pat rep (find #\i flags))
            (let ((n (%truncate-spec mod)))
              (cond
                ((null n) nil)
                ((>= n 0) (if (> (length value) n) (subseq value 0 n) value))
                (t (let ((keep (min (length value) (- n))))
                     (subseq value (- (length value) keep))))))))))

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
