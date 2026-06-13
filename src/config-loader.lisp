(in-package #:cl-tmux/config)

(defun apply-config-directive (tokens)
  "Apply one parsed config directive (list of string TOKENS) to live state.
   Returns T when applied, NIL for an unknown/invalid directive.
   Handles bind/unbind, set-hook, set[-g|-a|-s|-u|...], set-environment [-r],
   if-shell, run-shell/run [-b|-C|-t|-d], and the fixed-arity directive table."
  (when tokens
    (let ((cmd  (first tokens))
          (args (rest tokens)))
      (or (%apply-key-directive cmd args)
          (%apply-if-shell-directive cmd args)
          (%apply-set-environment-directive cmd args)
          (%apply-set-directive cmd args)
          (%apply-set-hook-directive cmd args)
          (%apply-run-shell-directive cmd args)
          (%apply-source-file-directive cmd args)
          (%apply-config-directive-inner tokens)))))

(defun %strip-config-comment (line)
  "Remove a trailing # comment from a config LINE.  Following tmux's lexer, a #
   begins a comment only when it is OUTSIDE single/double quotes and is NOT part of
   a format construct (#{ #( #[) nor an escaped ## .  Returns the line up to the
   comment, right-trimmed (or the whole line when there is no comment)."
  (let ((len (length line)) (i 0) (in-single nil) (in-double nil))
    (loop while (< i len)
          do (let ((c (char line i)))
               (cond
                 (in-single (when (char= c #\') (setf in-single nil)))
                 (in-double (cond ((char= c #\\) (incf i))   ; skip escaped char
                                  ((char= c #\") (setf in-double nil))))
                 ((char= c #\') (setf in-single t))
                 ((char= c #\") (setf in-double t))
                 ((char= c #\#)
                  (cond
                    ;; ## — escaped literal #, not a comment.
                    ((and (< (1+ i) len) (char= (char line (1+ i)) #\#)) (incf i))
                    ;; #{ / #( / #[ — a format construct, not a comment.
                    ((and (< (1+ i) len) (member (char line (1+ i)) '(#\{ #\( #\[))) nil)
                    ;; A # begins a comment only at the START of a token (line start or
                    ;; right after whitespace).  A # in the MIDDLE of an unquoted word —
                    ;; e.g. a hex colour bg=#0000ff or @var=#abc — is a literal character,
                    ;; matching tmux's lexer, which only enters comment scanning at a token
                    ;; boundary (cmd-parse.y yylex).  Without this guard such values were
                    ;; silently truncated to "bg=" unless the user quoted them.
                    ((and (> i 0)
                          (let ((p (char line (1- i))))
                            (not (or (char= p #\Space) (char= p #\Tab)))))
                     nil)
                    ;; Otherwise a comment begins here: drop the rest of the line.
                    (t (return (string-right-trim '(#\Space #\Tab) (subseq line 0 i)))))))
             (incf i))
          finally (return line))))

(defun apply-config-line (line)
  "Apply a single config LINE.  Blank lines and # comments (full-line and inline,
   respecting quotes and #{...} formats) are ignored.  Returns T when applied."
  (let ((trimmed (string-trim '(#\Space #\Tab #\Return #\Newline)
                              (%strip-config-comment line))))
    (and (plusp (length trimmed))
         (apply-config-directive (%config-tokens trimmed)))))

;;; ── %if / %else / %endif preprocessor support ───────────────────────────────
;;;
;;; tmux config files may contain conditional blocks:
;;;   %if <condition>
;;;   ...
;;;   %else
;;;   ...
;;;   %endif
;;;
;;; The condition is a tmux format string that evaluates to "1" (truthy) or
;;; "" / "0" (falsy).  A dynamic callback (*config-condition-evaluator*) is used
;;; so the config layer (which cannot depend on cl-tmux/format) can delegate
;;; evaluation to the top-level package which has access to full format expansion.
;;; When the callback is unset, all %if conditions are treated as truthy so that
;;; no directives are silently skipped.

(defvar *config-condition-evaluator* nil
  "When non-NIL, a function (string) → string that evaluates a %if condition.
   The string result is truthy when non-empty and not equal to \"0\".
   NIL means all %if conditions are treated as truthy (nothing skipped).")

(defun %eval-config-condition (cond-str)
  "Evaluate a %if condition string via *config-condition-evaluator*.
   Returns T when the condition is truthy, NIL otherwise.
   Defaults to T when *config-condition-evaluator* is NIL."
  (if *config-condition-evaluator*
      (let ((result (handler-case (funcall *config-condition-evaluator* cond-str)
                      (error () "1"))))
        (and result (plusp (length result)) (not (string= result "0"))))
      t))

(defun %preprocessor-line-p (trimmed)
  "Return :if, :else, :elif, :endif, or NIL indicating whether TRIMMED is a
   preprocessor directive line."
  (cond
    ((and (>= (length trimmed) 3) (string= (subseq trimmed 0 3) "%if")
          (or (= (length trimmed) 3) (not (alpha-char-p (char trimmed 3)))))
     :if)
    ((string= trimmed "%else")
     :else)
    ((and (>= (length trimmed) 5) (string= (subseq trimmed 0 5) "%elif")
          (or (= (length trimmed) 5) (not (alpha-char-p (char trimmed 5)))))
     :elif)
    ((string= trimmed "%endif")
     :endif)
    (t nil)))

(defun %skip-quoted-span (line i len end-ch escape-p)
  "Return the index past the quoted span whose opening delimiter is at I.
   Scans to the matching END-CH; when ESCAPE-P, a backslash skips the next char."
  (incf i)
  (loop while (and (< i len) (char/= (char line i) end-ch))
        do (if (and escape-p (char= (char line i) #\\)) (incf i 2) (incf i)))
  (min len (1+ i)))

(defun %line-brace-delta (line)
  "Net unquoted brace depth of LINE: count of '{' minus '}', ignoring braces
   inside single/double quotes or immediately after a backslash.  Used by
   load-config-from-stream to detect and join multi-line { ... } command blocks
   (tmux 3.x brace syntax)."
  (let ((delta 0) (i 0) (len (length line)))
    (loop while (< i len) do
      (let ((c (char line i)))
        (cond
          ((char= c #\\) (incf i 2))
          ((char= c #\") (setf i (%skip-quoted-span line i len #\" t)))
          ((char= c #\') (setf i (%skip-quoted-span line i len #\' nil)))
          ((char= c #\{) (incf delta) (incf i))
          ((char= c #\}) (decf delta) (incf i))
          (t (incf i)))))
    delta))

(defun %read-brace-block (first-line stream)
  "FIRST-LINE has opened an unbalanced { ... } block; keep reading from STREAM
   until the brace depth returns to zero (or EOF), then return all the lines
   joined into one logical line with \" ; \" separators so the inner commands
   become a semicolon sequence the bind parser already understands.
   Each line's inline # comment is stripped FIRST — otherwise a comment on an
   inner line would survive into the joined block and truncate it at that #, and
   a brace inside a comment would corrupt the depth count."
  (let* ((stripped-first (%strip-config-comment first-line))
         (depth (%line-brace-delta stripped-first))
         (parts (list stripped-first)))
    (loop while (> depth 0)
          for next = (read-line stream nil nil)
          while next
          for stripped = (%strip-config-comment next)
          do (push stripped parts)
             (incf depth (%line-brace-delta stripped)))
    (format nil "~{~A~^ ; ~}" (nreverse parts))))

(defun %line-continues-p (line)
  "T when LINE ends with an ODD number of backslashes — a continuation backslash
   that escapes the newline (an even count is escaped backslashes, not a
   continuation)."
  (let ((n 0) (i (1- (length line))))
    (loop while (and (>= i 0) (char= (char line i) #\\))
          do (incf n) (decf i))
    (oddp n)))

(defun %read-logical-config-line (first-line stream)
  "Join trailing-backslash continuation lines into one logical line: while a line
   ends in a continuation backslash, drop that backslash and append the next line.
   Mirrors tmux: `cmd arg1 \\<newline>arg2` is one command.  Returns the joined line."
  (let ((line first-line))
    (loop while (%line-continues-p line)
          for next = (read-line stream nil nil)
          while next
          do (setf line (concatenate 'string
                                     (subseq line 0 (1- (length line)))
                                     next)))
    line))

(defun load-config-from-stream (stream)
  "Apply every directive line read from STREAM, honoring %if/%elif/%else/%endif
   blocks.  Multi-line { ... } command blocks (tmux 3.x brace syntax) are joined
   into a single logical directive before being applied.  Returns the count applied."
  ;; COND-STACK: one state per open %if level — :ACTIVE (this branch is taken),
  ;; :SEEKING (no branch matched yet; keep evaluating %elif/%else), :TAKEN (a branch
  ;; already matched; skip the rest), or :DEAD (an ancestor was skipping when this
  ;; %if began).  A line is applied only when EVERY level is :ACTIVE.  The four
  ;; states are what a plain skip flag cannot express: distinguishing "still seeking
  ;; a match" from "a branch already matched" is required for correct %elif chains.
  (let ((cond-stack nil)
        (count 0))
    (flet ((active-p () (every (lambda (s) (eq s :active)) cond-stack)))
      (loop for raw = (read-line stream nil nil)
            while raw
            ;; Join trailing-backslash continuation lines, then strip any inline #
            ;; comment, before classifying — so a continued/commented directive (or
            ;; `%if 1 # note`) is seen as one clean logical line.
            for line = (%strip-config-comment
                        (%read-logical-config-line raw stream)) do
        (let* ((trimmed (string-trim '(#\Space #\Tab #\Return #\Newline) line))
               (pp-type (%preprocessor-line-p trimmed)))
          (case pp-type
            (:if
             ;; Only evaluate the condition in an active context; a dead block
             ;; never evaluates (matching tmux's short-circuit).
             (let ((cond-str (string-trim " \t" (subseq trimmed 3))))
               (push (cond ((not (active-p)) :dead)
                           ((%eval-config-condition cond-str) :active)
                           (t :seeking))
                     cond-stack)))
            (:elif
             (when cond-stack
               (let ((cond-str (string-trim " \t" (subseq trimmed 5))))
                 (setf (first cond-stack)
                       (case (first cond-stack)
                         (:seeking (if (%eval-config-condition cond-str) :active :seeking))
                         (:active  :taken)   ; prior branch matched → skip the rest
                         (t        (first cond-stack)))))))   ; :taken / :dead unchanged
            (:else
             (when cond-stack
               (setf (first cond-stack)
                     (case (first cond-stack)
                       (:seeking :active)    ; no branch matched → take the else
                       (:active  :taken)
                       (t        (first cond-stack))))))
            (:endif
             (when cond-stack (pop cond-stack)))
            (otherwise
             ;; Normal line: apply only when every %if level is active.
             (when (active-p)
               ;; Join a multi-line { ... } command block into one logical line.
               (let ((full-line (if (> (%line-brace-delta line) 0)
                                    (%read-brace-block line stream)
                                    line)))
                 (when (apply-config-line full-line)
                   (incf count)))))))))
    count))

(defun load-config-from-string (text)
  "Apply every directive line in TEXT, honoring %if/%else/%endif blocks.
   Returns the count of directives applied."
  (with-input-from-string (in text)
    (load-config-from-stream in)))

(defun %env-set-p (env-string)
  "True when environment variable string ENV-STRING is set and non-empty."
  (and env-string (plusp (length env-string))))

(defun %config-path-from (override xdg home)
  "Resolve the config-file path from environment values (OVERRIDE = $CL_TMUX_CONF,
   XDG = $XDG_CONFIG_HOME, each a string or NIL) and HOME (a directory pathname).

   Precedence (XDG Base Directory spec):
     1. $CL_TMUX_CONF                              — explicit override
     2. $XDG_CONFIG_HOME/cl-tmux/cl-tmux.conf
     3. ~/.config/cl-tmux/cl-tmux.conf             — XDG default when unset
   Empty strings are treated as unset.  Pure: no I/O, no environment access."
  (if (%env-set-p override)
      (pathname override)
      (let ((base (if (%env-set-p xdg)
                      xdg
                      (namestring (merge-pathnames ".config/" home)))))
        (pathname (format nil "~A/cl-tmux/cl-tmux.conf"
                          (string-right-trim "/" base))))))

(defun %tmux-conf-paths (home)
  "Return a list of candidate .tmux.conf paths in priority order:
     1. $XDG_CONFIG_HOME/tmux/tmux.conf
     2. ~/.config/tmux/tmux.conf  (XDG default)
     3. ~/.tmux.conf              (traditional location)"
  (let* ((xdg  (sb-ext:posix-getenv "XDG_CONFIG_HOME"))
         (base (if (%env-set-p xdg)
                   xdg
                   (namestring (merge-pathnames ".config/" home)))))
    (list (pathname (format nil "~A/tmux/tmux.conf"
                            (string-right-trim "/" base)))
          (merge-pathnames ".tmux.conf" home))))

(defun config-file-path ()
  "Path to the user config file, honoring $CL_TMUX_CONF then the XDG Base
   Directory spec ($XDG_CONFIG_HOME, default ~/.config).  See %config-path-from."
  (%config-path-from (sb-ext:posix-getenv "CL_TMUX_CONF")
                     (sb-ext:posix-getenv "XDG_CONFIG_HOME")
                     (user-homedir-pathname)))

(defun load-config-file (&optional (path (config-file-path)))
  "Load and apply the config file at PATH if it exists (returns the count of
   directives applied), or NIL when no file is found.
   PATH defaults to the XDG/cl-tmux path; pass NIL to auto-detect, which also
   searches the standard .tmux.conf locations for compatibility."
  (if path
      (with-open-file (in path :direction :input :if-does-not-exist nil)
        (when in (load-config-from-stream in)))
      ;; Auto-detect: try each candidate path in priority order.
      (let ((home (user-homedir-pathname)))
        (dolist (candidate (cons (config-file-path)
                                 (%tmux-conf-paths home)))
          (with-open-file (in candidate :direction :input :if-does-not-exist nil)
            (when in
              (return (load-config-from-stream in))))))))
