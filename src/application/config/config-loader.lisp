(in-package #:cl-tmux/config)

;;; -- Directive dispatch + comment stripping + single-line application --------
;;;
;;; apply-config-directive routes one token list to the right directive handler;
;;; %strip-config-comment and apply-config-line turn a raw config-file LINE into
;;; the token list that apply-config-directive consumes.  The %if/%elif/%else/
;;; %endif preprocessor and multi-line joining live in config-preprocessor.lisp;
;;; config-file path resolution and top-level file loading live in
;;; config-paths.lisp.

(defun apply-config-directive (tokens)
  "Apply one parsed config directive (list of string TOKENS) to live state.
   Returns T when applied, NIL for an unknown/invalid directive.
   Handles bind/unbind, set-hook, set[-g|-a|-s|-u|...], set-environment [-u|-r],
   if-shell, run-shell [-b|-C|-t|-d], source-file, and the fixed-arity
   directive table."
  (when tokens
    (let ((cmd  (first tokens))
          (args (rest tokens)))
      (if (string= cmd "set-environment")
          (%apply-set-environment-directive cmd args)
          (or (%apply-key-directive cmd args)
              (%apply-if-shell-directive cmd args)
              (%apply-set-directive cmd args)
              (%apply-set-hook-directive cmd args)
              (%apply-run-shell-directive cmd args)
              (%apply-source-file-directive cmd args)
              (%apply-config-directive-inner tokens))))))

(defun %at-comment-start-p (line i len)
  "Return T when position I in LINE is a '#' that begins a comment.
   A '#' is a comment start only when it is NOT:
     - followed by '#' (escaped literal ##),
     - followed by '{', '(', or '[' (tmux format construct #{...} #(...) #[...]),
     - in the middle of an unquoted word (i.e. the preceding character is not
       whitespace), matching tmux's cmd-parse.y lexer which only enters comment
       scanning at a token boundary.
   Assumes the caller has already established that LINE[I] = '#' and that we are
   not currently inside a quoted span."
  (let ((next-i (1+ i)))
    (cond
      ;; ## — escaped literal #, not a comment.
      ((and (< next-i len) (char= (char line next-i) #\#)) nil)
      ;; #{ / #( / #[ — a format construct, not a comment.
      ((and (< next-i len) (member (char line next-i) '(#\{ #\( #\[))) nil)
      ;; A # in the MIDDLE of an unquoted word (preceded by a non-whitespace char)
      ;; is a literal character — e.g. bg=#0000ff or @var=#abc.
      ((and (> i 0)
            (let ((prev (char line (1- i))))
              (not (or (char= prev #\Space) (char= prev #\Tab)))))
       nil)
      ;; At a token boundary: this '#' begins a comment.
      (t t))))

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
                  (if (%at-comment-start-p line i len)
                      (return (string-right-trim '(#\Space #\Tab) (subseq line 0 i)))
                      ;; ## — skip the doubled hash so the next iteration sees the
                      ;; second # as a plain character, not another comment candidate.
                      (when (and (< (1+ i) len) (char= (char line (1+ i)) #\#))
                        (incf i)))))
             (incf i))
          finally (return line))))

(defparameter *config-semicolon-owning-verbs*
  '("bind" "bind-key" "if-shell")
  "Config verbs that own their internal \";\" tokens: they receive the full
   token list including bare \";\" command separators because they split the
   sequence themselves (bind/bind-key multi-command sequences, if-shell
   then/else command blocks).  apply-config-line must NOT pre-split a line on
   a top-level \";\" for these verbs — tmux's cmd-parse.y treats them the same
   way.")

(defun apply-config-line (line)
  "Apply a single config LINE.  Blank lines and # comments (full-line and inline,
   respecting quotes and #{...} formats) are ignored.  Returns T when applied.

   A top-level unescaped \";\" separates command sequences and each segment is
   dispatched in order (tmux's cmd-parse.y: a bare \";\" is a command separator,
   while \"\\;\" is a literal \";\").  %config-tokens already collapses a
   backslash-escaped \"\\;\" into a literal \";\" joined to the adjacent token, so
   only genuinely-standalone \";\" tokens split.  Verbs that own their \";\" bodies
   (see *config-semicolon-owning-verbs*) are exempt: they receive the full token
   list unchanged."
  (let ((trimmed (string-trim '(#\Space #\Tab #\Return #\Newline)
                              (%strip-config-comment line))))
    (and (plusp (length trimmed))
         (let ((tokens (%config-tokens trimmed)))
           (if (member (first tokens) *config-semicolon-owning-verbs*
                       :test #'string=)
               (apply-config-directive tokens)
               (let ((applied nil))
                 (dolist (segment (%split-on-semicolons tokens))
                   (when (and segment (apply-config-directive segment))
                     (setf applied t)))
                 applied))))))
