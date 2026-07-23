(in-package #:cl-tmux/config)

;;; -- %if / %elif / %else / %endif preprocessor + line joining ----------------
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
;;;
;;; This file also owns the multi-line joining that must happen before a line
;;; is handed to apply-config-line: trailing-backslash continuation lines and
;;; tmux 3.x brace { ... } command blocks.  The top-level stream/string entry
;;; points live here too.

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
        (and result (not (member result '("" "0") :test #'string=))))
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
   inside single/double quotes or immediately after a backslash.  Used to
   detect and join multi-line { ... } command blocks (tmux 3.x brace syntax)."
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

(defun %config-cond-stack-active-p (cond-stack)
  "True when every nested config condition is currently active."
  (every (lambda (state) (eq state :active)) cond-stack))

(defun %update-config-cond-stack (pp-type trimmed cond-stack)
  "Compute the new COND-STACK for a preprocessor line of type PP-TYPE.
   Returns a fresh list; does not mutate the input.
   States: :ACTIVE (this branch is taken), :SEEKING (no branch matched yet),
   :TAKEN (a branch already matched; skip remaining), :DEAD (outer block skipped)."
  (case pp-type
    (:if
     ;; Push a new level.  When an outer level is not :active, the nested %if is
     ;; :dead regardless of the condition (its body is already being skipped).
     (let* ((cond-str (string-trim " \t" (subseq trimmed 3)))
            (new-state (cond ((not (%config-cond-stack-active-p cond-stack)) :dead)
                             ((%eval-config-condition cond-str) :active)
                             (t :seeking))))
       (cons new-state cond-stack)))
    (:elif
     ;; Transition the top-of-stack state; leave lower levels unchanged.
     (if (null cond-stack)
         cond-stack
         (let* ((cond-str (string-trim " \t" (subseq trimmed 5)))
                (old-top  (first cond-stack))
                (new-top  (case old-top
                            (:seeking (if (%eval-config-condition cond-str) :active :seeking))
                            (:active  :taken)
                            (t        old-top))))
           (cons new-top (rest cond-stack)))))
    (:else
     ;; Transition the top-of-stack state for the else branch.
     (if (null cond-stack)
         cond-stack
         (let* ((old-top (first cond-stack))
                (new-top (case old-top
                           (:seeking :active)
                           (:active  :taken)
                           (t        old-top))))
           (cons new-top (rest cond-stack)))))
    (:endif
     ;; Pop the innermost level.
     (rest cond-stack))
    (otherwise
     cond-stack)))

(defun %apply-config-logical-line (line stream cond-stack)
  "Apply LINE when the current COND-STACK is active."
  (when (%config-cond-stack-active-p cond-stack)
    ;; Join a multi-line { ... } command block into one logical line.
    (let ((full-line (if (> (%line-brace-delta line) 0)
                         (%read-brace-block line stream)
                         line)))
      (apply-config-line full-line))))

;;; Public config loaders.  These are exported (package-core.lisp) and called
;;; from config-paths.lisp (load-config-file) and the test suite.  They were
;;; dropped by an earlier file-split refactor while their exports and callers
;;; remained; restored here so config-file loading (and its tests) work.

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
    (loop for raw = (read-line stream nil nil)
          while raw
          for line = (%strip-config-comment
                      (%read-logical-config-line raw stream)) do
            (let* ((trimmed (string-trim '(#\Space #\Tab #\Return #\Newline) line))
                   (pp-type (%preprocessor-line-p trimmed)))
              (if pp-type
                  (setf cond-stack
                        (%update-config-cond-stack pp-type trimmed cond-stack))
                  (when (%apply-config-logical-line line stream cond-stack)
                    (incf count)))))
    count))

(defun load-config-from-string (text)
  "Apply every directive line in TEXT, honoring %if/%else/%endif blocks.
   Returns the count of directives applied."
  (with-input-from-string (in text)
    (load-config-from-stream in)))
