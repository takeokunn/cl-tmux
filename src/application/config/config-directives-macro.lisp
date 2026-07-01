(in-package #:cl-tmux/config)

;;; ── Runtime sb-posix helpers ─────────────────────────────────────────────────
;;; ── Renderer mouse-reporting hook ────────────────────────────────────────────
;;;
;;; %apply-option-side-effects must call the renderer to enable/disable mouse
;;; reporting when the 'mouse' option changes, but the config layer cannot carry
;;; a compile-time dependency on cl-tmux/renderer (circular).  A registered
;;; callback (consistent with *command-hook-runner*) is the solution: the
;;; renderer/orchestrate layer sets this at startup; config calls it without
;;; knowing who owns the terminal.

(defvar *mouse-reporting-hook* nil
  "When non-NIL, a function (enable-p) called whenever the 'mouse' option changes.
   ENABLE-P is T to enable mouse reporting, NIL to disable it.  Set by the
   orchestrate layer (events-loop or main.lisp) to cl-tmux/renderer:enable/disable.")

;;; ── Environment-variable helper ─────────────────────────────────────────────
;;;
;;; set-environment directives need to mutate the process environment.
;;; SB-POSIX is looked up lazily at call time — it is not an ASDF dependency of
;;; cl-tmux so it may not be loaded when this file first loads, but it IS loaded
;;; before any runtime or test caller reaches these functions.

(defun %config-posix-fn (name)
  "Return the SB-POSIX function named NAME (by call-time find-symbol), or NIL
   when SB-POSIX is absent or the function is not exported.  Defers the lookup
   so the defvar-at-load-time NIL trap is avoided."
  (let ((pkg (find-package "SB-POSIX")))
    (and pkg (find-symbol name pkg))))

(defun %config-setenv (name value)
  "Set environment variable NAME to VALUE for child processes.
   Looks up SB-POSIX:SETENV lazily.  A no-op when sb-posix is absent."
  (let ((fn (%config-posix-fn "SETENV")))
    (when fn (ignore-errors (funcall fn name value 1)))))

;;; ── run-shell tilde expansion helper ─────────────────────────────────────────

(defun %expand-leading-tilde (cmd)
  "Expand a leading \"~/\" in CMD to \"$HOME/\" using the HOME environment
   variable, so `run '~/.tmux/plugins/tpm/tpm'` resolves to the user's home.
   Leaves absolute (\"/abs\") and relative (\"rel\") strings unchanged.  Pure
   string transformation: returns CMD unchanged when it does not begin with ~/."
  (if (and (> (length cmd) 2)
           (char= (char cmd 0) #\~)
           (char= (char cmd 1) #\/))
      (concatenate 'string
                   (or (ignore-errors (sb-ext:posix-getenv "HOME")) "~")
                   (subseq cmd 1))
      cmd))

(defun %flag-token-contains-any-p (tok flags)
  "Return T when TOK contains any character from FLAGS."
  (and (stringp tok)
       (some (lambda (flag) (find flag tok :test #'char=)) flags)))

(defun %join-config-tokens (tokens)
  "Join TOKENS into a single space-separated string.
   Returns NIL for an empty token list."
  (when tokens
    (format nil "~{~A~^ ~}" tokens)))

(defun %leading-flag-token-p (tok &key (allow-single-dash nil))
  "Return T when TOK looks like a leading directive flag token."
  (and (stringp tok)
       (if allow-single-dash
           (and (> (length tok) 0) (char= (char tok 0) #\-))
           (and (> (length tok) 1) (char= (char tok 0) #\-)))))

(defun %consume-leading-flag-tokens (tokens consumer
                                     &key (allow-single-dash nil))
  "Consume leading flag TOKENS using CONSUMER.
   CONSUMER is called as (funcall CONSUMER FLAG-TOKEN REST) and must return two
   values: the updated REST token list and a generalized boolean that says
   whether scanning should continue."
  (loop while (and tokens
                   (%leading-flag-token-p (first tokens)
                                          :allow-single-dash allow-single-dash))
        do (multiple-value-bind (next-tokens continue-p)
               (funcall consumer (pop tokens) tokens)
             (setf tokens next-tokens)
             (unless continue-p (return))))
  tokens)

(defmacro %consuming-flags ((tokens tok rest) &body cond-clauses)
  `(%consume-leading-flag-tokens
    ,tokens
    (lambda (,tok ,rest)
      (cond ,@cond-clauses)
      (values ,rest t))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %resolve-config-directive-names (names)
    "Return the directive NAMES list, allowing a named list symbol."
    (cond
      ((symbolp names)
       (let ((value (symbol-value names)))
         (unless (listp value)
           (error "Directive alias symbol ~S does not name a list." names))
         value))
      ((listp names) names)
      (t
       (error "Directive alias list must be a list or symbol, got ~S." names)))))

(defun %expand-config-directive-rule (rule)
  "Expand one directive RULE into a list of COND clauses."
  (if (eq (first rule) :aliases)
      ;; (:aliases (name...) arity arglist body...)
      (destructuring-bind (names arity arglist &body body) (rest rule)
        (let ((names (%resolve-config-directive-names names)))
          (mapcar (lambda (name)
                    `((and (string= cmd ,name) (= (length args) ,arity))
                      (destructuring-bind ,arglist args
                        (declare (ignorable ,@arglist))
                        ,@body)))
                  names)))
      ;; (name arity arglist body...)
      (destructuring-bind (name arity arglist &body body) rule
        (list `((and (string= cmd ,name) (= (length args) ,arity))
                (destructuring-bind ,arglist args
                  (declare (ignorable ,@arglist))
                  ,@body))))))

;;; ── Declarative directive dispatch macro ──────────────────────────────────

(defmacro define-config-directives (&rest rules)
  "Build %APPLY-CONFIG-DIRECTIVE-INNER from a declarative table of directive RULES.

   Each RULE has one of two forms:
     (NAME ARITY (ARG...) &body BODY)
       NAME   – the directive keyword as a string (e.g. \"set-shell\")
       ARITY  – the exact number of arguments the directive takes
       (ARG…) – symbols bound to those arguments inside BODY
       BODY   – forms run when NAME matches with the right ARITY; their value is
                returned (non-NIL ⇒ the directive was applied).

     (:aliases (NAME...) ARITY (ARG...) &body BODY)
       Identical to the single-name form except CMD matches any string in (NAME...).
       Eliminates alias repetition when several spellings intentionally map to
       the same directive implementation.

   The outer APPLY-CONFIG-DIRECTIVE function wraps this inner dispatcher and
   handles 'bind' with variable-arity flags separately."
  `(defun %apply-config-directive-inner (tokens)
     "Apply one non-bind config directive (list of string TOKENS) to live state.
      Returns T when applied, NIL for an unknown/invalid directive."
     (when tokens
       (let ((cmd (first tokens)) (args (rest tokens)))
         (declare (ignorable args))
         (cond
           ,@(mapcan #'%expand-config-directive-rule rules)
           (t nil))))))
