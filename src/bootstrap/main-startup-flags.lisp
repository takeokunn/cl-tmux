;;; Startup flag parsing helpers.
;;;
;;; This file owns the shared macro used by startup-mode parsers plus the
;;; attach-session flag parser generated from it.

(in-package :cl-tmux)

;;; ── Flag-parser macro ────────────────────────────────────────────────────────
;;;
;;; define-flag-parser generates a parser for a set of boolean and value flags.
;;; Each FLAG-SPEC is one of:
;;;   (:bool  "flag-string"  variable-name)   — sets variable-name to T
;;;   (:value "flag-string"  variable-name)   — sets variable-name to the next arg
;;; The macro generates a loop over the args vector and produces a multi-value
;;; return of all variables in declaration order.
;;;
;;; The generated cond has a final error arm.  Startup flag parsers are strict:
;;; each argument must be declared in FLAG-SPECS, and unknown flags are
;;; rejected instead of being silently treated as positional input.

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun %flag-parser-clause (spec arg-sym args-sym index-sym)
  "Return the COND clause that handles SPEC for a generated flag parser."
  (ecase (first spec)
    (:bool
     (destructuring-bind (_ flag variable) spec
       (declare (ignore _))
       `((string= ,arg-sym ,flag)
         (setf ,variable t)
         (incf ,index-sym))))
    (:value
     (destructuring-bind (_ flag variable) spec
       (declare (ignore _))
       `((string= ,arg-sym ,flag)
         (incf ,index-sym)
         (when (< ,index-sym (length ,args-sym))
           (setf ,variable (nth ,index-sym ,args-sym))
           (incf ,index-sym))))))))

(defmacro define-flag-parser (parser-name (&rest defaults) &rest flag-specs)
  "Define PARSER-NAME as a function (ARGS) → (values ...) that parses FLAGS.
   DEFAULTS is a list of (variable-name default-value) bindings.
   FLAG-SPECS are (:bool FLAG VAR) or (:value FLAG VAR) declarations.
   Unknown flags signal an error; callers must declare every accepted flag."
  (let ((args-sym   (gensym "ARGS"))
        (index-sym  (gensym "INDEX"))
        (arg-sym    (gensym "ARG"))
        (var-names  (mapcar #'first defaults)))
    `(defun ,parser-name (,args-sym)
       ,(format nil "Generated flag parser for: ~{~A~^, ~}"
                (mapcar #'second flag-specs))
         (let (,@defaults
               (,index-sym 0))
           (loop while (< ,index-sym (length ,args-sym)) do
             (let ((,arg-sym (nth ,index-sym ,args-sym)))
               (cond
               ,@(mapcar (lambda (spec)
                           (%flag-parser-clause spec arg-sym args-sym index-sym))
                         flag-specs)
               (t (error "Unknown flag ~A for ~A" ,arg-sym ',parser-name)))))
         (values ,@var-names)))))

(define-flag-parser %parse-attach-flags
    ((name "0") (detach nil) (read-only-p nil))
  (:value "-t" name)
  (:bool  "-d" detach)
  (:bool  "-r" read-only-p))

(define-flag-parser %parse-new-session-flags
    ((name nil) (win-name nil) (detach nil) (start-dir nil))
  (:value "-s" name)
  (:value "-n" win-name)
  (:bool  "-d" detach)
  (:value "-c" start-dir))

;;; ── Global CLI flags (cl-cli) ────────────────────────────────────────────────
;;;
;;; `cl-tmux [flags] [command [flags]]` mirrors real tmux(1) (verified against
;;; `man 1 tmux`, tmux 3.7b: usage `tmux [-2CDhlNuVv] [-c shell-command]
;;; [-f file] [-L socket-name] [-S socket-path] [-T features] [command
;;; [flags]]`).  Global flags may appear in ANY order before the command word.
;;;
;;; This replaces main() calling %consume-global-socket-flags directly (still
;;; kept, and still covered by its own unit tests, for -L/-S callers that want
;;; the narrow hand-rolled scanner) with a real option parser, fixing a real
;;; bug: previously `cl-tmux -L sock -C` failed with a usage error because -L
;;; wasn't a *startup-modes* name and -C only worked as argv's first token.
;;;
;;; Flags with real, additional effects: -L/-S (socket), -f (config file, see
;;; config-paths.lisp), -2 (256-colour downsampling, see renderer-format.lisp
;;; *color-downsample-fn*), -C/-CC (control mode), -V (version), -h (usage).
;;; Flags accepted for tmux(1) compatibility with no further behaviour wired
;;; up: -D, -N, -T, -c, -u, -v (real tmux's own -l is likewise documented as
;;; "currently has no effect").  Accepting-without-erroring is still a real
;;; improvement: today every one of these is a fatal "unknown flag" error.

(defparameter *cli-app*
  (cl-cli:make-app
   :name "cl-tmux"
   :summary "A tmux-compatible terminal multiplexer."
   :auto-help nil ; -h/-V dispatch through run-usage/run-version below, not cl-cli's own help/version machinery, to keep their exact existing output.
   :global-options
   (list (cl-cli:make-option :name "socket-name"    :short #\L :kind :value)
         (cl-cli:make-option :name "socket-path"    :short #\S :kind :value)
         (cl-cli:make-option :name "file"           :short #\f :kind :value)
         (cl-cli:make-option :name "force-256"      :short #\2 :kind :flag)
         (cl-cli:make-option :name "control"        :short #\C :kind :count)
         (cl-cli:make-option :name "no-daemonize"   :short #\D :kind :flag)
         (cl-cli:make-option :name "no-start-server" :short #\N :kind :flag)
         (cl-cli:make-option :name "login-shell"    :short #\l :kind :flag)
         (cl-cli:make-option :name "utf8"           :short #\u :kind :flag)
         (cl-cli:make-option :name "features"       :short #\T :kind :value)
         (cl-cli:make-option :name "shell-command"  :short #\c :kind :value)
         (cl-cli:make-option :name "verbose"        :short #\v :kind :count)
         ;; :key overrides the default derived key (:version / :help), which
         ;; cl-cli reserves for its own built-in --version/--help dispatch
         ;; even with :auto-help nil (see %validate-user-option-keys).
         (cl-cli:make-option :name "version" :short #\V :kind :flag :key :print-version)
         (cl-cli:make-option :name "help"    :short #\h :kind :flag :key :print-help))
   :positionals (list (cl-cli:make-positional :key :mode-args :rest-p t)))
  "The root cl-cli app for cl-tmux's global startup flags.  See main()
   (main-startup.lisp), which also defines %parse-global-cli-argv /
   %apply-global-cli-invocation / %dispatch-global-cli-flag-actions — placed
   there rather than here because they call run-version / run-usage /
   run-control-mode / %usage-string, all defined later in the load order
   (main-startup-commands.lisp, main-startup.lisp).")
