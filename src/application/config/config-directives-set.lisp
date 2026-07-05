(in-package #:cl-tmux/config)

;;; -- Simple directive instantiation + set-option flag handling ---------------
;;;
;;; define-config-directives call (the fixed-arity directive table) and the full
;;; set-option flag handling machinery: %set-directive-p, %strip-set-flags,
;;; %coerce-set-value, %route-set-value, %apply-set-directive.
;;; Option runtime side effects and set-hook live in
;;; config-option-side-effects.lisp.

(declaim (special cl-tmux/options:*global-options*
                  cl-tmux/options:*server-options*))

;;; ── Simple directive definitions ─────────────────────────────────────────
;;;
;;; The set-option canonical commands all forward to cl-tmux/options:set-option
;;; at config-file load time, because no session/window/pane context is
;;; available during config parsing.
;;;
;;; Runtime commands that carry a window or pane context should call
;;; cl-tmux/options:set-option-for-window / set-option-for-pane directly to
;;; store in the per-struct local-options hash.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +set-directive-commands+
    '("set-option" "set-window-option" "set-session-option"))

  (defun %set-directive-config-rule (command-name)
    "Return the canonical fixed-arity config rule for one set-family command."
    `(,command-name 2 (option-name option-value)
      (cl-tmux/options:set-option option-name option-value)
      t)))

(defmacro define-core-config-directives ()
  "Define fixed-arity config directives from canonical directive facts."
  `(define-config-directives
     ("set-shell" 1 (path)
       (setf *default-shell* path)
       t)
     ("set-status-height" 1 (n)
       (let ((height (cl-tmux::%parse-integer-or-nil n :junk-allowed t)))
         (when (and height (plusp height))
           (setf *status-height* height)
           t)))
     ,@(mapcar #'%set-directive-config-rule +set-directive-commands+)
     ;; NOTE: set-hook is handled entirely by %apply-set-hook-directive (stores raw
     ;; command strings for format expansion at fire time); no entry needed here.
     ;; NOTE: source-file is handled entirely by %apply-source-file-directive
     ;; (wired into apply-config-directive before this table) to support -q/-n/-v
     ;; flags, glob patterns, and multiple paths.
     ;; NOTE: run-shell is handled entirely by %apply-run-shell-directive
     ;; (wired into apply-config-directive before this fixed-arity table), which
     ;; covers the bare 1-arg form as well as the flag-bearing forms.  No fixed-
     ;; arity entries are needed here.
     ;; NOTE: set-environment is handled entirely by %apply-set-environment-
     ;; directive (config-directives-runtime.lisp), which apply-config-directive
     ;; routes to unconditionally before reaching this fixed-arity table.  No
     ;; entry is needed here.
     ))

(define-core-config-directives)

;;; ── set-option flag handling (-g / -a / -s / ...) ──────────────────────────
;;;
;;; The fixed-arity directive table cannot match `set-option -g status off`
;;; (3 tokens vs arity 2), so %apply-set-directive consumes leading scope flags:
;;;   -g global (default)  -s server  -w window  -o only-if-unset
;;;   -a append  -u unset
;;; -s routes the write to *server-options* instead of *global-options*.
;;;
;;; The set-verb list is shared via +set-directive-commands+ rather than
;;; duplicated in the directive table and predicate.

;;; define-flag-mapping generates a block of (when FLAG-CHAR-P (setf VAR t)) forms
;;; from a declarative (FLAG-CHAR VARIABLE) fact table, matching define-csi-rules
;;; and define-config-directives in style.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %expand-flag-mapping-rule (rule tok-sym)
    "Expand one (FLAG-CHAR VARIABLE) rule into a conditional setf form."
    (destructuring-bind (flag-char variable) rule
      `(when (%flag-token-contains-any-p ,tok-sym '(,flag-char))
         (setf ,variable t)))))

(defmacro define-flag-mapping (tok-sym &rest rules)
  "Expand RULES — each (FLAG-CHAR VARIABLE) — into conditional setf forms.
   TOK-SYM names the token variable in scope at the expansion site."
  `(progn ,@(mapcar (lambda (r) (%expand-flag-mapping-rule r tok-sym)) rules)))

(defun %set-directive-p (cmd)
  "Return T when CMD is one of the canonical set-option directive verbs."
  (member cmd +set-directive-commands+
          :test #'string=))

(defparameter +unsupported-set-option-names+
  '()
  "Options whose tmux syntax implies behavior cl-tmux does not implement.
   Empty since terminal-overrides/terminal-features were un-rejected: real tmux
   ACCEPTS and stores them (they appear in virtually every real .tmux.conf, e.g.
   `set-option -ga terminal-overrides \",xterm-256color:Tc\"`), so rejecting them broke
   config transparency even though cl-tmux applies no terminal-matching
   behavior.  Kept as a named list so a future genuinely-unrepresentable option
   has a documented home.")

(defun %unsupported-set-option-p (name)
  "Return T when NAME is a set-option target cl-tmux must reject."
  (member name +unsupported-set-option-names+ :test #'string=))

(defun %strip-set-flags (args)
  "Consume leading -X flag tokens from a set directive's ARGS.
   Returns (values FLAG-PRESENT-P APPEND-P SERVER-P UNSET-P FORMAT-P POSITIONALS):
     FLAG-PRESENT-P – T when any flag was present
     APPEND-P       – T when -a appeared (append to existing value)
     SERVER-P       – T when -s appeared (route to server-options)
     UNSET-P        – T when -u appeared (remove the option)
     FORMAT-P       – T when -F appeared (expand value as format string)
   Recognised but currently treated as global: -g (global), -w (window),
   -p (pane).  These scope flags cannot be applied to per-object instances at
   config-load time because no window or pane context exists yet; options fall
   through to the global store so they take effect at the nearest practical
   scope.  -q is accepted silently.
   POSITIONALS is the remaining non-flag tokens (name and optional value);
   ONLY-IF-UNSET-P (trailing value) is T when -o appeared."
  (let ((flag-present-p nil)
        (append-p       nil)
        (server-p       nil)
        (unset-p        nil)
        (format-p       nil)
        (only-if-unset-p nil)
        (remaining      args))
    (loop while (and remaining
                     (let ((token (first remaining)))
                       (and (>= (length token) 2) (char= (char token 0) #\-))))
          do (let ((token (pop remaining)))
               (setf flag-present-p t)
               ;; Declarative flag table: each (FLAG-CHAR VARIABLE) arm.
               ;; -g, -w, -p, -q are accepted silently (not listed here).
               (define-flag-mapping token
                 (#\a append-p)
                 (#\s server-p)
                 (#\u unset-p)
                 (#\F format-p)
                 (#\o only-if-unset-p))))
    (values flag-present-p append-p server-p unset-p format-p remaining
            only-if-unset-p)))

(defun %coerce-set-value (raw-value format-p hostname)
  "Coerce RAW-VALUE for storage.  When FORMAT-P is T, expand it as a format
   string using a minimal context (HOSTNAME + cl-tmux version); on expansion
   failure the raw string is returned unchanged.
   HOSTNAME must be pre-computed by the caller to keep this function pure
   (no I/O side-effects)."
  (if format-p
      (let ((ctx (list :hostname hostname
                       :version (cl-tmux/version:version-string))))
        (handler-case
            (cl-tmux/format:expand-format raw-value ctx)
          (error () raw-value)))
      raw-value))

(defun %option-scope-triple (server-p)
  "Return (values GETTER SETTER TABLE) for the option scope indicated by SERVER-P.
   When SERVER-P is true, the server options store is used; otherwise the global
   options store.  The triple is consumed by %route-set-value."
  (if server-p
      (values #'cl-tmux/options:get-server-option
              #'cl-tmux/options:set-server-option
              cl-tmux/options:*server-options*)
      (values #'cl-tmux/options:get-option
              #'cl-tmux/options:set-option
              cl-tmux/options:*global-options*)))

(defun %route-set-value (name value server-p append-p unset-p)
  "Store VALUE under NAME in the appropriate option table, handling -u/-s/-a/-sa.
   Pure routing: all value coercion has already happened."
  (multiple-value-bind (getter setter table)
      (%option-scope-triple server-p)
    (cond
      (unset-p
       (remhash name table))
      (append-p
       (funcall setter
                name
                (cl-tmux/options:append-option-value
                 name (funcall getter name nil) value)))
      (t
       (funcall setter name value)))))

(defun %apply-set-directive (cmd args)
  "Apply a flag-bearing set-family directive (e.g. `set-option -g status off`,
   `set-option -s escape-time 0`, `set-option -ag word-separators x`).
   Routes -s writes to *server-options*; handles -a (append) and -u (unset).
   Returns T when applied; NIL when CMD is not a set verb or carries no flags."
  (when (%set-directive-p cmd)
    (multiple-value-bind (flag-present-p append-p server-p unset-p format-p positionals
                          only-if-unset-p)
        (%strip-set-flags args)
      (when (and flag-present-p (first positionals))
        (let ((name      (first positionals))
              (raw-value (%join-config-tokens (rest positionals))))
          (unless (%unsupported-set-option-p name)
            (if (and only-if-unset-p
                     (not unset-p)
                     (multiple-value-bind (getter setter table)
                         (%option-scope-triple server-p)
                       (declare (ignore getter setter))
                       (nth-value 1 (gethash name table))))
                ;; tmux `set-option -o`: an already-set option is skipped ("already
                ;; set" at config load); the directive itself is handled.
                t
                ;; Hoist (machine-instance) here so %coerce-set-value stays pure.
                ;; The hostname is only needed when format-p is T; compute it lazily.
                (let* ((hostname (when format-p (ignore-errors (machine-instance))))
                       (value    (%coerce-set-value raw-value format-p hostname)))
                  (%route-set-value name value server-p append-p unset-p)
                  ;; Side-effect: intercept special options that need runtime state
                  ;; updates (see config-option-side-effects.lisp).
                  (apply-option-side-effects name value unset-p)
                  t))))))))
