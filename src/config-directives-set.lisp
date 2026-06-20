(in-package #:cl-tmux/config)

;;; -- Simple directive instantiation + set-option flag handling ---------------
;;;
;;; define-config-directives call (the fixed-arity directive table) and the full
;;; set-option flag handling machinery: %set-directive-p, %strip-set-flags,
;;; %coerce-set-value, %route-set-value, %apply-set-directive, option side effects,
;;; and %apply-set-hook-directive.

(declaim (special cl-tmux/options:*global-options*
                  cl-tmux/options:*server-options*
                  cl-tmux/model:*update-environment*))

;;; ── Simple directive definitions ─────────────────────────────────────────
;;;
;;; The six set-option variants (set, set-option, setw, set-window-option,
;;; sets, set-session-option) all forward to cl-tmux/options:set-option at
;;; config-file load time, because no session/window/pane context is available
;;; during config parsing.
;;;
;;; Runtime commands that carry a window or pane context should call
;;; cl-tmux/options:set-option-for-window / set-option-for-pane directly to
;;; store in the per-struct local-options hash.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +set-directive-aliases+
    '("set" "set-option" "setw" "set-window-option" "sets" "set-session-option")))

(define-config-directives
  ("set-shell" 1 (path)
    (setf *default-shell* path)
    t)
  ("set-status-height" 1 (n)
    (let ((height (cl-tmux::%parse-integer-or-nil n :junk-allowed t)))
      (when (and height (plusp height))
        (setf *status-height* height)
        t)))
  (:aliases +set-directive-aliases+
    2 (option-name option-value)
    (cl-tmux/options:set-option option-name option-value)
    t)
  ;; NOTE: set-hook is handled entirely by %apply-set-hook-directive (stores raw
  ;; command strings for format expansion at fire time); no entry needed here.
  ;; NOTE: source-file is handled entirely by %apply-source-file-directive
  ;; (wired into apply-config-directive before this table) to support -q/-n/-v
  ;; flags, glob patterns, and multiple paths.
  ;; NOTE: run-shell is handled entirely by %apply-run-shell-directive
  ;; (wired into apply-config-directive before this fixed-arity table), which
  ;; covers the bare 1-arg form as well as the flag-bearing forms.  No fixed-
  ;; arity entries are needed here.
  ;; set-environment 2-arg form: VAR VALUE (no flags).
  ;; The %apply-set-environment-directive handler in apply-config-directive
  ;; owns this command family; these entries are retained for the fixed-arity
  ;; inner dispatcher but are not reached through apply-config-directive.
  ("set-environment" 2 (var-name var-value)
    (%config-setenv var-name var-value)
    t))

;;; ── set-option flag handling (set -g / -a / -s / ...) ──────────────────────
;;;
;;; The fixed-arity directive table cannot match `set -g status off` (3 tokens vs
;;; arity 2), so the canonical .tmux.conf form silently failed.  %apply-set-
;;; directive consumes leading scope flags:
;;;   -g global (default)  -s server  -w window  -o only-if-unset
;;;   -a append  -u unset
;;; -s routes the write to *server-options* instead of *global-options*.
;;;
;;; The set-verb list is shared via +set-directive-aliases+ rather than
;;; duplicated in the directive table and predicate.

(defun %set-directive-p (cmd)
  "Return T when CMD is one of the standard set-option directive verbs."
  (member cmd +set-directive-aliases+
          :test #'string=))

(defparameter +unsupported-set-option-names+
  '("terminal-overrides" "terminal-features")
  "Options whose tmux syntax implies behavior cl-tmux does not implement.")

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
   -p (pane), -o (only-if-unset — accepted, not enforced).  These scope
   flags cannot be applied to per-object instances at config-load time
   because no window or pane context exists yet; options fall through to
   the global store so they take effect at the nearest practical scope.
   POSITIONALS is the remaining non-flag tokens (name and optional value)."
  (let ((flag-present-p nil)
        (append-p       nil)
        (server-p       nil)
        (unset-p        nil)
        (format-p       nil)
        (remaining      args))
    (loop while (and remaining
                     (let ((tok (first remaining)))
                       (and (>= (length tok) 2) (char= (char tok 0) #\-))))
          do (let ((tok (pop remaining)))
               (setf flag-present-p t)
               (when (%flag-token-contains-any-p tok '(#\a)) (setf append-p t))
               (when (%flag-token-contains-any-p tok '(#\s)) (setf server-p t))
               (when (%flag-token-contains-any-p tok '(#\u)) (setf unset-p  t))
               ;; -F: expand the value as a format string before storing.
               (when (%flag-token-contains-any-p tok '(#\F)) (setf format-p t))
               ;; -g, -w, -p, -o, -q: accepted silently.
               ))
    (values flag-present-p append-p server-p unset-p format-p remaining)))

(defun %coerce-set-value (raw-value format-p)
  "Coerce RAW-VALUE for storage.  When FORMAT-P is T, expand it as a format
   string using a minimal context (hostname + cl-tmux version); on expansion
   failure the raw string is returned unchanged.  Pure: no side-effects."
  (if format-p
      (let ((ctx (list :hostname (machine-instance)
                       :version (cl-tmux/version:version-string))))
        (handler-case
            (cl-tmux/format:expand-format raw-value ctx)
          (error () raw-value)))
      raw-value))

(defun %set-option-accessors (server-p)
  "Return the getter, setter, and storage table for the requested scope."
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
      (%set-option-accessors server-p)
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
  "Apply a flag-bearing set-family directive (e.g. `set -g status off`,
   `set -s escape-time 0`, `set -ag word-separators x`).
   Routes -s writes to *server-options*; handles -a (append) and -u (unset).
   Returns T when applied; NIL when CMD is not a set verb or carries no flags."
  (when (%set-directive-p cmd)
    (multiple-value-bind (flag-present-p append-p server-p unset-p format-p positionals)
        (%strip-set-flags args)
      (when (and flag-present-p (first positionals))
        (let ((name      (first positionals))
              (raw-value (%join-config-tokens (rest positionals))))
          (unless (%unsupported-set-option-p name)
            (let ((value (%coerce-set-value raw-value format-p)))
              (%route-set-value name value server-p append-p unset-p)
              ;; Side-effect: intercept special options that need runtime state updates.
              (%apply-option-side-effects name value unset-p)
              t)))))))

;;; ── Option side-effect helpers ───────────────────────────────────────────────

(defun %nonempty-string-p (x)
  "T when X is a non-empty string."
  (and (stringp x) (plusp (length x))))

(defun %bind-prefix-key (value key-code-var)
  "Parse VALUE as a prefix key; when valid, store the byte in KEY-CODE-VAR (a special-var symbol)
   and arm that key in the prefix table."
  (let ((byte (%parse-prefix-key value)))
    (when byte
      (setf (symbol-value key-code-var) byte)
      (key-table-bind +table-prefix+ (code-char byte) :send-prefix))))

;;; ── Declarative option-side-effect dispatch ──────────────────────────────────
;;;
;;; define-option-side-effect-handlers builds %apply-option-side-effects from a
;;; Prolog-style fact table: one (NAME-STRING &body BODY) arm per option.  Each arm
;;; is guarded by (string= name NAME-STRING); VALUE is bound in BODY.  This matches
;;; define-csi-rules / define-config-directives in style.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %expand-option-side-effect-rule (rule)
    "Expand one option side-effect RULE into a list of COND clauses."
    (if (eq (first rule) :any-of)
        (destructuring-bind (names &body body) (rest rule)
          `((member name ',names :test #'string=) ,@body))
        (destructuring-bind (name-string &body body) rule
          `((string= name ,name-string) ,@body)))))

(defmacro define-option-side-effect-handlers (&rest rules)
  "Build %APPLY-OPTION-SIDE-EFFECTS from a declarative table of RULES.
   Each RULE has the form:
     (NAME-STRING &body BODY)   — NAME-STRING matched via STRING=; VALUE bound in BODY.
     (:any-of (NAME...) &body BODY) — VALUE bound in BODY when NAME is one of the list.
  Generates a COND dispatch over NAME."
  `(defun %apply-option-side-effects (name value unset-p)
     "Apply runtime side-effects for options that touch non-option state.
      Dispatches on NAME; VALUE holds the new option value string."
     (declare (ignorable value unset-p))
     (cond
       ,@(mapcar #'%expand-option-side-effect-rule rules))))

(define-option-side-effect-handlers
  ;; prefix / prefix2: parse and arm the key in the prefix table.
  ("prefix"
   (if unset-p
       (setf *prefix-key-code* +prefix-key-code+)
       (%bind-prefix-key value '*prefix-key-code*)))
  ("prefix2"
   (if unset-p
       (setf *prefix2-key-code* nil)
       (%bind-prefix-key value '*prefix2-key-code*)))
  ;; default-shell: update the shell used for new panes immediately.
  ("default-shell"
   (if unset-p
       (setf *default-shell* "/bin/sh")
       (when (%nonempty-string-p value)
         (setf *default-shell* value))))
  ;; escape-time: sync into server-options so every set form takes effect.
  ("escape-time"
   (if unset-p
       (cl-tmux/options:set-server-option "escape-time" 500)
       (when (%nonempty-string-p value)
         (cl-tmux/options:set-server-option "escape-time" value))))
  ;; status: off/false/0 hides the bar; numeric line count (capped at 5) or on/true → 1.
  ("status"
   (if unset-p
       (setf *status-height* 1)
       (let* ((off-p (member value '("off" "false" "0") :test #'equal))
              (n     (cl-tmux::%parse-integer-or-nil value :junk-allowed t)))
         (setf *status-height*
               (cond (off-p 0)
                     ((and n (> n 0)) (min n 5))
                     (t 1))))))
  ;; mouse: delegate to *mouse-reporting-hook* so config and renderer stay decoupled.
  ("mouse"
   (when *mouse-reporting-hook*
     (let ((on-p (and (not unset-p)
                      (member value '("on" "true" "1") :test #'equal))))
       (ignore-errors (funcall *mouse-reporting-hook* (and on-p t))))))
  ;; update-environment: propagate the space-separated variable list into the model.
  ("update-environment"
   (if unset-p
       (setf cl-tmux/model:*update-environment*
             (copy-list cl-tmux/model:+default-update-environment+))
       (when (%nonempty-string-p value)
         (setf cl-tmux/model:*update-environment*
               (remove-if (lambda (s) (zerop (length s)))
                          (uiop:split-string value :separator '(#\Space))))))))

(defun %apply-set-hook-directive (cmd args)
  "Handle 'set-hook [-r] [-u] event [command]' directives.
   -r or -u flag removes/unsets all hooks for the event; without them, registers
   the command.  The command is stored as a raw string (not converted to keyword)
   so that format variables and arguments (e.g. 'display-message #{session_name}')
   are expanded at hook-fire time via %run-command-line.
   Returns T when handled, NIL otherwise."
  (when (string= cmd "set-hook")
    ;; Consume ALL leading -X flags (not just -r/-u): -g/-a/-R are accepted and
    ;; skipped so `set-hook -g <event> <cmd>` registers EVENT, not "-g".
    (let* ((remove-p nil)
           (rest     (let ((remaining args))
                       (setf remaining
                             (%consume-leading-flag-tokens
                              remaining
                              (lambda (tok rest)
                                (when (member tok '("-r" "-u") :test #'string=)
                                  (setf remove-p t))
                                (values rest t))))
                       remaining))
           (event    (first rest))
           ;; The command may be a single quoted token or split across tokens;
           ;; join all remaining tokens as a single command line string.
           (cmd-str  (%join-config-tokens (rest rest))))
      (when event
        (if remove-p
            (progn (cl-tmux/hooks:clear-command-hooks event) t)
            (when cmd-str
              ;; Store the raw command string for execution at hook-fire time.
              (cl-tmux/hooks:set-command-hook event cmd-str)
              t))))))
