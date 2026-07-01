(in-package #:cl-tmux/config)

;;; -- Option runtime side effects + set-hook directive -------------------------
;;;
;;; %apply-set-directive (in config-directives-set.lisp) writes option values
;;; into the options tables, then calls apply-option-side-effects so that
;;; options which touch non-option runtime state (the prefix key table, the
;;; default shell, the status-bar height, mouse reporting, ...) take effect
;;; immediately.  %apply-set-hook-directive is the unrelated set-hook command
;;; handler; it lives here because it shares no code with the fixed-arity
;;; set-option table in config-directives-set.lisp.

(declaim (special cl-tmux/model:*update-environment*))

;;; ── Option side-effect helpers ───────────────────────────────────────────────

(defun %nonempty-string-p (x)
  "T when X is a non-empty string."
  (and (stringp x) (plusp (length x))))

(defun %bind-prefix-key (value key-code-var)
  "Parse VALUE as a prefix key; when valid, store the byte in KEY-CODE-VAR (a special-var symbol)
   and arm that key in the prefix table.
   VALUE \"None\" explicitly DISABLES the prefix (tmux KEYC_NONE): *prefix2-key-code*
   becomes NIL, *prefix-key-code* resets to the default +prefix-key-code+.
   A NIL parse of any other (unmatchable) name silently no-ops, leaving the prior
   prefix unchanged."
  (cond
    ((string-equal value "None")
     (setf (symbol-value key-code-var)
           (if (eq key-code-var '*prefix2-key-code*) nil +prefix-key-code+)))
    (t
     (let ((byte (%parse-prefix-key value)))
       (when byte
         (setf (symbol-value key-code-var) byte)
         (key-table-bind +table-prefix+ (code-char byte) :send-prefix))))))

;;; ── Declarative option-side-effect dispatch ──────────────────────────────────
;;;
;;; define-option-side-effect-handlers builds apply-option-side-effects from a
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
  "Build APPLY-OPTION-SIDE-EFFECTS from a declarative table of RULES.
   Each RULE has the form:
     (NAME-STRING &body BODY)   — NAME-STRING matched via STRING=; VALUE bound in BODY.
     (:any-of (NAME...) &body BODY) — VALUE bound in BODY when NAME is one of the list.
   Generates a COND dispatch over NAME."
  `(defun apply-option-side-effects (name value unset-p)
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
       (cl-tmux/options:set-server-option "escape-time" 10)
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
                     ((and n (> n 0)) (min n +max-status-lines+))
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

;;; ── set-hook directive ────────────────────────────────────────────────────────

(defun %apply-set-hook-directive (cmd args)
  "Handle 'set-hook [-a] [-r] [-u] event [command]' directives.
   -r or -u flag removes/unsets all hooks for the event; without them, registers
   the command, REPLACING any existing hook for the event (tmux semantics).  -a
   appends instead, preserving prior hooks.  The command is stored as a raw
   string (not converted to keyword)
   so that format variables and arguments (e.g. 'display-message #{session_name}')
   are expanded at hook-fire time via %run-command-line.
   Returns T when handled, NIL otherwise."
  (when (string= cmd "set-hook")
    ;; Consume ALL leading -X flags (not just -r/-u): -g/-a/-R/-w/-p are accepted
    ;; and skipped so `set-hook -g <event> <cmd>` registers EVENT, not "-g"; -t
    ;; takes a target argument that is consumed too (cl-tmux's hook model is
    ;; global, so window/pane/target scope is accepted but not differentiated).
    (let* ((remove-p nil)
           (append-p nil)
           (rest     (%consuming-flags (args tok rest)
                       ((member tok '("-r" "-u") :test #'string=)
                        (setf remove-p t))
                       ((string= tok "-a")
                        (setf append-p t))
                       ;; -t takes a target argument: drop it too.
                       ((string= tok "-t")
                        (setf rest (rest rest)))))
           (event    (first rest))
           ;; The command may be a single quoted token or split across tokens;
           ;; join all remaining tokens as a single command line string.
           (cmd-str  (%join-config-tokens (rest rest))))
      (when event
        (if remove-p
            (progn (cl-tmux/hooks:clear-command-hooks event) t)
            (when cmd-str
              ;; Store the raw command string for execution at hook-fire time.
              ;; Without -a, set-hook REPLACES the event's hook (tmux semantics);
              ;; with -a it appends, preserving any prior hooks.
              (if append-p
                  (cl-tmux/hooks:append-command-hook event cmd-str)
                  (cl-tmux/hooks:set-command-hook event cmd-str))
              t))))))
