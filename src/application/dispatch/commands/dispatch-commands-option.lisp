(in-package #:cl-tmux)

;;; -- set-option scope helpers + show-options %cmd-* handlers -----------------
;;;
;;; %with-option-scope (CPS), %cmd-set-option, %cmd-set-window-option,
;;; %cmd-show-options*, %cmd-show-window-options-arg, %cmd-show-session-options-arg,
;;; %cmd-show-server-options-arg.

;;; ── set-option scope helpers (CPS + data-logic separation) ─────────────────
;;;
;;; %cmd-set-option decomposes into three concerns:
;;;   1. Value expansion (-F flag) — data transformation before storage.
;;;   2. Scope resolution (-g/-w/-p/-t) — which store to use.
;;;   3. Operation dispatch (-u unset / -a append / -o guard / normal set).
;;;
;;; %with-option-scope resolves the scope ONCE and passes (scope target) to a
;;; continuation K.  %scope-getter/%scope-setter/%scope-remover (built by
;;; define-scope-accessor-table below) are pure scope→effect dispatch functions
;;; with ecase — exhaustive, so the compiler warns on any missing scope kind.

(defun %expand-F-flag (flags session raw-value)
  "Expand RAW-VALUE as a format string when FLAGS contains -F; else return as-is."
  (if (%flag-present-p flags #\F)
      (cl-tmux/format:expand-format
       raw-value
       (cl-tmux/format:format-context-from-session
        session (session-active-window session) (session-active-pane session)))
      raw-value))

(defun %user-option-name-p (name)
  "True when NAME is a user option (`@foo`): these stay global regardless of any
   window-scope name inference."
  (and (stringp name) (plusp (length name)) (char= (char name 0) #\@)))

(defun %with-option-scope (session flags target-str name k)
  "Resolve the option scope from FLAGS / TARGET-STR / option NAME, then call K with
   (scope target).  SCOPE is :pane, :window, :server, or :global; TARGET is the
   resolved pane/window (NIL otherwise).  Falls back to :global when -p/-w resolves
   to a NIL target.  With NO explicit -g/-w/-p/-s flag, a WINDOW-scoped option name
   (tmux options_scope_from_name) routes to the active window's local store."
  (let ((globalp (%flag-present-p flags #\g)))
    (cond
      ((and (%flag-present-p flags #\p) (not globalp))
       (let ((pane (if target-str
                       (%resolve-pane-in-window (session-active-window session) target-str)
                       (session-active-pane session))))
         (funcall k (if pane :pane :global) pane)))
      ((and (%flag-present-p flags #\w) (not globalp))
       (let ((win (%resolve-window-target-or-active session target-str)))
         (funcall k (if win :window :global) win)))
      ;; -s selects the SERVER option store (a namespace distinct from the global
      ;; session/window options).  Server options have no global/session split, so
      ;; -s routes to :server even alongside -g, matching the config-load path.
      ((%flag-present-p flags #\s)
       (funcall k :server nil))
      ;; No explicit scope flag: infer from the option NAME (tmux
      ;; options_scope_from_name).  A window-scoped option (not a user @-option)
      ;; routes to the -t / active window; session/server names stay :global.
      ((and name (not globalp)
            (not (%user-option-name-p name))
            (eq :window (cl-tmux/options:option-scope-from-name name)))
       (let ((win (%resolve-window-target-or-active session target-str)))
         (funcall k (if win :window :global) win)))
      (t
       (funcall k :global nil)))))

;;; define-scope-accessor-table builds three single-purpose dispatch functions
;;; (%scope-getter, %scope-setter, %scope-remover) from a declarative table of
;;; (SCOPE GETTER-FORM SETTER-FORM REMOVER-FORM) rules — NAME, VALUE, DEFAULT,
;;; and TARGET are bound in the forms.  This replaces a hand-written
;;; closures-in-ecase table (%scope-option-accessors) whose callers had to
;;; multiple-value-bind all three accessors and (declare (ignore ...)) the two
;;; they didn't need.

(defmacro define-scope-accessor-table (&rest rules)
  "Build %SCOPE-GETTER, %SCOPE-SETTER, and %SCOPE-REMOVER from RULES, each of
   the form (SCOPE GETTER-FORM SETTER-FORM REMOVER-FORM).  NAME/DEFAULT are
   bound for getter forms, NAME/VALUE for setter forms, and NAME for remover
   forms; TARGET is bound in all three."
  `(progn
     (defun %scope-getter (scope name target &optional default)
       (declare (ignorable target default))
       (ecase scope
         ,@(mapcar (lambda (rule)
                     (destructuring-bind (scope getter-form setter-form remover-form) rule
                       (declare (ignore setter-form remover-form))
                       `(,scope ,getter-form)))
                   rules)))
     (defun %scope-setter (scope name value target)
       (declare (ignorable target))
       (ecase scope
         ,@(mapcar (lambda (rule)
                     (destructuring-bind (scope getter-form setter-form remover-form) rule
                       (declare (ignore getter-form remover-form))
                       `(,scope ,setter-form)))
                   rules)))
     (defun %scope-remover (scope name target)
       (declare (ignorable target))
       (ecase scope
         ,@(mapcar (lambda (rule)
                     (destructuring-bind (scope getter-form setter-form remover-form) rule
                       (declare (ignore getter-form setter-form))
                       `(,scope ,remover-form)))
                   rules)))))

(define-scope-accessor-table
  (:pane
   (cl-tmux/options:get-option-for-pane name target)
   (cl-tmux/options:set-option-for-pane name value target)
   (remhash name (cl-tmux/model:pane-local-options target)))
  (:window
   (cl-tmux/options:get-option-for-window name target)
   (cl-tmux/options:set-option-for-window name value target)
   (remhash name (cl-tmux/model:window-local-options target)))
  (:global
   (cl-tmux/options:get-option name default)
   (cl-tmux/options:set-option name value)
   (remhash name cl-tmux/options:*global-options*))
  (:server
   (cl-tmux/options:get-server-option name default)
   (cl-tmux/options:set-server-option name value)
   (remhash name cl-tmux/options:*server-options*)))

(defun %scope-present-p (name scope target)
  "Return true when option NAME is explicitly present in SCOPE / TARGET's OWN
   store, without consulting inherited (parent/global) scopes.  Mirrors tmux's
   options_get_only, used by set-option -o (only-if-unset)."
  (ecase scope
    (:pane
     (nth-value 1 (gethash name (cl-tmux/model:pane-local-options target))))
    (:window
     (nth-value 1 (gethash name (cl-tmux/model:window-local-options target))))
    (:global
     (nth-value 1 (gethash name cl-tmux/options:*global-options*)))
    (:server
     (nth-value 1 (gethash name cl-tmux/options:*server-options*)))))

(defun %scope-append (name value scope target)
  "Append VALUE to option NAME in the store identified by SCOPE / TARGET.
   Style options (e.g. status-style) join with ',' via append-option-value."
  (%scope-setter scope name
                 (cl-tmux/options:append-option-value
                  name (%scope-getter scope name target nil) value)
                 target))

(defun %scope-set (name value scope target)
  "Store VALUE for option NAME in the store identified by SCOPE / TARGET."
  (%scope-setter scope name value target))

(defun %scope-unset (name scope target)
  "Remove NAME from the option store identified by SCOPE / TARGET."
  (%scope-remover scope name target))

(defun %cmd-set-option (session args)
  "set-option [-aFgopqsuUw] [-t target] <name> <value...>: set an option.
   Scope: -p pane-local, -w window-local, -g global (default), -s server-local.
   Operation: -u unset, -U unset on all panes (3.4+, accepted as -u here), -a
   append, -o only-if-unset, default: set.
   -F expands #{...} in VALUE before storage (one-shot format resolution).
   -q suppresses errors about unknown options."
  (with-command-input (flags positionals args "t"
                             :allowed-flags '(#\a #\F #\g #\o #\p #\q #\s #\u #\U #\w #\t)
                             :message "set-option: unsupported argument")
    (let* ((name       (first positionals))
           (raw-value  (format nil "~{~A~^ ~}" (rest positionals)))
           (target-str (%flag-value flags #\t))
           ;; -U (unset on all panes, 3.4+) collapses onto -u in this model.
           (unset-p    (or (%flag-present-p flags #\u)
                           (%flag-present-p flags #\U)))
           (append-p   (%flag-present-p flags #\a))
           (quiet-p    (%flag-present-p flags #\q))
           (only-if-unset-p (%flag-present-p flags #\o)))
      (cond
        ((null name) nil)
        ((cl-tmux/config::%unsupported-set-option-p name)
         ;; -q suppresses the unknown-option error overlay.
         (unless quiet-p
           (%overlayf "set-option: unsupported option ~A" name))
         nil)
        (t
         (let ((value (%expand-F-flag flags session raw-value)))
           (%with-option-scope session flags target-str name
             (lambda (scope target)
               (if (and only-if-unset-p
                        (not unset-p)
                        (%scope-present-p name scope target))
                   ;; tmux: `set -o` on an already-set option is an error —
                   ;; "already set: NAME" unless -q — and nothing else runs.
                   (unless quiet-p
                     (%overlayf "already set: ~A" name))
                   (progn
                     (cond
                       (unset-p
                        (%scope-unset name scope target))
                       (append-p
                        (%scope-append name value scope target))
                       (t
                        (%scope-set name value scope target)))
                     ;; Side-effects for special options (prefix/status/escape-time
                     ;; etc.) run after the operation.  Passes RAW value —
                     ;; side-effect parsers expect strings, not coerced types.
                     (cl-tmux/config:apply-option-side-effects name value unset-p)))))))))))

(defun %cmd-set-window-option (session args)
  "set-window-option: like set-option but defaults to WINDOW scope.  Prepends
   -w so a bare `set-window-option mode-keys vi` is window-local; an explicit
   -g still wins (global), since %cmd-set-option's (and windowp (not globalp))
   gate lets -g override the injected -w."
  (%cmd-set-option session (cons "-w" args)))

(defun %show-options-scope (flags default-scope)
  "Resolve show-options scope flags.  The current option store models session and
   window options through the global table; server options are separate."
  (cond
    ((%flag-present-p flags #\s) :server)
    ((eq default-scope :server) :server)
    (t nil)))

(defun %show-option-value-only (name scope)
  "Return only NAME's value for `show-options -v`, or NIL when NAME is unset."
  (let* ((line (cl-tmux/options:show-option name scope))
         (prefix (format nil "~A " name)))
    (when (and (not (search "(not set)" line))
               (>= (length line) (length prefix))
               (string= prefix line :end2 (length prefix)))
      (string-right-trim '(#\Newline #\Return)
                         (subseq line (length prefix))))))

(defun %show-options-with-hooks (text flags)
  "Append command-hook listings to TEXT when FLAGS contains -H.
   tmux's -H flag includes hooks in show-options output; this implementation
   reuses the existing command-hook formatter so the display path stays aligned."
  (if (%flag-present-p flags #\H)
      (if (plusp (length text))
          (format nil "~A~%~A" text (cl-tmux/hooks:describe-command-hooks))
          (cl-tmux/hooks:describe-command-hooks))
      text))

(defun %show-options-overlay (text flags)
  "Apply shared show-options overlays such as -H hooks."
  (show-overlay (%show-options-with-hooks text flags)))

(defun %show-options-command-body (flags positionals value-only-renderer
                                         single-renderer all-renderer)
  "Render the overlay for show-options-style commands.
   VALUE-ONLY-RENDERER, SINGLE-RENDERER, and ALL-RENDERER are callables that
   receive NAME / INHERITED-P and return rendered text or NIL."
  (let* ((name (first positionals))
         (quietp (%flag-present-p flags #\q))
         (value-only-p (%flag-present-p flags #\v))
         (inherited-p (%flag-present-p flags #\A)))
    (cond
      ((and name value-only-p)
       (let ((value (funcall value-only-renderer name inherited-p)))
         (when (or value (not quietp))
           (show-overlay (or value "")))))
      (name
       (let ((out (funcall single-renderer name inherited-p)))
         ;; tmux -q suppresses output for unknown/unset options: both "(not set)"
         ;; and "invalid option:" (returned for completely unregistered names).
         (unless (and quietp
                      (or (search "(not set)" out)
                          (search "invalid option:" out)))
           (%show-options-overlay out flags))))
      (t
       (%show-options-overlay (funcall all-renderer inherited-p) flags)))))

(defun %cmd-show-options* (session args default-scope value-only-renderer
                                      single-renderer all-renderer)
  "show-options argument form.
     Supports the common scriptable subset: -g/-w/-s scope flags, -t target
     consumption, -q quiet missing options, -v value-only, -A inherited options,
     -H hooks, and an optional option NAME positional.  Targets are consumed by
     the scriptable tmux syntax; option storage is currently global/server-scoped."
  (declare (ignore session))
  (with-command-input (flags positionals args "t"
                             :allowed-flags '(#\A #\H #\g #\w #\s #\t #\q #\v)
                             :max-positionals 1
                             :message "show-options: unsupported argument")
    (let ((scope (%show-options-scope flags default-scope)))
      (%show-options-command-body
       flags
       positionals
       (or value-only-renderer
           (lambda (name inherited-p)
             (declare (ignore inherited-p))
             (%show-option-value-only name scope)))
       (or single-renderer
           (lambda (name inherited-p)
             (declare (ignore inherited-p))
             (cl-tmux/options:show-option name scope)))
       (or all-renderer
           (lambda (inherited-p)
             (declare (ignore inherited-p))
             (cl-tmux/options:show-options scope)))))))

(defun %cmd-show-options-arg (session args)
  "show-options with arguments."
  (%cmd-show-options* session args nil nil nil nil))

(defun %cmd-show-window-options-arg (session args)
  "show-window-options with arguments; consumes tmux flags."
  (with-command-input (flags positionals args "t"
                             :allowed-flags '(#\A #\H #\g #\w #\s #\t #\q #\v)
                             :max-positionals 1
                             :message "show-options: unsupported argument")
    (let* ((target-str (%flag-value flags #\t))
           (win (%resolve-window-target-or-active session target-str)))
      (%show-options-command-body
       flags
       positionals
       (lambda (name inherited-p)
         (declare (ignore inherited-p))
         (cl-tmux/options:show-window-option name win :value-only-p t))
       (lambda (name inherited-p)
         (cl-tmux/options:show-window-option name win :inherited-p inherited-p))
       (lambda (inherited-p)
         (cl-tmux/options:show-window-options win :inherited-p inherited-p))))))

(defun %cmd-show-session-options-arg (session args)
  "show-session-options with arguments; consumes tmux flags."
  (%cmd-show-options* session args nil nil nil nil))

(defun %cmd-show-server-options-arg (session args)
  "show-server-options with arguments; defaults to the server option store."
  (%cmd-show-options* session args :server nil nil nil))

;;; -- -e VAR=val environment flag parser ----------------------------------------
;;;
;;; new-window and split-window accept repeated -e VAR=val flags to set
;;; environment variables in the new pane.  This helper collects them from
;;; an already-parsed flags alist (produced by %parse-command-flags with "e"
;;; in value-flags) into an alist suitable for %fork-pane's :extra-env.

(defun %collect-env-flags (flags-alist)
  "Extract all (-e . \"VAR=val\") entries from FLAGS-ALIST and return an alist
   of (\"VAR\" . \"val\") pairs.  Entries without \"=\" are included as (\"NAME\" . \"\").
   Multiple -e flags are supported; all are collected."
  (loop for (char . value) in flags-alist
        when (and (char= char #\e) (stringp value))
        collect (let ((eq-pos (position #\= value)))
                  (if eq-pos
                      (cons (subseq value 0 eq-pos)
                            (subseq value (1+ eq-pos)))
                      (cons value "")))))
