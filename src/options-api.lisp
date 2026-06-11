(in-package #:cl-tmux/options)

;;;; Option accessor API: type coercions, get/set functions,
;;;;  scoped window/pane overrides, and show-options helpers.

;;; ── Command-alias registry ────────────────────────────────────────────────
;;;
;;; Implements tmux's command-alias[] array option.  Each entry maps an alias
;;; name string to a command-line expansion string, e.g.
;;;   "e" → "new-window -n"
;;; When the alias is looked up, the expansion is tokenised and the caller's
;;; remaining arguments are appended.
;;;
;;; In .tmux.conf:
;;;   set -s command-alias[0] e='new-window -n'
;;;   set -s command-alias[1] gst='new-session -s'

(defvar *command-aliases* (make-hash-table :test #'equal)
  "Hash-table mapping alias name strings to their command-line expansion strings.")

(defun register-command-alias (alias expansion)
  "Register ALIAS as a shorthand for the EXPANSION command line."
  (setf (gethash alias *command-aliases*) expansion))

(defun lookup-command-alias (name)
  "Return the expansion string for alias NAME, or NIL when not found."
  (gethash name *command-aliases*))

(defun list-command-aliases ()
  "Return an alist of (alias . expansion) pairs from *command-aliases*."
  (let (result)
    (maphash (lambda (k v) (push (cons k v) result)) *command-aliases*)
    (sort result #'string< :key #'car)))

;;; ── Type coercions ────────────────────────────────────────────────────────

(defmacro define-type-coercions (&rest specs)
  "Generate a %COERCE-VALUE (type value) function from a declarative fact table.
   Each SPEC has the form (TYPE-KEYWORD &rest BODY) where BODY is evaluated with
   VALUE bound to the argument.  The generated function dispatches via ECASE."
  `(defun %coerce-value (type value)
     "Coerce VALUE to the Lisp type indicated by the TYPE keyword."
     (ecase type
       ,@(mapcar (lambda (spec)
                   (destructuring-bind (type-keyword &rest body) spec
                     `(,type-keyword (progn ,@body))))
                 specs))))

(define-type-coercions
  (:boolean
   (cond
     ((stringp value)
      (if (member value '("on" "true" "1") :test #'equal) t nil))
     (t (if value t nil))))
  (:integer
   ;; Non-numeric and non-number inputs (including nil) fall back to 0.
   ;; Callers that need to distinguish "unset" from "zero" should check
   ;; option presence via get-option before calling set-option.
   (cond
     ((stringp value)
      (or (parse-integer value :junk-allowed t) 0))
     ((numberp value)
      (truncate value))
     (t 0)))
  (:string
   (format nil "~A" value)))

;;; ── Option accessor generator ─────────────────────────────────────────────
;;;
;;; define-option-accessor generates a matched get/set pair for one option
;;; storage hash-table + registry hash-table, eliminating the structural
;;; duplication between the global and server option APIs.

(defmacro define-option-accessor (get-name set-name storage-var registry-var
                                  &key get-docstring set-docstring)
  "Generate GET-NAME (name &optional default) and SET-NAME (name value) functions
   operating on STORAGE-VAR (runtime values) and REGISTRY-VAR (type specs).
   GET-DOCSTRING and SET-DOCSTRING are optional docstring overrides."
  `(progn
     (defun ,get-name (name &optional default)
       ,(or get-docstring
            (format nil "Return the current value of option NAME from ~A.~%~
                         Returns DEFAULT (nil if not supplied) when NAME is not present."
                    storage-var))
       (multiple-value-bind (value presentp)
           (gethash name ,storage-var)
         (if presentp value default)))
     (defun ,set-name (name value)
       ,(or set-docstring
            (format nil "Coerce VALUE to the registered type for NAME and store it in ~A.~%~
                         Returns the coerced value.  If NAME is not in ~A the value is~%~
                         stored as-is (no coercion)."
                    storage-var registry-var))
       (let* ((spec    (gethash name ,registry-var))
              (coerced (if spec
                           (%coerce-value (option-spec-type spec) value)
                           value)))
         (setf (gethash name ,storage-var) coerced)
         coerced))))

;;; ── Public API ────────────────────────────────────────────────────────────

(define-option-accessor get-option set-option
  *global-options* *option-registry*
  :get-docstring
  "Return the current value of option NAME from *GLOBAL-OPTIONS*.
   Returns DEFAULT (nil if not supplied) when NAME is not present."
  :set-docstring
  "Coerce VALUE to the registered type for NAME and store it in *GLOBAL-OPTIONS*.
   Returns the coerced value.  If NAME is not in *OPTION-REGISTRY* the value is
   stored as-is (no coercion).")

(defun option-defined-p (name)
  "Return T if NAME is a registered option in *OPTION-REGISTRY*."
  (not (null (gethash name *option-registry*))))

(defun style-option-p (name)
  "True when NAME is a tmux STYLE option (its value is a comma-separated style
   string such as \"fg=red,bg=black,bold\").  tmux marks these OPTIONS_TABLE_IS_STYLE
   and `set -a` appends to them with a ',' separator, unlike plain string options
   which concatenate directly.  Every style option's name ends in \"-style\" EXCEPT
   clock-mode-style — a 12/24-hour choice that merely shares the suffix."
  (and (stringp name)
       (let* ((suffix "-style") (sl (length suffix)) (nl (length name)))
         ;; nl > sl (strict): a style option must have a real name BEFORE the
         ;; "-style" suffix, so the bare suffix "-style" is not a style option.
         (and (> nl sl)
              (string= name suffix :start1 (- nl sl))
              (not (string= name "clock-mode-style"))))))

(defun append-option-value (name old value)
  "Compute the new value for `set -a NAME` given the option's current OLD value and
   the appended VALUE.  For STYLE options with a non-empty current value, tmux
   inserts a ',' separator (so `status-style bg=red` then `set -ag status-style
   fg=blue` yields `bg=red,fg=blue`); plain string options concatenate with no
   separator.  An empty OLD or empty VALUE never introduces a stray comma."
  (let ((old-str (princ-to-string (or old "")))
        (val-str (princ-to-string (or value ""))))
    (if (and (style-option-p name) (plusp (length old-str)) (plusp (length val-str)))
        (concatenate 'string old-str "," val-str)
        (concatenate 'string old-str val-str))))

(defun all-options ()
  "Return an alist of (name . value) for every entry in *GLOBAL-OPTIONS*.
   Note: iteration order over the hash-table is arbitrary and may differ
   between runs.  Do not rely on ordering of the returned alist."
  (let (result)
    (maphash (lambda (k v) (push (cons k v) result))
             *global-options*)
    result))

;;; ── Server option API ─────────────────────────────────────────────────────

(define-option-accessor get-server-option set-server-option
  *server-options* *server-option-registry*
  :get-docstring
  "Return the current value of server option NAME from *SERVER-OPTIONS*.
   Returns DEFAULT (nil if not supplied) when NAME is not present."
  :set-docstring
  "Coerce VALUE to the registered type for NAME and store in *SERVER-OPTIONS*.
   Returns the coerced value.")

;;; ── Scoped option accessors (per-window / per-pane) ──────────────────────
;;;
;;; These functions implement the fallback chain:
;;;   pane-local  → global  → registered default
;;;   window-local → global → registered default
;;;
;;; The cl-tmux/model package is referenced by qualified name to avoid a
;;; circular dependency (model depends on config which depends on options).

(defun get-option-for-context (name &key pane window)
  "Resolve option NAME with full tmux scope precedence: pane-local -> window-local
   -> global -> registered default.  PANE and/or WINDOW may be NIL (skip that
   level).  Uses gethash present-p so a present-but-falsey override is honored
   (consistent with get-option-for-window/pane).  When both PANE and WINDOW are
   NIL this is equivalent to get-option.

   This is the single source of truth for scoped option resolution;
   get-option-for-window and get-option-for-pane delegate here."
  (multiple-value-bind (pv pp)
      (if pane
          (gethash name (cl-tmux/model:pane-local-options pane))
          (values nil nil))
    (if pp
        pv
        (multiple-value-bind (wv wp)
            (if window
                (gethash name (cl-tmux/model:window-local-options window))
                (values nil nil))
          (if wp
              wv
              (multiple-value-bind (gv gp) (gethash name *global-options*)
                (if gp
                    gv
                    (let ((spec (gethash name *option-registry*)))
                      (when spec (option-spec-default spec))))))))))

(defun get-option-for-window (name window)
  "Look up NAME in WINDOW's local options, falling back to *global-options*,
   then to the registered spec default.  Returns NIL when not found anywhere.

   Delegates to get-option-for-context with only :window supplied (pane level
   skipped); the present-p resolution ladder lives there in one place.  A
   window-local value explicitly set to a FALSEY value is honored and does NOT
   fall through to the global value."
  (get-option-for-context name :window window))

(defun set-option-for-window (name value window)
  "Coerce VALUE to the registered type for NAME and store it under NAME in
   WINDOW's local-options hash.  If NAME is not in *OPTION-REGISTRY* the value
   is stored as-is (no coercion).  Returns the coerced value."
  (let* ((spec    (gethash name *option-registry*))
         (coerced (if spec
                      (%coerce-value (option-spec-type spec) value)
                      value)))
    (setf (gethash name (cl-tmux/model:window-local-options window)) coerced)
    coerced))

(defun get-option-for-pane (name pane)
  "Look up NAME in PANE's local options, falling back to *global-options*,
   then to the registered spec default.  Returns NIL when not found anywhere.

   Delegates to get-option-for-context with only :pane supplied (window level
   skipped); the present-p resolution ladder lives there in one place.  A
   pane-local value explicitly set to a FALSEY value is honored and does NOT
   fall through to the global value."
  (get-option-for-context name :pane pane))

(defun set-option-for-pane (name value pane)
  "Coerce VALUE to the registered type for NAME and store it under NAME in
   PANE's local-options hash.  If NAME is not in *OPTION-REGISTRY* the value
   is stored as-is (no coercion).  Returns the coerced value."
  (let* ((spec    (gethash name *option-registry*))
         (coerced (if spec
                      (%coerce-value (option-spec-type spec) value)
                      value)))
    (setf (gethash name (cl-tmux/model:pane-local-options pane)) coerced)
    coerced))

;;; ── show-options helpers ──────────────────────────────────────────────────

(defun %option-value-string (value)
  "Format VALUE for show-options output in tmux-compatible format.
   Strings: printed as-is (no quotes).  Booleans: 'on'/'off'.
   Integers: decimal.  NIL: 'off'.  Anything else: princ-to-string."
  (cond
    ((eq value t)   "on")
    ((eq value nil) "off")
    ((stringp value) value)
    (t (princ-to-string value))))

(defun show-options (&optional scope)
  "Return a string of 'name value' lines for all options in SCOPE.
   SCOPE is :server for server options, otherwise global options are used.
   Output matches real tmux format: 'option-name value' (no Lisp quoting)."
  (with-output-to-string (s)
    (let* ((ht    (if (eq scope :server) *server-options* *global-options*))
           (pairs '()))
      (maphash (lambda (k v) (push (cons k v) pairs)) ht)
      (dolist (pair (sort pairs #'string< :key #'car))
        (format s "~A ~A~%" (car pair) (%option-value-string (cdr pair)))))))

(defun show-option (name &optional scope)
  "Return a string showing the current value of a single option NAME.
   SCOPE is :server for server options.
   Output matches real tmux format: 'option-name value'."
  (let* ((ht  (if (eq scope :server) *server-options* *global-options*))
         (val (gethash name ht :not-found)))
    (if (eq val :not-found)
        (format nil "~A: (not set)~%" name)
        (format nil "~A ~A~%" name (%option-value-string val)))))
