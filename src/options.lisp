(in-package #:cl-tmux/options)

;;; ── Global option storage ────────────────────────────────────────────────

(defvar *global-options* (make-hash-table :test #'equal)
  "Hash-table mapping option name strings to their current values.")

;;; ── Option specification ─────────────────────────────────────────────────

(defstruct option-spec
  "Describes one tmux option: its name, type keyword, and default value."
  name
  type
  default)

(defvar *option-registry* (make-hash-table :test #'equal)
  "Hash-table mapping option name strings to OPTION-SPEC instances.")

;;; ── Registration macro ───────────────────────────────────────────────────

(defmacro define-tmux-options (&rest specs)
  "Register tmux options and initialise *GLOBAL-OPTIONS* with their defaults.
Each SPEC has the form (name type default) where TYPE is :boolean, :integer,
or :string."
  `(progn
     ,@(mapcar (lambda (spec)
                 (destructuring-bind (name type default) spec
                   `(progn
                      (setf (gethash ,name *option-registry*)
                            (make-option-spec :name ,name
                                              :type ,type
                                              :default ,default))
                      (setf (gethash ,name *global-options*) ,default))))
               specs)))

;;; ── Registered options ───────────────────────────────────────────────────

(define-tmux-options
  ("status"           :boolean t)
  ("status-position"  :string  "bottom")
  ("status-interval"  :integer 15)
  ("status-left"      :string  nil)
  ("status-right"     :string  nil)
  ("history-limit"    :integer 2000)
  ("escape-time"      :integer 500)
  ("base-index"       :integer 0)
  ("mouse"            :boolean nil)
  ("default-shell"    :string  "/bin/sh"))

;;; ── Coercion helpers ─────────────────────────────────────────────────────

(defmacro define-type-coercions (&rest specs)
  "Generate a %COERCE-VALUE (type value) function from a declarative fact table.
Each SPEC has the form (TYPE-KEYWORD &rest BODY) where BODY is evaluated with
VALUE bound to the argument.  The generated function dispatches via ECASE."
  `(defun %coerce-value (type value)
     "Coerce VALUE to the Lisp type indicated by the TYPE keyword.
Dispatch is generated from the DEFINE-TYPE-COERCIONS fact table."
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
   (cond
     ((stringp value)
      (or (parse-integer value :junk-allowed t) 0))
     ((numberp value)
      (truncate value))
     (t 0)))
  (:string
   (format nil "~A" value)))

;;; ── Public API ───────────────────────────────────────────────────────────

(defun get-option (name &optional default)
  "Return the current value of option NAME from *GLOBAL-OPTIONS*.
Returns DEFAULT (nil if not supplied) when NAME is not present."
  (multiple-value-bind (value presentp)
      (gethash name *global-options*)
    (if presentp value default)))

(defun set-option (name value)
  "Coerce VALUE to the registered type for NAME and store it in *GLOBAL-OPTIONS*.
Returns the coerced value.  If NAME is not in *OPTION-REGISTRY* the value is
stored as-is (no coercion)."
  (let ((spec (gethash name *option-registry*)))
    (let ((coerced (if spec
                       (%coerce-value (option-spec-type spec) value)
                       value)))
      (setf (gethash name *global-options*) coerced)
      coerced)))

(defun option-defined-p (name)
  "Return T if NAME is a registered option in *OPTION-REGISTRY*."
  (not (null (gethash name *option-registry*))))

(defun all-options ()
  "Return an alist of (name . value) for every entry in *GLOBAL-OPTIONS*."
  (let (result)
    (maphash (lambda (k v) (push (cons k v) result))
             *global-options*)
    result))
