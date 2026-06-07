(in-package #:cl-tmux/options)

;;; Global option storage

(defvar *global-options* (make-hash-table :test #'equal)
  "Hash-table mapping option name strings to their current values.")

(defvar *server-options* (make-hash-table :test #'equal)
  "Hash-table for server-scoped options (set-option -s).
   Keys: escape-time, exit-empty, exit-unattached.")

;;; Option specification

(defstruct option-spec
  "Describes one tmux option: its name, type keyword, and default value."
  name
  type
  default)

(defvar *option-registry* (make-hash-table :test #'equal)
  "Hash-table mapping option name strings to OPTION-SPEC instances.")

;;; ── Unified option-table registration macro ───────────────────────────────
;;;
;;; define-option-table encapsulates the two-phase expand pattern shared by
;;; define-tmux-options and define-server-options: registering spec metadata
;;; (immutable after load) and initialising runtime default values.
;;;
;;; Parameters:
;;;   REGISTRY-VAR  — the *-option-registry* hash-table to receive specs
;;;   STORAGE-VAR   — the *-options* hash-table to receive runtime defaults
;;;   SPECS         — list of (name type default) triples

(defmacro define-option-table (registry-var storage-var &rest specs)
  "Register option specs in REGISTRY-VAR and initialise STORAGE-VAR with defaults.
   Each SPEC has the form (name type default) where TYPE is :boolean, :integer,
   or :string.  Phase 1 stores spec metadata; phase 2 stores runtime defaults."
  `(progn
     ;; Phase 1: register spec metadata (immutable after load)
     ,@(mapcar (lambda (spec)
                 (destructuring-bind (name type default) spec
                   `(setf (gethash ,name ,registry-var)
                          (make-option-spec :name ,name
                                            :type ,type
                                            :default ,default))))
               specs)
     ;; Phase 2: initialise runtime default values
     ,@(mapcar (lambda (spec)
                 (destructuring-bind (name _type default) spec
                   (declare (ignore _type))
                   `(setf (gethash ,name ,storage-var) ,default)))
               specs)))

;;; ── Convenience wrappers for the two standard option tables ───────────────

(defmacro define-tmux-options (&rest specs)
  "Register tmux options in *OPTION-REGISTRY* (spec metadata) and initialise
   *GLOBAL-OPTIONS* with their defaults (runtime state).  Each SPEC has the form
   (name type default) where TYPE is :boolean, :integer, or :string."
  `(define-option-table *option-registry* *global-options* ,@specs))

(defmacro define-server-options (&rest specs)
  "Register server options in *SERVER-OPTION-REGISTRY* and initialise
   *SERVER-OPTIONS* with their defaults.  Each SPEC has the form
   (name type default)."
  `(define-option-table *server-option-registry* *server-options* ,@specs))

;;; Registered options

(define-tmux-options
  ("status"                   :boolean t)
  ("status-position"          :string  "bottom")
  ("status-interval"          :integer 15)
  ("status-left"              :string  "[#{session_name}]")
  ("status-right"             :string  "#{time}")
  ("status-left-length"       :integer 40)
  ("status-right-length"      :integer 40)
  ("status-style"             :string  "")
  ("status-justify"           :string  "left")
  ("window-status-format"     :string  " #{window_index}:#{window_name} ")
  ("window-status-current-format" :string " #{window_index}:#{window_name}* ")
  ("window-status-style"      :string  "")
  ("window-status-current-style" :string "reverse")
  ("window-status-separator"  :string  " ")
  ("history-limit"            :integer 2000)
  ("escape-time"              :integer 500)
  ("base-index"               :integer 0)
  ("pane-base-index"          :integer 0)
  ("mouse"                    :boolean nil)
  ("default-command"          :string  "")
  ("default-shell"            :string  "/bin/sh")
  ("exit-unattached"          :boolean nil)
  ("pane-border-style"        :string  "")
  ("pane-active-border-style" :string  "fg=green")
  ("synchronize-panes"        :boolean nil)
  ("word-separators"          :string  " -_@")
  ("automatic-rename"         :boolean t)
  ("automatic-rename-format"  :string  "#{pane_current_command}")
  ("bell-action"              :string  "any")
  ("visual-bell"              :boolean nil)
  ("visual-activity"          :boolean nil)
  ("monitor-activity"         :boolean nil)
  ("buffer-limit"             :integer 50)
  ("focus-events"             :boolean nil)
  ("copy-command"             :string  "")
  ("set-titles"               :boolean nil)
  ("set-titles-string"        :string  "#W")
  ("remain-on-exit"           :boolean nil)
  ("renumber-windows"         :boolean nil)
  ("message-style"            :string  "")
  ("update-environment"       :string  "DISPLAY SSH_ASKPASS SSH_AUTH_SOCK SSH_CONNECTION WINDOWID XAUTHORITY")
  ;; Display options
  ("display-time"             :integer 750)    ; ms to show messages / pane numbers
  ("display-panes-time"       :integer 1000)   ; ms to show pane numbers (display-panes)
  ("display-panes-colour"     :string  "blue")
  ("display-panes-active-colour" :string "red")
  ;; Resize and timing
  ("repeat-time"              :integer 500)    ; ms window for repeatable bindings
  ("lock-after-time"          :integer 0)      ; 0 = disabled
  ;; Terminal settings
  ("default-terminal"         :string  "screen")
  ("terminal-overrides"       :string  "")
  ;; Window/pane defaults
  ("allow-rename"             :boolean t)
  ("aggressive-resize"        :boolean nil)
  ("alternate-screen"         :boolean t)
  ;; Status bar extras
  ("status-keys"              :string  "emacs")  ; emacs or vi
  ("mode-keys"                :string  "vi")     ; vi or emacs copy-mode keys
  ("status-left-style"        :string  "")
  ("status-right-style"       :string  "")
  ;; Pane display
  ("other-pane-height"        :integer 0)
  ("other-pane-width"         :integer 0)
  ;; Pane border status line (top / bottom / off)
  ("pane-border-status"       :string  "off")
  ("pane-border-format"       :string  " #{pane_index} ")
  ;; Clock display
  ("clock-mode-colour"        :string  "blue")
  ("clock-mode-style"         :integer 24)      ; 12 or 24 hour
  ;; Copy mode search
  ("wrap-search"              :boolean t)        ; wrap search in copy-mode
  ("copy-mode-current-match-style" :string "bg=magenta")
  ("copy-mode-match-style"    :string  "bg=green")
  ;; Session lifecycle
  ("destroy-unattached"       :boolean nil)     ; destroy session when no clients
  ("detach-on-destroy"        :boolean t)       ; detach when session destroyed
  ;; Window sizing
  ("default-size"             :string  "80x24") ; default WxH for new sessions
  ;; Input handling
  ("extended-keys"            :string  "off")   ; off / on / always
  ("key-table"                :string  "prefix") ; default key table
  ("prefix2"                  :string  "")      ; secondary prefix key
  ;; History / logging
  ("history-file"             :string  "")      ; save command history here
  ("fill-character"           :string  "")      ; char to fill empty areas
  ;; Locking
  ("lock-command"             :string  "lock -np") ; command to run on lock
  ;; Status format (tmux 3.2+ array-style; stored as single string here)
  ("status-format"            :string  "")
  ;; Popup defaults
  ("popup-border-lines"       :string  "single")
  ("popup-border-style"       :string  ""))

;;; Server-option registry and defaults

(defvar *server-option-registry* (make-hash-table :test #'equal)
  "Specs for server-scoped options (set with set-option -s).")

(define-server-options
  ("escape-time"          :integer 500)
  ("exit-empty"           :boolean t)
  ("exit-unattached"      :boolean nil)
  ("focus-events"         :boolean nil)  ; enable focus-events reporting (server-wide)
  ("set-clipboard"        :string  "on") ; external, on, or off
  ("terminal-features"    :string  "")
  ("terminal-overrides"   :string  "")
  ("command-alias"        :string  "")   ; array stored as single string for simplicity
  ("default-terminal"     :string  "screen")
  ("buffer-limit"         :integer 50))

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

(defun get-option-for-window (name window)
  "Look up NAME in WINDOW's local options, falling back to *global-options*,
   then to the registered spec default.  Returns NIL when not found anywhere."
  (or (gethash name (cl-tmux/model:window-local-options window))
      (gethash name *global-options*)
      (let ((spec (gethash name *option-registry*)))
        (when spec (option-spec-default spec)))))

(defun set-option-for-window (name value window)
  "Store VALUE under NAME in WINDOW's local-options hash.
   The value is stored as-is (no type coercion); callers that want coercion
   should call %coerce-value before passing value here.
   Returns VALUE."
  (setf (gethash name (cl-tmux/model:window-local-options window)) value))

(defun get-option-for-pane (name pane)
  "Look up NAME in PANE's local options, falling back to *global-options*,
   then to the registered spec default.  Returns NIL when not found anywhere."
  (or (gethash name (cl-tmux/model:pane-local-options pane))
      (gethash name *global-options*)
      (let ((spec (gethash name *option-registry*)))
        (when spec (option-spec-default spec)))))

(defun set-option-for-pane (name value pane)
  "Store VALUE under NAME in PANE's local-options hash.
   The value is stored as-is (no type coercion); callers that want coercion
   should call %coerce-value before passing value here.
   Returns VALUE."
  (setf (gethash name (cl-tmux/model:pane-local-options pane)) value))

;;; ── show-options helpers ──────────────────────────────────────────────────

(defun show-options (&optional scope)
  "Return a string of name value lines for all options in SCOPE.
   SCOPE is :server for server options, otherwise global options are used."
  (with-output-to-string (s)
    (let* ((ht    (if (eq scope :server) *server-options* *global-options*))
           (pairs '()))
      (maphash (lambda (k v) (push (cons k v) pairs)) ht)
      (dolist (pair (sort pairs #'string< :key #'car))
        (format s "~A ~S~%" (car pair) (cdr pair))))))

(defun show-option (name &optional scope)
  "Return a string showing the current value of a single option NAME.
   SCOPE is :server for server options."
  (let* ((ht  (if (eq scope :server) *server-options* *global-options*))
         (val (gethash name ht :not-found)))
    (if (eq val :not-found)
        (format nil "~A: (not set)~%" name)
        (format nil "~A ~S~%" name val))))
