(in-package #:cl-tmux/options)

;;;; Option display helpers: show-options/show-window-options rendering.

;;; ── show-options helpers ──────────────────────────────────────────────────

(defun %scope-ht (scope)
  "Return the options hash-table for SCOPE: *server-options* when SCOPE is :server,
   *global-options* otherwise."
  (if (eq scope :server) *server-options* *global-options*))

(defun %array-option-pairs (base scope)
  "Return sorted (NAME . VALUE) pairs for BASE[N] entries in SCOPE.
   Runtime values override registered defaults at the same index."
  (let ((by-index (make-hash-table)))
    (labels ((add-entry (name value)
               (let ((index (%array-entry-index-for-base base name)))
                 (when index
                   (setf (gethash index by-index) (cons name value))))))
      (maphash (lambda (name spec)
                 (add-entry name (option-spec-default spec)))
               (%scope-known-registry scope))
      (maphash (lambda (name spec)
                 (add-entry name (option-spec-default spec)))
               (%scope-registry scope))
      (maphash #'add-entry (%scope-ht scope)))
    (let ((indexed '()))
      (maphash (lambda (index pair)
                 (push (cons index pair) indexed))
               by-index)
      (mapcar #'cdr (sort indexed #'< :key #'car)))))

(defun %option-value-string (value)
  "Format VALUE for show-options output in tmux-compatible format.
   Strings: printed as-is (no quotes).  Booleans: 'on'/'off'.
   Integers: decimal.  NIL: 'off'.  Anything else: princ-to-string."
  (cond
    ((eq value t)   "on")
    ((eq value nil) "off")
    ((stringp value) value)
    (t (princ-to-string value))))

(defun %quote-option-string (value)
  "Quote string VALUE using tmux show-options display conventions."
  (cond
    ((string= value "") "''")
    ((or (find #\Space value)
         (find #\Tab value)
         (find #\" value)
         (find #\\ value))
     (with-output-to-string (s)
       (write-char #\" s)
       (loop for ch across value
             do (progn
                  (when (or (char= ch #\") (char= ch #\\))
                    (write-char #\\ s))
                  (write-char ch s)))
       (write-char #\" s)))
    (t value)))

(defun %option-value-display-string (value)
  "Format VALUE for show-options name/value output."
  (if (stringp value)
      (%quote-option-string value)
      (%option-value-string value)))

(defun show-options (&optional scope)
  "Return a string of 'name value' lines for all options in SCOPE.
   SCOPE is :server for server options, otherwise global options are used.
   Output matches real tmux format: 'option-name value' (no Lisp quoting)."
  (with-output-to-string (s)
    (let ((pairs '()))
      (maphash (lambda (k v) (push (cons k v) pairs)) (%scope-ht scope))
      (dolist (pair (sort pairs #'string< :key #'car))
        (format s "~A ~A~%" (car pair) (%option-value-display-string (cdr pair)))))))

(defun show-option (name &optional scope)
  "Return a string showing the current value of a single option NAME.
   SCOPE is :server for server options.
   Output matches real tmux format: 'option-name value'."
  (cond
    ((not (option-present-for-display-p name scope))
     (format nil "invalid option: ~A~%" name))
    (t
     (let ((val (gethash name (%scope-ht scope) :not-found)))
       (cond
         ((not (eq val :not-found))
          (format nil "~A ~A~%" name (%option-value-display-string val)))
         ((%array-option-p name scope)
          (with-output-to-string (s)
            (dolist (pair (%array-option-pairs name scope))
              (format s "~A ~A~%" (car pair) (%option-value-display-string (cdr pair))))))
         (t
          (format nil "~A: (not set)~%" name)))))))

(defun show-option-values (name &optional scope)
  "Return NAME's raw value-only show-options -v output, or NIL when unset."
  (let ((val (gethash name (%scope-ht scope) :not-found)))
    (cond
      ((not (eq val :not-found))
       (%option-value-string val))
      ((%array-option-p name scope)
       (with-output-to-string (s)
         (loop for pair in (%array-option-pairs name scope)
               for first-p = t then nil
               do (progn
                    (unless first-p
                      (terpri s))
                    (write-string (%option-value-string (cdr pair)) s)))))
      (t nil))))

(defun %hash-present-p (key table)
  "Return true when KEY is present in TABLE, regardless of the stored value."
  (nth-value 1 (gethash key table)))

(defun %registered-option-names ()
  "Return option registry names in tmux display order."
  (let ((names '()))
    (maphash (lambda (name spec)
               (declare (ignore spec))
               (push name names))
             *option-registry*)
    (sort names #'string<)))

(defun %window-local-option-present-p (name window)
  "Return true when WINDOW has an explicit local option NAME."
  (and window
       (%hash-present-p name (cl-tmux/model:window-local-options window))))

(defun %global-window-option-present-p (name)
  "Return true when NAME exists in the global/default option table."
  (and (or (and (stringp name)
                (plusp (length name))
                (char= (char name 0) #\@))
           (eq :window (option-scope-from-name name)))
       (or (%hash-present-p name *global-options*)
           (%hash-present-p name *option-registry*))))

(defun window-option-present-for-display-p (name window &key global-p inherited-p)
  "Return true when NAME may be shown by show-window-options.
tmux accepts registered option names even when they do not render in window
scope, but unset @ user options are invalid."
  (or (not (null (%exact-option-spec-for-scope name nil)))
      (and global-p (%global-window-option-present-p name))
      (and (not global-p)
           (or inherited-p
               (%window-local-option-present-p name window)))
      (and inherited-p (%global-window-option-present-p name))))

(defun %registered-window-option-names ()
  "Return registered names whose tmux scope is window."
  (remove-if-not (lambda (name)
                   (eq :window (option-scope-from-name name)))
                 (%registered-option-names)))

(defun %window-local-option-value (name window)
  "Return WINDOW-local option NAME and its present-p flag."
  (if window
      (gethash name (cl-tmux/model:window-local-options window))
      (values nil nil)))

(defun %window-option-names-for-display (window inherited-p global-p)
  "Return window option names in tmux display order for show-window-options."
  (cond
    ((or inherited-p global-p)
     (let ((names (copy-list (%registered-window-option-names))))
       (maphash (lambda (name value)
                  (declare (ignore value))
                  (when (%global-window-option-present-p name)
                    (pushnew name names :test #'equal)))
                *global-options*)
       (sort names #'string<)))
    (window
     (let ((names '()))
       (maphash (lambda (name value)
                  (declare (ignore value))
                  (push name names))
                (cl-tmux/model:window-local-options window))
       (sort names #'string<)))
    (t '())))

;;; ── Window-option resolution rules ──────────────────────────────────────
;;;
;;; define-window-option-resolution-rules builds %resolve-window-option-value
;;; from a declarative table of (GUARD INHERITED-MARKER &body BODY) rules.
;;; Each rule contributes one cond arm; BODY is evaluated with NAME in scope
;;; and must return the option value for that arm.
;;; INHERITED-MARKER is :yes when the arm yields an inherited value (triggers
;;; the tmux '* ' display prefix for -A / inherited-p output).

(defmacro define-window-option-resolution-rules (&rest rules)
  "Generate %resolve-window-option-value
     (name local-value local-present-p global-p inherited-p)
   from a declarative list of RULES.  Each RULE is:
     (GUARD INHERITED-MARKER &body BODY)
   where GUARD is a form with NAME / LOCAL-VALUE / LOCAL-PRESENT-P / GLOBAL-P /
   INHERITED-P in scope, INHERITED-MARKER is :yes when the arm yields an inherited
   value (triggers the '* ' display prefix), and BODY computes the resolved value.
   Returns (values value inherited-output-p present-p)."
  `(defun %resolve-window-option-value (name local-value local-present-p
                                         global-p inherited-p)
     "Walk the window-option resolution ladder for NAME and return
      (values value inherited-output-p present-p).
      Caller pre-computes local-value/local-present-p via %window-local-option-value
      and forwards the global-p/inherited-p flags from the show-window-option call."
     (declare (ignorable local-value local-present-p global-p inherited-p))
     (let ((inherited-output-p nil) (present-p nil) (value nil))
       (cond
         ,@(mapcar
            (lambda (rule)
              (destructuring-bind (guard inherited-marker &rest body) rule
                `(,guard
                  (setf present-p t
                        ,@(when (eq inherited-marker :yes)
                            '(inherited-output-p t))
                        value (progn ,@body)))))
            rules))
       (values value inherited-output-p present-p))))

(define-window-option-resolution-rules
  ;; -g flag: show global/default value when the option is present globally.
  (global-p :no
   (when (%global-window-option-present-p name) (get-option name)))
  ;; Window-local value is present: return it directly (no inherited marker).
  (local-present-p :no local-value)
  ;; -A (inherited-p) flag: show inherited effective value with '* ' prefix.
  ((and inherited-p (%global-window-option-present-p name)) :yes
   (get-option name))
  ;; Bare single-option query: fall back to the effective (inherited) value
  ;; matching real tmux — `show-window-options mode-keys` returns the effective
  ;; value even when not set window-locally.  No '* ' marker without -A.
  ((%global-window-option-present-p name) :no (get-option name)))

(defun show-window-option (name window &key inherited-p value-only-p global-p)
  "Return NAME rendered as tmux show-window-options/show-options -w output.
   When called for a specific option NAME:
   - Without -g/-A: returns the effective value (local → global → spec default),
     matching `tmux show-window-options <name>` which always shows effective value.
   - With -A: marks inherited (non-local) values with a leading '* ' prefix.
   - With -g: shows global/default value when present.
   INHERITED-P only controls the '* ' marker and is relevant for full-list output."
  (multiple-value-bind (local-value local-present-p)
      (%window-local-option-value name window)
    (multiple-value-bind (value inherited-output-p present-p)
        (%resolve-window-option-value name local-value local-present-p
                                      global-p inherited-p)
      (when present-p
        (if value-only-p
            (%option-value-string value)
            ;; tmux show-options -A marks inherited values with a leading "* "
            ;; prefix (e.g. "* mode-keys vi"), not a trailing "*" suffix.
            (if inherited-output-p
                (format nil "* ~A ~A~%" name (%option-value-string value))
                (format nil "~A ~A~%" name (%option-value-string value))))))))

(defun show-window-options (window &key inherited-p global-p)
  "Return tmux-style window option lines.
   Without -g/-A, tmux lists only window-local options.  -A includes inherited
   effective values; -g lists global/default window options."
  (with-output-to-string (s)
    (dolist (name (%window-option-names-for-display window inherited-p global-p))
      (let ((line (show-window-option name window
                                      :inherited-p inherited-p
                                      :global-p global-p)))
        (when line
          (write-string line s))))))
