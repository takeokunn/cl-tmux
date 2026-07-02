(in-package #:cl-tmux)

;;; -- Environment inspection and mutation commands ---------------------------
;;;

(defun %shell-single-quote (value)
  (with-output-to-string (out)
    (write-char #\' out)
    (loop for ch across value
          do (if (char= ch #\')
                 (write-string "'\\''" out)
                 (write-char ch out)))
    (write-char #\' out)))

(defun %format-show-environment-entry (name value shell-p)
  (if shell-p
      (if value
          (format nil "~A=~A; export ~A" name (%shell-single-quote value) name)
          (format nil "unset ~A" name))
      (if value
          (format nil "~A=~A" name value)
          (format nil "-~A" name))))

(defun %show-environment-list-overlay (names value-fn shell-p)
  "Show a NAME list using VALUE-FN to fetch each value.
   Non-shell entries are indented two spaces under the `environment` header to
   match tmux's listing layout; shell-form entries (`-s`) are emitted flush so
   the output can be sourced directly."
  (show-built-overlay (s)
    (format s "environment~%")
    (dolist (name names)
      (format s "~A~A~%"
              (if shell-p "" "  ")
              (%show-environment-entry-text name value-fn shell-p)))))

(defmacro %with-environment-scope ((scope target-session session flags conflict-message
                                    target-message)
                                   &body body)
  "Bind SCOPE to :global, :target, or :session and resolve TARGET-SESSION.
   A conflicting -g/-t combination is reported with CONFLICT-MESSAGE."
  `(let ((global-p (%flag-present-p ,flags #\g))
         (target-p (%flag-present-p ,flags #\t)))
     (cond
       ((and global-p target-p)
        (show-overlay ,conflict-message))
       (global-p
        (let ((,scope :global)
              (,target-session nil))
          ,@body))
       (target-p
        (let ((target-str (%flag-value ,flags #\t)))
          (with-target-session (,target-session target-str ,session
                                 :message ,target-message
                                 :on-missing :error)
            (let ((,scope :target))
              ,@body))))
       (t
        (let ((,scope :session)
              (,target-session ,session))
          ,@body)))))

(defun %show-environment-source (scope session target-session)
  "Return (values NAMES VALUE-FN HIDDEN-NAMES) for SCOPE."
  (ecase scope
    (:global
     (values (cl-tmux/model:process-environment-names)
             #'cl-tmux/model:process-environment-value
             cl-tmux/model:*global-hidden-environment-names*))
    (:target
     (values (cl-tmux/model:session-environment-names target-session)
             (lambda (name)
               (cl-tmux/model:session-environment-value target-session name))
             (cl-tmux/model:session-environment-hidden target-session)))
    (:session
     (values (cl-tmux/model:session-environment-names session)
             (lambda (name)
               (cl-tmux/model:session-environment-value session name))
             (cl-tmux/model:session-environment-hidden session)))))

(defun %show-environment-entry-text (name value-fn shell-p)
  (multiple-value-bind (value source)
      (funcall value-fn name)
    (declare (ignore source))
    (%format-show-environment-entry name value shell-p)))

(defun %set-environment-entry (scope target-session session name value remove-p
                               &optional hidden-p)
  "Apply a set-environment mutation for SCOPE.  HIDDEN-P marks the variable
   hidden (tmux -h): excluded from plain show-environment and from child
   environments; a plain set clears an existing hidden mark."
  (ecase scope
    (:global
     (cond
       (remove-p
        (setf cl-tmux/model:*global-hidden-environment-names*
              (delete name cl-tmux/model:*global-hidden-environment-names*
                      :test #'string=))
        (cl-tmux/model:process-unset-environment name))
       (t
        (if hidden-p
            (pushnew name cl-tmux/model:*global-hidden-environment-names*
                     :test #'string=)
            (setf cl-tmux/model:*global-hidden-environment-names*
                  (delete name cl-tmux/model:*global-hidden-environment-names*
                          :test #'string=)))
        (cl-tmux/model:process-set-environment name value))))
    (:target
     (if remove-p
         (cl-tmux/model:session-unset-environment target-session name)
         (cl-tmux/model:session-set-environment target-session name value
                                                :hidden hidden-p)))
    (:session
     (if remove-p
         (cl-tmux/model:session-unset-environment session name)
         (cl-tmux/model:session-set-environment session name value
                                                :hidden hidden-p)))))

(defun %cmd-show-environment-arg (session args)
  "show-environment [-hgs] [-t target] [NAME]: show environment variables.
   With NAME, show that variable.  -s prints shell assignment/unset syntax.
   -h lists ONLY hidden variables (tmux ENVIRON_HIDDEN); without -h, hidden
   variables are excluded from the listing."
  (with-command-input (flags positionals args "t"
                             :allowed-flags '(#\g #\h #\s #\t)
                             :max-positionals 1
                             :message "show-environment: unsupported argument")
    (let ((shell-p  (%flag-present-p flags #\s))
          (hidden-p (%flag-present-p flags #\h))
          (name (first positionals)))
      (%with-environment-scope (scope target-session session flags
                                 "show-environment: -g and -t are mutually exclusive"
                                 "show-environment: no such session: ~A")
        (multiple-value-bind (names value-fn hidden-names)
            (%show-environment-source scope session target-session)
          (let ((listed (if hidden-p
                            (sort (copy-list hidden-names) #'string<)
                            (remove-if (lambda (n)
                                         (member n hidden-names :test #'string=))
                                       names))))
            (if name
                (show-overlay (%show-environment-entry-text name value-fn shell-p))
                (%show-environment-list-overlay listed value-fn shell-p))))))))

(defun %cmd-set-environment-prompt (session args)
  "set-environment [-Fhgru] [-t target] NAME [VALUE]: set or unset an environment variable.
   -u (tmux's unset flag) or -r unsets the variable.  Otherwise VALUE is required.
   -F: expand VALUE as a format string (e.g. #{...}) before storing it.
   -h: mark the variable hidden (tmux ENVIRON_HIDDEN): excluded from plain
       show-environment listings and from child-process environments; shown
       only by show-environment -h.  A later plain set unhides it."
  (with-command-input (flags positionals args "t"
                             :allowed-flags '(#\F #\g #\h #\r #\t #\u)
                             :message "set-environment: unsupported argument")
    (let* ((remove-p (or (%flag-present-p flags #\u)
                         (%flag-present-p flags #\r)))
           (name (first positionals))
           (raw-value (format nil "~{~A~^ ~}" (rest positionals)))
           ;; -F: expand the value as a format string before storing.
           (value (if (%flag-present-p flags #\F)
                      (cl-tmux/format:expand-format
                       raw-value
                       (cl-tmux/format:format-context-from-session
                        session
                        (session-active-window session)
                        (session-active-pane session)))
                      raw-value)))
      (%with-environment-scope (scope target-session session flags
                                 "set-environment: -g and -t are mutually exclusive"
                                 "set-environment: no such session: ~A")
        (when (and name (plusp (length name)))
          (%set-environment-entry scope target-session session name value remove-p
                                  (%flag-present-p flags #\h)))))))
