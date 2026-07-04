(in-package #:cl-tmux)

;;; -- list-* command CLI parser ------------------------------------------------

(defun %list-command-value-flag-p (flag value-flags)
  "Return true when FLAG takes a value in VALUE-FLAGS."
  (and value-flags
       (not (null (position flag value-flags :test #'char=)))))

(defun %list-command-too-many-message (command-name max-positionals)
  "Return canonical too-many-arguments text for COMMAND-NAME."
  (format nil "command ~A: too many arguments (need at most ~D)"
          command-name max-positionals))

(defun %list-command-unknown-flag-message (command-name flag)
  "Return canonical unknown short flag text for COMMAND-NAME."
  (format nil "command ~A: unknown flag -~C" command-name flag))

(defun %list-command-invalid-long-flag-message (command-name)
  "Return canonical invalid long flag text for COMMAND-NAME."
  (format nil "command ~A: invalid flag --" command-name))

(defun %list-command-missing-flag-argument-message (command-name flag)
  "Return canonical missing flag argument text for COMMAND-NAME."
  (format nil "command ~A: -~C expects an argument" command-name flag))

(defun %parse-short-flag-bundle (token allowed-flags value-flags remaining)
  "Walk clustered short flags in TOKEN, consuming REMAINING for value flags."
  (loop with flags = nil
        for i from 1 below (length token)
        for flag = (char token i)
        do (cond
             ((not (find flag allowed-flags :test #'char=))
              (return-from %parse-short-flag-bundle
                (values nil remaining flag nil)))
             ((%list-command-value-flag-p flag value-flags)
              (let ((value (if (< (1+ i) (length token))
                               (subseq token (1+ i))
                               (pop remaining))))
                (unless value
                  (return-from %parse-short-flag-bundle
                    (values nil remaining nil flag)))
                (push (cons flag value) flags)
                (loop-finish)))
             (t
              (push (cons flag t) flags)))
        finally (return (values flags remaining nil nil))))

(defun %parse-list-command-input (command-name args value-flags allowed-flags
                                  max-positionals)
  "Parse list-* command ARGS with tmux 3.6a option/error ordering."
  (loop with flags = nil
        with positionals = nil
        with parsing-options-p = t
        with remaining = args
        while remaining
        for token = (pop remaining)
        do (cond
             ((and parsing-options-p (string= token "--"))
              (setf parsing-options-p nil))
             ((and parsing-options-p
                   (>= (length token) 2)
                   (char= (char token 0) #\-)
                   (char= (char token 1) #\-))
              (return-from %parse-list-command-input
                (values nil nil
                        (%list-command-invalid-long-flag-message
                         command-name))))
             ((and parsing-options-p
                   (>= (length token) 2)
                   (char= (char token 0) #\-))
              (multiple-value-bind (bundle-flags new-remaining unknown-flag missing-flag)
                  (%parse-short-flag-bundle token allowed-flags value-flags remaining)
                (setf remaining new-remaining)
                (cond
                  (unknown-flag
                   (return-from %parse-list-command-input
                     (values nil nil
                             (%list-command-unknown-flag-message
                              command-name unknown-flag))))
                  (missing-flag
                   (return-from %parse-list-command-input
                     (values nil nil
                             (%list-command-missing-flag-argument-message
                              command-name missing-flag))))
                  (t
                   (setf flags (append bundle-flags flags))))))
             (t
              (push token positionals)
              (setf parsing-options-p nil)
              (when (> (length positionals) max-positionals)
                (return-from %parse-list-command-input
                  (values flags
                          (nreverse positionals)
                          (%list-command-too-many-message
                           command-name max-positionals))))))
        finally
           (return (values flags (nreverse positionals) nil))))
