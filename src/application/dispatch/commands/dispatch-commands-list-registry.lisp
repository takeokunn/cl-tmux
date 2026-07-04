(in-package #:cl-tmux)

;;; -- list-commands registry projection ---------------------------------------

(defun %lc-usage (canonical-name)
  "Return the usage flags string for CANONICAL-NAME, or empty string when unknown."
  (or (cdr (assoc canonical-name *command-usage-table* :test #'string=))
      ""))

(defun %lc-all-names ()
  "Return all list-commands canonical names in sorted order."
  (sort (mapcar #'car *command-usage-table*) #'string<))

(defun %lc-resolve-name (input)
  "Resolve INPUT for list-commands."
  (let ((all (%lc-all-names)))
    (cond
      ((find input all :test #'string=)
       (values :exact input))
      (t
       (let ((matches (remove-if-not
                       (lambda (name)
                         (and (>= (length name) (length input))
                              (string= input name :end2 (length input))))
                       all)))
         (cond
           ((null matches) (values :unknown nil))
           ((= 1 (length matches)) (values :prefix (first matches)))
           (t (values :ambiguous
                      (format nil "ambiguous command: ~A, could be: ~{~A~^, ~}"
                              input (sort (copy-list matches) #'string<))))))))))

(defun %lc-subst-all (string pat replacement)
  "Replace all non-overlapping occurrences of PAT in STRING with REPLACEMENT."
  (cl-ppcre:regex-replace-all (cl-ppcre:quote-meta-chars pat) string replacement))

(defun %lc-render-command (canonical-name format-string)
  "Render one canonical command entry using FORMAT-STRING or default usage output."
  (let ((usage (%lc-usage canonical-name)))
    (if format-string
        (let ((line format-string))
          (setf line (%lc-subst-all line "#{command_list_name}" canonical-name))
          (setf line (%lc-subst-all line "#{command_list_alias}" ""))
          (setf line (%lc-subst-all line "#{command_list_usage}" usage))
          line)
        (format nil "~A ~A" canonical-name usage))))

(defun %list-command-public-names (&optional name)
  "Return sorted tmux public command names, optionally filtered by NAME."
  (let ((names (sort (copy-list *tmux-public-command-names*)
                     #'string<)))
    (if (and name (plusp (length name)))
        (remove-if-not (lambda (command)
                         (string-equal command name))
                       names)
        names)))

(defun %format-list-command-entry (format-string command-name)
  "Format one list-commands row with the public command name."
  (%lc-render-command command-name format-string))
