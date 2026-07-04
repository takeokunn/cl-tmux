(in-package #:cl-tmux/config)

;;; source-file directive handling.

(defun %glob-expand (path)
  "Expand a shell glob PATH to sorted regular-file namestrings."
  (if (find-if (lambda (c) (member c '(#\* #\? #\[) :test #'char=)) path)
      (sort (loop for p in (ignore-errors (directory (pathname path)))
                  unless (ignore-errors (uiop:directory-pathname-p p))
                    collect (namestring p))
            #'string<)
      (list path)))

(defun %glob-pattern-p (path)
  "True when PATH contains a shell glob metacharacter."
  (find-if (lambda (c) (member c '(#\* #\? #\[) :test #'char=)) path))

(defun %source-file-report-missing (path)
  "Report tmux-style source-file diagnostics for missing paths."
  (ignore-errors
    (let ((fn (find-symbol "ADD-MESSAGE-LOG" "CL-TMUX")))
      (when (and fn (fboundp fn))
        (funcall fn (format nil "No such file or directory: ~A" path))))))

(defun %parse-source-file-flags (args)
  "Parse the leading -Fnqv flags and -t target of source-file."
  (let ((parse-only nil) (quiet nil) (verbose nil) (format-p nil) (rest args))
    (setf rest
          (%consume-leading-flag-tokens
           rest
           (lambda (tok rest)
             (when (%flag-token-contains-any-p tok '(#\n)) (setf parse-only t))
             (when (%flag-token-contains-any-p tok '(#\q)) (setf quiet t))
             (when (%flag-token-contains-any-p tok '(#\v)) (setf verbose t))
             (when (%flag-token-contains-any-p tok '(#\F)) (setf format-p t))
             (let ((target-pos (position #\t tok)))
               (if target-pos
                   (progn
                     (when (and (= target-pos (1- (length tok))) rest)
                       (setf rest (cdr rest)))
                     (values rest nil))
                   (values rest t))))))
    (values parse-only quiet verbose format-p rest)))

(defun %parse-config-file-only (file)
  "Tokenise FILE without applying directives."
  (ignore-errors
    (with-open-file (in file :if-does-not-exist nil)
      (when in
        (loop for line = (read-line in nil nil)
              while line
              do (%config-tokens (%strip-config-comment line)))))))

(defun %resolve-source-file-path (raw format-p)
  "Resolve one source-file positional RAW to its on-disk path."
  (%expand-leading-tilde
   (if format-p
       (or (ignore-errors (cl-tmux/format:expand-format raw nil)) raw)
       raw)))

(defun %source-file-glob-matches (expanded quiet)
  "Expand EXPANDED to matching files and report unmatched globs unless QUIET."
  (let ((matches (%glob-expand expanded)))
    (when (and (%glob-pattern-p expanded) (null matches) (not quiet))
      (%source-file-report-missing expanded))
    matches))

(defun %load-or-parse-source-file (file parse-only quiet)
  "Apply one matched source-file FILE."
  (if (probe-file file)
      (progn
        (if parse-only
            (%parse-config-file-only file)
            (ignore-errors (load-config-file file)))
        t)
      (progn
        (unless quiet
          (%source-file-report-missing file))
        nil)))

(defun source-files (args)
  "Implement source-file [-Fnqv] [-t target-pane] path..."
  (multiple-value-bind (parse-only quiet verbose format-p positionals)
      (%parse-source-file-flags args)
    (declare (ignore verbose))
    (let ((ok t))
      (dolist (raw positionals ok)
        (let ((path (%resolve-source-file-path raw format-p)))
          (when (plusp (length path))
            (let ((matches (%source-file-glob-matches path quiet)))
              (if matches
                  (dolist (file matches)
                    (unless (or (%load-or-parse-source-file file parse-only quiet)
                                quiet)
                      (setf ok nil)))
                  (unless quiet
                    (setf ok nil))))))))))

(defun %apply-source-file-directive (cmd args)
  "Intercept source-file flags, glob patterns, and multiple paths."
  (when (string= cmd "source-file")
    (source-files args)))
