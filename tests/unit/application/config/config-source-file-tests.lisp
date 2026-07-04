(in-package #:cl-tmux/test)

;;;; source-file directive flags, glob expansion, missing-file diagnostics

(in-suite config-directives-suite)

;;; ── source-file: -q flags, glob patterns, multiple paths ─────────────────────

(test glob-expand-passthrough-non-glob
  "%glob-expand returns a non-glob path unchanged as a one-element list."
  (is (equal '("/etc/foo.conf") (cl-tmux/config::%glob-expand "/etc/foo.conf"))))

(test glob-expand-empty-for-no-matches
  "%glob-expand returns NIL for a glob that matches nothing."
  (is (null (cl-tmux/config::%glob-expand "/nonexistent-cl-tmux-xyz-dir/*.conf"))))

(test source-files-skips-flags-and-tolerates-missing
  "source-files skips -q/-n/-v flags and ignores missing files under -q."
  (is (eq t (cl-tmux/config:source-files '("-q" "/no/such/cl-tmux-file.conf")))))

(test source-files-glob-expands-and-loads-matching-files
  "source-file with a glob loads every matching file; %glob-expand finds them."
  (let ((dir (uiop:ensure-directory-pathname
              (merge-pathnames "cl-tmux-glob-tests/" (uiop:temporary-directory)))))
    (ensure-directories-exist dir)
    (unwind-protect
         (progn
           (with-open-file (f (merge-pathnames "a.conf" dir)
                              :direction :output :if-exists :supersede)
             (write-line "# cl-tmux glob test (no global mutation)" f))
           (with-open-file (f (merge-pathnames "b.conf" dir)
                              :direction :output :if-exists :supersede)
             (write-line "# cl-tmux glob test (no global mutation)" f))
           (let ((matches (cl-tmux/config::%glob-expand
                           (namestring (merge-pathnames "*.conf" dir)))))
             (is (= 2 (length matches)) "glob matched both .conf files (got ~A)" matches)
             (is (eq t (cl-tmux/config:source-files
                        (list (namestring (merge-pathnames "*.conf" dir)))))
                 "source-files loads the globbed files without error")))
      (ignore-errors (uiop:delete-directory-tree dir :validate t)))))

(test source-files-missing-path-logs-message-table
  "source-file on a missing path or unmatched glob (no -q) logs
   'No such file or directory: PATH' and returns NIL.
   Each row: (path description)."
  (dolist (row '(("/no/such/cl-tmux-srcfile-abc.conf"
                  "plain missing path logs No such file or directory")
                 ("/nonexistent-cl-tmux-glob-dir/*.conf"
                  "unmatched glob logs No such file or directory")))
    (destructuring-bind (path desc) row
      (let ((cl-tmux::*message-log* nil))
        (is (null (cl-tmux/config:source-files (list path))) desc)
        (is (= 1 (length cl-tmux::*message-log*))
            "exactly one diagnostic was logged")
        (is (search "No such file or directory" (cdr (first cl-tmux::*message-log*)))
            "diagnostic mentions 'No such file or directory'")
        (is (search path (cdr (first cl-tmux::*message-log*)))
            "diagnostic includes the offending path")))))

(test source-files-q-suppresses-missing-file-message
  "With -q, a missing file logs NOTHING (tmux CMD_PARSE_QUIET) and returns T."
  (let ((cl-tmux::*message-log* nil))
    (is (eq t (cl-tmux/config:source-files
               '("-q" "/no/such/cl-tmux-srcfile-xyz.conf"))))
    (is (null cl-tmux::*message-log*)
        "-q suppresses the diagnostic entirely")))

(test source-files-existing-file-logs-no-message
  "A successfully loaded file produces NO 'missing' diagnostic."
  (with-isolated-config
    (let ((cl-tmux::*message-log* nil))
      (with-temp-config-file (p "bind z next-window")
        (is (eq t (cl-tmux/config:source-files (list (namestring p)))))
        (is (null cl-tmux::*message-log*)
            "an existing, loaded file logs no missing-file diagnostic")))))
