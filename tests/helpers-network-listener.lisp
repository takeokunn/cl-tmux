(in-package #:cl-tmux/test)

;;; ── Throwaway test-socket paths ─────────────────────────────────────────────
;;;
;;; %test-socket-path is the single source of the "unique per-test Unix socket
;;; path under $TMPDIR (or /tmp)" pattern that used to be re-derived inline in
;;; client-tests.lisp and server-multi-tests.lisp.

(defun %test-socket-directory ()
  (let ((tmpdir (sb-ext:posix-getenv "TMPDIR")))
    (string-right-trim
     "/"
     (if (and tmpdir
              (plusp (length tmpdir))
              (uiop:directory-exists-p tmpdir))
         tmpdir
         "/tmp"))))

(defun %test-socket-path (label)
  "Unique throwaway socket path for LABEL (a descriptive tag embedded in the
   filename), under an existing $TMPDIR (or /tmp when unset or invalid)."
  (let ((dir (%test-socket-directory)))
    (format nil "~A/cl-tmux-~A-~D.sock" dir label (get-universal-time))))

(defmacro with-test-listener ((listener-var path-var path-form &key backlog) &body body)
  "Bind PATH-VAR to PATH-FORM (e.g. (%test-socket-path \"label\") or an
   explicit (socket-path name)) and LISTENER-VAR to a listener on it for the
   extent of BODY, tearing both down afterwards.  Skips BODY (via FiveAM
   `skip`) when Unix-domain sockets are unavailable — factors out the
   make-listener/unwind-protect/close-socket/delete-file boilerplate shared by
   every multi-client socket-lifecycle test."
  `(if (cl-tmux/net:unix-socket-available-p)
       (let* ((,path-var     ,path-form)
              (_             (ensure-directories-exist ,path-var))
              (,listener-var ,(if backlog
                                   `(cl-tmux/net:make-listener ,path-var :backlog ,backlog)
                                   `(cl-tmux/net:make-listener ,path-var))))
         (declare (ignore _))
         (unwind-protect
              (progn ,@body)
           (cl-tmux/net:close-socket ,listener-var)
           (ignore-errors (delete-file ,path-var))))
       (skip "Unix-domain socket unavailable (sandbox)")))
