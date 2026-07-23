(in-package #:cl-tmux/test)

;;;; socket-path and stale-socket tests

(describe "server-suite"

  ;;; -- socket-path naming ------------------------------------------------------

  ;; socket-path embeds the session name in the filename and always ends with .sock.
  (it "socket-path-properties-table"
    (dolist (row '(("mysess" "cl-tmux-mysess.sock" "session name embedded in path")
                   ("anysess" ".sock" "path always ends with .sock")))
      (destructuring-bind (sess expected desc) row
        (declare (ignore desc))
        (let ((path (cl-tmux::socket-path sess)))
          (expect (search expected path))))))

  ;; socket-path returns distinct paths for distinct session names.
  (it "socket-path-distinct-for-different-names"
    (let ((p1 (cl-tmux::socket-path "alpha"))
          (p2 (cl-tmux::socket-path "beta")))
      (expect (string/= p1 p2))))

  ;; socket-path embeds $TMPDIR in the result when it is set, overriding /tmp.
  (it "socket-path-uses-tmpdir-env-var"
    (with-temporary-posix-environment-variable ("TMUX_TMPDIR" nil)
      (with-temporary-posix-environment-variable ("TMPDIR" "/var/folders/test")
        (let ((path (cl-tmux::socket-path "envtest")))
          (expect (search "/var/folders/test" path))))))

  ;; socket-path uses /tmp as the socket directory when $TMPDIR is unset.
  (it "socket-path-falls-back-to-tmp-when-no-tmpdir"
    (with-temporary-posix-environment-variable ("TMUX_TMPDIR" nil)
      (with-temporary-posix-environment-variable ("TMPDIR" nil)
        (let ((path (cl-tmux::socket-path "tmptestfb")))
          (expect (search "/tmp" path))))))

  ;; socket-path prefers $TMUX_TMPDIR over $TMPDIR (tmux precedence).
  (it "socket-path-tmux-tmpdir-beats-tmpdir"
    (with-temporary-posix-environment-variable ("TMUX_TMPDIR" "/tmp/tmux-tmpdir-test")
      (let ((path (cl-tmux::socket-path "envtest2")))
        (expect (search "/tmp/tmux-tmpdir-test" path)))))

  ;; Sockets live in a per-UID directory.
  (it "socket-path-uses-per-uid-directory"
    (with-temporary-posix-environment-variable ("TMUX_TMPDIR" nil)
      (let ((path (cl-tmux::socket-path "uidtest")))
        (expect (search (format nil "cl-tmux-~D/" (sb-posix:getuid)) path)))))

  ;; The global -S flag returns its path verbatim; -L replaces the socket name.
  (it "socket-path-honors-global-flag-overrides"
    (let ((cl-tmux::*socket-path-override* "/tmp/custom-cl-tmux.sock")
          (cl-tmux::*socket-name-override* nil))
      (expect (string= "/tmp/custom-cl-tmux.sock" (cl-tmux::socket-path "whatever"))))
    (let ((cl-tmux::*socket-path-override* nil)
          (cl-tmux::*socket-name-override* "mylabel"))
      (let ((path (cl-tmux::socket-path "ignored-name")))
        (expect (search "cl-tmux-mylabel.sock" path))
        (expect (null (search "ignored-name" path))))))

  ;;; -- stale-socket ------------------------------------------------------------

  ;; %stale-socket-p returns T for an existing file that refuses connections.
  (it "stale-socket-p-detects-dead-socket-file"
    (expect (null (cl-tmux::%stale-socket-p "/nonexistent/cl-tmux-stale-probe.sock")))
    (let ((path (format nil "~A/cl-tmux-stale-test-~D.sock"
                        (string-right-trim "/" (or (sb-ext:posix-getenv "TMPDIR") "/tmp"))
                        (random 1000000))))
      (unwind-protect
           (progn
             (with-open-file (s path :direction :output :if-does-not-exist :create)
               (declare (ignore s)))
             (expect (eq t (and (cl-tmux::%stale-socket-p path) t))))
        (ignore-errors (delete-file path)))))

  ;; %stale-socket-p returns NIL when a live listener accepts on the path.
  (it "stale-socket-p-live-listener-is-not-stale"
    (let ((path (cl-tmux/net::%make-probe-socket-path)))
      (if (cl-tmux/net:unix-socket-available-p)
          (let ((listener (cl-tmux/net:make-listener path)))
            (unwind-protect
                 (expect (null (cl-tmux::%stale-socket-p path)))
              (cl-tmux/net:close-socket listener)
              (ignore-errors (delete-file path))))
          (expect t :to-be-truthy)))))
