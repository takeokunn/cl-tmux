(in-package #:cl-tmux/test)

;;;; socket-path and stale-socket tests

(in-suite server-suite)

;;; -- socket-path naming ------------------------------------------------------

(test socket-path-properties-table
  "socket-path embeds the session name in the filename and always ends with .sock."
  (dolist (row '(("mysess" "cl-tmux-mysess.sock" "session name embedded in path")
                 ("anysess" ".sock" "path always ends with .sock")))
    (destructuring-bind (sess expected desc) row
      (let ((path (cl-tmux::socket-path sess)))
        (is (search expected path) "~A: got ~S" desc path)))))

(test socket-path-distinct-for-different-names
  :description "socket-path returns distinct paths for distinct session names."
  (let ((p1 (cl-tmux::socket-path "alpha"))
        (p2 (cl-tmux::socket-path "beta")))
    (is (string/= p1 p2)
        "socket-path must be distinct for different session names")))

(test socket-path-uses-tmpdir-env-var
  :description "socket-path embeds $TMPDIR in the result when it is set, overriding /tmp."
  (with-temporary-posix-environment-variable ("TMPDIR" "/var/folders/test")
    (let ((path (cl-tmux::socket-path "envtest")))
      (is (search "/var/folders/test" path)
          "socket-path must use $TMPDIR when set, got ~S" path))))

(test socket-path-falls-back-to-tmp-when-no-tmpdir
  :description "socket-path uses /tmp as the socket directory when $TMPDIR is unset."
  (with-temporary-posix-environment-variable ("TMPDIR" nil)
    (let ((path (cl-tmux::socket-path "tmptestfb")))
      (is (search "/tmp" path)
          "socket-path must fall back to /tmp when $TMPDIR is unset, got ~S" path))))

(test socket-path-tmux-tmpdir-beats-tmpdir
  :description "socket-path prefers $TMUX_TMPDIR over $TMPDIR (tmux precedence)."
  (with-temporary-posix-environment-variable ("TMUX_TMPDIR" "/tmp/tmux-tmpdir-test")
    (let ((path (cl-tmux::socket-path "envtest2")))
      (is (search "/tmp/tmux-tmpdir-test" path)
          "socket-path must use $TMUX_TMPDIR when set, got ~S" path))))

(test socket-path-uses-per-uid-directory
  :description "Sockets live in a per-UID directory."
  (with-temporary-posix-environment-variable ("TMUX_TMPDIR" nil)
    (let ((path (cl-tmux::socket-path "uidtest")))
      (is (search (format nil "cl-tmux-~D/" (sb-posix:getuid)) path)
          "socket-path must place sockets in the per-UID directory, got ~S" path))))

(test socket-path-honors-global-flag-overrides
  :description "The global -S flag returns its path verbatim; -L replaces the socket name."
  (let ((cl-tmux::*socket-path-override* "/tmp/custom-cl-tmux.sock")
        (cl-tmux::*socket-name-override* nil))
    (is (string= "/tmp/custom-cl-tmux.sock" (cl-tmux::socket-path "whatever"))
        "-S must override the whole socket path verbatim"))
  (let ((cl-tmux::*socket-path-override* nil)
        (cl-tmux::*socket-name-override* "mylabel"))
    (let ((path (cl-tmux::socket-path "ignored-name")))
      (is (search "cl-tmux-mylabel.sock" path)
          "-L must select the socket name, got ~S" path)
      (is (null (search "ignored-name" path))
          "-L must replace the server-derived name, got ~S" path))))

;;; -- stale-socket ------------------------------------------------------------

(test stale-socket-p-detects-dead-socket-file
  :description "%stale-socket-p returns T for an existing file that refuses connections."
  (is (null (cl-tmux::%stale-socket-p "/nonexistent/cl-tmux-stale-probe.sock"))
      "a missing socket path is not stale - there is nothing to clean up")
  (let ((path (format nil "~A/cl-tmux-stale-test-~D.sock"
                      (string-right-trim "/" (or (sb-ext:posix-getenv "TMPDIR") "/tmp"))
                      (random 1000000))))
    (unwind-protect
         (progn
           (with-open-file (s path :direction :output :if-does-not-exist :create)
             (declare (ignore s)))
           (is (eq t (and (cl-tmux::%stale-socket-p path) t))
               "an existing path refusing connections must be stale"))
      (ignore-errors (delete-file path)))))

(test stale-socket-p-live-listener-is-not-stale
  :description "%stale-socket-p returns NIL when a live listener accepts on the path."
  (let ((path (cl-tmux/net::%make-probe-socket-path)))
    (if (cl-tmux/net:unix-socket-available-p)
        (let ((listener (cl-tmux/net:make-listener path)))
          (unwind-protect
               (is (null (cl-tmux::%stale-socket-p path))
                   "a live listening socket must not be reported stale")
            (cl-tmux/net:close-socket listener)
            (ignore-errors (delete-file path))))
        (is-true t "unix sockets unavailable in this sandbox - skipping"))))
