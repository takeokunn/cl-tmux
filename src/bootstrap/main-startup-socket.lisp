;;; Startup socket discovery and server auto-start helpers.

(in-package :cl-tmux)

(defconstant +server-socket-poll-interval-seconds+ 0.1
  "Seconds between socket-existence probes while waiting for a server to start.")

(defconstant +server-socket-poll-max-iterations+ 30
  "Maximum number of socket-existence probes (30 x 0.1 s = 3 s total wait).")

(defun %stale-socket-p (socket-path)
  "True when SOCKET-PATH exists but no server accepts connections on it.
   tmux treats such leftover socket files (e.g. after a crash) as stale:
   it unlinks them and starts a fresh server instead of failing to attach."
  (and (probe-file socket-path)
       (not (handler-case
                (let ((sock (cl-tmux/net:connect-to socket-path)))
                  (cl-tmux/net:close-socket sock)
                  t)
              (error () nil)))))

(defun %global-socket-flag-args ()
  "The global -L/-S flags to re-inject into a spawned server child's argv so it
   binds the same socket the parent resolved."
  (append (when *socket-path-override* (list "-S" *socket-path-override*))
          (when *socket-name-override* (list "-L" *socket-name-override*))))

(defun %launch-server-and-poll-when-live (socket-path exe args)
  (let ((launched
          (ignore-errors
            (sb-ext:run-program exe args
                                :wait nil
                                :output nil :error nil))))
    ;; Poll only when we actually attempted a launch.  This avoids the
    ;; unconditional 3-second dead-time when run-program silently failed.
    (when launched
      (loop repeat +server-socket-poll-max-iterations+
            until (probe-file socket-path)
            do (sleep +server-socket-poll-interval-seconds+)))))

(defun %ensure-server-running (session-name)
  "Start a background server for SESSION-NAME if no live socket exists.
   A stale socket file (present but refusing connections) is unlinked first,
   matching tmux's crash-recovery behaviour.
   Uses sb-ext:run-program with *posix-argv* to spawn a separate process.
   Only enters the polling loop when run-program succeeded.
   Polls every +server-socket-poll-interval-seconds+ for up to
   +server-socket-poll-max-iterations+ iterations for the socket to appear."
  (let* ((socket-path (socket-path session-name))
         (exe         (first sb-ext:*posix-argv*))
         (args        (append (%global-socket-flag-args)
                              (list "server" session-name))))
    (when (%stale-socket-p socket-path)
      (ignore-errors (delete-file socket-path)))
    (unless (probe-file socket-path)
      ;; Guard: run-program may fail in test environments or when the
      ;; binary is not yet on PATH.  Only poll if the spawn succeeded.
      ;; :wait nil means non-blocking, so run-program returns after starting the child.
      (%launch-server-and-poll-when-live socket-path exe args))))

(defun %socket-file-session-name (path)
  "Extract the cl-tmux session/server name from a socket PATH, or NIL."
  (when path
    (let* ((name (pathname-name path))
           (prefix "cl-tmux-"))
      (when (and name
                 (>= (length name) (length prefix))
                 (string= prefix name :end2 (length prefix)))
        (subseq name (length prefix))))))

(defun %running-server-name (&optional preferred-name)
  "Return the best known running server socket name, preferring PREFERRED-NAME.
   Falls back to the default \"0\" socket, then to the first cl-tmux socket in
   TMPDIR.  This supports CLI command forwarding even when the first server was
   launched with `new-session -s NAME` or `attach NAME`."
  (cond
    ((and preferred-name (probe-file (socket-path preferred-name)))
     preferred-name)
    ((probe-file (socket-path "0")) "0")
    (t
     (let ((pattern (merge-pathnames
                     "cl-tmux-*.sock"
                     (parse-namestring (format nil "~A/" (%socket-directory))))))
       (%socket-file-session-name (first (ignore-errors (directory pattern))))))))
