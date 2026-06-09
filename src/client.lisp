(in-package #:cl-tmux)

;;;; Detach-attach client.
;;;;
;;;; A thin terminal: it puts its own stdin in raw mode, forwards keystrokes and
;;;; resizes to the server as protocol frames, and paints the rendered frames
;;;; the server sends back.  It holds no session state — all prefix handling and
;;;; rendering happen server-side, so the client is the same for any session.

(defun run-command-client (name args)
  "Forward ARGS — a command name followed by its arguments — to the running server
   for session NAME as a single +msg-command+ frame, then exit.  This is the
   `cl-tmux <command>` CLI path: it drives a server from outside instead of
   attaching a terminal.  The server runs the command (server-multi.lisp,
   %handle-multi-client-message) against its live session.
   A target given as `-t <target>` flows through in ARGS — the server parses it
   like any other flag — so no special target extraction is needed here."
  (require :sb-posix)
  (when args
    (let ((socket (connect-to (socket-path name))))
      (unwind-protect
           (let ((stream (socket-stream socket)))
             (send-frame stream (msg-command (first args) nil (rest args)))
             (force-output stream))
        (close-socket socket)))))

(defun run-client (name &key detach-others)
  "Attach to the server at (socket-path NAME): forward stdin + resizes, render
   the frames the server returns, and exit on detach / server close.
   When DETACH-OTHERS is T, send a detach-others command before attaching so
   the server disconnects any currently attached clients."
  (require :sb-posix)
  (let ((socket (connect-to (socket-path name))))
    (unwind-protect
         (let ((stream            (socket-stream socket))
               (server-socket-fd  (socket-fd socket)))
           (multiple-value-setq (*term-rows* *term-cols*) (terminal-size))
           (setf *resize-pending* nil)
           (install-sigwinch-handler)
           (with-raw-mode
             (clear-display)
             ;; -d flag: ask the server to detach any existing clients first.
             (when detach-others
               (send-frame stream
                           (msg-command :detach-other-clients nil nil)))
             (send-frame stream (msg-attach *term-rows* *term-cols*))
             (loop
               (when *resize-pending*
                 (setf *resize-pending* nil)
                 (multiple-value-setq (*term-rows* *term-cols*) (terminal-size))
                 (send-frame stream (msg-resize *term-rows* *term-cols*)))
               (let ((ready (select-fds (list 0 server-socket-fd) +poll-timeout-us+)))
                 (when (member 0 ready)
                   (let ((b (read-byte-nonblock 0)))
                     (when b (send-frame stream (msg-key (vector b))))))
                 (when (member server-socket-fd ready)
                   (with-incoming-frame (type payload stream)
                     ((null type)        (return))
                     ((= type +msg-bye+) (return))
                     ((= type +msg-frame+)
                      (write-string (decode-text payload))
                      (force-output))))))))
      (close-socket socket))))
