(in-package #:cl-tmux)

;;;; Detach-attach client.
;;;;
;;;; A thin terminal: it puts its own stdin in raw mode, forwards keystrokes and
;;;; resizes to the server as protocol frames, and paints the rendered frames
;;;; the server sends back.  It holds no session state — all prefix handling and
;;;; rendering happen server-side, so the client is the same for any session.

(defun run-client (name)
  "Attach to the server at (socket-path NAME): forward stdin + resizes, render
   the frames the server returns, and exit on detach / server close."
  (require :sb-posix)
  (let ((socket (connect-to (socket-path name))))
    (unwind-protect
         (let ((stream (socket-stream socket))
               (sfd    (socket-fd socket)))
           (multiple-value-setq (*term-rows* *term-cols*) (terminal-size))
           (setf *resize-pending* nil)
           (install-sigwinch-handler)
           (with-raw-mode
             (clear-display)
             (send-frame stream (msg-attach *term-rows* *term-cols*))
             (loop
               ;; A SIGWINCH since last frame → tell the server the new size.
               (when *resize-pending*
                 (setf *resize-pending* nil)
                 (multiple-value-setq (*term-rows* *term-cols*) (terminal-size))
                 (send-frame stream (msg-resize *term-rows* *term-cols*)))
               (let ((ready (select-fds (list 0 sfd) 50000)))
                 ;; stdin → forward one keystroke to the server.
                 (when (member 0 ready)
                   ;; NB: arg is a µs timeout (the fn always reads stdin); 0 = non-blocking grab.
                   (let ((b (read-byte-nonblock 0)))
                     (when b
                       (send-frame stream (msg-key (vector b))))))
                 ;; server → paint a frame, or exit on bye/EOF.
                 (when (member sfd ready)
                   (multiple-value-bind (type payload) (read-frame stream)
                     (cond
                       ((null type)           (return))   ; server gone
                       ((= type +msg-bye+)    (return))   ; detached / closed
                       ((= type +msg-frame+)
                        (write-string (decode-text payload))
                        (force-output)))))))))
      (close-socket socket))))
