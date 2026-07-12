;;; Command-client I/O helpers.

(in-package :cl-tmux)

(defconstant +command-reply-timeout-us+ 2000000
  "Microseconds the CLI command client waits for the server's +msg-reply+ before
   giving up (2 s) -- bounds the wait so a hung server never blocks the client.")

(defconstant +command-reply-max-frames+ 10000
  "Maximum number of +msg-frame+ broadcasts the command client skips while waiting
   for +msg-reply+.  Prevents a continuously broadcasting server from delaying the
   reply indefinitely -- a server that saturates this limit is considered to have
   not replied and the client returns as if it timed out.")

(defconstant +stdin-read-max-octets+ (* 4 1024 1024)
  "Maximum bytes buffered from stdin for split-window -I forwarding (4 MiB).
   Bounds memory use when stdin is a large pipe that never closes -- reads stop
   once this limit is reached even if stdin still has data available.")

(defun %read-command-reply (stream socket-fd)
  "Read frames from STREAM until the server's +msg-reply+ arrives (skipping any
   rendered +msg-frame+/bye the multi-client server may broadcast first), and
   write its text to *standard-output*.  Gives up after +command-reply-timeout-us+
   of silence, on EOF, or after +command-reply-max-frames+ ignored broadcasts.
   This is the stdout side of `cl-tmux display -p ...`.
   SOCKET-FD is the raw file descriptor for STREAM, used by select-fds."
  (loop repeat +command-reply-max-frames+ do
    (unless (select-fds (list socket-fd) +command-reply-timeout-us+)
      (return))                          ; timed out waiting for a reply
    (with-incoming-frame (type payload stream)
      ((null type) (return))             ; EOF
      ((= type +msg-reply+)
       (let ((text (decode-text payload)))
         (when (plusp (length text))
           (write-string text)
           (unless (char= (char text (1- (length text))) #\Newline) (terpri))
           (force-output)))
       (return))
      ;; +msg-frame+ / +msg-bye+ etc.: a broadcast the command client ignores.
      (t nil))))

(defun %command-client-split-window-input-p (args)
  "True when ARGS names canonical split-window with the -I flag,
   indicating that the client must also forward its stdin to the new pane."
  (and args
       (string= "split-window" (string-downcase (first args)))
       (some (lambda (arg)
               (and (> (length arg) 1)
                    (char= (char arg 0) #\-)
                    (find #\I arg :start 1)))
             (rest args))))

(defun %utf8-char-byte-count (character)
  "Return the number of UTF-8 bytes needed to encode CHARACTER."
  (let ((code-point (char-code character)))
    (cond ((< code-point #x80)   1)
          ((< code-point #x800)  2)
          ((< code-point #x10000) 3)
          (t                      4))))

(defun %read-command-client-stdin-octets ()
  "Read command-client stdin as UTF-8 bytes for split-window -I forwarding.
   Stops at EOF or when +stdin-read-max-octets+ have been accumulated, whichever
   comes first -- prevents an indefinite hang when stdin is a long-running pipe
   that never closes (e.g. `some-process | cl-tmux split-window -I`)."
  (babel:string-to-octets
    (with-output-to-string (output-accumulator)
      (let ((byte-count 0))
        (loop for character = (read-char *standard-input* nil nil)
              while (and character (< byte-count +stdin-read-max-octets+))
              do (write-char character output-accumulator)
                 (incf byte-count (%utf8-char-byte-count character)))))
    :encoding :utf-8))

(defun %maybe-forward-command-client-stdin (stream args)
  "Forward stdin to STREAM when ARGS requests split-window -I."
  (when (%command-client-split-window-input-p args)
    (let ((bytes (%read-command-client-stdin-octets)))
      (when (plusp (length bytes))
        (send-frame stream (msg-key bytes))))))

(defun run-command-client (name args)
  "Forward ARGS -- a command name followed by its arguments -- to the running server
   for session NAME as a single +msg-command+ frame, then print the server's text
   reply (the command's output, e.g. `cl-tmux display -p '#{session_name}'`) and
   exit.  This is the `cl-tmux <command>` CLI path: it drives a server from outside
   instead of attaching a terminal.  A target given as `-t <target>` flows through
   in ARGS -- the server parses it like any other flag.
   When ARGS is NIL (no command words provided) this is a deliberate no-op: the
   caller is responsible for filtering out the empty-args case before invoking."
  (require :sb-posix)
  ;; Intentional early exit: no args means no command to forward.
  (when args
    (let ((socket (connect-to (socket-path name))))
      (unwind-protect
           (let ((stream (socket-stream socket)))
             (send-frame stream (msg-command (first args) nil (rest args)))
              (%maybe-forward-command-client-stdin stream args)
              (force-output stream)
              (%read-command-reply stream (socket-fd socket)))
         (close-socket socket)))))
