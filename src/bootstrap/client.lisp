(in-package #:cl-tmux)

;;;; Detach-attach client.
;;;;
;;;; A thin terminal: it puts its own stdin in raw mode, forwards keystrokes and
;;;; resizes to the server as protocol frames, and paints the rendered frames
;;;; the server sends back.  It holds no session state — all prefix handling and
;;;; rendering happen server-side, so the client is the same for any session.
;;;;
;;;; Event-loop decomposition:
;;;;   %maybe-send-resize    — pure resize check + frame send (testable without terminal)
;;;;   %forward-stdin-byte   — read one byte from stdin and forward it to the server
;;;;   %decode-server-frame  — pure: read one server frame, return disposition + text
;;;;   %receive-server-frame — effect boundary: call decode, then write text to stdout

;;; ── Constants ────────────────────────────────────────────────────────────────

(defconstant +command-reply-timeout-us+ 2000000
  "Microseconds the CLI command client waits for the server's +msg-reply+ before
   giving up (2 s) — bounds the wait so a hung server never blocks the client.")

(defconstant +command-reply-max-frames+ 10000
  "Maximum number of +msg-frame+ broadcasts the command client skips while waiting
   for +msg-reply+.  Prevents a continuously broadcasting server from delaying the
   reply indefinitely — a server that saturates this limit is considered to have
   not replied and the client returns as if it timed out.")

(defconstant +stdin-read-max-octets+ (* 4 1024 1024)
  "Maximum bytes buffered from stdin for split-window -I forwarding (4 MiB).
   Bounds memory use when stdin is a large pipe that never closes — reads stop
   once this limit is reached even if stdin still has data available.")

;;; ── %read-command-reply ──────────────────────────────────────────────────────

(defun %read-command-reply (stream socket-fd)
  "Read frames from STREAM until the server's +msg-reply+ arrives (skipping any
   rendered +msg-frame+/bye the multi-client server may broadcast first), and
   write its text to *standard-output*.  Gives up after +command-reply-timeout-us+
   of silence, on EOF, or after +command-reply-max-frames+ ignored broadcasts.
   This is the stdout side of `cl-tmux display -p …`.
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

;;; ── run-command-client ───────────────────────────────────────────────────────

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
   comes first — prevents an indefinite hang when stdin is a long-running pipe
   that never closes (e.g. `some-process | cl-tmux split-window -I`)."
  (babel:string-to-octets
   (with-output-to-string (output-accumulator)
     (let ((byte-count 0))
       (loop for character = (read-char *standard-input* nil nil)
             while (and character (< byte-count +stdin-read-max-octets+))
             do (write-char character output-accumulator)
                (incf byte-count (%utf8-char-byte-count character)))))
   :encoding :utf-8))

(defun run-command-client (name args)
  "Forward ARGS — a command name followed by its arguments — to the running server
   for session NAME as a single +msg-command+ frame, then print the server's text
   reply (the command's output, e.g. `cl-tmux display -p '#{session_name}'`) and
   exit.  This is the `cl-tmux <command>` CLI path: it drives a server from outside
   instead of attaching a terminal.  A target given as `-t <target>` flows through
   in ARGS — the server parses it like any other flag.
   When ARGS is NIL (no command words provided) this is a deliberate no-op: the
   caller is responsible for filtering out the empty-args case before invoking."
  (require :sb-posix)
  ;; Intentional early exit: no args means no command to forward.
  (when args
    (let ((socket (connect-to (socket-path name))))
      (unwind-protect
           (let ((stream (socket-stream socket)))
             (send-frame stream (msg-command (first args) nil (rest args)))
             (when (%command-client-split-window-input-p args)
               (let ((bytes (%read-command-client-stdin-octets)))
                 (when (plusp (length bytes))
                   (send-frame stream (msg-key bytes)))))
             (force-output stream)
             (%read-command-reply stream (socket-fd socket)))
        (close-socket socket)))))

;;; ── run-client event-loop helpers ───────────────────────────────────────────

(defun %maybe-send-resize (stream)
  "If *resize-pending* is set, clear it, sample the current terminal dimensions,
   update *term-rows* and *term-cols*, and send a +msg-resize+ frame on STREAM.
   Returns T when a resize frame was sent, NIL otherwise.
   This helper is extracted from the run-client event loop so the resize-dispatch
   path is independently testable without a live terminal."
  (when *resize-pending*
    (setf *resize-pending* nil)
    (multiple-value-setq (*term-rows* *term-cols*) (terminal-size))
    (send-frame stream (msg-resize *term-rows* *term-cols*))
    t))

(defun %forward-stdin-byte (stream)
  "Read one byte from stdin (non-blocking) and, if one is available, forward it
   to the server as a +msg-key+ frame on STREAM.  Returns T when a byte was
   forwarded, NIL when stdin had nothing ready."
  (let ((stdin-byte (read-byte-nonblock 0)))
    (when stdin-byte
      (send-frame stream (msg-key (vector stdin-byte)))
      t)))

(defun %decode-server-frame (stream)
  "Pure step: read one frame from the server STREAM and classify it.
   Returns (values disposition text) where:
     disposition  :exit    — server signalled end-of-session (+msg-bye+ or EOF);
                  :frame   — a rendered screen frame was received;
                  :ignore  — an unrecognised frame type (continue event loop).
     text         the decoded string payload for a :frame disposition, NIL otherwise.
   No I/O side effects — the caller (%receive-server-frame) decides what to write."
  (with-incoming-frame (type payload stream)
    ((null type)        (values :exit nil))
    ((= type +msg-bye+) (values :exit nil))
    ((= type +msg-frame+)
     (values :frame (decode-text payload)))
    (t (values :ignore nil))))

(defun %receive-server-frame (stream)
  "Effect boundary: read and dispatch one frame from the server STREAM.
   Calls %decode-server-frame (pure), then writes any :frame text to
   *standard-output* (the only side-effecting step).
   Returns :exit when the server signals end-of-session (+msg-bye+ or EOF),
   NIL to continue the event loop."
  (multiple-value-bind (disposition text) (%decode-server-frame stream)
    (case disposition
      (:exit   :exit)
      (:frame  (write-string text) (force-output) nil)
      (t       nil))))

;;; ── run-client ───────────────────────────────────────────────────────────────

(defun %receive-if-ready (stream server-socket-fd ready)
  "If SERVER-SOCKET-FD appears in the READY fd list, read and dispatch one server
   frame from STREAM via %receive-server-frame.  Returns :exit when the server
   signals end-of-session, NIL otherwise (including when the fd was not ready).
   Completes the naming symmetry with %maybe-send-resize and %forward-stdin-byte:
   every run-client event-loop action is a named helper so all three are
   independently unit-testable without driving the full attach loop."
  (when (member server-socket-fd ready)
    (%receive-server-frame stream)))

(defun run-client (name &key detach-others)
  "Attach to the server at (socket-path NAME): forward stdin + resizes, render
   the frames the server returns, and exit on detach / server close.
   When DETACH-OTHERS is T, send a detach-others command before attaching so
   the server disconnects any currently attached clients."
  (require :sb-posix)
  (let ((socket (connect-to (socket-path name))))
    (unwind-protect
         (let ((stream           (socket-stream socket))
               (server-socket-fd (socket-fd socket)))
           (multiple-value-setq (*term-rows* *term-cols*) (terminal-size))
           (setf *resize-pending* nil)
           (install-sigwinch-handler)
           (with-raw-mode
             (clear-display)
             ;; -d flag: ask the server to detach any existing clients first.
             (when detach-others
               (send-frame stream (msg-command :detach-other-clients nil nil)))
             ;; Carry the read-only bit (attach-session -r set *client-read-only*
             ;; in this client process) to the server in the attach frame's flags
             ;; byte, so the server enforces it per-connection.
             (send-frame stream (msg-attach *term-rows* *term-cols* *client-read-only*))
             (loop
               (%maybe-send-resize stream)
               (let ((ready (select-fds (list 0 server-socket-fd) +poll-timeout-us+)))
                 (when (member 0 ready)
                   (%forward-stdin-byte stream))
                 (when (eq :exit (%receive-if-ready stream server-socket-fd ready))
                   (return))))))
      (close-socket socket))))
