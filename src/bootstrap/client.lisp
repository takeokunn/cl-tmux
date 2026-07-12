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
