(in-package #:cl-tmux)

;;;; Multi-client server: a single select(2)-multiplexed event loop that serves
;;;; MANY attached clients at once, instead of the one-client-at-a-time model in
;;;; server.lisp (accept → serve-one-until-detach → accept-next).
;;;;
;;;; The loop owns a registry of connected clients (*clients*).  Each iteration:
;;;;   1. broadcasts a freshly rendered frame to every client when *dirty*;
;;;;   2. select()s on the listener fd + every client fd together;
;;;;   3. accepts a new connection when the listener is readable;
;;;;   4. dispatches a message from each readable client (keys/resize/detach/cmd).
;;;;
;;;; The session, PTYs, and per-pane reader threads are unchanged — the reader
;;;; threads still set *dirty* when pane output arrives, and the broadcast step
;;;; fans that single rendered frame out to all clients.  Because every client
;;;; receives the SAME frame, the session is rendered at the SMALLEST attached
;;;; client's geometry (%effective-client-size) so no client is sent a frame
;;;; larger than its terminal.  (Per-client independent sizing is a future
;;;; refinement; this matches tmux's window-size "smallest" mode.)
;;;;
;;;; Reuses the shared pieces from server.lisp / protocol / transport:
;;;;   process-client-keys, decode-size, decode-command-payload, render-…,
;;;;   send-frame/read-frame, msg-frame/msg-bye, socket-fd/-stream/close-socket.

;;; ── Client connection registry ──────────────────────────────────────────────

(defstruct (client-conn (:constructor %make-client-conn))
  "One attached client: its socket, a cached binary STREAM and FD, a private
   keystroke STATE (so each client has independent prefix/copy-mode state), the
   ROWS×COLS geometry it last reported, an optional command-stdin target pane,
   and its private message log."
  socket
  stream
  fd
  state
  stdin-target
  (message-log nil)
  ;; T when the client attached read-only (attach-session -r): its keys/mouse are
  ;; processed with *client-read-only* bound so pane input/paste/mouse are dropped.
  (read-only-p nil)
  (rows 24 :type fixnum)
  (cols 80 :type fixnum))

(defvar *clients* nil
  "List of CLIENT-CONN structs currently attached to the multi-client server.
   Mutated only by the single server event loop, so it needs no locking.")

;;; with-loop-safe-error is defined in server-multi-dispatch.lisp (which loads
;;; first) so it is available at compile time to every user, including here.

(define-multi-msg-dispatch
  ;; EOF: peer closed the connection.
  ((null type) :drop)
  ;; Client requested clean detach.
  ((= type +msg-detach+) :drop)
  ;; Initial attach or resize: update CONN's geometry and re-apply effective size.
  ((or (= type +msg-attach+) (= type +msg-resize+))
   (%handle-multi-attach-or-resize session conn type payload))
  ;; Keystroke: feed to the pane's stdin-target (split-window -I) or run through
  ;; the shared prefix/copy-mode pipeline with CONN's private state.
  ((= type +msg-key+)
   (%handle-multi-key-message session conn payload))
  ;; Command forwarding: run-command from a CLI client or control-mode client.
  ((= type +msg-command+)
   (%handle-multi-command-message session conn payload))
  ;; Unknown message type: treat as disconnect.
  (t :drop))

;;; ── Effective geometry (smallest attached client) ───────────────────────────

(defun %client-size-reduce (fn)
  "Apply FN (e.g. #'min or #'max) across all attached clients' rows and cols,
   returning (values ROWS COLS)."
  (values (reduce fn *clients* :key #'client-conn-rows)
          (reduce fn *clients* :key #'client-conn-cols)))

(defun %effective-client-size ()
  "Return (values ROWS COLS) the session should render at, per the `window-size`
   option over the attached clients:
     smallest — min over all clients (default; the only size every client can
                fully display in cl-tmux's shared single-frame broadcast model);
     largest  — max over all clients;
     latest   — the most recently attached/resized client (*clients* is kept
                most-recent-first);
     manual   — keep the current *term-rows*/*term-cols* (no auto-resize).
   Falls back to *term-rows*/*term-cols* when no clients are attached.
   NOTE: largest/latest can exceed a smaller client's terminal — they are honoured
   for parity, but smallest stays the safe default for the shared-frame design."
  (if (null *clients*)
      (values *term-rows* *term-cols*)
      (let ((mode (or (cl-tmux/options:get-option "window-size") "smallest")))
        (cond
          ((string-equal mode "largest")
           (%client-size-reduce #'max))
          ((string-equal mode "latest")
           (let ((c (first *clients*)))
             (values (client-conn-rows c) (client-conn-cols c))))
          ((string-equal mode "manual")
           (values *term-rows* *term-cols*))
          (t                            ; "smallest" and any unknown value
           (%client-size-reduce #'min))))))

(defun %apply-effective-size (session)
  "Set *term-rows*/*term-cols* to the effective (smallest-client) geometry,
   relayout SESSION's active window for the new size, and mark the screen dirty."
  (multiple-value-bind (rows cols) (%effective-client-size)
    (setf *term-rows* rows *term-cols* cols)
    (%relayout-active-window session rows cols)
    (%mark-dirty)))

;;; ── Frame broadcast ─────────────────────────────────────────────────────────

(defun %render-frame (session)
  "Pure: render SESSION at the current *term-rows* x *term-cols* and return the
   encoded +msg-frame+ byte vector.  No I/O side effects — the only inputs are
   the session model and the two dynamic vars."
  (msg-frame (render-session-to-string session *term-rows* *term-cols*)))

(defun %send-broadcast-frame (frame)
  "Effect boundary: send the pre-rendered FRAME to every attached client.
   A client whose send raises an error is silently dropped so one dead peer
   cannot wedge the broadcast loop."
  (dolist (conn (copy-list *clients*))
    (with-loop-safe-error (nil :on-error (%drop-client conn))
      (send-frame (client-conn-stream conn) frame))))

(defun %broadcast-frame (session)
  "When *dirty* and at least one client is attached, render ONE frame via
   %render-frame (pure) and broadcast it via %send-broadcast-frame (effect
   boundary), then clear *dirty*.  Factored into pure/effect layers so
   each step is independently testable."
  (when (and *dirty* *clients*)
    (setf *dirty* nil)
    (%send-broadcast-frame (%render-frame session))))

(defun %client-fds ()
  "The socket fds of every attached client (for the select read-set)."
  (mapcar #'client-conn-fd *clients*))

;;; ── Connection lifecycle ────────────────────────────────────────────────────

(defun %add-client (socket)
  "Register SOCKET as a new client: build its CLIENT-CONN (with a fresh keystroke
   state seeded to the current geometry), fire the client-attached hook, and mark
   the screen dirty so the new client gets an immediate paint.  Returns the conn."
  (let ((conn (%make-client-conn :socket socket
                                 :stream (socket-stream socket)
                                 :fd     (socket-fd socket)
                                 :state  (make-input-state)
                                 :rows   *term-rows*
                                 :cols   *term-cols*)))
    (push conn *clients*)
    (cl-tmux/hooks:run-hooks cl-tmux/hooks:+hook-client-attached+)
    (%mark-dirty)
    conn))

(defun %drop-client (conn &key bye)
  "Remove CONN: optionally send a bye frame, close its socket, fire the
   client-detached hook, and unregister it.  Safe to call more than once."
  (when (member conn *clients*)
    (when bye
      (ignore-errors (send-frame (client-conn-stream conn) (msg-bye))))
    (ignore-errors (close-socket (client-conn-socket conn)))
    (setf *clients* (remove conn *clients*))
    (cl-tmux/hooks:run-hooks cl-tmux/hooks:+hook-client-detached+)))
