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
   keystroke STATE (so each client has independent prefix/copy-mode state), and
   the ROWS×COLS geometry it last reported."
  socket
  stream
  fd
  state
  (rows 24 :type fixnum)
  (cols 80 :type fixnum))

(defvar *clients* nil
  "List of CLIENT-CONN structs currently attached to the multi-client server.
   Mutated only by the single server event loop, so it needs no locking.")

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
    (setf *dirty* t)
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

;;; ── Effective geometry (smallest attached client) ───────────────────────────

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
           (values (reduce #'max *clients* :key #'client-conn-rows)
                   (reduce #'max *clients* :key #'client-conn-cols)))
          ((string-equal mode "latest")
           (let ((c (first *clients*)))
             (values (client-conn-rows c) (client-conn-cols c))))
          ((string-equal mode "manual")
           (values *term-rows* *term-cols*))
          (t                            ; "smallest" and any unknown value
           (values (reduce #'min *clients* :key #'client-conn-rows)
                   (reduce #'min *clients* :key #'client-conn-cols)))))))

(defun %apply-effective-size (session)
  "Set *term-rows*/*term-cols* to the effective (smallest-client) geometry,
   relayout SESSION's active window for the new size, and mark the screen dirty."
  (multiple-value-bind (rows cols) (%effective-client-size)
    (setf *term-rows* rows *term-cols* cols)
    (let ((win (session-active-window session)))
      (when win (window-relayout win (- rows *status-height*) cols)))
    (setf *dirty* t)))

;;; ── Frame broadcast ─────────────────────────────────────────────────────────

(defun %broadcast-frame (session)
  "When *dirty* and at least one client is attached, render ONE frame and send it
   to every client, then clear *dirty*.  A client whose send raises is dropped so
   one dead peer cannot wedge the loop."
  (when (and *dirty* *clients*)
    (setf *dirty* nil)
    (let ((frame (msg-frame (render-session-to-string
                             session *term-rows* *term-cols*))))
      (dolist (conn (copy-list *clients*))
        (handler-case (send-frame (client-conn-stream conn) frame)
          (error () (%drop-client conn)))))))

;;; ── Per-client message dispatch ─────────────────────────────────────────────

(defun %handle-multi-client-message (type payload session conn)
  "Dispatch one message of TYPE/PAYLOAD from client CONN.  Returns a disposition:
     :quit           — a command ended the session (loop must stop);
     :drop           — CONN should be removed (EOF / detach / unknown type);
     :detach-others  — drop every OTHER client (the `attach -d` request);
     NIL             — keep serving.
   Resize/attach updates CONN's geometry and re-applies the effective size; keys
   run through the shared prefix/copy-mode pipeline with CONN's private state."
  (cond
    ((null type) :drop)
    ((= type +msg-detach+) :drop)
    ((or (= type +msg-attach+) (= type +msg-resize+))
     (multiple-value-bind (rows cols) (decode-size payload)
       (setf (client-conn-rows conn) rows
             (client-conn-cols conn) cols))
     ;; Mark CONN most-recent so window-size "latest" tracks the active client.
     (setf *clients* (cons conn (remove conn *clients*)))
     (%apply-effective-size session)
     nil)
    ((= type +msg-key+)
     (case (process-client-keys session payload (client-conn-state conn))
       (:quit   :quit)
       (:detach :drop)
       (t       (setf *dirty* t) nil)))
    ((= type +msg-command+)
     (multiple-value-bind (cmd target args) (decode-command-payload payload)
       (cond
         ;; The one built-in control command: drop all OTHER clients (attach -d).
         ((eq cmd :detach-other-clients) :detach-others)
         ;; Any other named command is run server-side — the CLI / control
         ;; command-forwarding path (`cl-tmux <cmd>` against a running server).
         ;; Reconstruct the token line: <name> [-t target] args..., and dispatch
         ;; through the same %run-command-tokens the command-prompt uses.
         (cmd
          (let ((tokens (append (list (string-downcase (symbol-name cmd)))
                                (when target (list "-t" target))
                                args))
                ;; Capture the command's overlay text (display-message, list-*, …)
                ;; instead of showing it to interactive clients, so it can be
                ;; returned to the CLI command client — the `cl-tmux display -p`
                ;; (and `cl-tmux list-sessions`, …) stdout path.
                (cl-tmux/prompt:*overlay* nil))
            (ignore-errors (%run-command-tokens session tokens))
            ;; Reply with the captured output to the requesting client (a no-op
            ;; for the socket-less test conn, whose stream is NIL).
            (when (client-conn-stream conn)
              (ignore-errors
                (send-frame (client-conn-stream conn)
                            (msg-reply (or cl-tmux/prompt:*overlay* ""))))))
          (setf *dirty* t)
          nil)
         (t (setf *dirty* t) nil))))
    (t :drop)))

;;; ── Event-loop iteration ────────────────────────────────────────────────────

(defun %exit-after-last-detach-p ()
  "True when no clients remain attached AND the exit-unattached option is on — the
   server should terminate (tmux exit-unattached).  Checked only after a real
   client drop, so a freshly-started server with no clients yet never exits."
  (and (null *clients*)
       (cl-tmux/options:get-option "exit-unattached")))

(defun %exit-when-empty-p ()
  "True when no sessions remain AND exit-empty is on (default) — the server should
   terminate once its last session is destroyed (tmux exit-empty).  The server
   starts with an initial session, so this only becomes true after a session is
   killed during the loop, never at startup."
  (and (null *server-sessions*)
       (cl-tmux/options:get-option "exit-empty")))

(defun %multi-serve-iteration (listener session)
  "One iteration of the multi-client server loop: broadcast a dirty frame, then
   select on the listener fd + every client fd; accept a new connection when the
   listener is readable, and dispatch a message from each readable client.
   Returns :quit when the session must end, else NIL.  Factored out (taking the
   listener + session, mutating *clients*) so the dispatch/teardown logic is
   unit-testable without driving a full process loop."
  (%broadcast-frame session)
  (let* ((lfd   (socket-fd listener))
         (ready (select-fds (cons lfd (%client-fds)) +poll-timeout-us+)))
    (when ready
      ;; New connection: accept and register (accept may return NIL on a race).
      (when (member lfd ready)
        (let ((sock (accept-connection listener)))
          (when sock (%add-client sock))))
      ;; Readable clients: read + dispatch one frame each.
      (loop for conn in (copy-list *clients*)
            when (member (client-conn-fd conn) ready)
              do (let ((disp (handler-case
                                  (multiple-value-bind (type payload)
                                      (read-frame (client-conn-stream conn))
                                    (%handle-multi-client-message type payload session conn))
                                (error () :drop))))
                   (case disp
                     (:quit (return :quit))
                     (:drop
                      (%drop-client conn :bye t)
                      ;; exit-unattached: terminate once the last client has detached.
                      (when (%exit-after-last-detach-p)
                        (return :quit)))
                     (:detach-others
                      (dolist (other (copy-list *clients*))
                        (unless (eq other conn) (%drop-client other :bye t))))))))))

(defun %run-multi-server-loop (listener session)
  "Drive %multi-serve-iteration until *running* clears or a command ends the
   session.  Drops every remaining client (with a bye) on exit."
  (unwind-protect
       (loop while *running* do
         (when (eq :quit (%multi-serve-iteration listener session))
           (setf *running* nil))
         ;; exit-empty: terminate once the last session has been destroyed.
         (when (%exit-when-empty-p)
           (setf *running* nil)))
    (dolist (conn (copy-list *clients*))
      (%drop-client conn :bye t))))
