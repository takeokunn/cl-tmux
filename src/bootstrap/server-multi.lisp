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
    (let ((win (session-active-window session)))
      (when win (window-relayout win (- rows *status-height*) cols)))
    (setf *dirty* t)))

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
    (handler-case (send-frame (client-conn-stream conn) frame)
      (error () (%drop-client conn)))))

(defun %broadcast-frame (session)
  "When *dirty* and at least one client is attached, render ONE frame via
   %render-frame (pure) and broadcast it via %send-broadcast-frame (effect
   boundary), then clear *dirty*.  Factored into pure/effect layers so
   each step is independently testable."
  (when (and *dirty* *clients*)
    (setf *dirty* nil)
    (%send-broadcast-frame (%render-frame session))))

;;; ── Per-client message dispatch ─────────────────────────────────────────────

(defun %server-split-window-input-command-p (cmd args)
  "True when decoded command payload requests split-window -I."
  (and (member cmd '(:split-window :splitw) :test #'eq)
       (some (lambda (arg)
               (and (> (length arg) 1)
                    (char= (char arg 0) #\-)
                    (find #\I arg :start 1)))
             args)))

(defun %forwarded-command-tokens (cmd target args)
  "Reconstruct the token line <name> [-t target] args... for a forwarded
   command, matching what the command-prompt would have typed interactively."
  (append (list (string-downcase (symbol-name cmd)))
          (when target (list "-t" target))
          args))

(defun %run-forwarded-command-tokens (session tokens input-command-p)
  "Run TOKENS server-side via %run-command-tokens, binding *defer-split-window-input*
   per INPUT-COMMAND-P.  Catches and reports any error so a bad forwarded command
   cannot take down the multi-client event loop; returns NIL on error."
  (handler-case
      (let ((*defer-split-window-input* input-command-p))
        (%run-command-tokens session tokens))
    (error (condition)
      (format *error-output*
              "~&cl-tmux: command failed: ~{~A~^ ~}: ~A~%"
              tokens condition)
      (force-output *error-output*)
      nil)))

(defun %reply-with-command-output (conn)
  "Send CONN the captured overlay text (display-message, list-*, ...) as a
   +msg-reply+ frame.  A no-op when CONN has no live socket (the test conn)."
  (when (client-conn-stream conn)
    (ignore-errors
      (send-frame (client-conn-stream conn)
                  (msg-reply (or cl-tmux/prompt:*overlay* ""))))))

(defun %dispatch-forwarded-command (session conn cmd target args)
  "Run a non-built-in forwarded command CMD/TARGET/ARGS server-side and reply to
   CONN with its output — the CLI / control command-forwarding path (`cl-tmux
   <cmd>` against a running server).  Sequencing contract: build tokens → run
   command → send reply → record stdin-target → mark dirty → return :quit if the
   command ended the session, else NIL."
  (let* ((tokens          (%forwarded-command-tokens cmd target args))
         (input-command-p (%server-split-window-input-command-p cmd args))
         ;; Capture the command's overlay text instead of showing it to
         ;; interactive clients, so it can be returned to the CLI command
         ;; client — the `cl-tmux display -p` (and `list-sessions`, ...) path.
         (cl-tmux/prompt:*overlay* nil)
         (*current-client-conn*   conn)
         (result (%run-forwarded-command-tokens session tokens input-command-p)))
    (%reply-with-command-output conn)
    (when (and input-command-p (cl-tmux/model::pane-p result))
      (setf (client-conn-stdin-target conn) result))
    (setf *dirty* t)
    (when (eq result :quit) :quit)))

;;; define-multi-msg-dispatch builds %handle-multi-client-message from a
;;; declarative rule table, delegating to define-message-dispatch-fn (server.lisp)
;;; so both event loops share the same COND-expansion engine.  TYPE, PAYLOAD,
;;; SESSION, and CONN are bound in every rule body.

(defmacro define-multi-msg-dispatch (&rest rules)
  "Build %handle-multi-client-message from a declarative message-type rule table.
   Each RULE is (condition &rest body).  TYPE, PAYLOAD, SESSION, and CONN are
   bound in every rule body.  Delegates to define-message-dispatch-fn (defined in
   server.lisp) so the single-client and multi-client dispatch macros share the
   same COND-expansion engine and cannot structurally diverge."
  `(define-message-dispatch-fn
       %handle-multi-client-message
       (type payload session conn)
       "Dispatch one message of TYPE/PAYLOAD from client CONN.  Returns a disposition:
     :quit           — a command ended the session (loop must stop);
     :drop           — CONN should be removed (EOF / detach / unknown type);
     :detach-others  — drop every OTHER client (the `attach -d` request);
     NIL             — keep serving.
   Resize/attach updates CONN's geometry and re-applies the effective size; keys
   run through the shared prefix/copy-mode pipeline with CONN's private state."
     ,@rules))

(define-multi-msg-dispatch
  ;; EOF: peer closed the connection.
  ((null type) :drop)
  ;; Client requested clean detach.
  ((= type +msg-detach+) :drop)
  ;; Initial attach or resize: update CONN's geometry and re-apply effective size.
  ((or (= type +msg-attach+) (= type +msg-resize+))
   (multiple-value-bind (rows cols) (decode-size payload)
     (setf (client-conn-rows conn) rows
           (client-conn-cols conn) cols))
   ;; attach-session -r: the read-only bit rides in the attach frame's optional
   ;; flags byte.  Record it on CONN so the +msg-key+ branch can bind
   ;; *client-read-only* and suppress pane input/paste/mouse for this client.
   (when (= type +msg-attach+)
     (setf (client-conn-read-only-p conn)
           (logtest (decode-attach-flags payload) +attach-flag-read-only+)))
   ;; Mark CONN most-recent so window-size "latest" tracks the active client.
   (setf *clients* (cons conn (remove conn *clients*)))
   (%apply-effective-size session)
   nil)
  ;; Keystroke: feed to the pane's stdin-target (split-window -I) or run through
  ;; the shared prefix/copy-mode pipeline with CONN's private state.
  ((= type +msg-key+)
   (let ((stdin-target (client-conn-stdin-target conn)))
     (if stdin-target
         (progn
           (pane-feed stdin-target payload)
           (setf *dirty* t)
           nil)
         ;; Bind *client-read-only* to this connection's flag so the existing
         ;; leaf-level enforcement (pane pty-write, paste, mouse forwarding)
         ;; honours attach-session -r per client.  Detach/copy-mode commands do
         ;; not pass through those gated sites, so they still work (CMD_READONLY).
         (let ((*client-read-only* (client-conn-read-only-p conn)))
           (case (process-client-keys session payload (client-conn-state conn))
             (:quit   :quit)
             (:detach :drop)
             (t       (setf *dirty* t) nil))))))
  ;; Command forwarding: run-command from a CLI client or control-mode client.
  ((= type +msg-command+)
   (multiple-value-bind (cmd target args) (decode-command-payload payload)
     (cond
       ;; The one built-in control command: drop all OTHER clients (attach -d).
       ((eq cmd :detach-other-clients) :detach-others)
       ;; Any other named command is run server-side and replied to CONN.
       (cmd (%dispatch-forwarded-command session conn cmd target args))
       (t (setf *dirty* t) nil))))
  ;; Unknown message type: treat as disconnect.
  (t :drop))

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
  (let* ((listener-fd (socket-fd listener))
         (ready       (select-fds (cons listener-fd (%client-fds)) +poll-timeout-us+)))
    (when ready
      ;; New connection: accept and register (accept may return NIL on a race).
      (when (member listener-fd ready)
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
