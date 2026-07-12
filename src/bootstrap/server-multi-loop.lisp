(in-package #:cl-tmux)

;;; ── Event-loop iteration ────────────────────────────────────────────────────

(defun %exit-when-empty-and-option-enabled-p (items option-name)
  "True when ITEMS is empty and OPTION-NAME is enabled."
  (and (null items)
       (cl-tmux/options:get-option option-name)))

(defun %exit-after-last-detach-p ()
  "True when no clients remain attached AND the exit-unattached option is on — the
   server should terminate (tmux exit-unattached).  Checked only after a real
   client drop, so a freshly-started server with no clients yet never exits."
  (%exit-when-empty-and-option-enabled-p *clients* "exit-unattached"))

(defun %exit-when-empty-p ()
  "True when no sessions remain AND exit-empty is on (default) — the server should
   terminate once its last session is destroyed (tmux exit-empty).  The server
   starts with an initial session, so this only becomes true after a session is
   killed during the loop, never at startup."
  (%exit-when-empty-and-option-enabled-p *server-sessions* "exit-empty"))

(defun %accept-pending-connection (listener listener-fd ready)
  "When LISTENER-FD is in READY, accept and register the new connection.
   accept-connection may return NIL on a race (peer disappeared between
   select and accept), in which case nothing is registered."
  (when (member listener-fd ready)
    (let ((sock (accept-connection listener)))
      (when sock (%add-client sock)))))

(defun %read-and-dispatch-client-message (session conn)
  "Read one frame from CONN and dispatch it via %handle-multi-client-message.
   A read/decode error is treated as a disconnect (:drop) so one malformed or
   dropped client cannot take down the multi-client event loop."
  (with-loop-safe-error (nil :on-error :drop)
    (multiple-value-bind (type payload) (read-frame (client-conn-stream conn))
      (%handle-multi-client-message type payload session conn))))

(defun %apply-client-disposition (disposition conn)
  "Act on DISPOSITION (the result of dispatching CONN's message): drop CONN on
   :drop (and exit if that was the last attached client with exit-unattached
   set), drop every OTHER client on :detach-others.  Returns :quit when the
   caller's loop must stop, else NIL."
  (case disposition
    (:quit :quit)
    (:drop
     (%drop-client conn :bye t)
     ;; exit-unattached: terminate once the last client has detached.
     (when (%exit-after-last-detach-p) :quit))
    (:detach-others
     (dolist (other (copy-list *clients*))
       (unless (eq other conn) (%drop-client other :bye t)))
     nil)))

(defun %dispatch-ready-clients (session ready)
  "Read + dispatch one message from every client whose fd is in READY.
   Returns :quit as soon as any client's disposition ends the session, else NIL
   once every ready client has been served."
  (loop for conn in (copy-list *clients*)
        when (member (client-conn-fd conn) ready)
          do (let ((disposition (%read-and-dispatch-client-message session conn)))
               (when (eq :quit (%apply-client-disposition disposition conn))
                 (return :quit)))))

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
      (%accept-pending-connection listener listener-fd ready)
      (%dispatch-ready-clients session ready))))

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
