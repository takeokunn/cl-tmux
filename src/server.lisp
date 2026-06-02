(in-package #:cl-tmux)

;;;; Detach-attach server.
;;;;
;;;; *server-sessions* is the authoritative registry of all live sessions (defvar
;;;; lives in runtime.lisp so dispatch.lisp can reference it before server loads).
;;;; run-server initialises it with the single initial session; new-session
;;;; (in dispatch.lisp) adds further sessions; kill-session removes them.

(defun server-add-session (session)
  "Register SESSION in *server-sessions* keyed by (session-name session).
   If a session with the same name already exists it is replaced."
  (setf *server-sessions*
        (cons (cons (session-name session) session)
              (remove (session-name session) *server-sessions*
                      :key #'car :test #'string=))))

(defun server-find-session (name)
  "Find a session by NAME in *server-sessions*.
   Match order:
     1. Exact name match
     2. $N notation (session id)
     3. Name prefix match (first matching session wins)
   Returns the session or NIL."
  (unless (and name (plusp (length name))) (return-from server-find-session nil))
  ;; 1. Exact name match
  (let ((exact (cdr (assoc name *server-sessions* :test #'string=))))
    (when exact (return-from server-find-session exact)))
  ;; 2. $N: match by session id
  (when (char= (char name 0) #\$)
    (let ((id (ignore-errors (parse-integer (subseq name 1)))))
      (when id
        (let ((by-id (find id (mapcar #'cdr *server-sessions*)
                           :key #'session-id)))
          (when by-id (return-from server-find-session by-id))))))
  ;; 3. Name prefix match — guard already ensures (length (car pair)) >= (length name),
  ;;    so :end2 (length name) is the correct clamp without the redundant min.
  (dolist (pair *server-sessions*)
    (when (and (stringp (car pair))
               (>= (length (car pair)) (length name))
               (string= name (car pair) :end2 (length name)))
      (return-from server-find-session (cdr pair))))
  nil)

(defun server-current-session ()
  "Return the most recently active session (highest session-last-active).
   Returns NIL when no sessions are registered."
  (let ((sessions (mapcar #'cdr *server-sessions*)))
    (when sessions
      (reduce (lambda (a b)
                (if (> (session-last-active b) (session-last-active a)) b a))
              sessions))))

(defun server-remove-session (name)
  "Remove the session named NAME from *server-sessions*."
  (setf *server-sessions*
        (remove name *server-sessions* :key #'car :test #'string=)))

(defun server-all-sessions ()
  "Return a list of all active sessions."
  (mapcar #'cdr *server-sessions*))

;;; ── Session groups ────────────────────────────────────────────────────────────
;;;
;;; A session group is a set of sessions that share the same window list.
;;; Sessions in the same group see the same windows; switching window in one
;;; automatically switches all others in the group.

(defparameter *session-groups* nil
  "Alist mapping group-id → list of sessions in that group.")

(defvar *group-id-counter* 0
  "Monotonic counter for session group ids. Never decremented — id reuse is impossible.")

(defun %next-group-id ()
  "Allocate a fresh group-id via a monotonic counter (never derives from alist length)."
  (incf *group-id-counter*))

;;; ── Session group helpers (data / logic decomposed) ──────────────────────────
;;;
;;; server-new-session-in-group has four distinct phases:
;;;   1. Allocate/reuse a group-id   (pure: id derivation)
;;;   2. Link session structs         (mutation: windows + group slot — data only)
;;;   3. Sync active window           (logic: policy that new session mirrors existing)
;;;   4. Update the registry alist    (mutation: *session-groups*)
;;; Each phase is extracted into a named sub-function so it is independently
;;; readable and testable.

(defun %resolve-group-id (session)
  "Return SESSION's existing group-id, or allocate a fresh one and install it."
  (or (session-group session)
      (let ((gid (%next-group-id)))
        (setf (session-group session) gid)
        gid)))

(defun %link-session-to-group (new-session existing-session group-id)
  "Share EXISTING-SESSION's windows into NEW-SESSION and assign GROUP-ID.
   Pure data wiring only — does not select the active window (see %sync-active-window)."
  (setf (session-windows new-session) (session-windows existing-session)
        (session-group   new-session) group-id))

(defun %sync-active-window (new-session existing-session)
  "Mirror EXISTING-SESSION's active window selection into NEW-SESSION.
   This is the policy step (new session follows existing session's view)
   and is intentionally separate from the data-linkage in %link-session-to-group."
  (let ((active-window (session-active-window existing-session)))
    (when active-window
      (session-select-window new-session active-window))))

(defun %register-in-group-alist (session group-id)
  "Add SESSION to the *session-groups* alist under GROUP-ID."
  (let ((entry (assoc group-id *session-groups*)))
    (if entry
        (pushnew session (cdr entry))
        (push (list group-id session) *session-groups*))))

(defun server-new-session-in-group (new-session existing-session)
  "Add NEW-SESSION to the same session group as EXISTING-SESSION.
   If EXISTING-SESSION is not yet in a group, a new group is created.
   Both sessions will share the same window list."
  (let ((group-id (%resolve-group-id existing-session)))
    (%link-session-to-group new-session existing-session group-id)
    (%sync-active-window new-session existing-session)
    (%register-in-group-alist existing-session group-id)
    (%register-in-group-alist new-session group-id)))

;;;; The server owns the session, PTYs, and per-pane reader threads, and serves
;;;; one attached client at a time over a Unix socket.  Client keystrokes are
;;;; run through the SAME process-byte pipeline the in-process loop uses, so
;;;; prefix commands / copy mode / prompts all behave identically when attached.
;;;; On detach the client disconnects but the session persists for re-attach;
;;;; the server only exits when the last window is killed (:quit) or *running*
;;;; is cleared.
;;;;
;;;; with-incoming-frame is defined in cl-tmux/transport so both server and
;;;; client can use it without creating a circular dependency.

(defun socket-path (name)
  "Filesystem path of the Unix socket for the server session named NAME."
  (format nil "~A/cl-tmux-~A.sock"
          (string-right-trim "/" (or (sb-ext:posix-getenv "TMPDIR") "/tmp"))
          name))

(defun apply-client-size (session payload)
  "Apply a client size PAYLOAD (rows,cols) to SESSION and relayout.
   Pure resize transform: updates dimensions and relayouts the active window.
   Does NOT mutate the dirty flag — that is the caller's responsibility."
  (multiple-value-bind (rows cols) (decode-size payload)
    (setf *term-rows* rows *term-cols* cols)
    (let ((win (session-active-window session)))
      (when win
        (window-relayout win (- rows *status-height*) cols)))))

(defun %dispatch-byte-result (result)
  "Map a single process-byte RESULT to a serve-loop disposition.
   Returns :quit (also clears *running*), :detach, or NIL (continue).
   This is the continuation passed between bytes in the key-processing stream."
  (cond ((eq result :quit)   (setf *running* nil) :quit)
        ((eq result :detach) :detach)
        (t                   nil)))

(defun %process-bytes-cps (session bytes state index)
  "CPS key-stream walker: process BYTES starting at INDEX through `process-byte`,
   calling itself as the continuation after each byte that returns NIL.
   Returns the first non-NIL disposition from `%dispatch-byte-result`, or NIL
   when all bytes are consumed without a quit/detach."
  (if (>= index (length bytes))
      nil
      (let ((disposition (%dispatch-byte-result
                          (process-byte session (aref bytes index) state))))
        (if disposition
            disposition
            (%process-bytes-cps session bytes state (1+ index))))))

(defun process-client-keys (session payload state)
  "Feed a client key PAYLOAD through `process-byte` (the shared keystroke
   pipeline) one byte at a time via a CPS walker, updating keystroke STATE.
   Returns the serve-loop disposition:
     :quit   — a command ended the session (also clears *running*);
     :detach — the user requested detach (the client should disconnect);
     NIL     — keep serving (the caller should mark the screen dirty).
   Takes the already-decoded PAYLOAD (not a socket), so the serve loop's
   quit/detach decision is unit-testable without a live client connection."
  (%process-bytes-cps session payload state 0))

;;; ── Message-type dispatch macro ──────────────────────────────────────────────
;;;
;;; define-msg-dispatch follows the define-csi-rules / with-incoming-frame
;;; Prolog-dispatch pattern: a declarative rule table whose keys are message-type
;;; predicates and whose bodies are handler forms.  TYPE and PAYLOAD are bound in
;;; every rule body.  The generated function returns the serve-loop outcome.

(defmacro define-msg-dispatch (&rest rules)
  "Build a %handle-client-message function from a declarative message-type rule
   table.  Each RULE is (condition &rest body).  TYPE, PAYLOAD, SESSION, and
   STATE are bound in every rule body.  The generated function dispatches via
   COND and returns whatever the matching arm returns.

   Prolog analogy:
     handle_msg(nil,         _, _, _) :- disconnect.
     handle_msg(msg_detach,  _, _, _) :- disconnect.
     handle_msg(msg_attach,  p, s, _) :- apply_client_size(s, p).
     handle_msg(msg_key,     p, s, k) :- process_client_keys(s, p, k)."
  `(defun %handle-client-message (type payload session state)
     "Dispatch one incoming client message by TYPE.
      Returns :quit (session ends), :detach (client disconnects cleanly),
      :disconnect (EOF / unknown-type teardown), or NIL (continue serving).
      SESSION is the current session; STATE is the per-client keystroke state."
     (declare (ignorable state))
     (cond
       ,@(mapcar (lambda (rule)
                   (destructuring-bind (condition &rest body) rule
                     `(,condition ,@body)))
                 rules))))

(define-msg-dispatch
  ;; EOF: peer closed the connection.
  ((null type)
   :disconnect)
  ;; Client requested clean detach.
  ((= type +msg-detach+)
   :detach)
  ;; Initial attach or resize: update terminal dimensions and mark dirty.
  ((or (= type +msg-attach+) (= type +msg-resize+))
   (apply-client-size session payload)
   (setf *dirty* t)
   nil)
  ;; Keystroke: run through the shared prefix/copy-mode pipeline.
  ((= type +msg-key+)
   (case (process-client-keys session payload state)
     (:quit   :quit)
     (:detach :detach)
     (t       (setf *dirty* t) nil)))
  ;; Unknown message type: treat as a graceful disconnect.
  (t
   :disconnect))

(defun %serve-one-poll-iteration (session stream fd state)
  "Perform one poll iteration: push a dirty frame if needed, then dispatch any
   incoming client message.  Returns the serve-loop disposition:
     :quit       — session must end (clears *running*);
     :disconnect — EOF or unknown message (connection closed);
     :detach     — client requested clean detach;
     NIL         — keep serving."
  ;; Push a fresh frame whenever the session changed.
  (when *dirty*
    (setf *dirty* nil)
    (send-frame stream
                (msg-frame (render-session-to-string
                            session *term-rows* *term-cols*))))
  ;; Wait briefly for an incoming client message.
  (when (select-fds (list fd) +poll-timeout-us+)
    (multiple-value-bind (type payload) (read-frame stream)
      (%handle-client-message type payload session state))))

(defun serve-client (session socket)
  "Serve one attached client on SOCKET until it detaches or disconnects.

   Returns :quit when the session itself should end (e.g. last window killed),
   NIL on a plain detach/disconnect (the server keeps the session alive)."
  (let ((stream  (socket-stream socket))
        (fd      (socket-fd socket))
        (state   (make-input-state))
        (outcome nil))
    (unwind-protect
         (block serve
           (setf *dirty* t)               ; force an initial paint for the client
           (loop while *running* do
             (case (%serve-one-poll-iteration session stream fd state)
               (:quit       (setf outcome :quit) (return-from serve))
               (:disconnect (return-from serve))
               (:detach     (return-from serve)))))
      (ignore-errors (send-frame stream (msg-bye)))
      (close-socket socket))
    outcome))

(defun run-server (name)
  "Run a headless server owning a session, serving clients attaching to
   (socket-path NAME).  The session persists across detaches until its last
   window is killed."
  (require :sb-posix)
  (ignore-errors (load-config-file))
  (setf *running* t *dirty* t *resize-pending* nil)
  (let* ((session (create-initial-session *term-rows* *term-cols*))
         (path    (socket-path name)))
    (setf *server-sessions* nil
          *session-groups*   nil
          *group-id-counter* 0)
    (server-add-session session)
    (ignore-errors (delete-file path))
    (let ((listener (make-listener path)))
      (dolist (pane (all-panes session))
        (start-reader-thread pane))
      (install-sigwinch-handler)
      (unwind-protect
           (loop while *running*
                 ;; Poll with timeout so *running* is checked between accepts.
                 do (when (select-fds (list (socket-fd listener)) +accept-timeout-us+)
                      ;; accept-connection may return NIL on a timeout (select→accept race).
                      (let ((client (accept-connection listener)))
                        (when (and client (eq :quit (serve-client session client)))
                          (setf *running* nil)))))
        (close-socket listener)
        (ignore-errors (delete-file path))
        (dolist (pane (all-panes session))
          (ignore-errors (pty-close (pane-fd pane) (pane-pid pane))))))))
