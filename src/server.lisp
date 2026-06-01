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
  ;; 3. Name prefix match
  (dolist (pair *server-sessions*)
    (when (and (stringp (car pair))
               (>= (length (car pair)) (length name))
               (string= name (car pair)
                         :end2 (min (length name) (length (car pair)))))
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

(defun %next-group-id ()
  "Allocate a fresh group-id (simple integer counter)."
  (1+ (length *session-groups*)))

(defun server-new-session-in-group (new-session existing-session)
  "Add NEW-SESSION to the same session group as EXISTING-SESSION.
   If EXISTING-SESSION is not yet in a group, a new group is created.
   Both sessions will share the same window list."
  (let ((gid (or (session-group existing-session)
                 (%next-group-id))))
    ;; Ensure the existing session has a group id.
    (setf (session-group existing-session) gid)
    ;; Share the windows list and set active window.
    (setf (session-windows new-session) (session-windows existing-session)
          (session-group   new-session) gid)
    ;; Use session-select-window to set the active window in the new session.
    (let ((aw (session-active-window existing-session)))
      (when aw
        (session-select-window new-session aw)))
    ;; Register in the group alist.
    (let ((entry (assoc gid *session-groups*)))
      (if entry
          (pushnew new-session (cdr entry))
          (push (list gid existing-session new-session) *session-groups*)))))

(defun server-sessions-in-group (group-id)
  "Return the list of sessions sharing GROUP-ID."
  (cdr (assoc group-id *session-groups*)))

;;;; The server owns the session, PTYs, and per-pane reader threads, and serves
;;;; one attached client at a time over a Unix socket.  Client keystrokes are
;;;; run through the SAME process-byte pipeline the in-process loop uses, so
;;;; prefix commands / copy mode / prompts all behave identically when attached.
;;;; On detach the client disconnects but the session persists for re-attach;
;;;; the server only exits when the last window is killed (:quit) or *running*
;;;; is cleared.

;;; ── Frame dispatch macro ────────────────────────────────────────────────────
;;;
;;; Both serve-client (server) and run-client (client) read one frame then
;;; dispatch on its type.  The Prolog-like table makes each handler a named
;;; clause that reads independently of the others.
;;;
;;;   handle_frame(nil, _)           :- disconnect.    % EOF
;;;   handle_frame(msg_detach, _)    :- disconnect.    % explicit detach
;;;   handle_frame(msg_attach|msg_resize, payload) :- apply_size(session, payload).
;;;   handle_frame(msg_key, payload) :- process_keys(session, payload).

(defmacro with-incoming-frame ((type-var payload-var stream) &rest rules)
  "Read one frame from STREAM, bind TYPE-VAR and PAYLOAD-VAR, then dispatch
   through the Prolog-like rule table RULES.  Each RULE is (condition &rest body).
   NIL type (EOF) must be handled by the caller if no rule matches."
  `(multiple-value-bind (,type-var ,payload-var) (read-frame ,stream)
     (cond ,@(mapcar (lambda (rule)
                       (destructuring-bind (condition &rest body) rule
                         `(,condition ,@body)))
                     rules))))

(defun socket-path (name)
  "Filesystem path of the Unix socket for the server session named NAME."
  (format nil "~A/cl-tmux-~A.sock"
          (string-right-trim "/" (or (sb-ext:posix-getenv "TMPDIR") "/tmp"))
          name))

(defun apply-client-size (session payload)
  "Apply a client size PAYLOAD (rows,cols) to SESSION and relayout."
  (multiple-value-bind (rows cols) (decode-size payload)
    (setf *term-rows* rows *term-cols* cols)
    (let ((win (session-active-window session)))
      (when win
        (window-relayout win (- rows *status-height*) cols)))
    (setf *dirty* t)))

(defun process-client-keys (session payload state)
  "Feed a client key PAYLOAD through `process-byte` (the shared keystroke
   pipeline) one byte at a time, updating keystroke STATE.  Returns the
   serve-loop disposition:
     :quit   — a command ended the session (also clears *running*);
     :detach — the user requested detach (the client should disconnect);
     NIL     — keep serving (the caller should mark the screen dirty).
   Takes the already-decoded PAYLOAD (not a socket), so the serve loop's
   quit/detach decision is unit-testable without a live client connection."
  (loop for b across payload
        for result = (process-byte session b state)
        when (eq result :quit)   do (setf *running* nil) (return :quit)
        when (eq result :detach) do (return :detach)
        finally (return nil)))

(defun serve-client (session socket)
  "Serve one attached client on SOCKET until it detaches or disconnects.

   Returns :quit when the session itself should end (e.g. last window killed),
   NIL on a plain detach/disconnect (the server keeps the session alive)."
  (let ((stream (socket-stream socket))
        (fd     (socket-fd socket))
        (state  (make-input-state))
        (outcome nil))
    (unwind-protect
         (block serve
           (setf *dirty* t)               ; force an initial paint for the client
           (loop while *running* do
             ;; Push a fresh frame whenever the session changed.
             (when *dirty*
               (setf *dirty* nil)
               (send-frame stream
                           (msg-frame (render-session-to-string
                                       session *term-rows* *term-cols*))))
             ;; Wait briefly for an incoming client message.
             (when (select-fds (list fd) +poll-timeout-us+)
               (with-incoming-frame (type payload stream)
                 ((null type)
                  (return-from serve))
                 ((= type +msg-detach+)
                  (return-from serve))
                 ((or (= type +msg-attach+) (= type +msg-resize+))
                  (apply-client-size session payload))
                 ((= type +msg-key+)
                  (case (process-client-keys session payload state)
                    (:quit   (setf outcome :quit) (return-from serve))
                    (:detach (return-from serve))
                    (t       (setf *dirty* t))))))))
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
    (setf *server-sessions* nil)
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
                      (when (eq :quit (serve-client session (accept-connection listener)))
                        (setf *running* nil))))
        (close-socket listener)
        (ignore-errors (delete-file path))
        (dolist (pane (all-panes session))
          (ignore-errors (pty-close (pane-fd pane) (pane-pid pane))))))))
