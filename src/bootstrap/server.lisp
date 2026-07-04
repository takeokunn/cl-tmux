(in-package #:cl-tmux)

;;;; Detach-attach server: socket serve-loop.
;;;;
;;;; The server owns the session, PTYs, and per-pane reader threads, and serves
;;;; one attached client at a time over a Unix socket.  Client keystrokes are
;;;; run through the SAME process-byte pipeline the in-process loop uses, so
;;;; prefix commands / copy mode / prompts all behave identically when attached.
;;;; On detach the client disconnects but the session persists for re-attach;
;;;; the server only exits when the last window is killed (:quit) or *running*
;;;; is cleared.
;;;;
;;;; Session registry management (server-add/find/remove/all/current-session and
;;;; session groups) lives in session-registry.lisp.
;;;;
;;;; with-incoming-frame is defined in cl-tmux/transport so both server and
;;;; client can use it without creating a circular dependency.

(defvar *bound-socket-path* nil
  "The socket path this server actually bound (#{socket_path}); NIL in
   standalone mode where no socket exists.")

(defvar *client-flags* nil
  "The single client's flag list (refresh-client -f, #{client_flags}):
   a list of flag-name strings, e.g. (\"no-output\" \"read-only\").")

(defvar *socket-path-override* nil
  "Full socket path from the global -S flag (tmux -S); when set, socket-path
   returns it verbatim for every server name.")

(defvar *socket-name-override* nil
  "Socket name from the global -L flag (tmux -L); when set, it replaces the
   server-name-derived socket file name inside the per-UID socket directory.")

(defun %socket-tmp-base ()
  "The socket base directory: $TMUX_TMPDIR, else $TMPDIR, else /tmp — the same
   precedence real tmux uses."
  (let ((tmux-tmpdir (sb-ext:posix-getenv "TMUX_TMPDIR"))
        (tmpdir      (sb-ext:posix-getenv "TMPDIR")))
    (string-right-trim
     "/"
     (cond ((and tmux-tmpdir (plusp (length tmux-tmpdir))) tmux-tmpdir)
           ((and tmpdir (plusp (length tmpdir))) tmpdir)
           (t "/tmp")))))

(defun %socket-directory ()
  "Per-UID socket directory <base>/cl-tmux-<uid> (tmux's /tmp/tmux-UID/),
   created mode 0700 when possible.  Returns the directory string without a
   trailing slash.  Creation/chmod failures are ignored — socket binding will
   surface a real permission problem with a better error."
  (require :sb-posix)
  (let* ((uid (handler-case (sb-posix:getuid) (error () 0)))
         (dir (format nil "~A/cl-tmux-~D" (%socket-tmp-base) uid)))
    (ignore-errors
      (ensure-directories-exist (format nil "~A/" dir))
      (sb-posix:chmod dir #o700))
    dir))

(defun socket-path (name)
  "Filesystem path of the Unix socket for the server named NAME.
   tmux layout: sockets live in a private per-UID directory under $TMUX_TMPDIR
   (or $TMPDIR, or /tmp).  The global -S flag (*socket-path-override*) supplies
   a verbatim path; -L (*socket-name-override*) picks a different socket name
   in the per-UID directory."
  (or *socket-path-override*
      (format nil "~A/cl-tmux-~A.sock"
              (%socket-directory)
              (or *socket-name-override* name))))

(defun apply-client-size (session payload)
  "Apply a client size PAYLOAD (rows,cols) to SESSION and relayout.
   Updates *term-rows*/*term-cols* and relayouts the active window.
   Does NOT mutate the dirty flag — that is the caller's responsibility."
  (multiple-value-bind (rows cols) (decode-size payload)
    (setf *term-rows* rows *term-cols* cols)
    (let ((active-window (session-active-window session)))
      (when active-window
        (window-relayout active-window (- rows *status-height*) cols)))))

(defun %dispatch-byte-result (result)
  "Map a single process-byte RESULT to a serve-loop disposition.
   Returns :quit, :detach, or NIL (continue).
   This is a pure predicate: it does NOT mutate *running*.
   The caller (%handle-client-message) is responsible for clearing *running*
   when the disposition is :quit."
  (cond ((eq result :quit)   :quit)
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
     :quit   — a command ended the session (caller must clear *running*);
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
;;;
;;; define-message-dispatch-fn is the shared COND-expansion engine used by both
;;; define-msg-dispatch (single-client server) and define-multi-msg-dispatch
;;; (multi-client server, server-multi.lisp + server-multi-loop.lisp).  Both
;;; wrappers delegate to it so the two event loops can never diverge in their
;;; macro structure.

(defmacro define-message-dispatch-fn (fn-name lambda-list docstring &rest rules)
  "Build a named message-dispatch function from a declarative rule table.
   FN-NAME is the symbol to DEFUN; LAMBDA-LIST is its full argument list;
   DOCSTRING is its documentation string.  Each RULE is (condition &rest body).
   The generated function dispatches via COND and returns whatever the matching
   arm returns.  Shared infrastructure for define-msg-dispatch (server.lisp) and
   define-multi-msg-dispatch (server-multi.lisp + server-multi-loop.lisp).

   Prolog analogy:
     fn(nil, ...) :- rule1-body.
     fn(T1,  ...) :- rule2-body.
     fn(T2,  ...) :- rule3-body."
  `(defun ,fn-name ,lambda-list
     ,docstring
     (cond
       ,@(mapcar (lambda (rule)
                   (destructuring-bind (condition &rest body) rule
                     `(,condition ,@body)))
                 rules))))

(defmacro define-msg-dispatch (&rest rules)
  "Build %handle-client-message from a declarative message-type rule table.
   Each RULE is (condition &rest body).  TYPE, PAYLOAD, SESSION, and STATE are
   bound in every rule body.  Delegates to define-message-dispatch-fn so this
   macro and define-multi-msg-dispatch (server-multi.lisp + server-multi-loop.lisp) share the same
   COND-expansion engine and cannot diverge in structure.

   Prolog analogy:
     handle_msg(nil,         _, _, _) :- disconnect.
     handle_msg(msg_detach,  _, _, _) :- disconnect.
     handle_msg(msg_attach,  p, s, _) :- apply_client_size(s, p).
     handle_msg(msg_key,     p, s, k) :- process_client_keys(s, p, k)."
  `(define-message-dispatch-fn
       %handle-client-message
       (type payload session state)
       "Dispatch one incoming client message by TYPE.
        Returns :quit (session ends, caller must clear *running*), :detach
        (client disconnects cleanly), :disconnect (EOF / unknown-type teardown),
        or NIL (continue serving).
        SESSION is the current session; STATE is the per-client keystroke state."
     ,@rules))

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
  ;; :quit arm also clears *running* here (effect boundary: %handle-client-message
  ;; is the outermost pure-ish boundary; the caller %run-multi-server-loop may
  ;; also act on :quit, but *running* must be cleared before that loop next polls).
  ((= type +msg-key+)
   (case (process-client-keys session payload state)
     (:quit   (setf *running* nil) :quit)
     (:detach :detach)
     (t       (setf *dirty* t) nil)))
  ;; Unknown message type: treat as a graceful disconnect.
  (t
   :disconnect))

(defun run-server (name)
  "Run a headless server owning a session, serving clients attaching to
   (socket-path NAME).  The session persists across detaches until its last
   window is killed."
  (require :sb-posix)
  (install-pty-port)              ; wire the CFFI PTY adapter into the domain port
  (install-session-repository)   ; wire the in-memory session store into the repository port
  (ignore-errors (load-config-file))
  (setf *running*          t
        *dirty*            t
        *resize-pending*   nil
        *server-sessions*  nil
        *session-groups*   nil
        *group-id-counter* 0)
  (let* ((session (create-initial-session *term-rows* *term-cols*))
         (path    (socket-path name)))
    (server-add-session session)
    (setf *bound-socket-path* path)
    (ignore-errors (delete-file path))
    (let ((listener (make-listener path)))
      (dolist (pane (all-panes session))
        (start-reader-thread pane))
      (setf *status-timer* (start-status-timer (lambda () (setf *dirty* t))))
      (install-sigwinch-handler)
      (unwind-protect
   ;; Multi-client event loop: a single select(2) over the listener fd +
   ;; every attached client fd, serving them all concurrently
   ;; (%run-multi-server-loop, server-multi-loop.lisp).
           (%run-multi-server-loop listener session)
        (close-socket listener)
        (ignore-errors (delete-file path))
        (dolist (pane (all-panes session))
          (ignore-errors (pty-close (pane-fd pane) (pane-pid pane))))))))
