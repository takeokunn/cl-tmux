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

(defun socket-path (name)
  "Filesystem path of the Unix socket for the server session named NAME."
  (format nil "~A/cl-tmux-~A.sock"
          (string-right-trim "/" (or (sb-ext:posix-getenv "TMPDIR") "/tmp"))
          name))

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

(defun run-server (name)
  "Run a headless server owning a session, serving clients attaching to
   (socket-path NAME).  The session persists across detaches until its last
   window is killed."
  (require :sb-posix)
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
    (ignore-errors (delete-file path))
    (let ((listener (make-listener path)))
      (dolist (pane (all-panes session))
        (start-reader-thread pane))
      (setf *status-timer* (start-status-timer (lambda () (setf *dirty* t))))
      (install-sigwinch-handler)
      (unwind-protect
           ;; Multi-client event loop: a single select(2) over the listener fd +
           ;; every attached client fd, serving them all concurrently
           ;; (%run-multi-server-loop, server-multi.lisp).
           (%run-multi-server-loop listener session)
        (close-socket listener)
        (ignore-errors (delete-file path))
        (dolist (pane (all-panes session))
          (ignore-errors (pty-close (pane-fd pane) (pane-pid pane))))))))
