(in-package #:cl-tmux)

;;;; Multi-client message handlers extracted from server-multi.lisp.
;;;;
;;;; The event loop keeps the dispatch table, while these helpers own the
;;;; per-message policy for attach/resize, keys, and forwarded commands.

(defun %handle-multi-attach-or-resize (session conn type payload)
  "Update CONN's geometry from PAYLOAD, keep attach -r state, refresh client
   ordering for window-size latest, and reapply the effective shared size."
  (multiple-value-bind (rows cols) (decode-size payload)
    (setf (client-conn-rows conn) rows
          (client-conn-cols conn) cols))
  ;; attach-session -r carries read-only state in the optional attach flags byte.
  (when (= type +msg-attach+)
    (setf (client-conn-read-only-p conn)
          (logtest (decode-attach-flags payload) +attach-flag-read-only+)))
  ;; Keep this client most-recent so window-size latest follows the active peer.
  (setf *clients* (cons conn (remove conn *clients*)))
  (%apply-effective-size session)
  nil)

(defun %handle-multi-key-message (session conn payload)
  "Feed PAYLOAD through the stdin-target fast path or the shared key pipeline."
  (let ((stdin-target (client-conn-stdin-target conn)))
    (if stdin-target
        (progn
          (pane-feed stdin-target payload)
          (%mark-dirty)
          nil)
        ;; Bind *client-read-only* so the shared leaf-level gating keeps attach -r
        ;; clients from writing to panes or forwarding mouse/paste input.
        (let ((*client-read-only* (client-conn-read-only-p conn)))
          (case (process-client-keys session payload (client-conn-state conn))
            (:quit   :quit)
            (:detach :drop)
            (t       (%mark-dirty) nil))))))

(defun %handle-multi-command-message (session conn payload)
  "Run a forwarded command or detach other clients, returning the loop disposition."
  (multiple-value-bind (cmd target args) (decode-command-payload payload)
    (cond
      ;; The one built-in control command: drop all OTHER clients (attach -d).
      ((eq cmd :detach-other-clients) :detach-others)
      ;; Any other named command is run server-side and replied to CONN.
      (cmd (%dispatch-forwarded-command session conn cmd target args))
      (t (%mark-dirty) nil))))

;;; ── Per-client message dispatch ─────────────────────────────────────────────

(defun %split-window-input-arg-p (arg)
  "True when ARG requests split-window -I."
  (and (> (length arg) 1)
       (char= (char arg 0) #\-)
       (find #\I arg :start 1)))

(defun %server-split-window-input-command-p (cmd args)
  "True when decoded command payload requests canonical split-window -I."
  (and (eq cmd :split-window)
       (some #'%split-window-input-arg-p args)))

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
  (with-loop-safe-error (condition
                          :on-error (progn
                                      (format *error-output*
                                              "~&cl-tmux: command failed: ~{~A~^ ~}: ~A~%"
                                              tokens condition)
                                      (force-output *error-output*)
                                      nil))
    (let ((*defer-split-window-input* input-command-p))
      (%run-command-tokens session tokens))))

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
    (%mark-dirty)
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
