(in-package #:cl-tmux)

;;;; Session / window / misc handlers split out from dispatch-handlers-b.lisp.

(define-command-handlers
  ;; ── detach -a (detach all OTHER clients) ──────────────────────────
  ;; In standalone mode this detaches the current session.
  ;; In server mode, this would detach all clients except the caller.
  ;; Here we lock all OTHER sessions as a proxy.
  (:detach-all-clients
   ;; Detach the current session and quit if requested, else just set *running* nil.
   (setf *running* nil)
   :detach)

  ;; ── move-pane ────────────────────────────────────────────────────────────
  ;; Move the active pane to a different window (like join-pane but interactive).
  (:move-pane
   (with-active-window (src-win session)
     (prompt-integer "move-pane to window (index)"
                     (lambda (idx)
                       (let* ((dst-win  (nth idx (session-windows session)))
                              (src-pane (and src-win (window-active-pane src-win))))
                         (when (and dst-win src-pane (not (eq src-win dst-win)))
                           (join-pane session src-win src-pane dst-win :h)))))))

  ;; ── list-panes ───────────────────────────────────────────────────────────
  ;; List all panes in the active window.
  (:list-panes
   (with-active-window (win session)
     (show-overlay
      (with-output-to-string (s)
        (let ((panes (window-panes win)))
          (if panes
              (loop for p in panes
                    for idx from 0
                    do (format s "~D: [~Dx~D] [~D,~D] pane ~D~A~%"
                               idx
                               (pane-width p) (pane-height p)
                               (pane-x p) (pane-y p)
                               (pane-id p)
                               (if (eq p (window-active-pane win)) " (active)" "")))
              (format s "(no panes)~%")))))))

  ;; ── list-commands ────────────────────────────────────────────────────────
  ;; List all recognized tmux commands.
  (:list-commands
   (show-overlay
    (with-output-to-string (s)
      (dolist (cmd (%list-command-public-names))
        (format s "~(~A~)~%" cmd)))))

  ;; ── kill-server ──────────────────────────────────────────────────────────
  ;; Terminate the server and all sessions.
  (:kill-server
   (loop for entry in *server-sessions*
         for sess = (cdr entry)
         do (dolist (pane (all-panes sess))
              (ignore-errors (pty-close (pane-fd pane) (pane-pid pane)))))
   (setf *running* nil)
   :quit)

  ;; ── previous-layout ──────────────────────────────────────────────────────
  ;; Cycle backward through named layouts (inverse of next-layout).
  (:previous-layout
   (with-active-window (win session)
     (%cycle-layout session win :prev)))

  ;; ── set-buffer ───────────────────────────────────────────────────────────
  (:set-buffer
   (prompt-nonempty "set-buffer text"
                    (lambda (text)
                      (cl-tmux/buffer:add-paste-buffer text)
                      (%overlayf "buffer set (~D chars)" (length text)))))

  ;; ── start-server ─────────────────────────────────────────────────────────
  ;; No-op when the server is already running (matches tmux behaviour).
  (:start-server
   (show-overlay "server already running"))

  ;; ── lock-client ──────────────────────────────────────────────────────────
  ;; Lock the client (equivalent to lock-session in standalone mode).
  (:lock-client
   (setf (session-locked-p session) t)
   (show-overlay "client locked"))

  ;; ── link-window ──────────────────────────────────────────────────────────
  ;; Bare :link-window (no args) — needs -s/-t targets to do anything useful.
  ;; The arg-taking form (link-window -s src -t dst) is handled by
  ;; %cmd-link-window in dispatch-core.lisp's *arg-command-table*.
  (:link-window
   (show-overlay "link-window: usage: link-window -s <src> -t <dst-session> [-k]"))

  ;; ── unlink-window ────────────────────────────────────────────────────────
  ;; Bare :unlink-window — the arg form (unlink-window -t target [-k]) is in
  ;; %cmd-unlink-window.  With no -t, unlink the active window if it is linked
  ;; in another session.
  (:unlink-window
   (%cmd-unlink-window session nil))

  )
