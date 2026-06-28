(in-package #:cl-tmux)

;;;; Server / environment / prompt-history handlers split out from dispatch-handlers-b.lisp.

(defconstant +ctrl-char-recover-mask+ #x40
  "Bitmask to recover the printable letter from a control-character byte.
   (logior ctrl-byte +ctrl-char-recover-mask+) yields the ASCII letter.
   Example: C-b is 0x02; (logior 0x02 0x40) = 0x42 = #\\b.")

(define-command-handlers
  ;; ── Server management ─────────────────────────────────────────────────────
  (:server-info
   (show-overlay
    (format nil "server info~%  sessions: ~D~%  term: ~Dx~D~%  prefix: C-~A (~D)"
            (length *server-sessions*)
            *term-cols* *term-rows*
            (code-char (logior cl-tmux/config:*prefix-key-code* +ctrl-char-recover-mask+))
            cl-tmux/config:*prefix-key-code*)))
  (:list-clients
   (show-built-overlay (s)
     (format s "clients~%")
     (if *server-sessions*
         (loop for (name . sess) in *server-sessions*
               do (format s "  ~A: ~A  ~Dx~D~%"
                          name (session-name sess) *term-cols* *term-rows*))
         (format s "  0: local  ~A  ~Dx~D~%"
                 (session-name session) *term-cols* *term-rows*))))
  (:suspend-client
   ;; Send SIGTSTP to the running process to suspend the client, matching
   ;; real tmux's C-b C-z behaviour.  Reset mouse and extended-keys reporting
   ;; first so the parent shell is not left receiving them while suspended.
   ;; ASSUMPTION: SIGTSTP is not blocked in this process (it is unblocked at
   ;; startup).  If blocked, kill returns without suspending and the client
   ;; continues running — which is preferable to a hard error.
   (disable-mouse-reporting)
   (disable-extended-keys)
   (ignore-errors (sb-posix:kill (sb-posix:getpid) sb-posix:sigtstp)))
  (:lock-server
   ;; Lock all sessions, not just the current one.
   (loop for entry in *server-sessions*
         for sess = (cdr entry)
         do (setf (session-locked-p sess) t)))

  ;; ── Environment ───────────────────────────────────────────────────────────
  (:show-environment
   (%cmd-show-environment-arg session nil))
  (:set-environment
   (prompt-nonempty "set-env NAME VALUE"
                    (lambda (input)
                      (let* ((parts (uiop:split-string input :separator " "))
                             (name  (first parts))
                             (value (format nil "~{~A~^ ~}" (rest parts))))
                        (when (and name (plusp (length name)))
                          (cl-tmux/model:session-set-environment session name value)
                          (%overlayf "set ~A=~A" name value))))))

  ;; ── resize-window ────────────────────────────────────────────────────────
  (:resize-window
   (with-active-window (win session)
     (prompt-nonempty "resize-window WxH"
                      (lambda (input)
                        (multiple-value-bind (cols rows)
                            (%parse-wxh input)
                          (when (and cols rows)
                            (window-relayout win rows cols)
                            (%overlayf "resized to ~Dx~D" cols rows)))))))

  ;; ── attach-session ───────────────────────────────────────────────────────
  (:attach-session
   (prompt-nonempty "attach-session -t name"
                    (lambda (name)
                      (let ((target (server-find-session name)))
                        (if target
                            (progn (%switch-to-session target)
                                   (%overlayf "attached to ~A" name))
                            (%overlayf "session not found: ~A" name))))))

  ;; ── respawn-window ───────────────────────────────────────────────────────
  ;; Restart the shell in every pane of the active window.
  (:respawn-window
   (with-active-window (win session)
     (let ((panes (window-panes win)))
       (dolist (pane panes)
         (let ((new-pane (respawn-pane session pane)))
           (start-reader-thread new-pane))))))

  ;; ── Prompt history ───────────────────────────────────────────────────────
  (:show-prompt-history
   (show-overlay
    (if *prompt-history*
        (with-output-to-string (s)
          (format s "prompt history~%")
          (dolist (entry (reverse *prompt-history*))
            (format s "  ~A~%" entry)))
        "(no prompt history)")))
  (:clear-prompt-history
   (setf *prompt-history* nil)))
