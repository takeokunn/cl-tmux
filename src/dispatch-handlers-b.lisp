(in-package #:cl-tmux)

;;;; Command handler rule table — part II.
;;;;  Popup/menu, break/join pane, pipe, prompt, options, paste-buffer,
;;;;  mark/layout, server management, environment, and miscellaneous.
;;;;  Registered into *command-dispatch-table* via define-command-handlers.

(define-command-handlers
  ;; ── Popup / menu overlays ──────────────────────────────────────────────────
  (:display-popup
   (prompt-start "popup command" ""
                 (lambda (cmd)
                   (unless (string= cmd "")
                     (let ((output (run-shell cmd)))
                       (show-popup (make-popup :title cmd
                                               :width  (min +popup-max-width+  *term-cols*)
                                               :height (min +popup-max-height+ (- *term-rows* +popup-margin+))
                                               :screen nil
                                               :pane   nil))
                       (show-overlay (%format-popup-overlay cmd output)))))))
  (:display-popup-dismiss
   (close-popup))
  (:display-menu
   (let ((items (list (cons "New Window"    :new-window)
                      (cons "Next Window"   :next-window)
                      (cons "Prev Window"   :prev-window)
                      (cons "Kill Pane"     :kill-pane)
                      (cons "Kill Window"   :kill-window)
                      (cons "Zoom Toggle"   :zoom-toggle)
                      (cons "List Sessions" :list-sessions)
                      (cons "Detach"        :detach))))
     (show-menu (make-menu :title "Menu" :items items :selected-index 0))
     (show-overlay (%format-menu *active-menu*))))
  (:menu-next
   (when *active-menu*
     (let ((n (length (menu-items *active-menu*))))
       (setf (menu-selected-index *active-menu*)
             (mod (1+ (menu-selected-index *active-menu*)) n))
       (show-overlay (%format-menu *active-menu*)))))
  (:menu-prev
   (when *active-menu*
     (let ((n (length (menu-items *active-menu*))))
       (setf (menu-selected-index *active-menu*)
             (mod (1- (menu-selected-index *active-menu*)) n))
       (show-overlay (%format-menu *active-menu*)))))
  (:menu-select
   (when *active-menu*
     (let* ((idx  (menu-selected-index *active-menu*))
            (item (nth idx (menu-items *active-menu*)))
            (cmd  (cdr item)))
       (close-menu)
       (clear-overlay)
       (when cmd
         (cond
           ;; Built-in keyword command: dispatch directly.
           ((keywordp cmd)
            (dispatch-command session cmd byte))
           ;; String command: run via command-line dispatcher.
           ((stringp cmd)
            (%run-command-line session cmd))
           ;; List command: (keyword arg) — dispatch with argument.
           ((and (consp cmd) (keywordp (first cmd)))
            (case (first cmd)
              ;; (:select-window N) — select window by id
              (:select-window
               (%with-window-focus-transition (session)
                 (select-window-by-number session (second cmd))))
              ;; (:switch-client name) — switch to named session
              (:switch-client
               (let ((target (server-find-session (second cmd))))
                 (when target
                   (%switch-to-session target))))
              ;; Default list: try running as command tokens
              (otherwise
               (%run-command-tokens session cmd)))))))))
  (:menu-dismiss
   (close-menu)
   (clear-overlay))

  ;; ── Break / join pane ─────────────────────────────────────────────────────
  (:break-pane
   (with-active-window (win session)
     (when (> (length (window-panes win)) 1)
       (let ((new-win (break-pane session)))
         (when new-win
           (start-reader-thread (window-active-pane new-win)))))))
  (:join-pane
   (with-active-window (dst-win session)
     (prompt-start "join-pane from window" ""
                   (lambda (idx-str)
                     (let ((idx (ignore-errors (parse-integer idx-str))))
                       (when idx
                         (let* ((src-win  (nth idx (session-windows session)))
                                (src-pane (and src-win (window-active-pane src-win))))
                           (when src-pane
                             (join-pane session src-win src-pane dst-win :h)))))))))

  ;; ── Pipe pane / synchronize ────────────────────────────────────────────────
  (:pipe-pane
   (with-active-pane (ap session)
     (if (pane-pipe-fd ap)
         (pipe-pane-close ap)
         (prompt-start "pipe-pane command" ""
                       (lambda (cmd)
                         (unless (string= cmd "")
                           (pipe-pane-open ap cmd)))))))
  (:synchronize-panes
   (%toggle-synchronize-panes))
  (:lock-session
   (setf (session-locked-p session) t))
  (:unlock-session
   (setf (session-locked-p session) nil))

  ;; ── Command prompt ────────────────────────────────────────────────────────
  (:command-prompt
   (prompt-start ": " ""
                 (lambda (input)
                   (unless (string= input "")
                     ;; Record to prompt history before executing.
                     (add-prompt-history input)
                     ;; Arg-aware: parses "<command> <args...>" so commands like
                     ;; display-message #{session_name} run with their arguments.
                     (%run-command-line session input)))))

  ;; ── Miscellaneous commands ─────────────────────────────────────────────────
  (:refresh-client
   ;; Force an immediate redraw of the terminal.  Useful after terminal resize
   ;; or when the display has been corrupted by another program.
   (setf *dirty* t))
  (:send-keys
   (with-active-pane (ap session)
     (prompt-start "send-keys" ""
                   (lambda (input)
                     (unless (string= input "")
                       (send-keys-to-pane ap input))))))
  (:clock-mode
   (with-active-pane (ap session)
     (setf *clock-mode-pane-id*
           (if (eql *clock-mode-pane-id* (pane-id ap))
               nil
               (pane-id ap)))))
  (:show-messages
   (show-overlay
    (if *message-log*
        (format nil "~{~A~%~}"
                (mapcar #'cdr *message-log*))
        "(no messages)")))
  (:show-hooks
   (show-overlay (cl-tmux/hooks:describe-command-hooks)))
  (:capture-pane
   (with-active-pane (ap session)
     (show-overlay (capture-pane ap))))
  (:clear-history
   (with-active-pane (ap session)
     (cl-tmux/terminal/actions:clear-scrollback (pane-screen ap))))
  (:choose-tree
   (show-overlay
    (with-output-to-string (stream)
      (let ((current-name (session-name session)))
        (if *server-sessions*
            (loop for (name . sess) in *server-sessions*
                  do (%format-tree-entry stream name current-name
                                         (session-windows sess)
                                         (session-active-window sess)))
            (%format-tree-entry stream current-name current-name
                                (session-windows session)
                                (session-active-window session)))))))
  (:customize-mode
   ;; Bare bind / keypress form: show the full customize tree (no filter).  The
   ;; scriptable customize-mode -f/-F/-t form lives in *arg-command-table*.
   (show-overlay (%format-customize-tree nil)))
  (:set-window-option  (%set-option-from-prompt "set-window-option"))
  (:set-session-option (%set-option-from-prompt "set-session-option"))

  ;; ── Paste-buffer commands ─────────────────────────────────────────────────
  ;; These delegate to helpers; the full implementations live in
  ;; dispatch-handlers-buffer.lisp, loaded after this file.
  (:list-buffers   (%cmd-list-buffers))
  (:show-buffer    (%cmd-show-buffer))
  (:choose-buffer  (%cmd-choose-buffer session))
  (:delete-buffer  (%cmd-delete-buffer))
  (:save-buffer    (%cmd-save-buffer))
  (:load-buffer    (%cmd-load-buffer))

  ;; ── Mark / layout helpers ─────────────────────────────────────────────────
  (:mark-pane
   (with-active-pane (ap session)
     (cond
       ((eq ap *server-marked-pane*)
        (setf (pane-marked ap) nil
              *server-marked-pane* nil))
       (t
        (when *server-marked-pane*
          (setf (pane-marked *server-marked-pane*) nil))
        (setf (pane-marked ap)       t
              *server-marked-pane*  ap)))))
  (:clear-mark
   (when *server-marked-pane*
     (setf (pane-marked *server-marked-pane*) nil
           *server-marked-pane* nil)))
  (:select-layout-spread
   (%apply-named-layout-to-session session :even-horizontal))
  (:next-layout
   (with-active-window (win session)
     (let* ((layouts #(:even-horizontal :even-vertical :tiled
                       :main-horizontal :main-vertical))
            (current (cl-tmux/model:window-layout-cycle-index win))
            (next    (mod (1+ current) (length layouts)))
            (name    (aref layouts next)))
       (setf (cl-tmux/model:window-layout-cycle-index win) next)
       (%apply-named-layout-to-session session name))))
  (:choose-client
   (show-overlay
    (with-output-to-string (stream)
      (format stream "Clients:~%")
      (format stream "  0: local  ~A  ~Dx~D~%"
              (session-name session)
              *term-cols*
              *term-rows*))))
  (:display-info
   (with-active-pane (ap session)
     (let* ((win    (session-active-window session))
            (screen (pane-screen ap)))
       (show-overlay
        (format nil "Session: ~A~%Window: ~A (~Dx~D) [~D pane~:P]~%Pane: ~D at (~D,~D) ~Dx~D~A"
                (session-name session)
                (if win (window-name win) "none")
                (if win (window-width  win) 0)
                (if win (window-height win) 0)
                (if win (length (window-panes win)) 0)
                (pane-id ap)
                (pane-x ap) (pane-y ap)
                (pane-width ap) (pane-height ap)
                (if (and screen (screen-copy-mode-p screen)) " [copy]" ""))))))
  (:move-window-prompt
   (with-active-window (win session)
     (prompt-start "move-window to index" ""
                   (lambda (idx-str)
                     (let ((idx (ignore-errors (parse-integer idx-str))))
                       (when idx
                         (session-move-window session win idx)))))))
  (:bind-key
   (prompt-start "bind key: " ""
                 (lambda (input)
                   (unless (string= input "")
                     (let* ((parts   (uiop:split-string input :separator " "))
                            (key-tok (and (first parts)
                                         (cl-tmux/config::%parse-key-token (first parts))))
                            (cmd-str (second parts))
                            (kw      (and cmd-str
                                          (cl-tmux/config::%command-keyword cmd-str))))
                       (if kw
                           (progn
                             (set-key-binding key-tok kw)
                             (show-overlay (format nil "bound ~A -> ~(~A~)" key-tok kw)))
                           (show-overlay (format nil "unknown command: ~A"
                                                 (or cmd-str input)))))))))
  (:unbind-key
   (prompt-start "unbind key: " ""
                 (lambda (input)
                   (unless (string= input "")
                     (let ((k (cl-tmux/config::%parse-key-token input)))
                       (remove-key-binding k)
                       (show-overlay (format nil "unbound ~A" k)))))))
  (:select-window-prompt
   (prompt-start "select window (name or number): " ""
                 (lambda (input)
                   (unless (string= input "")
                     (let* ((idx (ignore-errors (parse-integer input)))
                            (win (or (and idx (nth idx (session-windows session)))
                                     (find input (session-windows session)
                                           :key #'window-name
                                           :test #'string-equal))))
                       (if win
                           (%with-window-focus-transition (session)
                             (session-select-window session win))
                           (show-overlay (format nil "no window: ~A" input))))))))

  ;; ── Server management ─────────────────────────────────────────────────────
  (:server-info
   (show-overlay
    (format nil "server info~%  sessions: ~D~%  term: ~Dx~D~%  prefix: C-~A (~D)"
            (length *server-sessions*)
            *term-cols* *term-rows*
            (code-char (logior cl-tmux/config:*prefix-key-code* #x40))
            cl-tmux/config:*prefix-key-code*)))
  (:list-clients
   (show-overlay
    (with-output-to-string (s)
      (format s "clients~%")
      (if *server-sessions*
          (loop for (name . sess) in *server-sessions*
                do (format s "  ~A: ~Dx~D~%"
                           name *term-cols* *term-rows*))
          (format s "  0: local  ~A  ~Dx~D~%"
                  (session-name session) *term-cols* *term-rows*)))))
  (:suspend-client
   ;; Send SIGTSTP to the running process to suspend the client, matching
   ;; real tmux's C-b C-z behaviour.  Reset mouse and extended-keys reporting
   ;; first so the parent shell is not left receiving them while suspended.
   (disable-mouse-reporting)
   (disable-extended-keys)
   (ignore-errors (sb-posix:kill (sb-posix:getpid) sb-posix:sigtstp)))
  (:lock-server
   ;; Lock all sessions, not just the current one.
   (dolist (entry *server-sessions*)
     (setf (session-locked-p (cdr entry)) t)))

  ;; ── Environment ───────────────────────────────────────────────────────────
  (:show-environment
   (show-overlay
    (with-output-to-string (s)
      (format s "environment~%")
      (dolist (pair (cl-tmux/model:get-update-environment-vars))
        (format s "  ~A=~A~%" (car pair) (cdr pair))))))
  (:set-environment
   (prompt-start "set-env NAME VALUE" ""
                 (lambda (input)
                   (unless (string= input "")
                     (let* ((parts (uiop:split-string input :separator " "))
                            (name  (first parts))
                            (value (format nil "~{~A~^ ~}" (rest parts))))
                       (when (and name (plusp (length name)))
                         (ignore-errors
                           (let ((fn (find-symbol "SETENV" (find-package "SB-POSIX"))))
                             (when fn (funcall fn name value 1))))
                         (show-overlay (format nil "set ~A=~A" name value))))))))

  ;; ── resize-window ────────────────────────────────────────────────────────
  ;; Resize the active window's terminal (not just the split layout).
  ;; In standalone mode this matches the terminal size; with -x/-y it lets
  ;; a config override the window size so sub-windows can differ in size.
  (:resize-window
   (with-active-window (win session)
     (prompt-start "resize-window WxH" ""
                   (lambda (input)
                     (unless (string= input "")
                       (let* ((x-pos (position #\x input :test #'char-equal))
                              (cols  (when x-pos (parse-integer input :end x-pos :junk-allowed t)))
                              (rows  (when x-pos (parse-integer input :start (1+ x-pos)
                                                                :junk-allowed t))))
                         (when (and cols rows (> cols 0) (> rows 0))
                           (window-relayout win rows cols)
                           (show-overlay (format nil "resized to ~Dx~D" cols rows)))))))))

  ;; ── attach-session ───────────────────────────────────────────────────────
  ;; Switch the local client to a named session.
  (:attach-session
   (prompt-start "attach-session -t name" ""
                 (lambda (name)
                   (unless (string= name "")
                     (let ((target (server-find-session name)))
                       (if target
                           (progn (%switch-to-session target)
                                  (show-overlay (format nil "attached to ~A" name)))
                           (show-overlay (format nil "session not found: ~A" name))))))))

  ;; ── respawn-window ───────────────────────────────────────────────────────
  ;; Restart the shell in every pane of the active window.
  (:respawn-window
   (with-active-window (win session)
     (let ((panes (window-panes win)))
       (dolist (pane panes)
         (let ((new-pane (respawn-pane pane)))
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
   (setf *prompt-history* nil))

  ;; ── detach-client -a (detach all OTHER clients) ──────────────────────────
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
     (prompt-start "move-pane to window (index)" ""
                   (lambda (idx-str)
                     (let ((idx (ignore-errors (parse-integer idx-str))))
                       (when idx
                         (let* ((dst-win  (nth idx (session-windows session)))
                                (src-pane (and src-win (window-active-pane src-win))))
                           (when (and dst-win src-pane (not (eq src-win dst-win)))
                             (join-pane session src-win src-pane dst-win :h)))))))))

  ;; ── list-panes ───────────────────────────────────────────────────────────
  ;; List all panes in the active window (lsp alias).
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
  ;; List all recognized tmux commands (lscm alias).
  (:list-commands
   (show-overlay
    (with-output-to-string (s)
      (dolist (cmd (sort (copy-list cl-tmux/config::*bindable-commands*)
                         #'string< :key #'symbol-name))
        (format s "~(~A~)~%" cmd)))))

  ;; ── kill-server ──────────────────────────────────────────────────────────
  ;; Terminate the server and all sessions.
  (:kill-server
   (dolist (entry *server-sessions*)
     (let ((sess (cdr entry)))
       (dolist (pane (all-panes sess))
         (ignore-errors (pty-close (pane-fd pane) (pane-pid pane))))))
   (setf *running* nil)
   :quit)

  ;; ── previous-layout ──────────────────────────────────────────────────────
  ;; Cycle backward through named layouts (inverse of next-layout).
  (:previous-layout
   (with-active-window (win session)
     (let* ((layouts #(:even-horizontal :even-vertical :tiled
                       :main-horizontal :main-vertical))
            (current (cl-tmux/model:window-layout-cycle-index win))
            (prev    (mod (1- current) (length layouts)))
            (name    (aref layouts prev)))
       (setf (cl-tmux/model:window-layout-cycle-index win) prev)
       (%apply-named-layout-to-session session name))))

  ;; ── set-buffer ───────────────────────────────────────────────────────────
  ;; Set the contents of the most-recent paste buffer (setb alias).
  (:set-buffer
   (prompt-start "set-buffer text" ""
                 (lambda (text)
                   (unless (string= text "")
                     (cl-tmux/buffer:add-paste-buffer text)
                     (show-overlay (format nil "buffer set (~D chars)" (length text)))))))

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

  ;; ── select-pane -m / mark pane as the marked pane ────────────────────────
  ;; :mark-pane already handles this; this alias keeps the tmux name.
  (:select-pane-mark
   (with-active-pane (ap session)
     (cond
       ((eq ap *server-marked-pane*)
        (setf (pane-marked ap) nil
              *server-marked-pane* nil))
       (t
        (when *server-marked-pane*
          (setf (pane-marked *server-marked-pane*) nil))
        (setf (pane-marked ap)       t
              *server-marked-pane*  ap)))))

  ;; ── detach-client with -s (detach all clients attached to a session) ─────
  (:detach-client
   (setf *running* nil)
   :detach))
