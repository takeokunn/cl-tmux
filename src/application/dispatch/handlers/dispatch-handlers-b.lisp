(in-package #:cl-tmux)

;;;; Command handler rule table — part II.
;;;;  Break/join pane, pipe, options, paste-buffer, mark/layout.
;;;;  Registered into *command-dispatch-table* via define-command-handlers.

(defconstant +named-layouts+
  (if (boundp '+named-layouts+)
      (symbol-value '+named-layouts+)
      #(:even-horizontal :even-vertical :tiled :main-horizontal :main-vertical))
  "The ordered cycle of named window layouts used by next-layout and previous-layout.")

(defun %toggle-mark-pane (pane)
  "Toggle PANE as the server-wide marked pane: un-marks it when already marked,
   otherwise clears any prior mark and marks PANE."
  (cond
    ((eq pane *server-marked-pane*)
     (setf (pane-marked pane) nil
           *server-marked-pane* nil))
    (t
     (when *server-marked-pane*
       (setf (pane-marked *server-marked-pane*) nil))
     (setf (pane-marked pane)  t
           *server-marked-pane* pane))))

(defun %cycle-layout (session win direction)
  "Cycle the layout of WIN in DIRECTION (:next or :prev) through +named-layouts+."
  (let* ((current (cl-tmux/model:window-layout-cycle-index win))
         (n       (length +named-layouts+))
         (next    (mod (if (eq direction :next) (1+ current) (1- current)) n)))
    (setf (cl-tmux/model:window-layout-cycle-index win) next)
    (%apply-named-layout-to-session session (aref +named-layouts+ next))))

(define-command-handlers
  ;; ── Break / join pane ─────────────────────────────────────────────────────
  (:break-pane
   (with-active-window (win session)
     (when (> (length (window-panes win)) 1)
       (let ((new-win (break-pane session)))
         (when new-win
           (start-reader-thread (window-active-pane new-win)))))))
  (:join-pane
   (with-active-window (dst-win session)
     (prompt-integer "join-pane from window"
                      (lambda (idx)
                        (let* ((src-win  (nth idx (session-windows session)))
                               (src-pane (and src-win (window-active-pane src-win))))
                          (when src-pane
                            (join-pane session src-win src-pane dst-win :h)))))))

  ;; ── Pipe pane / synchronize ────────────────────────────────────────────────
  (:pipe-pane
   (with-active-pane (ap session)
     (if (pane-pipe-active-p ap)
         (pipe-pane-close ap)
         (prompt-nonempty "pipe-pane command"
                          (lambda (cmd) (pipe-pane-open ap cmd))))))
  (:synchronize-panes
   (%toggle-synchronize-panes))
  (:lock-session
   (setf (session-locked-p session) t))
  (:unlock-session
   (setf (session-locked-p session) nil))

  ;; ── Miscellaneous commands ─────────────────────────────────────────────────
  (:refresh-client
   ;; Force an immediate redraw of the terminal.  Useful after terminal resize
   ;; or when the display has been corrupted by another program.
   (setf *dirty* t))
  (:send-keys
   (with-active-pane (ap session)
     (prompt-nonempty "send-keys"
                      (lambda (input) (send-keys-to-pane ap input)))))
  (:clock-mode
   (with-active-pane (ap session)
     (setf *clock-mode-pane-id*
           (if (eql *clock-mode-pane-id* (pane-id ap))
               nil
               (pane-id ap)))))
  (:show-messages
   (show-overlay (%format-message-log-overlay)))
  (:show-hooks
   (show-overlay (cl-tmux/hooks:describe-command-hooks)))
  (:capture-pane
   (with-active-pane (ap session)
     (show-overlay (capture-pane ap))))
  (:clear-history
   (with-active-pane (ap session)
     (cl-tmux/terminal/actions:clear-scrollback (pane-screen ap))))
  (:choose-tree
   (show-built-overlay (stream)
     (let ((current-name (session-name session)))
       (loop for (name . sess) in (or *server-sessions*
                                      (list (cons current-name session)))
             do (%format-tree-entry stream name current-name
                                    (session-windows  sess)
                                    (session-active-window sess))))))
  (:customize-mode
   ;; Bare bind / keypress form: show the full customize tree (no filter).  The
   ;; scriptable customize-mode -f form lives in *arg-command-table*.
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
     (%toggle-mark-pane ap)))
  (:clear-mark
   (when *server-marked-pane*
     (setf (pane-marked *server-marked-pane*) nil
           *server-marked-pane* nil)))
  (:select-layout-spread
   (%apply-named-layout-to-session session :even-horizontal))
  (:next-layout
   (with-active-window (win session)
     (%cycle-layout session win :next)))
  (:choose-client
   (show-built-overlay (stream)
     (format stream "Clients:~%")
     (format stream "  0: local  ~A  ~Dx~D~%"
             (session-name session)
             *term-cols*
             *term-rows*)))
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
  )
