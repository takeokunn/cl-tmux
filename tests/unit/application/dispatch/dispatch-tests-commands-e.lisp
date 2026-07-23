(in-package #:cl-tmux/test)

;;;; Dispatch tests — part E (from commands-b): switch-client-next/prev,
;;;; last-session, new-session, kill-session, find-window, mark-pane/clear-mark,
;;;; next-layout, choose-client, display-info, bind/unbind-key, list/show/delete-buffer,
;;;; choose-buffer, select-window-prompt, move-window, swap-window, wait-for,
;;;; copy-mode-active-p, signal-channel-prompt.

(describe "dispatch-suite"

  ;;; ── :switch-client-next / :switch-client-prev dispatch ───────────────────────

  ;; :switch-client-next touches the next session in the registry.
  (it "dispatch-switch-client-next-moves-to-next-session"
    (with-fake-session (s1 :nwindows 1)
      (let* ((s2  (make-fake-session :nwindows 1))
             (reg (list (cons (session-name s1) s1)
                        (cons (session-name s2) s2))))
        (let ((cl-tmux::*server-sessions* reg))
          (cl-tmux::dispatch-command s1 :switch-client-next nil)
          (expect cl-tmux::*dirty* :to-be-truthy)))))

  ;; :switch-client-prev dispatches without error.
  (it "dispatch-switch-client-prev-does-not-error"
    (with-fake-session (s)
      (finishes (cl-tmux::dispatch-command s :switch-client-prev nil)
                ":switch-client-prev must not signal an error")))

  ;;; ── :last-session dispatch ────────────────────────────────────────────────────

  ;; :last-session dispatches without error when only one session exists.
  (it "dispatch-last-session-does-not-error"
    (with-fake-session (s)
      (let ((cl-tmux::*server-sessions* (list (cons (session-name s) s))))
        (finishes (cl-tmux::dispatch-command s :last-session nil)
                  ":last-session must not signal an error"))))

  ;;; ── :new-session dispatch ─────────────────────────────────────────────────────

  ;; :new-session dispatches without error.
  (it "dispatch-new-session-does-not-error"
    (with-fake-session (s)
      (let ((cl-tmux::*server-sessions* nil))
        (finishes (cl-tmux::dispatch-command s :new-session nil)
                  ":new-session must not signal an error"))))

  ;;; ── :kill-session dispatch ────────────────────────────────────────────────────

  ;; :kill-session with no remaining sessions returns :quit.
  (it "dispatch-kill-session-with-no-other-sessions-quits"
    (with-fake-session (s)
      (let ((name (session-name s)))
        (let ((cl-tmux::*server-sessions* (list (cons name s))))
          (expect (eq :quit (cl-tmux::dispatch-command s :kill-session nil)))))))

  ;; kill-session -C clears window activity/silence alerts and does NOT kill the
  ;; session (tmux's kill-session -C).
  (it "cmd-kill-session-C-clears-alerts-without-killing"
    (with-fake-session (s :nwindows 2)
      (let ((name (session-name s)))
        (let ((cl-tmux::*server-sessions* (list (cons name s))))
          (dolist (win (session-windows s))
            (setf (cl-tmux/model:window-activity-flag win) t
                  (cl-tmux/model:window-silence-flag  win) t))
          (cl-tmux::%cmd-kill-session-arg s '("-C"))
          (expect (cl-tmux::server-find-session name))
          (dolist (win (session-windows s))
            (expect (null (cl-tmux/model:window-activity-flag win)))
            (expect (null (cl-tmux/model:window-silence-flag win))))))))

  ;;; ── :find-window dispatch ─────────────────────────────────────────────────────

  ;; :find-window, :bind-key, :unbind-key, :load-buffer, and :wait-for each open a prompt.
  (it "dispatch-simple-commands-open-prompt-table"
    (dolist (cmd '(:find-window :bind-key :unbind-key :load-buffer :wait-for))
      (with-fake-session (s)
        (let ((*prompt* nil))
          (cl-tmux::dispatch-command s cmd nil)
          (expect (prompt-active-p))))))

  ;;; ── :mark-pane / :clear-mark dispatch ────────────────────────────────────────

  ;; :mark-pane sets pane-marked and updates *server-marked-pane* to the active pane.
  (it "dispatch-mark-pane-marks-pane-and-sets-server-pointer"
    (with-fake-session (s)
      (let ((ap (session-active-pane s)))
        (setf (pane-marked ap) nil)
        (cl-tmux::dispatch-command s :mark-pane nil)
        (expect (pane-marked ap) :to-be-truthy)
        (expect (eq ap cl-tmux::*server-marked-pane*)))))

  ;; :mark-pane on an already-marked pane clears the mark (toggle).
  (it "dispatch-mark-pane-toggles-off"
    (with-fake-session (s)
      (let ((ap (session-active-pane s)))
        (cl-tmux::dispatch-command s :mark-pane nil)
        (expect (pane-marked ap) :to-be-truthy)
        (cl-tmux::dispatch-command s :mark-pane nil)
        (expect (pane-marked ap) :to-be-falsy))))

  ;; :clear-mark clears the server-wide marked pane.
  (it "dispatch-clear-mark-clears-server-marked-pane"
    (with-fake-session (s :nwindows 1 :npanes 1)
      (let ((ap (session-active-pane s)))
        (cl-tmux::dispatch-command s :mark-pane nil)
        (expect (pane-marked ap) :to-be-truthy)
        (cl-tmux::dispatch-command s :clear-mark nil)
        (expect (pane-marked ap) :to-be-falsy))))

  ;; :mark-pane in a second window clears the mark from a pane in the first window.
  (it "dispatch-mark-pane-cross-window-clears-previous"
    (with-fake-session (s :nwindows 2)
      (let* ((win1 (first  (session-windows s)))
             (win2 (second (session-windows s)))
             (p1   (window-active-pane win1))
             (p2   (window-active-pane win2)))
        (session-select-window s win1)
        (cl-tmux::dispatch-command s :mark-pane nil)
        (expect (pane-marked p1))
        (session-select-window s win2)
        (cl-tmux::dispatch-command s :mark-pane nil)
        (expect (pane-marked p1) :to-be-falsy)
        (expect (pane-marked p2))
        (expect (eq p2 cl-tmux::*server-marked-pane*)))))

  ;;; ── :next-layout dispatch ─────────────────────────────────────────────────────

  ;; :next-layout applies the next layout from the cycle table.
  (it "dispatch-next-layout-cycles-layout"
    (with-fake-session (s :nwindows 1 :npanes 1)
      (finishes (cl-tmux::dispatch-command s :next-layout nil)
                ":next-layout must not signal an error")))

  ;;; ── :select-layout-tiled / :select-layout-spread dispatch ────────────────────

  ;; :select-layout-tiled and :select-layout-spread dispatch without error.
  (it "dispatch-select-layout-tiled-and-spread-do-not-error"
    (with-fake-session (s :nwindows 1 :npanes 1)
      (dolist (cmd '(:select-layout-tiled :select-layout-spread))
        (finishes (cl-tmux::dispatch-command s cmd nil)
                  "~A must not signal an error" cmd))))

  ;;; ── :choose-client dispatch ───────────────────────────────────────────────────

  ;; :choose-client opens an overlay with client information.
  (it "dispatch-choose-client-shows-overlay"
    (with-fake-session (s)
      (let ((*overlay* nil))
        (cl-tmux::dispatch-command s :choose-client nil)
        (assert-overlay-contains "Clients" (overlay-lines) ":choose-client")
        (assert-overlay-contains (session-name s) (overlay-lines)
                                  ":choose-client"))))

  ;; Scriptable local chooser commands reject removed tmux customization inputs.
  (it "command-line-local-chooser-commands-reject-unsupported-args"
    (with-command-line-rejection-cases (line expected-message buffer-state
                                        '(("choose-client -F name" "choose-client: unsupported argument" nil)
                                          ("choose-buffer -N" "choose-buffer: unsupported argument" populated)
                                          ("choose-buffer template" "choose-buffer: unsupported argument" populated)
                                          ("choose-tree -Z" "choose-tree: unsupported argument" nil)
                                          ("choose-window -G" "choose-window: unsupported argument" nil)
                                          ("list-buffers -f name" "list-buffers: unsupported argument" populated)))
      (with-fake-session (s :nwindows 2)
        (let ((*overlay* nil)
              (cl-tmux/buffer:*paste-buffers*
                (when buffer-state
                  (list (cons "buffer0" "alpha")))))
          (cl-tmux::%run-command-line s line)
          (assert-overlay-contains expected-message *overlay* line)))))

  ;; list-commands metadata exposes the strict local chooser/list-buffer surface.
  (it "command-usage-table-omits-local-chooser-unsupported-args"
    (dolist (name '("choose-buffer" "choose-client" "choose-tree" "choose-window" "list-buffers"))
      (expect (string= "" (cdr (assoc name cl-tmux::*command-usage-table* :test #'string=))))))

  ;;; ── :display-info dispatch ────────────────────────────────────────────────────

  ;; :display-info opens an overlay with session/window/pane details.
  (it "dispatch-display-info-shows-overlay"
    (with-fake-session (s)
      (let ((*overlay* nil))
        (cl-tmux::dispatch-command s :display-info nil)
        (assert-overlay-contains "Session" (overlay-lines) ":display-info")
        (assert-overlay-contains "Pane" (overlay-lines) ":display-info"))))

  ;;; ── :list-buffers / :show-buffer / :delete-buffer dispatch ───────────────────

  ;; :list-buffers with empty buffer ring opens an overlay saying '(no paste buffers)'.
  (it "dispatch-list-buffers-no-buffers-shows-overlay"
    (with-fake-session (s)
      (let ((*overlay* nil)
            (cl-tmux/buffer:*paste-buffers* nil))
        (cl-tmux::dispatch-command s :list-buffers nil)
        (assert-overlay-contains "no paste buffers" (overlay-lines)
                                  ":list-buffers"))))

  ;; :list-buffers with buffers lists them by name with their content preview.
  (it "dispatch-list-buffers-populated-shows-entries"
    (with-fake-session (s)
      (let ((*overlay* nil)
            (cl-tmux/buffer:*paste-buffers* (list (cons "buffer1" "hello")
                                                  (cons "buffer0" "world"))))
        (cl-tmux::dispatch-command s :list-buffers nil)
        (assert-overlay-contains "hello" (overlay-lines) ":list-buffers")
        (assert-overlay-contains "world" (overlay-lines) ":list-buffers")
        (assert-overlay-contains "buffer1:" (overlay-lines) ":list-buffers"))))

  ;; :show-buffer opens an overlay with buffer 0's content.
  (it "dispatch-show-buffer-shows-content"
    (with-fake-session (s)
      (let ((*overlay* nil)
            (cl-tmux/buffer:*paste-buffers* (list (cons "buffer0" "test-content"))))
        (cl-tmux::dispatch-command s :show-buffer nil)
        (assert-overlay-contains "test-content" (overlay-lines)
                                  ":show-buffer"))))

  ;; :delete-buffer removes the first paste buffer.
  (it "dispatch-delete-buffer-removes-first-entry"
    (with-fake-session (s)
      (let ((cl-tmux/buffer:*paste-buffers* (list (cons "buffer0" "todelete"))))
        (cl-tmux::dispatch-command s :delete-buffer nil)
        (expect (null cl-tmux/buffer:*paste-buffers*)))))

  ;; %paste-buffer-text replaces LF with CR by default so a multi-line paste
  ;; submits each line; -r (no-replace) keeps the raw bytes.
  (it "paste-buffer-text-translates-lf-to-cr-by-default"
    (dolist (c (list (list (format nil "a~Cb~Cc" #\Return #\Return)
                           (format nil "a~%b~%c") nil "default: LF -> CR")
                     (list (format nil "a~%b~%c")
                           (format nil "a~%b~%c") t   "-r: keep LF unchanged")
                     (list "abc" "abc" nil "no newlines -> unchanged")
                     (list nil   nil   nil "NIL contents -> NIL")))
      (destructuring-bind (expected input no-replace desc) c
        (declare (ignore desc))
        (expect (equal expected (cl-tmux::%paste-buffer-text input no-replace))))))

  ;; %paste-buffer-text -s SEPARATOR replaces LF with SEPARATOR instead of CR; -r
  ;; still wins (raw), and SEP may be empty or multi-character.
  (it "paste-buffer-text-separator-overrides-default"
    (dolist (c (list (list "a-b-c"               (format nil "a~%b~%c") nil "-"  "-s '-': LF -> '-'")
                     (list "a, b, c"              (format nil "a~%b~%c") nil ", " "-s ', ': LF -> multi-char sep")
                     (list "abc"                  (format nil "a~%b~%c") nil ""   "-s '': strip LF")
                     (list (format nil "a~%b~%c") (format nil "a~%b~%c") t   "-"  "-r wins over -s")))
      (destructuring-bind (expected input no-replace sep desc) c
        (declare (ignore desc))
        (expect (string= expected (cl-tmux::%paste-buffer-text input no-replace sep))))))

  ;;; ── :save-buffer / :load-buffer dispatch ─────────────────────────────────────

  ;; :save-buffer opens a prompt for the file path when buffer 0 exists.
  (it "dispatch-save-buffer-opens-prompt-when-buffer-exists"
    (with-fake-session (s)
      (let ((*prompt* nil)
            (cl-tmux/buffer:*paste-buffers* (list (cons "buffer0" "save-me"))))
        (cl-tmux::dispatch-command s :save-buffer nil)
        (expect (prompt-active-p)))))

  ;; :save-buffer with empty ring opens an overlay saying '(no paste buffers to save)'.
  (it "dispatch-save-buffer-shows-error-when-no-buffer"
    (with-fake-session (s)
      (let ((*overlay* nil)
            (cl-tmux/buffer:*paste-buffers* nil))
        (cl-tmux::dispatch-command s :save-buffer nil)
        (assert-overlay-contains "no paste buffers" (overlay-lines)
                                  ":save-buffer"))))

  ;;; ── :choose-buffer dispatch ───────────────────────────────────────────────────

  ;; :choose-buffer with buffers opens a listing overlay and a prompt.
  (it "dispatch-choose-buffer-opens-prompt-when-buffers-exist"
    (with-fake-session (s)
      (let ((*prompt* nil) (*overlay* nil)
            (cl-tmux/buffer:*paste-buffers* (list (cons "buffer1" "alpha")
                                                  (cons "buffer0" "beta"))))
        (cl-tmux::dispatch-command s :choose-buffer nil)
        (assert-overlay-active ":choose-buffer must open a listing overlay")
        (expect (prompt-active-p)))))

  ;; :choose-buffer with empty ring shows '(no paste buffers)' overlay.
  (it "dispatch-choose-buffer-no-buffers-shows-overlay"
    (with-fake-session (s)
      (let ((*overlay* nil)
            (cl-tmux/buffer:*paste-buffers* nil))
        (cl-tmux::dispatch-command s :choose-buffer nil)
        (assert-overlay-contains "no paste buffers" (overlay-lines)
                                  ":choose-buffer"))))

  ;;; ── :select-window-prompt / :move-window / :swap-window dispatch ─────────────

  ;; :select-window-prompt, :move-window, and :swap-window each open a prompt (requires ≥ 2 windows).
  (it "dispatch-two-window-commands-open-prompt-table"
    (dolist (cmd '(:select-window-prompt :move-window :swap-window))
      (with-fake-session (s :nwindows 2)
        (let ((*prompt* nil))
          (cl-tmux::dispatch-command s cmd nil)
          (expect (prompt-active-p))))))

  ;; :select-window-prompt on-submit with a valid index selects that window.
  (it "dispatch-select-window-prompt-selects-by-number"
    (with-fake-session (s :nwindows 2)
      (let ((*prompt* nil))
        (cl-tmux::dispatch-command s :select-window-prompt nil)
        (expect (prompt-active-p))
        (funcall (prompt-on-submit *prompt*) "1")
        (expect (eq (second (session-windows s)) (session-active-window s))))))

  ;; :select-window-prompt numeric input selects by window id, not list position.
  (it "dispatch-select-window-prompt-selects-by-window-id-with-gap"
    (with-fake-session (s :nwindows 3)
      (let* ((win0 (first (session-windows s)))
             (win2 (third (session-windows s))))
        (setf (window-name win2) "target"
              (session-windows s) (list win0 win2))
        (session-select-window s win0)
        (let ((*prompt* nil))
          (cl-tmux::dispatch-command s :select-window-prompt nil)
          (expect (prompt-active-p))
          (funcall (prompt-on-submit *prompt*) "2")
          (expect (eq win2 (session-active-window s)))))))

  ;;; ── %copy-mode-active-p ──────────────────────────────────────────────────────

  ;; %copy-mode-active-p returns NIL for a windowless session.
  (it "copy-mode-active-p-false-for-windowless-session"
    (with-empty-session (s)
      (expect (cl-tmux::%copy-mode-active-p s) :to-be-falsy)))

  ;;; ── %signal-channel-prompt helper ────────────────────────────────────────────

  ;; %signal-channel-prompt opens a prompt with the given label.
  (it "signal-channel-prompt-opens-prompt"
    (with-loop-state
      (let ((*prompt* nil))
        (cl-tmux::%signal-channel-prompt "test-channel")
        (expect (prompt-active-p))
        (expect (string= "test-channel" (prompt-label *prompt*)))))))
