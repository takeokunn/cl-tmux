(in-package #:cl-tmux/test)

;;;; Dispatch tests — part E (from commands-b): switch-client-next/prev,
;;;; last-session, new-session, kill-session, find-window, mark-pane/clear-mark,
;;;; next-layout, choose-client, display-info, bind/unbind-key, list/show/delete-buffer,
;;;; choose-buffer, select-window-prompt, move-window, swap-window, wait-for,
;;;; copy-mode-active-p, signal-channel-prompt.

(in-suite dispatch-suite)

;;; ── :switch-client-next / :switch-client-prev dispatch ───────────────────────

(test dispatch-switch-client-next-moves-to-next-session
  ":switch-client-next touches the next session in the registry."
  (with-fake-session (s1 :nwindows 1)
    (let* ((s2  (make-fake-session :nwindows 1))
           (reg (list (cons (session-name s1) s1)
                      (cons (session-name s2) s2))))
      (let ((cl-tmux::*server-sessions* reg))
        (cl-tmux::dispatch-command s1 :switch-client-next nil)
        (is-true cl-tmux::*dirty*
                 ":switch-client-next must mark *dirty*")))))

(test dispatch-switch-client-prev-does-not-error
  ":switch-client-prev dispatches without error."
  (with-fake-session (s)
    (finishes (cl-tmux::dispatch-command s :switch-client-prev nil)
              ":switch-client-prev must not signal an error")))

;;; ── :last-session dispatch ────────────────────────────────────────────────────

(test dispatch-last-session-does-not-error
  ":last-session dispatches without error when only one session exists."
  (with-fake-session (s)
    (let ((cl-tmux::*server-sessions* (list (cons (session-name s) s))))
      (finishes (cl-tmux::dispatch-command s :last-session nil)
                ":last-session must not signal an error"))))

;;; ── :new-session dispatch ─────────────────────────────────────────────────────

(test dispatch-new-session-does-not-error
  ":new-session dispatches without error."
  (with-fake-session (s)
    (let ((cl-tmux::*server-sessions* nil))
      (finishes (cl-tmux::dispatch-command s :new-session nil)
                ":new-session must not signal an error"))))

;;; ── :kill-session dispatch ────────────────────────────────────────────────────

(test dispatch-kill-session-with-no-other-sessions-quits
  ":kill-session with no remaining sessions returns :quit."
  (with-fake-session (s)
    (let ((name (session-name s)))
      (let ((cl-tmux::*server-sessions* (list (cons name s))))
        (is (eq :quit (cl-tmux::dispatch-command s :kill-session nil))
            ":kill-session with empty registry must return :quit")))))

;;; ── :find-window dispatch ─────────────────────────────────────────────────────

(test dispatch-simple-commands-open-prompt-table
  ":find-window, :bind-key, :unbind-key, :load-buffer, and :wait-for each open a prompt."
  (dolist (cmd '(:find-window :bind-key :unbind-key :load-buffer :wait-for))
    (with-fake-session (s)
      (let ((*prompt* nil))
        (cl-tmux::dispatch-command s cmd nil)
        (is (prompt-active-p) "~A must open a prompt" cmd)))))

;;; ── :mark-pane / :clear-mark dispatch ────────────────────────────────────────

(test dispatch-mark-pane-marks-pane-and-sets-server-pointer
  ":mark-pane sets pane-marked and updates *server-marked-pane* to the active pane."
  (with-fake-session (s)
    (let ((ap (session-active-pane s)))
      (setf (pane-marked ap) nil)
      (cl-tmux::dispatch-command s :mark-pane nil)
      (is-true (pane-marked ap) ":mark-pane must set pane-marked to T")
      (is (eq ap cl-tmux::*server-marked-pane*)
          "*server-marked-pane* must point to the newly marked pane"))))

(test dispatch-mark-pane-toggles-off
  ":mark-pane on an already-marked pane clears the mark (toggle)."
  (with-fake-session (s)
    (let ((ap (session-active-pane s)))
      (cl-tmux::dispatch-command s :mark-pane nil)
      (is-true (pane-marked ap) ":mark-pane must set the mark first")
      (cl-tmux::dispatch-command s :mark-pane nil)
      (is-false (pane-marked ap) ":mark-pane on marked pane must clear the mark"))))

(test dispatch-clear-mark-clears-server-marked-pane
  ":clear-mark clears the server-wide marked pane."
  (with-fake-session (s :nwindows 1 :npanes 1)
    (let ((ap (session-active-pane s)))
      (cl-tmux::dispatch-command s :mark-pane nil)
      (is-true (pane-marked ap) ":mark-pane must mark the active pane")
      (cl-tmux::dispatch-command s :clear-mark nil)
      (is-false (pane-marked ap)
                ":clear-mark must clear the server-wide marked pane"))))

(test dispatch-mark-pane-cross-window-clears-previous
  ":mark-pane in a second window clears the mark from a pane in the first window."
  (with-fake-session (s :nwindows 2)
    (let* ((win1 (first  (session-windows s)))
           (win2 (second (session-windows s)))
           (p1   (window-active-pane win1))
           (p2   (window-active-pane win2)))
      (session-select-window s win1)
      (cl-tmux::dispatch-command s :mark-pane nil)
      (is (pane-marked p1) "p1 must be marked in window 1")
      (session-select-window s win2)
      (cl-tmux::dispatch-command s :mark-pane nil)
      (is-false (pane-marked p1)
                "p1 in window 1 must be unmarked when window 2 pane is marked")
      (is (pane-marked p2) "p2 in window 2 must be marked")
      (is (eq p2 cl-tmux::*server-marked-pane*)
          "*server-marked-pane* must point to p2 after cross-window mark"))))

;;; ── :next-layout dispatch ─────────────────────────────────────────────────────

(test dispatch-next-layout-cycles-layout
  ":next-layout applies the next layout from the cycle table."
  (with-fake-session (s :nwindows 1 :npanes 1)
    (finishes (cl-tmux::dispatch-command s :next-layout nil)
              ":next-layout must not signal an error")))

;;; ── :select-layout-tiled / :select-layout-spread dispatch ────────────────────

(test dispatch-select-layout-tiled-and-spread-do-not-error
  ":select-layout-tiled and :select-layout-spread dispatch without error."
  (with-fake-session (s :nwindows 1 :npanes 1)
    (dolist (cmd '(:select-layout-tiled :select-layout-spread))
      (finishes (cl-tmux::dispatch-command s cmd nil)
                "~A must not signal an error" cmd))))

;;; ── :choose-client dispatch ───────────────────────────────────────────────────

(test dispatch-choose-client-shows-overlay
  ":choose-client opens an overlay with client information."
  (with-fake-session (s)
    (let ((*overlay* nil))
      (cl-tmux::dispatch-command s :choose-client nil)
      (is (overlay-active-p) ":choose-client must open an overlay")
      (let ((text (format nil "~{~A~%~}" (overlay-lines))))
        (is (search "Clients" text) "overlay must contain 'Clients'")
        (is (search (session-name s) text)
            "overlay must contain the session name")))))

;;; ── :display-info dispatch ────────────────────────────────────────────────────

(test dispatch-display-info-shows-overlay
  ":display-info opens an overlay with session/window/pane details."
  (with-fake-session (s)
    (let ((*overlay* nil))
      (cl-tmux::dispatch-command s :display-info nil)
      (is (overlay-active-p) ":display-info must open an overlay")
      (let ((text (format nil "~{~A~%~}" (overlay-lines))))
        (is (search "Session" text) "overlay must contain 'Session'")
        (is (search "Pane" text) "overlay must contain 'Pane'")))))

;;; ── :list-buffers / :show-buffer / :delete-buffer dispatch ───────────────────

(test dispatch-list-buffers-no-buffers-shows-overlay
  ":list-buffers with empty buffer ring opens an overlay saying '(no paste buffers)'."
  (with-fake-session (s)
    (let ((*overlay* nil)
          (cl-tmux/buffer:*paste-buffers* nil))
      (cl-tmux::dispatch-command s :list-buffers nil)
      (is (overlay-active-p) ":list-buffers must open an overlay")
      (let ((text (format nil "~{~A~%~}" (overlay-lines))))
        (is (search "no paste buffers" text)
            "overlay must say 'no paste buffers' when ring is empty")))))

(test dispatch-list-buffers-populated-shows-entries
  ":list-buffers with buffers lists them by name with their content preview."
  (with-fake-session (s)
    (let ((*overlay* nil)
          (cl-tmux/buffer:*paste-buffers* (list (cons "buffer1" "hello")
                                                (cons "buffer0" "world"))))
      (cl-tmux::dispatch-command s :list-buffers nil)
      (is (overlay-active-p) ":list-buffers must open an overlay")
      (let ((text (format nil "~{~A~%~}" (overlay-lines))))
        (is (search "hello" text) "overlay must list the first buffer's content")
        (is (search "world" text) "overlay must list the second buffer's content")
        (is (search "buffer1:" text) "overlay must show buffer names")))))

(test dispatch-show-buffer-shows-content
  ":show-buffer opens an overlay with buffer 0's content."
  (with-fake-session (s)
    (let ((*overlay* nil)
          (cl-tmux/buffer:*paste-buffers* (list (cons "buffer0" "test-content"))))
      (cl-tmux::dispatch-command s :show-buffer nil)
      (is (overlay-active-p) ":show-buffer must open an overlay")
      (let ((text (format nil "~{~A~%~}" (overlay-lines))))
        (is (search "test-content" text)
            "overlay must contain buffer 0 content")))))

(test dispatch-delete-buffer-removes-first-entry
  ":delete-buffer removes the first paste buffer."
  (with-fake-session (s)
    (let ((cl-tmux/buffer:*paste-buffers* (list (cons "buffer0" "todelete"))))
      (cl-tmux::dispatch-command s :delete-buffer nil)
      (is (null cl-tmux/buffer:*paste-buffers*)
          ":delete-buffer must remove buffer 0 from the ring"))))

(test paste-buffer-text-translates-lf-to-cr-by-default
  "%paste-buffer-text replaces LF with CR by default so a multi-line paste
   submits each line; -r (no-replace) keeps the raw bytes."
  (is (string= (format nil "a~Cb~Cc" #\Return #\Return)
               (cl-tmux::%paste-buffer-text (format nil "a~%b~%c") nil))
      "default paste must translate LF → CR")
  (is (string= (format nil "a~%b~%c")
               (cl-tmux::%paste-buffer-text (format nil "a~%b~%c") t))
      "-r must keep LF unchanged")
  (is (string= "abc" (cl-tmux::%paste-buffer-text "abc" nil))
      "text without newlines is unchanged")
  (is (null (cl-tmux::%paste-buffer-text nil nil))
      "NIL buffer contents → NIL"))

(test paste-buffer-text-separator-overrides-default
  "%paste-buffer-text -s SEPARATOR replaces LF with SEPARATOR instead of CR; -r
   still wins (raw), and SEP may be empty or multi-character."
  (is (string= "a-b-c"
               (cl-tmux::%paste-buffer-text (format nil "a~%b~%c") nil "-"))
      "-s '-' must replace each LF with '-'")
  (is (string= "a, b, c"
               (cl-tmux::%paste-buffer-text (format nil "a~%b~%c") nil ", "))
      "-s ', ' must replace each LF with a multi-character separator")
  (is (string= "abc"
               (cl-tmux::%paste-buffer-text (format nil "a~%b~%c") nil ""))
      "-s '' must strip the line breaks entirely")
  (is (string= (format nil "a~%b~%c")
               (cl-tmux::%paste-buffer-text (format nil "a~%b~%c") t "-"))
      "-r must take precedence over -s and keep the raw bytes"))

;;; ── :save-buffer / :load-buffer dispatch ─────────────────────────────────────

(test dispatch-save-buffer-opens-prompt-when-buffer-exists
  ":save-buffer opens a prompt for the file path when buffer 0 exists."
  (with-fake-session (s)
    (let ((*prompt* nil)
          (cl-tmux/buffer:*paste-buffers* (list (cons "buffer0" "save-me"))))
      (cl-tmux::dispatch-command s :save-buffer nil)
      (is (prompt-active-p) ":save-buffer must open a prompt when buffer exists"))))

(test dispatch-save-buffer-shows-error-when-no-buffer
  ":save-buffer with empty ring opens an overlay saying '(no paste buffers to save)'."
  (with-fake-session (s)
    (let ((*overlay* nil)
          (cl-tmux/buffer:*paste-buffers* nil))
      (cl-tmux::dispatch-command s :save-buffer nil)
      (is (overlay-active-p) ":save-buffer must open an overlay when no buffers")
      (let ((text (format nil "~{~A~%~}" (overlay-lines))))
        (is (search "no paste buffers" text)
            "overlay must mention 'no paste buffers'")))))

;;; ── :choose-buffer dispatch ───────────────────────────────────────────────────

(test dispatch-choose-buffer-opens-prompt-when-buffers-exist
  ":choose-buffer with buffers opens a listing overlay and a prompt."
  (with-fake-session (s)
    (let ((*prompt* nil) (*overlay* nil)
          (cl-tmux/buffer:*paste-buffers* (list (cons "buffer1" "alpha")
                                                (cons "buffer0" "beta"))))
      (cl-tmux::dispatch-command s :choose-buffer nil)
      (is (overlay-active-p) ":choose-buffer must open a listing overlay")
      (is (prompt-active-p) ":choose-buffer must open a prompt for the index"))))

(test dispatch-choose-buffer-no-buffers-shows-overlay
  ":choose-buffer with empty ring shows '(no paste buffers)' overlay."
  (with-fake-session (s)
    (let ((*overlay* nil)
          (cl-tmux/buffer:*paste-buffers* nil))
      (cl-tmux::dispatch-command s :choose-buffer nil)
      (is (overlay-active-p) ":choose-buffer must open an overlay")
      (let ((text (format nil "~{~A~%~}" (overlay-lines))))
        (is (search "no paste buffers" text)
            "overlay must say 'no paste buffers'")))))

;;; ── :select-window-prompt / :move-window / :swap-window dispatch ─────────────

(test dispatch-two-window-commands-open-prompt-table
  ":select-window-prompt, :move-window, and :swap-window each open a prompt (requires ≥ 2 windows)."
  (dolist (cmd '(:select-window-prompt :move-window :swap-window))
    (with-fake-session (s :nwindows 2)
      (let ((*prompt* nil))
        (cl-tmux::dispatch-command s cmd nil)
        (is (prompt-active-p) "~A must open a prompt" cmd)))))

(test dispatch-select-window-prompt-selects-by-number
  ":select-window-prompt on-submit with a valid index selects that window."
  (with-fake-session (s :nwindows 2)
    (let ((*prompt* nil))
      (cl-tmux::dispatch-command s :select-window-prompt nil)
      (is (prompt-active-p) "prompt must be open")
      (funcall (prompt-on-submit *prompt*) "1")
      (is (eq (second (session-windows s)) (session-active-window s))
          "on-submit with \"1\" must select the second window"))))

;;; ── %copy-mode-active-p ──────────────────────────────────────────────────────

(test copy-mode-active-p-false-for-windowless-session
  "%copy-mode-active-p returns NIL for a windowless session."
  (with-empty-session (s)
    (is-false (cl-tmux::%copy-mode-active-p s)
              "%copy-mode-active-p must return NIL for a windowless session")))

;;; ── %signal-channel-prompt helper ────────────────────────────────────────────

(test signal-channel-prompt-opens-prompt
  "%signal-channel-prompt opens a prompt with the given label."
  (with-loop-state
    (let ((*prompt* nil))
      (cl-tmux::%signal-channel-prompt "test-channel")
      (is (prompt-active-p) "%signal-channel-prompt must open a prompt")
      (is (string= "test-channel" (prompt-label *prompt*))
          "%signal-channel-prompt label must match the argument"))))

