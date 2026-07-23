(in-package #:cl-tmux/test)

;;;; Detach, kill, and prefix dispatch tests.

(describe "dispatch-suite"

  ;; C-b d returns :detach and does NOT clear *running* itself (the caller decides:
  ;; standalone stops, a server merely disconnects the client).
  (it "dispatch-detach-returns-detach"
    (with-fake-session (s)
      (expect (eq :detach (cl-tmux::dispatch-command s :detach nil)))
      (expect cl-tmux::*running* :to-be-truthy)))

  ;; Killing the last window or last pane ends the session with :quit.
  (it "dispatch-kill-last-returns-quit-table"
    (with-fake-session (s :nwindows 1)
      (expect (eq :quit (cl-tmux::dispatch-command s :kill-window nil))))
    (with-fake-session (s :nwindows 1 :npanes 1)
      (expect (eq :quit (cl-tmux::dispatch-command s :kill-pane nil)))))

  ;; Killing one of two windows leaves the session running with the other.
  (it "dispatch-kill-one-of-two-windows-survives"
    (with-fake-session (s :nwindows 2)
      (expect (null (cl-tmux::dispatch-command s :kill-window nil)))
      (expect (= 1 (length (session-windows s))))))

  ;; dispatch-prefix-command looks the byte up in the binding table (d -> detach).
  (it "dispatch-prefix-routes-binding"
    (with-fake-session (s)
      (expect (eq :detach (cl-tmux::dispatch-prefix-command s (char-code #\d))))))

  ;; In copy mode, the prefix-routed 'q' exits copy mode.
  (it "prefix-q-exits-copy-mode"
    (with-fake-session (s)
      (cl-tmux::dispatch-command s :copy-mode-enter nil)
      (cl-tmux::dispatch-prefix-command s (char-code #\q))
      (expect (screen-copy-mode-p (active-screen s)) :to-be-falsy))))
