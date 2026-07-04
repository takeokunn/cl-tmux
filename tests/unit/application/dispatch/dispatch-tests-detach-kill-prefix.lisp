(in-package #:cl-tmux/test)

;;;; Detach, kill, and prefix dispatch tests.

(in-suite dispatch-suite)

(test dispatch-detach-returns-detach
  "C-b d returns :detach and does NOT clear *running* itself (the caller decides:
   standalone stops, a server merely disconnects the client)."
  (with-fake-session (s)
    (is (eq :detach (cl-tmux::dispatch-command s :detach nil)))
    (is-true cl-tmux::*running* "dispatch-command must not clear *running*")))

(test dispatch-kill-last-returns-quit-table
  "Killing the last window or last pane ends the session with :quit."
  (with-fake-session (s :nwindows 1)
    (is (eq :quit (cl-tmux::dispatch-command s :kill-window nil))
        "killing last window -> :quit"))
  (with-fake-session (s :nwindows 1 :npanes 1)
    (is (eq :quit (cl-tmux::dispatch-command s :kill-pane nil))
        "killing last pane -> :quit")))

(test dispatch-kill-one-of-two-windows-survives
  "Killing one of two windows leaves the session running with the other."
  (with-fake-session (s :nwindows 2)
    (is (null (cl-tmux::dispatch-command s :kill-window nil)))
    (is (= 1 (length (session-windows s))))))

(test dispatch-prefix-routes-binding
  "dispatch-prefix-command looks the byte up in the binding table (d -> detach)."
  (with-fake-session (s)
    (is (eq :detach (cl-tmux::dispatch-prefix-command s (char-code #\d))))))

(test prefix-q-exits-copy-mode
  "In copy mode, the prefix-routed 'q' exits copy mode."
  (with-fake-session (s)
    (cl-tmux::dispatch-command s :copy-mode-enter nil)
    (cl-tmux::dispatch-prefix-command s (char-code #\q))
    (is-false (screen-copy-mode-p (active-screen s)))))
