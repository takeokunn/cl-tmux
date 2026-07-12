(in-package #:cl-tmux)

;;; -- Session lifecycle services ------------------------------------------------

(defun %current-session (&optional fallback)
  "The session the standalone client is currently viewing: the most-recently-
   touched (highest session-last-active) session in *server-sessions*, or FALLBACK
   when the registry is empty.  This is how session-switch commands (switch-client,
   choose-tree, last-session) change the displayed session -- they session-touch
   their target, and the event loop re-resolves the current session through here on
   every iteration, so the display follows the switch.  Delegates to the registry's
   server-current-session (highest last-active), adding the FALLBACK for the empty
   registry -- ties (same-second stamps) resolve there; deliberate switches are
   seconds apart in practice."
  (or (server-current-session) fallback))

(defun %switch-to-session (target)
  "Make TARGET the client's active session by bumping its last-active stamp (the
   renderer follows the most-recently-touched session via %current-session) and
   marking the screen dirty.  No-op when TARGET is NIL.  Returns TARGET when a switch
   happened, else NIL -- the single chokepoint every session move routes through.
   When destroy-unattached is on, the session the client was viewing becomes
   unattached on the switch and is destroyed (tmux's destroy-unattached)."
  (when target
    (let ((old (server-current-session)))   ; the session being left, if any
      (session-touch target)
      (setf *dirty* t)
      (when (and old (not (eq old target))
                 (cl-tmux/options:get-option "destroy-unattached"))
        (%destroy-session old))
      target)))

(defun %destroy-session (session)
  "Tear down SESSION: close its panes' PTYs, remove it from the server registry,
   and fire the session-closed hook.  The single chokepoint for session
   DESTRUCTION (every kill-session path routes through here) -- deliberately
   distinct from rename-session, which also removes+re-adds the registry entry but
   must NOT fire session-closed.  Returns the session name.

   PTY teardown is REFERENCE-COUNTED: grouped/linked sessions share the SAME window
   structs (session-registry %link-session-to-group aliases the window list), so a
   window still referenced by another live session must keep its PTYs open or the
   survivors lose the panes they display.  SESSION is still in *server-sessions*
   here, so an UNSHARED window has %window-session-count = 1 (close it) and a SHARED
   window has >= 2 (leave it) -- identical to the old unconditional close for the
   common single-session case."
  (when session
    (let ((name (session-name session)))
      (dolist (win (session-windows session))
        (when (<= (%window-session-count win) 1)
          (dolist (pane (window-panes win))
            (close-pane-pty pane)))
      (server-remove-session name)
      (cl-tmux/hooks:run-hooks cl-tmux/hooks:+hook-session-closed+ session)
      name))))
