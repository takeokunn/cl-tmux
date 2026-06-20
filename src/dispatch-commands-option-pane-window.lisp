(in-package #:cl-tmux)

;;; -- rename/select window %cmd-* handlers -----------------------------------
;;;
;;; %cmd-rename-window, %cmd-rename-session, %cmd-select-window.

(defun %cmd-rename-window (session args)
  "rename-window [-t target-window] <name...>: rename the target window (default:
   the active window) to the joined remaining ARGS.  Without -t parsing, a bare
   `rename-window -t @2 foo` would fold the flag tokens into the name and rename
   the wrong (active) window."
  (with-command-input (flags positionals args "t"
                             :allowed-flags '(#\t)
                             :message "rename-window: unsupported argument")
    (let* ((target-str (%flag-value flags #\t))
           (win        (%resolve-window-target-or-active session target-str))
           (name       (format nil "~{~A~^ ~}" positionals)))
      (when win
        (if (plusp (length name))
            (rename-window win name)
            (prompt-start "rename-window" (window-name win)
                          (lambda (new-name)
                            (rename-window win new-name))))))))

(defun %rename-session-checked (session new-name)
  "Rename SESSION to NEW-NAME, keeping *server-sessions* keyed by the new name and
   firing +hook-session-renamed+.  REFUSES (returns NIL) when NEW-NAME is empty or
   already used by a DIFFERENT session — tmux rejects a rename onto an existing name
   (`duplicate session`) rather than silently orphaning the other session; renaming
   to the session's CURRENT name is a harmless no-op that still succeeds.  The single
   chokepoint both rename paths (arg command + interactive prompt) route through.
   Returns T on success."
  (when (and new-name (plusp (length new-name)))
    (let ((existing (server-find-session new-name)))
      (unless (and existing (not (eq existing session)))   ; a different session owns it
        (server-remove-session (session-name session))
        (rename-session session new-name)
        (server-add-session session)
        (cl-tmux/hooks:run-hooks cl-tmux/hooks:+hook-session-renamed+ session)
        t))))

(defun %cmd-rename-session (session args)
  "rename-session [-t target-session] <name...>: rename the target session (default:
   the current one) to the joined remaining ARGS, updating the registry key.
   Refuses a name already used by another session (see %rename-session-checked).
   Without -t parsing, `rename-session -t old new` would fold the flag tokens into
   the name and rename the wrong (current) session."
  (with-command-input (flags positionals args "t"
                             :allowed-flags '(#\t)
                             :message "rename-session: unsupported argument")
    (let ((target-str (%flag-value flags #\t))
          (name       (format nil "~{~A~^ ~}" positionals)))
      (with-target-session (target-session target-str session
                                :on-missing :current)
        (if (plusp (length name))
            (%rename-session-checked target-session name)
            (prompt-start "rename-session" (session-name target-session)
                          (lambda (new-name)
                            (%rename-session-checked target-session new-name))))))))

(defun %select-window-select-last (session)
  (let ((prev (session-last-window session)))
    (when prev
      (%with-window-focus-transition (session)
        (session-select-window session prev)))))

(defun %select-window-cycle-next (session)
  (%cmd-cycle-window session #'next-cyclic))

(defun %select-window-cycle-prev (session)
  (%cmd-cycle-window session #'prev-cyclic))

(defun %select-window-select-target (session target toggle-p)
  (when target
    (%with-window-focus-transition (session)
      (let ((win (%resolve-window-target session target)))
        (when win
          ;; -T toggle: already on the target → jump to last window instead.
          (if (and toggle-p
                   (eq win (session-active-window session))
                   (session-last-window session))
              (session-select-window session (session-last-window session))
              (session-select-window session win)))))))

(defun %cmd-select-window (session args)
  "select-window [-t target] [-l] [-n] [-p] [-T]: select a window.
   -t target: window-id, name, or special shorthand (:! last, :+ next, :- prev).
   -l: select the last (previously active) window (same as C-b l).
   -n: select the next window.
   -p: select the previous window.
   -T: toggle — when the target is ALREADY the current window, behave like
       last-window instead (the `bind Tab select-window -T` two-window toggle).
   Delivers ?1004 focus events on the switch."
  (with-command-input (flags positionals args "t"
                             :allowed-flags '(#\l #\n #\p #\T #\t)
                             :max-positionals 0
                             :message "select-window: unsupported argument")
    (cond
      ((%flag-present-p flags #\l)
       (%select-window-select-last session))
      ((%flag-present-p flags #\n)
       (%select-window-cycle-next session))
      ((%flag-present-p flags #\p)
       (%select-window-cycle-prev session))
      (t
       (%select-window-select-target session
                                     (%flag-value flags #\t)
                                     (%flag-present-p flags #\T))))
    ;; after-select-window: tmux's per-command hook (run-hooks now fires both the
    ;; add-hook and the .tmux.conf set-hook registries).
    (cl-tmux/hooks:run-hooks cl-tmux/hooks:+hook-after-select-window+ session)))
