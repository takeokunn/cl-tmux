(in-package #:cl-tmux)

;;; -- Pane process and pipe commands ----------------------------------------
;;;

(defun %cmd-respawn-pane-arg (session args)
  "respawn-pane [-k] [-t target-pane]: restart
   the target pane's process (default: the active pane).
   -k: kill the existing process first.  WITHOUT -k, respawning a pane whose process
   is still running is an error (tmux behaviour) — use -k to force it.
   This is the scriptable form; the interactive :respawn-pane binding is unchanged."
  (with-command-input (flags positionals args "cet"
                             :allowed-flags '(#\k #\t #\c #\e)
                             :message "respawn-pane: unsupported argument")
    (let* ((target-str (%flag-value flags #\t))
           (kill-p (%flag-present-p flags #\k))
           (raw-dir (%flag-value flags #\c))
           (start-dir (%expand-start-dir session raw-dir))
           (extra-env (%collect-env-flags flags))
           (default-command (format nil "~{~A~^ ~}" positionals)))
      (with-target-context (target-session win pane session target-str)
        (declare (ignore target-session))
        (when (and win pane)
          (if (and (not kill-p) (cl-tmux/model:pane-live-p pane))
              ;; tmux: respawn-pane without -k on a still-running pane is an error.
              (show-overlay "respawn-pane: pane is active (use -k to force respawn)")
              (let ((new-pane (respawn-pane session pane
                                            :start-dir start-dir
                                            :default-command (and (plusp (length default-command))
                                                                  default-command)
                                            :extra-env extra-env)))
                (when new-pane
                  (start-reader-thread new-pane)
                  (setf *dirty* t)
                  t))))))))

(defun %cmd-respawn-window-arg (session args)
  "respawn-window [-k] [-t target-window]:
   restart every pane's process in the target window (default: the active window).
   -k: kill the existing processes first.  WITHOUT -k, respawning when ANY pane is
   still running is an error (tmux behaviour) — use -k to force it.  Scriptable form; the
   interactive :respawn-window binding is unchanged."
  (with-command-input (flags positionals args "cet"
                             :allowed-flags '(#\k #\t #\c #\e)
                             :message "respawn-window: unsupported argument")
    (let* ((target-str (%flag-value flags #\t))
           (win nil)
           (kill-p (%flag-present-p flags #\k))
           (raw-dir (%flag-value flags #\c))
           (start-dir (%expand-start-dir session raw-dir))
           (extra-env (%collect-env-flags flags))
           (default-command (let ((command (format nil "~{~A~^ ~}" positionals)))
                              (and (plusp (length command)) command))))
      (with-target-context (target-session resolved-win target-pane session target-str)
        (declare (ignore target-session target-pane))
        (setf win resolved-win))
      (when win
        (if (and (not kill-p) (%window-has-live-panes-p win))
            ;; tmux: respawn-window without -k while panes are running is an error.
            (show-overlay "respawn-window: window has active panes (use -k to force)")
            (progn
              (dolist (pane (cl-tmux/model:window-panes win))
                (let ((new-pane (respawn-pane session pane
                                              :start-dir start-dir
                                              :default-command default-command
                                              :extra-env extra-env)))
                  (when new-pane (start-reader-thread new-pane))))
              (setf *dirty* t)
              t))))))

(defun %resolve-pipe-pane-target (win resolved-pane target-str)
  "Resolve the pipe-pane target pane inside WIN, or fall back to RESOLVED-PANE
   when TARGET-STR is absent."
  (if target-str
      (%resolve-pane-in-window win target-str)
      resolved-pane))

(defun %cmd-pipe-pane-arg (session args)
  "pipe-pane [-o] [-I] [-O] [-t target-pane] [command]: open or close a pipe
   for the target pane (default: the active pane).
   -o: only open a pipe if none is currently open (no-op when one already is).
   -I: route command stdout back into the pane.
   -O: route pane output into the command stdin.
   Without -I or -O, the default is -O.
   -t target: the pane to pipe (pane-id in the active window; bare ids and %N
   are accepted).
   Without a command: close any open pipe on the target pane."
  (with-command-input (flags positionals args "t"
                             :allowed-flags '(#\I #\O #\o #\t)
                             :message "pipe-pane: unsupported argument")
    (let* ((only-open (%flag-present-p flags #\o))
           (pipe-in (%flag-present-p flags #\I))
           (pipe-out (%flag-present-p flags #\O))
           (command   (format nil "~{~A~^ ~}" positionals))
           (target-str (%flag-value flags #\t)))
      (with-target-context (target-session win resolved-pane session target-str)
        (declare (ignore target-session))
        (let* ((pane-output-to-command-p (or pipe-out (and (not pipe-in) (not pipe-out))))
               (command-output-to-pane-p pipe-in)
               (pane (%resolve-pipe-pane-target win resolved-pane target-str)))
          (when pane
            (cond
              ;; No command: close existing pipe.
              ((zerop (length command))
               (when (pane-pipe-active-p pane)
                 (pipe-pane-close pane)))
              ;; -o: skip if already piped.
              ((and only-open (pane-pipe-active-p pane)) nil)
              ;; Open the pipe.
              (t (pipe-pane-open pane command
                                 :pane-output-to-command-p pane-output-to-command-p
                                 :command-output-to-pane-p command-output-to-pane-p)))))))))

