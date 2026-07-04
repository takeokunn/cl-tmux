(in-package #:cl-tmux)

;;; -- Session destruction command ----------------------------------------------

(defun %alphabetical-neighbour (name dir)
  "The surviving session whose name is alphabetically just after (DIR +1) or
   before (DIR -1) NAME (the destroyed session's name, no longer in the registry),
   wrapping around.  Returns NIL when no sessions survive.  Backs detach-on-destroy
   previous/next."
  (let ((sorted (sort (mapcar #'cdr *server-sessions*) #'string< :key #'session-name)))
    (when sorted
      (if (plusp dir)
          (or (find-if (lambda (s) (string< name (session-name s))) sorted)
              (first sorted))
          (or (find-if (lambda (s) (string< (session-name s) name)) (reverse sorted))
              (car (last sorted)))))))

(defun %detach-on-destroy-action (destroyed-name)
  "Decide the standalone client's fate after the session it was viewing (named
   DESTROYED-NAME) is destroyed, per the detach-on-destroy option
   (off / on (default) / no-detached / previous / next).  Returns :QUIT when the
   client should detach -- which in the single-client standalone model means exit --
   or NIL when it switches to a surviving session (the event loop then follows the
   new current session).  No survivors -> always :QUIT.  off/no-detached fall to the
   most-recent survivor (the loop's natural choice); previous/next touch the
   alphabetical neighbour of DESTROYED-NAME so the loop moves there."
  (if (null *server-sessions*)
      :quit
      (let ((mode (or (cl-tmux/options:get-option "detach-on-destroy") "on")))
        (cond
          ((string= mode "on") :quit)
          ((string= mode "previous")
           (%switch-to-session (%alphabetical-neighbour destroyed-name -1)) nil)
          ((string= mode "next")
           (%switch-to-session (%alphabetical-neighbour destroyed-name 1)) nil)
          (t nil)))))   ; off / no-detached -> most-recent survivor (loop auto-follows)

(defun %cmd-kill-session-arg (session args)
  "kill-session [-a] [-C] [-t name]: kill session(s), or clear their alerts.
   -a: kill all sessions EXCEPT the one named by -t (or current session).
   -C: do NOT kill; instead clear the alert flags (activity/silence) on every
       window of the target session, matching tmux's `kill-session -C`.
   -t name: the target session (default: current session)."
  (with-command-flags+pos (flags positionals args "t")
    (declare (ignore positionals))
    (let* ((clear-alerts    (%flag-present-p flags #\C))
           (kill-all-others (%flag-present-p flags #\a))
           (target-name     (%flag-value flags #\t))
           (target-sess     (or (and target-name (server-find-session target-name))
                                session)))
      (cond
        ;; -C: clear alerts instead of killing.  tmux resets every window's
        ;; activity/silence (and bell) flags in the target session; cl-tmux does
        ;; not model bell separately, so clearing activity+silence is the faithful
        ;; subset.
        (clear-alerts
         (when target-sess
           (dolist (win (session-windows target-sess))
             (setf (window-activity-flag win) nil
                   (window-silence-flag  win) nil))))
        ;; -a: kill all sessions except target-sess (the "keep" session).
        (kill-all-others
         (dolist (entry (remove-if (lambda (e) (eq (cdr e) target-sess))
                                   *server-sessions*))
           (%destroy-session (cdr entry))))
        ;; No -a/-C: kill target-sess.
        (t
         (when target-sess
           (let ((name        (session-name target-sess))
                 (was-current (eq target-sess session)))
             (%destroy-session target-sess)
             ;; Killing the session the client is viewing -> detach-on-destroy.
             (when (and was-current
                        (eq :quit (%detach-on-destroy-action name)))
               (setf *running* nil)))))))))
