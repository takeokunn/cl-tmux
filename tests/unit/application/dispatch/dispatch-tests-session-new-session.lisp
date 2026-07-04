(in-package #:cl-tmux/test)

;;;; Dispatch session tests - new-session duplicate, grouped sessions, and new-window -S.

(in-suite dispatch-suite)

;;; -- new-session -s duplicate-name handling ----------------------------------

(test new-session-explicit-duplicate-name-refused
  "new-session -s NAME with an existing session NAME (no -A) is refused - the
   existing session is not orphaned."
  (with-fake-session (existing)
    (let ((caller (make-fake-session)))
      (setf (cl-tmux::session-name existing) "work")
      (with-registered-sessions (("work" existing))
        (let ((*overlay* nil))
          (is (null (cl-tmux::%cmd-new-session-arg caller '("-s" "work")))
              "duplicate -s name is refused (returns nil)")
          (is (eq existing (cl-tmux::server-find-session "work"))
              "the existing session is intact, not orphaned")
          (is (= 1 (length cl-tmux::*server-sessions*))
              "no second session was created"))))))

(test new-session-A-attaches-to-existing
  "new-session -A -s NAME attaches to (returns) the existing session NAME."
  (with-fake-session (existing)
    (let ((caller (make-fake-session)))
      (setf (cl-tmux::session-name existing) "work")
      (with-registered-sessions (("work" existing))
        (is (eq existing (cl-tmux::%cmd-new-session-arg caller '("-A" "-s" "work")))
            "-A returns the existing session")
        (is (= 1 (length cl-tmux::*server-sessions*))
            "no new session created")))))

(test new-session-auto-name-avoids-collision
  "An auto-generated session name that would collide bumps to the next free
   number instead of orphaning the existing session."
  (with-fake-session (s2)
    (setf (cl-tmux::session-name s2) "2")
    (with-registered-sessions (("2" s2))
      (let ((*overlay* nil))
        (let ((new (cl-tmux::%cmd-new-session-arg s2 '("-d"))))
          (is (not (null new)) "a session was created")
          (is (not (string= "2" (cl-tmux::session-name new)))
              "the new session did not reuse the colliding name 2")
          (is (eq s2 (cl-tmux::server-find-session "2"))
              "the existing session 2 is intact"))))))

(test new-session-e-sets-session-environment
  "new-session -e VAR=val stores VAR in the new session's environment overlay
   (inherited by windows created later in the session)."
  (with-fake-session (s2)
    (setf (cl-tmux::session-name s2) "7")
    (with-registered-sessions (("7" s2))
      (let ((*overlay* nil))
        (let ((new (cl-tmux::%cmd-new-session-arg
                    s2 '("-d" "-s" "envy" "-e" "CLTMUX_NS_E=bar"))))
          (is (not (null new)) "a session was created")
          (multiple-value-bind (value source)
              (cl-tmux/model:session-environment-value new "CLTMUX_NS_E")
            (declare (ignore source))
            (is (string= "bar" value)
                "new-session -e stores VAR=val in the session environment")))))))

(test new-session-P-prints-session-info
  "new-session -dP prints the new session info (default #{session_name}:) to an
   overlay, and -F overrides the format."
  (with-fake-session (s2)
    (setf (cl-tmux::session-name s2) "6")
    (with-registered-sessions (("6" s2))
      (let ((*overlay* nil))
        (cl-tmux::%cmd-new-session-arg s2 '("-d" "-P" "-s" "printy"))
        (is (and *overlay* (search "printy" *overlay*))
            "new-session -P shows the session name in an overlay"))
      (let ((*overlay* nil))
        (cl-tmux::%cmd-new-session-arg
         s2 '("-d" "-P" "-F" "ID=#{session_name}" "-s" "fmty"))
        (is (and *overlay* (search "ID=fmty" *overlay*))
            "new-session -F overrides the printed format")))))

(test new-window-S-selects-existing-named-window
  "new-window -S -n NAME selects an existing window with that name instead of
   creating a new one (tmux new-window -S)."
  (with-fake-session (s :nwindows 1)
    (let ((w (session-active-window s)))
      (setf (cl-tmux::window-name w) "ssh")
      (let ((before (length (cl-tmux::session-windows s))))
        (let ((result (cl-tmux::%cmd-new-window-arg s '("-S" "-n" "ssh"))))
          (is (eq w result)
              "new-window -S returns the existing window named ssh")
          (is (= before (length (cl-tmux::session-windows s)))
              "new-window -S must not create a second window"))))))

;;; -- new-session -t: grouped sessions ----------------------------------------

(test new-session-t-shares-target-windows
  "new-session -t TARGET creates a GROUPED session that SHARES the target's
   window list (tmux grouped sessions).  Built fork-free via make-session -
   no orphaned PTY/reader-thread, because the shared panes keep the threads
   already attached to them by the target session."
  (with-fake-session (target)
    (let ((caller (make-fake-session)))
      (setf (cl-tmux::session-name target) "base")
      (with-registered-sessions (("base" target))
        (let ((cl-tmux::*session-groups*  nil)
              (*overlay* nil))
          (let ((grouped (cl-tmux::%cmd-new-session-arg
                          caller '("-d" "-s" "clone" "-t" "base"))))
            (is (not (null grouped)) "a grouped session was created")
            (is (not (eq grouped target)) "it is a distinct session object")
            (is (eq (cl-tmux::session-windows grouped)
                    (cl-tmux::session-windows target))
                "grouped session SHARES the target's window list (same object)")
            (is (eq (cl-tmux::session-active-window grouped)
                    (cl-tmux::session-active-window target))
                "grouped session's active window mirrors the target's")
            (is (eq grouped (cl-tmux::server-find-session "clone"))
                "grouped session is registered under its own name")
            (is (and (cl-tmux::session-group grouped)
                     (eql (cl-tmux::session-group grouped)
                          (cl-tmux::session-group target)))
                "both sessions share the same group id")))))))

(test new-session-t-missing-target-refused
  "new-session -t with an unknown target is refused (returns nil) and registers
   no session - the partial group must not leak a half-built session."
  (with-fake-session (caller)
    (with-empty-registry
      (let ((cl-tmux::*session-groups*  nil)
            (*overlay* nil))
        (is (null (cl-tmux::%cmd-new-session-arg
                   caller '("-d" "-s" "clone" "-t" "ghost")))
            "missing -t target is refused (returns nil)")
        (is (null cl-tmux::*server-sessions*)
            "no session was registered")))))
