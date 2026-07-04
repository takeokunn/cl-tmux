(in-package #:cl-tmux/test)

;;;; Event-loop isolation fixtures.

(defmacro with-global-running (value &body body)
  "Run BODY with the GLOBAL value of cl-tmux::*running* set to VALUE, restoring
   the prior global value afterward.

   Why not (let ((cl-tmux::*running* value)) ...)?  A LET establishes a
   thread-LOCAL dynamic binding visible only in the current thread.  Reader and
   status-timer threads spawned inside BODY do NOT inherit the parent's dynamic
   bindings; they observe the GLOBAL value of *running*.  A LET binding is
   therefore invisible to them: they never see the stop signal, loop forever,
   outlive join-thread's timeout, and leak into later suites as background work.
   Mutating the global with SETF is what those threads actually observe, so any
   test that spawns a reader/timer thread must drive *running* through this
   macro rather than a LET."
  (let ((saved (gensym "SAVED-RUNNING")))
    `(let ((,saved cl-tmux::*running*))
       (setf cl-tmux::*running* ,value)
       (unwind-protect (progn ,@body)
         (setf cl-tmux::*running* ,saved)))))

(defun stop-cl-tmux-threads ()
  "Stop and join every PTY-reader / status-timer / background-shell thread that
   a test may have spawned, so none leaks into a later test.

   Dispatching :split-*, :new-window, :new-session or :respawn-pane spawns a real
   pane and calls START-READER-THREAD; that reader loops while the GLOBAL
   *running* is true.  We clear the global so the loops exit, join the named
   threads (bounded), then restore *running* to T for the next test.  Threads
   are matched by name, so no global registry is required.

   IMPORTANT: after signaling *running*=NIL we SLEEP before restoring it.
   Reader/timer loops only observe *running* between poll cycles (readers poll
   every +pty-poll-timeout-us+ ~= 50 ms).  Without the pause, *running* could
   flip back to T while a reader is still mid-poll and it would never stop.
   Sleeping ~3 poll cycles gives every reader a chance to observe the stop and
   exit before the bounded join."
  (let ((targets
          (remove-if-not
           (lambda (th)
             (let ((name (bordeaux-threads:thread-name th)))
               (and (stringp name)
                    (or (search "pty-reader" name)
                        (search "cl-tmux-status-timer" name)
                        (search "shell-bg" name)))))
           (bordeaux-threads:all-threads))))
    (when targets
      (setf cl-tmux::*running* nil)
      (sleep 0.15)
      (dolist (th targets)
        (ignore-errors (cl-tmux::%join-thread-with-timeout th 2)))
      (setf cl-tmux::*running* t))))

(defmacro with-loop-state (&body body)
  "Run BODY with the event-loop specials isolated, then stop any reader/timer
   threads BODY spawned (e.g. by dispatching a :split that creates a real pane).

   *running* is driven through its GLOBAL value (via WITH-GLOBAL-RUNNING) rather
   than a LET, because reader threads spawned during BODY read the global; a LET
   binding would be invisible to them and they would leak into later tests.
   STOP-CL-TMUX-THREADS joins them before returning.

   Also isolates prompt/overlay/menu/popup state so that UI state created by
   one test does not leak into subsequent event-loop tests."
  `(let ((cl-tmux::*dirty* nil)
         (cl-tmux::*last-mouse-click* nil)
         (cl-tmux::*key-table* nil)
         ;; Tests feed key bytes microseconds apart, a rate no real terminal
         ;; produces for typed keys.  Reset key history to avoid triggering the
         ;; assume-paste-time heuristic on every second key.
         (cl-tmux::*last-ground-key-time* nil)
         (cl-tmux::*server-marked-pane* nil)
         (cl-tmux::*client-read-only* nil)
         (cl-tmux/prompt:*prompt* nil)
         (cl-tmux/prompt:*overlay* nil)
         (cl-tmux/prompt:*overlay-scroll-offset* 0)
         (cl-tmux/prompt:*overlay-shown-at* 0)
         (cl-tmux/prompt:*display-panes-active* nil)
         (cl-tmux/prompt:*active-menu* nil)
         (cl-tmux/prompt:*active-popup* nil))
     (with-global-running t
       (unwind-protect (progn ,@body)
         (stop-cl-tmux-threads)))))
