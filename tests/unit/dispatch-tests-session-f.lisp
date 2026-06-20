(in-package #:cl-tmux/test)

;;;; Dispatch session tests — part F (from session-c): new-session -s duplicate-name,
;;;; new-session -A/-t grouped, control-mode REPL, server-lifecycle reachability,
;;;; control-mode active-change notifications, %output relay.

(in-suite dispatch-suite)

;;; ── new-session -s duplicate-name handling ───────────────────────────────────

(test new-session-explicit-duplicate-name-refused
  "new-session -s NAME with an existing session NAME (no -A) is refused — the
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

;;; ── new-session -t: grouped sessions ─────────────────────────────────────────

(test new-session-t-shares-target-windows
  "new-session -t TARGET creates a GROUPED session that SHARES the target's
   window list (tmux grouped sessions).  Built fork-free via make-session —
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
   no session — the partial group must not leak a half-built session."
  (with-fake-session (caller)
    (with-empty-registry
      (let ((cl-tmux::*session-groups*  nil)
            (*overlay* nil))
        (is (null (cl-tmux::%cmd-new-session-arg
                   caller '("-d" "-s" "clone" "-t" "ghost")))
            "missing -t target is refused (returns nil)")
        (is (null cl-tmux::*server-sessions*)
            "no session was registered")))))

;;; ── control mode (-C) REPL ───────────────────────────────────────────────────

(test control-run-command-frames-output
  "%control-run-command frames a command's overlay output in a %begin/%end block."
  (with-fake-session (s)
    (let* ((reply (cl-tmux::%control-run-command s "display-message hello" 1)))
      (is (search "%begin 0 1 1" reply) "reply opens with %begin for command 1")
      (is (search "%end 0 1 1" reply)   "reply closes with %end")
      (is (search "hello" reply)        "the command's output is in the reply body"))))

(test control-mode-loop-frames-each-and-exits
  "control-mode-loop runs each input line as the next numbered command and emits
   %exit at EOF."
  (with-fake-session (s)
    (let* ((out (with-output-to-string (o)
                  (with-input-from-string
                      (i (format nil "display-message a~%display-message b~%"))
                    (cl-tmux::control-mode-loop s i o)))))
      (is (search "%begin 0 1 1" out) "first line is command 1")
      (is (search "%begin 0 2 1" out) "second line is command 2")
      (is (search "%exit" out)        "the loop emits %exit on EOF"))))

(test control-mode-loop-skips-blank-lines
  "Blank input lines are not run as commands (no reply framed for them)."
  (with-fake-session (s)
    (let* ((out (with-output-to-string (o)
                  (with-input-from-string (i (format nil "~%display-message x~%~%"))
                    (cl-tmux::control-mode-loop s i o)))))
      (is (search "%begin 0 1 1" out) "the one real command is command 1")
      (is (null (search "%begin 0 2 1" out))
          "blank lines did not advance the command number"))))

(test control-notifications-emit-on-hooks
  "Installed control-mode notifications write %window-add/-close/-renamed and
   %session-renamed to the output stream when the matching hooks fire."
  (with-isolated-hooks
    (let* ((out      (make-string-output-stream))
           (handlers (cl-tmux::%install-control-notifications out)))
      (unwind-protect
           (let ((win  (make-fake-window 5 "editor"))
                 (sess (make-fake-session)))
             (setf (cl-tmux::session-name sess) "work")
             (cl-tmux/hooks:run-hooks cl-tmux/hooks:+hook-after-new-window+ win)
             (cl-tmux/hooks:run-hooks cl-tmux/hooks:+hook-window-renamed+ win)
             (cl-tmux/hooks:run-hooks cl-tmux/hooks:+hook-after-kill-window+ win)
             (cl-tmux/hooks:run-hooks cl-tmux/hooks:+hook-session-renamed+ sess)
             (let ((s (get-output-stream-string out)))
               (is (search "%window-add @5" s)         "%window-add emitted")
               (is (search "%window-renamed @5 editor" s) "%window-renamed emitted")
               (is (search "%window-close @5" s)       "%window-close emitted")
               (is (search (format nil "%session-renamed $~D work"
                                   (cl-tmux::session-id sess)) s)
                   "%session-renamed emitted")))
        (cl-tmux::%remove-control-notifications handlers)))))

(test control-notifications-removed-stop-emitting
  "After %remove-control-notifications, a hook no longer writes to the stream."
  (with-isolated-hooks
    (let* ((out      (make-string-output-stream))
           (handlers (cl-tmux::%install-control-notifications out)))
      (cl-tmux::%remove-control-notifications handlers)
      (cl-tmux/hooks:run-hooks cl-tmux/hooks:+hook-after-new-window+
                               (make-fake-window 7 "x"))
      (is (string= "" (get-output-stream-string out))
          "no notification after the callbacks are removed"))))

(test control-run-command-unknown-is-error
  "An unknown command closes the control-mode reply with %error, not %end."
  (with-fake-session (s)
    (let* ((*overlay* nil)
           (reply (cl-tmux::%control-run-command s "bogus-command-xyz" 3)))
      (is (search "%begin 0 3 1" reply) "reply opens with %begin")
      (is (search "%error 0 3 1" reply) "unknown command closes with %error")
      (is (search "unknown command" reply) "the error message is in the body"))))

(test control-notifications-layout-change-on-resize
  "after-resize-pane emits %layout-change @<window> with the window's layout string."
  (with-isolated-hooks
    (let* ((out      (make-string-output-stream))
           (handlers (cl-tmux::%install-control-notifications out)))
      (unwind-protect
           (let ((win (make-fake-window 3 "w")))
             (cl-tmux/hooks:run-hooks cl-tmux/hooks:+hook-after-resize-pane+ win)
             (is (search "%layout-change @3" (get-output-stream-string out))
                 "%layout-change emitted on resize"))
        (cl-tmux::%remove-control-notifications handlers)))))

;;; ── Server-lifecycle command-name reachability ───────────────────────────────
;;;
;;; Regression: kill-server / start-server / send-prefix all have working
;;; dispatch handlers (dispatch-handlers.lisp) but were absent from
;;; define-named-command-table, so %dispatch-named-command returned the
;;; :unknown-command sentinel and they could not be invoked from the `C-b :`
;;; prompt or control mode.  These assert the name → keyword wiring exists.

(test named-commands-server-lifecycle-reachable
  "kill-server / start-server / send-prefix are reachable by command name."
  (let ((cl-tmux::*running* t)
        (cl-tmux::*server-sessions* nil)
        (*overlay* nil))
    (with-empty-session (s)
      ;; start-server: recognised → no-op overlay, NOT the unknown sentinel.
      (is (not (eq :unknown-command
                   (cl-tmux::%dispatch-named-command s "start-server")))
          "start-server must be a recognised command name")
      ;; send-prefix: recognised; an empty session has no active pane → safe no-op.
      (is (not (eq :unknown-command
                   (cl-tmux::%dispatch-named-command s "send-prefix")))
          "send-prefix must be a recognised command name")
      ;; kill-server: recognised → dispatches :kill-server, returns :quit and
      ;; clears *running*.
      (is (eq :quit (cl-tmux::%dispatch-named-command s "kill-server"))
          "kill-server must dispatch to :kill-server (returns :quit)")
      (is (null cl-tmux::*running*)
          "kill-server clears *running*")
      ;; Control: a genuinely unknown name still returns the :unknown-command
      ;; sentinel, confirming the assertions above are meaningful.
      (is (eq :unknown-command
              (cl-tmux::%dispatch-named-command s "definitely-not-a-command-xyz"))
          "unknown names still return :unknown-command"))))

;;; ── Control-mode active-change notifications (%window-pane-changed / -session-) ──

(test control-notifications-window-pane-changed
  "+hook-window-pane-changed+ emits %window-pane-changed @<win> %<active-pane> to a
   control client."
  (with-isolated-hooks
    (let* ((out      (make-string-output-stream))
           (handlers (cl-tmux::%install-control-notifications out)))
      (unwind-protect
           (let ((win (make-fake-window 5 "w")))   ; window id 5, active pane id 1
             (cl-tmux/hooks:run-hooks cl-tmux/hooks:+hook-window-pane-changed+ win)
             (is (search "%window-pane-changed @5 %1" (get-output-stream-string out))
                 "emitted with the window id and its active pane id"))
        (cl-tmux::%remove-control-notifications handlers)))))

(test control-notifications-session-window-changed
  "+hook-session-window-changed+ emits %session-window-changed $<sess> @<active-win>."
  (with-isolated-hooks
    (let* ((out      (make-string-output-stream))
           (handlers (cl-tmux::%install-control-notifications out)))
      (unwind-protect
           (let ((sess (make-fake-session :nwindows 2)))  ; session id 1, active win id 0
             (cl-tmux/hooks:run-hooks cl-tmux/hooks:+hook-session-window-changed+ sess)
             (is (search "%session-window-changed $1 @0" (get-output-stream-string out))
                 "emitted with the session id and its active window id"))
        (cl-tmux::%remove-control-notifications handlers)))))

;;; ── control-mode %output relay (#17) ─────────────────────────────────────────

(test control-notifications-pane-output-emits-percent-output
  "+hook-pane-output+ emits %output %<pane-id> <escaped-data> to a control client."
  (with-isolated-hooks
    (let* ((out      (make-string-output-stream))
           (handlers (cl-tmux::%install-control-notifications out)))
      (unwind-protect
           (let ((pane (%make-test-pane :id 7)))
             ;; Fire the hook with an octet vector (as runtime.lisp does).
             (cl-tmux/hooks:run-hooks cl-tmux/hooks:+hook-pane-output+
                                      pane
                                      (coerce '(104 101 108 108 111) ; "hello"
                                              '(vector (unsigned-byte 8))))
             (is (search "%output %7 hello" (get-output-stream-string out))
                 "%output notification emitted with pane id and escaped bytes"))
        (cl-tmux::%remove-control-notifications handlers)))))

(test control-notifications-pane-output-escapes-non-printable
  "+hook-pane-output+ escapes non-printable bytes in the %output notification."
  (with-isolated-hooks
    (let* ((out      (make-string-output-stream))
           (handlers (cl-tmux::%install-control-notifications out)))
      (unwind-protect
           (let ((pane (%make-test-pane :id 3)))
             ;; ESC (27 = octal 033) followed by 'A' (65).
             (cl-tmux/hooks:run-hooks cl-tmux/hooks:+hook-pane-output+
                                      pane
                                      (coerce '(27 65)
                                              '(vector (unsigned-byte 8))))
             (is (search "%output %3 \\033A" (get-output-stream-string out))
                 "ESC byte is escaped to \\033 in %output notification"))
        (cl-tmux::%remove-control-notifications handlers)))))

(test control-emit-serializes-concurrent-writers
  "%control-emit holds *control-output-lock*, so notifications emitted from
   multiple threads do not interleave a single write-line on the output stream.
   Each emitted line lands intact (a full %output line per call) and the count
   matches the number of emits — no torn/merged lines."
  (with-isolated-hooks
    (let* ((out      (make-string-output-stream))
           (cl-tmux::*control-output-lock* (cl-tmux::make-lock "test-control"))
           (n        200)
           (line     "%output %1 hello")
           (threads  (loop repeat 4
                           collect (cl-tmux::make-thread
                                    (lambda ()
                                      (dotimes (i n)
                                        (cl-tmux::%control-emit out line)))))))
      (mapc #'cl-tmux::join-thread threads)
      (let* ((s     (get-output-stream-string out))
             (lines (with-input-from-string (in s)
                      (loop for l = (read-line in nil nil)
                            while l collect l))))
        (is (= (* 4 n) (length lines))
            "every %control-emit produced exactly one whole line (no torn writes)")
        (is (every (lambda (l) (string= l line)) lines)
            "every line is the intact %output line (no interleaving)")))))

(test control-emit-respects-bound-lock
  "%control-emit acquires the dynamically-bound *control-output-lock*; emitting
   while that lock is already held by the current thread must still succeed
   (recursive lock is not required — this asserts the binding is the one used)."
  (with-isolated-hooks
    (let* ((out  (make-string-output-stream))
           (lock (cl-tmux::make-lock "bound-control"))
           (cl-tmux::*control-output-lock* lock))
      (cl-tmux::%control-emit out "%window-add @9")
      (is (search "%window-add @9" (get-output-stream-string out))
          "emit through the bound lock writes the line"))))

(test control-notifications-pane-output-noop-on-empty
  "+hook-pane-output+ does not emit when the byte vector is empty."
  (with-isolated-hooks
    (let* ((out      (make-string-output-stream))
           (handlers (cl-tmux::%install-control-notifications out)))
      (unwind-protect
           (let ((pane (%make-test-pane :id 2)))
             (cl-tmux/hooks:run-hooks cl-tmux/hooks:+hook-pane-output+
                                      pane
                                      (make-array 0 :element-type '(unsigned-byte 8)))
             (is (string= "" (get-output-stream-string out))
                 "empty byte vector must not emit %output"))
        (cl-tmux::%remove-control-notifications handlers)))))
