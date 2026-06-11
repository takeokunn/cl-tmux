(in-package #:cl-tmux/test)

;;;; Options, session management, control mode, and server-lifecycle tests.
;;;;  (dispatch-commands-option.lisp, dispatch-commands-auto.lisp,
;;;;   session-registry.lisp, dispatch-control.lisp, dispatch-handlers.lisp)

(in-suite dispatch-suite)

;;; ── main-pane-width/height options flow into the main layouts ────────────────

(test apply-named-layout-main-vertical-reads-main-pane-width-option
  "%apply-named-layout-to-session :main-vertical sizes the main pane from the
   main-pane-width option (read at the cl-tmux layer, threaded into the model)."
  (with-isolated-options ("main-pane-width" 50)
    (let* ((p0  (make-no-pty-pane 1 0 0 100 30))
           (p1  (make-no-pty-pane 2 0 0 100 30))
           (win (make-window :id 1 :name "w" :width 100 :height 30
                             :panes (list p0 p1)))
           (sess (make-session :id 1 :name "0" :windows (list win))))
      (session-select-window sess win)
      (cl-tmux::%apply-named-layout-to-session sess :main-vertical)
      (is (= 50 (pane-width p0))
          "main pane width is taken from the main-pane-width option"))))

(test apply-named-layout-main-vertical-reads-other-pane-width-option
  "%apply-named-layout-to-session :main-vertical reads other-pane-width and sizes
   the other panes to it (main pane takes the rest)."
  (with-isolated-options ("other-pane-width" 25)
    (let* ((p0  (make-no-pty-pane 1 0 0 200 30))
           (p1  (make-no-pty-pane 2 0 0 200 30))
           (win (make-window :id 1 :name "w" :width 200 :height 30
                             :panes (list p0 p1)))
           (sess (make-session :id 1 :name "0" :windows (list win))))
      (session-select-window sess win)
      (cl-tmux::%apply-named-layout-to-session sess :main-vertical)
      (is (= 25 (pane-width p1))
          "the other pane uses the other-pane-width option (25)"))))

;;; ── default-size: detached new-session sizing ────────────────────────────────

(test parse-wxh-parses-size-strings
  "%parse-wxh parses \"WxH\" into (values W H); rejects malformed input."
  (multiple-value-bind (w h) (cl-tmux::%parse-wxh "80x24")
    (is (= 80 w)) (is (= 24 h)))
  (multiple-value-bind (w h) (cl-tmux::%parse-wxh "100x40")
    (is (= 100 w)) (is (= 40 h)))
  (multiple-value-bind (w h) (cl-tmux::%parse-wxh "junk")
    (is (null w) "no 'x' separator → NIL") (is (null h)))
  (multiple-value-bind (w h) (cl-tmux::%parse-wxh "80x")
    (is (null w) "missing height → NIL") (is (null h))))

(test cmd-new-session-detached-uses-default-size
  "new-session -d with no -x/-y sizes the detached session from the default-size
   option (it has no client to size it), not the current terminal."
  (with-isolated-options ("default-size" "100x40")
    (with-loop-state
      (let* ((cl-tmux::*server-sessions* nil)
             (s   (make-fake-session))
             (new (cl-tmux::%cmd-new-session-arg s '("-d" "-s" "bg"))))
        (is (= 100 (window-width (session-active-window new)))
            "detached new-session width comes from default-size (100)")
        (is (= 40 (window-height (session-active-window new)))
            "detached new-session height comes from default-size (40)")))))

;;; ── popup-border-lines: popup box-drawing character set ──────────────────────

(test popup-border-chars-per-style
  "%popup-border-chars returns the box-drawing set for each popup-border-lines
   value; an unknown value falls back to single."
  (with-isolated-options ("popup-border-lines" "double")
    (multiple-value-bind (tl tr bl br h) (cl-tmux::%popup-border-chars)
      (is (char= #\╔ tl)) (is (char= #\╗ tr)) (is (char= #\╚ bl))
      (is (char= #\╝ br)) (is (char= #\═ h))))
  (with-isolated-options ("popup-border-lines" "rounded")
    (multiple-value-bind (tl tr bl br h) (cl-tmux::%popup-border-chars)
      (declare (ignore h))
      (is (char= #\╭ tl)) (is (char= #\╮ tr)) (is (char= #\╰ bl)) (is (char= #\╯ br))))
  (with-isolated-options ("popup-border-lines" "simple")
    (multiple-value-bind (tl tr bl br h) (cl-tmux::%popup-border-chars)
      (is (char= #\+ tl)) (is (char= #\+ tr)) (is (char= #\+ bl))
      (is (char= #\+ br)) (is (char= #\- h))))
  (with-isolated-options ("popup-border-lines" "bogus")
    (multiple-value-bind (tl tr bl br h) (cl-tmux::%popup-border-chars)
      (declare (ignore tr bl br h))
      (is (char= #\┌ tl) "unknown value falls back to single (┌)"))))

(test format-popup-overlay-uses-border-lines-option
  "%format-popup-overlay draws the box with the popup-border-lines characters."
  (with-isolated-options ("popup-border-lines" "double")
    (let ((result (cl-tmux::%format-popup-overlay "t" "body")))
      (is (search "╔" result) "double border uses ╔ top-left")
      (is (search "╝" result) "double border uses ╝ bottom-right")
      (is (search "body" result) "body content is present"))))

;;; ── %current-session: the standalone loop follows the front session ──────────

(test current-session-follows-most-recently-touched
  "%current-session returns the session with the highest last-active, and falls
   back to its argument when the registry is empty.  This is what makes the
   single-client display follow session-switch commands."
  (with-loop-state
    (let ((s0 (make-fake-session))
          (s1 (make-fake-session)))
      (let ((cl-tmux::*server-sessions* nil))
        (is (eq s0 (cl-tmux::%current-session s0))
            "empty registry → fallback to the argument"))
      (setf (cl-tmux::session-last-active s0) 10
            (cl-tmux::session-last-active s1) 20)
      (let ((cl-tmux::*server-sessions* (list (cons "0" s0) (cons "1" s1))))
        (is (eq s1 (cl-tmux::%current-session s0))
            "registry non-empty → the highest last-active (s1)")
        (setf (cl-tmux::session-last-active s0) 30)
        (is (eq s0 (cl-tmux::%current-session s1))
            "after touching s0 it becomes current — the display follows")))))

(test switch-to-session-makes-it-the-current-session
  "%switch-to-session(target) makes target the %current-session, so the loop's
   re-resolution displays it (end-to-end switch → display)."
  (with-loop-state
    (let ((s0 (make-fake-session)) (s1 (make-fake-session)))
      (setf (cl-tmux::session-last-active s0) 100
            (cl-tmux::session-last-active s1) 50)
      (let ((cl-tmux::*server-sessions* (list (cons "0" s0) (cons "1" s1))))
        (is (eq s0 (cl-tmux::%current-session s0)) "s0 starts as current")
        (cl-tmux::%switch-to-session s1)
        (is (eq s1 (cl-tmux::%current-session s0))
            "after %switch-to-session s1, s1 is current")))))

;;; ── detach-on-destroy: client fate when its session is destroyed ─────────────

(test detach-on-destroy-on-quits-even-with-survivors
  "detach-on-destroy on (default): destroying the viewed session detaches (:quit)
   even when other sessions survive."
  (with-loop-state
    (with-isolated-options ("detach-on-destroy" "on")
      (let ((s1 (make-fake-session)))
        (let ((cl-tmux::*server-sessions* (list (cons "1" s1))))
          (is (eq :quit (cl-tmux::%detach-on-destroy-action "0"))
              "on + survivors → :quit"))))))

(test detach-on-destroy-off-switches-to-survivor
  "detach-on-destroy off: destroying the viewed session switches to a survivor."
  (with-loop-state
    (with-isolated-options ("detach-on-destroy" "off")
      (let ((s1 (make-fake-session)))
        (let ((cl-tmux::*server-sessions* (list (cons "1" s1))))
          (is (null (cl-tmux::%detach-on-destroy-action "0"))
              "off + survivors → nil (switch, loop follows)"))))))

(test detach-on-destroy-no-survivors-always-quits
  "With no surviving sessions, detach-on-destroy always detaches (:quit)."
  (with-loop-state
    (with-isolated-options ("detach-on-destroy" "off")
      (let ((cl-tmux::*server-sessions* nil))
        (is (eq :quit (cl-tmux::%detach-on-destroy-action "0"))
            "no survivors → :quit regardless of mode")))))

(test alphabetical-neighbour-prev-next-and-wrap
  "%alphabetical-neighbour finds the next/prev surviving session by name, wrapping."
  (let ((sa (make-fake-session)) (sc (make-fake-session)) (se (make-fake-session)))
    (setf (cl-tmux::session-name sa) "a"
          (cl-tmux::session-name sc) "c"
          (cl-tmux::session-name se) "e")
    (let ((cl-tmux::*server-sessions* (list (cons "a" sa) (cons "c" sc) (cons "e" se))))
      (is (eq se (cl-tmux::%alphabetical-neighbour "c"  1)) "next of c is e")
      (is (eq sa (cl-tmux::%alphabetical-neighbour "c" -1)) "prev of c is a")
      (is (eq sa (cl-tmux::%alphabetical-neighbour "z"  1)) "next of z wraps to a")
      (is (eq se (cl-tmux::%alphabetical-neighbour "0" -1)) "prev of 0 wraps to e"))))

(test detach-on-destroy-next-switches-to-alphabetical-neighbour
  "detach-on-destroy next switches to the alphabetically-next surviving session."
  (with-loop-state
    (with-isolated-options ("detach-on-destroy" "next")
      (let ((sa (make-fake-session)) (sc (make-fake-session)))
        (setf (cl-tmux::session-name sa) "a" (cl-tmux::session-last-active sa) 5
              (cl-tmux::session-name sc) "c" (cl-tmux::session-last-active sc) 5)
        (let ((cl-tmux::*server-sessions* (list (cons "a" sa) (cons "c" sc))))
          (is (null (cl-tmux::%detach-on-destroy-action "b")) "next returns nil (switch)")
          (is (eq sc (cl-tmux::%current-session sa))
              "next switched to the alphabetically-next survivor (c)"))))))

(test dispatch-kill-session-default-on-detaches-with-survivors
  ":kill-session with detach-on-destroy on (default) + a survivor returns :quit."
  (with-loop-state
    (with-isolated-options ("detach-on-destroy" "on")
      (let ((s1 (make-fake-session)) (s2 (make-fake-session)))
        (setf (cl-tmux::session-name s1) "cur" (cl-tmux::session-name s2) "other")
        (let ((cl-tmux::*server-sessions* (list (cons "cur" s1) (cons "other" s2))))
          (is (eq :quit (cl-tmux::dispatch-command s1 :kill-session nil))
              "kill current with survivors + on → :quit (detach)"))))))

(test dispatch-kill-session-off-keeps-running-with-survivors
  ":kill-session with detach-on-destroy off keeps running (switches to survivor)."
  (with-loop-state
    (with-isolated-options ("detach-on-destroy" "off")
      (let ((s1 (make-fake-session)) (s2 (make-fake-session)))
        (setf (cl-tmux::session-name s1) "cur" (cl-tmux::session-name s2) "other")
        (let ((cl-tmux::*server-sessions* (list (cons "cur" s1) (cons "other" s2))))
          (is (null (cl-tmux::dispatch-command s1 :kill-session nil))
              "kill current with survivors + off → nil (keep running)"))))))

;;; ── destroy-unattached: destroy the session left behind on a switch ──────────

(test destroy-unattached-off-keeps-left-session
  "With destroy-unattached off (default), switching away leaves the old session."
  (with-loop-state
    (with-isolated-options ("destroy-unattached" nil)
      (let ((a (make-fake-session)) (b (make-fake-session)))
        (setf (cl-tmux::session-name a) "a" (cl-tmux::session-last-active a) 20
              (cl-tmux::session-name b) "b" (cl-tmux::session-last-active b) 10)
        (let ((cl-tmux::*server-sessions* (list (cons "a" a) (cons "b" b))))
          (cl-tmux::%switch-to-session b)
          (is (cl-tmux::server-find-session "a")
              "old session survives when destroy-unattached is off"))))))

(test destroy-unattached-on-destroys-left-session
  "With destroy-unattached on, switching away destroys the session left behind."
  (with-loop-state
    (with-isolated-options ("destroy-unattached" t)
      (let ((a (make-fake-session)) (b (make-fake-session)))
        (setf (cl-tmux::session-name a) "a" (cl-tmux::session-last-active a) 20
              (cl-tmux::session-name b) "b" (cl-tmux::session-last-active b) 10)
        (let ((cl-tmux::*server-sessions* (list (cons "a" a) (cons "b" b))))
          (cl-tmux::%switch-to-session b)
          (is (null (cl-tmux::server-find-session "a"))
              "old session is destroyed when destroy-unattached is on")
          (is (cl-tmux::server-find-session "b") "target session survives"))))))

(test destroy-unattached-no-destroy-switching-to-current
  "Switching to the already-current session destroys nothing (old == target)."
  (with-loop-state
    (with-isolated-options ("destroy-unattached" t)
      (let ((a (make-fake-session)))
        (setf (cl-tmux::session-name a) "a" (cl-tmux::session-last-active a) 20)
        (let ((cl-tmux::*server-sessions* (list (cons "a" a))))
          (cl-tmux::%switch-to-session a)
          (is (cl-tmux::server-find-session "a")
              "switching to current destroys nothing"))))))

;;; ── rename-session: registry key + duplicate-name refusal ────────────────────

(test rename-session-checked-updates-registry-key
  "%rename-session-checked re-keys *server-sessions* under the new name."
  (with-loop-state
    (let ((s (make-fake-session)))
      (setf (cl-tmux::session-name s) "old")
      (let ((cl-tmux::*server-sessions* (list (cons "old" s))))
        (is (eq t (cl-tmux::%rename-session-checked s "new")) "rename succeeds")
        (is (string= "new" (cl-tmux::session-name s)) "name updated")
        (is (eq s (cl-tmux::server-find-session "new")) "findable under new name")
        (is (null (cl-tmux::server-find-session "old")) "old key removed")))))

(test rename-session-checked-refuses-duplicate-name
  "Renaming onto a name already used by a DIFFERENT session is refused — the other
   session must not be orphaned."
  (with-loop-state
    (let ((a (make-fake-session)) (b (make-fake-session)))
      (setf (cl-tmux::session-name a) "a" (cl-tmux::session-name b) "b")
      (let ((cl-tmux::*server-sessions* (list (cons "a" a) (cons "b" b))))
        (is (null (cl-tmux::%rename-session-checked a "b"))
            "rename a → existing name b is refused")
        (is (string= "a" (cl-tmux::session-name a)) "a keeps its name")
        (is (eq b (cl-tmux::server-find-session "b")) "b is not orphaned")
        (is (eq a (cl-tmux::server-find-session "a")) "a still findable")))))

(test rename-session-checked-to-own-name-noop-success
  "Renaming a session to its own current name succeeds as a harmless no-op."
  (with-loop-state
    (let ((s (make-fake-session)))
      (setf (cl-tmux::session-name s) "x")
      (let ((cl-tmux::*server-sessions* (list (cons "x" s))))
        (is (eq t (cl-tmux::%rename-session-checked s "x")) "self-rename succeeds")
        (is (eq s (cl-tmux::server-find-session "x")) "still findable")))))

(test rename-session-checked-fires-hook
  "%rename-session-checked fires +hook-session-renamed+ on a successful rename."
  (with-isolated-hooks
    (with-loop-state
      (let ((s (make-fake-session)) (fired nil))
        (setf (cl-tmux::session-name s) "old")
        (cl-tmux/hooks:add-hook cl-tmux/hooks:+hook-session-renamed+
                                (lambda (&rest _) (declare (ignore _)) (setf fired t)))
        (let ((cl-tmux::*server-sessions* (list (cons "old" s))))
          (cl-tmux::%rename-session-checked s "new")
          (is-true fired "the session-renamed hook fired"))))))

;;; ── move-window: index collision shifts others up; -a inserts after ──────────

(test window-id-occupied-and-shuffle-helpers
  "%window-id-occupied-p and the exclude argument."
  (with-loop-state
    (let* ((s (make-fake-session :nwindows 3))
           (w0 (find 0 (session-windows s) :key #'window-id)))
      (is-true  (cl-tmux::%window-id-occupied-p s 1 nil) "index 1 is occupied")
      (is-false (cl-tmux::%window-id-occupied-p s 9 nil) "index 9 is free")
      (is-false (cl-tmux::%window-id-occupied-p s 0 w0)
                "index 0 is free when its own window is excluded"))))

(test move-window-to-free-index
  "move-window -s W -t N to a free index just reassigns the id."
  (with-loop-state
    (let* ((s (make-fake-session :nwindows 2))
           (w1 (find 1 (session-windows s) :key #'window-id)))
      (cl-tmux::%cmd-move-window s '("-s" "1" "-t" "5"))
      (is (= 5 (window-id w1)) "moved to free index 5")
      (is (find 0 (session-windows s) :key #'window-id) "window 0 unchanged"))))

(test move-window-to-occupied-index-shifts-up
  "move-window onto an occupied index shifts the occupants up (no overwrite/no-op)."
  (with-loop-state
    (let* ((s  (make-fake-session :nwindows 3))            ; ids 0,1,2
           (w0 (find 0 (session-windows s) :key #'window-id))
           (w2 (find 2 (session-windows s) :key #'window-id)))
      (cl-tmux::%cmd-move-window s '("-s" "2" "-t" "0"))
      (is (= 0 (window-id w2)) "the moved window takes index 0")
      (is (= 1 (window-id w0)) "the window formerly at 0 shifted to 1")
      (is (= 3 (length (remove-duplicates (mapcar #'window-id (session-windows s)))))
          "all window indices remain distinct (no collision)"))))

(test move-window-a-inserts-after-target
  "move-window -a -t N inserts after index N (at N+1), shifting if occupied."
  (with-loop-state
    (let* ((s  (make-fake-session :nwindows 3))            ; ids 0,1,2
           (w2 (find 2 (session-windows s) :key #'window-id)))
      (cl-tmux::%cmd-move-window s '("-s" "2" "-a" "-t" "0"))
      (is (= 1 (window-id w2)) "with -a, the window lands after index 0 = index 1")
      (is (= 3 (length (remove-duplicates (mapcar #'window-id (session-windows s)))))
          "indices remain distinct"))))

;;; ── new-session -s duplicate-name handling ───────────────────────────────────

(test new-session-explicit-duplicate-name-refused
  "new-session -s NAME with an existing session NAME (no -A) is refused — the
   existing session is not orphaned."
  (with-loop-state
    (let ((existing (make-fake-session)) (caller (make-fake-session)))
      (setf (cl-tmux::session-name existing) "work")
      (let ((cl-tmux::*server-sessions* (list (cons "work" existing)))
            (*overlay* nil))
        (is (null (cl-tmux::%cmd-new-session-arg caller '("-s" "work")))
            "duplicate -s name is refused (returns nil)")
        (is (eq existing (cl-tmux::server-find-session "work"))
            "the existing session is intact, not orphaned")
        (is (= 1 (length cl-tmux::*server-sessions*))
            "no second session was created")))))

(test new-session-A-attaches-to-existing
  "new-session -A -s NAME attaches to (returns) the existing session NAME."
  (with-loop-state
    (let ((existing (make-fake-session)) (caller (make-fake-session)))
      (setf (cl-tmux::session-name existing) "work")
      (let ((cl-tmux::*server-sessions* (list (cons "work" existing))))
        (is (eq existing (cl-tmux::%cmd-new-session-arg caller '("-A" "-s" "work")))
            "-A returns the existing session")
        (is (= 1 (length cl-tmux::*server-sessions*))
            "no new session created")))))

(test new-session-auto-name-avoids-collision
  "An auto-generated session name that would collide bumps to the next free
   number instead of orphaning the existing session."
  (with-loop-state
    (let ((s2 (make-fake-session)))
      (setf (cl-tmux::session-name s2) "2")
      (let ((cl-tmux::*server-sessions* (list (cons "2" s2)))
            (*overlay* nil))
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
  (with-loop-state
    (let ((target (make-fake-session)) (caller (make-fake-session)))
      (setf (cl-tmux::session-name target) "base")
      (let ((cl-tmux::*server-sessions* (list (cons "base" target)))
            (cl-tmux::*session-groups*  nil)
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
              "both sessions share the same group id"))))))

(test new-session-t-missing-target-refused
  "new-session -t with an unknown target is refused (returns nil) and registers
   no session — the partial group must not leak a half-built session."
  (with-loop-state
    (let ((caller (make-fake-session)))
      (let ((cl-tmux::*server-sessions* nil)
            (cl-tmux::*session-groups*  nil)
            (*overlay* nil))
        (is (null (cl-tmux::%cmd-new-session-arg
                   caller '("-d" "-s" "clone" "-t" "ghost")))
            "missing -t target is refused (returns nil)")
        (is (null cl-tmux::*server-sessions*)
            "no session was registered")))))

;;; ── control mode (-C) REPL ───────────────────────────────────────────────────

(test control-run-command-frames-output
  "%control-run-command frames a command's overlay output in a %begin/%end block."
  (with-loop-state
    (let* ((s     (make-fake-session))
           (reply (cl-tmux::%control-run-command s "display-message hello" 1)))
      (is (search "%begin 0 1 1" reply) "reply opens with %begin for command 1")
      (is (search "%end 0 1 1" reply)   "reply closes with %end")
      (is (search "hello" reply)        "the command's output is in the reply body"))))

(test control-mode-loop-frames-each-and-exits
  "control-mode-loop runs each input line as the next numbered command and emits
   %exit at EOF."
  (with-loop-state
    (let* ((s   (make-fake-session))
           (out (with-output-to-string (o)
                  (with-input-from-string
                      (i (format nil "display-message a~%display-message b~%"))
                    (cl-tmux::control-mode-loop s i o)))))
      (is (search "%begin 0 1 1" out) "first line is command 1")
      (is (search "%begin 0 2 1" out) "second line is command 2")
      (is (search "%exit" out)        "the loop emits %exit on EOF"))))

(test control-mode-loop-skips-blank-lines
  "Blank input lines are not run as commands (no reply framed for them)."
  (with-loop-state
    (let* ((s   (make-fake-session))
           (out (with-output-to-string (o)
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
  (with-loop-state
    (let* ((*overlay* nil)
           (reply (cl-tmux::%control-run-command (make-fake-session)
                                                 "bogus-command-xyz" 3)))
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
