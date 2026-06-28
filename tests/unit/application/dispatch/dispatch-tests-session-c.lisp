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
  (with-isolated-config
    (with-isolated-options ("default-size" "100x40")
      (with-fake-session (s)
        (let* ((cl-tmux::*server-sessions* nil)
               (new (cl-tmux::%cmd-new-session-arg s '("-d" "-s" "bg")))
               (win (session-active-window new))
               (expected-height (- 40 cl-tmux/config:*status-height*)))
          (is (= 100 (window-width win))
              "detached new-session width comes from default-size (100)")
          (is (= expected-height (window-height win))
              "detached new-session window height is default-size rows minus status bar"))))))

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
  (with-fake-session (s0)
    (let ((s1 (make-fake-session)))
      (with-empty-registry
        (is (eq s0 (cl-tmux::%current-session s0))
            "empty registry → fallback to the argument"))
      (setf (cl-tmux::session-last-active s0) 10
            (cl-tmux::session-last-active s1) 20)
      (with-registered-sessions (("0" s0) ("1" s1))
        (is (eq s1 (cl-tmux::%current-session s0))
            "registry non-empty → the highest last-active (s1)")
        (setf (cl-tmux::session-last-active s0) 30)
        (is (eq s0 (cl-tmux::%current-session s1))
            "after touching s0 it becomes current — the display follows")))))

(test switch-to-session-makes-it-the-current-session
  "%switch-to-session(target) makes target the %current-session, so the loop's
   re-resolution displays it (end-to-end switch → display)."
  (with-fake-session (s0)
    (let ((s1 (make-fake-session)))
      (setf (cl-tmux::session-last-active s0) 100
            (cl-tmux::session-last-active s1) 50)
      (with-registered-sessions (("0" s0) ("1" s1))
        (is (eq s0 (cl-tmux::%current-session s0)) "s0 starts as current")
        (cl-tmux::%switch-to-session s1)
        (is (eq s1 (cl-tmux::%current-session s0))
            "after %switch-to-session s1, s1 is current")))))

;;; ── detach-on-destroy: client fate when its session is destroyed ─────────────

(test detach-on-destroy-with-survivors-table
  "detach-on-destroy on→:quit (detach); off→nil (switch to survivor).
   Each row: (opt-val expected description)."
  (dolist (row '(("on"  :quit "on + survivors → :quit (detach)")
                 ("off" nil   "off + survivors → nil (switch, loop follows)")))
    (destructuring-bind (opt-val expected desc) row
      (with-fake-session (s1)
        (with-isolated-options ("detach-on-destroy" opt-val)
          (with-registered-sessions (("1" s1))
            (is (eq expected (cl-tmux::%detach-on-destroy-action "0")) desc)))))))

(test detach-on-destroy-no-survivors-always-quits
  "With no surviving sessions, detach-on-destroy always detaches (:quit)."
  (with-loop-state
    (with-isolated-options ("detach-on-destroy" "off")
      (with-empty-registry
        (is (eq :quit (cl-tmux::%detach-on-destroy-action "0"))
            "no survivors → :quit regardless of mode")))))

(test alphabetical-neighbour-prev-next-and-wrap
  "%alphabetical-neighbour finds the next/prev surviving session by name, wrapping."
  (let ((sa (make-fake-session)) (sc (make-fake-session)) (se (make-fake-session)))
    (setf (cl-tmux::session-name sa) "a"
          (cl-tmux::session-name sc) "c"
          (cl-tmux::session-name se) "e")
    (with-registered-sessions (("a" sa) ("c" sc) ("e" se))
      (is (eq se (cl-tmux::%alphabetical-neighbour "c"  1)) "next of c is e")
      (is (eq sa (cl-tmux::%alphabetical-neighbour "c" -1)) "prev of c is a")
      (is (eq sa (cl-tmux::%alphabetical-neighbour "z"  1)) "next of z wraps to a")
      (is (eq se (cl-tmux::%alphabetical-neighbour "0" -1)) "prev of 0 wraps to e"))))

(test detach-on-destroy-next-switches-to-alphabetical-neighbour
  "detach-on-destroy next switches to the alphabetically-next surviving session."
  (with-fake-session (sa)
    (with-isolated-options ("detach-on-destroy" "next")
      (let ((sc (make-fake-session)))
        (setf (cl-tmux::session-name sa) "a" (cl-tmux::session-last-active sa) 5
              (cl-tmux::session-name sc) "c" (cl-tmux::session-last-active sc) 5)
        (with-registered-sessions (("a" sa) ("c" sc))
          (is (null (cl-tmux::%detach-on-destroy-action "b")) "next returns nil (switch)")
          (is (eq sc (cl-tmux::%current-session sa))
              "next switched to the alphabetically-next survivor (c)"))))))

(test dispatch-kill-session-detach-on-destroy-table
  ":kill-session on→:quit (detach); off→nil (switch to survivor).
   Each row: (opt-val expected description)."
  (dolist (row '(("on"  :quit "on + survivors → :quit (detach)")
                 ("off" nil   "off + survivors → nil (keep running)")))
    (destructuring-bind (opt-val expected desc) row
      (with-fake-session (s1)
        (with-isolated-options ("detach-on-destroy" opt-val)
          (let ((s2 (make-fake-session)))
            (setf (cl-tmux::session-name s1) "cur" (cl-tmux::session-name s2) "other")
            (with-registered-sessions (("cur" s1) ("other" s2))
              (is (eq expected (cl-tmux::dispatch-command s1 :kill-session nil)) desc))))))))

;;; ── destroy-unattached: destroy the session left behind on a switch ──────────

(test destroy-unattached-table
  "destroy-unattached off keeps the old session; on destroys it on switch-away.
   Each row: (opt-val expect-a-survives description)."
  (dolist (row '((nil t "off: old session survives")
                 (t   nil "on: old session is destroyed")))
    (destructuring-bind (opt-val expect-a-survives desc) row
      (with-fake-session (a)
        (with-isolated-options ("destroy-unattached" opt-val)
          (let ((b (make-fake-session)))
            (setf (cl-tmux::session-name a) "a" (cl-tmux::session-last-active a) 20
                  (cl-tmux::session-name b) "b" (cl-tmux::session-last-active b) 10)
            (with-registered-sessions (("a" a) ("b" b))
              (cl-tmux::%switch-to-session b)
              (if expect-a-survives
                  (is-true  (cl-tmux::server-find-session "a") desc)
                  (is-false (cl-tmux::server-find-session "a") desc))
              (is-true (cl-tmux::server-find-session "b") "target session always survives"))))))))


(test destroy-unattached-no-destroy-switching-to-current
  "Switching to the already-current session destroys nothing (old == target)."
  (with-fake-session (a)
    (with-isolated-options ("destroy-unattached" t)
      (setf (cl-tmux::session-name a) "a" (cl-tmux::session-last-active a) 20)
      (with-registered-sessions (("a" a))
        (cl-tmux::%switch-to-session a)
        (is (cl-tmux::server-find-session "a")
            "switching to current destroys nothing")))))

;;; ── rename-session: registry key + duplicate-name refusal ────────────────────

(test rename-session-checked-updates-registry-key
  "%rename-session-checked re-keys *server-sessions* under the new name."
  (with-fake-session (s)
    (setf (cl-tmux::session-name s) "old")
    (with-registered-sessions (("old" s))
      (is (eq t (cl-tmux::%rename-session-checked s "new")) "rename succeeds")
      (is (string= "new" (cl-tmux::session-name s)) "name updated")
      (is (eq s (cl-tmux::server-find-session "new")) "findable under new name")
      (is (null (cl-tmux::server-find-session "old")) "old key removed"))))

(test rename-session-checked-refuses-duplicate-name
  "Renaming onto a name already used by a DIFFERENT session is refused — the other
   session must not be orphaned."
  (with-fake-session (a)
    (let ((b (make-fake-session)))
      (setf (cl-tmux::session-name a) "a" (cl-tmux::session-name b) "b")
      (with-registered-sessions (("a" a) ("b" b))
        (is (null (cl-tmux::%rename-session-checked a "b"))
            "rename a → existing name b is refused")
        (is (string= "a" (cl-tmux::session-name a)) "a keeps its name")
        (is (eq b (cl-tmux::server-find-session "b")) "b is not orphaned")
        (is (eq a (cl-tmux::server-find-session "a")) "a still findable")))))

(test rename-session-checked-to-own-name-noop-success
  "Renaming a session to its own current name succeeds as a harmless no-op."
  (with-fake-session (s)
    (setf (cl-tmux::session-name s) "x")
    (with-registered-sessions (("x" s))
      (is (eq t (cl-tmux::%rename-session-checked s "x")) "self-rename succeeds")
      (is (eq s (cl-tmux::server-find-session "x")) "still findable"))))

(test rename-session-checked-fires-hook
  "%rename-session-checked fires +hook-session-renamed+ on a successful rename."
  (with-isolated-hooks
    (with-fake-session (s)
      (let ((fired nil))
        (setf (cl-tmux::session-name s) "old")
        (cl-tmux/hooks:add-hook cl-tmux/hooks:+hook-session-renamed+
                                (lambda (&rest _) (declare (ignore _)) (setf fired t)))
        (with-registered-sessions (("old" s))
          (cl-tmux::%rename-session-checked s "new")
          (is-true fired "the session-renamed hook fired"))))))

;;; ── move-window: index collision shifts others up; -a inserts after ──────────

(test window-id-occupied-and-shuffle-helpers
  "%window-id-occupied-p and the exclude argument."
  (with-fake-session (s :nwindows 3)
    (let* ((w0 (find 0 (session-windows s) :key #'window-id)))
      (is-true  (cl-tmux::%window-id-occupied-p s 1 nil) "index 1 is occupied")
      (is-false (cl-tmux::%window-id-occupied-p s 9 nil) "index 9 is free")
      (is-false (cl-tmux::%window-id-occupied-p s 0 w0)
                "index 0 is free when its own window is excluded"))))

(test move-window-to-free-index
  "move-window -s W -t N to a free index just reassigns the id."
  (with-fake-session (s :nwindows 2)
    (let* ((w1 (find 1 (session-windows s) :key #'window-id)))
      (cl-tmux::%cmd-move-window s '("-s" "1" "-t" "5"))
      (is (= 5 (window-id w1)) "moved to free index 5")
      (is (find 0 (session-windows s) :key #'window-id) "window 0 unchanged"))))

(test move-window-to-occupied-index-shifts-up
  "move-window onto an occupied index shifts the occupants up (no overwrite/no-op)."
  (with-fake-session (s :nwindows 3)
    (let* ((w0 (find 0 (session-windows s) :key #'window-id))
           (w2 (find 2 (session-windows s) :key #'window-id)))
      (cl-tmux::%cmd-move-window s '("-s" "2" "-t" "0"))
      (is (= 0 (window-id w2)) "the moved window takes index 0")
      (is (= 1 (window-id w0)) "the window formerly at 0 shifted to 1")
      (is (= 3 (length (remove-duplicates (mapcar #'window-id (session-windows s)))))
          "all window indices remain distinct (no collision)"))))

(test move-window-a-inserts-after-target
  "move-window -a -t N inserts after index N (at N+1), shifting if occupied."
  (with-fake-session (s :nwindows 3)
    (let* ((w2 (find 2 (session-windows s) :key #'window-id)))
      (cl-tmux::%cmd-move-window s '("-s" "2" "-a" "-t" "0"))
      (is (= 1 (window-id w2)) "with -a, the window lands after index 0 = index 1")
      (is (= 3 (length (remove-duplicates (mapcar #'window-id (session-windows s)))))
          "indices remain distinct"))))
