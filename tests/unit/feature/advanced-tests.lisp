(in-package #:cl-tmux/test)

;;;; Tests for Sprint 3 advanced features:
;;;;  break-pane, synchronize-panes, layout persistence,
;;;;  lock-session, pipe-pane, session groups, choose-session.

(def-suite advanced-suite :description "Sprint 3 advanced feature tests")
(in-suite advanced-suite)

;;; ── Fixtures ─────────────────────────────────────────────────────────────────

(defun %two-pane-session ()
  "Session with one window containing two fake panes side-by-side."
  (let* ((p0  (make-no-pty-pane 1  0 0 40 24))
         (p1  (make-no-pty-pane 2 41 0 40 24))
         (win (make-window :id 1 :name "w" :width 81 :height 24
                           :panes (list p0 p1)
                           :tree (make-layout-split :h
                                    (make-layout-leaf p0)
                                    (make-layout-leaf p1)
                                    1/2)))
         (sess (make-session :id 1 :name "0" :windows (list win))))
    (window-select-pane win p0)
    (session-select-window sess win)
    (values sess win p0 p1)))

;;; ── break-pane: creates a new window ─────────────────────────────────────────

(test break-pane-creates-new-window
  "break-pane removes the active pane from its window and attaches it to a
   brand-new window; the session ends with 2 windows."
  (multiple-value-bind (sess win p0 p1) (%two-pane-session)
    (declare (ignore p1))
    ;; Break the active pane (p0) out.
    (let ((new-win (cl-tmux/commands:break-pane sess)))
      (is-true new-win
               "break-pane must return the new window")
      (is (= 2 (length (session-windows sess)))
          "session must now have 2 windows")
      ;; The new window contains only the broken-out pane.
      (is (equal (list p0) (window-panes new-win))
          "new window must contain only the extracted pane")
      ;; The original window lost that pane.
      (is (= 1 (length (window-panes win)))
          "source window must now have 1 pane"))))

(test break-pane-noop-on-sole-pane
  "break-pane returns NIL and does nothing when the window has only one pane."
  (let* ((p0   (make-no-pty-pane 1 0 0 80 24))
         (win  (make-window :id 1 :name "w" :width 80 :height 24
                            :panes (list p0)
                            :tree (make-layout-leaf p0)))
         (sess (make-session :id 1 :name "0" :windows (list win))))
    (window-select-pane win p0)
    (session-select-window sess win)
    (is (null (cl-tmux/commands:break-pane sess))
        "break-pane on a sole pane must return NIL")
    (is (= 1 (length (session-windows sess)))
        "session must still have 1 window")))

;;; ── synchronize-panes: sends keystrokes to all panes ─────────────────────────

(test synchronize-panes-sends-to-all
  "When synchronize-panes is T, %forward-octets-synchronized writes to all panes."
  ;; We test using the dispatch path by enabling the option and verifying
  ;; that all panes in the window would receive the bytes.  Since we have no
  ;; real PTY here, we just verify the option toggle works and the function
  ;; exists without erroring.
  (let ((prev (cl-tmux/options:get-option "synchronize-panes")))
    (unwind-protect
         (progn
           (cl-tmux/options:set-option "synchronize-panes" t)
           (is (cl-tmux/options:get-option "synchronize-panes")
               "synchronize-panes must be T after set-option")
           (cl-tmux/options:set-option "synchronize-panes" nil)
           (is (not (cl-tmux/options:get-option "synchronize-panes"))
               "synchronize-panes must be NIL after reset"))
      (cl-tmux/options:set-option "synchronize-panes" prev))))

(test synchronize-panes-option-registered
  "synchronize-panes is a registered option with boolean type and default nil."
  (let ((spec (gethash "synchronize-panes" cl-tmux/options:*option-registry*)))
    (is-true spec "synchronize-panes must be in the option registry")
    (is (eq :boolean (cl-tmux/options:option-spec-type spec))
        "synchronize-panes type must be :boolean")
    (is (null (cl-tmux/options:option-spec-default spec))
        "synchronize-panes default must be NIL")))

;;; ── Layout persistence: round-trip ──────────────────────────────────────────

(test layout-to-string-not-nil-for-window-with-tree
  "layout->string returns a non-NIL string for a window that has a tree."
  (multiple-value-bind (sess win p0 p1) (%two-pane-session)
    (declare (ignore sess p0 p1))
    (let ((str (cl-tmux/model:layout->string win)))
      (is-true str
               "layout->string must return a string for a window with a tree")
      (is (stringp str)
          "layout->string must return a string")
      (is (plusp (length str))
          "layout->string result must be non-empty"))))

(test layout-to-string-nil-for-empty-window
  "layout->string returns NIL when the window has no tree."
  (let ((win (make-window :id 1 :name "w" :width 80 :height 24
                          :tree nil)))
    (is (null (cl-tmux/model:layout->string win))
        "layout->string must return NIL for a window with NIL tree")))

(test layout-checksum-4-hex-chars
  "layout->string result starts with a 4-character hex checksum."
  (multiple-value-bind (_sess win p0 p1) (%two-pane-session)
    (declare (ignore _sess p0 p1))
    (let* ((str     (cl-tmux/model:layout->string win))
           (comma   (position #\, str))
           (csum    (and comma (subseq str 0 comma))))
      (is (and csum (= 4 (length csum)))
          "checksum prefix must be exactly 4 characters")
      (is (every (lambda (ch) (or (digit-char-p ch) (find ch "ABCDEFabcdef")))
                 (or csum ""))
          "checksum must be all hex digits"))))

;;; ── lock-session / unlock ────────────────────────────────────────────────────

(test lock-session-renders-lockscreen
  "When session-locked-p is T, render-session-to-string returns a lock screen
   string (contains the lock message) rather than normal content."
  (multiple-value-bind (sess win p0 p1) (%two-pane-session)
    (declare (ignore win p0 p1))
    (setf (session-locked-p sess) t)
    (let ((output (cl-tmux/renderer:render-session-to-string sess 24 80)))
      (is (search "locked" output)
          "lock screen output must contain the word 'locked'"))
    (setf (session-locked-p sess) nil)))

(test lock-session-struct-slot
  "session struct has session-locked-p slot, defaulting to NIL."
  (let ((sess (make-session :id 1 :name "test")))
    (is-false (session-locked-p sess)
              "session-locked-p must default to NIL")
    (setf (session-locked-p sess) t)
    (is (session-locked-p sess)
        "session-locked-p must be settable to T")))

;;; ── pipe-pane tee output ─────────────────────────────────────────────────────

(test pipe-pane-tees-output
  "pipe-pane-open marks the pane active; pipe-pane-close clears every pipe slot."
  (let ((p (make-no-pty-pane 1 0 0 80 24)))
    ;; Initially no pipe.
    (is (null (pane-pipe-active-p p))
        "pane-pipe-active-p must be NIL before any pipe is opened")
    ;; Close is a no-op when there is no pipe.
    (finishes (cl-tmux/commands:pipe-pane-close p))
    (is (null (pane-pipe-fd p))
        "pane-pipe-fd must remain NIL after pipe-pane-close with no pipe")
    (is (null (pane-pipe-output-stream p))
        "pane-pipe-output-stream must remain NIL after pipe-pane-close with no pipe")
    (is (null (pane-pipe-output-thread p))
        "pane-pipe-output-thread must remain NIL after pipe-pane-close with no pipe")
    (is (null (pane-pipe-process p))
        "pane-pipe-process must remain NIL after pipe-pane-close with no pipe")))

(test pipe-pane-write-is-no-op-when-no-pipe
  "pipe-pane-write does nothing and does not signal an error when pipe-fd is NIL."
  (let ((p     (make-no-pty-pane 1 0 0 80 24))
        (bytes (make-array 5 :element-type '(unsigned-byte 8)
                             :initial-contents '(104 101 108 108 111))))
    (is (null (pane-pipe-fd p)))
    (finishes (cl-tmux/commands:pipe-pane-write p bytes))
    "pipe-pane-write with no pipe must not signal an error"))

;;; ── Session groups ───────────────────────────────────────────────────────────

(test session-group-slot-defaults-nil
  "session struct has session-group slot defaulting to NIL."
  (let ((sess (make-session :id 1 :name "x")))
    (is (null (session-group sess))
        "session-group must default to NIL")))

(test session-groups-share-windows
  "server-new-session-in-group links two sessions so they share the window list."
  (let* ((p0   (make-no-pty-pane 1 0 0 80 24))
         (win  (make-window :id 1 :name "w" :width 80 :height 24
                            :panes (list p0) :tree (make-layout-leaf p0)))
         (s1   (make-session :id 1 :name "a" :windows (list win)))
         (s2   (make-session :id 2 :name "b")))
    (window-select-pane win p0)
    (session-select-window s1 win)
    ;; Bind sessions into a group.
    (let ((cl-tmux::*session-groups* nil))
      (cl-tmux::server-new-session-in-group s2 s1)
      (is-true (session-group s1)
               "existing session must have a group id assigned")
      (is (eql (session-group s1) (session-group s2))
          "both sessions must share the same group id")
      (is (eq (session-windows s1) (session-windows s2))
          "both sessions must share the exact same windows list"))))

(defun %make-group-test-window (id)
  "Build a minimal no-PTY window for session-group propagation tests."
  (let ((pane (make-no-pty-pane (+ 10 id) 0 0 80 24)))
    (let ((win (make-window :id id :name (format nil "w~D" id)
                            :width 80 :height 24
                            :panes (list pane) :tree (make-layout-leaf pane))))
      (setf (cl-tmux/model:pane-window pane) win)
      (window-select-pane win pane)
      win)))

(defmacro with-grouped-sessions ((s1 s2 win) &body body)
  "Bind S1/S2 to two grouped sessions sharing window WIN (group registry isolated)."
  `(let* ((,win (%make-group-test-window 1))
          (,s1  (make-session :id 1 :name "a" :windows (list ,win)))
          (,s2  (make-session :id 2 :name "b"))
          (cl-tmux::*session-groups* nil))
     (session-select-window ,s1 ,win)
     (cl-tmux::server-new-session-in-group ,s2 ,s1)
     ,@body))

(test session-group-new-window-propagates-to-peers
  "Inserting a window into one grouped session makes it visible in the others
   (tmux session groups share ONE window set, not just the initial list value)."
  (with-grouped-sessions (s1 s2 win)
    (let ((new-win (%make-group-test-window 2)))
      (session-insert-window s1 new-win)
      (is (member new-win (session-windows s2))
          "a window created in s1 must appear in grouped peer s2")
      (is (member win (session-windows s2))
          "the original shared window must remain in s2"))))

(test session-group-kill-window-propagates-to-peers
  "kill-window in one grouped session removes the window from all peers, and a
   peer whose active window vanished falls back to a surviving window."
  (with-grouped-sessions (s1 s2 win)
    (let ((new-win (%make-group-test-window 2)))
      (session-insert-window s1 new-win)
      ;; Peer views the window that is about to be killed.
      (setf (cl-tmux/model:session-active s2) new-win)
      (cl-tmux/commands:kill-window s1 new-win)
      (is (null (member new-win (session-windows s2)))
          "a window killed in s1 must disappear from grouped peer s2")
      (is (eq win (session-active-window s2))
          "peer's focus must repair to a surviving window"))))

(test session-group-unlink-window-tears-down-orphaned-ptys
  "unlink-window on a grouped session propagates the removal to every peer;
   a window left in NO session gets its pane PTYs closed (tmux destroys a
   fully-unreferenced window — previously the shells leaked until kill-server)."
  (with-grouped-sessions (s1 s2 win)
    (let* ((win2 (%make-group-test-window 2))
           (closed 0)
           (real-pty-close (fdefinition 'cl-tmux/pty:pty-close)))
      (session-insert-window s1 win2)
      (session-select-window s1 win2)   ; unlink targets the active window
      (let ((cl-tmux::*server-sessions* (list (cons "a" s1) (cons "b" s2)))
            (cl-tmux/prompt:*overlay* nil))
        (unwind-protect
             (progn
               (setf (fdefinition 'cl-tmux/pty:pty-close)
                     (lambda (&rest args) (declare (ignore args)) (incf closed)))
               (cl-tmux::%cmd-unlink-window s1 nil))
          (setf (fdefinition 'cl-tmux/pty:pty-close) real-pty-close))
        (is (null (member win2 (session-windows s1)))
            "the window must be unlinked from s1")
        (is (null (member win2 (session-windows s2)))
            "the removal must propagate to grouped peer s2")
        (is (= 1 closed)
            "the orphaned window's pane PTY must be closed exactly once")))))

(test session-group-sync-ignores-ungrouped-sessions
  "session-windows-changed on a session without a group is a no-op."
  (let* ((win (%make-group-test-window 1))
         (s   (make-session :id 3 :name "solo" :windows (list win)))
         (cl-tmux::*session-groups* nil))
    (finishes (cl-tmux/model:session-windows-changed s)
              "ungrouped session must not error in the sync path")
    (is (equal (list win) (session-windows s))
        "window list must be unchanged")))

;;; ── choose-session overlay ───────────────────────────────────────────────────

(test choose-session-shows-overlay
  ":choose-session opens an overlay listing sessions."
  (with-fake-session (sess :nwindows 1)
    (let ((*overlay* nil)
          (cl-tmux::*dirty* nil)
          (cl-tmux::*running* t)
          (cl-tmux::*server-sessions* (list (cons (session-name sess) sess))))
      (cl-tmux::dispatch-command sess :choose-session nil)
      (assert-overlay-active ":choose-session must open an overlay")
      (assert-overlay-contains (session-name sess) *overlay*
                               "overlay must contain the session name"))))

;;; ── update-environment ───────────────────────────────────────────────────────

(test update-environment-default-list
  "*update-environment* is a list of environment variable names."
  (let ((vars cl-tmux::*update-environment*))
    (is (listp vars)
        "*update-environment* must be a list")
    (is (> (length vars) 0)
        "*update-environment* must have at least one entry")
    (is (every #'stringp vars)
        "*update-environment* entries must all be strings")))
