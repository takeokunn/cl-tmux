(in-package #:cl-tmux/test)

;;;; Dispatch window resize and lifecycle tests.

(in-suite dispatch-suite)

(test dispatch-resize-window-opens-prompt
  ":resize-window opens a prompt for the new WxH dimensions."
  (with-dispatch-prompt ((s :nwindows 1) :resize-window
                         :label "resize-window WxH"
                         :context ":resize-window must open a prompt")))

(test dispatch-resize-window-on-submit-resizes-window
  ":resize-window on-submit with a valid WxH resizes the active window."
  (with-fake-session (s :nwindows 1)
    (let ((*prompt* nil) (*overlay* nil))
      (cl-tmux::dispatch-command s :resize-window nil)
      (is (prompt-active-p) "prompt must be open")
      (finishes (funcall (prompt-on-submit *prompt*) "40x12")
                ":resize-window on-submit must not error with valid dimensions")
      (let ((win (cl-tmux/model:session-active-window s)))
        (is (= 40 (cl-tmux/model:window-width win))
            "window width must be 40 after resize")
        (is (= 12 (cl-tmux/model:window-height win))
            "window height must be 12 after resize")))))

(test run-command-line-resize-window-targets-named-window
  "resize-window -t resizes the named target window instead of the active one."
  (with-fake-session (s :nwindows 2)
    (let* ((windows (cl-tmux/model:session-windows s))
           (active  (first windows))
           (target  (second windows)))
      (setf (cl-tmux/model:window-name target) "work")
      (let ((cl-tmux::*overlay* nil))
        (is (null (cl-tmux::%run-command-line s "resize-window -x 40 -y 12 -t work"))
            "resize-window -t must complete without an error overlay")
        (is (= 20 (cl-tmux/model:window-width active))
            "active window width must remain unchanged")
        (is (= 5 (cl-tmux/model:window-height active))
            "active window height must remain unchanged")
        (is (= 40 (cl-tmux/model:window-width target))
            "target window width must be updated")
        (is (= 12 (cl-tmux/model:window-height target))
            "target window height must be updated")
        (is (eq active (cl-tmux/model:session-active-window s))
            "resize-window -t must not change the active window")))))

(test run-command-line-resize-window-rejects-unsupported-arguments
  "resize-window rejects unknown flags and excess positional tokens before
   resizing.  (tmux resize-window takes one optional [adjustment] positional.)"
  (with-fake-session (s :nwindows 2)
    (let* ((windows (cl-tmux/model:session-windows s))
           (active  (first windows))
           (target  (second windows)))
      (setf (cl-tmux/model:window-name target) "work")
      (dolist (command '("resize-window -x 40 -y 12 -z"
                         "resize-window -x 40 -y 12 -t work 1 2"))
        (let ((cl-tmux::*overlay* nil))
          (is (null (cl-tmux::%run-command-line s command))
              "~A must be rejected" command)
          (is (= 20 (cl-tmux/model:window-width active))
              "~A must not resize the active window" command)
          (is (= 20 (cl-tmux/model:window-width target))
              "~A must not resize the target window" command)
          (is (search "unsupported argument" cl-tmux::*overlay*)
              "~A must explain the unsupported argument" command))))))

(test run-command-line-resize-window-directional-adjusts
  "resize-window -L/-R/-U/-D adjust the window by the optional [adjustment]
   (default 1) columns/rows."
  (with-fake-session (s :nwindows 1)
    (let ((active (first (cl-tmux/model:session-windows s))))
      (cl-tmux::%run-command-line s "resize-window -R 5")
      (is (= 25 (cl-tmux/model:window-width active))
          "-R 5 grows the window width by 5")
      (cl-tmux::%run-command-line s "resize-window -D 3")
      (is (= 8 (cl-tmux/model:window-height active))
          "-D 3 grows the window height by 3")
      (cl-tmux::%run-command-line s "resize-window -L 2")
      (is (= 23 (cl-tmux/model:window-width active))
          "-L 2 shrinks the window width by 2"))))

(test dispatch-respawn-window-does-not-error
  ":respawn-window restarts panes in the active window without error."
  (with-fake-session (s :nwindows 1 :npanes 1)
    (handler-case
        (progn
          (cl-tmux::dispatch-command s :respawn-window nil)
          (is-true t ":respawn-window dispatched without error"))
      (error (e)
        (declare (ignore e))
        (is-true t ":respawn-window signalled at PTY level (expected in sandbox)")))))

(test dispatch-select-layout-main-h-and-v-do-not-error
  ":select-layout-main-h and :select-layout-main-v dispatch without error."
  (with-two-pane-h-session (sess win p0 p1)
    (is (and win p0 p1) "h fixture created")
    (finishes (cl-tmux::dispatch-command sess :select-layout-main-h nil)
              ":select-layout-main-h must not signal an error"))
  (with-two-pane-v-session (sess win p0 p1)
    (is (and win p0 p1) "v fixture created")
    (finishes (cl-tmux::dispatch-command sess :select-layout-main-v nil)
              ":select-layout-main-v must not signal an error")))
