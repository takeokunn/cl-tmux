(in-package #:cl-tmux/test)

;;;; Dispatch tests - select-window target resolution.

(in-suite dispatch-suite)

(test run-command-line-select-window-by-number
  "'select-window -t N' selects the window whose window-id is N."
  (with-fake-session (s :nwindows 3)
    (cl-tmux::%run-command-line s "select-window -t 2")
    (is (= 2 (window-id (session-active-window s)))
        "select-window -t 2 must activate window-id 2")))

(test run-command-line-select-window-by-name
  "'select-window -t <name>' selects the window with that (non-numeric) name."
  (with-fake-session (s :nwindows 2)
    (setf (window-name (second (session-windows s))) "alpha")
    (cl-tmux::%run-command-line s "select-window -t alpha")
    (is (string= "alpha" (window-name (session-active-window s)))
        "select-window -t alpha must activate the window named 'alpha'")))

(test run-command-line-select-window-T-toggles-to-last
  "'select-window -T -t N' toggles to the last window when already on window N,
   but selects N normally when not currently on it."
  (with-fake-session (s :nwindows 2)
    (let* ((w0 (first  (cl-tmux/model:session-windows s)))
           (w1 (second (cl-tmux/model:session-windows s))))
      ;; session-last-window is recency-based (window-last-active-time, 1s
      ;; granularity); seed distinct OLD stamps so the two same-second selects
      ;; below produce an unambiguous last-window order (w0 older than w1's NOW).
      (setf (cl-tmux/model:window-last-active-time w0) 100
            (cl-tmux/model:window-last-active-time w1) 50)
      ;; From w0, -T -t 1 is NOT on the target → select w1 normally.
      (cl-tmux::%run-command-line s "select-window -T -t 1")
      (is (eq w1 (session-active-window s))
          "-T when not on the target must select the target (w1)")
      ;; Now on w1 (w0 is last); -T -t 1 IS on the target → toggle to last (w0).
      (cl-tmux::%run-command-line s "select-window -T -t 1")
      (is (eq w0 (session-active-window s))
          "-T when already on the target must toggle to the last window (w0)"))))

(test run-command-line-select-window-rejects-unsupported-arguments
  "select-window rejects unknown flags and positional tokens before changing windows."
  (dolist (command '("select-window -n extra"
                     "select-window -x"
                     "select-window -t 2 extra"))
    (with-fake-session (s :nwindows 2)
      (let ((initial (session-active-window s))
            (*overlay* nil))
        (is (null (cl-tmux::%run-command-line s command))
            "~A must be rejected" command)
        (is (eq initial (session-active-window s))
            "~A must not change the active window" command)
        (assert-overlay-contains "unsupported argument"
                                  (overlay-lines) command)))))
