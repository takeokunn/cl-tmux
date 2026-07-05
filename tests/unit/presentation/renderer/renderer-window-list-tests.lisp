(in-package #:cl-tmux/test)

;;;; status window-list formatting and style expansion

(in-suite renderer-suite)

;;; These tests call %status-window-list-styled with empty style options so the
;;; same window-list behaviour is exercised through the live path.

(test status-window-list-brackets-active-window
  "Active window appears with the * marker in the window list."
  (with-isolated-options ("window-status-current-style" ""
                          "window-status-style" "")
    (let* ((sess (make-renderer-test-session 20 5 :content ""))
           (win  (session-active-window sess))
           (out  (cl-tmux/renderer::%status-window-list-styled sess win)))
      (is (search "1:1" out)
          "%status-window-list-styled should contain the active window 1:1 (got ~S)" out)
      (is (search "*" out)
          "%status-window-list-styled should contain * marker for active window (got ~S)" out))))

(test status-window-list-two-windows-formats-both
  "Both active and inactive windows appear with correct format strings."
  (with-isolated-options ("window-status-current-style" ""
                          "window-status-style" "")
    (let* ((s0   (make-screen 10 5))
           (p0   (make-pane :id 1 :x 0 :y 0 :width 10 :height 5 :fd -1 :screen s0))
           (w0   (make-window :id 1 :name "alpha" :width 10 :height 5 :panes (list p0)))
           (s1   (make-screen 10 5))
           (p1   (make-pane :id 2 :x 0 :y 0 :width 10 :height 5 :fd -1 :screen s1))
           (w1   (make-window :id 2 :name "beta"  :width 10 :height 5 :panes (list p1)))
           (sess (make-session :id 1 :name "0" :windows (list w0 w1))))
      (window-select-pane w0 p0)
      (window-select-pane w1 p1)
      (session-select-window sess w1)
      (let ((out (cl-tmux/renderer::%status-window-list-styled sess w1)))
        (is (search "beta*" out)
            "%status-window-list-styled should mark active window beta with * (got ~S)" out)
        (is (search "alpha" out)
            "%status-window-list-styled should include the inactive window alpha (got ~S)" out)
        (is (null (search "alpha*" out))
            "%status-window-list-styled must NOT mark inactive window alpha with * (got ~S)" out)))))

(test status-window-list-styled-active-gets-sgr
  "When window-status-current-style is set, %status-window-list-styled wraps
   the active window label in the configured SGR codes."
  (with-isolated-options ("window-status-current-style" "bold"
                          "window-status-style" "")
    (let* ((sess (make-renderer-test-session 20 5 :content ""))
           (win  (session-active-window sess))
           (out  (cl-tmux/renderer::%status-window-list-styled sess win)))
      (is (search "1:1" out)
          "%status-window-list-styled must include the window label 1:1 (got ~S)" out)
      (is (search (format nil "~C[0m" #\Escape) out)
          "%status-window-list-styled must emit SGR reset (got ~S)" out))))

(test window-status-current-style-applied-directly
  "%window-status-style returns the window-status-current-style option directly
   for the active window."
  (with-isolated-options ("window-status-current-style" "bg=red")
    (let* ((sess  (make-renderer-test-session 20 5 :content ""))
           (win   (session-active-window sess))
           (style (cl-tmux/renderer::%window-status-style sess win t)))
      (is (search "bg=red" style)
          "active window style must be bg=red (got ~S)" style))))

(test window-status-style-applied-directly
  "%window-status-style returns the window-status-style option directly
   for a non-active window."
  (with-isolated-options ("window-status-style" "fg=green")
    (let* ((sess  (make-renderer-test-session 20 5 :content ""))
           (win   (session-active-window sess))
           (style (cl-tmux/renderer::%window-status-style sess win nil)))
      (is (search "fg=green" style)
          "non-active window style must be fg=green (got ~S)" style))))

(test status-window-list-styled-no-style-no-sgr
  "When both style options are empty, %status-window-list-styled emits plain
   labels with no SGR wrapping."
  (with-isolated-options ("window-status-current-style" ""
                          "window-status-style" "")
    (let* ((sess (make-renderer-test-session 20 5 :content ""))
           (win  (session-active-window sess))
           (out  (cl-tmux/renderer::%status-window-list-styled sess win)))
      (is (search "1:1" out)
          "%status-window-list-styled must include the window label 1:1 (got ~S)" out)
      (is (null (search (format nil "~C[" #\Escape) out))
          "%status-window-list-styled must NOT emit SGR when styles are empty (got ~S)" out))))

(test status-window-list-inline-style-block-in-current-format
  "Inline #[fg=red] in window-status-current-format expands to real SGR in the
   window list, even when the per-window style option is empty."
  (with-isolated-options ("window-status-current-style" ""
                          "window-status-style" ""
                          "window-status-current-format" "#[fg=red]#{window_name}#[default]")
    (let* ((sess (make-renderer-test-session 20 5 :content ""))
           (win  (session-active-window sess))
           (out  (cl-tmux/renderer::%status-window-list-styled sess win)))
      (is (search (format nil "~C[31m" #\Escape) out)
          "inline #[fg=red] must emit SGR 31 in the window list (got ~S)" out)
      (is (null (search "#[" out))
          "no literal #[ block may survive into the window list (got ~S)" out)
      (is (search "1" out)
          "the window name must still be present (got ~S)" out)
      (is (search (format nil "~C[0" #\Escape) out)
          "a reset must close the inline style (got ~S)" out))))

(test status-window-list-inline-block-without-window-style-still-resets
  "A window label that injects SGR via #[...] is reset afterwards even when the
   window has no style option set (so the next window/separator is unstyled)."
  (with-isolated-options ("window-status-current-style" ""
                          "window-status-style" ""
                          "window-status-current-format" "#[fg=green]#{window_name}")
    (let* ((sess (make-renderer-test-session 20 5 :content ""))
           (win  (session-active-window sess))
           (out  (cl-tmux/renderer::%status-window-list-styled sess win)))
      (is (search (format nil "~C[32m" #\Escape) out)
          "inline #[fg=green] must emit SGR 32 (got ~S)" out)
      (is (search (format nil "~C[0m" #\Escape) out)
          "the injected style must be reset after the label (got ~S)" out))))

(test status-window-list-plain-format-unchanged-by-expansion
  "A window-status-current-format with no #[ block and no style option produces
   exactly the same plain label as before (no spurious SGR)."
  (with-isolated-options ("window-status-current-style" ""
                          "window-status-style" ""
                          "window-status-current-format" " #{window_index}:#{window_name} ")
    (let* ((sess (make-renderer-test-session 20 5 :content ""))
           (win  (session-active-window sess))
           (out  (cl-tmux/renderer::%status-window-list-styled sess win)))
      (is (search "1:1" out)
          "plain window label must be present (got ~S)" out)
      (is (null (search (format nil "~C[" #\Escape) out))
          "plain format with empty styles must emit NO SGR (got ~S)" out))))
