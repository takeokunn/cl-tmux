(in-package #:cl-tmux/test)

;;;; status window-list formatting and style expansion

(describe "renderer-suite"

  ;; These tests call %status-window-list-styled with empty style options so the
  ;; same window-list behaviour is exercised through the live path.

  ;; Active window appears with the * marker in the window list.
  (it "status-window-list-brackets-active-window"
    (with-isolated-options ("window-status-current-style" ""
                            "window-status-style" "")
      (let* ((sess (make-renderer-test-session 20 5 :content ""))
             (win  (session-active-window sess))
             (out  (cl-tmux/renderer::%status-window-list-styled sess win)))
        (expect (search "1:1" out))
        (expect (search "*" out)))))

  ;; Both active and inactive windows appear with correct format strings.
  (it "status-window-list-two-windows-formats-both"
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
          (expect (search "beta*" out))
          (expect (search "alpha" out))
          (expect (null (search "alpha*" out)))))))

  ;; When window-status-current-style is set, %status-window-list-styled wraps
  ;; the active window label in the configured SGR codes.
  (it "status-window-list-styled-active-gets-sgr"
    (with-isolated-options ("window-status-current-style" "bold"
                            "window-status-style" "")
      (let* ((sess (make-renderer-test-session 20 5 :content ""))
             (win  (session-active-window sess))
             (out  (cl-tmux/renderer::%status-window-list-styled sess win)))
        (expect (search "1:1" out))
        (expect (search (format nil "~C[0m" #\Escape) out)))))

  ;; %window-status-style returns the window-status-current-style option directly
  ;; for the active window.
  (it "window-status-current-style-applied-directly"
    (with-isolated-options ("window-status-current-style" "bg=red")
      (let* ((sess  (make-renderer-test-session 20 5 :content ""))
             (win   (session-active-window sess))
             (style (cl-tmux/renderer::%window-status-style sess win t)))
        (expect (search "bg=red" style)))))

  ;; %window-status-style returns the window-status-style option directly
  ;; for a non-active window.
  (it "window-status-style-applied-directly"
    (with-isolated-options ("window-status-style" "fg=green")
      (let* ((sess  (make-renderer-test-session 20 5 :content ""))
             (win   (session-active-window sess))
             (style (cl-tmux/renderer::%window-status-style sess win nil)))
        (expect (search "fg=green" style)))))

  ;; When both style options are empty, %status-window-list-styled emits plain
  ;; labels with no SGR wrapping.
  (it "status-window-list-styled-no-style-no-sgr"
    (with-isolated-options ("window-status-current-style" ""
                            "window-status-style" "")
      (let* ((sess (make-renderer-test-session 20 5 :content ""))
             (win  (session-active-window sess))
             (out  (cl-tmux/renderer::%status-window-list-styled sess win)))
        (expect (search "1:1" out))
        (expect (null (search (format nil "~C[" #\Escape) out))))))

  ;; Inline #[fg=red] in window-status-current-format expands to real SGR in the
  ;; window list, even when the per-window style option is empty.
  (it "status-window-list-inline-style-block-in-current-format"
    (with-isolated-options ("window-status-current-style" ""
                            "window-status-style" ""
                            "window-status-current-format" "#[fg=red]#{window_name}#[default]")
      (let* ((sess (make-renderer-test-session 20 5 :content ""))
             (win  (session-active-window sess))
             (out  (cl-tmux/renderer::%status-window-list-styled sess win)))
        (expect (search (format nil "~C[31m" #\Escape) out))
        (expect (null (search "#[" out)))
        (expect (search "1" out))
        (expect (search (format nil "~C[0" #\Escape) out)))))

  ;; A window label that injects SGR via #[...] is reset afterwards even when the
  ;; window has no style option set (so the next window/separator is unstyled).
  (it "status-window-list-inline-block-without-window-style-still-resets"
    (with-isolated-options ("window-status-current-style" ""
                            "window-status-style" ""
                            "window-status-current-format" "#[fg=green]#{window_name}")
      (let* ((sess (make-renderer-test-session 20 5 :content ""))
             (win  (session-active-window sess))
             (out  (cl-tmux/renderer::%status-window-list-styled sess win)))
        (expect (search (format nil "~C[32m" #\Escape) out))
        (expect (search (format nil "~C[0m" #\Escape) out)))))

  ;; A window-status-current-format with no #[ block and no style option produces
  ;; exactly the same plain label as before (no spurious SGR).
  (it "status-window-list-plain-format-unchanged-by-expansion"
    (with-isolated-options ("window-status-current-style" ""
                            "window-status-style" ""
                            "window-status-current-format" " #{window_index}:#{window_name} ")
      (let* ((sess (make-renderer-test-session 20 5 :content ""))
             (win  (session-active-window sess))
             (out  (cl-tmux/renderer::%status-window-list-styled sess win)))
        (expect (search "1:1" out))
        (expect (null (search (format nil "~C[" #\Escape) out)))))))
