(in-package #:cl-tmux/test)

;;;; Renderer overlay, cursor visibility, and message placement tests.

(in-suite renderer-suite)

;;; ── render-overlay ───────────────────────────────────────────────────────────

(test render-session-draws-overlay
  "When an overlay is active, its text appears in the composed frame."
  (let ((*overlay* nil))
    (show-overlay "OVERLAY-HELP-LINE")
    (unwind-protect
         (let ((out (render-session-to-string
                     (make-renderer-test-session 30 6 :content "hi") 7 30)))
           (is (search "OVERLAY-HELP-LINE" out)
               "overlay text should be composited into the frame"))
      (clear-overlay))))

(test render-overlay-draws-overlay-lines
  "render-overlay writes each overlay line at the top of the screen."
  (let ((*overlay* nil))
    (show-overlay (format nil "line one~%line two"))
    (unwind-protect
         (let ((buf (make-string-output-stream)))
           (cl-tmux/renderer::render-overlay buf 20 10)
           (let ((out (get-output-stream-string buf)))
             (is (search "line" out) "overlay text must appear in output")))
      (clear-overlay))))

(test message-style-applied-to-overlay
  "render-overlay reads the message-style option for its SGR colour."
  (with-isolated-options ("message-style" "fg=white,bg=blue,bold")
    (let ((eff (cl-tmux/options:get-option "message-style" "")))
      (is (search "bold" eff)     "message-style bold preserved (got ~S)" eff)
      (is (search "fg=white" eff) "fg=white in message-style (got ~S)" eff)
      (is (search "bg=blue" eff)  "bg=blue in message-style (got ~S)" eff))))

(test render-overlay-wires-message-style
  "render-overlay applies the message-style option: the rendered overlay SGR
   differs from the unstyled overlay."
  (let ((styled   (with-isolated-options ("message-style" "bg=red")
                    (let ((*overlay* nil))
                      (show-overlay "hello")
                      (unwind-protect
                           (render-overlay-output 20 10)
                        (clear-overlay)))))
        (unstyled (with-isolated-options ("message-style" "")
                    (let ((*overlay* nil))
                      (show-overlay "hello")
                      (unwind-protect
                           (render-overlay-output 20 10)
                        (clear-overlay))))))
    (is (not (string= styled unstyled))
        "message-style bg=red must change the overlay's rendered SGR (styled=~S unstyled=~S)"
        styled unstyled)))

;;; ── DECTCEM cursor-visibility in rendered output ────────────────────────────

(test render-session-hides-cursor-when-dectcem-off
  "When the active pane has screen-cursor-visible=NIL, ?25h must NOT appear in the frame."
  (let* ((sess (make-renderer-test-session 30 5))
         (ap   (session-active-pane sess))
         (screen (pane-screen ap)))
    (setf (cl-tmux/terminal/types:screen-cursor-visible screen) nil)
    (let ((out (render-session-to-string sess 6 30)))
      (is (search (format nil "~C[?25l" #\Escape) out)
          "?25l must be emitted (cursor hidden at start)")
      (is-false (search (format nil "~C[?25h" #\Escape) out)
                "?25h must NOT be emitted when screen-cursor-visible is NIL"))))

(test render-session-shows-cursor-when-dectcem-on
  "When screen-cursor-visible=T (default), ?25h appears in the frame."
  (let* ((sess (make-renderer-test-session 30 5))
         (ap   (session-active-pane sess))
         (screen (pane-screen ap)))
    (setf (cl-tmux/terminal/types:screen-cursor-visible screen) t)
    (let ((out (render-session-to-string sess 6 30)))
      (is (search (format nil "~C[?25h" #\Escape) out)
          "?25h must appear when screen-cursor-visible is T"))))

(test render-session-no-active-pane-shows-cursor
  "With no active pane (nil ap), the renderer always emits ?25h."
  (let* ((win  (make-window :id 1 :name "1" :width 20 :height 5 :panes nil))
         (sess (make-session :id 1 :name "0" :windows (list win))))
    (session-select-window sess win)
    (let ((out (render-session-to-string sess 6 20)))
      (is (search (format nil "~C[?25h" #\Escape) out)
          "?25h must be emitted when active pane is nil"))))

;;; ── Message placement (message-line / status-position) ───────────────────────

(test single-line-message-drawn-on-status-row
  "A single-line overlay is a MESSAGE: with terminal rows supplied it is drawn
   over the status area (bottom by default; message-line picks the row within a
   multi-line status bar; status-position top moves it to the top).  Multi-line
   overlays are pagers and stay top-anchored."
  ;; Each row: (status-height message-line status-position expected-row desc)
  ;; expected-row is 0-based; move-to emits 1-based ESC[row+1;1H.
  (dolist (row '((1 0 "bottom" 9 "default: last row of a 10-row terminal")
                 (2 0 "bottom" 8 "2-line status: message-line 0 = first status row")
                 (2 1 "bottom" 9 "2-line status: message-line 1 = second status row")
                 (1 0 "top"    0 "status-position top: message at row 0")))
    (destructuring-bind (height line position expected desc) row
      (with-isolated-config
        (let ((cl-tmux/config:*status-height* height)
              (cl-tmux/prompt:*overlay* "hello"))
          (cl-tmux/options:set-option "message-line" line)
          (cl-tmux/options:set-option "status-position" position)
          (let ((out (with-output-to-string (buf)
                       (cl-tmux/renderer::render-overlay buf 20 10))))
            (is (search (format nil "[~D;1H" (1+ expected)) out)
                "~A (expect row ~D): ~S" desc expected out)
            (is (search "hello" out) "the message text must be drawn")))))))

(test multi-line-overlay-stays-top-anchored
  "A multi-line overlay (pager) draws from row 0 even when rows are supplied."
  (with-isolated-config
    (let ((cl-tmux/prompt:*overlay* (format nil "line1~%line2~%line3")))
      (let ((out (with-output-to-string (buf)
                   (cl-tmux/renderer::render-overlay buf 20 10))))
        (is (search "[1;1H" out) "the pager must start at the top row")
        (is (search "line3" out) "all pager lines must be drawn")))))
