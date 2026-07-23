(in-package #:cl-tmux/test)

;;;; Renderer overlay, cursor visibility, and message placement tests.

(describe "renderer-suite"

  ;;; ── render-overlay ───────────────────────────────────────────────────────────

  ;; When an overlay is active, its text appears in the composed frame.
  (it "render-session-draws-overlay"
    (let ((*overlay* nil))
      (show-overlay "OVERLAY-HELP-LINE")
      (unwind-protect
           (let ((out (render-session-to-string
                       (make-renderer-test-session 30 6 :content "hi") 7 30)))
             (expect (search "OVERLAY-HELP-LINE" out)))
        (clear-overlay))))

  ;; render-overlay writes each overlay line at the top of the screen.
  (it "render-overlay-draws-overlay-lines"
    (let ((*overlay* nil))
      (show-overlay (format nil "line one~%line two"))
      (unwind-protect
           (let ((buf (make-string-output-stream)))
             (cl-tmux/renderer::render-overlay buf 20 10)
             (let ((out (get-output-stream-string buf)))
               (expect (search "line" out))))
        (clear-overlay))))

  ;; render-overlay reads the message-style option for its SGR colour.
  (it "message-style-applied-to-overlay"
    (with-isolated-options ("message-style" "fg=white,bg=blue,bold")
      (let ((eff (cl-tmux/options:get-option "message-style" "")))
        (expect (search "bold" eff))
        (expect (search "fg=white" eff))
        (expect (search "bg=blue" eff)))))

  ;; render-overlay applies the message-style option: the rendered overlay SGR
  ;; differs from the unstyled overlay.
  (it "render-overlay-wires-message-style"
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
      (expect (not (string= styled unstyled)))))

  ;;; ── DECTCEM cursor-visibility in rendered output ────────────────────────────

  ;; When the active pane has screen-cursor-visible=NIL, ?25h must NOT appear in the frame.
  (it "render-session-hides-cursor-when-dectcem-off"
    (let* ((sess (make-renderer-test-session 30 5))
           (ap   (session-active-pane sess))
           (screen (pane-screen ap)))
      (setf (cl-tmux/terminal/types:screen-cursor-visible screen) nil)
      (let ((out (render-session-to-string sess 6 30)))
        (expect (search (format nil "~C[?25l" #\Escape) out))
        (expect (search (format nil "~C[?25h" #\Escape) out) :to-be-falsy))))

  ;; When screen-cursor-visible=T (default), ?25h appears in the frame.
  (it "render-session-shows-cursor-when-dectcem-on"
    (let* ((sess (make-renderer-test-session 30 5))
           (ap   (session-active-pane sess))
           (screen (pane-screen ap)))
      (setf (cl-tmux/terminal/types:screen-cursor-visible screen) t)
      (let ((out (render-session-to-string sess 6 30)))
        (expect (search (format nil "~C[?25h" #\Escape) out)))))

  ;; With no active pane (nil ap), the renderer always emits ?25h.
  (it "render-session-no-active-pane-shows-cursor"
    (let* ((win  (make-window :id 1 :name "1" :width 20 :height 5 :panes nil))
           (sess (make-session :id 1 :name "0" :windows (list win))))
      (session-select-window sess win)
      (let ((out (render-session-to-string sess 6 20)))
        (expect (search (format nil "~C[?25h" #\Escape) out)))))

  ;;; ── Message placement (message-line / status-position) ───────────────────────

  ;; A single-line overlay is a MESSAGE: with terminal rows supplied it is drawn
  ;; over the status area (bottom by default; message-line picks the row within a
  ;; multi-line status bar; status-position top moves it to the top).  Multi-line
  ;; overlays are pagers and stay top-anchored.
  (it "single-line-message-drawn-on-status-row"
    ;; Each row: (status-height message-line status-position expected-row desc)
    ;; expected-row is 0-based; move-to emits 1-based ESC[row+1;1H.
    (dolist (row '((1 0 "bottom" 9 "default: last row of a 10-row terminal")
                   (2 0 "bottom" 8 "2-line status: message-line 0 = first status row")
                   (2 1 "bottom" 9 "2-line status: message-line 1 = second status row")
                   (1 0 "top"    0 "status-position top: message at row 0")))
      (destructuring-bind (height line position expected desc) row
        (declare (ignore desc))
        (with-isolated-config
          (let ((cl-tmux/config:*status-height* height)
                (cl-tmux/prompt:*overlay* "hello"))
            (cl-tmux/options:set-option "message-line" line)
            (cl-tmux/options:set-option "status-position" position)
            (let ((out (with-output-to-string (buf)
                         (cl-tmux/renderer::render-overlay buf 20 10))))
              (expect (search (format nil "[~D;1H" (1+ expected)) out))
              (expect (search "hello" out))))))))

  ;; A multi-line overlay (pager) draws from row 0 even when rows are supplied.
  (it "multi-line-overlay-stays-top-anchored"
    (with-isolated-config
      (let ((cl-tmux/prompt:*overlay* (format nil "line1~%line2~%line3")))
        (let ((out (with-output-to-string (buf)
                     (cl-tmux/renderer::render-overlay buf 20 10))))
          (expect (search "[1;1H" out))
          (expect (search "line3" out)))))))
