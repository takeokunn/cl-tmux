(in-package #:cl-tmux/test)

;;;; Direct unit tests for renderer-compose-effects.lisp's %render-passthrough
;;;; and %render-clipboard (and, transitively, the shared %drain-screen-queue
;;;; helper they're both built on). Existing tests cover the parser side that
;;;; populates screen-passthrough-queue/screen-clipboard-queue (parser-dcs-tests,
;;;; commands-tests-n) but nothing previously exercised the renderer-side drain
;;;; that reads those queues back out into the frame.

(describe "renderer-suite/compose-effects"

  ;; allow-passthrough off (the default) drains (clears) the queue without
  ;; writing its contents to the frame.
  (it "render-passthrough-off-drains-without-emitting"
    (with-isolated-options ("allow-passthrough" "off")
      (let* ((p (make-no-pty-pane 1 0 0 10 5))
             (s (cl-tmux/model:pane-screen p)))
        (push (format nil "~C]1337;a" #\Escape)
              (cl-tmux/terminal/types:screen-passthrough-queue s))
        (let ((out (with-output-to-string (buf)
                     (cl-tmux/renderer::%render-passthrough buf (list p)))))
          (expect (string= "" out))
          (expect (null (cl-tmux/terminal/types:screen-passthrough-queue s)))))))

  ;; allow-passthrough on emits queued sequences to the frame in FIFO
  ;; (oldest-first) order, then clears the queue.
  (it "render-passthrough-on-emits-in-fifo-order"
    (with-isolated-options ("allow-passthrough" "on")
      (let* ((p (make-no-pty-pane 1 0 0 10 5))
             (s (cl-tmux/model:pane-screen p)))
        ;; screen-passthrough-queue is push-accumulated (most-recent-first), so
        ;; pushing "first" before "second" means "first" was queued earliest.
        (push "first" (cl-tmux/terminal/types:screen-passthrough-queue s))
        (push "second" (cl-tmux/terminal/types:screen-passthrough-queue s))
        (let ((out (with-output-to-string (buf)
                     (cl-tmux/renderer::%render-passthrough buf (list p)))))
          (expect (string= "firstsecond" out))
          (expect (null (cl-tmux/terminal/types:screen-passthrough-queue s)))))))

  ;; allow-passthrough "all" (the other canonical emitting value) also emits.
  (it "render-passthrough-all-emits"
    (with-isolated-options ("allow-passthrough" "all")
      (let* ((p (make-no-pty-pane 1 0 0 10 5))
             (s (cl-tmux/model:pane-screen p)))
        (push "seq" (cl-tmux/terminal/types:screen-passthrough-queue s))
        (let ((out (with-output-to-string (buf)
                     (cl-tmux/renderer::%render-passthrough buf (list p)))))
          (expect (string= "seq" out))))))

  ;; set-clipboard off drains without emitting the OSC 52 payload.
  (it "render-clipboard-off-drains-without-emitting"
    (with-isolated-options ("set-clipboard" "off")
      (let* ((p (make-no-pty-pane 1 0 0 10 5))
             (s (cl-tmux/model:pane-screen p)))
        (push (format nil "~C]52;c;aGVsbG8=~C\\" #\Escape #\Escape)
              (cl-tmux/terminal/types:screen-clipboard-queue s))
        (let ((out (with-output-to-string (buf)
                     (cl-tmux/renderer::%render-clipboard buf (list p)))))
          (expect (string= "" out))
          (expect (null (cl-tmux/terminal/types:screen-clipboard-queue s)))))))

  ;; set-clipboard on (the tmux default) emits the queued OSC 52 sequence.
  (it "render-clipboard-on-emits"
    (with-isolated-options ("set-clipboard" "on")
      (let* ((p (make-no-pty-pane 1 0 0 10 5))
             (s (cl-tmux/model:pane-screen p)))
        (push "osc52-payload" (cl-tmux/terminal/types:screen-clipboard-queue s))
        (let ((out (with-output-to-string (buf)
                     (cl-tmux/renderer::%render-clipboard buf (list p)))))
          (expect (string= "osc52-payload" out))))))

  ;; set-clipboard external (the other canonical emitting value) also emits.
  (it "render-clipboard-external-emits"
    (with-isolated-options ("set-clipboard" "external")
      (let* ((p (make-no-pty-pane 1 0 0 10 5))
             (s (cl-tmux/model:pane-screen p)))
        (push "osc52-payload" (cl-tmux/terminal/types:screen-clipboard-queue s))
        (let ((out (with-output-to-string (buf)
                     (cl-tmux/renderer::%render-clipboard buf (list p)))))
          (expect (string= "osc52-payload" out))))))

  ;; Multiple panes are drained independently and concatenated in pane order.
  (it "render-passthrough-multiple-panes-concatenates-in-order"
    (with-isolated-options ("allow-passthrough" "on")
      (let* ((p1 (make-no-pty-pane 1 0 0 10 5))
             (p2 (make-no-pty-pane 2 0 0 10 5))
             (s1 (cl-tmux/model:pane-screen p1))
             (s2 (cl-tmux/model:pane-screen p2)))
        (push "from-p1" (cl-tmux/terminal/types:screen-passthrough-queue s1))
        (push "from-p2" (cl-tmux/terminal/types:screen-passthrough-queue s2))
        (let ((out (with-output-to-string (buf)
                     (cl-tmux/renderer::%render-passthrough buf (list p1 p2)))))
          (expect (string= "from-p1from-p2" out)))))))
