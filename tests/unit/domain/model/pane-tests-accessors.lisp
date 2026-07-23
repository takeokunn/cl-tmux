(in-package #:cl-tmux/test)

;;;; Pane tests - state, accessors, and defaults.

(describe "model-suite"

  ;;; ── Pane slot defaults ───────────────────────────────────────────────────────

  ;; pipe state, pane-window, and pane-marked all default to NIL for a fresh pane.
  (it "pane-nil-slot-defaults"
    (let ((pane (make-no-pty-pane 1 0 0 20 5)))
      (expect (null (pane-pipe-fd pane)))
      (expect (null (pane-pipe-output-stream pane)))
      (expect (null (pane-pipe-output-thread pane)))
      (expect (null (pane-pipe-process pane)))
      (expect (null (pane-window  pane)))
      (expect (null (pane-marked  pane)))))

  ;; pane-marked can be set to T and read back.
  (it "pane-marked-settable"
    (let ((pane (make-no-pty-pane 1 0 0 20 5)))
      (setf (pane-marked pane) t)
      (expect (pane-marked pane) :to-be-truthy)))

  ;;; ── pane struct accessor defaults ───────────────────────────────────────────

  ;; pane-id returns the id passed to make-no-pty-pane.
  (it "pane-id-slot-accessible"
    (let ((pane (make-no-pty-pane 7 0 0 20 5)))
      (expect (= 7 (pane-id pane)))))

  ;; pane-x, pane-y, pane-width, pane-height return the geometry set at construction.
  (it "pane-x-y-width-height-accessible"
    (let ((pane (make-no-pty-pane 1 3 5 40 10)))
      (expect (= 3  (pane-x      pane)))
      (expect (= 5  (pane-y      pane)))
      (expect (= 40 (pane-width  pane)))
      (expect (= 10 (pane-height pane)))))

  ;; make-no-pty-pane produces a pane with fd and pid both -1.
  (it "pane-no-pty-fd-and-pid-are-negative"
    (let ((pane (make-no-pty-pane 1 0 0 20 5)))
      (expect (= -1 (pane-fd  pane)))
      (expect (= -1 (pane-pid pane)))))

  ;; pane-screen returns the screen object set at construction.
  (it "pane-screen-accessible"
    (let* ((screen (make-screen 20 5))
           (pane   (make-pane :id 1 :x 0 :y 0 :width 20 :height 5
                              :fd -1 :pid -1 :screen screen)))
      (expect (eq screen (pane-screen pane)))))

  ;;; ── pane-feed with empty bytes ───────────────────────────────────────────────

  ;; pane-feed with an empty byte vector does not signal and leaves cursor at (0,0).
  (it "pane-feed-empty-bytes-is-noop"
    (let* ((screen (make-screen 10 5))
           (pane   (make-pane :id 1 :x 0 :y 0 :width 10 :height 5
                              :fd -1 :pid -1 :screen screen)))
      (finishes (pane-feed pane (make-array 0 :element-type '(unsigned-byte 8))))
      (expect (= 0 (screen-cursor-x screen)))
      (expect (= 0 (screen-cursor-y screen)))))

  ;;; ── pane-feed updates screen-dirty-p ────────────────────────────────────────

  ;; pane-feed marks the screen dirty after processing bytes.
  (it "pane-feed-sets-dirty-flag"
    (let* ((screen (make-screen 10 5))
           (pane   (make-pane :id 1 :x 0 :y 0 :width 10 :height 5
                              :fd -1 :pid -1 :screen screen)))
      (screen-clear-dirty screen)
      (pane-feed pane (babel:string-to-octets "A" :encoding :utf-8))
      ;; After writing a character the dirty flag must be set.
      (expect (cl-tmux/terminal/types:screen-dirty-p screen) :to-be-truthy)))

  ;;; ── %drain-response-queue writes replies back to a real PTY fd ──────────────

  ;; %drain-response-queue's write-back branch (pane-fd > 0) was previously only
  ;; reachable with a synthetic :fd -1 pane, so the actual write-pty call was
  ;; never exercised. A real pipe fd stands in for a PTY master fd here.
  (it "drain-response-queue-writes-queued-reply-to-real-fd"
    (with-pipe-fds (read-fd write-fd)
      (let* ((screen (make-screen 10 5))
             (pane   (make-pane :id 1 :x 0 :y 0 :width 10 :height 5
                                :fd write-fd :pid -1 :screen screen))
             (reply  (format nil "~C[?1;2c" #\Escape)))
        (setf (cl-tmux/terminal/types:screen-response-queue screen) (list reply))
        (cl-tmux/model::%drain-response-queue pane screen)
        (expect (null (cl-tmux/terminal/types:screen-response-queue screen)))
        (expect (equalp (babel:string-to-octets reply :encoding :utf-8)
                        (pty-read-blocking read-fd 32))))))

  ;; With a synthetic (no-PTY) pane, %drain-response-queue still clears the
  ;; queue (nothing accumulates unboundedly) but performs no write.
  (it "drain-response-queue-clears-queue-without-writing-when-no-pty"
    (let* ((screen (make-screen 10 5))
           (pane   (make-pane :id 1 :x 0 :y 0 :width 10 :height 5
                              :fd -1 :pid -1 :screen screen)))
      (setf (cl-tmux/terminal/types:screen-response-queue screen)
            (list (format nil "~C[?1;2c" #\Escape)))
      (finishes (cl-tmux/model::%drain-response-queue pane screen))
      (expect (null (cl-tmux/terminal/types:screen-response-queue screen))))))
