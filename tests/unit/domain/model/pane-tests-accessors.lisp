(in-package #:cl-tmux/test)

;;;; Pane tests - state, accessors, and defaults.

(in-suite model-suite)

;;; ── Pane slot defaults ───────────────────────────────────────────────────────

(test pane-nil-slot-defaults
  "pipe state, pane-window, and pane-marked all default to NIL for a fresh pane."
  (let ((pane (make-no-pty-pane 1 0 0 20 5)))
    (is (null (pane-pipe-fd pane)) "pane-pipe-fd must default to NIL")
    (is (null (pane-pipe-output-stream pane))
        "pane-pipe-output-stream must default to NIL")
    (is (null (pane-pipe-output-thread pane))
        "pane-pipe-output-thread must default to NIL")
    (is (null (pane-pipe-process pane)) "pane-pipe-process must default to NIL")
    (is (null (pane-window  pane)) "pane-window must default to NIL before attach")
    (is (null (pane-marked  pane)) "pane-marked must default to NIL")))

(test pane-marked-settable
  "pane-marked can be set to T and read back."
  (let ((pane (make-no-pty-pane 1 0 0 20 5)))
    (setf (pane-marked pane) t)
    (is-true (pane-marked pane)
             "pane-marked must return T after being set")))

;;; ── pane struct accessor defaults ───────────────────────────────────────────

(test pane-id-slot-accessible
  "pane-id returns the id passed to make-no-pty-pane."
  (let ((pane (make-no-pty-pane 7 0 0 20 5)))
    (is (= 7 (pane-id pane))
        "pane-id must return the id set at construction")))

(test pane-x-y-width-height-accessible
  "pane-x, pane-y, pane-width, pane-height return the geometry set at construction."
  (let ((pane (make-no-pty-pane 1 3 5 40 10)))
    (is (= 3  (pane-x      pane)) "pane-x must return 3")
    (is (= 5  (pane-y      pane)) "pane-y must return 5")
    (is (= 40 (pane-width  pane)) "pane-width must return 40")
    (is (= 10 (pane-height pane)) "pane-height must return 10")))

(test pane-no-pty-fd-and-pid-are-negative
  "make-no-pty-pane produces a pane with fd and pid both -1."
  (let ((pane (make-no-pty-pane 1 0 0 20 5)))
    (is (= -1 (pane-fd  pane)) "pane-fd must be -1 for a no-PTY pane")
    (is (= -1 (pane-pid pane)) "pane-pid must be -1 for a no-PTY pane")))

(test pane-screen-accessible
  "pane-screen returns the screen object set at construction."
  (let* ((screen (make-screen 20 5))
         (pane   (make-pane :id 1 :x 0 :y 0 :width 20 :height 5
                            :fd -1 :pid -1 :screen screen)))
    (is (eq screen (pane-screen pane))
        "pane-screen must return the exact screen object set at construction")))

;;; ── pane-feed with empty bytes ───────────────────────────────────────────────

(test pane-feed-empty-bytes-is-noop
  "pane-feed with an empty byte vector does not signal and leaves cursor at (0,0)."
  (let* ((screen (make-screen 10 5))
         (pane   (make-pane :id 1 :x 0 :y 0 :width 10 :height 5
                            :fd -1 :pid -1 :screen screen)))
    (finishes (pane-feed pane (make-array 0 :element-type '(unsigned-byte 8))))
    (is (= 0 (screen-cursor-x screen)) "cursor must stay at 0 after feeding empty bytes")
    (is (= 0 (screen-cursor-y screen)) "cursor must stay at 0 after feeding empty bytes")))

;;; ── pane-feed updates screen-dirty-p ────────────────────────────────────────

(test pane-feed-sets-dirty-flag
  "pane-feed marks the screen dirty after processing bytes."
  (let* ((screen (make-screen 10 5))
         (pane   (make-pane :id 1 :x 0 :y 0 :width 10 :height 5
                            :fd -1 :pid -1 :screen screen)))
    (screen-clear-dirty screen)
    (pane-feed pane (babel:string-to-octets "A" :encoding :utf-8))
    ;; After writing a character the dirty flag must be set.
    (is-true (cl-tmux/terminal/types:screen-dirty-p screen)
             "screen-dirty-p must be T after pane-feed writes a character")))
