(in-package #:cl-tmux/test)

;;;; Pane-level tests: pane struct, pane-feed, pane-reposition, next-pane-id.

(def-suite model-suite :description "Session / window / pane model")
(in-suite model-suite)

;;; ── pane-feed ────────────────────────────────────────────────────────────────

(test pane-feed-processes-bytes-into-screen
  "pane-feed feeds raw bytes through the screen emulator under the screen lock."
  (let* ((screen (make-screen 10 5))
         (pane   (make-pane :id 1 :x 0 :y 0 :width 10 :height 5
                            :fd -1 :pid -1 :screen screen)))
    (pane-feed pane (babel:string-to-octets "hi" :encoding :utf-8))
    (is (char= #\h (cell-char (screen-cell screen 0 0))))
    (is (char= #\i (cell-char (screen-cell screen 1 0))))
    (is (= 2 (screen-cursor-x screen)))))

;;; ── next-pane-id direct tests (pure, no PTY) ─────────────────────────────

(test next-pane-id-returns-one-for-empty-window
  "next-pane-id starts at 1 when the window has no panes."
  (let ((win (make-window :id 1 :name "w" :panes nil)))
    (is (= 1 (cl-tmux/model::next-pane-id win)))))

(test next-pane-id-fills-lowest-gap
  "next-pane-id returns the lowest positive id not already in use."
  (let* ((p1  (make-pane :id 1 :fd -1 :pid -1 :screen (make-screen 10 5)))
         (p3  (make-pane :id 3 :fd -1 :pid -1 :screen (make-screen 10 5)))
         (win (make-window :id 1 :name "w" :panes (list p1 p3))))
    ;; id 1 and 3 are used; 2 is the lowest gap
    (is (= 2 (cl-tmux/model::next-pane-id win)))))
