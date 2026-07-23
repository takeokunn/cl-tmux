(in-package #:cl-tmux/test)

;;;; Layout undo and per-session winlink ordering dispatch cases.

(describe "dispatch-suite"

  ;; select-layout -o restores the layout tree saved before the last layout
  ;; application; a second -o redoes (swap semantics).
  (it "select-layout-o-undoes-last-layout-change"
    (with-two-pane-h-session (s win p0 p1)
      (with-command-test-state (s :overlay t)
        (let ((before-tree (cl-tmux/model:window-tree win)))
          (cl-tmux::%run-command-line s "select-layout even-vertical")
          (let ((after-tree (cl-tmux/model:window-tree win)))
            (expect (not (eq before-tree after-tree)))
            (cl-tmux::%run-command-line s "select-layout -o")
            (expect (eq before-tree (cl-tmux/model:window-tree win)))
            (cl-tmux::%run-command-line s "select-layout -o")
            (expect (eq after-tree (cl-tmux/model:window-tree win))))))))

  ;; link-window -t sess:N links a window at index N in the destination while
  ;; the source session keeps the window's own index; target resolution and
  ;; #{window_index} follow the per-session winlink index.
  (it "link-window-per-session-winlink-index"
    (with-fake-session (a)
      (with-fake-session (b)
        (setf (cl-tmux/model:session-name a) "wla"
              (cl-tmux/model:session-name b) "wlb")
        (let ((win (cl-tmux/model:session-active-window a))
              (cl-tmux::*server-sessions* nil))
          (push (cons "wla" a) cl-tmux::*server-sessions*)
          (push (cons "wlb" b) cl-tmux::*server-sessions*)
          (let ((*overlay* nil))
            (cl-tmux::%cmd-link-window a '("-t" "wlb:7")))
          (expect (member win (cl-tmux/model:session-windows b)))
          (expect (= 7 (cl-tmux/model:session-window-index b win)))
          (expect (= (cl-tmux/model:window-id win)
                     (cl-tmux/model:session-window-index a win)))
          (expect (eq win (cl-tmux::%resolve-window-target b "7")))
          (expect (string= "7" (cl-tmux/format:expand-format
                                "#{window_index}"
                                (cl-tmux/format:format-context-from-session b win nil))))
          ;; Unlinking prunes the override so a later re-link starts clean.
          (setf (cl-tmux/model:session-windows b)
                (remove win (cl-tmux/model:session-windows b)))
          (cl-tmux/model:session-windows-changed b)
          (expect (zerop (hash-table-count (cl-tmux/model:session-window-index-map b))))))))

  ;; Window display order (status bar / list-windows) follows the per-session
  ;; winlink indexes: a window linked at a high index sorts after lower ones,
  ;; while sessions without overrides keep id order.
  (it "status-window-order-follows-winlink-indexes"
    (with-fake-session (s :nwindows 2)
      (let* ((wins (cl-tmux/model:session-windows s))
             (w0 (first wins))
             (w1 (second wins)))
        (expect (equal (list w0 w1)
                       (cl-tmux/model:session-windows-in-index-order s)))
        ;; Give the FIRST window a high per-session index: it must sort last.
        (cl-tmux/model:set-session-window-index s w0 99)
        (expect (equal (list w1 w0)
                       (cl-tmux/model:session-windows-in-index-order s)))))))
