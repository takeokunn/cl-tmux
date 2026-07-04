(in-package #:cl-tmux/test)

;;;; Layout undo and per-session winlink ordering dispatch cases.

(in-suite dispatch-suite)

(test select-layout-o-undoes-last-layout-change
  "select-layout -o restores the layout tree saved before the last layout
   application; a second -o redoes (swap semantics)."
  (with-two-pane-h-session (s win p0 p1)
    (with-command-test-state (s :overlay t)
      (let ((before-tree (cl-tmux/model:window-tree win)))
        (cl-tmux::%run-command-line s "select-layout even-vertical")
        (let ((after-tree (cl-tmux/model:window-tree win)))
          (is (not (eq before-tree after-tree))
              "applying a named layout must install a new tree")
          (cl-tmux::%run-command-line s "select-layout -o")
          (is (eq before-tree (cl-tmux/model:window-tree win))
              "-o must restore the pre-change tree")
          (cl-tmux::%run-command-line s "select-layout -o")
          (is (eq after-tree (cl-tmux/model:window-tree win))
              "a second -o must redo (swap semantics)"))))))

(test link-window-per-session-winlink-index
  "link-window -t sess:N links a window at index N in the destination while
   the source session keeps the window's own index; target resolution and
   #{window_index} follow the per-session winlink index."
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
        (is (member win (cl-tmux/model:session-windows b))
            "the window must be linked into the destination")
        (is (= 7 (cl-tmux/model:session-window-index b win))
            "the destination must address it at winlink index 7")
        (is (= (cl-tmux/model:window-id win)
               (cl-tmux/model:session-window-index a win))
            "the source session must keep the window's own index")
        (is (eq win (cl-tmux::%resolve-window-target b "7"))
            "select-window -t 7 in the destination must resolve the link")
        (is (string= "7" (cl-tmux/format:expand-format
                          "#{window_index}"
                          (cl-tmux/format:format-context-from-session b win nil)))
            "#{window_index} must show the per-session index")
        ;; Unlinking prunes the override so a later re-link starts clean.
        (setf (cl-tmux/model:session-windows b)
              (remove win (cl-tmux/model:session-windows b)))
        (cl-tmux/model:session-windows-changed b)
        (is (zerop (hash-table-count (cl-tmux/model:session-window-index-map b)))
            "removing the window must prune its winlink override")))))

(test status-window-order-follows-winlink-indexes
  "Window display order (status bar / list-windows) follows the per-session
   winlink indexes: a window linked at a high index sorts after lower ones,
   while sessions without overrides keep id order."
  (with-fake-session (s :nwindows 2)
    (let* ((wins (cl-tmux/model:session-windows s))
           (w0 (first wins))
           (w1 (second wins)))
      (is (equal (list w0 w1)
                 (cl-tmux/model:session-windows-in-index-order s))
          "without overrides the index order equals id order")
      ;; Give the FIRST window a high per-session index: it must sort last.
      (cl-tmux/model:set-session-window-index s w0 99)
      (is (equal (list w1 w0)
                 (cl-tmux/model:session-windows-in-index-order s))
          "an override index must reorder the display list"))))
