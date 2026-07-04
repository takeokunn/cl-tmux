(in-package #:cl-tmux/test)

;;;; Capture-pane and terminal rendering dispatch cases.

(in-suite dispatch-suite)

(test capture-pane-a-requires-alternate-screen
  "capture-pane -a errors with tmux's 'no alternate screen' unless the pane's
   alternate screen is in use; while active, -a captures the (live) alternate."
  (with-fake-session (s)
    (let ((screen (cl-tmux/model:pane-screen (cl-tmux/model:session-active-pane s))))
      (let ((*overlay* nil))
        (cl-tmux::%cmd-capture-pane-arg s '("-a" "-p"))
        (assert-overlay-contains "no alternate screen" (overlay-lines)
                                 "capture-pane -a without an alt screen"))
      ;; Enter the alternate screen: -a now captures.
      (cl-tmux/terminal/actions:enter-alt-screen screen)
      (let ((*overlay* nil))
        (cl-tmux::%cmd-capture-pane-arg s '("-a" "-p"))
        (is (null (and *overlay* (search "no alternate screen" *overlay*)))
            "capture-pane -a with an active alt screen must capture")))))

(test capture-pane-J-joins-wrapped-scrollback-rows
  "capture-pane -J joins a scrollback row that wrapped into the next row -
   the wrap flag travels with the row when it scrolls into history."
  (with-fake-session (s)
    (let* ((pane   (cl-tmux/model:session-active-pane s))
           (screen (cl-tmux/model:pane-screen pane))
           (w      (cl-tmux/terminal/types:screen-width screen)))
      ;; Row 0 content 'AB', marked wrapped; scroll it into history.
      (setf (cl-tmux/terminal/types:cell-char
             (cl-tmux/terminal/types:screen-cell screen 0 0)) #\A
            (cl-tmux/terminal/types:cell-char
             (cl-tmux/terminal/types:screen-cell screen 1 0)) #\B)
      (cl-tmux/terminal/types:%mark-line-wrapped screen 0)
      (cl-tmux/terminal/actions:scroll-up-one screen)
      ;; The visible top row now continues the wrapped line: 'CD'.
      (setf (cl-tmux/terminal/types:cell-char
             (cl-tmux/terminal/types:screen-cell screen 0 0)) #\C
            (cl-tmux/terminal/types:cell-char
             (cl-tmux/terminal/types:screen-cell screen 1 0)) #\D)
      (is (eq t (first (cl-tmux/terminal/types:screen-scrollback-wrapped screen)))
          "the wrap flag must travel into the scrollback")
      (let ((joined (cl-tmux/commands:capture-pane pane
                                                   :include-scrollback t
                                                   :join t))
            (plain  (cl-tmux/commands:capture-pane pane
                                                   :include-scrollback t)))
        (declare (ignorable w))
        ;; -J preserves trailing spaces, so assert AB and CD land on the SAME
        ;; line rather than being adjacent characters.
        (flet ((line-with-ab (text)
                 (find-if (lambda (l) (search "AB" l))
                          (uiop:split-string text :separator '(#\Newline)))))
          (is (search "CD" (or (line-with-ab joined) ""))
              "-J must join the wrapped history row with the visible row")
          (is (null (search "CD" (or (line-with-ab plain) "")))
              "without -J the rows must stay separate"))))))

(test decdwl-line-size-recorded-and-rendered
  "ESC # 6 (DECDWL) records the row's line size; the renderer re-emits
   ESC # 6 for that row (and ESC # 5 for unflagged rows) so the outer terminal
   draws double-width lines; ESC # 5 clears; RIS resets."
  (with-fake-session (s)
    (let* ((pane   (cl-tmux/model:session-active-pane s))
           (screen (cl-tmux/model:pane-screen pane)))
      (cl-tmux/terminal/emulator:screen-process-bytes
       screen (babel:string-to-octets (format nil "~C#6WIDE" #\Escape)
                                      :encoding :utf-8))
      (is (eql #\6 (gethash 0 (cl-tmux/terminal/types:screen-line-sizes screen)))
          "ESC # 6 must record double-width for the cursor row")
      (let ((out (cl-tmux/renderer::render-session-to-string s 5 20)))
        (is (search (format nil "~C#6" #\Escape) out)
            "the renderer must re-emit ESC # 6 for the flagged row")
        (is (search (format nil "~C#5" #\Escape) out)
            "unflagged rows must emit ESC # 5 while any flag is active"))
      (cl-tmux/terminal/emulator:screen-process-bytes
       screen (babel:string-to-octets (format nil "~C#5" #\Escape)
                                      :encoding :utf-8))
      (is (null (gethash 0 (cl-tmux/terminal/types:screen-line-sizes screen)))
          "ESC # 5 must clear the row's size flag")
      (cl-tmux/terminal/emulator:screen-process-bytes
       screen (babel:string-to-octets (format nil "~C#6~Cc" #\Escape #\Escape)
                                      :encoding :utf-8))
      (is (zerop (hash-table-count
                  (cl-tmux/terminal/types:screen-line-sizes screen)))
          "RIS must reset all line sizes"))))
