(in-package #:cl-tmux/test)

;;;; Copy-mode mouse entry, indicators, and -X command dispatch cases.

(describe "dispatch-suite"

  ;; copy-mode -M places the copy cursor at the in-flight mouse position and
  ;; begins a selection (the MouseDrag1Pane entry); without a mouse event -M
  ;; enters copy mode normally.
  (it "copy-mode-M-enters-at-mouse-position-with-selection"
    (with-fake-session (s)
      (let* ((win  (cl-tmux/model:session-active-window s))
             (pane (cl-tmux/model:window-active-pane win))
             (screen (cl-tmux/model:pane-screen pane)))
        ;; With a mouse event over the pane: cursor jumps + selection begins.
        (let ((cl-tmux::*current-mouse-event*
                (list :btn 32 :col 5 :row 2 :release-p nil)))
          (cl-tmux::%cmd-copy-mode-arg s '("-M"))
          (expect (cl-tmux/terminal/types:screen-copy-mode-p screen) :to-be-truthy)
          (expect (equal (cons (- 2 (cl-tmux/model:pane-y pane))
                               (- 5 (cl-tmux/model:pane-x pane)))
                         (cl-tmux/terminal/types:screen-copy-cursor screen)))
          (expect (cl-tmux/terminal/types:screen-copy-selecting screen) :to-be-truthy)
          (cl-tmux/commands:copy-mode-exit screen))
        ;; Without a mouse event: plain entry, no selection.
        (let ((cl-tmux::*current-mouse-event* nil))
          (cl-tmux::%cmd-copy-mode-arg s '("-M"))
          (expect (cl-tmux/terminal/types:screen-copy-mode-p screen) :to-be-truthy)
          (expect (null (cl-tmux/terminal/types:screen-copy-selecting screen)))))))

  ;; copy-mode -H suppresses the position indicator for this entry; a later plain
  ;; entry shows it again.
  (it "copy-mode-H-hides-position-indicator"
    (with-fake-session (s)
      (let ((screen (cl-tmux/model:pane-screen (cl-tmux/model:session-active-pane s))))
        (cl-tmux::%cmd-copy-mode-arg s '("-H"))
        (expect (cl-tmux/terminal/types:screen-copy-hide-position screen) :to-be-truthy)
        (cl-tmux/commands:copy-mode-exit screen)
        (cl-tmux::%cmd-copy-mode-arg s '())
        (expect (null (cl-tmux/terminal/types:screen-copy-hide-position screen))))))

  ;; The newly-added send-keys -X names resolve through the X dispatch tables:
  ;; stop-selection keeps the mark but stops extending; halfpage-down-and-cancel
  ;; and copy-pipe-end-of-line / jump-to-forward are registered.
  (it "copy-mode-x-new-command-names-resolve"
    (with-fake-session (s)
      (let* ((pane   (cl-tmux/model:session-active-pane s))
             (screen (cl-tmux/model:pane-screen pane)))
        (cl-tmux/commands:copy-mode-enter screen)
        (cl-tmux/commands:copy-mode-begin-selection screen)
        (expect (cl-tmux/terminal/types:screen-copy-selecting screen) :to-be-truthy)
        (cl-tmux::%run-command-line s "send-keys -X stop-selection")
        (expect (null (cl-tmux/terminal/types:screen-copy-selecting screen)))
        (expect (cl-tmux/terminal/types:screen-copy-mark screen) :to-be-truthy)
        ;; Registration checks for the other names.
        (expect (assoc "halfpage-down-and-cancel"
                       cl-tmux::*copy-mode-x-commands* :test #'string=)
                :to-be-truthy)
        (expect (find "copy-pipe-end-of-line"
                      cl-tmux::*send-keys-x-explicit-arg-specs*
                      :key #'first :test #'string=)
                :to-be-truthy)
        (expect (find "jump-to-forward"
                      cl-tmux::*send-keys-x-explicit-arg-specs*
                      :key #'first :test #'string=)
                :to-be-truthy))))

  ;; send-keys -X toggle-position flips the position-indicator visibility flag.
  (it "copy-mode-toggle-position-flips-indicator-visibility"
    (with-fake-session (s)
      (let ((screen (cl-tmux/model:pane-screen (cl-tmux/model:session-active-pane s))))
        (cl-tmux::%cmd-copy-mode-arg s '())
        (expect (null (cl-tmux/terminal/types:screen-copy-hide-position screen)))
        (cl-tmux::%run-command-line s "send-keys -X toggle-position")
        (expect (cl-tmux/terminal/types:screen-copy-hide-position screen) :to-be-truthy)
        (cl-tmux::%run-command-line s "send-keys -X toggle-position")
        (expect (null (cl-tmux/terminal/types:screen-copy-hide-position screen))))))

  ;; OSC 133;A records prompt marks; copy-mode previous-prompt/next-prompt jump
  ;; between them (shell-integration prompt jumping).
  (it "osc-133-prompt-marks-and-copy-mode-prompt-jumps"
    (with-fake-session (s)
      (let* ((pane   (cl-tmux/model:session-active-pane s))
             (screen (cl-tmux/model:pane-screen pane)))
        ;; Two prompts: one at row 0, output, one at row 2.
        (cl-tmux/terminal/emulator:screen-process-bytes
         screen (babel:string-to-octets
                 (format nil "~C]133;A~Cprompt-1~%output~%~C]133;A~Cprompt-2"
                         #\Escape (code-char 7) #\Escape (code-char 7))
                 :encoding :utf-8))
        (expect (= 2 (length (cl-tmux/terminal/types:screen-prompt-marks screen))))
        (cl-tmux/commands:copy-mode-enter screen)
        ;; Cursor starts at the bottom; previous-prompt goes to the second
        ;; prompt (row 2), a second one to the first (row 0).
        (cl-tmux/commands:copy-mode-previous-prompt screen)
        (expect (= 2 (car (cl-tmux/terminal/types:screen-copy-cursor screen))))
        (cl-tmux/commands:copy-mode-previous-prompt screen)
        (expect (= 0 (car (cl-tmux/terminal/types:screen-copy-cursor screen))))
        (cl-tmux/commands:copy-mode-next-prompt screen)
        (expect (= 2 (car (cl-tmux/terminal/types:screen-copy-cursor screen))))))))
