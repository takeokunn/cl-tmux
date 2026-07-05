(in-package #:cl-tmux/test)

;;;; Copy-mode mouse entry, indicators, and -X command dispatch cases.

(in-suite dispatch-suite)

(test copy-mode-M-enters-at-mouse-position-with-selection
  "copy-mode -M places the copy cursor at the in-flight mouse position and
   begins a selection (the MouseDrag1Pane entry); without a mouse event -M
   enters copy mode normally."
  (with-fake-session (s)
    (let* ((win  (cl-tmux/model:session-active-window s))
           (pane (cl-tmux/model:window-active-pane win))
           (screen (cl-tmux/model:pane-screen pane)))
      ;; With a mouse event over the pane: cursor jumps + selection begins.
      (let ((cl-tmux::*current-mouse-event*
              (list :btn 32 :col 5 :row 2 :release-p nil)))
        (cl-tmux::%cmd-copy-mode-arg s '("-M"))
        (is-true (cl-tmux/terminal/types:screen-copy-mode-p screen)
                 "-M must enter copy mode")
        (is (equal (cons (- 2 (cl-tmux/model:pane-y pane))
                         (- 5 (cl-tmux/model:pane-x pane)))
                   (cl-tmux/terminal/types:screen-copy-cursor screen))
            "-M must place the copy cursor at the mouse position")
        (is-true (cl-tmux/terminal/types:screen-copy-selecting screen)
                 "-M must begin a selection")
        (cl-tmux/commands:copy-mode-exit screen))
      ;; Without a mouse event: plain entry, no selection.
      (let ((cl-tmux::*current-mouse-event* nil))
        (cl-tmux::%cmd-copy-mode-arg s '("-M"))
        (is-true (cl-tmux/terminal/types:screen-copy-mode-p screen)
                 "-M without a mouse event must still enter copy mode")
        (is (null (cl-tmux/terminal/types:screen-copy-selecting screen))
            "-M without a mouse event must not begin a selection")))))

(test copy-mode-H-hides-position-indicator
  "copy-mode -H suppresses the position indicator for this entry; a later plain
   entry shows it again."
  (with-fake-session (s)
    (let ((screen (cl-tmux/model:pane-screen (cl-tmux/model:session-active-pane s))))
      (cl-tmux::%cmd-copy-mode-arg s '("-H"))
      (is-true (cl-tmux/terminal/types:screen-copy-hide-position screen)
               "-H must set the hide-position flag")
      (cl-tmux/commands:copy-mode-exit screen)
      (cl-tmux::%cmd-copy-mode-arg s '())
      (is (null (cl-tmux/terminal/types:screen-copy-hide-position screen))
          "a plain entry must clear the hide-position flag"))))

(test copy-mode-x-new-command-names-resolve
  "The newly-added send-keys -X names resolve through the X dispatch tables:
   stop-selection keeps the mark but stops extending; halfpage-down-and-cancel
   and copy-pipe-end-of-line / jump-to-forward are registered."
  (with-fake-session (s)
    (let* ((pane   (cl-tmux/model:session-active-pane s))
           (screen (cl-tmux/model:pane-screen pane)))
      (cl-tmux/commands:copy-mode-enter screen)
      (cl-tmux/commands:copy-mode-begin-selection screen)
      (is-true (cl-tmux/terminal/types:screen-copy-selecting screen)
               "precondition: selecting")
      (cl-tmux::%run-command-line s "send-keys -X stop-selection")
      (is (null (cl-tmux/terminal/types:screen-copy-selecting screen))
          "stop-selection must stop extending")
      (is-true (cl-tmux/terminal/types:screen-copy-mark screen)
               "stop-selection must KEEP the mark (unlike clear-selection)")
      ;; Registration checks for the other names.
      (is-true (assoc "halfpage-down-and-cancel"
                      cl-tmux::*copy-mode-x-commands* :test #'string=)
               "halfpage-down-and-cancel must be in the X table")
      (is-true (find "copy-pipe-end-of-line"
                     cl-tmux::*send-keys-x-explicit-arg-specs*
                     :key #'first :test #'string=)
               "bare copy-pipe-end-of-line must be in the arg specs")
      (is-true (find "jump-to-forward"
                     cl-tmux::*send-keys-x-explicit-arg-specs*
                     :key #'first :test #'string=)
               "jump-to-forward must be in the arg specs"))))

(test copy-mode-toggle-position-flips-indicator-visibility
  "send-keys -X toggle-position flips the position-indicator visibility flag."
  (with-fake-session (s)
    (let ((screen (cl-tmux/model:pane-screen (cl-tmux/model:session-active-pane s))))
      (cl-tmux::%cmd-copy-mode-arg s '())
      (is (null (cl-tmux/terminal/types:screen-copy-hide-position screen))
          "the indicator is visible on a plain entry")
      (cl-tmux::%run-command-line s "send-keys -X toggle-position")
      (is-true (cl-tmux/terminal/types:screen-copy-hide-position screen)
               "toggle-position must hide the indicator")
      (cl-tmux::%run-command-line s "send-keys -X toggle-position")
      (is (null (cl-tmux/terminal/types:screen-copy-hide-position screen))
          "a second toggle must show it again"))))

(test osc-133-prompt-marks-and-copy-mode-prompt-jumps
  "OSC 133;A records prompt marks; copy-mode previous-prompt/next-prompt jump
   between them (shell-integration prompt jumping)."
  (with-fake-session (s)
    (let* ((pane   (cl-tmux/model:session-active-pane s))
           (screen (cl-tmux/model:pane-screen pane)))
      ;; Two prompts: one at row 0, output, one at row 2.
      (cl-tmux/terminal/emulator:screen-process-bytes
       screen (babel:string-to-octets
               (format nil "~C]133;A~Cprompt-1~%output~%~C]133;A~Cprompt-2"
                       #\Escape (code-char 7) #\Escape (code-char 7))
               :encoding :utf-8))
      (is (= 2 (length (cl-tmux/terminal/types:screen-prompt-marks screen)))
          "two 133;A marks must be recorded")
      (cl-tmux/commands:copy-mode-enter screen)
      ;; Cursor starts at the bottom; previous-prompt goes to the second
      ;; prompt (row 2), a second one to the first (row 0).
      (cl-tmux/commands:copy-mode-previous-prompt screen)
      (is (= 2 (car (cl-tmux/terminal/types:screen-copy-cursor screen)))
          "previous-prompt must land on the newest prompt row")
      (cl-tmux/commands:copy-mode-previous-prompt screen)
      (is (= 0 (car (cl-tmux/terminal/types:screen-copy-cursor screen)))
          "a second previous-prompt must reach the older prompt")
      (cl-tmux/commands:copy-mode-next-prompt screen)
      (is (= 2 (car (cl-tmux/terminal/types:screen-copy-cursor screen)))
          "next-prompt must return to the newer prompt"))))
