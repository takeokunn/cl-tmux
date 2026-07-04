(in-package #:cl-tmux/test)

;;;; Display-menu and list-keys dispatch cases.

(in-suite dispatch-suite)

(test display-menu-O-keeps-menu-open-after-selection
  "display-menu -O keeps the menu open after a selection runs its command;
   without -O the menu closes (tmux -O)."
  (dolist (row '((t "display-menu -O -T t lbl k \"set -g @menu-ran 1\"" "with -O")
                 (nil "display-menu -T t lbl k \"set -g @menu-ran 1\"" "without -O")))
    (destructuring-bind (expect-open command desc) row
      (with-fake-session (s)
        (let ((*overlay* nil)
              (cl-tmux/prompt:*active-menu* nil))
          (cl-tmux::%run-command-line s command)
          (is-true cl-tmux/prompt:*active-menu* "menu must open (~A)" desc)
          (cl-tmux::dispatch-command s :menu-select 13)
          (is (string= "1" (cl-tmux/options:get-option "@menu-ran" nil))
              "the selected command must run (~A)" desc)
          (if expect-open
              (is-true cl-tmux/prompt:*active-menu*
                       "-O must keep the menu open")
              (is (null cl-tmux/prompt:*active-menu*)
                  "without -O the menu must close")))))))

(test list-keys-N-lists-only-noted-bindings
  "list-keys -N lists bindings carrying bind -N notes (with the note text);
   -a additionally includes un-noted bindings."
  (with-isolated-config
   (with-fake-session (s)
    (let ((*overlay* nil))
      (cl-tmux::%run-command-line
       s "bind -N \"Split the pane\" Y split-window")
      (cl-tmux::%run-command-line s "bind Z kill-pane")
      (let ((*overlay* nil))
        (cl-tmux::%run-command-line s "list-keys -N -T prefix")
        (is (search "Split the pane" *overlay*)
            "-N must show the note text")
        (is (null (search "Z " *overlay*))
            "-N must exclude un-noted bindings"))
      (let ((*overlay* nil))
        (cl-tmux::%run-command-line s "list-keys -N -a -T prefix")
        (is (search "Split the pane" *overlay*) "-Na keeps noted bindings")
        (is (search "Z" *overlay*) "-Na includes un-noted bindings"))))))
