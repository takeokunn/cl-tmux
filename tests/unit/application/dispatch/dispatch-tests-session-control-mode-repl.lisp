(in-package #:cl-tmux/test)

;;;; Dispatch session tests - control-mode REPL framing and command errors.

(describe "dispatch-suite"

  ;; %control-run-command frames a command's overlay output in a %begin/%end block.
  (it "control-run-command-frames-output"
    (with-fake-session (s)
      (let* ((reply (cl-tmux::%control-run-command s "display-message hello" 1)))
        (expect (search "%begin 0 1 1" reply))
        (expect (search "%end 0 1 1" reply))
        (expect (search "hello" reply)))))

  ;; control-mode-loop runs each input line as the next numbered command and emits
  ;; %exit at EOF.
  (it "control-mode-loop-frames-each-and-exits"
    (with-fake-session (s)
      (let* ((out (with-output-to-string (o)
                    (with-input-from-string
                        (i (format nil "display-message a~%display-message b~%"))
                      (cl-tmux::control-mode-loop s i o)))))
        (expect (search "%begin 0 1 1" out))
        (expect (search "%begin 0 2 1" out))
        (expect (search "%exit" out)))))

  ;; Blank input lines are not run as commands (no reply framed for them).
  (it "control-mode-loop-skips-blank-lines"
    (with-fake-session (s)
      (let* ((out (with-output-to-string (o)
                    (with-input-from-string (i (format nil "~%display-message x~%~%"))
                      (cl-tmux::control-mode-loop s i o)))))
        (expect (search "%begin 0 1 1" out))
        (expect (null (search "%begin 0 2 1" out))))))

  ;; An unknown command closes the control-mode reply with %error, not %end.
  (it "control-run-command-unknown-is-error"
    (with-fake-session (s)
      (let* ((*overlay* nil)
             (reply (cl-tmux::%control-run-command s "bogus-command-xyz" 3)))
        (expect (search "%begin 0 3 1" reply))
        (expect (search "%error 0 3 1" reply))
        (expect (search "unknown command" reply))))))
