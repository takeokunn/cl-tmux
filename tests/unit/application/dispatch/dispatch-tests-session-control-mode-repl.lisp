(in-package #:cl-tmux/test)

;;;; Dispatch session tests - control-mode REPL framing and command errors.

(in-suite dispatch-suite)

(test control-run-command-frames-output
  "%control-run-command frames a command's overlay output in a %begin/%end block."
  (with-fake-session (s)
    (let* ((reply (cl-tmux::%control-run-command s "display-message hello" 1)))
      (is (search "%begin 0 1 1" reply) "reply opens with %begin for command 1")
      (is (search "%end 0 1 1" reply)   "reply closes with %end")
      (is (search "hello" reply)        "the command's output is in the reply body"))))

(test control-mode-loop-frames-each-and-exits
  "control-mode-loop runs each input line as the next numbered command and emits
   %exit at EOF."
  (with-fake-session (s)
    (let* ((out (with-output-to-string (o)
                  (with-input-from-string
                      (i (format nil "display-message a~%display-message b~%"))
                    (cl-tmux::control-mode-loop s i o)))))
      (is (search "%begin 0 1 1" out) "first line is command 1")
      (is (search "%begin 0 2 1" out) "second line is command 2")
      (is (search "%exit" out)        "the loop emits %exit on EOF"))))

(test control-mode-loop-skips-blank-lines
  "Blank input lines are not run as commands (no reply framed for them)."
  (with-fake-session (s)
    (let* ((out (with-output-to-string (o)
                  (with-input-from-string (i (format nil "~%display-message x~%~%"))
                    (cl-tmux::control-mode-loop s i o)))))
      (is (search "%begin 0 1 1" out) "the one real command is command 1")
      (is (null (search "%begin 0 2 1" out))
          "blank lines did not advance the command number"))))

(test control-run-command-unknown-is-error
  "An unknown command closes the control-mode reply with %error, not %end."
  (with-fake-session (s)
    (let* ((*overlay* nil)
           (reply (cl-tmux::%control-run-command s "bogus-command-xyz" 3)))
      (is (search "%begin 0 3 1" reply) "reply opens with %begin")
      (is (search "%error 0 3 1" reply) "unknown command closes with %error")
      (is (search "unknown command" reply) "the error message is in the body"))))
