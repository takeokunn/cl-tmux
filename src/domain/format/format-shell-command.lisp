(in-package #:cl-tmux/format)

;;;; External shell-command expansion port for #(command).

(defconstant +format-shell-command-timeout+ 2
  "Seconds to allow #(shell-command) format expansion commands to run.")

(defconstant +format-shell-command-output-limit+ 4096
  "Maximum bytes captured from #(shell-command) expansion stdout.")

(defun %format-shell-capture-command (command)
  "Wrap COMMAND so stdout is bounded before UIOP accumulates it."
  (format nil "( ~A ) | head -c ~D" command +format-shell-command-output-limit+))

(defun %trim-one-trailing-newline (text)
  "Return TEXT without exactly one trailing newline."
  (if (and (plusp (length text))
           (char= (char text (1- (length text))) #\Newline))
      (subseq text 0 (1- (length text)))
      text))

(defun %run-format-shell-command (command)
  "Run COMMAND for #(shell-command) expansion and return normalized stdout."
  (handler-case
      (%trim-one-trailing-newline
       (uiop:run-program (list "/bin/sh" "-c" (%format-shell-capture-command command))
                         :output :string
                         :ignore-error-status t
                         :timeout +format-shell-command-timeout+))
    (error () "")))
