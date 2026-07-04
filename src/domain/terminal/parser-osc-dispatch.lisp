(in-package #:cl-tmux/terminal/parser)

;;;; OSC command parsing and dispatch.

(defun %parse-osc-command (payload semicolon-position)
  "Parse the OSC command integer from PAYLOAD up to SEMICOLON-POSITION."
  (handler-case
      (parse-integer (subseq payload 0 semicolon-position))
    (error () nil)))

(defun %handle-osc-52 (text)
  "Handle OSC 52 clipboard write: decode Base64 payload and call *osc52-handler*."
  (let* ((inner-semi   (position #\; text))
         (payload-data (and inner-semi (subseq text (1+ inner-semi)))))
    (when (and payload-data (string/= payload-data "?"))
      (let* ((decoded-bytes (and payload-data (%base64-decode payload-data)))
             (decoded-text  (and decoded-bytes
                                 (handler-case
                                     (babel:octets-to-string decoded-bytes :encoding :utf-8)
                                   (error () nil)))))
        (when (and decoded-text *osc52-handler*)
          (funcall *osc52-handler* decoded-text))))))

(defun %handle-osc-133 (screen body)
  "OSC 133 (shell integration / semantic prompts)."
  (when (and (plusp (length body)) (char-equal (char body 0) #\A))
    (let ((absolute (+ (screen-history-trimmed screen)
                       (length (screen-scrollback screen))
                       (screen-cursor-y screen))))
      (pushnew absolute (screen-prompt-marks screen)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro define-osc-rules (&rest rules)
    "Build %DISPATCH-OSC-COMMAND from a declarative OSC command table."
    `(defun %dispatch-osc-command (screen command body)
       (declare (type screen screen) (ignorable body))
       (cond
         ,@(loop for rule in rules
                 collect
                 (destructuring-bind (command-designator &body body-forms) rule
                   (let ((commands (if (listp command-designator)
                                       command-designator
                                       (list command-designator))))
                     `((member command ',commands)
                       (progn ,@body-forms)))))
         (t nil)))))

(define-osc-rules
  ((0 1 2)
   (set-screen-title screen body))
  (8
   (%handle-osc-8 screen body))
  (7
   (set-screen-cwd screen (%osc7-path body)))
  (10
   (%osc-color-command screen 10 body (screen-osc-default-fg screen)
                       #'(lambda (rgb)
                           (setf (screen-osc-default-fg screen) rgb))))
  (110
   (reset-osc-default-fg screen))
  (11
   (%osc-color-command screen 11 body (screen-osc-default-bg screen)
                       #'(lambda (rgb)
                           (setf (screen-osc-default-bg screen) rgb))))
  (111
   (reset-osc-default-bg screen))
  (4
   (%handle-osc-4 screen body))
  (104
   (%handle-osc-104 screen body))
  (52
   (%handle-osc-52 body))
  (133
   (%handle-osc-133 screen body)))

(defun %dispatch-osc (screen payload-buffer)
  "Parse accumulated OSC payload PAYLOAD-BUFFER and apply side effects to SCREEN."
  (let* ((payload  (babel:octets-to-string payload-buffer :encoding :utf-8 :errorp nil))
         (semi-pos (position #\; payload))
         (command  (%parse-osc-command payload (or semi-pos (length payload))))
         (body     (if semi-pos (subseq payload (1+ semi-pos)) "")))
    (when command
      (%dispatch-osc-command screen command body))))
