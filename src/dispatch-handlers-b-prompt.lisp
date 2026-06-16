;;;; Prompt-driven command handlers

(in-package #:cl-tmux)

(define-command-handlers
  (:command-prompt
   (prompt-nonempty ": "
                    (lambda (input)
                      (add-prompt-history input)
                      (%run-command-line session input))
                    :history *prompt-history*))
  (:move-window-prompt
   (with-active-window (win session)
     (prompt-integer "move-window to index"
                     (lambda (idx) (session-move-window session win idx)))))
  (:bind-key
   (prompt-nonempty "bind key: "
                    (lambda (input)
                      (let* ((parts   (uiop:split-string input :separator " "))
                             (key-tok (and (first parts)
                                          (cl-tmux/config::%parse-key-token (first parts))))
                             (cmd-str (second parts))
                             (kw      (and cmd-str
                                           (cl-tmux/config::%command-keyword cmd-str))))
                        (if kw
                            (progn
                              (set-key-binding key-tok kw)
                              (%overlayf "bound ~A -> ~(~A~)" key-tok kw))
                            (%overlayf "unknown command: ~A"
                                       (or cmd-str input)))))))
  (:unbind-key
   (prompt-nonempty "unbind key: "
                    (lambda (input)
                      (let ((k (cl-tmux/config::%parse-key-token input)))
                        (remove-key-binding k)
                        (%overlayf "unbound ~A" k)))))
  (:select-window-prompt
   (prompt-nonempty "select window (name or number): "
                    (lambda (input)
                      (let* ((idx (ignore-errors (parse-integer input)))
                             (win (or (and idx (find idx (session-windows session)
                                                     :key #'window-id))
                                      (find input (session-windows session)
                                            :key #'window-name
                                            :test #'string-equal))))
                        (if win
                            (%with-window-focus-transition (session)
                              (session-select-window session win))
                            (%overlayf "no window: ~A" input))))))
  )
