(in-package #:cl-tmux)

;;;; Paste-buffer command handler helpers.
;;;;
;;;; These functions implement the :list-buffers, :show-buffer, :choose-buffer,
;;;; :delete-buffer, :save-buffer, and :load-buffer command handlers that are
;;;; registered in dispatch-handlers.lisp via define-command-handlers.
;;;;
;;;; Keeping them in a dedicated file reduces the size of dispatch-handlers.lisp
;;;; while keeping the handler table in a single define-command-handlers call.

;;; -- %cmd-list-buffers -------------------------------------------------------

(defun %cmd-list-buffers ()
  "Show an overlay listing all paste buffers with name, length, and preview.
   Lines read `name: NN bytes: preview`, matching tmux's named-buffer listing."
  (show-overlay
   (with-output-to-string (stream)
     (let ((buffers (cl-tmux/buffer:list-paste-buffers-with-names)))
       (if buffers
           (loop for (name . text) in buffers
                 do (format stream "~A: ~D bytes: ~A~%"
                            name
                            (length text)
                            (subseq text 0 (min +buffer-preview-length+
                                                (length text)))))
           (format stream "(no paste buffers)~%"))))))

;;; -- %cmd-show-buffer --------------------------------------------------------

(defun %cmd-show-buffer ()
  "Show the full content of paste buffer 0 in an overlay."
  (let ((buffer (cl-tmux/buffer:get-paste-buffer 0)))
    (show-overlay (or buffer "(no paste buffers)"))))

;;; -- %cmd-choose-buffer ------------------------------------------------------

(defun %cmd-choose-buffer (session)
  "Show a numbered list of paste buffers; prompt for an index and paste it."
  (let ((buffers (cl-tmux/buffer:list-paste-buffers)))
    (if buffers
        (let ((listing
                (with-output-to-string (stream)
                  (loop for buffer in buffers
                        for index from 0
                        do (format stream "~D: ~A~%"
                                   index
                                   (subseq buffer 0 (min +buffer-preview-length+
                                                         (length buffer))))))))
          (show-overlay listing)
          (prompt-integer "choose buffer (index)"
                          (lambda (idx)
                            (let* ((text (cl-tmux/buffer:get-paste-buffer idx))
                                   (win  (session-active-window session))
                                   (ap   (and win (window-active-pane win))))
                              (%paste-to-pane ap text)))))
        (show-overlay "(no paste buffers)"))))

;;; -- %cmd-delete-buffer ------------------------------------------------------

(defun %cmd-delete-buffer ()
  "Delete paste buffer 0 and show a confirmation overlay."
  (let ((buffer (cl-tmux/buffer:get-paste-buffer 0)))
    (if buffer
        (progn
          (cl-tmux/buffer:delete-paste-buffer 0)
          (show-overlay "buffer 0 deleted"))
        (show-overlay "(no paste buffers to delete)"))))

;;; -- %cmd-save-buffer --------------------------------------------------------

(defun %cmd-save-buffer ()
  "Prompt for a file path and write paste buffer 0 to it."
  (let ((buffer (cl-tmux/buffer:get-paste-buffer 0)))
    (if buffer
        (prompt-start "save-buffer to file" ""
                      (lambda (path)
                        (unless (string= path "")
                          (handler-case
                              (progn
                                (with-open-file (f path
                                                   :direction :output
                                                   :if-exists :supersede
                                                   :if-does-not-exist :create)
                                  (write-string buffer f))
                                (show-overlay (format nil "saved to ~A" path)))
                            (error (e)
                              (show-overlay (format nil "save-buffer error: ~A" e)))))))
        (show-overlay "(no paste buffers to save)"))))

;;; -- %cmd-load-buffer --------------------------------------------------------

(defun %cmd-load-buffer ()
  "Prompt for a file path and load its content into the paste buffer ring."
  (prompt-start "load-buffer from file" ""
                (lambda (path)
                  (unless (string= path "")
                    (handler-case
                        (let ((content
                                (with-open-file (f path
                                                   :direction :input
                                                   :if-does-not-exist :error)
                                  (let ((s (make-string (file-length f))))
                                    (read-sequence s f)
                                    s))))
                          (cl-tmux/buffer:add-paste-buffer content)
                          (show-overlay (format nil "loaded ~D bytes from ~A"
                                                (length content) path)))
                      (error (e)
                        (show-overlay (format nil "load-buffer error: ~A" e))))))))
