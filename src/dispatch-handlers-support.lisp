(in-package #:cl-tmux)

;;;; Shared prompt and menu helpers used by dispatch-handlers*.lisp.
;;;;
;;;; prompt-nonempty and prompt-integer are reused across multiple sibling
;;;; handler files.  The remaining helpers here are the small support functions
;;;; that the main command table and prompt-driven siblings share.

(defun %confirm-prompt (msg ok-fn)
  "Show MSG as a y/n prompt; call OK-FN (no args) when the user types y/Y."
  (prompt-start msg ""
                (lambda (input)
                  (when (string-equal input "y")
                    (funcall ok-fn)))
                :single-key t))

(defun prompt-nonempty (label callback &key history)
  "Start a prompt labelled LABEL; call CALLBACK with the input only when non-empty."
  (prompt-start label ""
                (lambda (input)
                  (unless (string= input "")
                    (funcall callback input)))
                :history history))

(defun prompt-integer (label callback)
  "Start a prompt labelled LABEL; call CALLBACK with the parsed integer when the
   input parses as a valid integer.  Silently ignores non-numeric input."
  (prompt-start label ""
                (lambda (input)
                  (let ((n (ignore-errors (parse-integer input))))
                    (when n (funcall callback n))))))

(defun %byte-vector (byte)
  "Return a one-byte unsigned vector containing BYTE."
  (make-array 1 :element-type '(unsigned-byte 8) :initial-element byte))

(defun %buffer-preview (text &key (preview-length 40))
  "Return the leading PREVIEW-LENGTH characters of TEXT."
  (subseq text 0 (min preview-length (length text))))

(defun %paste-buffer-listing-string (buffers &key (preview-length 40))
  "Return a numbered paste-buffer preview listing for BUFFERS."
  (with-output-to-string (stream)
    (if buffers
        (loop for buffer in buffers
              for index from 0
              do (format stream "~D: ~A~%" index
                         (%buffer-preview buffer
                                          :preview-length preview-length)))
        (format stream "(no paste buffers)~%"))))

(defun %named-paste-buffer-listing-string (buffers &key (preview-length 40))
  "Return a named paste-buffer listing for BUFFERS."
  (with-output-to-string (stream)
    (if buffers
        (loop for (name . text) in buffers
              do (format stream "~A: ~D bytes: ~A~%"
                         name
                         (length text)
                         (%buffer-preview text
                                          :preview-length preview-length)))
        (format stream "(no paste buffers)~%"))))

(defun %copy-mode-search-prompt (session prompt-char search-fn)
  "Open a copy-mode search prompt with PROMPT-CHAR prefix and call SEARCH-FN
   on the entered term when non-empty."
  (let ((screen (%active-screen session)))
    (when screen
      (prompt-nonempty prompt-char
                       (lambda (term) (funcall search-fn screen term))))))

(defun %show-jk-menu (title items &optional empty-msg)
  "Show ITEMS as an interactive j/k menu titled TITLE.
   When ITEMS is empty and EMPTY-MSG is given, show EMPTY-MSG as a plain overlay instead."
  (if (and (null items) empty-msg)
      (show-overlay empty-msg)
      (progn
        (show-menu (make-menu :title title :items items :selected-index 0))
        (show-overlay (%format-menu *active-menu*)))))

(defun %copy-mode-cursor-fn (direction)
  "Return a one-arg function that moves the copy-mode cursor in DIRECTION."
  (lambda (s) (copy-mode-move-cursor s direction)))
