(in-package #:cl-tmux)

;;;; Shared prompt and menu helpers used by dispatch-handlers*.lisp.
;;;;
;;;; prompt-nonempty, prompt-history-nonempty, prompt-integer, and
;;;; %confirm-prompt are reused across multiple sibling handler files.  The
;;;; remaining helpers here are the small support functions that the main
;;;; command table and prompt-driven siblings share.

;;; -- Buffer preview constant -----------------------------------------------
;;;
;;; Buffer listing and preview truncate content to this many characters.
;;; A single named constant keeps every truncation site in sync.

(defconstant +buffer-preview-length+ 40
  "Maximum characters shown in a paste-buffer preview listing.")

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

(defun prompt-history-nonempty (label callback &key history single-key initial)
  "Start a prompt labelled LABEL; ignore empty input, record history, then call CALLBACK."
  (prompt-start label (or initial "")
                (lambda (input)
                  (unless (string= input "")
                    (add-prompt-history input)
                    (funcall callback input)))
                :history history
                :single-key single-key))

(defun prompt-integer (label callback)
  "Start a prompt labelled LABEL; call CALLBACK with the parsed integer when the
   input parses as a valid integer.  Silently ignores non-numeric input."
  (prompt-start label ""
                (lambda (input)
                  (let ((n (%parse-integer-or-nil input)))
                    (when n (funcall callback n))))))

(defun %prompt-or-run-name (label initial name run-fn)
  "Run RUN-FN with NAME when NAME is non-empty; otherwise start a prompt.
   Used by rename-style commands that accept either a direct name argument or an
   interactive prompt fallback."
  (if (plusp (length name))
      (funcall run-fn name)
      (prompt-start label initial run-fn)))

(defun %byte-vector (byte)
  "Return a one-byte unsigned vector containing BYTE."
  (make-array 1 :element-type '(unsigned-byte 8) :initial-element byte))

(defun %send-byte-to-pane (pane byte)
  "Write BYTE to PANE when the pane still has a live PTY."
  (when (and pane (cl-tmux/model:pane-live-p pane))
    (pty-write (pane-fd pane) (%byte-vector byte))
    t))

(defun %buffer-preview (text &key (preview-length +buffer-preview-length+))
  "Return the leading PREVIEW-LENGTH characters of TEXT."
  (subseq text 0 (min preview-length (length text))))

(defun %paste-buffer-listing-string (buffers &key (preview-length +buffer-preview-length+))
  "Return a numbered paste-buffer preview listing for BUFFERS."
  (with-output-to-string (stream)
    (if buffers
        (loop for buffer in buffers
              for index from 0
              do (format stream "~D: ~A~%" index
                         (%buffer-preview buffer
                                          :preview-length preview-length)))
        (format stream "(no paste buffers)~%"))))

(defun %named-paste-buffer-listing-string (buffers &key (preview-length +buffer-preview-length+))
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

(defun %show-session-menu (current-session)
  "Show the choose-session menu for CURRENT-SESSION."
  (let* ((sessions (or *server-sessions*
                       (list (cons (session-name current-session) current-session))))
         (items    (loop for (name . sess) in sessions
                         collect (cons (format nil "~A~A (~D window~:P)"
                                               (if (eq sess current-session) "*" " ")
                                               name
                                               (length (cl-tmux/model:session-windows sess)))
                                       (list :switch-client name)))))
    (%show-jk-menu "choose-session (j/k, Enter)" items)))

(defun %show-window-menu (session)
  "Show the choose-window menu for SESSION."
  (let* ((wins  (session-windows session))
         (act   (session-active-window session))
         (items (mapcar (lambda (w)
                          (cons (format nil "~A~A: ~A (~D pane~:P)"
                                        (if (eq w act) "*" " ")
                                        (window-id w)
                                        (window-name w)
                                        (length (window-panes w)))
                                (list :select-window (window-id w))))
                        wins)))
    (%show-jk-menu "choose-window (j/k, Enter)" items "(no windows)")))

(defun %show-window-search-results (session pattern)
  "Show a find-window overlay for SESSION and PATTERN."
  (let* ((wins    (session-windows session))
         (matches (remove-if-not
                   (lambda (w) (%window-matches-pattern-p w pattern))
                   wins)))
    (show-overlay
     (if matches
         (with-output-to-string (stream)
           (dolist (w matches)
             (format stream "~A: ~A~A~%"
                     (cl-tmux/model:window-id w)
                     (window-name w)
                     (if (eq w (session-active-window session))
                         " [active]" ""))))
         (format nil "no windows matching ~S~%" pattern)))))

(defun %show-display-panes-overlay (session)
  "Show the transient display-panes overlay for SESSION when it has panes."
  (with-active-window (win session)
    (let ((panes (window-panes win)))
      (when panes
        (let* ((panes-ms (or (cl-tmux/options:get-option "display-panes-time") 1000))
               (saved-ms (cl-tmux/options:get-option "display-time" 750)))
          (cl-tmux/options:set-option "display-time" panes-ms)
          (show-transient-overlay "")
          (setf cl-tmux/prompt:*display-panes-active* t)
          (cl-tmux/options:set-option "display-time" saved-ms)
          (setf *dirty* t))))))

(defun %show-window-options (win)
  "Show an overlay listing window options for WIN."
  (show-overlay
   (with-output-to-string (stream)
     (format stream "# window options~%")
     (write-string (cl-tmux/options:show-window-options win) stream))))

(defun %kill-current-pane-confirm (session)
  "Confirm and kill the active pane in SESSION."
  (with-active-window (win session)
    (let* ((ap  (window-active-pane win))
           (msg (if ap (format nil "kill-pane ~D? (y/n)" (pane-id ap)) "kill-pane? (y/n)")))
      (%confirm-prompt msg (lambda () (%handle-kill-result (kill-pane session)))))))

(defun %kill-current-window-confirm (session)
  "Confirm and kill the active window in SESSION."
  (with-active-window (win session)
    (%confirm-prompt (format nil "kill-window ~A? (y/n)" (window-name win))
                     (lambda () (%handle-kill-result
                                 (kill-window session (session-active-window session)))))))

(defun %respawn-current-pane (session)
  "Respawn the active pane in SESSION and start its reader thread."
  (with-active-window (win session)
    (let ((ap (window-active-pane win)))
      (when ap
        (let ((new-pane (respawn-pane session ap)))
          (start-reader-thread new-pane))))))

(defun %rename-current-window (session)
  "Prompt to rename the active window in SESSION."
  (with-active-window (win session)
    (%prompt-or-run-name "rename-window" (window-name win) ""
                         (lambda (name) (rename-window win name)))))

(defun %rename-current-session (session)
  "Prompt to rename SESSION."
  (%prompt-or-run-name "rename-session" (session-name session) ""
                       (lambda (name)
                         (%rename-session-checked session name))))

(defun %run-shell-prompt ()
  "Prompt for a shell command and display its output overlay."
  (prompt-nonempty "run-shell"
                   (lambda (cmd)
                     (show-overlay (run-shell cmd)))))

(defun %if-shell-prompt ()
  "Prompt for a shell command and show whether it succeeds."
  (prompt-nonempty "if-shell"
                   (lambda (cmd)
                     (if-shell cmd
                               (lambda () (%overlayf "[if-shell] ~A: ok" cmd))
                               :else-fn (lambda () (%overlayf "[if-shell] ~A: non-zero exit" cmd))))))

(defun %has-session-prompt ()
  "Prompt for a session name and show whether it exists."
  (prompt-nonempty "has-session"
                   (lambda (name)
                     (let ((found (server-find-session name)))
                       (show-overlay (if found "yes" "no"))))))

(defun %new-session-default-name ()
  "Create a new session with the next default numeric name."
  (let* ((rows (- *term-rows* *status-height*))
         (cols *term-cols*)
         (n    (1+ (length *server-sessions*)))
         (name (format nil "~D" n)))
    (new-session name rows cols)))

(defun %move-current-window (session)
  "Prompt for a target index and move the active window there."
  (with-active-window (win session)
    (prompt-integer "move-window"
                    (lambda (idx) (session-move-window session win idx)))))

(defun %swap-current-window (session)
  "Prompt for a target window id and swap it with the active window."
  (with-active-window (win session)
    (prompt-integer "swap-window"
                    (lambda (dst-id)
                      (%swap-window-ids
                       session win
                       (find dst-id (session-windows session)
                             :key #'window-id))))))

(defun %rotate-current-window (session direction)
  "Rotate the active window in SESSION by DIRECTION."
  (with-active-window (win session)
    (window-rotate win direction)))

(defun %select-window-by-byte (session byte)
  "Select a window by the digit encoded in BYTE."
  (when byte
    (select-window-by-number session (- byte (char-code #\0)))))

(defun %display-message-prompt (session)
  "Prompt for a message and display it."
  (prompt-nonempty "display-message"
                   (lambda (msg)
                     (%cmd-display-message session (list msg)))))

(defun %source-file-prompt ()
  "Prompt for a path and load it as a config file."
  (prompt-nonempty "source-file"
                   (lambda (path)
                     (load-config-file (pathname path)))))

(defun %confirm-before-prompt ()
  "Prompt for confirmation and show a simple overlay when accepted."
  (%confirm-prompt "confirm? (y/n)"
                   (lambda ()
                     (show-overlay "[confirmed]"))))

(defun %copy-mode-cursor-fn (direction)
  "Return a one-arg function that moves the copy-mode cursor in DIRECTION."
  (lambda (s) (copy-mode-move-cursor s direction)))
