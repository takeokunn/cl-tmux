(in-package #:cl-tmux/buffer)

(defconstant +default-buffer-limit+ 50
  "Fallback capacity for the paste-buffer ring when buffer-limit has not been configured.")

(defvar *paste-buffers* nil
  "List of paste buffers, most recent first.  Each entry is a (NAME . TEXT) cons:
   NAME is the buffer's name string (auto-assigned \"bufferN\" for unnamed adds, or
   an explicit name from set-buffer -b / capture-pane -b); TEXT is the content.
   The public string-returning accessors (get-paste-buffer / list-paste-buffers)
   hide the cons shape so existing callers keep seeing plain text.")

(defvar *buffer-auto-index* 0
  "Monotonic counter for auto-naming unnamed buffers buffer0, buffer1, ... — like
   tmux's automatic buffer names.")

(defun %buffer-limit ()
  "Return the configured buffer-limit, defaulting to +default-buffer-limit+ when options are not yet initialised."
  (or (ignore-errors (cl-tmux/options:get-option "buffer-limit"))
      +default-buffer-limit+))

(defun %next-auto-buffer-name ()
  "Return the next automatic buffer name (bufferN) and advance the counter."
  (prog1 (format nil "buffer~D" *buffer-auto-index*)
    (incf *buffer-auto-index*)))

(defun %enforce-buffer-limit ()
  "Trim *paste-buffers* to at most buffer-limit entries (drops the oldest)."
  (let ((limit (max 1 (%buffer-limit))))
    (when (> (length *paste-buffers*) limit)
      (setf *paste-buffers* (subseq *paste-buffers* 0 limit)))))

(defun add-paste-buffer (text &optional name)
  "Add TEXT as a paste buffer (pushed most-recent-first) and return TEXT.
   When NAME is supplied the buffer is named NAME, replacing any existing buffer
   of that name (set-buffer -b semantics); when NAME is NIL an automatic name
   bufferN is assigned.  Honours buffer-limit."
  (let ((bname (or name (%next-auto-buffer-name))))
    (when name
      ;; Explicit name: replace any existing buffer with the same name in place.
      (setf *paste-buffers*
            (remove bname *paste-buffers* :key #'car :test #'string=)))
    (push (cons bname text) *paste-buffers*)
    (%enforce-buffer-limit))
  text)

(defun rename-paste-buffer (source-name target-name)
  "Rename SOURCE-NAME, or the most recent buffer when SOURCE-NAME is NIL, to
   TARGET-NAME and return the preserved TEXT.  Returns NIL when there is no
   source buffer."
  (let ((entry (if source-name
                   (assoc source-name *paste-buffers* :test #'string=)
                   (first *paste-buffers*))))
    (when entry
      (let ((old-name (car entry))
            (text     (cdr entry)))
        (if (string= old-name target-name)
            text
            (progn
              (setf *paste-buffers*
                    (remove old-name *paste-buffers* :key #'car :test #'string=))
              (setf *paste-buffers*
                    (remove target-name *paste-buffers* :key #'car :test #'string=))
              (push (cons target-name text) *paste-buffers*)
              (%enforce-buffer-limit)
              text))))))


(defun get-paste-buffer (&optional (index 0))
  "Return the TEXT of the INDEXth paste buffer (0-based, most recent first), or NIL
   if empty or out of range."
  (let ((entry (nth index *paste-buffers*)))
    (and entry (cdr entry))))

(defun set-named-buffer (name text)
  "Set the buffer named NAME to TEXT (creating or replacing it).  Returns TEXT."
  (add-paste-buffer text name))

(defun get-buffer-by-name (name)
  "Return the TEXT of the buffer named NAME, or NIL when there is no such buffer."
  (let ((entry (assoc name *paste-buffers* :test #'string=)))
    (and entry (cdr entry))))

(defun buffer-names ()
  "Return the list of buffer names, most recent first."
  (mapcar #'car *paste-buffers*))

(defun list-paste-buffers ()
  "Return the buffer TEXTs as strings, most recent first."
  (mapcar #'cdr *paste-buffers*))

(defun list-paste-buffers-with-names ()
  "Return a copy of the (NAME . TEXT) entries, most recent first."
  (copy-alist *paste-buffers*))

(defun delete-paste-buffer (&optional (index 0))
  "Remove the INDEXth paste buffer. Return T if removed, NIL if index is out of range."
  (if (and (>= index 0) (< index (length *paste-buffers*)))
      (progn
        (setf *paste-buffers*
              (append (subseq *paste-buffers* 0 index)
                      (subseq *paste-buffers* (1+ index))))
        t)
      nil))

(defun delete-buffer-by-name (name)
  "Remove the buffer named NAME.  Return T when one was removed, NIL otherwise."
  (let ((before (length *paste-buffers*)))
    (setf *paste-buffers*
          (remove name *paste-buffers* :key #'car :test #'string=))
    (/= before (length *paste-buffers*))))

(defun clear-paste-buffers ()
  "Set *paste-buffers* to nil and reset the automatic-name counter."
  (setf *paste-buffers* nil
        *buffer-auto-index* 0))

(defun %osc52-inbound-clipboard (text)
  "Inbound OSC 52 clipboard write from an application: add TEXT to the paste-buffer
   ring UNLESS the set-clipboard option is \"off\", in which case tmux ignores
   application clipboard writes entirely.  \"on\"/\"external\" (the default) accept
   them.  The gate lives here (not in the terminal parser) so the parser layer
   stays decoupled from the options layer."
  (unless (string-equal (or (ignore-errors (cl-tmux/options:get-option "set-clipboard"))
                            "on")
                        "off")
    (add-paste-buffer text)))

(defun initialize-osc52-handler ()
  "Wire the OSC 52 clipboard handler to the paste buffer ring.
   Applications (e.g., vim, tmux copy-mode) can write clipboard data via
   ESC ] 52 ; c ; <base64> ST — decoded by parser-osc and forwarded here.
   The handler honours set-clipboard (off → ignore inbound writes).
   Called once at load time; separated from top-level to make the coupling explicit
   and allow re-initialisation if the handler variable is reset."
  (setf cl-tmux/terminal/parser:*osc52-handler* #'%osc52-inbound-clipboard))

;; Wire OSC 52 handler at module load time via an explicit named call.
(initialize-osc52-handler)
