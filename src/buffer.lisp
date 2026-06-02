(in-package #:cl-tmux/buffer)

(defconstant +default-buffer-limit+ 50
  "Fallback capacity for the paste-buffer ring when buffer-limit has not been configured.")

(defvar *paste-buffers* nil
  "List of paste buffer strings, most recent first.")

(defun %buffer-limit ()
  "Return the configured buffer-limit, defaulting to +default-buffer-limit+ when options are not yet initialised."
  (or (ignore-errors (cl-tmux/options:get-option "buffer-limit"))
      +default-buffer-limit+))

(defun add-paste-buffer (text)
  "Push TEXT onto *paste-buffers*, keeping at most buffer-limit entries. Return TEXT."
  (push text *paste-buffers*)
  (let ((limit (max 1 (%buffer-limit))))
    (when (> (length *paste-buffers*) limit)
      (setf *paste-buffers* (subseq *paste-buffers* 0 limit))))
  text)

(defun get-paste-buffer (&optional (index 0))
  "Return the INDEXth paste buffer (0-based), or NIL if empty or out of range."
  (nth index *paste-buffers*))

(defun list-paste-buffers ()
  "Return a copy of *paste-buffers*."
  (copy-list *paste-buffers*))

(defun delete-paste-buffer (&optional (index 0))
  "Remove the INDEXth paste buffer. Return T if removed, NIL if index is out of range."
  (if (and (>= index 0) (< index (length *paste-buffers*)))
      (progn
        (setf *paste-buffers*
              (append (subseq *paste-buffers* 0 index)
                      (subseq *paste-buffers* (1+ index))))
        t)
      nil))

(defun clear-paste-buffers ()
  "Set *paste-buffers* to nil."
  (setf *paste-buffers* nil))
