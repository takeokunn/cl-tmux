(in-package #:cl-tmux/buffer)

(defvar *paste-buffers* nil
  "List of paste buffer strings, most recent first.")

(defun add-paste-buffer (text)
  "Push TEXT onto *paste-buffers*, keeping at most 50 entries. Return TEXT."
  (push text *paste-buffers*)
  (when (> (length *paste-buffers*) 50)
    (setf *paste-buffers* (subseq *paste-buffers* 0 50)))
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
