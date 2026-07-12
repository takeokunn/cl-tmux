(in-package #:cl-tmux)

;;; -- Message log -------------------------------------------------------------

(defconstant +max-message-log-entries+ 100
  "Maximum number of entries retained in *message-log*.")

(defvar *message-log* nil
  "A list of (timestamp . text) cons pairs for :show-messages.")

(defvar *current-client-conn* nil
  "The client connection currently being served by the server-side command path,
   or NIL when running commands without a specific client context.")

(defun %option-or-default (option default)
  "Return OPTION when set, otherwise DEFAULT."
  (or option default))

(defun %message-log-limit ()
  "The effective message-log cap: the `message-limit` option (tmux default 1000),
   falling back to +max-message-log-entries+ when unset."
  (%option-or-default (cl-tmux/options:get-option "message-limit")
                      +max-message-log-entries+))

(defun %append-message-log-entry (log entry)
  "Prepend ENTRY to LOG and cap the result at the effective message-log limit."
  (%cap-list (cons entry log) (%message-log-limit)))

(defun add-message-log (msg)
  "Prepend MSG to *message-log*, capping the list at the `message-limit` option
   (tmux default 1000), falling back to +max-message-log-entries+ when unset."
  (let ((entry (cons (get-universal-time) msg)))
    (setf *message-log* (%append-message-log-entry *message-log* entry))
    (when *current-client-conn*
      (setf (client-conn-message-log *current-client-conn*)
            (%append-message-log-entry
             (client-conn-message-log *current-client-conn*)
             entry)))))

;;; -- Prompt history ----------------------------------------------------------

(defconstant +max-prompt-history+ 100
  "Maximum number of entries retained in *prompt-history*.")

(defvar *prompt-history* nil
  "A list of strings — the most recent command-prompt inputs, newest first.
   Populated by the :command-prompt handler; shown by :show-prompt-history.")

(defun %prompt-history-path ()
  "The configured history-file path (a non-empty string) or NIL when unset —
   NIL means command-prompt history is in-memory only (no persistence)."
  (let ((p (ignore-errors (cl-tmux/options:get-option "history-file"))))
    (and (stringp p) (plusp (length p)) p)))

(defmacro %with-prompt-history-path ((path) &body body)
  "Evaluate BODY with PATH bound to the configured history-file path when present."
  `(let ((,path (%prompt-history-path)))
     (when ,path
       ,@body)))

(defun save-prompt-history ()
  "Write *prompt-history* to the history-file, one entry per line, OLDEST first
   (so a later load preserves recency order).  No-op when history-file is unset;
   best-effort (I/O errors are ignored)."
  (%with-prompt-history-path (path)
    (ignore-errors
      (with-open-file (s path :direction :output :if-exists :supersede
                              :if-does-not-exist :create)
        (dolist (entry (reverse *prompt-history*))
          (write-line entry s))))))

(defun %effective-prompt-history-limit ()
  "The effective command-prompt history cap: the `prompt-history-limit` option
   (tmux default 100), falling back to +max-prompt-history+ when unset."
  (%option-or-default (cl-tmux/options:get-option "prompt-history-limit")
                      +max-prompt-history+))

(defun %read-history-lines (stream)
  "Read all non-empty lines from STREAM and return them newest-first (reversed).
   The file stores entries oldest-first; reversing during read yields newest-first
   in memory.  Pure stream reader — no global state mutation."
  (let ((entries nil))
    (loop for line = (read-line stream nil nil) while line
          do (when (plusp (length line)) (push line entries)))
    entries))

(defun load-prompt-history ()
  "Load *prompt-history* from the history-file (one entry per line, oldest first),
   newest-first in memory, capped at +max-prompt-history+.  No-op when the option
   is unset or the file is unreadable."
  (%with-prompt-history-path (path)
    (when (probe-file path)
      (ignore-errors
        (with-open-file (stream path :direction :input :if-does-not-exist nil)
          (when stream
            (let ((entries (%read-history-lines stream))
                  (limit   (%effective-prompt-history-limit)))
              (setf *prompt-history*
                    (subseq entries 0 (min (length entries) limit))))))))))

(defun add-prompt-history (entry)
  "Prepend ENTRY to *prompt-history*, capping at the prompt-history-limit option,
   and persist to the history-file when that option is set."
  (when (and (stringp entry) (plusp (length entry)))
    (push entry *prompt-history*)
    (let ((limit (%effective-prompt-history-limit)))
      (setf *prompt-history* (%cap-list *prompt-history* limit)))
    (save-prompt-history)))
