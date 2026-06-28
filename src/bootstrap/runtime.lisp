(in-package #:cl-tmux)

;;;; Runtime state and per-pane I/O threading.
;;;;
;;;; Threading model:
;;;;   * One reader thread per pane: blocking read(PTY fd) -> pane-feed ->
;;;;     screen update -> sets *dirty* T.
;;;;   * Main thread (see events.lisp): select(stdin, 50 ms) -> key dispatch or
;;;;     PTY forward -> render when *dirty*.
;;;;
;;;; PTY children may be spawned while reader/status threads are active, so
;;;; teardown must reliably join background threads and close pane processes.

;;; -- Shared state -----------------------------------------------------------

(defvar *dirty*   t   "Set by reader threads; cleared by the main render step.")
(defvar *running* t   "Loop sentinel; set nil by :detach command.")
(defvar *resize-pending* nil
  "Set by the SIGWINCH handler; the event loop relayouts once and clears it.")
(defvar *term-rows* 24
  "Current terminal height in rows; updated on SIGWINCH and at startup.
   Used by the renderer, pane-split, and resize logic throughout the codebase.")
(defvar *term-cols* 80
  "Current terminal width in columns; updated on SIGWINCH and at startup.
   Used by the renderer, pane-split, and resize logic throughout the codebase.")
(defvar *server-sessions* nil
  "Alist mapping session-name (string) to session object for the running server.")
(defvar *server-marked-pane* nil
  "The single server-wide marked pane (set by mark-pane / select-pane -m).
   NIL when no pane is marked.  Mirrors tmux's global marked-pane singleton.")
(defvar *client-read-only* nil
  "When non-NIL the attached client is read-only: keystrokes and mouse events
   are NOT forwarded to panes.  Set by attach-session -r.")
(defvar *status-timer* nil "Background thread for status-interval redraws.")

;;; -- Named constants --------------------------------------------------------

(defconstant +max-message-log-entries+ 100
  "Maximum number of entries retained in *message-log*.")

(defconstant +reader-thread-join-timeout+ 10
  "Seconds (real number) to wait for a PTY reader thread to terminate before
   giving up when the Lisp implementation supports bounded thread joins.")

(defun %join-thread-with-timeout (thread &optional (timeout +reader-thread-join-timeout+))
  "Join THREAD with a bounded wait when available.

   Bordeaux Threads does not standardize a timeout argument to JOIN-THREAD; SBCL
   provides one on SB-THREAD:JOIN-THREAD.  On other implementations, poll for the
   thread to exit and only call the portable join once it is already dead."
  #+sbcl
  (sb-thread:join-thread thread :timeout timeout)
  #-sbcl
  (let ((deadline (+ (get-internal-real-time)
                     (round (* timeout internal-time-units-per-second)))))
    (loop while (and (bordeaux-threads:thread-alive-p thread)
                     (< (get-internal-real-time) deadline))
          do (sleep 0.01))
    (unless (bordeaux-threads:thread-alive-p thread)
      (bordeaux-threads:join-thread thread))))

(defconstant +wait-for-channel-timeout+ 30
  "Seconds before wait-for-channel gives up waiting for a signal.
   A bounded wait prevents indefinite blocking when signal-channel is
   never called (e.g., after an unexpected server shutdown).")

(defconstant +default-display-time-ms+ 750
  "Default overlay display time in milliseconds when the display-time option is
   unset.  The status timer checks every +status-timer-poll-seconds+, so actual
   dismiss may lag up to that granularity.")

(defconstant +ms-per-second+ 1000.0
  "Milliseconds per second; used to convert display-time (ms) to seconds.")

;;; -- Wait-for channel synchronization ----------------------------------------

(defparameter *wait-channels* (make-hash-table :test #'equal)
  "Maps channel-name string to a plist (:lock lock :cv cv :locked bool).")

(defmacro with-channel-plist ((lk cv ch) &body body)
  "Bind LK and CV to the :lock and :cv fields of the channel plist CH."
  (let ((ch-var (gensym "CH")))
    `(let* ((,ch-var ,ch)
            (,lk (getf ,ch-var :lock))
            (,cv (getf ,ch-var :cv)))
       ,@body)))

(defun %ensure-channel (name)
  "Return the plist for channel NAME, creating it if absent."
  (or (gethash name *wait-channels*)
      (let* ((lk (make-lock (format nil "wf-~A" name)))
             (cv (make-condition-variable :name (format nil "wf-cv-~A" name)))
             (ch (list :lock lk :cv cv :locked nil)))
        (setf (gethash name *wait-channels*) ch)
        ch)))

(defun wait-for-channel (name)
  "Block the calling thread until channel NAME is signaled, or until
   +wait-for-channel-timeout+ seconds elapse.  Returns T if signaled, NIL
   on timeout.  A bounded wait prevents indefinite blocking when the
   corresponding signal-channel is never called."
  (with-channel-plist (lk cv (%ensure-channel name))
    (with-lock-held (lk)
      (condition-wait cv lk :timeout +wait-for-channel-timeout+))))

(defun signal-channel (name)
  "Signal all threads blocked on channel NAME."
  (let ((ch (%ensure-channel name)))
    (unless (getf ch :locked)
      (with-channel-plist (lk cv ch)
        (with-lock-held (lk)
          (condition-notify cv))))))

(defun lock-channel (name)
  "Lock channel NAME so signal-channel is suppressed (a no-op) until unlocked.
   While a channel is locked, any call to signal-channel for the same NAME
   checks the :locked flag and skips the condition-notify entirely.  This
   allows callers to temporarily block notifications without losing them
   permanently — the channel is not destroyed, only silenced."
  (let ((ch (%ensure-channel name)))
    (setf (getf ch :locked) t)))

(defun unlock-channel (name)
  "Unlock channel NAME, allowing subsequent signal-channel calls to notify waiters.
   Paired with lock-channel: once unlocked, signal-channel will again call
   condition-notify on the channel's condition variable.  Does not retroactively
   deliver signals that were suppressed while the channel was locked."
  (let ((ch (%ensure-channel name)))
    (setf (getf ch :locked) nil)))

(defun %cap-list (list limit)
  "Return LIST truncated to at most LIMIT elements; returns LIST unchanged when
   it already fits."
  (if (> (length list) limit) (subseq list 0 limit) list))

;;; -- Message log -------------------------------------------------------------

(defvar *message-log* nil
  "A list of (timestamp . text) cons pairs for :show-messages.")

(defvar *current-client-conn* nil
  "The client connection currently being served by the server-side command path,
   or NIL when running commands without a specific client context.")

(defun %message-log-limit ()
  "The effective message-log cap: the `message-limit` option (tmux default 1000),
   falling back to +max-message-log-entries+ when unset."
  (or (cl-tmux/options:get-option "message-limit")
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

(defun save-prompt-history ()
  "Write *prompt-history* to the history-file, one entry per line, OLDEST first
   (so a later load preserves recency order).  No-op when history-file is unset;
   best-effort (I/O errors are ignored)."
  (let ((path (%prompt-history-path)))
    (when path
      (ignore-errors
        (with-open-file (s path :direction :output :if-exists :supersede
                                :if-does-not-exist :create)
          (dolist (entry (reverse *prompt-history*))
            (write-line entry s)))))))

(defun %effective-prompt-history-limit ()
  "The effective command-prompt history cap: the `prompt-history-limit` option
   (tmux default 100), falling back to +max-prompt-history+ when unset."
  (or (cl-tmux/options:get-option "prompt-history-limit") +max-prompt-history+))

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
  (let ((path (%prompt-history-path)))
    (when (and path (probe-file path))
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

;;; -- Clock mode --------------------------------------------------------------

(defvar *clock-mode-pane-id* nil
  "When non-NIL, the pane-id of the pane displaying a digital clock overlay.")

;;; NOTE: popup, menu structs, *active-popup*, *active-menu* live in
;;; src/prompt.lisp (cl-tmux/prompt package) so the renderer can see them.

;;; -- SIGWINCH ---------------------------------------------------------------

(defun install-sigwinch-handler ()
  "Arm SIGWINCH so terminal resizes flag a one-shot relayout."
  (sb-sys:enable-interrupt
   sb-unix:sigwinch
   (lambda (&rest ignored)
     (declare (ignore ignored))
     (setf *resize-pending* t
           *dirty*           t))))
