(in-package #:cl-tmux)

;;;; Runtime state and per-pane I/O threading.
;;;;
;;;; Threading model:
;;;;   * One reader thread per pane: blocking read(PTY fd) -> pane-feed ->
;;;;     screen update -> sets *dirty* T.
;;;;   * Main thread (see events.lisp): select(stdin, 50 ms) -> key dispatch or
;;;;     PTY forward -> render when *dirty*.
;;;;
;;;; All PTY children are forked before any reader threads start (see main.lisp)
;;;; to avoid fork-in-multithreaded-process hazards.

;;; -- Shared state -----------------------------------------------------------

(defvar *dirty*   t   "Set by reader threads; cleared by the main render step.")
(defvar *running* t   "Loop sentinel; set nil by :detach command.")
(defvar *resize-pending* nil
  "Set by the SIGWINCH handler; the event loop relayouts once and clears it.")
(defvar *term-rows* 24)
(defvar *term-cols* 80)
(defvar *server-sessions* nil
  "Alist mapping session-name (string) to session object for the running server.")

;;; -- Named constants --------------------------------------------------------

(defconstant +max-message-log-entries+ 100
  "Maximum number of entries retained in *message-log*.")

(defconstant +reader-thread-join-timeout+ 10
  "Seconds to wait for a PTY reader thread to terminate before giving up.")

;;; -- Status-bar timer -------------------------------------------------------

(defparameter *status-dirty* nil
  "Set by the status-bar timer thread to trigger a status-bar repaint.")

(defvar *status-timer-thread* nil
  "Thread object for the status-bar interval timer, or NIL if not started.")

(defun %mark-status-dirty! ()
  "Named action: mark both the status bar and the frame as needing a repaint."
  (setf *status-dirty* t
        *dirty*        t))

(defun start-status-timer ()
  "Start a background thread that sets *STATUS-DIRTY* every STATUS-INTERVAL seconds.
   Only one timer thread runs at a time; calling this when one is already
   running is a no-op.  Returns the thread object."
  (when (and *status-timer-thread*
             (bordeaux-threads:thread-alive-p *status-timer-thread*))
    (return-from start-status-timer *status-timer-thread*))
  (setf *status-timer-thread*
        (make-thread
         (lambda ()
           (loop while *running* do
             (let ((interval (cl-tmux/options:get-option "status-interval" 15)))
               (sleep (max 1 interval)))
             (%mark-status-dirty!)))
         :name "cl-tmux-status-timer"))
  *status-timer-thread*)

;;; -- Wait-for channel synchronization ----------------------------------------

(defparameter *wait-channels* (make-hash-table :test #'equal)
  "Maps channel-name string to a plist (:lock lock :cv cv :locked bool).")

(defun %ensure-channel (name)
  "Return the plist for channel NAME, creating it if absent."
  (or (gethash name *wait-channels*)
      (let* ((lk (make-lock (format nil "wf-~A" name)))
             (cv (make-condition-variable :name (format nil "wf-cv-~A" name)))
             (ch (list :lock lk :cv cv :locked nil)))
        (setf (gethash name *wait-channels*) ch)
        ch)))

(defun wait-for-channel (name)
  "Block the calling thread until channel NAME is signaled."
  (let* ((ch (%ensure-channel name))
         (lk (getf ch :lock))
         (cv (getf ch :cv)))
    (with-lock-held (lk)
      (condition-wait cv lk))))

(defun signal-channel (name)
  "Signal all threads blocked on channel NAME."
  (let* ((ch (%ensure-channel name))
         (lk (getf ch :lock))
         (cv (getf ch :cv)))
    (unless (getf ch :locked)
      (with-lock-held (lk)
        (condition-notify cv)))))

(defun lock-channel (name)
  "Lock channel NAME so signal-channel is a no-op until unlocked."
  (let ((ch (%ensure-channel name)))
    (setf (getf ch :locked) t)))

(defun unlock-channel (name)
  "Unlock channel NAME, allowing signal-channel to proceed."
  (let ((ch (%ensure-channel name)))
    (setf (getf ch :locked) nil)))

;;; -- Message log -------------------------------------------------------------

(defvar *message-log* nil
  "A list of (timestamp . text) cons pairs for :show-messages.")

(defun add-message-log (msg)
  "Prepend MSG to *message-log*, capping the list at +max-message-log-entries+."
  (push (cons (get-universal-time) msg) *message-log*)
  (when (> (length *message-log*) +max-message-log-entries+)
    (setf *message-log* (subseq *message-log* 0 +max-message-log-entries+))))

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

;;; -- PTY reader thread ------------------------------------------------------
;;;
;;; CPS state machine: each state function takes (pane) and returns the next
;;; state function (or NIL to stop).

(defun reader-idle-state (pane)
  "Poll the pane PTY fd; transition to reading if data is available."
  (if (select-fds (list (pane-fd pane)) +pty-poll-timeout-us+)
      #'reader-reading-state
      #'reader-idle-state))

(defun reader-reading-state (pane)
  "Read one PTY chunk and feed it to PANE; transition to eof if EOF."
  (let ((bytes (pty-read-blocking (pane-fd pane) +pty-buf-size+)))
    (cond
      ((null bytes)
       #'reader-eof-state)
      (t
       (when (pane-pipe-fd pane)
         (pipe-pane-write pane bytes))
       (pane-feed pane bytes)
       (setf *dirty* t)
       #'reader-idle-state))))

(defun reader-eof-state (pane)
  "Terminal state: EOF received, stop the reader loop."
  (declare (ignore pane))
  nil)

(defun %run-reader-states (pane initial-state)
  "Drive the CPS reader state machine for PANE starting from INITIAL-STATE."
  (loop for state = initial-state then (funcall state pane)
        while (and *running* state)))

(defun %pane-reader-loop (pane)
  "Feed PTY output into PANE screen until EOF or *running* becomes NIL."
  (%run-reader-states pane #'reader-idle-state))

(defun start-reader-thread (pane)
  "Spawn a thread running %pane-reader-loop for PANE."
  (make-thread (lambda () (%pane-reader-loop pane))
               :name (format nil "pty-reader-~D" (pane-id pane))))

(defun stop-reader-threads (threads)
  "Signal shutdown and join each thread in THREADS with a bounded timeout."
  (setf *running* nil)
  (dolist (thread threads)
    (ignore-errors
      (bordeaux-threads:join-thread thread
                                    :timeout +reader-thread-join-timeout+))))
