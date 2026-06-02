(in-package #:cl-tmux/test)

;;;; Runtime state tests (src/runtime.lisp).
;;;; Tests: runtime-suite — global variables, reader-thread CPS, status timer,
;;;; channel synchronisation, and stop-reader-threads shutdown.

(def-suite runtime-suite :description "Runtime state variables and threading utilities")
(in-suite runtime-suite)

;;; ── Global variables exist and have sensible types ───────────────────────────

(test runtime-globals-exist
  :description "*running*, *dirty*, *resize-pending*, *term-rows*, *term-cols* are all boundp."
  (is (boundp 'cl-tmux::*running*)        "*running* must be bound")
  (is (boundp 'cl-tmux::*dirty*)          "*dirty* must be bound")
  (is (boundp 'cl-tmux::*resize-pending*) "*resize-pending* must be bound")
  (is (integerp cl-tmux::*term-rows*)     "*term-rows* must be an integer")
  (is (integerp cl-tmux::*term-cols*)     "*term-cols* must be an integer"))

(test runtime-term-rows-positive
  :description "*term-rows* default is a positive integer (at least 1 row)."
  (is (plusp cl-tmux::*term-rows*) "*term-rows* must be a positive integer, got ~D"
      cl-tmux::*term-rows*))

(test runtime-term-cols-positive
  :description "*term-cols* default is a positive integer (at least 1 column)."
  (is (plusp cl-tmux::*term-cols*) "*term-cols* must be a positive integer, got ~D"
      cl-tmux::*term-cols*))

(test runtime-max-message-log-entries-is-constant
  :description "+max-message-log-entries+ is a positive integer constant."
  (is (constantp '+max-message-log-entries+) "+max-message-log-entries+ must be a constant")
  (is (integerp cl-tmux::+max-message-log-entries+) "constant must be an integer")
  (is (plusp cl-tmux::+max-message-log-entries+) "constant must be positive"))

(test runtime-reader-thread-join-timeout-is-constant
  :description "+reader-thread-join-timeout+ is a positive integer constant."
  (is (integerp cl-tmux::+reader-thread-join-timeout+) "join timeout must be an integer")
  (is (plusp cl-tmux::+reader-thread-join-timeout+)    "join timeout must be positive"))

;;; ── %pane-reader-loop ────────────────────────────────────────────────────────

(test pane-reader-loop-is-fbound
  :description "%pane-reader-loop is a defined function (data/logic separation from start-reader-thread)."
  (is (fboundp 'cl-tmux::%pane-reader-loop)
      "%pane-reader-loop must be fbound"))

(test pane-reader-loop-exits-when-running-nil
  :description "%pane-reader-loop exits immediately when *running* is NIL without error."
  (let ((cl-tmux::*running* nil)
        (cl-tmux::*dirty*   nil)
        (pane (make-pane :id 1 :fd -1 :pid -1 :screen (make-screen 5 3))))
    (finishes (cl-tmux::%pane-reader-loop pane)
              "%pane-reader-loop must return cleanly when *running* is NIL")
    (is-false cl-tmux::*dirty* "*dirty* must remain NIL when loop exits immediately")))

;;; ── CPS reader states ────────────────────────────────────────────────────────

(test reader-eof-state-returns-nil
  :description "reader-eof-state is the terminal state: it returns NIL for any pane."
  (let ((pane (make-pane :id 1 :fd -1 :pid -1 :screen (make-screen 5 3))))
    (is (null (cl-tmux::reader-eof-state pane))
        "reader-eof-state must return NIL")))

(test reader-idle-state-is-fbound
  :description "reader-idle-state is a defined function (CPS idle→select state)."
  (is (fboundp 'cl-tmux::reader-idle-state)
      "reader-idle-state must be fbound"))

(test reader-reading-state-is-fbound
  :description "reader-reading-state is a defined function (CPS read→feed state)."
  (is (fboundp 'cl-tmux::reader-reading-state)
      "reader-reading-state must be fbound"))

(test run-reader-states-is-fbound
  :description "%run-reader-states is a defined function (CPS state-machine driver)."
  (is (fboundp 'cl-tmux::%run-reader-states)
      "%run-reader-states must be fbound"))

(test run-reader-states-exits-when-running-nil
  :description "%run-reader-states exits immediately when *running* is NIL, even
given a non-NIL initial state (loop while *running*)."
  (let* ((cl-tmux::*running* nil)
         (pane (make-pane :id 1 :fd -1 :pid -1 :screen (make-screen 5 3)))
         ;; A state function that should never be called.
         (boom (lambda (_p) (declare (ignore _p))
                 (error "state function called despite *running*=NIL"))))
    (finishes (cl-tmux::%run-reader-states pane boom)
              "%run-reader-states must exit immediately when *running* is NIL")))

;;; ── stop-reader-threads ──────────────────────────────────────────────────────

(test stop-reader-threads-sets-running-nil
  :description "stop-reader-threads sets *running* to NIL regardless of thread count."
  (let ((cl-tmux::*running* t))
    (cl-tmux::stop-reader-threads '())
    (is-false cl-tmux::*running* "*running* must be NIL after stop-reader-threads")))

(test stop-reader-threads-empty-list
  :description "stop-reader-threads is a no-op on an empty thread list (no join attempted)."
  (let ((cl-tmux::*running* t))
    (finishes (cl-tmux::stop-reader-threads '())
              "stop-reader-threads with empty list must not signal")
    (is-false cl-tmux::*running*)))

(test stop-reader-threads-joins-already-dead-thread
  :description "stop-reader-threads tolerates joining a thread that has already exited."
  (let* ((cl-tmux::*running* nil)
         (thread (bordeaux-threads:make-thread
                  (lambda ()
                    (loop while cl-tmux::*running* do (sleep 0.001)))
                  :name "test-dead-thread")))
    (sleep 0.05)
    (finishes (cl-tmux::stop-reader-threads (list thread))
              "stop-reader-threads must not signal when joining a dead thread")
    (is-false cl-tmux::*running*)))

;;; ── start-reader-thread ──────────────────────────────────────────────────────

(test start-reader-thread-is-fbound
  :description "start-reader-thread is a defined function."
  (is (fboundp 'cl-tmux::start-reader-thread)))

;;; ── start-status-timer ───────────────────────────────────────────────────────

(test start-status-timer-returns-a-thread
  :description "start-status-timer creates a bordeaux thread and returns it."
  (let ((cl-tmux::*running*            nil)
        (cl-tmux::*status-timer-thread* nil))
    (let ((thread (cl-tmux::start-status-timer)))
      (is-true (bordeaux-threads:threadp thread)
               "start-status-timer must return a thread")
      (is (eq thread cl-tmux::*status-timer-thread*)
          "*status-timer-thread* must be set to the returned thread")
      (ignore-errors
        (bordeaux-threads:join-thread thread :timeout 2)))))

(test start-status-timer-stores-thread-in-global
  :description "start-status-timer stores the thread object in *status-timer-thread*."
  (let ((cl-tmux::*running*            nil)
        (cl-tmux::*status-timer-thread* nil))
    (let ((thread (cl-tmux::start-status-timer)))
      (is (eq thread cl-tmux::*status-timer-thread*)
          "returned thread must match *status-timer-thread*")
      (ignore-errors
        (bordeaux-threads:join-thread thread :timeout 1)))))

(test start-status-timer-idempotent-when-alive
  :description "Calling start-status-timer twice returns the same thread object
when the first thread is still alive."
  (let ((cl-tmux::*running* t)
        (cl-tmux::*status-timer-thread* nil))
    (let* ((t1 (cl-tmux::start-status-timer))
           (t2 (cl-tmux::start-status-timer)))
      (is (eq t1 t2) "second call must return the same live thread")
      ;; Clean up: stop the timer thread.
      (setf cl-tmux::*running* nil)
      (ignore-errors (bordeaux-threads:join-thread t1 :timeout 3)))))

;;; ── %mark-status-dirty! ──────────────────────────────────────────────────────

(test mark-status-dirty-sets-both-flags
  :description "%mark-status-dirty! sets both *status-dirty* and *dirty* to T."
  (let ((cl-tmux::*status-dirty* nil)
        (cl-tmux::*dirty* nil))
    (cl-tmux::%mark-status-dirty!)
    (is-true cl-tmux::*status-dirty* "%mark-status-dirty! must set *status-dirty*")
    (is-true cl-tmux::*dirty*        "%mark-status-dirty! must set *dirty*")))

;;; ── add-message-log ──────────────────────────────────────────────────────────

(test add-message-log-prepends-entry
  :description "add-message-log prepends a (timestamp . text) cons and caps the log."
  (let ((cl-tmux::*message-log* nil))
    (cl-tmux::add-message-log "hello")
    (is (= 1 (length cl-tmux::*message-log*))
        "log should have 1 entry after one add-message-log")
    (is (string= "hello" (cdr (first cl-tmux::*message-log*)))
        "log entry text must match what was added")))

(test add-message-log-caps-at-max-entries
  :description "add-message-log caps *message-log* at +max-message-log-entries+ entries."
  (let ((cl-tmux::*message-log* nil)
        (limit cl-tmux::+max-message-log-entries+))
    (dotimes (i (+ limit 5))
      (cl-tmux::add-message-log (format nil "msg-~D" i)))
    (is (= limit (length cl-tmux::*message-log*))
        "*message-log* must not exceed +max-message-log-entries+, got ~D"
        (length cl-tmux::*message-log*))))

(test add-message-log-newest-first
  :description "add-message-log prepends: the most recently added entry is first."
  (let ((cl-tmux::*message-log* nil))
    (cl-tmux::add-message-log "first")
    (cl-tmux::add-message-log "second")
    (is (string= "second" (cdr (first cl-tmux::*message-log*)))
        "newest entry must be first in the log")
    (is (string= "first" (cdr (second cl-tmux::*message-log*)))
        "older entry must be second in the log")))

(test add-message-log-entry-has-timestamp
  :description "Each log entry has a non-zero timestamp (from get-universal-time)."
  (let ((cl-tmux::*message-log* nil))
    (cl-tmux::add-message-log "timed")
    (let ((ts (car (first cl-tmux::*message-log*))))
      (is (integerp ts) "log entry timestamp must be an integer")
      (is (plusp ts)    "log entry timestamp must be positive"))))

;;; ── Wait-for channel synchronization ─────────────────────────────────────────

(test ensure-channel-creates-entry
  :description "%ensure-channel creates a plist with :lock and :cv keys."
  (let ((cl-tmux::*wait-channels* (make-hash-table :test #'equal)))
    (let ((ch (cl-tmux::%ensure-channel "test-ch")))
      (is-true ch "%ensure-channel must return a plist")
      (is-true (getf ch :lock) "channel plist must have :lock")
      (is-true (getf ch :cv)   "channel plist must have :cv"))))

(test ensure-channel-is-idempotent
  :description "%ensure-channel returns the same plist for the same channel name."
  (let ((cl-tmux::*wait-channels* (make-hash-table :test #'equal)))
    (let ((ch1 (cl-tmux::%ensure-channel "idem"))
          (ch2 (cl-tmux::%ensure-channel "idem")))
      (is (eq ch1 ch2)
          "%ensure-channel must return the same plist on repeated calls"))))

(test lock-and-unlock-channel-toggle-flag
  :description "lock-channel sets :locked T; unlock-channel sets :locked NIL."
  (let ((cl-tmux::*wait-channels* (make-hash-table :test #'equal)))
    (cl-tmux::lock-channel "lk-test")
    (let ((ch (cl-tmux::%ensure-channel "lk-test")))
      (is-true (getf ch :locked) "lock-channel must set :locked to T"))
    (cl-tmux::unlock-channel "lk-test")
    (let ((ch (cl-tmux::%ensure-channel "lk-test")))
      (is-false (getf ch :locked) "unlock-channel must set :locked to NIL"))))

(test signal-channel-locked-is-noop
  :description "signal-channel does not error when the channel is locked."
  (let ((cl-tmux::*wait-channels* (make-hash-table :test #'equal)))
    (cl-tmux::lock-channel "sig-locked")
    ;; signal-channel on a locked channel is a no-op (no cv-notify) — must not signal.
    (finishes (cl-tmux::signal-channel "sig-locked")
              "signal-channel on a locked channel must not signal an error")))
