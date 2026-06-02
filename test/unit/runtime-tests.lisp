(in-package #:cl-tmux/test)

;;;; Runtime state tests (src/runtime.lisp).
;;;; Tests: runtime-suite — global variables, reader-thread CPS, status timer,
;;;; channel synchronisation, and stop-reader-threads shutdown.

(def-suite runtime-suite :description "Runtime state variables and threading utilities")
(in-suite runtime-suite)

(test runtime-globals-exist
  "*running*, *dirty*, *resize-pending*, *term-rows*, *term-cols* are all boundp."
  (is (boundp 'cl-tmux::*running*)        "*running* must be bound")
  (is (boundp 'cl-tmux::*dirty*)          "*dirty* must be bound")
  (is (boundp 'cl-tmux::*resize-pending*) "*resize-pending* must be bound")
  (is (integerp cl-tmux::*term-rows*)     "*term-rows* must be an integer")
  (is (integerp cl-tmux::*term-cols*)     "*term-cols* must be an integer"))

(test pane-reader-loop-is-fbound
  "%pane-reader-loop is a defined function (data/logic separation from start-reader-thread)."
  (is (fboundp 'cl-tmux::%pane-reader-loop)
      "%pane-reader-loop must be fbound"))

(test pane-reader-loop-exits-when-running-nil
  "%pane-reader-loop exits immediately when *running* is NIL.
   This verifies the loop sentinel is checked without needing a real PTY."
  (let ((cl-tmux::*running* nil)
        (cl-tmux::*dirty*   nil)
        (pane (make-pane :id 1 :fd -1 :pid -1 :screen (make-screen 5 3))))
    ;; With *running* = NIL, the loop body is never entered.
    ;; %pane-reader-loop should return without error.
    (finishes (cl-tmux::%pane-reader-loop pane))
    ;; *dirty* must not have been set (no data was read).
    (is-false cl-tmux::*dirty* "*dirty* must remain NIL when loop exits immediately")))

;;; ── stop-reader-threads ──────────────────────────────────────────────────────

(test stop-reader-threads-sets-running-nil
  "stop-reader-threads sets *running* to NIL regardless of thread count."
  (let ((cl-tmux::*running* t))
    (cl-tmux::stop-reader-threads '())
    (is-false cl-tmux::*running* "*running* must be NIL after stop-reader-threads")))

(test stop-reader-threads-empty-list
  "stop-reader-threads is a no-op on an empty thread list (no join attempted)."
  (let ((cl-tmux::*running* t))
    (finishes (cl-tmux::stop-reader-threads '()))
    (is-false cl-tmux::*running*)))

(test stop-reader-threads-joins-already-dead-thread
  "stop-reader-threads tolerates joining a thread that has already exited."
  (let* ((cl-tmux::*running* t)
         ;; Spawn a thread that exits immediately.
         (thread (bordeaux-threads:make-thread
                  (lambda () nil)
                  :name "test-dead-thread")))
    ;; Give the thread a moment to exit.
    (sleep 0.05)
    ;; join-thread on an already-dead thread should not signal.
    (finishes (cl-tmux::stop-reader-threads (list thread)))
    (is-false cl-tmux::*running*)))

;;; ── start-status-timer ───────────────────────────────────────────────────────

(test start-status-timer-returns-a-thread
  "start-status-timer returns a bordeaux thread object."
  (let ((cl-tmux::*running*            t)
        (cl-tmux::*status-timer-thread* nil))
    (let ((thread (cl-tmux::start-status-timer)))
      (unwind-protect
           (progn
             (is-true (bordeaux-threads:threadp thread)
                      "start-status-timer must return a thread")
             (is (eq thread cl-tmux::*status-timer-thread*)
                 "*status-timer-thread* must be set to the returned thread"))
        ;; Shut the timer down cleanly.
        (setf cl-tmux::*running* nil)
        (ignore-errors
          (bordeaux-threads:join-thread thread :timeout 2))))))

(test start-status-timer-is-idempotent
  "Calling start-status-timer a second time while the thread is alive is a no-op."
  (let ((cl-tmux::*running*            t)
        (cl-tmux::*status-timer-thread* nil))
    (let ((thread1 (cl-tmux::start-status-timer))
          (thread2 (cl-tmux::start-status-timer)))
      (unwind-protect
           (is (eq thread1 thread2)
               "second call must return the same thread")
        (setf cl-tmux::*running* nil)
        (ignore-errors
          (bordeaux-threads:join-thread thread1 :timeout 2))))))

;;; ── Reader CPS states (sandbox-safe) ─────────────────────────────────────────

(test reader-eof-state-returns-nil
  "reader-eof-state is the terminal state: it returns NIL for any pane."
  (let ((pane (make-pane :id 1 :fd -1 :pid -1 :screen (make-screen 5 3))))
    (is (null (cl-tmux::reader-eof-state pane))
        "reader-eof-state must return NIL")))

(test start-reader-thread-is-fbound
  "start-reader-thread is a defined function."
  (is (fboundp 'cl-tmux::start-reader-thread)))

;;; ── add-message-log ──────────────────────────────────────────────────────────

(test add-message-log-prepends-entry
  "add-message-log prepends a (timestamp . text) cons and caps the log."
  (let ((cl-tmux::*message-log* nil))
    (cl-tmux::add-message-log "hello")
    (is (= 1 (length cl-tmux::*message-log*)))
    (is (string= "hello" (cdr (first cl-tmux::*message-log*))))))

(test add-message-log-caps-at-max-entries
  "add-message-log caps *message-log* at +max-message-log-entries+ entries."
  (let ((cl-tmux::*message-log* nil)
        (limit cl-tmux::+max-message-log-entries+))
    (dotimes (i (+ limit 5))
      (cl-tmux::add-message-log (format nil "msg-~D" i)))
    (is (= limit (length cl-tmux::*message-log*))
        "*message-log* must not exceed +max-message-log-entries+")))
