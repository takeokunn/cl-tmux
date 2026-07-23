(in-package #:cl-tmux/test)

;;;; Dispatch test suite and shared support macros.
;;;;
;;;; This file defines no tests of its own — it only provides the shared
;;;; helper macros used by the dispatch-suite family of test files (some
;;;; still on the FiveAM-compat shim, some converted to cl-weave).  The
;;;; `dispatch-suite` symbol itself is auto-vivified by whichever sibling
;;;; file's `(in-suite dispatch-suite)` runs first, so no `def-suite`/
;;;; `in-suite` form is needed here.

(defmacro with-copy-mode-active-screen ((session-var screen-var &key feed) &body body)
  "Bind SESSION-VAR and SCREEN-VAR in an active copy-mode session.
   Optional FEED seeds the screen before BODY runs."
  `(with-fake-session (,session-var)
     (cl-tmux::dispatch-command ,session-var :copy-mode-enter nil)
     (let ((,screen-var (active-screen ,session-var)))
       ,@(when feed `((feed ,screen-var ,feed)))
       ,@body)))

(defmacro with-mocked-respawn-pane ((respawn-mock-var reader-mock-var) &body body)
  "Execute BODY with cl-tmux/model:respawn-pane and cl-tmux::start-reader-thread
   replaced by cl-weave mock functions, bound to RESPAWN-MOCK-VAR and
   READER-MOCK-VAR (originals restored automatically on exit via
   cl-weave:with-mocked-functions).  Query call history with
   (mock-calls respawn-mock-var) / (mock-calls reader-mock-var) — each entry
   is the full argument list as called, e.g. for respawn-pane:
   (session pane :start-dir \"...\" :default-command \"...\" :extra-env (...))."
  `(let ((,respawn-mock-var
          (make-mock-function
           (lambda (session pane &key start-dir default-command extra-env)
             (declare (ignore session start-dir default-command extra-env))
             pane)))
         (,reader-mock-var
          (make-mock-function (lambda (pane) (declare (ignore pane))))))
     (with-mocked-functions
         (((fdefinition 'cl-tmux/model:respawn-pane) ,respawn-mock-var)
          ((fdefinition 'cl-tmux::start-reader-thread) ,reader-mock-var))
       ,@body)))

(defmacro with-stubbed-switch-to-session ((target-var) &body body)
  "Execute BODY with cl-tmux::%switch-to-session replaced by a stub that
   records its TARGET argument into TARGET-VAR.  Restores the original
   function via unwind-protect."
  (let ((orig (gensym "ORIG-SWITCH-TO-SESSION")))
    `(let* ((,target-var nil)
            (,orig (fdefinition 'cl-tmux::%switch-to-session)))
       (unwind-protect
           (progn
             (setf (fdefinition 'cl-tmux::%switch-to-session)
                   (lambda (target) (setf ,target-var target)))
             ,@body)
         (setf (fdefinition 'cl-tmux::%switch-to-session) ,orig)))))
