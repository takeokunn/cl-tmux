(in-package #:cl-tmux/test)

;;;; Dispatch test suite and shared support macros.

(def-suite dispatch-suite :description "Command dispatch and prefix routing")
(in-suite dispatch-suite)

(defmacro with-copy-mode-active-screen ((session-var screen-var &key feed) &body body)
  "Bind SESSION-VAR and SCREEN-VAR in an active copy-mode session.
   Optional FEED seeds the screen before BODY runs."
  `(with-fake-session (,session-var)
     (cl-tmux::dispatch-command ,session-var :copy-mode-enter nil)
     (let ((,screen-var (active-screen ,session-var)))
       ,@(when feed `((feed ,screen-var ,feed)))
       ,@body)))

(defmacro with-mocked-respawn-pane ((calls-var reader-calls-var) &body body)
  "Execute BODY with cl-tmux/model:respawn-pane and cl-tmux::start-reader-thread
   replaced by recording stubs.  Restores originals via unwind-protect.
   CALLS-VAR -- list of (session pane start-dir default-command extra-env) tuples,
               accumulated in call order (oldest first).
   READER-CALLS-VAR -- list of pane arguments passed to start-reader-thread,
                      accumulated in call order (oldest first)."
  (let ((orig-respawn (gensym "ORIG-RESPAWN"))
        (orig-reader  (gensym "ORIG-READER")))
    `(let* ((,calls-var nil)
            (,reader-calls-var nil)
            (,orig-respawn (fdefinition 'cl-tmux/model:respawn-pane))
            (,orig-reader  (fdefinition 'cl-tmux::start-reader-thread)))
       (unwind-protect
           (progn
             (setf (fdefinition 'cl-tmux/model:respawn-pane)
                   (lambda (session pane &key start-dir default-command extra-env)
                     (setf ,calls-var
                           (append ,calls-var
                                   (list (list session pane start-dir
                                               default-command extra-env))))
                     pane))
             (setf (fdefinition 'cl-tmux::start-reader-thread)
                   (lambda (pane)
                     (setf ,reader-calls-var (append ,reader-calls-var (list pane)))))
             ,@body)
         (setf (fdefinition 'cl-tmux/model:respawn-pane) ,orig-respawn)
         (setf (fdefinition 'cl-tmux::start-reader-thread) ,orig-reader)))))

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
