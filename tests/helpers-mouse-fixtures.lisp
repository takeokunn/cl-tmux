(in-package #:cl-tmux/test)

;;;; Mouse fixture helpers.

(defmacro with-mouse-option ((mouse) &body body)
  "Run BODY with the session mouse option set to MOUSE, then restore NIL.
   This keeps mouse-enabled and mouse-disabled tests symmetric."
  `(unwind-protect
       (progn
         (cl-tmux/options:set-option "mouse" ,mouse)
         ,@body)
     (cl-tmux/options:set-option "mouse" nil)))

(defmacro with-two-pane-mouse-session ((sess-var win-var p0-var p1-var
                                        &key (mouse t))
                                       &body body)
  "Bind SESS-VAR WIN-VAR P0-VAR P1-VAR to a 2-pane horizontal split session
   suitable for mouse event tests: p0 (x=0 w=40) | p1 (x=41 w=40), window 81x24.
   Enables the 'mouse' session option for the duration of BODY, then restores it.
   BODY runs inside WITH-LOOP-STATE with *term-rows*=25 and *term-cols*=81."
  `(let* ((,p0-var  (make-pane :id 1 :fd -1 :pid -1
                                :x 0 :y 0 :width 40 :height 24
                                :screen (make-screen 40 24)))
          (,p1-var  (make-pane :id 2 :fd -1 :pid -1
                                :x 41 :y 0 :width 40 :height 24
                                :screen (make-screen 40 24)))
          (,win-var (make-window :id 1 :name "w" :width 81 :height 24
                                 :panes (list ,p0-var ,p1-var)
                                 :tree  (make-layout-split :h
                                           (make-layout-leaf ,p0-var)
                                           (make-layout-leaf ,p1-var)
                                           1/2)
                                 :active ,p0-var))
          (,sess-var (make-session :id 1 :name "0"
                                   :windows (list ,win-var) :active ,win-var)))
     (with-mouse-option (,mouse)
       (with-loop-state
         (let ((cl-tmux::*term-rows* 25) (cl-tmux::*term-cols* 81))
           ,@body)))))

(defmacro with-single-pane-mouse-session ((sess-var win-var p0-var &key (mouse t))
                                          &body body)
  "1-pane session (40×24) with optional MOUSE state; restores mouse=nil via
   unwind-protect. BODY runs inside WITH-LOOP-STATE with *term-rows*=25 and
   *term-cols*=40."
  `(let* ((,p0-var  (make-no-pty-pane 1 0 0 40 24))
          (,win-var (make-window :id 1 :name "w" :width 40 :height 24
                                 :panes (list ,p0-var)
                                 :tree  (make-layout-leaf ,p0-var)
                                 :active ,p0-var))
          (,sess-var (make-session :id 1 :name "0"
                                   :windows (list ,win-var) :active ,win-var)))
     (with-mouse-option (,mouse)
       (with-loop-state
         (let ((cl-tmux::*term-rows* 25) (cl-tmux::*term-cols* 40))
           ,@body)))))
