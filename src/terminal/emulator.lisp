(in-package #:cl-tmux/terminal/emulator)

;;;; Main entry points for the CPS VT100 terminal emulator.
;;;;
;;;; screen-process-bytes feeds raw PTY bytes into the CPS parser loop.
;;;; Screen construction lives in cl-tmux/terminal/types:make-screen, whose
;;;; struct default initialises the parser slot to ground-state.

(defun screen-process-bytes (screen bytes &key (start 0) (end (length bytes)))
  "Feed raw PTY bytes BYTES[START..END) into SCREEN, advancing the CPS parser."
  (loop for i from start below end
        for byte = (aref bytes i)
        do (setf (screen-parser screen)
                 (funcall (screen-parser screen) screen byte))))
