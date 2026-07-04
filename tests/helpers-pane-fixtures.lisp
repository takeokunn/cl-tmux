;;;; Pane fixture helpers for cl-tmux tests.

(in-package #:cl-tmux/test)

(defun make-no-pty-pane (id x y w h)
  "Build a pane with no real PTY and a matching virtual screen."
  (make-pane :id id :x x :y y :width w :height h
             :fd -1 :pid -1
             :screen (make-screen w h)))
