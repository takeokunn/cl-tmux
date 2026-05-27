(in-package #:cl-tmux/config)

;;; ASCII 2 = ^B.  tmux uses C-b as the default prefix.
(defconstant +prefix-key-code+ 2)

(defparameter *default-shell*
  (or #+sbcl (sb-ext:posix-getenv "SHELL")
      #-sbcl (uiop:getenv "SHELL")
      "/bin/sh")
  "Shell binary launched for new panes.")

(defparameter *status-height* 1
  "Number of rows reserved for the status bar at the bottom.")

(defconstant +pty-buf-size+ 4096
  "Byte buffer size for PTY reads.")

;;; After receiving the prefix key, the next keystroke is looked up here.
;;; Values are keywords that the main loop dispatches on.
(defparameter *key-bindings*
  (let ((h (make-hash-table)))
    (setf (gethash #\c  h) :new-window)
    (setf (gethash #\n  h) :next-window)
    (setf (gethash #\p  h) :prev-window)
    (setf (gethash #\"  h) :split-horizontal)  ; horizontal split → stacked
    (setf (gethash #\%  h) :split-vertical)    ; vertical split   → side-by-side
    (setf (gethash #\o  h) :next-pane)
    (setf (gethash #\d  h) :detach)
    (setf (gethash #\?  h) :list-keys)
    (setf (gethash #\[  h) :scroll-mode)
    h)
  "Prefix-key dispatch table.")
