;;;; Package definitions for cl-tmux.
;;;; All package declarations live here so cross-package dependencies are explicit.

(defpackage #:cl-tmux/version
  (:use #:cl)
  (:export #:version-string))

(in-package #:cl-tmux/version)

(defun version-string ()
  "Return the cl-tmux runtime version string."
  "0.1.0")
