(in-package #:cl-tmux/test)

;;;; Dispatch tests - target flag parsing.

(describe "dispatch-suite"

  ;; %parse-command-flags separates -t<value> flags, boolean flags, and positionals.
  (it "parse-command-flags-value-and-boolean"
    (multiple-value-bind (flags positionals)
        (cl-tmux::%parse-command-flags '("-t" "2") "t")
      (declare (ignore positionals))
      (expect (equal "2" (alist-value #\t flags))))
    (multiple-value-bind (flags positionals)
        (cl-tmux::%parse-command-flags '("-t2") "t")
      (declare (ignore positionals))
      (expect (equal "2" (alist-value #\t flags))))
    (multiple-value-bind (flags positionals)
        (cl-tmux::%parse-command-flags '("-d") "t")
      (declare (ignore positionals))
      (expect (eq t (alist-value #\d flags))))
    (multiple-value-bind (flags positionals)
        (cl-tmux::%parse-command-flags '("-d" "foo" "-t" "2" "bar") "t")
      (declare (ignore flags))
      (expect (equal '("foo" "bar") positionals)))))
