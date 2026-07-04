(in-package #:cl-tmux/test)

;;;; Dispatch tests - target flag parsing.

(in-suite dispatch-suite)

(test parse-command-flags-value-and-boolean
  "%parse-command-flags separates -t<value> flags, boolean flags, and positionals."
  (multiple-value-bind (flags positionals)
      (cl-tmux::%parse-command-flags '("-t" "2") "t")
    (declare (ignore positionals))
    (is (equal "2" (alist-value #\t flags))
        "-t 2 (separate) → value \"2\""))
  (multiple-value-bind (flags positionals)
      (cl-tmux::%parse-command-flags '("-t2") "t")
    (declare (ignore positionals))
    (is (equal "2" (alist-value #\t flags))
        "-t2 (attached) → value \"2\""))
  (multiple-value-bind (flags positionals)
      (cl-tmux::%parse-command-flags '("-d") "t")
    (declare (ignore positionals))
    (is (eq t (alist-value #\d flags))
        "-d (not a value flag) → boolean T"))
  (multiple-value-bind (flags positionals)
      (cl-tmux::%parse-command-flags '("-d" "foo" "-t" "2" "bar") "t")
    (declare (ignore flags))
    (is (equal '("foo" "bar") positionals)
        "non-flag tokens are positionals in order")))
