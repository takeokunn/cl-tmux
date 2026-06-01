(in-package #:cl-tmux/test)

;;;; Tests for src/input.lisp: with-raw-mode macroexpansion + export checks.
;;;; with-raw-mode touches fd 0 (stdin) so it is verified by macroexpansion
;;;; only; read-byte-nonblock execution needs fd injection and is omitted.

(def-suite input-suite :description "with-raw-mode macroexpansion + export checks (src/input.lisp)")
(in-suite input-suite)

(test with-raw-mode-expands-enable-before-body
  "The expansion calls enable-raw-mode! on fd 0 before evaluating BODY."
  (let* ((form (macroexpand-1 '(cl-tmux/input::with-raw-mode :body-marker)))
         (text (prin1-to-string form))
         (enable-pos (search "ENABLE-RAW-MODE!" text))
         (body-pos (search ":BODY-MARKER" text)))
    (is-true enable-pos "expansion mentions ENABLE-RAW-MODE!")
    (is-true body-pos "expansion contains the body form")
    (is (< enable-pos body-pos)
        "enable-raw-mode! precedes the body in the expansion")))

(test with-raw-mode-installs-disable-in-handler-and-cleanup
  "DISABLE-RAW-MODE! appears exactly twice: error handler + unwind cleanup."
  (let* ((form (macroexpand-1 '(cl-tmux/input::with-raw-mode :body-marker)))
         (text (prin1-to-string form))
         (count 0)
         (start 0))
    ;; Count non-overlapping occurrences of DISABLE-RAW-MODE! in the text.
    (loop for pos = (search "DISABLE-RAW-MODE!" text :start2 start)
          while pos
          do (incf count)
             (setf start (+ pos (length "DISABLE-RAW-MODE!"))))
    (is (= 2 count)
        "disable-raw-mode! installed in both handler-bind and unwind-protect")
    ;; The expansion should also establish a handler and an unwind-protect.
    (is-true (search "HANDLER-BIND" text) "uses handler-bind for error safety")
    (is-true (search "UNWIND-PROTECT" text) "uses unwind-protect for cleanup")))

(test with-raw-mode-is-a-macro
  "with-raw-mode is defined as a macro."
  (is-true (macro-function 'cl-tmux/input::with-raw-mode)))

(test input-symbols-exported-and-fbound
  "Public input symbols are exported and bound."
  ;; with-raw-mode is an exported macro.
  (is-true (macro-function (find-symbol "WITH-RAW-MODE" '#:cl-tmux/input))
           "with-raw-mode is a macro")
  ;; read-byte-nonblock is an exported function.
  (is-true (fboundp (find-symbol "READ-BYTE-NONBLOCK" '#:cl-tmux/input))
           "read-byte-nonblock is fbound")
  ;; Both names resolve as exported symbols of the package.
  (multiple-value-bind (sym status)
      (find-symbol "WITH-RAW-MODE" '#:cl-tmux/input)
    (declare (ignore sym))
    (is (eq :external status) "with-raw-mode is exported"))
  (multiple-value-bind (sym status)
      (find-symbol "READ-BYTE-NONBLOCK" '#:cl-tmux/input)
    (declare (ignore sym))
    (is (eq :external status) "read-byte-nonblock is exported")))
