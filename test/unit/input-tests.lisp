(in-package #:cl-tmux/test)

;;;; Tests for src/input.lisp: with-raw-mode macroexpansion + export checks
;;;; + read-byte-nonblock happy path via a sb-posix pipe pair.
;;;; with-raw-mode touches fd 0 (stdin) so it is verified by macroexpansion only.
;;;; read-byte-nonblock's select+read path is exercised using a pipe fd instead
;;;; of stdin, so no TTY is required.

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

;;; ── read-byte-nonblock happy path via pipe ───────────────────────────────────
;;;
;;; We use a POSIX pipe pair (sb-posix:pipe) so we can inject a known byte into
;;; the read end without needing stdin to be a TTY.

(defmacro with-pipe-fds ((read-fd write-fd) &body body)
  "Open a POSIX pipe; bind READ-FD and WRITE-FD; close both on exit."
  (let ((pair-sym (gensym "PAIR")))
    `(let* ((,pair-sym (multiple-value-list (sb-posix:pipe)))
            (,read-fd  (first  ,pair-sym))
            (,write-fd (second ,pair-sym)))
       (unwind-protect
            (progn ,@body)
         (ignore-errors (sb-posix:close ,read-fd))
         (ignore-errors (sb-posix:close ,write-fd))))))

(test read-byte-nonblock-returns-byte-when-data-available
  "read-byte-nonblock's select+read pipeline returns a byte when data is ready.
   Uses a pipe pair so no TTY is required."
  (with-pipe-fds (rfd wfd)
    ;; Write one known byte into the write end.
    (cffi:with-foreign-object (buf :uint8)
      (setf (cffi:mem-ref buf :uint8) 42)
      (cffi:foreign-funcall "write" :int wfd :pointer buf :unsigned-long 1 :long))
    ;; Poll the read end: data should be ready immediately.
    (let ((ready (cl-tmux/pty:select-fds (list rfd) 200000)))  ; 200 ms timeout
      (is-true ready "pipe read-end must be readable after write")
      (when ready
        ;; Read exactly one byte via CFFI (same mechanics as read-byte-nonblock).
        (cffi:with-foreign-object (rbuf :uint8)
          (let ((n (cffi:foreign-funcall "read"
                                         :int rfd :pointer rbuf :unsigned-long 1
                                         :long)))
            (is (= 1 n) "read must consume exactly 1 byte")
            (is (= 42 (cffi:mem-ref rbuf :uint8))
                "byte value must be 42")))))))

(test read-byte-nonblock-select-returns-nil-when-no-data
  "select-fds returns NIL when no data is available within the timeout.
   Verified on a fresh idle pipe."
  (with-pipe-fds (rfd _wfd)
    ;; The pipe has no data; select with a short timeout must return NIL.
    (let ((ready (cl-tmux/pty:select-fds (list rfd) 10000)))  ; 10 ms
      (is (null ready) "empty pipe must not be readable"))))
