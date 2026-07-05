(in-package #:cl-tmux/test)

;;;; POSIX pipe fixtures and pipe-pane assertions.

(defun write-byte-to-fd (fd byte-value)
  "Write a single BYTE-VALUE (0–255) to file descriptor FD via CFFI.
   Returns the write(2) return value (1 on success, negative on error).
   Shared by input-tests.lisp and pty-tests.lisp to eliminate repeated
   cffi:with-foreign-object / mem-ref / foreign-funcall write patterns."
  (cffi:with-foreign-object (buf :uint8)
    (setf (cffi:mem-ref buf :uint8) byte-value)
    (cffi:foreign-funcall "write" :int fd :pointer buf :unsigned-long 1 :long)))

(defmacro with-pipe-fds ((read-fd write-fd) &body body)
  "Open a POSIX pipe; bind READ-FD and WRITE-FD; close both on exit.
   BODY may begin with (declare ...) forms; they are valid in locally's body.
   Shared by input-tests.lisp, pty-tests.lisp, and pty-rawmode-tests.lisp."
  (let ((pair-sym (gensym "PAIR")))
    `(let* ((,pair-sym (multiple-value-list (sb-posix:pipe)))
            (,read-fd  (first  ,pair-sym))
            (,write-fd (second ,pair-sym)))
       (unwind-protect
            (locally ,@body)
         (ignore-errors (sb-posix:close ,read-fd))
         (ignore-errors (sb-posix:close ,write-fd))))))

(defmacro assert-pipe-pane-open-output-to-command-state (pane)
  "Assert the state of PANE after opening a command that consumes pane output."
  `(progn
     (is-true (cl-tmux/model:pane-pipe-active-p ,pane)
              "pane must be marked active after pipe-pane-open")
     (is-true (cl-tmux/model:pane-pipe-fd ,pane)
              "pane-pipe-fd must hold the command stdin stream")
     (is (null (cl-tmux/model:pane-pipe-output-stream ,pane))
         "pane-pipe-output-stream must remain NIL in output-to-command mode")
     (is (null (cl-tmux/model:pane-pipe-output-thread ,pane))
         "pane-pipe-output-thread must remain NIL in output-to-command mode")
     (is-true (cl-tmux/model:pane-pipe-process ,pane)
              "pane-pipe-process must keep the subprocess handle")))

(defmacro assert-pipe-pane-open-command-output-state (pane)
  "Assert the state of PANE after opening a command that writes back to pane."
  `(progn
     (is-true (cl-tmux/model:pane-pipe-active-p ,pane)
              "pane must be marked active after pipe-pane-open")
     (is (null (cl-tmux/model:pane-pipe-fd ,pane))
         "pane-pipe-fd must remain NIL in command-output-to-pane mode")
     (is-true (cl-tmux/model:pane-pipe-output-stream ,pane)
              "pane-pipe-output-stream must hold the command stdout stream")
     (is-true (cl-tmux/model:pane-pipe-output-thread ,pane)
              "pane-pipe-output-thread must hold the copier thread")
     (is-true (cl-tmux/model:pane-pipe-process ,pane)
              "pane-pipe-process must keep the subprocess handle")))

(defmacro assert-pipe-pane-closed-state (pane)
  "Assert that PANE has no pipe resources left."
  `(progn
     (is (null (cl-tmux/model:pane-pipe-active-p ,pane))
         "pane must be inactive after pipe-pane-close")
     (is (null (cl-tmux/model:pane-pipe-fd ,pane))
         "pane-pipe-fd must be NIL after pipe-pane-close")
     (is (null (cl-tmux/model:pane-pipe-output-stream ,pane))
         "pane-pipe-output-stream must be NIL after pipe-pane-close")
     (is (null (cl-tmux/model:pane-pipe-output-thread ,pane))
         "pane-pipe-output-thread must be NIL after pipe-pane-close")
     (is (null (cl-tmux/model:pane-pipe-process ,pane))
         "pane-pipe-process must be NIL after pipe-pane-close")))
