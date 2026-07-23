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
     (expect (cl-tmux/model:pane-pipe-active-p ,pane) :to-be-truthy)
     (expect (cl-tmux/model:pane-pipe-fd ,pane) :to-be-truthy)
     (expect (null (cl-tmux/model:pane-pipe-output-stream ,pane)))
     (expect (null (cl-tmux/model:pane-pipe-output-thread ,pane)))
     (expect (cl-tmux/model:pane-pipe-process ,pane) :to-be-truthy)))

(defmacro assert-pipe-pane-open-command-output-state (pane)
  "Assert the state of PANE after opening a command that writes back to pane."
  `(progn
     (expect (cl-tmux/model:pane-pipe-active-p ,pane) :to-be-truthy)
     (expect (null (cl-tmux/model:pane-pipe-fd ,pane)))
     (expect (cl-tmux/model:pane-pipe-output-stream ,pane) :to-be-truthy)
     (expect (cl-tmux/model:pane-pipe-output-thread ,pane) :to-be-truthy)
     (expect (cl-tmux/model:pane-pipe-process ,pane) :to-be-truthy)))

(defmacro assert-pipe-pane-closed-state (pane)
  "Assert that PANE has no pipe resources left."
  `(progn
     (expect (null (cl-tmux/model:pane-pipe-active-p ,pane)))
     (expect (null (cl-tmux/model:pane-pipe-fd ,pane)))
     (expect (null (cl-tmux/model:pane-pipe-output-stream ,pane)))
     (expect (null (cl-tmux/model:pane-pipe-output-thread ,pane)))
     (expect (null (cl-tmux/model:pane-pipe-process ,pane)))))
