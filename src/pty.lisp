(in-package #:cl-tmux/pty)

;;;; PTY management, terminal raw mode, and multiplexed I/O.
;;;;
;;;; Implemented in pure Common Lisp using:
;;;;   • sb-posix  — fork, setsid, dup2, tcgetattr/tcsetattr
;;;;   • CFFI      — posix_openpt, grantpt, unlockpt, ptsname,
;;;;                 ioctl, execv, select (all from libc — no custom C)
;;;;
;;;; FFI declarations and platform constants live in pty-ffi.lisp.

;;; ── Public: PTY creation ───────────────────────────────────────────────────

(defun open-pty (rows cols)
  "Open a master PTY device sized ROWS×COLS.
   Returns (values master-fd slave-path-string)."
  (let ((master (%posix-openpt (logior +o-rdwr+ +o-noctty+))))
    (when (< master 0)
      (error "posix_openpt failed"))
    (%grantpt  master)
    (%unlockpt master)
    (set-pty-size master rows cols)
    (values master (copy-seq (%ptsname master)))))  ; copy before next ptsname call

(defun set-pty-size (master-fd rows cols)
  "Notify the kernel PTY driver of a new ROWS×COLS window size."
  (cffi:with-foreign-object (ws '(:struct winsize))
    (setf (cffi:foreign-slot-value ws '(:struct winsize) 'ws-row)    rows
          (cffi:foreign-slot-value ws '(:struct winsize) 'ws-col)    cols
          (cffi:foreign-slot-value ws '(:struct winsize) 'ws-xpixel) 0
          (cffi:foreign-slot-value ws '(:struct winsize) 'ws-ypixel) 0)
    (cffi:foreign-funcall "ioctl"
                          :int master-fd
                          :unsigned-long +tiocswinsz+
                          :pointer ws
                          :int)))

;;; ── Private: forkpty helpers ───────────────────────────────────────────────

(defun %child-setup-tty (slave-path master-fd)
  "Set up the slave PTY as the controlling terminal for a new child process.
   Becomes session leader, opens the slave, installs it as the controlling
   terminal, wires it to stdin/stdout/stderr, and closes the now-unneeded fds.
   On failure (e.g., open returns -1 or ioctl fails) the child continues to
   %child-exec-shell; if execv then also fails, _exit(1) ensures the child
   does not accidentally return into the parent Lisp runtime."
  (sb-posix:setsid)
  (let ((slave (sb-posix:open slave-path (logior +o-rdwr+ +o-noctty+) 0)))
    (cffi:foreign-funcall "ioctl"
                          :int slave :unsigned-long +tiocsctty+ :int 0 :int)
    (sb-posix:dup2 slave 0)
    (sb-posix:dup2 slave 1)
    (sb-posix:dup2 slave 2)
    (sb-posix:close slave))
  (sb-posix:close master-fd))

(defun %child-exec-shell ()
  "Replace the current process image with *DEFAULT-SHELL*.
   Calls _exit(1) if execv fails — never returns normally.
   NOTE: _exit (not exit or sb-ext:quit) is used intentionally: _exit bypasses
   C atexit handlers and Lisp finalizers that are unsafe to call in a child
   process after fork, preventing double-flushing of stdio buffers and avoiding
   any SBCL runtime teardown that would race with the parent process."
  (let ((shell cl-tmux/config:*default-shell*))
    (cffi:with-foreign-string (path-ptr shell)
      (cffi:with-foreign-string (arg0-ptr shell)
        (cffi:with-foreign-object (argv :pointer 2)
          (setf (cffi:mem-aref argv :pointer 0) arg0-ptr
                (cffi:mem-aref argv :pointer 1) (cffi:null-pointer))
          (cffi:foreign-funcall "execv" :pointer path-ptr :pointer argv :int)))))
  ;; execv failed (wrong path, not executable, etc.) — fall through to _exit.
  (cffi:foreign-funcall "_exit" :int 1 :void))

(defun forkpty-with-shell (rows cols)
  "Fork a child shell process on a fresh PTY of size ROWS×COLS.
   Parent returns (values master-fd child-pid).
   Child execs *default-shell* and never returns to Lisp."
  (declare (type fixnum rows cols))
  (multiple-value-bind (master slave-path) (open-pty rows cols)
    (let ((pid (sb-posix:fork)))
      (cond
        ((< pid 0) (error "fork failed"))
        ((= pid 0)                             ; child
         (%child-setup-tty slave-path master)
         (%child-exec-shell))
        (t                                     ; parent
         (values master pid))))))

;;; ── FFI memory transfer helpers (data layer) ────────────────────────────────
;;;
;;; Prolog-like facts mapping the transfer direction:
;;;   copy_to_foreign(octets, ptr, len) :- for_each(i, 0, len, set_byte(ptr, i, octets[i])).
;;;   copy_from_foreign(ptr, n) → result :- for_each(i, 0, n, result[i] = byte(ptr, i)).

(declaim (inline %octets-to-foreign %foreign-to-octets))

(defun %octets-to-foreign (octets ptr len)
  "Copy LEN bytes from Lisp OCTETS vector into foreign memory PTR."
  (declare (type (simple-array (unsigned-byte 8) (*)) octets)
           (type fixnum len))
  (dotimes (i len)
    (setf (cffi:mem-aref ptr :uint8 i) (aref octets i))))

(defun %foreign-to-octets (ptr n)
  "Copy N bytes from foreign memory PTR into a fresh Lisp octet vector."
  (declare (type fixnum n))
  (let ((result (make-array n :element-type '(unsigned-byte 8))))
    (dotimes (i n) (setf (aref result i) (cffi:mem-aref ptr :uint8 i)))
    result))

;;; ── Public: PTY I/O ────────────────────────────────────────────────────────

(defun pty-write (fd data)
  "Write DATA (octet vector or UTF-8 string) to the PTY master fd."
  (etypecase data
    (string
     (pty-write fd (babel:string-to-octets data :encoding :utf-8)))
    ((simple-array (unsigned-byte 8) (*))
     (let ((len (length data)))
       (when (plusp len)
         (cffi:with-foreign-object (buf :uint8 len)
           (%octets-to-foreign data buf len)
           (%write fd buf len)))))))

(defun pty-read-blocking (fd buf-size)
  "Block until data arrives on FD, then return an octet vector.
   Returns NIL on EOF or error."
  (cffi:with-foreign-object (raw :uint8 buf-size)
    (let ((n (%read fd raw buf-size)))
      (when (plusp n)
        (%foreign-to-octets raw n)))))

(defun pty-close (master-fd child-pid)
  "Send SIGHUP to the child process and close the PTY master.

   A non-positive CHILD-PID is ignored: kill(-1)/kill(0) broadcast the signal to
   the whole process group (including this process), which must never happen.
   Likewise a negative MASTER-FD is not closed."
  (ignore-errors
    (when (> child-pid 0)
      (cffi:foreign-funcall "kill" :int child-pid :int +sighup+ :int))
    (when (>= master-fd 0)
      (sb-posix:close master-fd))))

;;; ── Public: select-based I/O multiplexing ─────────────────────────────────

(defun select-fds (fds timeout-us)
  "Poll FDS for readability with a TIMEOUT-US microsecond timeout.
   timeout-us = 0 → non-blocking; -1 → block indefinitely.
   Returns the sub-list of fds that are ready to read."
  (when (null fds) (return-from select-fds nil))
  (let ((maxfd (reduce #'max fds)))
    (cffi:with-foreign-objects ((rset :int32 +fd-set-words+)
                                (tv   :long  2))
      (fd-zero! rset)
      (dolist (fd fds) (fd-set! fd rset))
      (cond
        ((>= timeout-us 0)
         (setf (cffi:mem-aref tv :long 0) (floor timeout-us 1000000)
               (cffi:mem-aref tv :long 1) (mod   timeout-us 1000000))
         (%select (1+ maxfd) rset (cffi:null-pointer) (cffi:null-pointer) tv))
        (t
         (%select (1+ maxfd) rset (cffi:null-pointer) (cffi:null-pointer)
                  (cffi:null-pointer))))
      ;; Return only the fds that became readable.
      (loop for fd in fds when (fd-isset-p fd rset) collect fd))))

;;; ── Public: terminal geometry ──────────────────────────────────────────────

(defconstant +max-sane-rows+ 1000)
(defconstant +max-sane-cols+ 1000)

;;; Well-known POSIX file descriptors.
(defconstant +stdout-fd+ 1 "POSIX file descriptor number for standard output.")

(defun terminal-size ()
  "Return (values rows cols) of the terminal attached to stdout.
   Falls back to 24×80 if ioctl fails or reports an out-of-range size
   (a transient 0×0 or garbage read must not drive a resize)."
  (cffi:with-foreign-object (ws '(:struct winsize))
    (let ((r (cffi:foreign-funcall "ioctl"
                                   :int +stdout-fd+
                                   :unsigned-long +tiocgwinsz+
                                   :pointer ws
                                   :int)))
      (if (zerop r)
          (let ((rows (cffi:foreign-slot-value ws '(:struct winsize) 'ws-row))
                (cols (cffi:foreign-slot-value ws '(:struct winsize) 'ws-col)))
            (if (and (<= 1 rows +max-sane-rows+)
                     (<= 1 cols +max-sane-cols+))
                (values rows cols)
                (values 24 80)))
          (values 24 80)))))          ; safe fallback if ioctl fails

