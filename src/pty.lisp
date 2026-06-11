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

;;; ── exec argument-vector size constants ────────────────────────────────────

(defconstant +sh-argc+ 4
  "Number of pointers in the argv array for 'sh -c CMD NULL': /bin/sh, -c, cmd, NULL.")

(defconstant +shell-argc+ 2
  "Number of pointers in the argv array for 'shell NULL': shell-path, NULL.")

;;; ── Fork sentinel constants ────────────────────────────────────────────────

(defconstant +fork-child-pid+ 0
  "Return value of fork(2) in the child process (POSIX standard).")

(defconstant +fork-error-pid+ -1
  "Return value of fork(2) on failure (POSIX standard).")

;;; ── Private: forkpty helpers ───────────────────────────────────────────────

(defun %child-setup-tty (slave-path master-fd)
  "Set up the slave PTY as the controlling terminal for a new child process.
   MUST be called only in the child process after fork — NEVER from the parent.
   Becomes session leader, opens the slave, installs it as the controlling
   terminal, wires it to stdin/stdout/stderr, and closes the now-unneeded fds.
   On failure (e.g., open returns -1 or ioctl fails) the child continues to
   %child-exec-shell; if execv then also fails, _exit(1) ensures the child
   does not accidentally return into the parent Lisp runtime."
  (sb-posix:setsid)
  (let ((slave (sb-posix:open slave-path (logior +o-rdwr+ +o-noctty+) 0)))
    (cffi:foreign-funcall "ioctl"
                          :int slave :unsigned-long +tiocsctty+ :int 0 :int)
    (sb-posix:dup2 slave +stdin-fd+)
    (sb-posix:dup2 slave +stdout-fd+)
    (sb-posix:dup2 slave +stderr-fd+)
    (sb-posix:close slave))
  (sb-posix:close master-fd))

(defun %child-setenv (name value)
  "Call setenv(3) from the child process to set NAME=VALUE.
   MUST be called only in the child process after fork — NEVER from the parent."
  (cffi:with-foreign-string (name-ptr  name)
    (cffi:with-foreign-string (value-ptr value)
      (cffi:foreign-funcall "setenv"
                            :pointer name-ptr
                            :pointer value-ptr
                            :int 1
                            :int))))

(defun %exec-command (command)
  "Replace the current process image with /bin/sh -c COMMAND.
   Builds a +sh-argc+-element argv: [\"/bin/sh\", \"-c\", command, NULL].
   MUST be called only in the child process after fork — NEVER from the parent."
  (cffi:with-foreign-string (sh-ptr "/bin/sh")
    (cffi:with-foreign-string (dash-c-ptr "-c")
      (cffi:with-foreign-string (cmd-ptr command)
        (cffi:with-foreign-object (argv :pointer +sh-argc+)
          (setf (cffi:mem-aref argv :pointer 0) sh-ptr
                (cffi:mem-aref argv :pointer 1) dash-c-ptr
                (cffi:mem-aref argv :pointer 2) cmd-ptr
                (cffi:mem-aref argv :pointer 3) (cffi:null-pointer))
          (cffi:foreign-funcall "execv" :pointer sh-ptr :pointer argv :int))))))

(defun %exec-shell (shell-path)
  "Replace the current process image with SHELL-PATH directly.
   Builds a +shell-argc+-element argv: [shell-path, NULL].
   MUST be called only in the child process after fork — NEVER from the parent."
  (cffi:with-foreign-string (path-ptr shell-path)
    (cffi:with-foreign-string (arg0-ptr shell-path)
      (cffi:with-foreign-object (argv :pointer +shell-argc+)
        (setf (cffi:mem-aref argv :pointer 0) arg0-ptr
              (cffi:mem-aref argv :pointer 1) (cffi:null-pointer))
        (cffi:foreign-funcall "execv" :pointer path-ptr :pointer argv :int)))))

(defun %child-exec-shell (&optional start-dir term default-command extra-env)
  "Replace the current process image with a shell or default-command.
   When START-DIR is a non-empty string, chdir to it before execv.
   When TERM is a non-empty string, set TERM=TERM in the child environment.
   When DEFAULT-COMMAND is a non-empty string, run sh -c DEFAULT-COMMAND
   instead of *DEFAULT-SHELL* directly.
   EXTRA-ENV: alist of (NAME . VALUE) pairs to set in the child environment
   before exec (e.g. from new-window -e VAR=val).
   MUST be called only in the child process after fork — NEVER from the parent."
  (when (and start-dir (plusp (length start-dir)))
    (ignore-errors (sb-posix:chdir start-dir)))
  (when (and term (plusp (length term)))
    (%child-setenv "TERM" term))
  ;; Apply extra environment variables from -e flags (new-window / split-window).
  (dolist (pair extra-env)
    (when (and (consp pair) (stringp (car pair)) (stringp (cdr pair)))
      (%child-setenv (car pair) (cdr pair))))
  (if (and default-command (plusp (length default-command)))
      (%exec-command default-command)
      (%exec-shell cl-tmux/config:*default-shell*))
  ;; execv failed — fall through to _exit.
  (cffi:foreign-funcall "_exit" :int 1 :void))

(defun forkpty-with-shell (rows cols &key start-dir term default-command extra-env)
  "Fork a child shell process on a fresh PTY of size ROWS×COLS.
   START-DIR: when non-NIL, chdir to this path before exec.
   TERM: when non-NIL, set TERM=TERM in the child environment.
   DEFAULT-COMMAND: when non-NIL, run via sh -c instead of the shell directly.
   EXTRA-ENV: alist of (NAME . VALUE) pairs set in the child environment.
   Parent returns (values master-fd child-pid slave-path), where SLAVE-PATH is the
   PTY device path (e.g. /dev/pts/3) the child sees — surfaced as #{pane_tty}.
   Child execs and never returns to Lisp."
  (declare (type fixnum rows cols))
  (multiple-value-bind (master slave-path) (open-pty rows cols)
    (let ((pid (sb-posix:fork)))
      (cond
        ((= pid +fork-error-pid+) (error "fork failed"))
        ((= pid +fork-child-pid+)              ; child
         (%child-setup-tty slave-path master)
         (%child-exec-shell start-dir term default-command extra-env))
        (t                                     ; parent
         (values master pid slave-path))))))

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

(defun %foreign-to-octets (ptr byte-count)
  "Copy BYTE-COUNT bytes from foreign memory PTR into a fresh Lisp octet vector."
  (declare (type fixnum byte-count))
  (let ((result (make-array byte-count :element-type '(unsigned-byte 8))))
    (dotimes (i byte-count) (setf (aref result i) (cffi:mem-aref ptr :uint8 i)))
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

(defun pty-read-blocking (fd buffer-size)
  "Block until data arrives on FD, then return an octet vector of up to BUFFER-SIZE bytes.
   Returns NIL on EOF or error."
  (cffi:with-foreign-object (raw :uint8 buffer-size)
    (let ((byte-count (%read fd raw buffer-size)))
      (when (plusp byte-count)
        (%foreign-to-octets raw byte-count)))))

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

(defconstant +microseconds-per-second+ 1000000
  "Number of microseconds in one second; used in struct timeval decomposition.")

(defun %setup-timeval (tv timeout-us)
  "Write TIMEOUT-US microseconds into the struct timeval at foreign pointer TV.
   Decomposes timeout into (seconds, remainder-microseconds) using
   +microseconds-per-second+."
  (setf (cffi:mem-aref tv :long 0) (floor timeout-us +microseconds-per-second+)
        (cffi:mem-aref tv :long 1) (mod   timeout-us +microseconds-per-second+)))

(defun %collect-ready-fds (fds rset)
  "Return the sub-list of FDS whose bits are set in read-set RSET."
  (loop for fd in fds when (fd-isset-p fd rset) collect fd))

(defun select-fds (fds timeout-us)
  "Poll FDS for readability with a TIMEOUT-US microsecond timeout.
   timeout-us = 0 → non-blocking; -1 → block indefinitely.
   Returns the sub-list of fds that are ready to read, or NIL.

   The read-set is meaningful ONLY when select(2) returns a positive count: on a
   timeout it returns 0, and on an interrupted/failed call (e.g. EINTR from a
   SIGCHLD) it returns -1 and leaves the read-set UNDEFINED.  We therefore gate on
   the return value and never inspect stale bits — without this, an EINTR could
   make an idle fd spuriously report readable (an intermittent false positive)."
  (when (null fds) (return-from select-fds nil))
  (let ((maxfd (reduce #'max fds)))
    (cffi:with-foreign-objects ((rset :uint32 +fd-set-words+)
                                (tv   :long   2))
      (fd-zero! rset)
      (dolist (fd fds) (fd-set! fd rset))
      (let ((nready (if (>= timeout-us 0)
                        (progn
                          (%setup-timeval tv timeout-us)
                          (%select (1+ maxfd) rset
                                   (cffi:null-pointer) (cffi:null-pointer)
                                   tv))
                        (%select (1+ maxfd) rset
                                 (cffi:null-pointer) (cffi:null-pointer)
                                 (cffi:null-pointer)))))
        ;; Only a positive count leaves a valid read-set to inspect.
        (when (> nready 0)
          (%collect-ready-fds fds rset))))))

;;; ── Public: terminal geometry ──────────────────────────────────────────────

(defconstant +max-sane-rows+ 1000
  "Upper bound on terminal rows accepted from ioctl; values above this are clamped.")
(defconstant +max-sane-cols+ 1000
  "Upper bound on terminal columns accepted from ioctl; values above this are clamped.")

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

