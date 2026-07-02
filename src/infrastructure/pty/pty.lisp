(in-package #:cl-tmux/pty)

;;;; PTY management, terminal raw mode, and multiplexed I/O.
;;;;
;;;; Implemented in pure Common Lisp using:
;;;;   • SB-EXT    — process spawning with a PTY stream
;;;;   • CFFI      — ioctl, select, read/write (all from libc; no custom C)
;;;;   • sb-posix  — terminal raw mode and fallback fd close
;;;;
;;;; FFI declarations and platform constants live in pty-ffi.lisp.

;;; ── Public: PTY creation ───────────────────────────────────────────────────

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

;;; ── Private: spawned PTY helpers ───────────────────────────────────────────

(defvar *pty-processes* (make-hash-table :test #'eql)
  "MASTER-FD -> SB-EXT process object for PTYs spawned by forkpty-with-shell.")

(defun %string-non-empty-p (value)
  "Return T when VALUE is a non-empty string."
  (and (stringp value) (plusp (length value))))

(defun %spawn-environment-assignments (term extra-env)
  "Return TERM and valid EXTRA-ENV overrides as NAME=VALUE strings."
  (let ((assignments nil))
    (when (%string-non-empty-p term)
      (push (format nil "TERM=~A" term) assignments))
    (dolist (pair extra-env)
      (when (and (consp pair)
                 (stringp (car pair))
                 (stringp (cdr pair)))
        (push (format nil "~A=~A" (car pair) (cdr pair)) assignments)))
    (nreverse assignments)))

(defun %spawn-directory (start-dir)
  "Return a truename pathname for START-DIR, or NIL when it is absent/invalid.
   The old child path ignored chdir failures; keeping NIL preserves that behavior
   by letting the child inherit the current directory."
  (when (%string-non-empty-p start-dir)
    (ignore-errors (truename start-dir))))

(defun %target-program-and-args (default-command)
  "Return (values PROGRAM ARGS SEARCH-P) for SB-EXT:RUN-PROGRAM.
   When DEFAULT-COMMAND is a non-empty string, run it via /bin/sh -c.
   Otherwise run the configured default shell directly, searching PATH for it
   (SEARCH-P) unless it is already given as an absolute path."
  (if (%string-non-empty-p default-command)
      (values "/bin/sh" (list "-c" default-command) nil)
      (values cl-tmux/config:*default-shell* nil
              (not (and (stringp cl-tmux/config:*default-shell*)
                        (plusp (length cl-tmux/config:*default-shell*))
                        (char= (char cl-tmux/config:*default-shell* 0) #\/))))))

(defun %process-pty-fd (process)
  "Return the master fd for PROCESS's PTY stream."
  (let ((stream (sb-ext:process-pty process)))
    (unless stream
      (error "run-program did not return a PTY stream"))
    (sb-sys:fd-stream-fd stream)))

(defun %remember-pty-process (master-fd process)
  "Record PROCESS so pty-close can close SBCL's PTY stream object."
  (setf (gethash master-fd *pty-processes*) process))

(defun %take-pty-process (master-fd)
  "Remove and return the process object associated with MASTER-FD, if any."
  (let ((process (gethash master-fd *pty-processes*)))
    (remhash master-fd *pty-processes*)
    process))

(defun pty-child-exit-status (master-fd)
  "Exit information for MASTER-FD's child process, called at PTY EOF (the child
   has closed the slave, so the wait does not block for a live shell).
   Returns (values CODE KIND) where KIND is :exited (CODE = exit code) or
   :signaled (CODE = signal number), or NIL when the child is unknown (foreign
   fd, synthetic test pane) or the wait fails."
  (let ((process (gethash master-fd *pty-processes*)))
    (when process
      (handler-case
          (progn
            (sb-ext:process-wait process)
            (let ((code (sb-ext:process-exit-code process)))
              (when code
                (if (eq (sb-ext:process-status process) :signaled)
                    (values code :signaled)
                    (values code :exited)))))
        (error () nil)))))

(defun forkpty-with-shell (rows cols &key start-dir default-command environment)
  "Spawn a child shell process on a fresh PTY of size ROWS×COLS.
   START-DIR: when valid, run the child from this directory.
   DEFAULT-COMMAND: when non-NIL, run via sh -c instead of the shell directly.
   ENVIRONMENT: flat list of NAME=VALUE strings passed to RUN-PROGRAM.
   Returns (values master-fd child-pid slave-path).  SBCL exposes the master
   stream and pid but not a portable slave-path, so SLAVE-PATH is currently the
   empty string."
  (declare (type fixnum rows cols))
  (multiple-value-bind (program args search-p)
      (%target-program-and-args default-command)
    (let* ((process (sb-ext:run-program program args
                                        :search search-p
                                        :wait nil
                                        :pty t
                                        :environment environment
                                        :directory (%spawn-directory start-dir)))
           (master (%process-pty-fd process)))
      (set-pty-size master rows cols)
      (%remember-pty-process master process)
      (values master (sb-ext:process-pid process) ""))))

;;; ── FFI memory transfer helpers (data layer) ────────────────────────────────
;;;
;;; Prolog-like facts mapping the transfer direction:
;;;   copy_to_foreign(octets, ptr, len) :- for_each(i, 0, len, set_byte(ptr, i, octets[i])).
;;;   copy_from_foreign(ptr, n) → result :- for_each(i, 0, n, result[i] = byte(ptr, i)).

(declaim (inline %octets-to-foreign %foreign-to-octets))

(defun %octets-to-foreign (octets foreign-ptr len)
  "Copy LEN bytes from Lisp OCTETS vector into foreign memory FOREIGN-PTR."
  (declare (type (simple-array (unsigned-byte 8) (*)) octets)
           (type fixnum len))
  (dotimes (i len)
    (setf (cffi:mem-aref foreign-ptr :uint8 i) (aref octets i))))

(defun %foreign-to-octets (foreign-ptr byte-count)
  "Copy BYTE-COUNT bytes from foreign memory FOREIGN-PTR into a fresh Lisp octet vector."
  (declare (type fixnum byte-count))
  (let ((result (make-array byte-count :element-type '(unsigned-byte 8))))
    (dotimes (i byte-count) (setf (aref result i) (cffi:mem-aref foreign-ptr :uint8 i)))
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
      (let ((process (%take-pty-process master-fd)))
        (if process
            (sb-ext:process-close process)
            (sb-posix:close master-fd))))))

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
   The read-set is meaningful only when select(2) returns a positive count."
  (when fds
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
            (%collect-ready-fds fds rset)))))))

;;; ── Public: terminal geometry ──────────────────────────────────────────────

(defconstant +max-sane-rows+ 1000
  "Upper bound on terminal rows accepted from ioctl; values above this are clamped.")
(defconstant +max-sane-cols+ 1000
  "Upper bound on terminal columns accepted from ioctl; values above this are clamped.")

(defconstant +default-term-rows+ 24
  "Fallback terminal height in rows, used when ioctl fails or reports a
   nonsensical size (e.g., a transient 0x0 read). Mirrors the *term-rows*
   defvar default in runtime.lisp.")
(defconstant +default-term-cols+ 80
  "Fallback terminal width in columns, used when ioctl fails or reports a
   nonsensical size (e.g., a transient 0x0 read). Mirrors the *term-cols*
   defvar default in runtime.lisp.")

(defun terminal-size ()
  "Return (values rows cols) of the terminal attached to stdout.
   Falls back to +default-term-rows+ x +default-term-cols+ if ioctl fails or
   reports an out-of-range size (a transient 0x0 or garbage read must not
   drive a resize)."
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
                (values +default-term-rows+ +default-term-cols+)))
          (values +default-term-rows+ +default-term-cols+))))) ; safe fallback if ioctl fails

;;; ── Port adapter ─────────────────────────────────────────────────────────────
;;;
;;; install-pty-port wires this module's CFFI-backed functions into the
;;; cl-tmux/ports abstraction layer so that domain code (cl-tmux/model) calls
;;; through the port rather than referencing cl-tmux/pty symbols directly.
;;; Must be called before any pane is created (server startup or test setup).

(defun install-pty-port ()
  "Register the CFFI PTY implementation as the active cl-tmux/ports adapter."
  (setf cl-tmux/ports:*spawn-pty* #'forkpty-with-shell
        cl-tmux/ports:*write-pty* #'pty-write
        cl-tmux/ports:*resize-pty* #'set-pty-size
        cl-tmux/ports:*close-pty* #'pty-close))
