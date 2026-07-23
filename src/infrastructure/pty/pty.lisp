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

(defvar *pty-processes* (make-hash-table :test #'eql :synchronized t)
  "MASTER-FD -> cl-tty-kit PTY struct for PTYs spawned by forkpty-with-shell.
   :synchronized so the reader thread (pty-child-exit-status reads) and teardown
   (pty-close remhash) can touch it concurrently without a coarse external lock.
   The cl-tty-kit PTY struct owns the SBCL process object and its master stream;
   retaining it here keeps that stream (and therefore the master fd cl-tmux holds)
   reachable for the pane's lifetime, so SBCL's GC cannot close the fd out from
   under us.  pty-close / pty-child-exit-status reap through this table.")

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

(defun %remember-pty-process (master-fd pty)
  "Record the cl-tty-kit PTY struct so pty-close can reap it and so the struct
   (and the master stream/fd it owns) stays reachable for the pane's lifetime."
  (setf (gethash master-fd *pty-processes*) pty))

(defun %take-pty-process (master-fd)
  "Remove and return the cl-tty-kit PTY struct associated with MASTER-FD, if any."
  (let ((pty (gethash master-fd *pty-processes*)))
    (remhash master-fd *pty-processes*)
    pty))

(defconstant +pty-child-wait-timeout+ 5
  "Wall-clock timeout, in seconds, for PTY-CHILD-EXIT-STATUS's wait on a child
   that has already closed its PTY slave.  The child should already be
   exiting by then; this bounds the rare case where it lingers (e.g. a
   daemonizing grandchild still holding the PTY open) so the reader thread
   that calls this at EOF cannot block forever.")

(defun pty-child-exit-status (master-fd &optional (timeout +pty-child-wait-timeout+))
  "Exit information for MASTER-FD's child process, called at PTY EOF (the child
   has closed the slave, so the wait does not normally block for a live shell;
   bounded by TIMEOUT seconds regardless, default +PTY-CHILD-WAIT-TIMEOUT+ —
   override only for tests that need a live child to time out quickly).
   Returns (values CODE KIND) where KIND is :exited (CODE = exit code) or
   :signaled (CODE = signal number), or NIL when the child is unknown (foreign
   fd, synthetic test pane), the wait times out, or the wait fails."
  (let* ((pty (gethash master-fd *pty-processes*))
         (process (and pty (cl-tty-kit:pty-process pty))))
    (when process
      (handler-case
          (progn
            (bt:with-timeout (timeout)
              (sb-ext:process-wait process))
            (let ((code (sb-ext:process-exit-code process)))
              (when code
                (if (eq (sb-ext:process-status process) :signaled)
                    (values code :signaled)
                    (values code :exited)))))
        (bt:timeout () nil)
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
  ;; cl-tmux assembles the program/args/environment/directory; cl-tty-kit performs
  ;; the actual sb-ext:run-program :pty t spawn (the same mechanism cl-tmux used
  ;; directly before).  cl-tty-kit always searches PATH for a relative program,
  ;; which subsumes cl-tmux's SEARCH-P (absolute programs like /bin/sh are found
  ;; regardless), so SEARCH-P is no longer threaded through.
  (multiple-value-bind (program args search-p)
      (%target-program-and-args default-command)
    (declare (ignore search-p))
    (let ((pty (cl-tty-kit:make-pty :program program
                                    :args args
                                    :environment environment
                                    :directory (%spawn-directory start-dir)))
          (success nil))
      ;; make-pty has already spawned the child.  Everything below (fd/pid
      ;; extraction, ioctl resize, table registration) can signal; until
      ;; %remember-pty-process records the pty in *pty-processes*, nothing else
      ;; can reap the child or close the master fd.  Guard the post-spawn steps
      ;; so a non-local exit before successful registration tears the pty down
      ;; (closing its process + master stream/fd), avoiding a child/fd leak.
      (unwind-protect
           (let ((master (cl-tty-kit:pty-fd pty))
                 (pid (cl-tty-kit:pty-pid pty)))
             (set-pty-size master rows cols)
             ;; Retain the cl-tty-kit PTY struct keyed by MASTER so it (and the fd
             ;; it owns) survives GC until pty-close reaps it.
             (%remember-pty-process master pty)
             (setf success t)
             ;; SBCL exposes the master stream and pid but not a portable slave
             ;; path, so SLAVE-PATH stays the empty string, preserving the pane
             ;; tty field's existing (empty) value that callers store.
             (values master pid ""))
        (unless success
          (ignore-errors (cl-tty-kit:close-pty pty)))))))

;;; ── Public: PTY I/O ────────────────────────────────────────────────────────
;;;
;;; Byte-transparent master-fd read/write is delegated to cl-tty-kit's
;;; fd-centric layer (fd-read-octets / fd-write-octets), which wraps the same
;;; unix-read/unix-write calls cl-tmux formerly issued via CFFI.  cl-tmux keeps
;;; its own type-guarding and empty-noop conventions here so callers and tests
;;; observe unchanged behavior.

(defun pty-write (fd data)
  "Write DATA (octet vector or UTF-8 string) to the PTY master fd."
  (etypecase data
    (string
     (pty-write fd (babel:string-to-octets data :encoding :utf-8)))
    ((simple-array (unsigned-byte 8) (*))
     ;; Two noop guards preserving the prior raw-write(2) behavior:
     ;;   * empty vector — no write is issued (tests assert this).
     ;;   * negative fd — the "no PTY / dead pane" sentinel (pane-fd -1).  The
     ;;     former CFFI write(2) ignored its return value, so a write to fd -1
     ;;     silently failed; cl-tty-kit's fd-write-octets instead asserts a
     ;;     non-negative fd and signals, so we skip it here.  Real PTY master fds
     ;;     are always positive, and the domain already gates real writes on
     ;;     (> (pane-fd pane) 0).
     (when (and (>= fd 0) (plusp (length data)))
       (cl-tty-kit:fd-write-octets fd data)))))

(defun pty-read-blocking-into (fd buffer)
  "Block until data arrives on FD, read into the caller-supplied octet BUFFER, and
   return a fresh exact-size octet vector holding just the bytes read — or NIL on
   EOF/would-block.  Same return contract as pty-read-blocking (fresh exact-size
   vector, or NIL), but BUFFER is reused across calls to eliminate the per-read
   4 KB allocation on the hot read path: only the (subseq buffer 0 count) result
   (count bytes) is freshly allocated.  Because that result is a copy, BUFFER may
   be safely overwritten by the next read even if the caller retains the result.

   Callers gate this with select-fds, so FD is ready when we read: cl-tty-kit's
   fd-read-octets then returns the available bytes (positive count) without
   waiting to fill BUFFER.  A 0 (EOF) or NIL (would-block) result maps to NIL —
   the 'no data / child gone' signal the reader treats as EOF, matching the
   previous %read-based convention."
  (let ((count (cl-tty-kit:fd-read-octets fd buffer)))
    (when (and count (plusp count))
      (subseq buffer 0 count))))

(defun pty-read-blocking (fd buffer-size)
  "Block until data arrives on FD, then return an octet vector of up to BUFFER-SIZE bytes.
   Returns NIL on EOF or error.

   Thin allocating wrapper over pty-read-blocking-into: allocates a fresh
   BUFFER-SIZE scratch buffer per call and reads into it, preserving the historic
   (fd size) signature for callers/tests that do not manage their own buffer."
  (pty-read-blocking-into
   fd (make-array buffer-size :element-type '(unsigned-byte 8))))

(defun pty-close (master-fd child-pid)
  "Send SIGHUP to the child process and close the PTY master.

   A non-positive CHILD-PID is ignored: kill(-1)/kill(0) broadcast the signal to
   the whole process group (including this process), which must never happen.
   Likewise a negative MASTER-FD is not closed."
  (ignore-errors
    ;; cl-tmux-specific teardown: SIGHUP (NOT cl-tty-kit's SIGTERM->SIGKILL
    ;; escalation) then close the master.  Drop the retained cl-tty-kit PTY
    ;; struct from *pty-processes* so it is no longer reachable; closing its
    ;; SBCL process object closes the master stream (and fd), as before.
    (when (> child-pid 0)
      (cffi:foreign-funcall "kill" :int child-pid :int +sighup+ :int))
    (when (>= master-fd 0)
      (let ((pty (%take-pty-process master-fd)))
        (if pty
            (sb-ext:process-close (cl-tty-kit:pty-process pty))
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
   drive a resize).

   The underlying TIOCGWINSZ query is delegated to cl-tty-kit:terminal-size,
   which returns (values COLUMNS ROWS) — columns first.  We SWAP that to
   cl-tmux's (values ROWS COLS) contract; a transpose here would corrupt every
   pane's geometry.  cl-tty-kit returns (values NIL NIL) when the size is
   unavailable, which fails the integerp/range check below and falls back."
  (multiple-value-bind (cols rows) (cl-tty-kit:terminal-size +stdout-fd+)
    (if (and (integerp rows) (integerp cols)
             (<= 1 rows +max-sane-rows+)
             (<= 1 cols +max-sane-cols+))
        (values rows cols)
        (values +default-term-rows+ +default-term-cols+))))

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
