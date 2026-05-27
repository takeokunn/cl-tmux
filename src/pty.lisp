(in-package #:cl-tmux/pty)

;;;; PTY management, terminal raw mode, and multiplexed I/O.
;;;;
;;;; Implemented in pure Common Lisp using:
;;;;   • sb-posix  — fork, setsid, dup2, execv, tcgetattr/tcsetattr
;;;;   • CFFI      — posix_openpt, grantpt, unlockpt, ptsname,
;;;;                 ioctl, select (all from libc — no custom C)

;;; ── Load sb-posix ──────────────────────────────────────────────────────────

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-posix))

;;; ── CFFI: libc surface (no extra library needed) ───────────────────────────

(cffi:defcfun ("posix_openpt" %posix-openpt) :int (oflag :int))
(cffi:defcfun ("grantpt"      %grantpt)      :int (fd :int))
(cffi:defcfun ("unlockpt"     %unlockpt)     :int (fd :int))
;;; ptsname returns a pointer to a static string — copy it immediately
(cffi:defcfun ("ptsname"      %ptsname)      :string (fd :int))

;;; Raw fd read / write
(cffi:defcfun ("read"  %read)  :long
  (fd :int) (buf :pointer) (count :unsigned-long))
(cffi:defcfun ("write" %write) :long
  (fd :int) (buf :pointer) (count :unsigned-long))

;;; select(2)
(cffi:defcfun ("select" %select) :int
  (nfds      :int)
  (readfds   :pointer)
  (writefds  :pointer)
  (exceptfds :pointer)
  (timeout   :pointer))

;;; ── Platform constants ─────────────────────────────────────────────────────

;;; open(2) flags
(defconstant +o-rdwr+
  #+darwin #x0002
  #-darwin #o000002)

(defconstant +o-noctty+
  #+darwin  #x20000
  #-darwin  #o000400)

;;; ioctl request codes (TIOC*)
(defconstant +tiocsctty+
  #+darwin #x20007461
  #-darwin #x540E)

(defconstant +tiocgwinsz+
  #+darwin #x40087468
  #-darwin #x5413)

(defconstant +tiocswinsz+
  #+darwin #x80087467
  #-darwin #x5414)

;;; struct winsize — 4 × uint16, layout identical on every platform
(cffi:defcstruct winsize
  (ws-row    :uint16)
  (ws-col    :uint16)
  (ws-xpixel :uint16)
  (ws-ypixel :uint16))

;;; ── fd_set helpers for select(2) ───────────────────────────────────────────
;;;
;;; FD_SET / FD_ZERO / FD_ISSET are C macros; we implement them in Lisp.
;;; An fd_set is 128 bytes (32 int32 words) on 64-bit Linux and macOS.

(defconstant +fd-set-words+ 32)

(defun fd-zero! (ptr)
  (dotimes (i +fd-set-words+)
    (setf (cffi:mem-aref ptr :int32 i) 0)))

(defun fd-set! (fd ptr)
  (let ((word (floor fd 32))
        (bit  (mod   fd 32)))
    (setf (cffi:mem-aref ptr :int32 word)
          (logior (cffi:mem-aref ptr :int32 word) (ash 1 bit)))))

(defun fd-isset-p (fd ptr)
  (let ((word (floor fd 32))
        (bit  (mod   fd 32)))
    (not (zerop (logand (cffi:mem-aref ptr :int32 word) (ash 1 bit))))))

;;; ── Public: terminal raw mode ──────────────────────────────────────────────

(defvar *saved-termios* nil)

(defun enable-raw-mode! (fd)
  "Switch FD to raw (unbuffered, no-echo) mode; save old settings."
  (let ((termios (sb-posix:tcgetattr fd)))
    (setf *saved-termios* termios)
    ;; cfmakeraw equivalent built from individual flag bits
    (setf (sb-posix:termios-iflag termios)
          (logand (sb-posix:termios-iflag termios)
                  (lognot (logior sb-posix:ignbrk sb-posix:brkint
                                  sb-posix:parmrk sb-posix:istrip
                                  sb-posix:inlcr  sb-posix:igncr
                                  sb-posix:icrnl  sb-posix:ixon))))
    (setf (sb-posix:termios-oflag termios)
          (logand (sb-posix:termios-oflag termios)
                  (lognot sb-posix:opost)))
    (setf (sb-posix:termios-cflag termios)
          (logior (logand (sb-posix:termios-cflag termios)
                          (lognot (logior sb-posix:csize sb-posix:parenb)))
                  sb-posix:cs8))
    (setf (sb-posix:termios-lflag termios)
          (logand (sb-posix:termios-lflag termios)
                  (lognot (logior sb-posix:echo   sb-posix:echonl
                                  sb-posix:icanon sb-posix:isig
                                  sb-posix:iexten))))
    (setf (aref (sb-posix:termios-cc termios) sb-posix:vmin)  1
          (aref (sb-posix:termios-cc termios) sb-posix:vtime) 0)
    (sb-posix:tcsetattr fd sb-posix:tcsaflush termios)))

(defun disable-raw-mode! (fd)
  "Restore terminal settings saved by enable-raw-mode!."
  (when *saved-termios*
    (sb-posix:tcsetattr fd sb-posix:tcsaflush *saved-termios*)
    (setf *saved-termios* nil)))

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

(defun forkpty-with-shell (rows cols)
  "Fork a child shell process on a fresh PTY of size ROWS×COLS.
   Parent: returns (values master-fd child-pid).
   Child:  execs *default-shell* and never returns to Lisp."
  (multiple-value-bind (master slave-path)
      (open-pty rows cols)
    (let ((pid (sb-posix:fork)))
      (cond
        ((< pid 0) (error "fork failed"))

        ;; ── Child ──────────────────────────────────────────────────────────
        ((= pid 0)
         ;; Become session leader → PTY becomes our controlling terminal.
         (sb-posix:setsid)
         (let ((slave (sb-posix:open slave-path (logior +o-rdwr+ +o-noctty+) 0)))
           ;; TIOCSCTTY makes this PTY the controlling terminal.
           (cffi:foreign-funcall "ioctl"
                                 :int slave
                                 :unsigned-long +tiocsctty+
                                 :int 0
                                 :int)
           ;; Wire slave as stdin/stdout/stderr.
           (sb-posix:dup2 slave 0)
           (sb-posix:dup2 slave 1)
           (sb-posix:dup2 slave 2)
           (sb-posix:close slave))
         ;; Parent's master fd is not needed in the child.
         (sb-posix:close master)
         ;; Replace this image with the shell.
         ;; sb-posix:execv is absent on macOS SBCL, so call libc execv via CFFI.
         ;; On error, _exit immediately to avoid running parent-image atexit hooks.
         (let ((shell cl-tmux/config:*default-shell*))
           (cffi:with-foreign-string (path-ptr shell)
             (cffi:with-foreign-string (arg0-ptr shell)
               (cffi:with-foreign-object (argv :pointer 2)
                 (setf (cffi:mem-aref argv :pointer 0) arg0-ptr
                       (cffi:mem-aref argv :pointer 1) (cffi:null-pointer))
                 (cffi:foreign-funcall "execv"
                                       :pointer path-ptr
                                       :pointer argv
                                       :int)))))
         ;; If execv returned, exec failed.
         (cffi:foreign-funcall "_exit" :int 1 :void))

        ;; ── Parent ─────────────────────────────────────────────────────────
        (t
         (values master pid))))))

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
           (loop for i below len
                 do (setf (cffi:mem-aref buf :uint8 i) (aref data i)))
           (%write fd buf len)))))))

(defun pty-read-blocking (fd buf-size)
  "Block until data arrives on FD, then return an octet vector.
   Returns NIL on EOF or error."
  (cffi:with-foreign-object (raw :uint8 buf-size)
    (let ((n (%read fd raw buf-size)))
      (when (plusp n)
        (let ((result (make-array n :element-type '(unsigned-byte 8))))
          (loop for i below n
                do (setf (aref result i) (cffi:mem-aref raw :uint8 i)))
          result)))))

(defun pty-close (master-fd child-pid)
  "Send SIGHUP to the child process and close the PTY master."
  (ignore-errors
    (cffi:foreign-funcall "kill"  :int child-pid :int 1 :int)  ; SIGHUP = 1
    (sb-posix:close master-fd)))

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

(defun terminal-size ()
  "Return (values rows cols) of the terminal attached to stdout.
   Falls back to 24×80 if ioctl fails or reports an out-of-range size
   (a transient 0×0 or garbage read must not drive a resize)."
  (cffi:with-foreign-object (ws '(:struct winsize))
    (let ((r (cffi:foreign-funcall "ioctl"
                                   :int 1               ; stdout
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
