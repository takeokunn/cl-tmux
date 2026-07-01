(in-package #:cl-tmux/pty)

;;;; FFI declarations and platform constants for the PTY subsystem.
;;;;
;;;; This file is pure data: no side effects, no I/O, no process operations.
;;;; It declares the C surface, platform constants, and the inline fd_set
;;;; helpers that the logic in pty.lisp builds on.

;;; ── Load sb-posix ──────────────────────────────────────────────────────────

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-posix))

;;; ── CFFI: libc surface (no extra library needed) ───────────────────────────

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

;;; Standard file descriptor numbers
(defconstant +stdin-fd+  0 "Standard input.")
(defconstant +stdout-fd+ 1 "Standard output.")
(defconstant +stderr-fd+ 2 "Standard error.")

;;; POSIX signal numbers
(defconstant +sighup+ 1 "POSIX SIGHUP — sent to a process group leader on terminal close.")

;;; ioctl request codes (TIOC*)
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

(declaim (inline fd-zero! fd-set! fd-isset-p))

;;; fd_set words are accessed as :UINT32, not :INT32: bit 31 corresponds to
;;; (ash 1 31) = 2147483648, which is out of range for (signed-byte 32).  With
;;; :INT32 any fd whose (mod fd 32) = 31 (e.g. fd 31, 63, …) would signal a
;;; TYPE-ERROR when its bit is set.  Unsigned access stores the raw bit pattern.

(defun fd-zero! (ptr)
  "Clear every bit in the fd_set at foreign pointer PTR (the FD_ZERO macro)."
  (declare (type cffi:foreign-pointer ptr))
  (dotimes (i +fd-set-words+)
    (setf (cffi:mem-aref ptr :uint32 i) 0)))

(defun fd-set! (fd ptr)
  "Set the bit for FD in the fd_set at foreign pointer PTR (the FD_SET macro)."
  (declare (type fixnum fd)
           (type cffi:foreign-pointer ptr))
  (let ((word (floor fd 32))
        (bit  (mod   fd 32)))
    (setf (cffi:mem-aref ptr :uint32 word)
          (logior (cffi:mem-aref ptr :uint32 word) (ash 1 bit)))))

(defun fd-isset-p (fd ptr)
  "Return T when FD's bit is set in the fd_set at foreign pointer PTR
   (the FD_ISSET macro)."
  (declare (type fixnum fd)
           (type cffi:foreign-pointer ptr))
  (let ((word (floor fd 32))
        (bit  (mod   fd 32)))
    (not (zerop (logand (cffi:mem-aref ptr :uint32 word) (ash 1 bit))))))
