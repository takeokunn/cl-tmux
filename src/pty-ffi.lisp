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

(declaim (inline fd-zero! fd-set! fd-isset-p))

(defun fd-zero! (ptr)
  (declare (type cffi:foreign-pointer ptr))
  (dotimes (i +fd-set-words+)
    (setf (cffi:mem-aref ptr :int32 i) 0)))

(defun fd-set! (fd ptr)
  (declare (type fixnum fd)
           (type cffi:foreign-pointer ptr))
  (let ((word (floor fd 32))
        (bit  (mod   fd 32)))
    (setf (cffi:mem-aref ptr :int32 word)
          (logior (cffi:mem-aref ptr :int32 word) (ash 1 bit)))))

(defun fd-isset-p (fd ptr)
  (declare (type fixnum fd)
           (type cffi:foreign-pointer ptr))
  (let ((word (floor fd 32))
        (bit  (mod   fd 32)))
    (not (zerop (logand (cffi:mem-aref ptr :int32 word) (ash 1 bit))))))
