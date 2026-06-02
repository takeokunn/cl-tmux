(in-package #:cl-tmux/pty)

;;; ── Public: terminal raw mode ──────────────────────────────────────────────
;;;
;;; Saved termios state is keyed by fd rather than stored in a single global.
;;; This prevents data races when two threads independently call
;;; enable-raw-mode! / disable-raw-mode! on different file descriptors.

(defvar *saved-termios-table* (make-hash-table :test #'eql)
  "Maps fd (integer) to the termios struct saved before switching to raw mode.
   Access must be performed with the process-wide lock *termios-table-lock*.")

(defvar *termios-table-lock*
  (bordeaux-threads:make-lock "termios-table-lock")
  "Protects concurrent access to *saved-termios-table*.")

(defmacro with-raw-termios-flags ((termios) &body specs)
  "Apply raw-mode termios edits from a declarative spec.
   Each SPEC is:
     (:clear  accessor flag...)         — clear flags in the named slot
     (:replace accessor (clear...) (set...)) — clear some and set others"
  `(progn
     ,@(mapcar
        (lambda (spec)
          (ecase (first spec)
            (:clear
             (destructuring-bind (_ accessor &rest flags) spec
               (declare (ignore _))
               `(setf (,accessor ,termios)
                      (logand (,accessor ,termios)
                              (lognot (logior ,@flags))))))
            (:replace
             (destructuring-bind (_ accessor clear-list set-list) spec
               (declare (ignore _))
               `(setf (,accessor ,termios)
                      (logior (logand (,accessor ,termios)
                                      (lognot (logior ,@clear-list)))
                              ,@set-list))))))
        specs)))

(defun enable-raw-mode! (fd)
  "Switch FD to raw (unbuffered, no-echo) mode; save old settings in
   *saved-termios-table* keyed by FD (thread-safe)."
  (let ((termios (sb-posix:tcgetattr fd)))
    (bordeaux-threads:with-lock-held (*termios-table-lock*)
      (setf (gethash fd *saved-termios-table*) termios))
    (with-raw-termios-flags (termios)
      (:clear   sb-posix:termios-iflag
                sb-posix:ignbrk sb-posix:brkint sb-posix:parmrk sb-posix:istrip
                sb-posix:inlcr  sb-posix:igncr  sb-posix:icrnl  sb-posix:ixon)
      (:clear   sb-posix:termios-oflag
                sb-posix:opost)
      (:replace sb-posix:termios-cflag
                (sb-posix:csize sb-posix:parenb)
                (sb-posix:cs8))
      (:clear   sb-posix:termios-lflag
                sb-posix:echo sb-posix:echonl sb-posix:icanon
                sb-posix:isig sb-posix:iexten))
    (setf (aref (sb-posix:termios-cc termios) sb-posix:vmin)  1
          (aref (sb-posix:termios-cc termios) sb-posix:vtime) 0)
    (sb-posix:tcsetattr fd sb-posix:tcsaflush termios)))

(defun disable-raw-mode! (fd)
  "Restore terminal settings saved by enable-raw-mode! for FD.
   Uses *saved-termios-table* (per-fd, thread-safe)."
  (let ((saved (bordeaux-threads:with-lock-held (*termios-table-lock*)
                 (prog1 (gethash fd *saved-termios-table*)
                   (remhash fd *saved-termios-table*)))))
    (when saved
      (sb-posix:tcsetattr fd sb-posix:tcsaflush saved))))
