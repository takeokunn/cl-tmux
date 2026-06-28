(in-package #:cl-tmux/ports)

;;;; PTY Port — domain-side abstraction for PTY operations.
;;;;
;;;; Domain code (cl-tmux/model) calls spawn-pty/write-pty/resize-pty/close-pty.
;;;; Infrastructure (cl-tmux/pty) installs concrete implementations via
;;;; install-pty-port at server or test setup time.
;;;;
;;;; Dependency Inversion Principle:
;;;;   Domain (high-level)       → this abstraction
;;;;   Infrastructure (low-level) → implements this abstraction

;;; ── Port variables ───────────────────────────────────────────────────────────
;;;
;;; Each var holds a function installed by install-pty-port (cl-tmux/pty).
;;; Initial value NIL; domain functions guard with (> (pane-fd pane) 0) so
;;; these are only called when a real PTY fd exists.

(defvar *spawn-pty* nil
  "Function (rows cols &key start-dir default-command environment) → (values fd pid tty).
   Installed by cl-tmux/pty:install-pty-port.")

(defvar *write-pty* nil
  "Function (fd bytes) → nil.
   Installed by cl-tmux/pty:install-pty-port.")

(defvar *resize-pty* nil
  "Function (fd rows cols) → nil.
   Installed by cl-tmux/pty:install-pty-port.")

(defvar *close-pty* nil
  "Function (fd pid) → nil.
   Installed by cl-tmux/pty:install-pty-port.")

;;; ── Port functions ───────────────────────────────────────────────────────────
;;;
;;; These are the only PTY-related names the domain model imports.
;;; Replacing *spawn-pty* / *write-pty* / *resize-pty* / *close-pty* in tests
;;; allows mocking at the abstraction boundary rather than at the C-FFI layer.

(defun spawn-pty (rows cols &key start-dir default-command environment)
  "Spawn a PTY-backed shell process. Returns (values fd pid slave-path)."
  (funcall *spawn-pty* rows cols
           :start-dir start-dir
           :default-command default-command
           :environment environment))

(defun write-pty (fd bytes)
  "Write BYTES to PTY file descriptor FD."
  (funcall *write-pty* fd bytes))

(defun resize-pty (fd rows cols)
  "Resize the PTY at FD to ROWS x COLS."
  (funcall *resize-pty* fd rows cols))

(defun close-pty (fd pid)
  "Close PTY master FD and signal child PID."
  (funcall *close-pty* fd pid))
