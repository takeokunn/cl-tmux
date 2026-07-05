(in-package #:cl-tmux/terminal/types)

;;;; Screen state-mutation helpers (LOGIC layer).
;;;;
;;;; This file contains functions that mutate exactly one or a small fixed set
;;;; of screen slots.  They are separated from screen.lisp so that the DATA
;;;; layer (screen.lisp) contains only the defstruct definition, pure grid
;;;; accessors, and side-effect-free constructors.
;;;;
;;;; Load order:
;;;;   cell.lisp → screen.lisp → screen-metadata.lisp → screen-resize.lisp
;;;;   → screen-logic.lisp → …
;;;;
;;;; All three functions are exported from cl-tmux/terminal/types (declared in
;;;; src/bootstrap/package-terminal.lisp) and re-exported through the
;;;; cl-tmux/terminal umbrella package.

;;; ── Dirty-flag mutation ────────────────────────────────────────────────────

(defun screen-clear-dirty (screen)
  "Clear the dirty flag on SCREEN, marking it as freshly rendered.
   The renderer calls this after every successful frame paint so the next
   PTY write can re-arm the flag via (setf (screen-dirty-p …) t)."
  (setf (screen-dirty-p screen) nil))

;;; ── BEL consumption ────────────────────────────────────────────────────────

(defun screen-consume-bell (screen)
  "Return T and clear SCREEN's bell-pending flag when a BEL is pending.
   Returns NIL without side effects when no bell is pending.

   The renderer (cl-tmux/renderer-compose) calls this once per frame to relay
   a BEL to the outer terminal; the atomic test-and-clear here ensures the bell
   is delivered exactly once even when multiple frames race."
  (when (screen-bell-pending screen)
    (setf (screen-bell-pending screen) nil)
    t))

;;; ── Queue draining ───────────────────────────────────────────────────────────

(defun screen-drain-queue (screen queue-reader queue-writer)
  "Atomically read and clear a push-accumulated queue slot on SCREEN, returning
   the queued items in push order (oldest first).
   QUEUE-READER reads the current (reverse-chronological) list from SCREEN.
   QUEUE-WRITER is called with SCREEN and NIL to clear the slot.
   Used by the renderer to drain the passthrough-queue and clipboard-queue
   without mutating SCREEN's slots directly from the presentation layer."
  (let ((queued (nreverse (funcall queue-reader screen))))
    (funcall queue-writer screen nil)
    queued))

;;; ── SGR pen reset ──────────────────────────────────────────────────────────
;;;
;;; Both cl-tmux/terminal/sgr (DISPATCH layer) and cl-tmux/terminal/actions
;;; (modes-d.lisp, LOGIC layer) perform an identical five-slot SGR reset.
;;; The canonical definition lives in this shared file so neither layer needs
;;; to reference the other, resolving the historical load-order circularity.

(declaim (inline reset-sgr-pen))
(defun reset-sgr-pen (screen)
  "Reset all five SGR pen slots of SCREEN to VT100 power-on defaults:
   foreground / background = +default-color+ (terminal default), all
   attribute bits clear.  Inlined for use in the hot SGR dispatch path."
  (setf (screen-cur-fg       screen) +default-color+
        (screen-cur-bg       screen) +default-color+
        (screen-cur-attrs    screen) 0
        (screen-cur-attrs2   screen) 0
        (screen-cur-ul-color screen) 0))
