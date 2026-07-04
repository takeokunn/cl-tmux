(in-package #:cl-tmux/test)

;;;; Prompt, overlay, input, and format-context fixtures.

(defmacro with-clean-prompt (&body body)
  "Dynamically bind *prompt* to NIL and cl-tmux::*dirty* to NIL so prompt
   state never leaks between tests and dirty flags start clean."
  `(let ((*prompt* nil) (cl-tmux::*dirty* nil)) ,@body))

(defmacro with-clean-overlay (&body body)
  "Dynamically bind the four overlay specials (*overlay*, *overlay-scroll-offset*,
   *overlay-shown-at*, *display-panes-active*) to their inactive defaults so
   overlay state never leaks between tests.  Mirrors with-clean-prompt for the
   sibling overlay/popup/menu test file."
  `(let ((*overlay* nil)
         (*overlay-scroll-offset* 0)
         (*overlay-shown-at* 0)
         (*display-panes-active* nil))
     ,@body))

(defmacro with-empty-registry (&body body)
  "Bind *server-sessions* to NIL for the duration of BODY.
   Thin wrapper over `with-registered-sessions` for the empty-registry case."
  `(with-registered-sessions () ,@body))

(defmacro with-input-state ((var) &body body)
  "Bind VAR to a fresh make-input-state for use with process-byte tests."
  `(let ((,var (cl-tmux::make-input-state)))
     ,@body))

(defun feed-bytes (session input-state bytes)
  "Feed each element of BYTES to SESSION through INPUT-STATE one byte at a
   time via cl-tmux::process-byte, returning the outcome of the final byte.
   Removes the repeated 'feed ESC, feed the next byte, ...' one-call-per-byte
   pattern used to simulate multi-byte escape sequences (arrow keys, X10/SGR
   mouse reports, focus-in/out) arriving on the wire one octet at a time."
  (let ((outcome nil))
    (dolist (byte bytes outcome)
      (setf outcome (cl-tmux::process-byte session byte input-state)))))

(defun seed-scrollback (screen n)
  "Give SCREEN N dummy scrollback rows so copy-mode-scroll has room to move."
  (setf (cl-tmux/terminal/types::screen-scrollback screen)
        (loop repeat n collect (vector))))

;;; The 4-line let* that builds sess/win/pane/ctx from make-fake-session appears
;;; across format tests.  This macro encodes the standard extraction chain once.

(defmacro with-format-context ((sess-var win-var pane-var ctx-var)
                               (&key (nwindows 1) (npanes 1))
                               &body body)
  "Bind SESS-VAR/WIN-VAR/PANE-VAR/CTX-VAR to the first window, first pane, and
   format context of a fresh fake session with NWINDOWS windows and NPANES panes.
   Eliminates the recurring 4-line let* fixture in format-tests.lisp."
  `(let* ((,sess-var (make-fake-session :nwindows ,nwindows :npanes ,npanes))
          (,win-var  (first (cl-tmux/model:session-windows ,sess-var)))
          (,pane-var (first (cl-tmux/model:window-panes ,win-var)))
          (,ctx-var  (cl-tmux/format:format-context-from-session
                      ,sess-var ,win-var ,pane-var)))
     ,@body))
