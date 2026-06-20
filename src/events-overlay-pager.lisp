;;;; events-overlay-pager.lisp --- overlay pager ESC continuation handlers -----

;;; This file owns the pager ESC continuation states used while an overlay is
;;; active.  It stays separate from mouse dispatch so the input state machine is
;;; easier to scan and the mouse file keeps a single responsibility.

(in-package #:cl-tmux)

;;; ── Overlay pager escape-sequence handler ────────────────────────────────────

;;; When the overlay pager is active and ESC is received, we accumulate the byte
;;; sequence.  ESC [ A (Up) scrolls -1 and ESC [ B (Down) scrolls +1.  Any other
;;; sequence (including bare ESC) dismisses the overlay.

;;; The overlay escape handler uses two named continuation functions so each
;;; protocol state is explicit and independently readable.

(defun %overlay-escape-second-byte (buffer)
  "CPS state: received ESC, now reading the second byte.
   If the second byte is '[' we continue to %overlay-escape-final; otherwise dismiss."
  (lambda (_ignored-session byte)
    (declare (ignore _ignored-session))
    (vector-push-extend byte buffer)
    (if (= byte +byte-csi-bracket+)
        (values nil (%overlay-escape-final buffer))
        (progn
          (clear-overlay)
          (setf *dirty* t)
          (values nil #'%ground-input-state)))))

(defun %overlay-escape-final (buffer)
  "CPS state: received ESC '[', now reading the final byte.
   Up arrow scrolls -1; Down arrow scrolls +1; anything else dismisses."
  (lambda (_ignored-session byte)
    (declare (ignore _ignored-session))
    (vector-push-extend byte buffer)
    (cond
      ;; ESC [ A - Up arrow: scroll overlay up
      ((= byte +byte-arrow-up+)
       (overlay-scroll -1)
       (setf *dirty* t)
       (values nil #'%ground-input-state))
      ;; ESC [ B - Down arrow: scroll overlay down
      ((= byte +byte-arrow-down+)
       (overlay-scroll 1)
       (setf *dirty* t)
       (values nil #'%ground-input-state))
      ;; Unrecognised final byte: dismiss the overlay
      (t
       (clear-overlay)
       (setf *dirty* t)
       (values nil #'%ground-input-state)))))
