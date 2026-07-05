(in-package #:cl-tmux)

;;;; Shared keystroke state and escape-buffer construction.

;;; ── Copy-mode numeric prefix ─────────────────────────────────────────────────
;;;
;;; *copy-mode-prefix* accumulates digit bytes (0-9) pressed while copy mode
;;; is active.  When a non-digit navigation key is pressed, the accumulated
;;; count (clamped to min 1) is applied and the prefix is reset to 0.
;;; The variable lives on the main event-loop thread; no locking is needed.

(defvar *copy-mode-prefix* 0
  "Accumulated numeric prefix for copy-mode repeat counts.
   Set to 0 between commands.  Updated exclusively on the event-loop thread.")

;;; ── assume-paste-time (tmux server_client_assume_paste) ─────────────────────
;;;
;;; When two ground-state keys arrive within assume-paste-time milliseconds,
;;; tmux assumes a paste is in progress and bypasses key-binding interpretation
;;; (root-table -n bindings and the prefix key), forwarding the bytes to the
;;; pane instead — so pasted text containing bound characters does not trigger
;;; commands.  Bracketed paste (DECSET 2004) is the primary mechanism; this is
;;; the fallback for terminals pasting without it.

(defvar *last-ground-key-time* nil
  "internal-real-time of the previous PANE-FORWARDED ground-state key byte, or
   NIL before any.  Only forwarded (content) bytes stamp it: a paste burst is a
   stream of pane content, so \"the previous key was content, moments ago\" is
   the paste signal — binding/prefix keys do not count as paste context.
   Updated exclusively on the event-loop thread.")

(defun %stamp-ground-key-time ()
  "Record the arrival time of a pane-forwarded ground-state key byte."
  (setf *last-ground-key-time* (get-internal-real-time)))

(defun %assume-paste-byte-p ()
  "True when this key arrives within assume-paste-time milliseconds of the
   previous pane-forwarded key.  assume-paste-time 0 (or a non-integer value)
   disables the heuristic."
  (let ((prev *last-ground-key-time*)
        (ms   (let ((value (cl-tmux/options:get-option "assume-paste-time")))
                (if (integerp value) value 0))))
    (and prev
         (plusp ms)
         (< (- (get-internal-real-time) prev)
            (* ms (floor internal-time-units-per-second 1000))))))

(defun %make-escape-buffer (byte)
  "Return a fresh adjustable byte vector with BYTE as its sole element.
   Used to start escape-sequence accumulation: the ESC byte is the first element
   and subsequent bytes are appended as the CPS continuation reads them."
  (let ((escape-buffer (make-array 8 :element-type '(unsigned-byte 8)
                                      :fill-pointer 0 :adjustable t)))
    (vector-push-extend byte escape-buffer)
    escape-buffer))
