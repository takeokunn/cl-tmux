(in-package #:cl-tmux)

;;;; CSI-u legacy fallback helpers.
;;;;
;;;; This file keeps the transparent reinjection path separate from the
;;;; canonical CSI-u parser/namer in events-keystroke-keys.lisp.

(defun %csi-u-control-byte (codepoint)
  "The legacy control byte for Ctrl + CODEPOINT, or NIL when CODEPOINT has no
   control form.  Letters a-z / A-Z -> 1-26; Space and @ -> NUL (0); the symbols
   [ \\ ] ^ _ -> 27-31.  This is the byte a non-extended terminal would emit,
   used as the transparent fallback when an extended Ctrl chord is unbound."
  (cond
    ((<= 97 codepoint 122) (- codepoint 96))  ; a-z -> 1..26
    ((<= 65 codepoint 90)  (- codepoint 64))  ; A-Z -> 1..26
    ((= codepoint 32) 0)                      ; Space -> NUL
    ((= codepoint 64) 0)                      ; @     -> NUL
    ((<= 91 codepoint 95) (- codepoint 64))   ; [ \ ] ^ _ -> 27..31
    (t nil)))

(defun %csi-u-legacy-octets (codepoint bits)
  "The legacy byte encoding for the CSI-u chord CODEPOINT + modifier BITS (bit0
   Shift, bit1 Alt, bit2 Ctrl), or NIL when the chord has no one-/two-byte legacy
   form and must be matched by name instead.  Mirrors what a non-extended terminal
   sends, so re-injecting these octets keeps the chord transparent to the inner
   application:
     C-M-<key> -> ESC ^X   |   C-<key> -> ^X
     M-<char>  -> ESC <ch> |   plain / Shift-only printable -> <ch>"
  (let ((ctrl (logbitp 2 bits))
        (alt  (logbitp 1 bits))
        (cb   (%csi-u-control-byte codepoint)))
    (cond
      ((and ctrl alt cb)                        (vector +byte-esc+ cb))
      ((and ctrl cb)                            (vector cb))
      ((and alt (not ctrl) (<= 33 codepoint 126)) (vector +byte-esc+ codepoint))
      ((and (not ctrl) (not alt) (<= 32 codepoint 126)) (vector codepoint))
      (t nil))))

(defun %feed-octets-through-ground (session octets)
  "Re-inject OCTETS into the keystroke state machine starting at ground state,
   threading the CPS continuation so a multi-byte legacy form (the ESC <char> meta
   encoding) dispatches exactly as if the bytes had been typed.  This reuses the
   whole root/prefix/custom-table/copy-mode/forward dispatch tree for the legacy
   fallback of an unbound extended-keys chord."
  (let ((state #'%ground-input-state))
    (loop for b across octets
          do (multiple-value-bind (_ next) (funcall state session b)
               (declare (ignore _))
               (setf state (or next #'%ground-input-state))))))

(defun %handle-escape-csi-u (session buffer length)
  "Decode and dispatch a complete CSI-u (extended-keys) sequence
   ESC [ <codepoint> ; <mod> u in BUFFER.  A root-table binding for the canonical
   chord name wins first (covers string-only chords like C-S-a / S-Tab that have no
   legacy byte - the disambiguation extended-keys exists for).  In copy mode, the
   active copy-mode table is checked before the root table so extended C-M-* mode
   bindings dispatch directly.  Otherwise the chord falls back to its legacy byte
   form, re-injected through ground state so Ctrl / Alt / plain chords stay
   transparent (and still hit any `bind -n C-a`, the prefix key, copy mode, or the
   pane).  A chord with neither a binding nor a legacy form forwards the raw
   sequence."
  (multiple-value-bind (codepoint mod-value) (%csi-u-parse-params buffer length)
    (let* ((bits (and codepoint (max 0 (- mod-value 1))))
           (key  (and codepoint (%csi-u-key-name codepoint mod-value))))
      (cond
        ((null key)
         (unless (%copy-mode-active-p session)
           (%forward-octets-synchronized session (subseq buffer 0 length))))
        ((and (%copy-mode-active-p session)
              (%try-bound-string-key session (%active-copy-mode-table) key)))
        ((%try-bound-string-key session +table-root+ key))
        (t
         (let ((octets (%csi-u-legacy-octets codepoint bits)))
           (if octets
               (%feed-octets-through-ground session octets)
               (unless (%copy-mode-active-p session)
                 (%forward-octets-synchronized session (subseq buffer 0 length))))))))))
