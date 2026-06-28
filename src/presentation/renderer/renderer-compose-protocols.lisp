(in-package #:cl-tmux/renderer)

;;;; Terminal protocol control helpers for the cl-tmux renderer.
;;;;
;;;; This file owns the outer-terminal side effects that are not part of the
;;;; frame compositing pipeline itself: clear-display and the reporting toggles
;;;; for mouse, extended keys, and focus events.

(defun clear-display ()
  "Erase the entire terminal and move cursor home."
  (format t "~C[2J~C[H" +esc+ +esc+)
  (force-output))

;;; ── Mouse reporting control ────────────────────────────────────────────────

(defun enable-mouse-reporting ()
  "Emit DEC private mode sequences to enable mouse reporting on the outer terminal.
   Enables X10 tracking (?1000h), button-event tracking (?1002h), and SGR
   extended encoding (?1006h).  Flushes stdout immediately."
  (format t "~C[?1000h~C[?1002h~C[?1006h" +esc+ +esc+ +esc+)
  (force-output))

(defun disable-mouse-reporting ()
  "Emit DEC private mode sequences to disable mouse reporting on the outer terminal.
   Disables SGR encoding (?1006l), button-event tracking (?1002l), and X10
   tracking (?1000l).  Flushes stdout immediately."
  (format t "~C[?1006l~C[?1002l~C[?1000l" +esc+ +esc+ +esc+)
  (force-output))

;;; ── Extended-keys (CSI u / fixterms) reporting control ─────────────────────

(defun extended-keys-level (option-value)
  "Map the `extended-keys` option value to the modifyOtherKeys level: \"on\" → 1,
   \"always\" → 2, anything else (\"off\", NIL) → NIL (reporting stays off)."
  (cond
    ((null option-value) nil)
    ((string-equal option-value "on")     1)
    ((string-equal option-value "always") 2)
    (t nil)))

(defun enable-extended-keys (option-value)
  "Emit CSI > 4 ; N m to enable extended (CSI-u) key reporting on the outer terminal
   when OPTION-VALUE (the `extended-keys` option) is \"on\" (level 1) or \"always\"
   (level 2).  Returns the level emitted, or NIL when reporting stayed off.  Flushes
   stdout immediately."
  (let ((level (extended-keys-level option-value)))
    (when level
      (format t "~C[>4;~Dm" +esc+ level)
      (force-output))
    level))

(defun disable-extended-keys ()
  "Emit CSI > 4 ; 0 m to reset extended-keys reporting on the outer terminal.
   Flushes stdout immediately."
  (format t "~C[>4;0m" +esc+)
  (force-output))

;;; ── Focus event reporting (?1004) ───────────────────────────────────────────

(defun enable-focus-reporting ()
  "Emit ?1004h to enable focus-event reporting on the outer terminal.  Flushes."
  (format t "~C[?1004h" +esc+)
  (force-output))

(defun disable-focus-reporting ()
  "Emit ?1004l to disable focus-event reporting on the outer terminal.  Flushes."
  (format t "~C[?1004l" +esc+)
  (force-output))
