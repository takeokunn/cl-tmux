(in-package #:cl-tmux/terminal/parser)

;;;; OSC accumulator and dispatcher.
;;;;
;;;; Helper definitions live in parser-osc-helpers.lisp.

;;; ── OSC payload utilities ────────────────────────────────────────────────────

(defun %parse-osc-command (payload semicolon-position)
  "Parse the OSC command integer from PAYLOAD up to SEMICOLON-POSITION.
   Returns the integer, or NIL if the command field is not a valid integer."
  (handler-case
      (parse-integer (subseq payload 0 semicolon-position))
    (error () nil)))

(defun %handle-osc-52 (text)
  "Handle OSC 52 clipboard write: decode Base64 payload and call *osc52-handler*.
   Format: Pc ; Pd  where Pc is the clipboard target and Pd is Base64-encoded data
   or '?' for a read request (read requests are silently ignored)."
  (let* ((inner-semi   (position #\; text))
         (payload-data (and inner-semi (subseq text (1+ inner-semi)))))
    (when (and payload-data (string/= payload-data "?"))
      (let* ((decoded-bytes (and payload-data (%base64-decode payload-data)))
             (decoded-text  (and decoded-bytes
                                 (handler-case
                                     (babel:octets-to-string decoded-bytes :encoding :utf-8)
                                   (error () nil)))))
        (when (and decoded-text *osc52-handler*)
          (funcall *osc52-handler* decoded-text))))))

(define-osc-rules
  ((0 1 2)
   (set-screen-title screen body))
  (8
   (%handle-osc-8 screen body))
  (7
   (set-screen-cwd screen (%osc7-path body)))
  (10
   (%osc-color-command screen 10 body (screen-osc-default-fg screen)
                       #'(lambda (rgb)
                           (setf (screen-osc-default-fg screen) rgb))))
  (110
   (setf (screen-osc-default-fg screen) +osc-default-fg+))
  (11
   (%osc-color-command screen 11 body (screen-osc-default-bg screen)
                       #'(lambda (rgb)
                           (setf (screen-osc-default-bg screen) rgb))))
  (111
   (setf (screen-osc-default-bg screen) +osc-default-bg+))
  (4
   (%handle-osc-4 screen body))
  (52
   (%handle-osc-52 body)))

(defun %dispatch-osc (screen payload-buffer)
  "Parse accumulated OSC payload PAYLOAD-BUFFER and apply side effects to SCREEN.
   Handles:
     OSC 0/1/2        — set the window title
     OSC 7            — report current working directory
     OSC 10/11        — query/set default foreground/background colour
     OSC 110/111      — reset default foreground/background colour
     OSC 52           — write clipboard data (Base64-encoded)
   The command field is the integer before the first ';'; a payload with NO ';'
   (e.g. OSC 110) is a parameterless command with an empty body."
  (let* ((payload  (babel:octets-to-string payload-buffer :encoding :utf-8 :errorp nil))
         (semi-pos (position #\; payload))
         (command  (%parse-osc-command payload (or semi-pos (length payload))))
         (body     (if semi-pos (subseq payload (1+ semi-pos)) "")))
    (when command
      (%dispatch-osc-command screen command body))))

;;; ── CPS OSC accumulator continuations ──────────────────────────────────────
;;;
;;; make-osc-k builds the accumulator closure that collects raw OSC payload
;;; bytes.  make-osc-st-k builds the single-byte "waiting for backslash" bridge
;;; state used after an ESC inside an OSC sequence (potential ESC \ ST).
;;;
;;; Both continuations receive (screen byte) and return the next state function,
;;; matching the CPS state machine contract defined by define-state.

(defun make-osc-st-k (buffer)
  "Return a continuation waiting for the backslash of ESC \\ (String Terminator).
   BUFFER is the accumulated OSC payload so far.
   On backslash: dispatch the payload and return ground-state.
   On any other byte: return ground-state without dispatching (malformed ST)."
  (lambda (screen-arg byte)
    (declare (type screen screen-arg) (type (unsigned-byte 8) byte))
    (when (= byte #x5C)
      (%dispatch-osc screen-arg buffer))
    #'ground-state))

(defun make-osc-k (buffer)
  "Return a continuation that accumulates OSC payload bytes into BUFFER.
   Dispatches to %DISPATCH-OSC on BEL (#x07) or the start of ESC \\ termination."
  (lambda (screen-arg byte)
    (declare (type screen screen-arg) (type (unsigned-byte 8) byte))
    (cond
      ((= byte #x07)
       ;; BEL: OSC terminated — dispatch and return to ground.
       (%dispatch-osc screen-arg buffer)
       #'ground-state)
      ((= byte #x1B)
       ;; Possible ESC \ (ST) — hand off to the bridge state.
       (make-osc-st-k buffer))
      (t
       ;; Continue accumulating payload bytes.
       (vector-push-extend byte buffer)
       (make-osc-k buffer)))))
