(in-package #:cl-tmux)

;;;; Event loop and keystroke processing.

;;; ── Additional key bindings ──────────────────────────────────────────────────
;;;
;;; C-b ] → paste the top paste buffer into the active pane.
;;; This supplements the table in config.lisp (which sets C-b [ for copy-mode-enter).

(set-key-binding #\] :paste-buffer)
;;;;
;;;; The main thread reads stdin one byte at a time (50 ms select), routes the
;;;; prefix key (C-b) and its follow-up through the binding table, handles
;;;; copy-mode escape sequences, and forwards everything else to the active
;;;; pane's PTY.  Runtime state (*running*, *dirty*, …) lives in runtime.lisp.
;;;; Command dispatch lives in dispatch.lisp.

;;; ── Prompt key handler ──────────────────────────────────────────────────────
;;;
;;; define-prompt-key-rules generates HANDLE-PROMPT-KEY from a declarative
;;; byte-dispatch table, matching the Prolog-like rule style used throughout
;;; the codebase (define-command-handlers, define-csi-rules, define-state).

(defmacro define-prompt-key-rules (&rest rules)
  "Build HANDLE-PROMPT-KEY from a byte-dispatch table.
   Each RULE is (PATTERN &rest BODY) where PATTERN is:
     integer  → exact byte match
     list     → verbatim condition
     t        → default clause
   Always marks *dirty* after dispatching."
  `(defun handle-prompt-key (byte)
     "Route one input BYTE to the active prompt.
      Multibyte/UTF-8 input is not yet supported (non-ASCII bytes are ignored)."
     (cond
       ,@(mapcar
          (lambda (rule)
            (destructuring-bind (pattern &rest body) rule
              `(,(cond
                   ((eq pattern 't)    't)
                   ((integerp pattern) `(= byte ,pattern))
                   (t                   pattern))
                ,@body)))
          rules))
     (setf *dirty* t)))

(define-prompt-key-rules
  (13                                       ; Enter — submit and dismiss
   (let ((p *prompt*))
     (when (and p (prompt-on-submit p))
       (funcall (prompt-on-submit p) (prompt-buffer p)))
     (prompt-clear)))
  (27  (prompt-clear))                      ; Esc — cancel
  ((or (= byte 127) (= byte 8))
   (prompt-backspace))                      ; Backspace / DEL
  ((and (>= byte 32) (< byte 127))
   (prompt-input (code-char byte)))         ; printable ASCII — insert
  (t nil))                                  ; non-ASCII — ignore

;;; ── VT100 escape-sequence byte constants ───────────────────────────────────
(defconstant +byte-esc+         27  "ASCII ESC (0x1B)")
(defconstant +byte-csi-bracket+ 91  "CSI introducer '[' (0x5B)")
(defconstant +byte-arrow-up+    65  "CUU final byte 'A' (0x41)")
(defconstant +byte-arrow-down+  66  "CUD final byte 'B' (0x42)")
(defconstant +byte-q+          113  "Lowercase 'q' for copy-mode exit (0x71)")

;;; ── Escape sequence dispatch macro ─────────────────────────────────────────

(defmacro define-copy-mode-escape-table (&rest rules)
  "Build HANDLE-COPY-MODE-ESCAPE from a declarative table.
   Each RULE is (byte-list &body forms).
   byte-list is a list of constant symbols or integers specifying the sequence.
   The generated function returns T when a sequence is consumed, NIL otherwise."
  `(defun handle-copy-mode-escape (session bytes)
     "Match BYTES against the copy-mode escape table; dispatch and mark dirty.
      Returns T if consumed, NIL otherwise."
     (let ((screen (%active-screen session)))
       (when (and screen (copy-mode-active-p session))
         (cond
           ,@(mapcar
              (lambda (rule)
                (destructuring-bind (pattern &rest body) rule
                  `((and (= (length bytes) ,(length pattern))
                         ,@(loop for byte in pattern
                                 for i from 0
                                 collect `(= (aref bytes ,i) ,byte)))
                    ,@body
                    (setf *dirty* t)
                    t)))
              rules)
           (t nil))))))

(define-copy-mode-escape-table
  ((+byte-esc+ +byte-csi-bracket+ +byte-arrow-up+)   (copy-mode-scroll screen  3))
  ((+byte-esc+ +byte-csi-bracket+ +byte-arrow-down+) (copy-mode-scroll screen -3))
  ((+byte-q+)                                         (copy-mode-exit  screen)))

;;; ── CPS keystroke processing ─────────────────────────────────────────────────
;;;
;;; Each state is a function (SESSION BYTE) → (values OUTCOME NEXT-STATE)
;;; where OUTCOME is :QUIT, :DETACH, or NIL, and NEXT-STATE is the next state
;;; function (or NIL meaning "return to ground state").
;;;
;;; define-cps-state is the session-level analogue of define-state in
;;; terminal/parser.lisp: both express dispatch as Prolog-like ordered clauses.

;;; ── Prolog-like CPS state definition macro ───────────────────────────────────

(defmacro define-cps-state (name (session-var byte-var) &rest rules)
  "Build a (SESSION BYTE) → (values OUTCOME NEXT-STATE) function from
   ordered Prolog-like clauses.  Each RULE is (CONDITION &rest BODY);
   the first matching condition wins (ordered-cut semantics, like cond).
   Both SESSION and BYTE are declared ignorable so rules that need only
   one compile cleanly."
  `(defun ,name (,session-var ,byte-var)
     (declare (ignorable ,session-var ,byte-var))
     (cond
       ,@(mapcar
          (lambda (rule)
            (destructuring-bind (condition &rest body) rule
              `(,condition ,@body)))
          rules))))

;;; ── PTY forwarding helpers ───────────────────────────────────────────────────

(defun %forward-octets (session octets)
  "Forward raw OCTETS to SESSION's active-pane PTY."
  (with-active-pane (ap session)
    (pty-write (pane-fd ap) octets)))

;;; ── Named CPS state functions ────────────────────────────────────────────────
;;;
;;; Rules read like Prolog clauses:
;;;   ground_state(_, _)  :- overlay_active, !, clear_overlay.
;;;   ground_state(_, _)  :- prompt_active,  !, handle_prompt_key.
;;;   ground_state(_, 2)  :- !, transition(after_prefix_state).
;;;   ground_state(S, 27) :- copy_mode_active(S), !, start_escape_accumulation.
;;;   ground_state(S, B)  :- forward_octets(S, [B]).

(define-cps-state %ground-input-state (session byte)
  ;; ── Global overlays take priority ─────────────────────────────────────────
  ((overlay-active-p)
   (clear-overlay)
   (setf *dirty* t)
   (values nil #'%ground-input-state))
  ;; ── Active prompt captures all input ──────────────────────────────────────
  ((prompt-active-p)
   (handle-prompt-key byte)
   (values nil #'%ground-input-state))
  ;; ── Prefix key: arm command dispatcher ────────────────────────────────────
  ((= byte +prefix-key-code+)
   (values nil #'%after-prefix-input-state))
  ;; ── ESC in copy mode: start escape-sequence accumulation ─────────────────
  ((and (= byte +byte-esc+) (copy-mode-active-p session))
   (let ((buf (make-array 4 :element-type '(unsigned-byte 8) :fill-pointer 0)))
     (vector-push byte buf)
     (values nil (make-escape-input-k session buf))))
  ;; ── Default: forward raw byte to active pane ─────────────────────────────
  (t
   (%forward-octets session
                    (make-array 1 :element-type '(unsigned-byte 8)
                                  :initial-element byte))
   (values nil #'%ground-input-state)))

(define-cps-state %after-prefix-input-state (session byte)
  ;; One rule: dispatch the command table and return to ground.
  (t (values (dispatch-prefix-command session byte) #'%ground-input-state)))

(defun make-escape-input-k (session buf)
  "CPS continuation: accumulate an ESC [... sequence one byte at a time.
   On a complete 3-byte CSI-like sequence: try copy-mode dispatch or flush.
   On a 2-byte non-CSI sequence: flush directly.
   While still accumulating: recurse with the same buffer."
  (lambda (session-ignored byte)
    (declare (ignore session-ignored))
    (vector-push byte buf)
    (let ((len (fill-pointer buf)))
      (cond
        ((and (= len 3) (= (aref buf 1) +byte-csi-bracket+))
         (unless (handle-copy-mode-escape session buf)
           (%forward-octets session (subseq buf 0 len)))
         (values nil #'%ground-input-state))
        ((and (= len 2) (/= (aref buf 1) +byte-csi-bracket+))
         (%forward-octets session (subseq buf 0 len))
         (values nil #'%ground-input-state))
        (t (values nil (make-escape-input-k session buf)))))))

(defstruct input-state
  "Opaque CPS keystroke-processing state. Holds the current continuation."
  (continuation #'%ground-input-state :type function))

(defun process-byte (session byte state)
  "Feed BYTE to SESSION through the CPS keystroke pipeline STATE.
   Returns :QUIT, :DETACH, or NIL. Mutates STATE's continuation in place."
  (multiple-value-bind (outcome next)
      (funcall (input-state-continuation state) session byte)
    (setf (input-state-continuation state) (or next #'%ground-input-state))
    outcome))

;;; -- Main event loop --------------------------------------------------------

(defun %handle-resize (session)
  "Re-read terminal geometry and relayout the active window after SIGWINCH."
  (setf *resize-pending* nil)
  (multiple-value-setq (*term-rows* *term-cols*) (terminal-size))
  (let ((win (session-active-window session)))
    (when win
      (window-relayout win (- *term-rows* *status-height*) *term-cols*))))

(defun %handle-dirty (session)
  "Fit the active window to current terminal size and repaint."
  (setf *dirty* nil)
  (let ((win (session-active-window session)))
    (when win
      (ensure-window-fits win (- *term-rows* *status-height*) *term-cols*)))
  (render-session session *term-rows* *term-cols*))

(defun event-loop (session)
  "In-process event loop: read stdin, route keystrokes, repaint on dirty."
  (let ((state (make-input-state)))
    (loop while *running* do
      (let ((b (read-byte-nonblock +poll-timeout-us+)))
        (when (and b (member (process-byte session b state) '(:quit :detach)))
          (setf *running* nil)))
      (when *resize-pending* (%handle-resize session))
      (when *dirty*           (%handle-dirty session)))))
