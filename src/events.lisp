(in-package #:cl-tmux)

;;;; Event loop and keystroke processing.
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
(defconstant +byte-arrow-left+  68  "CUB final byte 'D'")
(defconstant +byte-arrow-right+ 67  "CUF final byte 'C'")
(defconstant +byte-space+       32  "Space (0x20)")
(defconstant +byte-y+          121  "Lowercase 'y' (0x79)")
(defconstant +byte-v+          118  "Lowercase 'v' (0x76)")
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
  ((+byte-esc+ +byte-csi-bracket+ +byte-arrow-up+)    (copy-mode-scroll screen  3))
  ((+byte-esc+ +byte-csi-bracket+ +byte-arrow-down+)  (copy-mode-scroll screen -3))
  ((+byte-esc+ +byte-csi-bracket+ +byte-arrow-left+)  (copy-mode-move-cursor screen :left))
  ((+byte-esc+ +byte-csi-bracket+ +byte-arrow-right+) (copy-mode-move-cursor screen :right))
  ((+byte-q+)                                          (copy-mode-exit  screen)))

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

;;; ── Mouse event dispatch ─────────────────────────────────────────────────────
;;;
;;; X10 mouse encoding: ESC [ M <btn+32> <col+33> <row+33> (1-based coords).
;;; SGR mouse encoding: ESC [ < N ; COL ; ROW M (or m for release).
;;;
;;; %DISPATCH-MOUSE-EVENT handles scroll-wheel (btns 64/65) and left-button
;;; press (btn 0, not release) to focus the clicked pane.

(defun %dispatch-mouse-event (session btn col row release-p)
  "Handle a parsed mouse event. BTN is the button number (X10 encoded minus 32),
   COL/ROW are 0-based screen coordinates, RELEASE-P is T for release events."
  (let* ((win (session-active-window session))
         (ap  (session-active-pane session)))
    (cond
      ((= btn 64)                            ; scroll up (wheel)
       (let ((sc (and ap (pane-screen ap))))
         (when sc (copy-mode-scroll sc 3))))
      ((= btn 65)                            ; scroll down (wheel)
       (let ((sc (and ap (pane-screen ap))))
         (when sc (copy-mode-scroll sc -3))))
      ((and (= btn 0) (not release-p))       ; left button press → focus pane
       (when win
         (let ((p (pane-at-position win col row)))
           (when p (window-select-pane win p)))))
      (t nil))
    (setf *dirty* t)))

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
  ;; ── ESC: always accumulate for mouse events + copy mode ──────────────────
  ((= byte +byte-esc+)
   (let ((buf (make-array 8 :element-type '(unsigned-byte 8)
                            :fill-pointer 0 :adjustable t)))
     (vector-push-extend byte buf)
     (values nil (make-escape-input-k session buf))))
  ;; ── Default: forward raw byte to active pane ─────────────────────────────
  (t
   (%forward-octets session
                    (make-array 1 :element-type '(unsigned-byte 8)
                                  :initial-element byte))
   (values nil #'%ground-input-state)))

(defun %make-prefix-csi-k (session buf)
  "CPS continuation: accumulate ESC [ FINAL for post-prefix arrow key sequences.
   Dispatches :select-pane-up/down/left/right on ESC [ A/B/D/C respectively."
  (lambda (session-arg byte)
    (declare (ignore session-arg))
    (vector-push byte buf)
    (let ((len (fill-pointer buf)))
      (cond
        ;; Complete 3-byte CSI sequence: ESC [ FINAL
        ((and (= len 3) (= (aref buf 1) +byte-csi-bracket+))
         (let ((cmd (case (aref buf 2)
                      (65 :select-pane-up)
                      (66 :select-pane-down)
                      (67 :select-pane-right)
                      (68 :select-pane-left)
                      (otherwise nil))))
           (values (if cmd
                       (dispatch-command session cmd nil)
                       (%passthrough-prefix session (subseq buf 0 len)))
                   #'%ground-input-state)))
        ;; 2-byte non-CSI: pass through
        ((and (= len 2) (/= (aref buf 1) +byte-csi-bracket+))
         (%forward-octets session (subseq buf 0 len))
         (values nil #'%ground-input-state))
        ;; Buffer at capacity (>= 3 bytes but unrecognised) — flush and return
        ;; to ground to avoid permanent stuck-state on malformed CSI sequences.
        ((>= len 3)
         (%forward-octets session (subseq buf 0 len))
         (values nil #'%ground-input-state))
        ;; Still accumulating (1 or 2 bytes so far)
        (t (values nil (%make-prefix-csi-k session buf)))))))

(define-cps-state %after-prefix-input-state (session byte)
  ;; ESC introduces a multi-byte prefix sequence (C-b arrow keys).
  ((= byte +byte-esc+)
   (let ((buf (make-array 4 :element-type '(unsigned-byte 8) :fill-pointer 0)))
     (vector-push byte buf)
     (values nil (%make-prefix-csi-k session buf))))
  ;; Single-byte: dispatch the command table and return to ground.
  (t (values (dispatch-prefix-command session byte) #'%ground-input-state)))

(defconstant +byte-mouse-intro+ 77
  "ASCII 'M' (0x4D) — the final byte of X10 mouse sequences ESC [ M.")

(defun make-escape-input-k (session buf)
  "CPS continuation: accumulate an ESC [... sequence one byte at a time.

   X10 mouse: ESC [ M <btn+32> <col+33> <row+33> — 6 bytes total.
     Detected when buf[0]=ESC buf[1]=[ buf[2]=M and we still need 3 more bytes.
     Dispatched via %DISPATCH-MOUSE-EVENT when len reaches 6.

   Copy-mode 3-byte CSI (ESC [ FINAL): try HANDLE-COPY-MODE-ESCAPE; if not
     handled and not in copy mode, forward to the active pane.

   2-byte non-CSI (ESC X): forward to the active pane.

   Otherwise: keep accumulating."
  (lambda (session-ignored byte)
    (declare (ignore session-ignored))
    (vector-push-extend byte buf)
    (let ((len (fill-pointer buf)))
      (cond
        ;; ── X10 mouse: ESC [ M btn col row (6 bytes) ──────────────────────
        ((and (= len 6)
              (= (aref buf 1) +byte-csi-bracket+)
              (= (aref buf 2) +byte-mouse-intro+))
         (let* ((raw-btn (aref buf 3))
                (raw-col (aref buf 4))
                (raw-row (aref buf 5))
                ;; X10 encoding: btn+32, col/row+33 (1-based → subtract 1 for 0-based)
                (btn     (- raw-btn 32))
                (col     (- raw-col 33))
                (row     (- raw-row 33))
                (release-p (= raw-btn 35)))  ; btn 3+32=35 = release in X10
           (%dispatch-mouse-event session btn col row release-p))
         (values nil #'%ground-input-state))
        ;; ── Still accumulating mouse intro (ESC [ M + up to 2 more bytes) ─
        ((and (>= len 3)
              (= (aref buf 1) +byte-csi-bracket+)
              (= (aref buf 2) +byte-mouse-intro+)
              (< len 6))
         (values nil (make-escape-input-k session buf)))
        ;; ── Copy-mode or forward: 3-byte CSI ESC [ FINAL ──────────────────
        ((and (= len 3) (= (aref buf 1) +byte-csi-bracket+)
              (/= (aref buf 2) +byte-mouse-intro+))
         (unless (handle-copy-mode-escape session buf)
           ;; Not in copy mode (or unrecognised): forward raw bytes to pane
           (unless (copy-mode-active-p session)
             (%forward-octets session (subseq buf 0 len))))
         (values nil #'%ground-input-state))
        ;; ── 2-byte non-CSI sequence: ESC X ────────────────────────────────
        ((and (= len 2) (/= (aref buf 1) +byte-csi-bracket+))
         (unless (copy-mode-active-p session)
           (%forward-octets session (subseq buf 0 len)))
         (values nil #'%ground-input-state))
        ;; ── Buffer overflow guard (> 6 unrecognised bytes) ────────────────
        ((> len 6)
         (unless (copy-mode-active-p session)
           (%forward-octets session (subseq buf 0 len)))
         (values nil #'%ground-input-state))
        ;; ── Still accumulating ─────────────────────────────────────────────
        (t (values nil (make-escape-input-k session buf)))))))

;;; ── Additional key bindings ─────────────────────────────────────────────────

;; C-b w — list-windows (standard tmux binding)
(set-key-binding #\w :list-windows)
;; C-b { / C-b } — swap-pane backward / forward
(set-key-binding #\{ :swap-pane-backward)
(set-key-binding #\} :swap-pane-forward)
;; C-b ; — last-pane (jump to previously active pane)
(set-key-binding #\; :last-pane)
;; C-b q — display-panes (show pane numbers)
(set-key-binding #\q :display-panes)

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

(defun %maybe-rename-window-from-title (session)
  "If the active pane has set an OSC title, propagate it to the active window name."
  (let* ((ap  (session-active-pane session))
         (sc  (when ap (pane-screen ap)))
         (win (session-active-window session)))
    (when (and sc win (not (string= (screen-title sc) "")))
      (unless (string= (screen-title sc) (window-name win))
        (setf (window-name win) (screen-title sc))
        (setf *dirty* t)))))

(defun %handle-dirty (session)
  "Fit the active window to current terminal size and repaint."
  (setf *dirty* nil)
  (%maybe-rename-window-from-title session)
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
