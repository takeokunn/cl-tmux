(in-package #:cl-tmux)

;;;; Event processing — core macros, mouse dispatch, overlay handler.

;;; ── Prompt key handler ──────────────────────────────────────────────────────
;;;
;;; define-prompt-key-rules generates HANDLE-PROMPT-KEY from a declarative
;;; byte-dispatch table, matching the Prolog-like rule style used throughout
;;; the codebase (define-command-handlers, define-csi-rules, define-state).

;;; UTF-8 accumulation state for the prompt (module-level; main-thread-only).
(defvar *prompt-utf8-acc* 0
  "Accumulated code-point bits from UTF-8 lead byte processing.")
(defvar *prompt-utf8-left* 0
  "Number of UTF-8 continuation bytes still expected (0 when idle).")

(defmacro define-prompt-key-rules (&rest rules)
  "Build HANDLE-PROMPT-KEY from a byte-dispatch table.
   Each RULE is (PATTERN &rest BODY) where PATTERN is:
     integer  → exact byte match
     list     → verbatim condition
     t        → default clause
   Always marks *dirty* after dispatching."
  `(defun handle-prompt-key (byte)
     "Route one input BYTE to the active prompt.
      UTF-8 multi-byte sequences are decoded via *prompt-utf8-acc* /
      *prompt-utf8-left* before the dispatch table is consulted."
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
   (setf *prompt-utf8-acc* 0 *prompt-utf8-left* 0)
   (let ((p *prompt*))
     (when (and p (prompt-on-submit p))
       (funcall (prompt-on-submit p) (prompt-buffer p)))
     (prompt-clear)))
  (27  (setf *prompt-utf8-acc* 0 *prompt-utf8-left* 0) (prompt-clear)) ; Esc — cancel
  (3   (setf *prompt-utf8-acc* 0 *prompt-utf8-left* 0) (prompt-clear)) ; C-c — cancel
  (1   (prompt-cursor-bol))                 ; C-a — beginning of line
  (5   (prompt-cursor-eol))                 ; C-e — end of line
  (2   (prompt-cursor-back))                ; C-b — cursor left
  (6   (prompt-cursor-forward))             ; C-f — cursor right
  (11  (prompt-kill-to-end))                ; C-k — kill to end
  (21  (prompt-kill-to-start))              ; C-u — kill to start
  (23  (prompt-kill-word-back))             ; C-w — kill previous word
  ((or (= byte 127) (= byte 8))
   (prompt-backspace))                      ; Backspace / DEL
  ((and (>= byte 32) (< byte 127))
   (prompt-input (code-char byte)))         ; printable ASCII — insert
  ;; UTF-8 continuation byte: fold into accumulator
  ((= (logand byte #xC0) #x80)
   (when (plusp *prompt-utf8-left*)
     (setf *prompt-utf8-acc*  (logior (ash *prompt-utf8-acc* 6)
                                       (logand byte #x3F)))
     (decf *prompt-utf8-left*)
     (when (zerop *prompt-utf8-left*)
       (let ((cp *prompt-utf8-acc*))
         (setf *prompt-utf8-acc* 0)
         (let ((ch (ignore-errors (code-char cp))))
           (when ch (prompt-input ch)))))))
  ;; UTF-8 lead byte: begin multi-byte decode
  ((and (>= byte #xC0) (/= byte #xFF))
   (multiple-value-bind (acc left)
       (cond ((< byte #xE0) (values (logand byte #x1F) 1))
             ((< byte #xF0) (values (logand byte #x0F) 2))
             (t             (values (logand byte #x07) 3)))
     (setf *prompt-utf8-acc*  acc
           *prompt-utf8-left* left)))
  (t nil))                                  ; other control bytes — ignore

;;; ── VT100 escape-sequence byte constants ───────────────────────────────────
(defconstant +byte-esc+         27  "ASCII ESC (0x1B)")
(defconstant +byte-csi-bracket+ 91  "CSI introducer '[' (0x5B)")
(defconstant +byte-arrow-up+    65  "CUU final byte 'A' (0x41)")
(defconstant +byte-arrow-down+  66  "CUD final byte 'B' (0x42)")
(defconstant +byte-arrow-left+  68  "CUB final byte 'D'")
(defconstant +byte-arrow-right+ 67  "CUF final byte 'C'")
(defconstant +byte-q+          113  "Lowercase 'q' for copy-mode exit (0x71)")

;;; ── Mouse button-number constants ───────────────────────────────────────────
;;; These are X10-encoded button numbers (raw byte minus 32).
(defconstant +mouse-btn-left+          0  "Left mouse button press (X10 btn 0).")
(defconstant +mouse-btn-release-x10+   3  "X10 release marker (btn 3+32=35).")
(defconstant +mouse-btn-motion+       32  "Button-1 drag/motion (X10 btn 32).")
(defconstant +mouse-btn-scroll-up+    64  "Scroll-wheel up (X10 btn 64).")
(defconstant +mouse-btn-scroll-down+  65  "Scroll-wheel down (X10 btn 65).")

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
  ((+byte-esc+ +byte-csi-bracket+ +byte-arrow-up+)    (copy-mode-move-cursor screen :up))
  ((+byte-esc+ +byte-csi-bracket+ +byte-arrow-down+)  (copy-mode-move-cursor screen :down))
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

(defun %arrow-final-to-ss3-bytes (final-byte)
  "Given a CSI arrow-key final byte (65=A/66=B/67=C/68=D), return the
   corresponding SS3 sequence bytes (ESC O A/B/C/D) as an octet vector,
   or NIL if the final byte is not an arrow key."
  (when (member final-byte '(65 66 67 68))
    (make-array 3 :element-type '(unsigned-byte 8)
                  :initial-contents (list 27 79 final-byte)))) ; ESC O <final>

;;; ── Mouse event dispatch ─────────────────────────────────────────────────────
;;;
;;; X10 mouse encoding: ESC [ M <btn+32> <col+33> <row+33> (1-based coords).
;;; SGR mouse encoding: ESC [ < N ; COL ; ROW M (or m for release).
;;;
;;; %DISPATCH-MOUSE-EVENT handles scroll-wheel (btns 64/65), left-button
;;; press (btn 0) to focus pane or begin selection, status bar clicks, and
;;; wheel-scroll enter/exit of copy-mode.
;;; All mouse handling is gated behind the "mouse" session option.

;;; ── Status bar column → window index mapping ─────────────────────────────────

(defun %status-col-to-window (session col)
  "Return the window at column COL of the status bar, or NIL.
   Mirrors the layout produced by %status-window-list: active window is
   ' [Name] ' (4 + name length chars), inactive windows are '  Name ' (4 + name)."
  (let ((x 0))
    ;; Skip the leading session-name prefix: \" <name>\".
    (incf x (+ 1 (length (session-name session))))
    (dolist (w (session-windows session))
      (let* (;; Active:   " [" name "] " = 4 extra chars; inactive: "  " name " " = 4 extra
             ;; Both formats use 4 + name-length total characters.
             (entry-len (+ 4 (length (window-name w)))))
        (when (and (>= col x) (< col (+ x entry-len)))
          (return-from %status-col-to-window w))
        (incf x entry-len)))
    nil))

(defun %mouse-status-bar-click (session col)
  "Handle a click at COL on the status bar row: select the clicked window."
  (let ((win (%status-col-to-window session col)))
    (when win
      (session-select-window session win))))

;;; ── Drag-resize state ────────────────────────────────────────────────────────

(defvar *mouse-drag-state* nil
  "Drag state for border-resize: NIL or (split orientation start-col start-row orig-ratio).
   Set on button-1 press on a border; cleared on button-1 release.")

(defun %border-check-node (col row node)
  "Internal helper for %border-at-position: walk NODE and return
   (values split orientation) if (COL, ROW) is on a border, else (values nil nil)."
  (etypecase node
    (layout-leaf (values nil nil))
    (layout-split
     ;; Check children first, then this split's own border.
     (multiple-value-bind (s o) (%border-check-node col row (layout-split-first node))
       (when s (return-from %border-check-node (values s o))))
     (multiple-value-bind (s o) (%border-check-node col row (layout-split-second node))
       (when s (return-from %border-check-node (values s o))))
     (let* ((orient (layout-split-orientation node))
            (first-leaves (layout-leaves (layout-split-first node)))
            (all-leaves   (layout-leaves node)))
       (ecase orient
         (:h
          (let* ((sep-col (reduce #'max first-leaves
                                  :key (lambda (p) (+ (pane-x p) (pane-width p)))))
                 (min-y   (reduce #'min all-leaves :key #'pane-y))
                 (max-y   (reduce #'max all-leaves
                                  :key (lambda (p) (+ (pane-y p) (pane-height p))))))
            (if (and (= col sep-col) (<= min-y row) (< row max-y))
                (values node :h)
                (values nil nil))))
         (:v
          (let* ((sep-row (reduce #'max first-leaves
                                  :key (lambda (p) (+ (pane-y p) (pane-height p)))))
                 (min-x   (reduce #'min all-leaves :key #'pane-x))
                 (max-x   (reduce #'max all-leaves
                                  :key (lambda (p) (+ (pane-x p) (pane-width p))))))
            (if (and (= row sep-row) (<= min-x col) (< col max-x))
                (values node :v)
                (values nil nil)))))))))

(defun %border-at-position (window col row)
  "Return (values layout-split orientation) when (COL, ROW) is on a pane separator,
   or (values NIL NIL) when it is not on any border."
  (let ((tree (window-tree window)))
    (if tree
        (%border-check-node col row tree)
        (values nil nil))))

(defun %apply-drag-resize (window split orientation col row)
  "Adjust SPLIT's ratio so the separator tracks (COL, ROW) within WINDOW."
  (let* ((all-panes (layout-leaves split))
         (orient    orientation))
    (ecase orient
      (:h
       (let* ((leftmost  (reduce #'min all-panes :key #'pane-x))
              (total     (layout-split-axis-extent split :h))
              (new-fst   (max 1 (min (1- total) (- col leftmost))))
              (new-ratio (/ new-fst (float (1- total)))))
         (setf (layout-split-ratio split) new-ratio)))
      (:v
       (let* ((topmost   (reduce #'min all-panes :key #'pane-y))
              (total     (layout-split-axis-extent split :v))
              (new-fst   (max 1 (min (1- total) (- row topmost))))
              (new-ratio (/ new-fst (float (1- total)))))
         (setf (layout-split-ratio split) new-ratio))))
    (let ((tree (window-tree window)))
      (when tree
        (layout-assign tree 0 0 (window-width window) (window-height window))))))

(defun %dispatch-mouse-event (session btn col row release-p)
  "Handle a parsed mouse event. BTN is the button number (X10 encoded minus 32),
   COL/ROW are 0-based screen coordinates, RELEASE-P is T for release events.
   All handling is gated on the global 'mouse' option."
  (unless (cl-tmux/options:get-option "mouse")
    (setf *dirty* t)
    (return-from %dispatch-mouse-event nil))
  (let* ((win    (session-active-window session))
         (ap     (session-active-pane session))
         (status-row (1- *term-rows*))  ; status bar is always bottom row
         (in-status  (= row status-row)))
    (cond
      ;; ── Status bar click ────────────────────────────────────────────────────
      ((and in-status (not release-p) (= btn +mouse-btn-left+))
       (%mouse-status-bar-click session col))

      ;; ── Scroll wheel up: enter copy-mode + scroll back ───────────────────
      ((= btn +mouse-btn-scroll-up+)
       (let ((sc (and ap (pane-screen ap))))
         (when sc
           (unless (screen-copy-mode-p sc)
             (copy-mode-enter sc))
           (copy-mode-scroll sc 3))))

      ;; ── Scroll wheel down: scroll forward, exit copy-mode at bottom ──────
      ((= btn +mouse-btn-scroll-down+)
       (let ((sc (and ap (pane-screen ap))))
         (when sc
           (copy-mode-scroll sc -3)
           (when (and (screen-copy-mode-p sc)
                      (zerop (screen-copy-offset sc)))
             (copy-mode-exit sc)))))

      ;; ── Left button press ─────────────────────────────────────────────────
      ((and (= btn +mouse-btn-left+) (not release-p) (not in-status))
       (when win
         ;; Check for border drag
         (multiple-value-bind (split orient)
             (%border-at-position win col row)
           (if split
               ;; Press on border: begin drag
               (setf *mouse-drag-state* (list split orient col row))
               ;; Press in pane: focus pane and begin copy selection
               (let ((p (pane-at-position win col row)))
                 (when p
                   (window-select-pane win p)
                   (let ((sc (pane-screen p)))
                     (unless (screen-copy-mode-p sc)
                       (copy-mode-enter sc))
                     (let ((px (- col (pane-x p)))
                           (py (- row (pane-y p))))
                       (setf (screen-copy-cursor sc) (cons py px))
                       (copy-mode-begin-selection sc)))))))))

      ;; ── Left button release: finalize selection or end drag ───────────────
      ((and (= btn +mouse-btn-left+) release-p)
       (if *mouse-drag-state*
           (setf *mouse-drag-state* nil)
           (when (and win ap)
             (let ((sc (pane-screen ap)))
               (when (and (screen-copy-mode-p sc)
                          (screen-copy-selecting sc))
                 (copy-mode-yank sc))))))

      ;; ── Mouse motion with button 1 (btn 32): drag selection or resize ─────
      ((= btn +mouse-btn-motion+)
       (if *mouse-drag-state*
           ;; Border drag in progress
           (destructuring-bind (split orient _c _r) *mouse-drag-state*
             (declare (ignore _c _r))
             (when win (%apply-drag-resize win split orient col row)))
           ;; Motion in pane: update copy selection cursor
           (when (and win ap)
             (let* ((p  (pane-at-position win col row))
                    (sc (and p (pane-screen p))))
               (when (and sc (screen-copy-mode-p sc) (screen-copy-selecting sc))
                 (let ((px (- col (pane-x p)))
                       (py (- row (pane-y p))))
                   (setf (screen-copy-cursor sc) (cons py px))
                   (setf (screen-dirty-p sc) t)))))))

      (t nil))
    (setf *dirty* t)))

;;; ── Overlay pager escape-sequence handler ────────────────────────────────────
;;;
;;; When the overlay pager is active and ESC is received, we accumulate the byte
;;; sequence.  ESC [ A (Up) scrolls -1 and ESC [ B (Down) scrolls +1.  Any other
;;; sequence (including bare ESC) dismisses the overlay.

(defun make-overlay-escape-k (buf)
  "CPS continuation factory: accumulate an ESC sequence for the overlay pager.
   The returned closure has the CPS signature (SESSION BYTE) → (values OUTCOME NEXT).
   SESSION is not needed — the overlay is global — so the closure ignores it.
   Handles ESC [ A (Up) → scroll -1, ESC [ B (Down) → scroll +1.
   Any other sequence (including bare ESC) dismisses the overlay."
  ;; BUF is captured from the enclosing call; we extend it byte by byte.
  (lambda (_session byte)
    (declare (ignore _session))
    (vector-push-extend byte buf)
    (let ((len (fill-pointer buf)))
      (cond
        ;; ESC [ — keep accumulating for the final byte
        ((and (= len 2) (= byte +byte-csi-bracket+))
         (values nil (make-overlay-escape-k buf)))
        ;; ESC [ A — Up arrow: scroll up
        ((and (= len 3) (= (aref buf 1) +byte-csi-bracket+)
              (= byte +byte-arrow-up+))
         (overlay-scroll -1)
         (setf *dirty* t)
         (values nil #'%ground-input-state))
        ;; ESC [ B — Down arrow: scroll down
        ((and (= len 3) (= (aref buf 1) +byte-csi-bracket+)
              (= byte +byte-arrow-down+))
         (overlay-scroll 1)
         (setf *dirty* t)
         (values nil #'%ground-input-state))
        ;; Bare ESC (2-byte non-CSI) or unrecognised sequence: dismiss
        (t
         (clear-overlay)
         (setf *dirty* t)
         (values nil #'%ground-input-state))))))
