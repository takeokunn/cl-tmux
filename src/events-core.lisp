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

;;; ── Vi-mode prompt key dispatch ──────────────────────────────────────────────
;;;
;;; When status-keys = "vi" and the prompt is in vi normal mode,
;;; single-byte commands navigate / edit rather than inserting text.
;;; Returns T when the byte was consumed by vi-normal dispatch.

(defun %handle-vi-normal-key (byte)
  "Dispatch BYTE in vi normal mode.  Returns T when the key was handled.
   Navigation: h (left), l (right), 0/^ (BOL), $ (EOL), w (word-forward),
               b (word-backward).
   Editing:    x (delete char), D (delete to end), dd/d$ (delete to end).
   Mode switch: a/i/A/I return to insert mode; : stays in normal (no-op).
   Enter:      submit (same as insert mode)."
  (let ((p *prompt*))
    (unless (and p (prompt-vi-normal-p p))
      (return-from %handle-vi-normal-key nil))
    (case byte
      ;; Navigation
      (104 (prompt-cursor-back)    t)        ; h — left
      (108 (prompt-cursor-forward) t)        ; l — right
      (48  (prompt-cursor-bol)     t)        ; 0 — beginning of line
      (94  (prompt-cursor-bol)     t)        ; ^ — beginning of line
      (36  (prompt-cursor-eol)     t)        ; $ — end of line
      (119 (prompt-cursor-forward) t)        ; w — word forward (approx: move right)
      (98  (prompt-cursor-back)    t)        ; b — word backward (approx: move left)
      ;; Editing
      (120 (prompt-delete-char)    t)        ; x — delete char under cursor
      (68  (prompt-kill-to-end)    t)        ; D — delete to end of line
      ;; Enter to insert mode
      (97                                    ; a — append (move right, enter insert)
       (prompt-cursor-forward)
       (setf (prompt-vi-normal-p p) nil)
       t)
      (65                                    ; A — append at end
       (prompt-cursor-eol)
       (setf (prompt-vi-normal-p p) nil)
       t)
      (105                                   ; i — insert mode
       (setf (prompt-vi-normal-p p) nil)
       t)
      (73                                    ; I — insert at beginning
       (prompt-cursor-bol)
       (setf (prompt-vi-normal-p p) nil)
       t)
      (13                                    ; Enter — submit
       (let ((active-prompt p))
         (when (prompt-on-submit active-prompt)
           (funcall (prompt-on-submit active-prompt) (prompt-buffer active-prompt)))
         (prompt-clear))
       t)
      (27  (prompt-clear) t)                 ; ESC in normal mode — cancel
      (3   (prompt-clear) t)                 ; C-c — cancel
      (otherwise nil))))                     ; unhandled — fall through to insert

(define-prompt-key-rules
  (13                                       ; Enter — submit and dismiss
   (setf *prompt-utf8-acc* 0 *prompt-utf8-left* 0)
   (let ((active-prompt *prompt*))
     (when (and active-prompt (prompt-on-submit active-prompt))
       (funcall (prompt-on-submit active-prompt) (prompt-buffer active-prompt)))
     (prompt-clear)))
  (27                                       ; Esc
   (setf *prompt-utf8-acc* 0 *prompt-utf8-left* 0)
   (let ((p *prompt*))
     (cond
       ;; vi mode: ESC enters normal mode (does NOT cancel the prompt).
       ((and p (string-equal (cl-tmux/options:get-option "status-keys" "emacs") "vi")
             (not (prompt-vi-normal-p p)))
        (setf (prompt-vi-normal-p p) t))
       ;; emacs mode or already in vi-normal: cancel.
       (t (prompt-clear)))))
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
  ;; Vi normal mode: intercept printable bytes before insert dispatch.
  ((%handle-vi-normal-key byte) nil)        ; consumed by vi-normal — already handled
  ((and (>= byte 32) (< byte 127))
   (prompt-input (code-char byte)))         ; printable ASCII — insert
  ;; UTF-8 continuation byte: fold into accumulator
  ((= (logand byte #xC0) #x80)
   (when (plusp *prompt-utf8-left*)
     (setf *prompt-utf8-acc*  (logior (ash *prompt-utf8-acc* 6)
                                       (logand byte #x3F)))
     (decf *prompt-utf8-left*)
     (when (zerop *prompt-utf8-left*)
       (let ((code-point *prompt-utf8-acc*))
         (setf *prompt-utf8-acc* 0)
         (let ((character (ignore-errors (code-char code-point))))
           (when character (prompt-input character)))))))
  ;; UTF-8 lead byte: begin multi-byte decode
  ((and (>= byte #xC0) (/= byte #xFF))
   (multiple-value-bind (accumulator bytes-left)
       (cond ((< byte #xE0) (values (logand byte #x1F) 1))
             ((< byte #xF0) (values (logand byte #x0F) 2))
             (t             (values (logand byte #x07) 3)))
     (setf *prompt-utf8-acc*  accumulator
           *prompt-utf8-left* bytes-left)))
  (t nil))                                  ; other control bytes — ignore

;;; ── VT100 escape-sequence byte constants ───────────────────────────────────
(defconstant +byte-esc+         27  "ASCII ESC (0x1B)")
(defconstant +byte-csi-bracket+ 91  "CSI introducer '[' (0x5B)")
(defconstant +byte-arrow-up+    65  "CUU final byte 'A' (0x41)")
(defconstant +byte-arrow-down+  66  "CUD final byte 'B' (0x42)")
(defconstant +byte-arrow-left+  68  "CUB final byte 'D'")
(defconstant +byte-arrow-right+ 67  "CUF final byte 'C'")
(defconstant +byte-q+          113  "Lowercase 'q' for copy-mode exit (0x71)")
(defconstant +byte-j+          106  "Lowercase 'j' — vi down / overlay scroll-down (0x6A)")
(defconstant +byte-k+          107  "Lowercase 'k' — vi up / overlay scroll-up (0x6B)")

;;; ── Additional copy-mode navigation byte constants ──────────────────────────
;;; Named for every raw byte used in the copy-mode dispatch table so that readers
;;; can audit the mapping without cross-referencing ASCII tables.
(defconstant +byte-h+          104  "Lowercase 'h' — vi left (0x68)")
(defconstant +byte-l+          108  "Lowercase 'l' — vi right (0x6C)")
(defconstant +byte-w+          119  "Lowercase 'w' — vi word-forward (0x77)")
(defconstant +byte-b+           98  "Lowercase 'b' — vi word-backward (0x62)")
(defconstant +byte-e+          101  "Lowercase 'e' — vi word-end (0x65)")
(defconstant +byte-g+          103  "Lowercase 'g' — vi jump-to-top (0x67)")
(defconstant +byte-i+          105  "Lowercase 'i' — exit copy mode (insert, 0x69)")
(defconstant +byte-n+          110  "Lowercase 'n' — search next (0x6E)")
(defconstant +byte-r+          114  "Lowercase 'r' — rectangle select toggle (0x72)")
(defconstant +byte-v+          118  "Lowercase 'v' — begin selection (0x76)")
(defconstant +byte-y+          121  "Lowercase 'y' — yank/copy (0x79)")
(defconstant +byte-capital-a+   65  "Uppercase 'A' — append selection (0x41)")
(defconstant +byte-capital-d+   68  "Uppercase 'D' — copy to end of line (0x44)")
(defconstant +byte-capital-g+   71  "Uppercase 'G' — jump to bottom (0x47)")
(defconstant +byte-capital-h+   72  "Uppercase 'H' — cursor to top of screen (0x48)")
(defconstant +byte-capital-l+   76  "Uppercase 'L' — cursor to bottom of screen (0x4C)")
(defconstant +byte-capital-m+   77  "Uppercase 'M' — cursor to middle of screen (0x4D)")
(defconstant +byte-capital-n+   78  "Uppercase 'N' — search prev (0x4E)")
(defconstant +byte-capital-v+   86  "Uppercase 'V' — begin line selection (0x56)")
(defconstant +byte-capital-y+   89  "Uppercase 'Y' — copy current line (0x59)")
(defconstant +byte-dollar+      36  "Dollar sign '$' — go to line end (0x24)")
(defconstant +byte-slash+       47  "Slash '/' — search forward (0x2F)")
(defconstant +byte-question+    63  "Question mark '?' — search backward (0x3F)")
(defconstant +byte-space+       32  "Space ' ' — begin selection (0x20)")

;;; ── CSI modifier-sequence byte constants ────────────────────────────────────
;;; These appear in ESC [ 1 ; MOD FINAL sequences generated by terminal emulators
;;; for Ctrl-arrow and Meta-arrow key combinations.
(defconstant +byte-csi-param-1+  49  "CSI intermediate parameter '1' (0x31)")
(defconstant +byte-csi-semi+     59  "CSI parameter separator ';' (0x3B)")
(defconstant +byte-csi-mod-ctrl+ 53  "CSI modifier '5' — Ctrl key (0x35)")
(defconstant +byte-csi-mod-meta+ 51  "CSI modifier '3' — Meta/Alt key (0x33)")
(defconstant +byte-tilde+       126  "VT function-key terminator '~' (0x7E)")
(defconstant +byte-sgr-lt+       60  "SGR mouse introducer '<' (0x3C)")
(defconstant +byte-digit-0+      48  "ASCII digit '0' (0x30)")
(defconstant +byte-digit-9+      57  "ASCII digit '9' (0x39)")

;;; ── Function-key parameter constants ────────────────────────────────────────
;;; These are the numeric parameters in ESC [ N ~ sequences.
;;; NOTE: +byte-page-up-param+ (53) has the same numeric value as
;;; +byte-csi-mod-ctrl+ (53) — they are semantically distinct: the former is the
;;; literal digit '5' in ESC [ 5 ~, the latter is the modifier byte in ESC [ 1 ; 5 F.
;;; Both constants are kept to make code at their respective call sites self-documenting.
(defconstant +byte-page-up-param+   53  "ESC [ 5 ~ PageUp parameter byte '5' (0x35)")
(defconstant +byte-page-down-param+ 54  "ESC [ 6 ~ PageDown parameter byte '6' (0x36)")

;;; ── Mouse button-number constants ───────────────────────────────────────────
;;; These are X10-encoded button numbers (raw byte minus 32).
(defconstant +mouse-btn-left+          0  "Left mouse button press (X10 btn 0).")
(defconstant +mouse-btn-middle+        1  "Middle mouse button press (X10 btn 1) — paste.")
(defconstant +mouse-btn-release-x10+   3  "X10 release marker (btn 3+32=35).")
(defconstant +mouse-btn-motion+       32  "Button-1 drag/motion (X10 btn 32).")
(defconstant +mouse-btn-scroll-up+    64  "Scroll-wheel up (X10 btn 64).")
(defconstant +mouse-btn-scroll-down+  65  "Scroll-wheel down (X10 btn 65).")

;;; ── SGR mouse final-byte constants ──────────────────────────────────────────
;;; ASCII 'M' (77) is used as both the X10 mouse-sequence intro final byte and the
;;; SGR press final byte.  A single constant covers both roles; the old duplicate
;;; +byte-sgr-press+ has been removed.
(defconstant +byte-ascii-m+     77  "ASCII 'M' (0x4D) — X10 mouse intro and SGR press final.")
(defconstant +byte-sgr-release+ 109 "ASCII 'm' (0x6D) — SGR mouse release final byte.")

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
       (when (and screen (%copy-mode-active-p session))
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
  "Forward raw OCTETS (an octet vector) to SESSION's active-pane PTY.
   Used by escape-sequence handlers and the copy-mode passthrough path.
   Does nothing if the active pane has no live PTY (fd = -1)."
  (with-active-pane (active-pane session)
    (pty-write (pane-fd active-pane) octets)))

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
  (let ((current-col 0))
    ;; Skip the leading session-name prefix: \" <name>\".
    (incf current-col (+ 1 (length (session-name session))))
    (dolist (window (session-windows session))
      (let* (;; Active:   " [" name "] " = 4 extra chars; inactive: "  " name " " = 4 extra
             ;; Both formats use 4 + name-length total characters.
             (entry-len (+ 4 (length (window-name window)))))
        (when (and (>= col current-col) (< col (+ current-col entry-len)))
          (return-from %status-col-to-window window))
        (incf current-col entry-len)))
    nil))

(defun %mouse-status-bar-click (session col)
  "Handle a click at COL on the status bar row: select the clicked window."
  (let ((window (%status-col-to-window session col)))
    (when window
      (%with-window-focus-transition (session)
        (session-select-window session window)))))

;;; ── Drag-resize state ────────────────────────────────────────────────────────

(defvar *mouse-drag-state* nil
  "Drag state for border-resize: NIL or (split orientation).
   Set on button-1 press on a border; cleared on button-1 release.
   The press coordinates (col, row) are not stored because they are not
   needed after the initial hit-test — only the split node and orientation
   matter for subsequent motion events.")

(defvar *last-mouse-click* nil
  "Double/triple-click detection state: (list time-ms row col count) of the most
   recent left mouse press, or NIL.  Reset per-test by WITH-LOOP-STATE so click
   counts do not leak between tests.")

(defun %now-ms ()
  "Current monotonic time in milliseconds, from GET-INTERNAL-REAL-TIME."
  (floor (* 1000 (get-internal-real-time)) internal-time-units-per-second))

(defun %mouse-click-count (last now-ms row col threshold-ms)
  "Compute the click count for a left press at (ROW,COL) at NOW-MS.  LAST is the
   previous (time row col count) record or NIL.  A press within THRESHOLD-MS of
   the previous press AT THE SAME cell increments the count (1→2 double, 2→3
   triple); otherwise the count resets to 1.  Pure — testable without a clock."
  (if (and last
           (<= (- now-ms (first last)) threshold-ms)
           (= row (second last))
           (= col (third last)))
      (1+ (fourth last))
      1))

(defun %border-check-node (col row node)
  "Internal helper for %border-at-position: walk NODE and return
   (values split orientation) if (COL, ROW) is on a border, else (values nil nil)."
  (etypecase node
    (layout-leaf (values nil nil))
    (layout-split
     ;; Check children first, then this split's own border.
     (multiple-value-bind (split orientation)
         (%border-check-node col row (layout-split-first node))
       (when split (return-from %border-check-node (values split orientation))))
     (multiple-value-bind (split orientation)
         (%border-check-node col row (layout-split-second node))
       (when split (return-from %border-check-node (values split orientation))))
     (let* ((orient      (layout-split-orientation node))
            (first-leaves (layout-leaves (layout-split-first node)))
            (all-leaves   (layout-leaves node)))
       (ecase orient
         (:h
          (let* ((sep-col (reduce #'max first-leaves
                                  :key (lambda (pane) (+ (pane-x pane) (pane-width pane)))))
                 (min-y   (reduce #'min all-leaves :key #'pane-y))
                 (max-y   (reduce #'max all-leaves
                                  :key (lambda (pane) (+ (pane-y pane) (pane-height pane))))))
            (if (and (= col sep-col) (<= min-y row) (< row max-y))
                (values node :h)
                (values nil nil))))
         (:v
          (let* ((sep-row (reduce #'max first-leaves
                                  :key (lambda (pane) (+ (pane-y pane) (pane-height pane)))))
                 (min-x   (reduce #'min all-leaves :key #'pane-x))
                 (max-x   (reduce #'max all-leaves
                                  :key (lambda (pane) (+ (pane-x pane) (pane-width pane))))))
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
  "Adjust SPLIT's ratio so the separator tracks (COL, ROW) within WINDOW.
   ORIENTATION is :h (horizontal split — moves the vertical separator) or
   :v (vertical split — moves the horizontal separator).  After updating the
   ratio, layout-assign is called to recompute all pane geometries."
  (let ((all-panes (layout-leaves split)))
    (ecase orientation
      (:h
       (let* ((leftmost  (reduce #'min all-panes :key #'pane-x))
              (total     (layout-split-axis-extent split :h))
              (new-first (max 1 (min (1- total) (- col leftmost))))
              (new-ratio (/ new-first (float (1- total)))))
         (setf (layout-split-ratio split) new-ratio)))
      (:v
       (let* ((topmost   (reduce #'min all-panes :key #'pane-y))
              (total     (layout-split-axis-extent split :v))
              (new-first (max 1 (min (1- total) (- row topmost))))
              (new-ratio (/ new-first (float (1- total)))))
         (setf (layout-split-ratio split) new-ratio))))
    (let ((tree (window-tree window)))
      (when tree
        (layout-assign tree 0 0 (window-width window) (window-height window))))))

(defun %mouse-key-name (btn release-p location)
  "Build the tmux mouse key name for a mouse event, e.g. \"WheelUpPane\",
   \"MouseDown1Pane\", \"MouseUp3Status\".  BTN is the X10 button code, RELEASE-P
   selects MouseUp vs MouseDown, and LOCATION is \"Pane\"/\"Status\"/\"Border\".
   Returns NIL for events with no standard binding name (motion/drag, unknown
   buttons), so the caller falls back to the built-in mouse behaviour.

   These names are exactly what %parse-key-token stores for `bind -n WheelUpPane`
   / `bind -n MouseDown1Pane` (multi-char tokens are kept as strings), so the
   result doubles as a root key-table lookup key."
  (let ((button (cond
                  ((= btn +mouse-btn-left+)        "1")
                  ((= btn +mouse-btn-middle+)      "2")
                  ((= btn 2)                       "3")   ; right button (no named constant)
                  (t nil))))
    (cond
      ((= btn +mouse-btn-scroll-up+)   (concatenate 'string "WheelUp" location))
      ((= btn +mouse-btn-scroll-down+) (concatenate 'string "WheelDown" location))
      (button (concatenate 'string (if release-p "MouseUp" "MouseDown")
                           button location))
      (t nil))))

(defun %dispatch-mouse-event (session btn col row release-p)
  "Handle a parsed mouse event. BTN is the button number (X10 encoded minus 32),
   COL/ROW are 0-based screen coordinates, RELEASE-P is T for release events.
   All handling is gated on the global 'mouse' option.

   A user mouse binding in the root key table (e.g. `bind -n WheelUpPane
   copy-mode`) takes precedence over the built-in behaviour; only when the
   reconstructed mouse key name is unbound do we fall through to the hardcoded
   scroll/click/drag handling below."
  (unless (cl-tmux/options:get-option "mouse")
    (setf *dirty* t)
    (return-from %dispatch-mouse-event nil))
  (let* ((active-window  (session-active-window session))
         (active-pane    (session-active-pane session))
         (status-row     (1- *term-rows*))  ; status bar is always bottom row
         (in-status      (= row status-row))
         (location       (cond (in-status "Status")
                               ((and active-window
                                     (%border-at-position active-window col row))
                                "Border")
                               (t "Pane")))
         (mouse-key      (%mouse-key-name btn release-p location))
         ;; When the active pane is in copy mode, a copy-mode-table mouse binding
         ;; (e.g. `bind -T copy-mode-vi WheelUpPane send -X halfpage-up`) takes
         ;; precedence over both the root binding and the built-in handling.
         (active-screen  (and active-pane (pane-screen active-pane)))
         (in-copy        (and active-screen (screen-copy-mode-p active-screen)))
         (copy-table     (if (equal (cl-tmux/options:get-option "mode-keys" "vi") "vi")
                             "copy-mode-vi" +table-copy-mode+)))
    ;; User mouse binding wins over the built-in handling: copy-mode table first
    ;; (when in copy mode), then the root table.
    (when (or (and in-copy (%try-bound-string-key session copy-table mouse-key))
              (%try-bound-string-key session +table-root+ mouse-key))
      (return-from %dispatch-mouse-event nil))
    (cond
      ;; ── Status bar click ────────────────────────────────────────────────────
      ((and in-status (not release-p) (= btn +mouse-btn-left+))
       (%mouse-status-bar-click session col))

      ;; ── Scroll wheel up: enter copy-mode + scroll back ───────────────────
      ((= btn +mouse-btn-scroll-up+)
       (let ((screen (and active-pane (pane-screen active-pane))))
         (when screen
           (unless (screen-copy-mode-p screen)
             (copy-mode-enter screen))
           (copy-mode-scroll screen 3))))

      ;; ── Scroll wheel down: scroll forward, exit copy-mode at bottom ──────
      ((= btn +mouse-btn-scroll-down+)
       (let ((screen (and active-pane (pane-screen active-pane))))
         (when screen
           (copy-mode-scroll screen -3)
           (when (and (screen-copy-mode-p screen)
                      (zerop (screen-copy-offset screen)))
             (copy-mode-exit screen)))))

      ;; ── Left button press ─────────────────────────────────────────────────
      ((and (= btn +mouse-btn-left+) (not release-p) (not in-status))
       ;; Double/triple-click detection: a click at the same cell within
       ;; double-click-time of the previous one selects a word (2) or line (3+),
       ;; matching tmux's default DoubleClick1Pane / TripleClick1Pane bindings.
       (let* ((now   (%now-ms))
              (count (%mouse-click-count *last-mouse-click* now row col
                                         (or (cl-tmux/options:get-option "double-click-time")
                                             500))))
         (setf *last-mouse-click* (list now row col count))
         (when active-window
           ;; Check for border drag
           (multiple-value-bind (split orient)
               (%border-at-position active-window col row)
             (if split
                 ;; Press on border: begin drag (store only what motion events need)
                 (setf *mouse-drag-state* (list split orient))
                 ;; Press in pane: focus pane and begin/extend the copy selection
                 (let ((target-pane (pane-at-position active-window col row)))
                   (when target-pane
                     ;; %select-pane-with-focus so clicking a pane delivers ?1004
                     ;; focus events, consistent with keyboard pane switches.
                     (%select-pane-with-focus active-window target-pane)
                     (let* ((screen    (pane-screen target-pane))
                            (pane-col  (- col (pane-x target-pane)))
                            (pane-row  (- row (pane-y target-pane))))
                       (unless (screen-copy-mode-p screen)
                         (copy-mode-enter screen))
                       ;; Route cursor mutation through the commands layer.
                       (copy-mode-set-cursor screen pane-row pane-col)
                       (cond
                         ((= count 2)  (copy-mode-select-word screen))
                         ((>= count 3) (copy-mode-begin-line-selection screen))
                         (t            (copy-mode-begin-selection screen)))))))))))

      ;; ── Left button release: finalize selection or end drag ───────────────
      ((and (= btn +mouse-btn-left+) release-p)
       (if *mouse-drag-state*
           (setf *mouse-drag-state* nil)
           (when (and active-window active-pane)
             (let ((screen (pane-screen active-pane)))
               (when (and (screen-copy-mode-p screen)
                          (screen-copy-selecting screen))
                 (copy-mode-yank screen))))))

      ;; ── Middle button press: paste the top paste-buffer into the pane ─────
      ;; xterm-style middle-click paste.  Focuses the pane under the pointer and
      ;; writes the most recent paste-buffer (honouring bracketed-paste mode).
      ((and (= btn +mouse-btn-middle+) (not release-p) (not in-status))
       (when active-window
         (let ((target-pane (pane-at-position active-window col row)))
           (when target-pane
             (%select-pane-with-focus active-window target-pane)
             (let ((text (cl-tmux/buffer:get-paste-buffer 0)))
               (when text
                 (%paste-to-pane target-pane text)))))))

      ;; ── Mouse motion with button 1 (btn 32): drag selection or resize ─────
      ((= btn +mouse-btn-motion+)
       (if *mouse-drag-state*
           ;; Border drag in progress — only split and orientation are stored
           (destructuring-bind (split orient) *mouse-drag-state*
             (when active-window (%apply-drag-resize active-window split orient col row)))
           ;; Motion in pane: update copy selection cursor
           (when (and active-window active-pane)
             (let* ((target-pane  (pane-at-position active-window col row))
                    (screen       (and target-pane (pane-screen target-pane))))
               (when (and screen (screen-copy-mode-p screen) (screen-copy-selecting screen))
                 (let ((pane-col (- col (pane-x target-pane)))
                       (pane-row (- row (pane-y target-pane))))
                   ;; Route cursor mutation through the commands layer.
                   (copy-mode-set-cursor screen pane-row pane-col)))))))

      (t nil))
    (setf *dirty* t)))

;;; ── Overlay pager escape-sequence handler ────────────────────────────────────
;;;
;;; When the overlay pager is active and ESC is received, we accumulate the byte
;;; sequence.  ESC [ A (Up) scrolls -1 and ESC [ B (Down) scrolls +1.  Any other
;;; sequence (including bare ESC) dismisses the overlay.
;;;
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
      ;; ESC [ A — Up arrow: scroll overlay up
      ((= byte +byte-arrow-up+)
       (overlay-scroll -1)
       (setf *dirty* t)
       (values nil #'%ground-input-state))
      ;; ESC [ B — Down arrow: scroll overlay down
      ((= byte +byte-arrow-down+)
       (overlay-scroll 1)
       (setf *dirty* t)
       (values nil #'%ground-input-state))
      ;; Unrecognised final byte: dismiss the overlay
      (t
       (clear-overlay)
       (setf *dirty* t)
       (values nil #'%ground-input-state)))))

(defun make-overlay-escape-k (buffer)
  "CPS continuation factory: accumulate an ESC sequence for the overlay pager.
   The returned closure has the CPS signature (SESSION BYTE) → (values OUTCOME NEXT).
   SESSION is not needed — the overlay is global — so the closure ignores it.
   Handles ESC [ A (Up) → scroll -1, ESC [ B (Down) → scroll +1.
   Any other sequence (including bare ESC) dismisses the overlay."
  (%overlay-escape-second-byte buffer))
