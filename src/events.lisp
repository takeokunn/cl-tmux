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
      ((and in-status (not release-p) (= btn 0))
       (%mouse-status-bar-click session col))

      ;; ── Scroll wheel up: enter copy-mode + scroll back ───────────────────
      ((= btn 64)
       (let ((sc (and ap (pane-screen ap))))
         (when sc
           (unless (screen-copy-mode-p sc)
             (copy-mode-enter sc))
           (copy-mode-scroll sc 3))))

      ;; ── Scroll wheel down: scroll forward, exit copy-mode at bottom ──────
      ((= btn 65)
       (let ((sc (and ap (pane-screen ap))))
         (when sc
           (copy-mode-scroll sc -3)
           (when (and (screen-copy-mode-p sc)
                      (zerop (screen-copy-offset sc)))
             (copy-mode-exit sc)))))

      ;; ── Left button press ─────────────────────────────────────────────────
      ((and (= btn 0) (not release-p) (not in-status))
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
      ((and (= btn 0) release-p)
       (if *mouse-drag-state*
           (setf *mouse-drag-state* nil)
           (when (and win ap)
             (let ((sc (pane-screen ap)))
               (when (and (screen-copy-mode-p sc)
                          (screen-copy-selecting sc))
                 (copy-mode-yank sc))))))

      ;; ── Mouse motion with button 1 (btn 32): drag selection or resize ─────
      ((= btn 32)
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

;;; ── Named CPS state functions ────────────────────────────────────────────────
;;;
;;; Rules read like Prolog clauses:
;;;   ground_state(_, _)  :- overlay_active, !, clear_overlay.
;;;   ground_state(_, _)  :- prompt_active,  !, handle_prompt_key.
;;;   ground_state(_, 2)  :- !, transition(after_prefix_state).
;;;   ground_state(S, 27) :- copy_mode_active(S), !, start_escape_accumulation.
;;;   ground_state(S, B)  :- forward_octets(S, [B]).

(define-cps-state %ground-input-state (session byte)
  ;; ── Locked session: any key unlocks ────────────────────────────────────────
  ((session-locked-p session)
   (setf (session-locked-p session) nil)
   (setf *dirty* t)
   (values nil #'%ground-input-state))
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
  ;; ── ESC: always accumulate for mouse events, arrows, copy mode ───────────
  ;; Even in copy mode we accumulate: arrow keys arrive as ESC [ FINAL and are
  ;; handled by handle-copy-mode-escape inside make-escape-input-k.  A lone ESC
  ;; (2-byte non-CSI) or unrecognised sequence exits copy mode instead of
  ;; forwarding to the pane (handled in make-escape-input-k).
  ((= byte +byte-esc+)
   (let ((buf (make-array 8 :element-type '(unsigned-byte 8)
                            :fill-pointer 0 :adjustable t)))
     (vector-push-extend byte buf)
     (values nil (make-escape-input-k session buf))))
  ;; ── Copy-mode single-byte navigation (unprefixed) ─────────────────────────
  ;; These keys are intercepted ONLY when copy mode is active; they are never
  ;; forwarded to the pane.  The check comes before the default forward branch.
  ((copy-mode-active-p session)
   (let ((sc (%active-screen session)))
     (when sc
       (case byte
         ;; q / Q — exit copy mode
         (#.+byte-q+ (copy-mode-exit sc))
         ;; j / C-n (14) / Down arrow handled via escape below → scroll down 1
         ((106 14)  (copy-mode-scroll sc -1))  ; j, C-n
         ;; k / C-p (16) / Up arrow handled via escape → scroll up 1
         ((107 16)  (copy-mode-scroll sc 1))   ; k, C-p
         ;; C-d (4) — scroll down half page
         (4         (copy-mode-scroll sc (- (floor (screen-height sc) 2))))
         ;; C-u (21) — scroll up half page
         (21        (copy-mode-scroll sc (floor (screen-height sc) 2)))
         ;; g — jump to top (maximum scrollback)
         (103       (copy-mode-scroll sc most-positive-fixnum))
         ;; G — jump to bottom (offset = 0, live view)
         (71        (copy-mode-scroll sc (- most-positive-fixnum)))
         ;; Space / v — begin selection
         ((32 118)  (copy-mode-begin-selection sc))
         ;; y — yank selection
         (121       (copy-mode-yank sc))
         ;; Any other byte is consumed without forwarding (no passthrough in copy mode)
         (otherwise nil)))
     (setf *dirty* t))
   (values nil #'%ground-input-state))
  ;; ── Default: forward raw byte to active pane (+ synchronize-panes broadcast) ─
  (t
   (%forward-octets-synchronized session
                                  (make-array 1 :element-type '(unsigned-byte 8)
                                               :initial-element byte))
   (values nil #'%ground-input-state)))

(defun %make-prefix-csi-k (session buf)
  "CPS continuation: accumulate ESC [ FINAL for post-prefix arrow key sequences.
   Dispatches :select-pane-up/down/left/right on ESC [ A/B/D/C (3-byte CSI).
   Dispatches C-arrow (ESC [ 1 ; 5 FINAL, 8 bytes) to :resize-{dir} (1 cell).
   Dispatches M-arrow (ESC [ 1 ; 3 FINAL, 8 bytes) to :resize-{dir} (5 cells).
   Unrecognised sequences are silently discarded (no passthrough-prefix)."
  (lambda (session-arg byte)
    (declare (ignore session-arg))
    (vector-push-extend byte buf)
    (let ((len (fill-pointer buf)))
      (cond
        ;; Complete 3-byte CSI sequence: ESC [ FINAL
        ((and (= len 3) (= (aref buf 1) +byte-csi-bracket+))
         (let* ((final (aref buf 2))
                ;; Detect whether this is the start of a longer modifier sequence:
                ;; ESC [ 1 — intermediate, not a final CSI byte yet.
                (is-param-1 (= final 49))) ; '1' = 49
           (cond
             ;; ESC [ 1 may be start of ESC [ 1 ; MOD FINAL — keep accumulating
             (is-param-1
              (values nil (%make-prefix-csi-k session buf)))
             (t
              (let ((cmd (case final
                           (65 :select-pane-up)
                           (66 :select-pane-down)
                           (67 :select-pane-right)
                           (68 :select-pane-left)
                           (otherwise nil))))
                ;; Unrecognised 3-byte CSI: silently discard (no passthrough).
                (values (when cmd (dispatch-command session cmd nil))
                        #'%ground-input-state))))))
        ;; 4-byte sequence starting ESC [ 1 ; — keep accumulating
        ((and (= len 4) (= (aref buf 1) +byte-csi-bracket+)
              (= (aref buf 2) 49)  ; '1'
              (= (aref buf 3) 59)) ; ';'
         (values nil (%make-prefix-csi-k session buf)))
        ;; 5-byte: ESC [ 1 ; MOD — keep accumulating for the final letter
        ((and (= len 5) (= (aref buf 1) +byte-csi-bracket+)
              (= (aref buf 2) 49) (= (aref buf 3) 59))
         (values nil (%make-prefix-csi-k session buf)))
        ;; Complete 6-byte modifier CSI: ESC [ 1 ; MOD FINAL
        ;; (where MOD=53='5' for Ctrl, MOD=51='3' for Meta)
        ((and (= len 6) (= (aref buf 1) +byte-csi-bracket+)
              (= (aref buf 2) 49) (= (aref buf 3) 59))
         (let* ((mod   (aref buf 4))
                (final (aref buf 5))
                (cmd
                  (cond
                    ;; C-arrow: ESC [ 1 ; 5 FINAL  (mod=53='5') → resize 1 cell
                    ((= mod 53)
                     (case final
                       (65 :c-arrow-up)    ; A
                       (66 :c-arrow-down)  ; B
                       (67 :c-arrow-right) ; C
                       (68 :c-arrow-left)  ; D
                       (otherwise nil)))
                    ;; M-arrow: ESC [ 1 ; 3 FINAL  (mod=51='3') → resize 5 cells
                    ((= mod 51)
                     (case final
                       (65 :resize-up)    ; A
                       (66 :resize-down)  ; B
                       (67 :resize-right) ; C
                       (68 :resize-left)  ; D
                       (otherwise nil)))
                    (t nil))))
           ;; C-arrow bindings dispatch resize-pane with amount=1 directly.
           ;; M-arrow bindings use the standard :resize-* commands (amount=5).
           ;; Unrecognised modifier sequence: silently discard.
           (let ((win (session-active-window session)))
             (when win
               (case cmd
                 (:c-arrow-up    (resize-pane win :up    1))
                 (:c-arrow-down  (resize-pane win :down  1))
                 (:c-arrow-right (resize-pane win :right 1))
                 (:c-arrow-left  (resize-pane win :left  1))
                 (otherwise
                  (when cmd (dispatch-command session cmd nil))))))
           (setf *dirty* t)
           (values nil #'%ground-input-state)))
        ;; 2-byte non-CSI: silently discard after prefix (no passthrough)
        ((and (= len 2) (/= (aref buf 1) +byte-csi-bracket+))
         (values nil #'%ground-input-state))
        ;; Buffer at capacity (>= 6 bytes but unrecognised) — discard and return
        ;; to ground to avoid permanent stuck-state on malformed CSI sequences.
        ((>= len 6)
         (values nil #'%ground-input-state))
        ;; Still accumulating (1-5 bytes so far)
        (t (values nil (%make-prefix-csi-k session buf)))))))

(define-cps-state %after-prefix-input-state (session byte)
  ;; ESC introduces a multi-byte prefix sequence (C-b arrow/modifier key sequences).
  ;; The buffer needs to be adjustable so %make-prefix-csi-k can vector-push-extend
  ;; up to 6 bytes for modifier sequences like ESC [ 1 ; 5 A (C-Up).
  ((= byte +byte-esc+)
   (let ((buf (make-array 8 :element-type '(unsigned-byte 8)
                            :fill-pointer 0 :adjustable t)))
     (vector-push-extend byte buf)
     (values nil (%make-prefix-csi-k session buf))))
  ;; Single-byte: dispatch the command table and return to ground.
  (t (values (dispatch-prefix-command session byte) #'%ground-input-state)))

(defconstant +byte-mouse-intro+ 77
  "ASCII 'M' (0x4D) — the final byte of X10 mouse sequences ESC [ M.")

;;; ── SGR mouse sequence parser ────────────────────────────────────────────────
;;;
;;; SGR format: ESC [ < Pb ; Px ; Py M (press) or m (release)
;;; Terminated by 'M' (press) or 'm' (release).

(defun %parse-sgr-mouse (buf len)
  "Parse an SGR mouse sequence from BUF (length LEN).
   Expected: ESC [ < Pb ; Px ; Py M|m
   Returns (values btn col row release-p) on success, or (values nil nil nil nil) on failure.
   Coordinates in BUF are 1-based; returned col/row are 0-based."
  ;; Minimum: ESC [ < D ; D ; D M = 9 bytes
  (when (and (>= len 9)
             (= (aref buf 0) 27)
             (= (aref buf 1) +byte-csi-bracket+)
             (= (aref buf 2) 60))  ; '<' = 60
    (let* ((s       (map 'string #'code-char (subseq buf 3 len)))
           (final   (char s (1- (length s))))
           (release-p (char= final #\m))
           (params  (subseq s 0 (1- (length s))))
           (parts   (loop for start = 0 then (1+ semi)
                          for semi  = (position #\; params :start start)
                          collect (subseq params start (or semi (length params)))
                          while semi)))
      (when (= (length parts) 3)
        (let ((btn (parse-integer (first  parts) :junk-allowed t))
              (col (parse-integer (second parts) :junk-allowed t))
              (row (parse-integer (third  parts) :junk-allowed t)))
          (when (and (integerp btn) (integerp col) (integerp row))
            ;; SGR coords are 1-based; convert to 0-based
            (values btn (1- col) (1- row) release-p))))))
  )

(defun %sgr-mouse-sequence-p (buf len)
  "True when BUF looks like the start of an SGR mouse sequence: ESC [ <."
  (and (>= len 3)
       (= (aref buf 0) 27)
       (= (aref buf 1) +byte-csi-bracket+)
       (= (aref buf 2) 60)))  ; '<' = 60

(defun %sgr-mouse-terminated-p (buf len)
  "True when BUF ends with 'M' (press) or 'm' (release) — SGR mouse final byte."
  (when (> len 3)
    (let ((last (aref buf (1- len))))
      (or (= last 77) (= last 109)))))   ; 'M'=77, 'm'=109

(defun make-escape-input-k (session buf)
  "CPS continuation: accumulate an ESC [... sequence one byte at a time.

   X10 mouse: ESC [ M <btn+32> <col+33> <row+33> — 6 bytes total.
     Detected when buf[0]=ESC buf[1]=[ buf[2]=M and we still need 3 more bytes.
     Dispatched via %DISPATCH-MOUSE-EVENT when len reaches 6.

   SGR mouse: ESC [ < Pb ; Px ; Py M|m — variable length, terminated by M or m.
     Detected when buf[2]='<' (60).  Accumulated until final byte M or m arrives.

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
        ;; ── Still accumulating X10 mouse intro (ESC [ M + up to 2 more) ──
        ((and (>= len 3)
              (= (aref buf 1) +byte-csi-bracket+)
              (= (aref buf 2) +byte-mouse-intro+)
              (< len 6))
         (values nil (make-escape-input-k session buf)))
        ;; ── SGR mouse terminated: ESC [ < Pb ; Px ; Py M|m ───────────────
        ((and (%sgr-mouse-sequence-p buf len)
              (%sgr-mouse-terminated-p buf len))
         (multiple-value-bind (btn col row release-p)
             (%parse-sgr-mouse buf len)
           (when btn
             (%dispatch-mouse-event session btn col row release-p)))
         (values nil #'%ground-input-state))
        ;; ── SGR mouse still accumulating ──────────────────────────────────
        ((%sgr-mouse-sequence-p buf len)
         (values nil (make-escape-input-k session buf)))
        ;; ── Copy-mode or forward: 3-byte CSI ESC [ FINAL ──────────────────
        ((and (= len 3) (= (aref buf 1) +byte-csi-bracket+)
              (/= (aref buf 2) +byte-mouse-intro+)
              (/= (aref buf 2) 60))          ; not SGR '<'
         ;; Check whether this is the start of a 4-byte ESC [ N ~ sequence
         ;; (function key / PageUp / PageDown): parameter digit followed by '~'.
         ;; Digits 5 and 6 (53='5', 54='6') indicate PageUp/PageDown.
         ;; If so, keep accumulating; otherwise dispatch or forward.
         (let ((third (aref buf 2)))
           (if (and (>= third 48) (<= third 57))  ; '0'..'9' — possible N~ seq
               ;; Could be ESC [ N ~ (4-byte); keep accumulating
               (values nil (make-escape-input-k session buf))
               (progn
                 (unless (handle-copy-mode-escape session buf)
                   ;; Not in copy mode (or unrecognised): forward raw bytes to pane
                   (unless (copy-mode-active-p session)
                     (%forward-octets session (subseq buf 0 len))))
                 (values nil #'%ground-input-state)))))
        ;; ── 4-byte function key: ESC [ N ~ ────────────────────────────────
        ;; PageUp = ESC [ 5 ~ (53 126), PageDown = ESC [ 6 ~ (54 126).
        ((and (= len 4) (= (aref buf 1) +byte-csi-bracket+)
              (= (aref buf 3) 126))           ; '~' = 126
         (let ((n (aref buf 2)))
           (cond
             ;; PageUp in copy mode
             ((and (= n 53) (copy-mode-active-p session))
              (let ((sc (%active-screen session)))
                (when sc
                  (copy-mode-scroll sc (screen-height sc))
                  (setf *dirty* t))))
             ;; PageDown in copy mode
             ((and (= n 54) (copy-mode-active-p session))
              (let ((sc (%active-screen session)))
                (when sc
                  (copy-mode-scroll sc (- (screen-height sc)))
                  (setf *dirty* t))))
             ;; Outside copy mode: forward raw bytes to pane
             (t
              (unless (copy-mode-active-p session)
                (%forward-octets session (subseq buf 0 len))))))
         (values nil #'%ground-input-state))
        ;; ── 4-byte accumulation: ESC [ N (not yet '~') — keep buffering ───
        ((and (= len 4) (= (aref buf 1) +byte-csi-bracket+)
              (/= (aref buf 3) 126))
         ;; Forward if no terminating ~ and not copy mode, return to ground
         (unless (copy-mode-active-p session)
           (%forward-octets session (subseq buf 0 len)))
         (values nil #'%ground-input-state))
        ;; ── 2-byte non-CSI sequence: ESC X ────────────────────────────────
        ;; In copy mode, a lone ESC (or ESC + non-CSI byte) exits copy mode.
        ;; Outside copy mode, forward the raw bytes to the pane.
        ((and (= len 2) (/= (aref buf 1) +byte-csi-bracket+))
         (if (copy-mode-active-p session)
             (let ((sc (%active-screen session)))
               (when sc (copy-mode-exit sc))
               (setf *dirty* t))
             (%forward-octets session (subseq buf 0 len)))
         (values nil #'%ground-input-state))
        ;; ── Buffer overflow guard (> 32 unrecognised bytes) ───────────────
        ((> len 32)
         (unless (copy-mode-active-p session)
           (%forward-octets session (subseq buf 0 len)))
         (values nil #'%ground-input-state))
        ;; ── Still accumulating ─────────────────────────────────────────────
        (t (values nil (make-escape-input-k session buf)))))))

;;; ── Additional key bindings ─────────────────────────────────────────────────

;; C-b w — choose-window (interactive overlay listing all windows)
(set-key-binding #\w :choose-window)
;; C-b s — choose-session (interactive overlay listing all sessions)
(set-key-binding #\s :choose-session)
;; C-b ! — break-pane (move active pane to a new window)
(set-key-binding #\! :break-pane)
;; C-b { / C-b } — swap-pane backward / forward
(set-key-binding #\{ :swap-pane-backward)
(set-key-binding #\} :swap-pane-forward)
;; C-b ; — last-pane (jump to previously active pane)
(set-key-binding #\; :last-pane)
;; C-b q — display-panes (show pane numbers)
(set-key-binding #\q :display-panes)
;; C-b ( / C-b ) — switch to prev/next session
(set-key-binding #\( :switch-client-prev)
(set-key-binding #\) :switch-client-next)
;; C-b L — last-session (switch to most recently active previous session)
(set-key-binding #\L :last-session)
;; C-b l — last-window (switch to previously active window)
(set-key-binding #\l :last-window)
;; C-b f — find-window (search window names and pane titles)
(set-key-binding #\f :find-window)

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

;;; ── Synchronize-panes broadcast ─────────────────────────────────────────────
;;;
;;; When the "synchronize-panes" window option is T, keystrokes sent to the
;;; active pane are also broadcast to every other pane in the same window.

(defun %forward-octets-synchronized (session octets)
  "Forward OCTETS to the active pane.  If synchronize-panes is enabled on
   the active window, also write to all other panes in the window."
  (let* ((win (session-active-window session))
         (ap  (and win (window-active-pane win))))
    (when ap
      (pty-write (pane-fd ap) octets)
      ;; Broadcast when synchronize-panes is enabled.
      (when (cl-tmux/options:get-option "synchronize-panes")
        (dolist (p (window-panes win))
          (unless (eq p ap)
            (ignore-errors (pty-write (pane-fd p) octets))))))))

;;; -- Main event loop --------------------------------------------------------

(defun %handle-resize (session)
  "Re-read terminal geometry and relayout the active window after SIGWINCH."
  (setf *resize-pending* nil)
  (multiple-value-setq (*term-rows* *term-cols*) (terminal-size))
  (let ((win (session-active-window session)))
    (when win
      (window-relayout win (- *term-rows* *status-height*) *term-cols*))))

(defun %maybe-rename-window-from-title (session)
  "If the active pane has set an OSC title and the window's automatic-rename
   option is enabled, propagate the title to the active window name."
  (let* ((ap  (session-active-pane session))
         (sc  (when ap (pane-screen ap)))
         (win (session-active-window session)))
    (when (and sc win
               (window-automatic-rename-p win)
               (not (string= (screen-title sc) "")))
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
