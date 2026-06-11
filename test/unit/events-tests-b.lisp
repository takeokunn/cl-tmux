(in-package #:cl-tmux/test)

;;;; events tests — part B: locked session, sgr-mouse edge cases, drag/modifier
;;;; arrow coverage, copy-mode cursor, vi navigation, table-driven, prompt-key.

(in-suite events-suite)

;;; ── process-byte with locked session ──────────────────────────────────────────

(test process-byte-unlocks-locked-session
  "Any byte unlocks a locked session; subsequent bytes are processed normally."
  (with-fake-session (s)
    (setf (session-locked-p s) t)
    (let ((state (cl-tmux::make-input-state)))
      (is (null (cl-tmux::process-byte s (char-code #\a) state))
          "first byte on locked session returns NIL (unlocks)")
      (is-false (session-locked-p s)
                "session must be unlocked after any byte"))))

;;; ── %sgr-mouse-sequence-p edge cases ────────────────────────────────────────

(test sgr-mouse-sequence-p-returns-nil-for-short-buffer
  "%sgr-mouse-sequence-p returns NIL for a buffer shorter than 3 bytes."
  (flet ((mk-buf (s)
           (make-array (length s) :element-type '(unsigned-byte 8)
                       :initial-contents (map 'list #'char-code s))))
    (let* ((short (mk-buf (format nil "~C[" #\Escape)))  ; only 2 bytes
           (len   (length short)))
      (is (null (cl-tmux::%sgr-mouse-sequence-p short len))
          "2-byte buffer must return NIL (need at least 3)"))))

(test sgr-mouse-terminated-p-returns-nil-for-short-buffer
  "%sgr-mouse-terminated-p returns NIL for a buffer of 3 or fewer bytes."
  (let ((buf (make-array 3 :element-type '(unsigned-byte 8)
                           :initial-contents '(27 91 60))))
    (is (null (cl-tmux::%sgr-mouse-terminated-p buf 3))
        "3-byte buffer must return NIL (need more than 3)")))

;;; ── %apply-drag-resize coverage ─────────────────────────────────────────────

(test apply-drag-resize-horizontal-updates-ratio
  "%apply-drag-resize on a :h split moves the separator to the dragged column."
  (let* ((p0   (make-pane :id 1 :fd -1 :pid -1 :x 0 :y 0 :width 40 :height 24
                           :screen (make-screen 40 24)))
         (p1   (make-pane :id 2 :fd -1 :pid -1 :x 41 :y 0 :width 40 :height 24
                           :screen (make-screen 40 24)))
         (leaf0 (make-layout-leaf p0))
         (leaf1 (make-layout-leaf p1))
         (split (make-layout-split :h leaf0 leaf1 1/2))
         (win   (make-window :id 1 :name "w" :width 81 :height 24
                             :panes (list p0 p1) :tree split)))
    (window-select-pane win p0)
    ;; Drag the border rightward: col=60 out of total ~81 columns.
    (cl-tmux::%apply-drag-resize win split :h 60 5)
    ;; The ratio must have changed from 1/2.
    (is (/= 1/2 (cl-tmux/model:layout-split-ratio split))
        "%apply-drag-resize must update the split ratio on :h drag")))

(test apply-drag-resize-vertical-updates-ratio
  "%apply-drag-resize on a :v split moves the separator to the dragged row."
  (let* ((p0   (make-pane :id 1 :fd -1 :pid -1 :x 0 :y 0  :width 80 :height 10
                           :screen (make-screen 80 10)))
         (p1   (make-pane :id 2 :fd -1 :pid -1 :x 0 :y 11 :width 80 :height 10
                           :screen (make-screen 80 10)))
         (leaf0 (make-layout-leaf p0))
         (leaf1 (make-layout-leaf p1))
         (split (make-layout-split :v leaf0 leaf1 1/2))
         (win   (make-window :id 1 :name "w" :width 80 :height 21
                             :panes (list p0 p1) :tree split)))
    (window-select-pane win p0)
    ;; Drag border downward: row=15 out of total ~21 rows.
    (cl-tmux::%apply-drag-resize win split :v 5 15)
    (is (/= 1/2 (cl-tmux/model:layout-split-ratio split))
        "%apply-drag-resize must update the split ratio on :v drag")))

;;; ── %dispatch-modifier-arrow coverage ───────────────────────────────────────

(test dispatch-modifier-arrow-ctrl-arrow-resizes-one-cell
  "C-arrow (mod-byte=53) dispatches resize-pane with amount=1 without signaling."
  (with-fake-session (s)
    ;; Feed C-b ESC [ 1 ; 5 A (C-Up) through process-byte.
    ;; Expect no error and NIL return.
    (let ((state (cl-tmux::make-input-state)))
      (cl-tmux::process-byte s 2   state)   ; C-b prefix
      (cl-tmux::process-byte s 27  state)   ; ESC
      (cl-tmux::process-byte s 91  state)   ; [
      (cl-tmux::process-byte s 49  state)   ; 1
      (cl-tmux::process-byte s 59  state)   ; ;
      (cl-tmux::process-byte s 53  state)   ; 5 (Ctrl)
      (is (null (cl-tmux::process-byte s 65 state))   ; A (Up)
          "C-b C-Up must return NIL (no quit/detach)"))))

(test dispatch-modifier-arrow-meta-arrow-dispatches-resize-command
  "M-arrow (mod-byte=51) dispatches :resize-* command without signaling."
  (with-fake-session (s)
    (let ((state (cl-tmux::make-input-state)))
      (cl-tmux::process-byte s 2   state)   ; C-b prefix
      (cl-tmux::process-byte s 27  state)   ; ESC
      (cl-tmux::process-byte s 91  state)   ; [
      (cl-tmux::process-byte s 49  state)   ; 1
      (cl-tmux::process-byte s 59  state)   ; ;
      (cl-tmux::process-byte s 51  state)   ; 3 (Meta)
      (is (null (cl-tmux::process-byte s 66 state))   ; B (Down)
          "C-b M-Down must return NIL (no quit/detach)"))))

;;; ── copy-mode-set-cursor command coverage ────────────────────────────────────

(test copy-mode-set-cursor-updates-cursor-position
  "copy-mode-set-cursor sets the copy-mode cursor to the given (row, col)."
  (with-fake-session (s)
    (let ((screen (active-screen s)))
      (cl-tmux::dispatch-command s :copy-mode-enter nil)
      (is (screen-copy-mode-p screen) "copy mode entered")
      ;; Place cursor at (3, 5)
      (cl-tmux/commands::copy-mode-set-cursor screen 3 5)
      (is (equal (cons 3 5) (screen-copy-cursor screen))
          "copy-mode-set-cursor must set cursor to (row . col)"))))

(test copy-mode-set-cursor-clamps-to-screen-bounds
  "copy-mode-set-cursor clamps row/col to [0, height-1] / [0, width-1]."
  (with-fake-session (s)
    (let ((screen (active-screen s)))
      (cl-tmux::dispatch-command s :copy-mode-enter nil)
      ;; Attempt to set cursor far out of bounds.
      (cl-tmux/commands::copy-mode-set-cursor screen 999 999)
      (let* ((cursor (screen-copy-cursor screen))
             (row    (car cursor))
             (col    (cdr cursor)))
        (is (<= 0 row (1- (screen-height screen)))
            "clamped row must be within [0, height-1]")
        (is (<= 0 col (1- (screen-width screen)))
            "clamped col must be within [0, width-1]")))))

;;; ── copy-mode cursor-in-interior no-op (negative path) ─────────────────────

(test copy-mode-j-at-interior-row-moves-cursor-not-scrolls
  "Plain 'j' when cursor is at an interior row (not bottom) moves cursor down without
   scrolling the viewport.  The offset must remain unchanged."
  (let ((s (make-fake-session)))
    (with-loop-state
      (let ((screen (active-screen s))
            (state  (cl-tmux::make-input-state)))
        (cl-tmux::dispatch-command s :copy-mode-enter nil)
        (seed-scrollback screen 10)
        ;; Scroll viewport up so there is room both above and below cursor.
        (cl-tmux/commands::copy-mode-scroll screen 5)
        (let ((initial-offset (screen-copy-offset screen)))
          ;; Place cursor at an interior row (not the bottom).
          (setf (cl-tmux/terminal/types:screen-copy-cursor screen)
                (cons 0 0))
          ;; 'j' should move cursor down without touching the offset.
          (cl-tmux::process-byte s (char-code #\j) state)
          (is (= initial-offset (screen-copy-offset screen))
              "j at interior row must not change copy-offset")
          (let ((new-row (car (screen-copy-cursor screen))))
            (is (= 1 new-row) "j must move cursor down by 1 row")))))))

(test copy-mode-k-at-interior-row-moves-cursor-not-scrolls
  "Plain 'k' when cursor is at an interior row (not top) moves cursor up without
   scrolling the viewport.  The offset must remain unchanged."
  (let ((s (make-fake-session)))
    (with-loop-state
      (let ((screen (active-screen s))
            (state  (cl-tmux::make-input-state)))
        (cl-tmux::dispatch-command s :copy-mode-enter nil)
        (seed-scrollback screen 10)
        ;; Scroll viewport up and place cursor at an interior row.
        (cl-tmux/commands::copy-mode-scroll screen 5)
        (let ((initial-offset (screen-copy-offset screen)))
          (setf (cl-tmux/terminal/types:screen-copy-cursor screen)
                (cons 5 0))  ; row 5 — not at top (row 0)
          ;; 'k' should move cursor up without touching the offset.
          (cl-tmux::process-byte s (char-code #\k) state)
          (is (= initial-offset (screen-copy-offset screen))
              "k at interior row must not change copy-offset")
          (let ((new-row (car (screen-copy-cursor screen))))
            (is (= 4 new-row) "k must move cursor up by 1 row")))))))

;;; ── %prefix-csi-arrow-cmd direct tests ──────────────────────────────────────

(test prefix-csi-arrow-cmd-maps-all-four-directions
  "%prefix-csi-arrow-cmd returns the correct command keyword for each arrow byte."
  (is (eq :select-pane-up    (cl-tmux::%prefix-csi-arrow-cmd 65))
      "A (65) must map to :select-pane-up")
  (is (eq :select-pane-down  (cl-tmux::%prefix-csi-arrow-cmd 66))
      "B (66) must map to :select-pane-down")
  (is (eq :select-pane-right (cl-tmux::%prefix-csi-arrow-cmd 67))
      "C (67) must map to :select-pane-right")
  (is (eq :select-pane-left  (cl-tmux::%prefix-csi-arrow-cmd 68))
      "D (68) must map to :select-pane-left"))

(test prefix-csi-arrow-cmd-returns-nil-for-non-arrows
  "%prefix-csi-arrow-cmd returns NIL for bytes that are not arrow final bytes."
  (is (null (cl-tmux::%prefix-csi-arrow-cmd 72))
      "H (72) must return NIL")
  (is (null (cl-tmux::%prefix-csi-arrow-cmd 0))
      "NUL (0) must return NIL")
  (is (null (cl-tmux::%prefix-csi-arrow-cmd 109))
      "m (109) must return NIL"))

;;; ── %border-at-position direct tests ────────────────────────────────────────

(test border-at-position-detects-h-split-border
  "%border-at-position returns the split node and :h when col is exactly on the separator."
  (let* ((p0    (make-pane :id 1 :fd -1 :pid -1 :x 0 :y 0 :width 40 :height 24
                            :screen (make-screen 40 24)))
         (p1    (make-pane :id 2 :fd -1 :pid -1 :x 41 :y 0 :width 40 :height 24
                            :screen (make-screen 40 24)))
         (leaf0 (make-layout-leaf p0))
         (leaf1 (make-layout-leaf p1))
         (split (make-layout-split :h leaf0 leaf1 1/2))
         (win   (make-window :id 1 :name "w" :width 81 :height 24
                             :panes (list p0 p1) :tree split)))
    (declare (ignore win))
    ;; The separator col for p0 (x=0 w=40) is at col 40.
    (multiple-value-bind (found-split orientation)
        (cl-tmux::%border-at-position
         (make-window :id 1 :name "w" :width 81 :height 24
                      :panes (list p0 p1) :tree split)
         40 5)
      (is (eq split found-split)
          "%border-at-position must return the split node at the separator column")
      (is (eq :h orientation)
          "%border-at-position must report :h orientation for horizontal split"))))

(test border-at-position-returns-nil-inside-pane
  "%border-at-position returns (values NIL NIL) when col is inside a pane."
  (let* ((p0    (make-pane :id 1 :fd -1 :pid -1 :x 0 :y 0 :width 40 :height 24
                            :screen (make-screen 40 24)))
         (p1    (make-pane :id 2 :fd -1 :pid -1 :x 41 :y 0 :width 40 :height 24
                            :screen (make-screen 40 24)))
         (leaf0 (make-layout-leaf p0))
         (leaf1 (make-layout-leaf p1))
         (split (make-layout-split :h leaf0 leaf1 1/2))
         (win   (make-window :id 1 :name "w" :width 81 :height 24
                             :panes (list p0 p1) :tree split)))
    (multiple-value-bind (found-split orientation)
        (cl-tmux::%border-at-position win 20 5)
      (is (null found-split)
          "%border-at-position must return NIL split inside pane")
      (is (null orientation)
          "%border-at-position must return NIL orientation inside pane"))))

(test border-at-position-returns-nil-for-single-pane-window
  "%border-at-position returns (values NIL NIL) when the window has no split."
  (let* ((p0  (make-pane :id 1 :fd -1 :pid -1 :x 0 :y 0 :width 80 :height 24
                          :screen (make-screen 80 24)))
         (win (make-window :id 1 :name "w" :width 80 :height 24
                           :panes (list p0)
                           :tree  (make-layout-leaf p0))))
    (multiple-value-bind (found-split orientation)
        (cl-tmux::%border-at-position win 20 10)
      (is (null found-split)
          "single-pane window must have no border")
      (is (null orientation)
          "single-pane window must have NIL orientation"))))

;;; ── %mouse-status-bar-click direct tests ─────────────────────────────────────

(test mouse-status-bar-click-selects-window
  "%mouse-status-bar-click changes the active window when the click col lands in an entry."
  (let* ((p0   (make-pane :id 1 :fd -1 :pid -1 :x 0 :y 0 :width 20 :height 5
                           :screen (make-screen 20 5)))
         (p1   (make-pane :id 2 :fd -1 :pid -1 :x 0 :y 0 :width 20 :height 5
                           :screen (make-screen 20 5)))
         (win0 (make-window :id 0 :name "w" :width 20 :height 5
                            :panes (list p0) :tree (make-layout-leaf p0)))
         (win1 (make-window :id 1 :name "x" :width 20 :height 5
                            :panes (list p1) :tree (make-layout-leaf p1)))
         (sess (make-session :id 1 :name "s" :windows (list win0 win1))))
    (window-select-pane win0 p0)
    (window-select-pane win1 p1)
    (session-select-window sess win0)
    ;; Session prefix " s" = 2 chars.
    ;; win0 "w" entry starts at col 2 (4 + 1 = 5 chars: "  w ?" or " [w] ")
    ;; win1 "x" entry starts at col 7.
    ;; Clicking col 7 should activate win1.
    (cl-tmux::%mouse-status-bar-click sess 7)
    (is (eq win1 (session-active-window sess))
        "%mouse-status-bar-click at col 7 must select win1")))

(test mouse-status-bar-click-does-nothing-for-out-of-range-col
  "%mouse-status-bar-click is a no-op when col falls before all window entries."
  (let* ((p0   (make-pane :id 1 :fd -1 :pid -1 :x 0 :y 0 :width 20 :height 5
                           :screen (make-screen 20 5)))
         (win0 (make-window :id 0 :name "w" :width 20 :height 5
                            :panes (list p0) :tree (make-layout-leaf p0)))
         (sess (make-session :id 1 :name "s" :windows (list win0))))
    (window-select-pane win0 p0)
    (session-select-window sess win0)
    ;; Col 0 is before the first entry — no window should be selected.
    (cl-tmux::%mouse-status-bar-click sess 0)
    (is (eq win0 (session-active-window sess))
        "%mouse-status-bar-click at col 0 must not change the active window")))

;;; ── Copy-mode additional vi navigation keys ──────────────────────────────────

(test copy-mode-h-moves-cursor-left
  "Plain 'h' (byte 104) moves the copy-mode cursor left by one column."
  (let ((s (make-fake-session)))
    (with-loop-state
      (let ((screen (active-screen s))
            (state  (cl-tmux::make-input-state)))
        (cl-tmux::dispatch-command s :copy-mode-enter nil)
        ;; Place cursor at column 3.
        (setf (cl-tmux/terminal/types:screen-copy-cursor screen) (cons 0 3))
        (cl-tmux::process-byte s 104 state)   ; h
        (let ((col (cdr (screen-copy-cursor screen))))
          (is (= 2 col) "h must move cursor left by 1 column"))))))

(test copy-mode-l-moves-cursor-right
  "Plain 'l' (byte 108) moves the copy-mode cursor right by one column."
  (let ((s (make-fake-session)))
    (with-loop-state
      (let ((screen (active-screen s))
            (state  (cl-tmux::make-input-state)))
        (cl-tmux::dispatch-command s :copy-mode-enter nil)
        ;; Place cursor at column 0.
        (setf (cl-tmux/terminal/types:screen-copy-cursor screen) (cons 0 0))
        (cl-tmux::process-byte s 108 state)   ; l
        (let ((col (cdr (screen-copy-cursor screen))))
          (is (= 1 col) "l must move cursor right by 1 column"))))))

(test copy-mode-i-exits-copy-mode
  "Plain 'i' (byte 105) exits copy mode without needing the C-b prefix."
  (let ((s (make-fake-session)))
    (with-loop-state
      (let ((screen (active-screen s))
            (state  (cl-tmux::make-input-state)))
        (cl-tmux::dispatch-command s :copy-mode-enter nil)
        (is (screen-copy-mode-p screen) "copy mode entered")
        (cl-tmux::process-byte s 105 state)   ; i
        (is-false (screen-copy-mode-p screen)
            "i must exit copy mode")))))

(test copy-mode-zero-moves-to-line-start
  "Plain '0' (byte 48) moves the cursor to the start of the current line."
  (let ((s (make-fake-session)))
    (with-loop-state
      (let ((screen (active-screen s))
            (state  (cl-tmux::make-input-state)))
        (cl-tmux::dispatch-command s :copy-mode-enter nil)
        ;; Place cursor somewhere in the middle.
        (setf (cl-tmux/terminal/types:screen-copy-cursor screen) (cons 2 5))
        (cl-tmux::process-byte s 48 state)   ; 0
        (let ((col (cdr (screen-copy-cursor screen))))
          (is (= 0 col) "0 must move cursor column to 0"))))))

(test copy-mode-dollar-moves-to-line-end
  "Plain '$' (byte 36) moves the cursor to the end of the current line."
  (let ((s (make-fake-session)))
    (with-loop-state
      (let ((screen (active-screen s))
            (state  (cl-tmux::make-input-state)))
        (cl-tmux::dispatch-command s :copy-mode-enter nil)
        ;; Start at col 0.
        (setf (cl-tmux/terminal/types:screen-copy-cursor screen) (cons 0 0))
        (cl-tmux::process-byte s 36 state)   ; $
        (let* ((col   (cdr (screen-copy-cursor screen)))
               (width (screen-width screen)))
          (is (= (1- width) col) "$ must move cursor column to width-1"))))))

(test copy-mode-ctrl-n-scrolls-down
  "C-n (byte 14) moves the cursor down by 1 in copy mode (same as j)."
  (let ((s (make-fake-session)))
    (with-loop-state
      (let ((screen (active-screen s))
            (state  (cl-tmux::make-input-state)))
        (cl-tmux::dispatch-command s :copy-mode-enter nil)
        (seed-scrollback screen 10)
        (cl-tmux/commands::copy-mode-scroll screen 5)
        (setf (cl-tmux/terminal/types:screen-copy-cursor screen)
              (cons (1- (screen-height screen)) 0))
        (let ((offset-before (screen-copy-offset screen)))
          (cl-tmux::process-byte s 14 state)   ; C-n
          (is (= (1- offset-before) (screen-copy-offset screen))
              "C-n at bottom row must scroll viewport down by 1"))))))

(test copy-mode-ctrl-p-scrolls-up
  "C-p (byte 16) moves the cursor up by 1 in copy mode (same as k)."
  (let ((s (make-fake-session)))
    (with-loop-state
      (let ((screen (active-screen s))
            (state  (cl-tmux::make-input-state)))
        (cl-tmux::dispatch-command s :copy-mode-enter nil)
        (seed-scrollback screen 10)
        ;; Cursor at top row → C-p scrolls viewport.
        (setf (cl-tmux/terminal/types:screen-copy-cursor screen) (cons 0 0))
        (cl-tmux::process-byte s 16 state)   ; C-p
        (is (= 1 (screen-copy-offset screen))
            "C-p at top row must scroll viewport up by 1")))))

(test copy-mode-H-moves-cursor-to-high
  "Plain 'H' (byte 72) moves the copy-mode cursor to the top row of the screen."
  (let ((s (make-fake-session)))
    (with-loop-state
      (let ((screen (active-screen s))
            (state  (cl-tmux::make-input-state)))
        (cl-tmux::dispatch-command s :copy-mode-enter nil)
        ;; Place cursor at some non-zero row first.
        (setf (cl-tmux/terminal/types:screen-copy-cursor screen) (cons 3 0))
        (cl-tmux::process-byte s 72 state)   ; H
        (let ((row (car (screen-copy-cursor screen))))
          (is (= 0 row) "H must move cursor to row 0 (top of screen)"))))))

(test copy-mode-L-moves-cursor-to-low
  "Plain 'L' (byte 76) moves the copy-mode cursor to the bottom row of the screen."
  (let ((s (make-fake-session)))
    (with-loop-state
      (let ((screen (active-screen s))
            (state  (cl-tmux::make-input-state)))
        (cl-tmux::dispatch-command s :copy-mode-enter nil)
        ;; Cursor at row 0.
        (setf (cl-tmux/terminal/types:screen-copy-cursor screen) (cons 0 0))
        (cl-tmux::process-byte s 76 state)   ; L
        (let* ((row    (car (screen-copy-cursor screen)))
               (height (screen-height screen)))
          (is (= (1- height) row) "L must move cursor to last row"))))))

(test copy-mode-V-begins-line-selection
  "Plain 'V' (byte 86) starts line-selection mode in copy mode without signaling."
  (let ((s (make-fake-session)))
    (with-loop-state
      (let ((screen (active-screen s))
            (state  (cl-tmux::make-input-state)))
        (cl-tmux::dispatch-command s :copy-mode-enter nil)
        (finishes (cl-tmux::process-byte s 86 state))
        ;; V activates either regular selection or line-selection — check both.
        (is (or (screen-copy-selecting screen)
                (screen-copy-line-selection-p screen))
            "V must activate some form of selection in copy mode")))))

(test copy-mode-space-begins-selection
  "Plain Space (byte 32) starts selection in copy mode."
  (let ((s (make-fake-session)))
    (with-loop-state
      (let ((screen (active-screen s))
            (state  (cl-tmux::make-input-state)))
        (cl-tmux::dispatch-command s :copy-mode-enter nil)
        (finishes (cl-tmux::process-byte s 32 state))
        (is (screen-copy-selecting screen)
            "Space must activate copy selection")))))
