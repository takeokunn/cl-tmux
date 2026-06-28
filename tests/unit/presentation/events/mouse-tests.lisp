(in-package #:cl-tmux/test)

;;;; Tests for mouse support: SGR/X10 parsing, status-bar click, option gating.

(def-suite mouse-suite :description "Mouse input parsing and dispatch")
(in-suite mouse-suite)

;;; ── SGR mouse parsing ────────────────────────────────────────────────────────

(test parse-sgr-mouse-table
  "Parse SGR mouse sequences: btn, col (0-based), row (0-based), release-p."
  (dolist (c '(("~C[<0;5;3M"   0  4  2  nil "left press: M final, coords 5;3 → col 4 row 2")
               ("~C[<0;10;7m"  0  9  6  t   "left release: m final, coords 10;7 → col 9 row 6")
               ("~C[<64;1;1M" 64  0  0  nil "wheel-up press: btn 64, coords 1;1 → col 0 row 0")))
    (destructuring-bind (fmt expected-btn expected-col expected-row expected-rel desc) c
      (let* ((seq (map '(simple-array (unsigned-byte 8) (*))
                       #'char-code
                       (format nil fmt #\Escape)))
             (len (length seq))
             (buf (make-array len
                              :element-type '(unsigned-byte 8)
                              :fill-pointer len
                              :adjustable t
                              :initial-contents (coerce seq 'list))))
        (multiple-value-bind (btn col row release-p)
            (cl-tmux::%parse-sgr-mouse buf (fill-pointer buf))
          (is (= expected-btn btn)         "~A: btn" desc)
          (is (= expected-col col)         "~A: col" desc)
          (is (= expected-row row)         "~A: row" desc)
          (is (eql expected-rel release-p) "~A: release-p" desc))))))

(test parse-sgr-mouse-rejects-malformed-parameters
  "Malformed SGR mouse parameters must not be accepted as partial numbers."
  (flet ((buf-from (s)
           (let* ((bytes (map 'list #'char-code s))
                  (len (length bytes)))
             (make-array len
                         :element-type '(unsigned-byte 8)
                         :fill-pointer len
                         :adjustable t
                         :initial-contents bytes))))
    (dolist (c '(("~C[<0;12x;3M" "junk after column")
                 ("~C[<0;5;3q"   "invalid final byte")
                 ("~C[<0;;3M"    "empty column")
                 ("~C[<0;0;3M"   "zero column")
                 ("~C[<0;5;0M"   "zero row")))
      (destructuring-bind (fmt desc) c
        (let ((buf (buf-from (format nil fmt #\Escape))))
          (multiple-value-bind (btn col row release-p)
              (cl-tmux::%parse-sgr-mouse buf (fill-pointer buf))
            (is (null btn)       "~A: btn" desc)
            (is (null col)       "~A: col" desc)
            (is (null row)       "~A: row" desc)
            (is (null release-p) "~A: release-p" desc)))))))

;;; ── X10 mouse via process-byte ───────────────────────────────────────────────

(test parse-x10-mouse-bytes
  "X10: ESC [ M <btn+32> <col+33> <row+33> — decode btn=0, col=0, row=1."
  ;; btn=0 → raw=32; col=0 → raw=33; row=1 → raw=34
  (with-single-pane-mouse-session (sess win p0)
    (let ((state (cl-tmux::make-input-state)))
      ;; ESC [ M 32 33 34  (btn=0, col=0, row=1)
      (cl-tmux::process-byte sess 27 state)
      (cl-tmux::process-byte sess 91 state)
      (cl-tmux::process-byte sess 77 state)
      (cl-tmux::process-byte sess 32 state)
      (cl-tmux::process-byte sess 33 state)
      (cl-tmux::process-byte sess 34 state)
      ;; Focus is unchanged (single pane, clicking in it); no error expected.
      (is (eq p0 (window-active-pane win))
          "single-pane click keeps p0 active"))))

;;; ── Mouse option gating ──────────────────────────────────────────────────────

(test mouse-option-gating-off-by-default
  "When the 'mouse' option is NIL, %dispatch-mouse-event is a no-op."
  (with-two-pane-h-session (sess win p0 p1 :mouse nil)
    ;; Click in right pane — should NOT change focus because mouse is off
    (cl-tmux::%dispatch-mouse-event sess 0 50 5 nil)
    (is (eq p0 (window-active-pane win))
        "mouse off: focus must not change on click")))

(test mouse-option-gating-on
  "When the 'mouse' option is T, left-click changes active pane."
  (with-two-pane-mouse-session (sess win p0 p1)
    ;; Click in right pane (col 50, row 5 — within pane area, not status bar)
    (cl-tmux::%dispatch-mouse-event sess 0 50 5 nil)
    (is (eq p1 (window-active-pane win))
        "mouse on: click in right pane must focus p1")))

;;; ── Status bar click ─────────────────────────────────────────────────────────

(test mouse-status-bar-click-selects-window
  "A left click on the status bar row selects the window that is actually rendered there."
  (with-two-window-status-session (sess win0 win1)
    ;; Session name "0" contributes columns 0-1.  With the custom formats
    ;; above, win0 occupies column 2, '|' is column 3, and win1 starts at 4.
    (is (eq win1 (cl-tmux::%status-col-to-window sess 4))
        "custom status-bar layout must map column 4 to the second window")
    (cl-tmux::%dispatch-mouse-event sess 0 4 5 nil)
    (is (eq win1 (session-active-window sess))
        "clicking the rendered second window must select it")))

(test status-col-to-window-basic
  "%status-col-to-window returns the window whose label contains the given col."
  (let* ((p0   (make-pane :id 1 :fd -1 :pid -1 :x 0 :y 0 :width 80 :height 23
                           :screen (make-screen 80 23)))
         (p1   (make-pane :id 2 :fd -1 :pid -1 :x 0 :y 0 :width 80 :height 23
                           :screen (make-screen 80 23)))
         (win0 (make-window :id 1 :name "a" :width 80 :height 23
                            :panes (list p0) :tree (make-layout-leaf p0) :active p0))
         (win1 (make-window :id 2 :name "b" :width 80 :height 23
                            :panes (list p1) :tree (make-layout-leaf p1) :active p1))
         (sess (make-session :id 1 :name "s" :windows (list win0 win1) :active win0)))
    ;; Session name "s" = 1 char; prefix = " s" = 2 chars.
    ;; win0 active " 1:a* " = 6 chars, columns 2–7.
    ;; The separator adds column 8, so win1 starts at column 9.
    (let ((found0 (cl-tmux::%status-col-to-window sess 3)))
      (is (eq win0 found0) "%status-col-to-window col 3 must be win0"))
    (let ((found1 (cl-tmux::%status-col-to-window sess 9)))
      (is (eq win1 found1) "%status-col-to-window col 9 must be win1"))
    (let ((found-nil (cl-tmux::%status-col-to-window sess 1)))
      (is (null found-nil) "col 1 is in session-name prefix, not a window"))))

;;; ── Wheel scroll enters copy mode ────────────────────────────────────────────

(test mouse-wheel-up-enters-copy-mode
  "Wheel-up (btn 64) automatically enters copy mode and scrolls up."
  (with-single-pane-mouse-session (sess win p0)
    (let ((sc (pane-screen p0)))
      (seed-scrollback sc 10)
      (is-false (screen-copy-mode-p sc) "not in copy mode initially")
      (cl-tmux::%dispatch-mouse-event sess 64 0 0 nil)
      (is-true (screen-copy-mode-p sc)
               "wheel-up must enter copy mode"))))

(test mouse-wheel-down-exits-copy-mode-at-bottom
  "Wheel-down (btn 65) while at offset 0 exits copy mode."
  (with-single-pane-mouse-session (sess win p0)
    (let ((sc (pane-screen p0)))
      ;; Enter copy mode manually
      (cl-tmux/commands:copy-mode-enter sc)
      (is-true (screen-copy-mode-p sc))
      ;; Already at offset 0 — wheel-down should exit copy mode
      (cl-tmux::%dispatch-mouse-event sess 65 0 0 nil)
      (is-false (screen-copy-mode-p sc)
                "wheel-down at offset 0 must exit copy mode"))))

;;; ── Mouse key-table bindings (bind -n WheelUpPane / MouseDown1Pane) ──────────

(test mouse-key-name-builds-tmux-names
  "%mouse-key-name maps (button, release-p, location) to tmux mouse key names."
  (check-table
   (list (list (cl-tmux::%mouse-key-name 64 nil :pane)   "WheelUpPane"
               "wheel-up in pane")
         (list (cl-tmux::%mouse-key-name 65 nil :pane)   "WheelDownPane"
               "wheel-down in pane")
         (list (cl-tmux::%mouse-key-name 0 nil :pane)    "MouseDown1Pane"
               "left press in pane")
         (list (cl-tmux::%mouse-key-name 0 t :pane)      "MouseUp1Pane"
               "left release in pane")
         (list (cl-tmux::%mouse-key-name 1 nil :pane)    "MouseDown2Pane"
               "middle press in pane")
         (list (cl-tmux::%mouse-key-name 2 nil :status)  "MouseDown3Status"
               "right press on status")
         (list (cl-tmux::%mouse-key-name 64 nil :status) "WheelUpStatus"
               "wheel-up on status")
         (list (cl-tmux::%mouse-key-name 32 nil :pane)   nil
               "motion has no standard name"))
   :test #'equal))

(test mouse-event-action-classifies-built-in-behavior
  "%mouse-event-action turns raw mouse state into a symbolic built-in action."
  (check-table
   (list (list (cl-tmux::%mouse-event-action 0 nil :status) :status-click
               "left click on status")
         (list (cl-tmux::%mouse-event-action 64 nil :pane) :scroll-up
               "wheel-up")
         (list (cl-tmux::%mouse-event-action 65 nil :pane) :scroll-down
               "wheel-down")
         (list (cl-tmux::%mouse-event-action 0 nil :pane) :left-press
               "left press in pane")
         (list (cl-tmux::%mouse-event-action 0 t :pane) :left-release
               "left release")
         (list (cl-tmux::%mouse-event-action 1 nil :pane) :middle-press
               "middle press")
         (list (cl-tmux::%mouse-event-action 32 nil :pane) :motion
               "motion")
         (list (cl-tmux::%mouse-event-action 2 nil :status) nil
               "right click on status is unbound"))
   :test #'eql))

(test mouse-wheel-up-binding-fires-and-overrides-default
  "bind -n WheelUpPane <cmd> fires on a wheel-up event instead of the built-in
   copy-mode scroll."
  (with-isolated-mouse-session (s :nwindows 2)
    (cl-tmux/config:apply-config-directive
     '("bind" "-n" "WheelUpPane" "next-window"))
    (cl-tmux::%dispatch-mouse-event s 64 0 0 nil)
    (is (eq (second (session-windows s)) (session-active-window s))
        "bound WheelUpPane must run next-window")
    (is-false (screen-copy-mode-p (active-screen s))
        "the binding overrides the default copy-mode scroll")))

(test mouse-unbound-wheel-up-keeps-default-behavior
  "With no WheelUpPane binding, wheel-up still enters copy mode (default)."
  (with-isolated-mouse-session (s)
    (cl-tmux::%dispatch-mouse-event s 64 0 0 nil)
    (is-true (screen-copy-mode-p (active-screen s))
        "unbound wheel-up falls through to the built-in copy-mode scroll")))

(test mouse-copy-mode-table-binding-fires
  "In copy mode, a copy-mode-vi mouse binding fires, overriding the built-in
   wheel scroll (the copy-mode table is consulted before the root table)."
  (with-isolated-mouse-session (s :nwindows 2)
    (cl-tmux/options:set-option "mode-keys" "vi")
    (cl-tmux/config:apply-config-directive
     '("bind" "-T" "copy-mode-vi" "WheelUpPane" "next-window"))
    (cl-tmux/commands:copy-mode-enter (active-screen s))
    (cl-tmux::%dispatch-mouse-event s 64 0 0 nil)
    (is (eq (second (session-windows s)) (session-active-window s))
        "copy-mode-vi WheelUpPane must run next-window")))

;;; ── Double / triple click detection ─────────────────────────────────────────

(test mouse-click-count-increments-within-threshold
  "%mouse-click-count increments when a press is within the threshold at the same cell."
  (check-table
   (list (list (cl-tmux::%mouse-click-count '(1000 5 0 1) 1200 5 0 500)
               2 "within 500ms, same cell → 2")
         (list (cl-tmux::%mouse-click-count '(1000 5 0 2) 1400 5 0 500)
               3 "third click within threshold → 3"))))

(test mouse-click-count-resets-when-slow-or-moved
  "%mouse-click-count resets to 1 when the press is too slow, at a different cell,
   or there is no previous click."
  (check-table
   (list (list (cl-tmux::%mouse-click-count '(1000 5 0 1) 1600 5 0 500)
               1 "beyond threshold → reset to 1")
         (list (cl-tmux::%mouse-click-count '(1000 5 0 1) 1100 6 0 500)
               1 "different column → reset")
         (list (cl-tmux::%mouse-click-count '(1000 5 0 1) 1100 5 1 500)
               1 "different row → reset")
         (list (cl-tmux::%mouse-click-count nil 1000 5 0 500)
               1 "no previous click → 1"))))

(test mouse-double-click-selects-word
  "Two quick left-clicks at the same cell select the word under the pointer."
  (with-isolated-mouse-session (s)
    (feed (active-screen s) "foo bar baz")
    ;; Two presses at col 5 row 0 (inside "bar"); the rapid succession is
    ;; naturally within double-click-time (500ms).
    (cl-tmux::%dispatch-mouse-event s 0 5 0 nil)
    (cl-tmux::%dispatch-mouse-event s 0 5 0 nil)
    (is (string= "bar" (cl-tmux/commands::%selection-text (active-screen s)))
        "double-click selects the word 'bar'")))

;;; ── Border-at-position ───────────────────────────────────────────────────────

(test border-at-position-h-split
  "%border-at-position finds the :h separator between two side-by-side panes."
  (with-h-split-81-24 (p0 p1 win)
    ;; Separator is at column 40
    (multiple-value-bind (split orient)
        (cl-tmux::%border-at-position win 40 5)
      (is-true split  "border at col 40 must find a split node")
      (is (eq :h orient) "split orientation must be :h"))
    ;; Not a border at col 0
    (multiple-value-bind (split orient)
        (cl-tmux::%border-at-position win 0 5)
      (declare (ignore orient))
      (is-false split "col 0 is inside p0, not a border"))))

(test border-at-position-no-split
  "%border-at-position returns NIL for a single-pane window."
  (let* ((p0  (make-pane :id 1 :fd -1 :pid -1
                          :x 0 :y 0 :width 80 :height 24
                          :screen (make-screen 80 24)))
         (win (make-window :id 1 :name "w" :width 80 :height 24
                           :panes (list p0)
                           :tree  (make-layout-leaf p0))))
    (multiple-value-bind (split orient)
        (cl-tmux::%border-at-position win 40 12)
      (declare (ignore orient))
      (is-false split  "no border in a single-pane window"))))

;;; ── Mouse passthrough to pane PTY ────────────────────────────────────────────

(test mouse-passthrough-skipped-when-pane-mode-is-zero
  "%try-mouse-passthrough returns NIL when the target pane has mouse-mode=0,
   so tmux-UI handling (copy-mode, pane select) takes over."
  (let* ((scr  (make-screen 40 24))
         (pane (make-pane :id 1 :fd -1 :pid -1
                          :x 0 :y 0 :width 40 :height 24
                          :screen scr)))
    ;; Default mouse-mode is 0 — no tracking requested
    (is (zerop (cl-tmux/terminal/types:screen-mouse-mode scr))
        "precondition: screen-mouse-mode starts at 0")
    (is-false (cl-tmux::%try-mouse-passthrough nil pane 0 5 3 nil)
              "no passthrough when pane mouse-mode is 0")))

(test mouse-passthrough-x10-mode-skips-release
  "In X10 mouse mode (mode 1), release events must NOT be forwarded to the
   pane; only button presses are forwarded in this mode."
  (let* ((scr  (make-screen 40 24))
         (pane (make-pane :id 1 :fd -1 :pid -1
                          :x 0 :y 0 :width 40 :height 24
                          :screen scr)))
    (setf (cl-tmux/terminal/types:screen-mouse-mode scr) 1)
    ;; Release with fd=-1 returns NIL from %encode-mouse-for-pane (fd guard),
    ;; but the filter for mode=1 should reject release events before fd-check.
    (is-false (cl-tmux::%try-mouse-passthrough nil pane 0 5 3 t)
              "X10 mode must not forward release events")))

(test mouse-passthrough-mode2-forwards-release
  "In button-event mode (mode 2), release events ARE forwarded."
  (with-pipe-fds (rfd wfd)
    (let* ((scr  (make-screen 40 24))
           (pane (make-pane :id 1 :fd wfd :pid -1
                            :x 0 :y 0 :width 40 :height 24
                            :screen scr)))
      (setf (cl-tmux/terminal/types:screen-mouse-mode scr) 2)
      (let ((result (cl-tmux::%try-mouse-passthrough nil pane 0 5 3 t)))
        (is (eq result t)
            "mode-2 must forward non-motion button releases"))
      (is-true (cl-tmux/pty:select-fds (list rfd) 20000)
               "forwarded release must reach the pane PTY"))))
