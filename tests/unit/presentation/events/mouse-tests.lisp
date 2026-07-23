(in-package #:cl-tmux/test)

;;;; Tests for mouse support: SGR/X10 parsing, status-bar click, option gating.

(describe "mouse-suite"

  ;;; ── SGR mouse parsing ────────────────────────────────────────────────────────

  ;; Parse SGR mouse sequences: btn, col (0-based), row (0-based), release-p.
  (it "parse-sgr-mouse-table"
    (dolist (c '(("~C[<0;5;3M"   0  4  2  nil "left press: M final, coords 5;3 → col 4 row 2")
                 ("~C[<0;10;7m"  0  9  6  t   "left release: m final, coords 10;7 → col 9 row 6")
                 ("~C[<64;1;1M" 64  0  0  nil "wheel-up press: btn 64, coords 1;1 → col 0 row 0")))
      (destructuring-bind (fmt expected-btn expected-col expected-row expected-rel desc) c
        (declare (ignore desc))
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
            (expect (= expected-btn btn))
            (expect (= expected-col col))
            (expect (= expected-row row))
            (expect (eql expected-rel release-p)))))))

  ;; Malformed SGR mouse parameters must not be accepted as partial numbers.
  (it "parse-sgr-mouse-rejects-malformed-parameters"
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
          (declare (ignore desc))
          (let ((buf (buf-from (format nil fmt #\Escape))))
            (multiple-value-bind (btn col row release-p)
                (cl-tmux::%parse-sgr-mouse buf (fill-pointer buf))
              (expect (null btn))
              (expect (null col))
              (expect (null row))
              (expect (null release-p))))))))

  ;;; ── X10 mouse via process-byte ───────────────────────────────────────────────

  ;; X10: ESC [ M <btn+32> <col+33> <row+33> — decode btn=0, col=0, row=1.
  (it "parse-x10-mouse-bytes"
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
        (expect (eq p0 (window-active-pane win))))))

  ;;; ── Mouse option gating ──────────────────────────────────────────────────────

  ;; When the 'mouse' option is NIL, %dispatch-mouse-event is a no-op.
  (it "mouse-option-gating-off-by-default"
    (with-two-pane-h-session (sess win p0 p1 :mouse nil)
      ;; Click in right pane — should NOT change focus because mouse is off
      (cl-tmux::%dispatch-mouse-event sess 0 50 5 nil)
      (expect (eq p0 (window-active-pane win)))))

  ;; When the 'mouse' option is T, left-click changes active pane.
  (it "mouse-option-gating-on"
    (with-two-pane-mouse-session (sess win p0 p1)
      ;; Click in right pane (col 50, row 5 — within pane area, not status bar)
      (cl-tmux::%dispatch-mouse-event sess 0 50 5 nil)
      (expect (eq p1 (window-active-pane win)))))

  ;;; ── Status bar click ─────────────────────────────────────────────────────────

  ;; A left click on the status bar row selects the window that is actually rendered there.
  (it "mouse-status-bar-click-selects-window"
    (with-two-window-status-session (sess win0 win1)
      ;; Session name "0" contributes columns 0-1.  With the custom formats
      ;; above, win0 occupies column 2, '|' is column 3, and win1 starts at 4.
      (expect (eq win1 (cl-tmux::%status-col-to-window sess 4)))
      (cl-tmux::%dispatch-mouse-event sess 0 4 5 nil)
      (expect (eq win1 (session-active-window sess)))))

  ;; %status-col-to-window returns the window whose label contains the given col.
  (it "status-col-to-window-basic"
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
        (expect (eq win0 found0)))
      (let ((found1 (cl-tmux::%status-col-to-window sess 9)))
        (expect (eq win1 found1)))
      (let ((found-nil (cl-tmux::%status-col-to-window sess 1)))
        (expect (null found-nil)))))

  ;;; ── Wheel scroll enters copy mode ────────────────────────────────────────────

  ;; Wheel-up (btn 64) automatically enters copy mode and scrolls up.
  (it "mouse-wheel-up-enters-copy-mode"
    (with-single-pane-mouse-session (sess win p0)
      (let ((sc (pane-screen p0)))
        (seed-scrollback sc 10)
        (expect (screen-copy-mode-p sc) :to-be-falsy)
        (cl-tmux::%dispatch-mouse-event sess 64 0 0 nil)
        (expect (screen-copy-mode-p sc) :to-be-truthy))))

  ;; Wheel-down (btn 65) while at offset 0 exits copy mode.
  (it "mouse-wheel-down-exits-copy-mode-at-bottom"
    (with-single-pane-mouse-session (sess win p0)
      (let ((sc (pane-screen p0)))
        ;; Enter copy mode manually
        (cl-tmux/commands:copy-mode-enter sc)
        (expect (screen-copy-mode-p sc) :to-be-truthy)
        ;; Already at offset 0 — wheel-down should exit copy mode
        (cl-tmux::%dispatch-mouse-event sess 65 0 0 nil)
        (expect (screen-copy-mode-p sc) :to-be-falsy))))

  ;;; ── Mouse key-table bindings (bind -n WheelUpPane / MouseDown1Pane) ──────────

  ;; %mouse-key-name maps (button, release-p, location) to tmux mouse key names.
  (it "mouse-key-name-builds-tmux-names"
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

  ;; %mouse-event-action turns raw mouse state into a symbolic built-in action.
  (it "mouse-event-action-classifies-built-in-behavior"
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

  ;; bind -n WheelUpPane <cmd> fires on a wheel-up event instead of the built-in
  ;; copy-mode scroll.
  (it "mouse-wheel-up-binding-fires-and-overrides-default"
    (with-isolated-mouse-session (s :nwindows 2)
      (cl-tmux/config:apply-config-directive
       '("bind" "-n" "WheelUpPane" "next-window"))
      (cl-tmux::%dispatch-mouse-event s 64 0 0 nil)
      (expect (eq (second (session-windows s)) (session-active-window s)))
      (expect (screen-copy-mode-p (active-screen s)) :to-be-falsy)))

  ;; With no WheelUpPane binding, wheel-up still enters copy mode (default).
  (it "mouse-unbound-wheel-up-keeps-default-behavior"
    (with-isolated-mouse-session (s)
      (cl-tmux::%dispatch-mouse-event s 64 0 0 nil)
      (expect (screen-copy-mode-p (active-screen s)) :to-be-truthy)))

  ;; In copy mode, a copy-mode-vi mouse binding fires, overriding the built-in
  ;; wheel scroll (the copy-mode table is consulted before the root table).
  (it "mouse-copy-mode-table-binding-fires"
    (with-isolated-mouse-session (s :nwindows 2)
      (cl-tmux/options:set-option "mode-keys" "vi")
      (cl-tmux/config:apply-config-directive
       '("bind" "-T" "copy-mode-vi" "WheelUpPane" "next-window"))
      (cl-tmux/commands:copy-mode-enter (active-screen s))
      (cl-tmux::%dispatch-mouse-event s 64 0 0 nil)
      (expect (eq (second (session-windows s)) (session-active-window s)))))

  ;;; ── Double / triple click detection ─────────────────────────────────────────

  ;; %mouse-click-count increments when a press is within the threshold at the same cell.
  (it "mouse-click-count-increments-within-threshold"
    (check-table
     (list (list (cl-tmux::%mouse-click-count '(1000 5 0 1) 1200 5 0 500)
                 2 "within 500ms, same cell → 2")
           (list (cl-tmux::%mouse-click-count '(1000 5 0 2) 1400 5 0 500)
                 3 "third click within threshold → 3"))))

  ;; %mouse-click-count resets to 1 when the press is too slow, at a different cell,
  ;; or there is no previous click.
  (it "mouse-click-count-resets-when-slow-or-moved"
    (check-table
     (list (list (cl-tmux::%mouse-click-count '(1000 5 0 1) 1600 5 0 500)
                 1 "beyond threshold → reset to 1")
           (list (cl-tmux::%mouse-click-count '(1000 5 0 1) 1100 6 0 500)
                 1 "different column → reset")
           (list (cl-tmux::%mouse-click-count '(1000 5 0 1) 1100 5 1 500)
                 1 "different row → reset")
           (list (cl-tmux::%mouse-click-count nil 1000 5 0 500)
                 1 "no previous click → 1"))))

  ;; Two quick left-clicks at the same cell select the word under the pointer.
  (it "mouse-double-click-selects-word"
    (with-isolated-mouse-session (s)
      (feed (active-screen s) "foo bar baz")
      ;; Two presses at col 5 row 0 (inside "bar"); the rapid succession is
      ;; naturally within double-click-time (500ms).
      (cl-tmux::%dispatch-mouse-event s 0 5 0 nil)
      (cl-tmux::%dispatch-mouse-event s 0 5 0 nil)
      (expect (string= "bar" (cl-tmux/commands::%selection-text (active-screen s))))))

  ;;; ── Border-at-position ───────────────────────────────────────────────────────

  ;; %border-at-position finds the :h separator between two side-by-side panes.
  (it "border-at-position-h-split"
    (with-h-split-81-24 (p0 p1 win)
      ;; Separator is at column 40
      (multiple-value-bind (split orient)
          (cl-tmux::%border-at-position win 40 5)
        (expect split :to-be-truthy)
        (expect (eq :h orient)))
      ;; Not a border at col 0
      (multiple-value-bind (split orient)
          (cl-tmux::%border-at-position win 0 5)
        (declare (ignore orient))
        (expect split :to-be-falsy))))

  ;; %border-at-position returns NIL for a single-pane window.
  (it "border-at-position-no-split"
    (let* ((p0  (make-pane :id 1 :fd -1 :pid -1
                            :x 0 :y 0 :width 80 :height 24
                            :screen (make-screen 80 24)))
           (win (make-window :id 1 :name "w" :width 80 :height 24
                             :panes (list p0)
                             :tree  (make-layout-leaf p0))))
      (multiple-value-bind (split orient)
          (cl-tmux::%border-at-position win 40 12)
        (declare (ignore orient))
        (expect split :to-be-falsy))))

  ;;; ── Mouse passthrough to pane PTY ────────────────────────────────────────────

  ;; %try-mouse-passthrough returns NIL when the target pane has mouse-mode=0,
  ;; so tmux-UI handling (copy-mode, pane select) takes over.
  (it "mouse-passthrough-skipped-when-pane-mode-is-zero"
    (let* ((scr  (make-screen 40 24))
           (pane (make-pane :id 1 :fd -1 :pid -1
                            :x 0 :y 0 :width 40 :height 24
                            :screen scr)))
      ;; Default mouse-mode is 0 — no tracking requested
      (expect (zerop (cl-tmux/terminal/types:screen-mouse-mode scr)))
      (expect (cl-tmux::%try-mouse-passthrough nil pane 0 5 3 nil) :to-be-falsy)))

  ;; In X10 mouse mode (mode 1), release events must NOT be forwarded to the
  ;; pane; only button presses are forwarded in this mode.
  (it "mouse-passthrough-x10-mode-skips-release"
    (let* ((scr  (make-screen 40 24))
           (pane (make-pane :id 1 :fd -1 :pid -1
                            :x 0 :y 0 :width 40 :height 24
                            :screen scr)))
      (setf (cl-tmux/terminal/types:screen-mouse-mode scr) 1)
      ;; Release with fd=-1 returns NIL from %encode-mouse-for-pane (fd guard),
      ;; but the filter for mode=1 should reject release events before fd-check.
      (expect (cl-tmux::%try-mouse-passthrough nil pane 0 5 3 t) :to-be-falsy)))

  ;; In button-event mode (mode 2), release events ARE forwarded.
  (it "mouse-passthrough-mode2-forwards-release"
    (with-pipe-fds (rfd wfd)
      (let* ((scr  (make-screen 40 24))
             (pane (make-pane :id 1 :fd wfd :pid -1
                              :x 0 :y 0 :width 40 :height 24
                              :screen scr)))
        (setf (cl-tmux/terminal/types:screen-mouse-mode scr) 2)
        (let ((result (cl-tmux::%try-mouse-passthrough nil pane 0 5 3 t)))
          (expect (eq result t)))
        (expect (cl-tmux/pty:select-fds (list rfd) 20000) :to-be-truthy)))))
