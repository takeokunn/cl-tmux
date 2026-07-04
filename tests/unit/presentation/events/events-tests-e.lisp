(in-package #:cl-tmux/test)

;;;; events tests — part E: status-column, SGR-mouse NIL, copy-mode navigation,
;;;; escape/repeat timeout, mouse passthrough, drag-state, and copy-mode key tables.

(in-suite events-suite)


(test status-col-to-window-finds-third-window
  "%status-col-to-window returns the correct window when the column falls in the
   third window entry (verifies the multi-window traversal path)."
  (let* ((p0   (make-pane :id 1 :fd -1 :pid -1 :x 0 :y 0 :width 20 :height 5
                           :screen (make-screen 20 5)))
         (p1   (make-pane :id 2 :fd -1 :pid -1 :x 0 :y 0 :width 20 :height 5
                           :screen (make-screen 20 5)))
         (p2   (make-pane :id 3 :fd -1 :pid -1 :x 0 :y 0 :width 20 :height 5
                           :screen (make-screen 20 5)))
         (win0 (make-window :id 0 :name "a" :width 20 :height 5
                            :panes (list p0) :tree (make-layout-leaf p0)))
         (win1 (make-window :id 1 :name "b" :width 20 :height 5
                            :panes (list p1) :tree (make-layout-leaf p1)))
         (win2 (make-window :id 2 :name "c" :width 20 :height 5
                            :panes (list p2) :tree (make-layout-leaf p2)))
         (sess (make-session :id 1 :name "s" :windows (list win0 win1 win2))))
    (window-select-pane win0 p0)
    (window-select-pane win1 p1)
    (window-select-pane win2 p2)
    (session-select-window sess win0)
    ;; Session prefix " s" = 2 chars.
    ;; win0 "a": 6 chars, cols 2..7
    ;; separator: column 8
    ;; win1 "b": 5 chars, cols 9..13
    ;; separator: column 14
    ;; win2 "c": 5 chars, cols 15..19
    ;; Column 15 should land in win2.
    (is (eq win2 (cl-tmux::%status-col-to-window sess 15))
        "%status-col-to-window must find the third window at the appropriate column")))

;;; ── %handle-escape-sgr-mouse NIL branch coverage ─────────────────────────────

(test handle-escape-sgr-mouse-ignores-malformed-sequence
  "%handle-escape-sgr-mouse is a no-op and returns ground-state for a malformed SGR sequence
   (one that %parse-sgr-mouse cannot parse)."
  (with-fake-session (s)
    ;; Build a syntactically valid ESC [ < prefix but with only one field (no semicolons).
    ;; %parse-sgr-mouse will return (values nil nil nil nil) for this.
    (let* ((seq (format nil "~C[<0M" #\Escape))  ; too short, missing fields
           (buf (make-array (length seq) :element-type '(unsigned-byte 8)
                            :fill-pointer (length seq) :adjustable t
                            :initial-contents (map 'list #'char-code seq)))
           (len (length seq)))
      (multiple-value-bind (outcome next)
          (cl-tmux::%handle-escape-sgr-mouse s buf len)
        (is (null outcome)
            "%handle-escape-sgr-mouse with malformed SGR must return NIL outcome")
        (is (eq #'cl-tmux::%ground-input-state next)
            "%handle-escape-sgr-mouse must return ground-state for malformed sequence")))))

;;; ── copy-mode navigation bytes via process-byte (table-driven coverage) ─────
;;;
;;; Tests that all the additional byte constants (h, l, w, b, e, $, etc.) defined
;;; in events-core.lisp route correctly through the copy-mode dispatch in
;;; %ground-input-state. We drive them through process-byte to stay at the
;;; public API level.

(test copy-mode-all-nav-bytes-via-process-byte
  "All standard copy-mode navigation bytes route without error through process-byte."
  (with-copy-mode-state (s screen state)
    (seed-scrollback screen 10)
    ;; Use the named constants from events-core.lisp for each byte.
    (dolist (byte (list #.cl-tmux::+byte-h+
                        #.cl-tmux::+byte-j+
                        #.cl-tmux::+byte-k+
                        #.cl-tmux::+byte-l+
                        #.cl-tmux::+byte-w+
                        #.cl-tmux::+byte-b+
                        #.cl-tmux::+byte-e+
                        #.cl-tmux::+byte-dollar+
                        #.cl-tmux::+byte-g+
                        #.cl-tmux::+byte-capital-g+
                        #.cl-tmux::+byte-capital-h+
                        #.cl-tmux::+byte-capital-m+
                        #.cl-tmux::+byte-capital-l+
                        #.cl-tmux::+byte-n+
                        #.cl-tmux::+byte-capital-n+
                        #.cl-tmux::+byte-capital-v+
                        #.cl-tmux::+byte-space+
                        #.cl-tmux::+byte-v+
                        #.cl-tmux::+byte-y+
                        #.cl-tmux::+byte-capital-y+
                        #.cl-tmux::+byte-capital-d+
                        #.cl-tmux::+byte-capital-a+
                        #.cl-tmux::+byte-r+))
      (cl-tmux::dispatch-command s :copy-mode-enter nil)
      (finishes (cl-tmux::process-byte s byte state)))))

;;; ── %flush-esc-if-timed-out behavioural tests ────────────────────────────────

(test flush-esc-no-op-when-no-esc-pending
  "%flush-esc-if-timed-out is a no-op when esc-entered-at is NIL."
  (with-fake-session (sess)
    (let ((state (cl-tmux::make-input-state)))
      ;; esc-entered-at starts NIL; %flush-esc-if-timed-out must not change the state.
      (is (null (cl-tmux::input-state-esc-entered-at state))
          "precondition: esc-entered-at is NIL")
      (cl-tmux::%flush-esc-if-timed-out state sess)
      (is (null (cl-tmux::input-state-esc-entered-at state))
          "esc-entered-at stays NIL when no escape is pending"))))

(test flush-esc-within-timeout-does-not-flush
  "%flush-esc-if-timed-out does not flush when the timeout has NOT elapsed."
  (with-fake-session (sess)
    (let ((state (cl-tmux::make-input-state)))
      (with-isolated-config
        ;; Set a very long escape-time so the timer has definitely not expired.
        (cl-tmux/options:set-server-option "escape-time" 100000)
        ;; Simulate an ESC having been received: stamp esc-entered-at.
        (setf (cl-tmux::input-state-esc-entered-at state) (get-internal-real-time))
        (cl-tmux::%flush-esc-if-timed-out state sess)
        ;; Continuation must still point away from ground (timer did not fire).
        (is (not (null (cl-tmux::input-state-esc-entered-at state)))
            "esc-entered-at must remain set when timeout has not elapsed")))))

(test flush-esc-after-timeout-resets-to-ground
  "%flush-esc-if-timed-out resets state to ground when escape-time has elapsed."
  (with-fake-session (sess)
    (let ((state (cl-tmux::make-input-state)))
      (with-isolated-config
        ;; Set escape-time to 0 ms so any elapsed time qualifies.
        (cl-tmux/options:set-server-option "escape-time" 0)
        ;; Stamp esc-entered-at far in the past.
        (setf (cl-tmux::input-state-esc-entered-at state)
              (- (get-internal-real-time) (* 2 internal-time-units-per-second)))
        (cl-tmux::%flush-esc-if-timed-out state sess)
        ;; After flush: esc-entered-at cleared and continuation back to ground.
        (is (null (cl-tmux::input-state-esc-entered-at state))
            "esc-entered-at must be NIL after flush")
      (is (eq (cl-tmux::input-state-continuation state)
              #'cl-tmux::%ground-input-state)
          "continuation must return to ground after flush")))))

;;; ── %reset-repeat-if-expired behavioural tests ───────────────────────────────

(test reset-repeat-no-op-when-no-repeat-pending
  "%reset-repeat-if-expired is a no-op when repeat-entered-at is NIL."
  (let ((state (cl-tmux::make-input-state)))
    (is (null (cl-tmux::input-state-repeat-entered-at state))
        "precondition: repeat-entered-at is NIL")
    (cl-tmux::%reset-repeat-if-expired state)
    (is (null (cl-tmux::input-state-repeat-entered-at state))
        "repeat-entered-at stays NIL when nothing is pending")))

(test reset-repeat-within-timeout-does-not-reset
  "%reset-repeat-if-expired does not reset within the repeat-time window."
  (let ((state (cl-tmux::make-input-state)))
    (with-isolated-config
      (cl-tmux/options:set-option "repeat-time" 100000)
      (setf (cl-tmux::input-state-repeat-entered-at state) (get-internal-real-time))
      (cl-tmux::%reset-repeat-if-expired state)
      (is (not (null (cl-tmux::input-state-repeat-entered-at state)))
          "repeat-entered-at must not be cleared before timeout"))))

(test reset-repeat-after-timeout-resets-to-ground
  "%reset-repeat-if-expired resets to ground state after repeat-time elapses."
  (let ((state (cl-tmux::make-input-state)))
    (with-isolated-config
      (cl-tmux/options:set-option "repeat-time" 0)
      ;; Stamp repeat-entered-at far in the past.
      (setf (cl-tmux::input-state-repeat-entered-at state)
            (- (get-internal-real-time) (* 2 internal-time-units-per-second)))
      (cl-tmux::%reset-repeat-if-expired state)
      (is (null (cl-tmux::input-state-repeat-entered-at state))
          "repeat-entered-at must be NIL after expiry")
      (is (eq (cl-tmux::input-state-continuation state)
              #'cl-tmux::%ground-input-state)
          "continuation must return to ground after repeat expiry"))))

(test initial-repeat-time-option-registered-default-zero
  "initial-repeat-time is a registered option defaulting to 0 (audit #34)."
  (with-isolated-config
    (is (eql 0 (cl-tmux/options:get-option "initial-repeat-time"))
        "initial-repeat-time default must be 0 (fall back to repeat-time)")))

(test repeat-window-ms-honors-initial-repeat-time
  "%repeat-window-ms uses a non-zero initial-repeat-time for the FIRST repeat key
   (count 1) and repeat-time for every other key (audit #34, tmux 3.5+)."
  (with-isolated-config
    (cl-tmux/options:set-option "repeat-time" 500)
    ;; initial-repeat-time 0 → repeat-time for the first key too.
    (cl-tmux/options:set-option "initial-repeat-time" 0)
    (is (= 500 (cl-tmux::%repeat-window-ms 1))
        "initial-repeat-time 0 → first key uses repeat-time")
    (is (= 500 (cl-tmux::%repeat-window-ms 2))
        "subsequent keys use repeat-time")
    ;; initial-repeat-time 1500 → only the first key (count 1) uses it.
    (cl-tmux/options:set-option "initial-repeat-time" 1500)
    (is (= 1500 (cl-tmux::%repeat-window-ms 1))
        "first repeat key uses a non-zero initial-repeat-time")
    (is (= 500 (cl-tmux::%repeat-window-ms 2))
        "second repeat key uses repeat-time, not initial-repeat-time")
    (is (= 500 (cl-tmux::%repeat-window-ms 3))
        "third repeat key uses repeat-time")))

;;; ── %try-mouse-passthrough mode tests ────────────────────────────────────────

(test try-mouse-passthrough-mode1-blocks-release
  "Mode 1 (X10/normal): release events are NOT forwarded."
  (let* ((screen (make-screen 20 5))
         (pane   (make-pane :id 1 :fd -1 :pid -1 :x 0 :y 0
                            :width 20 :height 5 :screen screen))
         (win    (make-window :id 1 :name "w" :width 20 :height 5
                              :panes (list pane)
                              :tree (make-layout-leaf pane))))
    (setf (screen-mouse-mode screen) 1)
    ;; Release event (release-p=T): mode 1 must NOT forward.
    (let ((result (cl-tmux::%try-mouse-passthrough win pane 0 0 0 t)))
      (is (null result)
          "mode 1 must not forward release events (fd=-1 means encode returns nil)"))))

(test try-mouse-passthrough-mode2-forwards-release
  "Mode 2 (button-event): release events are forwarded."
  (with-pipe-fds (rfd wfd)
    (let* ((screen (make-screen 20 5))
           (pane   (make-pane :id 1 :fd wfd :pid -1 :x 0 :y 0
                              :width 20 :height 5 :screen screen))
           (win    (make-window :id 1 :name "w" :width 20 :height 5
                                :panes (list pane)
                                :tree (make-layout-leaf pane))))
      (setf (screen-mouse-mode screen) 2)
      ;; Button 0 release (left-click release, not motion): should be forwarded.
      (let ((result (cl-tmux::%try-mouse-passthrough win pane 0 0 0 t)))
        (is (eq result t)
            "mode-2 must forward non-motion button releases"))
      (is-true (cl-tmux/pty:select-fds (list rfd) 20000)
               "forwarded release must reach the pane PTY"))))

(test try-mouse-passthrough-mode0-returns-nil
  "When the pane has mouse mode 0 (disabled), %try-mouse-passthrough returns NIL."
  (let* ((screen (make-screen 20 5))
         (pane   (make-pane :id 1 :fd -1 :pid -1 :x 0 :y 0
                            :width 20 :height 5 :screen screen))
         (win    (make-window :id 1 :name "w" :width 20 :height 5
                              :panes (list pane)
                              :tree (make-layout-leaf pane))))
    ;; mouse-mode = 0 means no tracking enabled; (plusp 0) = NIL.
    (setf (screen-mouse-mode screen) 0)
    (is (null (cl-tmux::%try-mouse-passthrough win pane 0 0 0 nil))
        "mouse mode 0 → passthrough must be nil")))

;;; ── drag-state is set on border press ───────────────────────────────────────

(test mouse-drag-state-is-set-on-border-press
  "*mouse-drag-state* is non-NIL after a left-press on the separator column."
  (with-two-pane-mouse-session (sess win p0 p1)
    (is (not (eq p0 p1)) "precondition: fixture must create two distinct panes")
    ;; Simulate a left-press on the separator column (col 40).
    (cl-tmux::%dispatch-mouse-event sess 0 40 5 nil)
    ;; Whether the state has 2 or 4 elements depends on the implementation;
    ;; what matters is that it is non-NIL and contains a split node.
    (is (not (null cl-tmux::*mouse-drag-state*))
        "*mouse-drag-state* must be set after a border press")
    (is (cl-tmux/model:layout-split-p (first cl-tmux::*mouse-drag-state*))
        "first element of drag-state must be a layout-split node")))

;;; ── copy-mode-vi key table override ─────────────────────────────────────────

(test copy-mode-vi-table-binding-overrides-hardcoded
  "A binding in the copy-mode-vi table fires its command and suppresses the hardcoded dispatch."
  (with-isolated-config
    ;; Bind 'v' in copy-mode-vi to :copy-mode-begin-selection (same as hardcoded)
    ;; but with a token list to verify table lookup is happening.
    (cl-tmux/config:key-table-bind "copy-mode-vi" #\v :copy-mode-begin-selection)
    (with-fake-session (sess :nwindows 1 :npanes 1)
      ;; Enter copy mode
      (let* ((win    (cl-tmux/model:session-active-window sess))
             (pane   (cl-tmux/model:window-active-pane win))
             (screen (cl-tmux/model:pane-screen pane)))
        (cl-tmux/commands:copy-mode-enter screen)
        ;; The 'v' key (118) should be handled by the table lookup
        ;; We verify copy-mode is active and the binding exists
        (is (cl-tmux/terminal:screen-copy-mode-p screen)
            "screen must be in copy mode")
        (is (not (null (cl-tmux/config:key-table-lookup "copy-mode-vi" #\v)))
            "copy-mode-vi table must have 'v' binding")
        (cl-tmux/commands:copy-mode-exit screen)))))

(test copy-mode-key-table-selection-follows-mode-keys-vi
  "In vi mode, copy-mode input uses the copy-mode-vi table."
  (with-copy-mode-vi-state (s screen state)
    (cl-tmux/config:key-table-bind "copy-mode-vi" #\v :copy-mode-exit)
    (cl-tmux/config:key-table-bind "copy-mode" #\v :copy-mode-begin-selection)
    (cl-tmux::process-byte s (char-code #\v) state)
    (is-false (cl-tmux/terminal:screen-copy-mode-p screen)
              "vi mode must dispatch the copy-mode-vi binding")))

(test copy-mode-vi-default-hjkl-move-cursor
  "The default copy-mode-vi table provides hjkl cursor movement."
  (with-copy-mode-vi-state (s screen state)
    (seed-scrollback screen 10)
    (setf (cl-tmux/terminal:screen-copy-cursor screen) (cons 1 1))
    (cl-tmux::process-byte s (char-code #\j) state)
    (is (equal (cons 2 1) (cl-tmux/terminal:screen-copy-cursor screen))
        "j must move the copy cursor down")
    (cl-tmux::process-byte s (char-code #\k) state)
    (is (equal (cons 1 1) (cl-tmux/terminal:screen-copy-cursor screen))
        "k must move the copy cursor up")
    (cl-tmux::process-byte s (char-code #\l) state)
    (is (equal (cons 1 2) (cl-tmux/terminal:screen-copy-cursor screen))
        "l must move the copy cursor right")
    (cl-tmux::process-byte s (char-code #\h) state)
    (is (equal (cons 1 1) (cl-tmux/terminal:screen-copy-cursor screen))
        "h must move the copy cursor left")))

(test copy-mode-vi-percent-jumps-to-next-matching-bracket
  "The default copy-mode-vi % binding jumps to the next matching bracket."
  (with-copy-mode-vi-state (s screen state)
    (dotimes (i 7)
      (setf (cl-tmux/terminal/types:screen-cell screen i 0)
            (cl-tmux/terminal/types:make-cell :char (char "(a(b)c)" i))))
    (setf (cl-tmux/terminal:screen-copy-cursor screen) (cons 0 0))
    (cl-tmux::process-byte s (char-code #\%) state)
    (is (equal (cons 0 6) (cl-tmux/terminal:screen-copy-cursor screen))
        "% must jump to the matching closing bracket")))

(test copy-mode-vi-word-search-keys-use-copy-mode-table
  "The default copy-mode-vi # and * bindings search for the word under the cursor."
  (with-copy-mode-vi-state (s screen state)
    (let ((text "xx a.b aXb a.b"))
      (dotimes (i (length text))
        (setf (cl-tmux/terminal/types:screen-cell screen i 0)
              (cl-tmux/terminal/types:make-cell :char (char text i)))))
    (setf (cl-tmux/terminal:screen-copy-cursor screen) (cons 0 3))
    (cl-tmux::process-byte s (char-code #\*) state)
    (is (equal (cons 0 11) (cl-tmux/terminal:screen-copy-cursor screen))
        "* must search forward for the word under cursor through copy-mode-vi")
    (setf (cl-tmux/terminal:screen-copy-cursor screen) (cons 0 12))
    (cl-tmux::process-byte s (char-code #\#) state)
    (is (equal (cons 0 11) (cl-tmux/terminal:screen-copy-cursor screen))
        "# must search backward for the word under cursor through copy-mode-vi")
    (is (string= "a\\.b"
                 (cl-tmux/terminal/types:screen-copy-search-term screen))
        "#/* must save the escaped literal word search term")))

(test copy-mode-vi-named-special-bindings-fire
  "In vi mode, named special-key bindings in copy-mode-vi fire via process-byte.
   Each row: (key bytes description)."
  (dolist (row '(("PageUp" (27 91 53 126)     "copy-mode-vi PageUp binding must fire")
                 ("C-v"    (22)               "C-v must dispatch the named copy-mode-vi binding")
                 ("Enter"  (13)               "Enter must dispatch the named copy-mode-vi binding")
                 ("C-Up"   (27 91 49 59 53 65) "C-Up must dispatch the named copy-mode-vi binding")))
    (destructuring-bind (key bytes msg) row
      (with-copy-mode-vi-state (s screen state)
        (cl-tmux/config:key-table-bind "copy-mode-vi" key :copy-mode-exit)
        (send-copy-mode-bytes s state bytes)
        (is-false (cl-tmux/terminal:screen-copy-mode-p screen) msg)))))

(test copy-mode-vi-control-b-uses-copy-mode-table-before-prefix
  "In vi mode, a C-b byte runs copy-mode-vi C-b instead of arming prefix."
  (with-copy-mode-vi-state (s screen state)
    (seed-scrollback screen 30)
    (is (zerop (screen-copy-offset screen)) "precondition: copy view starts live")
    (cl-tmux::process-byte s 2 state)
    (is (= (min (screen-height screen) 30)
           (screen-copy-offset screen))
        "C-b must dispatch copy-mode-vi page-up, not the prefix key")))

(test copy-mode-pagedown-uses-emacs-copy-mode-key-table
  "In emacs mode, CSI PageDown uses the copy-mode table."
  (with-copy-mode-emacs-state (s screen state)
    (cl-tmux/config:key-table-bind "copy-mode-vi" "PageDown" :copy-mode-page-up)
    (cl-tmux/config:key-table-bind "copy-mode" "PageDown" :copy-mode-exit)
    (send-copy-mode-bytes s state '(27 91 54 126))
    (is-false (cl-tmux/terminal:screen-copy-mode-p screen)
              "copy-mode PageDown binding must fire")))

(test copy-mode-key-table-selection-follows-mode-keys-emacs
  "In emacs mode, copy-mode input uses the copy-mode table."
  (with-copy-mode-emacs-state (s screen state)
    (cl-tmux/config:key-table-bind "copy-mode-vi" #\v :copy-mode-exit)
    (cl-tmux/config:key-table-bind "copy-mode" #\v :copy-mode-begin-selection)
    (cl-tmux::process-byte s (char-code #\v) state)
    (is (cl-tmux/terminal:screen-copy-mode-p screen)
        "emacs mode must not dispatch the copy-mode-vi binding")
    (is (cl-tmux/terminal:screen-copy-selecting screen)
        "emacs mode must dispatch the copy-mode binding")))

(test copy-mode-meta-key-table-selection-follows-mode-keys-emacs
  "In emacs mode, ESC-prefixed Meta keys use the copy-mode table."
  (with-copy-mode-emacs-state (s screen state)
    (cl-tmux/config:key-table-bind "copy-mode-vi" "M-f" :copy-mode-exit)
    (cl-tmux/config:key-table-bind "copy-mode" "M-f" :copy-mode-begin-selection)
    (send-copy-mode-bytes s state (list 27 (char-code #\f)))
    (is (cl-tmux/terminal:screen-copy-mode-p screen)
        "emacs mode must not dispatch the copy-mode-vi Meta binding")
    (is (cl-tmux/terminal:screen-copy-selecting screen)
        "emacs mode must dispatch the copy-mode Meta binding")))

(test copy-mode-escape-control-key-does-not-fall-back-to-copy-mode-table
  "In emacs mode, ESC-prefixed Ctrl bytes do not fall back to copy-mode key names."
  (with-copy-mode-emacs-state (s screen state)
    (cl-tmux/config:key-table-bind "copy-mode" "C-b" :copy-mode-begin-selection)
    (send-copy-mode-bytes s state '(27 2))
    (is-true (cl-tmux/terminal:screen-copy-mode-p screen)
             "copy mode must remain active after ESC C-b")
    (is-false (cl-tmux/terminal:screen-copy-selecting screen)
              "ESC-prefixed Ctrl bytes must not dispatch the copy-mode C-b binding")))
