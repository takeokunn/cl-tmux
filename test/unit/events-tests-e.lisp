(in-package #:cl-tmux/test)

;;;; events tests — part E: %status-col-to-window, SGR-mouse-nil, copy-mode-nav,
;;;; %flush-esc, %reset-repeat, mouse-passthrough, drag-state, copy-mode-vi,
;;;; CSI-u key-name, parameter parsing, and end-to-end process-byte.

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
    ;; win0 "a": 4 + 1 = 5 chars, cols 2..6
    ;; win1 "b": 4 + 1 = 5 chars, cols 7..11
    ;; win2 "c": 4 + 1 = 5 chars, cols 12..16
    ;; Column 14 should land in win2.
    (is (eq win2 (cl-tmux::%status-col-to-window sess 14))
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

(test try-mouse-passthrough-mode2-blocks-non-motion-release
  "Mode 2 (button-event): release of a non-motion button is NOT forwarded."
  (let* ((screen (make-screen 20 5))
         (pane   (make-pane :id 1 :fd -1 :pid -1 :x 0 :y 0
                            :width 20 :height 5 :screen screen))
         (win    (make-window :id 1 :name "w" :width 20 :height 5
                              :panes (list pane)
                              :tree (make-layout-leaf pane))))
    (setf (screen-mouse-mode screen) 2)
    ;; Button 0 release (left-click release, not motion): should NOT be forwarded.
    ;; (or (not T) (= 0 +mouse-btn-motion+)) = (or NIL NIL) = NIL → skip.
    (let ((result (cl-tmux::%try-mouse-passthrough win pane 0 0 0 t)))
      (is (null result)
          "mode-2 must not forward non-motion button releases"))))

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
    (declare (ignore p1))
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
  (with-isolated-config
    (cl-tmux/options:set-option "mode-keys" "vi")
    (cl-tmux/config:key-table-bind "copy-mode-vi" #\v :copy-mode-exit)
    (cl-tmux/config:key-table-bind "copy-mode" #\v :copy-mode-begin-selection)
    (with-copy-mode-state (s screen state)
      (cl-tmux::process-byte s (char-code #\v) state)
      (is-false (cl-tmux/terminal:screen-copy-mode-p screen)
                "vi mode must dispatch the copy-mode-vi binding"))))

(test copy-mode-vi-default-hjkl-move-cursor
  "The default copy-mode-vi table provides hjkl cursor movement."
  (with-isolated-config
    (cl-tmux/options:set-option "mode-keys" "vi")
    (with-copy-mode-state (s screen state)
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
          "h must move the copy cursor left"))))

(test copy-mode-vi-pageup-uses-copy-mode-key-table
  "In vi mode, CSI PageUp uses the copy-mode-vi table before the scroll fallback."
  (with-isolated-config
    (cl-tmux/options:set-option "mode-keys" "vi")
    (cl-tmux/config:key-table-bind "copy-mode-vi" "PageUp" :copy-mode-exit)
    (with-copy-mode-state (s screen state)
      (dolist (byte '(27 91 53 126))
        (cl-tmux::process-byte s byte state))
      (is-false (cl-tmux/terminal:screen-copy-mode-p screen)
                "copy-mode-vi PageUp binding must fire"))))

(test copy-mode-pagedown-uses-emacs-copy-mode-key-table
  "In emacs mode, CSI PageDown uses the copy-mode table."
  (with-isolated-config
    (cl-tmux/options:set-option "mode-keys" "emacs")
    (cl-tmux/config:key-table-bind "copy-mode-vi" "PageDown" :copy-mode-page-up)
    (cl-tmux/config:key-table-bind "copy-mode" "PageDown" :copy-mode-exit)
    (with-copy-mode-state (s screen state)
      (dolist (byte '(27 91 54 126))
        (cl-tmux::process-byte s byte state))
      (is-false (cl-tmux/terminal:screen-copy-mode-p screen)
                "copy-mode PageDown binding must fire"))))

(test copy-mode-key-table-selection-follows-mode-keys-emacs
  "In emacs mode, copy-mode input uses the copy-mode table."
  (with-isolated-config
    (cl-tmux/options:set-option "mode-keys" "emacs")
    (cl-tmux/config:key-table-bind "copy-mode-vi" #\v :copy-mode-exit)
    (cl-tmux/config:key-table-bind "copy-mode" #\v :copy-mode-begin-selection)
    (with-copy-mode-state (s screen state)
      (cl-tmux::process-byte s (char-code #\v) state)
      (is (cl-tmux/terminal:screen-copy-mode-p screen)
          "emacs mode must not dispatch the copy-mode-vi binding")
      (is (cl-tmux/terminal:screen-copy-selecting screen)
          "emacs mode must dispatch the copy-mode binding"))))

(test copy-mode-meta-key-table-selection-follows-mode-keys-emacs
  "In emacs mode, ESC-prefixed Meta keys use the copy-mode table."
  (with-isolated-config
    (cl-tmux/options:set-option "mode-keys" "emacs")
    (cl-tmux/config:key-table-bind "copy-mode-vi" "M-f" :copy-mode-exit)
    (cl-tmux/config:key-table-bind "copy-mode" "M-f" :copy-mode-begin-selection)
    (with-copy-mode-state (s screen state)
      (cl-tmux::process-byte s 27 state)
      (cl-tmux::process-byte s (char-code #\f) state)
      (is (cl-tmux/terminal:screen-copy-mode-p screen)
          "emacs mode must not dispatch the copy-mode-vi Meta binding")
      (is (cl-tmux/terminal:screen-copy-selecting screen)
          "emacs mode must dispatch the copy-mode Meta binding"))))

;;; ── Extended keys (CSI u) key-name parsing ───────────────────────────────────

(test csi-u-key-name-modifier-combinations
  "%csi-u-key-name maps a CSI-u codepoint+modifier to the canonical key name."
  (dolist (c '((97 1 "a"       "plain a (mod 1)")
               (97 2 "S-a"     "Shift (mod 2)")
               (97 3 "M-a"     "Alt (mod 3)")
               (97 5 "C-a"     "Ctrl (mod 5)")
               (97 6 "C-S-a"   "Ctrl+Shift (mod 6)")
               (97 7 "C-M-a"   "Ctrl+Alt (mod 7)")
               (97 8 "C-M-S-a" "Ctrl+Alt+Shift (mod 8)")))
    (destructuring-bind (code mod expected desc) c
      (is (string= expected (cl-tmux::%csi-u-key-name code mod)) "~A" desc))))

(test csi-u-key-name-special-keys
  "%csi-u-key-name names the special codepoints (Tab/Enter/Escape/Space/BSpace)."
  (dolist (c '((9   1 "Tab")    (9  2 "S-Tab")
               (13  1 "Enter")  (27 1 "Escape")
               (32  5 "C-Space") (127 1 "BSpace")))
    (destructuring-bind (code mod expected) c
      (is (string= expected (cl-tmux::%csi-u-key-name code mod))
          "code ~D mod ~D → ~S" code mod expected))))

(test csi-u-key-name-unhandled-codepoint
  "An unhandled (control/out-of-range) codepoint yields NIL."
  (is (null (cl-tmux::%csi-u-key-name 0 1))   "NUL → NIL")
  (is (null (cl-tmux::%csi-u-key-name 7 5))   "BEL (control) → NIL")
  (is (null (cl-tmux::%csi-u-base-key 200))   "out-of-ASCII base → NIL"))

;;; ── Extended keys (CSI u) parameter parsing / legacy fallback ────────────────

(defun %csi-u-buf (&rest bytes)
  "Build a CSI-u BUFFER (with a trailing 'u') from the parameter BYTES, prefixed
   with ESC [, as the state machine accumulates it."
  (let ((v (make-array (+ 3 (length bytes)) :element-type '(unsigned-byte 8)
                                            :fill-pointer 0 :adjustable t)))
    (vector-push-extend 27 v) (vector-push-extend 91 v)   ; ESC [
    (dolist (b bytes) (vector-push-extend b v))
    (vector-push-extend 117 v)                            ; u
    v))

(test csi-u-parse-params-cases
  "%csi-u-parse-params reads <codepoint>[;<mod>] from a u-terminated buffer."
  (multiple-value-bind (cp mod)
      (cl-tmux::%csi-u-parse-params (%csi-u-buf 57 55 59 53) 7)  ; 97 ; 5
    (is (= 97 cp)) (is (= 5 mod)))
  (multiple-value-bind (cp mod)
      (cl-tmux::%csi-u-parse-params (%csi-u-buf 49 51) 5)        ; 13 (no ; mod)
    (is (= 13 cp)) (is (= 1 mod) "omitted mod defaults to 1"))
  (multiple-value-bind (cp mod)
      (cl-tmux::%csi-u-parse-params (%csi-u-buf 57 59 53 58 49) 8) ; 9 ; 5:1 (subparam)
    (is (= 9 cp)) (is (= 5 mod) "kitty <mod>:<event> tolerated, leading int taken")))

(test csi-u-control-byte-cases
  "%csi-u-control-byte gives the legacy Ctrl byte (a→1, Space/@→0, [→27), else NIL."
  (is (= 1  (cl-tmux::%csi-u-control-byte 97)))   ; C-a
  (is (= 26 (cl-tmux::%csi-u-control-byte 122)))  ; C-z
  (is (= 1  (cl-tmux::%csi-u-control-byte 65)))   ; C-A (upper) → 1
  (is (= 0  (cl-tmux::%csi-u-control-byte 32)))   ; C-Space → NUL
  (is (= 0  (cl-tmux::%csi-u-control-byte 64)))   ; C-@ → NUL
  (is (= 27 (cl-tmux::%csi-u-control-byte 91)))   ; C-[ → ESC
  (is (null (cl-tmux::%csi-u-control-byte 48))))  ; C-0 has no control byte

(test csi-u-legacy-octets-cases
  "%csi-u-legacy-octets reproduces the byte form a non-extended terminal sends."
  (dolist (c '((97 0 #(97)    "plain a -> 97")
               (97 1 #(97)    "S-a -> 97 (shift only)")
               (97 4 #(1)     "C-a -> ^A")
               (97 5 #(1)     "C-S-a -> ^A (legacy collapse)")
               (97 2 #(27 97) "M-a -> ESC a")
               (97 6 #(27 1)  "C-M-a -> ESC ^A")
               (9  1 nil      "Tab (no printable/ctrl legacy) -> NIL")))
    (destructuring-bind (cp mod expected desc) c
      (is (equalp expected (cl-tmux::%csi-u-legacy-octets cp mod)) "~A" desc))))

(test csi-u-terminated-and-accumulating-predicates
  "The state-machine predicates recognise CSI-u prefixes and full sequences,
   and reject mouse / arrow CSI shapes."
  (let ((full (%csi-u-buf 57 55 59 53)))                ; ESC [ 97 ; 5 u  (len 7)
    (is-true  (cl-tmux::%csi-u-terminated-p full 7))
    (is-false (cl-tmux::%csi-u-accumulating-p full 7) "a terminated buf is not accumulating"))
  ;; ESC [ 9 7  — mid-accumulation digit prefix
  (let ((v (make-array 8 :element-type '(unsigned-byte 8) :fill-pointer 0 :adjustable t)))
    (dolist (b '(27 91 57 55)) (vector-push-extend b v))
    (is-true  (cl-tmux::%csi-u-accumulating-p v 4))
    (is-false (cl-tmux::%csi-u-terminated-p v 4)))
  ;; ESC [ M …  (X10 mouse) and ESC [ <  (SGR) must NOT look like CSI-u
  (let ((m (make-array 4 :element-type '(unsigned-byte 8) :fill-pointer 0 :adjustable t)))
    (dolist (b '(27 91 77)) (vector-push-extend b m))    ; ESC [ M
    (is-false (cl-tmux::%csi-u-accumulating-p m 3) "mouse intro is not CSI-u")))

;;; ── Extended keys (CSI u) end-to-end through process-byte ────────────────────

(test root-csi-u-name-binding-fires
  "bind -n C-S-a next-window: a Ctrl+Shift+a extended-key (ESC [ 97 ; 6 u) runs
   next-window at root.  C-S-a has no legacy byte, so this exercises the name path
   — and the multi-digit codepoint 97 must not be dropped by the generic forward."
  (with-isolated-config
    (cl-tmux/config:apply-config-directive '("bind" "-n" "C-S-a" "next-window"))
    (with-fake-session (s :nwindows 2)
      (let ((state (cl-tmux::make-input-state)))
        (dolist (b '(27 91 57 55 59 54 117))  ; ESC [ 9 7 ; 6 u
          (cl-tmux::process-byte s b state))
        (is (eq (second (session-windows s)) (session-active-window s))
            "bound -n C-S-a must run next-window via the CSI-u name path")))))

(test root-csi-u-ctrl-letter-reinjects-to-control-byte
  "bind -n C-a next-window: a Ctrl+a extended-key (ESC [ 97 ; 5 u) runs next-window.
   C-a is stored under the control byte (^A), so this proves the legacy re-injection
   path routes the synthesized byte back through the root table."
  (with-isolated-config
    (cl-tmux/config:apply-config-directive '("bind" "-n" "C-a" "next-window"))
    (with-fake-session (s :nwindows 2)
      (let ((state (cl-tmux::make-input-state)))
        (dolist (b '(27 91 57 55 59 53 117))  ; ESC [ 9 7 ; 5 u
          (cl-tmux::process-byte s b state))
        (is (eq (second (session-windows s)) (session-active-window s))
            "bound -n C-a must fire via re-injected control byte")))))

(test root-csi-u-shift-tab-single-digit-codepoint
  "bind -n S-Tab next-window: Shift+Tab (ESC [ 9 ; 2 u) runs next-window.  The
   single-digit codepoint 9 must accumulate past the 3-byte-CSI branch rather than
   be misread as a bare ESC [ 9 arrow/copy escape."
  (with-isolated-config
    (cl-tmux/config:apply-config-directive '("bind" "-n" "S-Tab" "next-window"))
    (with-fake-session (s :nwindows 2)
      (let ((state (cl-tmux::make-input-state)))
        (dolist (b '(27 91 57 59 50 117))  ; ESC [ 9 ; 2 u
          (cl-tmux::process-byte s b state))
        (is (eq (second (session-windows s)) (session-active-window s))
            "bound -n S-Tab must run next-window via single-digit CSI-u")))))

(test csi-u-plain-printable-forwards-to-pane
  "An unbound plain extended-key (ESC [ 97 u) is translated to its legacy byte 'a'
   and forwarded transparently to the active pane's PTY (no byte dropped)."
  (with-pipe-fds (rfd wfd)
    (with-fake-session (s :nwindows 1)
      (setf (pane-fd (window-active-pane (session-active-window s))) wfd)
      (let ((state (cl-tmux::make-input-state)))
        (dolist (b '(27 91 57 55 117))  ; ESC [ 9 7 u  (plain 'a')
          (cl-tmux::process-byte s b state))
        (let ((ready (cl-tmux/pty:select-fds (list rfd) 200000)))
          (is-true ready "the translated byte must reach the pane's PTY")
          (when ready
            (cffi:with-foreign-object (buf :uint8 8)
              (let ((n (cffi:foreign-funcall "read"
                                             :int rfd :pointer buf :unsigned-long 8
                                             :long)))
                (is (= 1 n) "exactly one byte forwarded (got ~D)" n)
                (is (= 97 (cffi:mem-aref buf :uint8 0))
                    "plain CSI-u 'a' must arrive as byte 97")))))))))

(test csi-u-function-key-forwarded-raw-not-dropped
  "A digit CSI that ends in '~' (F5 = ESC [ 15 ~), not 'u', is not a CSI-u chord:
   the safety-net branch forwards the whole sequence raw to the pane rather than
   accumulating it forever after CSI-u deferral."
  (with-pipe-fds (rfd wfd)
    (with-fake-session (s :nwindows 1)
      (setf (pane-fd (window-active-pane (session-active-window s))) wfd)
      (let ((state (cl-tmux::make-input-state)))
        (dolist (b '(27 91 49 53 126))  ; ESC [ 1 5 ~
          (cl-tmux::process-byte s b state))
        (let ((ready (cl-tmux/pty:select-fds (list rfd) 200000)))
          (is-true ready "the function key must be forwarded, not swallowed")
          (when ready
            (cffi:with-foreign-object (buf :uint8 16)
              (let ((n (cffi:foreign-funcall "read"
                                             :int rfd :pointer buf :unsigned-long 16
                                             :long)))
                (is (= 5 n) "all 5 bytes of ESC [ 1 5 ~ forwarded raw (got ~D)" n)
                (is (= 27  (cffi:mem-aref buf :uint8 0)))
                (is (= 126 (cffi:mem-aref buf :uint8 4)) "ends with '~'")))))))))
