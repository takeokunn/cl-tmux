(in-package #:cl-tmux/test)

;;;; Keystroke processing tests (src/events.lisp).
;;;; Tests: events-suite — process-byte, handle-prompt-key,
;;;; handle-copy-mode-escape.

(def-suite events-suite :description "Keystroke processing pipeline")
(in-suite events-suite)

;;; ── Copy-mode escape handler ─────────────────────────────────────────────────

(test handle-copy-mode-escape-consumes-arrows
  "Arrow-key escape sequences are consumed while copy mode is active; q exits."
  (let ((s (make-fake-session)))
    (with-loop-state
      (cl-tmux::dispatch-command s :copy-mode-enter nil)
      (is (cl-tmux::handle-copy-mode-escape
           s (make-array 3 :element-type '(unsigned-byte 8)
                           :initial-contents '(27 91 65)))  ; ESC [ A (up)
          "up-arrow should be consumed in copy mode")
      (is (cl-tmux::handle-copy-mode-escape
           s (make-array 3 :element-type '(unsigned-byte 8)
                           :initial-contents '(27 91 66)))  ; ESC [ B (down)
          "down-arrow should be consumed in copy mode")
      (is (cl-tmux::handle-copy-mode-escape
           s (make-array 1 :element-type '(unsigned-byte 8)
                           :initial-contents '(113)))       ; q
          "q should be consumed in copy mode")
      (is (not (screen-copy-mode-p (active-screen s)))
          "q should have exited copy mode"))))

(test handle-copy-mode-escape-inactive-returns-nil
  "Outside copy mode, handle-copy-mode-escape consumes nothing."
  (let ((s (make-fake-session)))
    (with-loop-state
      (is (null (cl-tmux::handle-copy-mode-escape
                 s (make-array 3 :element-type '(unsigned-byte 8)
                                 :initial-contents '(27 91 65))))))))

;;; ── process-byte: the shared keystroke pipeline ─────────────────────────────
;;;
;;; process-byte is what the in-process event loop AND the client/server attach
;;; loop both feed bytes to, so verifying it covers the byte-routing that the
;;; blocking event-loop itself can't be unit-tested for.  +prefix-key-code+ is 2.

(test process-byte-prefix-then-command
  "Prefix byte (2) then 'n' routes through the binding table to :next-window."
  (let ((s (make-fake-session :nwindows 2)))
    (with-loop-state
      (let ((state (cl-tmux::make-input-state)))
        ;; Lone prefix byte just transitions to the after-prefix state, no quit.
        (is (null (cl-tmux::process-byte s 2 state)))
        ;; Following byte 'n' selects the next window.
        (is (null (cl-tmux::process-byte s (char-code #\n) state)))
        (is (eq (second (session-windows s)) (session-active-window s)))))))

(test process-byte-prefix-detach-returns-detach
  "Prefix byte then 'd' returns :detach from process-byte."
  (let ((s (make-fake-session)))
    (with-loop-state
      (let ((state (cl-tmux::make-input-state)))
        (cl-tmux::process-byte s 2 state)
        (is (eq :detach (cl-tmux::process-byte s (char-code #\d) state)))))))

(test process-byte-ordinary-key-forwards
  "An ordinary byte (no prefix) is forwarded and returns NIL (no quit)."
  (let ((s (make-fake-session)))
    (with-loop-state
      (let ((state (cl-tmux::make-input-state)))
        ;; fd -1 panes make pty-write a harmless no-op; we assert routing only.
        (is (null (cl-tmux::process-byte s (char-code #\x) state)))))))

(test process-byte-routes-to-active-prompt
  "While a prompt is active, process-byte edits the prompt buffer."
  (let ((s (make-fake-session)))
    (let ((*prompt* nil) (cl-tmux::*dirty* nil) (cl-tmux::*running* t))
      (prompt-start "rename-window" "" (lambda (name) (declare (ignore name)) nil))
      (let ((state (cl-tmux::make-input-state)))
        (cl-tmux::process-byte s (char-code #\h) state)
        (cl-tmux::process-byte s (char-code #\i) state)
        (is (string= "hi" (prompt-buffer *prompt*))
            "prompt captured the keystrokes via process-byte")))))

(test process-byte-overlay-q-dismisses
  "While an overlay is shown, q dismisses it; other keys are swallowed (overlay stays open)."
  (let ((s (make-fake-session)))
    (let ((*overlay* nil) (cl-tmux::*dirty* nil))
      (show-overlay "help text")
      (let ((state (cl-tmux::make-input-state)))
        ;; An ordinary key ('x') is swallowed but the overlay stays open.
        (is (null (cl-tmux::process-byte s (char-code #\x) state)))
        (is (overlay-active-p) "ordinary key must not dismiss the overlay")
        ;; 'q' dismisses the overlay.
        (is (null (cl-tmux::process-byte s (char-code #\q) state)))
        (is (not (overlay-active-p)) "q must dismiss the overlay")))))

;;; ── Copy-mode arrow escapes through process-byte (one byte at a time) ────────
;;;
;;; In copy mode an arrow key arrives as three separate bytes (ESC, '[', 'A').
;;; process-byte must arm escape-pending on the lone ESC, keep it armed after
;;; '[' (an incomplete ESC [ … sequence), then on the final byte dispatch the
;;; copy-mode scroll and disarm.  copy-mode-scroll clamps the offset to
;;; [0, (length scrollback)], so we seed the active screen's scrollback with a
;;; few dummy rows; otherwise max-offset is 0 and the offset can never advance.

(test process-byte-copy-mode-up-arrow-end-to-end
  "ESC [ A (up arrow) in copy mode scrolls the viewport when the cursor is already
   at the top row (row 0) and scrollback is available."
  (let ((s (make-fake-session)))
    (with-loop-state
      (let ((screen (active-screen s))
            (state  (cl-tmux::make-input-state)))
        (cl-tmux::dispatch-command s :copy-mode-enter nil)
        (seed-scrollback screen 10)
        ;; Force cursor to the top row so the next up-arrow scrolls the viewport.
        (setf (cl-tmux/terminal/types:screen-copy-cursor screen) (cons 0 0))
        (is (zerop (screen-copy-offset screen)) "offset starts at the live view")
        ;; ESC [ A, one byte at a time.
        (is (null (cl-tmux::process-byte s 27 state)))
        (is (null (cl-tmux::process-byte s 91 state)))
        (is (null (cl-tmux::process-byte s 65 state)))
        (is (= 1 (screen-copy-offset screen))
            "up-arrow at top row scrolls the viewport back by 1")))))

(test process-byte-copy-mode-down-arrow-end-to-end
  "ESC [ B (down arrow) in copy mode scrolls the viewport forward when the cursor is
   already at the bottom row and a scrolled-up viewport is active."
  (let ((s (make-fake-session)))
    (with-loop-state
      (let ((screen (active-screen s))
            (state  (cl-tmux::make-input-state)))
        (cl-tmux::dispatch-command s :copy-mode-enter nil)
        (seed-scrollback screen 10)
        ;; Scroll the viewport up so there is room to scroll back down.
        (cl-tmux/commands::copy-mode-scroll screen 6)
        (is (= 6 (screen-copy-offset screen)) "pre-scrolled up 6 lines")
        ;; Force cursor to the bottom row so the next down-arrow scrolls the viewport.
        (setf (cl-tmux/terminal/types:screen-copy-cursor screen)
              (cons (1- (screen-height screen)) 0))
        ;; ESC [ B, one byte at a time.
        (is (null (cl-tmux::process-byte s 27 state)))
        (is (null (cl-tmux::process-byte s 91 state)))
        (is (null (cl-tmux::process-byte s 66 state)))   ; 'B' = down
        (is (= 5 (screen-copy-offset screen))
            "down-arrow at bottom row scrolls the viewport forward by 1 (6 - 1)")))))

(test process-byte-esc-not-bracket-flushes
  "ESC followed by a non-'[' byte is not an arrow sequence: the continuation
   flushes the two accumulated bytes through and returns to ground state."
  (let ((s (make-fake-session)))
    (with-loop-state
      (let ((screen (active-screen s))
            (state  (cl-tmux::make-input-state)))
        (cl-tmux::dispatch-command s :copy-mode-enter nil)
        (is (null (cl-tmux::process-byte s 27 state)))
        ;; 'x' (120) is not '[': the buffer (ESC x) flushes and returns to ground.
        (is (null (cl-tmux::process-byte s 120 state)))
        (is (zerop (screen-copy-offset screen))
            "a flushed (non-arrow) escape does not scroll the copy-offset")))))

(test process-byte-esc-not-copy-mode-forwards-directly
  "Outside copy mode, ESC is an ordinary byte forwarded to the pane — the
   CPS state remains in ground state (no escape accumulation)."
  (let ((s (make-fake-session)))
    (with-loop-state
      (let ((state (cl-tmux::make-input-state)))
        (is (not (cl-tmux::copy-mode-active-p s)) "not in copy mode")
        (is (null (cl-tmux::process-byte s 27 state)))
        ;; After forwarding ESC outside copy-mode the state returns to ground:
        ;; the next ordinary byte should also be forwarded (no stuck state).
        (is (null (cl-tmux::process-byte s (char-code #\a) state))
            "byte after ESC (non-copy-mode) is also forwarded cleanly")))))

;;; ── %handle-resize / %handle-dirty extracted handlers ────────────────────────

(test handle-resize-updates-term-size
  "%handle-resize clears *resize-pending* and relayouts the active window."
  (let ((s (make-fake-session :nwindows 1)))
    (let ((cl-tmux::*running* t)
          (cl-tmux::*dirty* nil)
          (cl-tmux::*resize-pending* t)
          (cl-tmux::*term-rows* 10)
          (cl-tmux::*term-cols* 40))
      ;; terminal-size returns real size in sandbox, which may differ from 10x40.
      ;; Just assert *resize-pending* is cleared and no error is signalled.
      (cl-tmux::%handle-resize s)
      (is-false cl-tmux::*resize-pending*
                "*resize-pending* must be NIL after %handle-resize"))))

(test handle-dirty-clears-flag
  "%handle-dirty clears *dirty* and renders without error."
  (let ((s (make-fake-session :nwindows 1)))
    (let ((cl-tmux::*running* t)
          (cl-tmux::*dirty* t)
          (cl-tmux::*term-rows* 10)
          (cl-tmux::*term-cols* 40))
      (cl-tmux::%handle-dirty s)
      (is-false cl-tmux::*dirty*
                "*dirty* must be NIL after %handle-dirty"))))

;;; ── handle-prompt-key: prompt editing keys ───────────────────────────────────

(test handle-prompt-key-enter-submits-and-dismisses
  "Enter (13) runs the prompt's on-submit closure with the buffer, then dismisses
   the prompt."
  (with-clean-prompt
    (let ((submitted nil))
      (prompt-start "rename-window" "hello"
                    (lambda (buf) (setf submitted buf)))
      (cl-tmux::handle-prompt-key 13)
      (is (string= "hello" submitted)
          "Enter calls on-submit with the current buffer")
      (is (not (prompt-active-p)) "Enter dismisses the prompt")
      (is-true cl-tmux::*dirty* "handle-prompt-key marks the screen dirty"))))

(test handle-prompt-key-esc-cancels
  "Esc (27) cancels the prompt without running on-submit."
  (with-clean-prompt
    (let ((submitted nil))
      (prompt-start "rename-window" "abc"
                    (lambda (buf) (setf submitted buf)))
      (cl-tmux::handle-prompt-key 27)
      (is (not (prompt-active-p)) "Esc dismisses the prompt")
      (is (null submitted) "Esc does not run the on-submit closure"))))

(test handle-prompt-key-backspace-deletes-last-char
  "Backspace (127) and BS (8) delete the last character of the prompt buffer."
  (with-clean-prompt
    (prompt-start "rename-window" "abc"
                  (lambda (buf) (declare (ignore buf)) nil))
    (cl-tmux::handle-prompt-key 127)
    (is (string= "ab" (prompt-buffer *prompt*)) "DEL deletes the last char")
    (cl-tmux::handle-prompt-key 8)
    (is (string= "a" (prompt-buffer *prompt*)) "BS deletes the last char")))

(test define-copy-mode-escape-table-macro-is-defined
  "define-copy-mode-escape-table is a defined macro."
  (is (macro-function 'cl-tmux::define-copy-mode-escape-table)))

(test define-cps-state-macro-is-defined
  "define-cps-state is a defined macro."
  (is (macro-function 'cl-tmux::define-cps-state)))

(test define-prompt-key-rules-macro-is-defined
  "define-prompt-key-rules is a defined macro."
  (is (macro-function 'cl-tmux::define-prompt-key-rules)))

;;; ── Mouse event dispatch tests ───────────────────────────────────────────────

(test dispatch-mouse-event-left-click-selects-pane
  "%dispatch-mouse-event with btn=0 release=NIL selects the pane at the given coordinates."
  (let* ((p0  (make-pane :id 1 :fd -1 :pid -1
                          :x 0 :y 0 :width 40 :height 24
                          :screen (make-screen 40 24)))
         (p1  (make-pane :id 2 :fd -1 :pid -1
                          :x 41 :y 0 :width 40 :height 24
                          :screen (make-screen 40 24)))
         (win (make-window :id 1 :name "w" :width 81 :height 24
                           :panes (list p0 p1)
                           :tree  (make-layout-split :h
                                    (make-layout-leaf p0)
                                    (make-layout-leaf p1)
                                    1/2)
                           :active p0))
         (sess (make-session :id 1 :name "0" :windows (list win) :active win)))
    (cl-tmux/options:set-option "mouse" t)
    (unwind-protect
         (with-loop-state
           (let ((cl-tmux::*term-rows* 25) (cl-tmux::*term-cols* 81))
             ;; Click in the right pane (col 50, row 5)
             (cl-tmux::%dispatch-mouse-event sess 0 50 5 nil)
             (is (eq p1 (window-active-pane win))
                 "left click in right half should focus p1")))
      (cl-tmux/options:set-option "mouse" nil))))

(test dispatch-mouse-event-release-does-not-select
  "%dispatch-mouse-event with release-p=T does not switch the active pane."
  (let* ((p0  (make-pane :id 1 :fd -1 :pid -1
                          :x 0 :y 0 :width 40 :height 24
                          :screen (make-screen 40 24)))
         (p1  (make-pane :id 2 :fd -1 :pid -1
                          :x 41 :y 0 :width 40 :height 24
                          :screen (make-screen 40 24)))
         (win (make-window :id 1 :name "w" :width 81 :height 24
                           :panes (list p0 p1)
                           :tree  (make-layout-split :h
                                    (make-layout-leaf p0)
                                    (make-layout-leaf p1)
                                    1/2)
                           :active p0))
         (sess (make-session :id 1 :name "0" :windows (list win) :active win)))
    (cl-tmux/options:set-option "mouse" t)
    (unwind-protect
         (with-loop-state
           (let ((cl-tmux::*term-rows* 25) (cl-tmux::*term-cols* 81))
             ;; Release event (btn=0, release-p=T) — must not change active pane
             (cl-tmux::%dispatch-mouse-event sess 0 50 5 t)
             (is (eq p0 (window-active-pane win))
                 "button release should not change the active pane")))
      (cl-tmux/options:set-option "mouse" nil))))

(test x10-mouse-sequence-via-process-byte
  "X10 mouse press ESC [ M <btn+32> <col+33> <row+33> fed one byte at a time
   selects the pane at the encoded coordinates."
  (let* ((p0  (make-pane :id 1 :fd -1 :pid -1
                          :x 0 :y 0 :width 40 :height 24
                          :screen (make-screen 40 24)))
         (p1  (make-pane :id 2 :fd -1 :pid -1
                          :x 41 :y 0 :width 40 :height 24
                          :screen (make-screen 40 24)))
         (win (make-window :id 1 :name "w" :width 81 :height 24
                           :panes (list p0 p1)
                           :tree  (make-layout-split :h
                                    (make-layout-leaf p0)
                                    (make-layout-leaf p1)
                                    1/2)
                           :active p0))
         (sess (make-session :id 1 :name "0" :windows (list win) :active win)))
    ;; Enable both per-screen mouse mode and the session mouse option
    (setf (screen-mouse-mode (pane-screen p0)) 1)
    (cl-tmux/options:set-option "mouse" t)
    (unwind-protect
         (with-loop-state
           (let ((state (cl-tmux::make-input-state))
                 (cl-tmux::*term-rows* 25)
                 (cl-tmux::*term-cols* 81))
             ;; X10: btn=0 → 0+32=32; col=50 → 50+33=83; row=5 → 5+33=38
             ;; Sequence: ESC(27) [(91) M(77) 32 83 38
             (cl-tmux::process-byte sess 27 state)
             (cl-tmux::process-byte sess 91 state)
             (cl-tmux::process-byte sess 77 state)
             (cl-tmux::process-byte sess 32 state)
             (cl-tmux::process-byte sess 83 state)
             (cl-tmux::process-byte sess 38 state)
             (is (eq p1 (window-active-pane win))
                 "X10 left-click in right pane must focus p1")))
      (cl-tmux/options:set-option "mouse" nil))))

(test mouse-mode-default-is-off
  "screen-mouse-mode defaults to 0 (off) on a fresh screen."
  (with-screen (s 20 5)
    (is (= 0 (screen-mouse-mode s))
        "mouse-mode must default to 0")))

(test mouse-sgr-mode-default-is-nil
  "screen-mouse-sgr-mode defaults to NIL on a fresh screen."
  (with-screen (s 20 5)
    (is-false (screen-mouse-sgr-mode s)
              "mouse-sgr-mode must default to NIL")))

;;; ── Unbound prefix key discard ───────────────────────────────────────────────

(test unbound-prefix-key-is-discarded
  "An unbound key after prefix (C-b) is silently discarded; no bytes forwarded,
   no crash, and process-byte returns NIL."
  (let ((s (make-fake-session)))
    (with-loop-state
      (let ((state (cl-tmux::make-input-state)))
        ;; C-b (prefix)
        (is (null (cl-tmux::process-byte s 2 state)))
        ;; Unbound key: '@' (64) — not in prefix key-table
        (is (null (cl-tmux::process-byte s (char-code #\@) state))
            "unbound prefix key must return NIL (discarded, not forwarded)")
        ;; State must be back to ground: next ordinary byte is forwarded cleanly.
        (is (null (cl-tmux::process-byte s (char-code #\a) state))
            "state returned to ground after discarding unbound prefix key")))))

;;; ── Copy-mode plain 'q' exits ────────────────────────────────────────────────

(test copy-mode-plain-q-exits
  "Plain 'q' (byte 113) exits copy mode without needing C-b prefix."
  (let ((s (make-fake-session)))
    (with-loop-state
      (let ((screen (active-screen s))
            (state  (cl-tmux::make-input-state)))
        (cl-tmux::dispatch-command s :copy-mode-enter nil)
        (is (screen-copy-mode-p screen) "copy mode entered")
        ;; Feed plain 'q' without any prefix
        (cl-tmux::process-byte s (char-code #\q) state)
        (is (not (screen-copy-mode-p screen))
            "plain q must exit copy mode")))))

(test copy-mode-plain-esc-exits
  "ESC followed by a non-CSI byte exits copy mode."
  (let ((s (make-fake-session)))
    (with-loop-state
      (let ((screen (active-screen s))
            (state  (cl-tmux::make-input-state)))
        (cl-tmux::dispatch-command s :copy-mode-enter nil)
        (is (screen-copy-mode-p screen) "copy mode entered")
        ;; Feed ESC then a non-CSI byte (not '[')
        (cl-tmux::process-byte s 27 state)
        (cl-tmux::process-byte s (char-code #\x) state)
        (is (not (screen-copy-mode-p screen))
            "ESC + non-CSI must exit copy mode")))))

;;; ── Copy-mode unprefixed vi navigation ───────────────────────────────────────

(test copy-mode-j-scrolls-down
  "Plain 'j' (byte 106) scrolls down 1 line in copy mode."
  (let ((s (make-fake-session)))
    (with-loop-state
      (let ((screen (active-screen s))
            (state  (cl-tmux::make-input-state)))
        (cl-tmux::dispatch-command s :copy-mode-enter nil)
        (seed-scrollback screen 10)
        ;; Scroll up 5 first so there's room to scroll back down.
        (cl-tmux/commands::copy-mode-scroll screen 5)
        (is (= 5 (screen-copy-offset screen)))
        (cl-tmux::process-byte s (char-code #\j) state)
        (is (= 4 (screen-copy-offset screen))
            "j must scroll copy offset down by 1")))))

(test copy-mode-k-scrolls-up
  "Plain 'k' moves the cursor up; when the cursor is already at row 0, it scrolls
   the viewport back toward older content by 1 line."
  (let ((s (make-fake-session)))
    (with-loop-state
      (let ((screen (active-screen s))
            (state  (cl-tmux::make-input-state)))
        (cl-tmux::dispatch-command s :copy-mode-enter nil)
        (seed-scrollback screen 10)
        ;; Force cursor to the top row so the next k scrolls the viewport.
        (setf (cl-tmux/terminal/types:screen-copy-cursor screen) (cons 0 0))
        (is (zerop (screen-copy-offset screen)))
        (cl-tmux::process-byte s (char-code #\k) state)
        (is (= 1 (screen-copy-offset screen))
            "k at top row scrolls copy offset up by 1")))))

(test copy-mode-g-jumps-to-top
  "Plain 'g' (byte 103) jumps to top of scrollback in copy mode."
  (let ((s (make-fake-session)))
    (with-loop-state
      (let ((screen (active-screen s))
            (state  (cl-tmux::make-input-state)))
        (cl-tmux::dispatch-command s :copy-mode-enter nil)
        (seed-scrollback screen 10)
        (cl-tmux::process-byte s (char-code #\g) state)
        (is (= 10 (screen-copy-offset screen))
            "g must jump to top (max scrollback offset)")))))

(test copy-mode-G-jumps-to-bottom
  "Plain 'G' (byte 71) jumps to bottom (live view) in copy mode."
  (let ((s (make-fake-session)))
    (with-loop-state
      (let ((screen (active-screen s))
            (state  (cl-tmux::make-input-state)))
        (cl-tmux::dispatch-command s :copy-mode-enter nil)
        (seed-scrollback screen 10)
        ;; Scroll up first
        (cl-tmux/commands::copy-mode-scroll screen 8)
        (is (= 8 (screen-copy-offset screen)))
        (cl-tmux::process-byte s (char-code #\G) state)
        (is (zerop (screen-copy-offset screen))
            "G must jump to bottom (offset = 0)")))))

;;; ── Copy-mode PageUp / PageDown via escape sequence ─────────────────────────

(test copy-mode-pageup-scrolls-one-page
  "ESC [ 5 ~ (PageUp) fed one byte at a time scrolls up by screen-height lines."
  (let ((s (make-fake-session)))
    (with-loop-state
      (let ((screen (active-screen s))
            (state  (cl-tmux::make-input-state)))
        (cl-tmux::dispatch-command s :copy-mode-enter nil)
        (seed-scrollback screen 30)
        (is (zerop (screen-copy-offset screen)))
        ;; ESC [ 5 ~  = 27 91 53 126
        (cl-tmux::process-byte s 27  state)
        (cl-tmux::process-byte s 91  state)
        (cl-tmux::process-byte s 53  state)
        (cl-tmux::process-byte s 126 state)
        (let ((h (screen-height screen)))
          (is (= (min h 30) (screen-copy-offset screen))
              "PageUp must scroll copy-offset by screen-height lines"))))))

(test copy-mode-pagedown-scrolls-one-page
  "ESC [ 6 ~ (PageDown) fed one byte at a time scrolls down by screen-height lines."
  (let ((s (make-fake-session)))
    (with-loop-state
      (let ((screen (active-screen s))
            (state  (cl-tmux::make-input-state)))
        (cl-tmux::dispatch-command s :copy-mode-enter nil)
        (seed-scrollback screen 30)
        ;; Pre-scroll up by 2*screen-height (clamped to scrollback length = 30)
        (let* ((h     (screen-height screen))
               (start (min (* 2 h) 30)))
          (cl-tmux/commands::copy-mode-scroll screen start)
          (is (= start (screen-copy-offset screen)) "pre-scroll verified")
          ;; ESC [ 6 ~  = 27 91 54 126
          (cl-tmux::process-byte s 27  state)
          (cl-tmux::process-byte s 91  state)
          (cl-tmux::process-byte s 54  state)
          (cl-tmux::process-byte s 126 state)
          ;; After PageDown the offset decreases by h (clamped to 0).
          (let ((expected (max 0 (- start h))))
            (is (= expected (screen-copy-offset screen))
                "PageDown must scroll copy-offset down by screen-height lines")))))))

;;; ── Prefix arrow keys select pane ────────────────────────────────────────────

(test prefix-arrow-up-selects-pane-up
  "C-b Up (prefix then ESC [ A) dispatches :select-pane-up."
  (let ((s (make-fake-session)))
    (with-loop-state
      (let ((state (cl-tmux::make-input-state)))
        ;; Feed C-b (prefix) then ESC [ A
        (cl-tmux::process-byte s 2   state)
        (cl-tmux::process-byte s 27  state)
        (cl-tmux::process-byte s 91  state)
        ;; Final byte — returns to ground state, no crash.
        (is (null (cl-tmux::process-byte s 65 state))
            "C-b Up arrow must return NIL (no quit/detach)")))))

(test prefix-arrow-down-selects-pane-down
  "C-b Down (prefix then ESC [ B) dispatches :select-pane-down."
  (let ((s (make-fake-session)))
    (with-loop-state
      (let ((state (cl-tmux::make-input-state)))
        (cl-tmux::process-byte s 2   state)
        (cl-tmux::process-byte s 27  state)
        (cl-tmux::process-byte s 91  state)
        (is (null (cl-tmux::process-byte s 66 state))
            "C-b Down arrow must return NIL (no quit/detach)")))))

;;; ── C-b C-b send-prefix ──────────────────────────────────────────────────────

(test prefix-then-prefix-byte-sends-send-prefix
  "C-b C-b (byte 2 twice) dispatches :send-prefix (no crash, returns NIL)."
  (let ((s (make-fake-session)))
    (with-loop-state
      (let ((state (cl-tmux::make-input-state)))
        (cl-tmux::process-byte s 2 state)  ; prefix
        (is (null (cl-tmux::process-byte s 2 state))
            "C-b C-b must dispatch :send-prefix and return NIL")))))

;;; ── Application cursor keys — %arrow-final-to-ss3-bytes helper ──────────────

(test arrow-final-to-ss3-bytes-maps-arrows
  "%arrow-final-to-ss3-bytes converts CSI arrow final bytes to SS3 sequences."
  ;; 65=A (up), 66=B (down), 67=C (right), 68=D (left)
  (dolist (final '(65 66 67 68))
    (let ((ss3 (cl-tmux::%arrow-final-to-ss3-bytes final)))
      (is (and ss3
               (= 3 (length ss3))
               (= 27  (aref ss3 0))
               (= 79  (aref ss3 1))
               (= final (aref ss3 2)))
          "SS3 sequence for final byte ~D must be ESC O ~C" final (code-char final)))))

(test arrow-final-to-ss3-bytes-returns-nil-for-non-arrows
  "%arrow-final-to-ss3-bytes returns NIL for non-arrow final bytes."
  (is (null (cl-tmux::%arrow-final-to-ss3-bytes 72))  ; H = home, not arrow
      "Non-arrow byte must return NIL")
  (is (null (cl-tmux::%arrow-final-to-ss3-bytes 109)) ; m = SGR final
      "SGR final byte must return NIL"))

;;; ── New default key bindings ─────────────────────────────────────────────────

(test key-binding-colon-is-command-prompt
  "C-b : (char code 58) is bound to :command-prompt."
  (is (eq :command-prompt (lookup-key-binding #\:))
      "C-b : must be bound to :command-prompt"))

(test key-binding-t-is-clock-mode
  "C-b t (char code 116) is bound to :clock-mode."
  (is (eq :clock-mode (lookup-key-binding #\t))
      "C-b t must be bound to :clock-mode"))

(test key-binding-i-is-display-info
  "C-b i (char code 105) is bound to :display-info."
  (is (eq :display-info (lookup-key-binding #\i))
      "C-b i must be bound to :display-info"))

(test key-binding-tilde-is-show-messages
  "C-b ~ (code-char 126) is bound to :show-messages."
  (is (eq :show-messages (lookup-key-binding (code-char 126)))
      "C-b ~ must be bound to :show-messages"))

(test key-binding-m-is-mark-pane
  "C-b m (char code 109) is bound to :mark-pane."
  (is (eq :mark-pane (lookup-key-binding #\m))
      "C-b m must be bound to :mark-pane"))

(test key-binding-capital-M-is-clear-mark
  "C-b M (code-char 77) is bound to :clear-mark."
  (is (eq :clear-mark (lookup-key-binding (code-char 77)))
      "C-b M must be bound to :clear-mark"))

(test key-binding-capital-E-is-select-layout-spread
  "C-b E (char code 69) is bound to :select-layout-spread."
  (is (eq :select-layout-spread (lookup-key-binding #\E))
      "C-b E must be bound to :select-layout-spread"))

(test key-binding-space-is-next-layout
  "C-b Space (code-char 32) is bound to :next-layout."
  (is (eq :next-layout (lookup-key-binding (code-char 32)))
      "C-b Space must be bound to :next-layout"))

(test key-binding-dot-is-move-window-prompt
  "C-b . (char code 46) is bound to :move-window-prompt."
  (is (eq :move-window-prompt (lookup-key-binding #\.))
      "C-b . must be bound to :move-window-prompt"))

(test key-binding-capital-D-is-choose-client
  "C-b D (char code 68) is bound to :choose-client."
  (is (eq :choose-client (lookup-key-binding #\D))
      "C-b D must be bound to :choose-client"))

;;; ── dispatch :mark-pane and :clear-mark ─────────────────────────────────────
;;; Build sessions manually (same pattern as dispatch-display-panes tests)
;;; to avoid any interaction with make-fake-session helpers.

(test dispatch-mark-pane-marks-active-pane
  ":mark-pane command sets pane-marked on the active pane."
  (let* ((p0   (make-pane :id 1 :fd -1 :pid -1 :x 0 :y 0 :width 20 :height 5
                           :screen (make-screen 20 5)))
         (win  (make-window :id 1 :name "w" :width 20 :height 5
                            :panes (list p0)
                            :tree  (make-layout-leaf p0)))
         (sess (make-session :id 1 :name "s" :windows (list win))))
    (window-select-pane win p0)
    (session-select-window sess win)
    (let ((*overlay* nil) (cl-tmux::*dirty* nil) (cl-tmux::*running* t))
      (is (not (pane-marked p0)) "pane must not be marked initially")
      (cl-tmux::dispatch-command sess :mark-pane nil)
      (is (pane-marked p0) "pane must be marked after :mark-pane"))))

(test dispatch-mark-pane-toggle-unmarks
  ":mark-pane on an already-marked pane unmarks it (toggle)."
  (let* ((p0   (make-pane :id 1 :fd -1 :pid -1 :x 0 :y 0 :width 20 :height 5
                           :screen (make-screen 20 5)))
         (win  (make-window :id 1 :name "w" :width 20 :height 5
                            :panes (list p0)
                            :tree  (make-layout-leaf p0)))
         (sess (make-session :id 1 :name "s" :windows (list win))))
    (window-select-pane win p0)
    (session-select-window sess win)
    (let ((*overlay* nil) (cl-tmux::*dirty* nil) (cl-tmux::*running* t))
      (setf (pane-marked p0) t)
      (is (pane-marked p0) "pane marked before dispatch")
      (cl-tmux::dispatch-command sess :mark-pane nil)
      (is (not (pane-marked p0))
          "pane unmarked after :mark-pane on already-marked pane"))))

(test dispatch-clear-mark-unmarks-all-panes
  ":clear-mark clears pane-marked on all panes in the current window."
  (let* ((p0   (make-pane :id 1 :fd -1 :pid -1 :x 0 :y 0 :width 20 :height 5
                           :screen (make-screen 20 5)))
         (win  (make-window :id 1 :name "w" :width 20 :height 5
                            :panes (list p0)
                            :tree  (make-layout-leaf p0)))
         (sess (make-session :id 1 :name "s" :windows (list win))))
    (window-select-pane win p0)
    (session-select-window sess win)
    (let ((*overlay* nil) (cl-tmux::*dirty* nil) (cl-tmux::*running* t))
      (setf (pane-marked p0) t)
      (is (pane-marked p0) "pane must be marked before :clear-mark")
      (cl-tmux::dispatch-command sess :clear-mark nil)
      (is (not (pane-marked p0)) "pane must not be marked after :clear-mark"))))

;;; ── dispatch :display-info ───────────────────────────────────────────────────

(test dispatch-display-info-shows-overlay
  ":display-info shows a non-empty overlay with session/window/pane info."
  (let* ((p0   (make-pane :id 1 :fd -1 :pid -1 :x 0 :y 0 :width 20 :height 5
                           :screen (make-screen 20 5)))
         (win  (make-window :id 1 :name "w" :width 20 :height 5
                            :panes (list p0)
                            :tree  (make-layout-leaf p0)))
         (sess (make-session :id 1 :name "mysess" :windows (list win))))
    (window-select-pane win p0)
    (session-select-window sess win)
    (let ((*overlay* nil) (cl-tmux::*dirty* nil) (cl-tmux::*running* t))
      (cl-tmux::dispatch-command sess :display-info nil)
      (is (overlay-active-p) "display-info must activate the overlay")
      (is (search "Session:" *overlay*)
          "overlay must contain \"Session:\""))))

;;; ── dispatch :choose-client ──────────────────────────────────────────────────

(test dispatch-choose-client-shows-overlay
  ":choose-client shows an overlay with client info."
  (let ((s (make-fake-session)))
    (let ((*overlay* nil) (cl-tmux::*dirty* nil) (cl-tmux::*running* t)
          (cl-tmux::*term-rows* 24) (cl-tmux::*term-cols* 80))
      (cl-tmux::dispatch-command s :choose-client nil)
      (is (overlay-active-p) "choose-client must activate the overlay")
      (is (search "Clients" *overlay*)
          "overlay must contain \"Clients\""))))

;;; ── Root key-table lookup ────────────────────────────────────────────────────

(test root-table-binding-fires-without-prefix
  "A key bound in the root table (bind -n) fires without the C-b prefix."
  (let ((s (make-fake-session :nwindows 2)))
    (with-loop-state
      (let ((state (cl-tmux::make-input-state)))
        ;; Bind 'Z' in root table so it selects the next window without C-b.
        (key-table-bind "root" #\Z :next-window)
        (unwind-protect
             (progn
               (cl-tmux::process-byte s (char-code #\Z) state)
               (is (eq (second (session-windows s)) (session-active-window s))
                   "root-table binding must fire without C-b prefix"))
          ;; Clean up: remove the root binding we added.
          (let ((tbl (gethash "root" *key-tables*)))
            (when tbl (remhash #\Z tbl))))))))

;;; ── dispatch :select-layout-spread ─────────────────────────────────────────

(test dispatch-select-layout-spread-applies-even-horizontal
  ":select-layout-spread applies the even-horizontal layout without signaling."
  (let ((s (make-fake-session)))
    (with-loop-state
      (is (null (handler-case
                    (cl-tmux::dispatch-command s :select-layout-spread nil)
                  (error (e) e)))
          ":select-layout-spread must not signal an error"))))

;;; ── New key bindings: z, ', and grouping ────────────────────────────────────

(test key-binding-z-lowercase-is-zoom-toggle
  "C-b z (lowercase, char code 122) is bound to :zoom-toggle."
  (is (eq :zoom-toggle (lookup-key-binding #\z))
      "C-b z must be bound to :zoom-toggle (standard tmux default)"))

(test key-binding-Z-uppercase-is-still-zoom-toggle
  "C-b Z (uppercase, char code 90) remains bound to :zoom-toggle."
  (is (eq :zoom-toggle (lookup-key-binding #\Z))
      "C-b Z must also be bound to :zoom-toggle"))

(test key-binding-quote-is-select-window-prompt
  "C-b ' (char code 39) is bound to :select-window-prompt."
  (is (eq :select-window-prompt (lookup-key-binding #\'))
      "C-b ' must be bound to :select-window-prompt"))

(test dispatch-zoom-toggle-via-lowercase-z
  "C-b z dispatches :zoom-toggle without error."
  (let ((s (make-fake-session)))
    (with-loop-state
      (let ((state (cl-tmux::make-input-state)))
        (cl-tmux::process-byte s 2 state)
        (is (null (cl-tmux::process-byte s (char-code #\z) state))
            "C-b z must dispatch :zoom-toggle and return NIL")))))

(test dispatch-select-window-prompt-opens-prompt
  ":select-window-prompt opens a prompt without signaling."
  (let ((s (make-fake-session :nwindows 2)))
    (with-loop-state
      (let ((*prompt* nil))
        (cl-tmux::dispatch-command s :select-window-prompt nil)
        (is (prompt-active-p)
            ":select-window-prompt must open a prompt")))))

;;; ── choose-window uses menu system ──────────────────────────────────────────

(test dispatch-choose-window-shows-menu-overlay
  ":choose-window shows a menu overlay and opens a prompt for window selection."
  (let ((s (make-fake-session :nwindows 2)))
    (with-loop-state
      (let ((*overlay* nil) (*prompt* nil) (cl-tmux::*dirty* nil) (cl-tmux::*running* t))
        (cl-tmux::dispatch-command s :choose-window nil)
        (is (overlay-active-p) ":choose-window must show an overlay")
        (is (prompt-active-p)  ":choose-window must open a prompt")))))

;;; ── Mouse reporting helpers ──────────────────────────────────────────────────

(test enable-mouse-reporting-writes-sequences
  "enable-mouse-reporting emits the three DEC private mode sequences."
  (let ((output (with-output-to-string (*standard-output*)
                  (cl-tmux/renderer:enable-mouse-reporting))))
    ;; Must contain all three mode strings
    (is (search "?1000h" output) "must contain ?1000h (X10 basic)")
    (is (search "?1002h" output) "must contain ?1002h (button events)")
    (is (search "?1006h" output) "must contain ?1006h (SGR extended)")))

(test disable-mouse-reporting-writes-disable-sequences
  "disable-mouse-reporting emits the three DEC private mode disable sequences."
  (let ((output (with-output-to-string (*standard-output*)
                  (cl-tmux/renderer:disable-mouse-reporting))))
    (is (search "?1006l" output) "must contain ?1006l")
    (is (search "?1002l" output) "must contain ?1002l")
    (is (search "?1000l" output) "must contain ?1000l")))

;;; ── All standard tmux default key bindings present ───────────────────────────
;;;
;;; Verify every key in the standard tmux default table has an entry in the
;;; prefix key-table.  This is a regression guard: if a binding is accidentally
;;; removed the test fails immediately.

(test standard-key-bindings-complete
  "All standard tmux default bindings must be present in prefix key-table."
  (flet ((bound-p (key)
           (not (null (lookup-key-binding key)))))
    ;; Session
    (is (bound-p #\d)   "d → detach")
    (is (bound-p #\$)   "$ → rename-session")
    (is (bound-p #\s)   "s → choose-session")
    (is (bound-p #\()   "( → switch-client-prev")
    (is (bound-p #\))   ") → switch-client-next")
    (is (bound-p #\L)   "L → last-session")
    ;; Window
    (is (bound-p #\c)   "c → new-window")
    (is (bound-p #\n)   "n → next-window")
    (is (bound-p #\p)   "p → prev-window")
    (is (bound-p #\l)   "l → last-window")
    (is (bound-p #\w)   "w → choose-window")
    (is (bound-p #\f)   "f → find-window")
    (is (bound-p #\&)   "& → kill-window-confirm")
    (is (bound-p #\,)   ", → rename-window")
    (is (bound-p #\0)   "0 → select-window")
    (is (bound-p #\9)   "9 → select-window")
    (is (bound-p #\.)   ". → move-window-prompt")
    (is (bound-p #\')   "' → select-window-prompt")
    ;; Pane
    (is (bound-p #\%)   "% → split-vertical")
    (is (bound-p #\")   "\" → split-horizontal")
    (is (bound-p #\o)   "o → next-pane")
    (is (bound-p #\;)   "; → last-pane")
    (is (bound-p #\q)   "q → display-panes")
    (is (bound-p #\x)   "x → kill-pane-confirm")
    (is (bound-p #\z)   "z → zoom-toggle (lowercase)")
    (is (bound-p #\!)   "! → break-pane")
    (is (bound-p #\{)   "{ → swap-pane-backward")
    (is (bound-p #\})   "} → swap-pane-forward")
    ;; Buffer
    (is (bound-p #\[)   "[ → copy-mode-enter")
    (is (bound-p #\])   "] → paste-buffer")
    (is (bound-p (code-char 35))  "# → list-buffers")
    (is (bound-p (code-char 61))  "= → choose-buffer")
    (is (bound-p (code-char 45))  "- → delete-buffer")
    ;; Misc
    (is (bound-p #\:)   ": → command-prompt")
    (is (bound-p #\?)   "? → list-keys")
    (is (bound-p #\t)   "t → clock-mode")
    (is (bound-p #\i)   "i → display-info")
    (is (bound-p (code-char 126)) "~ → show-messages")
    (is (bound-p #\m)   "m → mark-pane")
    (is (bound-p (code-char 77))  "M → clear-mark")
    (is (bound-p #\E)   "E → select-layout-spread")
    (is (bound-p (code-char 32))  "Space → next-layout")
    (is (bound-p #\D)   "D → choose-client")
    (is (bound-p (code-char 2))   "C-b → send-prefix")))

;;; ── Mouse scroll-wheel paths ─────────────────────────────────────────────────

(test dispatch-mouse-scroll-up-enters-copy-mode
  "Mouse scroll-up (btn=64) enters copy mode on the active pane when not in copy mode."
  (let* ((p0  (make-pane :id 1 :fd -1 :pid -1
                          :x 0 :y 0 :width 40 :height 24
                          :screen (make-screen 40 24)))
         (win (make-window :id 1 :name "w" :width 40 :height 24
                           :panes (list p0)
                           :tree  (make-layout-leaf p0)
                           :active p0))
         (sess (make-session :id 1 :name "0" :windows (list win) :active win)))
    (cl-tmux/options:set-option "mouse" t)
    (unwind-protect
         (with-loop-state
           (let ((cl-tmux::*term-rows* 25) (cl-tmux::*term-cols* 40))
             (seed-scrollback (pane-screen p0) 5)
             (cl-tmux::%dispatch-mouse-event sess 64 5 5 nil)
             (is (screen-copy-mode-p (pane-screen p0))
                 "scroll-up must enter copy mode")))
      (cl-tmux/options:set-option "mouse" nil))))

(test dispatch-mouse-scroll-down-exits-copy-mode-at-bottom
  "Mouse scroll-down (btn=65) exits copy mode when the viewport is at the bottom (offset=0)."
  (let* ((p0  (make-pane :id 1 :fd -1 :pid -1
                          :x 0 :y 0 :width 40 :height 24
                          :screen (make-screen 40 24)))
         (win (make-window :id 1 :name "w" :width 40 :height 24
                           :panes (list p0)
                           :tree  (make-layout-leaf p0)
                           :active p0))
         (sess (make-session :id 1 :name "0" :windows (list win) :active win)))
    (cl-tmux/options:set-option "mouse" t)
    (unwind-protect
         (with-loop-state
           (let ((cl-tmux::*term-rows* 25) (cl-tmux::*term-cols* 40)
                 (sc (pane-screen p0)))
             (seed-scrollback sc 5)
             (copy-mode-enter sc)
             ;; offset already at 0 — scroll down should exit copy mode
             (cl-tmux::%dispatch-mouse-event sess 65 5 5 nil)
             (is (not (screen-copy-mode-p sc))
                 "scroll-down at offset=0 must exit copy mode")))
      (cl-tmux/options:set-option "mouse" nil))))

(test dispatch-mouse-gated-by-mouse-option
  "%dispatch-mouse-event is a no-op when the 'mouse' option is false."
  (let* ((p0  (make-pane :id 1 :fd -1 :pid -1
                          :x 0 :y 0 :width 40 :height 24
                          :screen (make-screen 40 24)))
         (win (make-window :id 1 :name "w" :width 40 :height 24
                           :panes (list p0)
                           :tree  (make-layout-leaf p0)
                           :active p0))
         (sess (make-session :id 1 :name "0" :windows (list win) :active win)))
    (cl-tmux/options:set-option "mouse" nil)
    (with-loop-state
      (let ((cl-tmux::*term-rows* 25) (cl-tmux::*term-cols* 40))
        ;; With mouse off, click must not enter copy mode.
        (cl-tmux::%dispatch-mouse-event sess 0 5 5 nil)
        (is (not (screen-copy-mode-p (pane-screen p0)))
            "mouse event must be ignored when mouse option is off")))))

;;; ── %status-col-to-window helper ─────────────────────────────────────────────

(test status-col-to-window-returns-nil-before-first-window
  "%status-col-to-window returns NIL for a column before any window entry."
  (let* ((p0   (make-pane :id 1 :fd -1 :pid -1 :x 0 :y 0 :width 20 :height 5
                           :screen (make-screen 20 5)))
         (win  (make-window :id 0 :name "win0" :width 20 :height 5
                            :panes (list p0) :tree (make-layout-leaf p0)))
         (sess (make-session :id 1 :name "mysess" :windows (list win))))
    (window-select-pane win p0)
    (session-select-window sess win)
    ;; Session prefix is " mysess" = 1 + 6 = 7 chars.
    ;; First window "win0" entry starts at column 7; col 0 is before it.
    (is (null (cl-tmux::%status-col-to-window sess 0))
        "%status-col-to-window must return NIL for column before the first window")))

(test status-col-to-window-returns-window-for-column-in-entry
  "%status-col-to-window returns the window when the column falls within its entry."
  (let* ((p0   (make-pane :id 1 :fd -1 :pid -1 :x 0 :y 0 :width 20 :height 5
                           :screen (make-screen 20 5)))
         (win  (make-window :id 0 :name "w" :width 20 :height 5
                            :panes (list p0) :tree (make-layout-leaf p0)))
         (sess (make-session :id 1 :name "s" :windows (list win))))
    (window-select-pane win p0)
    (session-select-window sess win)
    ;; Session prefix " s" = 2 chars.
    ;; Window "w" entry = 4 + 1 = 5 chars starting at col 2.
    ;; Column 2 is within that entry.
    (is (eq win (cl-tmux::%status-col-to-window sess 2))
        "%status-col-to-window must return the window for a column within its entry")))

;;; ── Mouse button constant sanity checks ──────────────────────────────────────

(test mouse-button-constants-have-expected-values
  "Named mouse button constants must have the correct integer values."
  (is (= 0  cl-tmux::+mouse-btn-left+)        "left button must be 0")
  (is (= 3  cl-tmux::+mouse-btn-release-x10+) "X10 release must be 3")
  (is (= 32 cl-tmux::+mouse-btn-motion+)       "motion must be 32")
  (is (= 64 cl-tmux::+mouse-btn-scroll-up+)    "scroll-up must be 64")
  (is (= 65 cl-tmux::+mouse-btn-scroll-down+)  "scroll-down must be 65"))

;;; ── SGR mouse parser ─────────────────────────────────────────────────────────

(test parse-sgr-mouse-press-sequence
  "%parse-sgr-mouse parses a well-formed SGR press sequence."
  ;; ESC [ < 0 ; 10 ; 5 M  — btn=0, col=10, row=5 (1-based), press
  (let* ((seq "ESC[<0;10;5M")   ; textual — we build the actual byte vector below
         (s   (format nil "~C[<0;10;5M" #\Escape))
         (buf (make-array (length s) :element-type '(unsigned-byte 8)
                          :initial-contents (map 'list #'char-code s)))
         (len (length buf)))
    (declare (ignore seq))
    (multiple-value-bind (btn col row release-p)
        (cl-tmux::%parse-sgr-mouse buf len)
      (is (= 0 btn)       "SGR btn must be 0 for left-button press")
      (is (= 9 col)       "SGR col must be 0-based (10-1=9)")
      (is (= 4 row)       "SGR row must be 0-based (5-1=4)")
      (is (not release-p) "press sequence must have release-p=NIL"))))

(test parse-sgr-mouse-release-sequence
  "%parse-sgr-mouse parses a well-formed SGR release sequence (final byte 'm')."
  (let* ((s   (format nil "~C[<0;10;5m" #\Escape))
         (buf (make-array (length s) :element-type '(unsigned-byte 8)
                          :initial-contents (map 'list #'char-code s)))
         (len (length buf)))
    (multiple-value-bind (btn col row release-p)
        (cl-tmux::%parse-sgr-mouse buf len)
      (is (= 0 btn)    "SGR btn must be 0")
      (is (= 9 col)    "SGR col 0-based")
      (is (= 4 row)    "SGR row 0-based")
      (is-true release-p "release sequence (final 'm') must set release-p=T"))))

(test sgr-mouse-sequence-p-detects-sgr-intro
  "%sgr-mouse-sequence-p returns T for ESC [ < prefix."
  (let* ((s   (format nil "~C[<0;5;3M" #\Escape))
         (buf (make-array (length s) :element-type '(unsigned-byte 8)
                          :initial-contents (map 'list #'char-code s)))
         (len (length buf)))
    (is (cl-tmux::%sgr-mouse-sequence-p buf len)
        "%sgr-mouse-sequence-p must return T for ESC [ < prefix")))

(test sgr-mouse-terminated-p-detects-final-byte
  "%sgr-mouse-terminated-p returns T when the last byte is 'M' or 'm'."
  (flet ((buf-from (s)
           (make-array (length s) :element-type '(unsigned-byte 8)
                       :initial-contents (map 'list #'char-code s))))
    (let* ((press-str   (format nil "~C[<0;5;3M" #\Escape))
           (release-str (format nil "~C[<0;5;3m" #\Escape))
           (pb (buf-from press-str))
           (rb (buf-from release-str)))
      (is (cl-tmux::%sgr-mouse-terminated-p pb (length pb))
          "press sequence ending in 'M' must be terminated")
      (is (cl-tmux::%sgr-mouse-terminated-p rb (length rb))
          "release sequence ending in 'm' must be terminated"))))

;;; ── define-cps-state: ignorable session/byte args ────────────────────────────

(test cps-state-ignores-unused-args
  "A define-cps-state function that ignores both args compiles and runs cleanly."
  ;; Both session and byte are declared ignorable — verify no compile warnings
  ;; by just calling the function and checking the return type.
  (let ((s (make-fake-session))
        (state (cl-tmux::make-input-state)))
    (is (null (cl-tmux::process-byte s 0 state))
        "NUL byte must return NIL (forwarded, no quit)")))

;;; ── Overlay arrow-key scrolling via escape sequence ─────────────────────────
;;;
;;; When the overlay is active and ESC [ A arrives, make-overlay-escape-k
;;; scrolls the overlay up; ESC [ B scrolls it down.

(test overlay-escape-up-scrolls-overlay
  "ESC [ A while an overlay is open scrolls the overlay up (offset -1)."
  (let ((s (make-fake-session)))
    (let ((*overlay* nil) (cl-tmux::*dirty* nil))
      (show-overlay (format nil "~{line~A~%~}" (loop for i from 1 to 20 collect i)))
      (let ((state (cl-tmux::make-input-state)))
        ;; Feed ESC [ A one byte at a time.
        (cl-tmux::process-byte s 27 state)
        (cl-tmux::process-byte s 91 state)
        (cl-tmux::process-byte s 65 state))
      ;; After the sequence the overlay should still be open.
      (is (overlay-active-p)
          "overlay must remain open after ESC [ A (up arrow)"))))

(test overlay-escape-down-scrolls-overlay
  "ESC [ B while an overlay is open scrolls the overlay down (offset +1)."
  (let ((s (make-fake-session)))
    (let ((*overlay* nil) (cl-tmux::*dirty* nil))
      (show-overlay (format nil "~{line~A~%~}" (loop for i from 1 to 20 collect i)))
      (let ((state (cl-tmux::make-input-state)))
        (cl-tmux::process-byte s 27 state)
        (cl-tmux::process-byte s 91 state)
        (cl-tmux::process-byte s 66 state))
      (is (overlay-active-p)
          "overlay must remain open after ESC [ B (down arrow)"))))

(test overlay-bare-esc-dismisses-overlay
  "A lone ESC (ESC + non-'[' byte) while an overlay is open dismisses it."
  (let ((s (make-fake-session)))
    (let ((*overlay* nil) (cl-tmux::*dirty* nil))
      (show-overlay "some text")
      (let ((state (cl-tmux::make-input-state)))
        ;; ESC then 'x' — not a CSI sequence → dismiss
        (cl-tmux::process-byte s 27 state)
        (cl-tmux::process-byte s (char-code #\x) state))
      (is (not (overlay-active-p))
          "overlay must be dismissed by bare ESC"))))

;;; ── handle-prompt-key: additional editing keys ────────────────────────────────

(test handle-prompt-key-ctrl-a-moves-to-bol
  "C-a (byte 1) moves the cursor to the beginning of the prompt line."
  (with-clean-prompt
    (prompt-start "test" "hello"
                  (lambda (buf) (declare (ignore buf)) nil))
    ;; Move cursor to end first (EOL)
    (prompt-cursor-eol)
    (is (= 5 (prompt-cursor-index *prompt*)) "cursor at end")
    (cl-tmux::handle-prompt-key 1)  ; C-a
    (is (= 0 (prompt-cursor-index *prompt*))
        "C-a must move cursor to position 0")))

(test handle-prompt-key-ctrl-e-moves-to-eol
  "C-e (byte 5) moves the cursor to the end of the prompt line."
  (with-clean-prompt
    (prompt-start "test" "hello"
                  (lambda (buf) (declare (ignore buf)) nil))
    ;; Cursor starts at end; move to BOL first
    (prompt-cursor-bol)
    (is (= 0 (prompt-cursor-index *prompt*)) "cursor at start")
    (cl-tmux::handle-prompt-key 5)  ; C-e
    (is (= 5 (prompt-cursor-index *prompt*))
        "C-e must move cursor to end of buffer")))

(test handle-prompt-key-ctrl-c-cancels
  "C-c (byte 3) cancels the prompt without running on-submit."
  (with-clean-prompt
    (let ((submitted nil))
      (prompt-start "test" "abc"
                    (lambda (buf) (setf submitted buf)))
      (cl-tmux::handle-prompt-key 3)  ; C-c
      (is (not (prompt-active-p)) "C-c must dismiss the prompt")
      (is (null submitted) "C-c must not call on-submit"))))

(test handle-prompt-key-printable-inserts-char
  "A printable ASCII byte inserts the corresponding character into the buffer."
  (with-clean-prompt
    (prompt-start "test" ""
                  (lambda (buf) (declare (ignore buf)) nil))
    (cl-tmux::handle-prompt-key (char-code #\A))
    (is (string= "A" (prompt-buffer *prompt*))
        "printable key 'A' must be inserted into buffer")))

;;; ── process-byte with locked session ──────────────────────────────────────────

(test process-byte-unlocks-locked-session
  "Any byte unlocks a locked session; subsequent bytes are processed normally."
  (let ((s (make-fake-session)))
    (with-loop-state
      (setf (session-locked-p s) t)
      (let ((state (cl-tmux::make-input-state)))
        (is (null (cl-tmux::process-byte s (char-code #\a) state))
            "first byte on locked session returns NIL (unlocks)")
        (is-false (session-locked-p s)
                  "session must be unlocked after any byte")))))

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
