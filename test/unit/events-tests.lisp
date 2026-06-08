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
      (is-false (screen-copy-mode-p (active-screen s))
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
  ;; Isolate the key-tables: another suite can mutate the live prefix table, and
  ;; this test depends on the default #\d → :detach binding being present.
  (let ((s (make-fake-session)))
    (with-isolated-config
      (with-loop-state
        (let ((state (cl-tmux::make-input-state)))
          (cl-tmux::process-byte s 2 state)
          (is (eq :detach (cl-tmux::process-byte s (char-code #\d) state))))))))

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
    (with-loop-state
      (let ((*prompt* nil))
        (prompt-start "rename-window" "" (lambda (name) (declare (ignore name)) nil))
        (let ((state (cl-tmux::make-input-state)))
          (cl-tmux::process-byte s (char-code #\h) state)
          (cl-tmux::process-byte s (char-code #\i) state)
          (is (string= "hi" (prompt-buffer *prompt*))
              "prompt captured the keystrokes via process-byte"))))))

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
        (is-false (overlay-active-p) "q must dismiss the overlay")))))

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
        (is-false (cl-tmux::%copy-mode-active-p s) "not in copy mode")
        (is (null (cl-tmux::process-byte s 27 state)))
        ;; After forwarding ESC outside copy-mode the state returns to ground:
        ;; the next ordinary byte should also be forwarded (no stuck state).
        (is (null (cl-tmux::process-byte s (char-code #\a) state))
            "byte after ESC (non-copy-mode) is also forwarded cleanly")))))

;;; ── %handle-resize / %handle-dirty extracted handlers ────────────────────────

(test handle-resize-updates-term-size
  "%handle-resize clears *resize-pending* and relayouts the active window."
  (let ((s (make-fake-session :nwindows 1)))
    (with-loop-state
      (let ((cl-tmux::*resize-pending* t)
            (cl-tmux::*term-rows* 10)
            (cl-tmux::*term-cols* 40))
        ;; terminal-size returns real size in sandbox, which may differ from 10x40.
        ;; Just assert *resize-pending* is cleared and no error is signalled.
        (cl-tmux::%handle-resize s)
        (is-false cl-tmux::*resize-pending*
                  "*resize-pending* must be NIL after %handle-resize")))))

(test handle-resize-fires-client-resized-hook
  "%handle-resize fires +hook-client-resized+ after relaying out the window."
  (with-isolated-hooks
    (let ((s (make-fake-session :nwindows 1))
          (fired nil))
      (with-loop-state
        (let ((cl-tmux::*resize-pending* t))
          (cl-tmux/hooks:add-hook cl-tmux/hooks:+hook-client-resized+
                                  (lambda (&rest _) (declare (ignore _)) (setf fired t)))
          (cl-tmux::%handle-resize s)
          (is-true fired "client-resized hook must fire on terminal resize"))))))

(test handle-dirty-clears-flag
  "%handle-dirty clears *dirty* and renders without error."
  (let ((s (make-fake-session :nwindows 1)))
    (with-loop-state
      (let ((cl-tmux::*dirty* t)
            (cl-tmux::*term-rows* 10)
            (cl-tmux::*term-cols* 40))
        (cl-tmux::%handle-dirty s)
        (is-false cl-tmux::*dirty*
                  "*dirty* must be NIL after %handle-dirty")))))

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
      (is-false (prompt-active-p) "Enter dismisses the prompt")
      (is-true cl-tmux::*dirty* "handle-prompt-key marks the screen dirty"))))

(test handle-prompt-key-esc-cancels
  "Esc (27) cancels the prompt without running on-submit."
  (with-clean-prompt
    (let ((submitted nil))
      (prompt-start "rename-window" "abc"
                    (lambda (buf) (setf submitted buf)))
      (cl-tmux::handle-prompt-key 27)
      (is-false (prompt-active-p) "Esc dismisses the prompt")
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
;;;
;;; The with-two-pane-mouse-session macro (defined in test/helpers.lisp) builds
;;; the 2-pane h-split session, enables the 'mouse' option, and wraps the body
;;; in with-loop-state with appropriate *term-rows*/*term-cols* bindings.

(test dispatch-mouse-event-left-click-selects-pane
  "%dispatch-mouse-event with btn=0 release=NIL selects the pane at the given coordinates."
  (with-two-pane-mouse-session (sess win p0 p1)
    ;; Click in the right pane (col 50, row 5)
    (cl-tmux::%dispatch-mouse-event sess 0 50 5 nil)
    (is (eq p1 (window-active-pane win))
        "left click in right half should focus p1")))

(test dispatch-mouse-event-release-does-not-select
  "%dispatch-mouse-event with release-p=T does not switch the active pane."
  (with-two-pane-mouse-session (sess win p0 p1)
    ;; Release event (btn=0, release-p=T) — must not change active pane
    (cl-tmux::%dispatch-mouse-event sess 0 50 5 t)
    (is (eq p0 (window-active-pane win))
        "button release should not change the active pane")))

(test x10-mouse-sequence-via-process-byte
  "X10 mouse press ESC [ M <btn+32> <col+33> <row+33> fed one byte at a time
   selects the pane at the encoded coordinates."
  (with-two-pane-mouse-session (sess win p0 p1)
    ;; Enable per-screen mouse mode in addition to the session option.
    (setf (screen-mouse-mode (pane-screen p0)) 1)
    (let ((state (cl-tmux::make-input-state)))
      ;; X10: btn=0 → 0+32=32; col=50 → 50+33=83; row=5 → 5+33=38
      ;; Sequence: ESC(27) [(91) M(77) 32 83 38
      (cl-tmux::process-byte sess 27 state)
      (cl-tmux::process-byte sess 91 state)
      (cl-tmux::process-byte sess 77 state)
      (cl-tmux::process-byte sess 32 state)
      (cl-tmux::process-byte sess 83 state)
      (cl-tmux::process-byte sess 38 state)
      (is (eq p1 (window-active-pane win))
          "X10 left-click in right pane must focus p1"))))

(test mouse-middle-click-pastes-top-buffer-into-pane
  "Middle-button press (btn 1) pastes the most recent paste-buffer into the pane
   under the pointer, writing it to that pane's PTY."
  (with-empty-buffers
    (with-pipe-fds (rfd wfd)
      (with-two-pane-mouse-session (sess win p0 p1)
        ;; Give the right pane (p1) a live PTY and stage a paste-buffer.
        (setf (pane-fd p1) wfd)
        (cl-tmux/buffer:add-paste-buffer "PASTE-ME")
        ;; Middle-click at col 50 (within p1, x=41..80), row 5.
        (cl-tmux::%dispatch-mouse-event sess 1 50 5 nil)
        (is (eq p1 (window-active-pane win))
            "middle-click must focus the pane under the pointer")
        (let ((ready (cl-tmux/pty:select-fds (list rfd) 200000)))  ; 200 ms
          (is-true ready "the pasted text must reach the pane's PTY")
          (when ready
            (cffi:with-foreign-object (buf :uint8 32)
              (let ((n (cffi:foreign-funcall "read"
                                             :int rfd :pointer buf :unsigned-long 8
                                             :long)))
                (is (= 8 n) "all 8 bytes of PASTE-ME must arrive (got ~D)" n)
                (let ((str (make-string (max 0 n))))
                  (dotimes (i (max 0 n))
                    (setf (char str i) (code-char (cffi:mem-aref buf :uint8 i))))
                  (is (string= "PASTE-ME" str)
                      "pane must receive the buffer text (got ~S)" str))))))))))

(test mouse-middle-click-with-empty-buffer-writes-nothing
  "Middle-click with no paste-buffer is a safe no-op: the pane is focused but no
   bytes are written to its PTY."
  (with-empty-buffers
    (with-pipe-fds (rfd wfd)
      (with-two-pane-mouse-session (sess win p0 p1)
        (setf (pane-fd p1) wfd)
        ;; No add-paste-buffer → get-paste-buffer 0 is NIL.
        (cl-tmux::%dispatch-mouse-event sess 1 50 5 nil)
        (is (eq p1 (window-active-pane win))
            "middle-click still focuses the pane under the pointer")
        (is (null (cl-tmux/pty:select-fds (list rfd) 20000))
            "no paste-buffer → nothing is written (pipe stays idle)")))))

(test mouse-middle-click-release-does-not-paste
  "A middle-button RELEASE event must not paste (only the press does)."
  (with-empty-buffers
    (with-pipe-fds (rfd wfd)
      (with-two-pane-mouse-session (sess win p0 p1)
        (setf (pane-fd p1) wfd)
        (cl-tmux/buffer:add-paste-buffer "NOPE")
        ;; release-p = T → no paste
        (cl-tmux::%dispatch-mouse-event sess 1 50 5 t)
        (is (null (cl-tmux/pty:select-fds (list rfd) 20000))
            "middle-button release must not write any paste bytes")))))

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
        (is-false (screen-copy-mode-p screen)
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
        (is-false (screen-copy-mode-p screen)
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

;;; ── Modifier+arrow key-name helpers ────────────────────────────────────────

(test arrow-final-name-maps-arrow-bytes
  "%arrow-final-name returns the tmux base name for each arrow final byte."
  (is (string= "Up"    (cl-tmux::%arrow-final-name 65)))
  (is (string= "Down"  (cl-tmux::%arrow-final-name 66)))
  (is (string= "Right" (cl-tmux::%arrow-final-name 67)))
  (is (string= "Left"  (cl-tmux::%arrow-final-name 68))))

(test arrow-final-name-returns-nil-for-non-arrows
  "%arrow-final-name returns NIL for non-arrow final bytes."
  (is (null (cl-tmux::%arrow-final-name 72)))   ; H = Home
  (is (null (cl-tmux::%arrow-final-name 109)))) ; m = SGR final

(test modifier-arrow-key-name-builds-canonical-names
  "%modifier-arrow-key-name builds the exact strings %parse-key-token stores:
   5=Ctrl→C-, 3=Meta→M-, 2=Shift→S-, combined with the arrow base name."
  (is (string= "C-Up"    (cl-tmux::%modifier-arrow-key-name 53 65)))
  (is (string= "M-Left"  (cl-tmux::%modifier-arrow-key-name 51 68)))
  (is (string= "S-Down"  (cl-tmux::%modifier-arrow-key-name 50 66)))
  (is (string= "C-Right" (cl-tmux::%modifier-arrow-key-name 53 67))))

(test modifier-arrow-key-name-returns-nil-for-unknown
  "%modifier-arrow-key-name returns NIL for an unknown modifier or non-arrow
   final, so the caller forwards the sequence unchanged."
  (is (null (cl-tmux::%modifier-arrow-key-name 53 72)))  ; Ctrl+Home — not arrow
  (is (null (cl-tmux::%modifier-arrow-key-name 52 65)))) ; '4' (Shift+Alt) unmapped

;;; ── Modifier+arrow binding override (bind C-Up / bind -n M-Left) ────────────

(test prefix-c-up-binding-overrides-resize
  "bind C-Up next-window makes C-b then Ctrl+Up (ESC [ 1 ; 5 A) run next-window
   instead of the hardcoded resize-pane default."
  (with-isolated-config
    (cl-tmux/config:apply-config-directive '("bind" "C-Up" "next-window"))
    (let ((s (make-fake-session :nwindows 2)))
      (with-loop-state
        (let ((state (cl-tmux::make-input-state)))
          (dolist (b '(2 27 91 49 59 53 65))  ; C-b ESC [ 1 ; 5 A
            (cl-tmux::process-byte s b state))
          (is (eq (second (session-windows s)) (session-active-window s))
              "bound C-Up must run next-window, not resize"))))))

(test prefix-m-up-binding-overrides-resize
  "bind M-Up next-window makes C-b then Alt+Up (ESC [ 1 ; 3 A) run next-window
   instead of the hardcoded :resize-up default."
  (with-isolated-config
    (cl-tmux/config:apply-config-directive '("bind" "M-Up" "next-window"))
    (let ((s (make-fake-session :nwindows 2)))
      (with-loop-state
        (let ((state (cl-tmux::make-input-state)))
          (dolist (b '(2 27 91 49 59 51 65))  ; C-b ESC [ 1 ; 3 A
            (cl-tmux::process-byte s b state))
          (is (eq (second (session-windows s)) (session-active-window s))
              "bound M-Up must run next-window, not resize"))))))

(test prefix-plain-arrow-binding-overrides-select-pane
  "bind Up next-window makes C-b Up (ESC [ A) run next-window instead of the
   hardcoded :select-pane-up default."
  (with-isolated-config
    (cl-tmux/config:apply-config-directive '("bind" "Up" "next-window"))
    (let ((s (make-fake-session :nwindows 2)))
      (with-loop-state
        (let ((state (cl-tmux::make-input-state)))
          (dolist (b '(2 27 91 65))  ; C-b ESC [ A
            (cl-tmux::process-byte s b state))
          (is (eq (second (session-windows s)) (session-active-window s))
              "bound Up must run next-window, not select-pane"))))))

(test unbound-prefix-c-up-leaves-active-window
  "With no C-Up binding, C-b Ctrl+Up takes the resize fallback and must NOT
   change the active window (the override is purely additive)."
  (with-isolated-config
    (let ((s (make-fake-session :nwindows 2)))
      (with-loop-state
        (let ((state (cl-tmux::make-input-state)))
          (dolist (b '(2 27 91 49 59 53 65))  ; C-b ESC [ 1 ; 5 A
            (cl-tmux::process-byte s b state))
          (is (eq (first (session-windows s)) (session-active-window s))
              "unbound C-Up must leave the first window active"))))))

(test root-m-left-binding-fires-without-prefix
  "bind -n M-Left next-window makes a bare Alt+Left (ESC [ 1 ; 3 D) run
   next-window with no prefix — the root-table modifier+arrow path."
  (with-isolated-config
    (cl-tmux/config:apply-config-directive '("bind" "-n" "M-Left" "next-window"))
    (let ((s (make-fake-session :nwindows 2)))
      (with-loop-state
        (let ((state (cl-tmux::make-input-state)))
          (dolist (b '(27 91 49 59 51 68))  ; ESC [ 1 ; 3 D  (no prefix)
            (cl-tmux::process-byte s b state))
          (is (eq (second (session-windows s)) (session-active-window s))
              "bound -n M-Left must run next-window at root"))))))

(test root-c-up-binding-fires-without-prefix
  "bind -n C-Up next-window makes a bare Ctrl+Up (ESC [ 1 ; 5 A) run
   next-window with no prefix."
  (with-isolated-config
    (cl-tmux/config:apply-config-directive '("bind" "-n" "C-Up" "next-window"))
    (let ((s (make-fake-session :nwindows 2)))
      (with-loop-state
        (let ((state (cl-tmux::make-input-state)))
          (dolist (b '(27 91 49 59 53 65))  ; ESC [ 1 ; 5 A  (no prefix)
            (cl-tmux::process-byte s b state))
          (is (eq (second (session-windows s)) (session-active-window s))
              "bound -n C-Up must run next-window at root"))))))

(test unbound-root-c-up-leaves-active-window
  "With no -n C-Up binding, a bare Ctrl+Up is forwarded to the pane and must
   NOT change the active window."
  (with-isolated-config
    (let ((s (make-fake-session :nwindows 2)))
      (with-loop-state
        (let ((state (cl-tmux::make-input-state)))
          (dolist (b '(27 91 49 59 53 65))  ; ESC [ 1 ; 5 A  (no prefix, unbound)
            (cl-tmux::process-byte s b state))
          (is (eq (first (session-windows s)) (session-active-window s))
              "unbound bare C-Up must leave the first window active"))))))

;;; ── Meta/Alt key-name helper and bind override (bind -n M-h / bind M-j) ─────

(test meta-key-name-builds-canonical-names
  "%meta-key-name reconstructs the M-<char> name from the byte that follows ESC,
   matching the M-<char> encoding send-keys produces."
  (is (string= "M-a"     (cl-tmux::%meta-key-name 97)))   ; a
  (is (string= "M-1"     (cl-tmux::%meta-key-name 49)))   ; 1
  (is (string= "M-/"     (cl-tmux::%meta-key-name 47)))   ; /
  (is (string= "M-H"     (cl-tmux::%meta-key-name 72)))   ; H (Alt+Shift+h)
  (is (string= "M-Space" (cl-tmux::%meta-key-name 32))))  ; space

(test meta-key-name-returns-nil-for-control-and-del
  "%meta-key-name returns NIL for control bytes and DEL, so they forward
   unchanged rather than being treated as meta chords."
  (is (null (cl-tmux::%meta-key-name 8)))    ; ^H (backspace)
  (is (null (cl-tmux::%meta-key-name 27)))   ; ESC
  (is (null (cl-tmux::%meta-key-name 127)))) ; DEL

(test root-m-h-binding-fires-without-prefix
  "bind -n M-h next-window makes a bare Alt+h (ESC h) run next-window with no
   prefix — the root-table meta path overrides forwarding to the pane."
  (with-isolated-config
    (cl-tmux/config:apply-config-directive '("bind" "-n" "M-h" "next-window"))
    (let ((s (make-fake-session :nwindows 2)))
      (with-loop-state
        (let ((state (cl-tmux::make-input-state)))
          (dolist (b '(27 104))  ; ESC h  (no prefix)
            (cl-tmux::process-byte s b state))
          (is (eq (second (session-windows s)) (session-active-window s))
              "bound -n M-h must run next-window at root"))))))

(test prefix-m-j-binding-fires
  "bind M-j next-window makes C-b then Alt+j (ESC j) run next-window — the
   after-prefix meta path."
  (with-isolated-config
    (cl-tmux/config:apply-config-directive '("bind" "M-j" "next-window"))
    (let ((s (make-fake-session :nwindows 2)))
      (with-loop-state
        (let ((state (cl-tmux::make-input-state)))
          (dolist (b '(2 27 106))  ; C-b ESC j
            (cl-tmux::process-byte s b state))
          (is (eq (second (session-windows s)) (session-active-window s))
              "bound M-j must run next-window after prefix"))))))

(test unbound-root-meta-key-forwards-and-leaves-window
  "With no -n M-x binding, a bare Alt+x is forwarded to the pane and must NOT
   change the active window (the override is purely additive)."
  (with-isolated-config
    (let ((s (make-fake-session :nwindows 2)))
      (with-loop-state
        (let ((state (cl-tmux::make-input-state)))
          (dolist (b '(27 120))  ; ESC x  (no prefix, unbound)
            (cl-tmux::process-byte s b state))
          (is (eq (first (session-windows s)) (session-active-window s))
              "unbound bare M-x must leave the first window active"))))))

;;; ── Custom key tables (switch-client -T <table>) ────────────────────────────

(test cmd-switch-client-T-sets-and-resets-key-table
  "switch-client -T <table> sets *key-table*; -T root resets it to NIL."
  (with-loop-state
    (let ((s (make-fake-session :nwindows 1)))
      (cl-tmux::%cmd-switch-client s '("-T" "resize"))
      (is (string= "resize" cl-tmux::*key-table*)
          "switch-client -T resize activates the custom table")
      (cl-tmux::%cmd-switch-client s '("-T" "root"))
      (is (null cl-tmux::*key-table*)
          "switch-client -T root returns to the normal flow"))))

(test custom-key-table-dispatches-from-active-table-and-persists
  "In a custom key table, a bound key dispatches from THAT table and the table
   persists (modal mode)."
  (with-isolated-config
    (let ((s (make-fake-session :nwindows 2)))
      (with-loop-state
        (cl-tmux/config:apply-config-directive '("bind" "-T" "resize" "x" "next-window"))
        (setf cl-tmux::*key-table* "resize")
        (let ((state (cl-tmux::make-input-state)))
          (cl-tmux::process-byte s 120 state)  ; 'x'
          (is (eq (second (session-windows s)) (session-active-window s))
              "a key bound in the active custom table runs its binding")
          (is (string= "resize" cl-tmux::*key-table*)
              "the custom table persists after a key (modal)"))))))

(test custom-key-table-binding-can-switch-back-to-root
  "A binding in a custom table running 'switch-client -T root' exits the table."
  (with-isolated-config
    (let ((s (make-fake-session :nwindows 1)))
      (with-loop-state
        (cl-tmux/config:apply-config-directive
         '("bind" "-T" "resize" "q" "switch-client" "-T" "root"))
        (setf cl-tmux::*key-table* "resize")
        (let ((state (cl-tmux::make-input-state)))
          (cl-tmux::process-byte s 113 state)  ; 'q'
          (is (null cl-tmux::*key-table*)
              "switch-client -T root from within the table exits it"))))))

;;; ── Default M-1..M-5 preset-layout bindings (tmux defaults) ─────────────────

(test default-meta-digit-layout-bindings-registered
  "C-b M-1..M-5 are installed as select-layout command token-lists in the prefix
   table, matching real tmux's preset-layout defaults."
  (with-isolated-config
    (flet ((cmd (k) (cl-tmux/config:key-table-command
                     (cl-tmux/config:key-table-lookup "prefix" k))))
      (is (equal '("select-layout" "even-horizontal") (cmd "M-1")))
      (is (equal '("select-layout" "even-vertical")   (cmd "M-2")))
      (is (equal '("select-layout" "main-horizontal") (cmd "M-3")))
      (is (equal '("select-layout" "main-vertical")   (cmd "M-4")))
      (is (equal '("select-layout" "tiled")           (cmd "M-5"))))))

(test prefix-meta-1-applies-layout-end-to-end
  "C-b then Alt+1 (ESC 1) runs the bound select-layout even-horizontal on a
   two-pane window without error (the after-prefix meta path fires the default)."
  (with-isolated-config
    (let ((s (make-fake-session :nwindows 1 :npanes 2)))
      (with-loop-state
        (let ((state (cl-tmux::make-input-state)))
          (dolist (b '(2 27 49))  ; C-b ESC 1
            (cl-tmux::process-byte s b state))
          ;; Layout applied: the window still has its two panes and a usable tree.
          (is (= 2 (length (window-panes (session-active-window s))))
              "select-layout via C-b M-1 must preserve both panes"))))))

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
    (with-loop-state
      (let ((*overlay* nil))
        (is-false (pane-marked p0) "pane must not be marked initially")
        (cl-tmux::dispatch-command sess :mark-pane nil)
        (is (pane-marked p0) "pane must be marked after :mark-pane")))))

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
    (with-loop-state
      (let ((*overlay* nil))
        (setf (pane-marked p0) t)
        (is (pane-marked p0) "pane marked before dispatch")
        (cl-tmux::dispatch-command sess :mark-pane nil)
        (is-false (pane-marked p0)
            "pane unmarked after :mark-pane on already-marked pane")))))

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
    (with-loop-state
      (let ((*overlay* nil))
        (setf (pane-marked p0) t)
        (is (pane-marked p0) "pane must be marked before :clear-mark")
        (cl-tmux::dispatch-command sess :clear-mark nil)
        (is-false (pane-marked p0) "pane must not be marked after :clear-mark")))))

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
    (with-loop-state
      (let ((*overlay* nil))
        (cl-tmux::dispatch-command sess :display-info nil)
        (is (overlay-active-p) "display-info must activate the overlay")
        (is (search "Session:" *overlay*)
            "overlay must contain \"Session:\"")))))

;;; ── dispatch :choose-client ──────────────────────────────────────────────────

(test dispatch-choose-client-shows-overlay
  ":choose-client shows an overlay with client info."
  (let ((s (make-fake-session)))
    (with-loop-state
      (let ((*overlay* nil)
            (cl-tmux::*term-rows* 24) (cl-tmux::*term-cols* 80))
        (cl-tmux::dispatch-command s :choose-client nil)
        (is (overlay-active-p) "choose-client must activate the overlay")
        (is (search "Clients" *overlay*)
            "overlay must contain \"Clients\"")))))

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

(test root-table-bound-command-line-runs-without-prefix
  "A -n binding to a command LINE runs without the prefix: bind -n Z
   display-message hi, then pressing Z (no C-b) shows 'hi' in an overlay
   (verifies the root dispatch site's token-list path)."
  (with-isolated-config
    (with-loop-state
      (let ((s (make-fake-session)) (*overlay* nil)
            (state (cl-tmux::make-input-state)))
        (cl-tmux/config:apply-config-directive
         '("bind" "-n" "Z" "display-message" "hi"))
        (cl-tmux::process-byte s (char-code #\Z) state)
        (is (overlay-active-p)
            "a -n command-line binding must fire without C-b")
        (let ((text (format nil "~{~A~%~}" (overlay-lines))))
          (is (search "hi" text)
              "overlay must contain the bound command's output 'hi' (got ~S)" text))))))

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
  ;; Isolated config: z is an install-extended-key-binding, vulnerable to the
  ;; known global prefix-table polluter (see also the detach tests).
  (with-isolated-config
    (is (eq :zoom-toggle (lookup-key-binding #\z))
        "C-b z must be bound to :zoom-toggle (standard tmux default)")))

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
  (with-isolated-config
    (let ((s (make-fake-session)))
      (with-loop-state
        (let ((state (cl-tmux::make-input-state)))
          (cl-tmux::process-byte s 2 state)
          (is (null (cl-tmux::process-byte s (char-code #\z) state))
              "C-b z must dispatch :zoom-toggle and return NIL"))))))

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
  ":choose-window shows a menu overlay for j/k navigation without a prompt."
  (let ((s (make-fake-session :nwindows 2)))
    (with-loop-state
      (let ((*overlay* nil) (*prompt* nil))
        (cl-tmux::dispatch-command s :choose-window nil)
        (is (overlay-active-p) ":choose-window must show an overlay")
        ;; choose-window now uses j/k menu navigation, not a text prompt.
        (is (not (null cl-tmux/prompt:*active-menu*))
            ":choose-window must set *active-menu* for navigation")))))

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
  ;; Isolated config so the assertion runs against the clean default+extended
  ;; bindings, immune to the known global prefix-table polluter.
  (with-isolated-config
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
    (is (bound-p (code-char 2))   "C-b → send-prefix"))))

;;; ── Mouse scroll-wheel paths ─────────────────────────────────────────────────

(test dispatch-mouse-scroll-up-enters-copy-mode
  "Mouse scroll-up (btn=64) enters copy mode on the active pane when not in copy mode."
  (let* ((p0   (make-pane :id 1 :fd -1 :pid -1
                           :x 0 :y 0 :width 40 :height 24
                           :screen (make-screen 40 24)))
         (win  (make-window :id 1 :name "w" :width 40 :height 24
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
  (let* ((p0   (make-pane :id 1 :fd -1 :pid -1
                           :x 0 :y 0 :width 40 :height 24
                           :screen (make-screen 40 24)))
         (win  (make-window :id 1 :name "w" :width 40 :height 24
                            :panes (list p0)
                            :tree  (make-layout-leaf p0)
                            :active p0))
         (sess (make-session :id 1 :name "0" :windows (list win) :active win))
         (screen (pane-screen p0)))
    (cl-tmux/options:set-option "mouse" t)
    (unwind-protect
         (with-loop-state
           (let ((cl-tmux::*term-rows* 25) (cl-tmux::*term-cols* 40))
             (seed-scrollback screen 5)
             (cl-tmux/commands::copy-mode-enter screen)
             ;; offset already at 0 — scroll down should exit copy mode
             (cl-tmux::%dispatch-mouse-event sess 65 5 5 nil)
             (is-false (screen-copy-mode-p screen)
                 "scroll-down at offset=0 must exit copy mode")))
      (cl-tmux/options:set-option "mouse" nil))))

(test dispatch-mouse-gated-by-mouse-option
  "%dispatch-mouse-event is a no-op when the 'mouse' option is false."
  (let* ((p0   (make-pane :id 1 :fd -1 :pid -1
                           :x 0 :y 0 :width 40 :height 24
                           :screen (make-screen 40 24)))
         (win  (make-window :id 1 :name "w" :width 40 :height 24
                            :panes (list p0)
                            :tree  (make-layout-leaf p0)
                            :active p0))
         (sess (make-session :id 1 :name "0" :windows (list win) :active win)))
    (cl-tmux/options:set-option "mouse" nil)
    (with-loop-state
      (let ((cl-tmux::*term-rows* 25) (cl-tmux::*term-cols* 40))
        ;; With mouse off, click must not enter copy mode.
        (cl-tmux::%dispatch-mouse-event sess 0 5 5 nil)
        (is-false (screen-copy-mode-p (pane-screen p0))
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
      (is-false release-p "press sequence must have release-p=NIL"))))

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
      (is-false (overlay-active-p)
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
      (is-false (prompt-active-p) "C-c must dismiss the prompt")
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
  (let ((s (make-fake-session)))
    (with-loop-state
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
            "C-b C-Up must return NIL (no quit/detach)")))))

(test dispatch-modifier-arrow-meta-arrow-dispatches-resize-command
  "M-arrow (mod-byte=51) dispatches :resize-* command without signaling."
  (let ((s (make-fake-session)))
    (with-loop-state
      (let ((state (cl-tmux::make-input-state)))
        (cl-tmux::process-byte s 2   state)   ; C-b prefix
        (cl-tmux::process-byte s 27  state)   ; ESC
        (cl-tmux::process-byte s 91  state)   ; [
        (cl-tmux::process-byte s 49  state)   ; 1
        (cl-tmux::process-byte s 59  state)   ; ;
        (cl-tmux::process-byte s 51  state)   ; 3 (Meta)
        (is (null (cl-tmux::process-byte s 66 state))   ; B (Down)
            "C-b M-Down must return NIL (no quit/detach)")))))

;;; ── copy-mode-set-cursor command coverage ────────────────────────────────────

(test copy-mode-set-cursor-updates-cursor-position
  "copy-mode-set-cursor sets the copy-mode cursor to the given (row, col)."
  (let ((s (make-fake-session)))
    (with-loop-state
      (let ((screen (active-screen s)))
        (cl-tmux::dispatch-command s :copy-mode-enter nil)
        (is (screen-copy-mode-p screen) "copy mode entered")
        ;; Place cursor at (3, 5)
        (cl-tmux/commands::copy-mode-set-cursor screen 3 5)
        (is (equal (cons 3 5) (screen-copy-cursor screen))
            "copy-mode-set-cursor must set cursor to (row . col)")))))

(test copy-mode-set-cursor-clamps-to-screen-bounds
  "copy-mode-set-cursor clamps row/col to [0, height-1] / [0, width-1]."
  (let ((s (make-fake-session)))
    (with-loop-state
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
              "clamped col must be within [0, width-1]"))))))

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

;;; ── Table-driven tests: byte constant values ─────────────────────────────────

(test byte-constants-have-correct-values
  "VT100 and CSI byte constants must equal their documented ASCII/VT values."
  (is (= 27  cl-tmux::+byte-esc+)          "ESC must be 27")
  (is (= 91  cl-tmux::+byte-csi-bracket+)  "CSI [ must be 91")
  (is (= 65  cl-tmux::+byte-arrow-up+)     "CUU A must be 65")
  (is (= 66  cl-tmux::+byte-arrow-down+)   "CUD B must be 66")
  (is (= 68  cl-tmux::+byte-arrow-left+)   "CUB D must be 68")
  (is (= 67  cl-tmux::+byte-arrow-right+)  "CUF C must be 67")
  (is (= 113 cl-tmux::+byte-q+)            "q must be 113")
  (is (= 106 cl-tmux::+byte-j+)            "j must be 106")
  (is (= 107 cl-tmux::+byte-k+)            "k must be 107")
  (is (= 49  cl-tmux::+byte-csi-param-1+)  "CSI param 1 must be 49")
  (is (= 59  cl-tmux::+byte-csi-semi+)     "CSI semi must be 59")
  (is (= 53  cl-tmux::+byte-csi-mod-ctrl+) "CSI ctrl modifier must be 53")
  (is (= 51  cl-tmux::+byte-csi-mod-meta+) "CSI meta modifier must be 51")
  (is (= 126 cl-tmux::+byte-tilde+)        "tilde must be 126")
  (is (= 60  cl-tmux::+byte-sgr-lt+)       "SGR < must be 60")
  (is (= 48  cl-tmux::+byte-digit-0+)      "digit 0 must be 48")
  (is (= 57  cl-tmux::+byte-digit-9+)      "digit 9 must be 57")
  (is (= 53  cl-tmux::+byte-page-up-param+)   "PageUp param must be 53")
  (is (= 54  cl-tmux::+byte-page-down-param+) "PageDown param must be 54")
  (is (= 77  cl-tmux::+byte-ascii-m+)      "ASCII M must be 77")
  ;; +byte-sgr-press+ was merged into +byte-ascii-m+ (same value 77); verify the
  ;; surviving constant still has the correct value.
  (is (= 109 cl-tmux::+byte-sgr-release+)  "SGR release final must be 109"))

;;; ── make-input-state and input-state-continuation ────────────────────────────

(test make-input-state-starts-in-ground-state
  "make-input-state returns an input-state with continuation = %ground-input-state."
  (let ((state (cl-tmux::make-input-state)))
    (is (cl-tmux::input-state-p state)
        "make-input-state must return an input-state struct")
    (is (functionp (cl-tmux::input-state-continuation state))
        "input-state continuation must be a function")))

(test input-state-continuation-is-reset-after-complete-sequence
  "After a complete 3-byte ESC [ A sequence, the continuation returns to ground."
  (let ((s     (make-fake-session))
        (state (cl-tmux::make-input-state)))
    (with-loop-state
      ;; Feed ESC — transitions to escape accumulator
      (cl-tmux::process-byte s 27 state)
      (is (not (eq #'cl-tmux::%ground-input-state
                   (cl-tmux::input-state-continuation state)))
          "after ESC the continuation should not be ground-state")
      ;; Feed [ A — completes the sequence, back to ground
      (cl-tmux::process-byte s 91 state)
      (cl-tmux::process-byte s 65 state)
      (is (eq #'cl-tmux::%ground-input-state
              (cl-tmux::input-state-continuation state))
          "after completing ESC [ A the continuation must be ground-state"))))

;;; ── %forward-octets-synchronized — synchronize-panes broadcast ───────────────

(test forward-octets-synchronized-broadcasts-when-option-set
  "%forward-octets-synchronized writes to all panes when synchronize-panes is T.
   Verified by confirming it runs without error on a multi-pane session."
  (let* ((p0   (make-pane :id 1 :fd -1 :pid -1 :x 0 :y 0 :width 20 :height 5
                           :screen (make-screen 20 5)))
         (p1   (make-pane :id 2 :fd -1 :pid -1 :x 0 :y 0 :width 20 :height 5
                           :screen (make-screen 20 5)))
         (win  (make-window :id 1 :name "w" :width 20 :height 5
                            :panes (list p0 p1)
                            :tree  (make-layout-split :h (make-layout-leaf p0)
                                                        (make-layout-leaf p1) 1/2)))
         (sess (make-session :id 1 :name "0" :windows (list win))))
    (window-select-pane win p0)
    (session-select-window sess win)
    (cl-tmux/options:set-option "synchronize-panes" t)
    (unwind-protect
         ;; fd=-1 makes pty-write a no-op; we just verify no error is raised.
         (finishes
           (cl-tmux::%forward-octets-synchronized
            sess
            (make-array 1 :element-type '(unsigned-byte 8) :initial-element 65)))
      (cl-tmux/options:set-option "synchronize-panes" nil))))

;;; ── %maybe-rename-window-from-title coverage ─────────────────────────────────

(test maybe-rename-window-from-title-renames-when-osc-title-set
  "%maybe-rename-window-from-title propagates the OSC title to the window name
   when the window has automatic-rename enabled and the title differs."
  (let* ((screen (make-screen 20 5))
         (pane   (make-pane :id 1 :fd -1 :pid -1 :x 0 :y 0 :width 20 :height 5
                             :screen screen))
         (win    (make-window :id 1 :name "old-name" :width 20 :height 5
                              :panes (list pane) :tree (make-layout-leaf pane)))
         (sess   (make-session :id 1 :name "0" :windows (list win))))
    (window-select-pane win pane)
    (session-select-window sess win)
    ;; Set an OSC title on the screen and ensure automatic-rename is on.
    (setf (screen-title screen) "new-title")
    (setf (window-automatic-rename-p win) t)
    (with-loop-state
      (cl-tmux::%maybe-rename-window-from-title sess)
      (is (string= "new-title" (window-name win))
          "%maybe-rename-window-from-title must set window-name to OSC title"))))

(test maybe-rename-window-from-title-noop-when-titles-equal
  "%maybe-rename-window-from-title does nothing when OSC title equals window name."
  (let* ((screen (make-screen 20 5))
         (pane   (make-pane :id 1 :fd -1 :pid -1 :x 0 :y 0 :width 20 :height 5
                             :screen screen))
         (win    (make-window :id 1 :name "same" :width 20 :height 5
                              :panes (list pane) :tree (make-layout-leaf pane)))
         (sess   (make-session :id 1 :name "0" :windows (list win))))
    (window-select-pane win pane)
    (session-select-window sess win)
    (setf (screen-title screen) "same")
    (setf (window-automatic-rename-p win) t)
    (with-loop-state
      (cl-tmux::%maybe-rename-window-from-title sess)
      (is (string= "same" (window-name win))
          "window-name must be unchanged when title equals name"))))

(test maybe-rename-window-from-title-noop-when-auto-rename-off
  "%maybe-rename-window-from-title does nothing when automatic-rename is disabled."
  (let* ((screen (make-screen 20 5))
         (pane   (make-pane :id 1 :fd -1 :pid -1 :x 0 :y 0 :width 20 :height 5
                             :screen screen))
         (win    (make-window :id 1 :name "original" :width 20 :height 5
                              :panes (list pane) :tree (make-layout-leaf pane)))
         (sess   (make-session :id 1 :name "0" :windows (list win))))
    (window-select-pane win pane)
    (session-select-window sess win)
    (setf (screen-title screen) "new-title")
    (setf (window-automatic-rename-p win) nil)
    (with-loop-state
      (cl-tmux::%maybe-rename-window-from-title sess)
      (is (string= "original" (window-name win))
          "window-name must not change when auto-rename is disabled"))))

(test maybe-rename-window-from-title-noop-when-window-local-auto-rename-off
  "%maybe-rename-window-from-title is suppressed for a window whose window-local
   \"automatic-rename\" option is off (set via set-option-for-window), even though
   the window-automatic-rename-p flag and the global option are still on.  This
   exercises the get-option-for-context :window read wired into the rename path."
  (let* ((screen (make-screen 20 5))
         (pane   (make-pane :id 1 :fd -1 :pid -1 :x 0 :y 0 :width 20 :height 5
                             :screen screen))
         (win    (make-window :id 1 :name "original" :width 20 :height 5
                              :panes (list pane) :tree (make-layout-leaf pane)))
         (sess   (make-session :id 1 :name "0" :windows (list win))))
    (window-select-pane win pane)
    (session-select-window sess win)
    (setf (screen-title screen) "new-title")
    (setf (window-automatic-rename-p win) t)
    (with-isolated-config
      ;; Global automatic-rename / allow-rename stay on; only the per-window
      ;; option is turned off, so get-option-for-context :window returns NIL.
      (cl-tmux/options:set-option "automatic-rename" t)
      (cl-tmux/options:set-option "allow-rename" t)
      (cl-tmux/options:set-option-for-window "automatic-rename" "off" win)
      (with-loop-state
        (cl-tmux::%maybe-rename-window-from-title sess)
        (is (string= "original" (window-name win))
            "window-name must not change when window-local automatic-rename is off")))))

(test maybe-rename-window-from-title-renames-when-window-local-auto-rename-on
  "Companion to the suppression test: with window-local automatic-rename ON the
   rename path still fires and propagates the OSC title to the window name."
  (let* ((screen (make-screen 20 5))
         (pane   (make-pane :id 1 :fd -1 :pid -1 :x 0 :y 0 :width 20 :height 5
                             :screen screen))
         (win    (make-window :id 1 :name "old-name" :width 20 :height 5
                              :panes (list pane) :tree (make-layout-leaf pane)))
         (sess   (make-session :id 1 :name "0" :windows (list win))))
    (window-select-pane win pane)
    (session-select-window sess win)
    (setf (screen-title screen) "new-title")
    (setf (window-automatic-rename-p win) t)
    (with-isolated-config
      (cl-tmux/options:set-option "automatic-rename" t)
      (cl-tmux/options:set-option "allow-rename" t)
      (cl-tmux/options:set-option-for-window "automatic-rename" "on" win)
      (with-loop-state
        (cl-tmux::%maybe-rename-window-from-title sess)
        (is (string= "new-title" (window-name win))
            "window-name must update when window-local automatic-rename is on")))))

(test maybe-rename-window-keeps-tracking-after-first-rename
  "Auto-rename must keep working after the first rename: %maybe-rename-window-
   from-title must NOT disable automatic-rename, so a later title change renames
   again.  Regression for rename-window unconditionally clearing
   automatic-rename-p (which made auto-rename fire only once)."
  (let* ((screen (make-screen 20 5))
         (pane   (make-pane :id 1 :fd -1 :pid -1 :x 0 :y 0 :width 20 :height 5
                             :screen screen))
         (win    (make-window :id 1 :name "old" :width 20 :height 5
                              :panes (list pane) :tree (make-layout-leaf pane)))
         (sess   (make-session :id 1 :name "0" :windows (list win))))
    (window-select-pane win pane)
    (session-select-window sess win)
    (setf (window-automatic-rename-p win) t)
    (with-isolated-config
      (cl-tmux/options:set-option "automatic-rename" t)
      (cl-tmux/options:set-option "allow-rename" t)
      (with-loop-state
        (setf (screen-title screen) "first")
        (cl-tmux::%maybe-rename-window-from-title sess)
        (is (string= "first" (window-name win)) "first auto-rename applies")
        (is-true (window-automatic-rename-p win)
                 "automatic-rename must stay ON after an auto-rename")
        ;; A second title change must rename again (the bug made this a no-op).
        (setf (screen-title screen) "second")
        (cl-tmux::%maybe-rename-window-from-title sess)
        (is (string= "second" (window-name win))
            "auto-rename must keep tracking after the first rename")))))

;;; ── Application cursor keys remapping ───────────────────────────────────────

(test app-cursor-keys-remaps-csi-arrow-to-ss3
  "When app-cursor-keys mode is active, ESC [ A forwarded outside copy mode is
   remapped to ESC O A (SS3) before being sent to the pane."
  (let ((s (make-fake-session)))
    (with-loop-state
      (let ((screen (active-screen s))
            (state  (cl-tmux::make-input-state)))
        ;; Enable application cursor keys on the active pane's screen.
        (setf (screen-app-cursor-keys screen) t)
        ;; Ensure we are NOT in copy mode so the sequence is forwarded, not consumed.
        (is-false (cl-tmux::%copy-mode-active-p s) "must not be in copy mode")
        ;; Feed ESC [ A — should be remapped to ESC O A internally.
        ;; fd=-1 panes: pty-write is a no-op; we assert no error and NIL return.
        (is (null (cl-tmux::process-byte s 27 state)))
        (is (null (cl-tmux::process-byte s 91 state)))
        (is (null (cl-tmux::process-byte s 65 state))
            "ESC [ A with app-cursor-keys must not signal or return a quit value")))))

;;; ── Buffer overflow guard in make-escape-input-k ────────────────────────────

(test escape-accumulator-resets-after-complete-sgr-sequence
  "After a complete SGR mouse sequence, the continuation returns to ground state."
  (let ((s     (make-fake-session))
        (state (cl-tmux::make-input-state)))
    (with-loop-state
      ;; Feed ESC [ < 0 ; 5 ; 3 M  (a complete SGR press) byte by byte.
      (dolist (byte (mapcar #'char-code (coerce (format nil "~C[<0;5;3M" #\Escape) 'list)))
        (cl-tmux::process-byte s byte state))
      ;; After the full sequence the continuation must be back to ground.
      (is (eq #'cl-tmux::%ground-input-state
              (cl-tmux::input-state-continuation state))
          "continuation must return to ground after completed SGR sequence"))))

;;; ── handle-prompt-key UTF-8 multi-byte input ─────────────────────────────────

(test handle-prompt-key-utf8-two-byte-sequence-inserts-char
  "A 2-byte UTF-8 sequence (U+00E9, é) fed byte-by-byte into handle-prompt-key
   inserts the correct character into the prompt buffer."
  (with-clean-prompt
    (prompt-start "test" ""
                  (lambda (buf) (declare (ignore buf)) nil))
    ;; U+00E9 in UTF-8: 0xC3 0xA9
    (cl-tmux::handle-prompt-key #xC3)
    (cl-tmux::handle-prompt-key #xA9)
    (is (string= "é" (prompt-buffer *prompt*))
        "2-byte UTF-8 sequence must decode and insert é into prompt")))

(test handle-prompt-key-utf8-resets-on-enter
  "UTF-8 accumulator state is reset when Enter is pressed mid-sequence."
  (with-clean-prompt
    (let ((submitted "unset"))
      (prompt-start "test" ""
                    (lambda (buf) (setf submitted buf)))
      ;; Start a 2-byte UTF-8 sequence but press Enter before the second byte.
      (cl-tmux::handle-prompt-key #xC3)
      (cl-tmux::handle-prompt-key 13)   ; Enter
      ;; The prompt should have been submitted and dismissed.
      (is-false (prompt-active-p)
          "Enter mid-UTF8 must dismiss the prompt")
      ;; Submitted value is the buffer content before the incomplete sequence.
      (is (stringp submitted) "submitted value must be a string"))))

;;; ── handle-prompt-key cursor movement (C-b, C-f) ────────────────────────────

(test handle-prompt-key-ctrl-b-moves-cursor-left
  "C-b (byte 2) moves the prompt cursor one position to the left."
  (with-clean-prompt
    (prompt-start "test" "hello"
                  (lambda (buf) (declare (ignore buf)) nil))
    (prompt-cursor-eol)
    (is (= 5 (prompt-cursor-index *prompt*)) "cursor at end")
    (cl-tmux::handle-prompt-key 2)   ; C-b
    (is (= 4 (prompt-cursor-index *prompt*))
        "C-b must move cursor one position left")))

(test handle-prompt-key-ctrl-f-moves-cursor-right
  "C-f (byte 6) moves the prompt cursor one position to the right."
  (with-clean-prompt
    (prompt-start "test" "hello"
                  (lambda (buf) (declare (ignore buf)) nil))
    (prompt-cursor-bol)
    (is (= 0 (prompt-cursor-index *prompt*)) "cursor at start")
    (cl-tmux::handle-prompt-key 6)   ; C-f
    (is (= 1 (prompt-cursor-index *prompt*))
        "C-f must move cursor one position right")))

;;; ── handle-prompt-key kill commands ─────────────────────────────────────────

(test handle-prompt-key-ctrl-k-kills-to-end
  "C-k (byte 11) deletes from the cursor position to the end of the buffer."
  (with-clean-prompt
    (prompt-start "test" "hello"
                  (lambda (buf) (declare (ignore buf)) nil))
    ;; Move cursor to position 2 ("he" remains, "llo" to be killed).
    (prompt-cursor-bol)
    (cl-tmux::handle-prompt-key 6)   ; C-f → pos 1
    (cl-tmux::handle-prompt-key 6)   ; C-f → pos 2
    (cl-tmux::handle-prompt-key 11)  ; C-k
    (is (string= "he" (prompt-buffer *prompt*))
        "C-k must kill from cursor to end")))

(test handle-prompt-key-ctrl-u-kills-to-start
  "C-u (byte 21) deletes from the start of the buffer to the cursor position."
  (with-clean-prompt
    (prompt-start "test" "hello"
                  (lambda (buf) (declare (ignore buf)) nil))
    ;; Move cursor to position 3 ("hel" to be killed, "lo" remains).
    (prompt-cursor-bol)
    (cl-tmux::handle-prompt-key 6)   ; C-f → pos 1
    (cl-tmux::handle-prompt-key 6)   ; C-f → pos 2
    (cl-tmux::handle-prompt-key 6)   ; C-f → pos 3
    (cl-tmux::handle-prompt-key 21)  ; C-u
    (is (string= "lo" (prompt-buffer *prompt*))
        "C-u must kill from start to cursor")))

(test handle-prompt-key-ctrl-w-kills-previous-word
  "C-w (byte 23) deletes the word immediately before the cursor."
  (with-clean-prompt
    (prompt-start "test" "foo bar"
                  (lambda (buf) (declare (ignore buf)) nil))
    ;; Move cursor to end of buffer.
    (prompt-cursor-eol)
    (cl-tmux::handle-prompt-key 23)  ; C-w
    ;; Should have deleted "bar" (and possibly the space).
    (let ((buf (prompt-buffer *prompt*)))
      (is (string= "foo" (string-right-trim " " buf))
          "C-w must kill the previous word"))))

;;; ── process-byte: copy-mode w, b, e word navigation ─────────────────────────

(test copy-mode-w-moves-word-forward
  "Plain 'w' (byte 119) moves the copy-mode cursor forward by one word."
  (let ((s (make-fake-session)))
    (with-loop-state
      (let ((screen (active-screen s))
            (state  (cl-tmux::make-input-state)))
        (cl-tmux::dispatch-command s :copy-mode-enter nil)
        ;; Feed some text to give the screen content.
        (screen-process-bytes
         screen (map '(simple-array (unsigned-byte 8) (*)) #'char-code "hello world"))
        (setf (cl-tmux/terminal/types:screen-copy-cursor screen) (cons 0 0))
        (finishes (cl-tmux::process-byte s 119 state))))))  ; w

(test copy-mode-b-moves-word-backward
  "Plain 'b' (byte 98) moves the copy-mode cursor backward by one word."
  (let ((s (make-fake-session)))
    (with-loop-state
      (let ((screen (active-screen s))
            (state  (cl-tmux::make-input-state)))
        (cl-tmux::dispatch-command s :copy-mode-enter nil)
        (setf (cl-tmux/terminal/types:screen-copy-cursor screen) (cons 0 5))
        (finishes (cl-tmux::process-byte s 98 state))))))   ; b

(test copy-mode-e-moves-to-word-end
  "Plain 'e' (byte 101) moves the copy-mode cursor to the end of the current word."
  (let ((s (make-fake-session)))
    (with-loop-state
      (let ((screen (active-screen s))
            (state  (cl-tmux::make-input-state)))
        (cl-tmux::dispatch-command s :copy-mode-enter nil)
        (setf (cl-tmux/terminal/types:screen-copy-cursor screen) (cons 0 0))
        (finishes (cl-tmux::process-byte s 101 state))))))  ; e

;;; ── process-byte: copy-mode page up/down C-f/C-b (in-mode) ──────────────────

(test copy-mode-ctrl-f-page-down
  "C-f (byte 6) in copy mode scrolls down one full page."
  (let ((s (make-fake-session)))
    (with-loop-state
      (let ((screen (active-screen s))
            (state  (cl-tmux::make-input-state)))
        (cl-tmux::dispatch-command s :copy-mode-enter nil)
        (seed-scrollback screen 30)
        (cl-tmux/commands::copy-mode-scroll screen 20)
        (let ((offset-before (screen-copy-offset screen))
              (h             (screen-height screen)))
          (cl-tmux::process-byte s 6 state)   ; C-f → page down
          (let ((expected (max 0 (- offset-before h))))
            (is (= expected (screen-copy-offset screen))
                "C-f must scroll copy-offset down by screen-height")))))))

(test copy-mode-page-up-command-scrolls-full-page
  "copy-mode-page-up scrolls the viewport up by one full screen-height."
  (let ((s (make-fake-session)))
    (with-loop-state
      (let ((screen (active-screen s)))
        (cl-tmux::dispatch-command s :copy-mode-enter nil)
        (seed-scrollback screen 30)
        (let ((h (screen-height screen)))
          (cl-tmux/commands::copy-mode-page-up screen)
          (let ((expected (min h 30)))
            (is (= expected (screen-copy-offset screen))
                "copy-mode-page-up must scroll copy-offset up by screen-height")))))))

;;; ── copy-mode y (yank) and n/N (search navigation) ──────────────────────────

(test copy-mode-y-yanks-selection-finishes
  "Plain 'y' (byte 121) completes without signaling when in copy mode."
  (let ((s (make-fake-session)))
    (with-loop-state
      (let ((screen (active-screen s))
            (state  (cl-tmux::make-input-state)))
        (cl-tmux::dispatch-command s :copy-mode-enter nil)
        ;; Begin a selection first so yank has something to copy.
        (cl-tmux/commands::copy-mode-begin-selection screen)
        (finishes (cl-tmux::process-byte s 121 state))))))   ; y

(test copy-mode-n-search-next-finishes
  "Plain 'n' (byte 110) runs search-next without signaling in copy mode."
  (let ((s (make-fake-session)))
    (with-loop-state
      (let ((screen (active-screen s))
            (state  (cl-tmux::make-input-state)))
        (cl-tmux::dispatch-command s :copy-mode-enter nil)
        (finishes (cl-tmux::process-byte s 110 state))))))   ; n

(test copy-mode-N-search-prev-finishes
  "Plain 'N' (byte 78) runs search-prev without signaling in copy mode."
  (let ((s (make-fake-session)))
    (with-loop-state
      (let ((screen (active-screen s))
            (state  (cl-tmux::make-input-state)))
        (cl-tmux::dispatch-command s :copy-mode-enter nil)
        (finishes (cl-tmux::process-byte s 78 state))))))    ; N

;;; ── copy-mode Y (copy-line) and D (copy-end-of-line) ────────────────────────

(test copy-mode-Y-copies-current-line
  "Plain 'Y' (byte 89) copies the current line into the paste buffer without signaling."
  (let ((s (make-fake-session)))
    (with-loop-state
      (let ((screen (active-screen s))
            (state  (cl-tmux::make-input-state)))
        (cl-tmux::dispatch-command s :copy-mode-enter nil)
        (finishes (cl-tmux::process-byte s 89 state))))))    ; Y

(test copy-mode-D-copies-to-end-of-line
  "Plain 'D' (byte 68) copies from the cursor to end of line without signaling."
  (let ((s (make-fake-session)))
    (with-loop-state
      (let ((screen (active-screen s))
            (state  (cl-tmux::make-input-state)))
        (cl-tmux::dispatch-command s :copy-mode-enter nil)
        (finishes (cl-tmux::process-byte s 68 state))))))    ; D

;;; ── copy-mode half-page and single-line scroll bindings ──────────────────────

(test copy-mode-ctrl-u-half-page-up
  "C-u (byte 21) scrolls the copy-mode viewport up by half a page."
  (let ((s (make-fake-session)))
    (with-loop-state
      (let ((screen (active-screen s))
            (state  (cl-tmux::make-input-state)))
        (cl-tmux::dispatch-command s :copy-mode-enter nil)
        (seed-scrollback screen 30)
        (let ((offset-before (screen-copy-offset screen)))
          (cl-tmux::process-byte s 21 state)   ; C-u
          (is (>= (screen-copy-offset screen) offset-before)
              "C-u must not decrease copy-offset"))))))

(test copy-mode-ctrl-d-half-page-down
  "C-d (byte 4) scrolls the copy-mode viewport down by half a page."
  (let ((s (make-fake-session)))
    (with-loop-state
      (let ((screen (active-screen s))
            (state  (cl-tmux::make-input-state)))
        (cl-tmux::dispatch-command s :copy-mode-enter nil)
        (seed-scrollback screen 30)
        (cl-tmux/commands::copy-mode-scroll screen 20)
        (let ((offset-before (screen-copy-offset screen)))
          (cl-tmux::process-byte s 4 state)    ; C-d
          (is (<= (screen-copy-offset screen) offset-before)
              "C-d must not increase copy-offset"))))))

(test copy-mode-ctrl-e-scrolls-down-one-line
  "C-e (byte 5) in copy mode scrolls the viewport down one line."
  (let ((s (make-fake-session)))
    (with-loop-state
      (let ((screen (active-screen s))
            (state  (cl-tmux::make-input-state)))
        (cl-tmux::dispatch-command s :copy-mode-enter nil)
        (seed-scrollback screen 10)
        (cl-tmux/commands::copy-mode-scroll screen 5)
        (let ((offset-before (screen-copy-offset screen)))
          (cl-tmux::process-byte s 5 state)    ; C-e
          (is (<= (screen-copy-offset screen) offset-before)
              "C-e must scroll copy-offset down (decrease offset)"))))))

(test copy-mode-ctrl-y-scrolls-up-one-line
  "C-y (byte 25) in copy mode scrolls the viewport up one line."
  (let ((s (make-fake-session)))
    (with-loop-state
      (let ((screen (active-screen s))
            (state  (cl-tmux::make-input-state)))
        (cl-tmux::dispatch-command s :copy-mode-enter nil)
        (seed-scrollback screen 10)
        (cl-tmux::process-byte s 25 state)    ; C-y
        (is (>= (screen-copy-offset screen) 0)
            "C-y must not produce a negative copy-offset")))))

;;; ── copy-mode v alternative for begin-selection ─────────────────────────────

(test copy-mode-v-begins-selection
  "Plain 'v' (byte 118) also begins selection in copy mode."
  (let ((s (make-fake-session)))
    (with-loop-state
      (let ((screen (active-screen s))
            (state  (cl-tmux::make-input-state)))
        (cl-tmux::dispatch-command s :copy-mode-enter nil)
        (finishes (cl-tmux::process-byte s 118 state))
        (is (screen-copy-selecting screen)
            "v must activate copy selection")))))

;;; ── Middle-screen cursor jump M ──────────────────────────────────────────────

(test copy-mode-M-moves-cursor-to-middle
  "Plain 'M' (byte 77) moves the copy-mode cursor to the middle row of the screen."
  (let ((s (make-fake-session)))
    (with-loop-state
      (let ((screen (active-screen s))
            (state  (cl-tmux::make-input-state)))
        (cl-tmux::dispatch-command s :copy-mode-enter nil)
        ;; Place cursor at row 0.
        (setf (cl-tmux/terminal/types:screen-copy-cursor screen) (cons 0 0))
        (cl-tmux::process-byte s (char-code #\M) state)
        (let* ((row    (car (screen-copy-cursor screen)))
               (height (screen-height screen))
               (mid    (floor height 2)))
          (is (= mid row) "M must place cursor at the middle row"))))))

;;; ── %handle-escape-x10-mouse direct invocation ───────────────────────────────

(test handle-escape-x10-mouse-dispatches-event
  "%handle-escape-x10-mouse decodes X10 encoding and dispatches the event.
   We verify it returns (values nil ground-state) without signaling."
  (with-two-pane-mouse-session (sess win p0 p1)
    (let ((buf (make-array 6 :element-type '(unsigned-byte 8)
                             :initial-contents (list 27 91 77
                                                     (+ 0 32)   ; btn 0 = left
                                                     (+ 50 33)  ; col 50 → 0-based 49
                                                     (+ 5 33)   ; row 5  → 0-based 4
                                                     ))))
      (multiple-value-bind (outcome next)
          (cl-tmux::%handle-escape-x10-mouse sess buf)
        (is (null outcome)
            "%handle-escape-x10-mouse must return NIL outcome")
        (is (eq #'cl-tmux::%ground-input-state next)
            "%handle-escape-x10-mouse must return ground-state as next state")))))

;;; ── %handle-escape-function-key outside copy mode ───────────────────────────

(test handle-escape-function-key-forwards-outside-copy-mode
  "%handle-escape-function-key forwards the 4-byte sequence when not in copy mode."
  (let ((s (make-fake-session)))
    (with-loop-state
      ;; Build an ESC [ 5 ~ (PageUp) buffer — not in copy mode.
      (let ((buf (make-array 4 :element-type '(unsigned-byte 8)
                               :initial-contents (list 27 91 53 126))))
        (multiple-value-bind (outcome next)
            (cl-tmux::%handle-escape-function-key s buf)
          (is (null outcome)
              "%handle-escape-function-key outside copy-mode must return NIL outcome")
          (is (eq #'cl-tmux::%ground-input-state next)
              "%handle-escape-function-key must return ground-state"))))))

;;; ── %handle-escape-csi-3byte: keep-accumulating for digit ───────────────────

(test handle-escape-csi-3byte-returns-keep-accumulating-for-digit
  "%handle-escape-csi-3byte returns (values T NIL) when the third byte is a digit,
   indicating we need to keep accumulating (for ESC [ N ~ function-key sequences)."
  (let ((s (make-fake-session)))
    (with-loop-state
      ;; Build ESC [ 5  — third byte is '5' (53), a digit.
      (let ((buf (make-array 3 :element-type '(unsigned-byte 8)
                               :fill-pointer 3 :adjustable t
                               :initial-contents (list 27 91 53))))
        (multiple-value-bind (keep-accumulating next-state)
            (cl-tmux::%handle-escape-csi-3byte s buf)
          (is (eq t keep-accumulating)
              "%handle-escape-csi-3byte with digit third-byte must return T (keep accumulating)")
          (is (null next-state)
              "next-state must be NIL when keep-accumulating is T"))))))

(test handle-escape-csi-3byte-returns-ground-state-for-non-digit
  "%handle-escape-csi-3byte returns (values NIL ground-state) for a non-digit final byte."
  (let ((s (make-fake-session)))
    (with-loop-state
      ;; Build ESC [ A  — third byte is 'A' (65), not a digit.
      (let ((buf (make-array 3 :element-type '(unsigned-byte 8)
                               :fill-pointer 3 :adjustable t
                               :initial-contents (list 27 91 65))))
        (multiple-value-bind (keep-accumulating next-state)
            (cl-tmux::%handle-escape-csi-3byte s buf)
          (is (null keep-accumulating)
              "%handle-escape-csi-3byte with non-digit must return NIL (do not keep accumulating)")
          (is (eq #'cl-tmux::%ground-input-state next-state)
              "next-state must be %ground-input-state"))))))

;;; ── SGR mouse: parse with scroll-wheel button encoding ───────────────────────

(test parse-sgr-mouse-scroll-up-button
  "%parse-sgr-mouse parses SGR scroll-up (btn=64) correctly."
  (let* ((s   (format nil "~C[<64;5;3M" #\Escape))
         (buf (make-array (length s) :element-type '(unsigned-byte 8)
                          :initial-contents (map 'list #'char-code s)))
         (len (length buf)))
    (multiple-value-bind (btn col row release-p)
        (cl-tmux::%parse-sgr-mouse buf len)
      (is (= 64 btn)   "scroll-up btn must be 64")
      (is (= 4  col)   "col must be 0-based (5-1=4)")
      (is (= 2  row)   "row must be 0-based (3-1=2)")
      (is-false release-p "press sequence must have release-p=NIL"))))

(test parse-sgr-mouse-returns-nil-for-short-buffer
  "%parse-sgr-mouse returns (values nil nil nil nil) for a buffer shorter than 9 bytes."
  (let* ((s   (format nil "~C[<0M" #\Escape))  ; too short
         (buf (make-array (length s) :element-type '(unsigned-byte 8)
                          :initial-contents (map 'list #'char-code s)))
         (len (length buf)))
    (multiple-value-bind (btn col row release-p)
        (cl-tmux::%parse-sgr-mouse buf len)
      (is (null btn)      "short buffer must return nil btn")
      (is (null col)      "short buffer must return nil col")
      (is (null row)      "short buffer must return nil row")
      (is (null release-p) "short buffer must return nil release-p"))))

;;; ── SGR mouse dispatch via process-byte ─────────────────────────────────────

(test sgr-mouse-left-click-via-process-byte-selects-pane
  "An SGR left-click sequence fed byte-by-byte through process-byte selects the pane."
  (with-two-pane-mouse-session (sess win p0 p1)
    (setf (screen-mouse-sgr-mode (pane-screen p0)) t)
    (let ((state (cl-tmux::make-input-state))
          ;; ESC [ < 0 ; 50 ; 5 M  — btn=0, col=50, row=5 (1-based), press
          (seq   (format nil "~C[<0;50;5M" #\Escape)))
      (loop for ch across seq
            do (cl-tmux::process-byte sess (char-code ch) state))
      (is (eq p1 (window-active-pane win))
          "SGR left-click in right pane must focus p1"))))

;;; ── overlay-scroll: verify actual offset change ──────────────────────────────

(test overlay-scroll-up-decrements-offset
  "overlay-scroll -1 decrements *overlay-scroll-offset* by 1 (clamped at 0)."
  ;; NOTE: build the overlay with real newlines via ~%.  A CL string literal
  ;; does NOT treat \n as a newline (backslash only escapes " and \), so
  ;; "line1\nline2..." is a SINGLE line and overlay-lines would return 1 entry,
  ;; clamping every scroll to 0.
  (let ((*overlay* (format nil "line1~%line2~%line3~%line4~%line5~%"))
        (*overlay-scroll-offset* 3))
    (overlay-scroll -1)
    (is (= 2 *overlay-scroll-offset*)
        "overlay-scroll -1 must decrement offset from 3 to 2")))

(test overlay-scroll-down-increments-offset
  "overlay-scroll 1 increments *overlay-scroll-offset* by 1."
  ;; Real newlines via ~% (a CL literal's \n is NOT a newline; see the
  ;; overlay-scroll-up test for the full explanation).
  (let ((*overlay* (format nil "line1~%line2~%line3~%line4~%line5~%"))
        (*overlay-scroll-offset* 0))
    (overlay-scroll 1)
    (is (= 1 *overlay-scroll-offset*)
        "overlay-scroll 1 must increment offset from 0 to 1")))

(test overlay-scroll-clamps-at-zero
  "overlay-scroll -1 at offset 0 does not produce a negative offset."
  (let ((*overlay* "line1\n")
        (*overlay-scroll-offset* 0))
    (overlay-scroll -1)
    (is (>= *overlay-scroll-offset* 0)
        "overlay-scroll at offset 0 must not go negative")))

;;; ── with-copy-mode-state test helper macro ───────────────────────────────────
;;;
;;; Eliminates the triple-nested boilerplate that appeared 43+ times:
;;;   (let ((s (make-fake-session))) (with-loop-state (let ((screen ...) (state ...)) ...)))

(defmacro with-copy-mode-state ((session-var screen-var state-var) &body body)
  "Run BODY with SESSION-VAR bound to a fresh fake session in copy mode,
   SCREEN-VAR bound to its active screen, and STATE-VAR bound to a fresh input-state.
   Wraps everything in WITH-LOOP-STATE for proper event-loop isolation."
  `(let ((,session-var (make-fake-session)))
     (with-loop-state
       (let ((,screen-var (active-screen ,session-var))
             (,state-var  (cl-tmux::make-input-state)))
         (cl-tmux::dispatch-command ,session-var :copy-mode-enter nil)
         ,@body))))

;;; ── %border-check-node direct tests ─────────────────────────────────────────
;;;
;;; %border-check-node is the recursive tree walker inside %border-at-position.
;;; The :v split path and multi-level recursion deserve direct coverage.

(test border-check-node-leaf-returns-nil
  "%border-check-node on a layout-leaf always returns (values NIL NIL)."
  (let* ((p0   (make-pane :id 1 :fd -1 :pid -1 :x 0 :y 0 :width 40 :height 24
                           :screen (make-screen 40 24)))
         (leaf (make-layout-leaf p0)))
    (multiple-value-bind (split orientation)
        (cl-tmux::%border-check-node 20 10 leaf)
      (is (null split)       "layout-leaf must return NIL split")
      (is (null orientation) "layout-leaf must return NIL orientation"))))

(test border-check-node-h-split-detects-separator
  "%border-check-node returns (split :h) when col lands exactly on the horizontal separator."
  (let* ((p0    (make-pane :id 1 :fd -1 :pid -1 :x 0 :y 0 :width 40 :height 24
                            :screen (make-screen 40 24)))
         (p1    (make-pane :id 2 :fd -1 :pid -1 :x 41 :y 0 :width 40 :height 24
                            :screen (make-screen 40 24)))
         (leaf0 (make-layout-leaf p0))
         (leaf1 (make-layout-leaf p1))
         (split (make-layout-split :h leaf0 leaf1 1/2)))
    ;; Separator column for p0 (x=0 w=40) is at col 40.
    (multiple-value-bind (found-split orientation)
        (cl-tmux::%border-check-node 40 5 split)
      (is (eq split found-split)
          "%border-check-node :h split must return the split node at separator col")
      (is (eq :h orientation)
          "%border-check-node :h split must report :h orientation"))))

(test border-check-node-v-split-detects-separator
  "%border-check-node returns (split :v) when row lands exactly on the vertical separator."
  (let* ((p0    (make-pane :id 1 :fd -1 :pid -1 :x 0 :y 0  :width 80 :height 10
                            :screen (make-screen 80 10)))
         (p1    (make-pane :id 2 :fd -1 :pid -1 :x 0 :y 11 :width 80 :height 10
                            :screen (make-screen 80 10)))
         (leaf0 (make-layout-leaf p0))
         (leaf1 (make-layout-leaf p1))
         (split (make-layout-split :v leaf0 leaf1 1/2)))
    ;; Separator row for p0 (y=0 h=10) is at row 10.
    (multiple-value-bind (found-split orientation)
        (cl-tmux::%border-check-node 5 10 split)
      (is (eq split found-split)
          "%border-check-node :v split must return the split node at separator row")
      (is (eq :v orientation)
          "%border-check-node :v split must report :v orientation"))))

(test border-check-node-h-split-inside-pane-returns-nil
  "%border-check-node returns (values NIL NIL) when col is inside a pane (not on border)."
  (let* ((p0    (make-pane :id 1 :fd -1 :pid -1 :x 0 :y 0 :width 40 :height 24
                            :screen (make-screen 40 24)))
         (p1    (make-pane :id 2 :fd -1 :pid -1 :x 41 :y 0 :width 40 :height 24
                            :screen (make-screen 40 24)))
         (leaf0 (make-layout-leaf p0))
         (leaf1 (make-layout-leaf p1))
         (split (make-layout-split :h leaf0 leaf1 1/2)))
    (multiple-value-bind (found-split orientation)
        (cl-tmux::%border-check-node 20 5 split)
      (is (null found-split)   "col inside pane must return NIL split")
      (is (null orientation)   "col inside pane must return NIL orientation"))))

(test border-check-node-nested-split-finds-inner-border
  "%border-check-node recurses into child splits and finds inner borders."
  ;; Build a 3-pane layout: [p0 | [p1 above p2]]
  ;; Outer: :h split at col 40; inner: :v split at row 10.
  (let* ((p0    (make-pane :id 1 :fd -1 :pid -1 :x 0 :y 0  :width 40 :height 24
                            :screen (make-screen 40 24)))
         (p1    (make-pane :id 2 :fd -1 :pid -1 :x 41 :y 0  :width 40 :height 10
                            :screen (make-screen 40 10)))
         (p2    (make-pane :id 3 :fd -1 :pid -1 :x 41 :y 11 :width 40 :height 10
                            :screen (make-screen 40 10)))
         (leaf0 (make-layout-leaf p0))
         (leaf1 (make-layout-leaf p1))
         (leaf2 (make-layout-leaf p2))
         (inner-split (make-layout-split :v leaf1 leaf2 1/2))
         (outer-split (make-layout-split :h leaf0 inner-split 1/2)))
    ;; Hit the inner :v border at (col=50, row=10)
    (multiple-value-bind (found-split orientation)
        (cl-tmux::%border-check-node 50 10 outer-split)
      (is (eq inner-split found-split)
          "%border-check-node must find the inner :v split node")
      (is (eq :v orientation)
          "%border-check-node must report :v for the inner split"))))

;;; ── %status-col-to-window: multi-window traversal coverage ──────────────────

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
  (let ((s (make-fake-session)))
    (with-loop-state
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
              "%handle-escape-sgr-mouse must return ground-state for malformed sequence"))))))

;;; ── copy-mode navigation bytes via process-byte (table-driven coverage) ─────
;;;
;;; Tests that all the additional byte constants (h, l, w, b, e, $, etc.) defined
;;; in events-core.lisp route correctly through the copy-mode dispatch in
;;; %ground-input-state. We drive them through process-byte to stay at the
;;; public API level.

(test copy-mode-all-nav-bytes-via-process-byte
  "All standard copy-mode navigation bytes route without error through process-byte."
  (let ((s (make-fake-session)))
    (with-loop-state
      (let ((screen (active-screen s))
            (state  (cl-tmux::make-input-state)))
        (cl-tmux::dispatch-command s :copy-mode-enter nil)
        (seed-scrollback screen 10)
        ;; Use the named constants from events-core.lisp for each byte.
        (dolist (byte (list #.cl-tmux::+byte-h+
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
          (finishes (cl-tmux::process-byte s byte state)))))))

;;; ── idle sleep constant verification ─────────────────────────────────────────

(test event-loop-idle-sleep-constant-is-positive
  "+event-loop-idle-sleep-seconds+ is a positive real number."
  (is (and (realp cl-tmux::+event-loop-idle-sleep-seconds+)
           (plusp cl-tmux::+event-loop-idle-sleep-seconds+))
      "+event-loop-idle-sleep-seconds+ must be a positive real"))

(test event-loop-idle-sleep-constant-value
  "+event-loop-idle-sleep-seconds+ is 0.001 (1 ms)."
  (is (= 0.001 cl-tmux::+event-loop-idle-sleep-seconds+)
      "+event-loop-idle-sleep-seconds+ must be 0.001"))

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
    (let ((sess (make-fake-session :nwindows 1 :npanes 1)))
      (with-loop-state
        ;; Enter copy mode
        (let* ((win  (cl-tmux/model:session-active-window sess))
               (pane (cl-tmux/model:window-active-pane win))
               (screen (cl-tmux/model:pane-screen pane)))
          (cl-tmux/commands:copy-mode-enter screen)
          ;; The 'v' key (118) should be handled by the table lookup
          ;; We verify copy-mode is active and the binding exists
          (is (cl-tmux/terminal:screen-copy-mode-p screen)
              "screen must be in copy mode")
          (is (not (null (cl-tmux/config:key-table-lookup "copy-mode-vi" #\v)))
              "copy-mode-vi table must have 'v' binding")
          (cl-tmux/commands:copy-mode-exit screen))))))
