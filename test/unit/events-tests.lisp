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

(test process-byte-focus-in-fires-pane-focus-in
  "ESC [ I (outer-terminal focus gained, ?1004) fires pane-focus-in on the active pane."
  (with-isolated-hooks
    (let ((s (make-fake-session)) (fired nil))
      (with-loop-state
        (cl-tmux/hooks:add-hook "pane-focus-in"
                                (lambda (&rest _) (declare (ignore _)) (setf fired t)))
        (let ((state (cl-tmux::make-input-state)))
          (dolist (b '(27 91 73)) (cl-tmux::process-byte s b state)))  ; ESC [ I
        (is-true fired "ESC [ I must fire pane-focus-in")))))

(test process-byte-focus-out-fires-pane-focus-out
  "ESC [ O (outer-terminal focus lost, ?1004) fires pane-focus-out on the active pane."
  (with-isolated-hooks
    (let ((s (make-fake-session)) (fired nil))
      (with-loop-state
        (cl-tmux/hooks:add-hook "pane-focus-out"
                                (lambda (&rest _) (declare (ignore _)) (setf fired t)))
        (let ((state (cl-tmux::make-input-state)))
          (dolist (b '(27 91 79)) (cl-tmux::process-byte s b state)))  ; ESC [ O
        (is-true fired "ESC [ O must fire pane-focus-out")))))

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

(test copy-mode-plain-esc-clears-selection-stays-in-copy-mode
  "ESC followed by a non-CSI byte clears the selection but STAYS in copy mode.
   This matches tmux's default copy-mode-vi Escape → clear-selection binding.
   Use q or i to exit copy mode."
  (let ((s (make-fake-session)))
    (with-loop-state
      (let ((screen (active-screen s))
            (state  (cl-tmux::make-input-state)))
        (cl-tmux::dispatch-command s :copy-mode-enter nil)
        (is (screen-copy-mode-p screen) "copy mode must be active after enter")
        ;; Start a selection so we can verify it gets cleared.
        (cl-tmux/commands::copy-mode-begin-selection screen)
        (is (screen-copy-selecting screen) "selection must be active before ESC")
        ;; Feed ESC then a non-CSI byte (not '[')
        (cl-tmux::process-byte s 27 state)
        (cl-tmux::process-byte s (char-code #\x) state)
        ;; Selection is cleared but copy-mode stays active.
        (is-true  (screen-copy-mode-p   screen) "copy mode must remain active after ESC")
        (is-false (screen-copy-selecting screen) "selection must be cleared by ESC")))))

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

(test modifier-arrow-key-name-builds-combined-modifiers
  "Combined modifiers resolve via %modifier-prefix in canonical C-/M-/S- order:
   6=Ctrl+Shift, 7=Ctrl+Meta, 8=Ctrl+Meta+Shift, 4=Meta+Shift."
  (is (string= "C-S-Up"   (cl-tmux::%modifier-arrow-key-name 54 65)))  ; 6
  (is (string= "C-M-Up"   (cl-tmux::%modifier-arrow-key-name 55 65)))  ; 7
  (is (string= "C-M-S-Up" (cl-tmux::%modifier-arrow-key-name 56 65)))  ; 8
  (is (string= "M-S-Up"   (cl-tmux::%modifier-arrow-key-name 52 65)))) ; 4

(test modifier-arrow-key-name-returns-nil-for-unknown
  "%modifier-arrow-key-name returns NIL for a non-arrow final or a no-modifier
   value, so the caller forwards the sequence unchanged."
  (is (null (cl-tmux::%modifier-arrow-key-name 53 72)))  ; Ctrl+Home — not arrow
  (is (null (cl-tmux::%modifier-arrow-key-name 49 65)))) ; '1' = no modifier → NIL

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

;;; ── switch-client session selection (-t / -n / -p / -l) ─────────────────────

(defun %make-three-session-registry ()
  "Build three registered sessions named 0/1/2 (current = 1) with deterministic
   last-active stamps 10/30/20, and return them as (values s0 s1 s2).  Caller
   must run inside a binding that isolates cl-tmux::*server-sessions*."
  (let ((s0 (make-fake-session :nwindows 1))
        (s1 (make-fake-session :nwindows 1))
        (s2 (make-fake-session :nwindows 1)))
    (setf (cl-tmux::session-name s0) "0" (cl-tmux::session-last-active s0) 10
          (cl-tmux::session-name s1) "1" (cl-tmux::session-last-active s1) 30
          (cl-tmux::session-name s2) "2" (cl-tmux::session-last-active s2) 20
          cl-tmux::*server-sessions*
          (list (cons "0" s0) (cons "1" s1) (cons "2" s2)))
    (values s0 s1 s2)))

(test cmd-switch-client-t-switches-to-named-session
  "switch-client -t <name> makes the named session the front (touched) one."
  (with-loop-state
    (with-empty-registry
      (multiple-value-bind (s0 s1 s2) (%make-three-session-registry)
        (declare (ignore s0 s1))
        (let ((result (cl-tmux::%cmd-switch-client (cl-tmux::server-find-session "1")
                                                   '("-t" "2"))))
          (is (eq s2 result) "-t 2 selects session named 2")
          (is-true cl-tmux::*dirty* "a session switch marks the screen dirty"))))))

(test cmd-switch-client-n-and-p-cycle-sessions
  "switch-client -n / -p move to the next / previous session cyclically."
  (with-loop-state
    (with-empty-registry
      (multiple-value-bind (s0 s1 s2) (%make-three-session-registry)
        ;; current = s1; registry order is (s0 s1 s2): next → s2, prev → s0.
        (is (eq s2 (cl-tmux::%cmd-switch-client s1 '("-n")))
            "-n from session 1 goes to session 2")
        (is (eq s0 (cl-tmux::%cmd-switch-client s1 '("-p")))
            "-p from session 1 goes to session 0")))))

(test cmd-switch-client-l-switches-to-last-active
  "switch-client -l selects the second-most-recently-active session."
  (with-loop-state
    (with-empty-registry
      (multiple-value-bind (s0 s1 s2) (%make-three-session-registry)
        (declare (ignore s0))
        ;; last-active stamps 10/30/20 → desc order s1,s2,s0 → second = s2.
        (is (eq s2 (cl-tmux::%cmd-switch-client s1 '("-l")))
            "-l from the front session 1 returns to session 2")))))

(test cmd-switch-client-t-and-T-are-orthogonal
  "switch-client -t <name> -T <table> performs the session move AND arms the
   key table in one invocation."
  (with-loop-state
    (with-empty-registry
      (multiple-value-bind (s0 s1 s2) (%make-three-session-registry)
        (declare (ignore s0 s1))
        (let ((result (cl-tmux::%cmd-switch-client (cl-tmux::server-find-session "1")
                                                   '("-t" "2" "-T" "resize"))))
          (is (eq s2 result) "-t still switches the session when -T is also given")
          (is (string= "resize" cl-tmux::*key-table*)
              "-T still arms the key table when -t is also given"))))))

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
  (with-minimal-session (p0 win sess)
    (with-loop-state
      (let ((*overlay* nil))
        (is-false (pane-marked p0) "pane must not be marked initially")
        (cl-tmux::dispatch-command sess :mark-pane nil)
        (is (pane-marked p0) "pane must be marked after :mark-pane")))))

(test dispatch-mark-pane-toggle-unmarks
  ":mark-pane on an already-marked pane unmarks it (toggle)."
  (with-minimal-session (p0 win sess)
    (declare (ignore win))
    (with-loop-state
      (let ((*overlay* nil))
        (cl-tmux::dispatch-command sess :mark-pane nil)
        (is (pane-marked p0) "pane marked after first :mark-pane")
        (cl-tmux::dispatch-command sess :mark-pane nil)
        (is-false (pane-marked p0)
            "pane unmarked after :mark-pane on already-marked pane")))))

(test dispatch-clear-mark-unmarks-all-panes
  ":clear-mark clears the server-wide marked pane."
  (with-minimal-session (p0 win sess)
    (declare (ignore win))
    (with-loop-state
      (let ((*overlay* nil))
        (cl-tmux::dispatch-command sess :mark-pane nil)
        (is (pane-marked p0) "pane must be marked before :clear-mark")
        (cl-tmux::dispatch-command sess :clear-mark nil)
        (is-false (pane-marked p0) "pane must not be marked after :clear-mark")))))

;;; ── dispatch :display-info ───────────────────────────────────────────────────

(test dispatch-display-info-shows-overlay
  ":display-info shows a non-empty overlay with session/window/pane info."
  (with-minimal-session (p0 win sess)
    (declare (ignore p0 win))
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

;;; ── Function / navigation keys: ESC [ N ~ → key name → binding ───────────────

(test csi-tilde-parse-reads-param-and-modifier
  "%csi-tilde-parse returns (values PARAM MOD); MOD defaults to 1 and a ';mod'
   field carries the modifier (the modified-function-key form)."
  ;; ESC [ 5 ~  → 5, 1  (unmodified)
  (multiple-value-bind (p m)
      (cl-tmux::%csi-tilde-parse
       (make-array 4 :element-type '(unsigned-byte 8)
                     :initial-contents '(27 91 53 126)) 4)
    (is (= 5 p)) (is (= 1 m)))
  ;; ESC [ 1 5 ~ → 15, 1  (F5)
  (multiple-value-bind (p m)
      (cl-tmux::%csi-tilde-parse
       (make-array 5 :element-type '(unsigned-byte 8)
                     :initial-contents '(27 91 49 53 126)) 5)
    (is (= 15 p)) (is (= 1 m)))
  ;; ESC [ 1 5 ; 5 ~ → 15, 5  (Ctrl+F5)
  (multiple-value-bind (p m)
      (cl-tmux::%csi-tilde-parse
       (make-array 7 :element-type '(unsigned-byte 8)
                     :initial-contents '(27 91 49 53 59 53 126)) 7)
    (is (= 15 p)) (is (= 5 m)))
  ;; ESC [ ~ (empty param) → NIL → raw forward
  (is (null (cl-tmux::%csi-tilde-parse
             (make-array 3 :element-type '(unsigned-byte 8)
                           :initial-contents '(27 91 126)) 3))))

(test csi-tilde-key-joins-base-and-modifier
  "%csi-tilde-key combines base key + modifier prefix: F5, C-F5, S-Home."
  (flet ((k (bytes) (cl-tmux::%csi-tilde-key
                     (make-array (length bytes) :element-type '(unsigned-byte 8)
                                                :initial-contents bytes)
                     (length bytes))))
    (is (string= "F5"     (k '(27 91 49 53 126))))         ; ESC [ 15 ~
    (is (string= "C-F5"   (k '(27 91 49 53 59 53 126))))   ; ESC [ 15 ; 5 ~
    (is (string= "S-Home" (k '(27 91 49 59 50 126))))      ; ESC [ 1 ; 2 ~
    (is (null (k '(27 91 50 48 48 126)))                   ; ESC [ 200 ~ (paste)
        "an unmapped parameter yields NIL so it is forwarded raw")))

(test csi-tilde-key-name-maps-known-params
  "%csi-tilde-key-name maps vt parameters to canonical tmux key names."
  (is (string= "Home"     (cl-tmux::%csi-tilde-key-name 1)))
  (is (string= "Delete"   (cl-tmux::%csi-tilde-key-name 3)))
  (is (string= "PageUp"   (cl-tmux::%csi-tilde-key-name 5)))
  (is (string= "PageDown" (cl-tmux::%csi-tilde-key-name 6)))
  (is (string= "F5"       (cl-tmux::%csi-tilde-key-name 15)))
  (is (string= "F12"      (cl-tmux::%csi-tilde-key-name 24)))
  (is (null (cl-tmux::%csi-tilde-key-name 99))
      "an unknown parameter must map to NIL (forwarded raw, not bound)"))

(test normalize-key-alias-collapses-navigation-spellings
  "%normalize-key-alias maps tmux's aliases to the canonical input-side names."
  (is (string= "PageUp"   (cl-tmux/config::%normalize-key-alias "PPage")))
  (is (string= "PageDown" (cl-tmux/config::%normalize-key-alias "NPage")))
  (is (string= "Insert"   (cl-tmux/config::%normalize-key-alias "IC")))
  (is (string= "Delete"   (cl-tmux/config::%normalize-key-alias "DC")))
  (is (string= "PageUp"   (cl-tmux/config::%normalize-key-alias "pgup"))
      "alias matching is case-insensitive")
  (is (null (cl-tmux/config::%normalize-key-alias "F5"))
      "a non-alias token returns NIL so %parse-key-token keeps it verbatim"))

(test parse-key-token-normalizes-aliases-to-canonical
  "%parse-key-token collapses PPage→PageUp so bind-side and input-side keys match."
  (is (string= "PageUp"   (cl-tmux/config::%parse-key-token "PPage")))
  (is (string= "PageDown" (cl-tmux/config::%parse-key-token "NPage")))
  (is (string= "Insert"   (cl-tmux/config::%parse-key-token "IC")))
  (is (string= "F5"       (cl-tmux/config::%parse-key-token "F5"))
      "a canonical/non-alias name passes through unchanged"))

(test function-key-root-binding-fires-from-byte-stream
  "bind -n F5 fires when ESC [ 1 5 ~ is fed through the input state machine."
  (let ((s (make-fake-session :nwindows 2)))
    (with-loop-state
      (let ((state (cl-tmux::make-input-state)))
        (key-table-bind "root" "F5" :next-window)
        (unwind-protect
             (progn
               ;; ESC [ 1 5 ~  byte by byte.
               (dolist (byte '(27 91 49 53 126))
                 (cl-tmux::process-byte s byte state))
               (is (eq (second (session-windows s)) (session-active-window s))
                   "ESC [ 15 ~ must resolve to F5 and fire its root binding")
               (is (eq #'cl-tmux::%ground-input-state
                       (cl-tmux::input-state-continuation state))
                   "the state machine must return to ground after ESC [ 15 ~"))
          (let ((tbl (gethash "root" *key-tables*)))
            (when tbl (remhash "F5" tbl))))))))

(test page-up-alias-root-binding-fires-from-byte-stream
  "bind -n PPage (alias of PageUp) fires when ESC [ 5 ~ is fed: the alias
   normalisation and the input-side key name meet at the canonical \"PageUp\"."
  (let ((s (make-fake-session :nwindows 2)))
    (with-loop-state
      (let ((state (cl-tmux::make-input-state))
            (key   (cl-tmux/config::%parse-key-token "PPage")))
        (key-table-bind "root" key :next-window)
        (unwind-protect
             (progn
               ;; ESC [ 5 ~  byte by byte.
               (dolist (byte '(27 91 53 126))
                 (cl-tmux::process-byte s byte state))
               (is (eq (second (session-windows s)) (session-active-window s))
                   "ESC [ 5 ~ must resolve to PageUp and fire the PPage binding"))
          (let ((tbl (gethash "root" *key-tables*)))
            (when tbl (remhash key tbl))))))))

(test unbound-function-key-forwards-to-pane-not-bindings
  "An unbound F5 (ESC [ 15 ~) leaves the state machine at ground without firing a
   binding — preserving transparency so the pane application receives the key."
  (let ((s (make-fake-session :nwindows 2)))
    (with-loop-state
      (let ((state (cl-tmux::make-input-state))
            (before (session-active-window s)))
        ;; No binding installed for F5: feeding ESC [ 15 ~ must not switch windows.
        (dolist (byte '(27 91 49 53 126))
          (cl-tmux::process-byte s byte state))
        (is (eq before (session-active-window s))
            "an unbound F5 must not trigger any window command")
        (is (eq #'cl-tmux::%ground-input-state
                (cl-tmux::input-state-continuation state))
            "the state machine must return to ground after an unbound ESC [ 15 ~")))))

;;; ── SS3 function keys: ESC O P/Q/R/S (F1-F4), ESC O H/F (Home/End) ───────────

(test ss3-key-name-maps-f1-through-f4-and-home-end
  "%ss3-key-name maps the SS3 finals to canonical key names; others are NIL."
  (is (string= "F1"   (cl-tmux::%ss3-key-name (char-code #\P))))
  (is (string= "F2"   (cl-tmux::%ss3-key-name (char-code #\Q))))
  (is (string= "F3"   (cl-tmux::%ss3-key-name (char-code #\R))))
  (is (string= "F4"   (cl-tmux::%ss3-key-name (char-code #\S))))
  (is (string= "Home" (cl-tmux::%ss3-key-name (char-code #\H))))
  (is (string= "End"  (cl-tmux::%ss3-key-name (char-code #\F))))
  (is (null (cl-tmux::%ss3-key-name (char-code #\A)))
      "SS3 arrows are out of scope here and must map to NIL (forwarded raw)")
  (is (null (cl-tmux::%ss3-key-name (char-code #\Z)))
      "an unrecognised SS3 final must map to NIL"))

(test ss3-introducer-defers-one-byte-and-tracks-buffer
  "ESC O does not resolve immediately (it could be F1-F4); the decoder keeps
   accumulating and exposes the partial buffer for the escape-time flush replay."
  (let ((s (make-fake-session)))
    (with-loop-state
      (let ((cl-tmux::*esc-accum-buffer* nil)
            (state (cl-tmux::make-input-state)))
        (cl-tmux::process-byte s 27 state)              ; ESC
        (cl-tmux::process-byte s (char-code #\O) state) ; O
        (is (not (eq #'cl-tmux::%ground-input-state
                     (cl-tmux::input-state-continuation state)))
            "ESC O must keep accumulating, not resolve as Alt+O at length 2")
        (is (and cl-tmux::*esc-accum-buffer*
                 (equalp (coerce (subseq cl-tmux::*esc-accum-buffer* 0
                                         (fill-pointer cl-tmux::*esc-accum-buffer*))
                                 'list)
                         '(27 79)))
            "the replay buffer must hold the full partial sequence ESC O")))))

(test ss3-f1-root-binding-fires-from-byte-stream
  "bind -n F1 fires when ESC O P is fed through the input state machine, and the
   buffer-replay state is cleared once the sequence completes (back to ground)."
  (let ((s (make-fake-session :nwindows 2)))
    (with-loop-state
      (let ((cl-tmux::*esc-accum-buffer* nil)
            (state (cl-tmux::make-input-state)))
        (key-table-bind "root" "F1" :next-window)
        (unwind-protect
             (progn
               (dolist (byte (list 27 (char-code #\O) (char-code #\P)))
                 (cl-tmux::process-byte s byte state))
               (is (eq (second (session-windows s)) (session-active-window s))
                   "ESC O P must resolve to F1 and fire its root binding")
               (is (eq #'cl-tmux::%ground-input-state
                       (cl-tmux::input-state-continuation state))
                   "the state machine must return to ground after ESC O P")
               (is (null cl-tmux::*esc-accum-buffer*)
                   "the replay buffer must be cleared once back at ground"))
          (let ((tbl (gethash "root" *key-tables*)))
            (when tbl (remhash "F1" tbl))))))))

(test ss3-unbound-f1-does-not-fire-and-returns-to-ground
  "An unbound F1 (ESC O P) must not trigger a command and must leave the state
   machine at ground — the raw key is forwarded to the pane for transparency."
  (let ((s (make-fake-session :nwindows 2)))
    (with-loop-state
      (let ((cl-tmux::*esc-accum-buffer* nil)
            (state  (cl-tmux::make-input-state))
            (before (session-active-window s)))
        (dolist (byte (list 27 (char-code #\O) (char-code #\P)))
          (cl-tmux::process-byte s byte state))
        (is (eq before (session-active-window s))
            "an unbound F1 must not change the active window")
        (is (eq #'cl-tmux::%ground-input-state
                (cl-tmux::input-state-continuation state))
            "the state machine must return to ground after an unbound ESC O P")))))

;;; ── Prefix-table function keys: C-b then F5 / F1 (bind F5, bind F1) ──────────

(test prefix-function-key-csi-binding-fires
  "bind F5 next-window fires on C-b then ESC [ 15 ~ — the prefix-table path now
   resolves CSI function keys (previously the multi-digit tilde was swallowed)."
  (with-isolated-config
    (cl-tmux/config:apply-config-directive '("bind" "F5" "next-window"))
    (let ((s (make-fake-session :nwindows 2)))
      (with-loop-state
        (let ((cl-tmux::*esc-accum-buffer* nil)
              (state (cl-tmux::make-input-state)))
          ;; C-b (2) then ESC [ 1 5 ~
          (dolist (byte '(2 27 91 49 53 126))
            (cl-tmux::process-byte s byte state))
          (is (eq (second (session-windows s)) (session-active-window s))
              "C-b F5 must run the prefix-table binding")
          (is (eq #'cl-tmux::%ground-input-state
                  (cl-tmux::input-state-continuation state))
              "the state machine must return to ground after C-b F5"))))))

(test prefix-function-key-ss3-binding-fires
  "bind F1 next-window fires on C-b then ESC O P — the prefix-table path now
   resolves the SS3 function-key form too."
  (with-isolated-config
    (cl-tmux/config:apply-config-directive '("bind" "F1" "next-window"))
    (let ((s (make-fake-session :nwindows 2)))
      (with-loop-state
        (let ((cl-tmux::*esc-accum-buffer* nil)
              (state (cl-tmux::make-input-state)))
          ;; C-b (2) then ESC O P
          (dolist (byte (list 2 27 (char-code #\O) (char-code #\P)))
            (cl-tmux::process-byte s byte state))
          (is (eq (second (session-windows s)) (session-active-window s))
              "C-b F1 must run the prefix-table binding"))))))

(test prefix-arrow-binding-still-fires-after-digit-change
  "Regression guard: widening the 3-byte branch to accumulate on any digit final
   must not break the plain arrow path — C-b then ESC [ A still selects up."
  (with-isolated-config
    (cl-tmux/config:apply-config-directive '("bind" "Up" "next-window"))
    (let ((s (make-fake-session :nwindows 2)))
      (with-loop-state
        (let ((cl-tmux::*esc-accum-buffer* nil)
              (state (cl-tmux::make-input-state)))
          ;; C-b (2) then ESC [ A
          (dolist (byte (list 2 27 91 (char-code #\A)))
            (cl-tmux::process-byte s byte state))
          (is (eq (second (session-windows s)) (session-active-window s))
              "C-b Up must still resolve to the prefix-table Up binding"))))))

;;; ── Modified function keys & combined-modifier arrows (root bind -n) ─────────

(test modified-function-key-root-binding-fires-from-byte-stream
  "bind -n C-F5 fires when ESC [ 15 ; 5 ~ (Ctrl+F5) is fed through the machine —
   the modified-function-key form the unmodified path previously dropped."
  (let ((s (make-fake-session :nwindows 2)))
    (with-loop-state
      (let ((cl-tmux::*esc-accum-buffer* nil)
            (state (cl-tmux::make-input-state)))
        (key-table-bind "root" "C-F5" :next-window)
        (unwind-protect
             (progn
               ;; ESC [ 1 5 ; 5 ~
               (dolist (byte '(27 91 49 53 59 53 126))
                 (cl-tmux::process-byte s byte state))
               (is (eq (second (session-windows s)) (session-active-window s))
                   "ESC [ 15 ; 5 ~ must resolve to C-F5 and fire its binding"))
          (let ((tbl (gethash "root" *key-tables*)))
            (when tbl (remhash "C-F5" tbl))))))))

(test combined-modifier-arrow-root-binding-fires-from-byte-stream
  "bind -n C-S-Up fires when ESC [ 1 ; 6 A (Ctrl+Shift+Up) is fed — combined
   modifiers now resolve, matching the CSI-u path's handling of letter keys."
  (let ((s (make-fake-session :nwindows 2)))
    (with-loop-state
      (let ((cl-tmux::*esc-accum-buffer* nil)
            (state (cl-tmux::make-input-state)))
        (key-table-bind "root" "C-S-Up" :next-window)
        (unwind-protect
             (progn
               ;; ESC [ 1 ; 6 A
               (dolist (byte (list 27 91 49 59 54 (char-code #\A)))
                 (cl-tmux::process-byte s byte state))
               (is (eq (second (session-windows s)) (session-active-window s))
                   "ESC [ 1 ; 6 A must resolve to C-S-Up and fire its binding"))
          (let ((tbl (gethash "root" *key-tables*)))
            (when tbl (remhash "C-S-Up" tbl))))))))

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
;;; When the overlay is active and ESC [ A arrives, %overlay-escape-second-byte
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

