(in-package #:cl-tmux/test)

;;;; copy-mode-escape, process-byte, copy-mode-arrows, resize/dirty, handle-prompt-key, mouse dispatch, key-table — part I

(def-suite events-suite :description "Keystroke processing pipeline")
(in-suite events-suite)

;;; ── Copy-mode escape handler ─────────────────────────────────────────────────

(test handle-copy-mode-escape-consumes-arrows
  "Arrow-key escape sequences are consumed while copy mode is active; q exits."
  (with-fake-session (s)
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
        "q should have exited copy mode")))

(test handle-copy-mode-escape-inactive-returns-nil
  "Outside copy mode, handle-copy-mode-escape consumes nothing."
  (with-fake-session (s)
    (is (null (cl-tmux::handle-copy-mode-escape
               s (make-array 3 :element-type '(unsigned-byte 8)
                               :initial-contents '(27 91 65)))))))

;;; ── process-byte: the shared keystroke pipeline ─────────────────────────────
;;;
;;; process-byte is what the in-process event loop AND the client/server attach
;;; loop both feed bytes to, so verifying it covers the byte-routing that the
;;; blocking event-loop itself can't be unit-tested for.  +prefix-key-code+ is 2.

(test process-byte-prefix-then-command
  "Prefix byte (2) then 'n' routes through the binding table to :next-window."
  (with-fake-session (s :nwindows 2)
    (with-input-state (input-state)
      ;; Lone prefix byte just transitions to the after-prefix state, no quit.
      (is (null (cl-tmux::process-byte s 2 input-state)))
      ;; Following byte 'n' selects the next window.
      (is (null (cl-tmux::process-byte s (char-code #\n) input-state)))
      (is (eq (second (session-windows s)) (session-active-window s))))))

(test process-byte-prefix-detach-returns-detach
  "Prefix byte then 'd' returns :detach from process-byte."
  ;; Isolate the key-tables: another suite can mutate the live prefix table, and
  ;; this test depends on the default #\d → :detach binding being present.
  (with-isolated-config
    (with-fake-session (s)
      (with-input-state (input-state)
        (cl-tmux::process-byte s 2 input-state)
        (is (eq :detach (cl-tmux::process-byte s (char-code #\d) input-state)))))))

(test process-byte-ordinary-key-forwards
  "An ordinary byte (no prefix) is forwarded and returns NIL (no quit)."
  (with-fake-session (s)
    (with-input-state (input-state)
      ;; fd -1 panes make pty-write a harmless no-op; we assert routing only.
      (is (null (cl-tmux::process-byte s (char-code #\x) input-state))))))

(test process-byte-routes-to-active-prompt
  "While a prompt is active, process-byte edits the prompt buffer."
  (with-fake-session (s)
    (let ((*prompt* nil))
      (prompt-start "rename-window" "" (lambda (name) (declare (ignore name)) nil))
      (with-input-state (input-state)
        (cl-tmux::process-byte s (char-code #\h) input-state)
        (cl-tmux::process-byte s (char-code #\i) input-state)
        (is (string= "hi" (prompt-buffer *prompt*))
            "prompt captured the keystrokes via process-byte")))))

(test process-byte-overlay-q-dismisses
  "While an overlay is shown, q dismisses it; other keys are swallowed (overlay stays open)."
  (with-fake-session (s)
    (let ((*overlay* nil) (cl-tmux::*dirty* nil))
      (show-overlay "help text")
      (with-input-state (input-state)
        ;; An ordinary key ('x') is swallowed but the overlay stays open.
        (is (null (cl-tmux::process-byte s (char-code #\x) input-state)))
        (is (overlay-active-p) "ordinary key must not dismiss the overlay")
        ;; 'q' dismisses the overlay.
        (is (null (cl-tmux::process-byte s (char-code #\q) input-state)))
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
  (with-copy-mode-state (s screen input-state)
    (seed-scrollback screen 10)
    ;; Force cursor to the top row so the next up-arrow scrolls the viewport.
    (setf (cl-tmux/terminal/types:screen-copy-cursor screen) (cons 0 0))
    (is (zerop (screen-copy-offset screen)) "offset starts at the live view")
    ;; ESC [ A, one byte at a time.
    (is (null (cl-tmux::process-byte s 27 input-state)))
    (is (null (cl-tmux::process-byte s 91 input-state)))
    (is (null (cl-tmux::process-byte s 65 input-state)))
    (is (= 1 (screen-copy-offset screen))
        "up-arrow at top row scrolls the viewport back by 1")))

(test process-byte-copy-mode-down-arrow-end-to-end
  "ESC [ B (down arrow) in copy mode scrolls the viewport forward when the cursor is
   already at the bottom row and a scrolled-up viewport is active."
  (with-copy-mode-state (s screen input-state)
    (seed-scrollback screen 10)
    ;; Scroll the viewport up so there is room to scroll back down.
    (cl-tmux/commands::copy-mode-scroll screen 6)
    (is (= 6 (screen-copy-offset screen)) "pre-scrolled up 6 lines")
    ;; Force cursor to the bottom row so the next down-arrow scrolls the viewport.
    (setf (cl-tmux/terminal/types:screen-copy-cursor screen)
          (cons (1- (screen-height screen)) 0))
    ;; ESC [ B, one byte at a time.
    (is (null (cl-tmux::process-byte s 27 input-state)))
    (is (null (cl-tmux::process-byte s 91 input-state)))
    (is (null (cl-tmux::process-byte s 66 input-state)))   ; 'B' = down
    (is (= 5 (screen-copy-offset screen))
        "down-arrow at bottom row scrolls the viewport forward by 1 (6 - 1)")))

(test process-byte-esc-not-bracket-flushes
  "ESC followed by a non-'[' byte is not an arrow sequence: the continuation
   flushes the two accumulated bytes through and returns to ground state."
  (with-copy-mode-state (s screen input-state)
    (is (null (cl-tmux::process-byte s 27 input-state)))
    ;; 'x' (120) is not '[': the buffer (ESC x) flushes and returns to ground.
    (is (null (cl-tmux::process-byte s 120 input-state)))
    (is (zerop (screen-copy-offset screen))
        "a flushed (non-arrow) escape does not scroll the copy-offset")))

(test process-byte-esc-not-copy-mode-forwards-directly
  "Outside copy mode, ESC is an ordinary byte forwarded to the pane — the
   CPS state remains in ground state (no escape accumulation)."
  (with-fake-session (s)
    (with-input-state (input-state)
      (is-false (cl-tmux::%copy-mode-active-p s) "not in copy mode")
      (is (null (cl-tmux::process-byte s 27 input-state)))
      ;; After forwarding ESC outside copy-mode the state returns to ground:
      ;; the next ordinary byte should also be forwarded (no stuck state).
      (is (null (cl-tmux::process-byte s (char-code #\a) input-state))
          "byte after ESC (non-copy-mode) is also forwarded cleanly"))))

;;; ── %handle-resize / %handle-dirty extracted handlers ────────────────────────

(test handle-resize-updates-term-size
  "%handle-resize clears *resize-pending* and relayouts the active window."
  (with-fake-session (s :nwindows 1)
    (let ((cl-tmux::*resize-pending* t)
          (cl-tmux::*term-rows* 10)
          (cl-tmux::*term-cols* 40))
      ;; terminal-size returns real size in sandbox, which may differ from 10x40.
      ;; Just assert *resize-pending* is cleared and no error is signalled.
      (cl-tmux::%handle-resize s)
      (is-false cl-tmux::*resize-pending*
                "*resize-pending* must be NIL after %handle-resize"))))

(test handle-resize-fires-client-resized-hook
  "%handle-resize fires +hook-client-resized+ after relaying out the window."
  (with-isolated-hooks
    (let ((fired nil))
      (with-fake-session (s :nwindows 1)
        (let ((cl-tmux::*resize-pending* t))
          (cl-tmux/hooks:add-hook cl-tmux/hooks:+hook-client-resized+
                                  (lambda (&rest _) (declare (ignore _)) (setf fired t)))
          (cl-tmux::%handle-resize s)
          (is-true fired "client-resized hook must fire on terminal resize"))))))

(test handle-dirty-clears-flag
  "%handle-dirty clears *dirty* and renders without error."
  (with-fake-session (s :nwindows 1)
    (let ((cl-tmux::*dirty* t)
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

(test dispatch-macro-definitions
  "Event-dispatch macros used by the CPS state machine are all defined."
  (dolist (sym '(cl-tmux::define-copy-mode-escape-table
                 cl-tmux::define-cps-state
                 cl-tmux::define-prompt-key-rules
                 cl-tmux::define-copy-mode-vi-rules))
    (is (macro-function sym) "~S must be a defined macro" sym)))

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
    (let ((fired nil))
      (with-fake-session (s)
        (cl-tmux/hooks:add-hook "pane-focus-in"
                                (lambda (&rest _) (declare (ignore _)) (setf fired t)))
        (with-input-state (input-state)
          (dolist (b '(27 91 73)) (cl-tmux::process-byte s b input-state)))  ; ESC [ I
        (is-true fired "ESC [ I must fire pane-focus-in")))))

(test process-byte-focus-out-fires-pane-focus-out
  "ESC [ O (outer-terminal focus lost, ?1004) fires pane-focus-out on the active pane."
  (with-isolated-hooks
    (let ((fired nil))
      (with-fake-session (s)
        (cl-tmux/hooks:add-hook "pane-focus-out"
                                (lambda (&rest _) (declare (ignore _)) (setf fired t)))
        (with-input-state (input-state)
          (dolist (b '(27 91 79)) (cl-tmux::process-byte s b input-state)))  ; ESC [ O
        (is-true fired "ESC [ O must fire pane-focus-out")))))

(test x10-mouse-sequence-via-process-byte
  "X10 mouse press ESC [ M <btn+32> <col+33> <row+33> fed one byte at a time
   selects the pane at the encoded coordinates."
  (with-two-pane-mouse-session (sess win p0 p1)
    ;; Enable per-screen mouse mode in addition to the session option.
    (setf (screen-mouse-mode (pane-screen p0)) 1)
    (with-input-state (input-state)
      ;; X10: btn=0 → 0+32=32; col=50 → 50+33=83; row=5 → 5+33=38
      ;; Sequence: ESC(27) [(91) M(77) 32 83 38
      (cl-tmux::process-byte sess 27 input-state)
      (cl-tmux::process-byte sess 91 input-state)
      (cl-tmux::process-byte sess 77 input-state)
      (cl-tmux::process-byte sess 32 input-state)
      (cl-tmux::process-byte sess 83 input-state)
      (cl-tmux::process-byte sess 38 input-state)
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
  (with-fake-session (s)
    (with-input-state (input-state)
      ;; C-b (prefix)
      (is (null (cl-tmux::process-byte s 2 input-state)))
      ;; Unbound key: '@' (64) — not in prefix key-table
      (is (null (cl-tmux::process-byte s (char-code #\@) input-state))
          "unbound prefix key must return NIL (discarded, not forwarded)")
      ;; State must be back to ground: next ordinary byte is forwarded cleanly.
      (is (null (cl-tmux::process-byte s (char-code #\a) input-state))
          "state returned to ground after discarding unbound prefix key"))))

;;; ── Copy-mode plain 'q' exits ────────────────────────────────────────────────

(test copy-mode-plain-q-exits
  "Plain 'q' (byte 113) exits copy mode without needing C-b prefix."
  (with-copy-mode-state (s screen input-state)
    (is (screen-copy-mode-p screen) "copy mode entered")
    ;; Feed plain 'q' without any prefix
    (cl-tmux::process-byte s (char-code #\q) input-state)
    (is-false (screen-copy-mode-p screen)
        "plain q must exit copy mode")))

(test copy-mode-plain-esc-clears-selection-stays-in-copy-mode
  "ESC followed by a non-CSI byte clears the selection but STAYS in copy mode.
   This matches tmux's default copy-mode-vi Escape → clear-selection binding.
   Use q or i to exit copy mode."
  (with-copy-mode-state (s screen input-state)
    (is (screen-copy-mode-p screen) "copy mode must be active after enter")
    ;; Start a selection so we can verify it gets cleared.
    (cl-tmux/commands::copy-mode-begin-selection screen)
    (is (screen-copy-selecting screen) "selection must be active before ESC")
    ;; Feed ESC then a non-CSI byte (not '[')
    (cl-tmux::process-byte s 27 input-state)
    (cl-tmux::process-byte s (char-code #\x) input-state)
    ;; Selection is cleared but copy-mode stays active.
    (is-true  (screen-copy-mode-p   screen) "copy mode must remain active after ESC")
    (is-false (screen-copy-selecting screen) "selection must be cleared by ESC")))

;;; ── Copy-mode unprefixed vi navigation ───────────────────────────────────────

(test copy-mode-j-scrolls-down
  "Plain 'j' (byte 106) scrolls down 1 line in copy mode."
  (with-copy-mode-state (s screen input-state)
    (seed-scrollback screen 10)
    ;; Scroll up 5 first so there's room to scroll back down.
    (cl-tmux/commands::copy-mode-scroll screen 5)
    (is (= 5 (screen-copy-offset screen)))
    (cl-tmux::process-byte s (char-code #\j) input-state)
    (is (= 4 (screen-copy-offset screen))
        "j must scroll copy offset down by 1")))

(test copy-mode-k-scrolls-up
  "Plain 'k' moves the cursor up; when the cursor is already at row 0, it scrolls
   the viewport back toward older content by 1 line."
  (with-copy-mode-state (s screen input-state)
    (seed-scrollback screen 10)
    ;; Force cursor to the top row so the next k scrolls the viewport.
    (setf (cl-tmux/terminal/types:screen-copy-cursor screen) (cons 0 0))
    (is (zerop (screen-copy-offset screen)))
    (cl-tmux::process-byte s (char-code #\k) input-state)
    (is (= 1 (screen-copy-offset screen))
        "k at top row scrolls copy offset up by 1")))

(test copy-mode-g-jumps-to-top
  "Plain 'g' (byte 103) jumps to top of scrollback in copy mode."
  (with-copy-mode-state (s screen input-state)
    (seed-scrollback screen 10)
    (cl-tmux::process-byte s (char-code #\g) input-state)
    (is (= 10 (screen-copy-offset screen))
        "g must jump to top (max scrollback offset)")))

(test copy-mode-G-jumps-to-bottom
  "Plain 'G' (byte 71) jumps to bottom (live view) in copy mode."
  (with-copy-mode-state (s screen input-state)
    (seed-scrollback screen 10)
    ;; Scroll up first
    (cl-tmux/commands::copy-mode-scroll screen 8)
    (is (= 8 (screen-copy-offset screen)))
    (cl-tmux::process-byte s (char-code #\G) input-state)
    (is (zerop (screen-copy-offset screen))
        "G must jump to bottom (offset = 0)")))
