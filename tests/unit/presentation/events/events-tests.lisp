(in-package #:cl-tmux/test)

;;;; copy-mode-escape, process-byte, copy-mode-arrows, resize/dirty, handle-prompt-key, key-table — part I

(describe "events-suite"

  ;; ── Copy-mode escape handler ─────────────────────────────────────────────────

  ;; Arrow-key escape sequences are consumed while copy mode is active.
  (it "handle-copy-mode-escape-consumes-arrows"
    (with-fake-session (s)
      (cl-tmux::dispatch-command s :copy-mode-enter nil)
      (dolist (row (list (list (make-array 3 :element-type '(unsigned-byte 8)
                                             :initial-contents '(27 91 65))   ; ESC [ A (up)
                               "up-arrow should be consumed in copy mode")
                         (list (make-array 3 :element-type '(unsigned-byte 8)
                                             :initial-contents '(27 91 66))   ; ESC [ B (down)
                               "down-arrow should be consumed in copy mode")))
        (destructuring-bind (buf desc) row
          (declare (ignore desc))
          (expect (cl-tmux::handle-copy-mode-escape s buf))))
      (expect (screen-copy-mode-p (active-screen s)) :to-be-truthy)))

  ;; Outside copy mode, handle-copy-mode-escape consumes nothing.
  (it "handle-copy-mode-escape-inactive-returns-nil"
    (with-fake-session (s)
      (expect (null (cl-tmux::handle-copy-mode-escape
                     s (make-array 3 :element-type '(unsigned-byte 8)
                                     :initial-contents '(27 91 65)))))))

  ;; ── process-byte: the shared keystroke pipeline ─────────────────────────────
  ;;
  ;; process-byte is what the in-process event loop AND the client/server attach
  ;; loop both feed bytes to, so verifying it covers the byte-routing that the
  ;; blocking event-loop itself can't be unit-tested for.  +prefix-key-code+ is 2.

  ;; Prefix byte (2) then 'n' routes through the binding table to :next-window.
  (it "process-byte-prefix-then-command"
    (with-fake-session (s :nwindows 2)
      (with-input-state (input-state)
        ;; Lone prefix byte just transitions to the after-prefix state, no quit.
        (expect (null (cl-tmux::process-byte s 2 input-state)))
        ;; Following byte 'n' selects the next window.
        (expect (null (cl-tmux::process-byte s (char-code #\n) input-state)))
        (expect (eq (second (session-windows s)) (session-active-window s))))))

  ;; Prefix byte then 'd' returns :detach from process-byte.
  (it "process-byte-prefix-detach-returns-detach"
    ;; Isolate the key-tables: another suite can mutate the live prefix table, and
    ;; this test depends on the default #\d → :detach binding being present.
    (with-isolated-config
      (with-fake-session (s)
        (with-input-state (input-state)
          (cl-tmux::process-byte s 2 input-state)
          (expect (eq :detach (cl-tmux::process-byte s (char-code #\d) input-state)))))))

  ;; An ordinary byte (no prefix) is forwarded and returns NIL (no quit).
  (it "process-byte-ordinary-key-forwards"
    (with-fake-session (s)
      (with-input-state (input-state)
        ;; fd -1 panes make pty-write a harmless no-op; we assert routing only.
        (expect (null (cl-tmux::process-byte s (char-code #\x) input-state))))))

  ;; While a prompt is active, process-byte edits the prompt buffer.
  (it "process-byte-routes-to-active-prompt"
    (with-fake-session (s)
      (let ((*prompt* nil))
        (prompt-start "rename-window" "" (lambda (name) (declare (ignore name)) nil))
        (with-input-state (input-state)
          (cl-tmux::process-byte s (char-code #\h) input-state)
          (cl-tmux::process-byte s (char-code #\i) input-state)
          (expect (string= "hi" (prompt-buffer *prompt*)))))))

  ;; While an overlay is shown, q dismisses it; other keys are swallowed (overlay stays open).
  (it "process-byte-overlay-q-dismisses"
    (with-fake-session (s)
      (let ((*overlay* nil) (cl-tmux::*dirty* nil))
        (show-overlay "help text")
        (with-input-state (input-state)
          ;; An ordinary key ('x') is swallowed but the overlay stays open.
          (expect (null (cl-tmux::process-byte s (char-code #\x) input-state)))
          (assert-overlay-active "ordinary key must not dismiss the overlay")
          ;; 'q' dismisses the overlay.
          (expect (null (cl-tmux::process-byte s (char-code #\q) input-state)))
          (assert-overlay-inactive "q must dismiss the overlay")))))

  ;; ── Copy-mode arrow escapes through process-byte (one byte at a time) ────────
  ;;
  ;; In copy mode an arrow key arrives as three separate bytes (ESC, '[', 'A').
  ;; process-byte must arm escape-pending on the lone ESC, keep it armed after
  ;; '[' (an incomplete ESC [ … sequence), then on the final byte dispatch the
  ;; copy-mode scroll and disarm.  copy-mode-scroll clamps the offset to
  ;; [0, (length scrollback)], so we seed the active screen's scrollback with a
  ;; few dummy rows; otherwise max-offset is 0 and the offset can never advance.

  ;; ESC [ A (up arrow) in copy mode scrolls the viewport when the cursor is already
  ;; at the top row (row 0) and scrollback is available.
  (it "process-byte-copy-mode-up-arrow-end-to-end"
    (with-copy-mode-state (s screen input-state)
      (seed-scrollback screen 10)
      ;; Force cursor to the top row so the next up-arrow scrolls the viewport.
      (setf (cl-tmux/terminal/types:screen-copy-cursor screen) (cons 0 0))
      (expect (zerop (screen-copy-offset screen)))
      ;; ESC [ A, one byte at a time.
      (expect (null (feed-bytes s input-state '(27 91 65))))
      (expect (= 1 (screen-copy-offset screen)))))

  ;; ESC [ B (down arrow) in copy mode scrolls the viewport forward when the cursor is
  ;; already at the bottom row and a scrolled-up viewport is active.
  (it "process-byte-copy-mode-down-arrow-end-to-end"
    (with-copy-mode-state (s screen input-state)
      (seed-scrollback screen 10)
      ;; Scroll the viewport up so there is room to scroll back down.
      (cl-tmux/commands::copy-mode-scroll screen 6)
      (expect (= 6 (screen-copy-offset screen)))
      ;; Force cursor to the bottom row so the next down-arrow scrolls the viewport.
      (setf (cl-tmux/terminal/types:screen-copy-cursor screen)
            (cons (1- (screen-height screen)) 0))
      ;; ESC [ B, one byte at a time.
      (expect (null (feed-bytes s input-state '(27 91 66))))   ; 'B' = down
      (expect (= 5 (screen-copy-offset screen)))))

  ;; ESC followed by a non-'[' byte is not an arrow sequence: the continuation
  ;; flushes the two accumulated bytes through and returns to ground state.
  (it "process-byte-esc-not-bracket-flushes"
    (with-copy-mode-state (s screen input-state)
      (expect (null (cl-tmux::process-byte s 27 input-state)))
      ;; 'x' (120) is not '[': the buffer (ESC x) flushes and returns to ground.
      (expect (null (cl-tmux::process-byte s 120 input-state)))
      (expect (zerop (screen-copy-offset screen)))))

  ;; Outside copy mode, ESC is an ordinary byte forwarded to the pane — the
  ;; CPS state remains in ground state (no escape accumulation).
  (it "process-byte-esc-not-copy-mode-forwards-directly"
    (with-fake-session (s)
      (with-input-state (input-state)
        (expect (cl-tmux::%copy-mode-active-p s) :to-be-falsy)
        (expect (null (cl-tmux::process-byte s 27 input-state)))
        ;; After forwarding ESC outside copy-mode the state returns to ground:
        ;; the next ordinary byte should also be forwarded (no stuck state).
        (expect (null (cl-tmux::process-byte s (char-code #\a) input-state))))))

  ;; ── %handle-resize / %handle-dirty extracted handlers ────────────────────────

  ;; %handle-resize clears *resize-pending* and relayouts the active window.
  (it "handle-resize-updates-term-size"
    (with-fake-session (s :nwindows 1)
      (let ((cl-tmux::*resize-pending* t)
            (cl-tmux::*term-rows* 10)
            (cl-tmux::*term-cols* 40))
        ;; terminal-size returns real size in sandbox, which may differ from 10x40.
        ;; Just assert *resize-pending* is cleared and no error is signalled.
        (cl-tmux::%handle-resize s)
        (expect cl-tmux::*resize-pending* :to-be-falsy))))

  ;; %handle-resize fires +hook-client-resized+ after relaying out the window.
  (it "handle-resize-fires-client-resized-hook"
    (with-isolated-hooks
      (let ((fired nil))
        (with-fake-session (s :nwindows 1)
          (let ((cl-tmux::*resize-pending* t))
            (cl-tmux/hooks:add-hook cl-tmux/hooks:+hook-client-resized+
                                    (lambda (&rest _) (declare (ignore _)) (setf fired t)))
            (cl-tmux::%handle-resize s)
            (expect fired :to-be-truthy))))))

  ;; %handle-dirty clears *dirty* and renders without error.
  (it "handle-dirty-clears-flag"
    (with-fake-session (s :nwindows 1)
      (let ((cl-tmux::*dirty* t)
            (cl-tmux::*term-rows* 10)
            (cl-tmux::*term-cols* 40))
        (cl-tmux::%handle-dirty s)
        (expect cl-tmux::*dirty* :to-be-falsy))))

  ;; ── handle-prompt-key: prompt editing keys ───────────────────────────────────

  ;; Enter (13) runs the prompt's on-submit closure with the buffer, then dismisses
  ;; the prompt.
  (it "handle-prompt-key-enter-submits-and-dismisses"
    (with-clean-prompt
      (let ((submitted nil))
        (prompt-start "rename-window" "hello"
                      (lambda (buf) (setf submitted buf)))
        (cl-tmux::handle-prompt-key 13)
        (expect (string= "hello" submitted))
        (expect (prompt-active-p) :to-be-falsy)
        (expect cl-tmux::*dirty* :to-be-truthy))))

  ;; Esc (27) cancels the prompt without running on-submit.
  (it "handle-prompt-key-esc-cancels"
    (with-clean-prompt
      (let ((submitted nil))
        (prompt-start "rename-window" "abc"
                      (lambda (buf) (setf submitted buf)))
        (cl-tmux::handle-prompt-key 27)
        (expect (prompt-active-p) :to-be-falsy)
        (expect (null submitted)))))

  ;; Backspace (127) and BS (8) delete the last character of the prompt buffer.
  (it "handle-prompt-key-backspace-deletes-last-char"
    (with-clean-prompt
      (prompt-start "rename-window" "abc"
                    (lambda (buf) (declare (ignore buf)) nil))
      (cl-tmux::handle-prompt-key 127)
      (expect (string= "ab" (prompt-buffer *prompt*)))
      (cl-tmux::handle-prompt-key 8)
      (expect (string= "a" (prompt-buffer *prompt*)))))

  ;; Event-dispatch macros used by the CPS state machine are all defined.
  (it "dispatch-macro-definitions"
    (dolist (sym '(cl-tmux::define-copy-mode-escape-table
                   cl-tmux::define-cps-state
                   cl-tmux::define-prompt-key-rules))
      (expect (macro-function sym))))

  ;; ESC [ I/O fire pane-focus-in/pane-focus-out hooks on the active pane.
  (it "process-byte-focus-hook-table"
    (dolist (c '((73 "pane-focus-in"  "ESC [ I must fire pane-focus-in")
                 (79 "pane-focus-out" "ESC [ O must fire pane-focus-out")))
      (destructuring-bind (last-byte hook-name desc) c
        (declare (ignore desc))
        (with-isolated-hooks
          (let ((fired nil))
            (with-fake-session (s)
              (cl-tmux/hooks:add-hook hook-name
                                      (lambda (&rest _) (declare (ignore _)) (setf fired t)))
              (with-input-state (input-state)
                (feed-bytes s input-state (list 27 91 last-byte)))
              (expect fired :to-be-truthy)))))))

  ;; ── Unbound prefix key discard ───────────────────────────────────────────────

  ;; An unbound key after prefix (C-b) is silently discarded; no bytes forwarded,
  ;; no crash, and process-byte returns NIL.
  (it "unbound-prefix-key-is-discarded"
    (with-fake-session (s)
      (with-input-state (input-state)
        ;; C-b (prefix)
        (expect (null (cl-tmux::process-byte s 2 input-state)))
        ;; Unbound key: '@' (64) — not in prefix key-table
        (expect (null (cl-tmux::process-byte s (char-code #\@) input-state)))
        ;; State must be back to ground: next ordinary byte is forwarded cleanly.
        (expect (null (cl-tmux::process-byte s (char-code #\a) input-state))))))

  ;; ── Copy-mode plain 'q' exits ────────────────────────────────────────────────

  ;; Plain 'q' (byte 113) exits copy mode without needing C-b prefix.
  (it "copy-mode-plain-q-exits"
    (with-copy-mode-state (s screen input-state)
      (expect (screen-copy-mode-p screen))
      ;; Feed plain 'q' without any prefix
      (cl-tmux::process-byte s (char-code #\q) input-state)
      (expect (screen-copy-mode-p screen) :to-be-falsy)))

  ;; ESC followed by a non-CSI byte clears the selection but STAYS in copy mode.
  ;; This matches tmux's default copy-mode-vi Escape → clear-selection binding.
  ;; Use q or i to exit copy mode.
  (it "copy-mode-plain-esc-clears-selection-stays-in-copy-mode"
    (with-copy-mode-state (s screen input-state)
      (expect (screen-copy-mode-p screen))
      ;; Start a selection so we can verify it gets cleared.
      (cl-tmux/commands::copy-mode-begin-selection screen)
      (expect (screen-copy-selecting screen))
      ;; Feed ESC then a non-CSI byte (not '[') with no M-<key> binding.
      (cl-tmux::process-byte s 27 input-state)
      (cl-tmux::process-byte s (char-code #\@) input-state)
      ;; Selection is cleared but copy-mode stays active.
      (expect (screen-copy-mode-p   screen) :to-be-truthy)
      (expect (screen-copy-selecting screen) :to-be-falsy)))

  ;; ── Copy-mode unprefixed vi navigation ───────────────────────────────────────

  ;; Plain 'j' (byte 106) scrolls down 1 line in copy mode.
  (it "copy-mode-j-scrolls-down"
    (with-copy-mode-vi-state (s screen input-state)
      (seed-scrollback screen 10)
      ;; Scroll up 5 first so there's room to scroll back down.
      (cl-tmux/commands::copy-mode-scroll screen 5)
      (expect (= 5 (screen-copy-offset screen)))
      (cl-tmux::process-byte s (char-code #\j) input-state)
      (expect (= 4 (screen-copy-offset screen)))))

  ;; Plain 'k' moves the cursor up; when the cursor is already at row 0, it scrolls
  ;; the viewport back toward older content by 1 line.
  (it "copy-mode-k-scrolls-up"
    (with-copy-mode-vi-state (s screen input-state)
      (seed-scrollback screen 10)
      ;; Force cursor to the top row so the next k scrolls the viewport.
      (setf (cl-tmux/terminal/types:screen-copy-cursor screen) (cons 0 0))
      (expect (zerop (screen-copy-offset screen)))
      (cl-tmux::process-byte s (char-code #\k) input-state)
      (expect (= 1 (screen-copy-offset screen)))))

  ;; Plain 'g' (byte 103) jumps to top of scrollback in copy mode.
  (it "copy-mode-g-jumps-to-top"
    (with-copy-mode-vi-state (s screen input-state)
      (seed-scrollback screen 10)
      (cl-tmux::process-byte s (char-code #\g) input-state)
      (expect (= 10 (screen-copy-offset screen)))))

  ;; Plain 'G' (byte 71) jumps to bottom (live view) in copy mode.
  (it "copy-mode-G-jumps-to-bottom"
    (with-copy-mode-vi-state (s screen input-state)
      (seed-scrollback screen 10)
      ;; Scroll up first
      (cl-tmux/commands::copy-mode-scroll screen 8)
      (expect (= 8 (screen-copy-offset screen)))
      (cl-tmux::process-byte s (char-code #\G) input-state)
      (expect (zerop (screen-copy-offset screen))))))
