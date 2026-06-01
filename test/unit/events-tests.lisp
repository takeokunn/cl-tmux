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

(test process-byte-dismisses-overlay
  "While an overlay is shown, any key dismisses it and is consumed (returns NIL)."
  (let ((s (make-fake-session)))
    (let ((*overlay* nil) (cl-tmux::*dirty* nil))
      (show-overlay "help")
      (let ((state (cl-tmux::make-input-state)))
        (is (null (cl-tmux::process-byte s (char-code #\x) state)))
        (is (not (overlay-active-p)) "any key dismisses the overlay")))))

;;; ── Copy-mode arrow escapes through process-byte (one byte at a time) ────────
;;;
;;; In copy mode an arrow key arrives as three separate bytes (ESC, '[', 'A').
;;; process-byte must arm escape-pending on the lone ESC, keep it armed after
;;; '[' (an incomplete ESC [ … sequence), then on the final byte dispatch the
;;; copy-mode scroll and disarm.  copy-mode-scroll clamps the offset to
;;; [0, (length scrollback)], so we seed the active screen's scrollback with a
;;; few dummy rows; otherwise max-offset is 0 and the offset can never advance.

(test process-byte-copy-mode-up-arrow-end-to-end
  "ESC [ A fed one byte at a time while in copy mode scrolls the copy-offset
   up (toward older output) by 3 lines."
  (let ((s (make-fake-session)))
    (with-loop-state
      (let ((screen (active-screen s))
            (state  (cl-tmux::make-input-state)))
        (cl-tmux::dispatch-command s :copy-mode-enter nil)
        (seed-scrollback screen 10)
        (is (zerop (screen-copy-offset screen)) "offset starts at the live view")
        ;; ESC [ A, one byte at a time.
        (is (null (cl-tmux::process-byte s 27 state)))
        (is (null (cl-tmux::process-byte s 91 state)))
        (is (null (cl-tmux::process-byte s 65 state)))
        (is (= 3 (screen-copy-offset screen))
            "up-arrow scrolled the copy-offset up 3 lines")))))

(test process-byte-copy-mode-down-arrow-end-to-end
  "ESC [ B fed one byte at a time while in copy mode scrolls the copy-offset
   back down (toward the live view) by 3 lines."
  (let ((s (make-fake-session)))
    (with-loop-state
      (let ((screen (active-screen s))
            (state  (cl-tmux::make-input-state)))
        (cl-tmux::dispatch-command s :copy-mode-enter nil)
        (seed-scrollback screen 10)
        ;; Start scrolled up so a down-arrow has somewhere to go.
        (cl-tmux/commands::copy-mode-scroll screen 6)
        (is (= 6 (screen-copy-offset screen)) "pre-scrolled up 6 lines")
        ;; ESC [ B, one byte at a time.
        (is (null (cl-tmux::process-byte s 27 state)))
        (is (null (cl-tmux::process-byte s 91 state)))
        (is (null (cl-tmux::process-byte s 66 state)))   ; 'B' = down
        (is (= 3 (screen-copy-offset screen))
            "down-arrow scrolled the copy-offset down 3 lines (6 - 3)")))))

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
  (let ((*prompt* nil) (cl-tmux::*dirty* nil))
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
  (let ((*prompt* nil) (cl-tmux::*dirty* nil))
    (let ((submitted nil))
      (prompt-start "rename-window" "abc"
                    (lambda (buf) (setf submitted buf)))
      (cl-tmux::handle-prompt-key 27)
      (is (not (prompt-active-p)) "Esc dismisses the prompt")
      (is (null submitted) "Esc does not run the on-submit closure"))))

(test handle-prompt-key-backspace-deletes-last-char
  "Backspace (127) and BS (8) delete the last character of the prompt buffer."
  (let ((*prompt* nil) (cl-tmux::*dirty* nil))
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
