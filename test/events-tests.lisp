(in-package #:cl-tmux/test)

;;;; Event-dispatch tests (events.lisp).
;;;;
;;;; dispatch-command, the prefix-key router, the copy-mode escape handler and
;;;; the cyclic helpers are exercised here against hand-built sessions whose
;;;; panes use a fake fd (-1) — no PTY is forked and no shell is spawned, so
;;;; these run anywhere.  Forking commands (:new-window, :split-*) are NOT
;;;; dispatched here (they start reader threads that outlive the test); their
;;;; underlying window/pane logic is covered by model-tests.

(def-suite events-suite :description "Event loop dispatch and prefix routing")
(in-suite events-suite)

;;; ── Fixtures (no PTY) ───────────────────────────────────────────────────────

(defun make-fake-window (id name &key (npanes 1))
  "A window with NPANES fake panes (fd -1); the first pane is active."
  (let* ((panes (loop for i below npanes
                      collect (make-pane :id (1+ i) :x 0 :y 0 :width 20 :height 5
                                         :fd -1 :screen (make-screen 20 5))))
         (win   (make-window :id id :name name :width 20 :height 5 :panes panes)))
    (window-select-pane win (first panes))
    win))

(defun make-fake-session (&key (nwindows 1) (npanes 1))
  "A session of NWINDOWS fake windows (each with NPANES fake panes), no PTYs."
  (let* ((windows (loop for i below nwindows
                        collect (make-fake-window (1+ i) (format nil "~D" (1+ i))
                                                  :npanes npanes)))
         (sess    (make-session :id 1 :name "0" :windows windows)))
    (session-select-window sess (first windows))
    sess))

(defun active-screen (session)
  (pane-screen (window-active-pane (session-active-window session))))

(defmacro with-loop-state (&body body)
  "Dynamically bind the event-loop specials so dispatch side effects are isolated."
  `(let ((cl-tmux::*running* t) (cl-tmux::*dirty* nil)) ,@body))

;;; ── Cyclic helpers (pure) ───────────────────────────────────────────────────

(test next-cyclic-wraps
  "next-cyclic advances and wraps past the end."
  (is (eql 'b (cl-tmux::next-cyclic '(a b c) 'a)))
  (is (eql 'a (cl-tmux::next-cyclic '(a b c) 'c)))
  (is (eql 'b (cl-tmux::next-cyclic '(a b c) 'missing))))   ; unknown → idx 0 → element 1

(test prev-cyclic-wraps
  "prev-cyclic steps back and wraps past the front."
  (is (eql 'a (cl-tmux::prev-cyclic '(a b c) 'b)))
  (is (eql 'c (cl-tmux::prev-cyclic '(a b c) 'a))))

;;; ── Window / pane selection ────────────────────────────────────────────────

(test dispatch-next-window-cycles
  "C-b n moves to the next window and wraps around."
  (let ((s (make-fake-session :nwindows 2)))
    (with-loop-state
      (cl-tmux::dispatch-command s :next-window nil)
      (is (eq (second (session-windows s)) (session-active-window s)))
      (cl-tmux::dispatch-command s :next-window nil)
      (is (eq (first (session-windows s)) (session-active-window s))
          "second :next-window should wrap back to window 1"))))

(test dispatch-prev-window-wraps
  "C-b p from the first window wraps to the last."
  (let ((s (make-fake-session :nwindows 2)))
    (with-loop-state
      (cl-tmux::dispatch-command s :prev-window nil)
      (is (eq (second (session-windows s)) (session-active-window s))))))

(test dispatch-next-pane-cycles
  "C-b o moves to the next pane within the active window."
  (let* ((s   (make-fake-session :nwindows 1 :npanes 2))
         (win (session-active-window s))
         (p0  (first  (window-panes win)))
         (p1  (second (window-panes win))))
    (with-loop-state
      (is (eq p0 (window-active-pane win)))
      (cl-tmux::dispatch-command s :next-pane nil)
      (is (eq p1 (window-active-pane win))))))

(test dispatch-select-window-by-digit
  "C-b <n>: :select-window uses the pressed digit byte to pick the window."
  (let ((s (make-fake-session :nwindows 3)))
    (with-loop-state
      (cl-tmux::dispatch-command s :select-window (char-code #\2))
      (is (eq (third (session-windows s)) (session-active-window s)))
      (cl-tmux::dispatch-command s :select-window (char-code #\0))
      (is (eq (first (session-windows s)) (session-active-window s))))))

(test dispatch-unknown-command-passes-through
  "An unrecognized command falls through to the passthrough branch: it returns
   NIL (no quit), doesn't error, and still marks the session dirty."
  (let ((s (make-fake-session)))
    (with-loop-state
      (is (null (cl-tmux::dispatch-command s :no-such-command (char-code #\a))))
      (is-true cl-tmux::*dirty* "dispatch marks dirty even on passthrough"))))

;;; ── Rename ──────────────────────────────────────────────────────────────────

(test dispatch-rename-window-opens-prompt
  "C-b , opens a rename prompt seeded with the active window's name, and its
   on-submit closure renames the active window."
  (let ((s (make-fake-session :nwindows 1)))
    (let ((*prompt* nil) (cl-tmux::*dirty* nil) (cl-tmux::*running* t))
      (cl-tmux::dispatch-command s :rename-window nil)
      (is (prompt-active-p) "rename should open a prompt")
      (is (string= "1" (prompt-buffer *prompt*))
          "prompt seeded with current window name")
      (is (functionp (prompt-on-submit *prompt*))
          "prompt should carry an on-submit closure")
      ;; Running the closure with a new name renames the active window.
      (funcall (prompt-on-submit *prompt*) "renamed")
      (is (string= "renamed" (window-name (session-active-window s)))
          "on-submit closure should rename the active window"))))

;;; ── Copy mode ───────────────────────────────────────────────────────────────

(test dispatch-copy-mode-enter-exit
  "C-b [ enters copy mode on the active screen; exit clears it."
  (let ((s (make-fake-session)))
    (with-loop-state
      (cl-tmux::dispatch-command s :copy-mode-enter nil)
      (is (screen-copy-mode-p (active-screen s)) "copy mode should be on after enter")
      (cl-tmux::dispatch-command s :copy-mode-exit nil)
      (is (not (screen-copy-mode-p (active-screen s))) "copy mode should be off after exit"))))

(test copy-mode-active-p-reflects-state
  "copy-mode-active-p tracks the active screen's copy-mode flag."
  (let ((s (make-fake-session)))
    (with-loop-state
      (is (not (cl-tmux::copy-mode-active-p s)))
      (cl-tmux::dispatch-command s :copy-mode-enter nil)
      (is (cl-tmux::copy-mode-active-p s)))))

;;; ── Detach / kill ───────────────────────────────────────────────────────────

(test dispatch-detach-returns-detach
  "C-b d returns :detach and does NOT clear *running* itself (the caller decides:
   standalone stops, a server merely disconnects the client)."
  (let ((s (make-fake-session)))
    (let ((cl-tmux::*running* t) (cl-tmux::*dirty* nil))
      (is (eq :detach (cl-tmux::dispatch-command s :detach nil)))
      (is-true cl-tmux::*running* "dispatch-command must not clear *running*"))))

(test dispatch-kill-last-window-quits
  "Killing the only window ends the session (:quit)."
  (let ((s (make-fake-session :nwindows 1)))
    (with-loop-state
      (is (eq :quit (cl-tmux::dispatch-command s :kill-window nil))))))

(test dispatch-kill-one-of-two-windows-survives
  "Killing one of two windows leaves the session running with the other."
  (let ((s (make-fake-session :nwindows 2)))
    (with-loop-state
      (is (null (cl-tmux::dispatch-command s :kill-window nil)))
      (is (= 1 (length (session-windows s)))))))

(test dispatch-kill-last-pane-quits
  "Killing the sole pane of the sole window ends the session (:quit)."
  (let ((s (make-fake-session :nwindows 1 :npanes 1)))
    (with-loop-state
      (is (eq :quit (cl-tmux::dispatch-command s :kill-pane nil))))))

;;; ── Prefix routing & copy-mode escape sequences ─────────────────────────────

(test dispatch-prefix-routes-binding
  "dispatch-prefix-command looks the byte up in the binding table (d → detach)."
  (let ((s (make-fake-session)))
    (let ((cl-tmux::*running* t) (cl-tmux::*dirty* nil))
      (is (eq :detach (cl-tmux::dispatch-prefix-command s (char-code #\d)))))))

(test prefix-q-exits-copy-mode
  "In copy mode, the prefix-routed 'q' exits copy mode."
  (let ((s (make-fake-session)))
    (with-loop-state
      (cl-tmux::dispatch-command s :copy-mode-enter nil)
      (cl-tmux::dispatch-prefix-command s (char-code #\q))
      (is (not (screen-copy-mode-p (active-screen s)))))))

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
        ;; Lone prefix byte just arms prefix-pending, no quit.
        (is (null (cl-tmux::process-byte s 2 state)))
        (is (cl-tmux::input-state-prefix-pending state) "prefix armed")
        ;; Following byte 'n' selects the next window.
        (is (null (cl-tmux::process-byte s (char-code #\n) state)))
        (is (not (cl-tmux::input-state-prefix-pending state)) "prefix consumed")
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
        (is (null (cl-tmux::process-byte s (char-code #\x) state)))
        (is (not (cl-tmux::input-state-prefix-pending state)))))))

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

;;; ── list-keys overlay ───────────────────────────────────────────────────────

(test dispatch-list-keys-shows-overlay
  "C-b ? opens the key-binding help overlay."
  (let ((s (make-fake-session)))
    (let ((*overlay* nil) (cl-tmux::*dirty* nil) (cl-tmux::*running* t))
      (cl-tmux::dispatch-command s :list-keys nil)
      (is (overlay-active-p) "list-keys should open the help overlay"))))

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

(defun seed-scrollback (screen n)
  "Give SCREEN N dummy scrollback rows so copy-mode-scroll has room to move."
  (setf (cl-tmux/terminal/types::screen-scrollback screen)
        (loop repeat n collect (vector))))

(test process-byte-copy-mode-up-arrow-end-to-end
  "ESC [ A fed one byte at a time while in copy mode: ESC arms escape-pending,
   '[' keeps it pending, the final 'A' drains the buffer, disarms, and scrolls
   the copy-offset up (toward older output)."
  (let ((s (make-fake-session)))
    (with-loop-state
      (let ((screen (active-screen s))
            (state  (cl-tmux::make-input-state)))
        (cl-tmux::dispatch-command s :copy-mode-enter nil)
        (seed-scrollback screen 10)
        (is (zerop (screen-copy-offset screen)) "offset starts at the live view")
        ;; ESC arms the escape accumulator.
        (is (null (cl-tmux::process-byte s 27 state)))
        (is-true (cl-tmux::input-state-escape-pending state)
                 "ESC in copy mode arms escape-pending")
        ;; '[' (91) — incomplete ESC [ … sequence, still pending.
        (is (null (cl-tmux::process-byte s 91 state)))
        (is-true (cl-tmux::input-state-escape-pending state)
                 "escape still pending after '['")
        ;; 'A' (65) — completes ESC [ A, drains the buffer and disarms.
        (is (null (cl-tmux::process-byte s 65 state)))
        (is-false (cl-tmux::input-state-escape-pending state)
                  "escape drained and disarmed after the final byte")
        (is (= 0 (fill-pointer (cl-tmux::input-state-escape-buf state)))
            "escape buffer reset after completion")
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
        (is-true (cl-tmux::input-state-escape-pending state))
        (is (null (cl-tmux::process-byte s 91 state)))
        (is-true (cl-tmux::input-state-escape-pending state)
                 "escape still pending after '['")
        (is (null (cl-tmux::process-byte s 66 state)))   ; 'B' = down
        (is-false (cl-tmux::input-state-escape-pending state)
                  "escape disarmed after the final byte")
        (is (= 3 (screen-copy-offset screen))
            "down-arrow scrolled the copy-offset down 3 lines (6 - 3)")))))

(test process-byte-esc-not-bracket-flushes
  "ESC followed by a non-'[' byte is not an arrow sequence: %process-escape-byte
   flushes the two accumulated bytes through and disarms escape-pending."
  (let ((s (make-fake-session)))
    (with-loop-state
      (let ((screen (active-screen s))
            (state  (cl-tmux::make-input-state)))
        (cl-tmux::dispatch-command s :copy-mode-enter nil)
        (is (null (cl-tmux::process-byte s 27 state)))
        (is-true (cl-tmux::input-state-escape-pending state) "ESC armed pending")
        ;; 'x' (120) is not '[': the buffer (ESC x) flushes and disarms.
        (is (null (cl-tmux::process-byte s 120 state)))
        (is-false (cl-tmux::input-state-escape-pending state)
                  "non-'[' byte flushes the accumulated escape and disarms")
        (is (= 0 (fill-pointer (cl-tmux::input-state-escape-buf state)))
            "escape buffer reset after the flush")
        (is (zerop (screen-copy-offset screen))
            "a flushed (non-arrow) escape does not scroll the copy-offset")))))

(test process-byte-esc-not-copy-mode-forwards-directly
  "Outside copy mode, ESC is an ordinary byte forwarded to the pane — it must
   NOT arm escape-pending."
  (let ((s (make-fake-session)))
    (with-loop-state
      (let ((state (cl-tmux::make-input-state)))
        (is (not (cl-tmux::copy-mode-active-p s)) "not in copy mode")
        (is (null (cl-tmux::process-byte s 27 state)))
        (is-false (cl-tmux::input-state-escape-pending state)
                  "ESC outside copy mode does not arm escape-pending")))))

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
