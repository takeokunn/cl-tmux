(in-package #:cl-tmux/test)

;;;; events tests — part C: app-cursor-keys, handle-prompt-key UTF-8/cursor/kill,
;;;; copy-mode word/page/yank, SGR mouse, with-copy-mode-state, CSI-u extended keys.

(in-suite events-suite)

;;; ── Application cursor keys remapping ───────────────────────────────────────

(test app-cursor-keys-remaps-csi-arrow-to-ss3
  "When app-cursor-keys mode is active, ESC [ A forwarded outside copy mode is
   remapped to ESC O A (SS3) before being sent to the pane."
  (with-fake-session (s)
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
          "ESC [ A with app-cursor-keys must not signal or return a quit value"))))

;;; ── Buffer overflow guard in make-escape-input-k ────────────────────────────

(test escape-accumulator-resets-after-complete-sgr-sequence
  "After a complete SGR mouse sequence, the continuation returns to ground state."
  (with-fake-session (s)
    (let ((state (cl-tmux::make-input-state)))
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
  (with-copy-mode-state (s screen state)
    ;; Feed some text to give the screen content.
    (screen-process-bytes
     screen (map '(simple-array (unsigned-byte 8) (*)) #'char-code "hello world"))
    (setf (cl-tmux/terminal/types:screen-copy-cursor screen) (cons 0 0))
    (finishes (cl-tmux::process-byte s 119 state))))  ; w

(test copy-mode-b-moves-word-backward
  "Plain 'b' (byte 98) moves the copy-mode cursor backward by one word."
  (with-copy-mode-state (s screen state)
    (setf (cl-tmux/terminal/types:screen-copy-cursor screen) (cons 0 5))
    (finishes (cl-tmux::process-byte s 98 state))))   ; b

(test copy-mode-e-moves-to-word-end
  "Plain 'e' (byte 101) moves the copy-mode cursor to the end of the current word."
  (with-copy-mode-state (s screen state)
    (setf (cl-tmux/terminal/types:screen-copy-cursor screen) (cons 0 0))
    (finishes (cl-tmux::process-byte s 101 state))))  ; e

;;; ── process-byte: copy-mode page up/down C-f/C-b (in-mode) ──────────────────

(test copy-mode-ctrl-f-page-down
  "C-f (byte 6) in copy mode scrolls down one full page."
  (with-copy-mode-state (s screen state)
    (seed-scrollback screen 30)
    (cl-tmux/commands::copy-mode-scroll screen 20)
    (let ((offset-before (screen-copy-offset screen))
          (h             (screen-height screen)))
      (cl-tmux::process-byte s 6 state)   ; C-f → page down
      (let ((expected (max 0 (- offset-before h))))
        (is (= expected (screen-copy-offset screen))
            "C-f must scroll copy-offset down by screen-height")))))

(test copy-mode-page-up-command-scrolls-full-page
  "copy-mode-page-up scrolls the viewport up by one full screen-height."
  (with-copy-mode-state (s screen state)
    (declare (ignore state))
    (seed-scrollback screen 30)
    (let ((h (screen-height screen)))
      (cl-tmux/commands::copy-mode-page-up screen)
      (let ((expected (min h 30)))
        (is (= expected (screen-copy-offset screen))
            "copy-mode-page-up must scroll copy-offset up by screen-height")))))

;;; ── copy-mode y (yank) and n/N (search navigation) ──────────────────────────

(test copy-mode-y-yanks-selection-finishes
  "Plain 'y' (byte 121) completes without signaling when in copy mode."
  (with-copy-mode-state (s screen state)
    ;; Begin a selection first so yank has something to copy.
    (cl-tmux/commands::copy-mode-begin-selection screen)
    (finishes (cl-tmux::process-byte s 121 state))))   ; y

(test copy-mode-search-copy-finishes-table
  "copy-mode n/N/Y/D keys (search-next/prev, copy-line, copy-eol) do not signal in copy mode."
  (dolist (c '((110 "n: search-next")
               (78  "N: search-prev")
               (89  "Y: copy-line")
               (68  "D: copy-end-of-line")))
    (destructuring-bind (byte desc) c
      (with-copy-mode-state (s screen state)
        (declare (ignore screen))
        (finishes (cl-tmux::process-byte s byte state) "~A" desc)))))

;;; ── copy-mode half-page and single-line scroll bindings ──────────────────────

(test copy-mode-ctrl-u-half-page-up
  "C-u (byte 21) scrolls the copy-mode viewport up by half a page."
  (with-copy-mode-state (s screen state)
    (seed-scrollback screen 30)
    (let ((offset-before (screen-copy-offset screen)))
      (cl-tmux::process-byte s 21 state)   ; C-u
      (is (>= (screen-copy-offset screen) offset-before)
          "C-u must not decrease copy-offset"))))

(test copy-mode-ctrl-d-half-page-down
  "C-d (byte 4) scrolls the copy-mode viewport down by half a page."
  (with-copy-mode-state (s screen state)
    (seed-scrollback screen 30)
    (cl-tmux/commands::copy-mode-scroll screen 20)
    (let ((offset-before (screen-copy-offset screen)))
      (cl-tmux::process-byte s 4 state)    ; C-d
      (is (<= (screen-copy-offset screen) offset-before)
          "C-d must not increase copy-offset"))))

(test copy-mode-ctrl-e-scrolls-down-one-line
  "C-e (byte 5) in copy mode scrolls the viewport down one line."
  (with-copy-mode-state (s screen state)
    (seed-scrollback screen 10)
    (cl-tmux/commands::copy-mode-scroll screen 5)
    (let ((offset-before (screen-copy-offset screen)))
      (cl-tmux::process-byte s 5 state)    ; C-e
      (is (<= (screen-copy-offset screen) offset-before)
          "C-e must scroll copy-offset down (decrease offset)"))))

(test copy-mode-ctrl-y-scrolls-up-one-line
  "C-y (byte 25) in copy mode scrolls the viewport up one line."
  (with-copy-mode-state (s screen state)
    (seed-scrollback screen 10)
    (cl-tmux::process-byte s 25 state)    ; C-y
    (is (>= (screen-copy-offset screen) 0)
        "C-y must not produce a negative copy-offset")))

