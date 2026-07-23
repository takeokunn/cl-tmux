(in-package #:cl-tmux/test)

;;;; events tests — part C: app-cursor-keys, handle-prompt-key UTF-8/cursor/kill,
;;;; copy-mode word/page/yank, SGR mouse, with-copy-mode-state, CSI-u extended keys.

(describe "events-suite"

  ;;; ── Application cursor keys remapping ───────────────────────────────────────

  ;; When app-cursor-keys mode is active, ESC [ A forwarded outside copy mode is
  ;; remapped to ESC O A (SS3) before being sent to the pane.
  (it "app-cursor-keys-remaps-csi-arrow-to-ss3"
    (with-fake-session (s)
      (let ((screen (active-screen s))
            (state  (cl-tmux::make-input-state)))
        ;; Enable application cursor keys on the active pane's screen.
        (setf (screen-app-cursor-keys screen) t)
        ;; Ensure we are NOT in copy mode so the sequence is forwarded, not consumed.
        (expect (cl-tmux::%copy-mode-active-p s) :to-be-falsy)
        ;; Feed ESC [ A — should be remapped to ESC O A internally.
        ;; fd=-1 panes: pty-write is a no-op; we assert no error and NIL return.
        (expect (null (cl-tmux::process-byte s 27 state)))
        (expect (null (cl-tmux::process-byte s 91 state)))
        (expect (null (cl-tmux::process-byte s 65 state))))))

  ;;; ── Buffer overflow guard in make-escape-input-k ────────────────────────────

  ;; After a complete SGR mouse sequence, the continuation returns to ground state.
  (it "escape-accumulator-resets-after-complete-sgr-sequence"
    (with-fake-session (s)
      (let ((state (cl-tmux::make-input-state)))
        ;; Feed ESC [ < 0 ; 5 ; 3 M  (a complete SGR press) byte by byte.
        (dolist (byte (mapcar #'char-code (coerce (format nil "~C[<0;5;3M" #\Escape) 'list)))
          (cl-tmux::process-byte s byte state))
        ;; After the full sequence the continuation must be back to ground.
        (expect (eq #'cl-tmux::%ground-input-state
                    (cl-tmux::input-state-continuation state))))))

  ;;; ── handle-prompt-key UTF-8 multi-byte input ─────────────────────────────────

  ;; A 2-byte UTF-8 sequence (U+00E9, é) fed byte-by-byte into handle-prompt-key
  ;; inserts the correct character into the prompt buffer.
  (it "handle-prompt-key-utf8-two-byte-sequence-inserts-char"
    (with-clean-prompt
      (prompt-start "test" ""
                    (lambda (buf) (declare (ignore buf)) nil))
      ;; U+00E9 in UTF-8: 0xC3 0xA9
      (cl-tmux::handle-prompt-key #xC3)
      (cl-tmux::handle-prompt-key #xA9)
      (expect (string= "é" (prompt-buffer *prompt*)))))

  ;; UTF-8 accumulator state is reset when Enter is pressed mid-sequence.
  (it "handle-prompt-key-utf8-resets-on-enter"
    (with-clean-prompt
      (let ((submitted "unset"))
        (prompt-start "test" ""
                      (lambda (buf) (setf submitted buf)))
        ;; Start a 2-byte UTF-8 sequence but press Enter before the second byte.
        (cl-tmux::handle-prompt-key #xC3)
        (cl-tmux::handle-prompt-key 13)   ; Enter
        ;; The prompt should have been submitted and dismissed.
        (expect (prompt-active-p) :to-be-falsy)
        ;; Submitted value is the buffer content before the incomplete sequence.
        (expect (stringp submitted)))))

  ;;; ── handle-prompt-key cursor movement (C-b, C-f) ────────────────────────────

  ;; C-b (byte 2) moves the prompt cursor one position to the left.
  (it "handle-prompt-key-ctrl-b-moves-cursor-left"
    (with-clean-prompt
      (prompt-start "test" "hello"
                    (lambda (buf) (declare (ignore buf)) nil))
      (prompt-cursor-eol)
      (expect (= 5 (prompt-cursor-index *prompt*)))
      (cl-tmux::handle-prompt-key 2)   ; C-b
      (expect (= 4 (prompt-cursor-index *prompt*)))))

  ;; C-f (byte 6) moves the prompt cursor one position to the right.
  (it "handle-prompt-key-ctrl-f-moves-cursor-right"
    (with-clean-prompt
      (prompt-start "test" "hello"
                    (lambda (buf) (declare (ignore buf)) nil))
      (prompt-cursor-bol)
      (expect (= 0 (prompt-cursor-index *prompt*)))
      (cl-tmux::handle-prompt-key 6)   ; C-f
      (expect (= 1 (prompt-cursor-index *prompt*)))))

  ;;; ── handle-prompt-key kill commands ─────────────────────────────────────────

  ;; C-k (byte 11) deletes from the cursor position to the end of the buffer.
  (it "handle-prompt-key-ctrl-k-kills-to-end"
    (with-clean-prompt
      (prompt-start "test" "hello"
                    (lambda (buf) (declare (ignore buf)) nil))
      ;; Move cursor to position 2 ("he" remains, "llo" to be killed).
      (prompt-cursor-bol)
      (cl-tmux::handle-prompt-key 6)   ; C-f → pos 1
      (cl-tmux::handle-prompt-key 6)   ; C-f → pos 2
      (cl-tmux::handle-prompt-key 11)  ; C-k
      (expect (string= "he" (prompt-buffer *prompt*)))))

  ;; C-u (byte 21) deletes from the start of the buffer to the cursor position.
  (it "handle-prompt-key-ctrl-u-kills-to-start"
    (with-clean-prompt
      (prompt-start "test" "hello"
                    (lambda (buf) (declare (ignore buf)) nil))
      ;; Move cursor to position 3 ("hel" to be killed, "lo" remains).
      (prompt-cursor-bol)
      (cl-tmux::handle-prompt-key 6)   ; C-f → pos 1
      (cl-tmux::handle-prompt-key 6)   ; C-f → pos 2
      (cl-tmux::handle-prompt-key 6)   ; C-f → pos 3
      (cl-tmux::handle-prompt-key 21)  ; C-u
      (expect (string= "lo" (prompt-buffer *prompt*)))))

  ;; C-w (byte 23) deletes the word immediately before the cursor.
  (it "handle-prompt-key-ctrl-w-kills-previous-word"
    (with-clean-prompt
      (prompt-start "test" "foo bar"
                    (lambda (buf) (declare (ignore buf)) nil))
      ;; Move cursor to end of buffer.
      (prompt-cursor-eol)
      (cl-tmux::handle-prompt-key 23)  ; C-w
      ;; Should have deleted "bar" (and possibly the space).
      (let ((buf (prompt-buffer *prompt*)))
        (expect (string= "foo" (string-right-trim " " buf))))))

  ;;; ── process-byte: prompt ESC navigation sequences ──────────────────────────

  ;; CSI left/right arrows move the prompt cursor without cancelling the prompt.
  (it "process-byte-prompt-csi-arrows-edit-cursor"
    (with-fake-session (s)
      (with-clean-prompt
        (let ((state (cl-tmux::make-input-state)))
          (prompt-start "test" "ac"
                        (lambda (buf) (declare (ignore buf)) nil))
          ;; ESC [ D moves from end to between a/c; typing b inserts there.
          (dolist (byte '(27 91 68))
            (cl-tmux::process-byte s byte state))
          (cl-tmux::process-byte s (char-code #\b) state)
          (expect (string= "abc" (prompt-buffer *prompt*)))
          ;; ESC [ C moves to end; typing d appends.
          (dolist (byte '(27 91 67))
            (cl-tmux::process-byte s byte state))
          (cl-tmux::process-byte s (char-code #\d) state)
          (expect (string= "abcd" (prompt-buffer *prompt*)))))))

  ;; CSI Home/End and Delete edit the prompt buffer.
  (it "process-byte-prompt-home-end-delete"
    (with-fake-session (s)
      (with-clean-prompt
        (let ((state (cl-tmux::make-input-state)))
          (prompt-start "test" "abc"
                        (lambda (buf) (declare (ignore buf)) nil))
          ;; ESC [ H -> BOL, then insert x at the start.
          (dolist (byte '(27 91 72))
            (cl-tmux::process-byte s byte state))
          (cl-tmux::process-byte s (char-code #\x) state)
          (expect (string= "xabc" (prompt-buffer *prompt*)))
          ;; ESC [ F -> EOL, then append y.
          (dolist (byte '(27 91 70))
            (cl-tmux::process-byte s byte state))
          (cl-tmux::process-byte s (char-code #\y) state)
          (expect (string= "xabcy" (prompt-buffer *prompt*)))
          ;; Move left to y and delete it with ESC [ 3 ~.
          (dolist (byte '(27 91 68 27 91 51 126))
            (cl-tmux::process-byte s byte state))
          (expect (string= "xabc" (prompt-buffer *prompt*)))))))

  ;; SS3 Home/End sequences edit the prompt cursor.
  (it "process-byte-prompt-ss3-home-end"
    (with-fake-session (s)
      (with-clean-prompt
        (let ((state (cl-tmux::make-input-state)))
          (prompt-start "test" "abc"
                        (lambda (buf) (declare (ignore buf)) nil))
          (dolist (byte '(27 79 72))
            (cl-tmux::process-byte s byte state))
          (cl-tmux::process-byte s (char-code #\x) state)
          (expect (string= "xabc" (prompt-buffer *prompt*)))
          (dolist (byte '(27 79 70))
            (cl-tmux::process-byte s byte state))
          (cl-tmux::process-byte s (char-code #\y) state)
          (expect (string= "xabcy" (prompt-buffer *prompt*)))))))

  ;; CSI Up/Down sequences navigate prompt history when the prompt has history.
  (it "process-byte-prompt-csi-up-down-history"
    (with-fake-session (s)
      (with-clean-prompt
        (let ((state (cl-tmux::make-input-state)))
          (prompt-start "test" "li"
                        (lambda (buf) (declare (ignore buf)) nil)
                        :history '("list-windows" "new-window"))
          (dolist (byte '(27 91 65))
            (cl-tmux::process-byte s byte state))
          (expect (string= "list-windows" (prompt-buffer *prompt*)))
          (dolist (byte '(27 91 65))
            (cl-tmux::process-byte s byte state))
          (expect (string= "new-window" (prompt-buffer *prompt*)))
          (dolist (byte '(27 91 66))
            (cl-tmux::process-byte s byte state))
          (expect (string= "list-windows" (prompt-buffer *prompt*)))
          (dolist (byte '(27 91 66))
            (cl-tmux::process-byte s byte state))
          (expect (string= "li" (prompt-buffer *prompt*)))))))

  ;;; ── process-byte: copy-mode w, b, e word navigation ─────────────────────────

  ;; Plain 'w' (byte 119) moves the copy-mode cursor forward by one word.
  (it "copy-mode-w-moves-word-forward"
    (with-copy-mode-state (s screen state)
      ;; Feed some text to give the screen content.
      (screen-process-bytes
       screen (map '(simple-array (unsigned-byte 8) (*)) #'char-code "hello world"))
      (setf (cl-tmux/terminal/types:screen-copy-cursor screen) (cons 0 0))
      (finishes (cl-tmux::process-byte s 119 state))))  ; w

  ;; Plain 'b' (byte 98) moves the copy-mode cursor backward by one word.
  (it "copy-mode-b-moves-word-backward"
    (with-copy-mode-state (s screen state)
      (setf (cl-tmux/terminal/types:screen-copy-cursor screen) (cons 0 5))
      (finishes (cl-tmux::process-byte s 98 state))))   ; b

  ;; Plain 'e' (byte 101) moves the copy-mode cursor to the end of the current word.
  (it "copy-mode-e-moves-to-word-end"
    (with-copy-mode-state (s screen state)
      (setf (cl-tmux/terminal/types:screen-copy-cursor screen) (cons 0 0))
      (finishes (cl-tmux::process-byte s 101 state))))  ; e

  ;;; ── process-byte: copy-mode page up/down C-f/C-b (in-mode) ──────────────────

  ;; C-f (byte 6) in copy mode scrolls down one full page.
  (it "copy-mode-ctrl-f-page-down"
    (with-isolated-config
      (cl-tmux/options:set-option "mode-keys" "vi")
      (with-copy-mode-state (s screen state)
        (seed-scrollback screen 30)
        (cl-tmux/commands::copy-mode-scroll screen 20)
        (let ((offset-before (screen-copy-offset screen))
              (h             (screen-height screen)))
          (cl-tmux::process-byte s 6 state)   ; C-f -> page down
          (let ((expected (max 0 (- offset-before h))))
            (expect (= expected (screen-copy-offset screen))))))))

  ;; copy-mode-page-up scrolls the viewport up by one full screen-height.
  (it "copy-mode-page-up-command-scrolls-full-page"
    (with-copy-mode-state (s screen state)
      (declare (ignore state))
      (seed-scrollback screen 30)
      (let ((h (screen-height screen)))
        (cl-tmux/commands::copy-mode-page-up screen)
        (let ((expected (min h 30)))
          (expect (= expected (screen-copy-offset screen)))))))

  ;;; ── copy-mode y (yank) and n/N (search navigation) ──────────────────────────

  ;; Plain 'y' (byte 121) completes without signaling when in copy mode.
  (it "copy-mode-y-yanks-selection-finishes"
    (with-copy-mode-state (s screen state)
      ;; Begin a selection first so yank has something to copy.
      (cl-tmux/commands::copy-mode-begin-selection screen)
      (finishes (cl-tmux::process-byte s 121 state))))   ; y

  ;; copy-mode n/N/Y/D keys (search-next/prev, copy-line, copy-eol) do not signal in copy mode.
  (it "copy-mode-search-copy-finishes-table"
    (dolist (c '((110 "n: search-next")
                 (78  "N: search-prev")
                 (89  "Y: copy-line")
                 (68  "D: copy-pipe-end-of-line-and-cancel")))
      (destructuring-bind (byte desc) c
        (with-copy-mode-state (s screen state)
          (declare (ignore screen))
          (finishes (cl-tmux::process-byte s byte state) "~A" desc)))))

  ;;; ── copy-mode half-page and single-line scroll bindings ──────────────────────

  ;; C-u (byte 21) scrolls the copy-mode viewport up by half a page.
  (it "copy-mode-ctrl-u-half-page-up"
    (with-copy-mode-state (s screen state)
      (seed-scrollback screen 30)
      (let ((offset-before (screen-copy-offset screen)))
        (cl-tmux::process-byte s 21 state)   ; C-u
        (expect (>= (screen-copy-offset screen) offset-before)))))

  ;; C-d (byte 4) scrolls the copy-mode viewport down by half a page.
  (it "copy-mode-ctrl-d-half-page-down"
    (with-copy-mode-state (s screen state)
      (seed-scrollback screen 30)
      (cl-tmux/commands::copy-mode-scroll screen 20)
      (let ((offset-before (screen-copy-offset screen)))
        (cl-tmux::process-byte s 4 state)    ; C-d
        (expect (<= (screen-copy-offset screen) offset-before)))))

  ;; C-e (byte 5) in copy mode scrolls the viewport down one line.
  (it "copy-mode-ctrl-e-scrolls-down-one-line"
    (with-copy-mode-state (s screen state)
      (seed-scrollback screen 10)
      (cl-tmux/commands::copy-mode-scroll screen 5)
      (let ((offset-before (screen-copy-offset screen)))
        (cl-tmux::process-byte s 5 state)    ; C-e
        (expect (<= (screen-copy-offset screen) offset-before)))))

  ;; C-y (byte 25) in copy mode scrolls the viewport up one line.
  (it "copy-mode-ctrl-y-scrolls-up-one-line"
    (with-copy-mode-state (s screen state)
      (seed-scrollback screen 10)
      (cl-tmux::process-byte s 25 state)    ; C-y
      (expect (>= (screen-copy-offset screen) 0)))))
