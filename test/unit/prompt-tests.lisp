(in-package #:cl-tmux/test)

;;;; Interactive single-line input-prompt tests (prompt.lisp) plus the
;;;; event-loop wiring that drives it (handle-prompt-key in events.lisp) and
;;;; the status-bar display branch (render-status-bar in renderer.lisp).
;;;;
;;;; *prompt* is a special variable; every test that touches it rebinds it with
;;;; (let ((*prompt* nil)) ...) so prompt state never leaks between tests.
;;;; Windows are built with a fake fd (-1) so no PTY is forked.

(def-suite prompt-suite :description "Interactive input prompt")
(in-suite prompt-suite)

;;; ── Pure prompt state ───────────────────────────────────────────────────────

(test prompt-inactive-by-default
  "With no active prompt, prompt-active-p and prompt-text are NIL."
  (let ((*prompt* nil))
    (is (null (prompt-active-p)))
    (is (null (prompt-text)))))

(test prompt-start-activates
  "prompt-start seeds label/buffer/on-submit and activates the prompt."
  (let ((*prompt* nil))
    (prompt-start "rename-window" "old" (lambda (s) (declare (ignore s)) nil))
    (is (prompt-active-p))
    (is (string= "old" (prompt-buffer *prompt*)))
    (is (string= "rename-window" (prompt-label *prompt*)))
    (is (functionp (prompt-on-submit *prompt*)))
    (is (string= "rename-window: old|" (prompt-text)))))

(test prompt-input-appends
  "prompt-input appends a character; successive inserts accumulate."
  (let ((*prompt* nil))
    (prompt-start "rename-window" "ab" (lambda (s) (declare (ignore s)) nil))
    (prompt-input #\c)
    (is (string= "abc" (prompt-buffer *prompt*)))
    (prompt-input #\d)
    (is (string= "abcd" (prompt-buffer *prompt*)))))

(test prompt-backspace-deletes
  "prompt-backspace removes the last char; on an empty buffer it is a no-op."
  (let ((*prompt* nil))
    (prompt-start "rename-window" "abc" (lambda (s) (declare (ignore s)) nil))
    (prompt-backspace)
    (is (string= "ab" (prompt-buffer *prompt*)))
    (prompt-backspace)
    (prompt-backspace)
    (is (string= "" (prompt-buffer *prompt*)))
    (prompt-backspace)
    (is (string= "" (prompt-buffer *prompt*)) "backspace on empty buffer is a no-op")))

(test prompt-clear-dismisses
  "prompt-clear dismisses the active prompt."
  (let ((*prompt* nil))
    (prompt-start "rename-window" "x" (lambda (s) (declare (ignore s)) nil))
    (prompt-clear)
    (is (null (prompt-active-p)))
    (is (null (prompt-text)))))

(test prompt-input-inactive-noop
  "prompt-input with no active prompt does nothing and does not error."
  (let ((*prompt* nil))
    (finishes (prompt-input #\x))
    (is (null (prompt-active-p)))))

;;; ── handle-prompt-key wiring (real window, no PTY) ──────────────────────────

(defun make-rename-window ()
  "A single-pane window (fd -1) named \"old\" suitable for rename testing."
  (make-window :id 1 :name "old" :width 20 :height 5
               :panes (list (make-pane :id 1 :fd -1 :screen (make-screen 20 5)))))

(test handle-prompt-key-types-and-applies
  "Typing characters then Enter renames the target window and dismisses the prompt."
  (let ((win (make-rename-window)))
    (with-clean-prompt
      (prompt-start "rename-window" "" (lambda (name) (rename-window win name)))
      (cl-tmux::handle-prompt-key (char-code #\n))
      (cl-tmux::handle-prompt-key (char-code #\e))
      (cl-tmux::handle-prompt-key (char-code #\w))
      (cl-tmux::handle-prompt-key 13)            ; Enter
      (is (string= "new" (window-name win)) "Enter should apply the typed name")
      (is (null (prompt-active-p)) "Enter should dismiss the prompt"))))

(test handle-prompt-key-escape-cancels
  "Esc cancels the prompt and leaves the target window's name unchanged."
  (let ((win (make-rename-window)))
    (with-clean-prompt
      (prompt-start "rename-window" "old" (lambda (name) (rename-window win name)))
      (cl-tmux::handle-prompt-key (char-code #\x))
      (cl-tmux::handle-prompt-key 27)            ; Esc
      (is (null (prompt-active-p)) "Esc should dismiss the prompt")
      (is (string= "old" (window-name win)) "Esc must not rename"))))

(test handle-prompt-key-backspace
  "Backspace (127) deletes the last buffered character."
  (with-clean-prompt
    (prompt-start "rename-window" "ab" (lambda (s) (declare (ignore s)) nil))
    (cl-tmux::handle-prompt-key 127)
    (is (string= "a" (prompt-buffer *prompt*)))))

;;; ── handle-prompt-key edge bytes (new coverage) ─────────────────────────────

(test handle-prompt-key-ignores-control-byte
  "A control byte that matches no clause (Tab, 9) is ignored: it must not insert,
   clear, or submit — the buffer and active prompt are untouched."
  (with-clean-prompt
    (prompt-start "rename-window" "ab" (lambda (s) (declare (ignore s)) nil))
    (cl-tmux::handle-prompt-key 9)              ; Tab — matches no clause
    (is (string= "ab" (prompt-buffer *prompt*)) "control byte must not edit the buffer")
    (is (prompt-active-p) "control byte must not dismiss the prompt")
    (is (eq t cl-tmux::*dirty*) "handle-prompt-key always marks the screen dirty")))

(test handle-prompt-key-backspace-byte-8
  "Byte 8 (Ctrl-H) is the alternate backspace and deletes the last character."
  (with-clean-prompt
    (prompt-start "rename-window" "ab" (lambda (s) (declare (ignore s)) nil))
    (cl-tmux::handle-prompt-key 8)
    (is (string= "a" (prompt-buffer *prompt*)))))

(test handle-prompt-key-enter-empty-submits-empty
  "Enter on an empty buffer submits the empty string (window renamed to \"\")
   and dismisses the prompt."
  (let ((win (make-rename-window)))
    (with-clean-prompt
      (prompt-start "rename-window" "" (lambda (name) (rename-window win name)))
      (cl-tmux::handle-prompt-key 13)
      (is (string= "" (window-name win)) "Enter on empty buffer submits the empty string")
      (is (null (prompt-active-p)) "Enter should dismiss the prompt"))))

;;; ── Status-bar display ──────────────────────────────────────────────────────

(test status-bar-shows-prompt
  "render-status-bar shows the prompt text while a prompt is active."
  (let ((sess (make-test-session 40 10 :content "")))
    (let ((*prompt* nil))
      (prompt-start "rename-window" "foo" (lambda (s) (declare (ignore s)) nil))
      (let ((out (with-output-to-string (s)
                   (cl-tmux/renderer::render-status-bar s sess 10 40))))
        (is (search "rename-window: foo" out))))))

;;; ── Dismissible overlay (list-keys help) ────────────────────────────────────

(test overlay-inactive-by-default
  "With no overlay set, overlay-active-p is NIL and overlay-lines is empty."
  (let ((*overlay* nil))
    (is (not (overlay-active-p)))
    (is (null (overlay-lines)))))

(test overlay-show-splits-lines-and-clears
  "show-overlay activates a multi-line overlay; overlay-lines splits on newline;
   clear-overlay dismisses it."
  (let ((*overlay* nil))
    (show-overlay (format nil "line1~%line2~%line3"))
    (is (overlay-active-p))
    (is (equal '("line1" "line2" "line3") (overlay-lines)))
    (clear-overlay)
    (is (not (overlay-active-p)))
    (is (null (overlay-lines)))))

(test overlay-single-line
  "A single-line overlay yields exactly one line (no trailing empty line)."
  (let ((*overlay* nil))
    (show-overlay "solo")
    (is (equal '("solo") (overlay-lines)))))

;;; ── Low-severity edges (new coverage) ───────────────────────────────────────

(test prompt-text-active-empty-buffer
  "prompt-text on an active prompt with an empty buffer shows the cursor '|' at the end."
  (let ((*prompt* nil))
    (prompt-start "rename-window" "" (lambda (s) (declare (ignore s)) nil))
    (is (string= "rename-window: |" (prompt-text))
        "empty buffer shows label, colon, space, and cursor indicator")))

(test prompt-input-multibyte-char
  "prompt-input appends a high/multibyte character verbatim to the buffer."
  (let ((*prompt* nil))
    (prompt-start "rename-window" "a" (lambda (s) (declare (ignore s)) nil))
    (prompt-input (code-char #x3042))            ; HIRAGANA LETTER A
    (is (string= (concatenate 'string "a" (string (code-char #x3042)))
                 (prompt-buffer *prompt*)))
    (is (char= (code-char #x3042)
               (char (prompt-buffer *prompt*) 1))
        "the appended character keeps its full code point")))

;;; ── Prompt cursor navigation ────────────────────────────────────────────────

(test prompt-cursor-bol-moves-to-start
  "C-a (prompt-cursor-bol) moves cursor to index 0."
  (let ((*prompt* nil))
    (prompt-start "p" "abc" (lambda (s) (declare (ignore s)) nil))
    (is (= 3 (prompt-cursor-index *prompt*)) "cursor starts at end")
    (prompt-cursor-bol)
    (is (= 0 (prompt-cursor-index *prompt*)) "C-a brings cursor to start")))

(test prompt-cursor-eol-moves-to-end
  "C-e (prompt-cursor-eol) moves cursor to end of buffer."
  (let ((*prompt* nil))
    (prompt-start "p" "abc" (lambda (s) (declare (ignore s)) nil))
    (prompt-cursor-bol)
    (is (= 0 (prompt-cursor-index *prompt*)))
    (prompt-cursor-eol)
    (is (= 3 (prompt-cursor-index *prompt*)) "C-e brings cursor to end")))

(test prompt-cursor-back-and-forward
  "C-b / C-f move cursor one position; clamp at boundaries."
  (let ((*prompt* nil))
    (prompt-start "p" "ab" (lambda (s) (declare (ignore s)) nil))
    (is (= 2 (prompt-cursor-index *prompt*)))
    (prompt-cursor-back)
    (is (= 1 (prompt-cursor-index *prompt*)))
    (prompt-cursor-back)
    (is (= 0 (prompt-cursor-index *prompt*)))
    (prompt-cursor-back)
    (is (= 0 (prompt-cursor-index *prompt*)) "C-b clamped at start")
    (prompt-cursor-forward)
    (is (= 1 (prompt-cursor-index *prompt*)))
    (prompt-cursor-forward)
    (prompt-cursor-forward)
    (is (= 2 (prompt-cursor-index *prompt*)) "C-f clamped at end")))

(test prompt-insert-at-cursor-position
  "Characters insert at cursor-index, not always at the end."
  (let ((*prompt* nil))
    (prompt-start "p" "ac" (lambda (s) (declare (ignore s)) nil))
    (prompt-cursor-bol)
    (prompt-cursor-forward)          ; cursor now at index 1
    (prompt-input #\b)               ; insert 'b' between 'a' and 'c'
    (is (string= "abc" (prompt-buffer *prompt*)))
    (is (= 2 (prompt-cursor-index *prompt*)) "cursor advances after insert")))

(test prompt-backspace-at-cursor
  "Backspace deletes the char before the cursor, not always the last char."
  (let ((*prompt* nil))
    (prompt-start "p" "abc" (lambda (s) (declare (ignore s)) nil))
    (prompt-cursor-bol)
    (prompt-cursor-forward)          ; cursor at index 1, between 'a' and 'b'
    (prompt-cursor-forward)          ; cursor at index 2, between 'b' and 'c'
    (prompt-backspace)               ; delete 'b'
    (is (string= "ac" (prompt-buffer *prompt*)))
    (is (= 1 (prompt-cursor-index *prompt*)) "cursor moves back after backspace")))

(test prompt-kill-to-end
  "C-k deletes from cursor to end."
  (let ((*prompt* nil))
    (prompt-start "p" "hello" (lambda (s) (declare (ignore s)) nil))
    (prompt-cursor-bol)
    (prompt-cursor-forward)
    (prompt-cursor-forward)          ; cursor at 2
    (prompt-kill-to-end)
    (is (string= "he" (prompt-buffer *prompt*)))
    (is (= 2 (prompt-cursor-index *prompt*)))))

(test prompt-kill-to-start
  "C-u deletes from start to cursor."
  (let ((*prompt* nil))
    (prompt-start "p" "hello" (lambda (s) (declare (ignore s)) nil))
    (prompt-cursor-bol)
    (prompt-cursor-forward)
    (prompt-cursor-forward)          ; cursor at 2
    (prompt-kill-to-start)
    (is (string= "llo" (prompt-buffer *prompt*)))
    (is (= 0 (prompt-cursor-index *prompt*)))))

(test prompt-kill-word-back
  "C-w deletes the previous word."
  (let ((*prompt* nil))
    (prompt-start "p" "foo bar" (lambda (s) (declare (ignore s)) nil))
    ;; cursor is at end (index 7)
    (prompt-kill-word-back)
    (is (string= "foo " (prompt-buffer *prompt*)))
    (prompt-kill-word-back)
    (is (string= "" (prompt-buffer *prompt*)))))

(test prompt-cc-cancels
  "C-c (byte 3) cancels the prompt, same as Escape."
  (let ((*prompt* nil) (*dirty* nil))
    (prompt-start "p" "hello" (lambda (s) (declare (ignore s)) nil))
    (is (prompt-active-p))
    (cl-tmux::handle-prompt-key 3)   ; byte 3 = C-c
    (is (not (prompt-active-p)) "C-c must clear the prompt")))

(test overlay-lines-trailing-newline
  "A trailing newline yields a final empty line: overlay-lines collects the
   empty segment after the last newline."
  (let ((*overlay* nil))
    (show-overlay (format nil "a~%"))
    (is (equal '("a" "") (overlay-lines))
        "text ending in newline produces a trailing empty line")
    (show-overlay (format nil "a~%b~%"))
    (is (equal '("a" "b" "") (overlay-lines)))))
