(in-package #:cl-tmux/test)

;;;; Interactive single-line input-prompt tests (prompt.lisp) plus the
;;;; event-loop wiring that drives it (handle-prompt-key in events.lisp) and
;;;; the status-bar display branch (render-status-bar in renderer.lisp).
;;;;
;;;; Overlay, popup, and menu tests live in overlay-tests.lisp.
;;;; All prompt tests use with-clean-prompt (from helpers.lisp) to guarantee
;;;; that *prompt* and cl-tmux::*dirty* are reset; the raw let form is never used.

(def-suite prompt-suite :description "Interactive input prompt")
(in-suite prompt-suite)

;;; -- Shared test helpers -----------------------------------------------------

(defun make-rename-window ()
  "A single-pane window (fd -1) named \"old\" suitable for rename testing."
  (make-window :id 1 :name "old" :width 20 :height 5
               :panes (list (make-pane :id 1 :fd -1 :screen (make-screen 20 5)))))

(defmacro with-rename-window ((var) &body body)
  "Bind VAR to a fresh rename-window fixture and reset *prompt* cleanly for BODY."
  `(let ((,var (make-rename-window)))
     (with-clean-prompt
       ,@body)))

(defun make-noop-submit ()
  "Return a no-op on-submit function suitable for use in tests."
  (lambda (s) (declare (ignore s)) nil))

(defmacro with-prompt-at (position &body body)
  "Start a prompt seeded with \"hello\" (cursor at POSITION) and evaluate BODY.
   Binds *prompt* cleanly so state does not leak."
  `(with-clean-prompt
     (prompt-start "p" "hello" (make-noop-submit))
     (setf (prompt-cursor-index *prompt*) ,position)
     ,@body))

;;; -- Pure prompt state -------------------------------------------------------

(test prompt-inactive-by-default
  "With no active prompt, prompt-active-p and prompt-text are NIL."
  (with-clean-prompt
    (is (null (prompt-active-p)))
    (is (null (prompt-text)))))

(test prompt-start-activates
  "prompt-start seeds label/buffer/on-submit and activates the prompt."
  (with-clean-prompt
    (prompt-start "rename-window" "old" (make-noop-submit))
    (is (prompt-active-p))
    (is (string= "old" (prompt-buffer *prompt*)))
    (is (string= "rename-window" (prompt-label *prompt*)))
    (is (functionp (prompt-on-submit *prompt*)))
    (is (string= "rename-window: old|" (prompt-text)))))

(test prompt-input-appends
  "prompt-input appends a character; successive inserts accumulate."
  (with-clean-prompt
    (prompt-start "rename-window" "ab" (make-noop-submit))
    (prompt-input #\c)
    (is (string= "abc" (prompt-buffer *prompt*)))
    (prompt-input #\d)
    (is (string= "abcd" (prompt-buffer *prompt*)))))

(test prompt-backspace-deletes
  "prompt-backspace removes the last char; on an empty buffer it is a no-op."
  (with-clean-prompt
    (prompt-start "rename-window" "abc" (make-noop-submit))
    (prompt-backspace)
    (is (string= "ab" (prompt-buffer *prompt*)))
    (prompt-backspace)
    (prompt-backspace)
    (is (string= "" (prompt-buffer *prompt*)))
    (prompt-backspace)
    (is (string= "" (prompt-buffer *prompt*)) "backspace on empty buffer is a no-op")))

(test prompt-clear-dismisses
  "prompt-clear dismisses the active prompt."
  (with-clean-prompt
    (prompt-start "rename-window" "x" (make-noop-submit))
    (prompt-clear)
    (is (null (prompt-active-p)))
    (is (null (prompt-text)))))

(test prompt-input-inactive-noop
  "prompt-input with no active prompt does nothing and does not error."
  (with-clean-prompt
    (finishes (prompt-input #\x))
    (is (null (prompt-active-p)))))

;;; -- handle-prompt-key wiring (real window, no PTY) --------------------------

(test handle-prompt-key-types-and-applies
  "Typing characters then Enter renames the target window and dismisses the prompt."
  (with-rename-window (win)
    (prompt-start "rename-window" "" (lambda (name) (rename-window win name)))
    (cl-tmux::handle-prompt-key (char-code #\n))
    (cl-tmux::handle-prompt-key (char-code #\e))
    (cl-tmux::handle-prompt-key (char-code #\w))
    (cl-tmux::handle-prompt-key 13)            ; Enter
    (is (string= "new" (window-name win)) "Enter should apply the typed name")
    (is (null (prompt-active-p)) "Enter should dismiss the prompt")))

(test handle-prompt-key-escape-cancels
  "Esc cancels the prompt and leaves the target window's name unchanged."
  (with-rename-window (win)
    (prompt-start "rename-window" "old" (lambda (name) (rename-window win name)))
    (cl-tmux::handle-prompt-key (char-code #\x))
    (cl-tmux::handle-prompt-key 27)            ; Esc
    (is (null (prompt-active-p)) "Esc should dismiss the prompt")
    (is (string= "old" (window-name win)) "Esc must not rename")))

(test handle-prompt-key-backspace
  "Backspace (127) deletes the last buffered character."
  (with-clean-prompt
    (prompt-start "rename-window" "ab" (make-noop-submit))
    (cl-tmux::handle-prompt-key 127)
    (is (string= "a" (prompt-buffer *prompt*)))))

;;; -- handle-prompt-key edge bytes (new coverage) -----------------------------

(test handle-prompt-key-ignores-control-byte
  "A control byte that matches no clause (Tab, 9) is ignored: it must not insert,
   clear, or submit -- the buffer and active prompt are untouched."
  (with-clean-prompt
    (prompt-start "rename-window" "ab" (make-noop-submit))
    (cl-tmux::handle-prompt-key 9)              ; Tab -- matches no clause
    (is (string= "ab" (prompt-buffer *prompt*)) "control byte must not edit the buffer")
    (is (prompt-active-p) "control byte must not dismiss the prompt")
    (is (eq t cl-tmux::*dirty*) "handle-prompt-key always marks the screen dirty")))

(test handle-prompt-key-backspace-byte-8
  "Byte 8 (Ctrl-H) is the alternate backspace and deletes the last character."
  (with-clean-prompt
    (prompt-start "rename-window" "ab" (make-noop-submit))
    (cl-tmux::handle-prompt-key 8)
    (is (string= "a" (prompt-buffer *prompt*)))))

(test handle-prompt-key-enter-empty-is-noop-for-rename
  "Enter on an empty buffer dismisses the prompt but does NOT rename the window
   because rename-window ignores empty strings (matching real tmux behaviour)."
  (with-rename-window (win)
    (prompt-start "rename-window" "" (lambda (name) (rename-window win name)))
    (cl-tmux::handle-prompt-key 13)
    (is (string= "old" (window-name win))
        "Empty input must not rename the window -- original name preserved")
    (is (null (prompt-active-p)) "Enter should dismiss the prompt")))

;;; -- Status-bar display ------------------------------------------------------

(test status-bar-shows-prompt
  "render-status-bar shows the prompt text while a prompt is active."
  (let ((sess (make-test-session 40 10 :content "")))
    (with-clean-prompt
      (prompt-start "rename-window" "foo" (make-noop-submit))
      (let ((out (with-output-to-string (s)
                   (cl-tmux/renderer::render-status-bar s sess 10 40))))
        (is (search "rename-window: foo" out))))))

;;; -- Low-severity edges (existing coverage) ----------------------------------

(test prompt-text-active-empty-buffer
  "prompt-text on an active prompt with an empty buffer shows the cursor '|' at the end."
  (with-clean-prompt
    (prompt-start "rename-window" "" (make-noop-submit))
    (is (string= "rename-window: |" (prompt-text))
        "empty buffer shows label, colon, space, and cursor indicator")))

(test prompt-text-cursor-in-middle
  "prompt-text renders the cursor '|' at the correct interior position.
   'he|llo' when cursor-index is 2 and buffer is 'hello'."
  (with-clean-prompt
    (prompt-start "p" "hello" (make-noop-submit))
    (setf (prompt-cursor-index *prompt*) 2)
    (is (string= "p: he|llo" (prompt-text))
        "cursor at index 2 must split the buffer into prefix 'he' and suffix 'llo'")))

(test prompt-input-multibyte-char
  "prompt-input appends a high/multibyte character verbatim to the buffer."
  (with-clean-prompt
    (prompt-start "rename-window" "a" (make-noop-submit))
    (prompt-input (code-char #x3042))            ; HIRAGANA LETTER A
    (is (string= (concatenate 'string "a" (string (code-char #x3042)))
                 (prompt-buffer *prompt*)))
    (is (char= (code-char #x3042)
               (char (prompt-buffer *prompt*) 1))
        "the appended character keeps its full code point")))

;;; -- Prompt cursor navigation ------------------------------------------------

(test prompt-cursor-bol-moves-to-start
  "C-a (prompt-cursor-bol) moves cursor to index 0."
  (with-clean-prompt
    (prompt-start "p" "abc" (make-noop-submit))
    (is (= 3 (prompt-cursor-index *prompt*)) "cursor starts at end")
    (prompt-cursor-bol)
    (is (= 0 (prompt-cursor-index *prompt*)) "C-a brings cursor to start")))

(test prompt-cursor-eol-moves-to-end
  "C-e (prompt-cursor-eol) moves cursor to end of buffer."
  (with-clean-prompt
    (prompt-start "p" "abc" (make-noop-submit))
    (prompt-cursor-bol)
    (is (= 0 (prompt-cursor-index *prompt*)))
    (prompt-cursor-eol)
    (is (= 3 (prompt-cursor-index *prompt*)) "C-e brings cursor to end")))

(test prompt-cursor-back-and-forward
  "C-b / C-f move cursor one position; clamp at boundaries."
  (with-clean-prompt
    (prompt-start "p" "ab" (make-noop-submit))
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
  (with-clean-prompt
    (prompt-start "p" "ac" (make-noop-submit))
    (prompt-cursor-bol)
    (prompt-cursor-forward)          ; cursor now at index 1
    (prompt-input #\b)               ; insert 'b' between 'a' and 'c'
    (is (string= "abc" (prompt-buffer *prompt*)))
    (is (= 2 (prompt-cursor-index *prompt*)) "cursor advances after insert")))

(test prompt-backspace-at-cursor
  "Backspace deletes the char before the cursor, not always the last char."
  (with-clean-prompt
    (prompt-start "p" "abc" (make-noop-submit))
    (prompt-cursor-bol)
    (prompt-cursor-forward)          ; cursor at index 1, between 'a' and 'b'
    (prompt-cursor-forward)          ; cursor at index 2, between 'b' and 'c'
    (prompt-backspace)               ; delete 'b'
    (is (string= "ac" (prompt-buffer *prompt*)))
    (is (= 1 (prompt-cursor-index *prompt*)) "cursor moves back after backspace")))

(test prompt-kill-to-end
  "C-k deletes from cursor to end."
  (with-clean-prompt
    (prompt-start "p" "hello" (make-noop-submit))
    (prompt-cursor-bol)
    (prompt-cursor-forward)
    (prompt-cursor-forward)          ; cursor at 2
    (prompt-kill-to-end)
    (is (string= "he" (prompt-buffer *prompt*)))
    (is (= 2 (prompt-cursor-index *prompt*)))))

(test prompt-kill-to-start
  "C-u deletes from start to cursor."
  (with-clean-prompt
    (prompt-start "p" "hello" (make-noop-submit))
    (prompt-cursor-bol)
    (prompt-cursor-forward)
    (prompt-cursor-forward)          ; cursor at 2
    (prompt-kill-to-start)
    (is (string= "llo" (prompt-buffer *prompt*)))
    (is (= 0 (prompt-cursor-index *prompt*)))))

(test prompt-kill-word-back
  "C-w deletes the previous word."
  (with-clean-prompt
    (prompt-start "p" "foo bar" (make-noop-submit))
    ;; cursor is at end (index 7)
    (prompt-kill-word-back)
    (is (string= "foo " (prompt-buffer *prompt*)))
    (prompt-kill-word-back)
    (is (string= "" (prompt-buffer *prompt*)))))

(test prompt-kill-word-back-trailing-spaces
  "C-w skips trailing spaces before the word, then removes the word."
  (with-clean-prompt
    (prompt-start "p" "foo   " (make-noop-submit))
    ;; cursor at end (index 6); there are trailing spaces, then the word 'foo'
    (prompt-kill-word-back)
    (is (string= "" (prompt-buffer *prompt*))
        "C-w on trailing-space-only text must clear the entire buffer")))

(test prompt-kill-word-back-at-start
  "C-w at position 0 is a no-op (nothing to kill)."
  (with-clean-prompt
    (prompt-start "p" "hello" (make-noop-submit))
    (prompt-cursor-bol)
    (prompt-kill-word-back)
    (is (string= "hello" (prompt-buffer *prompt*))
        "C-w at start of buffer must leave buffer unchanged")
    (is (= 0 (prompt-cursor-index *prompt*))
        "cursor must remain at 0")))

(test prompt-cc-cancels
  "C-c (byte 3) cancels the prompt, same as Escape."
  (with-clean-prompt
    (prompt-start "p" "hello" (make-noop-submit))
    (is (prompt-active-p))
    (cl-tmux::handle-prompt-key 3)   ; byte 3 = C-c
    (is (not (prompt-active-p)) "C-c must clear the prompt")))
