(in-package #:cl-tmux/test)

;;;; Interactive single-line input-prompt tests (prompt.lisp) plus the
;;;; event-loop wiring that drives it (handle-prompt-key in events.lisp) and
;;;; the status-bar display branch (render-status-bar in renderer.lisp).
;;;;
;;;; Overlay, popup, and menu tests live in overlay-tests.lisp.
;;;; All prompt tests use with-clean-prompt (from helpers-b.lisp) to guarantee
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

(defmacro with-noop-prompt ((initial) &body body)
  "Start a prompt labelled \"p\" seeded with INITIAL text and a no-op submit callback.
   Binds *prompt* cleanly so state never leaks between tests."
  `(with-clean-prompt
     (prompt-start "p" ,initial (make-noop-submit))
     ,@body))

(defmacro with-prompt-at (position &body body)
  "Start a prompt seeded with \"hello\" (cursor at POSITION) and evaluate BODY.
   Binds *prompt* cleanly so state does not leak."
  `(with-noop-prompt ("hello")
     (setf (prompt-cursor-index *prompt*) ,position)
     ,@body))

;;; -- Prompt struct constructors and predicates --------------------------------

(test make-prompt-defaults
  "make-prompt with no keyword arguments fills slots to documented defaults."
  (let ((p (make-prompt)))
    (check-table (list (list (prompt-label p)        "" "default label is empty string")
                       (list (prompt-buffer p)       "" "default buffer is empty string")
                       (list (prompt-cursor-index p) 0  "default cursor-index is 0"))
                 :test #'equal)
    (is (null (prompt-on-submit p)) "default on-submit is NIL")))

(test make-prompt-keyword-args
  "make-prompt keyword arguments override all defaults."
  (let ((fn (lambda (s) s)))
    (let ((p (make-prompt :label "lbl" :buffer "buf"
                          :cursor-index 3 :on-submit fn)))
      (is (string= "lbl" (prompt-label p)))
      (is (string= "buf" (prompt-buffer p)))
      (is (= 3 (prompt-cursor-index p)))
      (is (eq fn (prompt-on-submit p))))))

(test prompt-p-recognises-prompt-struct
  "prompt-p returns T for a PROMPT and NIL for any other value."
  (let ((p (make-prompt)))
    (is (prompt-p p) "prompt-p must return T for a make-prompt result")
    (dolist (val (list nil 42 ""))
      (is (not (prompt-p val)) "prompt-p must return NIL for ~S" val))))

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

(test prompt-start-cursor-index-at-end
  "prompt-start sets cursor-index to the length of the initial buffer."
  (with-noop-prompt ("hello")
    (is (= 5 (prompt-cursor-index *prompt*))
        "cursor must start at end of initial buffer")))

(test prompt-start-empty-buffer-cursor-at-zero
  "prompt-start with an empty initial buffer places cursor at index 0."
  (with-noop-prompt ("")
    (is (= 0 (prompt-cursor-index *prompt*))
        "cursor must be at 0 for empty initial buffer")))

(test prompt-on-submit-accessor
  "prompt-on-submit stores and returns the callback supplied to prompt-start."
  (with-clean-prompt
    (let ((cb (lambda (s) (format nil "got:~A" s))))
      (prompt-start "p" "text" cb)
      (is (eq cb (prompt-on-submit *prompt*))
          "prompt-on-submit must return the exact function passed to prompt-start"))))

(test prompt-history-prev-next-restores-in-progress-input
  "History navigation walks newest-first entries and Down restores current input."
  (with-clean-prompt
    (prompt-start "p" "li" (make-noop-submit)
                  :history '("list-windows" "new-window"))
    (prompt-history-prev)
    (is (string= "list-windows" (prompt-buffer *prompt*))
        "first Up must load newest history entry")
    (prompt-history-prev)
    (is (string= "new-window" (prompt-buffer *prompt*))
        "second Up must load the next older history entry")
    (prompt-history-next)
    (is (string= "list-windows" (prompt-buffer *prompt*))
        "first Down must move toward newer history")
    (prompt-history-next)
    (is (string= "li" (prompt-buffer *prompt*))
        "Down from newest history must restore the in-progress input")
    (is (= 2 (prompt-cursor-index *prompt*))
        "restored input must place cursor at end")))

(test prompt-history-edit-resets-navigation-base
  "Editing a recalled history entry makes future Up navigation start from that edit."
  (with-clean-prompt
    (prompt-start "p" "" (make-noop-submit)
                  :history '("list-windows" "new-window"))
    (prompt-history-prev)
    (prompt-input #\s)
    (is (string= "list-windowss" (prompt-buffer *prompt*)))
    (prompt-history-next)
    (is (string= "list-windowss" (prompt-buffer *prompt*))
        "Down after editing must not replace the edited buffer with the original")
    (prompt-history-prev)
    (is (string= "list-windows" (prompt-buffer *prompt*))
        "Up after editing starts a fresh history walk")))

;;; -- Buffer editing ----------------------------------------------------------

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

;;; -- prompt-text display variants --------------------------------------------

(test prompt-text-active-empty-buffer
  "prompt-text on an active prompt with an empty buffer shows the cursor '|' at the end."
  (with-clean-prompt
    (prompt-start "rename-window" "" (make-noop-submit))
    (is (string= "rename-window: |" (prompt-text))
        "empty buffer shows label, colon, space, and cursor indicator")))

(test prompt-text-cursor-in-middle
  "prompt-text renders the cursor '|' at the correct interior position.
   'he|llo' when cursor-index is 2 and buffer is 'hello'."
  (with-prompt-at 2
    (is (string= "p: he|llo" (prompt-text))
        "cursor at index 2 must split the buffer into prefix 'he' and suffix 'llo'")))

(test prompt-text-cursor-at-start
  "prompt-text shows cursor '|' at position 0 when cursor-index is 0."
  (with-noop-prompt ("abc")
    (setf (prompt-cursor-index *prompt*) 0)
    (is (string= "p: |abc" (prompt-text))
        "cursor at index 0 must prefix all buffer text")))

(test prompt-text-cursor-at-end
  "prompt-text shows cursor '|' after all text when cursor-index equals buffer length."
  (with-noop-prompt ("end")
    ;; cursor is at 3 (= length of "end") by default from prompt-start
    (is (string= "p: end|" (prompt-text))
        "cursor at end must append '|' after all buffer text")))

(test prompt-input-multibyte-char
  "prompt-input appends a high/multibyte character verbatim to the buffer."
  (with-noop-prompt ("a")
    (prompt-input (code-char #x3042))            ; HIRAGANA LETTER A
    (is (string= (concatenate 'string "a" (string (code-char #x3042)))
                 (prompt-buffer *prompt*)))
    (is (char= (code-char #x3042)
               (char (prompt-buffer *prompt*) 1))
        "the appended character keeps its full code point")))

;;; -- Prompt cursor navigation ------------------------------------------------

(test prompt-cursor-bol-moves-to-start
  "C-a (prompt-cursor-bol) moves cursor to index 0."
  (with-noop-prompt ("abc")
    (is (= 3 (prompt-cursor-index *prompt*)) "cursor starts at end")
    (prompt-cursor-bol)
    (is (= 0 (prompt-cursor-index *prompt*)) "C-a brings cursor to start")))

(test prompt-cursor-bol-already-at-start-is-noop
  "prompt-cursor-bol when already at index 0 is a no-op."
  (with-noop-prompt ("abc")
    (prompt-cursor-bol)
    (is (= 0 (prompt-cursor-index *prompt*)))
    (prompt-cursor-bol)
    (is (= 0 (prompt-cursor-index *prompt*))
        "C-a when already at start must leave cursor at 0")))

(test prompt-cursor-eol-moves-to-end
  "C-e (prompt-cursor-eol) moves cursor to end of buffer."
  (with-noop-prompt ("abc")
    (prompt-cursor-bol)
    (is (= 0 (prompt-cursor-index *prompt*)))
    (prompt-cursor-eol)
    (is (= 3 (prompt-cursor-index *prompt*)) "C-e brings cursor to end")))

(test prompt-cursor-eol-already-at-end-is-noop
  "prompt-cursor-eol when already at the buffer end is a no-op."
  (with-noop-prompt ("abc")
    ;; cursor already at 3 from prompt-start
    (prompt-cursor-eol)
    (is (= 3 (prompt-cursor-index *prompt*))
        "C-e when already at end must leave cursor at 3")))

(test prompt-cursor-back-and-forward
  "C-b / C-f move cursor one position; clamp at boundaries."
  (with-noop-prompt ("ab")
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

(test prompt-cursor-and-kill-commands-inactive-noop
  "Cursor-movement, kill, and basic commands silently no-op when no prompt is active."
  (dolist (fn '(prompt-backspace     prompt-clear
                prompt-cursor-back   prompt-cursor-forward
                prompt-cursor-bol    prompt-cursor-eol
                prompt-kill-to-end   prompt-kill-to-start
                prompt-kill-word-back))
    (with-clean-prompt
      (finishes (funcall fn) "~A must not signal when inactive" fn))))

(test prompt-insert-at-cursor-position
  "Characters insert at cursor-index, not always at the end."
  (with-noop-prompt ("ac")
    (prompt-cursor-bol)
    (prompt-cursor-forward)          ; cursor now at index 1
    (prompt-input #\b)               ; insert 'b' between 'a' and 'c'
    (is (string= "abc" (prompt-buffer *prompt*)))
    (is (= 2 (prompt-cursor-index *prompt*)) "cursor advances after insert")))

(test prompt-backspace-at-cursor
  "Backspace deletes the char before the cursor, not always the last char."
  (with-noop-prompt ("abc")
    (prompt-cursor-bol)
    (prompt-cursor-forward)          ; cursor at index 1, between 'a' and 'b'
    (prompt-cursor-forward)          ; cursor at index 2, between 'b' and 'c'
    (prompt-backspace)               ; delete 'b'
    (is (string= "ac" (prompt-buffer *prompt*)))
    (is (= 1 (prompt-cursor-index *prompt*)) "cursor moves back after backspace")))

;;; -- Kill commands -----------------------------------------------------------

(test prompt-kill-to-end-table
  "C-k deletes from cursor to end; no-ops at the buffer end."
  (dolist (c '((2 "he"    2 "mid-buffer kill")
               (5 "hello" 5 "at end — no-op")
               (0 ""      0 "from start — clears buffer")))
    (destructuring-bind (pos expected-buf expected-idx desc) c
      (with-noop-prompt ("hello")
        (setf (prompt-cursor-index *prompt*) pos)
        (prompt-kill-to-end)
        (is (string= expected-buf (prompt-buffer *prompt*)) "~A: buffer" desc)
        (is (= expected-idx (prompt-cursor-index *prompt*)) "~A: cursor" desc)))))

(test prompt-kill-to-start-table
  "C-u deletes from start to cursor; no-ops at position 0."
  (dolist (c '((2 "llo"   0 "mid-buffer kill")
               (0 "hello" 0 "at start — no-op")))
    (destructuring-bind (pos expected-buf expected-idx desc) c
      (with-noop-prompt ("hello")
        (setf (prompt-cursor-index *prompt*) pos)
        (prompt-kill-to-start)
        (is (string= expected-buf (prompt-buffer *prompt*)) "~A: buffer" desc)
        (is (= expected-idx (prompt-cursor-index *prompt*)) "~A: cursor" desc)))))

(test prompt-kill-word-back
  "C-w deletes the previous word."
  (with-noop-prompt ("foo bar")
    ;; cursor is at end (index 7)
    (prompt-kill-word-back)
    (is (string= "foo " (prompt-buffer *prompt*)))
    (prompt-kill-word-back)
    (is (string= "" (prompt-buffer *prompt*)))))

(test prompt-kill-word-back-trailing-spaces
  "C-w skips trailing spaces before the word, then removes the word."
  (with-noop-prompt ("foo   ")
    ;; cursor at end (index 6); there are trailing spaces, then the word 'foo'
    (prompt-kill-word-back)
    (is (string= "" (prompt-buffer *prompt*))
        "C-w on trailing-space-only text must clear the entire buffer")))

(test prompt-kill-word-back-at-start
  "C-w at position 0 is a no-op (nothing to kill)."
  (with-prompt-at 0
    (prompt-kill-word-back)
    (is (string= "hello" (prompt-buffer *prompt*))
        "C-w at start of buffer must leave buffer unchanged")
    (is (= 0 (prompt-cursor-index *prompt*))
        "cursor must remain at 0")))

(test prompt-kill-word-back-middle-cursor
  "C-w at a mid-word position deletes only the portion before the cursor."
  (with-prompt-at 3
    (prompt-kill-word-back)
    (is (string= "lo" (prompt-buffer *prompt*))
        "C-w from mid-word must delete from word start to cursor")
    (is (= 0 (prompt-cursor-index *prompt*))
        "cursor must move to where the kill started")))

(test prompt-kill-word-back-multiple-spaces-between-words
  "C-w with multiple spaces between words skips all spaces then kills the word."
  (with-noop-prompt ("one   two   ")
    ;; cursor at end (index 12); trailing spaces then "two" then spaces then "one"
    (prompt-kill-word-back)  ; kills "   two" (spaces + word)
    ;; "one   " remains (6 chars), cursor at 6
    (is (= 6 (prompt-cursor-index *prompt*))
        "cursor must move to after the remaining text")
    (prompt-kill-word-back)  ; kills "one   " (word + trailing spaces)
    (is (string= "" (prompt-buffer *prompt*))
        "second C-w must clear the entire buffer")))

(test prompt-kill-word-back-single-word-char-after-space
  "C-w with a single word-char immediately left of cursor and a space further left
   kills only that one word-char — not the entire buffer.
   Regression test for the off-by-one in %word-kill-start: passing (1- end-index)
   to %skip-while-left caused the space-skip phase to miss the character at
   (end-index - 1), producing an incorrect kill-start of 0 for 'foo X' at end."
  (with-noop-prompt ("foo X")
    ;; cursor is at end (index 5); 'X' is immediately left, then a space, then "foo"
    (prompt-kill-word-back)
    (is (string= "foo " (prompt-buffer *prompt*))
        "C-w must kill only 'X', leaving 'foo ' in the buffer")
    (is (= 4 (prompt-cursor-index *prompt*))
        "cursor must move to index 4 (after the space)")))

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

(test handle-prompt-key-backspace-table
  "Bytes 127 (DEL) and 8 (C-H) both act as backspace."
  (dolist (byte '(127 8))
    (with-clean-prompt
      (prompt-start "rename-window" "ab" (make-noop-submit))
      (cl-tmux::handle-prompt-key byte)
      (is (string= "a" (prompt-buffer *prompt*))
          "byte ~D must backspace to \"a\"" byte))))

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

(test handle-prompt-key-enter-empty-is-noop-for-rename
  "Enter on an empty buffer dismisses the prompt but does NOT rename the window
   because rename-window ignores empty strings (matching real tmux behaviour)."
  (with-rename-window (win)
    (prompt-start "rename-window" "" (lambda (name) (rename-window win name)))
    (cl-tmux::handle-prompt-key 13)
    (is (string= "old" (window-name win))
        "Empty input must not rename the window -- original name preserved")
    (is (null (prompt-active-p)) "Enter should dismiss the prompt")))

(test handle-prompt-key-cc-cancels
  "C-c (byte 3) cancels the prompt, same as Escape."
  (with-clean-prompt
    (prompt-start "p" "hello" (make-noop-submit))
    (is (prompt-active-p))
    (cl-tmux::handle-prompt-key 3)   ; byte 3 = C-c
    (is (not (prompt-active-p)) "C-c must clear the prompt")))

;;; -- Status-bar display ------------------------------------------------------

(test status-bar-shows-prompt
  "render-status-bar shows the prompt text while a prompt is active."
  (let ((sess (make-test-session 40 10 :content "")))
    (with-clean-prompt
      (prompt-start "rename-window" "foo" (make-noop-submit))
      (let ((out (render-status-bar-output sess 10 40)))
        (is (search "rename-window: foo" out))))))
