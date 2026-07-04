(in-package #:cl-tmux/test)

;;;; Prompt editing, cursor, kill/delete, and change-notification tests.
;;;; Shared prompt fixtures live in prompt-tests.lisp and are loaded first.

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

;;; -- prompt-delete-char (vi x) -----------------------------------------------
;;;
;;; Direct unit tests for prompt-delete-char isolated from the event-dispatch
;;; layer.  (Event-level coverage already exists in events-tests-j.lisp via
;;; vi-normal-key-x-deletes-char-under-cursor, but the pure function is not
;;; isolated-tested there.)

(test prompt-delete-char-removes-char-under-cursor
  "prompt-delete-char (vi x) removes the character at the cursor; cursor stays."
  (with-noop-prompt ("abc")
    (setf (prompt-cursor-index *prompt*) 1)   ; cursor on 'b'
    (prompt-delete-char)
    (is (string= "ac" (prompt-buffer *prompt*))
        "delete-char must remove the character at the cursor")
    (is (= 1 (prompt-cursor-index *prompt*))
        "cursor must remain at index 1 after deletion")))

(test prompt-delete-char-at-start
  "prompt-delete-char at cursor 0 removes the first character."
  (with-noop-prompt ("xyz")
    (setf (prompt-cursor-index *prompt*) 0)
    (prompt-delete-char)
    (is (string= "yz" (prompt-buffer *prompt*))
        "delete-char at index 0 must remove the first character")
    (is (= 0 (prompt-cursor-index *prompt*))
        "cursor must stay at 0 after deleting the first character")))

(test prompt-delete-char-clamps-cursor-when-last-char-deleted
  "prompt-delete-char clamps the cursor when the last character in the buffer
   is deleted, so it does not exceed (1- new-length).
   Scenario: buffer \"ab\", cursor at 1 (the last char 'b'); after deleting 'b'
   the buffer becomes \"a\" (length 1) and the cursor must clamp from 1 to 0
   because index 1 is now past-end of the shortened buffer."
  (with-noop-prompt ("ab")
    ;; cursor starts at 2 (past-end after prompt-start); move to 1 to point at 'b'
    (setf (prompt-cursor-index *prompt*) 1)
    (prompt-delete-char)
    (is (string= "a" (prompt-buffer *prompt*))
        "delete-char on 'b' in \"ab\" must leave \"a\"")
    (is (= 0 (prompt-cursor-index *prompt*))
        "cursor must clamp from 1 to 0 after the last char is deleted")))

(test prompt-delete-char-at-end-is-noop
  "prompt-delete-char at cursor = length of buffer (past-end position) is a no-op."
  (with-noop-prompt ("abc")
    ;; cursor-index 3 = length of "abc" — past the last character
    (is (= 3 (prompt-cursor-index *prompt*)))
    (prompt-delete-char)
    (is (string= "abc" (prompt-buffer *prompt*))
        "delete-char at past-end position must not modify the buffer")
    (is (= 3 (prompt-cursor-index *prompt*))
        "cursor must not move when delete-char is a no-op")))

(test prompt-delete-char-inactive-is-noop
  "prompt-delete-char with no active prompt is a safe no-op."
  (with-clean-prompt
    (finishes (prompt-delete-char) "must not signal when prompt is inactive")
    (is (null (prompt-active-p)) "prompt must remain inactive")))

;;; -- prompt-clear on-cancel callback -----------------------------------------

(test prompt-clear-invokes-on-cancel-callback
  "prompt-clear calls the on-cancel callback when one is set."
  (with-clean-prompt
    (let ((cancel-fired nil))
      (prompt-start "search" "" (make-noop-submit)
                    :on-cancel (lambda () (setf cancel-fired t)))
      (prompt-clear)
      (is (null (prompt-active-p))
          "prompt must be dismissed after prompt-clear")
      (is-true cancel-fired
               "on-cancel callback must be invoked by prompt-clear"))))

(test prompt-clear-on-cancel-nil-is-noop
  "prompt-clear with no on-cancel callback does not error."
  (with-clean-prompt
    (prompt-start "p" "x" (make-noop-submit))
    (finishes (prompt-clear)
              "prompt-clear with no on-cancel must not signal")
    (is (null (prompt-active-p)) "prompt must be dismissed")))

;;; -- prompt-notify-change direct coverage ------------------------------------
;;;
;;; prompt-notify-change is exported but previously had no isolated unit test;
;;; regressions were only detectable via copy-mode-search integration tests.

(test prompt-notify-change-invokes-on-change-with-current-buffer
  "prompt-notify-change calls the on-change callback with the current buffer string."
  (with-clean-prompt
    (let ((received :unset))
      (prompt-start "search" "hello" (make-noop-submit)
                    :on-change (lambda (text) (setf received text)))
      (prompt-notify-change)
      (is (string= "hello" received)
          "on-change callback must receive the current buffer string"))))

(test prompt-notify-change-no-on-change-is-noop
  "prompt-notify-change with no on-change callback is a safe no-op."
  (with-clean-prompt
    (prompt-start "p" "text" (make-noop-submit))
    (finishes (prompt-notify-change)
              "prompt-notify-change with no on-change must not signal")))

(test prompt-notify-change-inactive-prompt-is-noop
  "prompt-notify-change with no active prompt is a safe no-op."
  (with-clean-prompt
    (finishes (prompt-notify-change)
              "prompt-notify-change with no active prompt must not signal")
    (is (null (prompt-active-p)) "prompt must remain inactive")))

(test prompt-notify-change-reflects-buffer-edits
  "prompt-notify-change after buffer edits reports the updated buffer contents."
  (with-clean-prompt
    (let ((last-received nil))
      (prompt-start "search" "" (make-noop-submit)
                    :on-change (lambda (text) (setf last-received text)))
      (prompt-input #\a)
      (is (string= "a" last-received)
          "on-change must be called with 'a' after typing 'a'")
      (prompt-input #\b)
      (is (string= "ab" last-received)
          "on-change must be called with 'ab' after typing 'b'")
      (prompt-backspace)
      (is (string= "a" last-received)
          "on-change must be called with 'a' after backspace"))))
