(in-package #:cl-tmux/test)

;;;; Prompt editing, cursor, kill/delete, and change-notification tests.
;;;; Shared prompt fixtures live in prompt-tests.lisp and are loaded first.

(describe "prompt-suite"

  ;;; -- Buffer editing ----------------------------------------------------------

  ;; prompt-input appends a character; successive inserts accumulate.
  (it "prompt-input-appends"
    (with-clean-prompt
      (prompt-start "rename-window" "ab" (make-noop-submit))
      (prompt-input #\c)
      (expect (string= "abc" (prompt-buffer *prompt*)))
      (prompt-input #\d)
      (expect (string= "abcd" (prompt-buffer *prompt*)))))

  ;; prompt-backspace removes the last char; on an empty buffer it is a no-op.
  (it "prompt-backspace-deletes"
    (with-clean-prompt
      (prompt-start "rename-window" "abc" (make-noop-submit))
      (prompt-backspace)
      (expect (string= "ab" (prompt-buffer *prompt*)))
      (prompt-backspace)
      (prompt-backspace)
      (expect (string= "" (prompt-buffer *prompt*)))
      (prompt-backspace)
      (expect (string= "" (prompt-buffer *prompt*)))))

  ;; prompt-clear dismisses the active prompt.
  (it "prompt-clear-dismisses"
    (with-clean-prompt
      (prompt-start "rename-window" "x" (make-noop-submit))
      (prompt-clear)
      (expect (null (prompt-active-p)))
      (expect (null (prompt-text)))))

  ;; prompt-input with no active prompt does nothing and does not error.
  (it "prompt-input-inactive-noop"
    (with-clean-prompt
      (finishes (prompt-input #\x))
      (expect (null (prompt-active-p)))))

  ;;; -- prompt-text display variants --------------------------------------------

  ;; prompt-text on an active prompt with an empty buffer shows the cursor '|' at the end.
  (it "prompt-text-active-empty-buffer"
    (with-clean-prompt
      (prompt-start "rename-window" "" (make-noop-submit))
      (expect (string= "rename-window: |" (prompt-text)))))

  ;; prompt-text renders the cursor '|' at the correct interior position.
  ;; 'he|llo' when cursor-index is 2 and buffer is 'hello'.
  (it "prompt-text-cursor-in-middle"
    (with-prompt-at 2
      (expect (string= "p: he|llo" (prompt-text)))))

  ;; prompt-text shows cursor '|' at position 0 when cursor-index is 0.
  (it "prompt-text-cursor-at-start"
    (with-noop-prompt ("abc")
      (setf (prompt-cursor-index *prompt*) 0)
      (expect (string= "p: |abc" (prompt-text)))))

  ;; prompt-text shows cursor '|' after all text when cursor-index equals buffer length.
  (it "prompt-text-cursor-at-end"
    (with-noop-prompt ("end")
      ;; cursor is at 3 (= length of "end") by default from prompt-start
      (expect (string= "p: end|" (prompt-text)))))

  ;; prompt-input appends a high/multibyte character verbatim to the buffer.
  (it "prompt-input-multibyte-char"
    (with-noop-prompt ("a")
      (prompt-input (code-char #x3042))            ; HIRAGANA LETTER A
      (expect (string= (concatenate 'string "a" (string (code-char #x3042)))
                       (prompt-buffer *prompt*)))
      (expect (char= (code-char #x3042)
                     (char (prompt-buffer *prompt*) 1)))))

  ;;; -- Prompt cursor navigation ------------------------------------------------

  ;; C-a (prompt-cursor-bol) moves cursor to index 0.
  (it "prompt-cursor-bol-moves-to-start"
    (with-noop-prompt ("abc")
      (expect (= 3 (prompt-cursor-index *prompt*)))
      (prompt-cursor-bol)
      (expect (= 0 (prompt-cursor-index *prompt*)))))

  ;; prompt-cursor-bol when already at index 0 is a no-op.
  (it "prompt-cursor-bol-already-at-start-is-noop"
    (with-noop-prompt ("abc")
      (prompt-cursor-bol)
      (expect (= 0 (prompt-cursor-index *prompt*)))
      (prompt-cursor-bol)
      (expect (= 0 (prompt-cursor-index *prompt*)))))

  ;; C-e (prompt-cursor-eol) moves cursor to end of buffer.
  (it "prompt-cursor-eol-moves-to-end"
    (with-noop-prompt ("abc")
      (prompt-cursor-bol)
      (expect (= 0 (prompt-cursor-index *prompt*)))
      (prompt-cursor-eol)
      (expect (= 3 (prompt-cursor-index *prompt*)))))

  ;; prompt-cursor-eol when already at the buffer end is a no-op.
  (it "prompt-cursor-eol-already-at-end-is-noop"
    (with-noop-prompt ("abc")
      ;; cursor already at 3 from prompt-start
      (prompt-cursor-eol)
      (expect (= 3 (prompt-cursor-index *prompt*)))))

  ;; C-b / C-f move cursor one position; clamp at boundaries.
  (it "prompt-cursor-back-and-forward"
    (with-noop-prompt ("ab")
      (expect (= 2 (prompt-cursor-index *prompt*)))
      (prompt-cursor-back)
      (expect (= 1 (prompt-cursor-index *prompt*)))
      (prompt-cursor-back)
      (expect (= 0 (prompt-cursor-index *prompt*)))
      (prompt-cursor-back)
      (expect (= 0 (prompt-cursor-index *prompt*)))
      (prompt-cursor-forward)
      (expect (= 1 (prompt-cursor-index *prompt*)))
      (prompt-cursor-forward)
      (prompt-cursor-forward)
      (expect (= 2 (prompt-cursor-index *prompt*)))))

  ;; Cursor-movement, kill, and basic commands silently no-op when no prompt is active.
  (it "prompt-cursor-and-kill-commands-inactive-noop"
    (dolist (fn '(prompt-backspace     prompt-clear
                  prompt-cursor-back   prompt-cursor-forward
                  prompt-cursor-bol    prompt-cursor-eol
                  prompt-kill-to-end   prompt-kill-to-start
                  prompt-kill-word-back))
      (with-clean-prompt
        (finishes (funcall fn) "~A must not signal when inactive" fn))))

  ;; Characters insert at cursor-index, not always at the end.
  (it "prompt-insert-at-cursor-position"
    (with-noop-prompt ("ac")
      (prompt-cursor-bol)
      (prompt-cursor-forward)          ; cursor now at index 1
      (prompt-input #\b)               ; insert 'b' between 'a' and 'c'
      (expect (string= "abc" (prompt-buffer *prompt*)))
      (expect (= 2 (prompt-cursor-index *prompt*)))))

  ;; Backspace deletes the char before the cursor, not always the last char.
  (it "prompt-backspace-at-cursor"
    (with-noop-prompt ("abc")
      (prompt-cursor-bol)
      (prompt-cursor-forward)          ; cursor at index 1, between 'a' and 'b'
      (prompt-cursor-forward)          ; cursor at index 2, between 'b' and 'c'
      (prompt-backspace)               ; delete 'b'
      (expect (string= "ac" (prompt-buffer *prompt*)))
      (expect (= 1 (prompt-cursor-index *prompt*)))))

  ;;; -- Kill commands -----------------------------------------------------------

  ;; C-k deletes from cursor to end; no-ops at the buffer end.
  (it "prompt-kill-to-end-table"
    (dolist (c '((2 "he"    2 "mid-buffer kill")
                 (5 "hello" 5 "at end — no-op")
                 (0 ""      0 "from start — clears buffer")))
      (destructuring-bind (pos expected-buf expected-idx desc) c
        (declare (ignore desc))
        (with-noop-prompt ("hello")
          (setf (prompt-cursor-index *prompt*) pos)
          (prompt-kill-to-end)
          (expect (string= expected-buf (prompt-buffer *prompt*)))
          (expect (= expected-idx (prompt-cursor-index *prompt*)))))))

  ;; C-u deletes from start to cursor; no-ops at position 0.
  (it "prompt-kill-to-start-table"
    (dolist (c '((2 "llo"   0 "mid-buffer kill")
                 (0 "hello" 0 "at start — no-op")))
      (destructuring-bind (pos expected-buf expected-idx desc) c
        (declare (ignore desc))
        (with-noop-prompt ("hello")
          (setf (prompt-cursor-index *prompt*) pos)
          (prompt-kill-to-start)
          (expect (string= expected-buf (prompt-buffer *prompt*)))
          (expect (= expected-idx (prompt-cursor-index *prompt*)))))))

  ;; C-w deletes the previous word.
  (it "prompt-kill-word-back"
    (with-noop-prompt ("foo bar")
      ;; cursor is at end (index 7)
      (prompt-kill-word-back)
      (expect (string= "foo " (prompt-buffer *prompt*)))
      (prompt-kill-word-back)
      (expect (string= "" (prompt-buffer *prompt*)))))

  ;; C-w skips trailing spaces before the word, then removes the word.
  (it "prompt-kill-word-back-trailing-spaces"
    (with-noop-prompt ("foo   ")
      ;; cursor at end (index 6); there are trailing spaces, then the word 'foo'
      (prompt-kill-word-back)
      (expect (string= "" (prompt-buffer *prompt*)))))

  ;; C-w at position 0 is a no-op (nothing to kill).
  (it "prompt-kill-word-back-at-start"
    (with-prompt-at 0
      (prompt-kill-word-back)
      (expect (string= "hello" (prompt-buffer *prompt*)))
      (expect (= 0 (prompt-cursor-index *prompt*)))))

  ;; C-w at a mid-word position deletes only the portion before the cursor.
  (it "prompt-kill-word-back-middle-cursor"
    (with-prompt-at 3
      (prompt-kill-word-back)
      (expect (string= "lo" (prompt-buffer *prompt*)))
      (expect (= 0 (prompt-cursor-index *prompt*)))))

  ;; C-w with multiple spaces between words skips all spaces then kills the word.
  (it "prompt-kill-word-back-multiple-spaces-between-words"
    (with-noop-prompt ("one   two   ")
      ;; cursor at end (index 12); trailing spaces then "two" then spaces then "one"
      (prompt-kill-word-back)  ; kills "   two" (spaces + word)
      ;; "one   " remains (6 chars), cursor at 6
      (expect (= 6 (prompt-cursor-index *prompt*)))
      (prompt-kill-word-back)  ; kills "one   " (word + trailing spaces)
      (expect (string= "" (prompt-buffer *prompt*)))))

  ;; C-w with a single word-char immediately left of cursor and a space further left
  ;; kills only that one word-char — not the entire buffer.
  ;; Regression test for the off-by-one in %word-kill-start: passing (1- end-index)
  ;; to %skip-while-left caused the space-skip phase to miss the character at
  ;; (end-index - 1), producing an incorrect kill-start of 0 for 'foo X' at end.
  (it "prompt-kill-word-back-single-word-char-after-space"
    (with-noop-prompt ("foo X")
      ;; cursor is at end (index 5); 'X' is immediately left, then a space, then "foo"
      (prompt-kill-word-back)
      (expect (string= "foo " (prompt-buffer *prompt*)))
      (expect (= 4 (prompt-cursor-index *prompt*)))))

  ;;; -- prompt-delete-char (vi x) -----------------------------------------------
  ;;;
  ;;; Direct unit tests for prompt-delete-char isolated from the event-dispatch
  ;;; layer.  (Event-level coverage already exists in events-tests-j.lisp via
  ;;; vi-normal-key-x-deletes-char-under-cursor, but the pure function is not
  ;;; isolated-tested there.)

  ;; prompt-delete-char (vi x) removes the character at the cursor; cursor stays.
  (it "prompt-delete-char-removes-char-under-cursor"
    (with-noop-prompt ("abc")
      (setf (prompt-cursor-index *prompt*) 1)   ; cursor on 'b'
      (prompt-delete-char)
      (expect (string= "ac" (prompt-buffer *prompt*)))
      (expect (= 1 (prompt-cursor-index *prompt*)))))

  ;; prompt-delete-char at cursor 0 removes the first character.
  (it "prompt-delete-char-at-start"
    (with-noop-prompt ("xyz")
      (setf (prompt-cursor-index *prompt*) 0)
      (prompt-delete-char)
      (expect (string= "yz" (prompt-buffer *prompt*)))
      (expect (= 0 (prompt-cursor-index *prompt*)))))

  ;; prompt-delete-char clamps the cursor when the last character in the buffer
  ;; is deleted, so it does not exceed (1- new-length).
  ;; Scenario: buffer "ab", cursor at 1 (the last char 'b'); after deleting 'b'
  ;; the buffer becomes "a" (length 1) and the cursor must clamp from 1 to 0
  ;; because index 1 is now past-end of the shortened buffer.
  (it "prompt-delete-char-clamps-cursor-when-last-char-deleted"
    (with-noop-prompt ("ab")
      ;; cursor starts at 2 (past-end after prompt-start); move to 1 to point at 'b'
      (setf (prompt-cursor-index *prompt*) 1)
      (prompt-delete-char)
      (expect (string= "a" (prompt-buffer *prompt*)))
      (expect (= 0 (prompt-cursor-index *prompt*)))))

  ;; prompt-delete-char at cursor = length of buffer (past-end position) is a no-op.
  (it "prompt-delete-char-at-end-is-noop"
    (with-noop-prompt ("abc")
      ;; cursor-index 3 = length of "abc" — past the last character
      (expect (= 3 (prompt-cursor-index *prompt*)))
      (prompt-delete-char)
      (expect (string= "abc" (prompt-buffer *prompt*)))
      (expect (= 3 (prompt-cursor-index *prompt*)))))

  ;; prompt-delete-char with no active prompt is a safe no-op.
  (it "prompt-delete-char-inactive-is-noop"
    (with-clean-prompt
      (finishes (prompt-delete-char) "must not signal when prompt is inactive")
      (expect (null (prompt-active-p)))))

  ;;; -- prompt-clear on-cancel callback -----------------------------------------

  ;; prompt-clear calls the on-cancel callback when one is set.
  (it "prompt-clear-invokes-on-cancel-callback"
    (with-clean-prompt
      (let ((cancel-fired nil))
        (prompt-start "search" "" (make-noop-submit)
                      :on-cancel (lambda () (setf cancel-fired t)))
        (prompt-clear)
        (expect (null (prompt-active-p)))
        (expect cancel-fired :to-be-truthy))))

  ;; prompt-clear with no on-cancel callback does not error.
  (it "prompt-clear-on-cancel-nil-is-noop"
    (with-clean-prompt
      (prompt-start "p" "x" (make-noop-submit))
      (finishes (prompt-clear)
                "prompt-clear with no on-cancel must not signal")
      (expect (null (prompt-active-p)))))

  ;;; -- prompt-notify-change direct coverage ------------------------------------
  ;;;
  ;;; prompt-notify-change is exported but previously had no isolated unit test;
  ;;; regressions were only detectable via copy-mode-search integration tests.

  ;; prompt-notify-change calls the on-change callback with the current buffer string.
  (it "prompt-notify-change-invokes-on-change-with-current-buffer"
    (with-clean-prompt
      (let ((received :unset))
        (prompt-start "search" "hello" (make-noop-submit)
                      :on-change (lambda (text) (setf received text)))
        (prompt-notify-change)
        (expect (string= "hello" received)))))

  ;; prompt-notify-change with no on-change callback is a safe no-op.
  (it "prompt-notify-change-no-on-change-is-noop"
    (with-clean-prompt
      (prompt-start "p" "text" (make-noop-submit))
      (finishes (prompt-notify-change)
                "prompt-notify-change with no on-change must not signal")))

  ;; prompt-notify-change with no active prompt is a safe no-op.
  (it "prompt-notify-change-inactive-prompt-is-noop"
    (with-clean-prompt
      (finishes (prompt-notify-change)
                "prompt-notify-change with no active prompt must not signal")
      (expect (null (prompt-active-p)))))

  ;; prompt-notify-change after buffer edits reports the updated buffer contents.
  (it "prompt-notify-change-reflects-buffer-edits"
    (with-clean-prompt
      (let ((last-received nil))
        (prompt-start "search" "" (make-noop-submit)
                      :on-change (lambda (text) (setf last-received text)))
        (prompt-input #\a)
        (expect (string= "a" last-received))
        (prompt-input #\b)
        (expect (string= "ab" last-received))
        (prompt-backspace)
        (expect (string= "a" last-received))))))
