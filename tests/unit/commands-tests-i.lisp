(in-package #:cl-tmux/test)

;;;; rectangle-sel-text, run-copy-command, set-cursor, send-keys-l, jump-to-char, goto-line, search-incr — part IX

(in-suite commands-suite)

;;; ── %rectangle-selection-text (direct unit tests) ────────────────────────────
;;;
;;; %rectangle-selection-text is exercised transitively through copy-mode-yank
;;; with rect-select=T.  These direct tests make boundary conditions explicit.

(test rectangle-selection-text-returns-nil-when-no-selection
  "%rectangle-selection-text returns NIL when no selection is active."
  (let ((s (make-screen 20 5)))
    (cl-tmux/commands::copy-mode-enter s)
    (setf (cl-tmux/terminal/types:screen-copy-selecting s) nil)
    (is (null (cl-tmux/commands::%rectangle-selection-text s))
        "%rectangle-selection-text must return NIL when copy-selecting is NIL")))

(test rectangle-selection-text-returns-nil-when-mark-nil
  "%rectangle-selection-text returns NIL when mark is NIL even if selecting is T."
  (let ((s (make-screen 20 5)))
    (cl-tmux/commands::copy-mode-enter s)
    (setf (cl-tmux/terminal/types:screen-copy-selecting s) t
          (cl-tmux/terminal/types:screen-copy-mark      s) nil
          (cl-tmux/terminal/types:screen-copy-cursor    s) (cons 0 5))
    (is (null (cl-tmux/commands::%rectangle-selection-text s))
        "%rectangle-selection-text must return NIL when mark is NIL")))

(test rectangle-selection-text-single-row
  "%rectangle-selection-text returns the correct column slice for a single-row selection."
  ;; Feed "hello world" to row 0; rectangle from col 0 to col 5 on row 0 only.
  (let ((s (make-screen 20 5)))
    (feed s "hello world")
    (cl-tmux/commands::copy-mode-enter s)
    (setf (cl-tmux/terminal/types:screen-copy-selecting    s) t
          (cl-tmux/terminal/types:screen-copy-mark         s) (cons 0 0)
          (cl-tmux/terminal/types:screen-copy-cursor       s) (cons 0 5))
    (let ((text (cl-tmux/commands::%rectangle-selection-text s)))
      (is (stringp text) "%rectangle-selection-text must return a string")
      (is (string= "hello" text)
          "%rectangle-selection-text must return cols 0-4 (got ~S)" text))))

(test rectangle-selection-text-multi-row-fixed-columns
  "%rectangle-selection-text extracts the same column range on every row."
  ;; Row 0 = "abcde", row 1 = "ABCDE"; rectangle col 1-3 (2 chars per row).
  (let ((s (make-screen 10 5)))
    (feed s (format nil "abcde~C~CABCDE" #\Return #\Linefeed))
    (cl-tmux/commands::copy-mode-enter s)
    (setf (cl-tmux/terminal/types:screen-copy-selecting s) t
          (cl-tmux/terminal/types:screen-copy-mark      s) (cons 0 1)
          (cl-tmux/terminal/types:screen-copy-cursor    s) (cons 1 3))
    (let ((text (cl-tmux/commands::%rectangle-selection-text s)))
      (is (stringp text) "%rectangle-selection-text must return a string")
      (is (search "bc" text)
          "%rectangle-selection-text must include cols 1-2 from row 0 (got ~S)" text)
      (is (search "BC" text)
          "%rectangle-selection-text must include cols 1-2 from row 1 (got ~S)" text)
      (is (find #\Newline text)
          "%rectangle-selection-text must separate rows with newlines"))))

;;; ── %run-copy-command (direct unit tests) ────────────────────────────────────
;;;
;;; %run-copy-command is exercised only transitively through copy-mode-yank when
;;; the 'copy-command' option is set.  These direct tests cover the no-op branch
;;; (empty option / empty text) and the error-handling contract.

(test run-copy-command-noop-when-nil-or-empty
  "%run-copy-command is a no-op for NIL and for an empty string."
  (finishes (cl-tmux/commands::%run-copy-command nil)
            "%run-copy-command with nil text must not signal")
  (finishes (cl-tmux/commands::%run-copy-command "")
            "%run-copy-command with empty text must not signal"))

(test run-copy-command-noop-when-option-unset
  "%run-copy-command is a no-op when the 'copy-command' option is not set."
  ;; Fresh option table: 'copy-command' is absent.
  (with-fresh-global-options
    (finishes (cl-tmux/commands::%run-copy-command "some text")
              "%run-copy-command with no copy-command option must not signal")))

(test run-copy-command-does-not-crash-on-bad-command
  "%run-copy-command swallows errors from a malformed copy-command."
  ;; Set copy-command to a command that will fail (exit non-zero or not found).
  (let ((cl-tmux/options:*global-options*
         (let ((h (make-hash-table :test #'equal)))
           (setf (gethash "copy-command" h) "false")
           h)))
    (finishes (cl-tmux/commands::%run-copy-command "hello")
              "%run-copy-command must not signal when the copy-command fails")))

;;; ── %resolve-copy-pipe-cmd (direct unit tests) ──────────────────────────────

(test resolve-copy-pipe-cmd-uses-explicit-command
  "%resolve-copy-pipe-cmd returns the explicit CMD when it is a non-empty string."
  (let ((cl-tmux/options:*global-options*
         (let ((h (make-hash-table :test #'equal)))
           (setf (gethash "copy-command" h) "fallback")
           h)))
    (is (string= "printf hi" (cl-tmux/commands::%resolve-copy-pipe-cmd "printf hi"))
        "explicit copy-pipe command must win over the global option")))

(test resolve-copy-pipe-cmd-falls-back-to-option
  "%resolve-copy-pipe-cmd falls back to the global copy-command option when CMD is empty."
  (let ((cl-tmux/options:*global-options*
         (let ((h (make-hash-table :test #'equal)))
           (setf (gethash "copy-command" h) "fallback")
           h)))
    (is (string= "fallback" (cl-tmux/commands::%resolve-copy-pipe-cmd ""))
        "empty CMD must use the global copy-command option")))

(test resolve-copy-pipe-cmd-returns-nil-when-unavailable
  "%resolve-copy-pipe-cmd returns NIL when neither CMD nor the global option is usable."
  (with-fresh-global-options
    (is (null (cl-tmux/commands::%resolve-copy-pipe-cmd nil))
        "nil CMD with no option must return NIL")
    (is (null (cl-tmux/commands::%resolve-copy-pipe-cmd ""))
        "empty CMD with no option must return NIL")))

;;; ── copy-mode-set-cursor (direct unit tests in commands group) ───────────────
;;;
;;; copy-mode-set-cursor is exported from cl-tmux/commands and tested in
;;; events-tests.lisp (via keystroke dispatch), but that test lives outside the
;;; commands audit scope.  Direct tests here make the commands group self-contained.

(test copy-mode-set-cursor-positions-cursor
  "copy-mode-set-cursor sets the cursor to the given row and column."
  (let ((s (make-screen 20 5)))
    (cl-tmux/commands::copy-mode-enter s)
    (cl-tmux/commands:copy-mode-set-cursor s 2 7)
    (is (equal (cons 2 7) (cl-tmux/terminal/types:screen-copy-cursor s))
        "copy-mode-set-cursor must set cursor to (2 . 7)")))

(test copy-mode-set-cursor-clamps-table
  "copy-mode-set-cursor clamps both row and column to [0, bound-1]."
  (dolist (row (list (list 99  0  4 #'car "row 99 → height-1=4")
                     (list -1  0  0 #'car "row -1 → 0")
                     (list  0 99 19 #'cdr "col 99 → width-1=19")
                     (list  0 -1  0 #'cdr "col -1 → 0")))
    (destructuring-bind (r c expected accessor desc) row
      (let ((s (make-screen 20 5)))
        (cl-tmux/commands::copy-mode-enter s)
        (cl-tmux/commands:copy-mode-set-cursor s r c)
        (is (= expected (funcall accessor (cl-tmux/terminal/types:screen-copy-cursor s)))
            "~A" desc)))))

(test copy-mode-set-cursor-noop-outside-copy-mode
  "copy-mode-set-cursor is a no-op when not in copy mode."
  (let ((s (make-screen 20 5)))
    ;; Do NOT enter copy mode.
    (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 1 1))
    (cl-tmux/commands:copy-mode-set-cursor s 3 7)
    (is (equal (cons 1 1) (cl-tmux/terminal/types:screen-copy-cursor s))
        "cursor must be unchanged outside copy mode")))

;;; ── send-keys -l (literal) vs translated ────────────────────────────────────
;;;
;;; send-keys-to-pane (pane string &key literal) is the production entry point,
;;; but it needs a pane with a real PTY (fd > -1) to observe output; fake panes
;;; have fd -1, where pty-write is a harmless no-op.  We therefore test the
;;; byte-production logic that distinguishes the two modes:
;;;   - non-literal: %translate-send-keys maps the key name "Enter" → CR (13).
;;;   - literal (-l): the string is emitted as raw UTF-8 bytes, so "Enter"
;;;     stays the 5 bytes E-n-t-e-r with no key-name interpretation.

(test send-keys-translated-enter-produces-cr
  "Without -l, %translate-send-keys maps the key name \"Enter\" to a single CR byte (13)."
  (let ((bytes (cl-tmux/commands::%translate-send-keys "Enter")))
    (is (= 1 (length bytes))
        "translated \"Enter\" must be exactly one byte (got length ~D)" (length bytes))
    (is (= 13 (aref bytes 0))
        "translated \"Enter\" must be CR (char code 13), got ~D" (aref bytes 0))))

(test send-keys-literal-enter-stays-five-bytes
  "With -l, the string \"Enter\" is written as raw UTF-8 bytes — five literal
   characters E-n-t-e-r — NOT translated to a CR.  This is the byte payload
   send-keys-to-pane writes when :literal is true."
  (let ((literal-bytes (babel:string-to-octets "Enter")))
    (is (= 5 (length literal-bytes))
        "literal \"Enter\" must be five bytes (got length ~D)" (length literal-bytes))
    (is (equalp #(69 110 116 101 114) literal-bytes)
        "literal \"Enter\" must be the ASCII bytes for E,n,t,e,r")
    ;; The literal payload must differ from the translated (single-CR) payload.
    (is (not (equalp literal-bytes
                     (cl-tmux/commands::%translate-send-keys "Enter")))
        "literal mode must NOT equal the translated single-CR payload")))

(test send-keys-literal-multibyte-utf8-preserves-bytes
  "With -l, a multi-byte UTF-8 string is emitted as its raw UTF-8 octets:
   \"café\" is 4 characters but encodes to 5 bytes (é = 2 bytes), so literal
   mode preserves the multi-byte encoding rather than counting characters."
  (let ((literal-bytes (babel:string-to-octets "café" :encoding :utf-8)))
    (is (= 5 (length literal-bytes))
        "literal \"café\" must be 5 UTF-8 bytes (got length ~D)" (length literal-bytes))
    (is (> (length literal-bytes) (length "café"))
        "byte count (~D) must exceed the 4-character count, proving multi-byte preservation"
        (length literal-bytes))
    ;; The é (U+00E9) encodes to the two-byte sequence C3 A9; assert the tail.
    (is (equalp #(195 169) (subseq literal-bytes 3))
        "the é must encode to the two UTF-8 bytes C3 A9 (got ~S)"
        (subseq literal-bytes 3))))

;;; ── Jump-to-char (vi f/F/t/T/;/,) ──────────────────────────────────────────

(test copy-mode-jump-basic-movements
  "jump-forward, jump-backward, and jump-to land the cursor at the expected column.
   Each row: (fn-sym initial-col char expected-col description)."
  (dolist (row '((cl-tmux/commands::copy-mode-jump-forward  0  #\l 2
                  "jump-forward 'l' from col 0 must land on col 2 (first 'l')")
                 (cl-tmux/commands::copy-mode-jump-forward  10 #\z 10
                  "no-match forward must leave cursor unchanged")
                 (cl-tmux/commands::copy-mode-jump-backward 10 #\l 9
                  "jump-backward 'l' from col 10 must land on col 9 ('l' in 'world')")
                 (cl-tmux/commands::copy-mode-jump-to       0  #\l 1
                  "jump-to 'l' from col 0 must land on col 1 (one before col 2)")))
    (destructuring-bind (fn-sym initial-col char expected-col desc) row
      (let ((s (copy-mode-screen :w 20 :h 3 :content "hello world"
                                 :cursor (cons 0 initial-col))))
        (funcall (symbol-function fn-sym) s char)
        (is (= expected-col (cdr (cl-tmux/terminal/types:screen-copy-cursor s)))
            desc)))))

(test copy-mode-jump-again-repeats-last
  "jump-again (vi ;) repeats the last jump-forward."
  (let ((s (copy-mode-screen :w 20 :h 3
                             :content "hello world"
                             :cursor (cons 0 0))))
    (cl-tmux/commands::copy-mode-jump-forward s #\l)   ; lands col 2
    (cl-tmux/commands::copy-mode-jump-again s)         ; next 'l'
    (is (= 3 (cdr (cl-tmux/terminal/types:screen-copy-cursor s)))
        "jump-again must advance to col 3 (second 'l')")))

(test copy-mode-jump-reverse-reverses-forward
  "jump-reverse (vi ,) performs the jump in the opposite direction."
  (let ((s (copy-mode-screen :w 20 :h 3
                             :content "hello world"
                             :cursor (cons 0 0))))
    (cl-tmux/commands::copy-mode-jump-forward s #\l)   ; lands col 2
    (cl-tmux/commands::copy-mode-jump-again  s)        ; lands col 3
    (cl-tmux/commands::copy-mode-jump-reverse s)       ; back to col 2
    (is (= 2 (cdr (cl-tmux/terminal/types:screen-copy-cursor s)))
        "jump-reverse after two forward jumps must return to col 2")))

(test copy-mode-jump-to-again-advances-past-adjacent
  "After t<char>, ; (jump-again) advances PAST the immediately-adjacent occurrence
   instead of sticking one cell before the same char (tmux cx+2, audit #18).
   'hello world' has 'l' at cols 2, 3, 9."
  (let ((s (copy-mode-screen :w 20 :h 3 :content "hello world" :cursor (cons 0 0))))
    (cl-tmux/commands::copy-mode-jump-to s #\l)          ; t l → col 1 (before 'l' @2)
    (is (= 1 (cdr (cl-tmux/terminal/types:screen-copy-cursor s)))
        "t l from col 0 lands at col 1")
    (cl-tmux/commands::copy-mode-jump-again s)            ; ; must advance, not stick
    (is (= 2 (cdr (cl-tmux/terminal/types:screen-copy-cursor s)))
        "; after t advances to col 2 (before 'l' @3), not stuck at col 1")
    (cl-tmux/commands::copy-mode-jump-again s)            ; ; → next 'l' @9 → col 8
    (is (= 8 (cdr (cl-tmux/terminal/types:screen-copy-cursor s)))
        "; again advances to col 8 (before 'l' @9)")))

(test copy-mode-jump-to-back-again-advances-past-adjacent
  "After T<char>, ; advances PAST the adjacent occurrence backward (tmux cx-2,
   audit #18).  'hello world' has 'l' at cols 2, 3."
  (let ((s (copy-mode-screen :w 20 :h 3 :content "hello world" :cursor (cons 0 5))))
    (cl-tmux/commands::copy-mode-jump-to-backward s #\l) ; T l → col 4 (after 'l' @3)
    (is (= 4 (cdr (cl-tmux/terminal/types:screen-copy-cursor s)))
        "T l from col 5 lands at col 4 (just after 'l' @3)")
    (cl-tmux/commands::copy-mode-jump-again s)            ; ; must advance backward
    (is (= 3 (cdr (cl-tmux/terminal/types:screen-copy-cursor s)))
        "; after T advances to col 3 (after 'l' @2), not stuck at col 4")))

;;; ── copy-mode-set-mark ───────────────────────────────────────────────────────

(test copy-mode-set-mark-stores-current-cursor
  "copy-mode-set-mark stores the current cursor position as the mark."
  (let ((s (make-screen 20 5)))
    (setf (screen-copy-mode-p s)  t
          (screen-copy-offset s)  2
          (cl-tmux/terminal/types:screen-copy-cursor s) (cons 3 7)
          (cl-tmux/terminal/types:screen-copy-mark   s) nil
          (cl-tmux/terminal/types:screen-copy-mark-offset s) 0)
    (cl-tmux/commands::copy-mode-set-mark s)
    (is (equal (cons 3 7) (cl-tmux/terminal/types:screen-copy-mark s))
        "mark must be set to current cursor position (row=3, col=7)")
    (is (= 2 (cl-tmux/terminal/types:screen-copy-mark-offset s))
        "mark-offset must match the current copy-offset")))

(test copy-mode-set-mark-does-not-start-selection
  "copy-mode-set-mark must NOT begin a visual selection."
  (let ((s (make-screen 20 5)))
    (setf (screen-copy-mode-p s)  t
          (screen-copy-offset s)  0
          (cl-tmux/terminal/types:screen-copy-cursor s)    (cons 1 4)
          (cl-tmux/terminal/types:screen-copy-selecting s) nil)
    (cl-tmux/commands::copy-mode-set-mark s)
    (is-false (cl-tmux/terminal/types:screen-copy-selecting s)
              "set-mark must not activate selection mode")))

(test copy-mode-set-mark-noop-outside-copy-mode
  "copy-mode-set-mark is a no-op when not in copy mode."
  (let ((s (make-screen 20 5)))
    (setf (screen-copy-mode-p s)  nil
          (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 0)
          (cl-tmux/terminal/types:screen-copy-mark   s) nil)
    (cl-tmux/commands::copy-mode-set-mark s)
    (is-false (cl-tmux/terminal/types:screen-copy-mark s)
              "mark must remain nil when not in copy mode")))

(test copy-mode-set-mark-noop-without-cursor
  "copy-mode-set-mark is a no-op when copy-cursor is nil."
  (let ((s (make-screen 20 5)))
    (setf (screen-copy-mode-p s)  t
          (cl-tmux/terminal/types:screen-copy-cursor s) nil
          (cl-tmux/terminal/types:screen-copy-mark   s) nil)
    (cl-tmux/commands::copy-mode-set-mark s)
    (is-false (cl-tmux/terminal/types:screen-copy-mark s)
              "mark must remain nil when cursor is nil")))

;;; ── copy-mode-goto-line ──────────────────────────────────────────────────────

(test copy-mode-goto-line-jumps-to-live-row
  "copy-mode-goto-line N with no scrollback jumps to viewport row N-1."
  ;; 10-wide, 5-row screen, no scrollback: vrow = viewport-row (offset=0, sb-n=0).
  ;; goto-line 3 = vrow 2 = viewport row 2.
  (let ((s (make-screen 10 5)))
    (cl-tmux/commands::copy-mode-enter s)
    (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 0))
    (cl-tmux/commands::copy-mode-goto-line s 3)
    (is (= 2 (car (cl-tmux/terminal/types:screen-copy-cursor s)))
        "goto-line 3 with no scrollback must land on viewport row 2 (vrow 2)")))

(test copy-mode-goto-line-clamps-over-max
  "copy-mode-goto-line clamps to the last valid row when N exceeds total rows."
  (let ((s (make-screen 10 3)))
    (cl-tmux/commands::copy-mode-enter s)
    (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 0))
    ;; 999 is way past the total row count (3-row screen, no scrollback = vrows 0-2)
    (cl-tmux/commands::copy-mode-goto-line s 999)
    ;; After clamping, cursor row must be within [0, height-1]
    (is (<= 0 (car (cl-tmux/terminal/types:screen-copy-cursor s)) 2)
        "goto-line out-of-range must clamp cursor to a valid viewport row")))

(test copy-mode-goto-line-noop-outside-copy-mode
  "copy-mode-goto-line is a no-op when not in copy mode."
  (let ((s (make-screen 10 5)))
    (setf (screen-copy-mode-p s)  nil
          (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 0))
    ;; Should not signal any error, screen must stay out of copy mode.
    (cl-tmux/commands::copy-mode-goto-line s 1)
    (is-false (cl-tmux/terminal/types:screen-copy-mode-p s)
              "screen must remain out of copy mode")))

;;; ── copy-mode-search-forward-incremental ─────────────────────────────────────

(test copy-mode-search-incremental-noop-outside-copy-mode
  "Neither incremental search function opens a prompt when not in copy mode."
  (dolist (fn '(cl-tmux/commands::copy-mode-search-forward-incremental
                cl-tmux/commands::copy-mode-search-backward-incremental))
    (let ((s (make-screen 10 5)))
      (setf (screen-copy-mode-p s) nil
            *prompt* nil
            cl-tmux/commands::*copy-mode-isearch-origin* nil)
      (funcall fn s)
      (is-false *prompt* "~S must not open a prompt outside copy mode" fn))))

(test copy-mode-search-forward-incremental-opens-prompt
  "Opens a prompt labelled search-forward when in copy mode."
  (let ((s (make-screen 10 5)))
    (setf (screen-copy-mode-p s)  t
          (screen-copy-cursor  s)  (cons 2 3)
          (screen-copy-offset  s)  0
          *prompt* nil
          cl-tmux/commands::*copy-mode-isearch-origin* nil)
    (unwind-protect
        (progn
          (cl-tmux/commands::copy-mode-search-forward-incremental s)
          (is-true  *prompt* "prompt must be open")
          (is (string= "search-forward" (prompt-label *prompt*))
              "prompt label must be search-forward"))
      (setf *prompt* nil
            cl-tmux/commands::*copy-mode-isearch-origin* nil))))

(test copy-mode-search-forward-incremental-saves-origin
  "Saves cursor+offset in *copy-mode-isearch-origin* when prompt opens."
  (let ((s (make-screen 10 5)))
    (setf (screen-copy-mode-p s)  t
          (screen-copy-cursor  s)  (cons 2 3)
          (screen-copy-offset  s)  5
          *prompt* nil
          cl-tmux/commands::*copy-mode-isearch-origin* nil)
    (unwind-protect
        (progn
          (cl-tmux/commands::copy-mode-search-forward-incremental s)
          (let ((origin cl-tmux/commands::*copy-mode-isearch-origin*))
            (is-true origin "origin must be non-nil after prompt open")
            (is (equal (cons 2 3) (car origin)) "origin cursor must match pre-search cursor")
            (is (= 5 (cdr origin))              "origin offset must match pre-search offset")))
      (setf *prompt* nil
            cl-tmux/commands::*copy-mode-isearch-origin* nil))))

(test copy-mode-search-forward-incremental-cancel-restores-cursor
  "prompt-clear (ESC/C-g) restores cursor and offset to pre-search position."
  (let ((s (make-screen 10 5)))
    (setf (screen-copy-mode-p s)  t
          (screen-copy-cursor  s)  (cons 2 3)
          (screen-copy-offset  s)  0
          *prompt* nil
          cl-tmux/commands::*copy-mode-isearch-origin* nil)
    (cl-tmux/commands::copy-mode-search-forward-incremental s)
    ;; Simulate the search having moved the cursor away.
    (setf (screen-copy-cursor s) (cons 0 1)
          (screen-copy-offset s) 2)
    ;; Cancel — must invoke the on-cancel closure which restores origin.
    (prompt-clear)
    (is (equal (cons 2 3) (screen-copy-cursor s))
        "cursor must be restored to pre-search position after cancel")
    (is (= 0 (screen-copy-offset s))
        "offset must be restored to pre-search value after cancel")
    (is-false cl-tmux/commands::*copy-mode-isearch-origin*
              "isearch origin must be cleared after cancel")))

;;; ── copy-mode-search-backward-incremental ────────────────────────────────────

(test copy-mode-search-backward-incremental-opens-prompt
  "Opens a prompt labelled search-backward when in copy mode."
  (let ((s (make-screen 10 5)))
    (setf (screen-copy-mode-p s)  t
          (screen-copy-cursor  s)  (cons 3 5)
          (screen-copy-offset  s)  0
          *prompt* nil
          cl-tmux/commands::*copy-mode-isearch-origin* nil)
    (unwind-protect
        (progn
          (cl-tmux/commands::copy-mode-search-backward-incremental s)
          (is-true  *prompt* "prompt must be open")
          (is (string= "search-backward" (prompt-label *prompt*))
              "prompt label must be search-backward"))
      (setf *prompt* nil
            cl-tmux/commands::*copy-mode-isearch-origin* nil))))

(test copy-mode-search-backward-incremental-cancel-restores-cursor
  "prompt-clear (ESC/C-g) restores cursor and offset when backward search is cancelled."
  (let ((s (make-screen 10 5)))
    (setf (screen-copy-mode-p s)  t
          (screen-copy-cursor  s)  (cons 3 5)
          (screen-copy-offset  s)  1
          *prompt* nil
          cl-tmux/commands::*copy-mode-isearch-origin* nil)
    (cl-tmux/commands::copy-mode-search-backward-incremental s)
    ;; Simulate search having moved the cursor away.
    (setf (screen-copy-cursor s) (cons 1 0)
          (screen-copy-offset s) 3)
    (prompt-clear)
    (is (equal (cons 3 5) (screen-copy-cursor s))
        "cursor must be restored to pre-search position after cancel")
    (is (= 1 (screen-copy-offset s))
        "offset must be restored to pre-search value after cancel")
    (is-false cl-tmux/commands::*copy-mode-isearch-origin*
              "isearch origin must be cleared after cancel")))

;;; ── copy-mode-next-matching-bracket ─────────────────────────────────────────

(test copy-mode-next-matching-bracket-paren-table
  "Cursor on '(' jumps forward to ')'; cursor on ')' jumps backward to '('."
  (dolist (row '((2 0 6 "on '(' (col 0) → finds ')' (col 6)")
                 (2 6 0 "on ')' (col 6) → finds '(' (col 0)")))
    (destructuring-bind (start-row start-col expected-col desc) row
      (let ((s (make-screen 20 5)))
        (setf (screen-copy-mode-p s) t)
        (dotimes (i 7)
          (setf (cl-tmux/terminal/types:screen-cell s i 2)
                (cl-tmux/terminal/types:make-cell :char (char "( foo )" i))))
        (setf (screen-copy-cursor s) (cons start-row start-col)
              (screen-copy-offset  s) 0)
        (cl-tmux/commands::copy-mode-next-matching-bracket s)
        (is (= expected-col (cdr (screen-copy-cursor s))) "~A" desc)))))

(test copy-mode-next-matching-bracket-nested-brackets
  "Nested brackets: cursor on outer '(' jumps to the outer matching ')'."
  (let ((s (make-screen 20 5)))
    (setf (screen-copy-mode-p s) t)
    ;; Write "(a(b)c)" at row 0.
    (dotimes (i 7)
      (setf (cl-tmux/terminal/types:screen-cell s i 0)
            (cl-tmux/terminal/types:make-cell :char (char "(a(b)c)" i))))
    (setf (screen-copy-cursor s) (cons 0 0)
          (screen-copy-offset  s) 0)
    (cl-tmux/commands::copy-mode-next-matching-bracket s)
    (is (= 6 (cdr (screen-copy-cursor s)))
        "cursor must land on the outer ')' at column 6")))

(test copy-mode-next-matching-bracket-noop-outside-copy-mode
  "Bracket matching is a no-op when not in copy mode."
  (let ((s (make-screen 20 5)))
    (setf (screen-copy-mode-p s) nil
          (screen-copy-cursor  s) (cons 0 3))
    (cl-tmux/commands::copy-mode-next-matching-bracket s)
    (is (equal (cons 0 3) (screen-copy-cursor s))
        "cursor must remain at (0,3) when not in copy mode")))

(test copy-mode-previous-matching-bracket-on-close
  "Cursor on ')' jumps backward to its matching '('."
  (let ((s (make-screen 20 5)))
    (setf (screen-copy-mode-p s) t)
    (dotimes (i 7)
      (setf (cl-tmux/terminal/types:screen-cell s i 2)
            (cl-tmux/terminal/types:make-cell :char (char "( foo )" i))))
    (setf (screen-copy-cursor s) (cons 2 6)
          (screen-copy-offset  s) 0)
    (cl-tmux/commands::copy-mode-previous-matching-bracket s)
    (is (= 0 (cdr (screen-copy-cursor s)))
        "cursor must land on the matching '(' at column 0")))

(test copy-mode-previous-matching-bracket-finds-previous-close
  "Cursor after a matched pair finds the previous ')' and jumps to its matching '('."
  (let ((s (make-screen 20 5)))
    (setf (screen-copy-mode-p s) t)
    (dotimes (i 12)
      (setf (cl-tmux/terminal/types:screen-cell s i 2)
            (cl-tmux/terminal/types:make-cell :char (char "( foo ) tail" i))))
    (setf (screen-copy-cursor s) (cons 2 8)
          (screen-copy-offset  s) 0)
    (cl-tmux/commands::copy-mode-previous-matching-bracket s)
    (is (= 0 (cdr (screen-copy-cursor s)))
        "cursor must land on the opener that matches the previous close bracket")))

(test copy-mode-previous-matching-bracket-noop-outside-copy-mode
  "Previous bracket matching is a no-op when not in copy mode."
  (let ((s (make-screen 20 5)))
    (setf (screen-copy-mode-p s) nil
          (screen-copy-cursor  s) (cons 0 3))
    (cl-tmux/commands::copy-mode-previous-matching-bracket s)
    (is (equal (cons 0 3) (screen-copy-cursor s))
        "cursor must remain at (0,3) when not in copy mode")))
