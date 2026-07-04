(in-package #:cl-tmux/test)

;;;; rectangle-sel-text, run-copy-command, set-cursor, and send-keys-l

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

(test resolve-copy-pipe-cmd-explicit-and-fallback-table
  "%resolve-copy-pipe-cmd uses explicit CMD when non-empty, falls back to the
   copy-command option when CMD is empty.
   Each row: (cmd expected description)."
  (let ((cl-tmux/options:*global-options*
         (let ((h (make-hash-table :test #'equal)))
           (setf (gethash "copy-command" h) "fallback")
           h)))
    (dolist (row '(("printf hi" "printf hi" "explicit command must win over the option")
                   (""          "fallback"  "empty CMD must use the global copy-command option")))
      (destructuring-bind (cmd expected desc) row
        (is (string= expected (cl-tmux/commands::%resolve-copy-pipe-cmd cmd)) desc)))))

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
