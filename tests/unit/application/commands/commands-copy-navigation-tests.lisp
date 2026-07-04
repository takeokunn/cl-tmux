(in-package #:cl-tmux/test)

;;;; copy-mode jump, mark, goto-line, incremental search, and bracket navigation

(in-suite commands-suite)

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

(test copy-mode-set-mark-noop-table
  "copy-mode-set-mark is a no-op when not in copy mode or when the cursor is NIL.
   Each row: (copy-mode-p cursor description)."
  (dolist (row '((nil (0 . 0) "mark must remain NIL when not in copy mode")
                 (t   nil     "mark must remain NIL when cursor is NIL")))
    (destructuring-bind (mode-p cursor desc) row
      (let ((s (make-screen 20 5)))
        (setf (screen-copy-mode-p s)                       mode-p
              (cl-tmux/terminal/types:screen-copy-cursor s) cursor
              (cl-tmux/terminal/types:screen-copy-mark   s) nil)
        (cl-tmux/commands::copy-mode-set-mark s)
        (is-false (cl-tmux/terminal/types:screen-copy-mark s) desc)))))

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

(test copy-mode-search-incremental-opens-prompt-with-correct-label
  "copy-mode-search-forward/backward-incremental each open a prompt with the
   correct direction label.  Each row: (fn-sym cursor label)."
  (dolist (row '((cl-tmux/commands::copy-mode-search-forward-incremental
                  (2 . 3) "search-forward")
                 (cl-tmux/commands::copy-mode-search-backward-incremental
                  (3 . 5) "search-backward")))
    (destructuring-bind (fn-sym cursor label) row
      (let ((s (make-screen 10 5)))
        (setf (screen-copy-mode-p s)  t
              (screen-copy-cursor  s)  cursor
              (screen-copy-offset  s)  0
              *prompt* nil
              cl-tmux/commands::*copy-mode-isearch-origin* nil)
        (unwind-protect
            (progn
              (funcall fn-sym s)
              (is-true *prompt* "prompt must be open")
              (is (string= label (prompt-label *prompt*))
                  "prompt label must be ~S" label))
          (setf *prompt* nil
                cl-tmux/commands::*copy-mode-isearch-origin* nil))))))

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

;;; ── copy-mode-search-backward-incremental ────────────────────────────────────

(test copy-mode-search-incremental-cancel-restores-cursor
  "prompt-clear (ESC/C-g) restores cursor and offset to pre-search position for
   both forward and backward incremental search.
   Each row: (fn-sym init-cursor init-offset moved-cursor moved-offset)."
  (dolist (row '((cl-tmux/commands::copy-mode-search-forward-incremental
                  (2 . 3) 0 (0 . 1) 2)
                 (cl-tmux/commands::copy-mode-search-backward-incremental
                  (3 . 5) 1 (1 . 0) 3)))
    (destructuring-bind (fn-sym init-cursor init-offset moved-cursor moved-offset) row
      (let ((s (make-screen 10 5)))
        (setf (screen-copy-mode-p s)  t
              (screen-copy-cursor  s)  init-cursor
              (screen-copy-offset  s)  init-offset
              *prompt* nil
              cl-tmux/commands::*copy-mode-isearch-origin* nil)
        (funcall fn-sym s)
        (setf (screen-copy-cursor s) moved-cursor
              (screen-copy-offset s) moved-offset)
        (prompt-clear)
        (is (equal init-cursor (screen-copy-cursor s))
            "cursor must be restored to pre-search position after cancel")
        (is (= init-offset (screen-copy-offset s))
            "offset must be restored to pre-search value after cancel")
        (is-false cl-tmux/commands::*copy-mode-isearch-origin*
                  "isearch origin must be cleared after cancel")))))

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

(test copy-mode-previous-matching-bracket-table
  "previous-matching-bracket jumps backward to the matching '(' from various positions.
   Each row: (string nchars cursor-col description)."
  (dolist (row '(("( foo )"      7  6 "cursor on ')' jumps to matching '('")
                 ("( foo ) tail" 12 8 "cursor after a matched pair finds the previous close")))
    (destructuring-bind (str nchars col desc) row
      (let ((s (make-screen 20 5)))
        (setf (screen-copy-mode-p s) t)
        (dotimes (i nchars)
          (setf (cl-tmux/terminal/types:screen-cell s i 2)
                (cl-tmux/terminal/types:make-cell :char (char str i))))
        (setf (screen-copy-cursor s) (cons 2 col)
              (screen-copy-offset  s) 0)
        (cl-tmux/commands::copy-mode-previous-matching-bracket s)
        (is (= 0 (cdr (screen-copy-cursor s))) desc)))))

(test copy-mode-previous-matching-bracket-noop-outside-copy-mode
  "Previous bracket matching is a no-op when not in copy mode."
  (let ((s (make-screen 20 5)))
    (setf (screen-copy-mode-p s) nil
          (screen-copy-cursor  s) (cons 0 3))
    (cl-tmux/commands::copy-mode-previous-matching-bracket s)
    (is (equal (cons 0 3) (screen-copy-cursor s))
        "cursor must remain at (0,3) when not in copy mode")))
