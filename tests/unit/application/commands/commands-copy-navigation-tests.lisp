(in-package #:cl-tmux/test)

;;;; copy-mode jump, mark, goto-line, incremental search, and bracket navigation

(describe "commands-suite"

  ;;; ── Jump-to-char (vi f/F/t/T/;/,) ──────────────────────────────────────────

  ;; jump-forward, jump-backward, and jump-to land the cursor at the expected column.
  ;; Each row: (fn-sym initial-col char expected-col description).
  (it "copy-mode-jump-basic-movements"
    (dolist (row '((cl-tmux/commands::copy-mode-jump-forward  0  #\l 2
                    "jump-forward 'l' from col 0 must land on col 2 (first 'l')")
                   (cl-tmux/commands::copy-mode-jump-forward  10 #\z 10
                    "no-match forward must leave cursor unchanged")
                   (cl-tmux/commands::copy-mode-jump-backward 10 #\l 9
                    "jump-backward 'l' from col 10 must land on col 9 ('l' in 'world')")
                   (cl-tmux/commands::copy-mode-jump-to       0  #\l 1
                    "jump-to 'l' from col 0 must land on col 1 (one before col 2)")))
      (destructuring-bind (fn-sym initial-col char expected-col desc) row
        (declare (ignore desc))
        (let ((s (copy-mode-screen :w 20 :h 3 :content "hello world"
                                   :cursor (cons 0 initial-col))))
          (funcall (symbol-function fn-sym) s char)
          (expect (= expected-col (cdr (cl-tmux/terminal/types:screen-copy-cursor s))))))))

  ;; jump-again (vi ;) repeats the last jump-forward.
  (it "copy-mode-jump-again-repeats-last"
    (let ((s (copy-mode-screen :w 20 :h 3
                               :content "hello world"
                               :cursor (cons 0 0))))
      (cl-tmux/commands::copy-mode-jump-forward s #\l)   ; lands col 2
      (cl-tmux/commands::copy-mode-jump-again s)         ; next 'l'
      (expect (= 3 (cdr (cl-tmux/terminal/types:screen-copy-cursor s))))))

  ;; jump-reverse (vi ,) performs the jump in the opposite direction.
  (it "copy-mode-jump-reverse-reverses-forward"
    (let ((s (copy-mode-screen :w 20 :h 3
                               :content "hello world"
                               :cursor (cons 0 0))))
      (cl-tmux/commands::copy-mode-jump-forward s #\l)   ; lands col 2
      (cl-tmux/commands::copy-mode-jump-again  s)        ; lands col 3
      (cl-tmux/commands::copy-mode-jump-reverse s)       ; back to col 2
      (expect (= 2 (cdr (cl-tmux/terminal/types:screen-copy-cursor s))))))

  ;; After t<char>, ; (jump-again) advances PAST the immediately-adjacent occurrence
  ;; instead of sticking one cell before the same char (tmux cx+2, audit #18).
  ;; 'hello world' has 'l' at cols 2, 3, 9.
  (it "copy-mode-jump-to-again-advances-past-adjacent"
    (let ((s (copy-mode-screen :w 20 :h 3 :content "hello world" :cursor (cons 0 0))))
      (cl-tmux/commands::copy-mode-jump-to s #\l)          ; t l → col 1 (before 'l' @2)
      (expect (= 1 (cdr (cl-tmux/terminal/types:screen-copy-cursor s))))
      (cl-tmux/commands::copy-mode-jump-again s)            ; ; must advance, not stick
      (expect (= 2 (cdr (cl-tmux/terminal/types:screen-copy-cursor s))))
      (cl-tmux/commands::copy-mode-jump-again s)            ; ; → next 'l' @9 → col 8
      (expect (= 8 (cdr (cl-tmux/terminal/types:screen-copy-cursor s))))))

  ;; After T<char>, ; advances PAST the adjacent occurrence backward (tmux cx-2,
  ;; audit #18).  'hello world' has 'l' at cols 2, 3.
  (it "copy-mode-jump-to-back-again-advances-past-adjacent"
    (let ((s (copy-mode-screen :w 20 :h 3 :content "hello world" :cursor (cons 0 5))))
      (cl-tmux/commands::copy-mode-jump-to-backward s #\l) ; T l → col 4 (after 'l' @3)
      (expect (= 4 (cdr (cl-tmux/terminal/types:screen-copy-cursor s))))
      (cl-tmux/commands::copy-mode-jump-again s)            ; ; must advance backward
      (expect (= 3 (cdr (cl-tmux/terminal/types:screen-copy-cursor s))))))

  ;;; ── copy-mode-set-mark ───────────────────────────────────────────────────────

  ;; copy-mode-set-mark stores the current cursor position as the mark.
  (it "copy-mode-set-mark-stores-current-cursor"
    (let ((s (make-screen 20 5)))
      (setf (screen-copy-mode-p s)  t
            (screen-copy-offset s)  2
            (cl-tmux/terminal/types:screen-copy-cursor s) (cons 3 7)
            (cl-tmux/terminal/types:screen-copy-mark   s) nil
            (cl-tmux/terminal/types:screen-copy-mark-offset s) 0)
      (cl-tmux/commands::copy-mode-set-mark s)
      (expect (equal (cons 3 7) (cl-tmux/terminal/types:screen-copy-mark s)))
      (expect (= 2 (cl-tmux/terminal/types:screen-copy-mark-offset s)))))

  ;; copy-mode-set-mark must NOT begin a visual selection.
  (it "copy-mode-set-mark-does-not-start-selection"
    (let ((s (make-screen 20 5)))
      (setf (screen-copy-mode-p s)  t
            (screen-copy-offset s)  0
            (cl-tmux/terminal/types:screen-copy-cursor s)    (cons 1 4)
            (cl-tmux/terminal/types:screen-copy-selecting s) nil)
      (cl-tmux/commands::copy-mode-set-mark s)
      (expect (cl-tmux/terminal/types:screen-copy-selecting s) :to-be-falsy)))

  ;; copy-mode-set-mark is a no-op when not in copy mode or when the cursor is NIL.
  ;; Each row: (copy-mode-p cursor description).
  (it "copy-mode-set-mark-noop-table"
    (dolist (row '((nil (0 . 0) "mark must remain NIL when not in copy mode")
                   (t   nil     "mark must remain NIL when cursor is NIL")))
      (destructuring-bind (mode-p cursor desc) row
        (declare (ignore desc))
        (let ((s (make-screen 20 5)))
          (setf (screen-copy-mode-p s)                       mode-p
                (cl-tmux/terminal/types:screen-copy-cursor s) cursor
                (cl-tmux/terminal/types:screen-copy-mark   s) nil)
          (cl-tmux/commands::copy-mode-set-mark s)
          (expect (cl-tmux/terminal/types:screen-copy-mark s) :to-be-falsy)))))

  ;;; ── copy-mode-goto-line ──────────────────────────────────────────────────────

  ;; copy-mode-goto-line N with no scrollback jumps to viewport row N-1.
  (it "copy-mode-goto-line-jumps-to-live-row"
    ;; 10-wide, 5-row screen, no scrollback: vrow = viewport-row (offset=0, sb-n=0).
    ;; goto-line 3 = vrow 2 = viewport row 2.
    (let ((s (make-screen 10 5)))
      (cl-tmux/commands::copy-mode-enter s)
      (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 0))
      (cl-tmux/commands::copy-mode-goto-line s 3)
      (expect (= 2 (car (cl-tmux/terminal/types:screen-copy-cursor s))))))

  ;; copy-mode-goto-line clamps to the last valid row when N exceeds total rows.
  (it "copy-mode-goto-line-clamps-over-max"
    (let ((s (make-screen 10 3)))
      (cl-tmux/commands::copy-mode-enter s)
      (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 0))
      ;; 999 is way past the total row count (3-row screen, no scrollback = vrows 0-2)
      (cl-tmux/commands::copy-mode-goto-line s 999)
      ;; After clamping, cursor row must be within [0, height-1]
      (expect (<= 0 (car (cl-tmux/terminal/types:screen-copy-cursor s)) 2))))

  ;; copy-mode-goto-line is a no-op when not in copy mode.
  (it "copy-mode-goto-line-noop-outside-copy-mode"
    (let ((s (make-screen 10 5)))
      (setf (screen-copy-mode-p s)  nil
            (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 0))
      ;; Should not signal any error, screen must stay out of copy mode.
      (cl-tmux/commands::copy-mode-goto-line s 1)
      (expect (cl-tmux/terminal/types:screen-copy-mode-p s) :to-be-falsy)))

  ;;; ── copy-mode-search-forward-incremental ─────────────────────────────────────

  ;; Neither incremental search function opens a prompt when not in copy mode.
  (it "copy-mode-search-incremental-noop-outside-copy-mode"
    (dolist (fn '(cl-tmux/commands::copy-mode-search-forward-incremental
                  cl-tmux/commands::copy-mode-search-backward-incremental))
      (let ((s (make-screen 10 5)))
        (setf (screen-copy-mode-p s) nil
              *prompt* nil
              cl-tmux/commands::*copy-mode-isearch-origin* nil)
        (funcall fn s)
        (expect *prompt* :to-be-falsy))))

  ;; copy-mode-search-forward/backward-incremental each open a prompt with the
  ;; correct direction label.  Each row: (fn-sym cursor label).
  (it "copy-mode-search-incremental-opens-prompt-with-correct-label"
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
                (expect *prompt* :to-be-truthy)
                (expect (string= label (prompt-label *prompt*))))
            (setf *prompt* nil
                  cl-tmux/commands::*copy-mode-isearch-origin* nil))))))

  ;; Saves cursor+offset in *copy-mode-isearch-origin* when prompt opens.
  (it "copy-mode-search-forward-incremental-saves-origin"
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
              (expect origin :to-be-truthy)
              (expect (equal (cons 2 3) (car origin)))
              (expect (= 5 (cdr origin)))))
        (setf *prompt* nil
              cl-tmux/commands::*copy-mode-isearch-origin* nil))))

  ;;; ── copy-mode-search-backward-incremental ────────────────────────────────────

  ;; prompt-clear (ESC/C-g) restores cursor and offset to pre-search position for
  ;; both forward and backward incremental search.
  ;; Each row: (fn-sym init-cursor init-offset moved-cursor moved-offset).
  (it "copy-mode-search-incremental-cancel-restores-cursor"
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
          (expect (equal init-cursor (screen-copy-cursor s)))
          (expect (= init-offset (screen-copy-offset s)))
          (expect cl-tmux/commands::*copy-mode-isearch-origin* :to-be-falsy)))))

  ;;; ── copy-mode-next-matching-bracket ─────────────────────────────────────────

  ;; Cursor on '(' jumps forward to ')'; cursor on ')' jumps backward to '('.
  (it "copy-mode-next-matching-bracket-paren-table"
    (dolist (row '((2 0 6 "on '(' (col 0) → finds ')' (col 6)")
                   (2 6 0 "on ')' (col 6) → finds '(' (col 0)")))
      (destructuring-bind (start-row start-col expected-col desc) row
        (declare (ignore desc))
        (let ((s (make-screen 20 5)))
          (setf (screen-copy-mode-p s) t)
          (dotimes (i 7)
            (setf (cl-tmux/terminal/types:screen-cell s i 2)
                  (cl-tmux/terminal/types:make-cell :char (char "( foo )" i))))
          (setf (screen-copy-cursor s) (cons start-row start-col)
                (screen-copy-offset  s) 0)
          (cl-tmux/commands::copy-mode-next-matching-bracket s)
          (expect (= expected-col (cdr (screen-copy-cursor s))))))))

  ;; Nested brackets: cursor on outer '(' jumps to the outer matching ')'.
  (it "copy-mode-next-matching-bracket-nested-brackets"
    (let ((s (make-screen 20 5)))
      (setf (screen-copy-mode-p s) t)
      ;; Write "(a(b)c)" at row 0.
      (dotimes (i 7)
        (setf (cl-tmux/terminal/types:screen-cell s i 0)
              (cl-tmux/terminal/types:make-cell :char (char "(a(b)c)" i))))
      (setf (screen-copy-cursor s) (cons 0 0)
            (screen-copy-offset  s) 0)
      (cl-tmux/commands::copy-mode-next-matching-bracket s)
      (expect (= 6 (cdr (screen-copy-cursor s))))))

  ;; Bracket matching is a no-op when not in copy mode.
  (it "copy-mode-next-matching-bracket-noop-outside-copy-mode"
    (let ((s (make-screen 20 5)))
      (setf (screen-copy-mode-p s) nil
            (screen-copy-cursor  s) (cons 0 3))
      (cl-tmux/commands::copy-mode-next-matching-bracket s)
      (expect (equal (cons 0 3) (screen-copy-cursor s)))))

  ;; previous-matching-bracket jumps backward to the matching '(' from various positions.
  ;; Each row: (string nchars cursor-col description).
  (it "copy-mode-previous-matching-bracket-table"
    (dolist (row '(("( foo )"      7  6 "cursor on ')' jumps to matching '('")
                   ("( foo ) tail" 12 8 "cursor after a matched pair finds the previous close")))
      (destructuring-bind (str nchars col desc) row
        (declare (ignore desc))
        (let ((s (make-screen 20 5)))
          (setf (screen-copy-mode-p s) t)
          (dotimes (i nchars)
            (setf (cl-tmux/terminal/types:screen-cell s i 2)
                  (cl-tmux/terminal/types:make-cell :char (char str i))))
          (setf (screen-copy-cursor s) (cons 2 col)
                (screen-copy-offset  s) 0)
          (cl-tmux/commands::copy-mode-previous-matching-bracket s)
          (expect (= 0 (cdr (screen-copy-cursor s))))))))

  ;; Previous bracket matching is a no-op when not in copy mode.
  (it "copy-mode-previous-matching-bracket-noop-outside-copy-mode"
    (let ((s (make-screen 20 5)))
      (setf (screen-copy-mode-p s) nil
            (screen-copy-cursor  s) (cons 0 3))
      (cl-tmux/commands::copy-mode-previous-matching-bracket s)
      (expect (equal (cons 0 3) (screen-copy-cursor s)))))

  ;; A bracket pair spanning two rows is matched across the row boundary —
  ;; every other bracket test keeps both brackets on one row.
  (it "copy-mode-next-matching-bracket-crosses-row-boundary"
    (let ((s (make-screen 20 5)))
      (setf (screen-copy-mode-p s) t)
      (setf (cl-tmux/terminal/types:screen-cell s 0 0)
            (cl-tmux/terminal/types:make-cell :char #\())
      (setf (cl-tmux/terminal/types:screen-cell s 3 1)
            (cl-tmux/terminal/types:make-cell :char #\)))
      (setf (screen-copy-cursor s) (cons 0 0)
            (screen-copy-offset  s) 0)
      (cl-tmux/commands::copy-mode-next-matching-bracket s)
      (expect (equal (cons 1 3) (screen-copy-cursor s)))))

  ;; An open bracket with no matching close anywhere in the buffer leaves the
  ;; cursor untouched — every other bracket test has a real match to find.
  (it "copy-mode-next-matching-bracket-no-match-leaves-cursor"
    (let ((s (make-screen 20 5)))
      (setf (screen-copy-mode-p s) t)
      (setf (cl-tmux/terminal/types:screen-cell s 0 0)
            (cl-tmux/terminal/types:make-cell :char #\())
      (setf (screen-copy-cursor s) (cons 0 0)
            (screen-copy-offset  s) 0)
      (cl-tmux/commands::copy-mode-next-matching-bracket s)
      (expect (equal (cons 0 0) (screen-copy-cursor s))))))
