(in-package #:cl-tmux/test)

;;;; Erase tests for erase.lisp through parser and direct action paths.
;;;; Suite: erase.

;;; ── SUITE: erase ────────────────────────────────────────────────────────────

(defun fill-screen (screen)
  "Fill every cell of SCREEN with 'X' and return SCREEN."
  (dotimes (y (screen-height screen) screen)
    (dotimes (x (screen-width screen))
      (feed screen "X"))))

(defmacro define-erase-parser-cases (&body cases)
  "Define ED/EL parser-path cases from declarative rows."
  (labels ((case-option (options key)
             (getf options key))
           (expand-setup (setup)
             (destructuring-bind (kind &rest args) setup
               (ecase kind
                 (:fill-screen '(fill-screen s))
                 (:feed `(feed s ,@args)))))
           (expand-row-string (row expected)
             `(expect (string= ,expected (row-string s ,row))))
           (expand-row-prefix (row expected end)
             `(expect (string= ,expected (row-string s ,row :end ,end))))
           (expand-cell (expected x y)
             `(expect (char= ,expected (char-at s ,x ,y))))
           (expand-blank-rows (height)
             `(dotimes (y ,height)
                (expect (row-blank-p s y))))
           (expand-assertion (assertion)
             (destructuring-bind (kind &rest args) assertion
               (ecase kind
                 (:row-string (apply #'expand-row-string args))
                 (:row-prefix (apply #'expand-row-prefix args))
                 (:row-blank `(expect (row-blank-p s ,@args)))
                 (:blank-rows (apply #'expand-blank-rows args))
                 (:cell (apply #'expand-cell args))
                 (:cursor-y `(expect (= ,@args (screen-cursor-y s)))))))
           (expand-case (case)
             (destructuring-bind (name description &rest options) case
               (declare (ignore description))
               (destructuring-bind (width height) (case-option options :screen)
                 `(it ,(string-downcase (symbol-name name))
                    (with-screen (s ,width ,height)
                      ,@(mapcar #'expand-setup (case-option options :setup))
                      ,@(when (case-option options :cursor)
                          `((feed s (esc ,(case-option options :cursor)))))
                      (feed s (esc ,(case-option options :command)))
                      ,@(mapcar #'expand-assertion
                                (case-option options :assertions))))))))
    `(progn ,@(mapcar #'expand-case cases))))

;;; ── Direct erase-display tests covering guarded edge cases ──────────────────
;;;
;;; These call erase-display directly to exercise the edge at cy=0 for mode 1
;;; (the when guard in erase.lisp) and other paths not clearly covered by the
;;; high-level CSI path above.

(defmacro define-direct-erase-cases (&body cases)
  "Define direct erase-display/erase-line cases from declarative rows."
  (labels ((case-option (options key)
             (getf options key))
           (expand-setup (setup)
             (destructuring-bind (kind &rest args) setup
               (ecase kind
                 (:fill-screen '(fill-screen s))
                 (:feed `(feed s ,@args)))))
           (expand-call (call)
             (destructuring-bind (kind mode) call
               (ecase kind
                 (:display `(cl-tmux/terminal/actions:erase-display s ,mode))
                 (:line `(cl-tmux/terminal/actions:erase-line s ,mode)))))
           (expand-row-string (row expected)
             `(expect (string= ,expected (row-string s ,row))))
           (expand-cell (expected x y)
             `(expect (char= ,expected (char-at s ,x ,y))))
           (expand-blank-rows (height)
             `(dotimes (y ,height)
                (expect (row-blank-p s y))))
           (expand-assertion (assertion)
             (destructuring-bind (kind &rest args) assertion
               (ecase kind
                 (:row-string (apply #'expand-row-string args))
                 (:row-blank `(expect (row-blank-p s ,@args)))
                 (:blank-rows (apply #'expand-blank-rows args))
                 (:cell (apply #'expand-cell args)))))
           (expand-case (case)
             (destructuring-bind (name description &rest options) case
               (declare (ignore description))
               (destructuring-bind (width height) (case-option options :screen)
                 (destructuring-bind (x y) (case-option options :cursor)
                   `(it ,(string-downcase (symbol-name name))
                      (with-screen (s ,width ,height)
                        ,@(mapcar #'expand-setup (case-option options :setup))
                        (setf (cl-tmux/terminal/types:screen-cursor-x s) ,x
                              (cl-tmux/terminal/types:screen-cursor-y s) ,y)
                        ,(expand-call (case-option options :call))
                        ,@(mapcar #'expand-assertion
                                  (case-option options :assertions)))))))))
    `(progn ,@(mapcar #'expand-case cases))))

(describe "terminal-suite/erase"

  (define-erase-parser-cases
    (erase-display-erases-to-end-of-screen
     "ESC[J erases from the cursor position to the end of the display."
     :screen (5 3)
     :setup ((:fill-screen))
     :cursor "[2;3H"
     :command "[0J"
     :assertions ((:row-string 0 "XXXXX")
                  (:cell #\Space 2 1)
                  (:cell #\Space 4 1)
                  (:row-blank 2)))
    (erase-display-erases-from-start-to-cursor
     "ESC[1J erases from the start of the display to the cursor (inclusive)."
     :screen (5 3)
     :setup ((:fill-screen))
     :cursor "[2;3H"
     :command "[1J"
     :assertions ((:row-blank 0)
                  (:cell #\Space 0 1)
                  (:cell #\Space 2 1)
                  (:cell #\X 3 1)))
    (erase-display-clears-entire-screen
     "ESC[2J erases the entire display."
     :screen (5 3)
     :setup ((:fill-screen))
     :command "[2J"
     :assertions ((:blank-rows 3)))
    (erase-line-erases-to-end-of-line
     "ESC[K erases from the cursor to the end of the current line."
     :screen (10 2)
     :setup ((:feed "abcdefghij"))
     :cursor "[1;5H"
     :command "[0K"
     :assertions ((:row-prefix 0 "abcd" 4)
                  (:cell #\Space 4 0)
                  (:cell #\Space 9 0)))
    (erase-line-erases-from-start-to-cursor
     "ESC[1K erases from the start of the line to the cursor (inclusive)."
     :screen (10 2)
     :setup ((:feed "abcdefghij"))
     :cursor "[1;4H"
     :command "[1K"
     :assertions ((:cell #\Space 0 0)
                  (:cell #\Space 3 0)
                  (:cell #\e 4 0)))
    (erase-line-clears-entire-line
     "ESC[2K erases the entire current line."
     :screen (10 2)
     :setup ((:feed "abcdefghij"))
     :cursor "[1;5H"
     :command "[2K"
     :assertions ((:row-blank 0)
                  (:cursor-y 0))))

  ;; With scroll-on-clear on, ESC[2J moves the visible content into the scrollback
  ;; before erasing, so a full-screen clear stays in history.
  (it "scroll-on-clear-on-pushes-screen-to-history"
    (let ((cl-tmux/terminal/actions::*scroll-on-clear-function* (lambda () t)))
      (with-screen (s 5 3)
        (fill-screen s)
        (expect (null (cl-tmux/terminal/types:screen-scrollback s)))
        (feed s (esc "[2J"))
        (expect (= 3 (length (cl-tmux/terminal/types:screen-scrollback s))))
        (dotimes (y 3)
          (expect (row-blank-p s y))))))

  ;; With scroll-on-clear off (no policy installed), ESC[2J erases without pushing to
  ;; the scrollback — the existing default behaviour.
  (it "scroll-on-clear-off-discards-content"
    (let ((cl-tmux/terminal/actions::*scroll-on-clear-function* nil))
      (with-screen (s 5 3)
        (fill-screen s)
        (feed s (esc "[2J"))
        (expect (null (cl-tmux/terminal/types:screen-scrollback s))))))

  ;; scroll-on-clear does not push to history on the alternate screen (no scrollback).
  (it "scroll-on-clear-skips-alternate-screen"
    (let ((cl-tmux/terminal/actions::*scroll-on-clear-function* (lambda () t)))
      (with-screen (s 5 3)
        (feed s (esc "[?1049h"))
        (fill-screen s)
        (feed s (esc "[2J"))
        (expect (null (cl-tmux/terminal/types:screen-scrollback s))))))

  (define-direct-erase-cases
    (erase-display-direct-mode-0-from-cy-zero
     "erase-display mode 0 with cursor at (0,0) erases the full screen."
     :screen (5 3)
     :setup ((:fill-screen))
     :cursor (0 0)
     :call (:display 0)
     :assertions ((:blank-rows 3)))
    (erase-display-direct-mode-1-at-cy-zero-skips-above-rows
     "erase-display mode 1 with cy=0 erases only from (0,0) to cursor on row 0.
     The 'when (> cy 0)' guard in erase.lisp means no above-rows erase is attempted."
     :screen (5 3)
     :setup ((:fill-screen))
     :cursor (2 0)
     :call (:display 1)
     :assertions ((:cell #\Space 0 0)
                  (:cell #\Space 2 0)
                  (:cell #\X 3 0)
                  (:row-string 1 "XXXXX")))
    (erase-display-direct-mode-2-clears-all-rows
     "erase-display mode 2 called directly blanks every row."
     :screen (5 3)
     :setup ((:fill-screen))
     :cursor (0 0)
     :call (:display 2)
     :assertions ((:blank-rows 3))))

  ;;; ── Direct erase-line tests for modes 1 and 2 ───────────────────────────────
  ;;;
  ;;; Coverage gap: modes 1 and 2 were only exercised through the CSI path.
  ;;; These call erase-line directly to give each mode an isolated assertion.

  (define-direct-erase-cases
    (erase-line-direct-mode-1-erases-start-to-cursor
     "erase-line mode 1 called directly blanks from col 0 to the cursor (inclusive)."
     :screen (10 5)
     :setup ((:feed "hello"))
     :cursor (3 0)
     :call (:line 1)
     :assertions ((:cell #\Space 0 0)
                  (:cell #\Space 3 0)
                  (:cell #\o 4 0)))
    (erase-line-direct-mode-2-erases-entire-line
     "erase-line mode 2 called directly blanks the entire current line."
     :screen (10 5)
     :setup ((:feed "hello"))
     :cursor (0 0)
     :call (:line 2)
     :assertions ((:row-blank 0)))))
