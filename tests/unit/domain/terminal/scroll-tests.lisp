(in-package #:cl-tmux/test)

;;;; Tests for scroll.lisp, erase.lisp, and edit.lisp terminal operations.
;;;; Suites: scroll-ops, erase, scroll-region, delete-insert-chars.

;;; ── SUITE: scroll-ops ───────────────────────────────────────────────────────
;;;
;;; Direct tests for scroll-up-one and scroll-down-one (defined in scroll.lisp).

(def-suite scroll-ops
  :description "Direct calls to scroll-up-one and scroll-down-one"
  :in terminal-suite)
(in-suite scroll-ops)

(defmacro define-scroll-operation-cases (&body cases)
  "Define direct scroll operation cases from declarative rows."
  (labels ((case-option (options key)
             (getf options key))
           (scrollback-length-form ()
             '(length (cl-tmux/terminal/types:screen-scrollback s)))
           (expand-step (step)
             (destructuring-bind (kind &rest args) step
               (ecase kind
                 (:feed `(feed s ,@args))
                 (:scroll-up '(cl-tmux/terminal/actions:scroll-up-one s))
                 (:scroll-down '(cl-tmux/terminal/actions:scroll-down-one s))
                 (:seed-scrollback-to-cap
                  (let ((width (first args)))
                    `(setf (cl-tmux/terminal/types:screen-scrollback s)
                           (loop repeat cap
                                 collect (make-array
                                          ,width
                                          :initial-element
                                          (cl-tmux/terminal/types:blank-cell)))))))))
           (expand-assertion (assertion)
             (destructuring-bind (kind &rest args) assertion
               (ecase kind
                 (:scrollback-length=
                  `(is (= ,(first args) ,(scrollback-length-form))
                       ,(second args)))
                 (:scrollback-length<=cap
                  `(is (<= ,(scrollback-length-form) cap)
                       ,(first args) cap))
                 (:first-scrollback-char
                  `(let ((row (first (cl-tmux/terminal/types:screen-scrollback s))))
                     (is (char= ,(first args) (cell-char (aref row ,(second args))))
                         ,(third args))))
                 (:scrollback-empty
                  `(is (null (cl-tmux/terminal/types:screen-scrollback s))
                       ,(first args)))
                 (:row-blank
                  `(is (row-blank-p s ,(first args)) ,(second args)))
                 (:cell
                  `(is (char= ,(first args)
                              (char-at s ,(second args) ,(third args)))
                       ,(fourth args))))))
           (expand-body (width height steps assertions cap-aware-p)
             (let ((forms (append (mapcar #'expand-step steps)
                                  (mapcar #'expand-assertion assertions))))
               (if cap-aware-p
                   `((let ((cap (or (cl-tmux/options:get-option "history-limit")
                                    cl-tmux/config:+max-scrollback-lines+)))
                       (declare (ignorable cap))
                       (with-screen (s ,width ,height)
                         ,@forms)))
                   `((with-screen (s ,width ,height)
                       ,@forms)))))
           (expand-case (case)
             (destructuring-bind (name description &rest options) case
               (destructuring-bind (width height) (case-option options :screen)
                 `(test ,name
                    ,description
                    ,@(expand-body width
                                   height
                                   (case-option options :steps)
                                   (case-option options :assertions)
                                   (case-option options :cap)))))))
    `(progn ,@(mapcar #'expand-case cases))))

(define-scroll-operation-cases
  (scroll-up-one-pushes-to-scrollback
   "scroll-up-one adds the displaced top row to the scrollback buffer."
   :screen (5 3)
   :steps ((:feed "hello")
           (:scroll-up))
   :assertions ((:scrollback-length= 1 "scrollback should have 1 entry after one scroll")
                (:first-scrollback-char #\h 0 "scrollback row 0 should start with 'h'")))
  (scroll-up-one-caps-at-max-scrollback
   "scroll-up-one trims the scrollback to the effective history-limit.
   trim-scroll-history honours the 'history-limit' option (default 2000)
   which supersedes +max-scrollback-lines+ (1000) at runtime."
   :screen (5 3)
   :cap t
   :steps ((:seed-scrollback-to-cap 5)
           (:scroll-up))
   :assertions ((:scrollback-length<=cap
                 "scrollback must not exceed the effective history-limit (~D)")))
  (scroll-up-partial-region-does-not-push-to-scrollback
   "Scrolling within a partial scroll region (scroll-top > 0) must NOT add to the
   scrollback: only full-top-of-screen scrolling contributes to history, matching
   real tmux grid_scroll_history_up semantics."
   :screen (5 5)
   :steps ((:feed (esc "[2;4r"))
           (:feed (esc "[4;1H"))
           (:feed (string #\Newline)))
   :assertions ((:scrollback-empty
                 "partial scroll-region scrolling must not populate scrollback")))
  (scroll-up-alt-screen-does-not-push-to-scrollback
   "Scrolling in the alternate screen must not pollute the primary scrollback."
   :screen (5 3)
   :steps ((:feed "line0")
           (:feed (esc "[?1049h"))
           (:feed "altline0")
           (:feed (string #\Newline))
           (:feed (string #\Newline))
           (:feed (string #\Newline)))
   :assertions ((:scrollback-empty
                 "alt-screen scrolling must not push to the primary scrollback")))
  (scroll-down-one-inserts-blank-top-row
   "scroll-down-one moves content down; the new top row is blank."
   :screen (5 3)
   :steps ((:feed "hi")
           (:scroll-down))
   :assertions ((:row-blank 0 "row 0 must be blank after scroll-down-one")
                (:cell #\h 0 1 "old row 0 content must be on row 1"))))

;;; ── SUITE: erase ────────────────────────────────────────────────────────────

(def-suite erase
  :description "ED (erase display) and EL (erase line) modes"
  :in terminal-suite)
(in-suite erase)

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
             `(is (string= ,expected (row-string s ,row))
                  "row ~D expected ~S, got ~S"
                  ,row ,expected (row-string s ,row)))
           (expand-row-prefix (row expected end)
             `(is (string= ,expected (row-string s ,row :end ,end))
                  "row ~D prefix expected ~S, got ~S"
                  ,row ,expected (row-string s ,row :end ,end)))
           (expand-cell (expected x y)
             `(is (char= ,expected (char-at s ,x ,y))
                  "cell (~D,~D) expected ~C, got ~C"
                  ,x ,y ,expected (char-at s ,x ,y)))
           (expand-blank-rows (height)
             `(dotimes (y ,height)
                (is (row-blank-p s y) "row ~D must be blank" y)))
           (expand-assertion (assertion)
             (destructuring-bind (kind &rest args) assertion
               (ecase kind
                 (:row-string (apply #'expand-row-string args))
                 (:row-prefix (apply #'expand-row-prefix args))
                 (:row-blank `(is (row-blank-p s ,@args)))
                 (:blank-rows (apply #'expand-blank-rows args))
                 (:cell (apply #'expand-cell args))
                 (:cursor-y `(is (= ,@args (screen-cursor-y s)))))))
           (expand-case (case)
             (destructuring-bind (name description &rest options) case
               (destructuring-bind (width height) (case-option options :screen)
                 `(test ,name
                    ,description
                    (with-screen (s ,width ,height)
                      ,@(mapcar #'expand-setup (case-option options :setup))
                      ,@(when (case-option options :cursor)
                          `((feed s (esc ,(case-option options :cursor)))))
                      (feed s (esc ,(case-option options :command)))
                      ,@(mapcar #'expand-assertion
                                (case-option options :assertions))))))))
    `(progn ,@(mapcar #'expand-case cases))))

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

(test scroll-on-clear-on-pushes-screen-to-history
  "With scroll-on-clear on, ESC[2J moves the visible content into the scrollback
   before erasing, so a full-screen clear stays in history."
  (let ((cl-tmux/terminal/actions::*scroll-on-clear-function* (lambda () t)))
    (with-screen (s 5 3)
      (fill-screen s)
      (is (null (cl-tmux/terminal/types:screen-scrollback s))
          "scrollback must be empty before the clear")
      (feed s (esc "[2J"))
      (is (= 3 (length (cl-tmux/terminal/types:screen-scrollback s)))
          "all 3 visible rows must be pushed to scrollback (got ~D)"
          (length (cl-tmux/terminal/types:screen-scrollback s)))
      (dotimes (y 3)
        (is (row-blank-p s y) "row ~D must be blank after the clear" y)))))

(test scroll-on-clear-off-discards-content
  "With scroll-on-clear off (no policy installed), ESC[2J erases without pushing to
   the scrollback — the existing default behaviour."
  (let ((cl-tmux/terminal/actions::*scroll-on-clear-function* nil))
    (with-screen (s 5 3)
      (fill-screen s)
      (feed s (esc "[2J"))
      (is (null (cl-tmux/terminal/types:screen-scrollback s))
          "scrollback must stay empty when scroll-on-clear is off"))))

(test scroll-on-clear-skips-alternate-screen
  "scroll-on-clear does not push to history on the alternate screen (no scrollback)."
  (let ((cl-tmux/terminal/actions::*scroll-on-clear-function* (lambda () t)))
    (with-screen (s 5 3)
      (feed s (esc "[?1049h"))      ; enter the alternate screen
      (fill-screen s)
      (feed s (esc "[2J"))
      (is (null (cl-tmux/terminal/types:screen-scrollback s))
          "an alt-screen clear must not push to the scrollback"))))

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
             `(is (string= ,expected (row-string s ,row))
                  "row ~D expected ~S, got ~S"
                  ,row ,expected (row-string s ,row)))
           (expand-cell (expected x y)
             `(is (char= ,expected (char-at s ,x ,y))
                  "cell (~D,~D) expected ~C, got ~C"
                  ,x ,y ,expected (char-at s ,x ,y)))
           (expand-blank-rows (height)
             `(dotimes (y ,height)
                (is (row-blank-p s y) "row ~D must be blank" y)))
           (expand-assertion (assertion)
             (destructuring-bind (kind &rest args) assertion
               (ecase kind
                 (:row-string (apply #'expand-row-string args))
                 (:row-blank `(is (row-blank-p s ,@args)))
                 (:blank-rows (apply #'expand-blank-rows args))
                 (:cell (apply #'expand-cell args)))))
           (expand-case (case)
             (destructuring-bind (name description &rest options) case
               (destructuring-bind (width height) (case-option options :screen)
                 (destructuring-bind (x y) (case-option options :cursor)
                   `(test ,name
                      ,description
                      (with-screen (s ,width ,height)
                        ,@(mapcar #'expand-setup (case-option options :setup))
                        (setf (cl-tmux/terminal/types:screen-cursor-x s) ,x
                              (cl-tmux/terminal/types:screen-cursor-y s) ,y)
                        ,(expand-call (case-option options :call))
                        ,@(mapcar #'expand-assertion
                                  (case-option options :assertions)))))))))
    `(progn ,@(mapcar #'expand-case cases))))

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
   :assertions ((:row-blank 0))))

;;; ── SUITE: scroll-region ────────────────────────────────────────────────────

(def-suite scroll-region
  :description "Scrolling, DECSTBM, reverse index, IL/DL"
  :in terminal-suite)
(in-suite scroll-region)

(defmacro define-scroll-region-cases (&body cases)
  "Define scroll-region parser-path cases from declarative rows."
  (labels ((case-option (options key)
             (getf options key))
           (expand-step (step)
             (destructuring-bind (kind &rest args) step
               (ecase kind
                 (:feed `(feed s ,@args))
                 (:feed-lines `(feed-lines s ,@args)))))
           (expand-row-prefix (row expected end message)
             `(is (string= ,expected (row-string s ,row :end ,end))
                  ,message (row-string s ,row :end ,end)))
           (expand-assertion (assertion)
             (destructuring-bind (kind &rest args) assertion
               (ecase kind
                 (:check-row `(check-row s ,@args))
                 (:row-prefix (apply #'expand-row-prefix args))
                 (:row-blank `(is (row-blank-p s ,@args))))))
           (expand-case (case)
             (destructuring-bind (name description &rest options) case
               (destructuring-bind (width height) (case-option options :screen)
                 `(test ,name
                    ,description
                    (with-screen (s ,width ,height)
                      ,@(mapcar #'expand-step (case-option options :steps))
                      ,@(mapcar #'expand-assertion
                                (case-option options :assertions))))))))
    `(progn ,@(mapcar #'expand-case cases))))

(define-scroll-region-cases
  (scroll-auto
   "Writing a 4th line into a 3-row screen scrolls the content up."
   :screen (5 3)
   :steps ((:feed-lines "L1" "L2" "L3" "L4"))
   :assertions ((:check-row 0 "L2")
                (:check-row 2 "L4")))
  (decstbm-restricts-scroll-to-region
   "DECSTBM restricts scrolling to the specified region (rows 2-3 of 5)."
   :screen (5 5)
   :steps ((:feed-lines "R0" "R1" "R2" "R3" "R4")
           (:feed (esc "[2;4r"))
           (:feed (esc "[4;1H"))
           (:feed-lines "" "NR"))
   :assertions ((:row-prefix 0 "R0" 2 "row 0 should be untouched, got ~S")))
  (reverse-index-scrolls-region-down
   "ESC M at the top of the scroll region scrolls the region down."
   :screen (5 3)
   :steps ((:feed-lines "AA" "BB" "CC")
           (:feed (esc "[1;1H"))
           (:feed (esc "M")))
   :assertions ((:row-prefix 1 "AA" 2
                  "after RI, old row 0 should be at row 1; got ~S")
                (:row-blank 0)))
  (il-insert-lines-pushes-content-down
   "ESC[2L (insert 2 lines) pushes existing content down."
   :screen (5 4)
   :steps ((:feed-lines "AA" "BB" "CC" "DD")
           (:feed (esc "[2;1H"))
           (:feed (esc "[2L")))
   :assertions ((:check-row 0 "AA")
                (:row-blank 1)
                (:row-blank 2)
                (:check-row 3 "BB")))
  (dl-delete-lines-pulls-content-up
   "ESC[2M (delete 2 lines) pulls content up."
   :screen (5 4)
   :steps ((:feed-lines "AA" "BB" "CC" "DD")
           (:feed (esc "[2;1H"))
           (:feed (esc "[2M")))
   :assertions ((:check-row 0 "AA")
                (:check-row 1 "DD")
                (:row-blank 2)
                (:row-blank 3))))

;;; ── SUITE: delete/insert characters (DCH / ICH) ─────────────────────────────
;;;
;;; Driven via the real CSI parser path: CSI n P (DCH) shifts the tail left
;;; and blanks the vacated end; CSI n @ (ICH) shifts the tail right and blanks
;;; the gap.  We also exercise the n >= width edge.

(def-suite delete-insert-chars
  :description "DCH (CSI P) and ICH (CSI @) via the CSI parser"
  :in terminal-suite)
(in-suite delete-insert-chars)

(defmacro define-csi-line-edit-cases (&body cases)
  "Define parser-path DCH/ICH cases from declarative rows."
  (labels ((case-option (options key)
             (getf options key))
           (expand-row-prefix (text end message)
             `(is (string= ,text (row-string s 0 :end ,end))
                  ,message (row-string s 0 :end ,end)))
           (expand-cells (rows)
             `(dolist (row ',rows)
                (destructuring-bind (expected col desc) row
                  (is (char= expected (char-at s col 0))
                      "~A" desc))))
           (expand-blank-range (width message)
             `(dotimes (x ,width)
                (is (char= #\Space (char-at s x 0))
                    ,message x (char-at s x 0))))
           (expand-assertion (assertion)
             (destructuring-bind (kind &rest args) assertion
               (ecase kind
                 (:row-prefix (apply #'expand-row-prefix args))
                 (:cells (apply #'expand-cells args))
                 (:blank-range (apply #'expand-blank-range args)))))
           (expand-case (case)
             (destructuring-bind (name description &rest options) case
               (destructuring-bind (width height) (case-option options :screen)
                 `(test ,name
                    ,description
                    (with-screen (s ,width ,height)
                      (feed s ,(case-option options :text))
                      (feed s (esc ,(case-option options :cursor)))
                      (feed s (csi ,(case-option options :params)
                                   ,(case-option options :final)))
                      ,@(mapcar #'expand-assertion
                                (case-option options :assertions))))))))
    `(progn ,@(mapcar #'expand-case cases))))

(define-csi-line-edit-cases
  (dch-shifts-left-and-blanks-tail
   "CSI 2 P at column 0 deletes 'ab' from 'abcde', shifting 'cde' left and
   blanking the two vacated cells at the end of the line."
   :screen (8 2)
   :text "abcde"
   :cursor "[1;1H"
   :params "2"
   :final #\P
   :assertions ((:cells ((#\c     0 "col0 should be 'c'")
                         (#\d     1 "col1 should be 'd'")
                         (#\e     2 "col2 should be 'e'")
                         (#\Space 3 "col3 should be blank")
                         (#\Space 4 "col4 should be blank")))))
  (dch-at-midline
   "CSI 1 P at a non-zero column deletes one char and shifts the rest left."
   :screen (8 2)
   :text "abcde"
   :cursor "[1;2H"
   :params "1"
   :final #\P
   :assertions ((:row-prefix "acde" 4 "expected 'acde', got ~S")
                (:cells ((#\Space 4 "vacated last cell should be blank")))))
  (dch-default-param-deletes-one
   "CSI P with no parameter deletes a single character (p1* defaults to 1)."
   :screen (8 2)
   :text "abcde"
   :cursor "[1;1H"
   :params ""
   :final #\P
   :assertions ((:row-prefix "bcde" 4 "expected 'bcde', got ~S")))
  (dch-n-ge-width-clears-from-cursor
   "CSI n P with n >= remaining width blanks the whole line from the cursor.
   delete-chars caps the blank-fill at (max cx (- w n)); when n >= w the shift
   loop runs empty and every cell from cursor to end is blanked."
   :screen (5 2)
   :text "abcde"
   :cursor "[1;1H"
   :params "9"
   :final #\P
   :assertions ((:blank-range 5 "col ~D should be blank after oversized DCH, got ~C")))
  (ich-shifts-right-and-blanks-gap
   "CSI 2 @ at column 0 inserts two blanks, pushing 'abcde' right; the trailing
   chars shifted past the right margin are lost."
   :screen (5 2)
   :text "abcde"
   :cursor "[1;1H"
   :params "2"
   :final #\@
   :assertions ((:cells ((#\Space 0 "col0 should be blank gap")
                         (#\Space 1 "col1 should be blank gap")
                         (#\a     2 "col2 should be 'a'")
                         (#\b     3 "col3 should be 'b'")
                         (#\c     4 "col4 should be 'c'")))))
  (ich-at-midline
   "CSI 1 @ at a non-zero column inserts one blank and pushes the tail right."
   :screen (6 2)
   :text "abcde"
   :cursor "[1;3H"
   :params "1"
   :final #\@
   :assertions ((:cells ((#\a     0 "col0 unchanged 'a'")
                         (#\b     1 "col1 unchanged 'b'")
                         (#\Space 2 "col2 should be the inserted blank")
                         (#\c     3 "col3 should be shifted 'c'")
                         (#\d     4 "col4 should be shifted 'd'")))))
  (ich-n-ge-width-blanks-from-cursor
   "CSI n @ with n >= remaining width blanks every cell from the cursor; the
   insert-chars blank-fill is capped at (min (1- w) (+ cx n -1))."
   :screen (5 2)
   :text "abcde"
   :cursor "[1;1H"
   :params "9"
   :final #\@
   :assertions ((:blank-range 5 "col ~D should be blank after oversized ICH, got ~C"))))
