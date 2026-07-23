(in-package #:cl-tmux/test)

;;;; CSI line-edit parser-path tests for edit.lisp.
;;;; Suite: delete-insert-chars.

;;; ── SUITE: delete/insert characters (DCH / ICH) ─────────────────────────────
;;;
;;; Driven via the real CSI parser path: CSI n P (DCH) shifts the tail left
;;; and blanks the vacated end; CSI n @ (ICH) shifts the tail right and blanks
;;; the gap.  We also exercise the n >= width edge.

(defmacro define-csi-line-edit-cases (&body cases)
  "Define parser-path DCH/ICH cases from declarative rows."
  (labels ((case-option (options key)
             (getf options key))
           (expand-row-prefix (text end message)
             (declare (ignore message))
             `(expect (string= ,text (row-string s 0 :end ,end))))
           (expand-cells (rows)
             `(dolist (row ',rows)
                (destructuring-bind (expected col desc) row
                  (declare (ignore desc))
                  (expect (char= expected (char-at s col 0))))))
           (expand-blank-range (width message)
             (declare (ignore message))
             `(dotimes (x ,width)
                (expect (char= #\Space (char-at s x 0)))))
           (expand-assertion (assertion)
             (destructuring-bind (kind &rest args) assertion
               (ecase kind
                 (:row-prefix (apply #'expand-row-prefix args))
                 (:cells (apply #'expand-cells args))
                 (:blank-range (apply #'expand-blank-range args)))))
           (expand-case (case)
             (destructuring-bind (name description &rest options) case
               (declare (ignore description))
               (destructuring-bind (width height) (case-option options :screen)
                 `(it ,(string-downcase (symbol-name name))
                    (with-screen (s ,width ,height)
                      (feed s ,(case-option options :text))
                      (feed s (esc ,(case-option options :cursor)))
                      (feed s (csi ,(case-option options :params)
                                   ,(case-option options :final)))
                      ,@(mapcar #'expand-assertion
                                (case-option options :assertions))))))))
    `(progn ,@(mapcar #'expand-case cases))))

(describe "terminal-suite/delete-insert-chars"

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
     :assertions ((:blank-range 5 "col ~D should be blank after oversized ICH, got ~C")))))
