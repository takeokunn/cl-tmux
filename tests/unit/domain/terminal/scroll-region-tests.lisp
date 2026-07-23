(in-package #:cl-tmux/test)

;;;; Scroll-region parser-path tests for scroll.lisp and edit.lisp.
;;;; Suite: scroll-region.

;;; ── SUITE: scroll-region ────────────────────────────────────────────────────

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
             (declare (ignore message))
             `(expect (string= ,expected (row-string s ,row :end ,end))))
           (expand-assertion (assertion)
             (destructuring-bind (kind &rest args) assertion
               (ecase kind
                 (:check-row `(check-row s ,@args))
                 (:row-prefix (apply #'expand-row-prefix args))
                 (:row-blank `(expect (row-blank-p s ,@args))))))
           (expand-case (case)
             (destructuring-bind (name &rest options) case
               (destructuring-bind (width height) (case-option options :screen)
                 `(it ,(string-downcase (symbol-name name))
                    (with-screen (s ,width ,height)
                      ,@(mapcar #'expand-step (case-option options :steps))
                      ,@(mapcar #'expand-assertion
                                (case-option options :assertions))))))))
    `(progn ,@(mapcar #'expand-case cases))))

(describe "terminal-suite/scroll-region"

  (define-scroll-region-cases
    (scroll-auto
     ;; Writing a 4th line into a 3-row screen scrolls the content up.
     :screen (5 3)
     :steps ((:feed-lines "L1" "L2" "L3" "L4"))
     :assertions ((:check-row 0 "L2")
                  (:check-row 2 "L4")))
    (decstbm-restricts-scroll-to-region
     ;; DECSTBM restricts scrolling to the specified region (rows 2-3 of 5).
     :screen (5 5)
     :steps ((:feed-lines "R0" "R1" "R2" "R3" "R4")
             (:feed (esc "[2;4r"))
             (:feed (esc "[4;1H"))
             (:feed-lines "" "NR"))
     :assertions ((:row-prefix 0 "R0" 2 "row 0 should be untouched, got ~S")))
    (reverse-index-scrolls-region-down
     ;; ESC M at the top of the scroll region scrolls the region down.
     :screen (5 3)
     :steps ((:feed-lines "AA" "BB" "CC")
             (:feed (esc "[1;1H"))
             (:feed (esc "M")))
     :assertions ((:row-prefix 1 "AA" 2
                    "after RI, old row 0 should be at row 1; got ~S")
                  (:row-blank 0)))
    (il-insert-lines-pushes-content-down
     ;; ESC[2L (insert 2 lines) pushes existing content down.
     :screen (5 4)
     :steps ((:feed-lines "AA" "BB" "CC" "DD")
             (:feed (esc "[2;1H"))
             (:feed (esc "[2L")))
     :assertions ((:check-row 0 "AA")
                  (:row-blank 1)
                  (:row-blank 2)
                  (:check-row 3 "BB")))
    (dl-delete-lines-pulls-content-up
     ;; ESC[2M (delete 2 lines) pulls content up.
     :screen (5 4)
     :steps ((:feed-lines "AA" "BB" "CC" "DD")
             (:feed (esc "[2;1H"))
             (:feed (esc "[2M")))
     :assertions ((:check-row 0 "AA")
                  (:check-row 1 "DD")
                  (:row-blank 2)
                  (:row-blank 3)))))
