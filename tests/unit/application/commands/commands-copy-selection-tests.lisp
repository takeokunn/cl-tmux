(in-package #:cl-tmux/test)

;;;; copy-mode selection commands (src/commands.lisp)

(defmacro with-copy-mode-select-word-screen ((screen &key content w h row col) &body body)
  `(let ((,screen (%copy-mode-select-word-screen ,content ,w ,h ,row ,col)))
     ,@body))

;; Must be genuine top-level DEFPARAMETERs (not nested inside DESCRIBE's
;; body): a special-variable declaration nested inside DESCRIBE only executes
;; at suite-registration time, so the compiler doesn't yet know the symbol is
;; special when it compiles the sibling IT forms in the same file that
;; reference it as a free variable.
(defparameter *copy-mode-select-word-cases*
  '((:name "word under cursor"
     :content "foo bar baz" :row 0 :col 5 :mark (0 . 4) :cursor (0 . 7) :text "bar")
    (:name "single separator cell"
     :content "foo bar baz" :row 0 :col 3 :mark (0 . 3) :cursor (0 . 4) :text " ")
    (:name "rightmost column"
     :content "cat" :w 3 :h 3 :row 0 :col 1 :mark (0 . 0) :cursor (0 . 3) :text "cat")
    (:name "start of row"
     :content "foo bar baz" :row 0 :col 0 :mark (0 . 0) :cursor (0 . 3) :text "foo")
    (:name "multi-space gap boundary"
     :content "ab   cd" :row 0 :col 5 :mark (0 . 5) :cursor (0 . 7) :text "cd")))

(defparameter *copy-mode-selection-mode-cases*
  '(("line" t)
    ("char" nil)))

(describe "commands-suite"

  ;; -- copy-mode-clear-selection (send -X clear-selection) ----------------------

  ;; copy-mode-clear-selection clears the mark + selection flags but keeps the
  ;; cursor and stays in copy mode (tmux clear-selection / default vi Escape).
  (it "copy-mode-clear-selection-drops-selection-keeps-cursor"
    (let ((s (copy-mode-screen)))
      (setf (cl-tmux/terminal/types:screen-copy-selecting        s) t
            (cl-tmux/terminal/types:screen-copy-mark             s) (cons 0 2)
            (cl-tmux/terminal/types:screen-copy-cursor           s) (cons 0 5)
            (cl-tmux/terminal/types:screen-copy-rect-select-p    s) t)
      (cl-tmux/commands::copy-mode-clear-selection s)
      (expect (cl-tmux/terminal/types:screen-copy-selecting s) :to-be-falsy)
      (expect (null (cl-tmux/terminal/types:screen-copy-mark s)))
      (expect (cl-tmux/terminal/types:screen-copy-rect-select-p s) :to-be-falsy)
      (expect (equal (cons 0 5) (cl-tmux/terminal/types:screen-copy-cursor s)))
      (expect (cl-tmux/terminal/types:screen-copy-mode-p s) :to-be-truthy)
      (expect (cl-tmux/terminal/types:screen-dirty-p s) :to-be-truthy)))

  ;; copy-mode-clear-selection is a clean no-op when there is no selection/mark.
  (it "copy-mode-clear-selection-noop-without-selection"
    (let ((s (copy-mode-screen)))
      (setf (cl-tmux/terminal/types:screen-copy-selecting s) nil
            (cl-tmux/terminal/types:screen-copy-mark      s) nil
            (cl-tmux/terminal/types:screen-copy-cursor    s) (cons 0 3)
            (cl-tmux/terminal/types:screen-dirty-p        s) nil)
      (finishes (cl-tmux/commands::copy-mode-clear-selection s))
      (expect (equal (cons 0 3) (cl-tmux/terminal/types:screen-copy-cursor s)))
      (expect (cl-tmux/terminal/types:screen-dirty-p s) :to-be-falsy)))

  ;; The send -X name clear-selection maps to the :copy-mode-clear-selection
  ;; dispatch keyword.
  (it "copy-mode-clear-selection-x-command-mapped"
    (expect (eq :copy-mode-clear-selection
                (copy-mode-x-command-value "clear-selection")))
    (expect (eq :copy-mode-stop-selection
                (copy-mode-x-command-value "stop-selection")))
    (expect (eq :copy-mode-yank
                (copy-mode-x-command-value "copy-selection-and-cancel")))
    (expect (eq :copy-mode-toggle-position
                (copy-mode-x-command-value "toggle-position")))
    (expect (copy-mode-x-command-value "scroll-mouse") :to-be-falsy))

  ;; -- copy-mode-other-end ------------------------------------------------------

  ;; Swapping the two ends must not change the selected text or normalised bounds.
  (it "copy-mode-other-end-preserves-selection-text"
    (let ((s (copy-mode-screen :content "foo bar baz")))
      (setf (cl-tmux/terminal/types:screen-copy-selecting s) t
            (cl-tmux/terminal/types:screen-copy-mark      s) (cons 0 4)
            (cl-tmux/terminal/types:screen-copy-cursor    s) (cons 0 6))
      (let ((text-before (cl-tmux/commands::%selection-text s)))
        (multiple-value-bind (sr0 er0 sc0 ec0) (cl-tmux/commands::%selection-bounds s)
          (cl-tmux/commands::copy-mode-other-end s)
          (let ((text-after (cl-tmux/commands::%selection-text s)))
            (multiple-value-bind (sr1 er1 sc1 ec1) (cl-tmux/commands::%selection-bounds s)
              (expect (string= text-before text-after))
              (expect (and (= sr0 sr1) (= er0 er1) (= sc0 sc1) (= ec0 ec1)))))))))

  ;; Two successive swaps restore the original cursor and mark.
  (it "copy-mode-other-end-double-swap-restores-original"
    (let ((s (copy-mode-screen)))
      (setf (cl-tmux/terminal/types:screen-copy-selecting s) t
            (cl-tmux/terminal/types:screen-copy-mark      s) (cons 0 2)
            (cl-tmux/terminal/types:screen-copy-cursor    s) (cons 0 5))
      (cl-tmux/commands::copy-mode-other-end s)
      (cl-tmux/commands::copy-mode-other-end s)
      (expect (equal (cons 0 5) (cl-tmux/terminal/types:screen-copy-cursor s)))
      (expect (equal (cons 0 2) (cl-tmux/terminal/types:screen-copy-mark s)))))

  ;; -- copy-mode-select-word ----------------------------------------------------

  (defun %copy-mode-select-word-screen (content w h row col)
    (let ((screen (apply #'copy-mode-screen
                         (append (when content (list :content content))
                                 (when w (list :w w))
                                 (when h (list :h h))))))
      (when (or row col)
        (setf (cl-tmux/terminal/types:screen-copy-cursor screen)
              (cons (or row 0) (or col 0))))
      screen))

  (defun %copy-mode-select-word-case-value (case key &optional default)
    (getf case key default))

  (defun %check-copy-mode-select-word-case (case)
    (with-copy-mode-select-word-screen
        (s :content (%copy-mode-select-word-case-value case :content)
           :w (%copy-mode-select-word-case-value case :w)
           :h (%copy-mode-select-word-case-value case :h)
           :row (%copy-mode-select-word-case-value case :row)
           :col (%copy-mode-select-word-case-value case :col))
      (cl-tmux/commands::copy-mode-select-word s)
      (check-table
       (list (list (cl-tmux/terminal/types:screen-copy-mark s)
                   (%copy-mode-select-word-case-value case :mark)
                   (format nil "~A mark" (%copy-mode-select-word-case-value case :name)))
             (list (cl-tmux/terminal/types:screen-copy-cursor s)
                   (%copy-mode-select-word-case-value case :cursor)
                   (format nil "~A cursor" (%copy-mode-select-word-case-value case :name))))
       :test #'equal)
      (expect (string= (%copy-mode-select-word-case-value case :text)
                       (cl-tmux/commands::%selection-text s)))))

  ;; copy-mode-select-word selects the expected range for word and boundary cases.
  (it "copy-mode-select-word-table"
    (dolist (case *copy-mode-select-word-cases*)
      (%check-copy-mode-select-word-case case)))

  ;; select-word marks the screen dirty.
  (it "copy-mode-select-word-sets-dirty-flag"
    (with-copy-mode-select-word-screen (s :content "foo bar baz" :row 0 :col 5)
      (setf (cl-tmux/terminal/types:screen-dirty-p s) nil)
      (expect (cl-tmux/terminal/types:screen-dirty-p s) :to-be-falsy)
      (cl-tmux/commands::copy-mode-select-word s)
      (expect (cl-tmux/terminal/types:screen-dirty-p s) :to-be-truthy)))

  ;; select-word is a harmless no-op when copy mode is not active.
  (it "copy-mode-select-word-no-op-when-not-in-copy-mode"
    (let ((s (make-screen 20 5)))
      (feed s "foo bar baz")
      (finishes (cl-tmux/commands::copy-mode-select-word s))
      (expect (cl-tmux/terminal/types:screen-copy-selecting s) :to-be-falsy)
      (expect (null (cl-tmux/terminal/types:screen-copy-mark s)))))

  ;; -- selection-mode -----------------------------------------------------------

  ;; selection-mode begins selection with the requested granularity.
  (it "copy-mode-selection-mode-table"
    (dolist (case *copy-mode-selection-mode-cases*)
      (destructuring-bind (mode line-selection-p) case
        (let ((s (copy-mode-screen)))
          (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 2))
          (cl-tmux/commands::copy-mode-selection-mode s mode)
          (expect (cl-tmux/terminal/types:screen-copy-selecting s) :to-be-truthy)
          (expect (eq line-selection-p
                      (cl-tmux/terminal/types:screen-copy-line-selection-p s))))))))
