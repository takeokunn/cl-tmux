(in-package #:cl-tmux/test)

;;;; copy-mode selection commands (src/commands.lisp)

(in-suite commands-suite)

;;; -- copy-mode-clear-selection (send -X clear-selection) ----------------------

(test copy-mode-clear-selection-drops-selection-keeps-cursor
  "copy-mode-clear-selection clears the mark + selection flags but keeps the
   cursor and stays in copy mode (tmux clear-selection / default vi Escape)."
  (let ((s (copy-mode-screen)))
    (setf (cl-tmux/terminal/types:screen-copy-selecting        s) t
          (cl-tmux/terminal/types:screen-copy-mark             s) (cons 0 2)
          (cl-tmux/terminal/types:screen-copy-cursor           s) (cons 0 5)
          (cl-tmux/terminal/types:screen-copy-rect-select-p    s) t)
    (cl-tmux/commands::copy-mode-clear-selection s)
    (is-false (cl-tmux/terminal/types:screen-copy-selecting s)
              "selection flag must be cleared")
    (is (null (cl-tmux/terminal/types:screen-copy-mark s))
        "mark must be dropped")
    (is-false (cl-tmux/terminal/types:screen-copy-rect-select-p s)
              "rectangle-select flag must be reset")
    (is (equal (cons 0 5) (cl-tmux/terminal/types:screen-copy-cursor s))
        "cursor position must be preserved (stay put in copy mode)")
    (is-true (cl-tmux/terminal/types:screen-copy-mode-p s)
             "must remain in copy mode (clear-selection does not cancel)")
    (is-true (cl-tmux/terminal/types:screen-dirty-p s)
             "screen must be dirty after clearing")))

(test copy-mode-clear-selection-noop-without-selection
  "copy-mode-clear-selection is a clean no-op when there is no selection/mark."
  (let ((s (copy-mode-screen)))
    (setf (cl-tmux/terminal/types:screen-copy-selecting s) nil
          (cl-tmux/terminal/types:screen-copy-mark      s) nil
          (cl-tmux/terminal/types:screen-copy-cursor    s) (cons 0 3)
          (cl-tmux/terminal/types:screen-dirty-p        s) nil)
    (finishes (cl-tmux/commands::copy-mode-clear-selection s))
    (is (equal (cons 0 3) (cl-tmux/terminal/types:screen-copy-cursor s))
        "cursor unchanged")
    (is-false (cl-tmux/terminal/types:screen-dirty-p s)
              "no dirty mark when there was nothing to clear")))

(test copy-mode-clear-selection-x-command-mapped
  "The send -X name clear-selection maps to the :copy-mode-clear-selection
   dispatch keyword."
  (is (eq :copy-mode-clear-selection
          (copy-mode-x-command-value "clear-selection"))
      "clear-selection must be a known send -X command")
  (is (eq :copy-mode-stop-selection
          (copy-mode-x-command-value "stop-selection"))
      "stop-selection is a supported send -X command (tmux window-copy)")
  (is (eq :copy-mode-yank
          (copy-mode-x-command-value "copy-selection-and-cancel"))
      "copy-selection-and-cancel copies the selection and exits copy mode")
  (is (eq :copy-mode-toggle-position
          (copy-mode-x-command-value "toggle-position"))
      "toggle-position is a supported send -X command (tmux window-copy)")
  (is-false (copy-mode-x-command-value "scroll-mouse")
            "scroll-mouse is no longer a supported send -X command"))

;;; -- copy-mode-other-end ------------------------------------------------------

(test copy-mode-other-end-preserves-selection-text
  "Swapping the two ends must not change the selected text or normalised bounds."
  (let ((s (copy-mode-screen :content "foo bar baz")))
    (setf (cl-tmux/terminal/types:screen-copy-selecting s) t
          (cl-tmux/terminal/types:screen-copy-mark      s) (cons 0 4)
          (cl-tmux/terminal/types:screen-copy-cursor    s) (cons 0 6))
    (let ((text-before (cl-tmux/commands::%selection-text s)))
      (multiple-value-bind (sr0 er0 sc0 ec0) (cl-tmux/commands::%selection-bounds s)
        (cl-tmux/commands::copy-mode-other-end s)
        (let ((text-after (cl-tmux/commands::%selection-text s)))
          (multiple-value-bind (sr1 er1 sc1 ec1) (cl-tmux/commands::%selection-bounds s)
            (is (string= text-before text-after)
                "selected text must be identical after other-end")
            (is (and (= sr0 sr1) (= er0 er1) (= sc0 sc1) (= ec0 ec1))
                "normalised selection bounds must be identical after other-end")))))))

(test copy-mode-other-end-double-swap-restores-original
  "Two successive swaps restore the original cursor and mark."
  (let ((s (copy-mode-screen)))
    (setf (cl-tmux/terminal/types:screen-copy-selecting s) t
          (cl-tmux/terminal/types:screen-copy-mark      s) (cons 0 2)
          (cl-tmux/terminal/types:screen-copy-cursor    s) (cons 0 5))
    (cl-tmux/commands::copy-mode-other-end s)
    (cl-tmux/commands::copy-mode-other-end s)
    (is (equal (cons 0 5) (cl-tmux/terminal/types:screen-copy-cursor s))
        "cursor must return to its original position after two swaps")
    (is (equal (cons 0 2) (cl-tmux/terminal/types:screen-copy-mark s))
        "mark must return to its original position after two swaps")))

;;; -- copy-mode-select-word ----------------------------------------------------

(defun %copy-mode-select-word-screen (content w h row col)
  (let ((screen (apply #'copy-mode-screen
                       (append (when content (list :content content))
                               (when w (list :w w))
                               (when h (list :h h))))))
    (when (or row col)
      (setf (cl-tmux/terminal/types:screen-copy-cursor screen)
            (cons (or row 0) (or col 0))))
    screen))

(defmacro with-copy-mode-select-word-screen ((screen &key content w h row col) &body body)
  `(let ((,screen (%copy-mode-select-word-screen ,content ,w ,h ,row ,col)))
     ,@body))

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
    (is (string= (%copy-mode-select-word-case-value case :text)
                 (cl-tmux/commands::%selection-text s))
        "~A text"
        (%copy-mode-select-word-case-value case :name))))

(test copy-mode-select-word-table
  "copy-mode-select-word selects the expected range for word and boundary cases."
  (dolist (case *copy-mode-select-word-cases*)
    (%check-copy-mode-select-word-case case)))

(test copy-mode-select-word-sets-dirty-flag
  "select-word marks the screen dirty."
  (with-copy-mode-select-word-screen (s :content "foo bar baz" :row 0 :col 5)
    (setf (cl-tmux/terminal/types:screen-dirty-p s) nil)
    (is-false (cl-tmux/terminal/types:screen-dirty-p s)
              "precondition: dirty-p NIL before select-word")
    (cl-tmux/commands::copy-mode-select-word s)
    (is-true (cl-tmux/terminal/types:screen-dirty-p s)
             "dirty-p must be T after select-word")))

(test copy-mode-select-word-no-op-when-not-in-copy-mode
  "select-word is a harmless no-op when copy mode is not active."
  (let ((s (make-screen 20 5)))
    (feed s "foo bar baz")
    (finishes (cl-tmux/commands::copy-mode-select-word s))
    (is-false (cl-tmux/terminal/types:screen-copy-selecting s)
              "selecting must remain NIL when not in copy mode")
    (is (null (cl-tmux/terminal/types:screen-copy-mark s))
        "mark must remain NIL when not in copy mode")))

;;; -- selection-mode -----------------------------------------------------------

(defparameter *copy-mode-selection-mode-cases*
  '(("line" t)
    ("char" nil)))

(test copy-mode-selection-mode-table
  "selection-mode begins selection with the requested granularity."
  (dolist (case *copy-mode-selection-mode-cases*)
    (destructuring-bind (mode line-selection-p) case
      (let ((s (copy-mode-screen)))
        (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 2))
        (cl-tmux/commands::copy-mode-selection-mode s mode)
        (is-true (cl-tmux/terminal/types:screen-copy-selecting s)
                 "selection-mode ~A must start selecting" mode)
        (is (eq line-selection-p
                (cl-tmux/terminal/types:screen-copy-line-selection-p s))
            "selection-mode ~A line-selection flag" mode)))))
