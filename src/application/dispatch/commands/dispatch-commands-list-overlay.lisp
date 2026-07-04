(in-package #:cl-tmux)

;;; -- List overlay presentation helpers ---------------------------------------

(defun %filtered-overlay-lines-string (lines filter)
  "Return the subset of LINES matching FILTER as one overlay string."
  (%overlay-lines-string
   (loop for line in lines
         when (or (null filter)
                  (search filter line :test #'char-equal))
           collect line)))

(defun %show-list-overlay-rows (rows filter &optional raw-text)
  "Show ROWS, using RAW-TEXT when FILTER is absent."
  (if filter
      (show-overlay (%filtered-overlay-lines-string rows filter))
      (show-overlay (or raw-text (%overlay-lines-string rows)))))

(defun %non-empty-overlay-lines (text)
  "Split TEXT into overlay rows and drop empty lines."
  (remove-if (lambda (line) (zerop (length line)))
             (uiop:split-string text :separator '(#\Newline))))

(defmacro with-list-overlay-rows ((rows display-p) rows-form &body body)
  "Bind ROWS and DISPLAY-P from ROWS-FORM and run BODY when display is needed."
  `(multiple-value-bind (,rows ,display-p) ,rows-form
     (when ,display-p
       ,@body)))

(defmacro with-list-overlay-rows/raw ((rows raw-text display-p) rows-form &body body)
  "Bind ROWS, RAW-TEXT, and DISPLAY-P from ROWS-FORM and run BODY when needed."
  `(multiple-value-bind (,rows ,raw-text ,display-p) ,rows-form
     (when ,display-p
       ,@body)))
