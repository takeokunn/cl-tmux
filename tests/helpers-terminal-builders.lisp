;;;; Terminal builder and inspection helpers for cl-tmux tests.

(in-package #:cl-tmux/test)

(defmacro with-screen ((var w h) &body body)
  "Bind VAR to a fresh screen of width W and height H for BODY."
  `(let ((,var (make-screen ,w ,h))) ,@body))

(defun octets (string)
  "Convert STRING to an (unsigned-byte 8) vector."
  (map '(simple-array (unsigned-byte 8) (*)) #'char-code string))

(defun feed (screen string)
  "Process STRING through SCREEN's emulator."
  (screen-process-bytes screen (octets string))
  screen)

(defun copy-mode-screen (&key (w 20) (h 5) (content "") cursor mark selecting)
  "Return a copy-mode screen pre-filled with CONTENT and optional copy state."
  (let ((screen (make-screen w h)))
    (unless (string= content "")
      (feed screen content))
    (cl-tmux/commands::copy-mode-enter screen)
    (when cursor
      (setf (cl-tmux/terminal/types:screen-copy-cursor screen) cursor))
    (when mark
      (setf (cl-tmux/terminal/types:screen-copy-mark screen) mark))
    (when selecting
      (setf (cl-tmux/terminal/types:screen-copy-selecting screen) selecting))
    screen))

(defmacro with-copy-mode-cursor ((screen-var row col &key (w 20) (h 5)) &body body)
  "Bind SCREEN-VAR to a fresh copy-mode screen with cursor at (ROW . COL)."
  `(let ((,screen-var (copy-mode-screen :w ,w :h ,h :cursor (cons ,row ,col))))
     ,@body))

(defun esc (fmt &rest args)
  "Build an escape sequence string with ESC prefix."
  (format nil "~C~?" #\Escape fmt args))

(defun csi (params final)
  "Build the string ESC [ PARAMS FINAL."
  (format nil "~C[~A~A" #\Escape params (string final)))

(defun row-string (screen y &key (start 0) end)
  "Return the characters of row Y from START to END."
  (let* ((w (screen-width screen))
         (e (or end w)))
    (with-output-to-string (s)
      (loop for x from start below (min e w)
            do (write-char (cell-char (screen-cell screen x y)) s)))))

(defun cell-at  (screen x y) (screen-cell screen x y))
(defun char-at  (screen x y) (cell-char   (screen-cell screen x y)))
(defun fg-at    (screen x y) (cell-fg     (screen-cell screen x y)))
(defun bg-at    (screen x y) (cell-bg     (screen-cell screen x y)))
(defun attrs-at (screen x y) (cell-attrs  (screen-cell screen x y)))

(defmacro check-cursor (screen cx cy)
  "Assert that SCREEN's cursor is at column CX, row CY."
  `(progn
     (is (= ,cx (screen-cursor-x ,screen))
         "cursor-x: expected ~D got ~D" ,cx (screen-cursor-x ,screen))
     (is (= ,cy (screen-cursor-y ,screen))
         "cursor-y: expected ~D got ~D" ,cy (screen-cursor-y ,screen))))

(defmacro check-table (rows &key (test #'=))
  "Assert each (ACTUAL EXPECTED DESC) row in ROWS with TEST."
  `(dolist (row ,rows)
     (destructuring-bind (actual expected desc) row
       (is (funcall ,test expected actual)
           "~A: expected ~S got ~S" desc expected actual))))

(defun row-blank-p (screen y)
  "Return T when every cell in row Y of SCREEN contains a space."
  (every (lambda (c) (char= #\Space c))
         (coerce (row-string screen y) 'list)))

(defun utf8-feed (screen lisp-string)
  "Encode LISP-STRING as UTF-8 and feed the bytes to SCREEN."
  (screen-process-bytes screen
                        (babel:string-to-octets lisp-string :encoding :utf-8))
  screen)

(defun feed-lines (screen &rest lines)
  "Feed LINES to SCREEN separated by CR/LF."
  (loop for (line . more) on lines
        do (feed screen line)
        when more do (feed screen (format nil "~C~C" #\Return #\Linefeed)))
  screen)

(defun display-row-string (screen y &key end)
  "Characters of viewport row Y via screen-display-cell."
  (let ((end (or end (screen-width screen))))
    (with-output-to-string (s)
      (loop for x below end
            do (write-char (cell-char (screen-display-cell screen x y)) s)))))
