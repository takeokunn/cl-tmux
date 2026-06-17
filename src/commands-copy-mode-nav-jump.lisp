(in-package #:cl-tmux/commands)

;;; Copy-mode jump-to-char and line-jump commands.

(defvar *copy-mode-last-jump* nil
  "Most recent jump-to-char state: (direction char till-p).")

(defun %copy-mode-jump (screen direction char till-p)
  "Move the cursor on the current line to the nearest CHAR in DIRECTION."
  (when (screen-copy-mode-p screen)
    (setf *copy-mode-last-jump* (list direction char till-p))
    (let* ((row   (car (screen-copy-cursor screen)))
           (col   (cdr (screen-copy-cursor screen)))
           (chars (%copy-mode-row-chars screen row))
           (w     (length chars)))
      (if (eq direction :forward)
          (loop for c from (1+ col) below w
                when (char= (aref chars c) char)
                  do (setf (cdr (screen-copy-cursor screen))
                           (if till-p (max 0 (1- c)) c))
                     (return t))
          (loop for c downfrom (1- col) to 0
                when (char= (aref chars c) char)
                  do (setf (cdr (screen-copy-cursor screen))
                           (if till-p (min (1- w) (1+ c)) c))
                     (return t))))))

(defmacro define-jump-to-char-commands (&rest specs)
  "Generate jump-to-char wrapper functions from a declarative (name dir till doc) table."
  `(progn
     ,@(mapcar (lambda (spec)
                 (destructuring-bind (name dir till doc) spec
                   `(defun ,name (screen char)
                      ,doc
                      (%copy-mode-jump screen ,dir char ,till))))
               specs)))

(define-jump-to-char-commands
  (copy-mode-jump-forward    :forward  nil "Jump to next CHAR on current line (vi f<char>).")
  (copy-mode-jump-backward   :backward nil "Jump to previous CHAR on current line (vi F<char>).")
  (copy-mode-jump-to         :forward  t   "Jump to just before next CHAR on current line (vi t<char>).")
  (copy-mode-jump-to-backward :backward t   "Jump to just after previous CHAR on current line (vi T<char>)."))

(defun %copy-mode-replay-last-jump (screen reverse-p)
  "Replay the most recent jump-to-char, optionally reversing its direction."
  (when *copy-mode-last-jump*
    (destructuring-bind (dir ch till) *copy-mode-last-jump*
      (%copy-mode-jump screen
                       (if reverse-p
                           (if (eq dir :forward) :backward :forward)
                           dir)
                       ch
                       till))))

(defun copy-mode-jump-again (screen)
  "Repeat the last jump-to-char with the same direction, char, and mode (vi ;)."
  (%copy-mode-replay-last-jump screen nil))

(defun copy-mode-jump-reverse (screen)
  "Reverse the last jump-to-char (vi ,): same char, opposite direction."
  (%copy-mode-replay-last-jump screen t))

(defun copy-mode-goto-line (screen line-number)
  "Jump to LINE-NUMBER (1-based: 1 = oldest scrollback row) in copy mode."
  (when (and (screen-copy-mode-p screen)
             (integerp line-number)
             (> line-number 0))
    (%set-cursor-vrow screen (1- line-number))))
