(in-package #:cl-tmux)

(declaim (special *clients*))

;;; Target resolution shared by arg-aware command handlers.

(defun %resolve-pane-in-window (win target-str)
  "Resolve TARGET-STR to a pane in WIN by pane-id; default to WIN's active pane.
   Accepts bare id (\"2\") and tmux %N sigil (\"%2\")."
  (or (and target-str win
           (let* ((digits (if (and (plusp (length target-str))
                                   (char= (char target-str 0) #\%))
                              (subseq target-str 1)
                              target-str))
                  (n (%parse-integer-or-nil digits)))
             (and n (find n (window-panes win) :key #'pane-id))))
      (and win (window-active-pane win))))

(defun %resolve-window-target (session target-str)
  "Resolve TARGET-STR to a window in SESSION.
   Shorthands: :! last, :+ next, :- prev, :^ first, :$ last."
  (let* ((wins (session-windows session))
         (act  (session-active-window session)))
    (cond
      ((member target-str '(":!" "!") :test #'string=)
       (session-last-window session))
      ((member target-str '(":+" "+") :test #'string=)
       (when wins
         (nth (mod (1+ (or (position act wins) 0)) (length wins)) wins)))
      ((member target-str '(":-" "-") :test #'string=)
       (when wins
         (nth (mod (1- (or (position act wins) 0)) (length wins)) wins)))
      ((member target-str '(":^" "^") :test #'string=) (first wins))
      ((member target-str '(":$" "$") :test #'string=) (car (last wins)))
      (t
       (let ((n (%parse-integer-or-nil target-str)))
         (if n
             (find n wins :key (lambda (w)
                                 (cl-tmux/model:session-window-index session w)))
             (find target-str wins :key #'window-name :test #'string-equal)))))))

(defun %resolve-client-target (target-str)
  "Resolve TARGET-STR to a client connection in *clients*.
   Accepts tmux-like names such as client-0 and client0, plus a bare numeric index."
  (when (and (stringp target-str) (plusp (length target-str)))
    (let* ((index-str (cond
                        ((and (>= (length target-str) 7)
                              (string-equal "client-" target-str :end2 7))
                         (subseq target-str 7))
                        ((and (>= (length target-str) 6)
                              (string-equal "client" target-str :end2 6))
                         (subseq target-str 6))
                        (t target-str)))
           (index (%parse-integer-or-nil index-str)))
      (and (integerp index)
           (>= index 0)
           (nth index *clients*)))))
