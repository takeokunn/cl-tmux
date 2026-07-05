(in-package #:cl-tmux)

;;;; Menu key dispatch rules.

;;; When the interactive menu overlay is active, most keystrokes are consumed
;;; and routed to the menu navigation commands rather than the active pane.
;;;
;;; define-menu-key-rules follows the same Prolog-like rule style as
;;; define-copy-mode-vi-rules and define-cps-state: each RULE is a
;;; (CONDITION &rest BODY) clause, matched in order.  Uniform "dispatch one
;;; menu command" arms are declarative facts; the digit-jump and default arms
;;; keep their custom bodies verbatim.

(defmacro define-menu-key-rules (&rest rules)
  "Build %DISPATCH-MENU-KEY from an ordered table of (CONDITION &rest BODY)
   rules.  Each matched BODY is responsible for marking *dirty* itself; the
   generated function always returns NIL so the caller stays in ground state
   regardless of which key was pressed."
  `(defun %dispatch-menu-key (session byte)
     "Dispatch BYTE to the active menu overlay and mark the display dirty.
      j — next item; k — previous item; Enter — select; q/Esc — dismiss;
      digit 0-9 — jump to that item index then refresh.  All other keys are
      swallowed (the menu remains open).  Always returns NIL so the caller
      stays in ground state regardless of which key was pressed."
     (declare (ignorable session byte))
     (cond
       ,@(mapcar
          (lambda (rule)
            (destructuring-bind (condition &rest body) rule
              `(,condition ,@body)))
          rules)
       (t nil))
     nil))

(define-menu-key-rules
  ;; j — next item
  ((= byte +byte-j+)
   (dispatch-command session :menu-next byte)
   (setf *dirty* t))
  ;; k — previous item
  ((= byte +byte-k+)
   (dispatch-command session :menu-prev byte)
   (setf *dirty* t))
  ;; Enter — select current item
  ((= byte +byte-enter+)
   (dispatch-command session :menu-select byte)
   (setf *dirty* t))
  ;; q / Escape — dismiss menu
  ((or (= byte +byte-q+) (= byte +byte-esc+))
   (dispatch-command session :menu-dismiss byte)
   (setf *dirty* t))
  ;; Digit 0-9: jump to that item index, then dispatch menu-next with 0 delta
  ;; to trigger overlay refresh via the dispatch-handlers.lisp path.
  ((and (>= byte +byte-digit-0+) (<= byte +byte-digit-9+))
   (let* ((digit  (- byte +byte-digit-0+))
          (length (length (menu-items *active-menu*))))
     (when (< digit length)
       (setf (menu-selected-index *active-menu*) digit)
       ;; Trigger show-overlay refresh via dispatch (avoids direct %format-menu call).
       (dispatch-command session :menu-next byte)
       (dispatch-command session :menu-prev byte)
       (setf *dirty* t)))))
