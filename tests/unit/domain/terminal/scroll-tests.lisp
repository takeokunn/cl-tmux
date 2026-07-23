(in-package #:cl-tmux/test)

;;;; Scroll operation tests for scroll.lisp.
;;;; Suite: scroll-ops.

;;; ── SUITE: scroll-ops ───────────────────────────────────────────────────────
;;;
;;; Direct tests for scroll-up-one and scroll-down-one (defined in scroll.lisp).

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
                  `(expect (= ,(first args) ,(scrollback-length-form))))
                 (:scrollback-length<=cap
                  `(expect (<= ,(scrollback-length-form) cap)))
                 (:first-scrollback-char
                  `(let ((row (first (cl-tmux/terminal/types:screen-scrollback s))))
                     (expect (char= ,(first args) (cell-char (aref row ,(second args)))))))
                 (:scrollback-empty
                  `(expect (null (cl-tmux/terminal/types:screen-scrollback s))))
                 (:row-blank
                  `(expect (row-blank-p s ,(first args))))
                 (:cell
                  `(expect (char= ,(first args)
                                  (char-at s ,(second args) ,(third args))))))))
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
               (declare (ignore description))
               (destructuring-bind (width height) (case-option options :screen)
                 `(it ,(string-downcase (symbol-name name))
                    ,@(expand-body width
                                   height
                                   (case-option options :steps)
                                   (case-option options :assertions)
                                   (case-option options :cap)))))))
    `(progn ,@(mapcar #'expand-case cases))))

(describe "terminal-suite/scroll-ops"

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
                  (:cell #\h 0 1 "old row 0 content must be on row 1")))))
