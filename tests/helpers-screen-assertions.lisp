(in-package #:cl-tmux/test)

;;;; Screen assertion DSL and command-state fixtures.

;;; These macros raise the abstraction level for common screen assertions,
;;; making test intent visible and reducing boilerplate IS calls.

(defmacro check-row (screen y expected-string)
  "Assert that row Y of SCREEN starts with EXPECTED-STRING."
  `(expect (string= ,expected-string
                    (row-string ,screen ,y :end (length ,expected-string)))))

(defmacro check-cell (screen x y &key char fg bg attrs)
  "Assert cell attributes at column X, row Y of SCREEN.
   Only non-NIL keyword arguments are checked."
  (let ((forms '()))
    (when char
      (push `(expect (char= ,char (char-at ,screen ,x ,y))) forms))
    (when fg
      (push `(expect (= ,fg (fg-at ,screen ,x ,y))) forms))
    (when bg
      (push `(expect (= ,bg (bg-at ,screen ,x ,y))) forms))
    (when attrs
      (push `(expect (= ,attrs (attrs-at ,screen ,x ,y))) forms))
    `(progn ,@(nreverse forms))))

(defmacro check-sgr-state (screen &key (fg 7) (bg 0) (attrs 0))
  "Assert the current SGR pen state (foreground, background, attribute bitmask)."
  `(progn
     (expect (= ,fg (cl-tmux/terminal/types:screen-cur-fg ,screen)))
     (expect (= ,bg (cl-tmux/terminal/types:screen-cur-bg ,screen)))
     (expect (= ,attrs (cl-tmux/terminal/types:screen-cur-attrs ,screen)))))

(defmacro with-command-test-state ((sess &key overlay) &body body)
  "Run BODY with a single-session server state and a clean dirty flag."
  `(let ((cl-tmux::*server-sessions* (list (cons "0" ,sess)))
         (cl-tmux::*dirty* nil)
         ,@(when overlay `((*overlay* nil))))
     ,@body))

(defmacro with-server-size-state ((&key (rows 24) (cols 80)) &body body)
  "Run BODY with the server's terminal-size and event-loop specials isolated:
   *term-rows*/*term-cols* seeded to ROWS/COLS, *dirty* cleared, and *running*
   set.  Extracted so tests exercising +msg-resize+/+msg-attach+/apply-client-size
   share one fixture instead of repeating the same four-binding LET."
  `(let ((cl-tmux::*term-rows* ,rows)
         (cl-tmux::*term-cols* ,cols)
         (cl-tmux::*dirty*    nil)
         (cl-tmux::*running*  t))
     ,@body))

(defmacro with-command-rejection-state ((sess command-form overlay-message
                                              description)
                                        &body body)
  "Assert that COMMAND-FORM is rejected and reports OVERLAY-MESSAGE."
  (declare (ignore description))
  `(with-command-test-state (,sess :overlay t)
     (expect (null ,command-form))
     (expect (search ,overlay-message *overlay*))
     ,@body
     (expect cl-tmux::*dirty* :to-be-falsy)))
