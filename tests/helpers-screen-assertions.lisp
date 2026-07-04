(in-package #:cl-tmux/test)

;;;; Screen assertion DSL and command-state fixtures.

;;; These macros raise the abstraction level for common screen assertions,
;;; making test intent visible and reducing boilerplate IS calls.

(defmacro check-row (screen y expected-string)
  "Assert that row Y of SCREEN starts with EXPECTED-STRING."
  `(is (string= ,expected-string
                (row-string ,screen ,y :end (length ,expected-string)))
       "row ~D: expected ~S got ~S"
       ,y ,expected-string
       (row-string ,screen ,y :end (length ,expected-string))))

(defmacro check-cell (screen x y &key char fg bg attrs)
  "Assert cell attributes at column X, row Y of SCREEN.
   Only non-NIL keyword arguments are checked."
  (let ((forms '()))
    (when char
      (push `(is (char= ,char (char-at ,screen ,x ,y))
                 "char at (~D,~D): expected ~C got ~C" ,x ,y ,char
                 (char-at ,screen ,x ,y))
            forms))
    (when fg
      (push `(is (= ,fg (fg-at ,screen ,x ,y))
                 "fg at (~D,~D): expected ~D got ~D" ,x ,y ,fg
                 (fg-at ,screen ,x ,y))
            forms))
    (when bg
      (push `(is (= ,bg (bg-at ,screen ,x ,y))
                 "bg at (~D,~D): expected ~D got ~D" ,x ,y ,bg
                 (bg-at ,screen ,x ,y))
            forms))
    (when attrs
      (push `(is (= ,attrs (attrs-at ,screen ,x ,y))
                 "attrs at (~D,~D): expected #x~X got #x~X" ,x ,y ,attrs
                 (attrs-at ,screen ,x ,y))
            forms))
    `(progn ,@(nreverse forms))))

(defmacro check-sgr-state (screen &key (fg 7) (bg 0) (attrs 0))
  "Assert the current SGR pen state (foreground, background, attribute bitmask)."
  `(progn
     (is (= ,fg (cl-tmux/terminal/types:screen-cur-fg ,screen))
         "cur-fg: expected ~D got ~D" ,fg
         (cl-tmux/terminal/types:screen-cur-fg ,screen))
     (is (= ,bg (cl-tmux/terminal/types:screen-cur-bg ,screen))
         "cur-bg: expected ~D got ~D" ,bg
         (cl-tmux/terminal/types:screen-cur-bg ,screen))
     (is (= ,attrs (cl-tmux/terminal/types:screen-cur-attrs ,screen))
         "cur-attrs: expected #x~X got #x~X" ,attrs
         (cl-tmux/terminal/types:screen-cur-attrs ,screen))))

(defmacro with-filled-screen ((var w h fill-char) &body body)
  "Bind VAR to a W×H screen pre-filled with FILL-CHAR, then run BODY."
  `(let ((,var (make-screen ,w ,h)))
     (dotimes (row ,h)
       (dotimes (col ,w)
         (feed ,var (string ,fill-char))))
     ,@body))

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
  `(with-command-test-state (,sess :overlay t)
     (is (null ,command-form)
         "~A must be rejected" ,description)
     (is (search ,overlay-message *overlay*)
         "~A must explain that the argument is unsupported" ,description)
     ,@body
     (is-false cl-tmux::*dirty*
               "~A must not mark the model dirty after rejection" ,description)))
