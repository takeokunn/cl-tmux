(in-package #:cl-tmux/test)

;;;; Interactive single-line input-prompt tests (prompt.lisp).
;;;;
;;;; Overlay, popup, and menu tests live in overlay-tests.lisp. Editing,
;;;; cursor, kill/delete, and change-notification coverage lives in
;;;; prompt-editing-tests.lisp. Event-loop wiring and status-bar coverage live
;;;; in prompt-tests-wiring.lisp.
;;;; All prompt tests use with-clean-prompt (from helpers-input-fixtures.lisp) to guarantee
;;;; that *prompt* and cl-tmux::*dirty* are reset; the raw let form is never used.

(def-suite prompt-suite :description "Interactive input prompt")
(in-suite prompt-suite)

;;; -- Shared test helpers -----------------------------------------------------

(defun make-rename-window ()
  "A single-pane window (fd -1) named \"old\" suitable for rename testing."
  (make-window :id 1 :name "old" :width 20 :height 5
               :panes (list (make-pane :id 1 :fd -1 :screen (make-screen 20 5)))))

(defmacro with-rename-window ((var) &body body)
  "Bind VAR to a fresh rename-window fixture and reset *prompt* cleanly for BODY."
  `(let ((,var (make-rename-window)))
     (with-clean-prompt
       ,@body)))

(defun make-noop-submit ()
  "Return a no-op on-submit function suitable for use in tests."
  (lambda (s) (declare (ignore s)) nil))

(defmacro with-noop-prompt ((initial) &body body)
  "Start a prompt labelled \"p\" seeded with INITIAL text and a no-op submit callback.
   Binds *prompt* cleanly so state never leaks between tests."
  `(with-clean-prompt
     (prompt-start "p" ,initial (make-noop-submit))
     ,@body))

(defmacro with-prompt-at (position &body body)
  "Start a prompt seeded with \"hello\" (cursor at POSITION) and evaluate BODY.
   Binds *prompt* cleanly so state does not leak."
  `(with-noop-prompt ("hello")
     (setf (prompt-cursor-index *prompt*) ,position)
     ,@body))

;;; -- Prompt struct constructors and predicates --------------------------------

(test make-prompt-defaults
  "make-prompt with no keyword arguments fills slots to documented defaults."
  (let ((p (make-prompt)))
    (check-table (list (list (prompt-label p)        "" "default label is empty string")
                       (list (prompt-buffer p)       "" "default buffer is empty string")
                       (list (prompt-cursor-index p) 0  "default cursor-index is 0"))
                 :test #'equal)
    (is (null (prompt-on-submit p)) "default on-submit is NIL")))

(test make-prompt-keyword-args
  "make-prompt keyword arguments override all defaults."
  (let ((fn (lambda (s) s)))
    (let ((p (make-prompt :label "lbl" :buffer "buf"
                          :cursor-index 3 :on-submit fn)))
      (is (string= "lbl" (prompt-label p)))
      (is (string= "buf" (prompt-buffer p)))
      (is (= 3 (prompt-cursor-index p)))
      (is (eq fn (prompt-on-submit p))))))

(test prompt-p-recognises-prompt-struct
  "prompt-p returns T for a PROMPT and NIL for any other value."
  (let ((p (make-prompt)))
    (is (prompt-p p) "prompt-p must return T for a make-prompt result")
    (dolist (val (list nil 42 ""))
      (is (not (prompt-p val)) "prompt-p must return NIL for ~S" val))))

;;; -- Pure prompt state -------------------------------------------------------

(test prompt-inactive-by-default
  "With no active prompt, prompt-active-p and prompt-text are NIL."
  (with-clean-prompt
    (is (null (prompt-active-p)))
    (is (null (prompt-text)))))

(test prompt-start-activates
  "prompt-start seeds label/buffer/on-submit and activates the prompt."
  (with-clean-prompt
    (prompt-start "rename-window" "old" (make-noop-submit))
    (is (prompt-active-p))
    (is (string= "old" (prompt-buffer *prompt*)))
    (is (string= "rename-window" (prompt-label *prompt*)))
    (is (functionp (prompt-on-submit *prompt*)))
    (is (string= "rename-window: old|" (prompt-text)))))

(test prompt-start-cursor-index-at-end
  "prompt-start sets cursor-index to the length of the initial buffer."
  (with-noop-prompt ("hello")
    (is (= 5 (prompt-cursor-index *prompt*))
        "cursor must start at end of initial buffer")))

(test prompt-start-empty-buffer-cursor-at-zero
  "prompt-start with an empty initial buffer places cursor at index 0."
  (with-noop-prompt ("")
    (is (= 0 (prompt-cursor-index *prompt*))
        "cursor must be at 0 for empty initial buffer")))

(test prompt-on-submit-accessor
  "prompt-on-submit stores and returns the callback supplied to prompt-start."
  (with-clean-prompt
    (let ((cb (lambda (s) (format nil "got:~A" s))))
      (prompt-start "p" "text" cb)
      (is (eq cb (prompt-on-submit *prompt*))
          "prompt-on-submit must return the exact function passed to prompt-start"))))

(test prompt-on-change-and-on-cancel-accessors
  "prompt-on-change and prompt-on-cancel return the exact callbacks passed to
   prompt-start via :on-change and :on-cancel."
  (with-clean-prompt
    (let ((change-cb (lambda (s) (declare (ignore s)) nil))
          (cancel-cb (lambda () nil)))
      (prompt-start "p" "text" (make-noop-submit)
                    :on-change change-cb :on-cancel cancel-cb)
      (is (eq change-cb (prompt-on-change *prompt*))
          "prompt-on-change must return the exact function passed to prompt-start")
      (is (eq cancel-cb (prompt-on-cancel *prompt*))
          "prompt-on-cancel must return the exact function passed to prompt-start"))))

(test prompt-single-key-accessor
  "prompt-single-key reflects the :single-key argument passed to prompt-start."
  (with-clean-prompt
    (prompt-start "p" "" (make-noop-submit) :single-key t)
    (is-true (prompt-single-key *prompt*)
             "prompt-single-key must be T when prompt-start is given :single-key t"))
  (with-clean-prompt
    (prompt-start "p" "" (make-noop-submit))
    (is (null (prompt-single-key *prompt*))
        "prompt-single-key must default to NIL when :single-key is not supplied")))

(test prompt-vi-normal-p-accessor-defaults-and-setf
  "prompt-vi-normal-p defaults to NIL on a fresh prompt and can be set with setf
   (the events layer flips it when entering/leaving vi normal mode)."
  (with-clean-prompt
    (prompt-start "p" "" (make-noop-submit))
    (is (null (prompt-vi-normal-p *prompt*))
        "prompt-vi-normal-p must default to NIL")
    (setf (prompt-vi-normal-p *prompt*) t)
    (is-true (prompt-vi-normal-p *prompt*)
             "setf on prompt-vi-normal-p must update the slot")))

(test with-active-prompt-runs-body-only-when-active
  "with-active-prompt binds its variable to *prompt* and runs BODY only when a
   prompt is active; it is a no-op (returns NIL) when *prompt* is NIL."
  (with-clean-prompt
    (is (null (with-active-prompt (p) p :ran))
        "with-active-prompt must not run BODY when *prompt* is NIL")
    (prompt-start "p" "hi" (make-noop-submit))
    (is (eq :ran (with-active-prompt (p) p :ran))
        "with-active-prompt must run BODY and return its value when active")
    (is (eq *prompt* (with-active-prompt (p) p))
        "with-active-prompt must bind its variable to the current *prompt*")))

(test prompt-history-prev-next-restores-in-progress-input
  "History navigation walks newest-first entries and Down restores current input."
  (with-clean-prompt
    (prompt-start "p" "li" (make-noop-submit)
                  :history '("list-windows" "new-window"))
    (prompt-history-prev)
    (is (string= "list-windows" (prompt-buffer *prompt*))
        "first Up must load newest history entry")
    (prompt-history-prev)
    (is (string= "new-window" (prompt-buffer *prompt*))
        "second Up must load the next older history entry")
    (prompt-history-next)
    (is (string= "list-windows" (prompt-buffer *prompt*))
        "first Down must move toward newer history")
    (prompt-history-next)
    (is (string= "li" (prompt-buffer *prompt*))
        "Down from newest history must restore the in-progress input")
    (is (= 2 (prompt-cursor-index *prompt*))
        "restored input must place cursor at end")))

(test prompt-history-edit-resets-navigation-base
  "Editing a recalled history entry makes future Up navigation start from that edit."
  (with-clean-prompt
    (prompt-start "p" "" (make-noop-submit)
                  :history '("list-windows" "new-window"))
    (prompt-history-prev)
    (prompt-input #\s)
    (is (string= "list-windowss" (prompt-buffer *prompt*)))
    (prompt-history-next)
    (is (string= "list-windowss" (prompt-buffer *prompt*))
        "Down after editing must not replace the edited buffer with the original")
    (prompt-history-prev)
    (is (string= "list-windows" (prompt-buffer *prompt*))
        "Up after editing starts a fresh history walk")))
