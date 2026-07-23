(in-package #:cl-tmux/test)

;;;; Interactive single-line input-prompt tests (prompt.lisp).
;;;;
;;;; Overlay, popup, and menu tests live in overlay-tests.lisp. Editing,
;;;; cursor, kill/delete, and change-notification coverage lives in
;;;; prompt-editing-tests.lisp. Event-loop wiring and status-bar coverage live
;;;; in prompt-tests-wiring.lisp.
;;;; All prompt tests use with-clean-prompt (from helpers-input-fixtures.lisp) to guarantee
;;;; that *prompt* and cl-tmux::*dirty* are reset; the raw let form is never used.

;;; -- Shared test helpers -----------------------------------------------------
;;; make-rename-window and with-prompt-at (below) are shared helpers used by
;;; the sibling files listed above.

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

(describe "prompt-suite"

  ;;; -- Prompt struct constructors and predicates --------------------------------

  ;; make-prompt with no keyword arguments fills slots to documented defaults.
  (it "make-prompt-defaults"
    (let ((p (make-prompt)))
      (check-table (list (list (prompt-label p)        "" "default label is empty string")
                         (list (prompt-buffer p)       "" "default buffer is empty string")
                         (list (prompt-cursor-index p) 0  "default cursor-index is 0"))
                   :test #'equal)
      (expect (null (prompt-on-submit p)))))

  ;; make-prompt keyword arguments override all defaults.
  (it "make-prompt-keyword-args"
    (let ((fn (lambda (s) s)))
      (let ((p (make-prompt :label "lbl" :buffer "buf"
                            :cursor-index 3 :on-submit fn)))
        (expect (string= "lbl" (prompt-label p)))
        (expect (string= "buf" (prompt-buffer p)))
        (expect (= 3 (prompt-cursor-index p)))
        (expect (eq fn (prompt-on-submit p))))))

  ;; prompt-p returns T for a PROMPT and NIL for any other value.
  (it "prompt-p-recognises-prompt-struct"
    (let ((p (make-prompt)))
      (expect (prompt-p p))
      (dolist (val (list nil 42 ""))
        (expect (not (prompt-p val))))))

  ;;; -- Pure prompt state -------------------------------------------------------

  ;; With no active prompt, prompt-active-p and prompt-text are NIL.
  (it "prompt-inactive-by-default"
    (with-clean-prompt
      (expect (null (prompt-active-p)))
      (expect (null (prompt-text)))))

  ;; prompt-start seeds label/buffer/on-submit and activates the prompt.
  (it "prompt-start-activates"
    (with-clean-prompt
      (prompt-start "rename-window" "old" (make-noop-submit))
      (expect (prompt-active-p))
      (expect (string= "old" (prompt-buffer *prompt*)))
      (expect (string= "rename-window" (prompt-label *prompt*)))
      (expect (functionp (prompt-on-submit *prompt*)))
      (expect (string= "rename-window: old|" (prompt-text)))))

  ;; prompt-start sets cursor-index to the length of the initial buffer.
  (it "prompt-start-cursor-index-at-end"
    (with-noop-prompt ("hello")
      (expect (= 5 (prompt-cursor-index *prompt*)))))

  ;; prompt-start with an empty initial buffer places cursor at index 0.
  (it "prompt-start-empty-buffer-cursor-at-zero"
    (with-noop-prompt ("")
      (expect (= 0 (prompt-cursor-index *prompt*)))))

  ;; prompt-on-submit stores and returns the callback supplied to prompt-start.
  (it "prompt-on-submit-accessor"
    (with-clean-prompt
      (let ((cb (lambda (s) (format nil "got:~A" s))))
        (prompt-start "p" "text" cb)
        (expect (eq cb (prompt-on-submit *prompt*))))))

  ;; prompt-on-change and prompt-on-cancel return the exact callbacks passed to
  ;; prompt-start via :on-change and :on-cancel.
  (it "prompt-on-change-and-on-cancel-accessors"
    (with-clean-prompt
      (let ((change-cb (lambda (s) (declare (ignore s)) nil))
            (cancel-cb (lambda () nil)))
        (prompt-start "p" "text" (make-noop-submit)
                      :on-change change-cb :on-cancel cancel-cb)
        (expect (eq change-cb (prompt-on-change *prompt*)))
        (expect (eq cancel-cb (prompt-on-cancel *prompt*))))))

  ;; prompt-single-key reflects the :single-key argument passed to prompt-start.
  (it "prompt-single-key-accessor"
    (with-clean-prompt
      (prompt-start "p" "" (make-noop-submit) :single-key t)
      (expect (prompt-single-key *prompt*) :to-be-truthy))
    (with-clean-prompt
      (prompt-start "p" "" (make-noop-submit))
      (expect (null (prompt-single-key *prompt*)))))

  ;; prompt-vi-normal-p defaults to NIL on a fresh prompt and can be set with setf
  ;; (the events layer flips it when entering/leaving vi normal mode).
  (it "prompt-vi-normal-p-accessor-defaults-and-setf"
    (with-clean-prompt
      (prompt-start "p" "" (make-noop-submit))
      (expect (null (prompt-vi-normal-p *prompt*)))
      (setf (prompt-vi-normal-p *prompt*) t)
      (expect (prompt-vi-normal-p *prompt*) :to-be-truthy)))

  ;; with-active-prompt binds its variable to *prompt* and runs BODY only when a
  ;; prompt is active; it is a no-op (returns NIL) when *prompt* is NIL.
  (it "with-active-prompt-runs-body-only-when-active"
    (with-clean-prompt
      (expect (null (with-active-prompt (p) p :ran)))
      (prompt-start "p" "hi" (make-noop-submit))
      (expect (eq :ran (with-active-prompt (p) p :ran)))
      (expect (eq *prompt* (with-active-prompt (p) p)))))

  ;; History navigation walks newest-first entries and Down restores current input.
  (it "prompt-history-prev-next-restores-in-progress-input"
    (with-clean-prompt
      (prompt-start "p" "li" (make-noop-submit)
                    :history '("list-windows" "new-window"))
      (prompt-history-prev)
      (expect (string= "list-windows" (prompt-buffer *prompt*)))
      (prompt-history-prev)
      (expect (string= "new-window" (prompt-buffer *prompt*)))
      (prompt-history-next)
      (expect (string= "list-windows" (prompt-buffer *prompt*)))
      (prompt-history-next)
      (expect (string= "li" (prompt-buffer *prompt*)))
      (expect (= 2 (prompt-cursor-index *prompt*)))))

  ;; Editing a recalled history entry makes future Up navigation start from that edit.
  (it "prompt-history-edit-resets-navigation-base"
    (with-clean-prompt
      (prompt-start "p" "" (make-noop-submit)
                    :history '("list-windows" "new-window"))
      (prompt-history-prev)
      (prompt-input #\s)
      (expect (string= "list-windowss" (prompt-buffer *prompt*)))
      (prompt-history-next)
      (expect (string= "list-windowss" (prompt-buffer *prompt*)))
      (prompt-history-prev)
      (expect (string= "list-windows" (prompt-buffer *prompt*))))))
