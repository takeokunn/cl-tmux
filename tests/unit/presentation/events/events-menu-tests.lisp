(in-package #:cl-tmux/test)

;;;; Events tests: menu key dispatch.

;;; Verify that %dispatch-menu-key routes each key class to the correct dispatch
;;; command.  We capture the dispatched command keyword by wrapping dispatch-command.

(defmacro with-dispatch-capture ((captured-var) &body body)
  "Run BODY with DISPATCH-COMMAND replaced by a version that conses onto
   CAPTURED-VAR instead of actually dispatching.  Restores the original
   definition via unwind-protect."
  `(let ((,captured-var nil)
         (orig (fdefinition 'cl-tmux::dispatch-command)))
     (unwind-protect
          (progn
            (setf (fdefinition 'cl-tmux::dispatch-command)
                  (lambda (session cmd arg)
                    (declare (ignore session arg))
                    (push cmd ,captured-var)))
            ,@body)
       (setf (fdefinition 'cl-tmux::dispatch-command) orig))))

(defmacro define-dispatch-menu-key-cases (&body cases)
  "Define menu-key dispatch routing tests from declarative rows."
  `(progn
     ,@(loop for case in cases
             for name = (first case)
             for byte = (third case)
             for expected-command = (fourth case)
             for dirty-message = (sixth case)
             collect `(it ,(string-downcase (symbol-name name))
                        (with-fake-session (s)
                          (with-dispatch-capture (dispatched)
                            (let ((cl-tmux::*dirty* nil))
                              (cl-tmux::%dispatch-menu-key s ,byte)
                              (expect (member ,expected-command dispatched))
                              ,@(when dirty-message
                                  `((expect cl-tmux::*dirty* :to-be-truthy))))))))))

(defmacro define-dispatch-menu-digit-cases (&body cases)
  "Define numeric %DISPATCH-MENU-KEY tests from declarative rows."
  `(progn
     ,@(loop for case in cases
             for name = (first case)
             for items = (third case)
             for selected-index = (fourth case)
             for digit = (fifth case)
             for expected-index = (sixth case)
             for expected-dispatched = (seventh case)
             for dirty-message = (eighth case)
             collect `(it ,(string-downcase (symbol-name name))
                        (with-fake-session (s)
                          (let ((cl-tmux/prompt:*active-menu*
                                  (cl-tmux/prompt:make-menu
                                   :items ,items
                                   :selected-index ,selected-index)))
                            (with-dispatch-capture (dispatched)
                              (let ((cl-tmux::*dirty* nil))
                                (cl-tmux::%dispatch-menu-key
                                 s
                                 (+ (char-code #\0) ,digit))
                                (expect (= ,expected-index
                                          (cl-tmux/prompt:menu-selected-index
                                           cl-tmux/prompt:*active-menu*)))
                                (expect (equal ,expected-dispatched dispatched))
                                ,@(when dirty-message
                                    `((expect cl-tmux::*dirty* :to-be-truthy)))))))))))

(describe "events-suite"

  (define-dispatch-menu-key-cases
    (dispatch-menu-key-j-sends-menu-next
     "%dispatch-menu-key with j (106) dispatches :menu-next."
     106 :menu-next
     "j must dispatch :menu-next"
     "j must mark the display dirty")
    (dispatch-menu-key-k-sends-menu-prev
     "%dispatch-menu-key with k (107) dispatches :menu-prev."
     107 :menu-prev
     "k must dispatch :menu-prev"
     "k must mark the display dirty")
    (dispatch-menu-key-enter-sends-menu-select
     "%dispatch-menu-key with Enter (13) dispatches :menu-select."
     13 :menu-select
     "Enter must dispatch :menu-select"
     "Enter must mark the display dirty")
    (dispatch-menu-key-q-sends-menu-dismiss
     "%dispatch-menu-key with q (113) dispatches :menu-dismiss."
     113 :menu-dismiss
     "q must dispatch :menu-dismiss"
     "q must mark the display dirty")
    (dispatch-menu-key-esc-sends-menu-dismiss
     "%dispatch-menu-key with ESC (27) dispatches :menu-dismiss."
     27 :menu-dismiss
     "ESC must dispatch :menu-dismiss"
     nil))

  ;; %dispatch-menu-key always returns NIL (caller stays in ground state).
  (it "dispatch-menu-key-returns-nil"
    (with-fake-session (s)
      (with-dispatch-capture (dispatched)
        (expect (cl-tmux::%dispatch-menu-key s 106) :to-be-falsy)
        (expect (cl-tmux::%dispatch-menu-key s 13) :to-be-falsy)
        ;; dispatched accumulates cmds from the capture lambda; verify it's a list
        (expect (listp dispatched) :to-be-truthy))))

  ;; A digit byte in range jumps *active-menu*'s selected-index directly (rather
  ;; than dispatching :menu-next/-prev), then dispatches :menu-next/:menu-prev
  ;; with a 0 net delta purely to trigger the overlay refresh.  Out-of-range
  ;; digits (>= the item count) are a no-op: the index and dispatch log are
  ;; both untouched.

  (define-dispatch-menu-digit-cases
    (dispatch-menu-key-digit-in-range-jumps-to-index
     "A digit byte within range sets menu-selected-index to that digit and
     triggers a refresh via :menu-next then :menu-prev."
     '(("one" . :a) ("two" . :b) ("three" . :c))
     0
     2
     2
     (list :menu-prev :menu-next)
     "digit jump must mark the display dirty"
     "digit '2' must jump menu-selected-index to 2"
     "digit jump must dispatch :menu-next then :menu-prev to refresh")
    (dispatch-menu-key-digit-out-of-range-is-noop
     "A digit byte >= the item count leaves menu-selected-index and the dispatch
     log untouched (no refresh is triggered for an invalid index)."
     '(("one" . :a))
     0
     5
     0
     nil
     nil
     "out-of-range digit must not change menu-selected-index"
     "out-of-range digit must not dispatch any menu command")))
