(in-package #:cl-tmux/test)

;;;; Events tests: vi-normal-key dispatch.

(defmacro with-vi-normal-prompt ((buf-var) &body body)
  "Run BODY with a vi-normal-mode prompt seeded with \"hello\".
   BUF-VAR is bound to a function of zero arguments that returns the current
   prompt-buffer string at the time it is called.
   BUF-VAR is declared IGNORABLE so tests that do not call funcall on it
   do not trigger a compiler warning."
  (let ((fn-sym (gensym "PROBE")))
    `(with-clean-prompt
       (prompt-start "test" "hello" (lambda (s) (declare (ignore s)) nil))
       (setf (prompt-vi-normal-p *prompt*) t)
       (let ((,fn-sym (lambda () (prompt-buffer *prompt*))))
         (let ((,buf-var ,fn-sym))
           (declare (ignorable ,buf-var))
           ,@body)))))

(defmacro define-vi-normal-cursor-cases (&body cases)
  "Define cursor-only %HANDLE-VI-NORMAL-KEY tests from declarative rows."
  `(progn
     ,@(loop for (name doc byte setup expected return-message assertion-message) in cases
             collect (progn
                       (list doc return-message assertion-message) ; unused in the it body
                       `(it ,(string-downcase (symbol-name name))
                          (with-vi-normal-prompt (get-buf)
                            ,setup
                            (let ((pos-before (prompt-cursor-index *prompt*)))
                              (expect (cl-tmux::%handle-vi-normal-key ,byte) :to-be-truthy)
                              (expect (= ,expected (prompt-cursor-index *prompt*))))))))))

(defmacro define-vi-normal-insert-mode-cases (&body cases)
  "Define %HANDLE-VI-NORMAL-KEY tests that switch from vi-normal to insert mode."
  `(progn
     ,@(loop for (name doc byte setup expected-cursor return-message cursor-message insert-message)
               in cases
             collect (progn
                       (list doc return-message cursor-message insert-message) ; unused in the it body
                       `(it ,(string-downcase (symbol-name name))
                          (with-vi-normal-prompt (get-buf)
                            ,setup
                            (let ((pos-before (prompt-cursor-index *prompt*)))
                              (expect (cl-tmux::%handle-vi-normal-key ,byte) :to-be-truthy)
                              ,@(when expected-cursor
                                  `((expect (= ,expected-cursor (prompt-cursor-index *prompt*)))))
                              (expect (prompt-vi-normal-p *prompt*) :to-be-falsy))))))))

(describe "events-suite"

  ;;; ── %handle-vi-normal-key: vi normal mode navigation ────────────────────────
  ;;;
  ;;; These tests cover every CASE arm of the generated %handle-vi-normal-key function.
  ;;; All tests set prompt-vi-normal-p = T via (setf (prompt-vi-normal-p *prompt*) t)
  ;;; to simulate ESC having been pressed in a vi-mode prompt.

  (define-vi-normal-cursor-cases
    (vi-normal-key-h-moves-cursor-left
     "%handle-vi-normal-key with h (104) moves the prompt cursor one step left."
     104 (prompt-cursor-eol) (1- pos-before)
     "h must return T"
     "h must move cursor one position left")
    (vi-normal-key-l-moves-cursor-right
     "%handle-vi-normal-key with l (108) moves the prompt cursor one step right."
     108 (prompt-cursor-bol) (1+ pos-before)
     "l must return T"
     "l must move cursor one position right")
    (vi-normal-key-0-moves-to-bol
     "%handle-vi-normal-key with 0 (48) moves the cursor to beginning of line."
     48 (prompt-cursor-eol) 0
     "0 must return T"
     "0 must move cursor to BOL")
    (vi-normal-key-caret-moves-to-bol
     "%handle-vi-normal-key with ^ (94) moves the cursor to beginning of line."
     94 (prompt-cursor-eol) 0
     "^ must return T"
     "^ must move cursor to BOL")
    (vi-normal-key-dollar-moves-to-eol
     "%handle-vi-normal-key with $ (36) moves the cursor to end of line."
     36 (prompt-cursor-bol) 5
     "$ must return T"
     "$ must move cursor to EOL"))

  ;; %handle-vi-normal-key with x (120) deletes the character under the cursor.
  (it "vi-normal-key-x-deletes-char-under-cursor"
    (with-vi-normal-prompt (get-buf)
      (prompt-cursor-bol)
      (expect (cl-tmux::%handle-vi-normal-key 120) :to-be-truthy)
      (expect (string= "ello" (funcall get-buf)))))

  ;; %handle-vi-normal-key with D (68) kills from cursor to end of line.
  (it "vi-normal-key-capital-d-kills-to-end"
    (with-vi-normal-prompt (get-buf)
      ;; Position at index 2 (between 'e' and 'l').
      (prompt-cursor-bol)
      (cl-tmux::handle-prompt-key 6)   ; C-f → index 1
      (cl-tmux::handle-prompt-key 6)   ; C-f → index 2
      (setf (prompt-vi-normal-p *prompt*) t) ; re-enable; handle-prompt-key may exit
      (expect (cl-tmux::%handle-vi-normal-key 68) :to-be-truthy)
      (expect (string= "he" (funcall get-buf)))))

  (define-vi-normal-insert-mode-cases
    (vi-normal-key-i-enters-insert-mode
     "%handle-vi-normal-key with i (105) clears vi-normal-p (enters insert mode)."
     105 (progn) nil
     "i must return T"
     nil
     "i must clear vi-normal-p")
    (vi-normal-key-a-appends-and-enters-insert-mode
     "%handle-vi-normal-key with a (97) moves right and enters insert mode."
     97 (prompt-cursor-bol) (1+ pos-before)
     "a must return T"
     "a must move cursor one position right"
     "a must clear vi-normal-p (insert mode)")
    (vi-normal-key-capital-a-appends-at-eol
     "%handle-vi-normal-key with A (65) moves to EOL and enters insert mode."
     65 (prompt-cursor-bol) 5
     "A must return T"
     "A must move cursor to EOL"
     "A must clear vi-normal-p (insert mode)")
    (vi-normal-key-capital-i-inserts-at-bol
     "%handle-vi-normal-key with I (73) moves to BOL and enters insert mode."
     73 (prompt-cursor-eol) 0
     "I must return T"
     "I must move cursor to BOL"
     "I must clear vi-normal-p (insert mode)"))

  ;; %handle-vi-normal-key with Enter (13) runs on-submit and clears the prompt.
  (it "vi-normal-key-enter-submits-prompt"
    (with-clean-prompt
      (let ((submitted nil))
        (prompt-start "test" "hello" (lambda (s) (setf submitted s)))
        (setf (prompt-vi-normal-p *prompt*) t)
        (expect (cl-tmux::%handle-vi-normal-key 13) :to-be-truthy)
        (expect (string= "hello" submitted))
        (expect (prompt-active-p) :to-be-falsy))))

  ;; %handle-vi-normal-key with ESC (27) cancels the prompt.
  (it "vi-normal-key-esc-cancels-prompt"
    (with-clean-prompt
      (prompt-start "test" "hello" (lambda (s) (declare (ignore s)) nil))
      (setf (prompt-vi-normal-p *prompt*) t)
      (expect (cl-tmux::%handle-vi-normal-key 27) :to-be-truthy)
      (expect (prompt-active-p) :to-be-falsy)))

  ;; %handle-vi-normal-key with C-c (3) cancels the prompt.
  (it "vi-normal-key-ctrl-c-cancels-prompt"
    (with-clean-prompt
      (prompt-start "test" "hello" (lambda (s) (declare (ignore s)) nil))
      (setf (prompt-vi-normal-p *prompt*) t)
      (expect (cl-tmux::%handle-vi-normal-key 3) :to-be-truthy)
      (expect (prompt-active-p) :to-be-falsy)))

  ;; %handle-vi-normal-key with an unrecognised byte returns NIL (fall through).
  (it "vi-normal-key-unhandled-returns-nil"
    (with-vi-normal-prompt (get-buf)
      ;; get-buf is ignorable (declared by with-vi-normal-prompt).
      ;; Byte 33 = '!' — not a vi navigation key.
      (expect (cl-tmux::%handle-vi-normal-key 33) :to-be-falsy)))

  ;; %handle-vi-normal-key is a no-op (returns NIL) when no prompt is active.
  (it "vi-normal-key-noop-when-prompt-absent"
    (with-clean-prompt
      ;; *prompt* is NIL; the function should short-circuit immediately.
      (expect (cl-tmux::%handle-vi-normal-key 104) :to-be-falsy)))

  ;; %handle-vi-normal-key is a no-op when the prompt is in insert (not vi-normal) mode.
  (it "vi-normal-key-noop-when-not-in-vi-normal-mode"
    (with-clean-prompt
      (prompt-start "test" "hello" (lambda (s) (declare (ignore s)) nil))
      ;; vi-normal-p is NIL by default.
      (expect (prompt-vi-normal-p *prompt*) :to-be-falsy)
      (expect (cl-tmux::%handle-vi-normal-key 104) :to-be-falsy)))

  ;;; ── define-prompt-vi-key-rules macro smoke test ─────────────────────────────

  ;; define-prompt-vi-key-rules must be a defined macro and %handle-vi-normal-key
  ;; must be a defined function (verifies the macro fired at load time).
  (it "define-prompt-vi-key-rules-is-defined"
    (expect (macro-function 'cl-tmux::define-prompt-vi-key-rules))
    (expect (fboundp 'cl-tmux::%handle-vi-normal-key))))
