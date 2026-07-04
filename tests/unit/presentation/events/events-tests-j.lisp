(in-package #:cl-tmux/test)

;;;; Events tests — part J: vi-normal-key dispatch, %dispatch-menu-key,
;;;; define-prompt-vi-key-rules coverage, %rename-from-osc-title / %rename-from-format-string.

(in-suite events-suite)

;;; ── %handle-vi-normal-key: vi normal mode navigation ────────────────────────
;;;
;;; These tests cover every CASE arm of the generated %handle-vi-normal-key function.
;;; All tests set prompt-vi-normal-p = T via (setf (prompt-vi-normal-p *prompt*) t)
;;; to simulate ESC having been pressed in a vi-mode prompt.

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
             collect `(test ,name
                        ,doc
                        (with-vi-normal-prompt (get-buf)
                          ,setup
                          (let ((pos-before (prompt-cursor-index *prompt*)))
                            (is-true (cl-tmux::%handle-vi-normal-key ,byte)
                                     ,return-message)
                            (is (= ,expected (prompt-cursor-index *prompt*))
                                ,assertion-message)))))))

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

(test vi-normal-key-x-deletes-char-under-cursor
  "%handle-vi-normal-key with x (120) deletes the character under the cursor."
  (with-vi-normal-prompt (get-buf)
    (prompt-cursor-bol)
    (is-true (cl-tmux::%handle-vi-normal-key 120) "x must return T")
    (is (string= "ello" (funcall get-buf))
        "x must delete the character at cursor position 0")))

(test vi-normal-key-capital-d-kills-to-end
  "%handle-vi-normal-key with D (68) kills from cursor to end of line."
  (with-vi-normal-prompt (get-buf)
    ;; Position at index 2 (between 'e' and 'l').
    (prompt-cursor-bol)
    (cl-tmux::handle-prompt-key 6)   ; C-f → index 1
    (cl-tmux::handle-prompt-key 6)   ; C-f → index 2
    (setf (prompt-vi-normal-p *prompt*) t) ; re-enable; handle-prompt-key may exit
    (is-true (cl-tmux::%handle-vi-normal-key 68) "D must return T")
    (is (string= "he" (funcall get-buf))
        "D must kill from cursor to end of line")))

(defmacro define-vi-normal-insert-mode-cases (&body cases)
  "Define %HANDLE-VI-NORMAL-KEY tests that switch from vi-normal to insert mode."
  `(progn
     ,@(loop for (name doc byte setup expected-cursor return-message cursor-message insert-message)
               in cases
             collect `(test ,name
                        ,doc
                        (with-vi-normal-prompt (get-buf)
                          ,setup
                          (let ((pos-before (prompt-cursor-index *prompt*)))
                            (is-true (cl-tmux::%handle-vi-normal-key ,byte)
                                     ,return-message)
                            ,@(when expected-cursor
                                `((is (= ,expected-cursor (prompt-cursor-index *prompt*))
                                      ,cursor-message)))
                            (is-false (prompt-vi-normal-p *prompt*)
                                      ,insert-message)))))))

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

(test vi-normal-key-enter-submits-prompt
  "%handle-vi-normal-key with Enter (13) runs on-submit and clears the prompt."
  (with-clean-prompt
    (let ((submitted nil))
      (prompt-start "test" "hello" (lambda (s) (setf submitted s)))
      (setf (prompt-vi-normal-p *prompt*) t)
      (is-true (cl-tmux::%handle-vi-normal-key 13) "Enter must return T")
      (is (string= "hello" submitted) "Enter must call on-submit with the buffer")
      (is-false (prompt-active-p) "Enter must dismiss the prompt"))))

(test vi-normal-key-esc-cancels-prompt
  "%handle-vi-normal-key with ESC (27) cancels the prompt."
  (with-clean-prompt
    (prompt-start "test" "hello" (lambda (s) (declare (ignore s)) nil))
    (setf (prompt-vi-normal-p *prompt*) t)
    (is-true (cl-tmux::%handle-vi-normal-key 27) "ESC must return T")
    (is-false (prompt-active-p) "ESC must dismiss the prompt")))

(test vi-normal-key-ctrl-c-cancels-prompt
  "%handle-vi-normal-key with C-c (3) cancels the prompt."
  (with-clean-prompt
    (prompt-start "test" "hello" (lambda (s) (declare (ignore s)) nil))
    (setf (prompt-vi-normal-p *prompt*) t)
    (is-true (cl-tmux::%handle-vi-normal-key 3) "C-c must return T")
    (is-false (prompt-active-p) "C-c must dismiss the prompt")))

(test vi-normal-key-unhandled-returns-nil
  "%handle-vi-normal-key with an unrecognised byte returns NIL (fall through)."
  (with-vi-normal-prompt (get-buf)
    ;; get-buf is ignorable (declared by with-vi-normal-prompt).
    ;; Byte 33 = '!' — not a vi navigation key.
    (is-false (cl-tmux::%handle-vi-normal-key 33)
              "unhandled key must return NIL (fall through to insert mode)")))

(test vi-normal-key-noop-when-prompt-absent
  "%handle-vi-normal-key is a no-op (returns NIL) when no prompt is active."
  (with-clean-prompt
    ;; *prompt* is NIL; the function should short-circuit immediately.
    (is-false (cl-tmux::%handle-vi-normal-key 104)
              "h with no prompt must return NIL")))

(test vi-normal-key-noop-when-not-in-vi-normal-mode
  "%handle-vi-normal-key is a no-op when the prompt is in insert (not vi-normal) mode."
  (with-clean-prompt
    (prompt-start "test" "hello" (lambda (s) (declare (ignore s)) nil))
    ;; vi-normal-p is NIL by default.
    (is-false (prompt-vi-normal-p *prompt*) "sanity: prompt starts in insert mode")
    (is-false (cl-tmux::%handle-vi-normal-key 104)
              "h in insert mode must return NIL (not consumed)")))

;;; ── %dispatch-menu-key ───────────────────────────────────────────────────────
;;;
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
     ,@(loop for (name doc byte expected-command dispatch-message dirty-message) in cases
             collect `(test ,name
                        ,doc
                        (with-fake-session (s)
                          (with-dispatch-capture (dispatched)
                            (let ((cl-tmux::*dirty* nil))
                              (cl-tmux::%dispatch-menu-key s ,byte)
                              (is (member ,expected-command dispatched)
                                  ,dispatch-message)
                              ,@(when dirty-message
                                  `((is-true cl-tmux::*dirty*
                                             ,dirty-message))))))))))

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

(test dispatch-menu-key-returns-nil
  "%dispatch-menu-key always returns NIL (caller stays in ground state)."
  (with-fake-session (s)
    (with-dispatch-capture (dispatched)
      (is-false (cl-tmux::%dispatch-menu-key s 106)
                "%dispatch-menu-key must return NIL for j")
      (is-false (cl-tmux::%dispatch-menu-key s 13)
                "%dispatch-menu-key must return NIL for Enter")
      ;; dispatched accumulates cmds from the capture lambda; verify it's a list
      (is-true (listp dispatched) "capture list must be a proper list"))))

;;; ── %dispatch-menu-key: digit 0-9 jump-to-item branch ────────────────────────
;;;
;;; A digit byte in range jumps *active-menu*'s selected-index directly (rather
;;; than dispatching :menu-next/-prev), then dispatches :menu-next/:menu-prev
;;; with a 0 net delta purely to trigger the overlay refresh.  Out-of-range
;;; digits (>= the item count) are a no-op: the index and dispatch log are
;;; both untouched.

(defmacro define-dispatch-menu-digit-cases (&body cases)
  "Define numeric %DISPATCH-MENU-KEY tests from declarative rows."
  `(progn
     ,@(loop for (name doc items selected-index digit expected-index
                       expected-dispatched dirty-message index-message
                       dispatch-message)
               in cases
             collect `(test ,name
                        ,doc
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
                                (is (= ,expected-index
                                       (cl-tmux/prompt:menu-selected-index
                                        cl-tmux/prompt:*active-menu*))
                                    ,index-message)
                                (is (equal ,expected-dispatched dispatched)
                                    ,dispatch-message)
                                ,@(when dirty-message
                                    `((is-true cl-tmux::*dirty*
                                               ,dirty-message)))))))))))

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
   "out-of-range digit must not dispatch any menu command"))

;;; ── define-prompt-vi-key-rules macro smoke test ─────────────────────────────

(test define-prompt-vi-key-rules-is-defined
  "define-prompt-vi-key-rules must be a defined macro and %handle-vi-normal-key
   must be a defined function (verifies the macro fired at load time)."
  (is (macro-function 'cl-tmux::define-prompt-vi-key-rules)
      "define-prompt-vi-key-rules must be defined as a macro")
  (is (fboundp 'cl-tmux::%handle-vi-normal-key)
      "%handle-vi-normal-key must be defined as a function"))

;;; ── %rename-from-osc-title and %rename-from-format-string ───────────────────

(defmacro define-rename-from-osc-title-cases (&body cases)
  "Define %RENAME-FROM-OSC-TITLE tests from declarative rows."
  `(progn
     ,@(loop for (name doc title allow-title expected assertion-message) in cases
             collect `(test ,name
                        ,doc
                        (with-screen (sc 20 5)
                          (setf (screen-title sc) ,title)
                          (is (string= ,expected
                                       (cl-tmux::%rename-from-osc-title sc
                                                                        ,allow-title))
                              ,assertion-message))))))

(define-rename-from-osc-title-cases
  (rename-from-osc-title-returns-title-when-allow-title
   "%rename-from-osc-title returns the non-empty screen title when ALLOW-TITLE is T."
   "my-title" t "my-title"
   "%rename-from-osc-title must return the screen title when allow-title is T")
  (rename-from-osc-title-returns-empty-when-not-allow-title
   "%rename-from-osc-title returns empty string when ALLOW-TITLE is NIL."
   "my-title" nil ""
   "%rename-from-osc-title must return \"\" when allow-title is NIL")
  (rename-from-osc-title-returns-empty-when-title-is-empty
   "%rename-from-osc-title returns empty string when the screen title is empty,
   even when ALLOW-TITLE is T."
   "" t ""
   "%rename-from-osc-title must return \"\" when the title is empty"))

(test auto-rename-name-uses-osc-title-for-process-less-pane
  "%auto-rename-name returns the OSC title for a pane with no real process (pid <= 0)."
  (with-auto-rename-session (screen pane win sess :win-name "old")
    ;; pane-pid is <= 0 by default in with-auto-rename-session.
    (setf (screen-title screen) "osc-title")
    (is (string= "osc-title"
                 (cl-tmux::%auto-rename-name sess win pane screen :allow-title t))
        "%auto-rename-name must use OSC title for process-less pane")))

;;; ── backspace server option (DEL translation) ────────────────────────────────

(defmacro define-backspace-option-byte-cases (&body cases)
  "Define %BACKSPACE-OPTION-BYTE parsing tests from declarative rows."
  `(progn
     ,@(loop for (name doc option-value expected assertion-message) in cases
             collect `(test ,name
                        ,doc
                        (with-isolated-config
                          (cl-tmux/options:set-server-option "backspace"
                                                             ,option-value)
                          (is (eql ,expected
                                   (cl-tmux::%backspace-option-byte))
                              ,assertion-message))))))

(define-backspace-option-byte-cases
  (backspace-option-byte-c-question-is-del
   "%backspace-option-byte parses C-? as DEL."
   "C-?" 127
   "C-? is DEL (the identity default)")
  (backspace-option-byte-c-h-is-bs
   "%backspace-option-byte parses C-h as BS."
   "C-h" 8
   "C-h is BS")
  (backspace-option-byte-c-uppercase-h-is-bs
   "%backspace-option-byte parses C-H as BS."
   "C-H" 8
   "C-H (uppercase) is BS")
  (backspace-option-byte-single-character-is-own-code
   "%backspace-option-byte parses a single character as its character code."
   "x" 120
   "a single character is its own code")
  (backspace-option-byte-bogus-value-is-nil
   "%backspace-option-byte returns NIL for unrecognised values."
   "bogus-value" nil
   "unrecognised values yield NIL"))

(test translate-backspace-octets-rewrites-del
  "%translate-backspace-octets rewrites DEL per the backspace option and is the
   identity for the default C-?."
  (with-isolated-config
    (let ((octets (coerce #(97 127 98) '(vector (unsigned-byte 8)))))
      (cl-tmux/options:set-server-option "backspace" "C-h")
      (is (equalp #(97 8 98) (cl-tmux::%translate-backspace-octets octets))
          "backspace C-h must rewrite DEL (127) to BS (8)")
      (cl-tmux/options:set-server-option "backspace" "C-?")
      (is (eq octets (cl-tmux::%translate-backspace-octets octets))
          "the default C-? must return the input unchanged (identity)"))))

;;; ── assume-paste-time (tmux server_client_assume_paste) ──────────────────────

(test assume-paste-byte-p-table
  "%assume-paste-byte-p: NIL with no key history; T within the window after a
   forwarded key; NIL when assume-paste-time is 0."
  (with-isolated-config
    (let ((cl-tmux::*last-ground-key-time* nil))
      (is (null (cl-tmux::%assume-paste-byte-p))
          "no previous key must never assume a paste")
      (cl-tmux/options:set-option "assume-paste-time" 1000) ; generous 1s window
      (cl-tmux::%stamp-ground-key-time)
      (is (eq t (and (cl-tmux::%assume-paste-byte-p) t))
          "a key right after a forwarded key must be assumed pasted")
      (cl-tmux/options:set-option "assume-paste-time" 0)
      (is (null (cl-tmux::%assume-paste-byte-p))
          "assume-paste-time 0 must disable the heuristic"))))

(test assume-paste-time-bypasses-root-binding-during-burst
  "A root -n bound key arriving within assume-paste-time of pane content is
   forwarded to the pane instead of running the binding (tmux paste protection);
   with assume-paste-time 0 the binding runs."
  (dolist (row '((1000 nil "fast key during a burst must NOT run the binding")
                 (0    t   "assume-paste-time 0 must run the binding")))
    (destructuring-bind (paste-ms expect-switch desc) row
      (with-isolated-config
        (with-fake-session (s :nwindows 2)
          (cl-tmux/options:set-option "assume-paste-time" paste-ms)
          (cl-tmux/config:key-table-bind "root" #\x :next-window)
          (let ((first-win (cl-tmux/model:session-active-window s))
                (state (cl-tmux::make-input-state)))
            ;; Plain content byte: forwarded, stamps the burst clock.
            (cl-tmux::process-byte s (char-code #\a) state)
            ;; Bound key arrives "immediately" (microseconds later).
            (cl-tmux::process-byte s (char-code #\x) state)
            (if expect-switch
                (is (not (eq first-win (cl-tmux/model:session-active-window s)))
                    "~A" desc)
                (is (eq first-win (cl-tmux/model:session-active-window s))
                    "~A" desc))))))))
