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

;;; NOTE on declarations in test bodies:
;;; CL DECLARE forms MUST appear before any executable forms in a body.
;;; Tests that do not use get-buf rely on IGNORABLE declared by the macro above.
;;; No extra (declare ...) is needed or allowed after executable forms.

(test vi-normal-key-h-moves-cursor-left
  "%handle-vi-normal-key with h (104) moves the prompt cursor one step left."
  (with-vi-normal-prompt (get-buf)
    ;; get-buf is ignorable (declared by with-vi-normal-prompt).
    (prompt-cursor-eol)
    (let ((pos-before (prompt-cursor-index *prompt*)))
      (is-true (cl-tmux::%handle-vi-normal-key 104) "h must return T")
      (is (= (1- pos-before) (prompt-cursor-index *prompt*))
          "h must move cursor one position left"))))

(test vi-normal-key-l-moves-cursor-right
  "%handle-vi-normal-key with l (108) moves the prompt cursor one step right."
  (with-vi-normal-prompt (get-buf)
    ;; get-buf is ignorable (declared by with-vi-normal-prompt).
    (prompt-cursor-bol)
    (let ((pos-before (prompt-cursor-index *prompt*)))
      (is-true (cl-tmux::%handle-vi-normal-key 108) "l must return T")
      (is (= (1+ pos-before) (prompt-cursor-index *prompt*))
          "l must move cursor one position right"))))

(test vi-normal-key-0-moves-to-bol
  "%handle-vi-normal-key with 0 (48) moves the cursor to beginning of line."
  (with-vi-normal-prompt (get-buf)
    ;; get-buf is ignorable (declared by with-vi-normal-prompt).
    (prompt-cursor-eol)
    (is-true (cl-tmux::%handle-vi-normal-key 48) "0 must return T")
    (is (= 0 (prompt-cursor-index *prompt*)) "0 must move cursor to BOL")))

(test vi-normal-key-caret-moves-to-bol
  "%handle-vi-normal-key with ^ (94) moves the cursor to beginning of line."
  (with-vi-normal-prompt (get-buf)
    ;; get-buf is ignorable (declared by with-vi-normal-prompt).
    (prompt-cursor-eol)
    (is-true (cl-tmux::%handle-vi-normal-key 94) "^ must return T")
    (is (= 0 (prompt-cursor-index *prompt*)) "^ must move cursor to BOL")))

(test vi-normal-key-dollar-moves-to-eol
  "%handle-vi-normal-key with $ (36) moves the cursor to end of line."
  (with-vi-normal-prompt (get-buf)
    ;; get-buf is ignorable (declared by with-vi-normal-prompt).
    (prompt-cursor-bol)
    (is-true (cl-tmux::%handle-vi-normal-key 36) "$ must return T")
    (is (= 5 (prompt-cursor-index *prompt*)) "$ must move cursor to EOL")))

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

(test vi-normal-key-i-enters-insert-mode
  "%handle-vi-normal-key with i (105) clears vi-normal-p (enters insert mode)."
  (with-vi-normal-prompt (get-buf)
    ;; get-buf is ignorable (declared by with-vi-normal-prompt).
    (is-true (cl-tmux::%handle-vi-normal-key 105) "i must return T")
    (is-false (prompt-vi-normal-p *prompt*)
              "i must clear vi-normal-p")))

(test vi-normal-key-a-appends-and-enters-insert-mode
  "%handle-vi-normal-key with a (97) moves right and enters insert mode."
  (with-vi-normal-prompt (get-buf)
    ;; get-buf is ignorable (declared by with-vi-normal-prompt).
    (prompt-cursor-bol)
    (let ((pos-before (prompt-cursor-index *prompt*)))
      (is-true (cl-tmux::%handle-vi-normal-key 97) "a must return T")
      (is (= (1+ pos-before) (prompt-cursor-index *prompt*))
          "a must move cursor one position right")
      (is-false (prompt-vi-normal-p *prompt*)
                "a must clear vi-normal-p (insert mode)"))))

(test vi-normal-key-capital-a-appends-at-eol
  "%handle-vi-normal-key with A (65) moves to EOL and enters insert mode."
  (with-vi-normal-prompt (get-buf)
    ;; get-buf is ignorable (declared by with-vi-normal-prompt).
    (prompt-cursor-bol)
    (is-true (cl-tmux::%handle-vi-normal-key 65) "A must return T")
    (is (= 5 (prompt-cursor-index *prompt*)) "A must move cursor to EOL")
    (is-false (prompt-vi-normal-p *prompt*)
              "A must clear vi-normal-p (insert mode)")))

(test vi-normal-key-capital-i-inserts-at-bol
  "%handle-vi-normal-key with I (73) moves to BOL and enters insert mode."
  (with-vi-normal-prompt (get-buf)
    ;; get-buf is ignorable (declared by with-vi-normal-prompt).
    (prompt-cursor-eol)
    (is-true (cl-tmux::%handle-vi-normal-key 73) "I must return T")
    (is (= 0 (prompt-cursor-index *prompt*)) "I must move cursor to BOL")
    (is-false (prompt-vi-normal-p *prompt*)
              "I must clear vi-normal-p (insert mode)")))

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

(test dispatch-menu-key-j-sends-menu-next
  "%dispatch-menu-key with j (106) dispatches :menu-next."
  (with-fake-session (s)
    (with-dispatch-capture (dispatched)
      (let ((cl-tmux::*dirty* nil))
        (cl-tmux::%dispatch-menu-key s 106)
        (is (member :menu-next dispatched)
            "j must dispatch :menu-next")
        (is-true cl-tmux::*dirty* "j must mark the display dirty")))))

(test dispatch-menu-key-k-sends-menu-prev
  "%dispatch-menu-key with k (107) dispatches :menu-prev."
  (with-fake-session (s)
    (with-dispatch-capture (dispatched)
      (let ((cl-tmux::*dirty* nil))
        (cl-tmux::%dispatch-menu-key s 107)
        (is (member :menu-prev dispatched)
            "k must dispatch :menu-prev")
        (is-true cl-tmux::*dirty* "k must mark the display dirty")))))

(test dispatch-menu-key-enter-sends-menu-select
  "%dispatch-menu-key with Enter (13) dispatches :menu-select."
  (with-fake-session (s)
    (with-dispatch-capture (dispatched)
      (let ((cl-tmux::*dirty* nil))
        (cl-tmux::%dispatch-menu-key s 13)
        (is (member :menu-select dispatched)
            "Enter must dispatch :menu-select")
        (is-true cl-tmux::*dirty* "Enter must mark the display dirty")))))

(test dispatch-menu-key-q-sends-menu-dismiss
  "%dispatch-menu-key with q (113) dispatches :menu-dismiss."
  (with-fake-session (s)
    (with-dispatch-capture (dispatched)
      (let ((cl-tmux::*dirty* nil))
        (cl-tmux::%dispatch-menu-key s 113)
        (is (member :menu-dismiss dispatched)
            "q must dispatch :menu-dismiss")
        (is-true cl-tmux::*dirty* "q must mark the display dirty")))))

(test dispatch-menu-key-esc-sends-menu-dismiss
  "%dispatch-menu-key with ESC (27) dispatches :menu-dismiss."
  (with-fake-session (s)
    (with-dispatch-capture (dispatched)
      (let ((cl-tmux::*dirty* nil))
        (cl-tmux::%dispatch-menu-key s 27)
        (is (member :menu-dismiss dispatched)
            "ESC must dispatch :menu-dismiss")))))

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

(test dispatch-menu-key-digit-in-range-jumps-to-index
  "A digit byte within range sets menu-selected-index to that digit and
   triggers a refresh via :menu-next then :menu-prev."
  (with-fake-session (s)
    (let ((cl-tmux/prompt:*active-menu*
            (cl-tmux/prompt:make-menu
             :items '(("one" . :a) ("two" . :b) ("three" . :c)))))
      (with-dispatch-capture (dispatched)
        (let ((cl-tmux::*dirty* nil))
          (cl-tmux::%dispatch-menu-key s (+ (char-code #\0) 2))
          (is (= 2 (cl-tmux/prompt:menu-selected-index cl-tmux/prompt:*active-menu*))
              "digit '2' must jump menu-selected-index to 2")
          (is (equal (list :menu-prev :menu-next) dispatched)
              "digit jump must dispatch :menu-next then :menu-prev to refresh")
          (is-true cl-tmux::*dirty* "digit jump must mark the display dirty"))))))

(test dispatch-menu-key-digit-out-of-range-is-noop
  "A digit byte >= the item count leaves menu-selected-index and the dispatch
   log untouched (no refresh is triggered for an invalid index)."
  (with-fake-session (s)
    (let ((cl-tmux/prompt:*active-menu*
            (cl-tmux/prompt:make-menu :items '(("one" . :a)) :selected-index 0)))
      (with-dispatch-capture (dispatched)
        (cl-tmux::%dispatch-menu-key s (+ (char-code #\0) 5))
        (is (zerop (cl-tmux/prompt:menu-selected-index cl-tmux/prompt:*active-menu*))
            "out-of-range digit must not change menu-selected-index")
        (is (null dispatched)
            "out-of-range digit must not dispatch any menu command")))))

;;; ── define-prompt-vi-key-rules macro smoke test ─────────────────────────────

(test define-prompt-vi-key-rules-is-defined
  "define-prompt-vi-key-rules must be a defined macro and %handle-vi-normal-key
   must be a defined function (verifies the macro fired at load time)."
  (is (macro-function 'cl-tmux::define-prompt-vi-key-rules)
      "define-prompt-vi-key-rules must be defined as a macro")
  (is (fboundp 'cl-tmux::%handle-vi-normal-key)
      "%handle-vi-normal-key must be defined as a function"))

;;; ── %rename-from-osc-title and %rename-from-format-string ───────────────────

(test rename-from-osc-title-returns-title-when-allow-title
  "%rename-from-osc-title returns the non-empty screen title when ALLOW-TITLE is T."
  (with-screen (sc 20 5)
    (setf (screen-title sc) "my-title")
    (is (string= "my-title" (cl-tmux::%rename-from-osc-title sc t))
        "%rename-from-osc-title must return the screen title when allow-title is T")))

(test rename-from-osc-title-returns-empty-when-not-allow-title
  "%rename-from-osc-title returns empty string when ALLOW-TITLE is NIL."
  (with-screen (sc 20 5)
    (setf (screen-title sc) "my-title")
    (is (string= "" (cl-tmux::%rename-from-osc-title sc nil))
        "%rename-from-osc-title must return \"\" when allow-title is NIL")))

(test rename-from-osc-title-returns-empty-when-title-is-empty
  "%rename-from-osc-title returns empty string when the screen title is empty,
   even when ALLOW-TITLE is T."
  (with-screen (sc 20 5)
    (setf (screen-title sc) "")
    (is (string= "" (cl-tmux::%rename-from-osc-title sc t))
        "%rename-from-osc-title must return \"\" when the title is empty")))

(test auto-rename-name-uses-osc-title-for-process-less-pane
  "%auto-rename-name returns the OSC title for a pane with no real process (pid <= 0)."
  (with-auto-rename-session (screen pane win sess :win-name "old")
    ;; pane-pid is <= 0 by default in with-auto-rename-session.
    (setf (screen-title screen) "osc-title")
    (is (string= "osc-title"
                 (cl-tmux::%auto-rename-name sess win pane screen :allow-title t))
        "%auto-rename-name must use OSC title for process-less pane")))

;;; ── backspace server option (DEL translation) ────────────────────────────────

(test backspace-option-byte-table
  "%backspace-option-byte parses tmux key syntax.
   Each row: (option-value expected description)."
  (dolist (row '(("C-?" 127 "C-? is DEL (the identity default)")
                 ("C-h" 8   "C-h is BS")
                 ("C-H" 8   "C-H (uppercase) is BS")
                 ("x"   120 "a single character is its own code")
                 ("bogus-value" nil "unrecognised values yield NIL")))
    (destructuring-bind (value expected desc) row
      (with-isolated-config
        (cl-tmux/options:set-server-option "backspace" value)
        (is (eql expected (cl-tmux::%backspace-option-byte)) "~A" desc)))))

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
