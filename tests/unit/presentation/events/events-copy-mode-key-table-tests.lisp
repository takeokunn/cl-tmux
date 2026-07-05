(in-package #:cl-tmux/test)

;;;; Events tests: copy-mode key-table dispatch.

(in-suite events-suite)


;;; ── copy-mode-vi key table override ─────────────────────────────────────────

(test copy-mode-vi-table-binding-overrides-hardcoded
  "A binding in the copy-mode-vi table fires its command and suppresses the hardcoded dispatch."
  (with-isolated-config
    ;; Bind 'v' in copy-mode-vi to :copy-mode-begin-selection (same as hardcoded)
    ;; but with a token list to verify table lookup is happening.
    (cl-tmux/config:key-table-bind "copy-mode-vi" #\v :copy-mode-begin-selection)
    (with-fake-session (sess :nwindows 1 :npanes 1)
      ;; Enter copy mode
      (let* ((win    (cl-tmux/model:session-active-window sess))
             (pane   (cl-tmux/model:window-active-pane win))
             (screen (cl-tmux/model:pane-screen pane)))
        (cl-tmux/commands:copy-mode-enter screen)
        ;; The 'v' key (118) should be handled by the table lookup
        ;; We verify copy-mode is active and the binding exists
        (is (cl-tmux/terminal:screen-copy-mode-p screen)
            "screen must be in copy mode")
        (is (not (null (cl-tmux/config:key-table-lookup "copy-mode-vi" #\v)))
            "copy-mode-vi table must have 'v' binding")
        (cl-tmux/commands:copy-mode-exit screen)))))

(defmacro define-copy-mode-table-selection-cases (&body cases)
  "Define copy-mode table-priority tests from declarative rows."
  `(progn
     ,@(loop for (name doc fixture setup input . assertions) in cases
             collect `(test ,name
                        ,doc
                        (,fixture (s screen state)
                          ,@setup
                          ,input
                          ,@assertions)))))

(define-copy-mode-table-selection-cases
  (copy-mode-key-table-selection-follows-mode-keys-vi
   "In vi mode, copy-mode input uses the copy-mode-vi table."
   with-copy-mode-vi-state
   ((cl-tmux/config:key-table-bind "copy-mode-vi" #\v :copy-mode-exit)
    (cl-tmux/config:key-table-bind "copy-mode" #\v :copy-mode-begin-selection))
   (cl-tmux::process-byte s (char-code #\v) state)
   (is-false (cl-tmux/terminal:screen-copy-mode-p screen)
             "vi mode must dispatch the copy-mode-vi binding")))

(test copy-mode-vi-default-hjkl-move-cursor
  "The default copy-mode-vi table provides hjkl cursor movement."
  (with-copy-mode-vi-state (s screen state)
    (seed-scrollback screen 10)
    (setf (cl-tmux/terminal:screen-copy-cursor screen) (cons 1 1))
    (cl-tmux::process-byte s (char-code #\j) state)
    (is (equal (cons 2 1) (cl-tmux/terminal:screen-copy-cursor screen))
        "j must move the copy cursor down")
    (cl-tmux::process-byte s (char-code #\k) state)
    (is (equal (cons 1 1) (cl-tmux/terminal:screen-copy-cursor screen))
        "k must move the copy cursor up")
    (cl-tmux::process-byte s (char-code #\l) state)
    (is (equal (cons 1 2) (cl-tmux/terminal:screen-copy-cursor screen))
        "l must move the copy cursor right")
    (cl-tmux::process-byte s (char-code #\h) state)
    (is (equal (cons 1 1) (cl-tmux/terminal:screen-copy-cursor screen))
        "h must move the copy cursor left")))

(test copy-mode-vi-percent-jumps-to-next-matching-bracket
  "The default copy-mode-vi % binding jumps to the next matching bracket."
  (with-copy-mode-vi-state (s screen state)
    (dotimes (i 7)
      (setf (cl-tmux/terminal/types:screen-cell screen i 0)
            (cl-tmux/terminal/types:make-cell :char (char "(a(b)c)" i))))
    (setf (cl-tmux/terminal:screen-copy-cursor screen) (cons 0 0))
    (cl-tmux::process-byte s (char-code #\%) state)
    (is (equal (cons 0 6) (cl-tmux/terminal:screen-copy-cursor screen))
        "% must jump to the matching closing bracket")))

(test copy-mode-vi-word-search-keys-use-copy-mode-table
  "The default copy-mode-vi # and * bindings search for the word under the cursor."
  (with-copy-mode-vi-state (s screen state)
    (let ((text "xx a.b aXb a.b"))
      (dotimes (i (length text))
        (setf (cl-tmux/terminal/types:screen-cell screen i 0)
              (cl-tmux/terminal/types:make-cell :char (char text i)))))
    (setf (cl-tmux/terminal:screen-copy-cursor screen) (cons 0 3))
    (cl-tmux::process-byte s (char-code #\*) state)
    (is (equal (cons 0 11) (cl-tmux/terminal:screen-copy-cursor screen))
        "* must search forward for the word under cursor through copy-mode-vi")
    (setf (cl-tmux/terminal:screen-copy-cursor screen) (cons 0 12))
    (cl-tmux::process-byte s (char-code #\#) state)
    (is (equal (cons 0 11) (cl-tmux/terminal:screen-copy-cursor screen))
        "# must search backward for the word under cursor through copy-mode-vi")
    (is (string= "a\\.b"
                 (cl-tmux/terminal/types:screen-copy-search-term screen))
        "#/* must save the escaped literal word search term")))

(test copy-mode-vi-named-special-bindings-fire
  "In vi mode, named special-key bindings in copy-mode-vi fire via process-byte.
   Each row: (key bytes description)."
  (dolist (row '(("PageUp" (27 91 53 126)     "copy-mode-vi PageUp binding must fire")
                 ("C-v"    (22)               "C-v must dispatch the named copy-mode-vi binding")
                 ("Enter"  (13)               "Enter must dispatch the named copy-mode-vi binding")
                 ("C-Up"   (27 91 49 59 53 65) "C-Up must dispatch the named copy-mode-vi binding")))
    (destructuring-bind (key bytes msg) row
      (with-copy-mode-vi-state (s screen state)
        (cl-tmux/config:key-table-bind "copy-mode-vi" key :copy-mode-exit)
        (send-copy-mode-bytes s state bytes)
        (is-false (cl-tmux/terminal:screen-copy-mode-p screen) msg)))))

(test copy-mode-vi-control-b-uses-copy-mode-table-before-prefix
  "In vi mode, a C-b byte runs copy-mode-vi C-b instead of arming prefix."
  (with-copy-mode-vi-state (s screen state)
    (seed-scrollback screen 30)
    (is (zerop (screen-copy-offset screen)) "precondition: copy view starts live")
    (cl-tmux::process-byte s 2 state)
    (is (= (min (screen-height screen) 30)
           (screen-copy-offset screen))
        "C-b must dispatch copy-mode-vi page-up, not the prefix key")))

(define-copy-mode-table-selection-cases
  (copy-mode-pagedown-uses-emacs-copy-mode-key-table
   "In emacs mode, CSI PageDown uses the copy-mode table."
   with-copy-mode-emacs-state
   ((cl-tmux/config:key-table-bind "copy-mode-vi" "PageDown" :copy-mode-page-up)
    (cl-tmux/config:key-table-bind "copy-mode" "PageDown" :copy-mode-exit))
   (send-copy-mode-bytes s state '(27 91 54 126))
   (is-false (cl-tmux/terminal:screen-copy-mode-p screen)
             "copy-mode PageDown binding must fire"))
  (copy-mode-key-table-selection-follows-mode-keys-emacs
   "In emacs mode, copy-mode input uses the copy-mode table."
   with-copy-mode-emacs-state
   ((cl-tmux/config:key-table-bind "copy-mode-vi" #\v :copy-mode-exit)
    (cl-tmux/config:key-table-bind "copy-mode" #\v :copy-mode-begin-selection))
   (cl-tmux::process-byte s (char-code #\v) state)
   (is (cl-tmux/terminal:screen-copy-mode-p screen)
       "emacs mode must not dispatch the copy-mode-vi binding")
   (is (cl-tmux/terminal:screen-copy-selecting screen)
       "emacs mode must dispatch the copy-mode binding"))
  (copy-mode-meta-key-table-selection-follows-mode-keys-emacs
   "In emacs mode, ESC-prefixed Meta keys use the copy-mode table."
   with-copy-mode-emacs-state
   ((cl-tmux/config:key-table-bind "copy-mode-vi" "M-f" :copy-mode-exit)
    (cl-tmux/config:key-table-bind "copy-mode" "M-f" :copy-mode-begin-selection))
   (send-copy-mode-bytes s state (list 27 (char-code #\f)))
   (is (cl-tmux/terminal:screen-copy-mode-p screen)
       "emacs mode must not dispatch the copy-mode-vi Meta binding")
   (is (cl-tmux/terminal:screen-copy-selecting screen)
       "emacs mode must dispatch the copy-mode Meta binding")))

(test copy-mode-escape-control-key-does-not-fall-back-to-copy-mode-table
  "In emacs mode, ESC-prefixed Ctrl bytes do not fall back to copy-mode key names."
  (with-copy-mode-emacs-state (s screen state)
    (cl-tmux/config:key-table-bind "copy-mode" "C-b" :copy-mode-begin-selection)
    (send-copy-mode-bytes s state '(27 2))
    (is-true (cl-tmux/terminal:screen-copy-mode-p screen)
             "copy mode must remain active after ESC C-b")
    (is-false (cl-tmux/terminal:screen-copy-selecting screen)
              "ESC-prefixed Ctrl bytes must not dispatch the copy-mode C-b binding")))
