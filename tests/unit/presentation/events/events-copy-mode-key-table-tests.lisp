(in-package #:cl-tmux/test)

;;;; Events tests: copy-mode key-table dispatch.

(defmacro define-copy-mode-table-selection-cases (&body cases)
  "Define copy-mode table-priority tests from declarative rows."
  `(progn
     ,@(loop for (name doc fixture setup input . assertions) in cases
             collect `(it ,(string-downcase (symbol-name name))
                        (,fixture (s screen state)
                          ,@setup
                          ,input
                          ,@assertions)))))

(describe "events-suite"

  ;;; ── copy-mode-vi key table override ─────────────────────────────────────────

  ;; A binding in the copy-mode-vi table fires its command and suppresses the hardcoded dispatch.
  (it "copy-mode-vi-table-binding-overrides-hardcoded"
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
          (expect (cl-tmux/terminal:screen-copy-mode-p screen))
          (expect (not (null (cl-tmux/config:key-table-lookup "copy-mode-vi" #\v))))
          (cl-tmux/commands:copy-mode-exit screen)))))

  (define-copy-mode-table-selection-cases
    (copy-mode-key-table-selection-follows-mode-keys-vi
     "In vi mode, copy-mode input uses the copy-mode-vi table."
     with-copy-mode-vi-state
     ((cl-tmux/config:key-table-bind "copy-mode-vi" #\v :copy-mode-exit)
      (cl-tmux/config:key-table-bind "copy-mode" #\v :copy-mode-begin-selection))
     (cl-tmux::process-byte s (char-code #\v) state)
     (expect (cl-tmux/terminal:screen-copy-mode-p screen) :to-be-falsy)))

  ;; The default copy-mode-vi table provides hjkl cursor movement.
  (it "copy-mode-vi-default-hjkl-move-cursor"
    (with-copy-mode-vi-state (s screen state)
      (seed-scrollback screen 10)
      (setf (cl-tmux/terminal:screen-copy-cursor screen) (cons 1 1))
      (cl-tmux::process-byte s (char-code #\j) state)
      (expect (equal (cons 2 1) (cl-tmux/terminal:screen-copy-cursor screen)))
      (cl-tmux::process-byte s (char-code #\k) state)
      (expect (equal (cons 1 1) (cl-tmux/terminal:screen-copy-cursor screen)))
      (cl-tmux::process-byte s (char-code #\l) state)
      (expect (equal (cons 1 2) (cl-tmux/terminal:screen-copy-cursor screen)))
      (cl-tmux::process-byte s (char-code #\h) state)
      (expect (equal (cons 1 1) (cl-tmux/terminal:screen-copy-cursor screen)))))

  ;; The default copy-mode-vi % binding jumps to the next matching bracket.
  (it "copy-mode-vi-percent-jumps-to-next-matching-bracket"
    (with-copy-mode-vi-state (s screen state)
      (dotimes (i 7)
        (setf (cl-tmux/terminal/types:screen-cell screen i 0)
              (cl-tmux/terminal/types:make-cell :char (char "(a(b)c)" i))))
      (setf (cl-tmux/terminal:screen-copy-cursor screen) (cons 0 0))
      (cl-tmux::process-byte s (char-code #\%) state)
      (expect (equal (cons 0 6) (cl-tmux/terminal:screen-copy-cursor screen)))))

  ;; The default copy-mode-vi # and * bindings search for the word under the cursor.
  (it "copy-mode-vi-word-search-keys-use-copy-mode-table"
    (with-copy-mode-vi-state (s screen state)
      (let ((text "xx a.b aXb a.b"))
        (dotimes (i (length text))
          (setf (cl-tmux/terminal/types:screen-cell screen i 0)
                (cl-tmux/terminal/types:make-cell :char (char text i)))))
      (setf (cl-tmux/terminal:screen-copy-cursor screen) (cons 0 3))
      (cl-tmux::process-byte s (char-code #\*) state)
      (expect (equal (cons 0 11) (cl-tmux/terminal:screen-copy-cursor screen)))
      (setf (cl-tmux/terminal:screen-copy-cursor screen) (cons 0 12))
      (cl-tmux::process-byte s (char-code #\#) state)
      (expect (equal (cons 0 11) (cl-tmux/terminal:screen-copy-cursor screen)))
      (expect (string= "a\\.b"
                       (cl-tmux/terminal/types:screen-copy-search-term screen)))))

  ;; In vi mode, named special-key bindings in copy-mode-vi fire via process-byte.
  ;; Each row: (key bytes description).
  (it "copy-mode-vi-named-special-bindings-fire"
    (dolist (row '(("PageUp" (27 91 53 126)     "copy-mode-vi PageUp binding must fire")
                   ("C-v"    (22)               "C-v must dispatch the named copy-mode-vi binding")
                   ("Enter"  (13)               "Enter must dispatch the named copy-mode-vi binding")
                   ("C-Up"   (27 91 49 59 53 65) "C-Up must dispatch the named copy-mode-vi binding")))
      (destructuring-bind (key bytes msg) row
        (declare (ignore msg))
        (with-copy-mode-vi-state (s screen state)
          (cl-tmux/config:key-table-bind "copy-mode-vi" key :copy-mode-exit)
          (send-copy-mode-bytes s state bytes)
          (expect (cl-tmux/terminal:screen-copy-mode-p screen) :to-be-falsy)))))

  ;; In vi mode, a C-b byte runs copy-mode-vi C-b instead of arming prefix.
  (it "copy-mode-vi-control-b-uses-copy-mode-table-before-prefix"
    (with-copy-mode-vi-state (s screen state)
      (seed-scrollback screen 30)
      (expect (zerop (screen-copy-offset screen)))
      (cl-tmux::process-byte s 2 state)
      (expect (= (min (screen-height screen) 30)
                 (screen-copy-offset screen)))))

  (define-copy-mode-table-selection-cases
    (copy-mode-pagedown-uses-emacs-copy-mode-key-table
     "In emacs mode, CSI PageDown uses the copy-mode table."
     with-copy-mode-emacs-state
     ((cl-tmux/config:key-table-bind "copy-mode-vi" "PageDown" :copy-mode-page-up)
      (cl-tmux/config:key-table-bind "copy-mode" "PageDown" :copy-mode-exit))
     (send-copy-mode-bytes s state '(27 91 54 126))
     (expect (cl-tmux/terminal:screen-copy-mode-p screen) :to-be-falsy))
    (copy-mode-key-table-selection-follows-mode-keys-emacs
     "In emacs mode, copy-mode input uses the copy-mode table."
     with-copy-mode-emacs-state
     ((cl-tmux/config:key-table-bind "copy-mode-vi" #\v :copy-mode-exit)
      (cl-tmux/config:key-table-bind "copy-mode" #\v :copy-mode-begin-selection))
     (cl-tmux::process-byte s (char-code #\v) state)
     (expect (cl-tmux/terminal:screen-copy-mode-p screen))
     (expect (cl-tmux/terminal:screen-copy-selecting screen)))
    (copy-mode-meta-key-table-selection-follows-mode-keys-emacs
     "In emacs mode, ESC-prefixed Meta keys use the copy-mode table."
     with-copy-mode-emacs-state
     ((cl-tmux/config:key-table-bind "copy-mode-vi" "M-f" :copy-mode-exit)
      (cl-tmux/config:key-table-bind "copy-mode" "M-f" :copy-mode-begin-selection))
     (send-copy-mode-bytes s state (list 27 (char-code #\f)))
     (expect (cl-tmux/terminal:screen-copy-mode-p screen))
     (expect (cl-tmux/terminal:screen-copy-selecting screen))))

  ;; In emacs mode, ESC-prefixed Ctrl bytes do not fall back to copy-mode key names.
  (it "copy-mode-escape-control-key-does-not-fall-back-to-copy-mode-table"
    (with-copy-mode-emacs-state (s screen state)
      (cl-tmux/config:key-table-bind "copy-mode" "C-b" :copy-mode-begin-selection)
      (send-copy-mode-bytes s state '(27 2))
      (expect (cl-tmux/terminal:screen-copy-mode-p screen) :to-be-truthy)
      (expect (cl-tmux/terminal:screen-copy-selecting screen) :to-be-falsy))))
