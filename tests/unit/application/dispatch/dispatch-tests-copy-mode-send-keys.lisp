(in-package #:cl-tmux/test)

;;;; Copy-mode and send-keys -X dispatch tests.

(defmacro check-send-keys-x-explicit-arg-specs (&rest cases)
  "Assert canonical explicit-argument facts for send-keys -X."
  `(dolist (case ',cases)
     (destructuring-bind (command expected-kind expected-handler) case
       (multiple-value-bind (kind handler)
           (cl-tmux::%send-keys-x-explicit-arg-spec command)
         (expect (eq kind expected-kind))
         (expect (eq handler expected-handler))))))

(defmacro check-send-keys-x-removed-explicit-arg-aliases (&rest commands)
  "Assert removed send-keys -X aliases never expose explicit-argument facts."
  `(dolist (command ',commands)
     (multiple-value-bind (kind handler)
         (cl-tmux::%send-keys-x-explicit-arg-spec command)
       (expect (null kind))
       (expect (null handler)))))

(describe "dispatch-suite"

  ;; C-b [ enters copy mode on the active screen; exit clears it.
  (it "dispatch-copy-mode-enter-exit"
    (with-fake-session (s)
      (cl-tmux::dispatch-command s :copy-mode-enter nil)
      (expect (screen-copy-mode-p (active-screen s)))
      (cl-tmux::dispatch-command s :copy-mode-exit nil)
      (expect (screen-copy-mode-p (active-screen s)) :to-be-falsy)))

  ;; %copy-mode-active-p tracks the active screen's copy-mode flag.
  (it "copy-mode-active-p-reflects-state"
    (with-fake-session (s)
      (expect (cl-tmux::%copy-mode-active-p s) :to-be-falsy)
      (cl-tmux::dispatch-command s :copy-mode-enter nil)
      (expect (cl-tmux::%copy-mode-active-p s))))

  ;; send-keys -R resets the target pane's terminal state (RIS): the cursor is homed.
  (it "send-keys-R-resets-pane-terminal-state"
    (with-fake-session (s :nwindows 1 :npanes 1)
      (let* ((win  (cl-tmux/model:session-active-window s))
             (pane (cl-tmux/model:window-active-pane win))
             (scr  (cl-tmux/model:pane-screen pane)))
        (setf (cl-tmux/terminal/types:screen-cursor-x scr) 5
              (cl-tmux/terminal/types:screen-cursor-y scr) 3)
        (cl-tmux::%cmd-send-keys-arg s '("-R"))
        (expect (= 0 (cl-tmux/terminal/types:screen-cursor-x scr)))
        (expect (= 0 (cl-tmux/terminal/types:screen-cursor-y scr))))))

  ;; The *copy-mode-x-commands* table maps all send-keys -X names to their
  ;; proper copy-mode keywords.
  (it "send-keys-x-command-table"
    (dolist (c '(("cursor-left"      :copy-mode-cursor-left)
                 ("cursor-right"     :copy-mode-cursor-right)
                 ("cursor-up"        :copy-mode-cursor-up)
                 ("cursor-down"      :copy-mode-cursor-down)
                 ("rectangle-toggle" :copy-mode-rectangle-toggle)
                 ("copy-selection"   :copy-mode-copy-selection-no-cancel)
                 ("select-word"      :copy-mode-select-word)
                 ("other-end"        :copy-mode-other-end)
                 ("copy-pipe"        :copy-mode-copy-pipe-no-cancel)
                 ("copy-pipe-and-cancel" :copy-mode-copy-pipe-and-cancel)
                 ("copy-pipe-end-of-line-and-cancel"
                  :copy-mode-copy-pipe-end-of-line-and-cancel)
                 ("next-matching-bracket" :copy-mode-next-matching-bracket)
                 ("previous-matching-bracket"
                  :copy-mode-previous-matching-bracket)))
      (destructuring-bind (name expected) c
        (expect (eq expected (copy-mode-x-command-value name))))))

  ;; send -X rectangle-toggle flips the screen's rectangle (block) selection flag
  ;; instead of starting a stream selection.
  (it "send-keys-x-rectangle-toggle-toggles-rect-select"
    (with-copy-mode-active-screen (s screen)
      (expect (screen-copy-rect-select-p screen) :to-be-falsy)
      (expect (cl-tmux::%dispatch-send-keys-X s "rectangle-toggle"))
      (expect (screen-copy-rect-select-p screen))
      (cl-tmux::%dispatch-send-keys-X s "rectangle-toggle")
      (expect (screen-copy-rect-select-p screen) :to-be-falsy)))

  ;; send -X copy-selection copies the selection and clears it but STAYS in copy
  ;; mode (tmux WINDOW_COPY_CMD_REDRAW), whereas copy-selection-and-cancel exits.
  (it "send-keys-x-copy-selection-stays-in-copy-mode"
    (expect (eq :copy-mode-copy-selection-no-cancel
                (copy-mode-x-command-value "copy-selection")))
    (expect (eq :copy-mode-yank
                (copy-mode-x-command-value "copy-selection-and-cancel"))))

  ;; send -X rectangle-toggle must temporarily focus the target pane and restore
  ;; the original session/window focus afterward.
  (it "send-keys-x-rectangle-toggle-restores-temporary-focus"
    (with-fake-session (s :nwindows 2)
      (let* ((original-window (session-active-window s))
             (original-pane   (session-active-pane s))
             (target-window   (second (session-windows s)))
             (target-pane     (first (window-panes target-window)))
             (target-screen   (pane-screen target-pane)))
        (setf (cl-tmux/model::session-active s) target-window
              (cl-tmux/model::window-active target-window) target-pane)
        (cl-tmux::dispatch-command s :copy-mode-enter nil)
        (setf (cl-tmux/model::session-active s) original-window
              (cl-tmux/model::window-active original-window) original-pane
              (cl-tmux/model::window-active target-window) target-pane)
        (expect (screen-copy-rect-select-p target-screen) :to-be-falsy)
        (expect (cl-tmux::%dispatch-send-keys-X s "rectangle-toggle" target-pane target-window nil))
        (expect (screen-copy-rect-select-p target-screen))
        (expect (eq original-window (session-active-window s)))
        (expect (eq original-pane (session-active-pane s))))))

  ;; send -X copy-pipe-and-cancel without an arg and with an explicit command both
  ;; copy the selection to the paste buffer and exit copy mode.
  ;; Each row: (pipe-args description).
  (it "send-keys-x-copy-pipe-and-cancel-variants"
    (dolist (row '((nil       "copy-pipe-and-cancel (no explicit command)")
                   (("cat")   "copy-pipe-and-cancel with explicit command")))
      (destructuring-bind (pipe-args desc) row
        (declare (ignore desc))
        (let ((cl-tmux/buffer:*paste-buffers* nil))
          (with-option-session (s)
            (with-loop-state
              (cl-tmux/options:set-option "copy-command" "")
              (let ((screen (active-screen s)))
                (feed screen "pipe-me")
                (cl-tmux::dispatch-command s :copy-mode-enter nil)
                (setf (screen-copy-selecting screen) t
                      (screen-copy-mark screen) (cons 0 0)
                      (screen-copy-cursor screen) (cons 0 7))
                (expect (cl-tmux::%dispatch-send-keys-X s "copy-pipe-and-cancel" nil nil pipe-args))
                (expect (string= "pipe-me" (cl-tmux/buffer:get-paste-buffer 0)))
                (expect (screen-copy-mode-p screen) :to-be-falsy))))))))

  ;; send -X copy-pipe-end-of-line-and-cancel with an explicit command should
  ;; copy from the cursor position through the end of the line and exit copy
  ;; mode.
  (it "send-keys-x-copy-pipe-end-of-line-and-cancel-copies-to-eol-and-exits"
    (let ((cl-tmux/buffer:*paste-buffers* nil))
      (with-option-session (s)
        (with-loop-state
          (cl-tmux/options:set-option "copy-command" "")
          (let ((screen (active-screen s)))
            (feed screen "pipe-me now")
            (cl-tmux::dispatch-command s :copy-mode-enter nil)
            (setf (screen-copy-selecting screen) t
                  (screen-copy-mark screen) (cons 0 5)
                  (screen-copy-cursor screen) (cons 0 5))
            (expect (cl-tmux::%dispatch-send-keys-X s
                                                     "copy-pipe-end-of-line-and-cancel"
                                                     nil nil
                                                     '("cat")))
            (expect (string= "me now" (cl-tmux/buffer:get-paste-buffer 0)))
            (expect (screen-copy-mode-p screen) :to-be-falsy))))))

  ;; send -X canonical jump commands must pass explicit character arguments
  ;; through to the copy-mode jump helpers.
  (it "send-keys-x-explicit-jump-commands"
    (with-copy-mode-active-screen (s screen :feed "hello world")
      (labels ((invoke (command start args)
                 (setf (screen-copy-cursor screen) start)
                 (expect (cl-tmux::%dispatch-send-keys-X s command nil nil args))
                 (screen-copy-cursor screen)))
        (check-table
         (list
          (list (invoke "jump-forward" (cons 0 0) '("l"))
                (cons 0 2)
                "jump-forward must land on the next matching character")
          (list (invoke "jump-backward" (cons 0 5) '("l"))
                (cons 0 3)
                "jump-backward must land on the previous matching character")
          (list (invoke "jump-to-forward" (cons 0 0) '("l"))
                (cons 0 1)
                "jump-to-forward must land just before the next matching character")
          (list (invoke "jump-to-backward" (cons 0 5) '("l"))
                (cons 0 4)
                "jump-to-backward must land just after the previous matching character"))
         :test #'equal))))

  ;; The explicit-arg lookup reads facts from the supplied table.
  (it "send-keys-x-explicit-arg-lookup-reads-fact-table"
    (multiple-value-bind (kind handler)
        (cl-tmux::%lookup-send-keys-x-explicit-arg-spec
         "demo" '(("demo" :text demo-handler)))
      (expect (eq :text kind))
      (expect (eq 'demo-handler handler)))
    (multiple-value-bind (kind handler)
        (cl-tmux::%lookup-send-keys-x-explicit-arg-spec
         "missing" '(("demo" :text demo-handler)))
      (expect (null kind))
      (expect (null handler))))

  ;; The explicit-arg fact table exposes canonical command names only.
  (it "send-keys-x-explicit-arg-facts-are-canonical"
    (check-send-keys-x-explicit-arg-specs
     ("jump-forward"                  :char cl-tmux/commands:copy-mode-jump-forward)
     ("jump-backward"                 :char cl-tmux/commands:copy-mode-jump-backward)
     ("jump-to-forward"               :char cl-tmux/commands:copy-mode-jump-to)
     ("jump-to-backward"              :char cl-tmux/commands:copy-mode-jump-to-backward)
     ("goto-line"                     :line cl-tmux/commands:copy-mode-goto-line)
     ("search-forward-text"           :text cl-tmux/commands:copy-mode-search-forward)
     ("search-backward-text"          :text cl-tmux/commands:copy-mode-search-backward)
     ("copy-pipe"                     :text cl-tmux/commands:copy-mode-copy-pipe-no-cancel)
     ("copy-pipe-and-cancel"          :text cl-tmux/commands:copy-mode-copy-pipe)
     ("copy-pipe-end-of-line-and-cancel"
      :text cl-tmux/commands:copy-mode-copy-pipe-end-of-line)
     ("copy-pipe-end-of-line"
      :text cl-tmux/commands:copy-mode-copy-pipe-end-of-line-no-cancel)
     ("copy-pipe-no-clear"            :text cl-tmux/commands:copy-mode-copy-pipe-no-clear)
     ("copy-pipe-line"                :text cl-tmux/commands:copy-mode-copy-pipe-line)
     ("copy-pipe-line-and-cancel"
      :text cl-tmux/commands:copy-mode-copy-pipe-line-and-cancel)
     ("pipe"                          :text cl-tmux/commands:copy-mode-pipe-no-cancel)
     ("pipe-no-clear"                 :text cl-tmux/commands:copy-mode-pipe-no-clear)
     ("pipe-and-cancel"               :text cl-tmux/commands:copy-mode-pipe-and-cancel)
     ("selection-mode"                :text cl-tmux/commands:copy-mode-selection-mode))
    (check-send-keys-x-removed-explicit-arg-aliases "jump-to"))

  ;; send -X jump-to was an alias for jump-to-forward; aliases are not accepted.
  (it "send-keys-x-rejects-removed-jump-to-alias"
    (with-copy-mode-active-screen (s screen :feed "hello world")
      (setf (screen-copy-cursor screen) (cons 0 0))
      (expect (cl-tmux::%dispatch-send-keys-X s "jump-to" nil nil '("l")) :to-be-falsy)
      (expect (equal (cons 0 0) (screen-copy-cursor screen)))))

  ;; send -X search-forward-text / search-backward-text must accept explicit
  ;; search text, including multi-token arguments, without opening a prompt.
  (it "send-keys-x-explicit-text-commands"
    (labels ((invoke (feed command start args)
               (with-copy-mode-active-screen (s screen :feed feed)
                 (setf (screen-copy-cursor screen) start)
                 (expect (cl-tmux::%dispatch-send-keys-X s command nil nil args))
                 (list (screen-copy-cursor screen)
                       (cl-tmux/terminal/types:screen-copy-search-term screen)))))
      (check-table
       (list
        (list (invoke "abc def abc" "search-forward-text" (cons 0 0) '("abc"))
              (list (cons 0 8) "abc")
              "search-forward-text must jump to the next matching text")
        (list (invoke "abc def abc" "search-backward-text" (cons 0 11) '("abc"))
              (list (cons 0 8) "abc")
              "search-backward-text must jump to the previous matching text")
        (list (invoke "zzzabc defzzz" "search-forward-text" (cons 0 0) '("abc" "def"))
              (list (cons 0 3) "abc def")
              "search-forward-text must join multiple positional tokens"))
       :test #'equal)))

  ;; send -X goto-line with an argument parses the line number and moves the copy
  ;; cursor to the requested row.
  (it "send-keys-x-goto-line-jumps-to-line-number"
    (with-copy-mode-active-screen (s screen)
      (labels ((invoke (args)
                 (setf (screen-copy-cursor screen) (cons 0 0))
                 (expect (cl-tmux::%dispatch-send-keys-X s "goto-line" nil nil args))
                 (screen-copy-cursor screen)))
        (check-table
         (list
          (list (invoke '("2"))
                (cons 1 0)
                "goto-line 2 must land on row 1")
          (list (invoke '("2" "ignored"))
                (cons 1 0)
                "goto-line must ignore trailing positional tokens"))
         :test #'equal))))

  ;; send -X cursor-right / cursor-left move the copy-mode cursor horizontally
  ;; (previously both mis-mapped to begin-selection, which did not move it).
  (it "send-keys-x-cursor-left-right-move-cursor"
    (with-fake-session (s)
      (cl-tmux::dispatch-command s :copy-mode-enter nil)
      (let ((screen (active-screen s)))
        (cl-tmux::%dispatch-send-keys-X s "cursor-right")
        (expect (= 1 (cdr (screen-copy-cursor screen))))
        (cl-tmux::%dispatch-send-keys-X s "cursor-left")
        (expect (= 0 (cdr (screen-copy-cursor screen)))))))

  ;; send -X cursor-up / cursor-down move the copy cursor vertically (the -X names
  ;; previously only scrolled a line, inconsistent with the arrow-key path).
  (it "send-keys-x-cursor-up-down-move-cursor-vertically"
    (with-fake-session (s)
      (cl-tmux::dispatch-command s :copy-mode-enter nil)
      (let* ((screen (active-screen s))
             (h      (screen-height screen)))
        (cl-tmux::%dispatch-send-keys-X s "cursor-up")
        (expect (= (- h 2) (car (screen-copy-cursor screen))))
        (cl-tmux::%dispatch-send-keys-X s "cursor-down")
        (expect (= (- h 1) (car (screen-copy-cursor screen))))))))
