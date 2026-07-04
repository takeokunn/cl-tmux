(in-package #:cl-tmux/test)

;;;; Copy-mode and send-keys -X dispatch tests.

(in-suite dispatch-suite)

(test dispatch-copy-mode-enter-exit
  "C-b [ enters copy mode on the active screen; exit clears it."
  (with-fake-session (s)
    (cl-tmux::dispatch-command s :copy-mode-enter nil)
    (is (screen-copy-mode-p (active-screen s)) "copy mode should be on after enter")
    (cl-tmux::dispatch-command s :copy-mode-exit nil)
    (is-false (screen-copy-mode-p (active-screen s)) "copy mode should be off after exit")))

(test copy-mode-active-p-reflects-state
  "%copy-mode-active-p tracks the active screen's copy-mode flag."
  (with-fake-session (s)
    (is-false (cl-tmux::%copy-mode-active-p s))
    (cl-tmux::dispatch-command s :copy-mode-enter nil)
    (is (cl-tmux::%copy-mode-active-p s))))

(test send-keys-R-resets-pane-terminal-state
  "send-keys -R resets the target pane's terminal state (RIS): the cursor is homed."
  (with-fake-session (s :nwindows 1 :npanes 1)
    (let* ((win  (cl-tmux/model:session-active-window s))
           (pane (cl-tmux/model:window-active-pane win))
           (scr  (cl-tmux/model:pane-screen pane)))
      (setf (cl-tmux/terminal/types:screen-cursor-x scr) 5
            (cl-tmux/terminal/types:screen-cursor-y scr) 3)
      (cl-tmux::%cmd-send-keys-arg s '("-R"))
      (is (= 0 (cl-tmux/terminal/types:screen-cursor-x scr))
          "send-keys -R must home the cursor x")
      (is (= 0 (cl-tmux/terminal/types:screen-cursor-y scr))
          "send-keys -R must home the cursor y"))))

(test send-keys-x-command-table
  "The *copy-mode-x-commands* table maps all send-keys -X names to their
   proper copy-mode keywords."
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
      (is (eq expected (copy-mode-x-command-value name))
          "~S must map to ~S" name expected))))

(test send-keys-x-rectangle-toggle-toggles-rect-select
  "send -X rectangle-toggle flips the screen's rectangle (block) selection flag
   instead of starting a stream selection."
  (with-copy-mode-active-screen (s screen)
    (is-false (screen-copy-rect-select-p screen) "rect-select off on entry")
    (is (cl-tmux::%dispatch-send-keys-X s "rectangle-toggle")
        "rectangle-toggle must be a handled -X command")
    (is (screen-copy-rect-select-p screen) "rectangle-toggle turns rect-select ON")
    (cl-tmux::%dispatch-send-keys-X s "rectangle-toggle")
    (is-false (screen-copy-rect-select-p screen) "a second toggle turns it OFF")))

(test send-keys-x-copy-selection-stays-in-copy-mode
  "send -X copy-selection copies the selection and clears it but STAYS in copy
   mode (tmux WINDOW_COPY_CMD_REDRAW), whereas copy-selection-and-cancel exits."
  (is (eq :copy-mode-copy-selection-no-cancel
          (copy-mode-x-command-value "copy-selection"))
      "copy-selection must map to the non-cancelling keyword")
  (is (eq :copy-mode-yank
          (copy-mode-x-command-value "copy-selection-and-cancel"))
      "copy-selection-and-cancel must map to the exit-on-yank keyword"))

(test send-keys-x-rectangle-toggle-restores-temporary-focus
  "send -X rectangle-toggle must temporarily focus the target pane and restore
   the original session/window focus afterward."
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
      (is-false (screen-copy-rect-select-p target-screen)
                "rect-select starts off on the target screen")
      (is (cl-tmux::%dispatch-send-keys-X s "rectangle-toggle" target-pane target-window nil)
          "rectangle-toggle must be handled via the temporary focus helper")
      (is (screen-copy-rect-select-p target-screen)
          "rectangle-toggle must toggle the target screen")
      (is (eq original-window (session-active-window s))
          "session focus must be restored to the original window")
      (is (eq original-pane (session-active-pane s))
          "session focus must be restored to the original pane"))))

(test send-keys-x-copy-pipe-and-cancel-variants
  "send -X copy-pipe-and-cancel without an arg and with an explicit command both
   copy the selection to the paste buffer and exit copy mode.
   Each row: (pipe-args description)."
  (dolist (row '((nil       "copy-pipe-and-cancel (no explicit command)")
                 (("cat")   "copy-pipe-and-cancel with explicit command")))
    (destructuring-bind (pipe-args desc) row
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
              (is (cl-tmux::%dispatch-send-keys-X s "copy-pipe-and-cancel" nil nil pipe-args)
                  "~A must be handled" desc)
              (is (string= "pipe-me" (cl-tmux/buffer:get-paste-buffer 0))
                  "~A must copy to the paste buffer" desc)
              (is-false (screen-copy-mode-p screen)
                        "~A must exit copy mode" desc))))))))

(test send-keys-x-copy-pipe-end-of-line-and-cancel-copies-to-eol-and-exits
  "send -X copy-pipe-end-of-line-and-cancel with an explicit command should
   copy from the cursor position through the end of the line and exit copy
   mode."
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
          (is (cl-tmux::%dispatch-send-keys-X s
                                              "copy-pipe-end-of-line-and-cancel"
                                              nil nil
                                              '("cat"))
              "copy-pipe-end-of-line-and-cancel with an argument must be handled")
          (is (string= "me now" (cl-tmux/buffer:get-paste-buffer 0))
              "text from the cursor through the end of the line must be copied")
          (is-false (screen-copy-mode-p screen)
                    "copy-pipe-end-of-line-and-cancel must exit copy mode"))))))

(test send-keys-x-explicit-jump-commands
  "send -X canonical jump commands must pass explicit character arguments
   through to the copy-mode jump helpers."
  (with-copy-mode-active-screen (s screen :feed "hello world")
    (labels ((invoke (command start args)
               (setf (screen-copy-cursor screen) start)
               (is (cl-tmux::%dispatch-send-keys-X s command nil nil args)
                   "~A with an explicit argument must be handled" command)
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

(test send-keys-x-rejects-removed-jump-to-alias
  "send -X jump-to was an alias for jump-to-forward; aliases are not accepted."
  (with-copy-mode-active-screen (s screen :feed "hello world")
    (setf (screen-copy-cursor screen) (cons 0 0))
    (is-false (cl-tmux::%dispatch-send-keys-X s "jump-to" nil nil '("l"))
              "jump-to must not dispatch as an unsupported alias")
    (is (equal (cons 0 0) (screen-copy-cursor screen))
        "unknown commands must leave the copy cursor unchanged")))

(test send-keys-x-explicit-text-commands
  "send -X search-forward-text / search-backward-text must accept explicit
   search text, including multi-token arguments, without opening a prompt."
  (labels ((invoke (feed command start args)
             (with-copy-mode-active-screen (s screen :feed feed)
               (setf (screen-copy-cursor screen) start)
               (is (cl-tmux::%dispatch-send-keys-X s command nil nil args)
                   "~A with an explicit argument must be handled" command)
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

(test send-keys-x-goto-line-jumps-to-line-number
  "send -X goto-line with an argument parses the line number and moves the copy
   cursor to the requested row."
  (with-copy-mode-active-screen (s screen)
    (labels ((invoke (args)
               (setf (screen-copy-cursor screen) (cons 0 0))
               (is (cl-tmux::%dispatch-send-keys-X s "goto-line" nil nil args)
                   "goto-line with explicit arguments must be handled")
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

(test send-keys-x-cursor-left-right-move-cursor
  "send -X cursor-right / cursor-left move the copy-mode cursor horizontally
   (previously both mis-mapped to begin-selection, which did not move it)."
  (with-fake-session (s)
    (cl-tmux::dispatch-command s :copy-mode-enter nil)
    (let ((screen (active-screen s)))
      (cl-tmux::%dispatch-send-keys-X s "cursor-right")
      (is (= 1 (cdr (screen-copy-cursor screen)))
          "cursor-right moves the cursor to column 1 (from the initial column 0)")
      (cl-tmux::%dispatch-send-keys-X s "cursor-left")
      (is (= 0 (cdr (screen-copy-cursor screen)))
          "cursor-left moves the cursor back to column 0"))))

(test send-keys-x-cursor-up-down-move-cursor-vertically
  "send -X cursor-up / cursor-down move the copy cursor vertically (the -X names
   previously only scrolled a line, inconsistent with the arrow-key path)."
  (with-fake-session (s)
    (cl-tmux::dispatch-command s :copy-mode-enter nil)
    (let* ((screen (active-screen s))
           (h      (screen-height screen)))
      (cl-tmux::%dispatch-send-keys-X s "cursor-up")
      (is (= (- h 2) (car (screen-copy-cursor screen)))
          "cursor-up moves the cursor one row up from the bottom")
      (cl-tmux::%dispatch-send-keys-X s "cursor-down")
      (is (= (- h 1) (car (screen-copy-cursor screen)))
          "cursor-down moves the cursor back to the bottom row"))))
