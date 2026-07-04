(in-package #:cl-tmux/test)

;;;; Popup and menu dispatch runtime tests.

(in-suite dispatch-suite)

;;; ── %format-popup-overlay helper ─────────────────────────────────────────────

(test format-popup-overlay-produces-box
  "%format-popup-overlay produces a box-drawing overlay string."
  (let ((result (cl-tmux::%format-popup-overlay "test" "body-text")))
    (is (stringp result) "%format-popup-overlay must return a string")
    (is (search "test" result) "overlay must contain the title")
    (is (search "body-text" result) "overlay must contain the output")
    (is (search "┌" result) "overlay must have a top-left corner")
    (is (search "└" result) "overlay must have a bottom-left corner")))

(test format-popup-overlay-nil-output-uses-empty-string
  "%format-popup-overlay with NIL output substitutes an empty string."
  (let ((result (cl-tmux::%format-popup-overlay "cmd" nil)))
    (is (stringp result) "%format-popup-overlay must not error with nil output")
    (is (search "cmd" result) "overlay must still contain the title")))

;;; ── Popup and buffer-preview positive-constant checks ────────────────────────

(test popup-and-buffer-preview-constants-positive-table
  "Popup dimension and buffer-preview constants must all be positive."
  (dolist (row (list (list cl-tmux::+popup-max-width+      "+popup-max-width+")
                     (list cl-tmux::+popup-max-height+     "+popup-max-height+")
                     (list cl-tmux::+popup-margin+         "+popup-margin+")
                     (list cl-tmux::+buffer-preview-length+ "+buffer-preview-length+")))
    (destructuring-bind (val name) row
      (is (> val 0) "~A must be positive" name))))

;;; ── :display-popup dispatch ──────────────────────────────────────────────────

(test dispatch-display-popup-opens-prompt
  ":display-popup opens a prompt for the shell command."
  (with-fake-session (s)
    (let ((*prompt* nil))
      (cl-tmux::dispatch-command s :display-popup nil)
      (is (prompt-active-p) ":display-popup must open a prompt")
      (is (string= "popup command" (prompt-label *prompt*))
          ":display-popup prompt label must be \"popup command\""))))

(test dispatch-display-popup-dismiss-clears-popup
  ":display-popup-dismiss clears *active-popup*."
  (with-fake-session (s)
    (setf cl-tmux::*active-popup*
          (make-popup :title "t" :width 40 :height 10 :screen nil :pane nil))
    (cl-tmux::dispatch-command s :display-popup-dismiss nil)
    (is (null cl-tmux::*active-popup*)
        ":display-popup-dismiss must set *active-popup* to nil")))

;;; ── :display-menu / :menu-next / :menu-prev / :menu-select / :menu-dismiss ──

(test dispatch-display-menu-opens-menu-and-overlay
  ":display-menu sets *active-menu* and opens an overlay."
  (with-fake-session (s)
    (let ((*overlay* nil)
          (cl-tmux::*active-menu* nil))
      (cl-tmux::dispatch-command s :display-menu nil)
      (is (not (null cl-tmux::*active-menu*))
          ":display-menu must set *active-menu*")
      (assert-overlay-active ":display-menu must open an overlay"))))

(test cmd-display-menu-x-y-sets-menu-position
  "display-menu -x/-y stores the position on the menu struct (default NIL = centred)."
  (with-fake-session (s)
    (let ((*overlay* nil)
          (cl-tmux::*active-menu* nil))
      (cl-tmux::%cmd-display-menu-arg
       s '("-x" "10" "-y" "5" "Item" "a" "next-window"))
      (is (not (null cl-tmux::*active-menu*)) "menu must be created")
      (is (= 10 (cl-tmux/prompt:menu-x cl-tmux::*active-menu*))
          "-x sets menu-x to 10")
      (is (= 5 (cl-tmux/prompt:menu-y cl-tmux::*active-menu*))
          "-y sets menu-y to 5"))))

(test cmd-display-menu-no-x-y-is-centered
  "display-menu without -x/-y leaves menu-x/menu-y NIL (centred default)."
  (with-fake-session (s)
    (let ((*overlay* nil)
          (cl-tmux::*active-menu* nil))
      (cl-tmux::%cmd-display-menu-arg s '("Item" "a" "next-window"))
      (is (not (null cl-tmux::*active-menu*)) "menu must be created")
      (is (null (cl-tmux/prompt:menu-x cl-tmux::*active-menu*))
          "menu-x defaults to NIL (centred)")
      (is (null (cl-tmux/prompt:menu-y cl-tmux::*active-menu*))
          "menu-y defaults to NIL (centred)"))))

(test run-command-line-display-menu-empty-args-reports-too-few
  "%run-command-line display-menu with no item args reports canonical syntax error."
  (with-fake-session (s)
    (let ((*overlay* nil)
          (cl-tmux::*active-menu* nil))
      (is (null (cl-tmux::%run-command-line s "display-menu"))
          "display-menu empty args returns no dispatch keyword")
      (is (null cl-tmux::*active-menu*)
          "display-menu empty args must not open the internal default menu")
      (assert-overlay-contains "command display-menu: too few arguments (need at least 1)"
                               (overlay-lines)
                               "display-menu empty args"))))

(test dispatch-menu-next-prev-table
  ":menu-next from 0 advances to 1; :menu-prev from 0 wraps to last index (1)."
  (dolist (cmd '(:menu-next :menu-prev))
    (with-fake-session (s)
      (let ((cl-tmux::*active-menu*
              (make-menu :title "t"
                         :items (list (cons "a" :ka) (cons "b" :kb))
                         :selected-index 0)))
        (cl-tmux::dispatch-command s cmd nil)
        (is (= 1 (menu-selected-index cl-tmux::*active-menu*))
            "~A from 0 must result in selected-index 1" cmd)))))

(test dispatch-menu-dismiss-clears-menu-and-overlay
  ":menu-dismiss clears *active-menu* and the overlay."
  (with-fake-session (s)
    (let ((cl-tmux::*active-menu*
            (make-menu :title "t" :items (list (cons "a" :ka)) :selected-index 0)))
      (cl-tmux::dispatch-command s :menu-dismiss nil)
      (is (null cl-tmux::*active-menu*)
          ":menu-dismiss must clear *active-menu*"))))
