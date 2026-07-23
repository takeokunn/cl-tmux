(in-package #:cl-tmux/test)

;;;; Popup and menu dispatch runtime tests.

(describe "dispatch-suite"

  ;; ── %format-popup-overlay helper ─────────────────────────────────────────────

  ;; %format-popup-overlay produces a box-drawing overlay string.
  (it "format-popup-overlay-produces-box"
    (let ((result (cl-tmux::%format-popup-overlay "test" "body-text")))
      (expect (stringp result))
      (expect (search "test" result))
      (expect (search "body-text" result))
      (expect (search "┌" result))
      (expect (search "└" result))))

  ;; %format-popup-overlay with NIL output substitutes an empty string.
  (it "format-popup-overlay-nil-output-uses-empty-string"
    (let ((result (cl-tmux::%format-popup-overlay "cmd" nil)))
      (expect (stringp result))
      (expect (search "cmd" result))))

  ;; ── Popup and buffer-preview positive-constant checks ────────────────────────

  ;; Popup dimension and buffer-preview constants must all be positive.
  (it "popup-and-buffer-preview-constants-positive-table"
    (dolist (row (list (list cl-tmux::+popup-max-width+      "+popup-max-width+")
                       (list cl-tmux::+popup-max-height+     "+popup-max-height+")
                       (list cl-tmux::+popup-margin+         "+popup-margin+")
                       (list cl-tmux::+buffer-preview-length+ "+buffer-preview-length+")))
      (destructuring-bind (val name) row
        (declare (ignore name))
        (expect (> val 0)))))

  ;; ── :display-popup dispatch ──────────────────────────────────────────────────

  ;; :display-popup opens a prompt for the shell command.
  (it "dispatch-display-popup-opens-prompt"
    (with-fake-session (s)
      (let ((*prompt* nil))
        (cl-tmux::dispatch-command s :display-popup nil)
        (expect (prompt-active-p))
        (expect (string= "popup command" (prompt-label *prompt*))))))

  ;; :display-popup-dismiss clears *active-popup*.
  (it "dispatch-display-popup-dismiss-clears-popup"
    (with-fake-session (s)
      (setf cl-tmux::*active-popup*
            (make-popup :title "t" :width 40 :height 10 :screen nil :pane nil))
      (cl-tmux::dispatch-command s :display-popup-dismiss nil)
      (expect (null cl-tmux::*active-popup*))))

  ;; ── :display-menu / :menu-next / :menu-prev / :menu-select / :menu-dismiss ──

  ;; :display-menu sets *active-menu* and opens an overlay.
  (it "dispatch-display-menu-opens-menu-and-overlay"
    (with-fake-session (s)
      (let ((*overlay* nil)
            (cl-tmux::*active-menu* nil))
        (cl-tmux::dispatch-command s :display-menu nil)
        (expect (not (null cl-tmux::*active-menu*)))
        (assert-overlay-active ":display-menu must open an overlay"))))

  ;; display-menu -x/-y stores the position on the menu struct (default NIL = centred).
  (it "cmd-display-menu-x-y-sets-menu-position"
    (with-fake-session (s)
      (let ((*overlay* nil)
            (cl-tmux::*active-menu* nil))
        (cl-tmux::%cmd-display-menu-arg
         s '("-x" "10" "-y" "5" "Item" "a" "next-window"))
        (expect (not (null cl-tmux::*active-menu*)))
        (expect (= 10 (cl-tmux/prompt:menu-x cl-tmux::*active-menu*)))
        (expect (= 5 (cl-tmux/prompt:menu-y cl-tmux::*active-menu*))))))

  ;; display-menu without -x/-y leaves menu-x/menu-y NIL (centred default).
  (it "cmd-display-menu-no-x-y-is-centered"
    (with-fake-session (s)
      (let ((*overlay* nil)
            (cl-tmux::*active-menu* nil))
        (cl-tmux::%cmd-display-menu-arg s '("Item" "a" "next-window"))
        (expect (not (null cl-tmux::*active-menu*)))
        (expect (null (cl-tmux/prompt:menu-x cl-tmux::*active-menu*)))
        (expect (null (cl-tmux/prompt:menu-y cl-tmux::*active-menu*))))))

  ;; %run-command-line display-menu with no item args reports canonical syntax error.
  (it "run-command-line-display-menu-empty-args-reports-too-few"
    (with-fake-session (s)
      (let ((*overlay* nil)
            (cl-tmux::*active-menu* nil))
        (expect (null (cl-tmux::%run-command-line s "display-menu")))
        (expect (null cl-tmux::*active-menu*))
        (assert-overlay-contains "command display-menu: too few arguments (need at least 1)"
                                 (overlay-lines)
                                 "display-menu empty args"))))

  ;; :menu-next from 0 advances to 1; :menu-prev from 0 wraps to last index (1).
  (it "dispatch-menu-next-prev-table"
    (dolist (cmd '(:menu-next :menu-prev))
      (with-fake-session (s)
        (let ((cl-tmux::*active-menu*
                (make-menu :title "t"
                           :items (list (cons "a" :ka) (cons "b" :kb))
                           :selected-index 0)))
          (cl-tmux::dispatch-command s cmd nil)
          (expect (= 1 (menu-selected-index cl-tmux::*active-menu*)))))))

  ;; :menu-dismiss clears *active-menu* and the overlay.
  (it "dispatch-menu-dismiss-clears-menu-and-overlay"
    (with-fake-session (s)
      (let ((cl-tmux::*active-menu*
              (make-menu :title "t" :items (list (cons "a" :ka)) :selected-index 0)))
        (cl-tmux::dispatch-command s :menu-dismiss nil)
        (expect (null cl-tmux::*active-menu*))))))
