(in-package #:cl-tmux/test)

;;;; Pane zoom, navigation, and resize dispatch cases.

(in-suite dispatch-suite)

(test pane-navigation-unzooms-unless-Z-table
  "Pane-navigation commands on a zoomed window unzoom it unless -Z is given;
   the pane-configuring select-pane forms leave zoom untouched.
   Each row: (command-line expect-zoomed-after description)."
  (dolist (row '(("select-pane -t %2"    nil "select-pane must pop zoom")
                 ("select-pane -Z -t %2" t   "select-pane -Z must keep zoom")
                 ("select-pane -m"       t   "select-pane -m (configure) must keep zoom")
                 ("swap-pane -U"         nil "swap-pane must pop zoom")
                 ("swap-pane -UZ"        t   "swap-pane -Z must keep zoom")
                 ("rotate-window"        nil "rotate-window must pop zoom")
                 ("rotate-window -Z"     t   "rotate-window -Z must keep zoom")
                 ("last-pane"            nil "last-pane must pop zoom")
                 ("last-pane -Z"         t   "last-pane -Z must keep zoom")))
    (destructuring-bind (command expect-zoomed desc) row
      (with-two-pane-h-session (s win p0 p1)
        (with-command-test-state (s :overlay t)
          ;; Arm last-pane's target and zoom the window.
          (cl-tmux/model:window-select-pane win p1)
          (cl-tmux/model:window-select-pane win p0)
          (cl-tmux/model:window-zoom-toggle win)
          (is-true (cl-tmux/model:window-zoom-p win)
                   "precondition: window must be zoomed (~A)" desc)
          (cl-tmux::%run-command-line s command)
          (is (eq expect-zoomed
                  (and (cl-tmux/model:window-zoom-p win) t))
              "~A" desc))))))

(test keyboard-pane-navigation-pops-zoom-table
  "The interactive pane-navigation keyword handlers unzoom a zoomed window
   (tmux window_pop_zoom; the default bindings carry no -Z) and then actually
   move - previously a zoomed window's single-leaf tree made them no-ops.
   Each row: (command expect-focus-moved description)."
  (dolist (row '((:select-pane-right t "prefix-arrow must unzoom and move")
                 (:next-pane         t "prefix-o must unzoom and cycle")
                 (:last-pane         t ":last-pane must unzoom and jump")
                 (:swap-pane-forward nil "swap keeps the same active pane")))
    (destructuring-bind (command expect-moved desc) row
      (with-two-pane-h-session (s win p0 p1)
        (with-command-test-state (s :overlay t)
          ;; Arm last-pane's target, focus p0, then zoom.
          (cl-tmux/model:window-select-pane win p1)
          (cl-tmux/model:window-select-pane win p0)
          (cl-tmux/model:window-zoom-toggle win)
          (is-true (cl-tmux/model:window-zoom-p win)
                   "precondition: window must be zoomed (~A)" desc)
          (cl-tmux::dispatch-command s command nil)
          (is-false (cl-tmux/model:window-zoom-p win)
                    "~A: the window must be unzoomed" desc)
          (if expect-moved
              (is (eq p1 (cl-tmux/model:window-active-pane win))
                  "~A: focus must move to the other pane" desc)
              (is (eq p0 (cl-tmux/model:window-active-pane win))
                  "~A" desc)))))))

(test resize-pane-T-trims-below-cursor-from-history
  "resize-pane -T drops the rows below the cursor and pulls rows out of the
   scrollback to refill the screen; the cursor lands on the bottom row."
  (with-fake-session (s)
    (let* ((pane   (cl-tmux/model:session-active-pane s))
           (screen (cl-tmux/model:pane-screen pane))
           (h      (cl-tmux/terminal/types:screen-height screen)))
      ;; History: one saved row of 'H' cells (newest).
      (let ((saved (make-array (cl-tmux/terminal/types:screen-width screen))))
        (dotimes (col (length saved))
          (setf (aref saved col) (cl-tmux/terminal/types:make-cell :char #\H)))
        (push saved (cl-tmux/terminal/types:screen-scrollback screen)))
      ;; Visible content: 'A' on row 0, cursor on row 0 -> everything below trims.
      (setf (cl-tmux/terminal/types:cell-char
             (cl-tmux/terminal/types:screen-cell screen 0 0)) #\A)
      (setf (cl-tmux/terminal/types:screen-cursor-y screen) 0)
      (cl-tmux::%cmd-resize-pane-arg s '("-T"))
      (is (= (1- h) (cl-tmux/terminal/types:screen-cursor-y screen))
          "-T must land the cursor on the bottom row")
      (is (char= #\A (cl-tmux/terminal/types:cell-char
                      (cl-tmux/terminal/types:screen-cell screen 0 (1- h))))
          "the surviving cursor row must shift to the bottom")
      (is (char= #\H (cl-tmux/terminal/types:cell-char
                      (cl-tmux/terminal/types:screen-cell screen 0 (- h 2))))
          "the newest history row must appear directly above it")
      (is (null (cl-tmux/terminal/types:screen-scrollback screen))
          "the pulled history row must leave the scrollback"))))

(test resize-pane-M-arms-border-drag-state
  "resize-pane -M with an in-flight mouse event on a pane border arms the
   border-drag state used by MouseDrag1Border."
  (with-two-pane-h-session (s win p0 p1)
    (with-command-test-state (s :overlay t)
      (let* ((border-col (+ (cl-tmux/model:pane-x p0)
                            (cl-tmux/model:pane-width p0)))
             (cl-tmux::*mouse-drag-state* nil)
             (cl-tmux::*current-mouse-event*
               (list :btn 32 :col border-col
                     :row (cl-tmux/model:pane-y p0) :release-p nil)))
        (cl-tmux::%cmd-resize-pane-arg s '("-M"))
        (is-true cl-tmux::*mouse-drag-state*
                 "-M on a border must arm the mouse drag state"))
      (let ((cl-tmux::*mouse-drag-state* nil)
            (cl-tmux::*current-mouse-event* nil))
        (cl-tmux::%cmd-resize-pane-arg s '("-M"))
        (is (null cl-tmux::*mouse-drag-state*)
            "-M without a mouse event must not arm the drag state")))))
