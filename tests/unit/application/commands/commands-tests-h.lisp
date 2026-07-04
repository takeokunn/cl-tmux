(in-package #:cl-tmux/test)

;;;; copy-mode-exit, break-pane, clear-history, rotate-window — part VI

(in-suite commands-suite)

;;; ── copy-mode-exit ───────────────────────────────────────────────────────────

(test copy-mode-exit-resets-all-copy-state
  "copy-mode-exit resets copy-mode-p, offset, mark, cursor, and selecting."
  (let ((s (copy-mode-screen)))
    ;; Set all copy-mode fields to non-default values.
    (setf (cl-tmux/terminal/types:screen-copy-offset    s) 5
          (cl-tmux/terminal/types:screen-copy-mark      s) (cons 2 3)
          (cl-tmux/terminal/types:screen-copy-cursor    s) (cons 2 5)
          (cl-tmux/terminal/types:screen-copy-selecting s) t)
    (cl-tmux/commands::copy-mode-exit s)
    (is-false (screen-copy-mode-p s)
              "copy-mode-p must be NIL after exit")
    (is (= 0 (cl-tmux/terminal/types:screen-copy-offset s))
        "copy-offset must be 0 after exit")
    (is (null (cl-tmux/terminal/types:screen-copy-mark s))
        "copy-mark must be NIL after exit")
    (is (null (cl-tmux/terminal/types:screen-copy-cursor s))
        "copy-cursor must be NIL after exit")
    (is-false (cl-tmux/terminal/types:screen-copy-selecting s)
              "copy-selecting must be NIL after exit")))

;;; ── copy-mode-half-page-down ─────────────────────────────────────────────────

(test copy-mode-half-page-down-scrolls-forward-by-half-height
  "copy-mode-half-page-down scrolls forward by floor(screen-height/2) lines."
  (let ((s (%screen-with-scrollback 30)))
    ;; First scroll back enough to allow scrolling forward.
    (setf (cl-tmux/terminal/types:screen-copy-offset s) 20)
    (cl-tmux/commands::copy-mode-half-page-down s)
    ;; height=5, floor(5/2)=2, so offset decreases by 2: 20-2=18.
    (is (= 18 (screen-copy-offset s))
        "copy-mode-half-page-down must reduce offset by floor(5/2)=2 for height=5")))

;;; ── break-pane ───────────────────────────────────────────────────────────────

(test break-pane-sole-pane-returns-nil
  "break-pane on a window with only one pane is a no-op and returns NIL."
  (let* ((pane (%make-test-pane))
         (win  (make-window :id 1 :name "w" :width 20 :height 5
                            :tree (make-layout-leaf pane)
                            :panes (list pane)))
         (sess (make-session :id 1 :name "0" :windows (list win))))
    (session-select-window sess win)
    (window-select-pane win pane)
    (is (null (cl-tmux/commands:break-pane sess))
        "break-pane on a sole-pane window must return NIL")))

(test break-pane-nil-src-win-returns-nil
  "break-pane when session has no active window returns NIL."
  ;; Build a session with no windows to exercise the nil-src-win guard.
  (let ((sess (make-session :id 1 :name "0" :windows nil)))
    (is (null (cl-tmux/commands:break-pane sess))
        "break-pane with no active window must return NIL")))

(test break-pane-moves-pane-to-new-window
  "break-pane removes the active pane and places it in a new window."
  (let* ((p0  (%make-test-pane :id 1 :w 10))
         (p1  (%make-test-pane :id 2 :x 11 :w 10))
         (win (make-window :id 1 :name "w" :width 21 :height 5
                           :tree (make-layout-split :h
                                    (make-layout-leaf p0) (make-layout-leaf p1)
                                    1/2)
                           :panes (list p0 p1)))
         (sess (make-session :id 1 :name "0" :windows (list win))))
    (session-select-window sess win)
    (window-select-pane win p0)
    (let ((new-win (cl-tmux/commands:break-pane sess)))
      (is-true new-win
          "break-pane must return a new window on success")
      (is (member new-win (session-windows sess))
          "new window must appear in the session's window list")
      (is (member p0 (window-panes new-win))
          "the active pane must be the sole pane of the new window")
      (is (= 1 (length (window-panes new-win)))
          "the new window must have exactly one pane")
      ;; Source window still has p1.
      (is (member p1 (window-panes win))
          "the source window must retain the non-active pane"))))

(test break-pane-respects-base-index
  "break-pane assigns the lowest free window id at or above base-index."
  (with-isolated-options ("base-index" 1)
    (let* ((p0  (%make-test-pane :id 1 :w 10))
           (p1  (%make-test-pane :id 2 :x 11 :w 10))
           (win (make-window :id 1 :name "w" :width 21 :height 5
                             :tree (make-layout-split :h
                                      (make-layout-leaf p0) (make-layout-leaf p1)
                                      1/2)
                             :panes (list p0 p1)))
           (sess (make-session :id 1 :name "0" :windows (list win))))
      (session-select-window sess win)
      (window-select-pane win p0)
      (let ((new-win (cl-tmux/commands:break-pane sess)))
        (is (= 2 (window-id new-win))
            "new window id must advance from base-index instead of using 0")
        (is (null (find 0 (session-windows sess) :key #'window-id))
            "break-pane must not create window id 0 when base-index is 1")))))

;;; ── break-pane (scriptable %cmd-break-pane-arg) ──────────────────────────────

(defun %break-arg-fixture ()
  "A window \"w\" with two panes p0 (active), p1 in session \"0\".
   Returns (values sess win p0 p1)."
  (let* ((p0  (%make-test-pane :id 1 :w 10))
         (p1  (%make-test-pane :id 2 :x 11 :w 10))
         (win (make-window :id 1 :name "w" :width 21 :height 5
                           :tree (make-layout-split :h (make-layout-leaf p0)
                                                    (make-layout-leaf p1) 1/2)
                           :panes (list p0 p1)))
         (sess (make-session :id 1 :name "0" :windows (list win))))
    (session-select-window sess win)
    (window-select-pane win p0)
    (values sess win p0 p1)))

(defun %break-extra-window (id)
  (let* ((pane (%make-test-pane :id (+ 100 id) :w 10))
         (win (make-window :id id :name (format nil "w~D" id)
                           :width 10 :height 5
                           :tree (make-layout-leaf pane)
                           :panes (list pane))))
    (window-select-pane win pane)
    win))

(test cmd-break-pane-switch-variants-table
  "break-pane always moves the active pane to a new window; -d controls
   whether the session switches to that new window.
   Each row: (args expect-switch-to-new description)."
  (dolist (row '((()     t   "no -d: session switches to the new window")
                 (("-d") nil "-d: session stays on the current window")))
    (destructuring-bind (args expect-switch desc) row
      (multiple-value-bind (sess win p0 p1) (%break-arg-fixture)
        (declare (ignore p1))
        (with-command-test-state (sess)
          (cl-tmux::%cmd-break-pane-arg sess args)
          (is (= 2 (length (session-windows sess)))
              "a new window must always be created")
          (let* ((new-win (find-if (lambda (w) (not (eq w win))) (session-windows sess)))
                 (expected-active (if expect-switch new-win win)))
            (is (member p0 (window-panes new-win))
                "active pane must always move to the new window")
            (is (eq expected-active (session-active-window sess)) desc)))))))


(test cmd-break-pane-n-names-new-window
  "break-pane -n NAME gives the new window that name."
  (multiple-value-bind (sess win p0 p1) (%break-arg-fixture)
    (declare (ignore p0 p1))
    (with-command-test-state (sess)
      (cl-tmux::%cmd-break-pane-arg sess '("-n" "logs"))
      (let ((new-win (find-if (lambda (w) (not (eq w win))) (session-windows sess))))
        (is (string= "logs" (window-name new-win))
            "the new window must be named 'logs'")))))

(test cmd-break-pane-rejects-unsupported-arguments-before-moving-pane
  "break-pane rejects unsupported arguments before mutation."
  (dolist (case (list (list '("-Z") "unknown flag")
                      (list '("extra") "positional argument")))
    (destructuring-bind (args description) case
      (multiple-value-bind (sess win p0 p1) (%break-arg-fixture)
        (with-command-rejection-state (sess
                                       (cl-tmux::%cmd-break-pane-arg sess args)
                                       "break-pane: unsupported argument"
                                       description)
          (is (= 1 (length (session-windows sess)))
              "~A must not create a new window" description)
          (is (equal (list p0 p1) (window-panes win))
              "~A must leave panes in their original window" description)
          (is (eq win (session-active-window sess))
              "~A must leave the active window unchanged" description))))))

(test cmd-break-pane-t-places-new-window-at-target-index
  "break-pane -t places the new window at the requested free index."
  (multiple-value-bind (sess win p0 p1) (%break-arg-fixture)
    (declare (ignore p1))
    (with-command-test-state (sess)
      (is-true (cl-tmux::%cmd-break-pane-arg sess '("-d" "-t" ":5")))
      (let ((new-win (find 5 (session-windows sess) :key #'window-id)))
        (is-true new-win "the requested window index must be created")
        (is (member p0 (window-panes new-win))
            "the active pane must move to the requested window"))
      (is (eq win (session-active-window sess))
          "-d keeps the source window active"))))

(test cmd-break-pane-t-occupied-index-is-rejected-before-moving-pane
  "break-pane -t rejects an occupied target index before moving the pane."
  (multiple-value-bind (sess win p0 p1) (%break-arg-fixture)
    (let ((other (%break-extra-window 2)))
      (session-insert-window sess other)
      (with-command-test-state (sess)
        (is-false (cl-tmux::%cmd-break-pane-arg sess '("-d" "-t" ":2")))
        (is (equal '(1 2) (mapcar #'window-id (session-windows sess)))
            "occupied target rejection must not renumber windows")
        (is (equal (list p0 p1) (window-panes win))
            "occupied target rejection must leave panes in the source window")
        (is (eq win (session-active-window sess))
            "occupied target rejection must leave the active window unchanged")
        (is-false cl-tmux::*dirty*
                  "occupied target rejection must not dirty the model")))))

(test cmd-break-pane-a-shifts-colliding-windows-after-target
  "break-pane -a inserts after the target index and shifts collisions upward."
  (multiple-value-bind (sess win p0 p1) (%break-arg-fixture)
    (declare (ignore p1))
    (let ((other (%break-extra-window 2)))
      (session-insert-window sess other)
      (with-command-test-state (sess)
        (is-true (cl-tmux::%cmd-break-pane-arg sess '("-d" "-a" "-t" ":1")))
        (is (equal '(1 2 3) (mapcar #'window-id (session-windows sess)))
            "window ids must remain ordered after shuffling")
        (let ((new-win (find 2 (session-windows sess) :key #'window-id)))
          (is (member p0 (window-panes new-win))
              "the broken pane must occupy the index after the target"))
        (is (= 3 (window-id other))
            "the previously colliding window must shift upward")
        (is (eq win (session-active-window sess))
            "-d keeps the source window active")))))

(test cmd-break-pane-b-shifts-colliding-windows-before-target
  "break-pane -b inserts before the target index and shifts collisions upward."
  (multiple-value-bind (sess win p0 p1) (%break-arg-fixture)
    (declare (ignore p1))
    (let ((other (%break-extra-window 2)))
      (session-insert-window sess other)
      (with-command-test-state (sess)
        (is-true (cl-tmux::%cmd-break-pane-arg sess '("-d" "-b" "-t" ":1")))
        (is (equal '(1 2 3) (mapcar #'window-id (session-windows sess)))
            "window ids must remain ordered after shuffling")
        (let ((new-win (find 1 (session-windows sess) :key #'window-id)))
          (is (member p0 (window-panes new-win))
              "the broken pane must occupy the target index"))
        (is (= 2 (window-id win))
            "the source window must shift upward when inserting before it")
        (is (= 3 (window-id other))
            "later colliding windows must also shift upward")))))

(test cmd-break-pane-p-f-prints-custom-format
  "break-pane -P -F prints pane information with the requested format."
  (multiple-value-bind (sess win p0 p1) (%break-arg-fixture)
    (declare (ignore win p0 p1))
    (with-command-test-state (sess :overlay t)
      (is-true (cl-tmux::%cmd-break-pane-arg
                sess '("-d" "-P" "-F" "MARK#{pane_id}")))
      (assert-overlay-uses-custom-format '("MARK" "1") *overlay*
                                         "break-pane -P -F overlay"))))

;;; ── clear-history (scriptable %cmd-clear-history-arg) ────────────────────────

(defun %clear-history-fixture ()
  "Single-pane window \"w\" in session \"0\" whose screen has a non-empty
   scrollback.  Returns (values sess win screen)."
  (let* ((screen (make-screen 10 3))
         (pane   (make-pane :id 1 :x 0 :y 0 :width 10 :height 3
                            :fd -1 :pid -1 :screen screen))
         (win    (make-window :id 1 :name "w" :width 10 :height 3
                              :tree (make-layout-leaf pane) :panes (list pane)))
         (sess   (make-session :id 1 :name "0" :windows (list win))))
    (session-select-window sess win)
    (window-select-pane win pane)
    (setf (cl-tmux/terminal/types:screen-scrollback screen)
          (list (make-array 10 :initial-element
                            (cl-tmux/terminal/types:make-cell
                             :char #\X :fg 7 :bg 0 :attrs 0 :width 1))))
    (values sess win screen)))

(test cmd-clear-history-all-forms-clear-scrollback
  "clear-history clears the target pane's scrollback for any flag combination.
   Each row: (args expected-message)."
  (dolist (row '((("-t" ":w") "clear-history -t must empty the target pane's scrollback")
                 (nil         "clear-history must default to the active pane and empty its scrollback")
                 (("-H")      "clear-history -H must clear the scrollback")))
    (destructuring-bind (args msg) row
      (multiple-value-bind (sess win screen) (%clear-history-fixture)
        (declare (ignore win))
        (with-command-test-state (sess)
          (cl-tmux::%cmd-clear-history-arg sess args)
          (is (null (cl-tmux/terminal/types:screen-scrollback screen)) msg))))))

;;; ── rotate-window (scriptable %cmd-rotate-window-arg) ────────────────────────

(defun %rotate-window-fixture ()
  "Three-pane window \"w\" (p0 p1 p2) in session \"0\".
   Returns (values sess win p0 p1 p2)."
  (let* ((p0 (%make-test-pane :id 1))
         (p1 (%make-test-pane :id 2))
         (p2 (%make-test-pane :id 3))
         (win (make-window :id 1 :name "w" :width 30 :height 6
                           :tree (make-layout-split :h (make-layout-leaf p0)
                                   (make-layout-split :h (make-layout-leaf p1)
                                                      (make-layout-leaf p2) 1/2)
                                   1/2)
                           :panes (list p0 p1 p2)))
         (sess (make-session :id 1 :name "0" :windows (list win))))
    (session-select-window sess win)
    (values sess win p0 p1 p2)))

(test cmd-rotate-window-forward-default
  "rotate-window with no direction rotates forward: first pane moves to end."
  (multiple-value-bind (sess win p0 p1 p2) (%rotate-window-fixture)
    (declare (ignore p2))
    (with-command-test-state (sess)
      (cl-tmux::%cmd-rotate-window-arg sess '("-t" ":w"))
      (is (eq p1 (first (window-panes win)))
          "second pane becomes first")
      (is (eq p0 (car (last (window-panes win))))
          "original first pane moves to end"))))

(test cmd-rotate-window-rejects-zoom-flag
  "rotate-window rejects the removed -Z zoom-preservation flag."
  (multiple-value-bind (sess win p0 p1 p2) (%rotate-window-fixture)
    (with-command-rejection-state (sess
                                   (cl-tmux::%cmd-rotate-window-arg sess '("-Z" "-t" ":w"))
                                   "rotate-window: unsupported argument"
                                   "rotate-window -Z")
      (is (equal (list p0 p1 p2) (window-panes win))
          "rejected -Z must not rotate panes")
      (assert-overlay-active
       "rejected -Z must show an error overlay"))))

(test cmd-rotate-window-d-rotates-backward
  "rotate-window -D -t :w rotates backward: the last pane moves to the front."
  (multiple-value-bind (sess win p0 p1 p2) (%rotate-window-fixture)
    (declare (ignore p0 p1))
    (with-command-test-state (sess)
      (cl-tmux::%cmd-rotate-window-arg sess '("-D" "-t" ":w"))
      (is (eq p2 (first (window-panes win)))
          "-D (backward) makes the last pane first"))))
