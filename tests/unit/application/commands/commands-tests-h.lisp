(in-package #:cl-tmux/test)

;;;; copy-mode-exit, break-pane, clear-history, rotate-window, find-window, next/prev-window — part V

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

(test cmd-rotate-window-forward-variants-table
  "rotate-window default and -Z both rotate forward: first pane moves to end.
   Each row: (args description)."
  (dolist (row (list (list '("-t" ":w")      "default (no direction): p1 becomes first, p0 moves to end")
                     (list '("-Z" "-t" ":w") "-Z accepted, still rotates forward")))
    (destructuring-bind (args desc) row
      (multiple-value-bind (sess win p0 p1 p2) (%rotate-window-fixture)
        (declare (ignore p2))
        (with-command-test-state (sess)
          (cl-tmux::%cmd-rotate-window-arg sess args)
          (is (eq p1 (first (window-panes win)))
              "~A: second pane becomes first" desc)
          (is (eq p0 (car (last (window-panes win))))
              "~A: original first pane moves to end" desc))))))

(test cmd-rotate-window-d-rotates-backward
  "rotate-window -D -t :w rotates backward: the last pane moves to the front."
  (multiple-value-bind (sess win p0 p1 p2) (%rotate-window-fixture)
    (declare (ignore p0 p1))
    (with-command-test-state (sess)
      (cl-tmux::%cmd-rotate-window-arg sess '("-D" "-t" ":w"))
      (is (eq p2 (first (window-panes win)))
          "-D (backward) makes the last pane first"))))

;;; ── find-window (scriptable %cmd-find-window-arg) ────────────────────────────

(defun %find-window-fixture ()
  "Session \"0\" with three named windows alpha/beta/gamma (alpha current).
   The beta window has title and content markers for find-window parity tests.
   Returns (values sess wa wb wg)."
  (let* ((pa (%make-test-pane :id 1))
         (pb (%make-test-pane :id 2))
         (pg (%make-test-pane :id 3)))
    (feed (cl-tmux/model:pane-screen pb) "beta content needle")
    (feed (cl-tmux/model:pane-screen pg) "gamma regex marker")
    (setf (cl-tmux/model:pane-title pa) "alpha pane")
    (setf (cl-tmux/model:pane-title pb) "beta pane title")
    (setf (cl-tmux/model:pane-title pg) "gamma pane title")
    (setf (cl-tmux/terminal/types:screen-title (cl-tmux/model:pane-screen pa))
          "alpha screen")
    (setf (cl-tmux/terminal/types:screen-title (cl-tmux/model:pane-screen pb))
          "beta screen title")
    (setf (cl-tmux/terminal/types:screen-title (cl-tmux/model:pane-screen pg))
          "gamma screen title")
    (let* ((wa (make-window :id 1 :name "alpha" :width 20 :height 5
                            :tree (make-layout-leaf pa) :panes (list pa)))
           (wb (make-window :id 2 :name "beta" :width 20 :height 5
                            :tree (make-layout-leaf pb) :panes (list pb)))
           (wg (make-window :id 3 :name "gamma" :width 20 :height 5
                            :tree (make-layout-leaf pg) :panes (list pg)))
           (sess (make-session :id 1 :name "0" :windows (list wa wb wg))))
      (session-select-window sess wa)
      (values sess wa wb wg))))

(test cmd-find-window-selects-matching-window
  "find-window <pattern> selects the window whose name matches (case-insensitive)."
  (multiple-value-bind (sess wa wb wg) (%find-window-fixture)
    (declare (ignore wa wg))
    (with-command-test-state (sess)
      (cl-tmux::%cmd-find-window-arg sess '("BET"))
      (is (eq wb (session-active-window sess))
          "find-window BET must select the 'beta' window (case-insensitive)"))))

(test cmd-find-window-supports-title-and-content-search
  "find-window matches pane titles, screen titles, and visible content by default."
  (multiple-value-bind (sess wa wb wg) (%find-window-fixture)
    (declare (ignore wa wg))
    (with-command-test-state (sess)
      (cl-tmux::%cmd-find-window-arg sess '("pane title"))
      (is (eq wb (session-active-window sess))
          "default find-window must match the pane title")
      (session-select-window sess wa)
      (cl-tmux::%cmd-find-window-arg sess '("screen title"))
      (is (eq wb (session-active-window sess))
          "default find-window must match the screen title")
      (session-select-window sess wa)
      (cl-tmux::%cmd-find-window-arg sess '("content needle"))
      (is (eq wb (session-active-window sess))
          "default find-window must match visible content"))))

(test cmd-find-window-honors-search-mode-flags
  "find-window accepts -i, -r, -T, and -C search-mode flags."
  (multiple-value-bind (sess wa wb wg) (%find-window-fixture)
    (declare (ignore wa wg))
    (with-command-test-state (sess)
      (cl-tmux::%cmd-find-window-arg sess '("-i" "BETA"))
      (is (eq wb (session-active-window sess))
          "-i must be accepted and keep matching case-insensitively")
      (session-select-window sess wa)
      (cl-tmux::%cmd-find-window-arg sess '("-r" "^g.*a$"))
      (is (eq wg (session-active-window sess))
          "-r must enable regex matching")
      (session-select-window sess wa)
      (cl-tmux::%cmd-find-window-arg sess '("-T" "screen title"))
      (is (eq wb (session-active-window sess))
          "-T must restrict matching to titles")
      (session-select-window sess wa)
      (cl-tmux::%cmd-find-window-arg sess '("-C" "content needle"))
      (is (eq wb (session-active-window sess))
          "-C must restrict matching to visible content"))))

(test cmd-find-window-targets-another-session
  "find-window -t scopes the search to the targeted session."
  (multiple-value-bind (cur cur-a cur-b cur-c) (%find-window-fixture)
    (declare (ignore cur-b cur-c))
    (multiple-value-bind (other other-a other-b other-c) (%find-window-fixture)
      (declare (ignore other-a other-c))
      (setf (session-name other) "other")
      (session-select-window other other-b)
      (let ((cl-tmux::*server-sessions* (list (cons "0" cur)
                                              (cons "other" other)))
            (cl-tmux::*dirty* nil))
        (cl-tmux::%cmd-find-window-arg cur '("-t" "other" "beta"))
        (is (eq cur-a (session-active-window cur))
            "-t must not change the calling session")
        (is (eq other-b (session-active-window other))
            "-t must select the matching window in the target session")))))

(test cmd-find-window-no-match-leaves-active
  "find-window with no matching window leaves the active window unchanged."
  (multiple-value-bind (sess wa wb wg) (%find-window-fixture)
    (declare (ignore wb wg))
    (cl-tmux::%cmd-find-window-arg sess '("zzz"))
    (is (eq wa (session-active-window sess))
        "no match must leave the original active window selected")))

(test cmd-find-window-rejects-extra-positional-args
  "find-window rejects extra positional arguments."
  (multiple-value-bind (sess wa wb wg) (%find-window-fixture)
    (declare (ignore wb wg))
    (session-select-window sess wa)
    (with-command-rejection-state (sess
                                   (cl-tmux::%cmd-find-window-arg sess '("ALP" "extra"))
                                   "find-window: unsupported argument"
                                   "find-window extra args")
      (is (eq wa (session-active-window sess))
          "rejected args must not change the active window")
      (assert-overlay-active
       "rejected args must show an error overlay"))))

(test cmd-find-window-rejects-zoom-flag
  "find-window rejects the removed -Z zoom flag."
  (multiple-value-bind (sess wa wb wg) (%find-window-fixture)
    (declare (ignore wg))
    (session-select-window sess wa)
    (with-command-rejection-state (sess
                                   (cl-tmux::%cmd-find-window-arg sess '("-Z" "beta"))
                                   "find-window: unsupported argument"
                                   "find-window -Z")
      (is (eq wa (session-active-window sess))
          "rejected -Z must not change the active window")
      (is-false (cl-tmux/model:window-zoom-p wb)
                "rejected -Z must not zoom the matching window")
      (assert-overlay-active
       "rejected -Z must show an error overlay"))))

(test window-matches-pattern-p-name
  "%window-matches-pattern-p matches the window name case-insensitively."
  (multiple-value-bind (sess wa wb wg) (%find-window-fixture)
    (declare (ignore sess wb wg))
    (is-true  (cl-tmux::%window-matches-pattern-p wa "ALP") "case-insensitive name match")
    (is-false (cl-tmux::%window-matches-pattern-p wa "beta") "non-matching name → NIL")))

;;; ── next-window / previous-window (scriptable -t) ────────────────────────────

(test cmd-next-and-previous-window-table
  "next-window advances to the next window; previous-window wraps to the last.
   Both operate on the current session (no -t flag).
   Each row: (direction expected-window-key description)."
  (dolist (row '((:next     :wb "next-window advances alpha → beta")
                 (:previous :wg "previous-window from alpha wraps to gamma")))
    (destructuring-bind (dir expected-key desc) row
      (multiple-value-bind (sess wa wb wg) (%find-window-fixture)
        (declare (ignore wa))
        (with-command-test-state (sess)
          (ecase dir
            (:next     (cl-tmux::%cmd-next-window-arg     sess '()))
            (:previous (cl-tmux::%cmd-previous-window-arg sess '())))
          (let ((expected (ecase expected-key (:wb wb) (:wg wg))))
            (is (eq expected (session-active-window sess)) desc)))))))


(test cmd-window-cycle-rejects-unsupported-arguments-before-cycling
  "next-window/previous-window reject unsupported arguments before changing the
   active window."
  (dolist (case (list (list #'cl-tmux::%cmd-next-window-arg
                            "next-window" '("-Z"))
                      (list #'cl-tmux::%cmd-next-window-arg
                            "next-window" '("extra"))
                      (list #'cl-tmux::%cmd-previous-window-arg
                            "previous-window" '("-Z"))
                      (list #'cl-tmux::%cmd-previous-window-arg
                            "previous-window" '("extra"))))
    (destructuring-bind (command command-name args) case
      (multiple-value-bind (sess wa wb wg) (%find-window-fixture)
        (declare (ignore wb wg))
        (with-command-rejection-state (sess
                                       (funcall command sess args)
                                       (format nil "~A: unsupported argument" command-name)
                                       (format nil "~A rejects ~S" command-name args))
          (is (eq wa (session-active-window sess))
              "~A leaves the active window unchanged for ~S" command-name args))))))

(test cmd-last-window-selects-previously-active-window
  "last-window selects the most recently active non-current window."
  (multiple-value-bind (sess wa wb wg) (%find-window-fixture)
    (declare (ignore wg))
    (with-command-test-state (sess)
      (session-select-window sess wb)
      (setf (cl-tmux/model:window-last-active-time wb) 40
            (cl-tmux/model:window-last-active-time wa) 30)
      (cl-tmux::%cmd-last-window-arg sess '())
      (is (eq wa (session-active-window sess))
          "last-window switches to the previous window")
      (is-true cl-tmux::*dirty*
               "last-window marks the display dirty"))))

(test cmd-last-window-rejects-unsupported-arguments-before-switching
  "last-window rejects unsupported arguments before changing the active window."
  (dolist (args '(("-Z") ("extra")))
    (multiple-value-bind (sess wa wb wg) (%find-window-fixture)
      (declare (ignore wg))
      (session-select-window sess wb)
      (setf (cl-tmux/model:window-last-active-time wb) 40
            (cl-tmux/model:window-last-active-time wa) 30)
      (with-command-rejection-state (sess
                                     (cl-tmux::%cmd-last-window-arg sess args)
                                     "last-window: unsupported argument"
                                     (format nil "last-window rejects ~S" args))
        (is (eq wb (session-active-window sess))
            "last-window leaves the active window unchanged for ~S" args)))))

(test cmd-next-window-t-targets-named-session
  "next-window -t NAME advances the NAMED session's window, leaving the current
   session's active window unchanged."
  (let* ((pc (%make-test-pane :id 1)) (poa (%make-test-pane :id 2))
         (pob (%make-test-pane :id 3))
         (cur-win (make-window :id 1 :name "cur" :width 20 :height 5
                               :tree (make-layout-leaf pc) :panes (list pc)))
         (cur     (make-session :id 1 :name "cur" :windows (list cur-win)))
         (o-a (make-window :id 2 :name "oa" :width 20 :height 5
                           :tree (make-layout-leaf poa) :panes (list poa)))
         (o-b (make-window :id 3 :name "ob" :width 20 :height 5
                           :tree (make-layout-leaf pob) :panes (list pob)))
         (other (make-session :id 2 :name "other" :windows (list o-a o-b))))
    (session-select-window cur cur-win)
    (session-select-window other o-a)
    (let ((cl-tmux::*server-sessions* (list (cons "cur" cur) (cons "other" other)))
          (cl-tmux::*dirty* nil))
      (cl-tmux::%cmd-next-window-arg cur '("-t" "other"))
      (is (eq o-b (session-active-window other))
          "next-window -t other advanced the OTHER session to its second window")
      (is (eq cur-win (session-active-window cur))
          "the current session's active window stays unchanged"))))

(test cmd-last-window-t-targets-named-session
  "last-window -t NAME selects the NAMED session's previous window, leaving the
   current session's active window unchanged."
  (let* ((pc (%make-test-pane :id 1)) (poa (%make-test-pane :id 2))
         (pob (%make-test-pane :id 3))
         (cur-win (make-window :id 1 :name "cur" :width 20 :height 5
                               :tree (make-layout-leaf pc) :panes (list pc)))
         (cur     (make-session :id 1 :name "cur" :windows (list cur-win)))
         (o-a (make-window :id 2 :name "oa" :width 20 :height 5
                           :tree (make-layout-leaf poa) :panes (list poa)))
         (o-b (make-window :id 3 :name "ob" :width 20 :height 5
                           :tree (make-layout-leaf pob) :panes (list pob)))
         (other (make-session :id 2 :name "other" :windows (list o-a o-b))))
    (session-select-window cur cur-win)
    (session-select-window other o-b)
    (setf (cl-tmux/model:window-last-active-time o-b) 40
          (cl-tmux/model:window-last-active-time o-a) 30)
    (let ((cl-tmux::*server-sessions* (list (cons "cur" cur) (cons "other" other)))
          (cl-tmux::*dirty* nil))
      (cl-tmux::%cmd-last-window-arg cur '("-t" "other"))
      (is (eq o-a (session-active-window other))
          "last-window -t other selects the OTHER session's previous window")
      (is (eq cur-win (session-active-window cur))
          "the current session's active window stays unchanged"))))

(test cmd-next-previous-window-a-table
  "next/previous-window -a jump to the nearest alerted window; no alerts → no-op.
   Fixture order: alpha(active) beta gamma.
   Each row: (dir flag-kw expected-key description)."
  (dolist (row '((:next     :activity :wg "next-window -a skips beta (no alert) and selects gamma")
                 (:next     :none     :wa "next-window -a with no alerts stays on the active window")
                 (:previous :silence  :wb "previous-window -a selects beta (the alerted window)")))
    (destructuring-bind (dir flag-kw expected-key desc) row
      (multiple-value-bind (sess wa wb wg) (%find-window-fixture)
        (with-command-test-state (sess)
          (ecase flag-kw
            (:activity (setf (cl-tmux/model:window-activity-flag wg) t))
            (:silence  (setf (cl-tmux/model:window-silence-flag  wb) t))
            (:none     nil))
          (ecase dir
            (:next     (cl-tmux::%cmd-next-window-arg     sess '("-a")))
            (:previous (cl-tmux::%cmd-previous-window-arg sess '("-a"))))
          (let ((expected (ecase expected-key (:wa wa) (:wb wb) (:wg wg))))
            (is (eq expected (session-active-window sess)) desc)))))))
