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

(test cmd-break-pane-moves-active-pane-and-switches
  "break-pane (no -d) moves the active pane into a new window and switches to it."
  (multiple-value-bind (sess win p0 p1) (%break-arg-fixture)
    (declare (ignore p1))
    (let ((cl-tmux::*server-sessions* (list (cons "0" sess)))
          (cl-tmux::*dirty* nil))
      (cl-tmux::%cmd-break-pane-arg sess '())
      (is (= 2 (length (session-windows sess)))
          "a new window is created (the session now has two)")
      (let ((new-win (find-if (lambda (w) (not (eq w win))) (session-windows sess))))
        (is (member p0 (window-panes new-win)) "the active pane moved to the new window")
        (is (eq new-win (session-active-window sess))
            "the session switches to the new window without -d")))))

(test cmd-break-pane-d-stays-on-current-window
  "break-pane -d creates the new window but does NOT switch to it."
  (multiple-value-bind (sess win p0 p1) (%break-arg-fixture)
    (declare (ignore p0 p1))
    (let ((cl-tmux::*server-sessions* (list (cons "0" sess)))
          (cl-tmux::*dirty* nil))
      (cl-tmux::%cmd-break-pane-arg sess '("-d"))
      (is (= 2 (length (session-windows sess))) "the new window is still created")
      (is (eq win (session-active-window sess))
          "-d keeps the current window active"))))

(test cmd-break-pane-n-names-new-window
  "break-pane -n NAME gives the new window that name."
  (multiple-value-bind (sess win p0 p1) (%break-arg-fixture)
    (declare (ignore p0 p1))
    (let ((cl-tmux::*server-sessions* (list (cons "0" sess)))
          (cl-tmux::*dirty* nil))
      (cl-tmux::%cmd-break-pane-arg sess '("-n" "logs"))
      (let ((new-win (find-if (lambda (w) (not (eq w win))) (session-windows sess))))
        (is (string= "logs" (window-name new-win))
            "the new window must be named 'logs'")))))

(test cmd-break-pane-rejects-unimplemented-arguments-before-moving-pane
  "break-pane rejects unsupported tmux compatibility arguments before mutation."
  (dolist (case (list (list '("-t" ":2") "target placement")
                      (list '("-F" "#{pane_id}") "print format")
                      (list '("-P") "print flag")
                      (list '("-a") "append flag")
                      (list '("extra") "positional argument")))
    (destructuring-bind (args description) case
      (multiple-value-bind (sess win p0 p1) (%break-arg-fixture)
        (let ((cl-tmux::*server-sessions* (list (cons "0" sess)))
              (cl-tmux::*dirty* nil)
              (*overlay* nil))
          (is (null (cl-tmux::%cmd-break-pane-arg sess args))
              "~A must be rejected" description)
          (is (search "break-pane: unsupported argument" *overlay*)
              "~A must explain that the argument is unsupported" description)
          (is (= 1 (length (session-windows sess)))
              "~A must not create a new window" description)
          (is (equal (list p0 p1) (window-panes win))
              "~A must leave panes in their original window" description)
          (is (eq win (session-active-window sess))
              "~A must leave the active window unchanged" description)
          (is-false cl-tmux::*dirty*
                    "~A must not mark the model dirty after rejection" description))))))

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

(test cmd-clear-history-clears-target-pane-scrollback
  "clear-history -t :w clears the target pane's scrollback."
  (multiple-value-bind (sess win screen) (%clear-history-fixture)
    (declare (ignore win))
    (let ((cl-tmux::*server-sessions* (list (cons "0" sess)))
          (cl-tmux::*dirty* nil))
      (cl-tmux::%cmd-clear-history-arg sess '("-t" ":w"))
      (is (null (cl-tmux/terminal/types:screen-scrollback screen))
          "clear-history -t must empty the target pane's scrollback"))))

(test cmd-clear-history-defaults-to-active-pane
  "clear-history with no -t clears the active pane's scrollback."
  (multiple-value-bind (sess win screen) (%clear-history-fixture)
    (declare (ignore win))
    (let ((cl-tmux::*server-sessions* (list (cons "0" sess)))
          (cl-tmux::*dirty* nil))
      (cl-tmux::%cmd-clear-history-arg sess '())
      (is (null (cl-tmux/terminal/types:screen-scrollback screen))
          "clear-history must default to the active pane and empty its scrollback"))))

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

(test cmd-rotate-window-rotates-forward-by-default
  "rotate-window -t :w (no direction) rotates forward: first pane moves to the end."
  (multiple-value-bind (sess win p0 p1 p2) (%rotate-window-fixture)
    (declare (ignore p2))
    (let ((cl-tmux::*server-sessions* (list (cons "0" sess)))
          (cl-tmux::*dirty* nil))
      (cl-tmux::%cmd-rotate-window-arg sess '("-t" ":w"))
      (is (eq p1 (first (window-panes win)))
          "forward rotate makes the second pane first")
      (is (eq p0 (car (last (window-panes win))))
          "the original first pane moves to the end"))))

(test cmd-rotate-window-d-rotates-backward
  "rotate-window -D -t :w rotates backward: the last pane moves to the front."
  (multiple-value-bind (sess win p0 p1 p2) (%rotate-window-fixture)
    (declare (ignore p0 p1))
    (let ((cl-tmux::*server-sessions* (list (cons "0" sess)))
          (cl-tmux::*dirty* nil))
      (cl-tmux::%cmd-rotate-window-arg sess '("-D" "-t" ":w"))
      (is (eq p2 (first (window-panes win)))
          "-D (backward) makes the last pane first"))))

(test cmd-rotate-window-rejects-unimplemented-zoom-flag
  "rotate-window rejects -Z because keep-zoom semantics are not implemented."
  (multiple-value-bind (sess win p0 p1 p2) (%rotate-window-fixture)
    (let ((cl-tmux::*server-sessions* (list (cons "0" sess)))
          (cl-tmux::*dirty* nil)
          (*overlay* nil))
      (is (null (cl-tmux::%cmd-rotate-window-arg sess '("-Z" "-t" ":w")))
          "-Z must be rejected instead of accepted as a no-op")
      (is (search "unsupported argument" *overlay*)
          "-Z must explain that the argument is unsupported")
      (is (equal (list p0 p1 p2) (window-panes win))
          "-Z must not rotate the window after rejection")
      (is-false cl-tmux::*dirty*
                "-Z must not mark the model dirty after rejection"))))

;;; ── find-window (scriptable %cmd-find-window-arg) ────────────────────────────

(defun %find-window-fixture ()
  "Session \"0\" with three named windows alpha/beta/gamma (alpha current).
   Returns (values sess wa wb wg)."
  (let* ((pa (%make-test-pane :id 1)) (pb (%make-test-pane :id 2))
         (pg (%make-test-pane :id 3))
         (wa (make-window :id 1 :name "alpha" :width 20 :height 5
                          :tree (make-layout-leaf pa) :panes (list pa)))
         (wb (make-window :id 2 :name "beta" :width 20 :height 5
                          :tree (make-layout-leaf pb) :panes (list pb)))
         (wg (make-window :id 3 :name "gamma" :width 20 :height 5
                          :tree (make-layout-leaf pg) :panes (list pg)))
         (sess (make-session :id 1 :name "0" :windows (list wa wb wg))))
    (session-select-window sess wa)
    (values sess wa wb wg)))

(test cmd-find-window-selects-matching-window
  "find-window <pattern> selects the window whose name matches (case-insensitive)."
  (multiple-value-bind (sess wa wb wg) (%find-window-fixture)
    (declare (ignore wa wg))
    (let ((cl-tmux::*dirty* nil))
      (cl-tmux::%cmd-find-window-arg sess '("BET"))
      (is (eq wb (session-active-window sess))
          "find-window BET must select the 'beta' window (case-insensitive)"))))

(test cmd-find-window-no-match-leaves-active
  "find-window with no matching window leaves the active window unchanged."
  (multiple-value-bind (sess wa wb wg) (%find-window-fixture)
    (declare (ignore wb wg))
    (cl-tmux::%cmd-find-window-arg sess '("zzz"))
    (is (eq wa (session-active-window sess))
        "no match must leave the original active window selected")))

(test cmd-find-window-rejects-unsupported-search-flags
  "find-window rejects unimplemented search-mode flags and extra positionals instead of accepting them."
  (multiple-value-bind (sess wa wb wg) (%find-window-fixture)
    (declare (ignore wb wg))
    (dolist (args '(("-i" "ALP")
                    ("-r" "ALP")
                    ("-C" "ALP")
                    ("-T" "ALP")
                    ("-Z" "ALP")
                    ("-t" ":0.1" "ALP")
                    ("ALP" "extra")))
      (let ((cl-tmux::*dirty* nil)
            (cl-tmux::*overlay* nil))
        (session-select-window sess wa)
        (is (null (cl-tmux::%cmd-find-window-arg sess args))
            "unsupported find-window args must return NIL: ~S" args)
        (is (eq wa (session-active-window sess))
            "unsupported find-window args must not change the active window: ~S" args)
        (is-false cl-tmux::*dirty*
                  "unsupported find-window args must not mark the display dirty: ~S" args)
        (is (overlay-active-p)
            "unsupported find-window args must show an error overlay: ~S" args)))))

(test window-matches-pattern-p-name
  "%window-matches-pattern-p matches the window name case-insensitively."
  (multiple-value-bind (sess wa wb wg) (%find-window-fixture)
    (declare (ignore sess wb wg))
    (is-true  (cl-tmux::%window-matches-pattern-p wa "ALP") "case-insensitive name match")
    (is-false (cl-tmux::%window-matches-pattern-p wa "beta") "non-matching name → NIL")))

;;; ── next-window / previous-window (scriptable -t) ────────────────────────────

(test cmd-next-window-cycles-current-session
  "next-window (no -t) advances the current session's active window."
  (multiple-value-bind (sess wa wb wg) (%find-window-fixture)   ; alpha(active) beta gamma
    (declare (ignore wa wg))
    (let ((cl-tmux::*server-sessions* (list (cons "0" sess)))
          (cl-tmux::*dirty* nil))
      (cl-tmux::%cmd-next-window-arg sess '())
      (is (eq wb (session-active-window sess))
          "next-window advances alpha → beta"))))

(test cmd-previous-window-wraps-backward
  "previous-window from the first window wraps to the last."
  (multiple-value-bind (sess wa wb wg) (%find-window-fixture)
    (declare (ignore wa wb))
    (let ((cl-tmux::*server-sessions* (list (cons "0" sess)))
          (cl-tmux::*dirty* nil))
      (cl-tmux::%cmd-previous-window-arg sess '())
      (is (eq wg (session-active-window sess))
          "previous-window from alpha wraps to gamma"))))

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
        (let ((cl-tmux::*server-sessions* (list (cons "0" sess)))
              (cl-tmux::*dirty* nil)
              (cl-tmux::*overlay* nil))
          (is (null (funcall command sess args))
              "~A rejects ~S" command-name args)
          (is (eq wa (session-active-window sess))
              "~A leaves the active window unchanged for ~S" command-name args)
          (is-false cl-tmux::*dirty*
                    "~A leaves dirty clear for ~S" command-name args)
          (is (search (format nil "~A: unsupported argument" command-name)
                      cl-tmux::*overlay*)
              "~A reports an unsupported argument for ~S" command-name args))))))

(test cmd-last-window-selects-previously-active-window
  "last-window selects the most recently active non-current window."
  (multiple-value-bind (sess wa wb wg) (%find-window-fixture)
    (declare (ignore wg))
    (let ((cl-tmux::*server-sessions* (list (cons "0" sess)))
          (cl-tmux::*dirty* nil))
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
      (let ((cl-tmux::*server-sessions* (list (cons "0" sess)))
            (cl-tmux::*dirty* nil)
            (cl-tmux::*overlay* nil))
        (session-select-window sess wb)
        (setf (cl-tmux/model:window-last-active-time wb) 40
              (cl-tmux/model:window-last-active-time wa) 30)
        (is (null (cl-tmux::%cmd-last-window-arg sess args))
            "last-window rejects ~S" args)
        (is (eq wb (session-active-window sess))
            "last-window leaves the active window unchanged for ~S" args)
        (is-false cl-tmux::*dirty*
                  "last-window leaves dirty clear for ~S" args)
        (is (search "last-window: unsupported argument" cl-tmux::*overlay*)
            "last-window reports an unsupported argument for ~S" args)))))

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

(test cmd-next-window-a-jumps-to-alerted-window
  "next-window -a skips windows without an alert and selects the next window whose
   activity (or silence) flag is set."
  (multiple-value-bind (sess wa wb wg) (%find-window-fixture)  ; alpha(active) beta gamma
    (declare (ignore wa wb))
    (let ((cl-tmux::*server-sessions* (list (cons "0" sess)))
          (cl-tmux::*dirty* nil))
      (setf (cl-tmux/model:window-activity-flag wg) t)   ; only gamma has an alert
      (cl-tmux::%cmd-next-window-arg sess '("-a"))
      (is (eq wg (session-active-window sess))
          "next-window -a skips beta (no alert) and selects gamma"))))

(test cmd-next-window-a-no-alerts-is-noop
  "next-window -a with no alerted windows leaves the active window unchanged."
  (multiple-value-bind (sess wa wb wg) (%find-window-fixture)
    (declare (ignore wb wg))
    (let ((cl-tmux::*server-sessions* (list (cons "0" sess)))
          (cl-tmux::*dirty* nil))
      (cl-tmux::%cmd-next-window-arg sess '("-a"))
      (is (eq wa (session-active-window sess))
          "next-window -a with no alerts stays on the active window"))))

(test cmd-previous-window-a-jumps-backward-to-alerted-window
  "previous-window -a scans backward to the nearest window with an alert."
  (multiple-value-bind (sess wa wb wg) (%find-window-fixture)  ; alpha(active) beta gamma
    (declare (ignore wa wg))
    (let ((cl-tmux::*server-sessions* (list (cons "0" sess)))
          (cl-tmux::*dirty* nil))
      (setf (cl-tmux/model:window-silence-flag wb) t)    ; beta has a silence alert
      (cl-tmux::%cmd-previous-window-arg sess '("-a"))
      (is (eq wb (session-active-window sess))
          "previous-window -a selects beta (the alerted window)"))))
