(in-package #:cl-tmux/test)

;;;; find-window and next/previous/last-window command behavior

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

(describe "commands-suite"

  ;;; ── find-window (scriptable %cmd-find-window-arg) ────────────────────────────

  ;; find-window <pattern> selects the window whose name matches (case-insensitive).
  (it "cmd-find-window-selects-matching-window"
    (multiple-value-bind (sess wa wb wg) (%find-window-fixture)
      (declare (ignore wa wg))
      (with-command-test-state (sess)
        (cl-tmux::%cmd-find-window-arg sess '("BET"))
        (expect (eq wb (session-active-window sess))))))

  ;; find-window matches pane titles, screen titles, and visible content by default.
  (it "cmd-find-window-supports-title-and-content-search"
    (multiple-value-bind (sess wa wb wg) (%find-window-fixture)
      (declare (ignore wa wg))
      (with-command-test-state (sess)
        (cl-tmux::%cmd-find-window-arg sess '("pane title"))
        (expect (eq wb (session-active-window sess)))
        (session-select-window sess wa)
        (cl-tmux::%cmd-find-window-arg sess '("screen title"))
        (expect (eq wb (session-active-window sess)))
        (session-select-window sess wa)
        (cl-tmux::%cmd-find-window-arg sess '("content needle"))
        (expect (eq wb (session-active-window sess))))))

  ;; find-window accepts -i, -r, -T, and -C search-mode flags.
  (it "cmd-find-window-honors-search-mode-flags"
    (multiple-value-bind (sess wa wb wg) (%find-window-fixture)
      (declare (ignore wa wg))
      (with-command-test-state (sess)
        (cl-tmux::%cmd-find-window-arg sess '("-i" "BETA"))
        (expect (eq wb (session-active-window sess)))
        (session-select-window sess wa)
        (cl-tmux::%cmd-find-window-arg sess '("-r" "^g.*a$"))
        (expect (eq wg (session-active-window sess)))
        (session-select-window sess wa)
        (cl-tmux::%cmd-find-window-arg sess '("-T" "screen title"))
        (expect (eq wb (session-active-window sess)))
        (session-select-window sess wa)
        (cl-tmux::%cmd-find-window-arg sess '("-C" "content needle"))
        (expect (eq wb (session-active-window sess))))))

  ;; find-window -t scopes the search to the targeted session.
  (it "cmd-find-window-targets-another-session"
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
          (expect (eq cur-a (session-active-window cur)))
          (expect (eq other-b (session-active-window other)))))))

  ;; find-window with no matching window leaves the active window unchanged.
  (it "cmd-find-window-no-match-leaves-active"
    (multiple-value-bind (sess wa wb wg) (%find-window-fixture)
      (declare (ignore wb wg))
      (cl-tmux::%cmd-find-window-arg sess '("zzz"))
      (expect (eq wa (session-active-window sess)))))

  ;; find-window rejects extra positional arguments.
  (it "cmd-find-window-rejects-extra-positional-args"
    (multiple-value-bind (sess wa wb wg) (%find-window-fixture)
      (declare (ignore wb wg))
      (session-select-window sess wa)
      (with-command-rejection-state (sess
                                     (cl-tmux::%cmd-find-window-arg sess '("ALP" "extra"))
                                     "find-window: unsupported argument"
                                     "find-window extra args")
        (expect (eq wa (session-active-window sess)))
        (assert-overlay-active
         "rejected args must show an error overlay"))))

  ;; find-window rejects the removed -Z zoom flag.
  (it "cmd-find-window-rejects-zoom-flag"
    (multiple-value-bind (sess wa wb wg) (%find-window-fixture)
      (declare (ignore wg))
      (session-select-window sess wa)
      (with-command-rejection-state (sess
                                     (cl-tmux::%cmd-find-window-arg sess '("-Z" "beta"))
                                     "find-window: unsupported argument"
                                     "find-window -Z")
        (expect (eq wa (session-active-window sess)))
        (expect (cl-tmux/model:window-zoom-p wb) :to-be-falsy)
        (assert-overlay-active
         "rejected -Z must show an error overlay"))))

  ;; %window-matches-pattern-p matches the window name case-insensitively.
  (it "window-matches-pattern-p-name"
    (multiple-value-bind (sess wa wb wg) (%find-window-fixture)
      (declare (ignore sess wb wg))
      (expect (cl-tmux::%window-matches-pattern-p wa "ALP") :to-be-truthy)
      (expect (cl-tmux::%window-matches-pattern-p wa "beta") :to-be-falsy)))

  ;;; ── next-window / previous-window (scriptable -t) ────────────────────────────

  ;; next-window advances to the next window; previous-window wraps to the last.
  ;; Both operate on the current session (no -t flag).
  ;; Each row: (direction expected-window-key description).
  (it "cmd-next-and-previous-window-table"
    (dolist (row '((:next     :wb "next-window advances alpha -> beta")
                   (:previous :wg "previous-window from alpha wraps to gamma")))
      (destructuring-bind (dir expected-key desc) row
        (declare (ignore desc))
        (multiple-value-bind (sess wa wb wg) (%find-window-fixture)
          (declare (ignore wa))
          (with-command-test-state (sess)
            (ecase dir
              (:next     (cl-tmux::%cmd-next-window-arg     sess '()))
              (:previous (cl-tmux::%cmd-previous-window-arg sess '())))
            (let ((expected (ecase expected-key (:wb wb) (:wg wg))))
              (expect (eq expected (session-active-window sess)))))))))

  ;; next-window/previous-window reject unsupported arguments before changing the
  ;; active window.
  (it "cmd-window-cycle-rejects-unsupported-arguments-before-cycling"
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
            (expect (eq wa (session-active-window sess))))))))

  ;; last-window selects the most recently active non-current window.
  (it "cmd-last-window-selects-previously-active-window"
    (multiple-value-bind (sess wa wb wg) (%find-window-fixture)
      (declare (ignore wg))
      (with-command-test-state (sess)
        (session-select-window sess wb)
        (setf (cl-tmux/model:window-last-active-time wb) 40
              (cl-tmux/model:window-last-active-time wa) 30)
        (cl-tmux::%cmd-last-window-arg sess '())
        (expect (eq wa (session-active-window sess)))
        (expect cl-tmux::*dirty* :to-be-truthy))))

  ;; last-window rejects unsupported arguments before changing the active window.
  (it "cmd-last-window-rejects-unsupported-arguments-before-switching"
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
          (expect (eq wb (session-active-window sess)))))))

  ;; next-window -t NAME advances the NAMED session's window, leaving the current
  ;; session's active window unchanged.
  (it "cmd-next-window-t-targets-named-session"
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
        (expect (eq o-b (session-active-window other)))
        (expect (eq cur-win (session-active-window cur))))))

  ;; last-window -t NAME selects the NAMED session's previous window, leaving the
  ;; current session's active window unchanged.
  (it "cmd-last-window-t-targets-named-session"
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
        (expect (eq o-a (session-active-window other)))
        (expect (eq cur-win (session-active-window cur))))))

  ;; next/previous-window -a jump to the nearest alerted window; no alerts -> no-op.
  ;; Fixture order: alpha(active) beta gamma.
  ;; Each row: (dir flag-kw expected-key description).
  (it "cmd-next-previous-window-a-table"
    (dolist (row '((:next     :activity :wg "next-window -a skips beta (no alert) and selects gamma")
                   (:next     :none     :wa "next-window -a with no alerts stays on the active window")
                   (:previous :silence  :wb "previous-window -a selects beta (the alerted window)")))
      (destructuring-bind (dir flag-kw expected-key desc) row
        (declare (ignore desc))
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
              (expect (eq expected (session-active-window sess))))))))))
