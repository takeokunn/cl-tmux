(in-package #:cl-tmux/test)

;;;; Session state tests: pure session / window helpers.
;;;;
;;;; These tests avoid real PTYs and therefore always run in sandboxed builds.

(in-suite model-suite)

;;; ── session-active-pane (no PTY) ────────────────────────────────────────────

(test session-active-pane
  "session-active-pane returns the active pane of the active window."
  (let* ((sess (make-fake-session :nwindows 1 :npanes 1))
         (win  (session-active-window sess))
         (pane (window-active-pane win)))
    (is (eq pane (session-active-pane sess))
        "session-active-pane must match window-active-pane of the active window")))

;;; ── %shell-basename — table-driven ──────────────────────────────────────────
;;;
;;; Four edge cases exercised in one table.  The individual per-case tests were
;;; consolidated here to eliminate structural duplication.

(test shell-basename-table
  "Table-driven: %shell-basename returns correct result for diverse shell paths."
  ;; Each entry: (default-shell expected description)
  (dolist (entry
           '(("/bin/bash"  "bash"   "%shell-basename strips /bin/ prefix")
             ("zsh"        "zsh"    "%shell-basename returns bare name when no slash")
             (nil          "window" "%shell-basename returns \"window\" when shell is NIL")
             ("/usr/bin/"  ""       "%shell-basename returns empty string for trailing-slash path")))
    (destructuring-bind (shell expected desc) entry
      (let ((cl-tmux/config:*default-shell* shell))
        (is (string= expected (cl-tmux/model::%shell-basename)) desc)))))

(test shell-basename-trailing-slash-is-string
  "%shell-basename returns a string even for a trailing-slash path."
  (let ((cl-tmux/config:*default-shell* "/usr/bin/"))
    (is (stringp (cl-tmux/model::%shell-basename))
        "%shell-basename must return a string even for a trailing-slash path")))

;;; ── session-insert-window ────────────────────────────────────────────────────

(test session-insert-window-sorts-by-id
  "session-insert-window keeps the window list sorted by window-id."
  (let* ((w0   (make-window :id 0 :name "a"))
         (w2   (make-window :id 2 :name "c"))
         (w1   (make-window :id 1 :name "b"))
         (sess (make-session :id 1 :name "s" :windows (list w0 w2))))
    (session-insert-window sess w1)
    (is (equal (list w0 w1 w2) (session-windows sess))
        "session-insert-window must sort windows by id ascending")))

(test session-insert-window-does-not-change-active
  "session-insert-window does not mutate the active window slot."
  (let* ((w0   (make-window :id 0 :name "a"))
         (w1   (make-window :id 1 :name "b"))
         (sess (make-session :id 1 :name "s" :windows (list w0))))
    (session-select-window sess w0)
    (session-insert-window sess w1)
    (is (eq w0 (session-active-window sess))
        "session-insert-window must not change the active window")))

(test kill-window-selects-previous-by-index
  "After killing the active window with no MRU history (timestamps tie at 0), tmux
   session_detach selects the PREVIOUS window by index — the greatest id strictly
   less than the killed id (here w0, id 0 < killed id 1)."
  (let* ((w0 (make-window :id 0 :name "a" :width 20 :height 5
                          :panes (list (make-no-pty-pane 1 0 0 20 5))))
         (w1 (make-window :id 1 :name "b" :width 20 :height 5
                          :panes (list (make-no-pty-pane 2 0 0 20 5))))
         (w3 (make-window :id 3 :name "d" :width 20 :height 5
                          :panes (list (make-no-pty-pane 3 0 0 20 5))))
         (sess (make-session :id 1 :name "0" :windows (list w0 w1 w3))))
    (session-select-window sess w1)       ; kill the middle window (id=1)
    (cl-tmux/commands:kill-window sess)
    ;; No unambiguous MRU (only w1 was focused, and it is gone) -> previous-by-index:
    ;; greatest id < 1 among {0,3} is w0.
    (is (eq w0 (session-active-window sess))
        "after killing id=1, the previous-by-index window id=0 must become active")))

;;; ── session-touch ────────────────────────────────────────────────────────────

(test session-touch-updates-last-active
  "session-touch sets session-last-active to a recent universal time and returns the session."
  (let ((sess (make-session :id 42 :name "t" :last-active 0)))
    (let ((before (get-universal-time)))
      (let ((result (session-touch sess)))
        (is (eq sess result)
            "session-touch must return the session")
        (is (>= (session-last-active sess) before)
            "last-active must be updated to at least the time before the call")))))

;;; ── all-panes ────────────────────────────────────────────────────────────────

(test all-panes-returns-flat-list-of-panes
  "all-panes returns a flat list of every pane across all windows."
  (let* ((p0   (make-no-pty-pane 1 0 0 20 5))
         (p1   (make-no-pty-pane 2 0 0 20 5))
         (w0   (make-window :id 0 :name "w0" :panes (list p0)))
         (w1   (make-window :id 1 :name "w1" :panes (list p1)))
         (sess (make-session :id 1 :name "s" :windows (list w0 w1))))
    (let ((panes (all-panes sess)))
      (is (= 2 (length panes))
          "all-panes must return all panes across both windows")
      (is-true (member p0 panes) "all-panes must include pane p0")
      (is-true (member p1 panes) "all-panes must include pane p1"))))

(test all-panes-empty-session
  "all-panes returns NIL for a session with no windows."
  (let ((sess (make-session :id 1 :name "s" :windows nil)))
    (is (null (all-panes sess))
        "all-panes must return NIL for a windowless session")))

;;; ── session-id ────────────────────────────────────────────────────────────────

(test session-id-slot-accessible
  "session-id returns the id passed to make-session."
  (let ((sess (make-session :id 99 :name "test")))
    (is (= 99 (session-id sess))
        "session-id must return the id set at construction")))

;;; ── session-name ─────────────────────────────────────────────────────────────

(test session-name-slot-accessible
  "session-name returns the name passed to make-session."
  (let ((sess (make-session :id 1 :name "my-session")))
    (is (string= "my-session" (session-name sess))
        "session-name must return the name set at construction")))

;;; ── session-clients slot ─────────────────────────────────────────────────────

(test session-clients-defaults-nil
  "session-clients defaults to NIL for a freshly created session."
  (let ((sess (make-session :id 1 :name "c")))
    (is (null (session-clients sess))
        "session-clients must default to NIL")))

;;; ── %next-window-id gap-filling ──────────────────────────────────────────────

(test next-window-id-fills-lowest-gap
  "%next-window-id returns the lowest id not yet used, starting from base-index."
  (let* ((w0   (make-window :id 0 :name "a"))
         (w2   (make-window :id 2 :name "c"))
         (sess (make-session :id 1 :name "s" :windows (list w0 w2))))
    (is (= 1 (cl-tmux/model::%next-window-id sess 0))
        "%next-window-id must return 1 (the lowest unused id)")))
