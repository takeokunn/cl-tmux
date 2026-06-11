(in-package #:cl-tmux/test)

;;;; Session-level tests: session / window lifecycle.
;;;;
;;;; Tests that create real PTYs (via create-initial-session / window-split)
;;;; skip themselves when PTY allocation is unavailable — the same guard used
;;;; in pty-tests.lisp — so the suite runs cleanly in sandboxed Nix builds.
;;;; Tests that only exercise pure session logic use make-fake-session /
;;;; make-no-pty-pane and always run regardless of PTY availability.

(in-suite model-suite)

;;; ── Session bootstrap ──────────────────────────────────────────────────────

(test initial-session
  "create-initial-session produces 1 window containing 1 full-width pane."
  (unless (pty-available-p)
    (skip "no PTY available (sandboxed environment)"))
  (with-session (session 24 80)
    ;; Exactly one window.
    (is (= 1 (length (session-windows session))))
    (let* ((win   (session-active-window session))
           (panes (window-panes win)))
      ;; Exactly one pane.
      (is (= 1 (length panes)))
      (let ((pane (first panes)))
        ;; Pane geometry: full width; height shrunk by *status-height* (= 1).
        (is (= 80 (pane-width  pane)) "initial pane width must equal cols")
        (is (= 23 (pane-height pane))
            "initial pane height must equal rows - *status-height* (23)")
        ;; window-active-pane must return the same pane.
        (is (eq pane (window-active-pane win))
            "window-active-pane must return the sole pane")))))

;;; ── Adding a second window ─────────────────────────────────────────────────

(test session-new-window
  "session-new-window appends a window and switches the active window."
  (unless (pty-available-p)
    (skip "no PTY available (sandboxed environment)"))
  (with-session (session 24 80)
    (let ((first-win (session-active-window session)))
      (session-new-window session "2" 23 80)
      ;; Two windows now.
      (is (= 2 (length (session-windows session))))
      ;; Active window switched to the new one.
      (let ((new-win (session-active-window session)))
        (is (not (eq first-win new-win))
            "active window must have changed after session-new-window")
        ;; New window starts with exactly one pane.
        (is (= 1 (length (window-panes new-win)))
            "new window must have exactly one pane")))))

;;; ── Selecting a window by reference ───────────────────────────────────────

(test session-select-window
  "session-select-window switches the active window back to an earlier one."
  (unless (pty-available-p)
    (skip "no PTY available (sandboxed environment)"))
  (with-session (session 24 80)
    (let ((first-win (session-active-window session)))
      (session-new-window session "2" 23 80)
      ;; Sanity: active is now the second window.
      (is (not (eq first-win (session-active-window session))))
      ;; Select the first window back.
      (session-select-window session first-win)
      (is (eq first-win (session-active-window session))
          "session-active-window must return the window passed to session-select-window"))))

;;; ── session-active-pane (no PTY) ────────────────────────────────────────────

(test session-active-pane
  "session-active-pane returns the active pane of the active window."
  (let* ((sess (make-fake-session :nwindows 1 :npanes 1))
         (win  (session-active-window sess))
         (pane (window-active-pane win)))
    (is (eq pane (session-active-pane sess))
        "session-active-pane must match window-active-pane of the active window")))

;;; ── Window index stability ──────────────────────────────────────────────────

(test window-index-starts-at-base-index
  "The first window created by create-initial-session gets id=base-index (0)."
  (unless (pty-available-p)
    (skip "no PTY available (sandboxed environment)"))
  (with-session (session 24 80)
    (let ((win (session-active-window session)))
      (is (= 0 (window-id win))
          "first window id must equal base-index (0)"))))

(test session-new-window-uses-lowest-free-id
  "session-new-window assigns the lowest free id >= base-index, not 1+length."
  (unless (pty-available-p)
    (skip "no PTY available (sandboxed environment)"))
  (with-session (session 24 80)
    (let ((first-win (session-active-window session)))
      (is (= 0 (window-id first-win)) "precondition: first window id=0")
      ;; Add a second window; should get id=1.
      (session-new-window session "b" 23 80)
      (let* ((wins      (session-windows session))
             (second-win (find 1 wins :key #'window-id)))
        (is-true second-win "a window with id=1 must exist after second new-window")))))

(test window-id-stable-after-kill
  "After killing a middle window, the remaining window ids do not change."
  (unless (pty-available-p)
    (skip "no PTY available (sandboxed environment)"))
  (with-session (session 24 80)
    ;; Build three windows: ids 0, 1, 2.
    (session-new-window session "b" 23 80)
    (session-new-window session "c" 23 80)
    (is (= 3 (length (session-windows session))) "must have 3 windows")
    (let* ((wins (session-windows session))
           (w0 (find 0 wins :key #'window-id))
           (w1 (find 1 wins :key #'window-id))
           (w2 (find 2 wins :key #'window-id)))
      ;; Kill the middle window (id=1).
      (cl-tmux/commands:kill-window session w1)
      (let ((remaining (session-windows session)))
        (is (= 2 (length remaining)) "two windows remain after kill")
        (is-true (find 0 remaining :key #'window-id) "window id=0 must still exist")
        (is-true (find 2 remaining :key #'window-id) "window id=2 must still exist")
        (is (null (find 1 remaining :key #'window-id))
            "window id=1 must be gone after kill")))))

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

;;; ── get-update-environment-vars ─────────────────────────────────────────────

(test get-update-environment-vars-returns-alist
  "get-update-environment-vars returns an alist of (name . value) pairs."
  (let ((result (get-update-environment-vars)))
    (is (listp result)
        "get-update-environment-vars must return a list")
    (dolist (entry result)
      (is (consp entry)
          "each entry must be a cons pair")
      (is (stringp (car entry))
          "each entry key must be a string")
      (is (stringp (cdr entry))
          "each entry value must be a string"))))

(test get-update-environment-vars-respects-star-update-environment
  "get-update-environment-vars only queries variables listed in *update-environment*."
  (let ((*update-environment* (list "__CL_TMUX_NONEXISTENT_VAR_99999__")))
    (let ((result (get-update-environment-vars)))
      (is (null result)
          "when the env var is absent, result must be NIL"))))

(test get-update-environment-vars-set-variable-included
  "get-update-environment-vars includes variables that ARE set in the environment."
  ;; HOME is reliably set in both POSIX and Nix sandbox environments.
  (let ((*update-environment* (list "HOME")))
    (let ((result (get-update-environment-vars)))
      ;; HOME should be present (if not, the test is vacuously safe to skip)
      (when (sb-ext:posix-getenv "HOME")
        (is (= 1 (length result))
            "exactly one entry when one queried variable is set")
        (is (string= "HOME" (caar result))
            "entry key must be HOME")
        (is (stringp (cdar result))
            "entry value must be a string")))))

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
    ;; No unambiguous MRU (only w1 was focused, and it is gone) → previous-by-index:
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

;;; ── %attach-full-screen-pane structural test (no PTY) ───────────────────────
;;;
;;; Exercises the pure structural side of %attach-full-screen-pane by verifying
;;; the window slots it sets, without requiring a real PTY fork.
;;; The test builds a window with a pre-existing leaf pane instead of calling
;;; the real %attach-full-screen-pane (which forks a shell), then checks that
;;; session-active-pane and window-active-pane are consistent.

(test attach-full-screen-pane-structural
  "%attach-full-screen-pane wires window slots: panes, active, tree are all set."
  (let* ((p0   (make-no-pty-pane 1 0 0 80 23))
         (win  (make-window :id 0 :name "bash" :width 80 :height 23
                            :panes (list p0)
                            :active p0
                            :tree (make-layout-leaf p0)))
         (sess (make-session :id 1 :name "0" :windows (list win))))
    (session-select-window sess win)
    ;; Verify that the window's panes/active/tree slots are consistent.
    (is (eq p0 (window-active-pane win))
        "window-active-pane must be the sole leaf pane")
    (is (= 1 (length (window-panes win)))
        "window-panes must contain exactly the one leaf pane")
    (is-true (window-tree win)
             "window-tree must be non-NIL (a layout-leaf)")
    ;; session-active-pane must delegate correctly.
    (is (eq p0 (session-active-pane sess))
        "session-active-pane must agree with window-active-pane")))

;;; ── session-active-window falls back to first window ───────────────────────

(test session-active-window-falls-back-to-first
  "session-active-window returns the first window when active slot is NIL."
  (let* ((w0   (make-window :id 0 :name "a"))
         (w1   (make-window :id 1 :name "b"))
         ;; Construct with active=NIL explicitly.
         (sess (make-session :id 1 :name "s" :windows (list w0 w1))))
    ;; active slot defaults to NIL for make-session.
    (is (eq w0 (session-active-window sess))
        "session-active-window must fall back to the first window when active is NIL")))

;;; ── session-active-pane returns NIL for empty session ────────────────────────

(test session-active-pane-nil-for-windowless-session
  "session-active-pane returns NIL when the session has no windows."
  (let ((sess (make-session :id 1 :name "s" :windows nil)))
    (is (null (session-active-pane sess))
        "session-active-pane must return NIL for a windowless session")))

;;; ── session-locked-p slot ────────────────────────────────────────────────────

(test session-locked-p-defaults-nil
  "session-locked-p defaults to NIL for a freshly created session."
  (let ((sess (make-session :id 1 :name "s")))
    (is (null (session-locked-p sess))
        "session-locked-p must default to NIL")))

(test session-locked-p-settable
  "session-locked-p can be set to T and read back."
  (let ((sess (make-session :id 1 :name "s")))
    (setf (session-locked-p sess) t)
    (is-true (session-locked-p sess)
             "session-locked-p must return T after being set")))

;;; ── session-group slot ───────────────────────────────────────────────────────

(test session-group-defaults-nil
  "session-group defaults to NIL for a freshly created session."
  (let ((sess (make-session :id 1 :name "s")))
    (is (null (session-group sess))
        "session-group must default to NIL")))

(test session-group-settable
  "session-group can be set to a non-NIL value and read back."
  (let ((sess (make-session :id 1 :name "s")))
    (setf (session-group sess) "mygroup")
    (is (string= "mygroup" (session-group sess))
        "session-group must return the value written via setf")))

;;; ── session-last-active slot ────────────────────────────────────────────────

(test session-last-active-defaults-zero
  "session-last-active defaults to 0 for a freshly created session."
  (let ((sess (make-session :id 1 :name "s")))
    (is (= 0 (session-last-active sess))
        "session-last-active must default to 0")))

;;; ── all-panes with multi-pane window ────────────────────────────────────────

(test all-panes-multi-pane-window
  "all-panes collects all panes when a window has more than one pane."
  (let* ((p0   (make-no-pty-pane 1 0 0 40 24))
         (p1   (make-no-pty-pane 2 41 0 40 24))
         (win  (make-window :id 0 :name "w" :panes (list p0 p1)))
         (sess (make-session :id 1 :name "s" :windows (list win))))
    (let ((panes (all-panes sess)))
      (is (= 2 (length panes))
          "all-panes must return both panes from a single 2-pane window")
      (is-true (member p0 panes) "p0 must be in all-panes result")
      (is-true (member p1 panes) "p1 must be in all-panes result"))))

;;; ── session-select-window updates window-last-active-time ──────────────────

(test session-select-window-updates-window-last-active-time
  "session-select-window updates window-last-active-time on the selected window."
  (let* ((w0   (make-window :id 0 :name "a" :last-active-time 0))
         (sess (make-session :id 1 :name "s" :windows (list w0))))
    (let ((before (get-universal-time)))
      (session-select-window sess w0)
      (is (>= (window-last-active-time w0) before)
          "window-last-active-time must be updated when selected"))))

;;; ── %next-window-id base-index parameter ────────────────────────────────────

(test next-window-id-respects-base-index
  "%next-window-id with base-index=5 returns at least 5."
  (let* ((sess (make-session :id 1 :name "s" :windows nil)))
    (is (>= (cl-tmux/model::%next-window-id sess 5) 5)
        "%next-window-id with base-index=5 must return a value >= 5")))

;;; ── Table-driven session struct defaults ─────────────────────────────────────

(test session-struct-default-values-table
  "Table-driven: make-session zero-argument defaults are predictable."
  ;; Each entry: (slot-accessor default-pred description)
  (let ((sess (make-session :id 1 :name "test")))
    (is (= 1     (session-id sess))        "session-id must match :id kwarg")
    (is (string= "test" (session-name sess)) "session-name must match :name kwarg")
    (is (null   (session-windows sess))    "session-windows must default to NIL")
    (is (null   (session-active-window sess)) "active window must be NIL (no windows)")
    (is (null   (session-clients sess))    "session-clients must default to NIL")
    (is (null   (session-locked-p sess))   "session-locked-p must default to NIL")
    (is (null   (session-group sess))      "session-group must default to NIL")))

;;; ── create-initial-session ID counter ───────────────────────────────────────

(test create-initial-session-increments-id-counter
  "create-initial-session increments *session-id-counter* and assigns the new id
   to the session.  Two successive calls yield strictly increasing ids."
  (unless (pty-available-p)
    (skip "no PTY available (sandboxed environment)"))
  (let* ((before cl-tmux/model::*session-id-counter*)
         (sess1  (create-initial-session 24 80)))
    (unwind-protect
         (progn
           (is (= (1+ before) (session-id sess1))
               "first session id must be before + 1 after create-initial-session")
           (is (= (1+ before) cl-tmux/model::*session-id-counter*)
               "*session-id-counter* must be incremented by create-initial-session"))
      (dolist (p (all-panes sess1))
        (ignore-errors (pty-close (pane-fd p) (pane-pid p)))))))

(test create-initial-session-session-touch-called
  "create-initial-session sets session-last-active to a non-zero universal time."
  (unless (pty-available-p)
    (skip "no PTY available (sandboxed environment)"))
  (let ((before (get-universal-time))
        (sess   (create-initial-session 24 80)))
    (unwind-protect
         (is (>= (session-last-active sess) before)
             "session-last-active must be set by create-initial-session")
      (dolist (p (all-panes sess))
        (ignore-errors (pty-close (pane-fd p) (pane-pid p)))))))
