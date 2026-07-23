(in-package #:cl-tmux/test)

;;;; Dispatch session tests: flag parsers and target/new-session command cases.

(describe "dispatch-suite"

  ;;; ── %resolve-layout-name helper (from define-layout-name-table) ─────────────

  ;; %resolve-layout-name maps canonical layout names to layout keywords.
  (it "resolve-layout-name-returns-correct-keywords"
    (check-table
     (list (list (cl-tmux::%resolve-layout-name "even-horizontal")
                 :even-horizontal
                 "canonical even-horizontal")
           (list (cl-tmux::%resolve-layout-name "even-vertical")
                 :even-vertical
                 "canonical even-vertical")
           (list (cl-tmux::%resolve-layout-name "main-horizontal")
                 :main-horizontal
                 "canonical main-horizontal")
           (list (cl-tmux::%resolve-layout-name "main-vertical")
                 :main-vertical
                 "canonical main-vertical")
           (list (cl-tmux::%resolve-layout-name "tiled")
                 :tiled
                 "canonical tiled"))
     :test #'eq)
    (dolist (name '("even-h" "even-v" "main-h" "main-v" "bogus"))
      (expect (null (cl-tmux::%resolve-layout-name name)))))

  ;; define-layout-name-table is a defined macro.
  (it "define-layout-name-table-macro-is-defined"
    (expect (macro-function 'cl-tmux::define-layout-name-table)))

  ;;; ── %parse-flag-token helper ──────────────────────────────────────────────

  ;;; %parse-flag-token returns a LIST of (char . value) entries (one per char in a
  ;;; cluster), so each assertion reads (first entries) / (second entries).

  ;; %parse-flag-token handles attached values, separate values, and boolean flags.
  (it "parse-flag-token-simple-table"
    (dolist (row '(("-t2" "t" ("foo")      #\t "2"  ("foo") "attached value -t2")
                   ("-t"  "t" ("2" "foo")  #\t "2"  ("foo") "separate value -t 2")
                   ("-d"  "t" ("foo")      #\d t     ("foo") "boolean flag -d")))
      (destructuring-bind (token value-flags rest expected-char expected-val expected-rest desc) row
        (declare (ignore desc))
        (multiple-value-bind (entries new-rest)
            (cl-tmux::%parse-flag-token token value-flags rest)
          (expect (equal expected-char (car (first entries))))
          (expect (equal expected-val  (cdr (first entries))))
          (expect (equal expected-rest new-rest))))))

  ;; %parse-flag-token splits a cluster of boolean flags: -ga → -g -a.
  (it "parse-flag-token-clusters-boolean-flags"
    (multiple-value-bind (entries new-rest)
        (cl-tmux::%parse-flag-token "-ga" "" '("foo"))
      (expect (equal '(#\g #\a) (mapcar #'car entries)))
      (expect (every (lambda (e) (eq t (cdr e))) entries))
      (expect (equal '("foo") new-rest))))

  ;; A value-flag inside a cluster ends it and takes the remainder as its value:
  ;; -gp50 with p a value-flag → -g and (p . "50").
  (it "parse-flag-token-cluster-stops-at-value-flag"
    (multiple-value-bind (entries new-rest)
        (cl-tmux::%parse-flag-token "-gp50" "p" '("foo"))
      (expect (equal '(#\g #\p) (mapcar #'car entries)))
      (expect (eq t   (cdr (first entries))))
      (expect (equal "50" (cdr (second entries))))
      (expect (equal '("foo") new-rest))))

  ;; %parse-command-flags expands a clustered -ga into separate -g and -a entries.
  (it "parse-command-flags-clustered-ga"
    (multiple-value-bind (flags positionals)
        (cl-tmux::%parse-command-flags '("-ga" "name" "val") "")
      (expect (and (assoc #\g flags) (assoc #\a flags)))
      (expect (equal '("name" "val") positionals))))

  ;;; ── %parse-flag-int helper ──────────────────────────────────────────────────

  ;; %parse-flag-int returns the integer for a numeric flag, NIL for absent/non-numeric/boolean flags.
  (it "parse-flag-int-table"
    (dolist (c '((((#\t . "5") (#\a . t)) #\t 5   "present numeric → integer")
                 (((#\a . t))              #\t nil "absent flag → NIL")
                 (((#\t . "abc"))          #\t nil "non-numeric value → NIL")
                 (((#\t . t))              #\t nil "boolean T flag → NIL")))
      (destructuring-bind (flags char expected desc) c
        (declare (ignore desc))
        (expect (equal expected (cl-tmux::%parse-flag-int flags char))))))

  ;;; ── shared target resolvers ─────────────────────────────────────────────────

  ;; %resolve-pane-in-window resolves bare and sigil pane ids, and falls back to the active pane.
  (it "resolve-pane-in-window-resolves-id-and-falls-back"
    (with-fake-session (s :nwindows 1 :npanes 2)
      (let* ((win  (session-active-window s))
             (pane0 (window-active-pane win))
             (pane1 (find-if-not (lambda (pane) (eq pane pane0))
                                 (window-panes win)))
             (pane1-id (format nil "~A" (pane-id pane1))))
        (expect (eq pane1 (cl-tmux::%resolve-pane-in-window win pane1-id)))
        (expect (eq pane1 (cl-tmux::%resolve-pane-in-window win (format nil "%~A" pane1-id))))
        (expect (eq pane0 (cl-tmux::%resolve-pane-in-window win "not-a-pane")))
        (expect (eq pane0 (cl-tmux::%resolve-pane-in-window win nil))))))

  ;; %resolve-window-target resolves window ids, shorthand names, and returns NIL for garbage.
  (it "resolve-window-target-resolves-id-and-name"
    (with-fake-session (s :nwindows 2)
      (let* ((wins (session-windows s))
             (w1   (second wins))
             (w1-id (format nil "~A" (window-id w1)))
             (w1-name "shell"))
        (setf (window-name w1) w1-name)
        (expect (eq w1 (cl-tmux::%resolve-window-target s w1-id)))
        (expect (eq w1 (cl-tmux::%resolve-window-target s w1-name)))
        (expect (eq w1 (cl-tmux::%resolve-window-target s ":+")))
        (expect (null (cl-tmux::%resolve-window-target s "no-such-window"))))))

  ;;; ── rename-session via command line updates *server-sessions* ───────────────

  ;; 'rename-session <name>' via command line updates *server-sessions*.
  (it "run-command-line-rename-session-updates-registry"
    (with-fake-session (s)
      (let ((orig (session-name s)))
        (let ((cl-tmux::*server-sessions* (list (cons orig s))))
          (cl-tmux::%run-command-line s "rename-session newsessname")
          (expect (string= "newsessname" (session-name s)))
          (expect (null (assoc orig cl-tmux::*server-sessions* :test #'equal)))
          (expect (assoc "newsessname" cl-tmux::*server-sessions* :test #'equal))))))

  ;;; ── new-window -a / -t flags ─────────────────────────────────────────────────

  ;; new-window -a inserts after the current window's id.
  (it "run-command-line-new-window-after-current"
    (with-fake-session (s :nwindows 2)
      (when (pty-available-p)
        (let* ((active-id (cl-tmux/model:window-id
                           (cl-tmux/model:session-active-window s)))
               (before-count (length (cl-tmux/model:session-windows s))))
          (cl-tmux::%run-command-line s "new-window -a")
          (stop-cl-tmux-threads)
          (expect (> (length (cl-tmux/model:session-windows s)) before-count))
          ;; The new window should have a higher id than active-id.
          (let ((new-win (cl-tmux/model:session-active-window s)))
            (expect (> (cl-tmux/model:window-id new-win) active-id)))))))

  ;; new-window -t N inserts at specific index N.
  (it "run-command-line-new-window-at-index"
    (with-fake-session (s :nwindows 1)
      (when (pty-available-p)
        (cl-tmux::%run-command-line s "new-window -t 5")
        (stop-cl-tmux-threads)
        ;; The new window should have id >= 5.
        (let ((new-win (cl-tmux/model:session-active-window s)))
          (expect (>= (cl-tmux/model:window-id new-win) 5))))))

  ;; new-window -d does not switch focus to the new window.
  (it "run-command-line-new-window-detach"
    (with-fake-session (s :nwindows 1)
      (when (pty-available-p)
        (let ((prev-win (cl-tmux/model:session-active-window s)))
          (cl-tmux::%run-command-line s "new-window -d")
          (stop-cl-tmux-threads)
          (expect (eq prev-win (cl-tmux/model:session-active-window s)))))))

  ;;; ── split-window -c start-dir ────────────────────────────────────────────────

  ;; split-window -c /tmp parses the -c flag without error.
  (it "run-command-line-split-window-c-accepts-dir"
    (with-fake-session (s :nwindows 1 :npanes 1)
      (when (pty-available-p)
        (let* ((win    (cl-tmux/model:session-active-window s))
               (before (length (cl-tmux/model:window-panes win))))
          ;; /tmp is always present; the new shell should chdir there.
          (cl-tmux::%run-command-line s "split-window -c /tmp")
          (stop-cl-tmux-threads)
          (expect (> (length (cl-tmux/model:window-panes win)) before))))))

  ;;; ── copy-mode -e ─────────────────────────────────────────────────────────────
  ;;;
  ;;; %cmd-copy-mode-arg accepts -e (auto-exit-on-bottom) without error; the
  ;;; auto-exit behaviour itself is DEFERRED (no screen slot yet), but the flag
  ;;; must be tolerated so bindings like `bind -n WheelUpPane copy-mode -e` work.
  ;;; We assert the observable outcome: copy mode is entered and no error is raised.

  ;; copy-mode -e is accepted without error and enters copy mode on the active screen.
  (it "cmd-copy-mode-arg-e-flag-enters-copy-mode"
    (let* ((s      (make-fake-session :nwindows 1 :npanes 1))
           (screen (active-screen s)))
      (expect (cl-tmux/terminal/types:screen-copy-mode-p screen) :to-be-falsy)
      (finishes (cl-tmux::%cmd-copy-mode-arg s '("-e"))
                "copy-mode -e must not signal an error")
      (expect (cl-tmux/terminal/types:screen-copy-mode-p screen) :to-be-truthy)))

  ;;; ── display-message -t ───────────────────────────────────────────────────────
  ;;;
  ;;; display-message -t <target> resolves the format context from the target's
  ;;; session/window/pane.  Overlay content is awkward to assert precisely, so we
  ;;; verify the observable behaviour: the call succeeds and produces an overlay.

  ;; display-message -t 0 <msg> runs without error and opens a transient overlay.
  (it "cmd-display-message-t-target-produces-overlay"
    (with-fake-session (s :nwindows 1 :npanes 1)
      (let ((*overlay* nil))
        (finishes (cl-tmux::%cmd-display-message s '("-t" "0" "hello"))
                  "display-message -t must not signal an error")
        (assert-overlay-active
            "display-message -t must open a transient overlay"))))

  ;; display-message -t <name> '#{session_name}' resolves the *targeted* session
  ;; from the registry — the expanded overlay text must contain the TARGET session's
  ;; name, not the dispatching session's, proving -t drives the format context.
  ;; Uses a populated *server-sessions* so -t actually resolves a distinct session
  ;; rather than falling back to the active one.
  (it "cmd-display-message-t-resolves-target-session-name"
    (with-fake-session (current :nwindows 1 :npanes 1)
      (let ((target (make-fake-session :nwindows 1 :npanes 1)))
        ;; Give the target a distinctive name so its presence in the overlay is
        ;; unambiguous evidence that -t resolved it (the fallback session is "0").
        (setf (session-name target) "target-sess")
        (let ((*overlay* nil)
              (cl-tmux::*server-sessions*
                (list (cons (session-name current) current)
                      (cons (session-name target)  target))))
          ;; Dispatch FROM `current` but target `target-sess`.
          (cl-tmux::%cmd-display-message
           current '("-t" "target-sess" "#{session_name}"))
          (assert-overlay-active
              "display-message -t must open a transient overlay")
          (assert-overlay-contains "target-sess" *overlay*
                                   "display-message -t overlay")
          (assert-overlay-not-contains "#{" *overlay*
                                       "display-message -t overlay")))))

  ;;; ── new-session -x / -y ──────────────────────────────────────────────────────
  ;;;
  ;;; The -x/-y flags set the initial width/height of a NEW session.  Dispatching a
  ;;; live new-session forks a real PTY (forkpty-with-shell), which the unit suite
  ;;; avoids — but the FLAG PARSING that derives cols/rows is fork-free and runs in
  ;;; %cmd-new-session-arg BEFORE the fork: it calls
  ;;;   (%parse-command-flags args "sncxyteF")
  ;;; and then resolves the -x/-y values into cols/rows.  We test that fork-free
  ;;; contract directly: x and y must be VALUE flags (in the "sncxyteF" spec), and
  ;;; the resulting detached-session dimensions must come from the parsed size
  ;;; string.  This guards against a regression where "sncxyteF" reverts to "snc" —
  ;;; then -x/-y would parse as boolean flags and "100"/"40" would leak into the
  ;;; positionals, which the assertions below would catch.

  ;; %parse-command-flags with the new-session 'sncxyteF' spec treats -x and -y as
  ;; VALUE flags, consuming '100' and '40' as their values rather than positionals.
  ;; This is the fork-free guard for new-session -x/-y dimension parsing.
  (it "new-session-x-y-flags-are-value-flags"
    (multiple-value-bind (flags positionals)
        (cl-tmux::%parse-command-flags '("-x" "100" "-y" "40" "rest") "sncxyteF")
      (expect (string= "100" (alist-value #\x flags)))
      (expect (string= "40" (alist-value #\y flags)))
      ;; The trailing non-flag token must remain a positional; the consumed
      ;; values must NOT leak into positionals (which is what a 'snc' regression
      ;; would cause).
      (assert-member "rest" positionals
                     :test #'string=
                     :context "new-session positionals")
      (assert-not-member "100" positionals
                         :test #'string=
                         :context "new-session positionals")
      (assert-not-member "40" positionals
                         :test #'string=
                         :context "new-session positionals")))

  ;; new-session rejects unsupported client-detach flags before any fork.
  (it "new-session-rejects-client-only-flags"
    (with-fake-session (s)
      (dolist (args '(("-X")
                      ("-D")
                      ("-f" "flags")))
        (let ((cl-tmux::*overlay* nil))
          (expect (null (cl-tmux::%cmd-new-session-arg s args)))
          (assert-overlay-contains "new-session: unsupported argument"
                                   cl-tmux::*overlay*
                                   args))))))
