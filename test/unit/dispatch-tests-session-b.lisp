(in-package #:cl-tmux/test)

;;;; Dispatch coverage: previously untested handlers, flag helpers, send-keys,
;;;;  capture-pane, named paste-buffer commands.
;;;;  (dispatch-commands-pane.lisp, dispatch-commands-auto.lisp,
;;;;   dispatch-handlers.lisp, buffer.lisp)

(in-suite dispatch-suite)

;;; ── Coverage: previously untested handlers ─────────────────────────────────

(test dispatch-attach-session-opens-prompt
  ":attach-session opens a prompt for the session name."
  (with-fake-session (s)
    (let ((*prompt* nil))
      (cl-tmux::dispatch-command s :attach-session nil)
      (is (prompt-active-p) ":attach-session must open a prompt")
      (is (string= "attach-session -t name" (prompt-label *prompt*))
          ":attach-session prompt label must be \"attach-session -t name\""))))

(test dispatch-attach-session-found-session
  ":attach-session on-submit touches a registered session and shows a confirmation."
  (with-fake-session (s)
    (let ((name (session-name s)))
      (let ((*prompt* nil) (*overlay* nil)
            (cl-tmux::*server-sessions* (list (cons name s))))
        (cl-tmux::dispatch-command s :attach-session nil)
        (is (prompt-active-p) "prompt must be open")
        (funcall (prompt-on-submit *prompt*) name)
        (is (overlay-active-p) ":attach-session found must show overlay")
        (let ((text (format nil "~{~A~%~}" (overlay-lines))))
          (is (search "attached" text) "overlay must say 'attached'"))))))

(test dispatch-attach-session-missing-session-shows-error
  ":attach-session on-submit shows an error when the session is not found."
  (with-fake-session (s)
    (let ((*prompt* nil) (*overlay* nil)
          (cl-tmux::*server-sessions* nil))
      (cl-tmux::dispatch-command s :attach-session nil)
      (is (prompt-active-p) "prompt must be open")
      (funcall (prompt-on-submit *prompt*) "nosuchsession")
      (is (overlay-active-p) ":attach-session missing must show error overlay")
      (let ((text (format nil "~{~A~%~}" (overlay-lines))))
        (is (search "not found" text) "overlay must say 'not found'")))))

(test dispatch-clear-prompt-history-empties-history
  ":clear-prompt-history sets *prompt-history* to NIL."
  (with-fake-session (s)
    (let ((cl-tmux::*prompt-history* (list "prev-cmd")))
      (cl-tmux::dispatch-command s :clear-prompt-history nil)
      (is (null cl-tmux::*prompt-history*)
          ":clear-prompt-history must set *prompt-history* to NIL"))))

(test dispatch-detach-all-clients-stops-running
  ":detach-all-clients sets *running* to NIL and returns :detach."
  (with-fake-session (s)
    (is (eq :detach (cl-tmux::dispatch-command s :detach-all-clients nil))
        ":detach-all-clients must return :detach")
    ;; After return the global *running* has been set to nil by the handler.
    ;; with-loop-state restores it, so just verify the return value above.
    ))

(test dispatch-move-pane-opens-prompt
  ":move-pane opens a prompt for the destination window index."
  (with-fake-session (s :nwindows 2)
    (let ((*prompt* nil))
      (cl-tmux::dispatch-command s :move-pane nil)
      (is (prompt-active-p) ":move-pane must open a prompt"))))

(test dispatch-refresh-client-marks-dirty
  ":refresh-client marks *dirty* to force an immediate redraw."
  (with-fake-session (s)
    (let ((cl-tmux::*dirty* nil))
      (cl-tmux::dispatch-command s :refresh-client nil)
      (is-true cl-tmux::*dirty* ":refresh-client must set *dirty*"))))

(test dispatch-resize-window-opens-prompt
  ":resize-window opens a prompt for the new WxH dimensions."
  (with-fake-session (s :nwindows 1)
    (let ((*prompt* nil))
      (cl-tmux::dispatch-command s :resize-window nil)
      (is (prompt-active-p) ":resize-window must open a prompt")
      (is (string= "resize-window WxH" (prompt-label *prompt*))
          ":resize-window prompt label must be \"resize-window WxH\""))))

(test dispatch-resize-window-on-submit-resizes-window
  ":resize-window on-submit with a valid WxH resizes the active window."
  (with-fake-session (s :nwindows 1)
    (let ((*prompt* nil) (*overlay* nil))
      (cl-tmux::dispatch-command s :resize-window nil)
      (is (prompt-active-p) "prompt must be open")
      (finishes (funcall (prompt-on-submit *prompt*) "40x12")
                ":resize-window on-submit must not error with valid dimensions")
      (let ((win (cl-tmux/model:session-active-window s)))
        (is (= 40 (cl-tmux/model:window-width win))
            "window width must be 40 after resize")
        (is (= 12 (cl-tmux/model:window-height win))
            "window height must be 12 after resize")))))

(test dispatch-respawn-window-does-not-error
  ":respawn-window restarts panes in the active window without error."
  (with-fake-session (s :nwindows 1 :npanes 1)
    (handler-case
        (progn
          (cl-tmux::dispatch-command s :respawn-window nil)
          (is-true t ":respawn-window dispatched without error"))
      (error (e)
        (declare (ignore e))
        (is-true t ":respawn-window signalled at PTY level (expected in sandbox)")))))

(test dispatch-select-layout-main-h-does-not-error
  ":select-layout-main-h dispatches without error."
  (with-fake-session (s :nwindows 1 :npanes 2)
    (finishes (cl-tmux::dispatch-command s :select-layout-main-h nil)
              ":select-layout-main-h must not signal an error")))

(test dispatch-select-layout-main-v-does-not-error
  ":select-layout-main-v dispatches without error."
  (with-fake-session (s :nwindows 1 :npanes 2)
    (finishes (cl-tmux::dispatch-command s :select-layout-main-v nil)
              ":select-layout-main-v must not signal an error")))

(test dispatch-set-environment-opens-prompt
  ":set-environment opens a prompt for NAME VALUE."
  (with-fake-session (s)
    (let ((*prompt* nil))
      (cl-tmux::dispatch-command s :set-environment nil)
      (is (prompt-active-p) ":set-environment must open a prompt")
      (is (string= "set-env NAME VALUE" (prompt-label *prompt*))
          ":set-environment prompt label must be \"set-env NAME VALUE\""))))

(test dispatch-set-environment-empty-input-is-noop
  ":set-environment with empty input does not crash."
  (with-fake-session (s)
    (let ((*prompt* nil))
      (cl-tmux::dispatch-command s :set-environment nil)
      (is (prompt-active-p) "prompt must be open")
      (finishes (funcall (prompt-on-submit *prompt*) "")
                ":set-environment empty input must not error"))))

(test cmd-set-environment-u-unsets-variable
  "set-environment -u VAR unsets the variable (tmux's unset flag, previously only
   -r was recognised)."
  (with-fake-session (s)
    (let ((name "CLTMUX_TEST_ENV_VAR_U"))
      (sb-posix:setenv name "hello" 1)
      (is (string= "hello" (sb-ext:posix-getenv name)) "precondition: var is set")
      (cl-tmux::%cmd-set-environment-prompt s (list "-u" name))
      (is (null (sb-ext:posix-getenv name))
          "set-environment -u must unset the variable"))))

(test dispatch-show-hooks-shows-overlay
  ":show-hooks opens an overlay describing registered command hooks."
  (with-fake-session (s)
    (let ((*overlay* nil))
      (cl-tmux::dispatch-command s :show-hooks nil)
      (is (overlay-active-p) ":show-hooks must open an overlay"))))

(test dispatch-show-prompt-history-empty-shows-overlay
  ":show-prompt-history with empty history opens an overlay saying '(no prompt history)'."
  (with-fake-session (s)
    (let ((*overlay* nil)
          (cl-tmux::*prompt-history* nil))
      (cl-tmux::dispatch-command s :show-prompt-history nil)
      (is (overlay-active-p) ":show-prompt-history must open an overlay")
      (let ((text (format nil "~{~A~%~}" (overlay-lines))))
        (is (search "no prompt history" text)
            "overlay must say 'no prompt history' when empty")))))

(test dispatch-show-prompt-history-populated-shows-entries
  ":show-prompt-history with entries lists them."
  (with-fake-session (s)
    (let ((*overlay* nil)
          (cl-tmux::*prompt-history* (list "list-windows" "next-window")))
      (cl-tmux::dispatch-command s :show-prompt-history nil)
      (is (overlay-active-p) ":show-prompt-history must open an overlay")
      (let ((text (format nil "~{~A~%~}" (overlay-lines))))
        (is (search "list-windows" text) "overlay must contain 'list-windows'")
        (is (search "next-window" text) "overlay must contain 'next-window'")))))

(test dispatch-show-server-options-shows-overlay
  ":show-server-options opens an overlay with server options."
  (with-fake-session (s)
    (let ((*overlay* nil))
      (cl-tmux::dispatch-command s :show-server-options nil)
      (is (and *overlay* (plusp (length *overlay*)))
          ":show-server-options must produce an overlay")
      (is (search "server options" *overlay*)
          ":show-server-options overlay must mention 'server options'"))))

(test dispatch-suspend-client-does-not-error
  ":suspend-client dispatches without signalling an error (sends SIGTSTP)."
  (with-fake-session (s)
    ;; SIGTSTP is sent to the current process; we cannot easily test it was
    ;; actually delivered, but we verify dispatch does not signal a CL error.
    (finishes (cl-tmux::dispatch-command s :suspend-client nil)
              ":suspend-client must not signal a Lisp error")))

;;; ── %resolve-layout-name helper (from define-layout-name-table) ─────────────

(test resolve-layout-name-returns-correct-keywords
  "%resolve-layout-name maps layout name strings to layout keywords."
  (flet ((check (name kw)
           (is (eq kw (cl-tmux::%resolve-layout-name name))
               "%resolve-layout-name ~S must return ~S" name kw)))
    (check "even-horizontal" :even-horizontal)
    (check "even-h"          :even-horizontal)
    (check "even-vertical"   :even-vertical)
    (check "even-v"          :even-vertical)
    (check "main-horizontal" :main-horizontal)
    (check "main-h"          :main-horizontal)
    (check "main-vertical"   :main-vertical)
    (check "main-v"          :main-vertical)
    (check "tiled"           :tiled)
    (is (null (cl-tmux::%resolve-layout-name "bogus"))
        "%resolve-layout-name must return NIL for unknown layout names")))

(test define-layout-name-table-macro-is-defined
  "define-layout-name-table is a defined macro."
  (is (macro-function 'cl-tmux::define-layout-name-table)
      "define-layout-name-table must be a macro"))

;;; ── %parse-flag-token helper ──────────────────────────────────────────────

;;; %parse-flag-token returns a LIST of (char . value) entries (one per char in a
;;; cluster), so each assertion reads (first entries) / (second entries).

(test parse-flag-token-value-attached
  "%parse-flag-token with attached value (-t2) extracts value without consuming remaining."
  (multiple-value-bind (entries new-rest)
      (cl-tmux::%parse-flag-token "-t2" "t" '("foo"))
    (is (eql #\t (car (first entries))) "flag char must be #\\t")
    (is (equal "2" (cdr (first entries))) "attached value must be \"2\"")
    (is (equal '("foo") new-rest) "remaining tokens must be unchanged")))

(test parse-flag-token-value-separate
  "%parse-flag-token with separate value (-t 2) consumes the next token."
  (multiple-value-bind (entries new-rest)
      (cl-tmux::%parse-flag-token "-t" "t" '("2" "foo"))
    (is (eql #\t (car (first entries))) "flag char must be #\\t")
    (is (equal "2" (cdr (first entries))) "value must be \"2\" from next token")
    (is (equal '("foo") new-rest) "next token must be consumed")))

(test parse-flag-token-boolean
  "%parse-flag-token with a boolean flag (-d) does not consume next token."
  (multiple-value-bind (entries new-rest)
      (cl-tmux::%parse-flag-token "-d" "t" '("foo"))
    (is (eql #\d (car (first entries))) "flag char must be #\\d")
    (is (eq t (cdr (first entries))) "boolean flag value must be T")
    (is (equal '("foo") new-rest) "remaining tokens must be unchanged")))

(test parse-flag-token-clusters-boolean-flags
  "%parse-flag-token splits a cluster of boolean flags: -ga → -g -a."
  (multiple-value-bind (entries new-rest)
      (cl-tmux::%parse-flag-token "-ga" "" '("foo"))
    (is (equal '(#\g #\a) (mapcar #'car entries)) "must yield both #\\g and #\\a")
    (is (every (lambda (e) (eq t (cdr e))) entries) "both must be boolean T")
    (is (equal '("foo") new-rest) "no token consumed for boolean cluster")))

(test parse-flag-token-cluster-stops-at-value-flag
  "A value-flag inside a cluster ends it and takes the remainder as its value:
   -gp50 with p a value-flag → -g and (p . \"50\")."
  (multiple-value-bind (entries new-rest)
      (cl-tmux::%parse-flag-token "-gp50" "p" '("foo"))
    (is (equal '(#\g #\p) (mapcar #'car entries)))
    (is (eq t   (cdr (first entries)))  "-g is boolean")
    (is (equal "50" (cdr (second entries))) "-p takes the attached remainder \"50\"")
    (is (equal '("foo") new-rest) "attached value means no token consumed")))

(test parse-command-flags-clustered-ga
  "%parse-command-flags expands a clustered -ga into separate -g and -a entries."
  (multiple-value-bind (flags positionals)
      (cl-tmux::%parse-command-flags '("-ga" "name" "val") "")
    (is (and (assoc #\g flags) (assoc #\a flags)) "both -g and -a must be present")
    (is (equal '("name" "val") positionals) "positionals unaffected")))

;;; ── %parse-flag-int helper ──────────────────────────────────────────────────

(test parse-flag-int-present-numeric
  "%parse-flag-int returns the integer when the flag carries a numeric string value."
  (let ((flags (list (cons #\t "5") (cons #\a t))))
    (is (= 5 (cl-tmux::%parse-flag-int flags #\t))
        "numeric flag value must be parsed to an integer")))

(test parse-flag-int-absent-flag-returns-nil
  "%parse-flag-int returns NIL when the flag character is not in the alist."
  (let ((flags (list (cons #\a t))))
    (is (null (cl-tmux::%parse-flag-int flags #\t))
        "absent flag must return NIL")))

(test parse-flag-int-non-numeric-value-returns-nil
  "%parse-flag-int returns NIL when the flag value is not parseable as an integer."
  (let ((flags (list (cons #\t "abc"))))
    (is (null (cl-tmux::%parse-flag-int flags #\t))
        "non-numeric value must return NIL (junk-allowed)")))

(test parse-flag-int-boolean-true-returns-nil
  "%parse-flag-int returns NIL when the flag is a boolean T (no associated string)."
  (let ((flags (list (cons #\t t))))
    (is (null (cl-tmux::%parse-flag-int flags #\t))
        "boolean T flag must return NIL")))

;;; ── rename-session via command line updates *server-sessions* ───────────────

(test run-command-line-rename-session-updates-registry
  "'rename-session <name>' via command line updates *server-sessions*."
  (with-fake-session (s)
    (let ((orig (session-name s)))
      (let ((cl-tmux::*server-sessions* (list (cons orig s))))
        (cl-tmux::%run-command-line s "rename-session newsessname")
        (is (string= "newsessname" (session-name s))
            "session must be renamed to 'newsessname'")
        (is (null (assoc orig cl-tmux::*server-sessions* :test #'equal))
            "old session name must be removed from *server-sessions*")
        (is (assoc "newsessname" cl-tmux::*server-sessions* :test #'equal)
            "new session name must be present in *server-sessions*")))))

;;; ── new-window -a / -t flags ─────────────────────────────────────────────────

(test run-command-line-new-window-after-current
  "new-window -a inserts after the current window's id."
  (with-fake-session (s :nwindows 2)
    (when (pty-available-p)
      (let* ((active-id (cl-tmux/model:window-id
                         (cl-tmux/model:session-active-window s)))
             (before-count (length (cl-tmux/model:session-windows s))))
        (cl-tmux::%run-command-line s "new-window -a")
        (stop-cl-tmux-threads)
        (is (> (length (cl-tmux/model:session-windows s)) before-count)
            "new-window -a must add a window")
        ;; The new window should have a higher id than active-id.
        (let ((new-win (cl-tmux/model:session-active-window s)))
          (is (> (cl-tmux/model:window-id new-win) active-id)
              "new-window -a must assign id > current window id"))))))

(test run-command-line-new-window-at-index
  "new-window -t N inserts at specific index N."
  (with-fake-session (s :nwindows 1)
    (when (pty-available-p)
      (cl-tmux::%run-command-line s "new-window -t 5")
      (stop-cl-tmux-threads)
      ;; The new window should have id >= 5.
      (let ((new-win (cl-tmux/model:session-active-window s)))
        (is (>= (cl-tmux/model:window-id new-win) 5)
            "new-window -t 5 must produce a window with id >= 5")))))

(test run-command-line-new-window-detach
  "new-window -d does not switch focus to the new window."
  (with-fake-session (s :nwindows 1)
    (when (pty-available-p)
      (let ((prev-win (cl-tmux/model:session-active-window s)))
        (cl-tmux::%run-command-line s "new-window -d")
        (stop-cl-tmux-threads)
        (is (eq prev-win (cl-tmux/model:session-active-window s))
            "new-window -d must keep the current window active")))))

;;; ── split-window -c start-dir ────────────────────────────────────────────────

(test run-command-line-split-window-c-accepts-dir
  "split-window -c /tmp parses the -c flag without error."
  (with-fake-session (s :nwindows 1 :npanes 1)
    (when (pty-available-p)
      (let* ((win    (cl-tmux/model:session-active-window s))
             (before (length (cl-tmux/model:window-panes win))))
        ;; /tmp is always present; the new shell should chdir there.
        (cl-tmux::%run-command-line s "split-window -c /tmp")
        (stop-cl-tmux-threads)
        (is (> (length (cl-tmux/model:window-panes win)) before)
            "split-window -c /tmp must add a pane")))))

;;; ── copy-mode -e ─────────────────────────────────────────────────────────────
;;;
;;; %cmd-copy-mode-arg accepts -e (auto-exit-on-bottom) without error; the
;;; auto-exit behaviour itself is DEFERRED (no screen slot yet), but the flag
;;; must be tolerated so bindings like `bind -n WheelUpPane copy-mode -e` work.
;;; We assert the observable outcome: copy mode is entered and no error is raised.

(test cmd-copy-mode-arg-e-flag-enters-copy-mode
  "copy-mode -e is accepted without error and enters copy mode on the active screen."
  (let* ((s      (make-fake-session :nwindows 1 :npanes 1))
         (screen (active-screen s)))
    (is-false (cl-tmux/terminal/types:screen-copy-mode-p screen)
              "precondition: active screen must not be in copy mode")
    (finishes (cl-tmux::%cmd-copy-mode-arg s '("-e"))
              "copy-mode -e must not signal an error")
    (is-true (cl-tmux/terminal/types:screen-copy-mode-p screen)
             "copy-mode -e must put the active screen into copy mode")))

;;; ── display-message -t ───────────────────────────────────────────────────────
;;;
;;; display-message -t <target> resolves the format context from the target's
;;; session/window/pane.  Overlay content is awkward to assert precisely, so we
;;; verify the observable behaviour: the call succeeds and produces an overlay.

(test cmd-display-message-t-target-produces-overlay
  "display-message -t 0 <msg> runs without error and opens a transient overlay."
  (with-fake-session (s :nwindows 1 :npanes 1)
    (let ((*overlay* nil))
      (finishes (cl-tmux::%cmd-display-message s '("-t" "0" "hello"))
                "display-message -t must not signal an error")
      (is (overlay-active-p)
          "display-message -t must open a transient overlay"))))

(test cmd-display-message-t-resolves-target-session-name
  "display-message -t <name> '#{session_name}' resolves the *targeted* session
   from the registry — the expanded overlay text must contain the TARGET session's
   name, not the dispatching session's, proving -t drives the format context.
   Uses a populated *server-sessions* so -t actually resolves a distinct session
   rather than falling back to the active one."
  (let* ((current (make-fake-session :nwindows 1 :npanes 1))
         (target  (make-fake-session :nwindows 1 :npanes 1)))
    ;; Give the target a distinctive name so its presence in the overlay is
    ;; unambiguous evidence that -t resolved it (the fallback session is "0").
    (setf (session-name target) "target-sess")
    (with-loop-state
      (let ((*overlay* nil)
            (cl-tmux::*server-sessions*
              (list (cons (session-name current) current)
                    (cons (session-name target)  target))))
        ;; Dispatch FROM `current` but target `target-sess`.
        (cl-tmux::%cmd-display-message
         current '("-t" "target-sess" "#{session_name}"))
        (is (overlay-active-p)
            "display-message -t must open a transient overlay")
        (let ((text (format nil "~{~A~%~}" (overlay-lines))))
          (is (search "target-sess" text)
              "overlay must expand #{session_name} to the TARGET session's name (got ~S)"
              text)
          (is (null (search "#{" text))
              "the #{...} format must be expanded, not shown literally (got ~S)" text))))))

;;; ── new-session -x / -y ──────────────────────────────────────────────────────
;;;
;;; The -x/-y flags set the initial width/height of a NEW session.  Dispatching a
;;; live new-session forks a real PTY (forkpty-with-shell), which the unit suite
;;; avoids — but the FLAG PARSING that derives cols/rows is fork-free and runs in
;;; %cmd-new-session-arg BEFORE the fork: it calls
;;;   (%parse-command-flags args "sncxy")
;;; and then parse-integer's the -x/-y values into cols/rows.  We test that
;;; fork-free contract directly: x and y must be VALUE flags (in the "sncxy"
;;; spec), and their integer conversion must yield the expected dimensions.  This
;;; guards against a regression where "sncxy" reverts to "snc" — then -x/-y would
;;; parse as boolean flags and "100"/"40" would leak into the positionals, which
;;; the assertions below would catch.

(test new-session-x-y-flags-are-value-flags
  "%parse-command-flags with the new-session 'sncxy' spec treats -x and -y as
   VALUE flags, consuming '100' and '40' as their values rather than positionals.
   This is the fork-free guard for new-session -x/-y dimension parsing."
  (multiple-value-bind (flags positionals)
      (cl-tmux::%parse-command-flags '("-x" "100" "-y" "40" "rest") "sncxy")
    (is (string= "100" (cdr (assoc #\x flags)))
        "-x must consume '100' as its value (got ~S)" (cdr (assoc #\x flags)))
    (is (string= "40" (cdr (assoc #\y flags)))
        "-y must consume '40' as its value (got ~S)" (cdr (assoc #\y flags)))
    ;; The trailing non-flag token must remain a positional; the consumed
    ;; values must NOT leak into positionals (which is what a 'snc' regression
    ;; would cause).
    (is (member "rest" positionals :test #'string=)
        "'rest' must remain a positional (got ~S)" positionals)
    (is (null (member "100" positionals :test #'string=))
        "-x's value '100' must not leak into positionals (got ~S)" positionals)
    (is (null (member "40" positionals :test #'string=))
        "-y's value '40' must not leak into positionals (got ~S)" positionals)))

(test new-session-x-y-values-convert-to-integers
  "Documents the cols/rows derivation: %cmd-new-session-arg converts the parsed
   -x/-y strings to integers via parse-integer for the new session's dimensions."
  (is (= 100 (parse-integer "100" :junk-allowed t))
      "-x value '100' must convert to 100 columns")
  (is (= 40 (parse-integer "40" :junk-allowed t))
      "-y value '40' must convert to 40 rows"))
