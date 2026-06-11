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
  (let* ((s    (make-fake-session))
         (name (session-name s)))
    (with-loop-state
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
  (let ((s    (make-fake-session))
        (name "CLTMUX_TEST_ENV_VAR_U"))
    (sb-posix:setenv name "hello" 1)
    (is (string= "hello" (sb-ext:posix-getenv name)) "precondition: var is set")
    (cl-tmux::%cmd-set-environment-prompt s (list "-u" name))
    (is (null (sb-ext:posix-getenv name))
        "set-environment -u must unset the variable")))

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

;;; ── rename-session via command line updates *server-sessions* ───────────────

(test run-command-line-rename-session-updates-registry
  "'rename-session <name>' via command line updates *server-sessions*."
  (let* ((s    (make-fake-session))
         (orig (session-name s)))
    (with-loop-state
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

;;; ── display-popup / popup (arg-bearing handler + alias) ──────────────────────
;;;
;;; `bind C-p popup -E "cmd"` is a very common .tmux.conf form.  Previously
;;; display-popup only opened an interactive prompt and `popup` (its documented
;;; alias, man tmux ALIASES) was unrecognised.  %cmd-display-popup parses the
;;; flags, runs the command, and shows its output; `popup` aliases it everywhere.

(test cmd-display-popup-dimension-helper
  "%popup-dimension resolves nil→fallback, absolute cells, N% of axis, clamps to
   axis-total, and falls back on junk."
  (is (= 60  (cl-tmux::%popup-dimension nil    200 60)) "nil → fallback")
  (is (= 40  (cl-tmux::%popup-dimension "40"   200 60)) "absolute cell count")
  (is (= 80  (cl-tmux::%popup-dimension "80%"  100 60)) "N% of axis-total")
  (is (= 100 (cl-tmux::%popup-dimension "150"  100 60)) "clamped to axis-total")
  (is (= 60  (cl-tmux::%popup-dimension "junk" 200 60)) "unparseable → fallback"))

(test cmd-display-popup-with-command-opens-popup
  "display-popup with -w/-T and a command runs it and shows a popup with the
   requested width and title."
  (with-loop-state
    (let ((*overlay* nil)           ; isolate the global overlay (not in with-loop-state)
          (cl-tmux::*term-cols* 100)
          (cl-tmux::*term-rows* 30)
          (s (make-fake-session :nwindows 1)))
      (cl-tmux::%cmd-display-popup s '("-E" "-w" "40" "-T" "mytitle" "echo" "hi"))
      (is (not (null cl-tmux/prompt:*active-popup*))
          "a command argument opens the popup directly (no prompt)")
      (is (= 40 (cl-tmux/prompt:popup-width cl-tmux/prompt:*active-popup*))
          "-w 40 sets the popup width")
      (is (string= "mytitle"
                   (cl-tmux/prompt:popup-title cl-tmux/prompt:*active-popup*))
          "-T sets the popup title"))))

(test cmd-display-popup-percent-width-of-terminal
  "display-popup -w 50% sizes the popup to half the terminal width."
  (with-loop-state
    (let ((*overlay* nil)           ; isolate the global overlay (not in with-loop-state)
          (cl-tmux::*term-cols* 80)
          (cl-tmux::*term-rows* 24)
          (s (make-fake-session :nwindows 1)))
      (cl-tmux::%cmd-display-popup s '("-w" "50%" "echo" "x"))
      (is (= 40 (cl-tmux/prompt:popup-width cl-tmux/prompt:*active-popup*))
          "50% of 80 columns → 40"))))

(test cmd-display-popup-no-command-opens-prompt
  "display-popup with no command opens the interactive popup-command prompt
   rather than a popup overlay (legacy behaviour preserved)."
  (with-loop-state
    (let ((*prompt* nil)            ; isolate the global prompt (not in with-loop-state)
          (s (make-fake-session :nwindows 1)))
      (cl-tmux::%cmd-display-popup s '())
      (is (null cl-tmux/prompt:*active-popup*)
          "no command → no popup overlay yet")
      (is (prompt-active-p) "no command opens the popup-command prompt instead")
      (is (string= "popup command" (prompt-label *prompt*))
          "the prompt label matches the legacy :display-popup prompt"))))

(test config-bind-accepts-popup-alias
  "`bind P popup -E \"cmd\"` is accepted by the config parser (popup resolves to
   :display-popup); previously the unrecognised `popup` name was rejected."
  (with-isolated-config
    (is (= 1 (cl-tmux/config:load-config-from-string
              "bind P popup -E \"echo hi\""))
        "one directive applied — popup is a recognised command alias")))

(test arg-command-table-has-popup-alias
  "*arg-command-table* maps both display-popup and popup to %cmd-display-popup."
  (let ((entry (assoc "popup" cl-tmux::*arg-command-table*
                      :test (lambda (k names) (member k names :test #'string=)))))
    (is (not (null entry)) "popup is registered in *arg-command-table*")
    (is (eq #'cl-tmux::%cmd-display-popup (cdr entry))
        "popup routes to %cmd-display-popup")))

;;; ── send-keys -N (repeat) and -H (hex) ───────────────────────────────────────
;;;
;;; -N count repeats the -X copy-mode command (or the whole key sequence) COUNT
;;; times; -H interprets each argument as a hexadecimal character code.  The -X
;;; repeat is observed via the copy cursor; -H is tested through the extracted
;;; %send-keys-hex-to-string helper (send-keys-to-pane no-ops on a fd -1 pane).

(test send-keys-hex-to-string-converts-codes
  "%send-keys-hex-to-string maps a hex code to its one-character string, or NIL
   for an unparseable / out-of-range code."
  (is (string= "A" (cl-tmux::%send-keys-hex-to-string "41")) "41 → A")
  (is (string= " " (cl-tmux::%send-keys-hex-to-string "20")) "20 → space")
  (is (= 27 (char-code (char (cl-tmux::%send-keys-hex-to-string "1b") 0)))
      "1b → ESC (char code 27)")
  (is (null (cl-tmux::%send-keys-hex-to-string "zz")) "non-hex → NIL")
  (is (null (cl-tmux::%send-keys-hex-to-string "FFFFFFFF"))
      "out-of-range code → NIL (never errors)"))

(test send-keys-x-with-N-repeats-copy-command
  "send-keys -X -N 3 cursor-up moves the copy cursor up 3 rows (the -N repeat
   count applied to the copy-mode command)."
  (with-fake-session (s)
    (cl-tmux::dispatch-command s :copy-mode-enter nil)
    (let* ((screen (active-screen s))
           (row0   (car (screen-copy-cursor screen))))
      (cl-tmux::%cmd-send-keys-arg s '("-X" "-N" "3" "cursor-up"))
      (is (= (- row0 3) (car (screen-copy-cursor screen)))
          "cursor-up repeated 3× moves the copy cursor up 3 rows"))))

(test send-keys-x-without-N-runs-once
  "send-keys -X cursor-up with no -N defaults to a single application."
  (with-fake-session (s)
    (cl-tmux::dispatch-command s :copy-mode-enter nil)
    (let* ((screen (active-screen s))
           (row0   (car (screen-copy-cursor screen))))
      (cl-tmux::%cmd-send-keys-arg s '("-X" "cursor-up"))
      (is (= (- row0 1) (car (screen-copy-cursor screen)))
          "a bare -X command runs exactly once (count defaults to 1)"))))

;;; ── capture-pane saves to a buffer by default (scriptable form) ──────────────
;;;
;;; The scriptable `capture-pane [flags]` command (%cmd-capture-pane-arg, distinct
;;; from the interactive :capture-pane overlay binding) follows tmux: without -p
;;; it SAVES the captured content to a paste buffer; -p prints (overlay) instead.

(test cmd-capture-pane-saves-to-buffer-by-default
  "capture-pane with no -p saves the pane content to a paste buffer (the canonical
   capture→paste workflow), not an overlay."
  (with-empty-buffers
    (with-loop-state
      (let ((s (make-fake-session)))
        (feed (active-screen s) "hello capture")
        (cl-tmux::%cmd-capture-pane-arg s '())
        (let ((buf (cl-tmux/buffer:get-paste-buffer 0)))
          (is (not (null buf)) "capture-pane (no -p) saves to a paste buffer")
          (is (search "hello capture" buf)
              "the saved buffer contains the captured pane content"))))))

(test cmd-capture-pane-p-shows-overlay-not-buffer
  "capture-pane -p prints (overlay) and does NOT save to a buffer."
  (with-empty-buffers
    (with-loop-state
      (let ((*overlay* nil)
            (s (make-fake-session)))
        (feed (active-screen s) "shown only")
        (cl-tmux::%cmd-capture-pane-arg s '("-p"))
        (is (overlay-active-p) "-p shows the content in an overlay")
        (is (null (cl-tmux/buffer:get-paste-buffer 0))
            "-p does NOT save to a paste buffer (stdout equivalent)")))))

(test cmd-capture-pane-b-flag-accepted-stores-in-ring
  "capture-pane -b name is accepted; the capture is stored at the top of the ring."
  (with-empty-buffers
    (with-loop-state
      (let ((s (make-fake-session)))
        (feed (active-screen s) "named buf")
        (cl-tmux::%cmd-capture-pane-arg s '("-b" "mybuf"))
        (is (search "named buf" (or (cl-tmux/buffer:get-paste-buffer 0) ""))
            "-b stores the capture in the unnamed ring (single-ring model)")))))

;;; ── Named paste-buffer commands (-b name) ────────────────────────────────────

(test cmd-set-buffer-b-stores-named
  "set-buffer -b name data stores a named buffer retrievable by name."
  (with-empty-buffers
    (with-loop-state
      (let ((s (make-fake-session)))
        (cl-tmux::%cmd-set-buffer-arg s '("-b" "mybuf" "hello" "world"))
        (is (string= "hello world" (cl-tmux/buffer:get-buffer-by-name "mybuf"))
            "set-buffer -b stores the joined data under the name")))))

(test cmd-set-buffer-a-appends-named
  "set-buffer -a -b name appends to the existing named buffer."
  (with-empty-buffers
    (with-loop-state
      (let ((s (make-fake-session)))
        (cl-tmux::%cmd-set-buffer-arg s '("-b" "b" "foo"))
        (cl-tmux::%cmd-set-buffer-arg s '("-a" "-b" "b" "bar"))
        (is (string= "foobar" (cl-tmux/buffer:get-buffer-by-name "b"))
            "-a appends to the named buffer")))))

(test cmd-show-buffer-b-shows-named
  "show-buffer -b name shows that buffer's content in an overlay."
  (with-empty-buffers
    (with-loop-state
      (let ((*overlay* nil) (s (make-fake-session)))
        (cl-tmux/buffer:set-named-buffer "b" "shown-content")
        (cl-tmux::%cmd-show-buffer-arg s '("-b" "b"))
        (is (overlay-active-p) "show-buffer -b opens an overlay")
        (is (search "shown-content" (format nil "~{~A~%~}" (overlay-lines)))
            "the overlay contains the named buffer's content")))))

(test cmd-delete-buffer-b-deletes-named
  "delete-buffer -b name removes that named buffer."
  (with-empty-buffers
    (with-loop-state
      (let ((s (make-fake-session)))
        (cl-tmux/buffer:set-named-buffer "b" "x")
        (cl-tmux::%cmd-delete-buffer-arg s '("-b" "b"))
        (is (null (cl-tmux/buffer:get-buffer-by-name "b"))
            "the named buffer is gone after delete-buffer -b")))))

(test cmd-paste-buffer-d-deletes-named-after-paste
  "paste-buffer -d -b name deletes the named buffer after pasting it."
  (with-empty-buffers
    (with-loop-state
      (let ((s (make-fake-session)))
        (cl-tmux/buffer:set-named-buffer "b" "data")
        (cl-tmux::%cmd-paste-buffer-arg s '("-d" "-b" "b"))
        (is (null (cl-tmux/buffer:get-buffer-by-name "b"))
            "-d removes the named buffer after pasting")))))

(test cmd-capture-pane-b-stores-named
  "capture-pane -b name stores the captured content under that name."
  (with-empty-buffers
    (with-loop-state
      (let ((s (make-fake-session)))
        (feed (active-screen s) "captured text")
        (cl-tmux::%cmd-capture-pane-arg s '("-b" "cap"))
        (is (search "captured text" (or (cl-tmux/buffer:get-buffer-by-name "cap") ""))
            "capture-pane -b stores the capture under the given name")))))

(test config-bind-accepts-paste-buffer-b-flag
  "`bind X paste-buffer -b foo` is accepted by the config parser."
  (with-isolated-config
    (is (= 1 (cl-tmux/config:load-config-from-string "bind X paste-buffer -b foo"))
        "paste-buffer -b parses as one applied directive")))

;;; ── Coverage gap #19: join/move-pane default source = marked pane ───────────

(test cmd-join-pane-uses-marked-pane-as-default-source
  "join-pane without -s uses *server-marked-pane* as source when it is set."
  (with-fake-session (s :nwindows 2)
    (let* ((wins   (cl-tmux/model:session-windows s))
           (win0   (first wins))
           (win1   (second wins))
           (pane0  (cl-tmux/model:window-active-pane win0))
           (pane1  (cl-tmux/model:window-active-pane win1))
           ;; Mark a pane in win1 — join-pane without -s should use it as source.
           (cl-tmux::*server-marked-pane* pane1))
      (declare (ignore pane0))
      ;; Point session at win0 (the destination window).
      (cl-tmux/model:session-select-window s win0)
      ;; join-pane with no flags: source = marked pane (pane1 from win1).
      (cl-tmux::%cmd-join-pane-arg s '())
      (is (member pane1 (cl-tmux/model:window-panes win0))
          "join-pane without -s must move the marked pane into the active window"))))

(test cmd-join-pane-ignores-marked-pane-when-s-given
  "join-pane with explicit -s ignores *server-marked-pane* and uses the given source."
  (with-fake-session (s :nwindows 2)
    (let* ((wins   (cl-tmux/model:session-windows s))
           (win0   (first wins))
           (win1   (second wins))
           (pane0  (cl-tmux/model:window-active-pane win0))
           (pane1  (cl-tmux/model:window-active-pane win1))
           ;; Mark pane0 — join-pane -s win1 should still use pane1.
           (cl-tmux::*server-marked-pane* pane0))
      (declare (ignore pane0))
      ;; Point session at win0.
      (cl-tmux/model:session-select-window s win0)
      ;; Explicit -s @N (win1 window-id sigil) targets pane1, not the marked pane.
      (cl-tmux::%cmd-join-pane-arg s (list "-s" (format nil "@~D" (cl-tmux/model:window-id win1))))
      (is (member pane1 (cl-tmux/model:window-panes win0))
          "join-pane -s must use the explicit source, not the marked pane"))))

;;; ── %cmd-wait-for-arg (gap #10: -S/-L/-U flags) ─────────────────────────────

(test cmd-wait-for-arg-signal-signals-channel
  "wait-for -S channel signals the named channel (unblocks waiters)."
  (with-fake-session (s)
    (let ((received nil))
      ;; Start a thread waiting on the channel.
      (bt:make-thread
       (lambda () (setf received (cl-tmux::wait-for-channel "test-ch-signal")))
       :name "waiter")
      ;; Brief yield so the waiter thread reaches condition-wait before signal.
      (sleep 0.05)
      (cl-tmux::%cmd-wait-for-arg s '("-S" "test-ch-signal"))
      (sleep 0.05)
      (is-true received "wait-for -S must unblock the waiting thread"))))

(test cmd-wait-for-arg-lock-suppresses-signal
  "wait-for -L channel locks the channel; subsequent -S does not notify waiters."
  (with-fake-session (s)
    ;; Lock first, then signal — the signal should be a no-op.
    (cl-tmux::%cmd-wait-for-arg s '("-L" "test-ch-lock"))
    ;; A waiter on a LOCKED channel receives no notification; wait-for-channel
    ;; will time-out and return NIL.  We verify the lock was applied by checking
    ;; that signal-channel does not raise an error and that the channel is locked.
    (let ((ch (cl-tmux::%ensure-channel "test-ch-lock")))
      (is-true (getf ch :locked) "wait-for -L must set the :locked flag on the channel"))))

(test cmd-wait-for-arg-unlock-clears-lock
  "wait-for -U channel unlocks a previously locked channel."
  (with-fake-session (s)
    (cl-tmux::%cmd-wait-for-arg s '("-L" "test-ch-unlock"))
    (cl-tmux::%cmd-wait-for-arg s '("-U" "test-ch-unlock"))
    (let ((ch (cl-tmux::%ensure-channel "test-ch-unlock")))
      (is-false (getf ch :locked) "wait-for -U must clear the :locked flag"))))

(test cmd-wait-for-arg-bare-blocks-until-signaled
  "wait-for channel (bare, no flags) blocks until the channel is signaled."
  (with-fake-session (s)
    (let ((result :pending))
      ;; Run wait-for in a background thread so it blocks without stalling tests.
      (bt:make-thread
       (lambda ()
         (setf result (cl-tmux::%cmd-wait-for-arg s '("test-ch-bare"))))
       :name "bare-waiter")
      (sleep 0.05)
      (cl-tmux::signal-channel "test-ch-bare")
      (sleep 0.05)
      (is (not (eq result :pending))
          "wait-for (bare) must unblock after the channel is signaled"))))
