(in-package #:cl-tmux/test)

;;;; Arg-taking command dispatch tests
;;;;  (src/dispatch-commands.lisp, dispatch-commands-lifecycle.lisp,
;;;;   dispatch-commands-pane.lisp, dispatch-commands-shell.lisp,
;;;;   dispatch-commands-auto.lisp, dispatch-commands-runner.lisp).

(in-suite dispatch-suite)

;;; ── %parse-command-flags + -t target commands ───────────────────────────────

(test parse-command-flags-value-and-boolean
  "%parse-command-flags separates -t<value> flags, boolean flags, and positionals."
  (flet ((flags (toks vf)
           (multiple-value-bind (f p) (cl-tmux::%parse-command-flags toks vf)
             (declare (ignore p)) f))
         (pos (toks vf)
           (multiple-value-bind (f p) (cl-tmux::%parse-command-flags toks vf)
             (declare (ignore f)) p)))
    (is (equal "2" (cdr (assoc #\t (flags '("-t" "2") "t"))))
        "-t 2 (separate) → value \"2\"")
    (is (equal "2" (cdr (assoc #\t (flags '("-t2") "t"))))
        "-t2 (attached) → value \"2\"")
    (is (eq t (cdr (assoc #\d (flags '("-d") "t"))))
        "-d (not a value flag) → boolean T")
    (is (equal '("foo" "bar") (pos '("-d" "foo" "-t" "2" "bar") "t"))
        "non-flag tokens are positionals in order")))

(test run-command-line-select-window-by-number
  "'select-window -t N' selects the window whose window-id is N."
  (with-fake-session (s :nwindows 3)
    (cl-tmux::%run-command-line s "select-window -t 2")
    (is (= 2 (window-id (session-active-window s)))
        "select-window -t 2 must activate window-id 2")))

(test run-command-line-select-window-by-name
  "'select-window -t <name>' selects the window with that (non-numeric) name."
  (with-fake-session (s :nwindows 2)
    (setf (window-name (second (session-windows s))) "alpha")
    (cl-tmux::%run-command-line s "select-window -t alpha")
    (is (string= "alpha" (window-name (session-active-window s)))
        "select-window -t alpha must activate the window named 'alpha'")))

(test run-command-line-select-window-T-toggles-to-last
  "'select-window -T -t N' toggles to the last window when already on window N,
   but selects N normally when not currently on it."
  (let* ((s  (make-fake-session :nwindows 2))   ; window-ids 0,1
         (w0 (first  (cl-tmux/model:session-windows s)))
         (w1 (second (cl-tmux/model:session-windows s))))
    (with-loop-state
      ;; session-last-window is recency-based (window-last-active-time, 1s
      ;; granularity); seed distinct OLD stamps so the two same-second selects
      ;; below produce an unambiguous last-window order (w0 older than w1's NOW).
      (setf (cl-tmux/model:window-last-active-time w0) 100
            (cl-tmux/model:window-last-active-time w1) 50)
      ;; From w0, -T -t 1 is NOT on the target → select w1 normally.
      (cl-tmux::%run-command-line s "select-window -T -t 1")
      (is (eq w1 (session-active-window s))
          "-T when not on the target must select the target (w1)")
      ;; Now on w1 (w0 is last); -T -t 1 IS on the target → toggle to last (w0).
      (cl-tmux::%run-command-line s "select-window -T -t 1")
      (is (eq w0 (session-active-window s))
          "-T when already on the target must toggle to the last window (w0)"))))

(test run-command-line-select-pane-by-id
  "'select-pane -t N' selects the pane with pane-id N in the active window."
  (let* ((s   (make-fake-session :nwindows 1 :npanes 2))
         (win (session-active-window s)))
    ;; make-fake-window panes have ids 1,2; the first is active.
    (with-loop-state
      (is (= 1 (pane-id (window-active-pane win))) "pane 1 is active initially")
      (cl-tmux::%run-command-line s "select-pane -t 2")
      (is (= 2 (pane-id (window-active-pane win)))
          "select-pane -t 2 must activate pane-id 2"))))

(test run-command-line-select-pane-by-pane-id-sigil
  "'select-pane -t %2' (the %N pane-id sigil) selects pane-id 2, like -t 2."
  (let* ((s   (make-fake-session :nwindows 1 :npanes 2))
         (win (session-active-window s)))
    (with-loop-state
      (is (= 1 (pane-id (window-active-pane win))) "pane 1 is active initially")
      (cl-tmux::%run-command-line s "select-pane -t %2")
      (is (= 2 (pane-id (window-active-pane win)))
          "select-pane -t %2 must activate pane-id 2 via the %N sigil"))))

(test run-command-line-select-pane-l-selects-last
  "'select-pane -l' returns to the previously active pane."
  (let* ((s   (make-fake-session :nwindows 1 :npanes 2))
         (win (session-active-window s)))
    (with-loop-state
      ;; Start on pane 1, move to pane 2 (pane 1 becomes last-active).
      (cl-tmux::%run-command-line s "select-pane -t 2")
      (is (= 2 (pane-id (window-active-pane win))) "now on pane 2")
      (cl-tmux::%run-command-line s "select-pane -l")
      (is (= 1 (pane-id (window-active-pane win)))
          "select-pane -l must return to the previously active pane (1)"))))

(test run-command-line-select-pane-t-T-titles-target-pane
  "'select-pane -t N -T title' sets pane N's title, NOT the active pane's."
  (let* ((s   (make-fake-session :nwindows 1 :npanes 2))
         (win (session-active-window s))
         (p1  (window-active-pane win))                       ; pane 1 (active)
         (p2  (find 2 (window-panes win) :key #'pane-id)))
    (with-loop-state
      (cl-tmux::%run-command-line s "select-pane -t 2 -T mytitle")
      (is (string= "mytitle" (cl-tmux/model:pane-title p2))
          "select-pane -t 2 -T must set pane 2's title")
      (is (not (string= "mytitle" (cl-tmux/model:pane-title p1)))
          "the active pane (1) title must be unchanged"))))

(test run-command-line-select-pane-t-d-disables-target-pane-input
  "'select-pane -t N -d' disables input on pane N, NOT the active pane."
  (let* ((s   (make-fake-session :nwindows 1 :npanes 2))
         (win (session-active-window s))
         (p1  (window-active-pane win))
         (p2  (find 2 (window-panes win) :key #'pane-id)))
    (with-loop-state
      (cl-tmux::%run-command-line s "select-pane -t 2 -d")
      (is-true  (cl-tmux/model:pane-input-disabled p2) "pane 2 input disabled")
      (is-false (cl-tmux/model:pane-input-disabled p1) "active pane 1 unaffected"))))

(test run-command-line-select-pane-M-clears-mark
  "'select-pane -M' on a marked pane clears the server-wide mark (toggle)."
  (let* ((s   (make-fake-session :nwindows 1 :npanes 2))
         (win (session-active-window s)))
    (with-loop-state
      (let ((ap (window-active-pane win)))
        (cl-tmux::dispatch-command s :mark-pane nil)
        (is (pane-marked ap) "pane must be marked before select-pane -M")
        (cl-tmux::%run-command-line s "select-pane -M")
        (is (null (pane-marked ap))
            "select-pane -M must clear the pane mark")))))

;;; ── kill-window / kill-pane with -t target ───────────────────────────────────

(test run-command-line-kill-window-by-target
  "'kill-window -t N' kills the window whose window-id is N."
  (with-fake-session (s :nwindows 3)
    (cl-tmux::%run-command-line s "kill-window -t 1")
    (is (= 2 (length (session-windows s))) "one window must be removed")
    (is (null (find 1 (session-windows s) :key #'window-id))
        "window-id 1 must be gone")))

(test run-command-line-kill-pane-by-target
  "'kill-pane -t N' kills the pane with pane-id N in the active window."
  (let* ((s   (make-fake-session :nwindows 1 :npanes 2))
         (win (session-active-window s)))   ; pane-ids 1,2
    (with-loop-state
      (cl-tmux::%run-command-line s "kill-pane -t 2")
      (is (= 1 (length (window-panes win))) "one pane must be removed")
      (is (null (find 2 (window-panes win) :key #'pane-id))
          "pane-id 2 must be gone"))))

(test run-command-line-kill-pane-invalid-target-is-noop
  "'kill-pane -t <nonexistent>' must NOT kill the active pane by accident."
  (let* ((s   (make-fake-session :nwindows 1 :npanes 2))
         (win (session-active-window s)))
    (with-loop-state
      (cl-tmux::%run-command-line s "kill-pane -t 99")
      (is (= 2 (length (window-panes win)))
          "no pane may be removed for a -t target that matches nothing"))))

(test run-command-line-kill-window-no-arg-kills-active
  "'kill-window' with no -t kills the active window (name-table fallthrough)."
  (let* ((s      (make-fake-session :nwindows 2))
         (active (session-active-window s)))
    (with-loop-state
      (cl-tmux::%run-command-line s "kill-window")
      (is (= 1 (length (session-windows s))) "the active window must be removed")
      (is (null (find active (session-windows s)))
          "the previously active window must be gone"))))

;;; ── swap-window -s -t (two value flags) ──────────────────────────────────────

(test run-command-line-swap-window-exchanges-indices
  "'swap-window -s X -t Y' exchanges the two windows' INDEX NUMBERS (ids): the
   content at X and Y trade indices, the list stays sorted by id."
  (let* ((s  (make-fake-session :nwindows 3))   ; ids 0,1,2
         (w0 (find 0 (session-windows s) :key #'window-id))
         (w1 (find 1 (session-windows s) :key #'window-id))
         (w2 (find 2 (session-windows s) :key #'window-id)))
    (with-loop-state
      (cl-tmux::%run-command-line s "swap-window -s 0 -t 2")
      (is (= 2 (window-id w0)) "window formerly #0 now has index 2")
      (is (= 0 (window-id w2)) "window formerly #2 now has index 0")
      (is (= 1 (window-id w1)) "the middle window keeps index 1")
      (is (equal '(0 1 2) (mapcar #'window-id (session-windows s)))
          "the window list stays sorted by index"))))

(test run-command-line-swap-window-default-source-is-active
  "'swap-window -t Y' uses the active window as the source."
  (let* ((s      (make-fake-session :nwindows 2))   ; active = id 0
         (active (session-active-window s))
         (w1     (find 1 (session-windows s) :key #'window-id)))
    (with-loop-state
      (cl-tmux::%run-command-line s "swap-window -t 1")
      (is (= 1 (window-id active)) "the active window's index becomes 1")
      (is (= 0 (window-id w1)) "the other window's index becomes 0"))))

(test run-command-line-swap-window-unknown-target-is-noop
  "'swap-window -s 0 -t 99' (no such dst) leaves the window indices unchanged."
  (let* ((s           (make-fake-session :nwindows 3))
         (ids-before  (mapcar #'window-id (session-windows s))))
    (with-loop-state
      (cl-tmux::%run-command-line s "swap-window -s 0 -t 99")
      (is (equal ids-before (mapcar #'window-id (session-windows s)))
          "a -t target that matches nothing must not change indices"))))

;;; ── arg-taking key bindings + source-file ────────────────────────────────────

(test dispatch-prefix-bound-command-line-runs
  "A key bound to a command line runs it: bind X display-message hi, then prefix+X
   shows 'hi' in an overlay (verifies dispatch-prefix-command's token-list path)."
  (with-isolated-config
    (with-loop-state
      (let ((s (make-fake-session)) (*overlay* nil))
        (cl-tmux/config:apply-config-directive '("bind" "X" "display-message" "hi"))
        (cl-tmux::dispatch-prefix-command s (char-code #\X))
        (is (overlay-active-p) "the bound display-message must open an overlay")
        (let ((text (format nil "~{~A~%~}" (overlay-lines))))
          (is (search "hi" text)
              "overlay must contain the bound command's output 'hi' (got ~S)" text))))))

(test cmd-source-file-loads-config-file
  "source-file <path> loads the file and applies its directives (end-to-end with
   the set -g fix)."
  (with-isolated-options ()
    (let ((path (format nil "/tmp/cl-tmux-srcfile-~D.conf" (get-universal-time))))
      (unwind-protect
           (progn
             (with-open-file (out path :direction :output :if-exists :supersede
                                       :if-does-not-exist :create)
               (write-line "set -g status off" out))
             (cl-tmux::%run-command-line (make-fake-session)
                                         (format nil "source-file ~A" path))
             (is (string= "off" (cl-tmux/options:get-option "status"))
                 "source-file must apply 'set -g status off' from the file"))
        (ignore-errors (delete-file path))))))

(test cmd-source-file-missing-path-no-crash
  "source-file on a non-existent path is a safe no-op (no error signalled)."
  (finishes (cl-tmux::%run-command-line (make-fake-session)
                                        "source-file /no/such/cl-tmux-file.conf")
            "source-file on a missing file must not signal"))

;;; ── move-window -t <n> (renumber the active window) ──────────────────────────

(test run-command-line-move-window-to-free-number
  "'move-window -t N' renumbers the active window to window-id N when N is free."
  (let* ((s   (make-fake-session :nwindows 2))   ; window-ids 0,1; active = 0
         (win (session-active-window s)))
    (with-loop-state
      (cl-tmux::%run-command-line s "move-window -t 5")
      (is (= 5 (window-id win))
          "active window must be renumbered to window-id 5"))))

(test run-command-line-move-window-to-taken-number-shifts-up
  "'move-window -t N' onto a taken index moves the window there and shifts the
   occupant up (tmux winlink_shuffle_up) — not a silent no-op."
  (let* ((s   (make-fake-session :nwindows 2))   ; ids 0 (active),1
         (win (session-active-window s))
         (w1  (find 1 (session-windows s) :key #'window-id)))
    (with-loop-state
      (cl-tmux::%run-command-line s "move-window -t 1")   ; 1 is taken
      (is (= 1 (window-id win))
          "the active window takes the requested index 1")
      (is (= 2 (window-id w1))
          "the window formerly at 1 shifts up to 2"))))

;;; ── if-shell -F <cond> <then> [<else>] (format-conditional) ──────────────────

(test run-command-line-if-shell-F-true-runs-then
  "if-shell -F with a truthy condition runs the THEN command line."
  (with-fake-session (s)
    (let ((*overlay* nil))
      (cl-tmux::%run-command-line s "if-shell -F 1 \"display-message yes\"")
      (is (overlay-active-p) "truthy if-shell -F must run THEN (overlay opens)")
      (is (search "yes" (format nil "~{~A~%~}" (overlay-lines)))
          "overlay must show the THEN command's output"))))

(test run-command-line-if-shell-F-false-runs-else
  "if-shell -F with a falsey condition (\"0\") runs the ELSE command line."
  (with-fake-session (s)
    (let ((*overlay* nil))
      (cl-tmux::%run-command-line
       s "if-shell -F 0 \"display-message yes\" \"display-message no\"")
      (is (search "no" (format nil "~{~A~%~}" (overlay-lines)))
          "falsey if-shell -F must run ELSE, not THEN"))))

(test run-command-line-if-shell-F-format-condition
  "if-shell -F evaluates a #{...} format as its condition.  A non-zero value is
   truthy.  We use #{window_count}=1 (truthy) rather than #{window_index} (which
   equals the window id; with base-index=0 the first window has id 0 = falsey)."
  (with-fake-session (s)
    (let ((*overlay* nil))
      (cl-tmux::%run-command-line
       s "if-shell -F \"#{window_count}\" \"display-message named\"")
      (is (search "named" (format nil "~{~A~%~}" (overlay-lines)))
          "a non-zero #{window_count} (1) must be truthy → THEN"))))

(test run-command-line-if-shell-F-empty-condition-no-then
  "if-shell -F with an empty condition and no else runs nothing."
  (with-fake-session (s)
    (let ((*overlay* nil))
      (cl-tmux::%run-command-line s "if-shell -F \"\" \"display-message x\"")
      (is (not (overlay-active-p))
          "an empty (falsey) condition with no else must not run THEN"))))

;;; ── %dispatch-named-command helper ──────────────────────────────────────────

(test dispatch-named-command-new-window
  "%dispatch-named-command \"next-window\" selects the next window."
  ;; Use next-window (no fork/no thread) to avoid leaking reader threads
  ;; that would prevent later PTY tests from forking.
  (with-fake-session (s :nwindows 2)
    (let ((*overlay* nil))
      (cl-tmux::%dispatch-named-command s "next-window")
      (is-true cl-tmux::*dirty*
               "%dispatch-named-command 'next-window' must mark *dirty*")
      (is (eq (second (session-windows s)) (session-active-window s))
          "next-window must switch to the second window"))))

(test dispatch-named-command-unknown-shows-overlay
  "%dispatch-named-command with an unrecognized name shows an overlay."
  (with-fake-session (s)
    (let ((*overlay* nil))
      (cl-tmux::%dispatch-named-command s "totally-unknown-xyz")
      (is (overlay-active-p) "unknown command must open an error overlay")
      (let ((text (format nil "~{~A~%~}" (overlay-lines))))
        (is (search "unknown command" text) "overlay must mention 'unknown command'")
        (is (search "totally-unknown-xyz" text)
            "overlay must include the bad command name")))))

;;; ── :show-messages dispatch ──────────────────────────────────────────────────

(test dispatch-show-messages-empty-log-shows-overlay
  ":show-messages with empty *message-log* opens an overlay saying '(no messages)'."
  (with-fake-session (s)
    (let ((*overlay* nil)
          (cl-tmux::*message-log* nil))
      (cl-tmux::dispatch-command s :show-messages nil)
      (is (overlay-active-p) ":show-messages must open an overlay")
      (let ((text (format nil "~{~A~%~}" (overlay-lines))))
        (is (search "no messages" text)
            "overlay must say '(no messages)' when log is empty")))))

(test dispatch-show-messages-populated-log-shows-entries
  ":show-messages with entries in *message-log* lists them."
  (with-fake-session (s)
    (let ((*overlay* nil)
          (cl-tmux::*message-log* (list (cons 0 "hello") (cons 1 "world"))))
      (cl-tmux::dispatch-command s :show-messages nil)
      (is (overlay-active-p) ":show-messages must open an overlay")
      (let ((text (format nil "~{~A~%~}" (overlay-lines))))
        (is (search "hello" text) "overlay must contain 'hello'")
        (is (search "world" text) "overlay must contain 'world'")))))

;;; ── :display-message logs to *message-log* ───────────────────────────────────

(test dispatch-display-message-logs-to-message-log
  ":display-message on-submit calls add-message-log, appending the message."
  (with-fake-session (s)
    (let ((*prompt* nil) (*overlay* nil)
          (cl-tmux::*message-log* nil))
      (cl-tmux::dispatch-command s :display-message nil)
      (is (prompt-active-p) ":display-message must open a prompt")
      ;; Submit a non-empty message.
      (funcall (prompt-on-submit *prompt*) "test-log-entry")
      (is-false (null cl-tmux::*message-log*)
                "*message-log* must be non-nil after submitting a message")
      (let ((last-msg (cdr (first cl-tmux::*message-log*))))
        (is (string= "test-log-entry" last-msg)
            "first message-log entry must be the submitted message (got ~S)" last-msg)))))

;;; ── :clock-mode dispatch ─────────────────────────────────────────────────────

(test dispatch-clock-mode-toggles-pane-id
  ":clock-mode sets *clock-mode-pane-id* to the active pane's id."
  (with-fake-session (s)
    (let ((cl-tmux::*clock-mode-pane-id* nil))
      (cl-tmux::dispatch-command s :clock-mode nil)
      (let ((ap (session-active-pane s)))
        (is (eql (pane-id ap) cl-tmux::*clock-mode-pane-id*)
            "*clock-mode-pane-id* must be set to active pane id after first :clock-mode")
        ;; Toggle off
        (cl-tmux::dispatch-command s :clock-mode nil)
        (is (null cl-tmux::*clock-mode-pane-id*)
            "*clock-mode-pane-id* must be nil after second :clock-mode (toggle off)")))))

;;; ── :capture-pane dispatch ───────────────────────────────────────────────────

(test dispatch-capture-pane-shows-overlay
  ":capture-pane opens an overlay containing the pane content."
  (with-fake-session (s)
    (let ((*overlay* nil))
      ;; Feed some text into the active pane's screen.
      (let ((ap (session-active-pane s)))
        (when ap
          (feed (pane-screen ap) "CAPTEST")))
      (cl-tmux::dispatch-command s :capture-pane nil)
      (is (overlay-active-p) ":capture-pane must open an overlay")
      (let ((text (format nil "~{~A~%~}" (overlay-lines))))
        (is (search "CAPTEST" text)
            "capture-pane overlay must contain the pane's fed content")))))

;;; ── :send-keys dispatch ──────────────────────────────────────────────────────

(test dispatch-send-keys-opens-prompt
  ":send-keys opens a prompt for the keys string."
  (with-fake-session (s)
    (let ((*prompt* nil))
      (cl-tmux::dispatch-command s :send-keys nil)
      (is (prompt-active-p) ":send-keys must open a prompt")
      (is (string= "send-keys" (prompt-label *prompt*))
          ":send-keys prompt label must be \"send-keys\""))))

(test dispatch-send-keys-no-crash-with-no-pty
  ":send-keys on-submit with a no-PTY pane (fd=-1) does not signal an error."
  (with-fake-session (s)
    (let ((*prompt* nil))
      (cl-tmux::dispatch-command s :send-keys nil)
      (is (prompt-active-p) "prompt must be open")
      ;; Submitting keys to fd=-1 pane must not error.
      (finishes (funcall (prompt-on-submit *prompt*) "hello")
                "send-keys on-submit must not error with fd=-1 pane"))))

;;; ── :choose-tree dispatch ────────────────────────────────────────────────────

(test dispatch-choose-tree-shows-overlay
  ":choose-tree opens an overlay with session and window entries."
  (with-fake-session (s :nwindows 2)
    (let ((*overlay* nil)
          (cl-tmux::*server-sessions* nil))
      (cl-tmux::dispatch-command s :choose-tree nil)
      (is (overlay-active-p) ":choose-tree must open an overlay")
      (let ((text (format nil "~{~A~%~}" (overlay-lines))))
        (is (search (session-name s) text)
            "overlay must contain the session name")))))

(test dispatch-choose-tree-with-server-sessions
  ":choose-tree with multiple server sessions lists them all."
  (let* ((s1 (make-fake-session :nwindows 1))
         (s2 (make-fake-session :nwindows 2))
         (reg (list (cons (session-name s1) s1)
                    (cons (session-name s2) s2))))
    (with-loop-state
      (let ((*overlay* nil)
            (cl-tmux::*server-sessions* reg))
        (cl-tmux::dispatch-command s1 :choose-tree nil)
        (is (overlay-active-p) ":choose-tree must open an overlay with server sessions")
        (let ((text (format nil "~{~A~%~}" (overlay-lines))))
          (is (search (session-name s1) text)
              "overlay must contain session 1 name")
          (is (search (session-name s2) text)
              "overlay must contain session 2 name"))))))

;;; ── :set-window-option dispatch ──────────────────────────────────────────────

(test dispatch-set-window-option-opens-prompt
  ":set-window-option opens a prompt."
  (with-fake-session (s)
    (let ((*prompt* nil))
      (cl-tmux::dispatch-command s :set-window-option nil)
      (is (prompt-active-p) ":set-window-option must open a prompt")
      (is (string= "set-window-option" (prompt-label *prompt*))
          ":set-window-option prompt label must be \"set-window-option\""))))

;;; ── :set-session-option dispatch ─────────────────────────────────────────────

(test dispatch-set-session-option-opens-prompt
  ":set-session-option opens a prompt."
  (with-fake-session (s)
    (let ((*prompt* nil))
      (cl-tmux::dispatch-command s :set-session-option nil)
      (is (prompt-active-p) ":set-session-option must open a prompt")
      (is (string= "set-session-option" (prompt-label *prompt*))
          ":set-session-option prompt label must be \"set-session-option\""))))

;;; ── :confirm-before dispatch ─────────────────────────────────────────────────
;;;
;;; confirm-before is implemented in dispatch.lisp as a prompt with an
;;; on-submit lambda.  The helper below eliminates repetition across the input
;;; variants.

(defmacro with-confirm-before-prompt ((on-submit-var) &body body)
  "Activate a confirm-before prompt in an isolated environment and bind
   ON-SUBMIT-VAR to the prompt's on-submit function, then execute BODY."
  `(let ((sess (make-fake-session)))
     (with-loop-state
       (with-clean-prompt
         (let ((*overlay* nil))
           (cl-tmux::dispatch-command sess :confirm-before nil)
           (is-true (prompt-active-p)
                    "dispatch-command :confirm-before must activate a prompt")
           (let ((,on-submit-var (prompt-on-submit *prompt*)))
             ,@body))))))

(test confirm-before-y-dispatches
  "dispatch-command :confirm-before activates a prompt; submitting \"y\" shows the overlay."
  (with-confirm-before-prompt (on-submit)
    (funcall on-submit "y")
    (is (overlay-active-p)
        "on-submit with \"y\" must show the [confirmed] overlay")))

(test confirm-before-n-cancels
  "dispatch-command :confirm-before activates a prompt; submitting \"n\" does NOT show overlay."
  (with-confirm-before-prompt (on-submit)
    (funcall on-submit "n")
    (is (null (overlay-active-p))
        "on-submit with \"n\" must NOT show the overlay")))

(test confirm-before-empty-cancels
  "dispatch-command :confirm-before activates a prompt; submitting \"\" does NOT show overlay."
  (with-confirm-before-prompt (on-submit)
    (funcall on-submit "")
    (is (null (overlay-active-p))
        "on-submit with empty string must NOT show the overlay")))

(test confirm-before-arg-is-single-key-and-y-runs-command
  "confirm-before COMMAND opens a SINGLE-KEY prompt: one 'y' keypress (no Enter)
   runs the command."
  (with-isolated-config
    (with-loop-state
      (let ((cl-tmux/prompt:*prompt* nil)
            (s (make-fake-session)))
        (cl-tmux::%cmd-confirm-before-arg s '("set" "-g" "status-left" "YES"))
        (is (prompt-active-p) "confirm-before must open a prompt")
        (is-true (prompt-single-key *prompt*) "the prompt must be single-key")
        (cl-tmux::handle-prompt-key (char-code #\y))   ; single key, no Enter
        (is (null (prompt-active-p)) "prompt must dismiss after the single key")
        (is (string= "YES" (cl-tmux/options:get-option "status-left"))
            "'y' must run the confirmed command")))))

(test confirm-before-arg-single-key-other-cancels
  "A non-y single key cancels confirm-before without running the command."
  (with-isolated-config
    (with-loop-state
      (let ((cl-tmux/prompt:*prompt* nil)
            (s (make-fake-session)))
        (cl-tmux/options:set-option "status-left" "ORIG")
        (cl-tmux::%cmd-confirm-before-arg s '("set" "-g" "status-left" "YES"))
        (cl-tmux::handle-prompt-key (char-code #\n))   ; 'n' cancels
        (is (null (prompt-active-p)) "prompt must dismiss on a non-y key")
        (is (string= "ORIG" (cl-tmux/options:get-option "status-left"))
            "a non-y key must NOT run the command")))))

(test command-prompt-1-single-key-substitutes-one-keypress
  "command-prompt -1 -p k: 'set -g status-left %1' is a single-key prompt: one
   keypress (no Enter) is substituted for %1 and the command runs."
  (with-isolated-config
    (with-loop-state
      (let ((cl-tmux/prompt:*prompt* nil)
            (s (make-fake-session)))
        (cl-tmux::%cmd-command-prompt-arg
         s '("-1" "-p" "k:" "set -g status-left %1"))
        (is (prompt-active-p) "command-prompt -1 must open a prompt")
        (is-true (prompt-single-key *prompt*) "the prompt must be single-key")
        (cl-tmux::handle-prompt-key (char-code #\Z))   ; one key, no Enter
        (is (null (prompt-active-p)) "prompt must dismiss after one key")
        (is (string= "Z" (cl-tmux/options:get-option "status-left"))
            "%1 must be substituted with the single keypress 'Z'")))))

;;; ── %set-option-from-prompt helper ──────────────────────────────────────────

(test set-option-from-prompt-helper-opens-prompt
  "%set-option-from-prompt opens a prompt with the given label."
  (with-loop-state
    (let ((*prompt* nil))
      (cl-tmux::%set-option-from-prompt "test-label")
      (is (prompt-active-p) "%set-option-from-prompt must open a prompt")
      (is (string= "test-label" (prompt-label *prompt*))
          "%set-option-from-prompt prompt label must match the argument"))))

(test set-option-from-prompt-sets-option
  "%set-option-from-prompt on-submit with 'name value' calls set-option."
  (with-loop-state
    (let ((*prompt* nil))
      (cl-tmux::%set-option-from-prompt "set-window-option")
      (is (prompt-active-p) "prompt must be open")
      ;; Submit "mouse on" which maps to set-option "mouse" "on"
      (finishes (funcall (prompt-on-submit *prompt*) "mouse on")
                "%set-option-from-prompt on-submit must not error"))))

;;; ── %paste-to-pane helper ────────────────────────────────────────────────────

(test paste-to-pane-no-crash-fd-minus-one
  "%paste-to-pane with fd=-1 pane is a no-op (guard skips the write)."
  (let* ((screen (make-screen 20 5))
         (pane   (make-pane :id 1 :fd -1 :pid -1 :x 0 :y 0 :width 20 :height 5
                            :screen screen)))
    (finishes (cl-tmux::%paste-to-pane pane "hello world")
              "%paste-to-pane must not error with fd=-1 pane")))

(test paste-to-pane-nil-text-is-noop
  "%paste-to-pane with nil text is a no-op."
  (let* ((screen (make-screen 20 5))
         (pane   (make-pane :id 1 :fd -1 :pid -1 :x 0 :y 0 :width 20 :height 5
                            :screen screen)))
    (finishes (cl-tmux::%paste-to-pane pane nil)
              "%paste-to-pane with nil text must not error")))

;;; ── %format-tree-entry helper ────────────────────────────────────────────────

(test format-tree-entry-marks-current-session
  "%format-tree-entry marks the current session with an asterisk."
  (let* ((screen (make-screen 20 5))
         (pane   (make-pane :id 1 :fd -1 :pid -1 :x 0 :y 0 :width 20 :height 5
                            :screen screen))
         (win    (make-window :id 0 :name "test-win" :width 20 :height 5
                              :panes (list pane)
                              :tree  (make-layout-leaf pane))))
    (window-select-pane win pane)
    (let ((output
            (with-output-to-string (s)
              (cl-tmux::%format-tree-entry s "mysess" "mysess"
                                          (list win) win))))
      (is (search "* mysess" output)
          "current session must be marked with '* ' prefix")
      (is (search "test-win" output)
          "window name must appear in the output"))))

(test format-tree-entry-non-current-session-uses-space
  "%format-tree-entry uses '  ' prefix for non-current sessions."
  (let* ((screen (make-screen 20 5))
         (pane   (make-pane :id 1 :fd -1 :pid -1 :x 0 :y 0 :width 20 :height 5
                            :screen screen))
         (win    (make-window :id 0 :name "w" :width 20 :height 5
                              :panes (list pane)
                              :tree  (make-layout-leaf pane))))
    (window-select-pane win pane)
    (let ((output
            (with-output-to-string (s)
              (cl-tmux::%format-tree-entry s "other" "current"
                                          (list win) win))))
      (is-false (search "* other" output)
                "non-current session must not start with '* '")
      (is (search "  other" output)
          "non-current session must start with '  '"))))

;;; ── :choose-session / :list-sessions-full aliases ────────────────────────────

(test dispatch-choose-session-shows-session-list
  ":choose-session shows the session list overlay (same body as :list-sessions)."
  (with-fake-session (s :nwindows 1)
    (let ((*overlay* nil)
          (cl-tmux::*server-sessions* nil))
      (cl-tmux::dispatch-command s :choose-session nil)
      (is (overlay-active-p) ":choose-session must open an overlay")
      (let ((text (format nil "~{~A~%~}" (overlay-lines))))
        (is (search (session-name s) text)
            ":choose-session overlay must contain the session name")))))

(test dispatch-list-sessions-full-shows-session-list
  ":list-sessions-full shows the session list overlay."
  (with-fake-session (s :nwindows 1)
    (let ((*overlay* nil)
          (cl-tmux::*server-sessions* nil))
      (cl-tmux::dispatch-command s :list-sessions-full nil)
      (is (overlay-active-p) ":list-sessions-full must open an overlay"))))

;;; ── :resize-left/:resize-right/:resize-up/:resize-down dispatch ──────────────

(test dispatch-resize-commands-do-not-error
  "The four resize commands dispatch without signalling an error."
  (with-fake-session (s)
    (dolist (cmd '(:resize-left :resize-right :resize-up :resize-down))
      (finishes (cl-tmux::dispatch-command s cmd nil)
                "~A must not signal an error" cmd))))

;;; ── :rotate-window / :rotate-window-reverse dispatch ─────────────────────────

(test dispatch-rotate-window-does-not-error
  ":rotate-window dispatches without error on a single-pane window."
  (with-fake-session (s :nwindows 1 :npanes 1)
    (finishes (cl-tmux::dispatch-command s :rotate-window nil)
              ":rotate-window must not signal an error")))

(test dispatch-rotate-window-reverse-does-not-error
  ":rotate-window-reverse dispatches without error on a single-pane window."
  (with-fake-session (s :nwindows 1 :npanes 1)
    (finishes (cl-tmux::dispatch-command s :rotate-window-reverse nil)
              ":rotate-window-reverse must not signal an error")))

;;; ── :split-horizontal / :split-vertical (no-focus) dispatch ─────────────────

(test dispatch-split-horizontal-no-focus-does-not-error
  ":split-horizontal-no-focus dispatches without error."
  (with-fake-session (s :nwindows 1 :npanes 1)
    (finishes (cl-tmux::dispatch-command s :split-horizontal-no-focus nil)
              ":split-horizontal-no-focus must not signal an error")))

(test dispatch-split-vertical-no-focus-does-not-error
  ":split-vertical-no-focus dispatches without error."
  (with-fake-session (s :nwindows 1 :npanes 1)
    (finishes (cl-tmux::dispatch-command s :split-vertical-no-focus nil)
              ":split-vertical-no-focus must not signal an error")))

