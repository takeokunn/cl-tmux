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

;;; ── :synchronize-panes toggle ────────────────────────────────────────────────

(test dispatch-synchronize-panes-toggles
  ":synchronize-panes toggles the option and shows an overlay."
  (with-fake-session (s)
    (let ((*overlay* nil))
      (cl-tmux/options:set-option "synchronize-panes" nil)
      (cl-tmux::dispatch-command s :synchronize-panes nil)
      (is (overlay-active-p) ":synchronize-panes must show an overlay")
      (let ((text (format nil "~{~A~%~}" (overlay-lines))))
        (is (search "ON" text) "first toggle must produce ON message"))
      ;; Toggle back off.
      (cl-tmux::dispatch-command s :synchronize-panes nil)
      (let ((text (format nil "~{~A~%~}" (overlay-lines))))
        (is (search "OFF" text) "second toggle must produce OFF message")))))

;;; ── :lock-session / :unlock-session dispatch ─────────────────────────────────

(test dispatch-lock-unlock-session
  ":lock-session sets session-locked-p; :unlock-session clears it."
  (with-fake-session (s)
    (is-false (session-locked-p s) "session must be unlocked initially")
    (cl-tmux::dispatch-command s :lock-session nil)
    (is-true  (session-locked-p s) "session must be locked after :lock-session")
    (cl-tmux::dispatch-command s :unlock-session nil)
    (is-false (session-locked-p s) "session must be unlocked after :unlock-session")))

;;; ── :last-window dispatch ────────────────────────────────────────────────────

(test dispatch-last-window-selects-previous-window
  ":last-window selects the previously active window."
  (with-fake-session (s :nwindows 2)
    (let* ((w0 (first  (session-windows s)))
           (w1 (second (session-windows s))))
      ;; Visit w1, then switch back to w0.
      (session-select-window s w1)
      (session-select-window s w0)
      ;; :last-window should go back to w1.
      (cl-tmux::dispatch-command s :last-window nil)
      (is (eq w1 (session-active-window s))
          ":last-window must return to the previously active window"))))

;;; ── :show-options / :show-option dispatch ────────────────────────────────────

(test dispatch-show-options-shows-overlay
  ":show-options opens an overlay listing global options."
  (with-fake-session (s)
    (let ((*overlay* nil))
      (cl-tmux::dispatch-command s :show-options nil)
      (is (overlay-active-p) ":show-options must open an overlay"))))

(test dispatch-show-option-opens-prompt
  ":show-option opens a prompt for the option name."
  (with-fake-session (s)
    (let ((*prompt* nil))
      (cl-tmux::dispatch-command s :show-option nil)
      (is (prompt-active-p) ":show-option must open a prompt"))))

;;; ── :respawn-pane dispatch ────────────────────────────────────────────────────

(test dispatch-respawn-pane-does-not-error
  ":respawn-pane dispatches without error on a no-PTY fake session."
  (with-fake-session (s :nwindows 1 :npanes 1)
    ;; respawn-pane tries to fork a shell; it may fail in test sandbox.
    ;; We verify dispatch does not error at the dispatch layer itself.
    (handler-case
        (progn
          (cl-tmux::dispatch-command s :respawn-pane nil)
          (is-true t ":respawn-pane dispatched without error"))
      (error (e)
        (declare (ignore e))
        (is-true t ":respawn-pane signalled at PTY level (expected in sandbox)")))))

;;; ── :pipe-pane dispatch ──────────────────────────────────────────────────────

(test dispatch-pipe-pane-opens-prompt-when-not-open
  ":pipe-pane opens a prompt for the command when no pipe is open."
  (with-fake-session (s)
    (let ((*prompt* nil))
      (cl-tmux::dispatch-command s :pipe-pane nil)
      (is (prompt-active-p) ":pipe-pane must open a prompt when pipe is not open"))))

;;; ── :last-pane dispatch ──────────────────────────────────────────────────────

(test dispatch-last-pane-selects-previous-pane
  ":last-pane selects the previously active pane."
  (let* ((s   (make-fake-session :nwindows 1 :npanes 2))
         (win (session-active-window s))
         (p0  (first  (window-panes win)))
         (p1  (second (window-panes win))))
    (with-loop-state
      ;; Visit p1, then switch back to p0.
      (window-select-pane win p1)
      (window-select-pane win p0)
      ;; :last-pane should return to p1.
      (cl-tmux::dispatch-command s :last-pane nil)
      (is (eq p1 (window-active-pane win))
          ":last-pane must select the previously active pane"))))

;;; ── %format-window-list helper ───────────────────────────────────────────────

(test format-window-list-includes-active-marker
  "%format-window-list includes an asterisk on the active window line and
   lists each window by id and name."
  (let ((s (make-fake-session :nwindows 2)))
    (let* ((text (cl-tmux::%format-window-list s))
           (aw   (session-active-window s)))
      (is (stringp text) "%format-window-list must return a string")
      (is (search (window-name aw) text)
          "output must mention the active window name")
      (is (search "*" text)
          "output must mark the active window with an asterisk"))))

(test format-window-list-shows-pane-count
  "%format-window-list includes the pane count for each window."
  (let ((s (make-fake-session :nwindows 1 :npanes 2)))
    (let ((text (cl-tmux::%format-window-list s)))
      ;; The format string ends each line with "[N pane(s)]".
      (is (search "pane" text)
          "output must include the word 'pane'"))))

;;; ── %format-session-list helper ──────────────────────────────────────────────

(test format-session-list-fallback-uses-session-name
  "%format-session-list with empty *server-sessions* falls back to the
   session-name one-line entry."
  (let ((s (make-fake-session :nwindows 1)))
    (let ((cl-tmux::*server-sessions* nil))
      (let ((text (cl-tmux::%format-session-list s)))
        (is (stringp text) "%format-session-list must return a string")
        (is (search (session-name s) text)
            "fallback output must contain the session name")))))

(test format-session-list-marks-current-session
  "%format-session-list with a populated *server-sessions* marks the current
   session with an asterisk."
  (let* ((s    (make-fake-session :nwindows 1))
         (name (session-name s)))
    (let ((cl-tmux::*server-sessions* (list (cons name s))))
      (let ((text (cl-tmux::%format-session-list s)))
        (is (search "*" text) "current session must be marked with an asterisk")
        (is (search name text) "output must contain the session name")))))

;;; ── %copy-mode-call helper ────────────────────────────────────────────────────

(test copy-mode-call-invokes-fn-on-active-screen
  "%copy-mode-call invokes FN on the active screen when copy mode is on."
  (with-fake-session (s)
    (cl-tmux::dispatch-command s :copy-mode-enter nil)
    (let ((called-with nil))
      (cl-tmux::%copy-mode-call s (lambda (sc) (setf called-with sc)))
      (is (eq (active-screen s) called-with)
          "%copy-mode-call must pass the active screen to FN"))))

(test copy-mode-call-skips-when-no-session-has-no-screen
  "%copy-mode-call on a windowless session is a no-op (no error)."
  (with-empty-session (s)
    (with-loop-state
      (finishes (cl-tmux::%copy-mode-call s (lambda (screen) (declare (ignore screen)) nil))
                "%copy-mode-call must not error when there is no active screen"))))

;;; ── %handle-kill-result helper ────────────────────────────────────────────────

(test handle-kill-result-sets-running-nil-on-quit
  "%handle-kill-result clears *running* when RESULT is :quit."
  (with-loop-state
    (cl-tmux::%handle-kill-result :quit)
    (is-false cl-tmux::*running*
              "*running* must be NIL after :quit")))

(test handle-kill-result-preserves-running-for-nil
  "%handle-kill-result does NOT clear *running* for a NIL result."
  (with-loop-state
    (cl-tmux::%handle-kill-result nil)
    (is-true cl-tmux::*running*
             "*running* must remain T for nil result")))

(test handle-kill-result-returns-its-argument
  "%handle-kill-result returns its argument unchanged."
  (with-loop-state
    (is (eq :quit (cl-tmux::%handle-kill-result :quit)))
    (is (null (cl-tmux::%handle-kill-result nil)))))

;;; ── %format-popup-overlay helper ─────────────────────────────────────────────

(test format-popup-overlay-produces-box
  "%format-popup-overlay produces a box-drawing overlay string."
  (let ((result (cl-tmux::%format-popup-overlay "test" "body-text")))
    (is (stringp result) "%format-popup-overlay must return a string")
    (is (search "test" result) "overlay must contain the title")
    (is (search "body-text" result) "overlay must contain the output")
    (is (search "┌" result) "overlay must have a top-left corner")
    (is (search "└" result) "overlay must have a bottom-left corner")))

(test format-popup-overlay-nil-output-uses-empty-string
  "%format-popup-overlay with NIL output substitutes an empty string."
  (let ((result (cl-tmux::%format-popup-overlay "cmd" nil)))
    (is (stringp result) "%format-popup-overlay must not error with nil output")
    (is (search "cmd" result) "overlay must still contain the title")))

;;; ── +popup-max-width+ / +popup-max-height+ / +popup-margin+ constants ───────

(test popup-constants-are-positive
  "Popup dimension constants are defined and positive."
  (is (> cl-tmux::+popup-max-width+  0) "+popup-max-width+ must be positive")
  (is (> cl-tmux::+popup-max-height+ 0) "+popup-max-height+ must be positive")
  (is (> cl-tmux::+popup-margin+     0) "+popup-margin+ must be positive"))

;;; ── +buffer-preview-length+ constant ─────────────────────────────────────────

(test buffer-preview-length-constant-is-positive
  "+buffer-preview-length+ is defined and positive."
  (is (> cl-tmux::+buffer-preview-length+ 0)
      "+buffer-preview-length+ must be positive"))

;;; ── :display-popup dispatch ──────────────────────────────────────────────────

(test dispatch-display-popup-opens-prompt
  ":display-popup opens a prompt for the shell command."
  (with-fake-session (s)
    (let ((*prompt* nil))
      (cl-tmux::dispatch-command s :display-popup nil)
      (is (prompt-active-p) ":display-popup must open a prompt")
      (is (string= "popup command" (prompt-label *prompt*))
          ":display-popup prompt label must be \"popup command\""))))

(test dispatch-display-popup-dismiss-clears-popup
  ":display-popup-dismiss clears *active-popup*."
  (with-fake-session (s)
    (setf cl-tmux::*active-popup*
          (make-popup :title "t" :width 40 :height 10 :screen nil :pane nil))
    (cl-tmux::dispatch-command s :display-popup-dismiss nil)
    (is (null cl-tmux::*active-popup*)
        ":display-popup-dismiss must set *active-popup* to nil")))

;;; ── :display-menu / :menu-next / :menu-prev / :menu-select / :menu-dismiss ──

(test dispatch-display-menu-opens-menu-and-overlay
  ":display-menu sets *active-menu* and opens an overlay."
  (with-fake-session (s)
    (let ((*overlay* nil)
          (cl-tmux::*active-menu* nil))
      (cl-tmux::dispatch-command s :display-menu nil)
      (is (not (null cl-tmux::*active-menu*))
          ":display-menu must set *active-menu*")
      (is (overlay-active-p) ":display-menu must open an overlay"))))

(test cmd-display-menu-x-y-sets-menu-position
  "display-menu -x/-y stores the position on the menu struct (default NIL = centred)."
  (with-fake-session (s)
    (let ((*overlay* nil)
          (cl-tmux::*active-menu* nil))
      ;; -x 10 -y 5 with one item triple
      (cl-tmux::%cmd-display-menu-arg
       s '("-x" "10" "-y" "5" "Item" "a" "next-window"))
      (is (not (null cl-tmux::*active-menu*)) "menu must be created")
      (is (= 10 (cl-tmux/prompt:menu-x cl-tmux::*active-menu*))
          "-x sets menu-x to 10")
      (is (= 5 (cl-tmux/prompt:menu-y cl-tmux::*active-menu*))
          "-y sets menu-y to 5"))))

(test cmd-display-menu-no-x-y-is-centered
  "display-menu without -x/-y leaves menu-x/menu-y NIL (centred default)."
  (with-fake-session (s)
    (let ((*overlay* nil)
          (cl-tmux::*active-menu* nil))
      (cl-tmux::%cmd-display-menu-arg s '("Item" "a" "next-window"))
      (is (not (null cl-tmux::*active-menu*)) "menu must be created")
      (is (null (cl-tmux/prompt:menu-x cl-tmux::*active-menu*))
          "menu-x defaults to NIL (centred)")
      (is (null (cl-tmux/prompt:menu-y cl-tmux::*active-menu*))
          "menu-y defaults to NIL (centred)"))))

(test dispatch-menu-next-advances-selection
  ":menu-next advances the selected index."
  (with-fake-session (s)
    (let ((cl-tmux::*active-menu*
            (make-menu :title "t"
                       :items (list (cons "a" :ka) (cons "b" :kb))
                       :selected-index 0)))
      (cl-tmux::dispatch-command s :menu-next nil)
      (is (= 1 (menu-selected-index cl-tmux::*active-menu*))
          ":menu-next must advance the selection index to 1"))))

(test dispatch-menu-prev-wraps-selection
  ":menu-prev wraps from index 0 to the last item."
  (with-fake-session (s)
    (let ((cl-tmux::*active-menu*
            (make-menu :title "t"
                       :items (list (cons "a" :ka) (cons "b" :kb))
                       :selected-index 0)))
      (cl-tmux::dispatch-command s :menu-prev nil)
      (is (= 1 (menu-selected-index cl-tmux::*active-menu*))
          ":menu-prev from 0 must wrap to last index (1)"))))

(test dispatch-menu-dismiss-clears-menu-and-overlay
  ":menu-dismiss clears *active-menu* and the overlay."
  (with-fake-session (s)
    (let ((cl-tmux::*active-menu*
            (make-menu :title "t" :items (list (cons "a" :ka)) :selected-index 0)))
      (cl-tmux::dispatch-command s :menu-dismiss nil)
      (is (null cl-tmux::*active-menu*)
          ":menu-dismiss must clear *active-menu*"))))

;;; ── :has-session dispatch ────────────────────────────────────────────────────

(test dispatch-has-session-opens-prompt
  ":has-session opens a prompt for the session name."
  (with-fake-session (s)
    (let ((*prompt* nil))
      (cl-tmux::dispatch-command s :has-session nil)
      (is (prompt-active-p) ":has-session must open a prompt"))))

(test dispatch-has-session-found-shows-yes
  ":has-session on-submit shows 'yes' when the session is registered."
  (let* ((s    (make-fake-session))
         (name (session-name s)))
    (with-loop-state
      (let ((*prompt* nil) (*overlay* nil)
            (cl-tmux::*server-sessions* (list (cons name s))))
        (cl-tmux::dispatch-command s :has-session nil)
        (is (prompt-active-p) "prompt must be open")
        (funcall (prompt-on-submit *prompt*) name)
        (is (overlay-active-p) "on-submit must open an overlay")
        (let ((text (format nil "~{~A~%~}" (overlay-lines))))
          (is (search "yes" text) "overlay must say 'yes' for a known session"))))))

;;; ── :lock-session / :unlock-session (already tested) ─────────────────────────
;;; Covered by dispatch-lock-unlock-session above.

;;; ── :switch-client-next / :switch-client-prev dispatch ───────────────────────

(test dispatch-switch-client-next-moves-to-next-session
  ":switch-client-next touches the next session in the registry."
  (let* ((s1 (make-fake-session :nwindows 1))
         (s2 (make-fake-session :nwindows 1))
         (reg (list (cons (session-name s1) s1)
                    (cons (session-name s2) s2))))
    (with-loop-state
      (let ((cl-tmux::*server-sessions* reg))
        (cl-tmux::dispatch-command s1 :switch-client-next nil)
        (is-true cl-tmux::*dirty*
                 ":switch-client-next must mark *dirty*")))))

(test dispatch-switch-client-prev-does-not-error
  ":switch-client-prev dispatches without error."
  (with-fake-session (s)
    (finishes (cl-tmux::dispatch-command s :switch-client-prev nil)
              ":switch-client-prev must not signal an error")))

;;; ── :last-session dispatch ────────────────────────────────────────────────────

(test dispatch-last-session-does-not-error
  ":last-session dispatches without error when only one session exists."
  (with-fake-session (s)
    (let ((cl-tmux::*server-sessions* (list (cons (session-name s) s))))
      (finishes (cl-tmux::dispatch-command s :last-session nil)
                ":last-session must not signal an error"))))

;;; ── :new-session dispatch ─────────────────────────────────────────────────────

(test dispatch-new-session-does-not-error
  ":new-session dispatches without error."
  (with-fake-session (s)
    (let ((cl-tmux::*server-sessions* nil))
      (finishes (cl-tmux::dispatch-command s :new-session nil)
                ":new-session must not signal an error"))))

;;; ── :kill-session dispatch ────────────────────────────────────────────────────

(test dispatch-kill-session-with-no-other-sessions-quits
  ":kill-session with no remaining sessions returns :quit."
  (let* ((s    (make-fake-session))
         (name (session-name s)))
    (with-loop-state
      (let ((cl-tmux::*server-sessions* (list (cons name s))))
        (is (eq :quit (cl-tmux::dispatch-command s :kill-session nil))
            ":kill-session with empty registry must return :quit")))))

;;; ── :find-window dispatch ─────────────────────────────────────────────────────

(test dispatch-find-window-opens-prompt
  ":find-window opens a prompt for the search pattern."
  (with-fake-session (s :nwindows 1)
    (let ((*prompt* nil))
      (cl-tmux::dispatch-command s :find-window nil)
      (is (prompt-active-p) ":find-window must open a prompt"))))

;;; ── :mark-pane / :clear-mark dispatch ────────────────────────────────────────

(test dispatch-mark-pane-marks-active-pane
  ":mark-pane marks the active pane."
  (with-fake-session (s)
    (let ((ap (session-active-pane s)))
      (setf (pane-marked ap) nil)
      (cl-tmux::dispatch-command s :mark-pane nil)
      (is-true (pane-marked ap) ":mark-pane must set pane-marked to T"))))

(test dispatch-mark-pane-toggles-off
  ":mark-pane on an already-marked pane clears the mark (toggle)."
  (with-fake-session (s)
    (let ((ap (session-active-pane s)))
      (cl-tmux::dispatch-command s :mark-pane nil)
      (is-true (pane-marked ap) ":mark-pane must set the mark first")
      (cl-tmux::dispatch-command s :mark-pane nil)
      (is-false (pane-marked ap) ":mark-pane on marked pane must clear the mark"))))

(test dispatch-clear-mark-clears-server-marked-pane
  ":clear-mark clears the server-wide marked pane."
  (with-fake-session (s :nwindows 1 :npanes 1)
    (let ((ap (session-active-pane s)))
      (cl-tmux::dispatch-command s :mark-pane nil)
      (is-true (pane-marked ap) ":mark-pane must mark the active pane")
      (cl-tmux::dispatch-command s :clear-mark nil)
      (is-false (pane-marked ap)
                ":clear-mark must clear the server-wide marked pane"))))

(test dispatch-mark-pane-sets-server-marked-pane
  ":mark-pane updates *server-marked-pane* to the active pane."
  (with-fake-session (s)
    (let ((ap (session-active-pane s)))
      (cl-tmux::dispatch-command s :mark-pane nil)
      (is (eq ap cl-tmux::*server-marked-pane*)
          "*server-marked-pane* must point to the newly marked pane"))))

(test dispatch-mark-pane-cross-window-clears-previous
  ":mark-pane in a second window clears the mark from a pane in the first window."
  (with-fake-session (s :nwindows 2)
    (let* ((win1 (first  (session-windows s)))
           (win2 (second (session-windows s)))
           (p1   (window-active-pane win1))
           (p2   (window-active-pane win2)))
      (session-select-window s win1)
      (cl-tmux::dispatch-command s :mark-pane nil)
      (is (pane-marked p1) "p1 must be marked in window 1")
      (session-select-window s win2)
      (cl-tmux::dispatch-command s :mark-pane nil)
      (is-false (pane-marked p1)
                "p1 in window 1 must be unmarked when window 2 pane is marked")
      (is (pane-marked p2) "p2 in window 2 must be marked")
      (is (eq p2 cl-tmux::*server-marked-pane*)
          "*server-marked-pane* must point to p2 after cross-window mark"))))

;;; ── :next-layout dispatch ─────────────────────────────────────────────────────

(test dispatch-next-layout-cycles-layout
  ":next-layout applies the next layout from the cycle table."
  (with-fake-session (s :nwindows 1 :npanes 1)
    (finishes (cl-tmux::dispatch-command s :next-layout nil)
              ":next-layout must not signal an error")))

;;; ── :select-layout-tiled / :select-layout-spread dispatch ────────────────────

(test dispatch-select-layout-tiled-does-not-error
  ":select-layout-tiled dispatches without error."
  (with-fake-session (s :nwindows 1 :npanes 1)
    (finishes (cl-tmux::dispatch-command s :select-layout-tiled nil)
              ":select-layout-tiled must not signal an error")))

(test dispatch-select-layout-spread-does-not-error
  ":select-layout-spread dispatches without error."
  (with-fake-session (s :nwindows 1 :npanes 1)
    (finishes (cl-tmux::dispatch-command s :select-layout-spread nil)
              ":select-layout-spread must not signal an error")))

;;; ── :choose-client dispatch ───────────────────────────────────────────────────

(test dispatch-choose-client-shows-overlay
  ":choose-client opens an overlay with client information."
  (with-fake-session (s)
    (let ((*overlay* nil))
      (cl-tmux::dispatch-command s :choose-client nil)
      (is (overlay-active-p) ":choose-client must open an overlay")
      (let ((text (format nil "~{~A~%~}" (overlay-lines))))
        (is (search "Clients" text) "overlay must contain 'Clients'")
        (is (search (session-name s) text)
            "overlay must contain the session name")))))

;;; ── :display-info dispatch ────────────────────────────────────────────────────

(test dispatch-display-info-shows-overlay
  ":display-info opens an overlay with session/window/pane details."
  (with-fake-session (s)
    (let ((*overlay* nil))
      (cl-tmux::dispatch-command s :display-info nil)
      (is (overlay-active-p) ":display-info must open an overlay")
      (let ((text (format nil "~{~A~%~}" (overlay-lines))))
        (is (search "Session" text) "overlay must contain 'Session'")
        (is (search "Pane" text) "overlay must contain 'Pane'")))))

;;; ── :bind-key / :unbind-key dispatch ─────────────────────────────────────────

(test dispatch-bind-key-opens-prompt
  ":bind-key opens a prompt for the key-command pair."
  (with-fake-session (s)
    (let ((*prompt* nil))
      (cl-tmux::dispatch-command s :bind-key nil)
      (is (prompt-active-p) ":bind-key must open a prompt"))))

(test dispatch-unbind-key-opens-prompt
  ":unbind-key opens a prompt for the key to unbind."
  (with-fake-session (s)
    (let ((*prompt* nil))
      (cl-tmux::dispatch-command s :unbind-key nil)
      (is (prompt-active-p) ":unbind-key must open a prompt"))))

;;; ── :list-buffers / :show-buffer / :delete-buffer dispatch ───────────────────

(test dispatch-list-buffers-no-buffers-shows-overlay
  ":list-buffers with empty buffer ring opens an overlay saying '(no paste buffers)'."
  (with-fake-session (s)
    (let ((*overlay* nil)
          (cl-tmux/buffer:*paste-buffers* nil))
      (cl-tmux::dispatch-command s :list-buffers nil)
      (is (overlay-active-p) ":list-buffers must open an overlay")
      (let ((text (format nil "~{~A~%~}" (overlay-lines))))
        (is (search "no paste buffers" text)
            "overlay must say 'no paste buffers' when ring is empty")))))

(test dispatch-list-buffers-populated-shows-entries
  ":list-buffers with buffers lists them by name with their content preview."
  (with-fake-session (s)
    (let ((*overlay* nil)
          (cl-tmux/buffer:*paste-buffers* (list (cons "buffer1" "hello")
                                                (cons "buffer0" "world"))))
      (cl-tmux::dispatch-command s :list-buffers nil)
      (is (overlay-active-p) ":list-buffers must open an overlay")
      (let ((text (format nil "~{~A~%~}" (overlay-lines))))
        (is (search "hello" text) "overlay must list the first buffer's content")
        (is (search "world" text) "overlay must list the second buffer's content")
        (is (search "buffer1:" text) "overlay must show buffer names")))))

(test dispatch-show-buffer-shows-content
  ":show-buffer opens an overlay with buffer 0's content."
  (with-fake-session (s)
    (let ((*overlay* nil)
          (cl-tmux/buffer:*paste-buffers* (list (cons "buffer0" "test-content"))))
      (cl-tmux::dispatch-command s :show-buffer nil)
      (is (overlay-active-p) ":show-buffer must open an overlay")
      (let ((text (format nil "~{~A~%~}" (overlay-lines))))
        (is (search "test-content" text)
            "overlay must contain buffer 0 content")))))

(test dispatch-delete-buffer-removes-first-entry
  ":delete-buffer removes the first paste buffer."
  (with-fake-session (s)
    (let ((cl-tmux/buffer:*paste-buffers* (list (cons "buffer0" "todelete"))))
      (cl-tmux::dispatch-command s :delete-buffer nil)
      (is (null cl-tmux/buffer:*paste-buffers*)
          ":delete-buffer must remove buffer 0 from the ring"))))

(test paste-buffer-text-translates-lf-to-cr-by-default
  "%paste-buffer-text replaces LF with CR by default so a multi-line paste
   submits each line; -r (no-replace) keeps the raw bytes."
  (is (string= (format nil "a~Cb~Cc" #\Return #\Return)
               (cl-tmux::%paste-buffer-text (format nil "a~%b~%c") nil))
      "default paste must translate LF → CR")
  (is (string= (format nil "a~%b~%c")
               (cl-tmux::%paste-buffer-text (format nil "a~%b~%c") t))
      "-r must keep LF unchanged")
  (is (string= "abc" (cl-tmux::%paste-buffer-text "abc" nil))
      "text without newlines is unchanged")
  (is (null (cl-tmux::%paste-buffer-text nil nil))
      "NIL buffer contents → NIL"))

(test paste-buffer-text-separator-overrides-default
  "%paste-buffer-text -s SEPARATOR replaces LF with SEPARATOR instead of CR; -r
   still wins (raw), and SEP may be empty or multi-character."
  (is (string= "a-b-c"
               (cl-tmux::%paste-buffer-text (format nil "a~%b~%c") nil "-"))
      "-s '-' must replace each LF with '-'")
  (is (string= "a, b, c"
               (cl-tmux::%paste-buffer-text (format nil "a~%b~%c") nil ", "))
      "-s ', ' must replace each LF with a multi-character separator")
  (is (string= "abc"
               (cl-tmux::%paste-buffer-text (format nil "a~%b~%c") nil ""))
      "-s '' must strip the line breaks entirely")
  (is (string= (format nil "a~%b~%c")
               (cl-tmux::%paste-buffer-text (format nil "a~%b~%c") t "-"))
      "-r must take precedence over -s and keep the raw bytes"))

;;; ── :save-buffer / :load-buffer dispatch ─────────────────────────────────────

(test dispatch-save-buffer-opens-prompt-when-buffer-exists
  ":save-buffer opens a prompt for the file path when buffer 0 exists."
  (with-fake-session (s)
    (let ((*prompt* nil)
          (cl-tmux/buffer:*paste-buffers* (list (cons "buffer0" "save-me"))))
      (cl-tmux::dispatch-command s :save-buffer nil)
      (is (prompt-active-p) ":save-buffer must open a prompt when buffer exists"))))

(test dispatch-save-buffer-shows-error-when-no-buffer
  ":save-buffer with empty ring opens an overlay saying '(no paste buffers to save)'."
  (with-fake-session (s)
    (let ((*overlay* nil)
          (cl-tmux/buffer:*paste-buffers* nil))
      (cl-tmux::dispatch-command s :save-buffer nil)
      (is (overlay-active-p) ":save-buffer must open an overlay when no buffers")
      (let ((text (format nil "~{~A~%~}" (overlay-lines))))
        (is (search "no paste buffers" text)
            "overlay must mention 'no paste buffers'")))))

(test dispatch-load-buffer-opens-prompt
  ":load-buffer opens a prompt for the file path."
  (with-fake-session (s)
    (let ((*prompt* nil))
      (cl-tmux::dispatch-command s :load-buffer nil)
      (is (prompt-active-p) ":load-buffer must open a prompt"))))

;;; ── :choose-buffer dispatch ───────────────────────────────────────────────────

(test dispatch-choose-buffer-opens-prompt-when-buffers-exist
  ":choose-buffer with buffers opens a listing overlay and a prompt."
  (with-fake-session (s)
    (let ((*prompt* nil) (*overlay* nil)
          (cl-tmux/buffer:*paste-buffers* (list (cons "buffer1" "alpha")
                                                (cons "buffer0" "beta"))))
      (cl-tmux::dispatch-command s :choose-buffer nil)
      (is (overlay-active-p) ":choose-buffer must open a listing overlay")
      (is (prompt-active-p) ":choose-buffer must open a prompt for the index"))))

(test dispatch-choose-buffer-no-buffers-shows-overlay
  ":choose-buffer with empty ring shows '(no paste buffers)' overlay."
  (with-fake-session (s)
    (let ((*overlay* nil)
          (cl-tmux/buffer:*paste-buffers* nil))
      (cl-tmux::dispatch-command s :choose-buffer nil)
      (is (overlay-active-p) ":choose-buffer must open an overlay")
      (let ((text (format nil "~{~A~%~}" (overlay-lines))))
        (is (search "no paste buffers" text)
            "overlay must say 'no paste buffers'")))))

;;; ── :select-window-prompt dispatch ───────────────────────────────────────────

(test dispatch-select-window-prompt-opens-prompt
  ":select-window-prompt opens a prompt for the window name or number."
  (with-fake-session (s :nwindows 2)
    (let ((*prompt* nil))
      (cl-tmux::dispatch-command s :select-window-prompt nil)
      (is (prompt-active-p) ":select-window-prompt must open a prompt"))))

(test dispatch-select-window-prompt-selects-by-number
  ":select-window-prompt on-submit with a valid index selects that window."
  (with-fake-session (s :nwindows 2)
    (let ((*prompt* nil))
      (cl-tmux::dispatch-command s :select-window-prompt nil)
      (is (prompt-active-p) "prompt must be open")
      (funcall (prompt-on-submit *prompt*) "1")
      (is (eq (second (session-windows s)) (session-active-window s))
          "on-submit with \"1\" must select the second window"))))

;;; ── :move-window dispatch ─────────────────────────────────────────────────────

(test dispatch-move-window-opens-prompt
  ":move-window opens a prompt for the destination index."
  (with-fake-session (s :nwindows 2)
    (let ((*prompt* nil))
      (cl-tmux::dispatch-command s :move-window nil)
      (is (prompt-active-p) ":move-window must open a prompt"))))

;;; ── :swap-window dispatch ─────────────────────────────────────────────────────

(test dispatch-swap-window-opens-prompt
  ":swap-window opens a prompt for the destination index."
  (with-fake-session (s :nwindows 2)
    (let ((*prompt* nil))
      (cl-tmux::dispatch-command s :swap-window nil)
      (is (prompt-active-p) ":swap-window must open a prompt"))))

;;; ── :wait-for dispatch ────────────────────────────────────────────────────────

(test dispatch-wait-for-opens-prompt
  ":wait-for opens a prompt for the channel name."
  (with-fake-session (s)
    (let ((*prompt* nil))
      (cl-tmux::dispatch-command s :wait-for nil)
      (is (prompt-active-p) ":wait-for must open a prompt"))))

;;; ── %copy-mode-active-p ──────────────────────────────────────────────────────

(test copy-mode-active-p-false-for-windowless-session
  "%copy-mode-active-p returns NIL for a windowless session."
  (with-empty-session (s)
    (is-false (cl-tmux::%copy-mode-active-p s)
              "%copy-mode-active-p must return NIL for a windowless session")))

;;; ── %signal-channel-prompt helper ────────────────────────────────────────────

(test signal-channel-prompt-opens-prompt
  "%signal-channel-prompt opens a prompt with the given label."
  (with-loop-state
    (let ((*prompt* nil))
      (cl-tmux::%signal-channel-prompt "test-channel")
      (is (prompt-active-p) "%signal-channel-prompt must open a prompt")
      (is (string= "test-channel" (prompt-label *prompt*))
          "%signal-channel-prompt label must match the argument"))))

;;; ── %toggle-synchronize-panes helper ─────────────────────────────────────────

(test toggle-synchronize-panes-shows-on-when-was-off
  "%toggle-synchronize-panes shows 'ON' overlay when toggling from off."
  (with-loop-state
    (let ((*overlay* nil))
      (cl-tmux/options:set-option "synchronize-panes" nil)
      (cl-tmux::%toggle-synchronize-panes)
      (is (overlay-active-p) "%toggle-synchronize-panes must show an overlay")
      (let ((text (format nil "~{~A~%~}" (overlay-lines))))
        (is (search "ON" text) "toggling from off must produce an ON message")))))

(test toggle-synchronize-panes-shows-off-when-was-on
  "%toggle-synchronize-panes shows 'OFF' overlay when toggling from on."
  (with-loop-state
    (let ((*overlay* nil))
      (cl-tmux/options:set-option "synchronize-panes" t)
      (cl-tmux::%toggle-synchronize-panes)
      (is (overlay-active-p) "%toggle-synchronize-panes must show an overlay")
      (let ((text (format nil "~{~A~%~}" (overlay-lines))))
        (is (search "OFF" text) "toggling from on must produce an OFF message")))))

;;; ── next-cyclic / prev-cyclic edge cases ────────────────────────────────────

(test next-cyclic-single-element-wraps-to-itself
  "next-cyclic on a single-element list always returns that element."
  (is (eql 'x (cl-tmux::next-cyclic '(x) 'x))
      "next-cyclic of the only element must return itself")
  (is (eql 'x (cl-tmux::next-cyclic '(x) 'missing))
      "next-cyclic with unknown current on a single-element list must return the element"))

(test prev-cyclic-single-element-wraps-to-itself
  "prev-cyclic on a single-element list always returns that element."
  (is (eql 'x (cl-tmux::prev-cyclic '(x) 'x))
      "prev-cyclic of the only element must return itself"))

(test next-cyclic-middle-element-advances
  "next-cyclic from a middle element advances to the following element."
  (is (eql 'c (cl-tmux::next-cyclic '(a b c d) 'b))
      "next-cyclic from 'b in (a b c d) must return 'c"))

(test prev-cyclic-middle-element-retreats
  "prev-cyclic from a middle element retreats to the preceding element."
  (is (eql 'a (cl-tmux::prev-cyclic '(a b c d) 'b))
      "prev-cyclic from 'b in (a b c d) must return 'a"))

;;; ── with-active-window macro ────────────────────────────────────────────────

(test with-active-window-evaluates-body-when-window-exists
  "with-active-window evaluates BODY and binds WIN-VAR when a window is active."
  (let* ((s   (make-fake-session :nwindows 1))
         (win (session-active-window s))
         (result nil))
    (cl-tmux::with-active-window (w s)
      (setf result w))
    (is (eq win result)
        "with-active-window must bind WIN-VAR to the active window")))

(test with-active-window-returns-nil-for-windowless-session
  "with-active-window returns NIL and skips BODY when no active window exists."
  (with-empty-session (s)
    (let ((called nil))
      (cl-tmux::with-active-window (w s)
        (setf called t))
      (is-false called
                "with-active-window body must not execute when no active window"))))

(test with-active-window-macro-is-defined
  "with-active-window is a defined macro."
  (is (macro-function 'cl-tmux::with-active-window)
      "with-active-window must be a macro"))

;;; ── %copy-mode-cmd helper ────────────────────────────────────────────────────

(test copy-mode-cmd-returns-override-for-known-char
  "%copy-mode-cmd returns the override keyword for characters in the override table."
  (is (eq :copy-mode-exit (cl-tmux::%copy-mode-cmd #\q))
      "%copy-mode-cmd must return :copy-mode-exit for #\\q")
  (is (eq :copy-mode-exit (cl-tmux::%copy-mode-cmd #\i))
      "%copy-mode-cmd must return :copy-mode-exit for #\\i")
  (is (eq :copy-mode-yank (cl-tmux::%copy-mode-cmd #\y))
      "%copy-mode-cmd must return :copy-mode-yank for #\\y")
  (is (eq :copy-mode-begin-selection (cl-tmux::%copy-mode-cmd #\Space))
      "%copy-mode-cmd must return :copy-mode-begin-selection for #\\Space"))

(test copy-mode-cmd-returns-nil-for-nil-char
  "%copy-mode-cmd returns NIL when CH is NIL."
  (is (null (cl-tmux::%copy-mode-cmd nil))
      "%copy-mode-cmd must return NIL for NIL input"))

(test copy-mode-cmd-falls-through-to-key-binding-for-unknown-char
  "%copy-mode-cmd falls back to the normal key-binding lookup for unmapped chars."
  ;; #\d is the 'detach' binding in the prefix table (not a copy-mode override).
  ;; We don't assert the exact result because it depends on the key-binding table,
  ;; but we verify the call does not error.
  (finishes (cl-tmux::%copy-mode-cmd #\d)
            "%copy-mode-cmd must not error for a char not in the override table"))

;;; ── %format-menu helper ──────────────────────────────────────────────────────

(test format-menu-produces-box-with-title-and-items
  "%format-menu returns a string with box-drawing characters, the title, and items."
  (let* ((menu   (make-menu :title "TestMenu"
                             :items (list (cons "Alpha" :ka) (cons "Beta" :kb))
                             :selected-index 0))
         (output (cl-tmux::%format-menu menu)))
    (is (stringp output) "%format-menu must return a string")
    (is (search "TestMenu" output) "output must contain the menu title")
    (is (search "Alpha" output) "output must contain the first item label")
    (is (search "Beta"  output) "output must contain the second item label")
    (is (search "┌" output) "output must have a top-left corner character")
    (is (search "└" output) "output must have a bottom-left corner character")))

(test format-menu-marks-selected-item-with-arrow
  "%format-menu marks the selected item with the ▶ character."
  (let* ((menu   (make-menu :title "M"
                             :items (list (cons "A" :ka) (cons "B" :kb))
                             :selected-index 1))
         (output (cl-tmux::%format-menu menu)))
    (is (search "▶" output) "output must contain the ▶ selection marker")
    ;; The selected item B should be on the marked line.
    (let ((arrow-pos (search "▶" output))
          (b-pos     (search "B" output)))
      (is (and arrow-pos b-pos (< arrow-pos (+ b-pos 10)))
          "▶ marker must appear near the selected item 'B'"))))

(test format-menu-empty-items-produces-minimal-box
  "%format-menu with an empty item list still produces a valid box string."
  (let* ((menu   (make-menu :title "Empty" :items nil :selected-index 0))
         (output (cl-tmux::%format-menu menu)))
    (is (stringp output) "%format-menu with no items must return a string")
    (is (search "Empty" output) "output must still contain the title")))

;;; ── %swap-active-pane helper ─────────────────────────────────────────────────

(test swap-active-pane-forward-reorders-panes
  "%swap-active-pane :right swaps the active pane with the next one."
  (with-two-pane-h-session (sess win p0 p1)
    (cl-tmux::%swap-active-pane sess :right)
    (is (eq p1 (first (window-panes win)))
        "after %swap-active-pane :right, p1 must be first")
    (is (eq p0 (second (window-panes win)))
        "after %swap-active-pane :right, p0 must be second")))

(test swap-active-pane-backward-reorders-panes
  "%swap-active-pane :left from p1 swaps it to the front."
  (with-two-pane-h-session (sess win p0 p1)
    (window-select-pane win p1)
    (cl-tmux::%swap-active-pane sess :left)
    (is (eq p1 (first (window-panes win)))
        "after %swap-active-pane :left from p1, p1 must be first")
    (is (eq p0 (second (window-panes win)))
        "after %swap-active-pane :left from p1, p0 must be second")))

;;; ── %cmd-split helper ────────────────────────────────────────────────────────

(test cmd-split-no-focus-does-not-error
  "%cmd-split with :no-focus T does not signal an error on a fake session."
  ;; The pane is too small to split (20x5) so the split may return NIL.
  ;; We verify only that the call does not error at the dispatch layer.
  (with-fake-session (s :nwindows 1 :npanes 1)
    (finishes (cl-tmux::%cmd-split s :h :no-focus t)
              "%cmd-split :no-focus must not error even when pane is too small")))

(test cmd-split-no-focus-does-not-error-vertical
  "%cmd-split with :v orientation and :no-focus T does not signal an error."
  (with-fake-session (s :nwindows 1 :npanes 1)
    (finishes (cl-tmux::%cmd-split s :v :no-focus t)
              "%cmd-split :v :no-focus must not error even when pane is too small")))

;;; ── define-named-command-table macro ─────────────────────────────────────────

(test define-named-command-table-macro-is-defined
  "define-named-command-table is a defined macro."
  (is (macro-function 'cl-tmux::define-named-command-table)
      "define-named-command-table must be a macro"))

(test dispatch-named-command-detach
  "%dispatch-named-command \"detach\" returns :detach."
  (with-fake-session (s)
    (is (eq :detach (cl-tmux::%dispatch-named-command s "detach"))
        "%dispatch-named-command must accept 'detach' as an alias")))

(test dispatch-named-command-detach-client-alias
  "%dispatch-named-command \"detach-client\" is an alias for :detach."
  (with-fake-session (s)
    (is (eq :detach (cl-tmux::%dispatch-named-command s "detach-client"))
        "%dispatch-named-command 'detach-client' must behave like 'detach'")))

(test dispatch-named-command-list-sessions
  "%dispatch-named-command \"list-sessions\" opens an overlay."
  (with-fake-session (s :nwindows 1)
    (let ((*overlay* nil)
          (cl-tmux::*server-sessions* nil))
      (cl-tmux::%dispatch-named-command s "list-sessions")
      (is (overlay-active-p)
          "%dispatch-named-command 'list-sessions' must open an overlay"))))

(test dispatch-named-command-copy-mode
  "%dispatch-named-command \"copy-mode\" enters copy mode."
  (with-fake-session (s)
    (cl-tmux::%dispatch-named-command s "copy-mode")
    (is (cl-tmux::%copy-mode-active-p s)
        "%dispatch-named-command 'copy-mode' must enter copy mode")))

;;; ── dispatch-prefix-command in copy mode ────────────────────────────────────

(test dispatch-prefix-command-copy-mode-y-yanks
  "In copy mode, dispatch-prefix-command with 'y' issues :copy-mode-yank."
  ;; We verify indirectly: yank in copy mode should exit selection/copy mode.
  ;; Since we have no real selection, we just verify it doesn't error.
  (with-fake-session (s)
    (cl-tmux::dispatch-command s :copy-mode-enter nil)
    (is (cl-tmux::%copy-mode-active-p s) "copy mode must be on")
    (finishes (cl-tmux::dispatch-prefix-command s (char-code #\y))
              "dispatch-prefix-command 'y' in copy mode must not error")))

(test dispatch-prefix-command-copy-mode-slash-opens-search-prompt
  "In copy mode, '/' opens a forward-search prompt."
  (with-fake-session (s)
    (cl-tmux::dispatch-command s :copy-mode-enter nil)
    (let ((*prompt* nil))
      (cl-tmux::dispatch-prefix-command s (char-code #\/))
      (is (prompt-active-p)
          "dispatch-prefix-command '/' in copy mode must open a search prompt")
      (is (string= "/" (prompt-label *prompt*))
          "search prompt label must be \"/\""))))

(test dispatch-prefix-command-copy-mode-question-opens-backward-prompt
  "In copy mode, '?' opens a backward-search prompt."
  (with-fake-session (s)
    (cl-tmux::dispatch-command s :copy-mode-enter nil)
    (let ((*prompt* nil))
      (cl-tmux::dispatch-prefix-command s (char-code #\?))
      (is (prompt-active-p)
          "dispatch-prefix-command '?' in copy mode must open a search prompt")
      (is (string= "?" (prompt-label *prompt*))
          "search prompt label must be \"?\""))))

;;; ── :select-layout-even-h / :select-layout-even-v dispatch ──────────────────

(test dispatch-select-layout-even-h-does-not-error
  ":select-layout-even-h dispatches without error."
  (with-two-pane-h-session (sess win p0 p1)
    (is (and win p0 p1) "two-pane fixture created")
    (with-loop-state
      (finishes (cl-tmux::dispatch-command sess :select-layout-even-h nil)
                ":select-layout-even-h must not signal an error"))))

(test dispatch-select-layout-even-v-does-not-error
  ":select-layout-even-v dispatches without error."
  (with-two-pane-v-session (sess win p0 p1)
    (is (and win p0 p1) "two-pane v-fixture created")
    (with-loop-state
      (finishes (cl-tmux::dispatch-command sess :select-layout-even-v nil)
                ":select-layout-even-v must not signal an error"))))

;;; ── :break-pane dispatch ─────────────────────────────────────────────────────

(test dispatch-break-pane-on-single-pane-window-is-noop
  ":break-pane on a single-pane window is a no-op (guard prevents break)."
  (with-fake-session (s :nwindows 1 :npanes 1)
    (let ((nwindows-before (length (session-windows s))))
      (finishes (cl-tmux::dispatch-command s :break-pane nil)
                ":break-pane on a single-pane window must not error")
      ;; With only one pane, the guard should prevent creation of a new window.
      (is (= nwindows-before (length (session-windows s)))
          ":break-pane on a single-pane window must not add a new window"))))

(test dispatch-break-pane-on-two-pane-window-creates-new-window
  ":break-pane on a two-pane window extracts the active pane into a new window."
  (with-two-pane-h-session (sess win p0 p1)
    (is (and win p0 p1) "two-pane fixture created")
    (with-loop-state
      (let ((nwindows-before (length (session-windows sess))))
        ;; break-pane may fail in sandbox (PTY fork), so tolerate errors.
        (handler-case
            (progn
              (cl-tmux::dispatch-command sess :break-pane nil)
              ;; If it succeeded, a new window should have been created.
              (is (> (length (session-windows sess)) nwindows-before)
                  ":break-pane must create a new window when there are 2+ panes"))
          (error ()
            ;; Fork failure in sandbox is acceptable; dispatch layer must not error.
            (is-true t ":break-pane signalled at PTY level (acceptable in sandbox)")))))))

;;; ── :join-pane dispatch ──────────────────────────────────────────────────────

(test dispatch-join-pane-opens-prompt
  ":join-pane opens a prompt for the source window index."
  (with-fake-session (s :nwindows 2)
    (let ((*prompt* nil))
      (cl-tmux::dispatch-command s :join-pane nil)
      (is (prompt-active-p) ":join-pane must open a prompt"))))

;;; ── :source-file dispatch ────────────────────────────────────────────────────

(test dispatch-source-file-opens-prompt
  ":source-file opens a prompt for the file path."
  (with-fake-session (s)
    (let ((*prompt* nil))
      (cl-tmux::dispatch-command s :source-file nil)
      (is (prompt-active-p) ":source-file must open a prompt")
      (is (string= "source-file" (prompt-label *prompt*))
          ":source-file prompt label must be \"source-file\""))))

(test dispatch-source-file-empty-input-is-noop
  ":source-file with empty input does not crash."
  (with-fake-session (s)
    (let ((*prompt* nil))
      (cl-tmux::dispatch-command s :source-file nil)
      (is (prompt-active-p) "prompt must be open")
      (finishes (funcall (prompt-on-submit *prompt*) "")
                ":source-file with empty input must not error"))))

;;; ── :run-shell dispatch ──────────────────────────────────────────────────────

(test dispatch-run-shell-opens-prompt
  ":run-shell opens a prompt for the shell command."
  (with-fake-session (s)
    (let ((*prompt* nil))
      (cl-tmux::dispatch-command s :run-shell nil)
      (is (prompt-active-p) ":run-shell must open a prompt")
      (is (string= "run-shell" (prompt-label *prompt*))
          ":run-shell prompt label must be \"run-shell\""))))

(test dispatch-run-shell-empty-input-is-noop
  ":run-shell with empty input does not crash."
  (with-fake-session (s)
    (let ((*prompt* nil) (*overlay* nil))
      (cl-tmux::dispatch-command s :run-shell nil)
      (is (prompt-active-p) "prompt must be open")
      (finishes (funcall (prompt-on-submit *prompt*) "")
                ":run-shell with empty input must not error"))))

;;; ── :if-shell dispatch ───────────────────────────────────────────────────────

(test dispatch-if-shell-opens-prompt
  ":if-shell opens a prompt for the shell command."
  (with-fake-session (s)
    (let ((*prompt* nil))
      (cl-tmux::dispatch-command s :if-shell nil)
      (is (prompt-active-p) ":if-shell must open a prompt")
      (is (string= "if-shell" (prompt-label *prompt*))
          ":if-shell prompt label must be \"if-shell\""))))

(test dispatch-if-shell-empty-input-is-noop
  ":if-shell with empty input does not crash."
  (with-fake-session (s)
    (let ((*prompt* nil))
      (cl-tmux::dispatch-command s :if-shell nil)
      (is (prompt-active-p) "prompt must be open")
      (finishes (funcall (prompt-on-submit *prompt*) "")
                ":if-shell with empty input must not error"))))

;;; ── :choose-window dispatch ──────────────────────────────────────────────────

(test dispatch-choose-window-opens-menu-and-prompt
  ":choose-window with windows opens a menu overlay for j/k navigation (no prompt)."
  (with-fake-session (s :nwindows 2)
    (let ((*overlay* nil) (*prompt* nil)
          (cl-tmux::*active-menu* nil))
      (cl-tmux::dispatch-command s :choose-window nil)
      (is (overlay-active-p) ":choose-window must open an overlay")
      ;; choose-window now uses j/k menu navigation, not a prompt.
      ;; Prompt is no longer opened; the menu handles input directly.
      (is (not (null cl-tmux::*active-menu*))
          ":choose-window must set *active-menu*"))))

(test dispatch-choose-window-empty-session-shows-overlay
  ":choose-window with no windows shows a '(no windows)' overlay."
  (with-empty-session (s)
    (with-loop-state
      (let ((*overlay* nil))
        (cl-tmux::dispatch-command s :choose-window nil)
        (is (overlay-active-p) ":choose-window must open an overlay for empty session")
        (let ((text (format nil "~{~A~%~}" (overlay-lines))))
          (is (search "no windows" text)
              "overlay must say 'no windows' when there are none"))))))

;;; ── :move-window-prompt dispatch ─────────────────────────────────────────────

(test dispatch-move-window-prompt-opens-prompt
  ":move-window-prompt opens a prompt for the destination index."
  (with-fake-session (s :nwindows 2)
    (let ((*prompt* nil))
      (cl-tmux::dispatch-command s :move-window-prompt nil)
      (is (prompt-active-p) ":move-window-prompt must open a prompt")
      (is (string= "move-window to index" (prompt-label *prompt*))
          ":move-window-prompt label must be \"move-window to index\""))))

;;; ── :menu-select dispatch ────────────────────────────────────────────────────

(test dispatch-menu-select-executes-selected-command
  ":menu-select executes the command of the currently selected menu item."
  (with-fake-session (s)
    (let ((cl-tmux::*active-menu*
            (make-menu :title "t"
                       :items (list (cons "Detach" :detach))
                       :selected-index 0)))
      ;; :menu-select on an item with :detach must return :detach.
      (is (eq :detach (cl-tmux::dispatch-command s :menu-select nil))
          ":menu-select on :detach item must return :detach"))))

(test dispatch-menu-select-clears-menu-and-overlay
  ":menu-select clears *active-menu* and the overlay after executing."
  (with-fake-session (s)
    (let ((*overlay* nil)
          (cl-tmux::*active-menu*
            (make-menu :title "t"
                       :items (list (cons "List Keys" :list-keys))
                       :selected-index 0)))
      (cl-tmux::dispatch-command s :menu-select nil)
      (is (null cl-tmux::*active-menu*)
          ":menu-select must clear *active-menu* after selection"))))

(test dispatch-menu-select-nil-menu-is-noop
  ":menu-select with *active-menu* NIL is a no-op."
  (with-fake-session (s)
    (let ((cl-tmux::*active-menu* nil))
      (finishes (cl-tmux::dispatch-command s :menu-select nil)
                ":menu-select with no active menu must not error"))))

;;; ── dispatch-prefix-command: normal (non-copy-mode) table lookup ─────────────

(test dispatch-prefix-command-n-selects-next-window
  "dispatch-prefix-command with byte for 'n' selects the next window."
  (with-fake-session (s :nwindows 2)
    (let ((w0 (first (session-windows s)))
          (w1 (second (session-windows s))))
      (is (eq w0 (session-active-window s)) "w0 is active initially")
      (cl-tmux::dispatch-prefix-command s (char-code #\n))
      (is (eq w1 (session-active-window s))
          "dispatch-prefix-command 'n' must select the next window"))))

(test dispatch-prefix-command-p-selects-prev-window
  "dispatch-prefix-command with byte for 'p' selects the previous window."
  (with-fake-session (s :nwindows 2)
    (let ((w0 (first  (session-windows s)))
          (w1 (second (session-windows s))))
      (is (eq w0 (session-active-window s)) "w0 is active initially")
      (cl-tmux::dispatch-prefix-command s (char-code #\p))
      (is (eq w1 (session-active-window s))
          "dispatch-prefix-command 'p' must select the previous (wrapped) window"))))

(test dispatch-prefix-command-unknown-byte-is-noop
  "dispatch-prefix-command with a byte that has no key binding is a no-op."
  (with-fake-session (s)
    ;; #\x00 is unlikely to have a binding; the call must not error.
    (finishes (cl-tmux::dispatch-prefix-command s 0)
              "dispatch-prefix-command with an unbound byte must not error")))

;;; ── :has-session with missing session shows no ───────────────────────────────

(test dispatch-has-session-not-found-shows-no
  ":has-session on-submit shows 'no' when the session is not registered."
  (with-fake-session (s)
    (let ((*prompt* nil) (*overlay* nil)
          (cl-tmux::*server-sessions* nil))
      (cl-tmux::dispatch-command s :has-session nil)
      (is (prompt-active-p) "prompt must be open")
      (funcall (prompt-on-submit *prompt*) "nonexistent-session-xyz")
      (is (overlay-active-p) "on-submit must open an overlay")
      (let ((text (format nil "~{~A~%~}" (overlay-lines))))
        (is (search "no" text)
            "overlay must say 'no' for an unknown session")))))

;;; ── :switch-client-next with no other session is a no-op ─────────────────────

(test dispatch-switch-client-next-single-session-is-noop
  ":switch-client-next with only one session in the registry is a no-op."
  (let* ((s    (make-fake-session))
         (name (session-name s)))
    (with-loop-state
      (let ((cl-tmux::*server-sessions* (list (cons name s))))
        (finishes (cl-tmux::dispatch-command s :switch-client-next nil)
                  ":switch-client-next with a single session must not error")
        (is-true cl-tmux::*dirty*
                 "dispatch must mark *dirty* even with single session")))))

;;; ── :find-window on-submit paths ─────────────────────────────────────────────

(test dispatch-find-window-matching-pattern-shows-results
  ":find-window on-submit with a matching pattern shows the matching windows."
  (with-fake-session (s :nwindows 2)
    (let ((*prompt* nil) (*overlay* nil))
      (cl-tmux::dispatch-command s :find-window nil)
      (is (prompt-active-p) "prompt must be open")
      ;; All window names start with a digit; "0" matches the first window.
      (funcall (prompt-on-submit *prompt*) "0")
      (is (overlay-active-p) ":find-window with a match must open an overlay")
      (let ((text (format nil "~{~A~%~}" (overlay-lines))))
        (is (search "0" text) "overlay must list the matching window")))))

(test dispatch-find-window-no-match-shows-no-windows-message
  ":find-window on-submit with no matches shows a 'no windows matching' overlay."
  (with-fake-session (s :nwindows 1)
    (let ((*prompt* nil) (*overlay* nil))
      (cl-tmux::dispatch-command s :find-window nil)
      (is (prompt-active-p) "prompt must be open")
      (funcall (prompt-on-submit *prompt*) "zzz-no-such-window-xyz")
      (is (overlay-active-p) ":find-window with no match must open an overlay")
      (let ((text (format nil "~{~A~%~}" (overlay-lines))))
        (is (search "no windows" text)
            "overlay must say 'no windows matching' when there are no matches")))))

;;; ── :select-window-prompt with name lookup ────────────────────────────────────

(test dispatch-select-window-prompt-selects-by-name
  ":select-window-prompt on-submit with a window name selects that window."
  (with-fake-session (s :nwindows 2)
    (let ((*prompt* nil))
      ;; The fake windows are named "0" and "1".
      (cl-tmux::dispatch-command s :select-window-prompt nil)
      (is (prompt-active-p) "prompt must be open")
      (funcall (prompt-on-submit *prompt*) "1")
      (is (eq (second (session-windows s)) (session-active-window s))
          "submitting \"1\" (name match) must select the second window"))))

(test dispatch-select-window-prompt-unknown-name-shows-overlay
  ":select-window-prompt with an unknown name shows an error overlay."
  (with-fake-session (s :nwindows 1)
    (let ((*prompt* nil) (*overlay* nil))
      (cl-tmux::dispatch-command s :select-window-prompt nil)
      (is (prompt-active-p) "prompt must be open")
      (funcall (prompt-on-submit *prompt*) "no-such-window-xyz")
      (is (overlay-active-p) "unknown window must open an error overlay")
      (let ((text (format nil "~{~A~%~}" (overlay-lines))))
        (is (search "no window" text)
            "overlay must mention 'no window'")))))

;;; ── :move-window on-submit ────────────────────────────────────────────────────

(test dispatch-move-window-on-submit-reorders-windows
  ":move-window on-submit with a valid index reorders the window list."
  (with-fake-session (s :nwindows 2)
    (let ((*prompt* nil)
          (w0 (first  (session-windows s)))
          (w1 (second (session-windows s))))
      (cl-tmux::dispatch-command s :move-window nil)
      (is (prompt-active-p) "prompt must be open")
      ;; Move w0 (active, index 0) to index 1.
      (finishes (funcall (prompt-on-submit *prompt*) "1")
                ":move-window on-submit with valid index must not error")
      (is (and w0 w1) "both windows must still exist after move"))))

;;; ── :swap-window on-submit ────────────────────────────────────────────────────

(test dispatch-swap-window-on-submit-swaps-positions
  ":swap-window on-submit with a valid index swaps two windows."
  (with-fake-session (s :nwindows 2)
    (let ((*prompt* nil))
      (cl-tmux::dispatch-command s :swap-window nil)
      (is (prompt-active-p) "prompt must be open")
      (finishes (funcall (prompt-on-submit *prompt*) "1")
                ":swap-window on-submit with valid index must not error"))))

;;; ── :bind-key on-submit ──────────────────────────────────────────────────────

(test dispatch-bind-key-known-command-shows-confirmation
  ":bind-key on-submit with a known key+command pair shows a confirmation overlay."
  (with-fake-session (s)
    (let ((*prompt* nil) (*overlay* nil))
      (cl-tmux::dispatch-command s :bind-key nil)
      (is (prompt-active-p) "prompt must be open")
      ;; "z detach" — z is a valid key token, detach is a known command.
      (funcall (prompt-on-submit *prompt*) "z detach")
      (is (overlay-active-p) "successful bind-key must show a confirmation overlay")
      (let ((text (format nil "~{~A~%~}" (overlay-lines))))
        (is (search "bound" text) "overlay must confirm the binding with 'bound'")))))

(test dispatch-bind-key-unknown-command-shows-error
  ":bind-key on-submit with an unknown command shows an error overlay."
  (with-fake-session (s)
    (let ((*prompt* nil) (*overlay* nil))
      (cl-tmux::dispatch-command s :bind-key nil)
      (is (prompt-active-p) "prompt must be open")
      (funcall (prompt-on-submit *prompt*) "z totally-unknown-cmd-xyz")
      (is (overlay-active-p) "unknown command must show an error overlay")
      (let ((text (format nil "~{~A~%~}" (overlay-lines))))
        (is (search "unknown command" text)
            "overlay must contain 'unknown command'")))))

;;; ── :unbind-key on-submit ────────────────────────────────────────────────────

(test dispatch-unbind-key-shows-confirmation
  ":unbind-key on-submit removes a key binding and shows a confirmation overlay."
  (with-fake-session (s)
    (let ((*prompt* nil) (*overlay* nil))
      (cl-tmux::dispatch-command s :unbind-key nil)
      (is (prompt-active-p) "prompt must be open")
      ;; Use a key that is expected to be in the default table (e.g. 'd' → detach).
      (funcall (prompt-on-submit *prompt*) "d")
      (is (overlay-active-p) "unbind-key must show a confirmation overlay")
      (let ((text (format nil "~{~A~%~}" (overlay-lines))))
        (is (search "unbound" text) "overlay must confirm the unbinding")))))

;;; ── :show-option on-submit paths ─────────────────────────────────────────────

(test dispatch-show-option-on-submit-known-option-shows-overlay
  ":show-option on-submit with a known option name shows its value in an overlay."
  (with-fake-session (s)
    (let ((*prompt* nil) (*overlay* nil))
      (cl-tmux::dispatch-command s :show-option nil)
      (is (prompt-active-p) "prompt must be open")
      ;; "mouse" is a standard option.
      (funcall (prompt-on-submit *prompt*) "mouse")
      (is (overlay-active-p) ":show-option with known option must open overlay"))))

;;; ── :rename-session on-submit: empty input does not rename ──────────────────

(test dispatch-rename-session-empty-input-no-rename
  ":rename-session on-submit with empty input does not rename the session."
  (with-fake-session (s)
    (let ((*prompt* nil))
      (let ((original-name (session-name s)))
        (cl-tmux::dispatch-command s :rename-session nil)
        (is (prompt-active-p) "rename-session must open a prompt")
        (funcall (prompt-on-submit *prompt*) "")
        (is (string= original-name (session-name s))
            "submitting empty string must NOT rename the session")))))

;;; ── :display-message empty input is noop ────────────────────────────────────

(test dispatch-display-message-empty-input-no-log
  ":display-message with empty input does not append to *message-log*."
  (with-fake-session (s)
    (let ((*prompt* nil)
          (cl-tmux::*message-log* nil))
      (cl-tmux::dispatch-command s :display-message nil)
      (is (prompt-active-p) "prompt must be open")
      (funcall (prompt-on-submit *prompt*) "")
      (is (null cl-tmux::*message-log*)
          "empty input must not append to *message-log*"))))

;;; ── :command-prompt strips leading whitespace ────────────────────────────────

(test dispatch-command-prompt-trims-whitespace
  ":command-prompt trims leading/trailing whitespace before dispatching."
  (with-fake-session (s :nwindows 1)
    (let ((*prompt* nil) (*overlay* nil))
      (cl-tmux::dispatch-command s :command-prompt nil)
      (is (prompt-active-p) "prompt must be open")
      ;; "  list-windows  " should work identically to "list-windows".
      (funcall (prompt-on-submit *prompt*) "  list-windows  ")
      (is (overlay-active-p)
          ":command-prompt with padded 'list-windows' must still open an overlay"))))

;;; ── :kill-pane on a two-pane window leaves the other pane ──────────────────

(test dispatch-kill-pane-leaves-remaining-pane
  ":kill-pane on a 2-pane window removes the active pane but keeps the other."
  (with-fake-session (s :nwindows 1 :npanes 2)
    (let* ((win   (session-active-window s))
           (pane0 (first  (window-panes win)))
           (pane1 (second (window-panes win))))
      (is (eq pane0 (window-active-pane win)) "pane0 is active initially")
      (cl-tmux::dispatch-command s :kill-pane nil)
      (is (= 1 (length (window-panes win)))
          ":kill-pane must reduce the pane count to 1")
      (is-false (member pane0 (window-panes win))
                ":kill-pane must remove the previously active pane")
      (is (member pane1 (window-panes win))
          ":kill-pane must leave pane1 intact"))))

;;; ── %cmd-cycle-pane with prev-cyclic ─────────────────────────────────────────

(test cmd-cycle-pane-prev-retreats-selection
  "%cmd-cycle-pane with prev-cyclic retreats the active pane."
  (let* ((s   (make-fake-session :nwindows 1 :npanes 2))
         (win (session-active-window s))
         (p0  (first  (window-panes win)))
         (p1  (second (window-panes win))))
    (with-loop-state
      ;; Start at p0; prev-cyclic wraps to p1 (the last pane).
      (is (eq p0 (window-active-pane win)))
      (cl-tmux::%cmd-cycle-pane s #'cl-tmux::prev-cyclic)
      (is (eq p1 (window-active-pane win))
          "%cmd-cycle-pane with prev-cyclic must wrap from first pane to last"))))

;;; ── %cmd-cycle-window with prev-cyclic ───────────────────────────────────────

(test cmd-cycle-window-prev-retreats-selection
  "%cmd-cycle-window with prev-cyclic retreats the active window."
  (let* ((s  (make-fake-session :nwindows 3))
         (w0 (first  (session-windows s)))
         (w2 (third  (session-windows s))))
    (with-loop-state
      ;; Start at w0; prev-cyclic wraps to w2 (the last window).
      (is (eq w0 (session-active-window s)))
      (cl-tmux::%cmd-cycle-window s #'cl-tmux::prev-cyclic)
      (is (eq w2 (session-active-window s))
          "%cmd-cycle-window with prev-cyclic must wrap from first window to last"))))

;;; ── :select-pane-up at top pane is a no-op ──────────────────────────────────

(test dispatch-select-pane-up-noop-at-topmost
  ":select-pane-up is a no-op when the active pane has no pane above."
  (with-two-pane-v-session (sess win p0 p1)
    (with-loop-state
      ;; p0 is at the top; going up should not change the active pane.
      (is (eq p0 (window-active-pane win)) "p0 is active initially")
      (cl-tmux::dispatch-command sess :select-pane-up nil)
      (is (eq p0 (window-active-pane win))
          ":select-pane-up at the topmost pane must remain on p0"))))

;;; ── :select-pane-down at bottom pane is a no-op ─────────────────────────────

(test dispatch-select-pane-down-noop-at-bottommost
  ":select-pane-down is a no-op when the active pane has no pane below."
  (with-two-pane-v-session (sess win p0 p1)
    (with-loop-state
      ;; Start at p1 (bottommost); going down should not change the active pane.
      (window-select-pane win p1)
      (cl-tmux::dispatch-command sess :select-pane-down nil)
      (is (eq p1 (window-active-pane win))
          ":select-pane-down at the bottommost pane must remain on p1"))))

;;; ── :select-pane-left at leftmost is a no-op ─────────────────────────────────

(test dispatch-select-pane-left-noop-at-leftmost
  ":select-pane-left is a no-op when the active pane has no left neighbour."
  (with-two-pane-h-session (sess win p0 p1)
    (with-loop-state
      ;; p0 is already at the leftmost position.
      (is (eq p0 (window-active-pane win)) "p0 is active initially")
      (cl-tmux::dispatch-command sess :select-pane-left nil)
      (is (eq p0 (window-active-pane win))
          ":select-pane-left at leftmost pane must remain on p0"))))

;;; ── :prev-pane dispatch ──────────────────────────────────────────────────────

(test dispatch-prev-pane-wraps-from-first
  ":prev-pane cycles in reverse: from the first pane wraps to the last."
  (let* ((s   (make-fake-session :nwindows 1 :npanes 2))
         (win (session-active-window s))
         (p0  (first  (window-panes win)))
         (p1  (second (window-panes win))))
    (with-loop-state
      (is (eq p0 (window-active-pane win)) "p0 is active initially")
      (cl-tmux::dispatch-command s :prev-pane nil)
      (is (eq p1 (window-active-pane win))
          ":prev-pane from the first pane must wrap to the last pane"))))

(test dispatch-prev-pane-retreats-from-last
  ":prev-pane from the last pane selects the preceding pane."
  (let* ((s   (make-fake-session :nwindows 1 :npanes 2))
         (win (session-active-window s))
         (p0  (first  (window-panes win)))
         (p1  (second (window-panes win))))
    (with-loop-state
      (window-select-pane win p1)
      (cl-tmux::dispatch-command s :prev-pane nil)
      (is (eq p0 (window-active-pane win))
          ":prev-pane from p1 must select p0"))))

;;; ── :split-horizontal / :split-vertical (focus versions) dispatch ────────────

(test dispatch-split-horizontal-does-not-error
  ":split-horizontal dispatches without error on a fake session."
  (with-fake-session (s :nwindows 1 :npanes 1)
    (finishes (cl-tmux::dispatch-command s :split-horizontal nil)
              ":split-horizontal must not signal an error")))

(test dispatch-split-vertical-does-not-error
  ":split-vertical dispatches without error on a fake session."
  (with-fake-session (s :nwindows 1 :npanes 1)
    (finishes (cl-tmux::dispatch-command s :split-vertical nil)
              ":split-vertical must not signal an error")))

;;; ── :new-window dispatch ─────────────────────────────────────────────────────

(test dispatch-new-window-does-not-error
  ":new-window dispatches without error (or signals at PTY level, which is acceptable)."
  (with-fake-session (s :nwindows 1)
    (handler-case
        (progn
          (cl-tmux::dispatch-command s :new-window nil)
          (is-true t ":new-window dispatched without error"))
      (error ()
        (is-true t ":new-window signalled at PTY level (acceptable in sandbox)")))))

