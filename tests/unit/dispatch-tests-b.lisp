(in-package #:cl-tmux/test)

;;;; dispatch tests — part B: swap-pane forward/backward, kill-pane-confirm,
;;;; pane-resp, select-layout, yank-buffer, list-clients, resize-pane,
;;;; rotate-window, select-pane direction, zoom.

(in-suite dispatch-suite)

;;; ── :swap-pane-forward dispatch ──────────────────────────────────────────────
;;;
;;; Use the shared fixture macro to avoid repeating pane/window/session setup.


(test cmd-swap-pane-s-t-swaps-specific-panes
  "swap-pane -s 1 -t 3 swaps the two named panes' positions in the window list
   (not just the active pane with a neighbour)."
  (with-fake-session (s :nwindows 1 :npanes 3)
    (let* ((win (session-active-window s))
           (p1  (find 1 (window-panes win) :key #'pane-id))
           (p3  (find 3 (window-panes win) :key #'pane-id)))
      (is (eq p1 (first (window-panes win)))         "pane 1 starts first")
      (is (eq p3 (car (last (window-panes win))))    "pane 3 starts last")
      (cl-tmux::%run-command-line s "swap-pane -s 1 -t 3")
      (is (eq p3 (first (window-panes win)))
          "after swap-pane -s 1 -t 3, pane 3 is first")
      (is (eq p1 (car (last (window-panes win))))
          "after swap, pane 1 is last"))))

(test cmd-swap-pane-t-swaps-active-with-target
  "swap-pane -t 2 swaps the active pane (1) with pane 2 (-s defaults to active)."
  (with-fake-two-pane-session (s)
    (let* ((win (session-active-window s))
           (p1  (find 1 (window-panes win) :key #'pane-id))
           (p2  (find 2 (window-panes win) :key #'pane-id)))
      (is (eq p1 (first (window-panes win))) "pane 1 (active) starts first")
      (cl-tmux::%run-command-line s "swap-pane -t 2")
      (is (eq p2 (first (window-panes win)))
          "after swap-pane -t 2, pane 2 is first (swapped with active pane 1)"))))

(test cmd-swap-pane-rejects-unsupported-arguments
  "swap-pane rejects unsupported flags, unknown flags, and positional tokens
   before mutating panes."
  (dolist (command '("swap-pane -d"
                     "swap-pane -Z"
                     "swap-pane -Z extra"
                     "swap-pane -x"
                     "swap-pane -s 1 -t 3 extra"))
    (with-fake-session (s :nwindows 1 :npanes 3)
      (let* ((win (session-active-window s))
             (before (copy-list (window-panes win)))
             (*overlay* nil))
        (is (null (cl-tmux::%run-command-line s command))
            "~A must be rejected" command)
        (is (equal before (window-panes win))
            "~A must not reorder panes" command)
        (assert-overlay-contains "unsupported argument" *overlay* command)))))

;;; ── :swap-pane-forward / :swap-pane-backward dispatch ───────────────────────

(test dispatch-swap-pane-table
  ":swap-pane-forward (p0 active) and :swap-pane-backward (p1 active) both
   move p1 to first position and mark *dirty*."
  (dolist (row '((:swap-pane-forward  nil "forward: p0 active")
                 (:swap-pane-backward t   "backward: p1 active")))
    (destructuring-bind (cmd select-p1 desc) row
      (with-two-pane-h-session (sess win p0 p1)
        (when select-p1 (window-select-pane win p1))
        (cl-tmux::dispatch-command sess cmd nil)
        (is (eq p1 (first  (window-panes win))) "~A: p1 must be first"  desc)
        (is (eq p0 (second (window-panes win))) "~A: p0 must be second" desc)
        (is-true cl-tmux::*dirty*               "~A: must mark *dirty*" desc)))))

;;; ── :kill-pane-confirm dispatch ──────────────────────────────────────────────

(test dispatch-kill-pane-confirm-table
  ":kill-pane-confirm opens a prompt; y kills the active pane; n leaves it."
  (dolist (c '((nil 2 "no answer: pane count unchanged")
               ("y" 1 "y: active pane killed")
               ("n" 2 "n: pane preserved")))
    (destructuring-bind (answer expected-count desc) c
      (with-fake-two-pane-session (s)
        (let ((*prompt* nil))
          (cl-tmux::dispatch-command s :kill-pane-confirm nil)
          (is (prompt-active-p) "prompt must open for ~A" desc)
          (is-true (prompt-single-key *prompt*) "prompt must accept a single key for ~A" desc)
          (when answer
            (cl-tmux::handle-prompt-key (char-code (char answer 0)))
            (is-false (prompt-active-p) "prompt must close after answer for ~A" desc))
          (is (= expected-count
                 (length (window-panes (session-active-window s))))
              "~A" desc))))))

;;; ── :kill-window-confirm dispatch ────────────────────────────────────────────

(test dispatch-kill-window-confirm-table
  ":kill-window-confirm opens a prompt; y kills the active window; n leaves it."
  (dolist (c '((nil 2 "no answer: window count unchanged")
               ("y" 1 "y: active window killed")
               ("n" 2 "n: window preserved")))
    (destructuring-bind (answer expected-count desc) c
      (with-fake-session (s :nwindows 2)
        (let ((*prompt* nil))
          (cl-tmux::dispatch-command s :kill-window-confirm nil)
          (is (prompt-active-p) "prompt must open for ~A" desc)
          (is-true (prompt-single-key *prompt*) "prompt must accept a single key for ~A" desc)
          (when answer
            (cl-tmux::handle-prompt-key (char-code (char answer 0)))
            (is-false (prompt-active-p) "prompt must close after answer for ~A" desc))
          (is (= expected-count (length (session-windows s))) "~A" desc))))))

(test dispatch-kill-window-confirm-prompt-includes-window-name
  ":kill-window-confirm prompt label includes the current window name."
  (with-fake-session (s :nwindows 1)
    (let ((*prompt* nil))
      (let ((wname (window-name (session-active-window s))))
        (cl-tmux::dispatch-command s :kill-window-confirm nil)
        (is (prompt-active-p) "prompt must be open")
        (is (search wname (prompt-label *prompt*))
            "prompt label must contain the window name")))))

;;; ── :send-prefix dispatch ────────────────────────────────────────────────────

(test dispatch-send-prefix-command-is-defined
  ":send-prefix command is registered in dispatch-command without error."
  ;; We cannot test actual PTY writes in unit tests (fd=-1), but we verify
  ;; that dispatching :send-prefix does not signal any error and marks dirty.
  (with-fake-session (s)
    ;; Should not error even with fd=-1 (the guard (> fd 0) protects the write).
    (finishes (cl-tmux::dispatch-command s :send-prefix nil))
    (is-true cl-tmux::*dirty* ":send-prefix must mark *dirty*")))

(test dispatch-send-prefix-read-only-does-not-write
  ":send-prefix does not inject the prefix byte when the client is read-only."
  (with-isolated-config
    (with-fake-session (s)
      (let* ((pane (window-active-pane (session-active-window s)))
             (writes nil)
             (orig (fdefinition 'cl-tmux/pty:pty-write)))
        (setf (pane-fd pane) 9999)
        (unwind-protect
             (progn
               (setf (fdefinition 'cl-tmux/pty:pty-write)
                     (lambda (fd bytes)
                       (push (list fd (coerce bytes 'list)) writes)))
               (let ((cl-tmux::*client-read-only* t))
                 (cl-tmux::dispatch-command s :send-prefix nil))
               (is (null writes)
                   "read-only clients must not write a prefix byte to the pane"))
          (setf (fdefinition 'cl-tmux/pty:pty-write) orig))))))

;;; ── unbound prefix key no-op ─────────────────────────────────────────────────

(test dispatch-unknown-command-is-noop
  "An unrecognized prefix key is silently discarded (no passthrough corruption)."
  ;; Previously the otherwise clause called %passthrough-prefix, injecting
  ;; raw bytes into the pane.  After the fix it must be a silent no-op.
  (with-fake-session (s)
    ;; Dispatching an unknown command must return NIL and must not error.
    (is (null (cl-tmux::dispatch-command s :no-such-command-xyz nil))
        "unknown command must return NIL")
    (is-true cl-tmux::*dirty*
             "dispatch must mark *dirty* even for unknown commands")))

;;; ── :paste-buffer bracketed-paste wrapping ───────────────────────────────────

(test dispatch-paste-buffer-no-crash-without-buffer
  ":paste-buffer with an empty paste buffer is a no-op (no error, marks dirty)."
  (with-fake-session (s)
    (finishes (cl-tmux::dispatch-command s :paste-buffer nil))
    (is-true cl-tmux::*dirty* ":paste-buffer must mark *dirty*")))

;;; ── command-prompt %N template substitution ─────────────────────────────────

(test substitute-percent-replaces-single-percent-args
  "%substitute-percent replaces tmux-style %1/%2 (single percent) with the args —
   the classic `command-prompt -p name: \"new-window -n '%1'\"` idiom."
  (is (string= "new-window -n 'shell'"
               (cl-tmux::%substitute-percent "new-window -n '%1'" '("shell")))
      "%1 must be replaced by the first arg")
  (is (string= "swap a b"
               (cl-tmux::%substitute-percent "swap %1 %2" '("a" "b")))
      "%1 and %2 must be replaced positionally"))

(test substitute-percent-handles-literal-and-edge-cases
  "%% is a literal percent; a missing arg expands to empty; %1 does not match
   inside %10; a non-arg %x is left verbatim."
  (dolist (c '(("100%% done" ()          "100% done" "%% → literal %")
               ("x%2"        ("only-one") "x"         "reference past arg list → empty")
               ("%10"        ("v")        "v0"        "%1 must not match inside %10")
               ("%z"         ("a")        "%z"        "non-digit %x is left verbatim")))
    (destructuring-bind (template args expected desc) c
      (is (string= expected (cl-tmux::%substitute-percent template args)) "~A" desc))))

;;; ── :command-prompt dispatch ─────────────────────────────────────────────────

(test dispatch-command-prompt-opens-prompt
  ":command-prompt opens a prompt with label \": \"."
  (with-fake-session (s)
    (let ((*prompt* nil))
      (cl-tmux::dispatch-command s :command-prompt nil)
      (is (prompt-active-p) ":command-prompt must open a prompt")
      (is (string= ": " (prompt-label *prompt*))
          ":command-prompt prompt label must be \": \""))))

(test dispatch-command-prompt-empty-input-is-noop
  ":command-prompt with empty input does not crash."
  (with-fake-session (s)
    (let ((*prompt* nil))
      (cl-tmux::dispatch-command s :command-prompt nil)
      (is (prompt-active-p) "prompt must be open")
      (finishes (funcall (prompt-on-submit *prompt*) "")
                "empty input must not signal an error"))))

(test dispatch-command-prompt-unknown-command-shows-overlay
  ":command-prompt with an unknown command name shows an error overlay."
  (with-fake-session (s)
    (let ((*prompt* nil) (*overlay* nil))
      (cl-tmux::dispatch-command s :command-prompt nil)
      (funcall (prompt-on-submit *prompt*) "no-such-command-xyz")
      (assert-overlay-contains "unknown command" *overlay*
                               "unknown command"))))

(test dispatch-command-prompt-known-command-executes
  ":command-prompt with 'list-windows' executes that command (opens overlay)."
  (with-fake-session (s :nwindows 1)
    (let ((*prompt* nil) (*overlay* nil))
      (cl-tmux::dispatch-command s :command-prompt nil)
      (funcall (prompt-on-submit *prompt*) "list-windows")
      (assert-overlay-active "list-windows via command-prompt must open an overlay"))))

;;; ── %run-command-line / display-message with arguments ───────────────────────

(test command-prompt-display-message-expands-format
  ":command-prompt 'display-message #{session_name}' expands the format and shows
   the result (not the literal #{...})."
  (with-fake-session (s)
    (let ((*prompt* nil) (*overlay* nil))
      (cl-tmux::dispatch-command s :command-prompt nil)
      (funcall (prompt-on-submit *prompt*) "display-message #{session_name}")
      (assert-overlay-contains "0" *overlay*
                               "command-prompt display-message")
      (assert-overlay-not-contains "#{" *overlay*
                                   "command-prompt display-message"))))

(test run-command-line-no-arg-command-falls-through
  "%run-command-line with a bare command name dispatches it by name (no args)."
  (with-fake-session (s :nwindows 2)
    (let ((*overlay* nil))
      (cl-tmux::%run-command-line s "next-window")
      (is (eq (second (session-windows s)) (session-active-window s))
          "next-window via %run-command-line must switch to the second window"))))

(test run-command-line-display-message-joins-args
  "display-message with multiple unquoted args joins them with spaces."
  (with-fake-session (s)
    (let ((*overlay* nil))
      (cl-tmux::%run-command-line s "display-message hello world")
      (assert-overlay-contains "hello world" *overlay*
                               "display-message hello world"))))

(test display-message-l-flag-shows-literal-format
  "display-message -l shows ARGS verbatim, WITHOUT expanding #{...} formats —
   the inverse of the default expansion path."
  (with-fake-session (s)
    (let ((*overlay* nil))
      (cl-tmux::%run-command-line s "display-message -l #{session_name}")
      (assert-overlay-contains "#{session_name}" *overlay*
                               "display-message -l")
      (assert-overlay-not-contains "0" *overlay*
                                   "display-message -l"))))

(test display-message-rejects-unsupported-flags
  "The in-server display-message command supports overlays via -l/-d/-t only;
   client/stdout/verbose flags must fail instead of becoming
   no-op behavior."
  (with-fake-session (s)
    (dolist (command '("display-message -c someclient #{session_name}"
                       "display-message -p #{session_name}"
                       "display-message -v #{session_name}"))
      (let ((*overlay* nil)
            (cl-tmux::*message-log* nil))
        (is (null (cl-tmux::%run-command-line s command))
            "~A must be rejected" command)
        (assert-overlay-contains "unsupported argument" *overlay* command)
        (assert-overlay-not-contains "someclient" *overlay* command)
        (assert-overlay-not-contains "0" *overlay* command)
        (is (null cl-tmux::*message-log*)
            "~A must not add a message-log entry" command)))))

(test run-command-line-empty-is-noop
  "%run-command-line with blank input does not signal an error."
  (with-fake-session (s)
    (finishes (cl-tmux::%run-command-line s "   ")
              "blank command line must be a safe no-op")))

(test run-command-line-set-option-coerces-boolean
  "'set-option monitor-activity off' stores NIL and 'set-option ... on' stores T (type-coerced).
   Uses monitor-activity — a side-effect-free :boolean option — because `status` is
   now a choice/string option (off|on|2..5), not a boolean."
  (with-fake-session (s)
    (with-isolated-options ()
      (cl-tmux::%run-command-line s "set-option monitor-activity off")
      (is (null (cl-tmux/options:get-option "monitor-activity"))
          "set-option monitor-activity off → NIL (boolean coercion)")
      (cl-tmux::%run-command-line s "set-option monitor-activity on")
      (is (eq t (cl-tmux/options:get-option "monitor-activity"))
          "set-option monitor-activity on → T"))))

(test run-command-line-set-option-string-and-quoted
  "'set-option' stores string option values, and a quoted value keeps its spaces/format."
  (with-fake-session (s)
    (with-isolated-options ()
      (cl-tmux::%run-command-line s "set-option status-left bar")
      (is (string= "bar" (cl-tmux/options:get-option "status-left"))
          "unquoted string value")
      (cl-tmux::%run-command-line s "set-option status-left \"#{session_name} x\"")
      (is (string= "#{session_name} x" (cl-tmux/options:get-option "status-left"))
          "quoted value keeps its space and #{...} intact"))))

(test run-command-line-set-option-scope-flag
  "'set-option -g status off' sets the 'status' option (not an option literally named
   '-g') — the canonical tmux form must work."
  (with-option-session (s)
    (cl-tmux::%run-command-line s "set-option -g status off")
    (is (string= "off" (cl-tmux/options:get-option "status"))
        "set-option -g status off must set 'status' to the choice string \"off\"")
    (is (null (cl-tmux/options:get-option "-g"))
        "must NOT create an option literally named '-g'")))

(test with-option-scope-s-flag-selects-server-scope
  "%with-option-scope routes the -s flag to :server scope with a NIL target
   (audit #9: -s previously fell through to :global)."
  (let ((scope-seen nil)
        (target-seen :unset))
    (cl-tmux::%with-option-scope (make-fake-session) '((#\s . t)) nil
                                 (lambda (scope target)
                                   (setf scope-seen scope
                                         target-seen target)))
    (is (eq :server scope-seen) "-s must select :server scope")
    (is (null target-seen) "server scope has no per-object target")))

(test scope-set-server-writes-server-store
  "%scope-set with :server scope writes the server option store, readable via
   get-server-option (audit #9 end-to-end: server routing reaches the store).
   Uses the real store with restore — mirroring the config-path server tests —
   because rebinding *server-options* in a test unit does not reliably shadow the
   accessor's special binding."
  (let ((original (cl-tmux/options:get-server-option "escape-time")))
    (unwind-protect
         (progn
           (cl-tmux::%scope-set "escape-time" "250" :server nil)
           (is (eql 250 (cl-tmux/options:get-server-option "escape-time"))
               "%scope-set :server must write escape-time to the server store"))
      (cl-tmux/options:set-server-option "escape-time" (or original 500)))))

(test run-command-line-set-option-append-flag
  "'set-option -a <name> <value>' appends to the option's current value."
  (with-fake-session (s)
    (with-isolated-options ("status-left" "A")
      (cl-tmux::%run-command-line s "set-option -a status-left B")
      (is (string= "AB" (cl-tmux/options:get-option "status-left"))
          "set-option -a must append B to the existing 'A'"))))

(test run-command-line-set-option-rejects-unsupported-flags
  "set-option and set-window-option reject unknown flags before mutating option stores."
  (with-fake-session (s)
    (with-isolated-options ("status-left" "ORIG")
      (let ((*overlay* nil))
        (is (null (cl-tmux::%run-command-line s "set-option -x status-left bad"))
            "set-option -x must be rejected")
        (is (string= "ORIG" (cl-tmux/options:get-option "status-left"))
            "set-option -x must not mutate the global option")
        (assert-overlay-contains "unsupported argument" *overlay*
                                  "set-option -x"))))
  (with-fake-session (s :nwindows 1)
    (let ((cl-tmux/options:*global-options* (make-hash-table :test #'equal))
          (*overlay* nil))
      (let ((win (session-active-window s)))
        (is (null (cl-tmux::%run-command-line s "set-window-option -x mode-keys vi"))
            "set-window-option -x must be rejected")
        (is (not (nth-value 1 (gethash "mode-keys"
                                       (cl-tmux/model:window-local-options win))))
            "set-window-option -x must not mutate the window-local option")
        (assert-overlay-active "set-window-option -x must show an error overlay")))))
