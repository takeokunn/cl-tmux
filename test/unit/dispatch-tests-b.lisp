(in-package #:cl-tmux/test)

;;;; dispatch tests — part B: swap-pane forward/backward, kill-pane-confirm,
;;;; pane-resp, select-layout, yank-buffer, list-clients, resize-pane,
;;;; rotate-window, select-pane direction, zoom.

(in-suite dispatch-suite)

;;; ── :swap-pane-forward dispatch ──────────────────────────────────────────────
;;;
;;; Use the shared fixture macro to avoid repeating pane/window/session setup.

(test dispatch-swap-pane-forward-changes-pane-order
  ":swap-pane-forward swaps the active pane with the next pane in the list."
  (with-two-pane-h-session (sess win p0 p1)
    ;; p0 is active and at index 0; forward swap puts p1 at index 0.
    (cl-tmux::dispatch-command sess :swap-pane-forward nil)
    (is (eq p1 (first (window-panes win)))
        "after :swap-pane-forward, p1 must be first in the panes list")
    (is (eq p0 (second (window-panes win)))
        "after :swap-pane-forward, p0 must be second in the panes list")))

(test dispatch-swap-pane-forward-marks-dirty
  ":swap-pane-forward marks *dirty*."
  (with-two-pane-h-session (sess win p0 p1)
    (is (and win p0 p1) "fixture created")
    (cl-tmux::dispatch-command sess :swap-pane-forward nil)
    (is-true cl-tmux::*dirty*
             ":swap-pane-forward must mark *dirty*")))

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
  (with-fake-session (s :nwindows 1 :npanes 2)
    (let* ((win (session-active-window s))
           (p1  (find 1 (window-panes win) :key #'pane-id))
           (p2  (find 2 (window-panes win) :key #'pane-id)))
      (is (eq p1 (first (window-panes win))) "pane 1 (active) starts first")
      (cl-tmux::%run-command-line s "swap-pane -t 2")
      (is (eq p2 (first (window-panes win)))
          "after swap-pane -t 2, pane 2 is first (swapped with active pane 1)"))))

;;; ── :swap-pane-backward dispatch ─────────────────────────────────────────────

(test dispatch-swap-pane-backward-changes-pane-order
  ":swap-pane-backward swaps the active pane with the previous pane (wrapping)."
  (with-two-pane-h-session (sess win p0 p1)
    ;; Start with p1 active so that backward swap moves it to index 0.
    (window-select-pane win p1)
    (cl-tmux::dispatch-command sess :swap-pane-backward nil)
    (is (eq p1 (first (window-panes win)))
        "after :swap-pane-backward from p1, p1 must be first in the panes list")
    (is (eq p0 (second (window-panes win)))
        "after :swap-pane-backward from p1, p0 must be second in the panes list")))

(test dispatch-swap-pane-backward-marks-dirty
  ":swap-pane-backward marks *dirty*."
  (with-two-pane-h-session (sess win p0 p1)
    (is-false (null p0) "fixture created")
    (window-select-pane win p1)
    (cl-tmux::dispatch-command sess :swap-pane-backward nil)
    (is-true cl-tmux::*dirty*
             ":swap-pane-backward must mark *dirty*")))

;;; ── :kill-pane-confirm dispatch ──────────────────────────────────────────────

(test dispatch-kill-pane-confirm-opens-prompt
  ":kill-pane-confirm opens a y/n prompt and does NOT kill immediately."
  (with-fake-session (s :nwindows 1 :npanes 2)
    (let ((*prompt* nil))
      (cl-tmux::dispatch-command s :kill-pane-confirm nil)
      (is (prompt-active-p)
          ":kill-pane-confirm must open a prompt")
      ;; Window should still have both panes (no kill yet).
      (is (= 2 (length (window-panes (session-active-window s))))
          ":kill-pane-confirm must not kill the pane before confirmation"))))

(test dispatch-kill-pane-confirm-kills-on-y
  ":kill-pane-confirm kills the pane when the user submits \"y\"."
  (with-fake-session (s :nwindows 1 :npanes 2)
    (let ((*prompt* nil))
      (cl-tmux::dispatch-command s :kill-pane-confirm nil)
      (is (prompt-active-p) "prompt must be open")
      ;; Submit "y" — should kill the active pane.
      (funcall (prompt-on-submit *prompt*) "y")
      (is (= 1 (length (window-panes (session-active-window s))))
          "submitting \"y\" must kill the active pane"))))

(test dispatch-kill-pane-confirm-no-kill-on-n
  ":kill-pane-confirm does NOT kill when the user submits \"n\"."
  (with-fake-session (s :nwindows 1 :npanes 2)
    (let ((*prompt* nil))
      (cl-tmux::dispatch-command s :kill-pane-confirm nil)
      (is (prompt-active-p) "prompt must be open")
      ;; Submit "n" — should NOT kill.
      (funcall (prompt-on-submit *prompt*) "n")
      (is (= 2 (length (window-panes (session-active-window s))))
          "submitting \"n\" must NOT kill the pane"))))

;;; ── :kill-window-confirm dispatch ────────────────────────────────────────────

(test dispatch-kill-window-confirm-opens-prompt
  ":kill-window-confirm opens a y/n prompt and does NOT kill immediately."
  (with-fake-session (s :nwindows 2)
    (let ((*prompt* nil))
      (cl-tmux::dispatch-command s :kill-window-confirm nil)
      (is (prompt-active-p)
          ":kill-window-confirm must open a prompt")
      ;; Both windows should still be present.
      (is (= 2 (length (session-windows s)))
          ":kill-window-confirm must not kill the window before confirmation"))))

(test dispatch-kill-window-confirm-kills-on-y
  ":kill-window-confirm kills the window when the user submits \"y\"."
  (with-fake-session (s :nwindows 2)
    (let ((*prompt* nil))
      (cl-tmux::dispatch-command s :kill-window-confirm nil)
      (is (prompt-active-p) "prompt must be open")
      ;; Submit "y" — should kill the active window.
      (funcall (prompt-on-submit *prompt*) "y")
      (is (= 1 (length (session-windows s)))
          "submitting \"y\" must kill the active window"))))

(test dispatch-kill-window-confirm-no-kill-on-n
  ":kill-window-confirm does NOT kill when the user submits \"n\"."
  (with-fake-session (s :nwindows 2)
    (let ((*prompt* nil))
      (cl-tmux::dispatch-command s :kill-window-confirm nil)
      (is (prompt-active-p) "prompt must be open")
      ;; Submit "n" — should NOT kill.
      (funcall (prompt-on-submit *prompt*) "n")
      (is (= 2 (length (session-windows s)))
          "submitting \"n\" must NOT kill the window"))))

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
  (is (string= "100% done" (cl-tmux::%substitute-percent "100%% done" '()))
      "%% → literal %")
  (is (string= "x" (cl-tmux::%substitute-percent "x%2" '("only-one")))
      "a reference past the arg list expands to empty")
  (is (string= "v0" (cl-tmux::%substitute-percent "%10" '("v")))
      "%1 must not match inside %10 (single left-to-right pass)")
  (is (string= "%z" (cl-tmux::%substitute-percent "%z" '("a")))
      "a non-digit %x is left verbatim"))

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
      (is (overlay-active-p) "unknown command must open an error overlay")
      (let ((text (format nil "~{~A~%~}" (overlay-lines))))
        (is (search "unknown command" text)
            "overlay must contain the 'unknown command' error message")))))

(test dispatch-command-prompt-known-command-executes
  ":command-prompt with 'list-windows' executes that command (opens overlay)."
  (with-fake-session (s :nwindows 1)
    (let ((*prompt* nil) (*overlay* nil))
      (cl-tmux::dispatch-command s :command-prompt nil)
      (funcall (prompt-on-submit *prompt*) "list-windows")
      (is (overlay-active-p) "list-windows via command-prompt must open an overlay"))))

;;; ── %run-command-line / display-message with arguments ───────────────────────

(test command-prompt-display-message-expands-format
  ":command-prompt 'display-message #{session_name}' expands the format and shows
   the result (not the literal #{...})."
  (with-fake-session (s)
    (let ((*prompt* nil) (*overlay* nil))
      (cl-tmux::dispatch-command s :command-prompt nil)
      (funcall (prompt-on-submit *prompt*) "display-message #{session_name}")
      (is (overlay-active-p) "display-message must open an overlay")
      (let ((text (format nil "~{~A~%~}" (overlay-lines))))
        (is (search "0" text)
            "overlay must contain the expanded session name '0' (got ~S)" text)
        (is (null (search "#{" text))
            "the #{...} format must be expanded, not shown literally (got ~S)" text)))))

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
      (let ((text (format nil "~{~A~%~}" (overlay-lines))))
        (is (search "hello world" text)
            "joined args 'hello world' must appear in the overlay (got ~S)" text)))))

(test display-message-l-flag-shows-literal-format
  "display-message -l shows ARGS verbatim, WITHOUT expanding #{...} formats —
   the inverse of the default expansion path."
  (with-fake-session (s)
    (let ((*overlay* nil))
      (cl-tmux::%run-command-line s "display-message -l #{session_name}")
      (is (overlay-active-p) "display-message -l must still open an overlay")
      (let ((text (format nil "~{~A~%~}" (overlay-lines))))
        (is (search "#{session_name}" text)
            "-l must show the literal #{session_name}, not expand it (got ~S)" text)))))

(test display-message-c-flag-consumes-client-arg
  "display-message -c <client> consumes the client name (a no-op target) instead
   of leaking it into the format text."
  (with-fake-session (s)
    (let ((*overlay* nil))
      (cl-tmux::%run-command-line s "display-message -c someclient #{session_name}")
      (let ((text (format nil "~{~A~%~}" (overlay-lines))))
        (is (search "0" text) "the expanded session name must appear")
        (is (null (search "someclient" text))
            "-c client name must NOT appear in the message (got ~S)" text)))))

(test run-command-line-empty-is-noop
  "%run-command-line with blank input does not signal an error."
  (with-fake-session (s)
    (finishes (cl-tmux::%run-command-line s "   ")
              "blank command line must be a safe no-op")))

(test run-command-line-set-option-coerces-boolean
  "'set monitor-activity off' stores NIL and 'set ... on' stores T (type-coerced).
   Uses monitor-activity — a side-effect-free :boolean option — because `status` is
   now a choice/string option (off|on|2..5), not a boolean."
  (with-fake-session (s)
    (with-isolated-options ()
      (cl-tmux::%run-command-line s "set monitor-activity off")
      (is (null (cl-tmux/options:get-option "monitor-activity"))
          "set monitor-activity off → NIL (boolean coercion)")
      (cl-tmux::%run-command-line s "set monitor-activity on")
      (is (eq t (cl-tmux/options:get-option "monitor-activity"))
          "set monitor-activity on → T"))))

(test run-command-line-set-option-string-and-quoted
  "'set' stores string option values, and a quoted value keeps its spaces/format."
  (with-fake-session (s)
    (with-isolated-options ()
      (cl-tmux::%run-command-line s "set status-left bar")
      (is (string= "bar" (cl-tmux/options:get-option "status-left"))
          "unquoted string value")
      (cl-tmux::%run-command-line s "set status-left \"#{session_name} x\"")
      (is (string= "#{session_name} x" (cl-tmux/options:get-option "status-left"))
          "quoted value keeps its space and #{...} intact"))))

(test run-command-line-set-option-scope-flag
  "'set -g status off' sets the 'status' option (not an option literally named
   '-g') — the canonical tmux form must work."
  (with-fake-session (s)
    (with-isolated-options ()
      (cl-tmux::%run-command-line s "set -g status off")
      (is (string= "off" (cl-tmux/options:get-option "status"))
          "set -g status off must set 'status' to the choice string \"off\"")
      (is (null (cl-tmux/options:get-option "-g"))
          "must NOT create an option literally named '-g'"))))

(test run-command-line-set-option-append-flag
  "'set -a <name> <value>' appends to the option's current value."
  (with-fake-session (s)
    (with-isolated-options ("status-left" "A")
      (cl-tmux::%run-command-line s "set -a status-left B")
      (is (string= "AB" (cl-tmux/options:get-option "status-left"))
          "set -a must append B to the existing 'A'"))))
