(in-package #:cl-tmux/test)

;;;; hooks tests — part B: command hooks (set-hook directive), set-hook -u,
;;;; list-command-hooks, runtime set-hook, config set-hook -g, show-hooks.

(in-suite hooks-suite)

;;; ── Command hooks (the `set-hook` directive) ──────────────────────────────────

(test set-command-hook-stores-keyword
  "set-command-hook registers a command keyword under an event name."
  (with-isolated-hooks
    (cl-tmux/hooks:set-command-hook "after-new-window" :next-window)
    (is (equal '(:next-window) (cl-tmux/hooks:command-hooks "after-new-window"))
        "command-hooks must return the registered keyword")))

(test set-command-hook-replaces-existing
  "set-command-hook REPLACES the event's hook list (tmux set-hook without -a)."
  (with-isolated-hooks
    (cl-tmux/hooks:set-command-hook "after-new-window" :next-window)
    (cl-tmux/hooks:set-command-hook "after-new-window" :rename-window)
    (is (equal '(:rename-window)
               (cl-tmux/hooks:command-hooks "after-new-window"))
        "a second set-command-hook must replace, not accumulate")))

(test append-command-hook-accumulates-in-order
  "append-command-hook accumulates in registration order (tmux set-hook -a)."
  (with-isolated-hooks
    (cl-tmux/hooks:append-command-hook "after-new-window" :next-window)
    (cl-tmux/hooks:append-command-hook "after-new-window" :rename-window)
    (is (equal '(:next-window :rename-window)
               (cl-tmux/hooks:command-hooks "after-new-window"))
        "append-command-hook must accumulate in order")))

(test clear-command-hooks-removes-all
  "clear-command-hooks removes every command hook for an event."
  (with-isolated-hooks
    (cl-tmux/hooks:set-command-hook "after-new-window" :next-window)
    (cl-tmux/hooks:clear-command-hooks "after-new-window")
    (is (null (cl-tmux/hooks:command-hooks "after-new-window"))
        "after clear-command-hooks the event must have no command hooks")))

;;; ── set-hook -u (unset) directive ────────────────────────────────────────────
;;;
;;; The `set-hook -u <event>` directive clears every command hook registered for
;;; the event (same effect as -r), reverting the event to having no hooks.

(test set-hook-u-unsets-registered-command-hook
  "set-hook -u <event> clears the command hooks previously registered for that event."
  (with-isolated-hooks
    (with-isolated-config
      ;; Register a command hook via the config directive.
      (cl-tmux/config:load-config-from-string "set-hook after-new-window next-window")
      (is (equal '("next-window") (cl-tmux/hooks:command-hooks "after-new-window"))
          "precondition: set-hook must register the raw command string")
      ;; Now unset it via -u.
      (cl-tmux/config:load-config-from-string "set-hook -u after-new-window")
      (is (null (cl-tmux/hooks:command-hooks "after-new-window"))
          "set-hook -u must leave the event with no command hooks"))))

(test set-hook-replaces-by-default-and-appends-with-a
  "The set-hook directive REPLACES the event's hook by default and APPENDS with
   -a (tmux semantics), so a config re-setting the same event does not silently
   accumulate duplicate command runs."
  (with-isolated-hooks
    (with-isolated-config
      (cl-tmux/config:load-config-from-string "set-hook after-new-window next-window")
      (cl-tmux/config:load-config-from-string "set-hook after-new-window previous-window")
      (is (equal '("previous-window") (cl-tmux/hooks:command-hooks "after-new-window"))
          "a plain second set-hook must REPLACE the prior hook")
      (cl-tmux/config:load-config-from-string "set-hook -a after-new-window last-window")
      (is (equal '("previous-window" "last-window")
                 (cl-tmux/hooks:command-hooks "after-new-window"))
          "set-hook -a must APPEND, preserving the prior hook"))))

;;; ── list-command-hooks ────────────────────────────────────────────────────────

(test list-command-hooks-returns-alist
  "list-command-hooks returns an alist of (event-name . command-keyword-list)
   for every registered command hook event."
  (with-isolated-hooks
    (cl-tmux/hooks:append-command-hook "after-new-window" :next-window)
    (cl-tmux/hooks:append-command-hook "after-new-window" :rename-window)
    (cl-tmux/hooks:set-command-hook "pane-exited"      :kill-pane)
    (let ((alist (cl-tmux/hooks:list-command-hooks)))
      (is (= 2 (length alist))
          "list-command-hooks must return one entry per registered event")
      (let ((nw-entry (assoc "after-new-window" alist :test #'string=))
            (pe-entry (assoc "pane-exited"      alist :test #'string=)))
        (is (not (null nw-entry))
            "after-new-window must appear in the alist")
        (is (equal '(:next-window :rename-window) (cdr nw-entry))
            "after-new-window must list both commands in order")
        (is (not (null pe-entry))
            "pane-exited must appear in the alist")
        (is (equal '(:kill-pane) (cdr pe-entry))
            "pane-exited must list its single command")))))

(test list-command-hooks-empty-registry
  "list-command-hooks returns NIL on an empty *command-hooks* table."
  (with-isolated-hooks
    (is (null (cl-tmux/hooks:list-command-hooks))
        "list-command-hooks must return NIL when no command hooks are registered")))

(test list-command-hooks-does-not-mutate-on-sort
  "Sorting the result of list-command-hooks does not corrupt *command-hooks*.
   (Ensures the caller — describe-command-hooks — uses copy-list before sorting.)"
  (with-isolated-hooks
    (cl-tmux/hooks:set-command-hook "z-event" :next-window)
    (cl-tmux/hooks:set-command-hook "a-event" :prev-window)
    ;; Sort the result in place (worst-case destructive caller).
    (sort (cl-tmux/hooks:list-command-hooks) #'string< :key #'car)
    ;; Both events must still be retrievable from the live registry.
    (is (equal '(:next-window) (cl-tmux/hooks:command-hooks "z-event"))
        "z-event must survive a destructive sort of the alist snapshot")
    (is (equal '(:prev-window) (cl-tmux/hooks:command-hooks "a-event"))
        "a-event must survive a destructive sort of the alist snapshot")))

(test set-hook-directive-registers-command-hook
  "set-hook <event> <command> stores the raw command string for later execution."
  (with-isolated-hooks
    (let ((applied (cl-tmux/config:load-config-from-string
                    "set-hook after-new-window next-window")))
      (is (= 1 applied) "set-hook must apply as exactly 1 directive")
      ;; Command hooks are now stored as raw strings (run via %run-command-line
      ;; at hook-fire time) rather than pre-resolved keywords, so format
      ;; expansion and argument passing work in hook commands.
      (is (equal '("next-window") (cl-tmux/hooks:command-hooks "after-new-window"))
          "set-hook must register the command string for after-new-window"))))

(test set-hook-directive-stores-string-with-args
  "set-hook stores multi-token command strings (e.g. 'display-message #{session_name}')."
  (with-isolated-hooks
    (cl-tmux/config:load-config-from-string
     "set-hook after-new-window display-message #{session_name}")
    (let ((hooks (cl-tmux/hooks:command-hooks "after-new-window")))
      (is (= 1 (length hooks)) "must register exactly one hook")
      (is (stringp (first hooks)) "hook entry must be a string for format expansion")
      (is (search "display-message" (first hooks))
          "hook string must include the command name"))))

(test set-hook-directive-accepts-any-command-name
  "set-hook stores any command string, even unknown names (validated at fire time)."
  (with-isolated-hooks
    (cl-tmux/config:load-config-from-string "set-hook after-new-window no-such-command")
    ;; String is stored and validated at fire time, not at config-load time.
    ;; This matches real tmux behavior where set-hook doesn't validate command names.
    (is (equal '("no-such-command") (cl-tmux/hooks:command-hooks "after-new-window"))
        "set-hook must store the command string regardless of whether it is known")))

(test run-command-hooks-dispatches-registered-commands
  "run-command-hooks dispatches each registered command keyword on the session."
  (with-isolated-hooks
    (with-fake-session (s :nwindows 2)
      (cl-tmux/hooks:set-command-hook "after-new-window" :next-window)
      (cl-tmux::run-command-hooks "after-new-window" s)
      (is (eq (second (session-windows s)) (session-active-window s))
          "run-command-hooks must dispatch :next-window, advancing the active window"))))

;;; ── set-hook as a RUNTIME command (command-prompt / bind / control mode) ─────

(test runtime-set-hook-command-registers-with-g-flag
  "set-hook run at runtime via %run-command-line registers the command hook, and
   the -g flag is skipped (EVENT is after-new-window, not \"-g\")."
  (with-isolated-hooks
    (with-fake-session (s)
      (cl-tmux::%run-command-line s "set-hook -g after-new-window next-window")
      (is (equal '("next-window") (cl-tmux/hooks:command-hooks "after-new-window"))
          "runtime set-hook -g must register under the event, not under -g")
      (is (null (cl-tmux/hooks:command-hooks "-g"))
          "no hook may be registered under the literal flag -g"))))

(test runtime-set-hook-u-unsets-command-hook
  "set-hook -u <event> run at runtime clears the event's command hooks."
  (with-isolated-hooks
    (with-fake-session (s)
      (cl-tmux/hooks:set-command-hook "after-new-window" "next-window")
      (cl-tmux::%run-command-line s "set-hook -u after-new-window")
      (is (null (cl-tmux/hooks:command-hooks "after-new-window"))
          "runtime set-hook -u must clear the event's command hooks"))))

(test runtime-set-hook-r-runs-event-hooks-immediately
  "set-hook -R <event> <command> run at runtime registers the hook and fires it immediately."
  (with-isolated-hooks
    (with-fake-session (s :nwindows 2)
      (cl-tmux::%run-command-line s "set-hook -R after-new-window next-window")
      (is (equal '("next-window") (cl-tmux/hooks:command-hooks "after-new-window"))
          "runtime set-hook -R must register the command hook")
      (is (eq (second (session-windows s)) (session-active-window s))
          "runtime set-hook -R must run the registered hook immediately"))))

(test config-set-hook-g-flag-registers-under-event
  "set-hook -g <event> <cmd> in .tmux.conf registers under EVENT, not \"-g\"
   (regression: leading flags other than -r/-u were previously taken as the event)."
  (with-isolated-hooks
    (cl-tmux/config:load-config-from-string "set-hook -g after-new-window next-window")
    (is (equal '("next-window") (cl-tmux/hooks:command-hooks "after-new-window"))
        "config set-hook -g must register under the event")
    (is (null (cl-tmux/hooks:command-hooks "-g"))
        "config set-hook must not register under the literal flag -g")))

(test command-hook-fires-on-after-kill-pane-via-runner
  "Killing a pane fires the after-kill-pane command hook through the runner."
  (with-isolated-hooks
    (with-fake-session (s :nwindows 2 :npanes 2)
      ;; Register s in *server-sessions* so %session-of-pane can find it when
      ;; the command hook runner needs to dispatch :next-window against a session.
      (let ((cl-tmux::*server-sessions* (list (cons (cl-tmux::session-name s) s))))
        (cl-tmux/hooks:set-command-hook "after-kill-pane" :next-window)
        ;; kill-pane fires after-kill-pane (now before window-remove-pane so the
        ;; pane is still in *server-sessions*), then the runner dispatches :next-window.
        ;; No fork: fake panes have fd -1 so pty-close is a guarded no-op.
        (cl-tmux/commands:kill-pane s)
        (is (eq (second (session-windows s)) (session-active-window s))
            "after-kill-pane command hook (:next-window) must advance the active window")))))

;;; ── show-hooks (inspect registered command hooks) ─────────────────────────────

(test describe-command-hooks-empty-message
  "describe-command-hooks reports the empty state when no command hooks are set."
  (with-isolated-hooks
    (is (search "no command hooks" (cl-tmux/hooks:describe-command-hooks))
        "describe-command-hooks must report the empty state")))

(test describe-command-hooks-lists-registered
  "describe-command-hooks lists each event and its (downcased) commands."
  (with-isolated-hooks
    (cl-tmux/hooks:set-command-hook "after-new-window" :next-window)
    (let ((desc (cl-tmux/hooks:describe-command-hooks)))
      (is (search "after-new-window" desc) "must list the event name")
      (is (search "next-window" desc) "must list the command (downcased)"))))

(test dispatch-show-hooks-opens-overlay
  ":show-hooks dispatches without error and opens an overlay listing the hooks."
  (with-isolated-hooks
    (with-fake-session (s)
      (let ((*overlay* nil))
        (cl-tmux/hooks:set-command-hook "after-new-window" :next-window)
        (cl-tmux::dispatch-command s :show-hooks nil)
        (assert-overlay-active ":show-hooks must open an overlay")
        (is (search "after-new-window" (or *overlay* ""))
            "the overlay must list the registered hook")))))
