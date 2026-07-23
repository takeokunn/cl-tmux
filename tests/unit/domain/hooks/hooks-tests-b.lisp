(in-package #:cl-tmux/test)

;;;; hooks tests — part B: command hooks (set-hook directive), set-hook -u,
;;;; list-command-hooks, runtime set-hook, config set-hook -g, show-hooks.

(describe "hooks-suite"

  ;;; ── Command hooks (the `set-hook` directive) ──────────────────────────────────

  ;; set-command-hook registers a command keyword under an event name.
  (it "set-command-hook-stores-keyword"
    (with-isolated-hooks
      (cl-tmux/hooks:set-command-hook "after-new-window" :next-window)
      (expect (equal '(:next-window) (cl-tmux/hooks:command-hooks "after-new-window")))))

  ;; set-command-hook REPLACES the event's hook list (tmux set-hook without -a).
  (it "set-command-hook-replaces-existing"
    (with-isolated-hooks
      (cl-tmux/hooks:set-command-hook "after-new-window" :next-window)
      (cl-tmux/hooks:set-command-hook "after-new-window" :rename-window)
      (expect (equal '(:rename-window)
                 (cl-tmux/hooks:command-hooks "after-new-window")))))

  ;; append-command-hook accumulates in registration order (tmux set-hook -a).
  (it "append-command-hook-accumulates-in-order"
    (with-isolated-hooks
      (cl-tmux/hooks:append-command-hook "after-new-window" :next-window)
      (cl-tmux/hooks:append-command-hook "after-new-window" :rename-window)
      (expect (equal '(:next-window :rename-window)
                 (cl-tmux/hooks:command-hooks "after-new-window")))))

  ;; clear-command-hooks removes every command hook for an event.
  (it "clear-command-hooks-removes-all"
    (with-isolated-hooks
      (cl-tmux/hooks:set-command-hook "after-new-window" :next-window)
      (cl-tmux/hooks:clear-command-hooks "after-new-window")
      (expect (null (cl-tmux/hooks:command-hooks "after-new-window")))))

  ;; clear-command-hooks on an event with no registered command hooks is a safe no-op.
  (it "clear-command-hooks-unregistered-event-is-noop"
    (with-isolated-hooks
      (finishes (cl-tmux/hooks:clear-command-hooks "totally-unknown-event"))
      (finishes (cl-tmux/hooks:clear-command-hooks "after-new-window"))
      (expect (null (cl-tmux/hooks:command-hooks "after-new-window")))))

  ;;; ── set-hook -u (unset) directive ────────────────────────────────────────────
  ;;;
  ;;; The `set-hook -u <event>` directive clears every command hook registered for
  ;;; the event (same effect as -r), reverting the event to having no hooks.

  ;; set-hook -u <event> clears the command hooks previously registered for that event.
  (it "set-hook-u-unsets-registered-command-hook"
    (with-isolated-hooks
      (with-isolated-config
        ;; Register a command hook via the config directive.
        (cl-tmux/config:load-config-from-string "set-hook after-new-window next-window")
        (expect (equal '("next-window") (cl-tmux/hooks:command-hooks "after-new-window")))
        ;; Now unset it via -u.
        (cl-tmux/config:load-config-from-string "set-hook -u after-new-window")
        (expect (null (cl-tmux/hooks:command-hooks "after-new-window"))))))

  ;; The set-hook directive REPLACES the event's hook by default and APPENDS with
  ;; -a (tmux semantics), so a config re-setting the same event does not silently
  ;; accumulate duplicate command runs.
  (it "set-hook-replaces-by-default-and-appends-with-a"
    (with-isolated-hooks
      (with-isolated-config
        (cl-tmux/config:load-config-from-string "set-hook after-new-window next-window")
        (cl-tmux/config:load-config-from-string "set-hook after-new-window previous-window")
        (expect (equal '("previous-window") (cl-tmux/hooks:command-hooks "after-new-window")))
        (cl-tmux/config:load-config-from-string "set-hook -a after-new-window last-window")
        (expect (equal '("previous-window" "last-window")
                   (cl-tmux/hooks:command-hooks "after-new-window"))))))

  ;;; ── list-command-hooks ────────────────────────────────────────────────────────

  ;; list-command-hooks returns an alist of (event-name . command-keyword-list)
  ;; for every registered command hook event.
  (it "list-command-hooks-returns-alist"
    (with-isolated-hooks
      (cl-tmux/hooks:append-command-hook "after-new-window" :next-window)
      (cl-tmux/hooks:append-command-hook "after-new-window" :rename-window)
      (cl-tmux/hooks:set-command-hook "pane-exited"      :kill-pane)
      (let ((alist (cl-tmux/hooks:list-command-hooks)))
        (expect (= 2 (length alist)))
        (let ((nw-entry (assoc "after-new-window" alist :test #'string=))
              (pe-entry (assoc "pane-exited"      alist :test #'string=)))
          (expect (not (null nw-entry)))
          (expect (equal '(:next-window :rename-window) (cdr nw-entry)))
          (expect (not (null pe-entry)))
          (expect (equal '(:kill-pane) (cdr pe-entry)))))))

  ;; list-command-hooks returns NIL on an empty *command-hooks* table.
  (it "list-command-hooks-empty-registry"
    (with-isolated-hooks
      (expect (null (cl-tmux/hooks:list-command-hooks)))))

  ;; Sorting the result of list-command-hooks does not corrupt *command-hooks*.
  ;; (Ensures the caller — describe-command-hooks — uses copy-list before sorting.)
  (it "list-command-hooks-does-not-mutate-on-sort"
    (with-isolated-hooks
      (cl-tmux/hooks:set-command-hook "z-event" :next-window)
      (cl-tmux/hooks:set-command-hook "a-event" :prev-window)
      ;; Sort the result in place (worst-case destructive caller).
      (sort (cl-tmux/hooks:list-command-hooks) #'string< :key #'car)
      ;; Both events must still be retrievable from the live registry.
      (expect (equal '(:next-window) (cl-tmux/hooks:command-hooks "z-event")))
      (expect (equal '(:prev-window) (cl-tmux/hooks:command-hooks "a-event")))))

  ;; set-hook <event> <command> stores the raw command string for later execution.
  (it "set-hook-directive-registers-command-hook"
    (with-isolated-hooks
      (let ((applied (cl-tmux/config:load-config-from-string
                      "set-hook after-new-window next-window")))
        (expect (= 1 applied))
        ;; Command hooks are now stored as raw strings (run via %run-command-line
        ;; at hook-fire time) rather than pre-resolved keywords, so format
        ;; expansion and argument passing work in hook commands.
        (expect (equal '("next-window") (cl-tmux/hooks:command-hooks "after-new-window"))))))

  ;; set-hook stores multi-token command strings (e.g. 'display-message #{session_name}').
  (it "set-hook-directive-stores-string-with-args"
    (with-isolated-hooks
      (cl-tmux/config:load-config-from-string
       "set-hook after-new-window display-message #{session_name}")
      (let ((hooks (cl-tmux/hooks:command-hooks "after-new-window")))
        (expect (= 1 (length hooks)))
        (expect (stringp (first hooks)))
        (expect (search "display-message" (first hooks))))))

  ;; set-hook stores any command string, even unknown names (validated at fire time).
  (it "set-hook-directive-accepts-any-command-name"
    (with-isolated-hooks
      (cl-tmux/config:load-config-from-string "set-hook after-new-window no-such-command")
      ;; String is stored and validated at fire time, not at config-load time.
      ;; This matches real tmux behavior where set-hook doesn't validate command names.
      (expect (equal '("no-such-command") (cl-tmux/hooks:command-hooks "after-new-window")))))

  ;; run-command-hooks dispatches each registered command keyword on the session.
  (it "run-command-hooks-dispatches-registered-commands"
    (with-isolated-hooks
      (with-fake-session (s :nwindows 2)
        (cl-tmux/hooks:set-command-hook "after-new-window" :next-window)
        (cl-tmux::run-command-hooks "after-new-window" s)
        (expect (eq (second (session-windows s)) (session-active-window s))))))

  ;;; ── set-hook as a RUNTIME command (command-prompt / bind / control mode) ─────

  ;; set-hook run at runtime via %run-command-line registers the command hook, and
  ;; the -g flag is skipped (EVENT is after-new-window, not "-g").
  (it "runtime-set-hook-command-registers-with-g-flag"
    (with-isolated-hooks
      (with-fake-session (s)
        (cl-tmux::%run-command-line s "set-hook -g after-new-window next-window")
        (expect (equal '("next-window") (cl-tmux/hooks:command-hooks "after-new-window")))
        (expect (null (cl-tmux/hooks:command-hooks "-g"))))))

  ;; set-hook -u <event> run at runtime clears the event's command hooks.
  (it "runtime-set-hook-u-unsets-command-hook"
    (with-isolated-hooks
      (with-fake-session (s)
        (cl-tmux/hooks:set-command-hook "after-new-window" "next-window")
        (cl-tmux::%run-command-line s "set-hook -u after-new-window")
        (expect (null (cl-tmux/hooks:command-hooks "after-new-window"))))))

  ;; set-hook -R <event> <command> run at runtime registers the hook and fires it immediately.
  (it "runtime-set-hook-r-runs-event-hooks-immediately"
    (with-isolated-hooks
      (with-fake-session (s :nwindows 2)
        (cl-tmux::%run-command-line s "set-hook -R after-new-window next-window")
        (expect (equal '("next-window") (cl-tmux/hooks:command-hooks "after-new-window")))
        (expect (eq (second (session-windows s)) (session-active-window s))))))

  ;; set-hook -g <event> <cmd> in .tmux.conf registers under EVENT, not "-g"
  ;; (regression: leading flags other than -r/-u were previously taken as the event).
  (it "config-set-hook-g-flag-registers-under-event"
    (with-isolated-hooks
      (cl-tmux/config:load-config-from-string "set-hook -g after-new-window next-window")
      (expect (equal '("next-window") (cl-tmux/hooks:command-hooks "after-new-window")))
      (expect (null (cl-tmux/hooks:command-hooks "-g")))))

  ;; Killing a pane fires the after-kill-pane command hook through the runner.
  (it "command-hook-fires-on-after-kill-pane-via-runner"
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
          (expect (eq (second (session-windows s)) (session-active-window s)))))))

  ;;; ── *command-hook-runner* and run-command-hooks-via-runner ───────────────────

  ;; run-command-hooks-via-runner is a safe no-op when *command-hook-runner* is NIL.
  (it "command-hook-runner-nil-is-noop"
    (with-isolated-hooks
      (let ((cl-tmux/hooks:*command-hook-runner* nil))
        (finishes
          (cl-tmux/hooks:run-command-hooks-via-runner "after-new-window" nil)
          "run-command-hooks-via-runner with nil runner must not signal"))))

  ;; run-command-hooks-via-runner calls *command-hook-runner* with event-name and session
  ;; when a runner is installed.
  (it "command-hook-runner-installed-is-called"
    (with-isolated-hooks
      (let ((received-event nil)
            (received-session nil))
        (let ((cl-tmux/hooks:*command-hook-runner*
               (lambda (event session)
                 (setf received-event event
                       received-session session))))
          (cl-tmux/hooks:run-command-hooks-via-runner "after-new-window" :fake-session)
          (expect (string= "after-new-window" received-event))
          (expect (eq :fake-session received-session))))))

  ;; Setting *command-hook-runner* to NIL still allows lisp-function hooks to run
  ;; normally; only the command-hook dispatch is skipped.
  (it "command-hook-runner-nil-does-not-suppress-lisp-hooks"
    (with-isolated-hooks
      (let ((cl-tmux/hooks:*command-hook-runner* nil)
            (called nil))
        (cl-tmux/hooks:add-hook "after-new-window" (lambda () (setf called t)))
        (cl-tmux/hooks:run-hooks "after-new-window")
        (expect called :to-be-truthy))))

  ;;; ── show-hooks (inspect registered command hooks) ─────────────────────────────

  ;; describe-command-hooks reports the empty state when no command hooks are set.
  (it "describe-command-hooks-empty-message"
    (with-isolated-hooks
      (expect (search "no command hooks" (cl-tmux/hooks:describe-command-hooks)))))

  ;; describe-command-hooks lists each event and its (downcased) commands.
  (it "describe-command-hooks-lists-registered"
    (with-isolated-hooks
      (cl-tmux/hooks:set-command-hook "after-new-window" :next-window)
      (let ((desc (cl-tmux/hooks:describe-command-hooks)))
        (expect (search "after-new-window" desc))
        (expect (search "next-window" desc)))))

  ;; :show-hooks dispatches without error and opens an overlay listing the hooks.
  (it "dispatch-show-hooks-opens-overlay"
    (with-isolated-hooks
      (with-fake-session (s)
        (with-clean-overlay
          (cl-tmux/hooks:set-command-hook "after-new-window" :next-window)
          (cl-tmux::dispatch-command s :show-hooks nil)
          (assert-overlay-active ":show-hooks must open an overlay")
          (expect (search "after-new-window" (or *overlay* ""))))))))
