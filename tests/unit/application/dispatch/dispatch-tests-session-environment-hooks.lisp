(in-package #:cl-tmux/test)

;;;; Dispatch session environment, client size, and hook tests

(in-suite dispatch-suite)

;;; ── set-environment -h / show-environment -h (hidden variables) ──────────────

(test set-environment-h-hides-variable
  "set-environment -h marks a session variable hidden: excluded from the child
   environment and the plain show-environment listing, listed only by -h, and
   unhidden again by a later plain set (tmux ENVIRON_HIDDEN)."
  (with-fake-session (s)
    (cl-tmux::%cmd-set-environment-prompt s '("CLTMUX_VIS" "v1"))
    (cl-tmux::%cmd-set-environment-prompt s '("-h" "CLTMUX_HID" "secret"))
    ;; Child environment: hidden var excluded, visible var included.
    (let ((child (cl-tmux/model:session-child-environment s)))
      (is (notany (lambda (e) (eql 0 (search "CLTMUX_HID=" e))) child)
          "hidden variable must not reach the child environment")
      (is (some (lambda (e) (eql 0 (search "CLTMUX_VIS=" e))) child)
          "visible variable must reach the child environment"))
    ;; Plain listing excludes hidden; -h lists only hidden.
    (let ((*overlay* nil))
      (cl-tmux::%cmd-show-environment-arg s '())
      (is (null (search "CLTMUX_HID" *overlay*))
          "plain show-environment must exclude the hidden variable")
      (is (search "CLTMUX_VIS" *overlay*)
          "plain show-environment must include the visible variable"))
    (let ((*overlay* nil))
      (cl-tmux::%cmd-show-environment-arg s '("-h"))
      (is (search "CLTMUX_HID" *overlay*)
          "show-environment -h must list the hidden variable")
      (is (null (search "CLTMUX_VIS" *overlay*))
          "show-environment -h must list ONLY hidden variables"))
    ;; A later plain set unhides.
    (cl-tmux::%cmd-set-environment-prompt s '("CLTMUX_HID" "public-now"))
    (is (some (lambda (e) (eql 0 (search "CLTMUX_HID=" e)))
              (cl-tmux/model:session-child-environment s))
        "a plain set must clear the hidden mark")))

(test set-environment-hg-hides-global-variable
  "set-environment -hg marks a GLOBAL variable hidden: stripped from child
   environments even though it lives in the real process environment."
  (with-fake-session (s)
    (let ((cl-tmux/model:*global-hidden-environment-names* nil))
      (unwind-protect
           (progn
             (cl-tmux::%cmd-set-environment-prompt
              s '("-h" "-g" "CLTMUX_GHID" "gsecret"))
             (is (string= "gsecret" (sb-ext:posix-getenv "CLTMUX_GHID"))
                 "the global variable itself must be set in the process env")
             (is (notany (lambda (e) (eql 0 (search "CLTMUX_GHID=" e)))
                         (cl-tmux/model:session-child-environment s))
                 "hidden global must be stripped from child environments"))
        (cl-tmux/model:process-unset-environment "CLTMUX_GHID")))))

;;; ── refresh-client -C (client size) ──────────────────────────────────────────

(test refresh-client-C-sets-client-size
  "refresh-client -C WxH updates the client size and relayouts; a malformed or
   absent size leaves the dimensions untouched."
  (with-fake-session (s)
    (let ((cl-tmux::*term-rows* 24)
          (cl-tmux::*term-cols* 80))
      (cl-tmux::%cmd-refresh-client-arg s '("-C" "100x40"))
      (is (= 40 cl-tmux::*term-rows*) "-C 100x40 must set rows to 40")
      (is (= 100 cl-tmux::*term-cols*) "-C 100x40 must set cols to 100")
      (cl-tmux::%cmd-refresh-client-arg s '("-C" "garbage"))
      (is (= 40 cl-tmux::*term-rows*) "malformed -C must not change rows")
      (cl-tmux::%cmd-refresh-client-arg s '("-S"))
      (is (= 100 cl-tmux::*term-cols*) "-S alone must not change the size"))))

;;; ── set-hook -t (session-scoped hooks) ───────────────────────────────────────

(test set-hook-t-scopes-hook-to-named-session
  "set-hook -t SESSION scopes the hook: it fires only when the event's derived
   session is the named one (tmux per-target hooks; previously -t was consumed
   and the hook fired globally)."
  (with-isolated-config
   (with-isolated-hooks
    (with-fake-session (alpha)
      (with-fake-session (beta)
        (setf (cl-tmux/model:session-name alpha) "alpha"
              (cl-tmux/model:session-name beta)  "beta")
        (let ((*overlay* nil))
          (apply-config-directive
           '("set-hook" "-t" "alpha" "my-scoped-event" "set -g @scoped fired"))
          ;; Firing for the OTHER session must not run the hook.
          (cl-tmux::run-command-hooks "my-scoped-event" beta)
          (is (null (cl-tmux/options:get-option "@scoped" nil))
              "the scoped hook must not fire for a different session")
          ;; Firing for the named session runs it.
          (cl-tmux::run-command-hooks "my-scoped-event" alpha)
          (is (string= "fired" (cl-tmux/options:get-option "@scoped"))
              "the scoped hook must fire for its named session")
          ;; A global hook (no -t) still fires for any session.
          (apply-config-directive
           '("set-hook" "my-global-event" "set -g @global fired"))
          (cl-tmux::run-command-hooks "my-global-event" beta)
          (is (string= "fired" (cl-tmux/options:get-option "@global"))
              "a global hook must fire for any session")))))))

(test set-hook-w-and-p-scope-to-object-ids
  "set-hook -w -t / -p -t scope hooks to a window/pane id: the hook fires only
   when the event's fire-time target is that object (tmux per-target hooks)."
  (with-isolated-config
   (with-isolated-hooks
    (with-fake-session (s :nwindows 2)
      (let* ((wins (cl-tmux/model:session-windows s))
             (w0   (first wins))
             (w1   (second wins))
             (p0   (first (cl-tmux/model:window-panes w0))))
        ;; %derive-hook-session resolves window/pane targets via the registry.
        (let ((*overlay* nil)
              (cl-tmux::*server-sessions* (list (cons "s" s))))
          ;; Window scope: fires for that window (or a pane inside it) only.
          (apply-config-directive
           (list "set-hook" "-w" "-t"
                 (format nil "@~D" (cl-tmux/model:window-id w0))
                 "win-scoped-event" "set -g @wscope fired"))
          (cl-tmux::run-command-hooks "win-scoped-event" w1)
          (is (null (cl-tmux/options:get-option "@wscope" nil))
              "a window-scoped hook must not fire for another window")
          (cl-tmux::run-command-hooks "win-scoped-event" p0)
          (is (string= "fired" (cl-tmux/options:get-option "@wscope"))
              "a window-scoped hook must fire for a pane inside its window")
          ;; Pane scope: fires for that pane only.
          (apply-config-directive
           (list "set-hook" "-p" "-t"
                 (format nil "%~D" (cl-tmux/model:pane-id p0))
                 "pane-scoped-event" "set -g @pscope fired"))
          (cl-tmux::run-command-hooks "pane-scoped-event" w0)
          (is (null (cl-tmux/options:get-option "@pscope" nil))
              "a pane-scoped hook must not fire for a window target")
          (cl-tmux::run-command-hooks "pane-scoped-event" p0)
          (is (string= "fired" (cl-tmux/options:get-option "@pscope"))
              "a pane-scoped hook must fire for its pane")))))))
