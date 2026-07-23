(in-package #:cl-tmux/test)

;;;; switch-client and custom key table event tests

(describe "events-suite"

  ;;; ── Custom key tables (switch-client -T <table>) ────────────────────────────

  ;; switch-client -T <table> sets *key-table*; -T root resets it to NIL.
  (it "cmd-switch-client-T-sets-and-resets-key-table"
    (with-fake-session (s :nwindows 1)
        (cl-tmux::%cmd-switch-client s '("-T" "resize"))
        (expect (string= "resize" cl-tmux::*key-table*))
        (cl-tmux::%cmd-switch-client s '("-T" "root"))
        (expect (null cl-tmux::*key-table*))))

  ;; In a custom key table, a bound key dispatches from THAT table and the table
  ;; persists (modal mode).
  (it "custom-key-table-dispatches-from-active-table-and-persists"
    (with-isolated-config
      (with-fake-session (s :nwindows 2)
          (cl-tmux/config:apply-config-directive '("bind" "-T" "resize" "x" "next-window"))
          (setf cl-tmux::*key-table* "resize")
          (let ((state (cl-tmux::make-input-state)))
            (cl-tmux::process-byte s 120 state)  ; 'x'
            (expect (eq (second (session-windows s)) (session-active-window s)))
            (expect (string= "resize" cl-tmux::*key-table*))))))

  ;; A binding in a custom table running 'switch-client -T root' exits the table.
  (it "custom-key-table-binding-can-switch-back-to-root"
    (with-isolated-config
      (with-fake-session (s :nwindows 1)
          (cl-tmux/config:apply-config-directive
           '("bind" "-T" "resize" "q" "switch-client" "-T" "root"))
          (setf cl-tmux::*key-table* "resize")
          (let ((state (cl-tmux::make-input-state)))
            (cl-tmux::process-byte s 113 state)  ; 'q'
            (expect (null cl-tmux::*key-table*))))))

  ;;; ── switch-client session selection (-t / -n / -p / -l) ─────────────────────

  (defun %make-three-session-registry ()
    "Build three registered sessions named 0/1/2 (current = 1) with deterministic
     last-active stamps 10/30/20, and return them as (values s0 s1 s2).  Caller
     must run inside a binding that isolates cl-tmux::*server-sessions*."
    (let ((s0 (make-fake-session :nwindows 1))
          (s1 (make-fake-session :nwindows 1))
          (s2 (make-fake-session :nwindows 1)))
      (setf (cl-tmux::session-name s0) "0" (cl-tmux::session-last-active s0) 10
            (cl-tmux::session-name s1) "1" (cl-tmux::session-last-active s1) 30
            (cl-tmux::session-name s2) "2" (cl-tmux::session-last-active s2) 20
            cl-tmux::*server-sessions*
            (list (cons "0" s0) (cons "1" s1) (cons "2" s2)))
      (values s0 s1 s2)))

  (defun %assert-switch-client-selection (current args expected description)
    "Assert that SWITCH-CLIENT selects EXPECTED from CURRENT with ARGS."
    (declare (ignore description))
    (expect (eq expected (cl-tmux::%cmd-switch-client current args))))

  (defun %assert-switch-client-rejection (session args)
    "Assert that SWITCH-CLIENT rejects ARGS without mutating the session state."
    (let ((*overlay* nil)
          (cl-tmux::*dirty* nil))
      (expect (cl-tmux::%cmd-switch-client session args) :to-be-falsy)
      (assert-overlay-contains "switch-client: unsupported argument"
                               (overlay-lines)
                               (format nil "~S must report the rejection" args))
      (expect (eq session (cl-tmux::server-current-session)))
      (expect cl-tmux::*dirty* :to-be-falsy)))

  ;; switch-client -t <name> makes the named session the front (touched) one.
  (it "cmd-switch-client-t-switches-to-named-session"
    (with-loop-state
      (with-empty-registry
        (multiple-value-bind (s0 s1 s2) (%make-three-session-registry)
          (declare (ignore s0 s1))
          (%assert-switch-client-selection (cl-tmux::server-find-session "1")
                                           '("-t" "2")
                                           s2
                                           "-t 2 selects session named 2")
          (expect cl-tmux::*dirty* :to-be-truthy)))))

  ;; switch-client -n / -p move to the next / previous session cyclically.
  (it "cmd-switch-client-n-and-p-cycle-sessions"
    (with-loop-state
      (with-empty-registry
        (multiple-value-bind (s0 s1 s2) (%make-three-session-registry)
          ;; current = s1; registry order is (s0 s1 s2): next → s2, prev → s0.
          (%assert-switch-client-selection s1 '("-n") s2
                                           "-n from session 1 goes to session 2")
          (%assert-switch-client-selection s1 '("-p") s0
                                           "-p from session 1 goes to session 0")))))

  ;; switch-client -l selects the second-most-recently-active session.
  (it "cmd-switch-client-l-switches-to-last-active"
    (with-loop-state
      (with-empty-registry
        (multiple-value-bind (s0 s1 s2) (%make-three-session-registry)
          (declare (ignore s0))
          ;; last-active stamps 10/30/20 → desc order s1,s2,s0 → second = s2.
          (%assert-switch-client-selection s1 '("-l") s2
                                           "-l from the front session 1 returns to session 2")))))

  ;; switch-client -t <name> -T <table> performs the session move AND arms the
  ;; key table in one invocation.
  (it "cmd-switch-client-t-and-T-are-orthogonal"
    (with-loop-state
      (with-empty-registry
        (multiple-value-bind (s0 s1 s2) (%make-three-session-registry)
          (declare (ignore s0 s1))
          (%assert-switch-client-selection (cl-tmux::server-find-session "1")
                                           '("-t" "2" "-T" "resize")
                                           s2
                                           "-t still switches the session when -T is also given")
          (expect (string= "resize" cl-tmux::*key-table*))))))

  ;; switch-client rejects unsupported flags before switching sessions.
  (it "cmd-switch-client-rejects-unsupported-flags"
    (with-loop-state
      (with-empty-registry
        (multiple-value-bind (s0 s1 s2) (%make-three-session-registry)
          (declare (ignore s0 s2))
          (dolist (args '(("-c" "client-0" "-t" "2")
                          ("-E" "-t" "2")
                          ("-Z" "-t" "2")
                          ("-F" "#{session_name}" "-t" "2")
                          ("-f" "flags" "-t" "2")))
            (%assert-switch-client-rejection s1 args))))))

  ;; switch-client -r is accepted as a redraw request without changing sessions.
  (it "cmd-switch-client-r-refreshes-without-session-switch"
    (with-loop-state
      (with-empty-registry
        (multiple-value-bind (s0 s1 s2) (%make-three-session-registry)
          (declare (ignore s0 s2))
          (setf cl-tmux::*dirty* nil)
          (%assert-switch-client-selection s1 '("-r") t
                                           "-r returns true as a handled refresh request")
          (expect cl-tmux::*dirty* :to-be-truthy)))))

  ;;; ── Default M-1..M-5 preset-layout bindings (tmux defaults) ─────────────────

  ;; C-b M-1..M-5 are installed as select-layout command token-lists in the prefix
  ;; table, matching real tmux's preset-layout defaults.
  (it "default-meta-digit-layout-bindings-registered"
    (with-isolated-config
      (dolist (c '(("M-1" ("select-layout" "even-horizontal") "M-1 -> even-horizontal")
                   ("M-2" ("select-layout" "even-vertical")   "M-2 -> even-vertical")
                   ("M-3" ("select-layout" "main-horizontal") "M-3 -> main-horizontal")
                   ("M-4" ("select-layout" "main-vertical")   "M-4 -> main-vertical")
                   ("M-5" ("select-layout" "tiled")           "M-5 -> tiled")))
        (destructuring-bind (key expected desc) c
          (declare (ignore desc))
          (expect (equal expected (key-table-command-value "prefix" key)))))))

  ;; C-b M-n/M-p/M-o are installed as command token-lists in the prefix table,
  ;; matching tmux's alert-window and reverse-rotate defaults.
  (it "default-meta-window-bindings-registered"
    (with-isolated-config
      (dolist (c '(("M-n" ("next-window" "-a")     "M-n -> next alerted window")
                   ("M-p" ("previous-window" "-a") "M-p -> previous alerted window")
                   ("M-o" ("rotate-window" "-D")   "M-o -> rotate backward")))
        (destructuring-bind (key expected desc) c
          (declare (ignore desc))
          (expect (equal expected (key-table-command-value "prefix" key)))))))

  ;; C-b C and C-b C-z are installed in the prefix table.
  (it "default-prefix-customize-and-suspend-bindings-registered"
    (with-isolated-config
      (expect (equal '("customize-mode") (key-table-command-value "prefix" #\C)))
      (expect (eq :suspend-client (key-table-command-value "prefix" (code-char 26))))))

  ;; copy-mode-enter -u pre-scrolls to the oldest scrollback content.
  (it "copy-mode-enter-u-scrolls-to-oldest-scrollback"
    (with-fake-session (s)
      (let ((screen (active-screen s)))
        (seed-scrollback screen 30)
        (finishes (cl-tmux::%cmd-copy-mode-arg s '("-u"))
          "copy-mode-enter -u must not signal an error")
        (expect (screen-copy-mode-p screen) :to-be-truthy)
        (expect (= 30 (screen-copy-offset screen))))))

  ;; C-b then Alt+1 (ESC 1) runs the bound select-layout even-horizontal on a
  ;; two-pane window without error (the after-prefix meta path fires the default).
  (it "prefix-meta-1-applies-layout-end-to-end"
    (with-isolated-config
      (with-fake-two-pane-session (s)
          (let ((state (cl-tmux::make-input-state)))
            (dolist (b '(2 27 49))  ; C-b ESC 1
              (cl-tmux::process-byte s b state))
            ;; Layout applied: the window still has its two panes and a usable tree.
            (expect (= 2 (length (window-panes (session-active-window s))))))))))
