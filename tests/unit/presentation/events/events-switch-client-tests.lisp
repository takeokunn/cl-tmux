(in-package #:cl-tmux/test)

;;;; switch-client and custom key table event tests

(in-suite events-suite)

;;; ── Custom key tables (switch-client -T <table>) ────────────────────────────

(test cmd-switch-client-T-sets-and-resets-key-table
  "switch-client -T <table> sets *key-table*; -T root resets it to NIL."
  (with-fake-session (s :nwindows 1)
      (cl-tmux::%cmd-switch-client s '("-T" "resize"))
      (is (string= "resize" cl-tmux::*key-table*)
          "switch-client -T resize activates the custom table")
      (cl-tmux::%cmd-switch-client s '("-T" "root"))
      (is (null cl-tmux::*key-table*)
          "switch-client -T root returns to the normal flow")))

(test custom-key-table-dispatches-from-active-table-and-persists
  "In a custom key table, a bound key dispatches from THAT table and the table
   persists (modal mode)."
  (with-isolated-config
    (with-fake-session (s :nwindows 2)
        (cl-tmux/config:apply-config-directive '("bind" "-T" "resize" "x" "next-window"))
        (setf cl-tmux::*key-table* "resize")
        (let ((state (cl-tmux::make-input-state)))
          (cl-tmux::process-byte s 120 state)  ; 'x'
          (is (eq (second (session-windows s)) (session-active-window s))
              "a key bound in the active custom table runs its binding")
          (is (string= "resize" cl-tmux::*key-table*)
              "the custom table persists after a key (modal)")))))

(test custom-key-table-binding-can-switch-back-to-root
  "A binding in a custom table running 'switch-client -T root' exits the table."
  (with-isolated-config
    (with-fake-session (s :nwindows 1)
        (cl-tmux/config:apply-config-directive
         '("bind" "-T" "resize" "q" "switch-client" "-T" "root"))
        (setf cl-tmux::*key-table* "resize")
        (let ((state (cl-tmux::make-input-state)))
          (cl-tmux::process-byte s 113 state)  ; 'q'
          (is (null cl-tmux::*key-table*)
              "switch-client -T root from within the table exits it")))))

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
  (is (eq expected (cl-tmux::%cmd-switch-client current args))
      description))

(defun %assert-switch-client-rejection (session args)
  "Assert that SWITCH-CLIENT rejects ARGS without mutating the session state."
  (let ((*overlay* nil)
        (cl-tmux::*dirty* nil))
    (is-false (cl-tmux::%cmd-switch-client session args)
              "~S must be rejected" args)
    (assert-overlay-contains "switch-client: unsupported argument"
                             (overlay-lines)
                             (format nil "~S must report the rejection" args))
    (is (eq session (cl-tmux::server-current-session))
        "~S must leave the current session unchanged" args)
    (is-false cl-tmux::*dirty*
              "~S must not mark the display dirty" args)))

(test cmd-switch-client-t-switches-to-named-session
  "switch-client -t <name> makes the named session the front (touched) one."
  (with-loop-state
    (with-empty-registry
      (multiple-value-bind (s0 s1 s2) (%make-three-session-registry)
        (declare (ignore s0 s1))
        (%assert-switch-client-selection (cl-tmux::server-find-session "1")
                                         '("-t" "2")
                                         s2
                                         "-t 2 selects session named 2")
        (is-true cl-tmux::*dirty* "a session switch marks the screen dirty")))))

(test cmd-switch-client-n-and-p-cycle-sessions
  "switch-client -n / -p move to the next / previous session cyclically."
  (with-loop-state
    (with-empty-registry
      (multiple-value-bind (s0 s1 s2) (%make-three-session-registry)
        ;; current = s1; registry order is (s0 s1 s2): next → s2, prev → s0.
        (%assert-switch-client-selection s1 '("-n") s2
                                         "-n from session 1 goes to session 2")
        (%assert-switch-client-selection s1 '("-p") s0
                                         "-p from session 1 goes to session 0")))))

(test cmd-switch-client-l-switches-to-last-active
  "switch-client -l selects the second-most-recently-active session."
  (with-loop-state
    (with-empty-registry
      (multiple-value-bind (s0 s1 s2) (%make-three-session-registry)
        (declare (ignore s0))
        ;; last-active stamps 10/30/20 → desc order s1,s2,s0 → second = s2.
        (%assert-switch-client-selection s1 '("-l") s2
                                         "-l from the front session 1 returns to session 2")))))

(test cmd-switch-client-t-and-T-are-orthogonal
  "switch-client -t <name> -T <table> performs the session move AND arms the
   key table in one invocation."
  (with-loop-state
    (with-empty-registry
      (multiple-value-bind (s0 s1 s2) (%make-three-session-registry)
        (declare (ignore s0 s1))
        (%assert-switch-client-selection (cl-tmux::server-find-session "1")
                                         '("-t" "2" "-T" "resize")
                                         s2
                                         "-t still switches the session when -T is also given")
        (is (string= "resize" cl-tmux::*key-table*)
            "-T still arms the key table when -t is also given")))))

(test cmd-switch-client-rejects-compatibility-flags
  "switch-client rejects standalone/tmux compatibility flags before switching sessions."
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

(test cmd-switch-client-r-refreshes-without-session-switch
  "switch-client -r is accepted as a redraw request without changing sessions."
  (with-loop-state
    (with-empty-registry
      (multiple-value-bind (s0 s1 s2) (%make-three-session-registry)
        (declare (ignore s0 s2))
        (setf cl-tmux::*dirty* nil)
        (%assert-switch-client-selection s1 '("-r") t
                                         "-r returns true as a handled refresh request")
        (is-true cl-tmux::*dirty*
                 "-r marks the display dirty")))))

;;; ── Default M-1..M-5 preset-layout bindings (tmux defaults) ─────────────────

(test default-meta-digit-layout-bindings-registered
  "C-b M-1..M-5 are installed as select-layout command token-lists in the prefix
   table, matching real tmux's preset-layout defaults."
  (with-isolated-config
    (dolist (c '(("M-1" ("select-layout" "even-horizontal") "M-1 -> even-horizontal")
                 ("M-2" ("select-layout" "even-vertical")   "M-2 -> even-vertical")
                 ("M-3" ("select-layout" "main-horizontal") "M-3 -> main-horizontal")
                 ("M-4" ("select-layout" "main-vertical")   "M-4 -> main-vertical")
                 ("M-5" ("select-layout" "tiled")           "M-5 -> tiled")))
      (destructuring-bind (key expected desc) c
        (is (equal expected (key-table-command-value "prefix" key)) "~A" desc)))))

(test default-meta-window-bindings-registered
  "C-b M-n/M-p/M-o are installed as command token-lists in the prefix table,
   matching tmux's alert-window and reverse-rotate defaults."
  (with-isolated-config
    (dolist (c '(("M-n" ("next-window" "-a")     "M-n -> next alerted window")
                 ("M-p" ("previous-window" "-a") "M-p -> previous alerted window")
                 ("M-o" ("rotate-window" "-D")   "M-o -> rotate backward")))
      (destructuring-bind (key expected desc) c
        (is (equal expected (key-table-command-value "prefix" key)) "~A" desc)))))

(test default-prefix-customize-and-suspend-bindings-registered
  "C-b C and C-b C-z are installed in the prefix table."
  (with-isolated-config
    (is (equal '("customize-mode") (key-table-command-value "prefix" #\C))
        "C -> customize-mode")
    (is (eq :suspend-client (key-table-command-value "prefix" (code-char 26)))
        "C-z -> suspend-client")))

(test copy-mode-enter-u-scrolls-to-oldest-scrollback
  "copy-mode-enter -u pre-scrolls to the oldest scrollback content."
  (with-fake-session (s)
    (let ((screen (active-screen s)))
      (seed-scrollback screen 30)
      (finishes (cl-tmux::%cmd-copy-mode-arg s '("-u"))
        "copy-mode-enter -u must not signal an error")
      (is-true (screen-copy-mode-p screen)
               "copy-mode-enter -u must enter copy mode")
      (is (= 30 (screen-copy-offset screen))
          "copy-mode-enter -u must scroll to the oldest scrollback row"))))

(test prefix-meta-1-applies-layout-end-to-end
  "C-b then Alt+1 (ESC 1) runs the bound select-layout even-horizontal on a
   two-pane window without error (the after-prefix meta path fires the default)."
  (with-isolated-config
    (with-fake-two-pane-session (s)
        (let ((state (cl-tmux::make-input-state)))
          (dolist (b '(2 27 49))  ; C-b ESC 1
            (cl-tmux::process-byte s b state))
          ;; Layout applied: the window still has its two panes and a usable tree.
          (is (= 2 (length (window-panes (session-active-window s))))
              "select-layout via C-b M-1 must preserve both panes")))))
