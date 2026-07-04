(in-package #:cl-tmux/test)

;;;; Dispatch set-option command-line tests.

(in-suite dispatch-suite)

(test run-command-line-set-option-coerces-boolean
  "'set-option monitor-activity off' stores NIL and 'set-option ... on' stores T (type-coerced).
   Uses monitor-activity — a side-effect-free :boolean option — because `status` is
   now a choice/string option (off|on|2..5), not a boolean."
  (with-fake-session (s)
    (with-isolated-options ()
      (cl-tmux::%run-command-line s "set-option -g monitor-activity off")
      (is (null (cl-tmux/options:get-option "monitor-activity"))
          "set-option monitor-activity off → NIL (boolean coercion)")
      (cl-tmux::%run-command-line s "set-option -g monitor-activity on")
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
    (cl-tmux::%with-option-scope (make-fake-session) '((#\s . t)) nil nil
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
      (cl-tmux/options:set-server-option "escape-time" (or original 10)))))

(test run-command-line-set-option-append-flag
  "'set-option -a <name> <value>' appends to the option's current value."
  (with-fake-session (s)
    (with-isolated-options ("status-left" "A")
      (cl-tmux::%run-command-line s "set-option -a status-left B")
      (is (string= "AB" (cl-tmux/options:get-option "status-left"))
          "set-option -a must append B to the existing 'A'"))))

(test run-command-line-set-option-short-aliases-are-rejected
  "Runtime dispatch accepts canonical option commands only."
  (with-fake-session (s)
    (with-isolated-options ("status-left" "ORIG")
      (let ((*overlay* nil))
        (is (null (cl-tmux::%run-command-line s "set -g status-left YES"))
            "set must be rejected")
        (is (string= "ORIG" (cl-tmux/options:get-option "status-left"))
            "set must not mutate the global option")
        (assert-overlay-active "set must show an error overlay"))))
  (with-fake-session (s :nwindows 1)
    (let ((*overlay* nil))
      (let ((win (session-active-window s)))
        (is (null (cl-tmux::%run-command-line s "setw mode-keys vi"))
            "setw must be rejected")
        (is (not (nth-value 1 (gethash "mode-keys"
                                       (cl-tmux/model:window-local-options win))))
            "setw must not mutate the window-local option")
        (assert-overlay-active "setw must show an error overlay")))))

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
