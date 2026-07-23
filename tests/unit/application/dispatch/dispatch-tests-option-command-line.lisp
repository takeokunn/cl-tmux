(in-package #:cl-tmux/test)

;;;; Dispatch set-option command-line tests.

(describe "dispatch-suite"

  ;; 'set-option monitor-activity off' stores NIL and 'set-option ... on' stores T (type-coerced).
  ;; Uses monitor-activity — a side-effect-free :boolean option — because `status` is
  ;; now a choice/string option (off|on|2..5), not a boolean.
  (it "run-command-line-set-option-coerces-boolean"
    (with-fake-session (s)
      (with-isolated-options ()
        (cl-tmux::%run-command-line s "set-option -g monitor-activity off")
        (expect (null (cl-tmux/options:get-option "monitor-activity")))
        (cl-tmux::%run-command-line s "set-option -g monitor-activity on")
        (expect (eq t (cl-tmux/options:get-option "monitor-activity"))))))

  ;; 'set-option' stores string option values, and a quoted value keeps its spaces/format.
  (it "run-command-line-set-option-string-and-quoted"
    (with-fake-session (s)
      (with-isolated-options ()
        (cl-tmux::%run-command-line s "set-option status-left bar")
        (expect (string= "bar" (cl-tmux/options:get-option "status-left")))
        (cl-tmux::%run-command-line s "set-option status-left \"#{session_name} x\"")
        (expect (string= "#{session_name} x" (cl-tmux/options:get-option "status-left"))))))

  ;; 'set-option -g status off' sets the 'status' option (not an option literally named
  ;; '-g') — the canonical tmux form must work.
  (it "run-command-line-set-option-scope-flag"
    (with-option-session (s)
      (cl-tmux::%run-command-line s "set-option -g status off")
      (expect (string= "off" (cl-tmux/options:get-option "status")))
      (expect (null (cl-tmux/options:get-option "-g")))))

  ;; %with-option-scope routes the -s flag to :server scope with a NIL target
  ;; (audit #9: -s previously fell through to :global).
  (it "with-option-scope-s-flag-selects-server-scope"
    (let ((scope-seen nil)
          (target-seen :unset))
      (cl-tmux::%with-option-scope (make-fake-session) '((#\s . t)) nil nil
                                   (lambda (scope target)
                                     (setf scope-seen scope
                                           target-seen target)))
      (expect (eq :server scope-seen))
      (expect (null target-seen))))

  ;; %scope-set with :server scope writes the server option store, readable via
  ;; get-server-option (audit #9 end-to-end: server routing reaches the store).
  ;; Uses the real store with restore — mirroring the config-path server tests —
  ;; because rebinding *server-options* in a test unit does not reliably shadow the
  ;; accessor's special binding.
  (it "scope-set-server-writes-server-store"
    (let ((original (cl-tmux/options:get-server-option "escape-time")))
      (unwind-protect
           (progn
             (cl-tmux::%scope-set "escape-time" "250" :server nil)
             (expect (eql 250 (cl-tmux/options:get-server-option "escape-time"))))
        (cl-tmux/options:set-server-option "escape-time" (or original 10)))))

  ;; 'set-option -a <name> <value>' appends to the option's current value.
  (it "run-command-line-set-option-append-flag"
    (with-fake-session (s)
      (with-isolated-options ("status-left" "A")
        (cl-tmux::%run-command-line s "set-option -a status-left B")
        (expect (string= "AB" (cl-tmux/options:get-option "status-left"))))))

  ;; Runtime dispatch accepts canonical option commands only.
  (it "run-command-line-set-option-short-aliases-are-rejected"
    (with-fake-session (s)
      (with-isolated-options ("status-left" "ORIG")
        (let ((*overlay* nil))
          (expect (null (cl-tmux::%run-command-line s "set -g status-left YES")))
          (expect (string= "ORIG" (cl-tmux/options:get-option "status-left")))
          (assert-overlay-active "set must show an error overlay"))))
    (with-fake-session (s :nwindows 1)
      (let ((*overlay* nil))
        (let ((win (session-active-window s)))
          (expect (null (cl-tmux::%run-command-line s "setw mode-keys vi")))
          (expect (not (nth-value 1 (gethash "mode-keys"
                                             (cl-tmux/model:window-local-options win)))))
          (assert-overlay-active "setw must show an error overlay")))))

  ;; set-option and set-window-option reject unknown flags before mutating option stores.
  (it "run-command-line-set-option-rejects-unsupported-flags"
    (with-fake-session (s)
      (with-isolated-options ("status-left" "ORIG")
        (let ((*overlay* nil))
          (expect (null (cl-tmux::%run-command-line s "set-option -x status-left bad")))
          (expect (string= "ORIG" (cl-tmux/options:get-option "status-left")))
          (assert-overlay-contains "unsupported argument" *overlay*
                                    "set-option -x"))))
    (with-fake-session (s :nwindows 1)
      (let ((cl-tmux/options:*global-options* (make-hash-table :test #'equal))
            (*overlay* nil))
        (let ((win (session-active-window s)))
          (expect (null (cl-tmux::%run-command-line s "set-window-option -x mode-keys vi")))
          (expect (not (nth-value 1 (gethash "mode-keys"
                                             (cl-tmux/model:window-local-options win)))))
          (assert-overlay-active "set-window-option -x must show an error overlay"))))))
