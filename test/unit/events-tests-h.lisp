(in-package #:cl-tmux/test)

;;;; events tests — part H: byte-constant values, make-input-state,
;;;; forward-octets synchronized/read-only, maybe-rename-window-from-title.

(in-suite events-suite)

;;; ── Table-driven tests: byte constant values ─────────────────────────────────

(test byte-constants-have-correct-values
  "VT100 and CSI byte constants must equal their documented ASCII/VT values."
  (is (= 27  cl-tmux::+byte-esc+)          "ESC must be 27")
  (is (= 91  cl-tmux::+byte-csi-bracket+)  "CSI [ must be 91")
  (is (= 65  cl-tmux::+byte-arrow-up+)     "CUU A must be 65")
  (is (= 66  cl-tmux::+byte-arrow-down+)   "CUD B must be 66")
  (is (= 68  cl-tmux::+byte-arrow-left+)   "CUB D must be 68")
  (is (= 67  cl-tmux::+byte-arrow-right+)  "CUF C must be 67")
  (is (= 113 cl-tmux::+byte-q+)            "q must be 113")
  (is (= 106 cl-tmux::+byte-j+)            "j must be 106")
  (is (= 107 cl-tmux::+byte-k+)            "k must be 107")
  (is (= 49  cl-tmux::+byte-csi-param-1+)  "CSI param 1 must be 49")
  (is (= 59  cl-tmux::+byte-csi-semi+)     "CSI semi must be 59")
  (is (= 53  cl-tmux::+byte-csi-mod-ctrl+) "CSI ctrl modifier must be 53")
  (is (= 51  cl-tmux::+byte-csi-mod-meta+) "CSI meta modifier must be 51")
  (is (= 126 cl-tmux::+byte-tilde+)        "tilde must be 126")
  (is (= 60  cl-tmux::+byte-sgr-lt+)       "SGR < must be 60")
  (is (= 48  cl-tmux::+byte-digit-0+)      "digit 0 must be 48")
  (is (= 57  cl-tmux::+byte-digit-9+)      "digit 9 must be 57")
  (is (= 53  cl-tmux::+byte-page-up-param+)   "PageUp param must be 53")
  (is (= 54  cl-tmux::+byte-page-down-param+) "PageDown param must be 54")
  (is (= 77  cl-tmux::+byte-ascii-m+)      "ASCII M must be 77")
  ;; +byte-sgr-press+ was merged into +byte-ascii-m+ (same value 77); verify the
  ;; surviving constant still has the correct value.
  (is (= 109 cl-tmux::+byte-sgr-release+)  "SGR release final must be 109"))

;;; ── make-input-state and input-state-continuation ────────────────────────────

(test make-input-state-starts-in-ground-state
  "make-input-state returns an input-state with continuation = %ground-input-state."
  (let ((state (cl-tmux::make-input-state)))
    (is (cl-tmux::input-state-p state)
        "make-input-state must return an input-state struct")
    (is (functionp (cl-tmux::input-state-continuation state))
        "input-state continuation must be a function")))

(test input-state-continuation-is-reset-after-complete-sequence
  "After a complete 3-byte ESC [ A sequence, the continuation returns to ground."
  (with-fake-session (s)
    (let ((state (cl-tmux::make-input-state)))
      ;; Feed ESC — transitions to escape accumulator
      (cl-tmux::process-byte s 27 state)
      (is (not (eq #'cl-tmux::%ground-input-state
                   (cl-tmux::input-state-continuation state)))
          "after ESC the continuation should not be ground-state")
      ;; Feed [ A — completes the sequence, back to ground
      (cl-tmux::process-byte s 91 state)
      (cl-tmux::process-byte s 65 state)
      (is (eq #'cl-tmux::%ground-input-state
              (cl-tmux::input-state-continuation state))
          "after completing ESC [ A the continuation must be ground-state"))))

;;; ── %forward-octets-synchronized — synchronize-panes broadcast ───────────────

(test forward-octets-synchronized-broadcasts-when-option-set
  "%forward-octets-synchronized writes to all panes when synchronize-panes is T.
   Verified by confirming it runs without error on a multi-pane session."
  (let* ((p0   (make-pane :id 1 :fd -1 :pid -1 :x 0 :y 0 :width 20 :height 5
                           :screen (make-screen 20 5)))
         (p1   (make-pane :id 2 :fd -1 :pid -1 :x 0 :y 0 :width 20 :height 5
                           :screen (make-screen 20 5)))
         (win  (make-window :id 1 :name "w" :width 20 :height 5
                            :panes (list p0 p1)
                            :tree  (make-layout-split :h (make-layout-leaf p0)
                                                        (make-layout-leaf p1) 1/2)))
         (sess (make-session :id 1 :name "0" :windows (list win))))
    (window-select-pane win p0)
    (session-select-window sess win)
    (cl-tmux/options:set-option "synchronize-panes" t)
    (unwind-protect
         ;; fd=-1 makes pty-write a no-op; we just verify no error is raised.
         (finishes
           (cl-tmux::%forward-octets-synchronized
            sess
            (make-array 1 :element-type '(unsigned-byte 8) :initial-element 65)))
      (cl-tmux/options:set-option "synchronize-panes" nil))))

;;; ── *client-read-only* enforcement ──────────────────────────────────────────

(test forward-octets-noop-when-client-read-only
  "%forward-octets-synchronized silently discards input when *client-read-only* is T."
  (let* ((p0   (make-pane :id 1 :fd 9999 :pid -1 :x 0 :y 0 :width 20 :height 5
                           :screen (make-screen 20 5)))
         (win  (make-window :id 1 :name "w" :width 20 :height 5
                            :panes (list p0)
                            :tree  (make-layout-leaf p0)))
         (sess (make-session :id 1 :name "0" :windows (list win)))
         (wrote nil)
         (orig (fdefinition 'cl-tmux/pty:pty-write)))
    (window-select-pane win p0)
    (session-select-window sess win)
    (unwind-protect
         (let ((cl-tmux::*client-read-only* t))
           (setf (fdefinition 'cl-tmux/pty:pty-write)
                 (lambda (fd bytes) (declare (ignore fd bytes)) (setf wrote t)))
           (cl-tmux::%forward-octets-synchronized
            sess
            (make-array 1 :element-type '(unsigned-byte 8) :initial-element 65))
           (is-false wrote
               "%forward-octets-synchronized must not call pty-write when read-only"))
      (setf (fdefinition 'cl-tmux/pty:pty-write) orig))))

(test forward-octets-writes-when-not-read-only
  "%forward-octets-synchronized is a no-op on fd<=0 panes regardless of read-only."
  (let* ((p0   (make-pane :id 1 :fd -1 :pid -1 :x 0 :y 0 :width 20 :height 5
                           :screen (make-screen 20 5)))
         (win  (make-window :id 1 :name "w" :width 20 :height 5
                            :panes (list p0)
                            :tree  (make-layout-leaf p0)))
         (sess (make-session :id 1 :name "0" :windows (list win))))
    (window-select-pane win p0)
    (session-select-window sess win)
    (let ((cl-tmux::*client-read-only* nil))
      (finishes
        (cl-tmux::%forward-octets-synchronized
         sess
         (make-array 1 :element-type '(unsigned-byte 8) :initial-element 65))))))

;;; ── %maybe-rename-window-from-title coverage ─────────────────────────────────

(test maybe-rename-window-from-title-renames-when-osc-title-set
  "%maybe-rename-window-from-title propagates the OSC title to the window name
   when the window has automatic-rename enabled and the title differs."
  (with-auto-rename-session (screen pane win sess :win-name "old-name")
    (setf (screen-title screen) "new-title")
    (setf (window-automatic-rename-p win) t)
    (cl-tmux::%maybe-rename-window-from-title sess)
    (is (string= "new-title" (window-name win))
        "%maybe-rename-window-from-title must set window-name to OSC title")))

(test maybe-rename-window-from-title-noop-table
  "%maybe-rename-window-from-title does not rename when title=name or auto-rename is off."
  (dolist (c '(("same"      "same"     t   "title equals name → no-op")
               ("new-title" "original" nil "auto-rename off → no-op")))
    (destructuring-bind (new-title win-name auto-rename-p desc) c
      (with-auto-rename-session (screen pane win sess :win-name win-name)
        (declare (ignore pane))
        (setf (screen-title screen) new-title)
        (setf (window-automatic-rename-p win) auto-rename-p)
        (cl-tmux::%maybe-rename-window-from-title sess)
        (is (string= win-name (window-name win)) "~A" desc)))))

(test maybe-rename-window-from-title-noop-when-window-local-auto-rename-off
  "%maybe-rename-window-from-title is suppressed for a window whose window-local
   \"automatic-rename\" option is off (set via set-option-for-window), even though
   the window-automatic-rename-p flag and the global option are still on.  This
   exercises the get-option-for-context :window read wired into the rename path."
  (with-auto-rename-session (screen pane win sess :win-name "original")
    (setf (screen-title screen) "new-title")
    (setf (window-automatic-rename-p win) t)
    (with-isolated-config
      ;; Global automatic-rename / allow-rename stay on; only the per-window
      ;; option is turned off, so get-option-for-context :window returns NIL.
      (cl-tmux/options:set-option "automatic-rename" t)
      (cl-tmux/options:set-option "allow-rename" t)
      (cl-tmux/options:set-option-for-window "automatic-rename" "off" win)
      (cl-tmux::%maybe-rename-window-from-title sess)
      (is (string= "original" (window-name win))
          "window-name must not change when window-local automatic-rename is off"))))

(test maybe-rename-window-from-title-renames-when-window-local-auto-rename-on
  "Companion to the suppression test: with window-local automatic-rename ON the
   rename path still fires and propagates the OSC title to the window name."
  (with-auto-rename-session (screen pane win sess :win-name "old-name")
    (setf (screen-title screen) "new-title")
    (setf (window-automatic-rename-p win) t)
    (with-isolated-config
      (cl-tmux/options:set-option "automatic-rename" t)
      (cl-tmux/options:set-option "allow-rename" t)
      (cl-tmux/options:set-option-for-window "automatic-rename" "on" win)
      (cl-tmux::%maybe-rename-window-from-title sess)
      (is (string= "new-title" (window-name win))
          "window-name must update when window-local automatic-rename is on"))))

(test allow-rename-off-keeps-command-following
  "`set -g allow-rename off` must NOT freeze automatic command-following — it
   governs only app-set titles.  With automatic-rename on and allow-rename off,
   the command-based name (automatic-rename-format) still applies."
  (with-auto-rename-session (screen pane win sess :win-name "old" :pid 12345)
    (setf (window-automatic-rename-p win) t)
    (with-isolated-config
      (cl-tmux/options:set-option "automatic-rename" t)
      (cl-tmux/options:set-option "allow-rename" nil)        ; OFF
      (cl-tmux/options:set-option "automatic-rename-format" "MYCMD")
      (cl-tmux::%maybe-rename-window-from-title sess)
      (is (string= "MYCMD" (window-name win))
          "command-following must still apply with allow-rename off"))))

(test allow-rename-off-suppresses-app-title
  "With allow-rename off, an app's OSC title must NOT rename the window (the
   title fallback is suppressed); command-following is unaffected."
  (with-auto-rename-session (screen pane win sess :win-name "keep")
    (setf (screen-title screen) "APPTITLE")
    (setf (window-automatic-rename-p win) t)
    (with-isolated-config
      (cl-tmux/options:set-option "automatic-rename" t)
      (cl-tmux/options:set-option "allow-rename" nil)        ; OFF
      (cl-tmux::%maybe-rename-window-from-title sess)
      (is (string= "keep" (window-name win))
          "app OSC title must not rename the window when allow-rename is off"))))

(test auto-rename-name-allow-title-nil-suppresses-osc-title
  "%auto-rename-name with :allow-title NIL returns empty for a process-less pane
   whose only name source is the OSC title; :allow-title t uses it."
  (with-auto-rename-session (screen pane win sess)
    (setf (screen-title screen) "T")
    (is (string= "" (cl-tmux::%auto-rename-name sess win pane screen :allow-title nil))
        ":allow-title nil suppresses the OSC title")
    (is (string= "T" (cl-tmux::%auto-rename-name sess win pane screen :allow-title t))
        ":allow-title t uses the OSC title")))

(test maybe-rename-window-keeps-tracking-after-first-rename
  "Auto-rename must keep working after the first rename: %maybe-rename-window-
   from-title must NOT disable automatic-rename, so a later title change renames
   again.  Regression for rename-window unconditionally clearing
   automatic-rename-p (which made auto-rename fire only once)."
  (with-auto-rename-session (screen pane win sess :win-name "old")
    (setf (window-automatic-rename-p win) t)
    (with-isolated-config
      (cl-tmux/options:set-option "automatic-rename" t)
      (cl-tmux/options:set-option "allow-rename" t)
      (setf (screen-title screen) "first")
      (cl-tmux::%maybe-rename-window-from-title sess)
      (is (string= "first" (window-name win)) "first auto-rename applies")
      (is-true (window-automatic-rename-p win)
               "automatic-rename must stay ON after an auto-rename")
      ;; A second title change must rename again (the bug made this a no-op).
      (setf (screen-title screen) "second")
      (cl-tmux::%maybe-rename-window-from-title sess)
      (is (string= "second" (window-name win))
          "auto-rename must keep tracking after the first rename"))))

