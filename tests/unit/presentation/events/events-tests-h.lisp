(in-package #:cl-tmux/test)

;;;; events tests — part H: byte-constant values, make-input-state,
;;;; forward-octets synchronized/read-only, maybe-rename-window-from-title.

(describe "events-suite"

  ;;; ── Table-driven tests: byte constant values ─────────────────────────────────

  ;; VT100 and CSI byte constants must equal their documented ASCII/VT values.
  (it "byte-constants-have-correct-values"
    ;; +byte-sgr-press+ was merged into +byte-ascii-m+ (same value 77); the
    ;; surviving constant's value is verified below via "ASCII M".
    (check-table
     (list (list cl-tmux::+byte-esc+              27  "ESC must be 27")
           (list cl-tmux::+byte-csi-bracket+       91  "CSI [ must be 91")
           (list cl-tmux::+byte-arrow-up+          65  "CUU A must be 65")
           (list cl-tmux::+byte-arrow-down+        66  "CUD B must be 66")
           (list cl-tmux::+byte-arrow-left+        68  "CUB D must be 68")
           (list cl-tmux::+byte-arrow-right+       67  "CUF C must be 67")
           (list cl-tmux::+byte-q+                 113 "q must be 113")
           (list cl-tmux::+byte-j+                 106 "j must be 106")
           (list cl-tmux::+byte-k+                 107 "k must be 107")
           (list cl-tmux::+byte-csi-param-1+       49  "CSI param 1 must be 49")
           (list cl-tmux::+byte-csi-semi+          59  "CSI semi must be 59")
           (list cl-tmux::+byte-csi-mod-ctrl+      53  "CSI ctrl modifier must be 53")
           (list cl-tmux::+byte-csi-mod-meta+      51  "CSI meta modifier must be 51")
           (list cl-tmux::+byte-tilde+             126 "tilde must be 126")
           (list cl-tmux::+byte-sgr-lt+            60  "SGR < must be 60")
           (list cl-tmux::+byte-digit-0+           48  "digit 0 must be 48")
           (list cl-tmux::+byte-digit-9+           57  "digit 9 must be 57")
           (list cl-tmux::+byte-page-up-param+     53  "PageUp param must be 53")
           (list cl-tmux::+byte-page-down-param+   54  "PageDown param must be 54")
           (list cl-tmux::+byte-ascii-m+           77  "ASCII M must be 77")
           (list cl-tmux::+byte-sgr-release+       109 "SGR release final must be 109"))))

  ;;; ── make-input-state and input-state-continuation ────────────────────────────

  ;; make-input-state returns an input-state with continuation = %ground-input-state.
  (it "make-input-state-starts-in-ground-state"
    (let ((state (cl-tmux::make-input-state)))
      (expect (cl-tmux::input-state-p state))
      (expect (functionp (cl-tmux::input-state-continuation state)))))

  ;; After a complete 3-byte ESC [ A sequence, the continuation returns to ground.
  (it "input-state-continuation-is-reset-after-complete-sequence"
    (with-fake-session (s)
      (let ((state (cl-tmux::make-input-state)))
        ;; Feed ESC — transitions to escape accumulator
        (cl-tmux::process-byte s 27 state)
        (expect (not (eq #'cl-tmux::%ground-input-state
                     (cl-tmux::input-state-continuation state))))
        ;; Feed [ A — completes the sequence, back to ground
        (cl-tmux::process-byte s 91 state)
        (cl-tmux::process-byte s 65 state)
        (expect (eq #'cl-tmux::%ground-input-state
                (cl-tmux::input-state-continuation state))))))

  ;;; ── %forward-octets-synchronized — synchronize-panes broadcast ───────────────

  ;; %forward-octets-synchronized writes to all panes when synchronize-panes is T.
  ;; Verified by confirming it runs without error on a multi-pane session.
  (it "forward-octets-synchronized-broadcasts-when-option-set"
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

  ;; %forward-octets-synchronized silently discards input when *client-read-only* is T.
  (it "forward-octets-noop-when-client-read-only"
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
             (expect wrote :to-be-falsy))
        (setf (fdefinition 'cl-tmux/pty:pty-write) orig))))

  ;; %forward-octets-synchronized is a no-op on fd<=0 panes regardless of read-only.
  (it "forward-octets-writes-when-not-read-only"
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

  ;; %maybe-rename-window-from-title propagates the OSC title to the window name
  ;; when the window has automatic-rename enabled and the title differs.
  (it "maybe-rename-window-from-title-renames-when-osc-title-set"
    (with-auto-rename-session (screen pane win sess :win-name "old-name")
      (setf (screen-title screen) "new-title")
      (setf (window-automatic-rename-p win) t)
      (cl-tmux::%maybe-rename-window-from-title sess)
      (expect (string= "new-title" (window-name win)))))

  ;; %maybe-rename-window-from-title does not rename when title=name or auto-rename is off.
  (it "maybe-rename-window-from-title-noop-table"
    (dolist (c '(("same"      "same"     t   "title equals name → no-op")
                 ("new-title" "original" nil "auto-rename off → no-op")))
      (destructuring-bind (new-title win-name auto-rename-p desc) c
        (declare (ignore desc))
        (with-auto-rename-session (screen pane win sess :win-name win-name)
          (setf (screen-title screen) new-title)
          (setf (window-automatic-rename-p win) auto-rename-p)
          (cl-tmux::%maybe-rename-window-from-title sess)
          (expect (string= win-name (window-name win)))))))

  ;; %maybe-rename-window-from-title is suppressed for a window whose window-local
  ;; "automatic-rename" option is off (set via set-option-for-window), even though
  ;; the window-automatic-rename-p flag and the global option are still on.  This
  ;; exercises the get-option-for-context :window read wired into the rename path.
  (it "maybe-rename-window-from-title-noop-when-window-local-auto-rename-off"
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
        (expect (string= "original" (window-name win))))))

  ;; Companion to the suppression test: with window-local automatic-rename ON the
  ;; rename path still fires and propagates the OSC title to the window name.
  (it "maybe-rename-window-from-title-renames-when-window-local-auto-rename-on"
    (with-auto-rename-session (screen pane win sess :win-name "old-name")
      (setf (screen-title screen) "new-title")
      (setf (window-automatic-rename-p win) t)
      (with-isolated-config
        (cl-tmux/options:set-option "automatic-rename" t)
        (cl-tmux/options:set-option "allow-rename" t)
        (cl-tmux/options:set-option-for-window "automatic-rename" "on" win)
        (cl-tmux::%maybe-rename-window-from-title sess)
        (expect (string= "new-title" (window-name win))))))

  ;; `set -g allow-rename off` must NOT freeze automatic command-following — it
  ;; governs only app-set titles.  With automatic-rename on and allow-rename off,
  ;; the command-based name (automatic-rename-format) still applies.
  (it "allow-rename-off-keeps-command-following"
    (with-auto-rename-session (screen pane win sess :win-name "old" :pid 12345)
      (setf (window-automatic-rename-p win) t)
      (with-isolated-config
        (cl-tmux/options:set-option "automatic-rename" t)
        (cl-tmux/options:set-option "allow-rename" nil)        ; OFF
        (cl-tmux/options:set-option "automatic-rename-format" "MYCMD")
        (cl-tmux::%maybe-rename-window-from-title sess)
        (expect (string= "MYCMD" (window-name win))))))

  ;; With allow-rename off, an app's OSC title must NOT rename the window (the
  ;; title fallback is suppressed); command-following is unaffected.
  (it "allow-rename-off-suppresses-app-title"
    (with-auto-rename-session (screen pane win sess :win-name "keep")
      (setf (screen-title screen) "APPTITLE")
      (setf (window-automatic-rename-p win) t)
      (with-isolated-config
        (cl-tmux/options:set-option "automatic-rename" t)
        (cl-tmux/options:set-option "allow-rename" nil)        ; OFF
        (cl-tmux::%maybe-rename-window-from-title sess)
        (expect (string= "keep" (window-name win))))))

  ;; %auto-rename-name with :allow-title NIL returns empty for a process-less pane
  ;; whose only name source is the OSC title; :allow-title t uses it.
  (it "auto-rename-name-allow-title-nil-suppresses-osc-title"
    (with-auto-rename-session (screen pane win sess)
      (setf (screen-title screen) "T")
      (expect (string= "" (cl-tmux::%auto-rename-name sess win pane screen :allow-title nil)))
      (expect (string= "T" (cl-tmux::%auto-rename-name sess win pane screen :allow-title t)))))

  ;; Auto-rename must keep working after the first rename: %maybe-rename-window-
  ;; from-title must NOT disable automatic-rename, so a later title change renames
  ;; again.  Regression for rename-window unconditionally clearing
  ;; automatic-rename-p (which made auto-rename fire only once).
  (it "maybe-rename-window-keeps-tracking-after-first-rename"
    (with-auto-rename-session (screen pane win sess :win-name "old")
      (setf (window-automatic-rename-p win) t)
      (with-isolated-config
        (cl-tmux/options:set-option "automatic-rename" t)
        (cl-tmux/options:set-option "allow-rename" t)
        (setf (screen-title screen) "first")
        (cl-tmux::%maybe-rename-window-from-title sess)
        (expect (string= "first" (window-name win)))
        (expect (window-automatic-rename-p win) :to-be-truthy)
        ;; A second title change must rename again (the bug made this a no-op).
        (setf (screen-title screen) "second")
        (cl-tmux::%maybe-rename-window-from-title sess)
        (expect (string= "second" (window-name win)))))))
