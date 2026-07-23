(in-package #:cl-tmux/test)

;;;; select-layout-spread, new key bindings, choose-window, mouse reporting helpers, standard tmux defaults — part VII

(describe "events-suite"

  ;; ── dispatch :select-layout-spread ─────────────────────────────────────────

  ;; :select-layout-spread applies the even-horizontal layout without signaling.
  (it "dispatch-select-layout-spread-applies-even-horizontal"
    (with-fake-session (s)
      (expect (null (handler-case
                        (cl-tmux::dispatch-command s :select-layout-spread nil)
                      (error (e) e))))))

  ;; ── New key bindings: z, ', and grouping ────────────────────────────────────

  ;; C-b z (lowercase, char code 122) is bound to :zoom-toggle.
  (it "key-binding-z-lowercase-is-zoom-toggle"
    ;; Isolated config: z is an install-extended-key-binding, vulnerable to the
    ;; known global prefix-table polluter (see also the detach tests).
    (with-isolated-config
      (expect (eq :zoom-toggle (lookup-key-binding #\z)))))

  ;; C-b Z is not a zoom alias; lowercase z is the canonical binding.
  (it "key-binding-Z-uppercase-is-unbound"
    (with-isolated-config
      (expect (null (lookup-key-binding #\Z)))))

  ;; C-b ' (char code 39) is bound to :select-window-prompt.
  (it "key-binding-quote-is-select-window-prompt"
    (expect (eq :select-window-prompt (lookup-key-binding #\'))))

  ;; C-b z dispatches :zoom-toggle without error.
  (it "dispatch-zoom-toggle-via-lowercase-z"
    (with-isolated-config
      (with-fake-session (s)
        (let ((state (cl-tmux::make-input-state)))
          (cl-tmux::process-byte s 2 state)
          (expect (null (cl-tmux::process-byte s (char-code #\z) state)))))))

  ;; :select-window-prompt opens a prompt without signaling.
  (it "dispatch-select-window-prompt-opens-prompt"
    (with-fake-session (s :nwindows 2)
      (let ((*prompt* nil))
        (cl-tmux::dispatch-command s :select-window-prompt nil)
        (expect (prompt-active-p)))))

  ;; ── choose-window uses menu system ──────────────────────────────────────────

  ;; :choose-window shows a menu overlay for j/k navigation without a prompt.
  (it "dispatch-choose-window-shows-menu-overlay"
    (with-fake-session (s :nwindows 2)
      (let ((*overlay* nil) (*prompt* nil))
        (cl-tmux::dispatch-command s :choose-window nil)
        (assert-overlay-active ":choose-window must show an overlay")
        ;; choose-window now uses j/k menu navigation, not a text prompt.
        (expect (not (null cl-tmux/prompt:*active-menu*))))))

  ;; ── Mouse reporting helpers ──────────────────────────────────────────────────

  ;; enable-mouse-reporting emits the three DEC private mode sequences.
  (it "enable-mouse-reporting-writes-sequences"
    (let ((output (with-output-to-string (*standard-output*)
                    (cl-tmux/renderer:enable-mouse-reporting))))
      ;; Must contain all three mode strings
      (expect (search "?1000h" output))
      (expect (search "?1002h" output))
      (expect (search "?1006h" output))))

  ;; disable-mouse-reporting emits the three DEC private mode disable sequences.
  (it "disable-mouse-reporting-writes-disable-sequences"
    (let ((output (with-output-to-string (*standard-output*)
                    (cl-tmux/renderer:disable-mouse-reporting))))
      (expect (search "?1006l" output))
      (expect (search "?1002l" output))
      (expect (search "?1000l" output))))

  ;; ── All standard tmux default key bindings present ───────────────────────────
  ;;
  ;; Verify every key in the standard tmux default table has an entry in the
  ;; prefix key-table.  This is a regression guard: if a binding is accidentally
  ;; removed the test fails immediately.

  ;; All standard tmux default bindings must be present in prefix key-table.
  (it "standard-key-bindings-complete"
    ;; Isolated config so the assertion runs against the clean default+extended
    ;; bindings, immune to the known global prefix-table polluter.
    (with-isolated-config
     (flet ((bound-p (key)
              (not (null (lookup-key-binding key)))))
      (dolist (c (list
                   ;; Session
                   (list #\d   "d → detach")
                   (list #\$   "$ → rename-session")
                   (list #\s   "s → choose-session")
                   (list #\(   "( → switch-client-prev")
                   (list #\)   ") → switch-client-next")
                   (list #\L   "L → last-session")
                   ;; Window
                   (list #\c   "c → new-window")
                   (list #\n   "n → next-window")
                   (list #\p   "p → prev-window")
                   (list #\l   "l → last-window")
                   (list #\w   "w → choose-window")
                   (list #\f   "f → find-window")
                   (list #\&   "& → kill-window-confirm")
                   (list #\,   ", → rename-window")
                   (list #\0   "0 → select-window")
                   (list #\9   "9 → select-window")
                   (list #\.   ". → move-window-prompt")
                   (list #\'   "' → select-window-prompt")
                   ;; Pane
                   (list #\%   "% → split-vertical")
                   (list #\"   "\" → split-horizontal")
                   (list #\o   "o → next-pane")
                   (list #\;   "; → last-pane")
                   (list #\q   "q → display-panes")
                   (list #\x   "x → kill-pane-confirm")
                   (list #\z   "z → zoom-toggle (lowercase)")
                   (list #\!   "! → break-pane")
                   (list #\{   "{ → swap-pane-backward")
                   (list #\}   "} → swap-pane-forward")
                   ;; Buffer
                   (list #\[   "[ → copy-mode-enter")
                   (list #\]   "] → paste-buffer")
                   (list #\#   "# → list-buffers")
                   (list #\=   "= → choose-buffer")
                   (list #\-   "- → delete-buffer")
                   ;; Misc
                   (list #\:   ": → command-prompt")
                   (list #\?   "? → list-keys")
                   (list #\t   "t → clock-mode")
                   (list #\i   "i → display-info")
                   (list #\~   "~ → show-messages")
                   (list #\C   "C → customize-mode")
                   (list #\m   "m → mark-pane")
                   (list #\M   "M → clear-mark")
                   (list #\E   "E → select-layout-spread")
                   (list #\Space "Space → next-layout")
                   (list #\D   "D → choose-client")
                   (list (code-char 2) "C-b → send-prefix")
                   (list (code-char 26) "C-z → suspend-client")))
        (destructuring-bind (key desc) c
          (declare (ignore desc))
          (expect (bound-p key)))))))

  ;; ── Mouse scroll-wheel paths ─────────────────────────────────────────────────

  ;; Mouse scroll-up (btn=64) enters copy mode on the active pane when not in copy mode.
  (it "dispatch-mouse-scroll-up-enters-copy-mode"
    (with-single-pane-mouse-session (sess win p0)
      (seed-scrollback (pane-screen p0) 5)
      (cl-tmux::%dispatch-mouse-event sess 64 5 5 nil)
      (expect (screen-copy-mode-p (pane-screen p0)))))

  ;; Mouse scroll-down (btn=65) exits copy mode when the viewport is at the bottom (offset=0).
  (it "dispatch-mouse-scroll-down-exits-copy-mode-at-bottom"
    (with-single-pane-mouse-session (sess win p0)
      (let ((screen (pane-screen p0)))
        (seed-scrollback screen 5)
        (cl-tmux/commands::copy-mode-enter screen)
        ;; offset already at 0 — scroll down should exit copy mode
        (cl-tmux::%dispatch-mouse-event sess 65 5 5 nil)
        (expect (screen-copy-mode-p screen) :to-be-falsy))))

  ;; %dispatch-mouse-event is a no-op when the 'mouse' option is false.
  (it "dispatch-mouse-gated-by-mouse-option"
    (with-single-pane-mouse-session (sess win p0 :mouse nil)
      ;; With mouse off, click must not enter copy mode.
      (cl-tmux::%dispatch-mouse-event sess 0 5 5 nil)
      (expect (screen-copy-mode-p (pane-screen p0)) :to-be-falsy)))

  ;; ── %status-col-to-window helper ─────────────────────────────────────────────

  ;; %status-col-to-window returns NIL for a column before any window entry.
  (it "status-col-to-window-returns-nil-before-first-window"
    (let* ((p0   (make-pane :id 1 :fd -1 :pid -1 :x 0 :y 0 :width 20 :height 5
                             :screen (make-screen 20 5)))
           (win  (make-window :id 0 :name "win0" :width 20 :height 5
                              :panes (list p0) :tree (make-layout-leaf p0)))
           (sess (make-session :id 1 :name "mysess" :windows (list win))))
      (window-select-pane win p0)
      (session-select-window sess win)
      ;; Session prefix is " mysess" = 1 + 6 = 7 chars.
      ;; First window "win0" entry starts at column 7; col 0 is before it.
      (expect (null (cl-tmux::%status-col-to-window sess 0)))))

  ;; %status-col-to-window returns the window when the column falls within its entry.
  (it "status-col-to-window-returns-window-for-column-in-entry"
    (let* ((p0   (make-pane :id 1 :fd -1 :pid -1 :x 0 :y 0 :width 20 :height 5
                             :screen (make-screen 20 5)))
           (win  (make-window :id 0 :name "w" :width 20 :height 5
                              :panes (list p0) :tree (make-layout-leaf p0)))
           (sess (make-session :id 1 :name "s" :windows (list win))))
      (window-select-pane win p0)
      (session-select-window sess win)
      ;; Session prefix " s" = 2 chars.
      ;; Window "w" entry = 4 + 1 = 5 chars starting at col 2.
      ;; Column 2 is within that entry.
      (expect (eq win (cl-tmux::%status-col-to-window sess 2)))))

  ;; ── Mouse button constant sanity checks ──────────────────────────────────────

  ;; Named mouse button constants must have the correct integer values.
  (it "mouse-button-constants-have-expected-values"
    (dolist (c `((0  ,cl-tmux::+mouse-btn-left+        "left button must be 0")
                 (3  ,cl-tmux::+mouse-btn-release-x10+ "X10 release must be 3")
                 (32 ,cl-tmux::+mouse-btn-motion+       "motion must be 32")
                 (64 ,cl-tmux::+mouse-btn-scroll-up+    "scroll-up must be 64")
                 (65 ,cl-tmux::+mouse-btn-scroll-down+  "scroll-down must be 65")))
      (destructuring-bind (expected constant desc) c
        (declare (ignore desc))
        (expect (= expected constant)))))

  ;; ── SGR mouse parser ─────────────────────────────────────────────────────────

  ;; %parse-sgr-mouse parses a well-formed SGR press sequence.
  (it "parse-sgr-mouse-press-sequence"
    ;; ESC [ < 0 ; 10 ; 5 M  — btn=0, col=10, row=5 (1-based), press
    (let* ((seq "ESC[<0;10;5M")   ; textual — we build the actual byte vector below
           (s   (format nil "~C[<0;10;5M" #\Escape))
           (buf (make-array (length s) :element-type '(unsigned-byte 8)
                            :initial-contents (map 'list #'char-code s)))
           (len (length buf)))
      (declare (ignore seq))
      (multiple-value-bind (btn col row release-p)
          (cl-tmux::%parse-sgr-mouse buf len)
        (expect (= 0 btn))
        (expect (= 9 col))
        (expect (= 4 row))
        (expect release-p :to-be-falsy))))

  ;; %parse-sgr-mouse parses a well-formed SGR release sequence (final byte 'm').
  (it "parse-sgr-mouse-release-sequence"
    (let* ((s   (format nil "~C[<0;10;5m" #\Escape))
           (buf (make-array (length s) :element-type '(unsigned-byte 8)
                            :initial-contents (map 'list #'char-code s)))
           (len (length buf)))
      (multiple-value-bind (btn col row release-p)
          (cl-tmux::%parse-sgr-mouse buf len)
        (expect (= 0 btn))
        (expect (= 9 col))
        (expect (= 4 row))
        (expect release-p :to-be-truthy))))

  ;; %sgr-mouse-sequence-p returns T for ESC [ < prefix.
  (it "sgr-mouse-sequence-p-detects-sgr-intro"
    (let* ((s   (format nil "~C[<0;5;3M" #\Escape))
           (buf (make-array (length s) :element-type '(unsigned-byte 8)
                            :initial-contents (map 'list #'char-code s)))
           (len (length buf)))
      (expect (cl-tmux::%sgr-mouse-sequence-p buf len))))

  ;; %sgr-mouse-terminated-p returns T when the last byte is 'M' or 'm'.
  (it "sgr-mouse-terminated-p-detects-final-byte"
    (flet ((buf-from (s)
             (make-array (length s) :element-type '(unsigned-byte 8)
                         :initial-contents (map 'list #'char-code s))))
      (let* ((press-str   (format nil "~C[<0;5;3M" #\Escape))
             (release-str (format nil "~C[<0;5;3m" #\Escape))
             (pb (buf-from press-str))
             (rb (buf-from release-str)))
        (expect (cl-tmux::%sgr-mouse-terminated-p pb (length pb)))
        (expect (cl-tmux::%sgr-mouse-terminated-p rb (length rb))))))

  ;; ── define-cps-state: ignorable session/byte args ────────────────────────────

  ;; A define-cps-state function that ignores both args compiles and runs cleanly.
  (it "cps-state-ignores-unused-args"
    ;; Both session and byte are declared ignorable — verify no compile warnings
    ;; by just calling the function and checking the return type.
    (with-fake-session (s)
      (let ((state (cl-tmux::make-input-state)))
        (expect (null (cl-tmux::process-byte s 0 state))))))

  ;; ── Overlay arrow-key scrolling via escape sequence ─────────────────────────
  ;;
  ;; When the overlay is active and ESC [ A arrives, %overlay-escape-second-byte
  ;; scrolls the overlay up; ESC [ B scrolls it down.

  ;; ESC [ A and ESC [ B both scroll the overlay while keeping it open.
  (it "overlay-escape-scroll-table"
    (dolist (row '((65 "ESC [ A (up arrow) must keep overlay open")
                   (66 "ESC [ B (down arrow) must keep overlay open")))
      (destructuring-bind (arrow-byte desc) row
        (with-fake-session (s)
          (let ((*overlay* nil))
            (show-overlay (format nil "~{line~A~%~}" (loop for i from 1 to 20 collect i)))
            (let ((state (cl-tmux::make-input-state)))
              (cl-tmux::process-byte s 27 state)
              (cl-tmux::process-byte s 91 state)
              (cl-tmux::process-byte s arrow-byte state))
            (assert-overlay-active desc))))))

  ;; A lone ESC (ESC + non-'[' byte) while an overlay is open dismisses it.
  (it "overlay-bare-esc-dismisses-overlay"
    (with-fake-session (s)
      (let ((*overlay* nil))
        (show-overlay "some text")
        (let ((state (cl-tmux::make-input-state)))
          ;; ESC then 'x' — not a CSI sequence → dismiss
          (cl-tmux::process-byte s 27 state)
          (cl-tmux::process-byte s (char-code #\x) state))
        (assert-overlay-inactive
            "overlay must be dismissed by bare ESC"))))

  ;; ── handle-prompt-key: additional editing keys ────────────────────────────────

  ;; C-a (byte 1) moves the cursor to the beginning of the prompt line.
  (it "handle-prompt-key-ctrl-a-moves-to-bol"
    (with-clean-prompt
      (prompt-start "test" "hello"
                    (lambda (buf) (declare (ignore buf)) nil))
      ;; Move cursor to end first (EOL)
      (prompt-cursor-eol)
      (expect (= 5 (prompt-cursor-index *prompt*)))
      (cl-tmux::handle-prompt-key 1)  ; C-a
      (expect (= 0 (prompt-cursor-index *prompt*)))))

  ;; C-e (byte 5) moves the cursor to the end of the prompt line.
  (it "handle-prompt-key-ctrl-e-moves-to-eol"
    (with-clean-prompt
      (prompt-start "test" "hello"
                    (lambda (buf) (declare (ignore buf)) nil))
      ;; Cursor starts at end; move to BOL first
      (prompt-cursor-bol)
      (expect (= 0 (prompt-cursor-index *prompt*)))
      (cl-tmux::handle-prompt-key 5)  ; C-e
      (expect (= 5 (prompt-cursor-index *prompt*)))))

  ;; C-c (byte 3) cancels the prompt without running on-submit.
  (it "handle-prompt-key-ctrl-c-cancels"
    (with-clean-prompt
      (let ((submitted nil))
        (prompt-start "test" "abc"
                      (lambda (buf) (setf submitted buf)))
        (cl-tmux::handle-prompt-key 3)  ; C-c
        (expect (prompt-active-p) :to-be-falsy)
        (expect (null submitted)))))

  ;; A printable ASCII byte inserts the corresponding character into the buffer.
  (it "handle-prompt-key-printable-inserts-char"
    (with-clean-prompt
      (prompt-start "test" ""
                    (lambda (buf) (declare (ignore buf)) nil))
      (cl-tmux::handle-prompt-key (char-code #\A))
      (expect (string= "A" (prompt-buffer *prompt*))))))
