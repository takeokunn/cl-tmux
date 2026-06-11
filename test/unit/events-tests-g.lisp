(in-package #:cl-tmux/test)

;;;; select-layout-spread, new key bindings, choose-window, mouse reporting helpers, standard tmux defaults — part VII

(in-suite events-suite)

;;; ── dispatch :select-layout-spread ─────────────────────────────────────────

(test dispatch-select-layout-spread-applies-even-horizontal
  ":select-layout-spread applies the even-horizontal layout without signaling."
  (with-fake-session (s)
    (is (null (handler-case
                  (cl-tmux::dispatch-command s :select-layout-spread nil)
                (error (e) e)))
        ":select-layout-spread must not signal an error")))

;;; ── New key bindings: z, ', and grouping ────────────────────────────────────

(test key-binding-z-lowercase-is-zoom-toggle
  "C-b z (lowercase, char code 122) is bound to :zoom-toggle."
  ;; Isolated config: z is an install-extended-key-binding, vulnerable to the
  ;; known global prefix-table polluter (see also the detach tests).
  (with-isolated-config
    (is (eq :zoom-toggle (lookup-key-binding #\z))
        "C-b z must be bound to :zoom-toggle (standard tmux default)")))

(test key-binding-Z-uppercase-is-still-zoom-toggle
  "C-b Z (uppercase, char code 90) remains bound to :zoom-toggle."
  (is (eq :zoom-toggle (lookup-key-binding #\Z))
      "C-b Z must also be bound to :zoom-toggle"))

(test key-binding-quote-is-select-window-prompt
  "C-b ' (char code 39) is bound to :select-window-prompt."
  (is (eq :select-window-prompt (lookup-key-binding #\'))
      "C-b ' must be bound to :select-window-prompt"))

(test dispatch-zoom-toggle-via-lowercase-z
  "C-b z dispatches :zoom-toggle without error."
  (with-isolated-config
    (with-fake-session (s)
      (let ((state (cl-tmux::make-input-state)))
        (cl-tmux::process-byte s 2 state)
        (is (null (cl-tmux::process-byte s (char-code #\z) state))
            "C-b z must dispatch :zoom-toggle and return NIL")))))

(test dispatch-select-window-prompt-opens-prompt
  ":select-window-prompt opens a prompt without signaling."
  (with-fake-session (s :nwindows 2)
    (let ((*prompt* nil))
      (cl-tmux::dispatch-command s :select-window-prompt nil)
      (is (prompt-active-p)
          ":select-window-prompt must open a prompt"))))

;;; ── choose-window uses menu system ──────────────────────────────────────────

(test dispatch-choose-window-shows-menu-overlay
  ":choose-window shows a menu overlay for j/k navigation without a prompt."
  (with-fake-session (s :nwindows 2)
    (let ((*overlay* nil) (*prompt* nil))
      (cl-tmux::dispatch-command s :choose-window nil)
      (is (overlay-active-p) ":choose-window must show an overlay")
      ;; choose-window now uses j/k menu navigation, not a text prompt.
      (is (not (null cl-tmux/prompt:*active-menu*))
          ":choose-window must set *active-menu* for navigation"))))

;;; ── Mouse reporting helpers ──────────────────────────────────────────────────

(test enable-mouse-reporting-writes-sequences
  "enable-mouse-reporting emits the three DEC private mode sequences."
  (let ((output (with-output-to-string (*standard-output*)
                  (cl-tmux/renderer:enable-mouse-reporting))))
    ;; Must contain all three mode strings
    (is (search "?1000h" output) "must contain ?1000h (X10 basic)")
    (is (search "?1002h" output) "must contain ?1002h (button events)")
    (is (search "?1006h" output) "must contain ?1006h (SGR extended)")))

(test disable-mouse-reporting-writes-disable-sequences
  "disable-mouse-reporting emits the three DEC private mode disable sequences."
  (let ((output (with-output-to-string (*standard-output*)
                  (cl-tmux/renderer:disable-mouse-reporting))))
    (is (search "?1006l" output) "must contain ?1006l")
    (is (search "?1002l" output) "must contain ?1002l")
    (is (search "?1000l" output) "must contain ?1000l")))

;;; ── All standard tmux default key bindings present ───────────────────────────
;;;
;;; Verify every key in the standard tmux default table has an entry in the
;;; prefix key-table.  This is a regression guard: if a binding is accidentally
;;; removed the test fails immediately.

(test standard-key-bindings-complete
  "All standard tmux default bindings must be present in prefix key-table."
  ;; Isolated config so the assertion runs against the clean default+extended
  ;; bindings, immune to the known global prefix-table polluter.
  (with-isolated-config
   (flet ((bound-p (key)
            (not (null (lookup-key-binding key)))))
    ;; Session
    (is (bound-p #\d)   "d → detach")
    (is (bound-p #\$)   "$ → rename-session")
    (is (bound-p #\s)   "s → choose-session")
    (is (bound-p #\()   "( → switch-client-prev")
    (is (bound-p #\))   ") → switch-client-next")
    (is (bound-p #\L)   "L → last-session")
    ;; Window
    (is (bound-p #\c)   "c → new-window")
    (is (bound-p #\n)   "n → next-window")
    (is (bound-p #\p)   "p → prev-window")
    (is (bound-p #\l)   "l → last-window")
    (is (bound-p #\w)   "w → choose-window")
    (is (bound-p #\f)   "f → find-window")
    (is (bound-p #\&)   "& → kill-window-confirm")
    (is (bound-p #\,)   ", → rename-window")
    (is (bound-p #\0)   "0 → select-window")
    (is (bound-p #\9)   "9 → select-window")
    (is (bound-p #\.)   ". → move-window-prompt")
    (is (bound-p #\')   "' → select-window-prompt")
    ;; Pane
    (is (bound-p #\%)   "% → split-vertical")
    (is (bound-p #\")   "\" → split-horizontal")
    (is (bound-p #\o)   "o → next-pane")
    (is (bound-p #\;)   "; → last-pane")
    (is (bound-p #\q)   "q → display-panes")
    (is (bound-p #\x)   "x → kill-pane-confirm")
    (is (bound-p #\z)   "z → zoom-toggle (lowercase)")
    (is (bound-p #\!)   "! → break-pane")
    (is (bound-p #\{)   "{ → swap-pane-backward")
    (is (bound-p #\})   "} → swap-pane-forward")
    ;; Buffer
    (is (bound-p #\[)   "[ → copy-mode-enter")
    (is (bound-p #\])   "] → paste-buffer")
    (is (bound-p (code-char 35))  "# → list-buffers")
    (is (bound-p (code-char 61))  "= → choose-buffer")
    (is (bound-p (code-char 45))  "- → delete-buffer")
    ;; Misc
    (is (bound-p #\:)   ": → command-prompt")
    (is (bound-p #\?)   "? → list-keys")
    (is (bound-p #\t)   "t → clock-mode")
    (is (bound-p #\i)   "i → display-info")
    (is (bound-p (code-char 126)) "~ → show-messages")
    (is (bound-p #\m)   "m → mark-pane")
    (is (bound-p (code-char 77))  "M → clear-mark")
    (is (bound-p #\E)   "E → select-layout-spread")
    (is (bound-p (code-char 32))  "Space → next-layout")
    (is (bound-p #\D)   "D → choose-client")
    (is (bound-p (code-char 2))   "C-b → send-prefix"))))

;;; ── Mouse scroll-wheel paths ─────────────────────────────────────────────────

(test dispatch-mouse-scroll-up-enters-copy-mode
  "Mouse scroll-up (btn=64) enters copy mode on the active pane when not in copy mode."
  (let* ((p0   (make-pane :id 1 :fd -1 :pid -1
                           :x 0 :y 0 :width 40 :height 24
                           :screen (make-screen 40 24)))
         (win  (make-window :id 1 :name "w" :width 40 :height 24
                            :panes (list p0)
                            :tree  (make-layout-leaf p0)
                            :active p0))
         (sess (make-session :id 1 :name "0" :windows (list win) :active win)))
    (cl-tmux/options:set-option "mouse" t)
    (unwind-protect
         (with-loop-state
           (let ((cl-tmux::*term-rows* 25) (cl-tmux::*term-cols* 40))
             (seed-scrollback (pane-screen p0) 5)
             (cl-tmux::%dispatch-mouse-event sess 64 5 5 nil)
             (is (screen-copy-mode-p (pane-screen p0))
                 "scroll-up must enter copy mode")))
      (cl-tmux/options:set-option "mouse" nil))))

(test dispatch-mouse-scroll-down-exits-copy-mode-at-bottom
  "Mouse scroll-down (btn=65) exits copy mode when the viewport is at the bottom (offset=0)."
  (let* ((p0   (make-pane :id 1 :fd -1 :pid -1
                           :x 0 :y 0 :width 40 :height 24
                           :screen (make-screen 40 24)))
         (win  (make-window :id 1 :name "w" :width 40 :height 24
                            :panes (list p0)
                            :tree  (make-layout-leaf p0)
                            :active p0))
         (sess (make-session :id 1 :name "0" :windows (list win) :active win))
         (screen (pane-screen p0)))
    (cl-tmux/options:set-option "mouse" t)
    (unwind-protect
         (with-loop-state
           (let ((cl-tmux::*term-rows* 25) (cl-tmux::*term-cols* 40))
             (seed-scrollback screen 5)
             (cl-tmux/commands::copy-mode-enter screen)
             ;; offset already at 0 — scroll down should exit copy mode
             (cl-tmux::%dispatch-mouse-event sess 65 5 5 nil)
             (is-false (screen-copy-mode-p screen)
                 "scroll-down at offset=0 must exit copy mode")))
      (cl-tmux/options:set-option "mouse" nil))))

(test dispatch-mouse-gated-by-mouse-option
  "%dispatch-mouse-event is a no-op when the 'mouse' option is false."
  (let* ((p0   (make-pane :id 1 :fd -1 :pid -1
                           :x 0 :y 0 :width 40 :height 24
                           :screen (make-screen 40 24)))
         (win  (make-window :id 1 :name "w" :width 40 :height 24
                            :panes (list p0)
                            :tree  (make-layout-leaf p0)
                            :active p0))
         (sess (make-session :id 1 :name "0" :windows (list win) :active win)))
    (cl-tmux/options:set-option "mouse" nil)
    (with-loop-state
      (let ((cl-tmux::*term-rows* 25) (cl-tmux::*term-cols* 40))
        ;; With mouse off, click must not enter copy mode.
        (cl-tmux::%dispatch-mouse-event sess 0 5 5 nil)
        (is-false (screen-copy-mode-p (pane-screen p0))
            "mouse event must be ignored when mouse option is off")))))

;;; ── %status-col-to-window helper ─────────────────────────────────────────────

(test status-col-to-window-returns-nil-before-first-window
  "%status-col-to-window returns NIL for a column before any window entry."
  (let* ((p0   (make-pane :id 1 :fd -1 :pid -1 :x 0 :y 0 :width 20 :height 5
                           :screen (make-screen 20 5)))
         (win  (make-window :id 0 :name "win0" :width 20 :height 5
                            :panes (list p0) :tree (make-layout-leaf p0)))
         (sess (make-session :id 1 :name "mysess" :windows (list win))))
    (window-select-pane win p0)
    (session-select-window sess win)
    ;; Session prefix is " mysess" = 1 + 6 = 7 chars.
    ;; First window "win0" entry starts at column 7; col 0 is before it.
    (is (null (cl-tmux::%status-col-to-window sess 0))
        "%status-col-to-window must return NIL for column before the first window")))

(test status-col-to-window-returns-window-for-column-in-entry
  "%status-col-to-window returns the window when the column falls within its entry."
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
    (is (eq win (cl-tmux::%status-col-to-window sess 2))
        "%status-col-to-window must return the window for a column within its entry")))

;;; ── Mouse button constant sanity checks ──────────────────────────────────────

(test mouse-button-constants-have-expected-values
  "Named mouse button constants must have the correct integer values."
  (is (= 0  cl-tmux::+mouse-btn-left+)        "left button must be 0")
  (is (= 3  cl-tmux::+mouse-btn-release-x10+) "X10 release must be 3")
  (is (= 32 cl-tmux::+mouse-btn-motion+)       "motion must be 32")
  (is (= 64 cl-tmux::+mouse-btn-scroll-up+)    "scroll-up must be 64")
  (is (= 65 cl-tmux::+mouse-btn-scroll-down+)  "scroll-down must be 65"))

;;; ── SGR mouse parser ─────────────────────────────────────────────────────────

(test parse-sgr-mouse-press-sequence
  "%parse-sgr-mouse parses a well-formed SGR press sequence."
  ;; ESC [ < 0 ; 10 ; 5 M  — btn=0, col=10, row=5 (1-based), press
  (let* ((seq "ESC[<0;10;5M")   ; textual — we build the actual byte vector below
         (s   (format nil "~C[<0;10;5M" #\Escape))
         (buf (make-array (length s) :element-type '(unsigned-byte 8)
                          :initial-contents (map 'list #'char-code s)))
         (len (length buf)))
    (declare (ignore seq))
    (multiple-value-bind (btn col row release-p)
        (cl-tmux::%parse-sgr-mouse buf len)
      (is (= 0 btn)       "SGR btn must be 0 for left-button press")
      (is (= 9 col)       "SGR col must be 0-based (10-1=9)")
      (is (= 4 row)       "SGR row must be 0-based (5-1=4)")
      (is-false release-p "press sequence must have release-p=NIL"))))

(test parse-sgr-mouse-release-sequence
  "%parse-sgr-mouse parses a well-formed SGR release sequence (final byte 'm')."
  (let* ((s   (format nil "~C[<0;10;5m" #\Escape))
         (buf (make-array (length s) :element-type '(unsigned-byte 8)
                          :initial-contents (map 'list #'char-code s)))
         (len (length buf)))
    (multiple-value-bind (btn col row release-p)
        (cl-tmux::%parse-sgr-mouse buf len)
      (is (= 0 btn)    "SGR btn must be 0")
      (is (= 9 col)    "SGR col 0-based")
      (is (= 4 row)    "SGR row 0-based")
      (is-true release-p "release sequence (final 'm') must set release-p=T"))))

(test sgr-mouse-sequence-p-detects-sgr-intro
  "%sgr-mouse-sequence-p returns T for ESC [ < prefix."
  (let* ((s   (format nil "~C[<0;5;3M" #\Escape))
         (buf (make-array (length s) :element-type '(unsigned-byte 8)
                          :initial-contents (map 'list #'char-code s)))
         (len (length buf)))
    (is (cl-tmux::%sgr-mouse-sequence-p buf len)
        "%sgr-mouse-sequence-p must return T for ESC [ < prefix")))

(test sgr-mouse-terminated-p-detects-final-byte
  "%sgr-mouse-terminated-p returns T when the last byte is 'M' or 'm'."
  (flet ((buf-from (s)
           (make-array (length s) :element-type '(unsigned-byte 8)
                       :initial-contents (map 'list #'char-code s))))
    (let* ((press-str   (format nil "~C[<0;5;3M" #\Escape))
           (release-str (format nil "~C[<0;5;3m" #\Escape))
           (pb (buf-from press-str))
           (rb (buf-from release-str)))
      (is (cl-tmux::%sgr-mouse-terminated-p pb (length pb))
          "press sequence ending in 'M' must be terminated")
      (is (cl-tmux::%sgr-mouse-terminated-p rb (length rb))
          "release sequence ending in 'm' must be terminated"))))

;;; ── define-cps-state: ignorable session/byte args ────────────────────────────

(test cps-state-ignores-unused-args
  "A define-cps-state function that ignores both args compiles and runs cleanly."
  ;; Both session and byte are declared ignorable — verify no compile warnings
  ;; by just calling the function and checking the return type.
  (let ((s (make-fake-session))
        (state (cl-tmux::make-input-state)))
    (is (null (cl-tmux::process-byte s 0 state))
        "NUL byte must return NIL (forwarded, no quit)")))

;;; ── Overlay arrow-key scrolling via escape sequence ─────────────────────────
;;;
;;; When the overlay is active and ESC [ A arrives, %overlay-escape-second-byte
;;; scrolls the overlay up; ESC [ B scrolls it down.

(test overlay-escape-up-scrolls-overlay
  "ESC [ A while an overlay is open scrolls the overlay up (offset -1)."
  (let ((s (make-fake-session)))
    (let ((*overlay* nil) (cl-tmux::*dirty* nil))
      (show-overlay (format nil "~{line~A~%~}" (loop for i from 1 to 20 collect i)))
      (let ((state (cl-tmux::make-input-state)))
        ;; Feed ESC [ A one byte at a time.
        (cl-tmux::process-byte s 27 state)
        (cl-tmux::process-byte s 91 state)
        (cl-tmux::process-byte s 65 state))
      ;; After the sequence the overlay should still be open.
      (is (overlay-active-p)
          "overlay must remain open after ESC [ A (up arrow)"))))

(test overlay-escape-down-scrolls-overlay
  "ESC [ B while an overlay is open scrolls the overlay down (offset +1)."
  (let ((s (make-fake-session)))
    (let ((*overlay* nil) (cl-tmux::*dirty* nil))
      (show-overlay (format nil "~{line~A~%~}" (loop for i from 1 to 20 collect i)))
      (let ((state (cl-tmux::make-input-state)))
        (cl-tmux::process-byte s 27 state)
        (cl-tmux::process-byte s 91 state)
        (cl-tmux::process-byte s 66 state))
      (is (overlay-active-p)
          "overlay must remain open after ESC [ B (down arrow)"))))

(test overlay-bare-esc-dismisses-overlay
  "A lone ESC (ESC + non-'[' byte) while an overlay is open dismisses it."
  (let ((s (make-fake-session)))
    (let ((*overlay* nil) (cl-tmux::*dirty* nil))
      (show-overlay "some text")
      (let ((state (cl-tmux::make-input-state)))
        ;; ESC then 'x' — not a CSI sequence → dismiss
        (cl-tmux::process-byte s 27 state)
        (cl-tmux::process-byte s (char-code #\x) state))
      (is-false (overlay-active-p)
          "overlay must be dismissed by bare ESC"))))

;;; ── handle-prompt-key: additional editing keys ────────────────────────────────

(test handle-prompt-key-ctrl-a-moves-to-bol
  "C-a (byte 1) moves the cursor to the beginning of the prompt line."
  (with-clean-prompt
    (prompt-start "test" "hello"
                  (lambda (buf) (declare (ignore buf)) nil))
    ;; Move cursor to end first (EOL)
    (prompt-cursor-eol)
    (is (= 5 (prompt-cursor-index *prompt*)) "cursor at end")
    (cl-tmux::handle-prompt-key 1)  ; C-a
    (is (= 0 (prompt-cursor-index *prompt*))
        "C-a must move cursor to position 0")))

(test handle-prompt-key-ctrl-e-moves-to-eol
  "C-e (byte 5) moves the cursor to the end of the prompt line."
  (with-clean-prompt
    (prompt-start "test" "hello"
                  (lambda (buf) (declare (ignore buf)) nil))
    ;; Cursor starts at end; move to BOL first
    (prompt-cursor-bol)
    (is (= 0 (prompt-cursor-index *prompt*)) "cursor at start")
    (cl-tmux::handle-prompt-key 5)  ; C-e
    (is (= 5 (prompt-cursor-index *prompt*))
        "C-e must move cursor to end of buffer")))

(test handle-prompt-key-ctrl-c-cancels
  "C-c (byte 3) cancels the prompt without running on-submit."
  (with-clean-prompt
    (let ((submitted nil))
      (prompt-start "test" "abc"
                    (lambda (buf) (setf submitted buf)))
      (cl-tmux::handle-prompt-key 3)  ; C-c
      (is-false (prompt-active-p) "C-c must dismiss the prompt")
      (is (null submitted) "C-c must not call on-submit"))))

(test handle-prompt-key-printable-inserts-char
  "A printable ASCII byte inserts the corresponding character into the buffer."
  (with-clean-prompt
    (prompt-start "test" ""
                  (lambda (buf) (declare (ignore buf)) nil))
    (cl-tmux::handle-prompt-key (char-code #\A))
    (is (string= "A" (prompt-buffer *prompt*))
        "printable key 'A' must be inserted into buffer")))

