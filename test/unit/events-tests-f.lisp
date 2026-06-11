(in-package #:cl-tmux/test)

;;;; copy-mode PageUp/PageDown, prefix-arrow, send-prefix, modifier+arrow, meta/alt bindings — part VI

(in-suite events-suite)

;;; ── Copy-mode PageUp / PageDown via escape sequence ─────────────────────────

(test copy-mode-pageup-scrolls-one-page
  "ESC [ 5 ~ (PageUp) fed one byte at a time scrolls up by screen-height lines."
  (let ((s (make-fake-session)))
    (with-loop-state
      (let ((screen (active-screen s))
            (state  (cl-tmux::make-input-state)))
        (cl-tmux::dispatch-command s :copy-mode-enter nil)
        (seed-scrollback screen 30)
        (is (zerop (screen-copy-offset screen)))
        ;; ESC [ 5 ~  = 27 91 53 126
        (cl-tmux::process-byte s 27  state)
        (cl-tmux::process-byte s 91  state)
        (cl-tmux::process-byte s 53  state)
        (cl-tmux::process-byte s 126 state)
        (let ((h (screen-height screen)))
          (is (= (min h 30) (screen-copy-offset screen))
              "PageUp must scroll copy-offset by screen-height lines"))))))

(test copy-mode-pagedown-scrolls-one-page
  "ESC [ 6 ~ (PageDown) fed one byte at a time scrolls down by screen-height lines."
  (let ((s (make-fake-session)))
    (with-loop-state
      (let ((screen (active-screen s))
            (state  (cl-tmux::make-input-state)))
        (cl-tmux::dispatch-command s :copy-mode-enter nil)
        (seed-scrollback screen 30)
        ;; Pre-scroll up by 2*screen-height (clamped to scrollback length = 30)
        (let* ((h     (screen-height screen))
               (start (min (* 2 h) 30)))
          (cl-tmux/commands::copy-mode-scroll screen start)
          (is (= start (screen-copy-offset screen)) "pre-scroll verified")
          ;; ESC [ 6 ~  = 27 91 54 126
          (cl-tmux::process-byte s 27  state)
          (cl-tmux::process-byte s 91  state)
          (cl-tmux::process-byte s 54  state)
          (cl-tmux::process-byte s 126 state)
          ;; After PageDown the offset decreases by h (clamped to 0).
          (let ((expected (max 0 (- start h))))
            (is (= expected (screen-copy-offset screen))
                "PageDown must scroll copy-offset down by screen-height lines")))))))

;;; ── Prefix arrow keys select pane ────────────────────────────────────────────

(test prefix-arrow-up-selects-pane-up
  "C-b Up (prefix then ESC [ A) dispatches :select-pane-up."
  (let ((s (make-fake-session)))
    (with-loop-state
      (let ((state (cl-tmux::make-input-state)))
        ;; Feed C-b (prefix) then ESC [ A
        (cl-tmux::process-byte s 2   state)
        (cl-tmux::process-byte s 27  state)
        (cl-tmux::process-byte s 91  state)
        ;; Final byte — returns to ground state, no crash.
        (is (null (cl-tmux::process-byte s 65 state))
            "C-b Up arrow must return NIL (no quit/detach)")))))

(test prefix-arrow-down-selects-pane-down
  "C-b Down (prefix then ESC [ B) dispatches :select-pane-down."
  (let ((s (make-fake-session)))
    (with-loop-state
      (let ((state (cl-tmux::make-input-state)))
        (cl-tmux::process-byte s 2   state)
        (cl-tmux::process-byte s 27  state)
        (cl-tmux::process-byte s 91  state)
        (is (null (cl-tmux::process-byte s 66 state))
            "C-b Down arrow must return NIL (no quit/detach)")))))

;;; ── C-b C-b send-prefix ──────────────────────────────────────────────────────

(test prefix-then-prefix-byte-sends-send-prefix
  "C-b C-b (byte 2 twice) dispatches :send-prefix (no crash, returns NIL)."
  (let ((s (make-fake-session)))
    (with-loop-state
      (let ((state (cl-tmux::make-input-state)))
        (cl-tmux::process-byte s 2 state)  ; prefix
        (is (null (cl-tmux::process-byte s 2 state))
            "C-b C-b must dispatch :send-prefix and return NIL")))))

;;; ── Modifier+arrow key-name helpers ────────────────────────────────────────

(test arrow-final-name-maps-arrow-bytes
  "%arrow-final-name returns the tmux base name for each arrow final byte."
  (is (string= "Up"    (cl-tmux::%arrow-final-name 65)))
  (is (string= "Down"  (cl-tmux::%arrow-final-name 66)))
  (is (string= "Right" (cl-tmux::%arrow-final-name 67)))
  (is (string= "Left"  (cl-tmux::%arrow-final-name 68))))

(test arrow-final-name-returns-nil-for-non-arrows
  "%arrow-final-name returns NIL for non-arrow final bytes."
  (is (null (cl-tmux::%arrow-final-name 72)))   ; H = Home
  (is (null (cl-tmux::%arrow-final-name 109)))) ; m = SGR final

(test modifier-arrow-key-name-builds-canonical-names
  "%modifier-arrow-key-name builds the exact strings %parse-key-token stores:
   5=Ctrl→C-, 3=Meta→M-, 2=Shift→S-, combined with the arrow base name."
  (is (string= "C-Up"    (cl-tmux::%modifier-arrow-key-name 53 65)))
  (is (string= "M-Left"  (cl-tmux::%modifier-arrow-key-name 51 68)))
  (is (string= "S-Down"  (cl-tmux::%modifier-arrow-key-name 50 66)))
  (is (string= "C-Right" (cl-tmux::%modifier-arrow-key-name 53 67))))

(test modifier-arrow-key-name-builds-combined-modifiers
  "Combined modifiers resolve via %modifier-prefix in canonical C-/M-/S- order:
   6=Ctrl+Shift, 7=Ctrl+Meta, 8=Ctrl+Meta+Shift, 4=Meta+Shift."
  (is (string= "C-S-Up"   (cl-tmux::%modifier-arrow-key-name 54 65)))  ; 6
  (is (string= "C-M-Up"   (cl-tmux::%modifier-arrow-key-name 55 65)))  ; 7
  (is (string= "C-M-S-Up" (cl-tmux::%modifier-arrow-key-name 56 65)))  ; 8
  (is (string= "M-S-Up"   (cl-tmux::%modifier-arrow-key-name 52 65)))) ; 4

(test modifier-arrow-key-name-returns-nil-for-unknown
  "%modifier-arrow-key-name returns NIL for a non-arrow final or a no-modifier
   value, so the caller forwards the sequence unchanged."
  (is (null (cl-tmux::%modifier-arrow-key-name 53 72)))  ; Ctrl+Home — not arrow
  (is (null (cl-tmux::%modifier-arrow-key-name 49 65)))) ; '1' = no modifier → NIL

;;; ── Modifier+arrow binding override (bind C-Up / bind -n M-Left) ────────────

(test prefix-c-up-binding-overrides-resize
  "bind C-Up next-window makes C-b then Ctrl+Up (ESC [ 1 ; 5 A) run next-window
   instead of the hardcoded resize-pane default."
  (with-isolated-config
    (cl-tmux/config:apply-config-directive '("bind" "C-Up" "next-window"))
    (let ((s (make-fake-session :nwindows 2)))
      (with-loop-state
        (let ((state (cl-tmux::make-input-state)))
          (dolist (b '(2 27 91 49 59 53 65))  ; C-b ESC [ 1 ; 5 A
            (cl-tmux::process-byte s b state))
          (is (eq (second (session-windows s)) (session-active-window s))
              "bound C-Up must run next-window, not resize"))))))

(test prefix-m-up-binding-overrides-resize
  "bind M-Up next-window makes C-b then Alt+Up (ESC [ 1 ; 3 A) run next-window
   instead of the hardcoded :resize-up default."
  (with-isolated-config
    (cl-tmux/config:apply-config-directive '("bind" "M-Up" "next-window"))
    (let ((s (make-fake-session :nwindows 2)))
      (with-loop-state
        (let ((state (cl-tmux::make-input-state)))
          (dolist (b '(2 27 91 49 59 51 65))  ; C-b ESC [ 1 ; 3 A
            (cl-tmux::process-byte s b state))
          (is (eq (second (session-windows s)) (session-active-window s))
              "bound M-Up must run next-window, not resize"))))))

(test prefix-plain-arrow-binding-overrides-select-pane
  "bind Up next-window makes C-b Up (ESC [ A) run next-window instead of the
   hardcoded :select-pane-up default."
  (with-isolated-config
    (cl-tmux/config:apply-config-directive '("bind" "Up" "next-window"))
    (let ((s (make-fake-session :nwindows 2)))
      (with-loop-state
        (let ((state (cl-tmux::make-input-state)))
          (dolist (b '(2 27 91 65))  ; C-b ESC [ A
            (cl-tmux::process-byte s b state))
          (is (eq (second (session-windows s)) (session-active-window s))
              "bound Up must run next-window, not select-pane"))))))

(test unbound-prefix-c-up-leaves-active-window
  "With no C-Up binding, C-b Ctrl+Up takes the resize fallback and must NOT
   change the active window (the override is purely additive)."
  (with-isolated-config
    (let ((s (make-fake-session :nwindows 2)))
      (with-loop-state
        (let ((state (cl-tmux::make-input-state)))
          (dolist (b '(2 27 91 49 59 53 65))  ; C-b ESC [ 1 ; 5 A
            (cl-tmux::process-byte s b state))
          (is (eq (first (session-windows s)) (session-active-window s))
              "unbound C-Up must leave the first window active"))))))

(test root-m-left-binding-fires-without-prefix
  "bind -n M-Left next-window makes a bare Alt+Left (ESC [ 1 ; 3 D) run
   next-window with no prefix — the root-table modifier+arrow path."
  (with-isolated-config
    (cl-tmux/config:apply-config-directive '("bind" "-n" "M-Left" "next-window"))
    (let ((s (make-fake-session :nwindows 2)))
      (with-loop-state
        (let ((state (cl-tmux::make-input-state)))
          (dolist (b '(27 91 49 59 51 68))  ; ESC [ 1 ; 3 D  (no prefix)
            (cl-tmux::process-byte s b state))
          (is (eq (second (session-windows s)) (session-active-window s))
              "bound -n M-Left must run next-window at root"))))))

(test root-c-up-binding-fires-without-prefix
  "bind -n C-Up next-window makes a bare Ctrl+Up (ESC [ 1 ; 5 A) run
   next-window with no prefix."
  (with-isolated-config
    (cl-tmux/config:apply-config-directive '("bind" "-n" "C-Up" "next-window"))
    (let ((s (make-fake-session :nwindows 2)))
      (with-loop-state
        (let ((state (cl-tmux::make-input-state)))
          (dolist (b '(27 91 49 59 53 65))  ; ESC [ 1 ; 5 A  (no prefix)
            (cl-tmux::process-byte s b state))
          (is (eq (second (session-windows s)) (session-active-window s))
              "bound -n C-Up must run next-window at root"))))))

(test unbound-root-c-up-leaves-active-window
  "With no -n C-Up binding, a bare Ctrl+Up is forwarded to the pane and must
   NOT change the active window."
  (with-isolated-config
    (let ((s (make-fake-session :nwindows 2)))
      (with-loop-state
        (let ((state (cl-tmux::make-input-state)))
          (dolist (b '(27 91 49 59 53 65))  ; ESC [ 1 ; 5 A  (no prefix, unbound)
            (cl-tmux::process-byte s b state))
          (is (eq (first (session-windows s)) (session-active-window s))
              "unbound bare C-Up must leave the first window active"))))))

;;; ── Meta/Alt key-name helper and bind override (bind -n M-h / bind M-j) ─────

(test meta-key-name-builds-canonical-names
  "%meta-key-name reconstructs the M-<char> name from the byte that follows ESC,
   matching the M-<char> encoding send-keys produces."
  (is (string= "M-a"     (cl-tmux::%meta-key-name 97)))   ; a
  (is (string= "M-1"     (cl-tmux::%meta-key-name 49)))   ; 1
  (is (string= "M-/"     (cl-tmux::%meta-key-name 47)))   ; /
  (is (string= "M-H"     (cl-tmux::%meta-key-name 72)))   ; H (Alt+Shift+h)
  (is (string= "M-Space" (cl-tmux::%meta-key-name 32))))  ; space

(test meta-key-name-returns-nil-for-control-and-del
  "%meta-key-name returns NIL for control bytes and DEL, so they forward
   unchanged rather than being treated as meta chords."
  (is (null (cl-tmux::%meta-key-name 8)))    ; ^H (backspace)
  (is (null (cl-tmux::%meta-key-name 27)))   ; ESC
  (is (null (cl-tmux::%meta-key-name 127)))) ; DEL

(test root-m-h-binding-fires-without-prefix
  "bind -n M-h next-window makes a bare Alt+h (ESC h) run next-window with no
   prefix — the root-table meta path overrides forwarding to the pane."
  (with-isolated-config
    (cl-tmux/config:apply-config-directive '("bind" "-n" "M-h" "next-window"))
    (let ((s (make-fake-session :nwindows 2)))
      (with-loop-state
        (let ((state (cl-tmux::make-input-state)))
          (dolist (b '(27 104))  ; ESC h  (no prefix)
            (cl-tmux::process-byte s b state))
          (is (eq (second (session-windows s)) (session-active-window s))
              "bound -n M-h must run next-window at root"))))))

(test prefix-m-j-binding-fires
  "bind M-j next-window makes C-b then Alt+j (ESC j) run next-window — the
   after-prefix meta path."
  (with-isolated-config
    (cl-tmux/config:apply-config-directive '("bind" "M-j" "next-window"))
    (let ((s (make-fake-session :nwindows 2)))
      (with-loop-state
        (let ((state (cl-tmux::make-input-state)))
          (dolist (b '(2 27 106))  ; C-b ESC j
            (cl-tmux::process-byte s b state))
          (is (eq (second (session-windows s)) (session-active-window s))
              "bound M-j must run next-window after prefix"))))))

(test unbound-root-meta-key-forwards-and-leaves-window
  "With no -n M-x binding, a bare Alt+x is forwarded to the pane and must NOT
   change the active window (the override is purely additive)."
  (with-isolated-config
    (let ((s (make-fake-session :nwindows 2)))
      (with-loop-state
        (let ((state (cl-tmux::make-input-state)))
          (dolist (b '(27 120))  ; ESC x  (no prefix, unbound)
            (cl-tmux::process-byte s b state))
          (is (eq (first (session-windows s)) (session-active-window s))
              "unbound bare M-x must leave the first window active"))))))

;;; ── Custom key tables (switch-client -T <table>) ────────────────────────────

(test cmd-switch-client-T-sets-and-resets-key-table
  "switch-client -T <table> sets *key-table*; -T root resets it to NIL."
  (with-loop-state
    (let ((s (make-fake-session :nwindows 1)))
      (cl-tmux::%cmd-switch-client s '("-T" "resize"))
      (is (string= "resize" cl-tmux::*key-table*)
          "switch-client -T resize activates the custom table")
      (cl-tmux::%cmd-switch-client s '("-T" "root"))
      (is (null cl-tmux::*key-table*)
          "switch-client -T root returns to the normal flow"))))

(test custom-key-table-dispatches-from-active-table-and-persists
  "In a custom key table, a bound key dispatches from THAT table and the table
   persists (modal mode)."
  (with-isolated-config
    (let ((s (make-fake-session :nwindows 2)))
      (with-loop-state
        (cl-tmux/config:apply-config-directive '("bind" "-T" "resize" "x" "next-window"))
        (setf cl-tmux::*key-table* "resize")
        (let ((state (cl-tmux::make-input-state)))
          (cl-tmux::process-byte s 120 state)  ; 'x'
          (is (eq (second (session-windows s)) (session-active-window s))
              "a key bound in the active custom table runs its binding")
          (is (string= "resize" cl-tmux::*key-table*)
              "the custom table persists after a key (modal)"))))))

(test custom-key-table-binding-can-switch-back-to-root
  "A binding in a custom table running 'switch-client -T root' exits the table."
  (with-isolated-config
    (let ((s (make-fake-session :nwindows 1)))
      (with-loop-state
        (cl-tmux/config:apply-config-directive
         '("bind" "-T" "resize" "q" "switch-client" "-T" "root"))
        (setf cl-tmux::*key-table* "resize")
        (let ((state (cl-tmux::make-input-state)))
          (cl-tmux::process-byte s 113 state)  ; 'q'
          (is (null cl-tmux::*key-table*)
              "switch-client -T root from within the table exits it"))))))

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

(test cmd-switch-client-t-switches-to-named-session
  "switch-client -t <name> makes the named session the front (touched) one."
  (with-loop-state
    (with-empty-registry
      (multiple-value-bind (s0 s1 s2) (%make-three-session-registry)
        (declare (ignore s0 s1))
        (let ((result (cl-tmux::%cmd-switch-client (cl-tmux::server-find-session "1")
                                                   '("-t" "2"))))
          (is (eq s2 result) "-t 2 selects session named 2")
          (is-true cl-tmux::*dirty* "a session switch marks the screen dirty"))))))

(test cmd-switch-client-n-and-p-cycle-sessions
  "switch-client -n / -p move to the next / previous session cyclically."
  (with-loop-state
    (with-empty-registry
      (multiple-value-bind (s0 s1 s2) (%make-three-session-registry)
        ;; current = s1; registry order is (s0 s1 s2): next → s2, prev → s0.
        (is (eq s2 (cl-tmux::%cmd-switch-client s1 '("-n")))
            "-n from session 1 goes to session 2")
        (is (eq s0 (cl-tmux::%cmd-switch-client s1 '("-p")))
            "-p from session 1 goes to session 0")))))

(test cmd-switch-client-l-switches-to-last-active
  "switch-client -l selects the second-most-recently-active session."
  (with-loop-state
    (with-empty-registry
      (multiple-value-bind (s0 s1 s2) (%make-three-session-registry)
        (declare (ignore s0))
        ;; last-active stamps 10/30/20 → desc order s1,s2,s0 → second = s2.
        (is (eq s2 (cl-tmux::%cmd-switch-client s1 '("-l")))
            "-l from the front session 1 returns to session 2")))))

(test cmd-switch-client-t-and-T-are-orthogonal
  "switch-client -t <name> -T <table> performs the session move AND arms the
   key table in one invocation."
  (with-loop-state
    (with-empty-registry
      (multiple-value-bind (s0 s1 s2) (%make-three-session-registry)
        (declare (ignore s0 s1))
        (let ((result (cl-tmux::%cmd-switch-client (cl-tmux::server-find-session "1")
                                                   '("-t" "2" "-T" "resize"))))
          (is (eq s2 result) "-t still switches the session when -T is also given")
          (is (string= "resize" cl-tmux::*key-table*)
              "-T still arms the key table when -t is also given"))))))

;;; ── Default M-1..M-5 preset-layout bindings (tmux defaults) ─────────────────

(test default-meta-digit-layout-bindings-registered
  "C-b M-1..M-5 are installed as select-layout command token-lists in the prefix
   table, matching real tmux's preset-layout defaults."
  (with-isolated-config
    (flet ((cmd (k) (cl-tmux/config:key-table-command
                     (cl-tmux/config:key-table-lookup "prefix" k))))
      (is (equal '("select-layout" "even-horizontal") (cmd "M-1")))
      (is (equal '("select-layout" "even-vertical")   (cmd "M-2")))
      (is (equal '("select-layout" "main-horizontal") (cmd "M-3")))
      (is (equal '("select-layout" "main-vertical")   (cmd "M-4")))
      (is (equal '("select-layout" "tiled")           (cmd "M-5"))))))

(test prefix-meta-1-applies-layout-end-to-end
  "C-b then Alt+1 (ESC 1) runs the bound select-layout even-horizontal on a
   two-pane window without error (the after-prefix meta path fires the default)."
  (with-isolated-config
    (let ((s (make-fake-session :nwindows 1 :npanes 2)))
      (with-loop-state
        (let ((state (cl-tmux::make-input-state)))
          (dolist (b '(2 27 49))  ; C-b ESC 1
            (cl-tmux::process-byte s b state))
          ;; Layout applied: the window still has its two panes and a usable tree.
          (is (= 2 (length (window-panes (session-active-window s))))
              "select-layout via C-b M-1 must preserve both panes"))))))

