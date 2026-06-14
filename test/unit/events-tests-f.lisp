(in-package #:cl-tmux/test)

;;;; copy-mode PageUp/PageDown, prefix-arrow, send-prefix, modifier+arrow, meta/alt bindings — part VI

(in-suite events-suite)

;;; ── Copy-mode PageUp / PageDown via escape sequence ─────────────────────────

(test copy-mode-pageup-scrolls-one-page
  "ESC [ 5 ~ (PageUp) fed one byte at a time scrolls up by screen-height lines."
  (with-fake-session (s)
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
            "PageUp must scroll copy-offset by screen-height lines")))))

(test copy-mode-pagedown-scrolls-one-page
  "ESC [ 6 ~ (PageDown) fed one byte at a time scrolls down by screen-height lines."
  (with-fake-session (s)
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
              "PageDown must scroll copy-offset down by screen-height lines"))))))

;;; ── Prefix arrow keys select pane ────────────────────────────────────────────

(test prefix-arrow-up-down-returns-nil-table
  "C-b Up (ESC [ A) and C-b Down (ESC [ B) each return NIL (no quit/detach)."
  (dolist (row '((65 "C-b Up arrow must return NIL (no quit/detach)")
                 (66 "C-b Down arrow must return NIL (no quit/detach)")))
    (destructuring-bind (final desc) row
      (with-fake-session (s)
        (let ((state (cl-tmux::make-input-state)))
          (cl-tmux::process-byte s 2   state)
          (cl-tmux::process-byte s 27  state)
          (cl-tmux::process-byte s 91  state)
          (is (null (cl-tmux::process-byte s final state)) "~A" desc))))))

;;; ── C-b C-b send-prefix ──────────────────────────────────────────────────────

(test prefix-then-prefix-byte-sends-send-prefix
  "C-b C-b (byte 2 twice) dispatches :send-prefix (no crash, returns NIL)."
  (with-fake-session (s)
    (let ((state (cl-tmux::make-input-state)))
      (cl-tmux::process-byte s 2 state)  ; prefix
      (is (null (cl-tmux::process-byte s 2 state))
          "C-b C-b must dispatch :send-prefix and return NIL"))))

;;; ── Modifier+arrow key-name helpers ────────────────────────────────────────

(test arrow-final-name-table
  "%arrow-final-name returns the tmux base name for arrow finals and NIL for others."
  (dolist (row '((65 "Up"    "A → Up")
                 (66 "Down"  "B → Down")
                 (67 "Right" "C → Right")
                 (68 "Left"  "D → Left")
                 (72 nil     "H (Home) → NIL")
                 (109 nil    "m (SGR final) → NIL")))
    (destructuring-bind (byte expected desc) row
      (is (equal expected (cl-tmux::%arrow-final-name byte)) "~A" desc))))

(test modifier-arrow-key-name-table
  "%modifier-arrow-key-name builds C-/M-/S- prefixed arrow names; NIL for non-arrow or no-modifier."
  (dolist (row '((53 65 "C-Up"    "5=Ctrl + A → C-Up")
                 (51 68 "M-Left"  "3=Meta + D → M-Left")
                 (50 66 "S-Down"  "2=Shift + B → S-Down")
                 (53 67 "C-Right" "5=Ctrl + C → C-Right")
                 (54 65 "C-S-Up"   "6=Ctrl+Shift + A → C-S-Up")
                 (55 65 "C-M-Up"   "7=Ctrl+Meta + A → C-M-Up")
                 (56 65 "C-M-S-Up" "8=Ctrl+Meta+Shift + A → C-M-S-Up")
                 (52 65 "M-S-Up"   "4=Meta+Shift + A → M-S-Up")
                 (53 72 nil        "Ctrl+H (Home final) → NIL")
                 (49 65 nil        "1=no-modifier → NIL")))
    (destructuring-bind (mod arrow expected desc) row
      (is (equal expected (cl-tmux::%modifier-arrow-key-name mod arrow)) "~A" desc))))

;;; ── Modifier+arrow binding override (bind C-Up / bind -n M-Left) ────────────

(test prefix-modifier-arrow-overrides-table
  "Binding C-Up/M-Up/Up to next-window makes C-b + sequence run next-window (not resize/select-pane)."
  (dolist (c '(("C-Up" (2 27 91 49 59 53 65) "C-b C-Up → next-window, not resize")
               ("M-Up" (2 27 91 49 59 51 65) "C-b M-Up → next-window, not resize")
               ("Up"   (2 27 91 65)           "C-b Up → next-window, not select-pane")))
    (destructuring-bind (key-name bytes desc) c
      (with-isolated-config
        (cl-tmux/config:apply-config-directive (list "bind" key-name "next-window"))
        (with-fake-session (s :nwindows 2)
          (let ((state (cl-tmux::make-input-state)))
            (dolist (b bytes) (cl-tmux::process-byte s b state))
            (is (eq (second (session-windows s)) (session-active-window s))
                "~A" desc)))))))

(test unbound-prefix-modifier-arrow-leaves-window-table
  "Without a binding, C-b + modifier+arrow sequences leave the first window active."
  (dolist (c '(((2 27 91 49 59 53 65) "C-b C-Up unbound: first window stays")
               ((27 91 49 59 53 65)    "bare C-Up unbound: first window stays")))
    (destructuring-bind (bytes desc) c
      (with-isolated-config
        (with-fake-session (s :nwindows 2)
          (let ((state (cl-tmux::make-input-state)))
            (dolist (b bytes) (cl-tmux::process-byte s b state))
            (is (eq (first (session-windows s)) (session-active-window s))
                "~A" desc)))))))

(test root-modifier-arrow-binding-table
  "Bindings with -n fire modifier+arrow sequences at root without prefix."
  (dolist (c '(("M-Left" (27 91 49 59 51 68) "M-Left bare → next-window")
               ("C-Up"   (27 91 49 59 53 65) "C-Up bare → next-window")))
    (destructuring-bind (key-name bytes desc) c
      (with-isolated-config
        (cl-tmux/config:apply-config-directive (list "bind" "-n" key-name "next-window"))
        (with-fake-session (s :nwindows 2)
          (let ((state (cl-tmux::make-input-state)))
            (dolist (b bytes) (cl-tmux::process-byte s b state))
            (is (eq (second (session-windows s)) (session-active-window s))
                "~A" desc)))))))

;;; ── Meta/Alt key-name helper and bind override (bind -n M-h / bind M-j) ─────

(test meta-key-name-table
  "%meta-key-name returns M-<char> for printable bytes and NIL for control bytes and DEL."
  (dolist (row '((97  "M-a"     "a → M-a")
                 (49  "M-1"     "1 → M-1")
                 (47  "M-/"     "/ → M-/")
                 (72  "M-H"     "H (Alt+Shift+h) → M-H")
                 (32  "M-Space" "space → M-Space")
                 (8   nil       "^H (backspace) → NIL")
                 (27  nil       "ESC → NIL")
                 (127 nil       "DEL → NIL")))
    (destructuring-bind (byte expected desc) row
      (is (equal expected (cl-tmux::%meta-key-name byte)) "~A" desc))))

(test root-m-h-binding-fires-without-prefix
  "bind -n M-h next-window makes a bare Alt+h (ESC h) run next-window with no
   prefix — the root-table meta path overrides forwarding to the pane."
  (with-isolated-config
    (cl-tmux/config:apply-config-directive '("bind" "-n" "M-h" "next-window"))
    (with-fake-session (s :nwindows 2)
        (let ((state (cl-tmux::make-input-state)))
          (dolist (b '(27 104))  ; ESC h  (no prefix)
            (cl-tmux::process-byte s b state))
          (is (eq (second (session-windows s)) (session-active-window s))
              "bound -n M-h must run next-window at root")))))

(test prefix-m-j-binding-fires
  "bind M-j next-window makes C-b then Alt+j (ESC j) run next-window — the
   after-prefix meta path."
  (with-isolated-config
    (cl-tmux/config:apply-config-directive '("bind" "M-j" "next-window"))
    (with-fake-session (s :nwindows 2)
        (let ((state (cl-tmux::make-input-state)))
          (dolist (b '(2 27 106))  ; C-b ESC j
            (cl-tmux::process-byte s b state))
          (is (eq (second (session-windows s)) (session-active-window s))
              "bound M-j must run next-window after prefix")))))

(test unbound-root-meta-key-forwards-and-leaves-window
  "With no -n M-x binding, a bare Alt+x is forwarded to the pane and must NOT
   change the active window (the override is purely additive)."
  (with-isolated-config
    (with-fake-session (s :nwindows 2)
        (let ((state (cl-tmux::make-input-state)))
          (dolist (b '(27 120))  ; ESC x  (no prefix, unbound)
            (cl-tmux::process-byte s b state))
          (is (eq (first (session-windows s)) (session-active-window s))
              "unbound bare M-x must leave the first window active")))))

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
    (with-fake-session (s :nwindows 1 :npanes 2)
        (let ((state (cl-tmux::make-input-state)))
          (dolist (b '(2 27 49))  ; C-b ESC 1
            (cl-tmux::process-byte s b state))
          ;; Layout applied: the window still has its two panes and a usable tree.
          (is (= 2 (length (window-panes (session-active-window s))))
              "select-layout via C-b M-1 must preserve both panes")))))

