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

(test copy-mode-pageup-is-bound-in-copy-mode-tables
  "PageUp is installed as :copy-mode-page-up in both copy-mode tables."
  (with-isolated-config
    (is (eq :copy-mode-page-up (key-table-command-value "copy-mode" "PageUp"))
        "copy-mode PageUp must be registered as :copy-mode-page-up")
    (is (eq :copy-mode-page-up (key-table-command-value "copy-mode-vi" "PageUp"))
        "copy-mode-vi PageUp must be registered as :copy-mode-page-up")))

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
  "C-b C-b dispatches :send-prefix and writes one literal prefix byte."
  (with-isolated-config
    (with-fake-session (s)
      (let* ((pane (window-active-pane (session-active-window s)))
             (state (cl-tmux::make-input-state))
             (writes nil)
             (orig (fdefinition 'cl-tmux/pty:pty-write)))
        (setf (pane-fd pane) 9999)
        (unwind-protect
             (progn
               (setf (fdefinition 'cl-tmux/pty:pty-write)
                     (lambda (fd bytes)
                       (push (list fd (coerce bytes 'list)) writes)))
               (cl-tmux::process-byte s 2 state)  ; prefix
               (is (null (cl-tmux::process-byte s 2 state))
                   "C-b C-b must dispatch :send-prefix and return NIL")
               (is (equal '((9999 (2))) (reverse writes))
                   "send-prefix must write exactly one literal prefix byte"))
          (setf (fdefinition 'cl-tmux/pty:pty-write) orig))))))

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

(test unbound-prefix-shift-arrow-does-not-forward
  "Unbound C-b S-Up is consumed by the prefix table and must not leak to the pane."
  (with-isolated-config
    (with-fake-session (s)
      (let* ((pane (window-active-pane (session-active-window s)))
             (state (cl-tmux::make-input-state))
             (writes nil)
             (orig (fdefinition 'cl-tmux/pty:pty-write)))
        (setf (pane-fd pane) 9999)
        (unwind-protect
             (progn
               (setf (fdefinition 'cl-tmux/pty:pty-write)
                     (lambda (fd bytes)
                       (push (list fd (coerce bytes 'list)) writes)))
               (dolist (b '(2 27 91 49 59 50 65)) ; C-b, ESC [ 1 ; 2 A
                 (cl-tmux::process-byte s b state))
               (is (null writes)
                   "unbound prefixed S-Up must not be forwarded to the pane"))
          (setf (fdefinition 'cl-tmux/pty:pty-write) orig))))))

(test default-prefix-arrow-selects-neighbour-pane
  "Default C-b Right/Left are real prefix-table bindings and select neighbour panes."
  (with-isolated-config
    (with-loop-state
      (let* ((win (%vsplit-window 20))
             (left (first (window-panes win)))
             (right (second (window-panes win)))
             (s (make-session :id 1 :name "s" :windows (list win)))
             (state (cl-tmux::make-input-state)))
        (session-select-window s win)
        (window-select-pane win left)
        (dolist (b '(2 27 91 67))
          (cl-tmux::process-byte s b state))
        (is (eq right (window-active-pane win))
            "C-b Right must select the right pane")
        (dolist (b '(2 27 91 68))
          (cl-tmux::process-byte s b state))
        (is (eq left (window-active-pane win))
            "C-b Left must select the left pane")))))

(test default-prefix-control-arrow-resizes-and-repeats
  "Default C-b C-Right resizes by one cell and keeps repeat mode for the next C-Right."
  (with-isolated-config
    (with-loop-state
      (let* ((win (%vsplit-window 20))
             (left (first (window-panes win)))
             (right (second (window-panes win)))
             (s (make-session :id 1 :name "s" :windows (list win)))
             (state (cl-tmux::make-input-state)))
        (session-select-window s win)
        (window-select-pane win left)
        (dolist (b '(2 27 91 49 59 53 67))
          (cl-tmux::process-byte s b state))
        (is (= 21 (pane-width left))
            "C-b C-Right must grow the active pane by one cell")
        (is (= 19 (pane-width right))
            "C-b C-Right must shrink the right pane by one cell")
        (dolist (b '(27 91 49 59 53 67))
          (cl-tmux::process-byte s b state))
        (is (= 22 (pane-width left))
            "repeat C-Right without another prefix must grow again")
        (is (= 18 (pane-width right))
            "repeat C-Right without another prefix must shrink again")))))

(test default-prefix-meta-arrow-resizes-by-five
  "Default C-b M-Right resizes by five cells through the prefix-table binding."
  (with-isolated-config
    (with-loop-state
      (let* ((win (%vsplit-window 20))
             (left (first (window-panes win)))
             (right (second (window-panes win)))
             (s (make-session :id 1 :name "s" :windows (list win)))
             (state (cl-tmux::make-input-state)))
        (session-select-window s win)
        (window-select-pane win left)
        (dolist (b '(2 27 91 49 59 51 67))
          (cl-tmux::process-byte s b state))
        (is (= 25 (pane-width left))
            "C-b M-Right must grow the active pane by five cells")
        (is (= 15 (pane-width right))
            "C-b M-Right must shrink the right pane by five cells")))))

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
        (let* ((pane (window-active-pane (session-active-window s)))
               (state (cl-tmux::make-input-state))
               (writes nil)
               (orig (fdefinition 'cl-tmux/pty:pty-write)))
          (setf (pane-fd pane) 9999)
          (unwind-protect
               (progn
                 (setf (fdefinition 'cl-tmux/pty:pty-write)
                       (lambda (fd bytes)
                         (push (list fd (coerce bytes 'list)) writes)))
          (dolist (b '(27 120))  ; ESC x  (no prefix, unbound)
            (cl-tmux::process-byte s b state))
          (is (eq (first (session-windows s)) (session-active-window s))
              "unbound bare M-x must leave the first window active")
          (is (equal '((9999 (27 120))) (reverse writes))
              "unbound bare M-x must be forwarded to the active pane"))
            (setf (fdefinition 'cl-tmux/pty:pty-write) orig))))))

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

(test cmd-switch-client-accepts-client-targeting-and-control-flags
  "switch-client accepts the tmux client-targeting/control flags -c/-E/-Z
   (standalone single-client semantics) and still performs the -t switch."
  (with-loop-state
    (with-empty-registry
      (multiple-value-bind (s0 s1 s2) (%make-three-session-registry)
        (declare (ignore s0))
        (dolist (args '(("-c" "client-0" "-t" "2")
                        ("-E" "-t" "2")
                        ("-Z" "-t" "2")
                        ("-F" "#{session_name}" "-t" "2")))
          (%assert-switch-client-selection s1 args s2
            (format nil "~S must be accepted and switch to session 2" args)))))))

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
