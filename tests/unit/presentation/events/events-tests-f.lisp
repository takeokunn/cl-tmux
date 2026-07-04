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
