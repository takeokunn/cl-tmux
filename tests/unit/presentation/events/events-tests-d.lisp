(in-package #:cl-tmux/test)

;;;; application cursor keys, new default bindings, mark-pane/display-info, root key-table, function/navigation keys — part V

(in-suite events-suite)

;;; ── Application cursor keys — %arrow-final-to-ss3-bytes helper ──────────────

(test arrow-final-to-ss3-bytes-maps-arrows
  "%arrow-final-to-ss3-bytes converts CSI arrow final bytes to SS3 sequences."
  ;; 65=A (up), 66=B (down), 67=C (right), 68=D (left)
  (dolist (final '(65 66 67 68))
    (let ((ss3 (cl-tmux::%arrow-final-to-ss3-bytes final)))
      (is (and ss3
               (= 3 (length ss3))
               (= 27  (aref ss3 0))
               (= 79  (aref ss3 1))
               (= final (aref ss3 2)))
          "SS3 sequence for final byte ~D must be ESC O ~C" final (code-char final)))))

(test arrow-final-to-ss3-bytes-returns-nil-for-non-arrows
  "%arrow-final-to-ss3-bytes returns NIL for non-arrow final bytes."
  (is (null (cl-tmux::%arrow-final-to-ss3-bytes 72))  ; H = home, not arrow
      "Non-arrow byte must return NIL")
  (is (null (cl-tmux::%arrow-final-to-ss3-bytes 109)) ; m = SGR final
      "SGR final byte must return NIL"))

;;; ── New default key bindings ─────────────────────────────────────────────────

(test key-bindings-table
  "Default prefix bindings cover command-prompt, clock, info, messages, and layout/pane management."
  (dolist (c '((58  :command-prompt       "C-b : → :command-prompt")
               (116 :clock-mode           "C-b t → :clock-mode")
               (105 :display-info         "C-b i → :display-info")
               (126 :show-messages        "C-b ~ → :show-messages")
               (109 :mark-pane            "C-b m → :mark-pane")
               (77  :clear-mark           "C-b M → :clear-mark")
               (69  :select-layout-spread "C-b E → :select-layout-spread")
               (32  :next-layout          "C-b Space → :next-layout")
               (46  :move-window-prompt   "C-b . → :move-window-prompt")
               (68  :choose-client        "C-b D → :choose-client")))
    (destructuring-bind (code expected desc) c
      (is (eq expected (lookup-key-binding (code-char code)))
          "~A" desc))))

;;; ── dispatch :mark-pane and :clear-mark ─────────────────────────────────────
;;; Build sessions manually (same pattern as dispatch-display-panes tests)
;;; to avoid any interaction with make-fake-session helpers.

(test dispatch-mark-pane-marks-active-pane
  ":mark-pane command sets pane-marked on the active pane."
  (with-minimal-loop-session (p0 win sess)
    (let ((*overlay* nil))
      (is-false (pane-marked p0) "pane must not be marked initially")
      (cl-tmux::dispatch-command sess :mark-pane nil)
      (is (pane-marked p0) "pane must be marked after :mark-pane"))))

(test dispatch-mark-pane-toggle-unmarks
  ":mark-pane on an already-marked pane unmarks it (toggle)."
  (with-minimal-loop-session (p0 win sess)
    (let ((*overlay* nil))
      (cl-tmux::dispatch-command sess :mark-pane nil)
      (is (pane-marked p0) "pane marked after first :mark-pane")
      (cl-tmux::dispatch-command sess :mark-pane nil)
      (is-false (pane-marked p0)
          "pane unmarked after :mark-pane on already-marked pane"))))

(test dispatch-clear-mark-unmarks-all-panes
  ":clear-mark clears the server-wide marked pane."
  (with-minimal-loop-session (p0 win sess)
    (let ((*overlay* nil))
      (cl-tmux::dispatch-command sess :mark-pane nil)
      (is (pane-marked p0) "pane must be marked before :clear-mark")
      (cl-tmux::dispatch-command sess :clear-mark nil)
      (is-false (pane-marked p0) "pane must not be marked after :clear-mark"))))

;;; ── Root key-table lookup ────────────────────────────────────────────────────

(test root-table-binding-fires-without-prefix
  "A key bound in the root table (bind -n) fires without the C-b prefix."
  (with-fake-session (s :nwindows 2)
    (let ((state (cl-tmux::make-input-state)))
      ;; Bind 'Z' in root table so it selects the next window without C-b.
      (key-table-bind "root" #\Z :next-window)
      (unwind-protect
           (progn
             (cl-tmux::process-byte s (char-code #\Z) state)
             (is (eq (second (session-windows s)) (session-active-window s))
                 "root-table binding must fire without C-b prefix"))
        ;; Clean up: remove the root binding we added.
        (let ((tbl (gethash "root" *key-tables*)))
          (when tbl (remhash #\Z tbl)))))))

(test root-table-bound-command-line-runs-without-prefix
  "A -n binding to a command LINE runs without the prefix: bind -n Z
   display-message hi, then pressing Z (no C-b) shows 'hi' in an overlay
   (verifies the root dispatch site's token-list path)."
  (with-isolated-config
    (with-fake-session (s)
      (let ((*overlay* nil)
            (state (cl-tmux::make-input-state)))
        (cl-tmux/config:apply-config-directive
         '("bind" "-n" "Z" "display-message" "hi"))
        (cl-tmux::process-byte s (char-code #\Z) state)
        (assert-overlay-active "a -n command-line binding must fire without C-b")
        (assert-overlay-contains "hi" *overlay*
                                 "overlay must contain the bound command's output 'hi'")))))


;;; ── Root key-table repeat mode (bind -n -r) ─────────────────────────────

(test root-repeatable-binding-enters-repeat-mode
  "A -r binding in the root table returns :REPEATABLE so process-byte stamps the
   repeat window (parity: tmux sets CLIENT_REPEAT on root, not just prefix)."
  (with-fake-session (s :nwindows 3)
    (let ((state (cl-tmux::make-input-state)))
      (key-table-bind "root" #\Z :next-window :repeatable t)
      (unwind-protect
           (progn
             ;; First press: fires + arms repeat mode.
             (is (eq :repeatable (cl-tmux::process-byte s (char-code #\Z) state))
                 "a -r root binding must return :REPEATABLE on first press")
             (is (not (null (cl-tmux::input-state-repeat-entered-at state)))
                 "repeat-entered-at must be stamped after a -r root binding")
             (is (eq #'cl-tmux::%after-root-repeat-input-state
                     (cl-tmux::input-state-continuation state))
                 "the continuation must be the root-scoped repeat state"))
        (let ((tbl (gethash "root" *key-tables*)))
          (when tbl (remhash #\Z tbl)))))))

(test root-repeatable-binding-repeats-without-prefix
  "After a -r root binding arms repeat mode, the SAME key fires again without any
   prefix (the root-repeat state re-looks-up the byte in the root table)."
  (with-fake-session (s :nwindows 3)
    (let ((state (cl-tmux::make-input-state)))
      (key-table-bind "root" #\Z :next-window :repeatable t)
      (unwind-protect
           (let ((w0 (session-active-window s)))
             (cl-tmux::process-byte s (char-code #\Z) state) ; window 1 -> 2
             (let ((w1 (session-active-window s)))
               (is (not (eq w0 w1)) "first press advances the active window")
               ;; Second press WITHOUT a prefix: repeat state fires it again.
               (is (eq :repeatable (cl-tmux::process-byte s (char-code #\Z) state))
                   "the repeated key must fire (and stay :REPEATABLE) without a prefix")
               (is (not (eq w1 (session-active-window s)))
                   "the repeated key must advance the window a second time")))
        (let ((tbl (gethash "root" *key-tables*)))
          (when tbl (remhash #\Z tbl)))))))

(test root-repeat-mode-broken-by-non-repeatable-key
  "A non-repeatable key pressed during root repeat mode exits repeat mode and is
   reprocessed as a normal ground keystroke (clears repeat-entered-at)."
  (with-fake-session (s :nwindows 3)
    (let ((state (cl-tmux::make-input-state)))
      (key-table-bind "root" #\Z :next-window :repeatable t)
      ;; A second, NON-repeatable root binding: pressing it during repeat mode
      ;; still resolves in the root table but must break the repeat sequence.
      (key-table-bind "root" #\Y :prev-window)
      (unwind-protect
           (progn
             (cl-tmux::process-byte s (char-code #\Z) state) ; arm repeat mode
             (is (not (null (cl-tmux::input-state-repeat-entered-at state)))
                 "precondition: repeat mode armed")
             (is (null (cl-tmux::process-byte s (char-code #\Y) state))
                 "a non-repeatable root key must NOT return :REPEATABLE")
             (is (null (cl-tmux::input-state-repeat-entered-at state))
                 "a non-repeatable key must clear repeat-entered-at")
             (is (eq #'cl-tmux::%ground-input-state
                     (cl-tmux::input-state-continuation state))
                 "the continuation must return to ground after a non-repeatable key"))
        (let ((tbl (gethash "root" *key-tables*)))
          (when tbl (remhash #\Z tbl) (remhash #\Y tbl)))))))

;;; ── Function / navigation keys: ESC [ N ~ → key name → binding ───────────────

(test csi-tilde-parse-reads-param-and-modifier
  "%csi-tilde-parse returns (values PARAM MOD); MOD defaults to 1 and a ';mod'
   field carries the modifier (the modified-function-key form)."
  ;; ESC [ 5 ~  → 5, 1  (unmodified)
  (multiple-value-bind (p m)
      (cl-tmux::%csi-tilde-parse
       (make-array 4 :element-type '(unsigned-byte 8)
                     :initial-contents '(27 91 53 126)) 4)
    (is (= 5 p)) (is (= 1 m)))
  ;; ESC [ 1 5 ~ → 15, 1  (F5)
  (multiple-value-bind (p m)
      (cl-tmux::%csi-tilde-parse
       (make-array 5 :element-type '(unsigned-byte 8)
                     :initial-contents '(27 91 49 53 126)) 5)
    (is (= 15 p)) (is (= 1 m)))
  ;; ESC [ 1 5 ; 5 ~ → 15, 5  (Ctrl+F5)
  (multiple-value-bind (p m)
      (cl-tmux::%csi-tilde-parse
       (make-array 7 :element-type '(unsigned-byte 8)
                     :initial-contents '(27 91 49 53 59 53 126)) 7)
    (is (= 15 p)) (is (= 5 m)))
  ;; ESC [ ~ (empty param) -> NIL -> raw forward
  (is (null (cl-tmux::%csi-tilde-parse
             (make-array 3 :element-type '(unsigned-byte 8)
                           :initial-contents '(27 91 126)) 3))))

(test csi-tilde-key-joins-base-and-modifier
  "%csi-tilde-key combines base key + modifier prefix: F5, C-F5, S-Home."
  (flet ((k (bytes) (cl-tmux::%csi-tilde-key
                     (make-array (length bytes) :element-type '(unsigned-byte 8)
                                                :initial-contents bytes)
                     (length bytes))))
    (dolist (c '(((27 91 49 53 126)       "F5"     "ESC [ 15 ~       -> F5")
                 ((27 91 49 53 59 53 126) "C-F5"   "ESC [ 15 ; 5 ~  -> C-F5")
                 ((27 91 49 59 50 126)    "S-Home" "ESC [ 1 ; 2 ~   -> S-Home")
                 ((27 91 50 48 48 126)    nil       "ESC [ 200 ~ (paste) -> NIL")))
      (destructuring-bind (bytes expected desc) c
        (is (equal expected (k bytes)) "~A" desc)))))

(test csi-tilde-key-name-maps-known-params
  "%csi-tilde-key-name maps vt parameters to canonical tmux key names;
   an unknown parameter maps to NIL (forwarded raw, not bound)."
  (dolist (c '((1  "Home") (3  "Delete") (5  "PageUp")
               (6  "PageDown") (15 "F5") (24 "F12")
               (99 nil)))
    (destructuring-bind (param expected) c
      (is (equal expected (cl-tmux::%csi-tilde-key-name param))
          "param ~D → ~S" param expected))))

(test parse-key-token-keeps-navigation-spellings-literal
  "%parse-key-token is canonical-only: PPage/NPage/IC remain literal key names,
   not input-side aliases for PageUp/PageDown/Insert."
  (dolist (c '(("PPage" "PPage") ("NPage" "NPage")
               ("IC"    "IC")    ("F5"    "F5")))
    (destructuring-bind (input expected) c
      (is (string= expected (cl-tmux/config::%parse-key-token input))
          "~A → ~S" input expected))))

(test function-key-root-binding-fires-from-byte-stream
  "bind -n F5 fires when ESC [ 1 5 ~ is fed through the input state machine."
  (with-fake-session (s :nwindows 2)
    (let ((state (cl-tmux::make-input-state)))
      (key-table-bind "root" "F5" :next-window)
      (unwind-protect
           (progn
             ;; ESC [ 1 5 ~  byte by byte.
             (dolist (byte '(27 91 49 53 126))
               (cl-tmux::process-byte s byte state))
             (is (eq (second (session-windows s)) (session-active-window s))
                 "ESC [ 15 ~ must resolve to F5 and fire its root binding")
             (is (eq #'cl-tmux::%ground-input-state
                     (cl-tmux::input-state-continuation state))
                 "the state machine must return to ground after ESC [ 15 ~"))
        (let ((tbl (gethash "root" *key-tables*)))
          (when tbl (remhash "F5" tbl)))))))

(test page-up-literal-binding-does-not-fire-from-page-up-byte-stream
  "bind -n PPage stores a literal key name.  ESC [ 5 ~ resolves to PageUp, so it
   must not fire a PPage binding."
  (with-fake-session (s :nwindows 2)
    (let ((state (cl-tmux::make-input-state))
          (key   (cl-tmux/config::%parse-key-token "PPage")))
      (key-table-bind "root" key :next-window)
      (unwind-protect
           (progn
             ;; ESC [ 5 ~  byte by byte.
             (dolist (byte '(27 91 53 126))
               (cl-tmux::process-byte s byte state))
             (is (eq (first (session-windows s)) (session-active-window s))
                 "ESC [ 5 ~ must resolve to PageUp and leave the literal PPage binding untouched"))
        (let ((tbl (gethash "root" *key-tables*)))
          (when tbl (remhash key tbl)))))))

(test unbound-function-key-forwards-to-pane-not-bindings
  "An unbound F5 (ESC [ 15 ~) leaves the state machine at ground without firing a
   binding — preserving transparency so the pane application receives the key."
  (with-fake-session (s :nwindows 2)
    (let ((state (cl-tmux::make-input-state))
          (before (session-active-window s)))
      ;; No binding installed for F5: feeding ESC [ 15 ~ must not switch windows.
      (dolist (byte '(27 91 49 53 126))
        (cl-tmux::process-byte s byte state))
      (is (eq before (session-active-window s))
          "an unbound F5 must not trigger any window command")
      (is (eq #'cl-tmux::%ground-input-state
              (cl-tmux::input-state-continuation state))
          "the state machine must return to ground after an unbound ESC [ 15 ~"))))

;;; ── SS3 function keys: ESC O P/Q/R/S (F1-F4), ESC O H/F (Home/End) ───────────

(test ss3-key-name-maps-f1-through-f4-and-home-end
  "%ss3-key-name maps the SS3 finals to canonical key names; SS3 arrows and
   unrecognised finals map to NIL (forwarded raw)."
  (dolist (c '((#\P "F1") (#\Q "F2") (#\R "F3") (#\S "F4")
               (#\H "Home") (#\F "End")
               (#\A nil) (#\Z nil)))
    (destructuring-bind (ch expected) c
      (is (equal expected (cl-tmux::%ss3-key-name (char-code ch)))
          "SS3 final ~C → ~S" ch expected))))

(test ss3-introducer-defers-one-byte-and-tracks-buffer
  "ESC O does not resolve immediately (it could be F1-F4); the decoder keeps
   accumulating and exposes the partial buffer for the escape-time flush replay."
  (with-fake-session (s)
    (let ((cl-tmux::*esc-accum-buffer* nil)
          (state (cl-tmux::make-input-state)))
      (cl-tmux::process-byte s 27 state)              ; ESC
      (cl-tmux::process-byte s (char-code #\O) state) ; O
      (is (not (eq #'cl-tmux::%ground-input-state
                   (cl-tmux::input-state-continuation state)))
          "ESC O must keep accumulating, not resolve as Alt+O at length 2")
      (is (and cl-tmux::*esc-accum-buffer*
               (equalp (coerce (subseq cl-tmux::*esc-accum-buffer* 0
                                       (fill-pointer cl-tmux::*esc-accum-buffer*))
                               'list)
                       '(27 79)))
          "the replay buffer must hold the full partial sequence ESC O"))))

(test ss3-f1-root-binding-fires-from-byte-stream
  "bind -n F1 fires when ESC O P is fed through the input state machine, and the
   buffer-replay state is cleared once the sequence completes (back to ground)."
  (with-fake-session (s :nwindows 2)
    (let ((cl-tmux::*esc-accum-buffer* nil)
          (state (cl-tmux::make-input-state)))
      (key-table-bind "root" "F1" :next-window)
      (unwind-protect
           (progn
             (dolist (byte (list 27 (char-code #\O) (char-code #\P)))
               (cl-tmux::process-byte s byte state))
             (is (eq (second (session-windows s)) (session-active-window s))
                 "ESC O P must resolve to F1 and fire its root binding")
             (is (eq #'cl-tmux::%ground-input-state
                     (cl-tmux::input-state-continuation state))
                 "the state machine must return to ground after ESC O P")
             (is (null cl-tmux::*esc-accum-buffer*)
                 "the replay buffer must be cleared once back at ground"))
        (let ((tbl (gethash "root" *key-tables*)))
          (when tbl (remhash "F1" tbl)))))))

(test ss3-unbound-f1-does-not-fire-and-returns-to-ground
  "An unbound F1 (ESC O P) must not trigger a command and must leave the state
   machine at ground — the raw key is forwarded to the pane for transparency."
  (with-fake-session (s :nwindows 2)
    (let ((cl-tmux::*esc-accum-buffer* nil)
          (state  (cl-tmux::make-input-state))
          (before (session-active-window s)))
      (dolist (byte (list 27 (char-code #\O) (char-code #\P)))
        (cl-tmux::process-byte s byte state))
      (is (eq before (session-active-window s))
          "an unbound F1 must not change the active window")
      (is (eq #'cl-tmux::%ground-input-state
              (cl-tmux::input-state-continuation state))
          "the state machine must return to ground after an unbound ESC O P"))))

;;; ── Prefix-table function keys: C-b then F5 / F1 (bind F5, bind F1) ──────────

(test prefix-function-key-csi-binding-fires
  "bind F5 next-window fires on C-b then ESC [ 15 ~ — the prefix-table path now
   resolves CSI function keys (previously the multi-digit tilde was swallowed)."
  (with-isolated-config
    (cl-tmux/config:apply-config-directive '("bind" "F5" "next-window"))
    (with-fake-session (s :nwindows 2)
      (let ((cl-tmux::*esc-accum-buffer* nil)
            (state (cl-tmux::make-input-state)))
        ;; C-b (2) then ESC [ 1 5 ~
        (dolist (byte '(2 27 91 49 53 126))
          (cl-tmux::process-byte s byte state))
        (is (eq (second (session-windows s)) (session-active-window s))
            "C-b F5 must run the prefix-table binding")
        (is (eq #'cl-tmux::%ground-input-state
                (cl-tmux::input-state-continuation state))
            "the state machine must return to ground after C-b F5")))))

(test prefix-function-key-ss3-binding-fires
  "bind F1 next-window fires on C-b then ESC O P — the prefix-table path now
   resolves the SS3 function-key form too."
  (with-isolated-config
    (cl-tmux/config:apply-config-directive '("bind" "F1" "next-window"))
    (with-fake-session (s :nwindows 2)
      (let ((cl-tmux::*esc-accum-buffer* nil)
            (state (cl-tmux::make-input-state)))
        ;; C-b (2) then ESC O P
        (dolist (byte (list 2 27 (char-code #\O) (char-code #\P)))
          (cl-tmux::process-byte s byte state))
        (is (eq (second (session-windows s)) (session-active-window s))
            "C-b F1 must run the prefix-table binding")))))

(test prefix-arrow-binding-still-fires-after-digit-change
  "Regression guard: widening the 3-byte branch to accumulate on any digit final
   must not break the plain arrow path — C-b then ESC [ A still selects up."
  (with-isolated-config
    (cl-tmux/config:apply-config-directive '("bind" "Up" "next-window"))
    (with-fake-session (s :nwindows 2)
      (let ((cl-tmux::*esc-accum-buffer* nil)
            (state (cl-tmux::make-input-state)))
        ;; C-b (2) then ESC [ A
        (dolist (byte (list 2 27 91 (char-code #\A)))
          (cl-tmux::process-byte s byte state))
        (is (eq (second (session-windows s)) (session-active-window s))
            "C-b Up must still resolve to the prefix-table Up binding")))))

;;; ── Modified function keys & combined-modifier arrows (root bind -n) ─────────

(test modified-function-key-root-binding-fires-from-byte-stream
  "bind -n C-F5 fires when ESC [ 15 ; 5 ~ (Ctrl+F5) is fed through the machine —
   the modified-function-key form the unmodified path previously dropped."
  (with-fake-session (s :nwindows 2)
    (let ((cl-tmux::*esc-accum-buffer* nil)
          (state (cl-tmux::make-input-state)))
      (key-table-bind "root" "C-F5" :next-window)
      (unwind-protect
           (progn
             ;; ESC [ 1 5 ; 5 ~
             (dolist (byte '(27 91 49 53 59 53 126))
               (cl-tmux::process-byte s byte state))
             (is (eq (second (session-windows s)) (session-active-window s))
                 "ESC [ 15 ; 5 ~ must resolve to C-F5 and fire its binding"))
        (let ((tbl (gethash "root" *key-tables*)))
          (when tbl (remhash "C-F5" tbl)))))))

(test combined-modifier-arrow-root-binding-fires-from-byte-stream
  "bind -n C-S-Up fires when ESC [ 1 ; 6 A (Ctrl+Shift+Up) is fed — combined
   modifiers now resolve, matching the CSI-u path's handling of letter keys."
  (with-fake-session (s :nwindows 2)
    (let ((cl-tmux::*esc-accum-buffer* nil)
          (state (cl-tmux::make-input-state)))
      (key-table-bind "root" "C-S-Up" :next-window)
      (unwind-protect
           (progn
             ;; ESC [ 1 ; 6 A
             (dolist (byte (list 27 91 49 59 54 (char-code #\A)))
               (cl-tmux::process-byte s byte state))
             (is (eq (second (session-windows s)) (session-active-window s))
                 "ESC [ 1 ; 6 A must resolve to C-S-Up and fire its binding"))
        (let ((tbl (gethash "root" *key-tables*)))
          (when tbl (remhash "C-S-Up" tbl)))))))
