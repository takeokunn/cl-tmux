(in-package #:cl-tmux/test)

;;;; events tests — part D: application cursor keys, new default bindings,
;;;; dispatch :mark-pane/:display-info/:choose-client, root key-table,
;;;; function/navigation keys, handle-prompt-key printable key.

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

(test key-binding-colon-is-command-prompt
  "C-b : (char code 58) is bound to :command-prompt."
  (is (eq :command-prompt (lookup-key-binding #\:))
      "C-b : must be bound to :command-prompt"))

(test key-binding-t-is-clock-mode
  "C-b t (char code 116) is bound to :clock-mode."
  (is (eq :clock-mode (lookup-key-binding #\t))
      "C-b t must be bound to :clock-mode"))

(test key-binding-i-is-display-info
  "C-b i (char code 105) is bound to :display-info."
  (is (eq :display-info (lookup-key-binding #\i))
      "C-b i must be bound to :display-info"))

(test key-binding-tilde-is-show-messages
  "C-b ~ (code-char 126) is bound to :show-messages."
  (is (eq :show-messages (lookup-key-binding (code-char 126)))
      "C-b ~ must be bound to :show-messages"))

(test key-binding-m-is-mark-pane
  "C-b m (char code 109) is bound to :mark-pane."
  (is (eq :mark-pane (lookup-key-binding #\m))
      "C-b m must be bound to :mark-pane"))

(test key-binding-capital-M-is-clear-mark
  "C-b M (code-char 77) is bound to :clear-mark."
  (is (eq :clear-mark (lookup-key-binding (code-char 77)))
      "C-b M must be bound to :clear-mark"))

(test key-binding-capital-E-is-select-layout-spread
  "C-b E (char code 69) is bound to :select-layout-spread."
  (is (eq :select-layout-spread (lookup-key-binding #\E))
      "C-b E must be bound to :select-layout-spread"))

(test key-binding-space-is-next-layout
  "C-b Space (code-char 32) is bound to :next-layout."
  (is (eq :next-layout (lookup-key-binding (code-char 32)))
      "C-b Space must be bound to :next-layout"))

(test key-binding-dot-is-move-window-prompt
  "C-b . (char code 46) is bound to :move-window-prompt."
  (is (eq :move-window-prompt (lookup-key-binding #\.))
      "C-b . must be bound to :move-window-prompt"))

(test key-binding-capital-D-is-choose-client
  "C-b D (char code 68) is bound to :choose-client."
  (is (eq :choose-client (lookup-key-binding #\D))
      "C-b D must be bound to :choose-client"))

;;; ── dispatch :mark-pane and :clear-mark ─────────────────────────────────────
;;; Build sessions manually (same pattern as dispatch-display-panes tests)
;;; to avoid any interaction with make-fake-session helpers.

(test dispatch-mark-pane-marks-active-pane
  ":mark-pane command sets pane-marked on the active pane."
  (with-minimal-session (p0 win sess)
    (with-loop-state
      (let ((*overlay* nil))
        (is-false (pane-marked p0) "pane must not be marked initially")
        (cl-tmux::dispatch-command sess :mark-pane nil)
        (is (pane-marked p0) "pane must be marked after :mark-pane")))))

(test dispatch-mark-pane-toggle-unmarks
  ":mark-pane on an already-marked pane unmarks it (toggle)."
  (with-minimal-session (p0 win sess)
    (declare (ignore win))
    (with-loop-state
      (let ((*overlay* nil))
        (cl-tmux::dispatch-command sess :mark-pane nil)
        (is (pane-marked p0) "pane marked after first :mark-pane")
        (cl-tmux::dispatch-command sess :mark-pane nil)
        (is-false (pane-marked p0)
            "pane unmarked after :mark-pane on already-marked pane")))))

(test dispatch-clear-mark-unmarks-all-panes
  ":clear-mark clears the server-wide marked pane."
  (with-minimal-session (p0 win sess)
    (declare (ignore win))
    (with-loop-state
      (let ((*overlay* nil))
        (cl-tmux::dispatch-command sess :mark-pane nil)
        (is (pane-marked p0) "pane must be marked before :clear-mark")
        (cl-tmux::dispatch-command sess :clear-mark nil)
        (is-false (pane-marked p0) "pane must not be marked after :clear-mark")))))

;;; ── dispatch :display-info ───────────────────────────────────────────────────

(test dispatch-display-info-shows-overlay
  ":display-info shows a non-empty overlay with session/window/pane info."
  (with-minimal-session (p0 win sess)
    (declare (ignore p0 win))
    (with-loop-state
      (let ((*overlay* nil))
        (cl-tmux::dispatch-command sess :display-info nil)
        (is (overlay-active-p) "display-info must activate the overlay")
        (is (search "Session:" *overlay*)
            "overlay must contain \"Session:\"")))))

;;; ── dispatch :choose-client ──────────────────────────────────────────────────

(test dispatch-choose-client-shows-overlay
  ":choose-client shows an overlay with client info."
  (let ((s (make-fake-session)))
    (with-loop-state
      (let ((*overlay* nil)
            (cl-tmux::*term-rows* 24) (cl-tmux::*term-cols* 80))
        (cl-tmux::dispatch-command s :choose-client nil)
        (is (overlay-active-p) "choose-client must activate the overlay")
        (is (search "Clients" *overlay*)
            "overlay must contain \"Clients\"")))))

;;; ── Root key-table lookup ────────────────────────────────────────────────────

(test root-table-binding-fires-without-prefix
  "A key bound in the root table (bind -n) fires without the C-b prefix."
  (let ((s (make-fake-session :nwindows 2)))
    (with-loop-state
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
            (when tbl (remhash #\Z tbl))))))))

(test root-table-bound-command-line-runs-without-prefix
  "A -n binding to a command LINE runs without the prefix: bind -n Z
   display-message hi, then pressing Z (no C-b) shows 'hi' in an overlay
   (verifies the root dispatch site's token-list path)."
  (with-isolated-config
    (with-loop-state
      (let ((s (make-fake-session)) (*overlay* nil)
            (state (cl-tmux::make-input-state)))
        (cl-tmux/config:apply-config-directive
         '("bind" "-n" "Z" "display-message" "hi"))
        (cl-tmux::process-byte s (char-code #\Z) state)
        (is (overlay-active-p)
            "a -n command-line binding must fire without C-b")
        (let ((text (format nil "~{~A~%~}" (overlay-lines))))
          (is (search "hi" text)
              "overlay must contain the bound command's output 'hi' (got ~S)" text))))))

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
  ;; ESC [ ~ (empty param) → NIL → raw forward
  (is (null (cl-tmux::%csi-tilde-parse
             (make-array 3 :element-type '(unsigned-byte 8)
                           :initial-contents '(27 91 126)) 3))))

(test csi-tilde-key-joins-base-and-modifier
  "%csi-tilde-key combines base key + modifier prefix: F5, C-F5, S-Home."
  (flet ((k (bytes) (cl-tmux::%csi-tilde-key
                     (make-array (length bytes) :element-type '(unsigned-byte 8)
                                                :initial-contents bytes)
                     (length bytes))))
    (is (string= "F5"     (k '(27 91 49 53 126))))         ; ESC [ 15 ~
    (is (string= "C-F5"   (k '(27 91 49 53 59 53 126))))   ; ESC [ 15 ; 5 ~
    (is (string= "S-Home" (k '(27 91 49 59 50 126))))      ; ESC [ 1 ; 2 ~
    (is (null (k '(27 91 50 48 48 126)))                   ; ESC [ 200 ~ (paste)
        "an unmapped parameter yields NIL so it is forwarded raw")))

(test csi-tilde-key-name-maps-known-params
  "%csi-tilde-key-name maps vt parameters to canonical tmux key names."
  (is (string= "Home"     (cl-tmux::%csi-tilde-key-name 1)))
  (is (string= "Delete"   (cl-tmux::%csi-tilde-key-name 3)))
  (is (string= "PageUp"   (cl-tmux::%csi-tilde-key-name 5)))
  (is (string= "PageDown" (cl-tmux::%csi-tilde-key-name 6)))
  (is (string= "F5"       (cl-tmux::%csi-tilde-key-name 15)))
  (is (string= "F12"      (cl-tmux::%csi-tilde-key-name 24)))
  (is (null (cl-tmux::%csi-tilde-key-name 99))
      "an unknown parameter must map to NIL (forwarded raw, not bound)"))

(test normalize-key-alias-collapses-navigation-spellings
  "%normalize-key-alias maps tmux's aliases to the canonical input-side names."
  (is (string= "PageUp"   (cl-tmux/config::%normalize-key-alias "PPage")))
  (is (string= "PageDown" (cl-tmux/config::%normalize-key-alias "NPage")))
  (is (string= "Insert"   (cl-tmux/config::%normalize-key-alias "IC")))
  (is (string= "Delete"   (cl-tmux/config::%normalize-key-alias "DC")))
  (is (string= "PageUp"   (cl-tmux/config::%normalize-key-alias "pgup"))
      "alias matching is case-insensitive")
  (is (null (cl-tmux/config::%normalize-key-alias "F5"))
      "a non-alias token returns NIL so %parse-key-token keeps it verbatim"))

(test parse-key-token-normalizes-aliases-to-canonical
  "%parse-key-token collapses PPage→PageUp so bind-side and input-side keys match."
  (is (string= "PageUp"   (cl-tmux/config::%parse-key-token "PPage")))
  (is (string= "PageDown" (cl-tmux/config::%parse-key-token "NPage")))
  (is (string= "Insert"   (cl-tmux/config::%parse-key-token "IC")))
  (is (string= "F5"       (cl-tmux/config::%parse-key-token "F5"))
      "a canonical/non-alias name passes through unchanged"))

(test function-key-root-binding-fires-from-byte-stream
  "bind -n F5 fires when ESC [ 1 5 ~ is fed through the input state machine."
  (let ((s (make-fake-session :nwindows 2)))
    (with-loop-state
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
            (when tbl (remhash "F5" tbl))))))))

(test page-up-alias-root-binding-fires-from-byte-stream
  "bind -n PPage (alias of PageUp) fires when ESC [ 5 ~ is fed: the alias
   normalisation and the input-side key name meet at the canonical \"PageUp\"."
  (let ((s (make-fake-session :nwindows 2)))
    (with-loop-state
      (let ((state (cl-tmux::make-input-state))
            (key   (cl-tmux/config::%parse-key-token "PPage")))
        (key-table-bind "root" key :next-window)
        (unwind-protect
             (progn
               ;; ESC [ 5 ~  byte by byte.
               (dolist (byte '(27 91 53 126))
                 (cl-tmux::process-byte s byte state))
               (is (eq (second (session-windows s)) (session-active-window s))
                   "ESC [ 5 ~ must resolve to PageUp and fire the PPage binding"))
          (let ((tbl (gethash "root" *key-tables*)))
            (when tbl (remhash key tbl))))))))

(test unbound-function-key-forwards-to-pane-not-bindings
  "An unbound F5 (ESC [ 15 ~) leaves the state machine at ground without firing a
   binding — preserving transparency so the pane application receives the key."
  (let ((s (make-fake-session :nwindows 2)))
    (with-loop-state
      (let ((state (cl-tmux::make-input-state))
            (before (session-active-window s)))
        ;; No binding installed for F5: feeding ESC [ 15 ~ must not switch windows.
        (dolist (byte '(27 91 49 53 126))
          (cl-tmux::process-byte s byte state))
        (is (eq before (session-active-window s))
            "an unbound F5 must not trigger any window command")
        (is (eq #'cl-tmux::%ground-input-state
                (cl-tmux::input-state-continuation state))
            "the state machine must return to ground after an unbound ESC [ 15 ~")))))

;;; ── SS3 function keys: ESC O P/Q/R/S (F1-F4), ESC O H/F (Home/End) ───────────

(test ss3-key-name-maps-f1-through-f4-and-home-end
  "%ss3-key-name maps the SS3 finals to canonical key names; others are NIL."
  (is (string= "F1"   (cl-tmux::%ss3-key-name (char-code #\P))))
  (is (string= "F2"   (cl-tmux::%ss3-key-name (char-code #\Q))))
  (is (string= "F3"   (cl-tmux::%ss3-key-name (char-code #\R))))
  (is (string= "F4"   (cl-tmux::%ss3-key-name (char-code #\S))))
  (is (string= "Home" (cl-tmux::%ss3-key-name (char-code #\H))))
  (is (string= "End"  (cl-tmux::%ss3-key-name (char-code #\F))))
  (is (null (cl-tmux::%ss3-key-name (char-code #\A)))
      "SS3 arrows are out of scope here and must map to NIL (forwarded raw)")
  (is (null (cl-tmux::%ss3-key-name (char-code #\Z)))
      "an unrecognised SS3 final must map to NIL"))

(test ss3-introducer-defers-one-byte-and-tracks-buffer
  "ESC O does not resolve immediately (it could be F1-F4); the decoder keeps
   accumulating and exposes the partial buffer for the escape-time flush replay."
  (let ((s (make-fake-session)))
    (with-loop-state
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
            "the replay buffer must hold the full partial sequence ESC O")))))

(test ss3-f1-root-binding-fires-from-byte-stream
  "bind -n F1 fires when ESC O P is fed through the input state machine, and the
   buffer-replay state is cleared once the sequence completes (back to ground)."
  (let ((s (make-fake-session :nwindows 2)))
    (with-loop-state
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
            (when tbl (remhash "F1" tbl))))))))

(test ss3-unbound-f1-does-not-fire-and-returns-to-ground
  "An unbound F1 (ESC O P) must not trigger a command and must leave the state
   machine at ground — the raw key is forwarded to the pane for transparency."
  (let ((s (make-fake-session :nwindows 2)))
    (with-loop-state
      (let ((cl-tmux::*esc-accum-buffer* nil)
            (state  (cl-tmux::make-input-state))
            (before (session-active-window s)))
        (dolist (byte (list 27 (char-code #\O) (char-code #\P)))
          (cl-tmux::process-byte s byte state))
        (is (eq before (session-active-window s))
            "an unbound F1 must not change the active window")
        (is (eq #'cl-tmux::%ground-input-state
                (cl-tmux::input-state-continuation state))
            "the state machine must return to ground after an unbound ESC O P")))))

;;; ── Prefix-table function keys: C-b then F5 / F1 (bind F5, bind F1) ──────────

(test prefix-function-key-csi-binding-fires
  "bind F5 next-window fires on C-b then ESC [ 15 ~ — the prefix-table path now
   resolves CSI function keys (previously the multi-digit tilde was swallowed)."
  (with-isolated-config
    (cl-tmux/config:apply-config-directive '("bind" "F5" "next-window"))
    (let ((s (make-fake-session :nwindows 2)))
      (with-loop-state
        (let ((cl-tmux::*esc-accum-buffer* nil)
              (state (cl-tmux::make-input-state)))
          ;; C-b (2) then ESC [ 1 5 ~
          (dolist (byte '(2 27 91 49 53 126))
            (cl-tmux::process-byte s byte state))
          (is (eq (second (session-windows s)) (session-active-window s))
              "C-b F5 must run the prefix-table binding")
          (is (eq #'cl-tmux::%ground-input-state
                  (cl-tmux::input-state-continuation state))
              "the state machine must return to ground after C-b F5"))))))

(test prefix-function-key-ss3-binding-fires
  "bind F1 next-window fires on C-b then ESC O P — the prefix-table path now
   resolves the SS3 function-key form too."
  (with-isolated-config
    (cl-tmux/config:apply-config-directive '("bind" "F1" "next-window"))
    (let ((s (make-fake-session :nwindows 2)))
      (with-loop-state
        (let ((cl-tmux::*esc-accum-buffer* nil)
              (state (cl-tmux::make-input-state)))
          ;; C-b (2) then ESC O P
          (dolist (byte (list 2 27 (char-code #\O) (char-code #\P)))
            (cl-tmux::process-byte s byte state))
          (is (eq (second (session-windows s)) (session-active-window s))
              "C-b F1 must run the prefix-table binding"))))))

(test prefix-arrow-binding-still-fires-after-digit-change
  "Regression guard: widening the 3-byte branch to accumulate on any digit final
   must not break the plain arrow path — C-b then ESC [ A still selects up."
  (with-isolated-config
    (cl-tmux/config:apply-config-directive '("bind" "Up" "next-window"))
    (let ((s (make-fake-session :nwindows 2)))
      (with-loop-state
        (let ((cl-tmux::*esc-accum-buffer* nil)
              (state (cl-tmux::make-input-state)))
          ;; C-b (2) then ESC [ A
          (dolist (byte (list 2 27 91 (char-code #\A)))
            (cl-tmux::process-byte s byte state))
          (is (eq (second (session-windows s)) (session-active-window s))
              "C-b Up must still resolve to the prefix-table Up binding"))))))

;;; ── Modified function keys & combined-modifier arrows (root bind -n) ─────────

(test modified-function-key-root-binding-fires-from-byte-stream
  "bind -n C-F5 fires when ESC [ 15 ; 5 ~ (Ctrl+F5) is fed through the machine —
   the modified-function-key form the unmodified path previously dropped."
  (let ((s (make-fake-session :nwindows 2)))
    (with-loop-state
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
            (when tbl (remhash "C-F5" tbl))))))))

(test combined-modifier-arrow-root-binding-fires-from-byte-stream
  "bind -n C-S-Up fires when ESC [ 1 ; 6 A (Ctrl+Shift+Up) is fed — combined
   modifiers now resolve, matching the CSI-u path's handling of letter keys."
  (let ((s (make-fake-session :nwindows 2)))
    (with-loop-state
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
            (when tbl (remhash "C-S-Up" tbl))))))))

;;; ── dispatch :select-layout-spread ─────────────────────────────────────────

(test dispatch-select-layout-spread-applies-even-horizontal
  ":select-layout-spread applies the even-horizontal layout without signaling."
  (let ((s (make-fake-session)))
    (with-loop-state
      (is (null (handler-case
                    (cl-tmux::dispatch-command s :select-layout-spread nil)
                  (error (e) e)))
          ":select-layout-spread must not signal an error"))))

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
    (let ((s (make-fake-session)))
      (with-loop-state
        (let ((state (cl-tmux::make-input-state)))
          (cl-tmux::process-byte s 2 state)
          (is (null (cl-tmux::process-byte s (char-code #\z) state))
              "C-b z must dispatch :zoom-toggle and return NIL"))))))

(test dispatch-select-window-prompt-opens-prompt
  ":select-window-prompt opens a prompt without signaling."
  (let ((s (make-fake-session :nwindows 2)))
    (with-loop-state
      (let ((*prompt* nil))
        (cl-tmux::dispatch-command s :select-window-prompt nil)
        (is (prompt-active-p)
            ":select-window-prompt must open a prompt")))))

;;; ── choose-window uses menu system ──────────────────────────────────────────

(test dispatch-choose-window-shows-menu-overlay
  ":choose-window shows a menu overlay for j/k navigation without a prompt."
  (let ((s (make-fake-session :nwindows 2)))
    (with-loop-state
      (let ((*overlay* nil) (*prompt* nil))
        (cl-tmux::dispatch-command s :choose-window nil)
        (is (overlay-active-p) ":choose-window must show an overlay")
        ;; choose-window now uses j/k menu navigation, not a text prompt.
        (is (not (null cl-tmux/prompt:*active-menu*))
            ":choose-window must set *active-menu* for navigation")))))

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

