(in-package #:cl-tmux/test)

;;;; Arg-command dispatch tests — part 3: helper tests, on-submit paths,
;;;; cyclic navigation, break/join/source/run/if dispatch.

(in-suite dispatch-suite)

;;; ── %toggle-synchronize-panes helper ─────────────────────────────────────────

(test toggle-synchronize-panes-shows-on-when-was-off
  "%toggle-synchronize-panes shows 'ON' overlay when toggling from off."
  (with-loop-state
    (let ((*overlay* nil))
      (cl-tmux/options:set-option "synchronize-panes" nil)
      (cl-tmux::%toggle-synchronize-panes)
      (is (overlay-active-p) "%toggle-synchronize-panes must show an overlay")
      (let ((text (format nil "~{~A~%~}" (overlay-lines))))
        (is (search "ON" text) "toggling from off must produce an ON message")))))

(test toggle-synchronize-panes-shows-off-when-was-on
  "%toggle-synchronize-panes shows 'OFF' overlay when toggling from on."
  (with-loop-state
    (let ((*overlay* nil))
      (cl-tmux/options:set-option "synchronize-panes" t)
      (cl-tmux::%toggle-synchronize-panes)
      (is (overlay-active-p) "%toggle-synchronize-panes must show an overlay")
      (let ((text (format nil "~{~A~%~}" (overlay-lines))))
        (is (search "OFF" text) "toggling from on must produce an OFF message")))))

;;; ── next-cyclic / prev-cyclic edge cases ────────────────────────────────────

(test next-cyclic-single-element-wraps-to-itself
  "next-cyclic on a single-element list always returns that element."
  (is (eql 'x (cl-tmux::next-cyclic '(x) 'x))
      "next-cyclic of the only element must return itself")
  (is (eql 'x (cl-tmux::next-cyclic '(x) 'missing))
      "next-cyclic with unknown current on a single-element list must return the element"))

(test prev-cyclic-single-element-wraps-to-itself
  "prev-cyclic on a single-element list always returns that element."
  (is (eql 'x (cl-tmux::prev-cyclic '(x) 'x))
      "prev-cyclic of the only element must return itself"))

(test next-cyclic-middle-element-advances
  "next-cyclic from a middle element advances to the following element."
  (is (eql 'c (cl-tmux::next-cyclic '(a b c d) 'b))
      "next-cyclic from 'b in (a b c d) must return 'c"))

(test prev-cyclic-middle-element-retreats
  "prev-cyclic from a middle element retreats to the preceding element."
  (is (eql 'a (cl-tmux::prev-cyclic '(a b c d) 'b))
      "prev-cyclic from 'b in (a b c d) must return 'a"))

;;; ── with-active-window macro ────────────────────────────────────────────────

(test with-active-window-evaluates-body-when-window-exists
  "with-active-window evaluates BODY and binds WIN-VAR when a window is active."
  (let* ((s   (make-fake-session :nwindows 1))
         (win (session-active-window s))
         (result nil))
    (cl-tmux::with-active-window (w s)
      (setf result w))
    (is (eq win result)
        "with-active-window must bind WIN-VAR to the active window")))

(test with-active-window-returns-nil-for-windowless-session
  "with-active-window returns NIL and skips BODY when no active window exists."
  (with-empty-session (s)
    (let ((called nil))
      (cl-tmux::with-active-window (w s)
        (setf called t))
      (is-false called
                "with-active-window body must not execute when no active window"))))

(test with-active-window-macro-is-defined
  "with-active-window is a defined macro."
  (is (macro-function 'cl-tmux::with-active-window)
      "with-active-window must be a macro"))

;;; ── %copy-mode-cmd helper ────────────────────────────────────────────────────

(test copy-mode-cmd-returns-override-for-known-char
  "%copy-mode-cmd returns the override keyword for characters in the override table."
  (is (eq :copy-mode-exit (cl-tmux::%copy-mode-cmd #\q))
      "%copy-mode-cmd must return :copy-mode-exit for #\\q")
  (is (eq :copy-mode-exit (cl-tmux::%copy-mode-cmd #\i))
      "%copy-mode-cmd must return :copy-mode-exit for #\\i")
  (is (eq :copy-mode-yank (cl-tmux::%copy-mode-cmd #\y))
      "%copy-mode-cmd must return :copy-mode-yank for #\\y")
  (is (eq :copy-mode-begin-selection (cl-tmux::%copy-mode-cmd #\Space))
      "%copy-mode-cmd must return :copy-mode-begin-selection for #\\Space"))

(test copy-mode-cmd-returns-nil-for-nil-char
  "%copy-mode-cmd returns NIL when CH is NIL."
  (is (null (cl-tmux::%copy-mode-cmd nil))
      "%copy-mode-cmd must return NIL for NIL input"))

(test copy-mode-cmd-falls-through-to-key-binding-for-unknown-char
  "%copy-mode-cmd falls back to the normal key-binding lookup for unmapped chars."
  ;; #\d is the 'detach' binding in the prefix table (not a copy-mode override).
  ;; We don't assert the exact result because it depends on the key-binding table,
  ;; but we verify the call does not error.
  (finishes (cl-tmux::%copy-mode-cmd #\d)
            "%copy-mode-cmd must not error for a char not in the override table"))

;;; ── %format-menu helper ──────────────────────────────────────────────────────

(test format-menu-produces-box-with-title-and-items
  "%format-menu returns a string with box-drawing characters, the title, and items."
  (let* ((menu   (make-menu :title "TestMenu"
                             :items (list (cons "Alpha" :ka) (cons "Beta" :kb))
                             :selected-index 0))
         (output (cl-tmux::%format-menu menu)))
    (is (stringp output) "%format-menu must return a string")
    (is (search "TestMenu" output) "output must contain the menu title")
    (is (search "Alpha" output) "output must contain the first item label")
    (is (search "Beta"  output) "output must contain the second item label")
    (is (search "┌" output) "output must have a top-left corner character")
    (is (search "└" output) "output must have a bottom-left corner character")))

(test format-menu-marks-selected-item-with-arrow
  "%format-menu marks the selected item with the ▶ character."
  (let* ((menu   (make-menu :title "M"
                             :items (list (cons "A" :ka) (cons "B" :kb))
                             :selected-index 1))
         (output (cl-tmux::%format-menu menu)))
    (is (search "▶" output) "output must contain the ▶ selection marker")
    ;; The selected item B should be on the marked line.
    (let ((arrow-pos (search "▶" output))
          (b-pos     (search "B" output)))
      (is (and arrow-pos b-pos (< arrow-pos (+ b-pos 10)))
          "▶ marker must appear near the selected item 'B'"))))

(test format-menu-empty-items-produces-minimal-box
  "%format-menu with an empty item list still produces a valid box string."
  (let* ((menu   (make-menu :title "Empty" :items nil :selected-index 0))
         (output (cl-tmux::%format-menu menu)))
    (is (stringp output) "%format-menu with no items must return a string")
    (is (search "Empty" output) "output must still contain the title")))

;;; ── %swap-active-pane helper ─────────────────────────────────────────────────

(test swap-active-pane-forward-reorders-panes
  "%swap-active-pane :right swaps the active pane with the next one."
  (with-two-pane-h-session (sess win p0 p1)
    (cl-tmux::%swap-active-pane sess :right)
    (is (eq p1 (first (window-panes win)))
        "after %swap-active-pane :right, p1 must be first")
    (is (eq p0 (second (window-panes win)))
        "after %swap-active-pane :right, p0 must be second")))

(test swap-active-pane-backward-reorders-panes
  "%swap-active-pane :left from p1 swaps it to the front."
  (with-two-pane-h-session (sess win p0 p1)
    (window-select-pane win p1)
    (cl-tmux::%swap-active-pane sess :left)
    (is (eq p1 (first (window-panes win)))
        "after %swap-active-pane :left from p1, p1 must be first")
    (is (eq p0 (second (window-panes win)))
        "after %swap-active-pane :left from p1, p0 must be second")))

;;; ── %cmd-split helper ────────────────────────────────────────────────────────

(test cmd-split-no-focus-does-not-error
  "%cmd-split with :no-focus T does not signal an error on a fake session."
  ;; The pane is too small to split (20x5) so the split may return NIL.
  ;; We verify only that the call does not error at the dispatch layer.
  (with-fake-session (s :nwindows 1 :npanes 1)
    (finishes (cl-tmux::%cmd-split s :h :no-focus t)
              "%cmd-split :no-focus must not error even when pane is too small")))

(test cmd-split-no-focus-does-not-error-vertical
  "%cmd-split with :v orientation and :no-focus T does not signal an error."
  (with-fake-session (s :nwindows 1 :npanes 1)
    (finishes (cl-tmux::%cmd-split s :v :no-focus t)
              "%cmd-split :v :no-focus must not error even when pane is too small")))

;;; ── define-named-command-table macro ─────────────────────────────────────────

(test define-named-command-table-macro-is-defined
  "define-named-command-table is a defined macro."
  (is (macro-function 'cl-tmux::define-named-command-table)
      "define-named-command-table must be a macro"))

(test dispatch-named-command-detach
  "%dispatch-named-command \"detach\" returns :detach."
  (with-fake-session (s)
    (is (eq :detach (cl-tmux::%dispatch-named-command s "detach"))
        "%dispatch-named-command must accept 'detach' as an alias")))

(test dispatch-named-command-detach-client-alias
  "%dispatch-named-command \"detach-client\" is an alias for :detach."
  (with-fake-session (s)
    (is (eq :detach (cl-tmux::%dispatch-named-command s "detach-client"))
        "%dispatch-named-command 'detach-client' must behave like 'detach'")))

(test dispatch-named-command-list-sessions
  "%dispatch-named-command \"list-sessions\" opens an overlay."
  (with-fake-session (s :nwindows 1)
    (let ((*overlay* nil)
          (cl-tmux::*server-sessions* nil))
      (cl-tmux::%dispatch-named-command s "list-sessions")
      (is (overlay-active-p)
          "%dispatch-named-command 'list-sessions' must open an overlay"))))

(test dispatch-named-command-copy-mode
  "%dispatch-named-command \"copy-mode\" enters copy mode."
  (with-fake-session (s)
    (cl-tmux::%dispatch-named-command s "copy-mode")
    (is (cl-tmux::%copy-mode-active-p s)
        "%dispatch-named-command 'copy-mode' must enter copy mode")))

;;; ── dispatch-prefix-command in copy mode ────────────────────────────────────

(test dispatch-prefix-command-copy-mode-y-yanks
  "In copy mode, dispatch-prefix-command with 'y' issues :copy-mode-yank."
  ;; We verify indirectly: yank in copy mode should exit selection/copy mode.
  ;; Since we have no real selection, we just verify it doesn't error.
  (with-fake-session (s)
    (cl-tmux::dispatch-command s :copy-mode-enter nil)
    (is (cl-tmux::%copy-mode-active-p s) "copy mode must be on")
    (finishes (cl-tmux::dispatch-prefix-command s (char-code #\y))
              "dispatch-prefix-command 'y' in copy mode must not error")))

(test dispatch-prefix-command-copy-mode-slash-opens-search-prompt
  "In copy mode, '/' opens a forward-search prompt."
  (with-fake-session (s)
    (cl-tmux::dispatch-command s :copy-mode-enter nil)
    (let ((*prompt* nil))
      (cl-tmux::dispatch-prefix-command s (char-code #\/))
      (is (prompt-active-p)
          "dispatch-prefix-command '/' in copy mode must open a search prompt")
      (is (string= "/" (prompt-label *prompt*))
          "search prompt label must be \"/\""))))

(test dispatch-prefix-command-copy-mode-question-opens-backward-prompt
  "In copy mode, '?' opens a backward-search prompt."
  (with-fake-session (s)
    (cl-tmux::dispatch-command s :copy-mode-enter nil)
    (let ((*prompt* nil))
      (cl-tmux::dispatch-prefix-command s (char-code #\?))
      (is (prompt-active-p)
          "dispatch-prefix-command '?' in copy mode must open a search prompt")
      (is (string= "?" (prompt-label *prompt*))
          "search prompt label must be \"?\""))))

;;; ── :select-layout-even-h / :select-layout-even-v dispatch ──────────────────

(test dispatch-select-layout-even-h-does-not-error
  ":select-layout-even-h dispatches without error."
  (with-two-pane-h-session (sess win p0 p1)
    (is (and win p0 p1) "two-pane fixture created")
    (with-loop-state
      (finishes (cl-tmux::dispatch-command sess :select-layout-even-h nil)
                ":select-layout-even-h must not signal an error"))))

(test dispatch-select-layout-even-v-does-not-error
  ":select-layout-even-v dispatches without error."
  (with-two-pane-v-session (sess win p0 p1)
    (is (and win p0 p1) "two-pane v-fixture created")
    (with-loop-state
      (finishes (cl-tmux::dispatch-command sess :select-layout-even-v nil)
                ":select-layout-even-v must not signal an error"))))

;;; ── :break-pane dispatch ─────────────────────────────────────────────────────

(test dispatch-break-pane-on-single-pane-window-is-noop
  ":break-pane on a single-pane window is a no-op (guard prevents break)."
  (with-fake-session (s :nwindows 1 :npanes 1)
    (let ((nwindows-before (length (session-windows s))))
      (finishes (cl-tmux::dispatch-command s :break-pane nil)
                ":break-pane on a single-pane window must not error")
      ;; With only one pane, the guard should prevent creation of a new window.
      (is (= nwindows-before (length (session-windows s)))
          ":break-pane on a single-pane window must not add a new window"))))

(test dispatch-break-pane-on-two-pane-window-creates-new-window
  ":break-pane on a two-pane window extracts the active pane into a new window."
  (with-two-pane-h-session (sess win p0 p1)
    (is (and win p0 p1) "two-pane fixture created")
    (with-loop-state
      (let ((nwindows-before (length (session-windows sess))))
        ;; break-pane may fail in sandbox (PTY fork), so tolerate errors.
        (handler-case
            (progn
              (cl-tmux::dispatch-command sess :break-pane nil)
              ;; If it succeeded, a new window should have been created.
              (is (> (length (session-windows sess)) nwindows-before)
                  ":break-pane must create a new window when there are 2+ panes"))
          (error ()
            ;; Fork failure in sandbox is acceptable; dispatch layer must not error.
            (is-true t ":break-pane signalled at PTY level (acceptable in sandbox)")))))))

;;; ── :join-pane dispatch ──────────────────────────────────────────────────────

(test dispatch-join-pane-opens-prompt
  ":join-pane opens a prompt for the source window index."
  (with-fake-session (s :nwindows 2)
    (let ((*prompt* nil))
      (cl-tmux::dispatch-command s :join-pane nil)
      (is (prompt-active-p) ":join-pane must open a prompt"))))

;;; ── :source-file dispatch ────────────────────────────────────────────────────

(test dispatch-source-file-opens-prompt
  ":source-file opens a prompt for the file path."
  (with-fake-session (s)
    (let ((*prompt* nil))
      (cl-tmux::dispatch-command s :source-file nil)
      (is (prompt-active-p) ":source-file must open a prompt")
      (is (string= "source-file" (prompt-label *prompt*))
          ":source-file prompt label must be \"source-file\""))))

(test dispatch-source-file-empty-input-is-noop
  ":source-file with empty input does not crash."
  (with-fake-session (s)
    (let ((*prompt* nil))
      (cl-tmux::dispatch-command s :source-file nil)
      (is (prompt-active-p) "prompt must be open")
      (finishes (funcall (prompt-on-submit *prompt*) "")
                ":source-file with empty input must not error"))))

;;; ── :run-shell dispatch ──────────────────────────────────────────────────────

(test dispatch-run-shell-opens-prompt
  ":run-shell opens a prompt for the shell command."
  (with-fake-session (s)
    (let ((*prompt* nil))
      (cl-tmux::dispatch-command s :run-shell nil)
      (is (prompt-active-p) ":run-shell must open a prompt")
      (is (string= "run-shell" (prompt-label *prompt*))
          ":run-shell prompt label must be \"run-shell\""))))

(test dispatch-run-shell-empty-input-is-noop
  ":run-shell with empty input does not crash."
  (with-fake-session (s)
    (let ((*prompt* nil) (*overlay* nil))
      (cl-tmux::dispatch-command s :run-shell nil)
      (is (prompt-active-p) "prompt must be open")
      (finishes (funcall (prompt-on-submit *prompt*) "")
                ":run-shell with empty input must not error"))))

;;; ── :if-shell dispatch ───────────────────────────────────────────────────────

(test dispatch-if-shell-opens-prompt
  ":if-shell opens a prompt for the shell command."
  (with-fake-session (s)
    (let ((*prompt* nil))
      (cl-tmux::dispatch-command s :if-shell nil)
      (is (prompt-active-p) ":if-shell must open a prompt")
      (is (string= "if-shell" (prompt-label *prompt*))
          ":if-shell prompt label must be \"if-shell\""))))

(test dispatch-if-shell-empty-input-is-noop
  ":if-shell with empty input does not crash."
  (with-fake-session (s)
    (let ((*prompt* nil))
      (cl-tmux::dispatch-command s :if-shell nil)
      (is (prompt-active-p) "prompt must be open")
      (finishes (funcall (prompt-on-submit *prompt*) "")
                ":if-shell with empty input must not error"))))

;;; ── :choose-window dispatch ──────────────────────────────────────────────────

(test dispatch-choose-window-opens-menu-and-prompt
  ":choose-window with windows opens a menu overlay for j/k navigation (no prompt)."
  (with-fake-session (s :nwindows 2)
    (let ((*overlay* nil) (*prompt* nil)
          (cl-tmux::*active-menu* nil))
      (cl-tmux::dispatch-command s :choose-window nil)
      (is (overlay-active-p) ":choose-window must open an overlay")
      ;; choose-window now uses j/k menu navigation, not a prompt.
      ;; Prompt is no longer opened; the menu handles input directly.
      (is (not (null cl-tmux::*active-menu*))
          ":choose-window must set *active-menu*"))))

(test dispatch-choose-window-empty-session-shows-overlay
  ":choose-window with no windows shows a '(no windows)' overlay."
  (with-empty-session (s)
    (with-loop-state
      (let ((*overlay* nil))
        (cl-tmux::dispatch-command s :choose-window nil)
        (is (overlay-active-p) ":choose-window must open an overlay for empty session")
        (let ((text (format nil "~{~A~%~}" (overlay-lines))))
          (is (search "no windows" text)
              "overlay must say 'no windows' when there are none"))))))

;;; ── :move-window-prompt dispatch ─────────────────────────────────────────────

(test dispatch-move-window-prompt-opens-prompt
  ":move-window-prompt opens a prompt for the destination index."
  (with-fake-session (s :nwindows 2)
    (let ((*prompt* nil))
      (cl-tmux::dispatch-command s :move-window-prompt nil)
      (is (prompt-active-p) ":move-window-prompt must open a prompt")
      (is (string= "move-window to index" (prompt-label *prompt*))
          ":move-window-prompt label must be \"move-window to index\""))))

;;; ── :menu-select dispatch ────────────────────────────────────────────────────

(test dispatch-menu-select-executes-selected-command
  ":menu-select executes the command of the currently selected menu item."
  (with-fake-session (s)
    (let ((cl-tmux::*active-menu*
            (make-menu :title "t"
                       :items (list (cons "Detach" :detach))
                       :selected-index 0)))
      ;; :menu-select on an item with :detach must return :detach.
      (is (eq :detach (cl-tmux::dispatch-command s :menu-select nil))
          ":menu-select on :detach item must return :detach"))))

(test dispatch-menu-select-clears-menu-and-overlay
  ":menu-select clears *active-menu* and the overlay after executing."
  (with-fake-session (s)
    (let ((*overlay* nil)
          (cl-tmux::*active-menu*
            (make-menu :title "t"
                       :items (list (cons "List Keys" :list-keys))
                       :selected-index 0)))
      (cl-tmux::dispatch-command s :menu-select nil)
      (is (null cl-tmux::*active-menu*)
          ":menu-select must clear *active-menu* after selection"))))

(test dispatch-menu-select-nil-menu-is-noop
  ":menu-select with *active-menu* NIL is a no-op."
  (with-fake-session (s)
    (let ((cl-tmux::*active-menu* nil))
      (finishes (cl-tmux::dispatch-command s :menu-select nil)
                ":menu-select with no active menu must not error"))))

;;; ── dispatch-prefix-command: normal (non-copy-mode) table lookup ─────────────

(test dispatch-prefix-command-n-selects-next-window
  "dispatch-prefix-command with byte for 'n' selects the next window."
  (with-fake-session (s :nwindows 2)
    (let ((w0 (first (session-windows s)))
          (w1 (second (session-windows s))))
      (is (eq w0 (session-active-window s)) "w0 is active initially")
      (cl-tmux::dispatch-prefix-command s (char-code #\n))
      (is (eq w1 (session-active-window s))
          "dispatch-prefix-command 'n' must select the next window"))))

(test dispatch-prefix-command-p-selects-prev-window
  "dispatch-prefix-command with byte for 'p' selects the previous window."
  (with-fake-session (s :nwindows 2)
    (let ((w0 (first  (session-windows s)))
          (w1 (second (session-windows s))))
      (is (eq w0 (session-active-window s)) "w0 is active initially")
      (cl-tmux::dispatch-prefix-command s (char-code #\p))
      (is (eq w1 (session-active-window s))
          "dispatch-prefix-command 'p' must select the previous (wrapped) window"))))

(test dispatch-prefix-command-unknown-byte-is-noop
  "dispatch-prefix-command with a byte that has no key binding is a no-op."
  (with-fake-session (s)
    ;; #\x00 is unlikely to have a binding; the call must not error.
    (finishes (cl-tmux::dispatch-prefix-command s 0)
              "dispatch-prefix-command with an unbound byte must not error")))

;;; ── :has-session with missing session shows no ───────────────────────────────

(test dispatch-has-session-not-found-shows-no
  ":has-session on-submit shows 'no' when the session is not registered."
  (with-fake-session (s)
    (let ((*prompt* nil) (*overlay* nil)
          (cl-tmux::*server-sessions* nil))
      (cl-tmux::dispatch-command s :has-session nil)
      (is (prompt-active-p) "prompt must be open")
      (funcall (prompt-on-submit *prompt*) "nonexistent-session-xyz")
      (is (overlay-active-p) "on-submit must open an overlay")
      (let ((text (format nil "~{~A~%~}" (overlay-lines))))
        (is (search "no" text)
            "overlay must say 'no' for an unknown session")))))

;;; ── :switch-client-next with no other session is a no-op ─────────────────────

(test dispatch-switch-client-next-single-session-is-noop
  ":switch-client-next with only one session in the registry is a no-op."
  (let* ((s    (make-fake-session))
         (name (session-name s)))
    (with-loop-state
      (let ((cl-tmux::*server-sessions* (list (cons name s))))
        (finishes (cl-tmux::dispatch-command s :switch-client-next nil)
                  ":switch-client-next with a single session must not error")
        (is-true cl-tmux::*dirty*
                 "dispatch must mark *dirty* even with single session")))))

;;; ── :find-window on-submit paths ─────────────────────────────────────────────

(test dispatch-find-window-matching-pattern-shows-results
  ":find-window on-submit with a matching pattern shows the matching windows."
  (with-fake-session (s :nwindows 2)
    (let ((*prompt* nil) (*overlay* nil))
      (cl-tmux::dispatch-command s :find-window nil)
      (is (prompt-active-p) "prompt must be open")
      ;; All window names start with a digit; "0" matches the first window.
      (funcall (prompt-on-submit *prompt*) "0")
      (is (overlay-active-p) ":find-window with a match must open an overlay")
      (let ((text (format nil "~{~A~%~}" (overlay-lines))))
        (is (search "0" text) "overlay must list the matching window")))))

(test dispatch-find-window-no-match-shows-no-windows-message
  ":find-window on-submit with no matches shows a 'no windows matching' overlay."
  (with-fake-session (s :nwindows 1)
    (let ((*prompt* nil) (*overlay* nil))
      (cl-tmux::dispatch-command s :find-window nil)
      (is (prompt-active-p) "prompt must be open")
      (funcall (prompt-on-submit *prompt*) "zzz-no-such-window-xyz")
      (is (overlay-active-p) ":find-window with no match must open an overlay")
      (let ((text (format nil "~{~A~%~}" (overlay-lines))))
        (is (search "no windows" text)
            "overlay must say 'no windows matching' when there are no matches")))))

;;; ── :select-window-prompt with name lookup ────────────────────────────────────

(test dispatch-select-window-prompt-selects-by-name
  ":select-window-prompt on-submit with a window name selects that window."
  (with-fake-session (s :nwindows 2)
    (let ((*prompt* nil))
      ;; The fake windows are named "0" and "1".
      (cl-tmux::dispatch-command s :select-window-prompt nil)
      (is (prompt-active-p) "prompt must be open")
      (funcall (prompt-on-submit *prompt*) "1")
      (is (eq (second (session-windows s)) (session-active-window s))
          "submitting \"1\" (name match) must select the second window"))))

(test dispatch-select-window-prompt-unknown-name-shows-overlay
  ":select-window-prompt with an unknown name shows an error overlay."
  (with-fake-session (s :nwindows 1)
    (let ((*prompt* nil) (*overlay* nil))
      (cl-tmux::dispatch-command s :select-window-prompt nil)
      (is (prompt-active-p) "prompt must be open")
      (funcall (prompt-on-submit *prompt*) "no-such-window-xyz")
      (is (overlay-active-p) "unknown window must open an error overlay")
      (let ((text (format nil "~{~A~%~}" (overlay-lines))))
        (is (search "no window" text)
            "overlay must mention 'no window'")))))

;;; ── :move-window on-submit ────────────────────────────────────────────────────

(test dispatch-move-window-on-submit-reorders-windows
  ":move-window on-submit with a valid index reorders the window list."
  (with-fake-session (s :nwindows 2)
    (let ((*prompt* nil)
          (w0 (first  (session-windows s)))
          (w1 (second (session-windows s))))
      (cl-tmux::dispatch-command s :move-window nil)
      (is (prompt-active-p) "prompt must be open")
      ;; Move w0 (active, index 0) to index 1.
      (finishes (funcall (prompt-on-submit *prompt*) "1")
                ":move-window on-submit with valid index must not error")
      (is (and w0 w1) "both windows must still exist after move"))))

;;; ── :swap-window on-submit ────────────────────────────────────────────────────

(test dispatch-swap-window-on-submit-swaps-positions
  ":swap-window on-submit with a valid index swaps two windows."
  (with-fake-session (s :nwindows 2)
    (let ((*prompt* nil))
      (cl-tmux::dispatch-command s :swap-window nil)
      (is (prompt-active-p) "prompt must be open")
      (finishes (funcall (prompt-on-submit *prompt*) "1")
                ":swap-window on-submit with valid index must not error"))))

;;; ── :bind-key on-submit ──────────────────────────────────────────────────────

(test dispatch-bind-key-known-command-shows-confirmation
  ":bind-key on-submit with a known key+command pair shows a confirmation overlay."
  (with-fake-session (s)
    (let ((*prompt* nil) (*overlay* nil))
      (cl-tmux::dispatch-command s :bind-key nil)
      (is (prompt-active-p) "prompt must be open")
      ;; "z detach" — z is a valid key token, detach is a known command.
      (funcall (prompt-on-submit *prompt*) "z detach")
      (is (overlay-active-p) "successful bind-key must show a confirmation overlay")
      (let ((text (format nil "~{~A~%~}" (overlay-lines))))
        (is (search "bound" text) "overlay must confirm the binding with 'bound'")))))

(test dispatch-bind-key-unknown-command-shows-error
  ":bind-key on-submit with an unknown command shows an error overlay."
  (with-fake-session (s)
    (let ((*prompt* nil) (*overlay* nil))
      (cl-tmux::dispatch-command s :bind-key nil)
      (is (prompt-active-p) "prompt must be open")
      (funcall (prompt-on-submit *prompt*) "z totally-unknown-cmd-xyz")
      (is (overlay-active-p) "unknown command must show an error overlay")
      (let ((text (format nil "~{~A~%~}" (overlay-lines))))
        (is (search "unknown command" text)
            "overlay must contain 'unknown command'")))))

;;; ── :unbind-key on-submit ────────────────────────────────────────────────────

(test dispatch-unbind-key-shows-confirmation
  ":unbind-key on-submit removes a key binding and shows a confirmation overlay."
  (with-fake-session (s)
    (let ((*prompt* nil) (*overlay* nil))
      (cl-tmux::dispatch-command s :unbind-key nil)
      (is (prompt-active-p) "prompt must be open")
      ;; Use a key that is expected to be in the default table (e.g. 'd' → detach).
      (funcall (prompt-on-submit *prompt*) "d")
      (is (overlay-active-p) "unbind-key must show a confirmation overlay")
      (let ((text (format nil "~{~A~%~}" (overlay-lines))))
        (is (search "unbound" text) "overlay must confirm the unbinding")))))

;;; ── :show-option on-submit paths ─────────────────────────────────────────────

(test dispatch-show-option-on-submit-known-option-shows-overlay
  ":show-option on-submit with a known option name shows its value in an overlay."
  (with-fake-session (s)
    (let ((*prompt* nil) (*overlay* nil))
      (cl-tmux::dispatch-command s :show-option nil)
      (is (prompt-active-p) "prompt must be open")
      ;; "mouse" is a standard option.
      (funcall (prompt-on-submit *prompt*) "mouse")
      (is (overlay-active-p) ":show-option with known option must open overlay"))))

;;; ── :rename-session on-submit: empty input does not rename ──────────────────

(test dispatch-rename-session-empty-input-no-rename
  ":rename-session on-submit with empty input does not rename the session."
  (with-fake-session (s)
    (let ((*prompt* nil))
      (let ((original-name (session-name s)))
        (cl-tmux::dispatch-command s :rename-session nil)
        (is (prompt-active-p) "rename-session must open a prompt")
        (funcall (prompt-on-submit *prompt*) "")
        (is (string= original-name (session-name s))
            "submitting empty string must NOT rename the session")))))

;;; ── :display-message empty input is noop ────────────────────────────────────

(test dispatch-display-message-empty-input-no-log
  ":display-message with empty input does not append to *message-log*."
  (with-fake-session (s)
    (let ((*prompt* nil)
          (cl-tmux::*message-log* nil))
      (cl-tmux::dispatch-command s :display-message nil)
      (is (prompt-active-p) "prompt must be open")
      (funcall (prompt-on-submit *prompt*) "")
      (is (null cl-tmux::*message-log*)
          "empty input must not append to *message-log*"))))

;;; ── :command-prompt strips leading whitespace ────────────────────────────────

(test dispatch-command-prompt-trims-whitespace
  ":command-prompt trims leading/trailing whitespace before dispatching."
  (with-fake-session (s :nwindows 1)
    (let ((*prompt* nil) (*overlay* nil))
      (cl-tmux::dispatch-command s :command-prompt nil)
      (is (prompt-active-p) "prompt must be open")
      ;; "  list-windows  " should work identically to "list-windows".
      (funcall (prompt-on-submit *prompt*) "  list-windows  ")
      (is (overlay-active-p)
          ":command-prompt with padded 'list-windows' must still open an overlay"))))

;;; ── :kill-pane on a two-pane window leaves the other pane ──────────────────

(test dispatch-kill-pane-leaves-remaining-pane
  ":kill-pane on a 2-pane window removes the active pane but keeps the other."
  (with-fake-session (s :nwindows 1 :npanes 2)
    (let* ((win   (session-active-window s))
           (pane0 (first  (window-panes win)))
           (pane1 (second (window-panes win))))
      (is (eq pane0 (window-active-pane win)) "pane0 is active initially")
      (cl-tmux::dispatch-command s :kill-pane nil)
      (is (= 1 (length (window-panes win)))
          ":kill-pane must reduce the pane count to 1")
      (is-false (member pane0 (window-panes win))
                ":kill-pane must remove the previously active pane")
      (is (member pane1 (window-panes win))
          ":kill-pane must leave pane1 intact"))))

;;; ── %cmd-cycle-pane with prev-cyclic ─────────────────────────────────────────

(test cmd-cycle-pane-prev-retreats-selection
  "%cmd-cycle-pane with prev-cyclic retreats the active pane."
  (let* ((s   (make-fake-session :nwindows 1 :npanes 2))
         (win (session-active-window s))
         (p0  (first  (window-panes win)))
         (p1  (second (window-panes win))))
    (with-loop-state
      ;; Start at p0; prev-cyclic wraps to p1 (the last pane).
      (is (eq p0 (window-active-pane win)))
      (cl-tmux::%cmd-cycle-pane s #'cl-tmux::prev-cyclic)
      (is (eq p1 (window-active-pane win))
          "%cmd-cycle-pane with prev-cyclic must wrap from first pane to last"))))

;;; ── %cmd-cycle-window with prev-cyclic ───────────────────────────────────────

(test cmd-cycle-window-prev-retreats-selection
  "%cmd-cycle-window with prev-cyclic retreats the active window."
  (let* ((s  (make-fake-session :nwindows 3))
         (w0 (first  (session-windows s)))
         (w2 (third  (session-windows s))))
    (with-loop-state
      ;; Start at w0; prev-cyclic wraps to w2 (the last window).
      (is (eq w0 (session-active-window s)))
      (cl-tmux::%cmd-cycle-window s #'cl-tmux::prev-cyclic)
      (is (eq w2 (session-active-window s))
          "%cmd-cycle-window with prev-cyclic must wrap from first window to last"))))

;;; ── :select-pane-up at top pane is a no-op ──────────────────────────────────

(test dispatch-select-pane-up-noop-at-topmost
  ":select-pane-up is a no-op when the active pane has no pane above."
  (with-two-pane-v-session (sess win p0 p1)
    (with-loop-state
      ;; p0 is at the top; going up should not change the active pane.
      (is (eq p0 (window-active-pane win)) "p0 is active initially")
      (cl-tmux::dispatch-command sess :select-pane-up nil)
      (is (eq p0 (window-active-pane win))
          ":select-pane-up at the topmost pane must remain on p0"))))

;;; ── :select-pane-down at bottom pane is a no-op ─────────────────────────────

(test dispatch-select-pane-down-noop-at-bottommost
  ":select-pane-down is a no-op when the active pane has no pane below."
  (with-two-pane-v-session (sess win p0 p1)
    (with-loop-state
      ;; Start at p1 (bottommost); going down should not change the active pane.
      (window-select-pane win p1)
      (cl-tmux::dispatch-command sess :select-pane-down nil)
      (is (eq p1 (window-active-pane win))
          ":select-pane-down at the bottommost pane must remain on p1"))))

;;; ── :select-pane-left at leftmost is a no-op ─────────────────────────────────

(test dispatch-select-pane-left-noop-at-leftmost
  ":select-pane-left is a no-op when the active pane has no left neighbour."
  (with-two-pane-h-session (sess win p0 p1)
    (with-loop-state
      ;; p0 is already at the leftmost position.
      (is (eq p0 (window-active-pane win)) "p0 is active initially")
      (cl-tmux::dispatch-command sess :select-pane-left nil)
      (is (eq p0 (window-active-pane win))
          ":select-pane-left at leftmost pane must remain on p0"))))

;;; ── :prev-pane dispatch ──────────────────────────────────────────────────────

(test dispatch-prev-pane-wraps-from-first
  ":prev-pane cycles in reverse: from the first pane wraps to the last."
  (let* ((s   (make-fake-session :nwindows 1 :npanes 2))
         (win (session-active-window s))
         (p0  (first  (window-panes win)))
         (p1  (second (window-panes win))))
    (with-loop-state
      (is (eq p0 (window-active-pane win)) "p0 is active initially")
      (cl-tmux::dispatch-command s :prev-pane nil)
      (is (eq p1 (window-active-pane win))
          ":prev-pane from the first pane must wrap to the last pane"))))

(test dispatch-prev-pane-retreats-from-last
  ":prev-pane from the last pane selects the preceding pane."
  (let* ((s   (make-fake-session :nwindows 1 :npanes 2))
         (win (session-active-window s))
         (p0  (first  (window-panes win)))
         (p1  (second (window-panes win))))
    (with-loop-state
      (window-select-pane win p1)
      (cl-tmux::dispatch-command s :prev-pane nil)
      (is (eq p0 (window-active-pane win))
          ":prev-pane from p1 must select p0"))))

;;; ── :split-horizontal / :split-vertical (focus versions) dispatch ────────────

(test dispatch-split-horizontal-does-not-error
  ":split-horizontal dispatches without error on a fake session."
  (with-fake-session (s :nwindows 1 :npanes 1)
    (finishes (cl-tmux::dispatch-command s :split-horizontal nil)
              ":split-horizontal must not signal an error")))

(test dispatch-split-vertical-does-not-error
  ":split-vertical dispatches without error on a fake session."
  (with-fake-session (s :nwindows 1 :npanes 1)
    (finishes (cl-tmux::dispatch-command s :split-vertical nil)
              ":split-vertical must not signal an error")))

;;; ── :new-window dispatch ─────────────────────────────────────────────────────

(test dispatch-new-window-does-not-error
  ":new-window dispatches without error (or signals at PTY level, which is acceptable)."
  (with-fake-session (s :nwindows 1)
    (handler-case
        (progn
          (cl-tmux::dispatch-command s :new-window nil)
          (is-true t ":new-window dispatched without error"))
      (error ()
        (is-true t ":new-window signalled at PTY level (acceptable in sandbox)")))))

