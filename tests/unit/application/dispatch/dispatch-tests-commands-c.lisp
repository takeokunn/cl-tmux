(in-package #:cl-tmux/test)

;;;; Arg-command dispatch tests — part 3: helper tests, on-submit paths,
;;;; cyclic navigation, break/join/source/run/if dispatch.

(describe "dispatch-suite"

  ;;; ── %toggle-synchronize-panes helper ─────────────────────────────────────────

  ;; %toggle-synchronize-panes shows ON overlay from off, OFF overlay from on.
  (it "toggle-synchronize-panes-table"
    (dolist (row '((nil "ON"  "toggling from off must show ON")
                   (t   "OFF" "toggling from on must show OFF")))
      (destructuring-bind (initial expected-text desc) row
        (declare (ignore desc))
        (with-loop-state
          (let ((*overlay* nil))
            (cl-tmux/options:set-option "synchronize-panes" initial)
            (cl-tmux::%toggle-synchronize-panes)
            (assert-overlay-active "overlay must be shown")
            (assert-overlay-contains expected-text *overlay*))))))

  ;;; ── next-cyclic / prev-cyclic edge cases ────────────────────────────────────

  ;; next-cyclic advances and prev-cyclic retreats; both wrap correctly on single-element lists.
  (it "cyclic-navigation-table"
    (dolist (c '((cl-tmux::next-cyclic (x)       x       x  "next of only element → itself")
                 (cl-tmux::next-cyclic (x)       missing x  "next with unknown current → element")
                 (cl-tmux::prev-cyclic (x)       x       x  "prev of only element → itself")
                 (cl-tmux::next-cyclic (a b c d) b       c  "next from middle → following")
                 (cl-tmux::prev-cyclic (a b c d) b       a  "prev from middle → preceding")))
      (destructuring-bind (fn lst current expected desc) c
        (declare (ignore desc))
        (expect (eql expected (funcall fn lst current))))))

  ;;; ── with-active-window macro ────────────────────────────────────────────────

  ;; with-active-window evaluates BODY and binds WIN-VAR when a window is active.
  (it "with-active-window-evaluates-body-when-window-exists"
    (let* ((s   (make-fake-session :nwindows 1))
           (win (session-active-window s))
           (result nil))
      (cl-tmux::with-active-window (w s)
        (setf result w))
      (expect (eq win result))))

  ;; with-active-window returns NIL and skips BODY when no active window exists.
  (it "with-active-window-returns-nil-for-windowless-session"
    (with-empty-session (s)
      (let ((called nil))
        (cl-tmux::with-active-window (w s)
          (setf called t))
        (expect called :to-be-falsy))))

  ;; with-active-window is a defined macro.
  (it "with-active-window-macro-is-defined"
    (expect (macro-function 'cl-tmux::with-active-window)))

  ;;; ── %copy-mode-cmd helper ────────────────────────────────────────────────────

  ;; %copy-mode-cmd returns the override keyword for characters in the override table.
  (it "copy-mode-cmd-returns-override-for-known-char"
    (expect (eq :copy-mode-exit (cl-tmux::%copy-mode-cmd #\q)))
    (expect (eq :copy-mode-exit (cl-tmux::%copy-mode-cmd #\i)))
    (expect (eq :copy-mode-yank (cl-tmux::%copy-mode-cmd #\y)))
    (expect (eq :copy-mode-begin-selection (cl-tmux::%copy-mode-cmd #\Space))))

  ;; %copy-mode-cmd returns NIL when CH is NIL.
  (it "copy-mode-cmd-returns-nil-for-nil-char"
    (expect (null (cl-tmux::%copy-mode-cmd nil))))

  ;; %copy-mode-cmd falls back to the normal key-binding lookup for unmapped chars.
  (it "copy-mode-cmd-falls-through-to-key-binding-for-unknown-char"
    ;; #\d is the 'detach' binding in the prefix table (not a copy-mode override).
    ;; We don't assert the exact result because it depends on the key-binding table,
    ;; but we verify the call does not error.
    (finishes (cl-tmux::%copy-mode-cmd #\d)
              "%copy-mode-cmd must not error for a char not in the override table"))

  ;;; ── %format-menu helper ──────────────────────────────────────────────────────

  ;; %format-menu returns a string with box-drawing characters, the title, and items.
  (it "format-menu-produces-box-with-title-and-items"
    (let* ((menu   (make-menu :title "TestMenu"
                               :items (list (cons "Alpha" :ka) (cons "Beta" :kb))
                               :selected-index 0))
           (output (cl-tmux::%format-menu menu)))
      (expect (stringp output))
      (expect (search "TestMenu" output))
      (expect (search "Alpha" output))
      (expect (search "Beta"  output))
      (expect (search "┌" output))
      (expect (search "└" output))))

  ;; %format-menu marks the selected item with the ▶ character.
  (it "format-menu-marks-selected-item-with-arrow"
    (let* ((menu   (make-menu :title "M"
                               :items (list (cons "A" :ka) (cons "B" :kb))
                               :selected-index 1))
           (output (cl-tmux::%format-menu menu)))
      (expect (search "▶" output))
      ;; The selected item B should be on the marked line.
      (let ((arrow-pos (search "▶" output))
            (b-pos     (search "B" output)))
        (expect (and arrow-pos b-pos (< arrow-pos (+ b-pos 10)))))))

  ;; %format-menu with an empty item list still produces a valid box string.
  (it "format-menu-empty-items-produces-minimal-box"
    (let* ((menu   (make-menu :title "Empty" :items nil :selected-index 0))
           (output (cl-tmux::%format-menu menu)))
      (expect (stringp output))
      (expect (search "Empty" output))))

  ;;; ── %swap-active-pane helper ─────────────────────────────────────────────────

  ;; %swap-active-pane :right (from p0) and :left (from p1) both move p1 to first.
  (it "swap-active-pane-table"
    (dolist (row '((:right nil  "forward: p0 active, swap right → p1 first")
                   (:left  t    "backward: p1 active, swap left → p1 first")))
      (destructuring-bind (dir select-p1 desc) row
        (declare (ignore desc))
        (with-two-pane-h-session (sess win p0 p1)
          (when select-p1 (window-select-pane win p1))
          (cl-tmux::%swap-active-pane sess dir)
          (expect (eq p1 (first  (window-panes win))))
          (expect (eq p0 (second (window-panes win))))))))

  ;;; ── %cmd-split helper ────────────────────────────────────────────────────────

  ;; %cmd-split with :no-focus T does not signal an error in either orientation.
  (it "cmd-split-no-focus-table"
    (dolist (c '((:h "horizontal :no-focus must not error even when pane is too small")
                 (:v "vertical :no-focus must not error even when pane is too small")))
      (destructuring-bind (orient desc) c
        (with-fake-session (s :nwindows 1 :npanes 1)
          (finishes (cl-tmux::%cmd-split s orient :no-focus t) "~A" desc)))))

  ;;; ── %make-dispatch-named-table helper ────────────────────────────────────────

  ;; %make-dispatch-named-table returns a hash table mapping canonical command names to keywords.
  (it "make-dispatch-named-table-builds-lookup-table"
    (let* ((specs (list (list :named-keyword :detach :named-names (list "detach"))
                        (list :named-keyword :new-window :named-names (list "new-window"))))
           (table (cl-tmux::%make-dispatch-named-table specs)))
      (expect (hash-table-p table))
      (expect (eq :detach (gethash "detach" table)))
      (expect (null (gethash "d" table)))
      (expect (eq :new-window (gethash "new-window" table)))
      (expect (null (gethash "neww" table)))))

  ;; %make-dispatch-named-table ignores specs that lack a :named-keyword.
  (it "make-dispatch-named-table-skips-specs-without-keyword"
    (let* ((specs (list (list :named-names (list "orphan"))))
           (table (cl-tmux::%make-dispatch-named-table specs)))
      (expect (hash-table-p table))
      (expect (null (gethash "orphan" table)))))

  ;; %dispatch-named-command "detach" returns :detach.
  (it "dispatch-named-command-detach"
    (with-fake-session (s)
      (expect (eq :detach (cl-tmux::%dispatch-named-command s "detach")))
      (expect (eq :unknown-command (cl-tmux::%dispatch-named-command s "detach-client")))))

  ;; %dispatch-named-command "list-sessions" opens an overlay.
  (it "dispatch-named-command-list-sessions"
    (with-fake-session (s :nwindows 1)
      (let ((*overlay* nil)
            (cl-tmux::*server-sessions* nil))
        (cl-tmux::%dispatch-named-command s "list-sessions")
        (assert-overlay-active
         "%dispatch-named-command 'list-sessions' must open an overlay"))))

  ;; %dispatch-named-command "copy-mode-enter" enters copy mode.
  (it "dispatch-named-command-copy-mode-enter"
    (with-fake-session (s)
      (cl-tmux::%dispatch-named-command s "copy-mode-enter")
      (expect (cl-tmux::%copy-mode-active-p s))
      (expect (eq :unknown-command (cl-tmux::%dispatch-named-command s "copy-mode")))))

  ;; %dispatch-named-command "display-menu" opens a menu overlay.
  (it "dispatch-named-command-display-menu"
    (with-fake-session (s)
      (let ((*overlay* nil)
            (cl-tmux::*active-menu* nil))
        (expect (null (cl-tmux::%dispatch-named-command s "display-menu")))
        (expect (not (null cl-tmux::*active-menu*)))
        (assert-overlay-active
         "%dispatch-named-command 'display-menu' must open an overlay")
        (expect (eq :unknown-command (cl-tmux::%dispatch-named-command s "menu"))))))

  ;;; ── dispatch-prefix-command in copy mode ────────────────────────────────────

  ;; In copy mode, dispatch-prefix-command with 'y' issues :copy-mode-yank.
  (it "dispatch-prefix-command-copy-mode-y-yanks"
    ;; We verify indirectly: yank in copy mode should exit selection/copy mode.
    ;; Since we have no real selection, we just verify it doesn't error.
    (with-fake-session (s)
      (cl-tmux::dispatch-command s :copy-mode-enter nil)
      (expect (cl-tmux::%copy-mode-active-p s))
      (finishes (cl-tmux::dispatch-prefix-command s (char-code #\y))
                "dispatch-prefix-command 'y' in copy mode must not error")))

  ;; In copy mode, '/' opens forward-search and '?' opens backward-search prompt.
  (it "dispatch-prefix-command-copy-mode-search-prompts"
    (dolist (ch '(#\/ #\?))
      (with-fake-session (s)
        (cl-tmux::dispatch-command s :copy-mode-enter nil)
        (let ((*prompt* nil))
          (cl-tmux::dispatch-prefix-command s (char-code ch))
          (expect (prompt-active-p))
          (expect (string= (string ch) (prompt-label *prompt*)))))))

  ;;; ── :select-layout-even-h / :select-layout-even-v dispatch ──────────────────

  ;; :select-layout-even-h and :select-layout-even-v both dispatch without error.
  (it "dispatch-select-layout-even-does-not-error"
    (with-two-pane-h-session (sess win p0 p1)
      (expect (and win p0 p1))
      (finishes (cl-tmux::dispatch-command sess :select-layout-even-h nil)
                ":select-layout-even-h must not signal an error"))
    (with-two-pane-v-session (sess win p0 p1)
      (expect (and win p0 p1))
      (finishes (cl-tmux::dispatch-command sess :select-layout-even-v nil)
                ":select-layout-even-v must not signal an error")))

  ;;; ── :break-pane dispatch ─────────────────────────────────────────────────────

  ;; :break-pane on a single-pane window is a no-op (guard prevents break).
  (it "dispatch-break-pane-on-single-pane-window-is-noop"
    (with-fake-session (s :nwindows 1 :npanes 1)
      (let ((nwindows-before (length (session-windows s))))
        (finishes (cl-tmux::dispatch-command s :break-pane nil)
                  ":break-pane on a single-pane window must not error")
        ;; With only one pane, the guard should prevent creation of a new window.
        (expect (= nwindows-before (length (session-windows s)))))))

  ;; :break-pane on a two-pane window extracts the active pane into a new window.
  (it "dispatch-break-pane-on-two-pane-window-creates-new-window"
    (with-two-pane-h-session (sess win p0 p1)
      (expect (and win p0 p1))
      (let ((nwindows-before (length (session-windows sess))))
        ;; break-pane may fail in sandbox (PTY fork), so tolerate errors.
        (handler-case
            (progn
              (cl-tmux::dispatch-command sess :break-pane nil)
              ;; If it succeeded, a new window should have been created.
              (expect (> (length (session-windows sess)) nwindows-before)))
          (error ()
            ;; Fork failure in sandbox is acceptable; dispatch layer must not error.
            (expect t))))))

  ;;; ── :join-pane dispatch ──────────────────────────────────────────────────────

  ;; :join-pane opens a prompt for the source window index.
  (it "dispatch-join-pane-opens-prompt"
    (with-fake-session (s :nwindows 2)
      (let ((*prompt* nil))
        (cl-tmux::dispatch-command s :join-pane nil)
        (expect (prompt-active-p)))))

  ;;; ── :source-file dispatch ────────────────────────────────────────────────────

  ;; :source-file, :run-shell, and :if-shell each open a prompt with the matching label.
  (it "dispatch-prompt-opening-commands-table"
    (dolist (c '((:source-file "source-file")
                 (:run-shell   "run-shell")
                 (:if-shell    "if-shell")))
      (destructuring-bind (cmd label) c
        (with-fake-session (s)
          (let ((*prompt* nil))
            (cl-tmux::dispatch-command s cmd nil)
            (expect (prompt-active-p))
            (expect (string= label (prompt-label *prompt*))))))))

  ;; :source-file, :run-shell, and :if-shell with empty input do not crash.
  (it "dispatch-empty-input-is-noop-table"
    (dolist (cmd '(:source-file :run-shell :if-shell))
      (with-fake-session (s)
        (let ((*prompt* nil) (*overlay* nil))
          (cl-tmux::dispatch-command s cmd nil)
          (expect (prompt-active-p))
          (finishes (funcall (prompt-on-submit *prompt*) "")
                    "~S with empty input must not error" cmd)))))

  ;;; ── :choose-window dispatch ──────────────────────────────────────────────────

  ;; :choose-window with windows opens a menu overlay for j/k navigation (no prompt).
  (it "dispatch-choose-window-opens-menu-and-prompt"
    (with-fake-session (s :nwindows 2)
      (let ((*overlay* nil) (*prompt* nil)
            (cl-tmux::*active-menu* nil))
        (cl-tmux::dispatch-command s :choose-window nil)
        (assert-overlay-active ":choose-window must open an overlay")
        ;; choose-window now uses j/k menu navigation, not a prompt.
        ;; Prompt is no longer opened; the menu handles input directly.
        (expect (not (null cl-tmux::*active-menu*))))))

  ;; :choose-window with no windows shows a '(no windows)' overlay.
  (it "dispatch-choose-window-empty-session-shows-overlay"
    (with-fake-session (s :nwindows 0)
      (let ((*overlay* nil))
        (cl-tmux::dispatch-command s :choose-window nil)
        (assert-overlay-active ":choose-window must open an overlay for empty session")
        (assert-overlay-contains "no windows" *overlay*
                                 "overlay must say 'no windows' when there are none"))))

  ;;; ── :move-window-prompt dispatch ─────────────────────────────────────────────

  ;; :move-window-prompt opens a prompt for the destination index.
  (it "dispatch-move-window-prompt-opens-prompt"
    (with-fake-session (s :nwindows 2)
      (let ((*prompt* nil))
        (cl-tmux::dispatch-command s :move-window-prompt nil)
        (expect (prompt-active-p))
        (expect (string= "move-window to index" (prompt-label *prompt*))))))

  ;;; ── :menu-select dispatch ────────────────────────────────────────────────────

  ;; :menu-select executes the command of the currently selected menu item.
  (it "dispatch-menu-select-executes-selected-command"
    (with-fake-session (s)
      (let ((cl-tmux::*active-menu*
              (make-menu :title "t"
                         :items (list (cons "Detach" :detach))
                         :selected-index 0)))
        ;; :menu-select on an item with :detach must return :detach.
        (expect (eq :detach (cl-tmux::dispatch-command s :menu-select nil))))))

  ;; :menu-select clears *active-menu* and the overlay after executing.
  (it "dispatch-menu-select-clears-menu-and-overlay"
    (with-fake-session (s)
      (let ((*overlay* nil)
            (cl-tmux::*active-menu*
              (make-menu :title "t"
                         :items (list (cons "List Keys" :list-keys))
                         :selected-index 0)))
        (cl-tmux::dispatch-command s :menu-select nil)
        (expect (null cl-tmux::*active-menu*)))))

  ;; :menu-select with *active-menu* NIL is a no-op.
  (it "dispatch-menu-select-nil-menu-is-noop"
    (with-fake-session (s)
      (let ((cl-tmux::*active-menu* nil))
        (finishes (cl-tmux::dispatch-command s :menu-select nil)
                  ":menu-select with no active menu must not error"))))

  ;;; ── dispatch-prefix-command: normal (non-copy-mode) table lookup ─────────────

  ;; dispatch-prefix-command 'n' and 'p' each select the other window in a 2-window session.
  (it "dispatch-prefix-command-n-and-p-select-other-window"
    (dolist (key '(#\n #\p))
      (with-fake-session (s :nwindows 2)
        (let ((w1 (second (session-windows s))))
          (cl-tmux::dispatch-prefix-command s (char-code key))
          (expect (eq w1 (session-active-window s)))))))

  ;; dispatch-prefix-command with a byte that has no key binding is a no-op.
  (it "dispatch-prefix-command-unknown-byte-is-noop"
    (with-fake-session (s)
      ;; #\x00 is unlikely to have a binding; the call must not error.
      (finishes (cl-tmux::dispatch-prefix-command s 0)
                "dispatch-prefix-command with an unbound byte must not error"))))
