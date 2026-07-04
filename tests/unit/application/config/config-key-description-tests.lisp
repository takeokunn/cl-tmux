(in-package #:cl-tmux/test)

;;;; Key binding description and list-keys rendering tests.

;;; ── Import the config symbols we need ────────────────────────────────────

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(cl-tmux/config:describe-key-bindings)))

;;; ── describe-key-bindings (list-keys help text) ─────────────────────────────

(test describe-key-bindings-lists-commands
  "describe-key-bindings produces help text naming the bound commands."
  (let ((text (describe-key-bindings)))
    (dolist (sub '("new-window" "detach" "select-window"))
      (is (search sub text) "should list ~A" sub))))

(test prefix-named-single-byte-key-reachable-at-runtime
  "Runtime prefix dispatch resolves a named single-byte key binding (Enter, byte
   13) via candidate probing, so `bind Enter <cmd>` is reachable — not just
   printable single characters (audit #35)."
  (with-isolated-config
    (cl-tmux/config:load-config-from-string "bind Enter next-window")
    (let ((entry (cl-tmux::%key-table-entry-by-candidates
                  cl-tmux/config:+table-prefix+
                  (cl-tmux::%single-byte-key-candidates 13))))
      (is (not (null entry))
          "Enter (byte 13) must resolve a prefix-table entry after bind Enter")
      (is (eq :next-window (cl-tmux/config:key-table-command entry))
          "the resolved entry must be the bound next-window command"))))

(test default-prefix-string-bindings-are-listed-and-repeatable
  "Default prefix multi-byte keys are present in the key table and resize arrows are repeatable."
  (with-isolated-config
    (let ((up-entry (cl-tmux/config:key-table-lookup "prefix" "Up"))
          (ctrl-right-entry (cl-tmux/config:key-table-lookup "prefix" "C-Right"))
          (meta-right-entry (cl-tmux/config:key-table-lookup "prefix" "M-Right"))
          (text (cl-tmux/config:describe-key-bindings-for-table "prefix")))
      (is (eq :select-pane-up (cl-tmux/config:key-table-command up-entry))
          "prefix Up must select the pane above")
      (is (equal '("resize-pane" "-R" "1")
                 (cl-tmux/config:key-table-command ctrl-right-entry))
          "prefix C-Right must resize right by 1")
      (is (equal '("resize-pane" "-R" "5")
                 (cl-tmux/config:key-table-command meta-right-entry))
          "prefix M-Right must resize right by 5")
      (is (cl-tmux/config:key-table-repeatable-p ctrl-right-entry)
          "prefix C-Right must be repeatable")
      (is (cl-tmux/config:key-table-repeatable-p meta-right-entry)
          "prefix M-Right must be repeatable")
      (is (search "bind-key -T prefix Up select-pane-up" text)
          "list-keys must show the prefix Up binding")
      (is (search "bind-key -T prefix -r C-Right resize-pane -R 1" text)
          "list-keys must show repeatable C-Right resize binding"))))

(test default-prefix-char-and-digit-bindings
  "Every char/digit entry from config-prefix-defaults.lisp's
   define-initial-key-bindings form is bound in the prefix table to its
   documented command (audit: test-abstraction/high)."
  (with-isolated-config
    (check-copy-mode-bindings "prefix"
      (#\c :new-window            "c opens a new window")
      (#\n :next-window           "n selects the next window")
      (#\p :prev-window           "p selects the previous window")
      (#\" :split-horizontal      "\" splits the pane horizontally")
      (#\% :split-vertical        "% splits the pane vertically")
      (#\o :next-pane             "o selects the next pane")
      (#\d :detach                "d detaches the client")
      (#\? :list-keys             "? lists key bindings")
      (#\[ :copy-mode-enter       "[ enters copy-mode")
      (#\] :paste-buffer          "] pastes the buffer")
      (#\x :kill-pane-confirm     "x confirms killing the pane")
      (#\& :kill-window-confirm   "& confirms killing the window")
      (#\, :rename-window         ", renames the window")
      (#\H :resize-left           "H resizes the pane left")
      (#\J :resize-down           "J resizes the pane down")
      (#\K :resize-up             "K resizes the pane up")
      (#\L :last-session          "L selects the last session (extended binding overrides bootstrap resize-right default)")
      (#\$ :rename-session        "$ renames the session")
      (#\! :break-pane            "! breaks the pane (extended binding overrides bootstrap if-shell default)")
      (#\0 :select-window         "digit 0 selects window 0")
      (#\9 :select-window         "digit 9 selects window 9"))))

(test default-copy-mode-vi-bindings-are-listed
  "Default copy-mode-vi keys are present in the key table and list-keys output."
  (with-isolated-config
    (check-copy-mode-bindings "copy-mode-vi"
      ("Escape"  :copy-mode-clear-selection              "Escape clears selection")
      (#\q       :copy-mode-exit                         "q exits copy-mode")
      (#\i       :copy-mode-exit                         "i exits copy-mode")
      (#\j       :copy-mode-cursor-down                  "j moves down")
      (#\h       :copy-mode-cursor-left                  "h moves left")
      (#\%       :copy-mode-next-matching-bracket        "% jumps to matching bracket")
      (#\#       :copy-mode-search-backward-word         "# searches backward for word")
      (#\*       :copy-mode-search-forward-word          "* searches forward for word")
      (#\,       :copy-mode-jump-reverse                 ", reverses last jump")
      (#\;       :copy-mode-jump-again                   "; repeats last jump")
      (#\A       :copy-mode-append-selection-and-cancel  "A appends selection and cancels")
      (#\D       :copy-mode-copy-pipe-end-of-line-and-cancel "D copies to end of line")
      (#\J       :copy-mode-scroll-down-line             "J scrolls down one line")
      (#\K       :copy-mode-scroll-up-line               "K scrolls up one line")
      (#\P       :copy-mode-other-end                    "P toggles position")
      (#\R       :copy-mode-rectangle-toggle             "R toggles rectangle selection")
      (#\X       :copy-mode-set-mark                     "X sets mark")
      (#\o       :copy-mode-other-end                    "o moves to other selection end")
      (#\z       :copy-mode-scroll-middle                "z centres the cursor")
      (#\r       :copy-mode-refresh-from-pane            "r refreshes from pane")
      (#\f       :copy-mode-jump-forward                 "f jumps forward to char")
      (#\F       :copy-mode-jump-backward                "F jumps backward to char")
      (#\t       :copy-mode-jump-to                      "t jumps to before char")
      (#\T       :copy-mode-jump-to-backward             "T jumps backward to before char")
      (#\:       :copy-mode-goto-line                    ": opens goto-line prompt")
      (#\{       :copy-mode-prev-paragraph               "{ moves to prev paragraph")
      (#\}       :copy-mode-next-paragraph               "} moves to next paragraph")
      ("M-x"     :copy-mode-jump-to-mark                  "M-x jumps to mark")
      ("C-b"     :copy-mode-page-up                       "C-b pages up")
      ("C-v"     :copy-mode-rectangle-toggle              "C-v toggles rectangle selection")
      ("C-Up"    :copy-mode-scroll-up-line                "C-Up scrolls up one line")
      ("C-Down"  :copy-mode-scroll-down-line              "C-Down scrolls down one line")
      ("Enter"   :copy-mode-copy-pipe-and-cancel          "Enter copy-pipe-and-cancel")
      ("BSpace"  :copy-mode-cursor-left                   "BSpace moves left")
      ("PageUp"  :copy-mode-page-up                       "PageUp pages up"))
    (let ((text (cl-tmux/config:describe-key-bindings-for-table "copy-mode-vi")))
      (dolist (fragment '("bind-key -T copy-mode-vi j copy-mode-cursor-down"
                          "bind-key -T copy-mode-vi Escape copy-mode-clear-selection"
                          "bind-key -T copy-mode-vi q copy-mode-exit"
                          "bind-key -T copy-mode-vi i copy-mode-exit"
                          "bind-key -T copy-mode-vi f copy-mode-jump-forward"
                          "bind-key -T copy-mode-vi F copy-mode-jump-backward"
                          "bind-key -T copy-mode-vi t copy-mode-jump-to"
                          "bind-key -T copy-mode-vi T copy-mode-jump-to-backward"
                          "bind-key -T copy-mode-vi r copy-mode-refresh-from-pane"
                          "bind-key -T copy-mode-vi : copy-mode-goto-line"
                          "bind-key -T copy-mode-vi % copy-mode-next-matching-bracket"
                          "bind-key -T copy-mode-vi # copy-mode-search-backward-word"
                          "bind-key -T copy-mode-vi * copy-mode-search-forward-word"
                          "bind-key -T copy-mode-vi A copy-mode-append-selection-and-cancel"
                          "bind-key -T copy-mode-vi M-x copy-mode-jump-to-mark"
                          "bind-key -T copy-mode-vi C-b copy-mode-page-up"
                          "bind-key -T copy-mode-vi C-Up copy-mode-scroll-up-line"
                          "bind-key -T copy-mode-vi Enter copy-mode-copy-pipe-and-cancel"
                          "bind-key -T copy-mode-vi PageUp copy-mode-page-up"))
        (is (search fragment text)
            (format nil "list-keys must show ~A" fragment))))))

(test default-copy-mode-emacs-control-bindings
  "Default copy-mode emacs control keys match tmux movement, selection and search defaults."
  (with-isolated-config
    (check-copy-mode-bindings "copy-mode"
      ("C-Space" :copy-mode-begin-selection "C-Space begins selection")
      ("C-a" :copy-mode-line-start "C-a moves to line start")
      ("C-c" :copy-mode-exit "C-c exits copy-mode")
      ("C-e" :copy-mode-line-end "C-e moves to line end")
      ("C-f" :copy-mode-cursor-right "C-f moves right")
      ("C-b" :copy-mode-cursor-left "C-b moves left")
      ("C-g" :copy-mode-clear-selection "C-g clears selection")
      ("C-l" :copy-mode-cursor-centre-vertical
       "C-l centres the cursor vertically")
      ("C-k" :copy-mode-copy-pipe-end-of-line-and-cancel
       "C-k copy-pipes to end of line and cancels")
      ("C-n" :copy-mode-cursor-down "C-n moves down")
      ("C-p" :copy-mode-cursor-up "C-p moves up")
      ("C-r" :copy-mode-search-backward-incremental
       "C-r starts backward incremental search")
      ("C-s" :copy-mode-search-forward-incremental
       "C-s starts forward incremental search")
      ("C-v" :copy-mode-page-down "C-v pages down")
      ("C-w" :copy-mode-copy-pipe-and-cancel
       "C-w copy-pipes and cancels"))))

(test default-copy-mode-emacs-printable-and-meta-bindings
  "Default copy-mode emacs printable and meta keys expose tmux navigation defaults."
  (with-isolated-config
    (check-copy-mode-bindings "copy-mode"
      (#\Space :copy-mode-page-down "Space pages down")
      (#\, :copy-mode-jump-reverse ", reverses the last jump")
      (#\; :copy-mode-jump-again "; repeats the last jump")
      (#\N :copy-mode-search-prev "N repeats search backward")
      (#\P :copy-mode-other-end "P toggles position")
      (#\R :copy-mode-rectangle-toggle "R toggles rectangle")
      (#\X :copy-mode-set-mark "X sets mark")
      (#\n :copy-mode-search-next "n repeats search forward")
      (#\r :copy-mode-refresh-from-pane "r refreshes from pane")
      (#\f :copy-mode-jump-forward "f jumps forward")
      (#\F :copy-mode-jump-backward "F jumps backward")
      (#\t :copy-mode-jump-to "t jumps to a character")
      (#\T :copy-mode-jump-to-backward
       "T jumps backward to a character")
      (#\g :copy-mode-goto-line "g opens the goto-line prompt")
      ("M-f" :copy-mode-word-end "M-f moves to next word end")
      ("M-l" :copy-mode-cursor-centre-horizontal
       "M-l centres the cursor horizontally")
      ("M-x" :copy-mode-jump-to-mark "M-x jumps to mark")
      ("M-{" :copy-mode-prev-paragraph
       "M-{ moves to the previous paragraph")
      ("M-}" :copy-mode-next-paragraph
       "M-} moves to the next paragraph"))
    (let ((text (cl-tmux/config:describe-key-bindings-for-table "copy-mode")))
      (is (search "bind-key -T copy-mode C-Space copy-mode-begin-selection" text)
          "list-keys must show copy-mode C-Space")
      (is (search "bind-key -T copy-mode C-l copy-mode-cursor-centre-vertical" text)
          "list-keys must show copy-mode C-l")
      (is (search "bind-key -T copy-mode M-f copy-mode-word-end" text)
          "list-keys must show copy-mode M-f")
      (is (search "bind-key -T copy-mode M-l copy-mode-cursor-centre-horizontal"
                  text)
          "list-keys must show copy-mode M-l")
      (is (search "bind-key -T copy-mode f copy-mode-jump-forward" text)
          "list-keys must show copy-mode f")
      (is (search "bind-key -T copy-mode F copy-mode-jump-backward" text)
          "list-keys must show copy-mode F")
      (is (search "bind-key -T copy-mode t copy-mode-jump-to" text)
          "list-keys must show copy-mode t")
      (is (search "bind-key -T copy-mode T copy-mode-jump-to-backward" text)
          "list-keys must show copy-mode T")
      (is (search "bind-key -T copy-mode g copy-mode-goto-line" text)
          "list-keys must show copy-mode g")
      (is (search "bind-key -T copy-mode M-x copy-mode-jump-to-mark" text)
          "list-keys must show copy-mode M-x"))))

(test default-copy-mode-emacs-q-exits
  "Default copy-mode emacs q exits copy-mode and appears in list-keys output."
  (with-isolated-config
    (let ((escape-entry (cl-tmux/config:key-table-lookup "copy-mode" "Escape"))
          (q-entry (cl-tmux/config:key-table-lookup "copy-mode" #\q))
          (text (cl-tmux/config:describe-key-bindings-for-table "copy-mode")))
      (is (eq :copy-mode-exit
              (cl-tmux/config:key-table-command escape-entry))
          "copy-mode Escape must exit copy-mode")
      (is (eq :copy-mode-exit
              (cl-tmux/config:key-table-command q-entry))
          "copy-mode q must exit copy-mode")
      (is (search "bind-key -T copy-mode Escape copy-mode-exit" text)
          "list-keys must show copy-mode Escape")
      (is (search "bind-key -T copy-mode q copy-mode-exit" text)
          "list-keys must show copy-mode q"))))

(test default-copy-mode-emacs-control-meta-bracket-bindings
  "Default copy-mode emacs C-M-b/C-M-f jump between matching brackets."
  (with-isolated-config
    (let ((prev-entry (cl-tmux/config:key-table-lookup "copy-mode" "C-M-b"))
          (next-entry (cl-tmux/config:key-table-lookup "copy-mode" "C-M-f"))
          (text (cl-tmux/config:describe-key-bindings-for-table "copy-mode")))
      (is (eq :copy-mode-previous-matching-bracket
              (cl-tmux/config:key-table-command prev-entry))
          "copy-mode C-M-b must jump to the previous matching bracket")
      (is (eq :copy-mode-next-matching-bracket
              (cl-tmux/config:key-table-command next-entry))
          "copy-mode C-M-f must jump to the next matching bracket")
      (is (search "bind-key -T copy-mode C-M-b copy-mode-previous-matching-bracket"
                  text)
          "list-keys must show copy-mode C-M-b")
      (is (search "bind-key -T copy-mode C-M-f copy-mode-next-matching-bracket"
                  text)
          "list-keys must show copy-mode C-M-f"))))

(test default-copy-mode-emacs-modifier-arrow-bindings
  "Default copy-mode emacs C-Up/C-Down and M-Up/M-Down match tmux scrolling keys."
  (with-isolated-config
    (let ((c-up-entry (cl-tmux/config:key-table-lookup "copy-mode" "C-Up"))
          (c-down-entry (cl-tmux/config:key-table-lookup "copy-mode" "C-Down"))
          (m-up-entry (cl-tmux/config:key-table-lookup "copy-mode" "M-Up"))
          (m-down-entry (cl-tmux/config:key-table-lookup "copy-mode" "M-Down"))
          (text (cl-tmux/config:describe-key-bindings-for-table "copy-mode")))
      (is (eq :copy-mode-scroll-up-line
              (cl-tmux/config:key-table-command c-up-entry))
          "copy-mode C-Up must scroll up one line")
      (is (eq :copy-mode-scroll-down-line
              (cl-tmux/config:key-table-command c-down-entry))
          "copy-mode C-Down must scroll down one line")
      (is (eq :copy-mode-half-page-up
              (cl-tmux/config:key-table-command m-up-entry))
          "copy-mode M-Up must half-page up")
      (is (eq :copy-mode-half-page-down
              (cl-tmux/config:key-table-command m-down-entry))
          "copy-mode M-Down must half-page down")
      (is (search "bind-key -T copy-mode C-Up copy-mode-scroll-up-line" text)
          "list-keys must show copy-mode C-Up")
      (is (search "bind-key -T copy-mode M-Down copy-mode-half-page-down" text)
          "list-keys must show copy-mode M-Down"))))

(test default-copy-mode-named-navigation-bindings
  "Every named-key entry in +default-copy-mode-named-navigation-bindings+ is
   bound in the base copy-mode table to its documented command (audit:
   test-abstraction/medium — PageDown/Home/End/Left/Right/Down/PageUp were
   previously unchecked by name)."
  (with-isolated-config
    (check-copy-mode-bindings "copy-mode"
      ("Up"       :copy-mode-cursor-up   "Up moves the cursor up")
      ("Down"     :copy-mode-cursor-down "Down moves the cursor down")
      ("Left"     :copy-mode-cursor-left "Left moves the cursor left")
      ("Right"    :copy-mode-cursor-right "Right moves the cursor right")
      ("C-Up"     :copy-mode-scroll-up-line "C-Up scrolls up one line")
      ("C-Down"   :copy-mode-scroll-down-line "C-Down scrolls down one line")
      ("PageUp"   :copy-mode-page-up "PageUp pages up")
      ("PageDown" :copy-mode-page-down "PageDown pages down")
      ("Home"     :copy-mode-line-start "Home moves to the line start")
      ("End"      :copy-mode-line-end "End moves to the line end"))))

(test describe-key-bindings-for-key-filters-by-key-label
  "describe-key-bindings-for-key returns only bindings whose key label matches."
  (with-isolated-config
    (let ((text (cl-tmux/config:describe-key-bindings-for-key "prefix" "C-Right")))
      (is (search "bind-key -T prefix -r C-Right resize-pane -R 1" text)
          "C-Right binding must be listed")
      (is (null (search "bind-key -T prefix Up select-pane-up" text))
          "different keys must be omitted"))))
