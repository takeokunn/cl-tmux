(in-package #:cl-tmux/test)

;;;; load-config-file, command-keyword, parse-bind-args, key-table edge cases — part III

(in-suite config-directives-suite)

;;; load-config-file on a real temp file

(test load-config-file-existing-temp-file
  "load-config-file applies an existing file's directives and returns the count."
  (with-isolated-config
    (with-temp-config-file (p "bind z next-window" "set-status-height 3")
      (is (= 2 (load-config-file p))
          "load-config-file should apply and count both directives")
      (is (eq :next-window (lookup-key-binding #\z))
          "#\\z should be bound to :next-window after loading the temp file")
      (is (= 3 *status-height*)
          "*status-height* should be 3 after loading the temp file, got ~A"
          *status-height*))))

;;; %command-keyword: resolution and non-interning contract

(test command-keyword-does-not-intern-unknown
  "%command-keyword must NOT intern an unknown command name into :keyword."
  (let ((name "CL-TMUX-NONEXISTENT-COMMAND-DO-NOT-INTERN-ME"))
    (is (null (find-symbol name :keyword))
        "precondition: ~A must not be interned in :keyword before the call" name)
    (is (null (cl-tmux/config::%command-keyword name))
        "an unknown command name must resolve to NIL")
    (is (null (find-symbol name :keyword))
        "%command-keyword must not have interned ~A into the keyword package"
        name)))

(test command-keyword-returns-bindable-keyword
  "%command-keyword returns the command keyword for a recognized bindable name."
  (dolist (case '(("new-window" :new-window "new-window should resolve to :new-window")
                  ("NEW-WINDOW" :new-window "resolution should be case-insensitive")
                  ("split-horizontal" :split-horizontal
                   "split-horizontal should resolve to :split-horizontal")
                  ("prev-window" :prev-window "prev-window should resolve to :prev-window")
                  ("copy-mode-enter" :copy-mode-enter
                   "copy-mode-enter should resolve to :copy-mode-enter")
                  ("swap-pane-forward" :swap-pane-forward
                   "swap-pane-forward should resolve to :swap-pane-forward")
                  ("detach" :detach "detach should resolve to :detach")))
    (destructuring-bind (name expected desc) case
      (is (eq expected (cl-tmux/config::%command-keyword name)) "~A" desc))))

(test command-keyword-rejects-standard-tmux-abbreviations
  "%command-keyword rejects shorthand tmux abbreviations so config sticks to
   canonical command names."
  (dolist (name '("breakp" "killp" "next" "prev" "last" "displayp" "rotatew"))
    (is (null (cl-tmux/config::%command-keyword name))
        "~A must be rejected" name)))

(test bind-canonical-name-rejects-shorthand
  "bind rejects shorthand tmux abbreviations now that config is canonical-only."
  (with-isolated-config
    (is (= 0 (load-config-from-string "bind b breakp")))
    (is (null (lookup-key-binding #\b))
        "breakp must no longer bind anything")
    (is (= 0 (load-config-from-string "bind @ previous-window")))
    (is (null (lookup-key-binding #\@))
        "previous-window must no longer bind anything")
    (is (= 0 (load-config-from-string "bind Q definitely-not-a-command"))
        "an unknown abbreviation must still be rejected")))

(test command-keyword-rejects-non-bindable-keyword
  "%command-keyword returns NIL for interned-but-non-bindable keywords."
  (let ((kw (intern "COPY-MODE-EXIT" :keyword)))
    (is (eq :copy-mode-exit kw)
        "the keyword :copy-mode-exit should be interned")
    (is (not (member :copy-mode-exit cl-tmux/config::*bindable-commands*))
        "precondition: :copy-mode-exit must not be in *bindable-commands*")
    (is (null (cl-tmux/config::%command-keyword "copy-mode-exit"))
        "%command-keyword must reject an interned-but-non-bindable keyword"))
  (let ((cl-tmux/config::*bindable-commands* '(:detach)))
    (is (null (cl-tmux/config::%command-keyword "new-window"))
        "with *bindable-commands* not containing :new-window, NIL is returned")
    (is (eq :detach (cl-tmux/config::%command-keyword "detach"))
        "a name still present in the rebound *bindable-commands* resolves")))

;;; %config-tokens (tokenizer)

(test config-tokens-splits-on-whitespace
  "%config-tokens splits a line into whitespace-separated tokens."
  (is (equal '("bind" "c" "new-window")
             (cl-tmux/config::%config-tokens "bind c new-window")))
  (is (equal '("set-shell" "/bin/bash")
             (cl-tmux/config::%config-tokens "  set-shell  /bin/bash  ")))
  (is (null (cl-tmux/config::%config-tokens ""))
      "empty string yields no tokens")
  (is (null (cl-tmux/config::%config-tokens "   "))
      "whitespace-only string yields no tokens"))

;;; %parse-key-token

(test parse-key-token-table
  "%parse-key-token: 1-char token → character; multi-char → string; C-<letter> →
   control char; C-Space/C-@ → NUL; C-<named-key> kept as string."
  (dolist (row (list (list "c"       #\c            "single char 'c'")
                     (list "%"       #\%            "single char '%'")
                     (list "\""      #\"            "single char '\"'")
                     (list "M-1"     "M-1"          "multi-char → string")
                     (list "F1"      "F1"           "multi-char F1 → string")
                     (list "C-a"     (code-char 1)  "C-a → ^A (1)")
                     (list "C-z"     (code-char 26) "C-z → ^Z (26)")
                     (list "C-b"     (code-char 2)  "C-b → ^B (2)")
                     (list "C-A"     (code-char 1)  "C-A → ^A (case-insensitive)")
                     (list "C-Space" (code-char 0)  "C-Space → NUL")
                     (list "C-@"     (code-char 0)  "C-@ → NUL")
                     (list "C-["     (code-char 27) "C-[ → ESC (27)")
                     (list "C-\\"    (code-char 28) "C-\\ → FS (28)")
                     (list "C-]"     (code-char 29) "C-] → GS (29)")
                     (list "C-Left"  "C-Left"       "C-<named-key> stays string")
                     (list "C-Up"    "C-Up"         "C-<named-key> stays string")))
    (destructuring-bind (input expected desc) row
      (is (equal expected (cl-tmux/config::%parse-key-token input)) "~A" desc))))

(test parse-key-token-canonicalizes-multi-modifier-order
  "%parse-key-token re-orders two-or-more modifier prefixes into canonical
   C-/M-/S- order, so the spelling order in a binding does not matter and matches
   what the event loop emits (audit #15)."
  (dolist (row (list (list "M-C-x"    "C-M-x"    "M-C-x → C-M-x")
                     (list "C-M-x"    "C-M-x"    "C-M-x stays canonical")
                     (list "S-C-Up"   "C-S-Up"   "S-C-Up → C-S-Up")
                     (list "M-C-Left" "C-M-Left" "M-C-Left → C-M-Left")
                     (list "S-M-C-x"  "C-M-S-x"  "S-M-C-x → C-M-S-x (3 modifiers)")))
    (destructuring-bind (input expected desc) row
      (is (equal expected (cl-tmux/config::%parse-key-token input)) "~A" desc))))

(test bind-control-letter-fires-via-control-char
  "bind C-a <cmd> binds the control character ^A (byte 1) so a real Ctrl-a
   keypress (which the event loop reads as byte 1) resolves to the command."
  (with-isolated-config
    (cl-tmux/config:apply-config-directive '("bind" "C-a" "next-window"))
    (is (eq :next-window (lookup-key-binding (code-char 1)))
        "C-a must bind ^A (byte 1) in the prefix table")))

(test bind-modifier-arrow-stores-canonical-string-key
  "bind C-Up <cmd> stores the command under the string key \"C-Up\" in the
   prefix table — the canonical key name the event loop reconstructs from the
   ESC [ 1 ; 5 A wire sequence.  (Without this, modifier+arrow binds were dead.)"
  (with-isolated-config
    (cl-tmux/config:apply-config-directive '("bind" "C-Up" "next-window"))
    (let ((entry (cl-tmux/config:key-table-lookup "prefix" "C-Up")))
      (is (eq :next-window (cl-tmux/config:key-table-command entry))
          "C-Up must bind under string key \"C-Up\" in the prefix table"))))

(test bind-n-modifier-arrow-stores-in-root-table
  "bind -n M-Left <cmd> stores under string key \"M-Left\" in the ROOT table so
   a bare (no-prefix) Alt+Left fires it."
  (with-isolated-config
    (cl-tmux/config:apply-config-directive '("bind" "-n" "M-Left" "next-window"))
    (let ((entry (cl-tmux/config:key-table-lookup "root" "M-Left")))
      (is (eq :next-window (cl-tmux/config:key-table-command entry))
          "M-Left must bind under string key \"M-Left\" in the root table"))))

(test bind-plain-arrow-stores-canonical-string-key
  "bind Up <cmd> stores under string key \"Up\" in the prefix table, matching
   the name reconstructed from the ESC [ A wire sequence."
  (with-isolated-config
    (cl-tmux/config:apply-config-directive '("bind" "Up" "next-window"))
    (let ((entry (cl-tmux/config:key-table-lookup "prefix" "Up")))
      (is (eq :next-window (cl-tmux/config:key-table-command entry))
          "Up must bind under string key \"Up\" in the prefix table"))))

(test bind-n-meta-key-stores-in-root-table
  "bind -n M-h <cmd> stores under string key \"M-h\" in the ROOT table so a bare
   (no-prefix) Alt+h, which arrives as ESC h, fires it."
  (with-isolated-config
    (cl-tmux/config:apply-config-directive '("bind" "-n" "M-h" "next-window"))
    (let ((entry (cl-tmux/config:key-table-lookup "root" "M-h")))
      (is (eq :next-window (cl-tmux/config:key-table-command entry))
          "M-h must bind under string key \"M-h\" in the root table"))))

;;; status option: off / on / line-count parsing → *status-height*

(test set-status-directive-table
  "`set -g status` maps string values to the expected status height."
  (dolist (case '(("off" 0 "status off → height 0")
                  ("0" 0 "status 0 → height 0")
                  ("on" 1 "status on → height 1")
                  ("2" 2 "status 2 must reserve 2 rows")
                  ("5" 5 "status 5 → 5 rows")
                  ("9" 5 "status 9 → clamped to 5 rows")))
    (destructuring-bind (value expected desc) case
      (with-isolated-config
        (cl-tmux/config:apply-config-directive (list "set" "-g" "status" value))
        (is (= expected cl-tmux/config:*status-height*) "~A" desc)))))

;;; apply-config-line

(test apply-config-line-applies-valid-directives
  "apply-config-line applies a directive line and returns T."
  (with-isolated-config
    (is (eq t (cl-tmux/config::apply-config-line "bind z new-window"))
        "valid directive line should return T")
    (is (eq :new-window (lookup-key-binding #\z)))))

(test apply-config-line-ignores-blank-and-comments
  "apply-config-line returns NIL for blank lines and # comments."
  (is (null (cl-tmux/config::apply-config-line ""))
      "blank line should return NIL")
  (is (null (cl-tmux/config::apply-config-line "   "))
      "whitespace-only line should return NIL")
  (is (null (cl-tmux/config::apply-config-line "# this is a comment"))
      "comment line should return NIL"))
