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
  (is (eq :new-window (cl-tmux/config::%command-keyword "new-window"))
      "new-window should resolve to :new-window")
  (is (eq :new-window (cl-tmux/config::%command-keyword "NEW-WINDOW"))
      "resolution should be case-insensitive")
  (is (eq :split-horizontal (cl-tmux/config::%command-keyword "split-horizontal"))
      "split-horizontal should resolve to :split-horizontal"))

(test command-keyword-resolves-tmux-aliases
  "%command-keyword resolves tmux command-name aliases (full names whose keyword
   differs, plus short forms) to the canonical bindable keyword."
  (is (eq :prev-window      (cl-tmux/config::%command-keyword "previous-window")))
  (is (eq :copy-mode-enter  (cl-tmux/config::%command-keyword "copy-mode")))
  (is (eq :swap-pane-forward (cl-tmux/config::%command-keyword "swap-pane")))
  (is (eq :detach           (cl-tmux/config::%command-keyword "detach-client")))
  (is (eq :show-window-options (cl-tmux/config::%command-keyword "showw")))
  (is (eq :prev-window      (cl-tmux/config::%command-keyword "PREVIOUS-WINDOW"))
      "alias resolution must be case-insensitive"))

(test command-keyword-resolves-standard-tmux-abbreviations
  "%command-keyword resolves the standard tmux command abbreviations (man tmux
   ALIASES) for arg-less bindable commands to their canonical keyword."
  (is (eq :break-pane    (cl-tmux/config::%command-keyword "breakp")))
  (is (eq :kill-pane     (cl-tmux/config::%command-keyword "killp")))
  (is (eq :next-window   (cl-tmux/config::%command-keyword "next")))
  (is (eq :prev-window   (cl-tmux/config::%command-keyword "prev")))
  (is (eq :last-window   (cl-tmux/config::%command-keyword "last")))
  (is (eq :display-panes (cl-tmux/config::%command-keyword "displayp")))
  (is (eq :rotate-window (cl-tmux/config::%command-keyword "rotatew"))))

(test bind-tmux-abbreviation-fires
  "bind b breakp binds :break-pane via the abbreviation, and an unknown abbrev
   is still rejected."
  (with-isolated-config
    (is (= 1 (load-config-from-string "bind b breakp")))
    (is (eq :break-pane (lookup-key-binding #\b))
        "abbrev breakp must bind :break-pane")
    (is (= 0 (load-config-from-string "bind Q definitely-not-a-command"))
        "an unknown abbreviation must still be rejected")))

(test command-name-aliases-target-bindable-keywords
  "Every alias value must be a member of *bindable-commands* (else the bind would
   resolve to a keyword the dispatcher rejects)."
  (dolist (pair cl-tmux/config::*command-name-aliases*)
    (is (member (cdr pair) cl-tmux/config::*bindable-commands*)
        "alias ~A -> ~A must target a bindable keyword" (car pair) (cdr pair))))

(test bind-tmux-alias-name-fires
  "bind x previous-window binds the canonical :prev-window keyword via the alias."
  (with-isolated-config
    (let ((applied (load-config-from-string "bind x previous-window")))
      (is (= 1 applied))
      (is (eq :prev-window (lookup-key-binding #\x))
          "previous-window alias must bind :prev-window"))))

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

(test parse-key-token-single-char-returns-char
  "%parse-key-token returns a character for a 1-char token."
  (is (char= #\c   (cl-tmux/config::%parse-key-token "c")))
  (is (char= #\%   (cl-tmux/config::%parse-key-token "%")))
  (is (char= #\"   (cl-tmux/config::%parse-key-token "\""))))

(test parse-key-token-multi-char-returns-string
  "%parse-key-token returns the string itself for tokens longer than 1 char."
  (is (string= "M-1" (cl-tmux/config::%parse-key-token "M-1")))
  (is (string= "F1"  (cl-tmux/config::%parse-key-token "F1"))))

(test parse-key-token-control-letters-return-control-char
  "%parse-key-token converts C-<letter> to its control character so the binding
   matches the byte the event loop sees: C-a→^A(1), C-z→^Z(26), C-b→^B(2)."
  (is (char= (code-char 1)  (cl-tmux/config::%parse-key-token "C-a")) "C-a → ^A (1)")
  (is (char= (code-char 26) (cl-tmux/config::%parse-key-token "C-z")) "C-z → ^Z (26)")
  (is (char= (code-char 2)  (cl-tmux/config::%parse-key-token "C-b")) "C-b → ^B (2)")
  ;; Case-insensitive: C-A is the same control char as C-a.
  (is (char= (code-char 1)  (cl-tmux/config::%parse-key-token "C-A")) "C-A → ^A (1)"))

(test parse-key-token-control-space-and-at-return-nul
  "C-Space and C-@ both map to the NUL control character (byte 0)."
  (is (char= (code-char 0) (cl-tmux/config::%parse-key-token "C-Space")) "C-Space → NUL")
  (is (char= (code-char 0) (cl-tmux/config::%parse-key-token "C-@")) "C-@ → NUL"))

(test parse-key-token-control-punctuation-return-control-char
  "C-[ C-\\ C-] map to control bytes 27/28/29."
  (is (char= (code-char 27) (cl-tmux/config::%parse-key-token "C-[")) "C-[ → ESC (27)")
  (is (char= (code-char 28) (cl-tmux/config::%parse-key-token "C-\\")) "C-\\ → FS (28)")
  (is (char= (code-char 29) (cl-tmux/config::%parse-key-token "C-]")) "C-] → GS (29)"))

(test parse-key-token-control-modifier-arrow-stays-string
  "C-<named-key> (e.g. C-Left) that the event loop encodes as a multi-byte
   sequence is kept as the string for the deferred modifier-key path."
  (is (string= "C-Left"  (cl-tmux/config::%parse-key-token "C-Left")))
  (is (string= "C-Up"    (cl-tmux/config::%parse-key-token "C-Up"))))

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

(test set-status-numeric-shows-bar
  "`set -g status 2` reserves two status rows (multi-line); the count is clamped
   to tmux's maximum of 5."
  (with-isolated-config
    (cl-tmux/config:apply-config-directive '("set" "-g" "status" "2"))
    (is (= 2 cl-tmux/config:*status-height*)
        "status 2 must reserve 2 rows")
    (cl-tmux/config:apply-config-directive '("set" "-g" "status" "5"))
    (is (= 5 cl-tmux/config:*status-height*) "status 5 → 5 rows")
    (cl-tmux/config:apply-config-directive '("set" "-g" "status" "9"))
    (is (= 5 cl-tmux/config:*status-height*) "status 9 → clamped to 5 rows")))

(test set-status-off-hides-bar
  "`set -g status off` (and 0/false) hides the status bar (height 0)."
  (with-isolated-config
    (cl-tmux/config:apply-config-directive '("set" "-g" "status" "off"))
    (is (= 0 cl-tmux/config:*status-height*) "status off → height 0")
    (cl-tmux/config:apply-config-directive '("set" "-g" "status" "0"))
    (is (= 0 cl-tmux/config:*status-height*) "status 0 → height 0")))

(test set-status-on-shows-bar
  "`set -g status on` shows the status bar (height 1)."
  (with-isolated-config
    (cl-tmux/config:apply-config-directive '("set" "-g" "status" "off"))
    (cl-tmux/config:apply-config-directive '("set" "-g" "status" "on"))
    (is (= 1 cl-tmux/config:*status-height*) "status on → height 1")))

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

