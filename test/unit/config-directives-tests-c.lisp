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

(test define-config-directives-macro-is-defined
  "define-config-directives is a defined macro."
  (is (macro-function 'cl-tmux/config::define-config-directives)))

(test define-key-directive-handlers-macro-is-defined
  "define-key-directive-handlers is a defined macro."
  (is (macro-function 'cl-tmux/config::define-key-directive-handlers)))

(test env-set-p-correctly-classifies-strings
  "%env-set-p returns T for non-empty strings and NIL for nil or empty strings."
  (is-true  (cl-tmux/config::%env-set-p "/some/path")  "non-empty string is set")
  (is-true  (cl-tmux/config::%env-set-p "x")           "single-char string is set")
  (is-false (cl-tmux/config::%env-set-p nil)            "nil is not set")
  (is-false (cl-tmux/config::%env-set-p "")             "empty string is not set"))

;;; Tokenizer with quote/escape support

(test config-tokenizer-quoted-double-quotes
  "%config-tokens with a double-quoted string produces a single token preserving spaces."
  (let ((tokens (cl-tmux/config::%config-tokens "bind n \"foo bar\"")))
    (is (equal '("bind" "n" "foo bar") tokens)
        "double-quoted string must yield a single token with spaces: got ~S" tokens)))

(test config-tokenizer-single-quotes
  "%config-tokens with a single-quoted string produces a single token."
  (let ((tokens (cl-tmux/config::%config-tokens "set-shell '/usr/bin/my shell'")))
    (is (= 2 (length tokens))
        "single-quoted path must produce 2 tokens, got ~D: ~S" (length tokens) tokens)
    (is (string= "/usr/bin/my shell" (second tokens))
        "second token must be the single-quoted value, got ~S" (second tokens))))

(test config-tokenizer-backslash-escape
  "%config-tokens: backslash outside quotes escapes the next character."
  ;; The Lisp literal "foo\\ bar" is the 8-char string  foo\ bar  (with a real
  ;; backslash); that is what the tokenizer must collapse to "foo bar".
  (let ((tokens (cl-tmux/config::%config-tokens "foo\\ bar")))
    (is (= 1 (length tokens))
        "backslash-escaped space must yield a single token, got ~S" tokens)
    (is (string= "foo bar" (first tokens))
        "token must be foo bar after backslash-space, got ~S" (first tokens))))

(test config-tokenizer-empty-double-quotes
  "%config-tokens: empty double-quotes produces an empty string token."
  (let ((tokens (cl-tmux/config::%config-tokens "cmd \"\"")))
    (is (= 2 (length tokens))
        "empty double-quotes must yield 2 tokens, got ~S" tokens)
    (is (string= "" (second tokens))
        "second token must be the empty string, got ~S" (second tokens))))

(test config-tokenizer-mixed
  "%config-tokens: mix of plain tokens, quoted tokens, and backslash escapes."
  ;; "a \"b c\" d\\ e" is the literal  a "b c" d\ e  — quoted token preserves the
  ;; inner space; the backslash-space escapes to keep "d e" as one token.
  (let ((tokens (cl-tmux/config::%config-tokens "a \"b c\" d\\ e")))
    (is (= 3 (length tokens))
        "must have 3 tokens, got ~S" tokens)
    (is (string= "a" (first tokens)) "first token is a")
    (is (string= "b c" (second tokens)) "second token is b c")
    (is (string= "d e" (third tokens)) "third token is d e (backslash-space)")))

;;; bind-key with flags

(test bind-key-no-prefix-n-flag
  "bind -n binds in the root key-table (no prefix required)."
  (with-isolated-key-tables
    (apply-config-directive '("bind" "-n" "C" "new-window"))
    (let ((entry (cl-tmux/config:key-table-lookup "root" #\C)))
      (is (not (null entry))
          "bind -n must add a binding to the root table")
      (is (eq :new-window (cl-tmux/config:key-table-command entry))
          "root binding must be :new-window"))))

(test bind-key-repeatable-r-flag
  "bind -r marks the binding as repeatable."
  (with-isolated-key-tables
    (apply-config-directive '("bind" "-r" "H" "resize-left"))
    (let ((entry (cl-tmux/config:key-table-lookup "prefix" #\H)))
      (is (not (null entry))
          "bind -r must add a binding to the prefix table")
      (is (cl-tmux/config:key-table-repeatable-p entry)
          "binding must be marked repeatable with -r flag"))))

(test bind-key-custom-table-T-flag
  "bind -T table-name binds in the named key-table."
  (with-isolated-key-tables
    (apply-config-directive '("bind" "-T" "copy-mode" "q" "copy-mode-enter"))
    (let ((entry (cl-tmux/config:key-table-lookup "copy-mode" #\q)))
      (is (not (null entry))
          "bind -T copy-mode must add a binding to the copy-mode table")
      (is (eq :copy-mode-enter (cl-tmux/config:key-table-command entry))
          "copy-mode binding must be :copy-mode-enter"))))

(test bind-key-simple-also-updates-key-table
  "Simple bind (no flags) also updates the prefix key-table."
  (with-isolated-key-tables
    (apply-config-directive '("bind" "z" "new-window"))
    (let ((entry (cl-tmux/config:key-table-lookup "prefix" #\z)))
      (is (not (null entry))
          "simple bind must also add to the prefix key-table")
      (is (eq :new-window (cl-tmux/config:key-table-command entry))
          "prefix binding must be :new-window"))))

;;; bind-key directive alias
;;;
;;; NOTE: The six individual set-option alias tests (set-directive-stores-option,
;;; setw-directive-stores-option, etc.) have been removed.  The table-driven test
;;; set-option-directive-aliases-table-driven (below) supersedes them all.

(test bind-key-alias-accepted
  "bind-key is accepted as an alias for bind and creates a prefix binding."
  (with-isolated-key-tables
    (is (eq t (apply-config-directive '("bind-key" "z" "new-window")))
        "bind-key directive should return T")
    (is (eq :new-window (lookup-key-binding #\z))
        "#\\z should be bound to :new-window via bind-key")))

(test bind-key-alias-n-flag
  "bind-key -n binds in the root table (no prefix required)."
  (with-isolated-key-tables
    (apply-config-directive '("bind-key" "-n" "F" "new-window"))
    (let ((entry (cl-tmux/config:key-table-lookup "root" #\F)))
      (is (not (null entry))
          "bind-key -n must add a binding to the root table")
      (is (eq :new-window (cl-tmux/config:key-table-command entry))
          "root table binding must be :new-window"))))

;;; unbind-key directive alias

(test unbind-key-alias-removes-binding
  "unbind-key is accepted as an alias for unbind and removes a binding."
  (with-isolated-config
    (is (eq :new-window (lookup-key-binding #\c))
        "#\\c should be bound to :new-window before unbind-key")
    (is (eq t (apply-config-directive '("unbind-key" "c")))
        "a valid unbind-key directive should return T")
    (is (null (lookup-key-binding #\c))
        "#\\c should be unbound after the unbind-key directive")))

;;; unbind with -n flag

(test unbind-with-n-flag-removes-root-binding
  "unbind -n removes a binding from the root table."
  (with-isolated-key-tables
    (apply-config-directive '("bind" "-n" "X" "new-window"))
    (let ((entry (cl-tmux/config:key-table-lookup "root" #\X)))
      (is (not (null entry)) "binding must exist before unbind -n"))
    (is (eq t (apply-config-directive '("unbind" "-n" "X")))
        "unbind -n must return T")
    (is (null (cl-tmux/config:key-table-lookup "root" #\X))
        "root binding must be removed after unbind -n")))

;;; unbind with -T flag

(test unbind-with-T-flag-removes-named-table-binding
  "unbind -T copy-mode removes a binding from the named table."
  (with-isolated-key-tables
    (apply-config-directive '("bind" "-T" "copy-mode" "q" "copy-mode-enter"))
    (let ((entry (cl-tmux/config:key-table-lookup "copy-mode" #\q)))
      (is (not (null entry)) "binding must exist before unbind -T"))
    (is (eq t (apply-config-directive '("unbind" "-T" "copy-mode" "q")))
        "unbind -T copy-mode must return T")
    (is (null (cl-tmux/config:key-table-lookup "copy-mode" #\q))
        "copy-mode binding must be removed after unbind -T")))

;;; %whitespace-p
;;; NOTE: set-option-directive-stores-option, set-window-option-directive-stores-option,
;;; sets-directive-stores-option, and set-session-option-directive-stores-option have
;;; been removed — all superseded by set-option-directive-aliases-table-driven (below).

(test whitespace-p-recognizes-space-and-tab
  "%whitespace-p returns T for space and tab, NIL for other chars."
  (is-true  (cl-tmux/config::%whitespace-p #\Space) "space is whitespace")
  (is-true  (cl-tmux/config::%whitespace-p #\Tab)   "tab is whitespace")
  (is-false (cl-tmux/config::%whitespace-p #\a)     "letter is not whitespace")
  (is-false (cl-tmux/config::%whitespace-p #\Newline) "newline is not whitespace"))

;;; %parse-bind-key-args edge cases

(test parse-bind-key-args-empty-returns-nil
  "%parse-bind-key-args with empty args list returns NIL."
  (is (null (cl-tmux/config::%parse-bind-key-args '()))
      "empty args must return NIL"))

(test parse-bind-key-args-T-flag-missing-table-returns-nil
  "%parse-bind-key-args with -T and no table name returns NIL."
  (is (null (cl-tmux/config::%parse-bind-key-args '("-T")))
      "-T with no following table name must return NIL"))

(test parse-bind-key-args-unknown-command-returns-nil
  "%parse-bind-key-args with an unknown command returns NIL."
  (is (null (cl-tmux/config::%parse-bind-key-args '("z" "unknown-bogus-command")))
      "unknown command must cause %parse-bind-key-args to return NIL"))

(test parse-bind-key-args-n-and-r-flags-combined
  "%parse-bind-key-args with -n -r binds in root table with repeatable."
  (multiple-value-bind (table key kw repeatable)
      (cl-tmux/config::%parse-bind-key-args '("-n" "-r" "z" "new-window"))
    (is (string= "root" table) "table must be root for -n flag")
    (is (char= #\z key)        "key must be #\\z")
    (is (eq :new-window kw)    "command must be :new-window")
    (is-true repeatable        "repeatable must be T for -r flag")))

;;; %parse-unbind-key-args edge cases

(test parse-unbind-key-args-empty-returns-nil-nil
  "%parse-unbind-key-args with empty args returns (values nil nil)."
  (multiple-value-bind (table key)
      (cl-tmux/config::%parse-unbind-key-args '())
    (is (null table) "table must be NIL for empty args")
    (is (null key)   "key must be NIL for empty args")))

(test parse-unbind-key-args-extra-arg-returns-nil-nil
  "%parse-unbind-key-args with extra trailing arg returns (values nil nil)."
  (multiple-value-bind (table key)
      (cl-tmux/config::%parse-unbind-key-args '("z" "extra"))
    (is (null table) "table must be NIL when extra arg present")
    (is (null key)   "key must be NIL when extra arg present")))

(test parse-unbind-key-args-T-flag-missing-table-returns-nil
  "%parse-unbind-key-args with -T and no table name returns (values nil nil)."
  (multiple-value-bind (table key)
      (cl-tmux/config::%parse-unbind-key-args '("-T"))
    (is (null table) "table must be NIL when -T has no table name")
    (is (null key)   "key must be NIL when -T has no table name")))

;;; Backslash-escape edge case: backslash at end of string

(test config-tokenizer-backslash-at-end-of-string
  "%config-tokens: backslash at the end of string does not signal an error."
  (finishes (cl-tmux/config::%config-tokens "token\\")))

;;; load-config-from-string: all-blank and all-comment input

(test load-from-string-blank-input-returns-zero
  "load-config-from-string with only blanks and comments returns 0."
  (with-isolated-config
    (let ((applied (load-config-from-string
                    (format nil "# comment~%~%   ~%# another~%"))))
      (is (= 0 applied)
          "blank/comment-only input must apply 0 directives, got ~A" applied))))

;;; bind -r -n combined (order-independent flags)

(test bind-key-r-then-n-flag
  "bind -r -n binds in root table and marks repeatable (flag order insensitive)."
  (with-isolated-key-tables
    (apply-config-directive '("bind" "-r" "-n" "G" "new-window"))
    (let ((entry (cl-tmux/config:key-table-lookup "root" #\G)))
      (is (not (null entry))
          "bind -r -n must add a binding to the root table")
      (is (eq :new-window (cl-tmux/config:key-table-command entry))
          "root table binding must be :new-window")
      (is (cl-tmux/config:key-table-repeatable-p entry)
          "binding must be repeatable when -r flag is present"))))

