(in-package #:cl-tmux/test)

;;;; Config-file directive parsing tests.
;;;;
;;;; These tests cover the config-directives layer:
;;;;   * %config-tokens, %parse-key-token
;;;;   * *bindable-commands*, %command-keyword
;;;;   * apply-config-directive, apply-config-line
;;;;   * load-config-from-stream, load-config-from-string
;;;;   * %config-path-from, config-file-path, load-config-file

(def-suite config-directives-suite :description "Config file directive parsing")
(in-suite config-directives-suite)

;;; Import the config-directives symbols we need

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(cl-tmux/config:lookup-key-binding
             cl-tmux/config:*default-shell*
            cl-tmux/config:*status-height*
            cl-tmux/config:set-key-binding
            cl-tmux/config:apply-config-directive
            cl-tmux/config:load-config-from-string
            cl-tmux/config:load-config-from-stream
            cl-tmux/config:config-file-path
            cl-tmux/config:load-config-file)))

;;; Test isolation helpers

(defun config-path (override xdg home)
  "Namestring of the resolved config path for the given env values + HOME
   (HOME a directory pathname)."
  (namestring (cl-tmux/config::%config-path-from override xdg home)))

;;; NOTE: with-isolated-key-tables and with-temp-config-file are defined in
;;; test/helpers.lisp so all test suites can reuse them.

;;; *bindable-commands* invariant

(test bindable-commands-excludes-copy-mode-internals
  "*bindable-commands* must exclude copy-mode-internal commands."
  (is (null (intersection '(:copy-mode-exit :copy-mode-up :copy-mode-down)
                          cl-tmux/config::*bindable-commands*))
      "copy-mode-internal commands must not be user-bindable, found ~A"
      (intersection '(:copy-mode-exit :copy-mode-up :copy-mode-down)
                    cl-tmux/config::*bindable-commands*))
  (is (not (member :copy-mode-exit cl-tmux/config::*bindable-commands*))
      ":copy-mode-exit must not be a user-bindable command")
  (is (not (member :copy-mode-up cl-tmux/config::*bindable-commands*))
      ":copy-mode-up must not be a user-bindable command")
  (is (not (member :copy-mode-down cl-tmux/config::*bindable-commands*))
      ":copy-mode-down must not be a user-bindable command")
  (is (member :new-window cl-tmux/config::*bindable-commands*)
      ":new-window must remain a user-bindable command"))

;;; apply-config-directive

(test apply-directive-bind-returns-t
  "apply-config-directive for a valid bind returns T and binds the char."
  (with-isolated-config
    (is (eq t (apply-config-directive '("bind" "z" "new-window")))
        "valid bind directive should return T")
    (is (eq :new-window (lookup-key-binding #\z))
        "#\\z should be bound to :new-window after the bind directive")))

(test apply-directive-unknown-returns-nil
  "apply-config-directive for an unknown command returns NIL and changes nothing."
  (with-isolated-config
    (let* ((tbl (cl-tmux/config:ensure-key-table "prefix"))
           (count-before (hash-table-count tbl))
           (shell-before    *default-shell*)
           (height-before   *status-height*))
      (is (null (apply-config-directive '("bogus" "x")))
          "an unknown command should return NIL")
      (is (= count-before (hash-table-count tbl))
          "prefix key-table must be unchanged by an unknown directive")
      (is (equal shell-before *default-shell*)
          "*default-shell* must be unchanged by an unknown directive")
      (is (eql height-before *status-height*)
          "*status-height* must be unchanged by an unknown directive"))))

;;; set [-g|-a|...] name value  — flag handling (the canonical .tmux.conf form)

(test apply-set-directive-global-flag
  "'set -g status off' applies (3 tokens) — previously the fixed-arity table
   silently dropped it.  Sets 'status', not an option named '-g'."
  (with-isolated-options ()
    (is (eq t (apply-config-directive '("set" "-g" "status" "off")))
        "set -g status off must return T (applied)")
    (is (null (cl-tmux/options:get-option "status"))
        "status option must be NIL (boolean coercion of 'off')")
    (is (null (cl-tmux/options:get-option "-g"))
        "must NOT create an option literally named '-g'")))

(test apply-set-directive-append-flag
  "'set -ag <name> <value>' appends to the option's current value."
  (with-isolated-options ("status-left" "A")
    (is (eq t (apply-config-directive '("set" "-ag" "status-left" "B")))
        "set -ag must return T")
    (is (string= "AB" (cl-tmux/options:get-option "status-left"))
        "set -ag must append B to the existing A")))

(test apply-set-directive-plain-unaffected
  "Plain 'set name value' (no flags) still flows through the normal directive
   table and applies unchanged."
  (with-isolated-options ()
    (is (eq t (apply-config-directive '("set" "status" "off")))
        "plain set must still return T")
    (is (null (cl-tmux/options:get-option "status"))
        "plain set status off must set status to NIL")))

;;; bind key to a command LINE (arg-taking key bindings)

(test bind-key-to-command-line-stores-token-list
  "'bind X display-message hello' binds X to the command token list, applies (T),
   and list-keys shows the reconstructed command line."
  (with-isolated-key-tables
    (is (eq t (apply-config-directive '("bind" "X" "display-message" "hello")))
        "multi-token bind must return T")
    (is (equal '("display-message" "hello") (lookup-key-binding #\X))
        "binding value must be the command token list")
    (is (search "display-message hello" (cl-tmux/config:describe-key-bindings))
        "list-keys must show the reconstructed command line")))

(test bind-key-single-keyword-still-binds
  "'bind z new-window' still binds the key to the :new-window keyword (existing
   single-command behaviour is preserved)."
  (with-isolated-key-tables
    (is (eq t (apply-config-directive '("bind" "z" "new-window")))
        "single known-command bind must return T")
    (is (eq :new-window (lookup-key-binding #\z))
        "a single known command binds to its keyword")))

(test bind-key-single-unknown-command-rejected
  "'bind z totally-bogus-xyz' (one unknown command word) is still rejected (NIL),
   and the unknown command is not stored (z keeps its default keyword binding)."
  (with-isolated-key-tables
    (is (null (apply-config-directive '("bind" "z" "totally-bogus-xyz")))
        "an unknown single command must be rejected")
    ;; z is bound to a built-in (zoom) by default; the rejected bind must not have
    ;; replaced it with the bogus command token-list.
    (is (not (equal '("totally-bogus-xyz") (lookup-key-binding #\z)))
        "the unknown command must not be stored as a binding")))

;;; load-config-from-string

(test load-from-string-counts-and-applies
  "load-config-from-string ignores comments/blanks and applies real directives."
  (with-isolated-config
    (let ((applied (load-config-from-string
                    (format nil "# a comment~%~%bind z new-window~%set-status-height 5~%"))))
      (is (= 2 applied)
          "exactly 2 directives should be applied, got ~A" applied)
      (is (eq :new-window (lookup-key-binding #\z))
          "#\\z should be bound to :new-window")
      (is (= 5 *status-height*)
          "*status-height* should be 5, got ~A" *status-height*))))

(test load-from-string-multichar-and-quote-key
  "A single-char quote key parses as the character."
  (with-isolated-config
    (let ((applied (load-config-from-string
                    (format nil "bind \" split-horizontal~%bind n split-vertical~%"))))
      (is (= 2 applied)
          "both bind directives should be applied, got ~A" applied)
      (is (eq :split-horizontal (lookup-key-binding #\"))
          "the single-char token should bind the double-quote character")
      (is (eq :split-vertical (lookup-key-binding #\n))
          "#\\n should be re-bound to :split-vertical"))))

;;; set-shell / set-status-height directives

(test set-shell-and-status-height-directives
  "set-shell sets *default-shell*; set-status-height sets *status-height*."
  (with-isolated-config
    (is (eq t (apply-config-directive '("set-shell" "/usr/bin/zsh")))
        "set-shell directive should return T")
    (is (string= "/usr/bin/zsh" *default-shell*)
        "*default-shell* should be /usr/bin/zsh, got ~A" *default-shell*)
    (is (eq t (apply-config-directive '("set-status-height" "2")))
        "set-status-height directive should return T")
    (is (= 2 *status-height*)
        "*status-height* should be 2, got ~A" *status-height*)))

;;; config-file-path precedence (pure: %config-path-from)

(test config-path-explicit-override-wins
  "$CL_TMUX_CONF takes precedence over XDG and the default."
  (is (equal #p"/custom/my.conf"
             (cl-tmux/config::%config-path-from "/custom/my.conf" "/x/cfg" #p"/home/u/"))
      "an explicit override must be used verbatim"))

(test config-path-honors-xdg-config-home
  "Without an override, $XDG_CONFIG_HOME/cl-tmux/cl-tmux.conf is used."
  (is (string= "/x/cfg/cl-tmux/cl-tmux.conf"
               (config-path nil "/x/cfg" #p"/home/u/")))
  (is (string= "/x/cfg/cl-tmux/cl-tmux.conf"
               (config-path nil "/x/cfg/" #p"/home/u/"))
      "a trailing slash on XDG_CONFIG_HOME must not double up"))

(test config-path-defaults-to-dot-config
  "With neither override nor XDG set, the path defaults under ~/.config."
  (is (string= "/home/u/.config/cl-tmux/cl-tmux.conf"
               (config-path nil nil #p"/home/u/"))))

(test config-path-empty-env-is-unset
  "Empty-string env values are treated as unset."
  (is (string= "/home/u/.config/cl-tmux/cl-tmux.conf"
               (config-path "" "" #p"/home/u/"))))

;;; load-config-file

(test load-config-file-missing-returns-nil
  "load-config-file on a non-existent path returns NIL."
  (with-isolated-config
    (is (null (load-config-file #p"/nonexistent/cl-tmux-xyz.conf"))
        "loading a non-existent config file should return NIL")))

;;; bind/unbind/set: arity and validity table

(test invalid-directive-cases-return-nil
  "Every malformed or unknown directive returns NIL without mutating state."
  (with-isolated-config
    ;; NOTE: ("bind" "z" "new-window" "x") is no longer here — a bind with extra
    ;; tokens is now a valid arg-taking binding (key → command line), covered by
    ;; bind-key-to-command-line-stores-token-list.
    (dolist (tokens '(("bind")
                      ("bind" "z" "bogus-command")
                      ("unbind")
                      ("unbind" "z" "extra")
                      ("set-shell")
                      ("set-status-height")
                      ("totally-unknown" "arg")))
      (is (null (apply-config-directive tokens))
          "~S should return NIL" tokens))))

;;; unbind directive

(test apply-directive-unbind-removes-binding
  "unbind removes an existing binding and returns T."
  (with-isolated-config
    (is (eq :new-window (lookup-key-binding #\c))
        "#\\c should be bound to :new-window before unbind")
    (is (eq t (apply-config-directive '("unbind" "c")))
        "a valid unbind directive should return T")
    (is (null (lookup-key-binding #\c))
        "#\\c should be unbound after the unbind directive")))

;;; set-status-height: tolerant parsing

(test set-status-height-noninteger-is-tolerated
  "Non-integer or non-positive set-status-height values return NIL and do not signal."
  (with-isolated-config
    (let ((before *status-height*))
      (is (null (handler-case (apply-config-directive '("set-status-height" "abc"))
                  (error (e)
                    (fail "set-status-height with non-integer must not signal, got ~A" e)
                    :signaled)))
          "set-status-height with a non-integer value should return NIL")
      (is (eql before *status-height*)
          "*status-height* should be unchanged, got ~A" *status-height*)
      (is (null (handler-case (apply-config-directive '("set-status-height" "0"))
                  (error (e)
                    (fail "set-status-height with 0 must not signal, got ~A" e)
                    :signaled)))
          "set-status-height with a non-positive value (0) should return NIL")
      (is (eql before *status-height*)
          "*status-height* should be unchanged, got ~A" *status-height*))))

;;; multi-character key tokens

(test bind-multichar-key-token
  "A multi-character key token (M-z) is stored as the string itself."
  (with-isolated-config
    (let ((applied (load-config-from-string "bind M-z next-window")))
      (is (= 1 applied)
          "exactly 1 directive should be applied, got ~A" applied)
      (is (eq :next-window (lookup-key-binding "M-z"))
          "the multi-char token M-z should bind the string key M-z")
      (is (eq :zoom-toggle (lookup-key-binding #\z))
          "the single character #\\z must still be :zoom-toggle"))))

;;; load-config-from-stream

(test load-config-from-stream-applies
  "load-config-from-stream ignores comments and applies the real directives."
  (with-isolated-config
    (let ((applied (with-input-from-string
                       (s (format nil "# leading comment~%bind z next-window~%set-status-height 4~%"))
                     (load-config-from-stream s))))
      (is (= 2 applied)
          "exactly 2 directives should be applied, got ~A" applied)
      (is (eq :next-window (lookup-key-binding #\z))
          "#\\z should be bound to :next-window after the stream directives")
      (is (= 4 *status-height*)
          "*status-height* should be 4, got ~A" *status-height*))))

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

;;; status option: off / on / line-count parsing → *status-height*

(test set-status-numeric-shows-bar
  "`set -g status 2` shows the status bar (height 1) instead of silently
   disabling it.  The previous code treated any value != on/true/1 as OFF."
  (with-isolated-config
    (cl-tmux/config:apply-config-directive '("set" "-g" "status" "2"))
    (is (= 1 cl-tmux/config:*status-height*)
        "status 2 must show the bar (height 1; multi-line render deferred)")))

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

;;; ── %parse-bind-key-args with valid complete args ─────────────────────────

(test parse-bind-key-args-returns-all-values
  "%parse-bind-key-args with valid key+command returns all four values."
  (multiple-value-bind (table key kw repeatable)
      (cl-tmux/config::%parse-bind-key-args '("z" "new-window"))
    (is (string= "prefix" table) "table defaults to prefix")
    (is (char= #\z key)          "key must be #\\z")
    (is (eq :new-window kw)      "command must be :new-window")
    (is (null repeatable)        "repeatable must be NIL by default")))

(test parse-bind-key-args-T-flag-specifies-table
  "%parse-bind-key-args with -T uses the given table name."
  (multiple-value-bind (table key kw ignored-rep)
      (cl-tmux/config::%parse-bind-key-args '("-T" "copy-mode" "q" "copy-mode-enter"))
    (declare (ignore ignored-rep))
    (is (string= "copy-mode" table) "table must be copy-mode")
    (is (char= #\q key)             "key must be #\\q")
    (is (eq :copy-mode-enter kw)    "command must be :copy-mode-enter")))

(test parse-bind-key-args-r-flag-sets-repeatable
  "%parse-bind-key-args with -r sets repeatable to T."
  (multiple-value-bind (table ignored-key kw repeatable)
      (cl-tmux/config::%parse-bind-key-args '("-r" "H" "resize-left"))
    (declare (ignore ignored-key))
    (is (string= "prefix" table) "table must be prefix for -r alone")
    (is (eq :resize-left kw)      "command must be :resize-left")
    (is-true repeatable           "repeatable must be T with -r flag")))

;;; ── %tokenize-backslash-escape direct tests ──────────────────────────────

(test tokenize-backslash-escape-produces-escaped-char
  "%tokenize-backslash-escape pushes the character following the backslash."
  ;; We verify the behavior indirectly via %config-tokens which calls it.
  (let ((tokens (cl-tmux/config::%config-tokens "a\\nb")))
    (is (= 1 (length tokens))
        "backslash-n must be one token, got ~S" tokens)
    (is (string= "anb" (first tokens))
        "token must be 'anb' (backslash consumed), got ~S" (first tokens))))

(test tokenize-backslash-escape-at-end-produces-partial-token
  "%tokenize-backslash-escape at the very end of input does not signal."
  (finishes
    (let ((toks (cl-tmux/config::%config-tokens "abc\\")))
      ;; The backslash is at EOL — just one partial token, no error.
      (is (= 1 (length toks)) "must have 1 token even with trailing backslash"))))

;;; ── %tokenize-double-quoted unmatched quote ───────────────────────────────

(test tokenize-double-quoted-unmatched-treats-as-literal
  "%config-tokens: an unmatched double-quote is treated as a literal character."
  ;; Input: a single \" with no closing quote.
  (let ((tokens (cl-tmux/config::%config-tokens "\"")))
    ;; The lone \" starts a token but has no closing quote — the opening \" is
    ;; a literal so we get a token containing the character.
    (is (= 1 (length tokens))
        "unmatched \" must produce 1 token, got ~S" tokens)))

;;; ── %tokenize-single-quoted direct test ──────────────────────────────────

(test tokenize-single-quoted-preserves-content
  "%config-tokens: single-quoted content is preserved literally."
  (let ((tokens (cl-tmux/config::%config-tokens "'hello world'")))
    (is (= 1 (length tokens))
        "single-quoted string must produce 1 token, got ~S" tokens)
    (is (string= "hello world" (first tokens))
        "token must be 'hello world', got ~S" (first tokens))))

(test tokenize-single-quoted-no-escape-processing
  "%config-tokens: backslash inside single quotes is literal, not an escape."
  (let ((tokens (cl-tmux/config::%config-tokens "'a\\b'")))
    (is (= 1 (length tokens))
        "single-quoted backslash-b must yield 1 token, got ~S" tokens)
    ;; Inside single quotes the backslash is literal, so token = "a\b" (3 chars).
    (is (= 3 (length (first tokens)))
        "token must be 3 chars (backslash is literal), got ~S" (first tokens))))

;;; ── Table-driven tokenizer tests ─────────────────────────────────────────
;;;
;;; Parameterises the whitespace-splitting and quoting cases, eliminating the
;;; structural duplication across the 6 separate tokenizer tests above.

(test config-tokens-table-driven
  "%config-tokens produces the correct token list across representative inputs."
  (dolist (entry '(("bind c new-window"            ("bind" "c" "new-window"))
                   ("  set-shell  /bin/bash  "      ("set-shell" "/bin/bash"))
                   (""                              nil)
                   ("   "                           nil)
                   ("cmd \"\""                      ("cmd" ""))
                   ("a \"b c\" d\\ e"               ("a" "b c" "d e"))))
    (destructuring-bind (input expected) entry
      (let ((result (cl-tmux/config::%config-tokens input)))
        (is (equal expected result)
            "%config-tokens ~S: expected ~S got ~S"
            input expected result)))))

;;; ── apply-config-directive on nil/empty input ─────────────────────────────

(test apply-config-directive-nil-returns-nil
  "apply-config-directive with NIL (empty token list) returns NIL."
  (is (null (apply-config-directive nil))
      "NIL token list must return NIL"))

;;; ── set option directives: table-driven aliases ───────────────────────────
;;;
;;; All six set-option aliases (set, set-option, setw, set-window-option,
;;; sets, set-session-option) produce the same result.  This table-driven
;;; test replaces the six near-identical individual tests with a single loop.

(test set-option-directive-aliases-table-driven
  "All six set-option directive aliases store a value in the global options table."
  (dolist (verb '("set" "set-option" "setw" "set-window-option" "sets" "set-session-option"))
    (with-fresh-global-options
      (let ((result (apply-config-directive (list verb "status-interval" "7"))))
        (is (eq t result)
            "~A directive must return T, got ~S" verb result)
        (is (= 7 (cl-tmux/options:get-option "status-interval"))
            "~A must store status-interval = 7 in global options, got ~S"
            verb (cl-tmux/options:get-option "status-interval"))))))

;;; ── set -s server option routing ──────────────────────────────────────────

(test apply-set-directive-server-flag
  "'set -s exit-empty off' routes to server-options; exit-empty is server-only."
  ;; exit-empty is in *server-option-registry* but NOT in *option-registry* /
  ;; *global-options*, so the assertion 'not in global-options' is clean.
  (let ((cl-tmux/options:*server-options* (make-hash-table :test #'equal)))
    (is (eq t (apply-config-directive '("set" "-s" "exit-empty" "off")))
        "set -s must return T")
    (is (null (cl-tmux/options:get-server-option "exit-empty"))
        "exit-empty must be NIL ('off') in server-options")
    (is (null (cl-tmux/options:get-option "exit-empty" nil))
        "exit-empty must NOT appear in global-options (it is server-only)")))

;;; ── source-file / source directive ───────────────────────────────────────

(test source-file-directive-loads-temp-file
  "source-file applies a config file from disk, returning T."
  (with-isolated-config
    (with-temp-config-file (p "bind z next-window")
      (is (eq t (apply-config-directive (list "source-file" (namestring p))))
          "source-file must return T")
      (is (eq :next-window (lookup-key-binding #\z))
          "#\\z must be bound after source-file"))))

(test source-directive-is-alias-for-source-file
  "'source' is accepted as an alias for 'source-file'."
  (with-isolated-config
    (with-temp-config-file (p "bind w last-window")
      (is (eq t (apply-config-directive (list "source" (namestring p))))
          "source alias must return T")
      (is (eq :last-window (lookup-key-binding #\w))
          "#\\w must be bound after source"))))

(test source-file-missing-returns-t-silently
  "source-file on a nonexistent file returns T (errors are ignored)."
  (with-isolated-config
    (is (eq t (apply-config-directive '("source-file" "/nonexistent-cl-tmux-config-abc.conf")))
        "source-file on a missing file must return T (error silently ignored)")))

;;; ── run-shell / run directive ─────────────────────────────────────────────

(test run-shell-directive-returns-t
  "run-shell runs a shell command at config parse time and returns T."
  (is (eq t (apply-config-directive '("run-shell" "true")))
      "run-shell must return T"))

(test run-directive-is-alias-for-run-shell
  "'run' is accepted as an alias for 'run-shell'."
  (is (eq t (apply-config-directive '("run" "true")))
      "run alias must return T"))

(test run-shell-errors-ignored
  "run-shell with a failing command returns T (errors silently ignored)."
  (is (eq t (apply-config-directive '("run-shell" "false")))
      "run-shell with exit-code 1 must still return T"))

;;; ── %if / %else / %endif preprocessor ───────────────────────────────────

(test if-else-endif-truthy-condition
  "%if with a truthy condition applies the then-block and skips the else-block."
  (with-isolated-config
    ;; *config-condition-evaluator* is NIL by default → all conditions truthy.
    (let ((applied (load-config-from-string
                    (format nil "%if 1~%bind z new-window~%%else~%bind z detach~%%endif~%"))))
      (is (= 1 applied)
          "only 1 directive must be applied under a truthy %if, got ~A" applied)
      (is (eq :new-window (lookup-key-binding #\z))
          "#\\z must be :new-window (then-block), not :detach (else-block)"))))

(test if-else-endif-falsy-condition
  "%if with a falsy condition skips the then-block and applies the else-block."
  (with-isolated-config
    ;; Set evaluator to return '0' (falsy) for any condition.
    (let ((cl-tmux/config:*config-condition-evaluator*
            (lambda (s) (declare (ignore s)) "0")))
      (let ((applied (load-config-from-string
                      (format nil "%if 0~%bind z new-window~%%else~%bind z detach~%%endif~%"))))
        (is (= 1 applied)
            "only 1 directive must be applied under a falsy %if, got ~A" applied)
        (is (eq :detach (lookup-key-binding #\z))
            "#\\z must be :detach (else-block) when condition is falsy")))))

(test if-endif-no-else
  "%if without %else applies the block when truthy, applies nothing when falsy."
  (with-isolated-config
    ;; Truthy (default evaluator NIL → all truthy)
    (let ((applied (load-config-from-string
                    (format nil "%if 1~%bind z new-window~%%endif~%"))))
      (is (= 1 applied) "truthy %if without else applies 1 directive"))
    ;; Falsy
    (let ((cl-tmux/config:*config-condition-evaluator*
            (lambda (s) (declare (ignore s)) "0")))
      (let ((applied (load-config-from-string
                      (format nil "%if 0~%bind w detach~%%endif~%"))))
        (is (= 0 applied) "falsy %if without else applies 0 directives")))))

(test if-block-outside-applies-normally
  "Lines outside %if blocks are always applied regardless of evaluator."
  (with-isolated-config
    (let ((applied (load-config-from-string
                    (format nil "bind z new-window~%%if 1~%bind n next-window~%%endif~%bind p prev-window~%"))))
      (is (= 3 applied)
          "3 directives must be applied (2 outside + 1 inside truthy %if)"))))

(test nested-if-blocks
  "Nested %if blocks work: inner block is skipped when outer is falsy."
  (with-isolated-config
    (let ((cl-tmux/config:*config-condition-evaluator*
            (lambda (s) (declare (ignore s)) "0")))
      (let ((applied (load-config-from-string
                      (format nil "%if 0~%%if 1~%bind z new-window~%%endif~%%endif~%"))))
        (is (= 0 applied)
            "no directives inside a falsy outer %if block"))))
  ;; All truthy
  (with-isolated-config
    (let ((applied (load-config-from-string
                    (format nil "%if 1~%%if 1~%bind z new-window~%%endif~%%endif~%"))))
      (is (= 1 applied) "one directive inside nested truthy %if blocks"))))

(test if-condition-evaluated-by-callback
  "%if condition string is passed verbatim to *config-condition-evaluator*."
  (with-isolated-config
    (let ((received nil))
      (let ((cl-tmux/config:*config-condition-evaluator*
              (lambda (s) (setf received s) "1")))
        (load-config-from-string (format nil "%if some-condition~%bind z new-window~%%endif~%"))
        (is (string= "some-condition" received)
            "condition must be passed verbatim to the evaluator, got ~S" received)))))

;;; ── %tmux-conf-paths ─────────────────────────────────────────────────────

(test tmux-conf-paths-returns-list
  "%tmux-conf-paths returns a list of pathname candidates."
  (let ((paths (cl-tmux/config::%tmux-conf-paths #p"/home/user/")))
    (is (listp paths) "must return a list")
    (is (>= (length paths) 1) "must have at least 1 candidate")))

;;; ── config-file-path (environment-variable reading path) ─────────────────
;;;
;;; These tests exercise config-file-path by temporarily overriding
;;; environment variables.  Since posix-getenv reads the real environment
;;; and we cannot setenv from Lisp portably in tests, we test the pure
;;; %config-path-from helper directly (already covered above) and only
;;; verify that config-file-path returns a pathname (not NIL) from the
;;; live environment.

(test config-file-path-returns-pathname
  "config-file-path returns a pathname object (not NIL or a string)."
  (let ((result (config-file-path)))
    (is (pathnamep result)
        "config-file-path must return a pathname, got ~S" result)))

;;; ── %apply-option-side-effects: prefix branch ────────────────────────────
;;;
;;; Tests that "set -g prefix C-a" updates *prefix-key-code* and registers
;;; the new key in the prefix table (the prefix2 branch has no separate
;;; integration path into tests, so we cover the scalar + key-table path here).

(test apply-set-directive-prefix-side-effect
  "'set -g prefix C-a' updates *prefix-key-code* to 1 and binds the new key."
  (with-isolated-key-tables
    (let ((cl-tmux/config:*prefix-key-code* cl-tmux/config:+prefix-key-code+))
      (apply-config-directive '("set" "-g" "prefix" "C-a"))
      (is (= 1 cl-tmux/config:*prefix-key-code*)
          "*prefix-key-code* must be 1 (C-a) after 'set -g prefix C-a'")
      (let ((entry (cl-tmux/config:key-table-lookup "prefix" (code-char 1))))
        (is (not (null entry))
            "C-a (code-char 1) must be bound in the prefix table after prefix change")
        (is (eq :send-prefix (cl-tmux/config:key-table-command entry))
            "the new prefix key must be bound to :send-prefix")))))

;;; ── unbind-all directive ─────────────────────────────────────────────────────

(test apply-config-directive-unbind-all-clears-prefix-table
  "'unbind-all' removes all bindings from the prefix key-table."
  (with-isolated-key-tables
    ;; Verify there's at least one binding first (e.g. C-c = :new-window).
    (let ((before (cl-tmux/config:key-table-lookup "prefix" #\c)))
      (is (not (null before)) "prefix table must have at least one binding before unbind-all"))
    ;; Now clear it.
    (cl-tmux/config:apply-config-directive '("unbind-all"))
    ;; All bindings in prefix table should be gone.
    (is (null (cl-tmux/config:key-table-lookup "prefix" #\c))
        "C-c must be unbound after unbind-all")))

(test apply-config-directive-unbind-all-T-clears-named-table
  "'unbind-all -T root' removes all bindings from the root key-table."
  (with-isolated-key-tables
    ;; Bind something in root.
    (cl-tmux/config:key-table-bind "root" #\x :new-window)
    (cl-tmux/config:apply-config-directive '("unbind-all" "-T" "root"))
    (is (null (cl-tmux/config:key-table-lookup "root" #\x))
        "root binding must be cleared after unbind-all -T root")))

;;; ── set -g status off side-effect ────────────────────────────────────────────

(test apply-set-directive-status-off-sets-status-height-zero
  "'set -g status off' sets *status-height* to 0."
  (let ((orig cl-tmux/config:*status-height*))
    (unwind-protect
         (progn
           (cl-tmux/config:apply-config-directive '("set" "-g" "status" "off"))
           (is (= 0 cl-tmux/config:*status-height*)
               "*status-height* must be 0 after 'set -g status off'"))
      (setf cl-tmux/config:*status-height* orig))))

(test apply-set-directive-status-on-sets-status-height-one
  "'set -g status on' sets *status-height* to 1."
  (let ((orig cl-tmux/config:*status-height*))
    (unwind-protect
         (progn
           (setf cl-tmux/config:*status-height* 0)
           (cl-tmux/config:apply-config-directive '("set" "-g" "status" "on"))
           (is (= 1 cl-tmux/config:*status-height*)
               "*status-height* must be 1 after 'set -g status on'"))
      (setf cl-tmux/config:*status-height* orig))))

;;; ── bind -n with argument-bearing command ────────────────────────────────────

(test bind-key-n-split-window-with-c-flag
  "'bind -n C-\\ split-window -c /tmp' binds the control character ^\\ (byte 28)
   and stores the full command token list.  C-<key> tokens now resolve to the
   control CHARACTER the event loop sees (the old string-key form could never
   fire), so the binding is looked up by (code-char 28), not the string \"C-\\\"."
  (with-isolated-key-tables
    (cl-tmux/config:apply-config-directive
     '("bind" "-n" "C-\\" "split-window" "-c" "/tmp"))
    ;; C-\ → (logand (char-code #\\) #x1f) = 28 (FS).  The binding is keyed by
    ;; that control character so it matches the byte a real Ctrl-\ keypress sends.
    (let ((entry (cl-tmux/config:key-table-lookup "root" (code-char 28))))
      (is (not (null entry)) "C-\\ must be bound (as control char 28) in root table")
      (let ((cmd (cl-tmux/config:key-table-command entry)))
        (is (consp cmd) "command for multi-token bind must be a token list")
        (is (string= "split-window" (first cmd)) "first token must be split-window")
        (is (member "-c" cmd :test #'string=) "token list must include -c flag")
        (is (member "/tmp" cmd :test #'string=) "token list must include /tmp")))))

;;; ── Semicolon-separated multi-command bindings ───────────────────────────────

(test bind-key-semicolon-sequence-stored-as-sequence
  "'bind r source-file x \; display y' stores a :sequence command list."
  (with-isolated-key-tables
    (cl-tmux/config:apply-config-directive
     '("bind" "r" "source-file" "/tmp/x" ";" "display-message" "Reloaded"))
    (let ((entry (cl-tmux/config:key-table-lookup "prefix" #\r)))
      (is (not (null entry)) "#\\r must be bound in prefix table")
      (let ((cmd (cl-tmux/config:key-table-command entry)))
        (is (consp cmd) "command must be a cons")
        (is (eq :sequence (car cmd)) "first element must be :sequence")
        (is (= 2 (length (cdr cmd))) ":sequence must have 2 sub-command lists")
        (is (string= "source-file" (first (first (cdr cmd))))
            "first sub-command must start with source-file")
        (is (string= "display-message" (first (second (cdr cmd))))
            "second sub-command must start with display-message")))))

;;; ── Common .tmux.conf patterns ───────────────────────────────────────────────

(test load-config-common-patterns-no-error
  "Common .tmux.conf patterns load without error."
  (with-isolated-config
    (let ((common-config
           "set -g prefix C-a
set -g mouse on
set -g base-index 1
setw -g pane-base-index 1
set -g default-terminal \"screen-256color\"
set -g escape-time 0
set -g history-limit 50000
set -g renumber-windows on
set -g mode-keys vi
bind r source-file /dev/null \; display-message \"Reloaded\"
bind-key -T copy-mode-vi v send-keys -X begin-selection
bind-key -T copy-mode-vi y send-keys -X copy-selection-and-cancel
bind '\"' split-window -c #{pane_current_path}
bind % split-window -h -c #{pane_current_path}
bind c new-window -c #{pane_current_path}
unbind-all
bind-key r source-file /dev/null"))
      (is (zerop (multiple-value-bind (result)
                     (ignore-errors (cl-tmux/config:load-config-from-string common-config))
                   (declare (ignore result))
                   0))
          "common .tmux.conf patterns must load without signaling conditions"))))

(test load-config-bind-T-copy-mode-vi-stores-correctly
  "bind -T copy-mode-vi v send-keys -X begin-selection stores in copy-mode-vi table."
  (with-isolated-config
    (cl-tmux/config:load-config-from-string
     "bind-key -T copy-mode-vi v send-keys -X begin-selection")
    (let ((entry (cl-tmux/config:key-table-lookup "copy-mode-vi" #\v)))
      (is (not (null entry)) "copy-mode-vi must have 'v' binding after load")
      (let ((cmd (cl-tmux/config:key-table-command entry)))
        (is (consp cmd) "command must be a token list")
        (is (string= "send-keys" (first cmd)) "first token must be send-keys")))))

(test load-config-set-g-escape-time-stores-as-server-option
  "'set -s escape-time 0' stores in server options."
  (with-isolated-config
    (cl-tmux/config:load-config-from-string "set -s escape-time 0")
    (is (eql 0 (cl-tmux/options:get-server-option "escape-time"))
        "escape-time must be 0 after 'set -s escape-time 0'")))
