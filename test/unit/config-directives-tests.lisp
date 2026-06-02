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

(defmacro with-isolated-key-tables (&body body)
  "Run BODY with a fresh *key-tables* and config isolation from
   with-isolated-config.  Prevents key-table mutations from leaking between tests."
  `(let ((cl-tmux/config:*key-tables* (make-hash-table :test #'equal)))
     (with-isolated-config
       ,@body)))

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
    (dolist (tokens '(("bind")
                      ("bind" "z" "new-window" "x")
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
    (let ((p (merge-pathnames "cl-tmux-test.conf" (uiop:temporary-directory))))
      (unwind-protect
           (progn
             (with-open-file (out p :direction :output
                                    :if-exists :supersede
                                    :if-does-not-exist :create)
               (write-line "bind z next-window" out)
               (write-line "set-status-height 3" out)
               (finish-output out))
             (is (= 2 (load-config-file p))
                 "load-config-file should apply and count both directives")
             (is (eq :next-window (lookup-key-binding #\z))
                 "#\\z should be bound to :next-window after loading the temp file")
             (is (= 3 *status-height*)
                 "*status-height* should be 3 after loading the temp file, got ~A"
                 *status-height*))
        (when (probe-file p)
          (delete-file p))))))

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
  (let ((tokens (cl-tmux/config::%config-tokens "foo\ bar")))
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
  (let ((tokens (cl-tmux/config::%config-tokens "a \"b c\" d\ e")))
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

;;; set directive

(test set-directive-stores-option
  "The set directive stores a value in the global options table."
  (let ((cl-tmux/options:*global-options* (make-hash-table :test #'equal)))
    (apply-config-directive '("set" "status-interval" "30"))
    (is (= 30 (cl-tmux/options:get-option "status-interval"))
        "set must store status-interval = 30 in global options")))

(test setw-directive-stores-option
  "The setw directive stores a value in the global options table."
  (let ((cl-tmux/options:*global-options* (make-hash-table :test #'equal)))
    (apply-config-directive '("setw" "status-interval" "5"))
    (is (= 5 (cl-tmux/options:get-option "status-interval"))
        "setw must store the option value")))

;;; bind-key directive alias

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

;;; set-option directive alias

(test set-option-directive-stores-option
  "The set-option directive (long form of set) stores a value in global options."
  (let ((cl-tmux/options:*global-options* (make-hash-table :test #'equal)))
    (apply-config-directive '("set-option" "status-interval" "20"))
    (is (= 20 (cl-tmux/options:get-option "status-interval"))
        "set-option must store status-interval = 20")))

(test set-window-option-directive-stores-option
  "The setw/set-window-option directive stores a value in global options."
  (let ((cl-tmux/options:*global-options* (make-hash-table :test #'equal)))
    (apply-config-directive '("set-window-option" "history-limit" "3000"))
    (is (= 3000 (cl-tmux/options:get-option "history-limit"))
        "set-window-option must store history-limit = 3000")))

(test sets-directive-stores-option
  "The sets directive (session-scoped alias) stores a value in global options."
  (let ((cl-tmux/options:*global-options* (make-hash-table :test #'equal)))
    (apply-config-directive '("sets" "status-interval" "10"))
    (is (= 10 (cl-tmux/options:get-option "status-interval"))
        "sets must store status-interval = 10")))

(test set-session-option-directive-stores-option
  "The set-session-option directive stores a value in global options."
  (let ((cl-tmux/options:*global-options* (make-hash-table :test #'equal)))
    (apply-config-directive '("set-session-option" "history-limit" "1500"))
    (is (= 1500 (cl-tmux/options:get-option "history-limit"))
        "set-session-option must store history-limit = 1500")))

;;; %whitespace-p

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
    (is repeatable             "repeatable must be T for -r flag")))

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
