(in-package #:cl-tmux/test)

;;;; Config-file directive parsing tests.
;;;;
;;;; These tests cover the config-directives layer:
;;;;   • %config-tokens, %parse-key-token
;;;;   • *bindable-commands*, %command-keyword
;;;;   • apply-config-directive, apply-config-line
;;;;   • load-config-from-stream, load-config-from-string
;;;;   • %config-path-from, config-file-path, load-config-file

(def-suite config-directives-suite :description "Config file directive parsing")
(in-suite config-directives-suite)

;;; ── Import the config-directives symbols we need ─────────────────────────

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(cl-tmux/config:lookup-key-binding
            cl-tmux/config:*key-bindings*
            cl-tmux/config:*default-shell*
            cl-tmux/config:*status-height*
            cl-tmux/config:set-key-binding
            cl-tmux/config:apply-config-directive
            cl-tmux/config:load-config-from-string
            cl-tmux/config:load-config-from-stream
            cl-tmux/config:config-file-path
            cl-tmux/config:load-config-file)))

;;; ── Helper ────────────────────────────────────────────────────────────────

(defun config-path (override xdg home)
  "Namestring of the resolved config path for the given env values + HOME
   (HOME a directory pathname)."
  (namestring (cl-tmux/config::%config-path-from override xdg home)))

;;; ── *bindable-commands* invariant ─────────────────────────────────────────

(test bindable-commands-excludes-copy-mode-internals
  "*bindable-commands* is the USER-bindable subset and must exclude the three
   copy-mode-internal commands (:copy-mode-exit/-up/-down), while still
   containing a genuinely bindable command such as :new-window."
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

;;; ── apply-config-directive ─────────────────────────────────────────────────

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
    (let ((bindings-before (copy-alist *key-bindings*))
          (shell-before    *default-shell*)
          (height-before   *status-height*))
      (is (null (apply-config-directive '("bogus" "x")))
          "an unknown command should return NIL")
      (is (equal bindings-before *key-bindings*)
          "*key-bindings* must be unchanged by an unknown directive")
      (is (equal shell-before *default-shell*)
          "*default-shell* must be unchanged by an unknown directive")
      (is (eql height-before *status-height*)
          "*status-height* must be unchanged by an unknown directive"))))

;;; ── load-config-from-string ────────────────────────────────────────────────

(test load-from-string-counts-and-applies
  "load-config-from-string ignores comments/blanks and applies real directives."
  (with-isolated-config
    (let ((applied (load-config-from-string
                    (format nil "# a comment~%~%bind z new-window~%set-status-height 5~%"))))
      (is (= 2 applied)
          "exactly 2 directives should be applied (bind + set-status-height), got ~A"
          applied)
      (is (eq :new-window (lookup-key-binding #\z))
          "#\\z should be bound to :new-window")
      (is (= 5 *status-height*)
          "*status-height* should be 5, got ~A" *status-height*))))

(test load-from-string-multichar-and-quote-key
  "A single-char #\\\" key parses as the character, and a hyphenated command
   name maps to its keyword."
  (with-isolated-config
    (let ((applied (load-config-from-string
                    (format nil "bind \" split-horizontal~%bind n split-vertical~%"))))
      (is (= 2 applied)
          "both bind directives should be applied, got ~A" applied)
      (is (eq :split-horizontal (lookup-key-binding #\"))
          "the single-char token \" should bind the #\\\" character")
      (is (eq :split-vertical (lookup-key-binding #\n))
          "#\\n should be re-bound to :split-vertical"))))

;;; ── set-shell / set-status-height directives ───────────────────────────────

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

;;; ── config-file-path precedence (pure: %config-path-from) ───────────────────

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
               (config-path nil "/x/cfg/" #p"/home/u/"))   ; trailing slash tolerated
      "a trailing slash on XDG_CONFIG_HOME must not double up"))

(test config-path-defaults-to-dot-config
  "With neither override nor XDG set, the path defaults under ~/.config."
  (is (string= "/home/u/.config/cl-tmux/cl-tmux.conf"
               (config-path nil nil #p"/home/u/"))))

(test config-path-empty-env-is-unset
  "Empty-string env values are treated as unset (fall through to the default)."
  (is (string= "/home/u/.config/cl-tmux/cl-tmux.conf"
               (config-path "" "" #p"/home/u/"))))

;;; ── load-config-file ───────────────────────────────────────────────────────

(test load-config-file-missing-returns-nil
  "load-config-file on a non-existent path returns NIL."
  (with-isolated-config
    (is (null (load-config-file #p"/nonexistent/cl-tmux-xyz.conf"))
        "loading a non-existent config file should return NIL")))

;;; ── bind/unbind/set: arity and validity table ──────────────────────────────

(test invalid-directive-cases-return-nil
  "Every malformed or unknown directive returns NIL without mutating state."
  (with-isolated-config
    (dolist (tokens '(("bind")                         ; too few args
                      ("bind" "z" "new-window" "x")   ; too many args
                      ("bind" "z" "bogus-command")     ; unrecognised command
                      ("unbind")                       ; missing key
                      ("unbind" "z" "extra")           ; too many args
                      ("set-shell")                    ; missing path
                      ("set-status-height")            ; missing value
                      ("totally-unknown" "arg")))      ; unknown command name
      (is (null (apply-config-directive tokens))
          "~S should return NIL" tokens))))

;;; ── unbind directive ───────────────────────────────────────────────────────

(test apply-directive-unbind-removes-binding
  "unbind removes an existing binding and returns T."
  (with-isolated-config
    (is (eq :new-window (lookup-key-binding #\c))
        "#\\c should be bound to :new-window before unbind")
    (is (eq t (apply-config-directive '("unbind" "c")))
        "a valid unbind directive should return T")
    (is (null (lookup-key-binding #\c))
        "#\\c should be unbound after the unbind directive")))

;;; ── set-status-height: tolerant parsing ────────────────────────────────────

(test set-status-height-noninteger-is-tolerated
  "Non-integer or non-positive set-status-height values return NIL, do not signal,
   and leave *status-height* unchanged."
  (with-isolated-config
    (let ((before *status-height*))
      (is (null (handler-case (apply-config-directive '("set-status-height" "abc"))
                  (error (e)
                    (fail "set-status-height with a non-integer must not signal, got ~A" e)
                    :signaled)))
          "set-status-height with a non-integer value should return NIL")
      (is (eql before *status-height*)
          "*status-height* should be unchanged after a non-integer value, got ~A"
          *status-height*)
      (is (null (handler-case (apply-config-directive '("set-status-height" "0"))
                  (error (e)
                    (fail "set-status-height with 0 must not signal, got ~A" e)
                    :signaled)))
          "set-status-height with a non-positive value (0) should return NIL")
      (is (eql before *status-height*)
          "*status-height* should be unchanged after a non-positive value, got ~A"
          *status-height*))))

;;; ── multi-character key tokens ─────────────────────────────────────────────

(test bind-multichar-key-token
  "A multi-character key token (M-z) is stored as the string itself, not
   decomposed into its characters."
  (with-isolated-config
    (let ((applied (load-config-from-string "bind M-z next-window")))
      (is (= 1 applied)
          "exactly 1 directive should be applied, got ~A" applied)
      (is (eq :next-window (lookup-key-binding "M-z"))
          "the multi-char token M-z should bind the STRING key \"M-z\"")
      ;; #\z is unbound in the default table, so the multi-char token must not
      ;; have bound the trailing character on its own.
      (is (null (lookup-key-binding #\z))
          "the single character #\\z must not be bound by the M-z token"))))

;;; ── load-config-from-stream ────────────────────────────────────────────────

(test load-config-from-stream-applies
  "load-config-from-stream ignores comments and applies the real directives."
  (with-isolated-config
    (let ((applied (with-input-from-string
                       (s (format nil "# leading comment~%bind z next-window~%set-status-height 4~%"))
                     (load-config-from-stream s))))
      (is (= 2 applied)
          "exactly 2 directives should be applied (bind + set-status-height), got ~A"
          applied)
      (is (eq :next-window (lookup-key-binding #\z))
          "#\\z should be bound to :next-window after the stream directives")
      (is (= 4 *status-height*)
          "*status-height* should be 4, got ~A" *status-height*))))

;;; ── load-config-file on a real temp file ───────────────────────────────────

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

;;; ── %command-keyword: resolution & non-interning contract ──────────────────

(test command-keyword-does-not-intern-unknown
  "%command-keyword must NOT intern an unknown command name into :keyword.
   After attempting to resolve a nonsense name, FIND-SYMBOL must still report it
   absent from the keyword package (proving INTERN was not used)."
  (let ((name "CL-TMUX-NONEXISTENT-COMMAND-DO-NOT-INTERN-ME"))
    (is (null (find-symbol name :keyword))
        "precondition: ~A must not be interned in :keyword before the call" name)
    (is (null (cl-tmux/config::%command-keyword name))
        "an unknown command name must resolve to NIL")
    (is (null (find-symbol name :keyword))
        "%command-keyword must not have interned ~A into the keyword package"
        name)))

(test command-keyword-returns-bindable-keyword
  "%command-keyword returns the command keyword for a recognized, bindable
   command name (case-insensitively)."
  (is (eq :new-window (cl-tmux/config::%command-keyword "new-window"))
      "\"new-window\" should resolve to :new-window")
  (is (eq :new-window (cl-tmux/config::%command-keyword "NEW-WINDOW"))
      "resolution should be case-insensitive (string-upcase)")
  (is (eq :split-horizontal (cl-tmux/config::%command-keyword "split-horizontal"))
      "\"split-horizontal\" should resolve to :split-horizontal"))

(test command-keyword-rejects-non-bindable-keyword
  "Even when the keyword already exists in :keyword, %command-keyword returns NIL
   unless it is a member of *bindable-commands*.  :copy-mode-exit is a real,
   interned keyword that is deliberately excluded from *bindable-commands*."
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

;;; ── %config-tokens (tokenizer) ───────────────────────────────────────────────

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

;;; ── %parse-key-token ────────────────────────────────────────────────────────

(test parse-key-token-single-char-returns-char
  "%parse-key-token returns a character for a 1-char token."
  (is (char= #\c   (cl-tmux/config::%parse-key-token "c")))
  (is (char= #\%   (cl-tmux/config::%parse-key-token "%")))
  (is (char= #\"   (cl-tmux/config::%parse-key-token "\""))))

(test parse-key-token-multi-char-returns-string
  "%parse-key-token returns the string itself for tokens longer than 1 char."
  (is (string= "M-1" (cl-tmux/config::%parse-key-token "M-1")))
  (is (string= "F1"  (cl-tmux/config::%parse-key-token "F1"))))

;;; ── apply-config-line ────────────────────────────────────────────────────────

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

(test env-set-p-correctly-classifies-strings
  "%env-set-p returns T for non-empty strings and NIL for nil or empty strings."
  (is-true  (cl-tmux/config::%env-set-p "/some/path")  "non-empty string is set")
  (is-true  (cl-tmux/config::%env-set-p "x")           "single-char string is set")
  (is-false (cl-tmux/config::%env-set-p nil)            "nil is not set")
  (is-false (cl-tmux/config::%env-set-p "")             "empty string is not set"))
