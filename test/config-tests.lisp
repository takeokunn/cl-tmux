(in-package #:cl-tmux/test)

;;;; Configuration and key-binding tests.
;;;;
;;;; These tests are purely functional (no PTY, no threads) and cover:
;;;;   • the compile-time constant +prefix-key-code+,
;;;;   • known bindings in the default *key-bindings* table,
;;;;   • the lookup-key-binding helper, and
;;;;   • structural invariants of *key-bindings* itself.

(def-suite config-suite :description "Key bindings and configuration")
(in-suite config-suite)

;;; ── Import the config symbols we need ────────────────────────────────────

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(cl-tmux/config:lookup-key-binding
            cl-tmux/config:describe-key-bindings
            cl-tmux/config:*key-bindings*
            cl-tmux/config:+prefix-key-code+
            cl-tmux/config:*default-shell*
            cl-tmux/config:*status-height*
            cl-tmux/config:set-key-binding
            cl-tmux/config:remove-key-binding
            cl-tmux/config:load-config-from-string
            cl-tmux/config:load-config-from-stream
            cl-tmux/config:apply-config-directive
            cl-tmux/config:config-file-path
            cl-tmux/config:load-config-file)))

;;; ── Constant value ─────────────────────────────────────────────────────────

(test prefix-key-code
  "+prefix-key-code+ is 2 (ASCII STX / C-b)."
  (is (= 2 +prefix-key-code+)
      "+prefix-key-code+ should be 2, got ~A" +prefix-key-code+))

;;; ── Known default bindings ────────────────────────────────────────────────

(test lookup-c-binds-new-window
  "C-b c creates a new window."
  (is (eq :new-window (lookup-key-binding #\c))
      "#\\c should be bound to :new-window"))

(test lookup-d-binds-detach
  "C-b d detaches the client."
  (is (eq :detach (lookup-key-binding #\d))
      "#\\d should be bound to :detach"))

(test lookup-unknown-returns-nil
  "An unbound key returns NIL."
  (is (null (lookup-key-binding #\z))
      "#\\z should return NIL (unbound)"))

;;; ── Structural invariants of *key-bindings* ───────────────────────────────

(test all-bindings-have-keyword-values
  "Every value (cdr) in *key-bindings* is a keyword symbol."
  (dolist (binding *key-bindings*)
    (is (keywordp (cdr binding))
        "binding ~A should have a keyword value, got ~A"
        binding (cdr binding))))

(test all-bindings-have-char-or-string-keys
  "Every key (car) in *key-bindings* is a character or a string."
  (dolist (binding *key-bindings*)
    (is (or (characterp (car binding))
            (stringp    (car binding)))
        "binding ~A should have a character or string key, got ~A"
        binding (car binding))))

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

;;; ── Config-file loading tests ──────────────────────────────────────────────
;;;
;;; All of the directives below mutate the special variables *key-bindings*,
;;; *default-shell* and *status-height*.  Each mutating test rebinds these
;;; specials dynamically with WITH-ISOLATED-CONFIG so the changes never leak
;;; out of the test and affect other tests or other suites.

(defmacro with-isolated-config (&body body)
  "Run BODY with the mutable config specials dynamically rebound to copies,
   so directives applied in a test never leak into other suites."
  `(let ((cl-tmux/config:*key-bindings*  (copy-alist cl-tmux/config:*key-bindings*))
         (cl-tmux/config:*default-shell* cl-tmux/config:*default-shell*)
         (cl-tmux/config:*status-height* cl-tmux/config:*status-height*))
     ,@body))

;;; ── set-key-binding / remove-key-binding ──────────────────────────────────

(test set-key-binding-adds-new
  "set-key-binding adds a brand-new binding that lookup-key-binding finds."
  (with-isolated-config
    (is (null (lookup-key-binding #\z))
        "#\\z should start unbound")
    (set-key-binding #\z :new-window)
    (is (eq :new-window (lookup-key-binding #\z))
        "#\\z should be bound to :new-window after set-key-binding")))

(test set-key-binding-replaces-existing
  "set-key-binding on an existing key replaces the command without duplicating."
  (with-isolated-config
    (set-key-binding #\z :new-window)
    (let ((before (count #\z *key-bindings* :key #'car :test #'equal)))
      (is (= 1 before)
          "#\\z should appear exactly once after first bind, got ~A" before))
    (set-key-binding #\z :detach)
    (is (eq :detach (lookup-key-binding #\z))
        "#\\z should now be bound to :detach")
    (let ((after (count #\z *key-bindings* :key #'car :test #'equal)))
      (is (= 1 after)
          "#\\z should still appear exactly once (no duplicate), got ~A" after))))

(test remove-key-binding-removes
  "remove-key-binding removes a binding so lookup returns NIL afterward."
  (with-isolated-config
    (set-key-binding #\z :new-window)
    (is (eq :new-window (lookup-key-binding #\z))
        "#\\z should be bound before removal")
    (remove-key-binding #\z)
    (is (null (lookup-key-binding #\z))
        "#\\z should be unbound after remove-key-binding")))

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

(defun config-path (override xdg home)
  "Namestring of the resolved config path for the given env values + HOME
   (HOME a directory pathname)."
  (namestring (cl-tmux/config::%config-path-from override xdg home)))

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

;;; ── describe-key-bindings (list-keys help text) ─────────────────────────────

(test describe-key-bindings-lists-commands
  "describe-key-bindings produces help text naming the bound commands."
  (let ((text (describe-key-bindings)))
    (is (search "new-window" text)   "should list new-window")
    (is (search "detach" text)       "should list detach")
    (is (search "select-window" text) "should list select-window")))

;;; ── load-config-file ───────────────────────────────────────────────────────

(test load-config-file-missing-returns-nil
  "load-config-file on a non-existent path returns NIL."
  (with-isolated-config
    (is (null (load-config-file #p"/nonexistent/cl-tmux-xyz.conf"))
        "loading a non-existent config file should return NIL")))

;;; ── bind directive: command validation & arity ─────────────────────────────

(test apply-directive-bind-unknown-command-returns-nil
  "A bind directive targeting an unrecognized command returns NIL and binds nothing."
  (with-isolated-config
    (is (null (apply-config-directive '("bind" "z" "no-such-command")))
        "bind to an unknown command should return NIL")
    (is (null (lookup-key-binding #\z))
        "#\\z must stay unbound after a bind to an unknown command")))

(test apply-directive-bind-wrong-argcount-returns-nil
  "A bind directive with the wrong number of args returns NIL and binds nothing."
  (with-isolated-config
    (is (null (apply-config-directive '("bind" "z")))
        "bind with only a key (1 arg) should return NIL")
    (is (null (apply-config-directive '("bind" "z" "a" "b")))
        "bind with 3 args should return NIL")
    (is (null (lookup-key-binding #\z))
        "#\\z must stay unbound after wrong-arity bind directives")))

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

(test apply-directive-unbind-wrong-argcount-returns-nil
  "An unbind directive with the wrong number of args returns NIL."
  (with-isolated-config
    (is (null (apply-config-directive '("unbind")))
        "unbind with no key (0 args) should return NIL")
    (is (null (apply-config-directive '("unbind" "a" "b")))
        "unbind with 2 args should return NIL")))

;;; ── set-* directives: arity ────────────────────────────────────────────────

(test apply-directive-set-wrong-argcount-returns-nil
  "set-shell and set-status-height with no value return NIL."
  (with-isolated-config
    (is (null (apply-config-directive '("set-shell")))
        "set-shell with no value should return NIL")
    (is (null (apply-config-directive '("set-status-height")))
        "set-status-height with no value should return NIL")))

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
;;;
;;; %command-keyword maps a config-file command NAME (case-insensitive) to its
;;; bindable command keyword.  Its documented contract is that it uses
;;; FIND-SYMBOL (not INTERN), so an unknown command name is NEVER interned into
;;; the keyword package, and only keywords listed in *bindable-commands* are
;;; returned even when the keyword already exists.

(test command-keyword-does-not-intern-unknown
  "%command-keyword must NOT intern an unknown command name into :keyword.
   After attempting to resolve a nonsense name, FIND-SYMBOL must still report it
   absent from the keyword package (proving INTERN was not used)."
  (let ((name "CL-TMUX-NONEXISTENT-COMMAND-DO-NOT-INTERN-ME"))
    ;; Guard: the name must not already be interned (nothing in the codebase
    ;; uses it); if it somehow were, this test could not prove the contract.
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
  ;; Make sure :copy-mode-exit is genuinely interned, so we are testing the
  ;; bindability gate rather than the find-symbol path.
  (let ((kw (intern "COPY-MODE-EXIT" :keyword)))
    (is (eq :copy-mode-exit kw)
        "the keyword :copy-mode-exit should be interned")
    (is (not (member :copy-mode-exit cl-tmux/config::*bindable-commands*))
        "precondition: :copy-mode-exit must not be in *bindable-commands*")
    (is (null (cl-tmux/config::%command-keyword "copy-mode-exit"))
        "%command-keyword must reject an interned-but-non-bindable keyword"))
  ;; The gate is genuinely consulted: binding *bindable-commands* to a list that
  ;; excludes :new-window makes a previously-resolvable name return NIL.
  (let ((cl-tmux/config::*bindable-commands* '(:detach)))
    (is (null (cl-tmux/config::%command-keyword "new-window"))
        "with *bindable-commands* not containing :new-window, NIL is returned")
    (is (eq :detach (cl-tmux/config::%command-keyword "detach"))
        "a name still present in the rebound *bindable-commands* resolves")))
