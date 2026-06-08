(in-package #:cl-tmux/config)

;;; ── Config file parsing + directive processing ───────────────────────────
;;;
;;; This file depends on the key-binding mutators defined in config.lisp
;;; (set-key-binding, remove-key-binding) and the mutable specials
;;; (*key-tables*, *default-shell*, *status-height*).

;;; ── Tokenizer phase helpers ──────────────────────────────────────────────

(defun %whitespace-p (ch)
  "True when CH is a configuration whitespace character (space or tab)."
  (or (char= ch #\Space) (char= ch #\Tab)))

;;; ── Tokenizer phase helpers ──────────────────────────────────────────────
;;;
;;; Each helper handles one tokenizer state; all share the PUSH-CHAR closure
;;; and return the updated character index.

(defun %tokenize-backslash-escape (line i len push-char)
  "Consume a backslash-escaped character starting at I.  Calls PUSH-CHAR on
   the escaped character.  Returns the new index past both characters."
  (let ((next (1+ i)))
    (if (< next len)
        (progn (funcall push-char (char line next))
               (+ next 1))
        (+ i 1))))

(defun %tokenize-double-quoted (line i len push-char)
  "Consume a double-quoted region beginning at I (the opening-quote position).
   Handles backslash escapes inside.  If no closing quote exists, treats the
   opening quote as a literal character.  Returns the new index."
  (let ((close-pos (position #\" line :start (1+ i))))
    (if (not close-pos)
        ;; No closing quote — treat the opening \" as a literal.
        (progn (funcall push-char (char line i))
               (1+ i))
        ;; Found a closing quote — process quoted content.
        (let ((j (1+ i)))            ; skip opening \"
          (loop while (and (< j len) (char/= (char line j) #\"))
                do (let ((quoted-char (char line j)))
                     (cond
                       ((and (char= quoted-char #\\) (< (1+ j) len))
                        (incf j)
                        (funcall push-char (char line j)))
                       (t
                        (funcall push-char quoted-char))))
                   (incf j))
          (when (< j len) (incf j))  ; skip closing \"
          j))))

(defun %tokenize-single-quoted (line i len push-char)
  "Consume a single-quoted region beginning at I.  No escapes inside.
   Returns the new index past the closing quote (or EOL if unmatched)."
  (let ((j (1+ i)))                  ; skip opening '
    (loop while (and (< j len) (char/= (char line j) #\'))
          do (funcall push-char (char line j))
             (incf j))
    (when (< j len) (incf j))        ; skip closing '
    j))

(defun %config-tokens (line)
  "Tokenize LINE into a list of strings, handling:
   - unquoted whitespace as delimiter
   - \"double quoted\" strings (spaces preserved, \\x escapes processed)
   - 'single quoted' strings (literal content, no escapes)
   - \\ (backslash) escaping of the next character outside quotes
   Returns a list of token strings."
  (let* ((tokens   '())
         (current  (make-array 0 :element-type 'character :adjustable t :fill-pointer 0))
         (in-token nil)
         (len      (length line)))
    (flet ((push-char (ch)
             (vector-push-extend ch current)
             (setf in-token t))
           (finish-token ()
             (when in-token
               (push (copy-seq current) tokens)
               (setf (fill-pointer current) 0)
               (setf in-token nil))))
      (let ((i 0))
        (loop while (< i len) do
          (let ((ch (char line i)))
            (cond
              ((char= ch #\\)
               (setf i (%tokenize-backslash-escape line i len #'push-char)))
              ((char= ch #\")
               (setf in-token t)
               (setf i (%tokenize-double-quoted line i len #'push-char)))
              ((char= ch #\')
               (setf in-token t)
               (setf i (%tokenize-single-quoted line i len #'push-char)))
              ((%whitespace-p ch)
               (finish-token)
               (incf i))
              (t
               (push-char ch)
               (incf i)))))
        (finish-token)))
    (nreverse tokens)))

(defun %parse-control-char (rest)
  "Map REST (the part after a \"C-\" prefix) to its control CHARACTER, or NIL
   when REST does not denote a single control-able key.
   C-a..C-z → ^A..^Z (1..26); C-Space / C-@ → NUL (0);
   C-[ C-\\ C-] C-^ C-_ → 27..31.  The control byte is (logand code #x1f)."
  (cond
    ((string-equal rest "Space") (code-char 0))
    ((= (length rest) 1)
     (let ((c (char-upcase (char rest 0))))
       (cond
         ((char= c #\@) (code-char 0))
         ((char<= #\A c #\Z) (code-char (logand (char-code c) #x1f)))
         ((member c '(#\[ #\\ #\] #\^ #\_) :test #'char=)
          (code-char (logand (char-code c) #x1f)))
         (t nil))))
    (t nil)))

(defun %parse-key-token (token)
  "Parse a bind-key key TOKEN into the key-table key.
   A single-character TOKEN denotes that character.  A \"C-<key>\" token denotes
   the corresponding control CHARACTER (C-a→^A, C-Space→NUL, ...) so that Ctrl
   bindings match the byte the event loop sees when the key is pressed (the loop
   looks keys up via (code-char byte)).  Any other multi-character token (named
   keys like F1, Up, Home, or modifier combos like M-x / C-Left that the event
   loop encodes as multi-byte sequences) is kept as the string itself, matching
   the key-table key format used by the lookup path."
  (cond
    ((= (length token) 1) (char token 0))
    ((and (> (length token) 2)
          (char-equal (char token 0) #\C)
          (char= (char token 1) #\-))
     ;; "C-<key>": convert to the control char when single-key; otherwise (e.g.
     ;; "C-Left") fall back to the string for the deferred modifier-key path.
     (or (%parse-control-char (subseq token 2)) token))
    (t token)))

(defparameter *bindable-commands*
  '(;; Window lifecycle
    :new-window :next-window :prev-window :last-window :find-window
    :rename-window :choose-window :list-windows :move-window-prompt :swap-window
    :rotate-window :rotate-window-reverse :next-layout
    :select-layout-even-h :select-layout-even-v :select-layout-tiled
    :select-layout-main-h :select-layout-main-v :select-layout-spread
    ;; Pane lifecycle
    :next-pane :prev-pane :last-pane :display-panes
    :split-horizontal :split-vertical
    :split-horizontal-no-focus :split-vertical-no-focus
    :kill-pane :kill-pane-confirm :kill-window :kill-window-confirm
    :respawn-pane :break-pane :join-pane
    :swap-pane-forward :swap-pane-backward
    :resize-left :resize-right :resize-up :resize-down
    :zoom-toggle :mark-pane :clear-mark
    :synchronize-panes :pipe-pane :display-info
    ;; Session lifecycle
    :new-session :kill-session :rename-session :detach
    :list-sessions :list-sessions-full :choose-session
    :switch-client-next :switch-client-prev :last-session
    :has-session :lock-session :unlock-session
    ;; Key bindings / config
    :list-keys :source-file :bind-key :unbind-key
    ;; Selection / navigation
    :select-window ; the pressed digit chooses the window
    :select-window-prompt :select-pane-left :select-pane-right
    :select-pane-up :select-pane-down
    ;; Copy / paste / buffers
    :paste-buffer :copy-mode-enter :send-prefix
    :list-buffers :show-buffer :choose-buffer :delete-buffer
    :save-buffer :load-buffer
    ;; Display / info
    :show-options :show-option
    :show-window-options :show-session-options :show-server-options
    :show-messages :show-hooks
    :display-message :display-popup
    :capture-pane :clear-history :clock-mode
    ;; Scripting / hooks
    :run-shell :if-shell :command-prompt :wait-for
    ;; Client management
    :choose-client :choose-tree :refresh-client :suspend-client
    ;; Server management
    :server-info :list-clients :lock-server :detach-all-clients
    :kill-server :start-server :lock-client
    ;; Window management (additional)
    :resize-window :respawn-window :attach-session :move-pane
    :previous-layout :link-window :unlink-window
    ;; Pane management (additional)
    :list-panes :set-buffer :select-pane-mark :detach-client
    ;; Info / listing
    :list-commands
    ;; Environment
    :show-environment :set-environment
    ;; Prompt history
    :show-prompt-history :clear-prompt-history
    ;; Set-option (interactive)
    :set-window-option :set-session-option)
  "Command keywords a config-file bind directive may target.
   Type: list of keyword symbols.
   This is the user-bindable subset of commands cl-tmux:dispatch-command handles.
   It deliberately EXCLUDES copy-mode-internal commands (:copy-mode-exit,
   :copy-mode-begin-selection, :copy-mode-yank), which are produced by copy-mode
   interception rather than by key lookup.
   Updated whenever a new dispatchable command is added to dispatch-handlers.")

(defparameter *command-name-aliases*
  '(;; full tmux names whose keyword differs from the keyword-ized name
    ("previous-window" . :prev-window)
    ("copy-mode"        . :copy-mode-enter)
    ("move-window"      . :move-window-prompt)
    ("swap-pane"        . :swap-pane-forward)
    ("detach-client"    . :detach)
    ;; standard tmux command abbreviations (see man tmux "ALIASES") for the
    ;; arg-less bindable commands, so `bind <key> <abbrev>` resolves directly
    ("showw"     . :show-window-options)
    ("shows"     . :show-session-options)
    ("breakp"    . :break-pane)
    ("clearhist" . :clear-history)
    ("displayp"  . :display-panes)
    ("popup"     . :display-popup)   ; man tmux: display-popup (alias: popup)
    ("findw"     . :find-window)
    ("joinp"     . :join-pane)
    ("killp"     . :kill-pane)
    ("last"      . :last-window)
    ("loadb"     . :load-buffer)
    ("lock"      . :lock-server)
    ("locks"     . :lock-session)
    ("lockc"     . :lock-client)
    ("lsb"       . :list-buffers)
    ("movep"     . :move-pane)
    ("next"      . :next-window)
    ("nextl"     . :next-layout)
    ("pasteb"    . :paste-buffer)
    ("prev"      . :prev-window)
    ("prevl"     . :previous-layout)
    ("refresh"   . :refresh-client)
    ("respawnp"  . :respawn-pane)
    ("respawnw"  . :respawn-window)
    ("rotatew"   . :rotate-window)
    ("saveb"     . :save-buffer)
    ("showb"     . :show-buffer)
    ("showmsgs"  . :show-messages)
    ("show"      . :show-options)
    ;; Single-token abbreviations of ARG-bearing commands.  `bind X <abbrev> args`
    ;; (multi-token) already works — stored unvalidated, resolved via the runtime
    ;; *arg-command-table* — but a BARE `bind X <abbrev>` goes through
    ;; %command-keyword and needs an alias here.  Each maps to the same keyword the
    ;; full command name resolves to (all verified members of *bindable-commands*).
    ("capturep"  . :capture-pane)
    ("commandp"  . :command-prompt)
    ("deleteb"   . :delete-buffer)
    ("has"       . :has-session)
    ("killw"     . :kill-window)
    ("lastp"     . :last-pane)
    ("resizew"   . :resize-window)
    ("selectw"   . :select-window)
    ("setb"      . :set-buffer)
    ("swapp"     . :swap-pane-forward))
  "tmux command names whose canonical bindable keyword is NOT simply the
   keyword-ized form of the name — full tmux names (previous-window, copy-mode,
   detach-client) and the standard short aliases (man tmux \"ALIASES\": breakp,
   killp, next, prev, etc.).  Mirrors the alias rows of the runtime named-command
   table (dispatch-core.lisp define-named-command-table); duplicated here because
   the config layer sits below the cl-tmux package and cannot call it.  Every
   VALUE must be a member of *bindable-commands* (enforced by a unit test).")

(defun %command-keyword (name)
  "Return the bindable command keyword named by NAME (case-insensitive), or NIL
   if NAME is not a recognized command.  Recognizes the canonical command names
   (resolved via FIND-SYMBOL so unknown names are never interned into the keyword
   package) plus the tmux aliases in *command-name-aliases*.  Genuinely-unknown
   names still resolve to NIL so config typos are rejected at load time."
  (or (cdr (assoc name *command-name-aliases* :test #'string-equal))
      (let ((keyword (find-symbol (string-upcase name) :keyword)))
        (and keyword (member keyword *bindable-commands*) keyword))))

;;; ── Environment-variable helper ─────────────────────────────────────────────
;;;
;;; set-environment / setenv directives need to mutate the process environment.
;;; We use sb-posix:setenv resolved at runtime (not compile time) so that the
;;; config package does not need a compile-time dependency on sb-posix.

(defun %config-setenv (name value)
  "Set environment variable NAME to VALUE for child processes.
   Resolves sb-posix:setenv at runtime so the call is safe even when sb-posix
   has not been loaded at compile time.  A no-op when sb-posix is absent."
  (let ((pkg (find-package "SB-POSIX")))
    (when pkg
      (let ((fn (find-symbol "SETENV" pkg)))
        (when fn
          (ignore-errors (funcall fn name value 1)))))))

;;; ── run-shell tilde expansion helper ─────────────────────────────────────────

(defun %expand-leading-tilde (cmd)
  "Expand a leading \"~/\" in CMD to \"$HOME/\" using the HOME environment
   variable, so `run '~/.tmux/plugins/tpm/tpm'` resolves to the user's home.
   Leaves absolute (\"/abs\") and relative (\"rel\") strings unchanged.  Pure
   string transformation: returns CMD unchanged when it does not begin with ~/."
  (if (and (> (length cmd) 2)
           (char= (char cmd 0) #\~)
           (char= (char cmd 1) #\/))
      (concatenate 'string
                   (or (ignore-errors (sb-ext:posix-getenv "HOME")) "~")
                   (subseq cmd 1))
      cmd))

;;; ── Declarative directive dispatch macro ──────────────────────────────────

(defmacro define-config-directives (&rest rules)
  "Build %APPLY-CONFIG-DIRECTIVE-INNER from a declarative table of directive RULES.

   Each RULE has one of two forms:
     (NAME ARITY (ARG...) &body BODY)
       NAME   – the directive keyword as a string (e.g. \"set-shell\")
       ARITY  – the exact number of arguments the directive takes
       (ARG…) – symbols bound to those arguments inside BODY
       BODY   – forms run when NAME matches with the right ARITY; their value is
                returned (non-NIL ⇒ the directive was applied).

     (:aliases (NAME...) ARITY (ARG...) &body BODY)
       Identical to the single-name form except CMD matches any string in (NAME...).
       Eliminates alias repetition (source-file/source, set/setw/…, etc.).

   The outer APPLY-CONFIG-DIRECTIVE function wraps this inner dispatcher and
   handles 'bind' with variable-arity flags separately."
  (flet ((expand-rule (rule)
           ;; Returns a list of cond arms (one arm per name).
           (if (eq (first rule) :aliases)
               ;; (:aliases (name...) arity arglist body...)
               (destructuring-bind (names arity arglist &body body) (rest rule)
                 (mapcar (lambda (name)
                           `((and (string= cmd ,name) (= (length args) ,arity))
                             (destructuring-bind ,arglist args
                               (declare (ignorable ,@arglist))
                               ,@body)))
                         names))
               ;; (name arity arglist body...)
               (destructuring-bind (name arity arglist &body body) rule
                 (list `((and (string= cmd ,name) (= (length args) ,arity))
                         (destructuring-bind ,arglist args
                           (declare (ignorable ,@arglist))
                           ,@body)))))))
    `(defun %apply-config-directive-inner (tokens)
       "Apply one non-bind config directive (list of string TOKENS) to live state.
        Returns T when applied, NIL for an unknown/invalid directive."
       (when tokens
         (let ((cmd (first tokens)) (args (rest tokens)))
           (declare (ignorable args))
           (cond
             ,@(mapcan #'expand-rule rules)
             (t nil)))))))

;;; ── bind-key flag parsing ────────────────────────────────────────────────
;;;
;;; %parse-bind-key-args handles the optional flags before key and command:
;;;   bind [-n] [-r] [-T table] key command
;;; Returns (values table key command repeatable) or NIL on parse failure.

(defun %parse-bind-key-args (args)
  "Parse the ARGS list for a bind directive (excludes the \"bind\" verb itself).
   Returns (values table key command repeatable note) where TABLE is +TABLE-PREFIX+
   by default and NOTE is the -N description string (or NIL), or NIL when ARGS do
   not form a valid binding."
  (let ((table      +table-prefix+)
        (repeatable nil)
        (note       nil)
        (remaining  args))
    (loop
      (cond
        ((null remaining) (return nil))
        ((string= (first remaining) "-n")
         (setf table     +table-root+)
         (setf remaining (rest remaining)))
        ((string= (first remaining) "-r")
         (setf repeatable t)
         (setf remaining  (rest remaining)))
        ((string= (first remaining) "-T")
         (setf remaining (rest remaining))
         (when (null remaining) (return nil))
         (setf table     (first remaining))
         (setf remaining (rest remaining)))
        ;; -N "note": tmux 3.1+ key-binding description.  Capture the (already
        ;; single-token, quote-joined) note argument so list-keys can display it.
        ;; It MUST be consumed here — otherwise the fall-through below would
        ;; mis-read "-N" as the key and the note as the command.
        ((string= (first remaining) "-N")
         (setf remaining (rest remaining))
         (when (null remaining) (return nil))
         (setf note      (first remaining))
         (setf remaining (rest remaining)))
        (t
         ;; Need a key plus at least one command token.
         (when (null (rest remaining)) (return nil))
         (let* ((key-token  (%parse-key-token (first remaining)))
                ;; Strip an optional { ... } block wrapper (tmux 3.x brace
                ;; syntax) so it reuses the semicolon-sequence machinery below.
                (cmd-tokens (%strip-brace-block (rest remaining)))
                ;; Split on ";" tokens to support multi-command sequences:
                ;; bind r source-file ~/.tmux.conf \; display "Reloaded!"
                ;; — or:  bind r { source-file ~/.tmux.conf ; display "Reloaded!" }
                (sequences  (%split-on-semicolons cmd-tokens)))
           ;; An empty block (`bind r { }`) leaves no command — reject it.
           (when (null cmd-tokens) (return nil))
           (return
             (if (= (length sequences) 1)
                 ;; Single command: use the existing single-command path.
                 (let ((tokens (first sequences)))
                   (if (= (length tokens) 1)
                       ;; Single word: resolve to a keyword.
                       (let ((keyword (%command-keyword (first tokens))))
                         (if keyword (values table key-token keyword repeatable note) nil))
                       ;; Multi-token: store as token list.
                       (values table key-token tokens repeatable note)))
                 ;; Multiple commands: store as :sequence list of token lists.
                 (values table key-token (cons :sequence sequences) repeatable note)))))))))

;;; ── Semicolon-sequence splitter ──────────────────────────────────────────
;;;
;;; tmux bind directives support ";" (from "\;" in the config line) as a
;;; command separator: bind r source-file ~/.tmux.conf \; display "Reloaded!"
;;; %split-on-semicolons splits a flat token list on ";" tokens,
;;; removing empty segments, yielding a list of per-command token lists.

(defun %strip-brace-block (tokens)
  "When TOKENS form a `{ ... }` block — first token \"{\" and last token \"}\" —
   return the inner tokens; otherwise return TOKENS unchanged.  This lets the
   tmux 3.x brace form `bind r { cmd1 ; cmd2 }` reuse %split-on-semicolons
   exactly like the older `bind r cmd1 \\; cmd2` form.  An empty block `{ }`
   yields NIL (no commands)."
  (if (and (cdr tokens)
           (string= (first tokens) "{")
           (string= (car (last tokens)) "}"))
      (butlast (rest tokens))
      tokens))

(defun %split-on-semicolons (tokens)
  "Split TOKENS on \";\" tokens, returning a list of per-command token lists.
   Empty segments (consecutive semicolons or trailing) are discarded.
   When no semicolons are present, returns (list tokens) unchanged."
  (let ((result  '())
        (current '()))
    (dolist (tok tokens)
      (if (string= tok ";")
          (progn (when current (push (nreverse current) result))
                 (setf current '()))
          (push tok current)))
    (when current (push (nreverse current) result))
    (if result (nreverse result) (list tokens))))

;;; ── unbind-key flag parsing ──────────────────────────────────────────────
;;;
;;; %parse-unbind-key-args handles optional [-n] [-T table] flags before the key.
;;; Returns (values table key) or (values nil nil) on parse failure.

(defun %parse-unbind-key-args (args)
  "Parse the ARGS list for an unbind directive (excludes the verb itself).
   Returns (values table key) where TABLE is +TABLE-PREFIX+ by default,
   or (values nil nil) on parse failure."
  (let ((table     +table-prefix+)
        (remaining args))
    (loop
      (cond
        ((null remaining) (return (values nil nil)))
        ((string= (first remaining) "-n")
         (setf table     +table-root+)
         (setf remaining (rest remaining)))
        ((string= (first remaining) "-T")
         (setf remaining (rest remaining))
         (when (null remaining) (return (values nil nil)))
         (setf table     (first remaining))
         (setf remaining (rest remaining)))
        (t
         (unless (= (length remaining) 1) (return (values nil nil)))
         (return (values table (%parse-key-token (first remaining)))))))))

;;; ── Declarative bind/unbind verb dispatch ────────────────────────────────

(defmacro define-key-directive-handlers (&rest rules)
  "Build %APPLY-KEY-DIRECTIVE from a declarative table of verb RULES.
   Each RULE is (VERBS &body BODY) where VERBS is a list of verb strings
   and BODY is evaluated with CMD and ARGS in scope."
  `(defun %apply-key-directive (cmd args)
     "Dispatch a bind/unbind directive.  Returns T on success, NIL on failure."
     (cond
       ,@(mapcar
          (lambda (rule)
            (destructuring-bind (verbs &body body) rule
              `((member cmd ',verbs :test #'string=)
                ,@body)))
          rules)
       (t nil))))

(define-key-directive-handlers
  (("bind" "bind-key")
   (multiple-value-bind (table key command repeatable note)
       (%parse-bind-key-args args)
     (when command
       ;; COMMAND is a keyword (built-in) or a token list (`bind key cmd args`).
       ;; NOTE is the optional -N description, surfaced by list-keys.
       (key-table-bind table key command :repeatable repeatable :note note)
       t)))
  (("unbind" "unbind-key")
   (multiple-value-bind (table key)
       (%parse-unbind-key-args args)
     (when (and table key)
       (let ((tbl (gethash table *key-tables*)))
         (when tbl (remhash key tbl)))
       t)))
  ;; unbind-all [-T table]: clear all bindings in a key-table (default: prefix).
  ;; -T specifies the table; without -T the prefix table is cleared.
  (("unbind-all")
   (let* ((t-pos  (position "-T" args :test #'string=))
          (table  (if (and t-pos (nth (1+ t-pos) args))
                      (nth (1+ t-pos) args)
                      +table-prefix+))
          (inner  (gethash table *key-tables*)))
     (when inner (clrhash inner))
     t)))

;;; ── Simple directive definitions ─────────────────────────────────────────
;;;
;;; The six set-option variants (set, set-option, setw, set-window-option,
;;; sets, set-session-option) all forward to cl-tmux/options:set-option at
;;; config-file load time, because no session/window/pane context is available
;;; during config parsing.
;;;
;;; Runtime commands that carry a window or pane context should call
;;; cl-tmux/options:set-option-for-window / set-option-for-pane directly to
;;; store in the per-struct local-options hash.

(define-config-directives
  ("set-shell" 1 (path)
    (setf *default-shell* path)
    t)
  ("set-status-height" 1 (n)
    (let ((height (parse-integer n :junk-allowed t)))
      (when (and height (plusp height))
        (setf *status-height* height)
        t)))
  ("set" 2 (option-name option-value)
    (cl-tmux/options:set-option option-name option-value)
    t)
  ("set-option" 2 (option-name option-value)
    (cl-tmux/options:set-option option-name option-value)
    t)
  ("setw" 2 (option-name option-value)
    (cl-tmux/options:set-option option-name option-value)
    t)
  ("set-window-option" 2 (option-name option-value)
    (cl-tmux/options:set-option option-name option-value)
    t)
  ("sets" 2 (option-name option-value)
    (cl-tmux/options:set-option option-name option-value)
    t)
  ("set-session-option" 2 (option-name option-value)
    (cl-tmux/options:set-option option-name option-value)
    t)
  ("set-hook" 2 (event-name command-name)
    (let ((keyword (%command-keyword command-name)))
      (when keyword
        (cl-tmux/hooks:set-command-hook event-name keyword)
        t)))
  ("source-file" 1 (path)
    (ignore-errors (load-config-file (pathname path)))
    t)
  ("source" 1 (path)
    (ignore-errors (load-config-file (pathname path)))
    t)
  ;; NOTE: run-shell/run are handled entirely by %apply-run-shell-directive
  ;; (wired into apply-config-directive before this fixed-arity table), which
  ;; covers the bare 1-arg form as well as the flag-bearing forms.  No fixed-
  ;; arity entries are needed here.
  ;; set-environment / setenv 2-arg form: VAR VALUE (no flags).
  ;; The %apply-set-environment-directive handler in apply-config-directive
  ;; intercepts this first (handling -r/-g flags); these entries are fallbacks.
  ("set-environment" 2 (var-name var-value)
    (%config-setenv var-name var-value)
    t)
  ("setenv" 2 (var-name var-value)
    (%config-setenv var-name var-value)
    t))

;;; ── set-option flag handling (set -g / -a / -s / ...) ──────────────────────
;;;
;;; The fixed-arity directive table cannot match `set -g status off` (3 tokens vs
;;; arity 2), so the canonical .tmux.conf form silently failed.  %apply-set-
;;; directive consumes leading scope flags:
;;;   -g global (default)  -s server  -w window  -o only-if-unset
;;;   -a append  -u unset
;;; -s routes the write to *server-options* instead of *global-options*.

(defparameter *set-directive-names*
  '("set" "set-option" "setw" "set-window-option" "sets" "set-session-option")
  "Config directive verbs that forward to the global option store.")

(defun %strip-set-flags (args)
  "Consume leading -X flag tokens from a set directive's ARGS.
   Returns (values HAD-FLAG APPEND-P SERVER-P UNSET-P POSITIONALS):
     HAD-FLAG  – T when any flag was present
     APPEND-P  – T when -a appeared (append to existing value)
     SERVER-P  – T when -s appeared (route to server-options)
     UNSET-P   – T when -u appeared (remove the option)
   Recognised but currently treated as global: -g (global), -w (window),
   -p (pane), -o (only-if-unset — accepted, not enforced).  These scope
   flags cannot be applied to per-object instances at config-load time
   because no window or pane context exists yet; options fall through to
   the global store so they take effect at the nearest practical scope.
   POSITIONALS is the remaining non-flag tokens (name and optional value)."
  (let ((had-flag   nil)
        (append-p   nil)
        (server-p   nil)
        (unset-p    nil)
        (format-p   nil)
        (remaining  args))
    (loop while (and remaining
                     (let ((tok (first remaining)))
                       (and (>= (length tok) 2) (char= (char tok 0) #\-))))
          do (let ((tok (pop remaining)))
               (setf had-flag t)
               (when (find #\a tok) (setf append-p t))
               (when (find #\s tok) (setf server-p t))
               (when (find #\u tok) (setf unset-p  t))
               ;; -F: expand the value as a format string before storing.
               (when (find #\F tok) (setf format-p t))
               ;; -g, -w, -p, -o, -q: accepted silently.
               ))
    (values had-flag append-p server-p unset-p format-p remaining)))

(defun %apply-set-directive (cmd args)
  "Apply a flag-bearing set-family directive (e.g. `set -g status off`,
   `set -s escape-time 0`, `set -ag word-separators x`).
   Routes -s writes to *server-options*; handles -a (append) and -u (unset).
   Returns T when applied; NIL when CMD is not a set verb or carries no flags."
  (when (member cmd *set-directive-names* :test #'string=)
    (multiple-value-bind (had-flag append-p server-p unset-p format-p positionals)
        (%strip-set-flags args)
      (when (and had-flag (first positionals))
        (let* ((name      (first positionals))
               (raw-value (format nil "~{~A~^ ~}" (rest positionals)))
               ;; -F: expand value as a format string with basic hostname context.
               (value     (if format-p
                              (let ((ctx (list :hostname (machine-instance)
                                               :version "3.5")))
                                (handler-case
                                    (cl-tmux/format:expand-format raw-value ctx)
                                  (error () raw-value)))
                              raw-value)))
          (cond
            ;; -u: unset option — remove override, revert to registered default.
            (unset-p
             (if server-p
                 (remhash name cl-tmux/options:*server-options*)
                 (remhash name cl-tmux/options:*global-options*)))
            ;; -s + -a: append to server option.
            ((and server-p append-p)
             (cl-tmux/options:set-server-option
              name (concatenate 'string
                                (princ-to-string
                                 (or (cl-tmux/options:get-server-option name nil) ""))
                                value)))
            ;; -s: server option (escape-time, exit-empty, exit-unattached, etc.)
            (server-p
             (cl-tmux/options:set-server-option name value))
            ;; -a: append to global option.
            (append-p
             (cl-tmux/options:set-option
              name (concatenate 'string
                                (princ-to-string
                                 (or (cl-tmux/options:get-option name nil) ""))
                                value)))
            ;; Default: set global option.
            (t
             (cl-tmux/options:set-option name value)))
          ;; Special: command-alias[N] alias=expansion array syntax.
          (%apply-command-alias-directive name value)
          ;; Side-effect: intercept special options that need runtime state updates.
          (%apply-option-side-effects name value)
          t)))))

(defun %apply-option-side-effects (name value)
  "Apply runtime side-effects for options that touch non-option state.
   Handles: prefix, prefix2 (key routing), default-shell, status-height,
            escape-time, history-limit."
  (cond
    ((string= name "prefix")
     (let ((byte (%parse-prefix-key value)))
       (when byte
         (setf *prefix-key-code* byte)
         (key-table-bind +table-prefix+ (code-char byte) :send-prefix))))
    ;; prefix2: a second prefix key that arms the prefix table (same as primary prefix).
    ;; Stores the byte in *prefix2-key-code* so %ground-input-state transitions to
    ;; %after-prefix-input-state when this key is pressed — same as pressing C-b.
    ;; Real tmux: pressing prefix2 arms the prefix table; C-b and prefix2 are equivalent.
    ((string= name "prefix2")
     (let ((byte (%parse-prefix-key value)))
       (when byte
         (setf *prefix2-key-code* byte)
         ;; Also bind prefix2 in prefix table → send-prefix so C-b prefix2
         ;; (i.e., pressing prefix2 AFTER the prefix) sends prefix2 to the pane.
         (key-table-bind +table-prefix+ (code-char byte) :send-prefix))))
    ;; default-shell: update the shell used for new panes immediately.
    ((string= name "default-shell")
     (when (and (stringp value) (plusp (length value)))
       (setf *default-shell* value)))
    ;; status: off/on or a line count (tmux accepts 2..5 for a multi-line bar).
    ;; off/false/0 hides the bar; on/true or any positive integer shows it.
    ;; status: off/false/0 hides the bar; on/true shows 1 line; a numeric line
    ;; count 1..5 reserves that many rows (tmux caps at 5).  The renderer draws
    ;; the main bar on the outer line and status-format[1..N-1] on the rest.
    ((string= name "status")
     (let* ((off-p (member value '("off" "false" "0") :test #'equal))
            (n     (parse-integer value :junk-allowed t)))
       (setf *status-height*
             (cond (off-p 0)
                   ((and n (> n 0)) (min n 5))  ; numeric line count (tmux max 5)
                   (t 1)))))                     ; on/true/other truthy → 1 line
    ;; mouse: on/off enables/disables mouse reporting on the outer terminal.
    ;; We call the renderer's mouse-reporting functions via symbol lookup to
    ;; avoid a compile-time circular dependency.
    ((string= name "mouse")
     (let ((on-p (member value '("on" "true" "1") :test #'equal))
           (pkg   (find-package "CL-TMUX/RENDERER")))
       (when pkg
         (let ((fn (find-symbol (if on-p "ENABLE-MOUSE-REPORTING"
                                        "DISABLE-MOUSE-REPORTING")
                                pkg)))
           (when fn (ignore-errors (funcall fn)))))))
    ;; update-environment: propagate the space-separated name list into
    ;; *update-environment* so that get-update-environment-vars picks it up
    ;; via the dynamic variable rather than re-parsing the option on every call.
    ((string= name "update-environment")
     (when (and (stringp value) (plusp (length value)))
       (setf cl-tmux/model:*update-environment*
             (remove-if (lambda (s) (zerop (length s)))
                        (uiop:split-string value :separator '(#\Space))))))
    ;; status-position: top or bottom adjusts status bar position.
    ;; (stored in options; renderer reads it at render time — no extra side effect needed.)
    ;; terminal-overrides / terminal-features: parse known capability flags.
    ;; The most common use is setting Tc (true-color) or RGB for 24-bit support.
    ;; We accept these silently; the outer terminal already supports true-color,
    ;; so the actual rendering path always uses 24-bit when the color requires it.
    ;; Logging the intent preserves compatibility without behavioral impact.
    ((member name '("terminal-overrides" "terminal-features") :test #'string=)
     ;; No runtime side-effect needed: cl-tmux always emits 24-bit SGR sequences
     ;; when the color is true-color (the renderer handles this transparently).
     ;; The option is stored in *global-options* / *server-options* by the caller.
     nil)
    ))

(defun %apply-set-hook-directive (cmd args)
  "Handle 'set-hook [-r] [-u] event [command]' directives.
   -r or -u flag removes/unsets all hooks for the event; without them, registers
   the command.  The command is stored as a raw string (not converted to keyword)
   so that format variables and arguments (e.g. 'display-message #{session_name}')
   are expanded at hook-fire time via %run-command-line.
   Returns T when handled, NIL otherwise."
  (when (or (string= cmd "set-hook") (string= cmd "hook"))
    (let* ((remove-p (and (first args)
                          (or (string= (first args) "-r")
                              (string= (first args) "-u"))))
           (rest     (if remove-p (rest args) args))
           (event    (first rest))
           ;; The command may be a single quoted token or split across tokens;
           ;; join all remaining tokens as a single command line string.
           (cmd-str  (when (rest rest)
                       (format nil "~{~A~^ ~}" (rest rest)))))
      (when event
        (if remove-p
            (progn (cl-tmux/hooks:clear-command-hooks event) t)
            (when cmd-str
              ;; Store the raw command string for execution at hook-fire time.
              (cl-tmux/hooks:set-command-hook event cmd-str)
              t))))))

;;; ── set-environment flag handling (set-environment -r VAR) ──────────────────
;;;
;;; The fixed-arity table handles only `set-environment VAR VALUE` (2 args).
;;; The `-r` form (unset) passes 2 args: "-r" and VAR, which the fixed-arity
;;; table rejects because arg[0] ≠ a variable name.  This handler intercepts
;;; the unset form before the fixed-arity table gets a chance to reject it.

(defun %apply-set-environment-directive (cmd args)
  "Handle 'set-environment [-g] [-r] VAR [VALUE]' config directives.
   -r unsets the variable (removes it from child-process environment).
   -g is accepted and ignored (global scope is the only scope supported).
   Returns T when handled, NIL otherwise."
  (when (member cmd '("set-environment" "setenv") :test #'string=)
    (let* (;; Consume optional flags: -g (global, default), -r (remove/unset).
           (remove-p   nil)
           (remaining  args))
      (loop while (and remaining
                       (let ((tok (first remaining)))
                         (and (>= (length tok) 2) (char= (char tok 0) #\-))))
            do (let ((tok (pop remaining)))
                 (when (find #\r tok) (setf remove-p t))))
      (let ((var-name  (first remaining))
            (var-value (second remaining)))
        (when var-name
          (if remove-p
              ;; Unset: remove from process environment if sb-posix available.
              (let ((pkg (find-package "SB-POSIX")))
                (when pkg
                  (let ((fn (find-symbol "UNSETENV" pkg)))
                    (when fn (ignore-errors (funcall fn var-name))))))
              ;; Set: value required for non-remove form.
              (when var-value
                (%config-setenv var-name var-value)))
          t)))))

;;; ── if-shell config-time conditional ────────────────────────────────────────
;;;
;;; tmux's `if-shell` can appear as a standalone directive in .tmux.conf:
;;;   if-shell 'uname | grep -q Darwin' 'set -g prefix C-a' 'set -g prefix C-b'
;;; It runs the condition via /bin/sh, then applies THEN-CMD or ELSE-CMD.
;;; This is different from the run-time :if-shell dispatch (which is interactive).

(defun %apply-if-shell-directive (cmd args)
  "Handle 'if-shell CONDITION THEN-CMD [ELSE-CMD]' config directives.
   Runs CONDITION as a shell command; exit 0 means truthy.
   Returns T when handled, NIL otherwise."
  (when (member cmd '("if-shell" "if") :test #'string=)
    (when (>= (length args) 2)
      (let* ((condition (first args))
             (then-cmd  (second args))
             (else-cmd  (third args))
             (exit-code (nth-value 2
                          (ignore-errors
                            (uiop:run-program
                             (list "/bin/sh" "-c" condition)
                             :ignore-error-status t))))
             (truthy-p  (eql exit-code 0))
             (apply-str (if truthy-p then-cmd else-cmd)))
        (when apply-str
          (apply-config-directive (%config-tokens apply-str)))
        t))))

;;; ── command-alias array syntax handling ─────────────────────────────────────
;;;
;;; tmux stores command aliases as an array option in .tmux.conf:
;;;   set -s command-alias[0] e='new-window -n'
;;; The option name carries the index (`command-alias[0]`).  After %strip-set-
;;; flags the positionals look like: ("command-alias[0]" "e=new-window -n").
;;; This function detects that pattern and routes it to the alias registry.

(defun %apply-command-alias-directive (name value)
  "If NAME looks like 'command-alias[N]', parse VALUE as 'alias=expansion'
   and register the alias.  Returns T when handled, NIL otherwise."
  (when (and (>= (length name) 13)
             (string= (subseq name 0 13) "command-alias"))
    (let ((eq-pos (position #\= value)))
      (when eq-pos
        (cl-tmux/options:register-command-alias
         (subseq value 0 eq-pos)
         (subseq value (1+ eq-pos)))
        t))))

;;; ── run-shell / run flag handling (run-shell -b/-t/-d/-C 'cmd') ──────────────
;;;
;;; The fixed-arity table only matches the bare 1-arg form `run-shell 'cmd'`, so
;;; the common real-world `run-shell -b 'cmd'` / `run -b '~/.tmux/...'` forms
;;; (with leading flags) silently failed.  This handler strips leading flags
;;; before the fixed-arity table and runs whatever shell command remains.

(defun %apply-run-shell-directive (cmd args)
  "Handle 'run-shell [-b] [-C] [-t target] [-d delay] shell-command' directives
   (alias 'run').  Consumes leading flags:
     -b           run in background (boolean; we run synchronously regardless)
     -C           run a tmux command instead of a shell command (boolean)
     -t <target>  target pane (takes the next token as its value)
     -d <delay>   delay (takes the next token as its value)
   Unknown leading -X flags: a single bare flag token is skipped to stay
   tolerant.  Stops at the first non-flag token; that token plus any remaining
   tokens (joined by spaces) form the shell command.
   Returns T when CMD is run-shell/run (handled), NIL otherwise."
  (when (member cmd '("run-shell" "run") :test #'string=)
    (let ((tmux-command-p nil)
          (remaining      args))
      ;; Consume leading flag tokens.
      (loop while (and remaining
                       (let ((tok (first remaining)))
                         (and (>= (length tok) 1) (char= (char tok 0) #\-))))
            do (let ((tok (pop remaining)))
                 (cond
                   ((string= tok "-C") (setf tmux-command-p t))
                   ((string= tok "-b")) ; background flag, no argument
                   ((or (string= tok "-t") (string= tok "-d"))
                    ;; These flags take the next token as their value.
                    (when remaining (pop remaining)))
                   ;; Unknown bare -X flag: skip the single flag token only.
                   (t nil))))
      ;; Remaining tokens (joined) form the shell command.
      (let ((command (when remaining
                       (format nil "~{~A~^ ~}" remaining))))
        (cond
          ;; No command after flags: a flag-only invocation is a no-op but handled.
          ((null command) t)
          ;; -C: run a tmux command, not a shell command.  Wiring tmux-command
          ;; execution here is out of scope; treat as handled/no-op for now.
          (tmux-command-p t)
          ;; Shell command: run it the same way the fixed-arity entries do.
          (t
           (let ((expanded (%expand-leading-tilde command)))
             (ignore-errors
               ;; :timeout guards against a hanging run-shell command blocking
               ;; config loading (mirrors %expand-paren in format.lisp).
               (uiop:run-program (list "/bin/sh" "-c" expanded)
                                 :ignore-error-status t :timeout 2)))
           t))))))

(defun apply-config-directive (tokens)
  "Apply one parsed config directive (list of string TOKENS) to live state.
   Returns T when applied, NIL for an unknown/invalid directive.
   Handles bind/unbind, set-hook, set[-g|-a|-s|-u|...], set-environment [-r],
   if-shell, run-shell/run [-b|-C|-t|-d], and the fixed-arity directive table."
  (when tokens
    (let ((cmd  (first tokens))
          (args (rest tokens)))
      (or (%apply-key-directive cmd args)
          (%apply-if-shell-directive cmd args)
          (%apply-set-environment-directive cmd args)
          (%apply-set-directive cmd args)
          (%apply-set-hook-directive cmd args)
          (%apply-run-shell-directive cmd args)
          (%apply-config-directive-inner tokens)))))

(defun apply-config-line (line)
  "Apply a single config LINE.  Blank lines and #-comments are ignored.
   Returns T when a directive was applied."
  (let ((trimmed (string-trim '(#\Space #\Tab #\Return #\Newline) line)))
    (and (plusp (length trimmed))
         (char/= (char trimmed 0) #\#)
         (apply-config-directive (%config-tokens trimmed)))))

;;; ── %if / %else / %endif preprocessor support ───────────────────────────────
;;;
;;; tmux config files may contain conditional blocks:
;;;   %if <condition>
;;;   ...
;;;   %else
;;;   ...
;;;   %endif
;;;
;;; The condition is a tmux format string that evaluates to "1" (truthy) or
;;; "" / "0" (falsy).  A dynamic callback (*config-condition-evaluator*) is used
;;; so the config layer (which cannot depend on cl-tmux/format) can delegate
;;; evaluation to the top-level package which has access to full format expansion.
;;; When the callback is unset, all %if conditions are treated as truthy so that
;;; no directives are silently skipped.

(defvar *config-condition-evaluator* nil
  "When non-NIL, a function (string) → string that evaluates a %if condition.
   The string result is truthy when non-empty and not equal to \"0\".
   NIL means all %if conditions are treated as truthy (nothing skipped).")

(defun %eval-config-condition (cond-str)
  "Evaluate a %if condition string via *config-condition-evaluator*.
   Returns T when the condition is truthy, NIL otherwise.
   Defaults to T when *config-condition-evaluator* is NIL."
  (if *config-condition-evaluator*
      (let ((result (handler-case (funcall *config-condition-evaluator* cond-str)
                      (error () "1"))))
        (and result (plusp (length result)) (not (string= result "0"))))
      t))

(defun %preprocessor-line-p (trimmed)
  "Return :if, :else, :elif, :endif, or NIL indicating whether TRIMMED is a
   preprocessor directive line."
  (cond
    ((and (>= (length trimmed) 3) (string= (subseq trimmed 0 3) "%if")
          (or (= (length trimmed) 3) (not (alpha-char-p (char trimmed 3)))))
     :if)
    ((string= trimmed "%else")
     :else)
    ((and (>= (length trimmed) 5) (string= (subseq trimmed 0 5) "%elif")
          (or (= (length trimmed) 5) (not (alpha-char-p (char trimmed 5)))))
     :elif)
    ((string= trimmed "%endif")
     :endif)
    (t nil)))

(defun %line-brace-delta (line)
  "Net unquoted brace depth of LINE: count of '{' minus '}', ignoring braces
   inside single/double quotes or immediately after a backslash.  Used by
   load-config-from-stream to detect and join multi-line { ... } command blocks
   (tmux 3.x brace syntax)."
  (let ((delta 0) (i 0) (len (length line)))
    (loop while (< i len) do
      (let ((c (char line i)))
        (cond
          ((char= c #\\) (incf i 2))                    ; skip escaped char
          ((char= c #\")                                ; skip double-quoted span
           (incf i)
           (loop while (and (< i len) (char/= (char line i) #\"))
                 do (if (char= (char line i) #\\) (incf i 2) (incf i)))
           (incf i))
          ((char= c #\')                                ; skip single-quoted span
           (incf i)
           (loop while (and (< i len) (char/= (char line i) #\'))
                 do (incf i))
           (incf i))
          ((char= c #\{) (incf delta) (incf i))
          ((char= c #\}) (decf delta) (incf i))
          (t (incf i)))))
    delta))

(defun %read-brace-block (first-line stream)
  "FIRST-LINE has opened an unbalanced { ... } block; keep reading from STREAM
   until the brace depth returns to zero (or EOF), then return all the lines
   joined into one logical line with \" ; \" separators so the inner commands
   become a semicolon sequence the bind parser already understands."
  (let ((depth (%line-brace-delta first-line))
        (parts (list first-line)))
    (loop while (> depth 0)
          for next = (read-line stream nil nil)
          while next
          do (push next parts)
             (incf depth (%line-brace-delta next)))
    (format nil "~{~A~^ ; ~}" (nreverse parts))))

(defun %line-continues-p (line)
  "T when LINE ends with an ODD number of backslashes — a continuation backslash
   that escapes the newline (an even count is escaped backslashes, not a
   continuation)."
  (let ((n 0) (i (1- (length line))))
    (loop while (and (>= i 0) (char= (char line i) #\\))
          do (incf n) (decf i))
    (oddp n)))

(defun %read-logical-config-line (first-line stream)
  "Join trailing-backslash continuation lines into one logical line: while a line
   ends in a continuation backslash, drop that backslash and append the next line.
   Mirrors tmux: `cmd arg1 \\<newline>arg2` is one command.  Returns the joined line."
  (let ((line first-line))
    (loop while (%line-continues-p line)
          for next = (read-line stream nil nil)
          while next
          do (setf line (concatenate 'string
                                     (subseq line 0 (1- (length line)))
                                     next)))
    line))

(defun load-config-from-stream (stream)
  "Apply every directive line read from STREAM, honoring %if/%elif/%else/%endif
   blocks.  Multi-line { ... } command blocks (tmux 3.x brace syntax) are joined
   into a single logical directive before being applied.  Returns the count applied."
  ;; COND-STACK: one state per open %if level — :ACTIVE (this branch is taken),
  ;; :SEEKING (no branch matched yet; keep evaluating %elif/%else), :TAKEN (a branch
  ;; already matched; skip the rest), or :DEAD (an ancestor was skipping when this
  ;; %if began).  A line is applied only when EVERY level is :ACTIVE.  The four
  ;; states are what a plain skip flag cannot express: distinguishing "still seeking
  ;; a match" from "a branch already matched" is required for correct %elif chains.
  (let ((cond-stack nil)
        (count 0))
    (flet ((active-p () (every (lambda (s) (eq s :active)) cond-stack)))
      (loop for raw = (read-line stream nil nil)
            while raw
            ;; Join trailing-backslash continuation lines into one logical line
            ;; before classifying it (so a continued directive is one directive).
            for line = (%read-logical-config-line raw stream) do
        (let* ((trimmed (string-trim '(#\Space #\Tab #\Return #\Newline) line))
               (pp-type (%preprocessor-line-p trimmed)))
          (case pp-type
            (:if
             ;; Only evaluate the condition in an active context; a dead block
             ;; never evaluates (matching tmux's short-circuit).
             (let ((cond-str (string-trim " \t" (subseq trimmed 3))))
               (push (cond ((not (active-p)) :dead)
                           ((%eval-config-condition cond-str) :active)
                           (t :seeking))
                     cond-stack)))
            (:elif
             (when cond-stack
               (let ((cond-str (string-trim " \t" (subseq trimmed 5))))
                 (setf (first cond-stack)
                       (case (first cond-stack)
                         (:seeking (if (%eval-config-condition cond-str) :active :seeking))
                         (:active  :taken)   ; prior branch matched → skip the rest
                         (t        (first cond-stack)))))))   ; :taken / :dead unchanged
            (:else
             (when cond-stack
               (setf (first cond-stack)
                     (case (first cond-stack)
                       (:seeking :active)    ; no branch matched → take the else
                       (:active  :taken)
                       (t        (first cond-stack))))))
            (:endif
             (when cond-stack (pop cond-stack)))
            (otherwise
             ;; Normal line: apply only when every %if level is active.
             (when (active-p)
               ;; Join a multi-line { ... } command block into one logical line.
               (let ((full-line (if (> (%line-brace-delta line) 0)
                                    (%read-brace-block line stream)
                                    line)))
                 (when (apply-config-line full-line)
                   (incf count)))))))))
    count))

(defun load-config-from-string (text)
  "Apply every directive line in TEXT, honoring %if/%else/%endif blocks.
   Returns the count of directives applied."
  (with-input-from-string (in text)
    (load-config-from-stream in)))

(defun %env-set-p (env-string)
  "True when environment variable string ENV-STRING is set and non-empty."
  (and env-string (plusp (length env-string))))

(defun %config-path-from (override xdg home)
  "Resolve the config-file path from environment values (OVERRIDE = $CL_TMUX_CONF,
   XDG = $XDG_CONFIG_HOME, each a string or NIL) and HOME (a directory pathname).

   Precedence (XDG Base Directory spec):
     1. $CL_TMUX_CONF                              — explicit override
     2. $XDG_CONFIG_HOME/cl-tmux/cl-tmux.conf
     3. ~/.config/cl-tmux/cl-tmux.conf             — XDG default when unset
   Empty strings are treated as unset.  Pure: no I/O, no environment access."
  (if (%env-set-p override)
      (pathname override)
      (let ((base (if (%env-set-p xdg)
                      xdg
                      (namestring (merge-pathnames ".config/" home)))))
        (pathname (format nil "~A/cl-tmux/cl-tmux.conf"
                          (string-right-trim "/" base))))))

(defun %tmux-conf-paths (home)
  "Return a list of candidate .tmux.conf paths in priority order:
     1. $XDG_CONFIG_HOME/tmux/tmux.conf
     2. ~/.config/tmux/tmux.conf  (XDG default)
     3. ~/.tmux.conf              (traditional location)"
  (let* ((xdg  (sb-ext:posix-getenv "XDG_CONFIG_HOME"))
         (base (if (%env-set-p xdg)
                   xdg
                   (namestring (merge-pathnames ".config/" home)))))
    (list (pathname (format nil "~A/tmux/tmux.conf"
                            (string-right-trim "/" base)))
          (merge-pathnames ".tmux.conf" home))))

(defun config-file-path ()
  "Path to the user config file, honoring $CL_TMUX_CONF then the XDG Base
   Directory spec ($XDG_CONFIG_HOME, default ~/.config).  See %config-path-from."
  (%config-path-from (sb-ext:posix-getenv "CL_TMUX_CONF")
                     (sb-ext:posix-getenv "XDG_CONFIG_HOME")
                     (user-homedir-pathname)))

(defun load-config-file (&optional (path (config-file-path)))
  "Load and apply the config file at PATH if it exists (returns the count of
   directives applied), or NIL when no file is found.
   PATH defaults to the XDG/cl-tmux path; pass NIL to auto-detect, which also
   searches the standard .tmux.conf locations for compatibility."
  (if path
      (with-open-file (in path :direction :input :if-does-not-exist nil)
        (when in (load-config-from-stream in)))
      ;; Auto-detect: try each candidate path in priority order.
      (let ((home (user-homedir-pathname)))
        (dolist (candidate (cons (config-file-path)
                                 (%tmux-conf-paths home)))
          (with-open-file (in candidate :direction :input :if-does-not-exist nil)
            (when in
              (return (load-config-from-stream in))))))))
