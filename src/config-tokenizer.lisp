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
         ((char<= #\A c #\Z) (code-char (logand (char-code c) +ctrl-mask+)))
         ((member c '(#\[ #\\ #\] #\^ #\_) :test #'char=)
          (code-char (logand (char-code c) +ctrl-mask+)))
         (t nil))))
    (t nil)))

(defparameter *key-name-aliases*
  '(("PPage" . "PageUp")   ("PgUp" . "PageUp")
    ("NPage" . "PageDown") ("PgDn" . "PageDown")
    ("IC"    . "Insert")
    ("DC"    . "Delete"))
  "tmux navigation-key spellings that denote the same key as a canonical name.
   Both spellings must collapse to one string so the bind-side key and the
   event-loop's emitted key (see %csi-tilde-key-name) match in the key table.")

(defun %normalize-key-alias (token)
  "Return the canonical key name for TOKEN when it is a known alias (case-
   insensitively), else NIL.  Lets `bind -n PPage` and `bind -n PageUp` resolve
   to the same binding."
  (cdr (assoc token *key-name-aliases* :test #'string-equal)))

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
    ;; Navigation-key aliases → the canonical name the event loop emits for the
    ;; corresponding ESC [ N ~ sequence (see %csi-tilde-key-name).  Without this
    ;; `bind -n PPage <cmd>` would store "PPage" while the keypress resolves to
    ;; "PageUp", and the binding would never fire.
    ((%normalize-key-alias token))
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
    :choose-client :choose-tree :refresh-client :suspend-client :customize-mode
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

