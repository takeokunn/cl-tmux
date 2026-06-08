(in-package #:cl-tmux/config)

;;; ASCII 2 = ^B.  tmux uses C-b as the default prefix.
(defconstant +prefix-key-code+ 2)

;;; ── Runtime-modifiable prefix key ────────────────────────────────────────
;;;
;;; *prefix-key-code* shadows +prefix-key-code+ for the event loop so that
;;; "set -g prefix C-a" (or any other .tmux.conf directive) can remap the
;;; prefix at runtime without recompiling.  The constant is preserved for
;;; compile-time uses (e.g., test expectations about the default value).

(defparameter *prefix-key-code* +prefix-key-code+
  "Runtime prefix key byte.  Default 2 (C-b).  Change via 'set -g prefix C-a'.")

(defparameter *prefix2-key-code* nil
  "Second prefix key byte, or NIL when not set.  Changed via 'set -g prefix2 C-a'.
   When set, pressing this key also arms the prefix key-table (same as the primary prefix).")

(defun %parse-prefix-key (key-str)
  "Parse a tmux key name KEY-STR into a byte value.
   Supports: C-x control (logand char 0x1f), single printable char.
   Returns an integer, or NIL for unrecognized names."
  (cond
    ((and (= (length key-str) 3)
          (char-equal (char key-str 0) #\C)
          (char= (char key-str 1) #\-))
     (logand (char-code (char key-str 2)) #x1f))
    ((= (length key-str) 1)
     (char-code (char key-str 0)))
    (t nil)))

;;; ── Key-table name constants ──────────────────────────────────────────────
;;;
;;; All references to the standard table names use these constants so a typo
;;; is caught at compile time and a rename touches only one place.

(defconstant +table-prefix+
  (if (boundp '+table-prefix+) +table-prefix+ "prefix")
  "Name of the default key-table (requires prefix key).")
(defconstant +table-root+
  (if (boundp '+table-root+) +table-root+ "root")
  "Name of the root key-table (no prefix required).")
(defconstant +table-copy-mode+
  (if (boundp '+table-copy-mode+) +table-copy-mode+ "copy-mode")
  "Name of the copy-mode key-table.")

;;; ── Shell default ─────────────────────────────────────────────────────────
;;;
;;; *default-shell* starts as "/bin/sh".  The ORCHESTRATE layer (main.lisp)
;;; calls init-default-shell at startup to read $SHELL from the environment.
;;; This keeps the DATA-layer defparameter free of I/O side-effects.

(defparameter *default-shell* "/bin/sh"
  "Shell binary launched for new panes.")

(defun init-default-shell ()
  "Set *DEFAULT-SHELL* from $SHELL if that variable is set and non-empty.
   Call this once at program startup (in main.lisp) before forking any panes."
  (let ((shell (sb-ext:posix-getenv "SHELL")))
    (when (and shell (plusp (length shell)))
      (setf *default-shell* shell))))

(defparameter *status-height* 1
  "Number of rows reserved for the status bar at the bottom.")

(defconstant +pty-buf-size+ 4096
  "Byte buffer size for PTY reads.")

(defconstant +max-scrollback-lines+ 1000
  "Maximum rows retained in the per-pane scrollback buffer.")

(defconstant +poll-timeout-us+ 50000
  "Select timeout in microseconds for stdin/socket polling (50 ms ≈ 20 fps max).")

(defconstant +accept-timeout-us+ 100000
  "Select timeout in microseconds for the server accept-connection loop (100 ms).
   Prevents blocking forever so *running* is checked between connection attempts.")

(defconstant +pty-poll-timeout-us+ 50000
  "Select timeout in microseconds for per-pane PTY reader threads (50 ms).
   Allows the reader loop to observe *running* even when the shell is silent.")

;;; ── Key-table system ──────────────────────────────────────────────────────
;;;
;;; *key-tables* maps table-name (string) → hash-table of chord → (keyword . flags).
;;; Flags is a plist; :repeatable T means the prefix stays active after dispatch.
;;; Standard table names: +table-prefix+, +table-root+, +table-copy-mode+.
;;;
;;; set-key-binding / remove-key-binding are thin wrappers around key-table-bind
;;; for the prefix table — the only table that matters for the main dispatch path.

(defparameter *key-tables*
  (make-hash-table :test #'equal)
  "Hash-table mapping table-name string → inner hash-table of chord → (keyword . flags).")

(defun ensure-key-table (name)
  "Return the inner hash-table for key-table NAME, creating it if absent.
   Idempotent: repeated calls with the same NAME return the same object."
  (or (gethash name *key-tables*)
      (setf (gethash name *key-tables*)
            (make-hash-table :test #'equal))))

(defun key-table-bind (table key command &key repeatable note)
  "Add a binding for KEY → COMMAND in TABLE (a table-name string).
   :REPEATABLE T marks the binding so the prefix table stays active after dispatch.
   :NOTE is an optional description string (from `bind -N`) shown by list-keys."
  (let ((inner (ensure-key-table table)))
    (setf (gethash key inner)
          (cons command (list :repeatable repeatable :note note)))))

(defun key-table-lookup (table key)
  "Return the (command . flags) cons for KEY in TABLE, or NIL."
  (let ((inner (gethash table *key-tables*)))
    (when inner (gethash key inner))))

(defun key-table-command (entry)
  "Extract the command keyword from a key-table entry (car)."
  (car entry))

(defun key-table-repeatable-p (entry)
  "Return T if the key-table entry is marked repeatable.
   Safe to call with NIL (returns NIL without signaling)."
  (and entry (getf (cdr entry) :repeatable)))

(defun key-table-note (entry)
  "Return the -N description string for a key-table ENTRY, or NIL.
   Safe to call with NIL (returns NIL without signaling)."
  (and entry (getf (cdr entry) :note)))

;;; ── Initial key-binding data (declarative) ────────────────────────────────
;;;
;;; define-initial-key-bindings registers key-table bindings at load time.
;;; After load, set-key-binding / remove-key-binding delegate to key-table-bind
;;; directly — no sync step needed.

(defmacro define-initial-key-bindings (&rest pairs)
  "Define INSTALL-DEFAULT-PREFIX-BINDINGS, a function that populates the prefix
   key-table with the standard default bindings.  Each PAIR is
   (char-literal command-keyword); the special entry (:digits command) binds
   digit chars 0-9 to COMMAND.

   Defining a function (rather than emitting top-level side effects) lets
   INITIALIZE-DEFAULT-KEY-TABLES reinstall the defaults whenever *key-tables*
   is rebuilt — notably under test isolation (with-isolated-config), which
   binds a fresh empty *key-tables* and must restore the standard bindings so
   that e.g. unbind tests find #\\c bound to :new-window."
  `(defun install-default-prefix-bindings ()
     "Bind every standard default key into the prefix key-table. Idempotent."
     ,@(mapcan
        (lambda (pair)
          (if (eq (first pair) :digits)
              (loop for d from 0 to 9
                    collect `(key-table-bind +table-prefix+ (digit-char ,d) ,(second pair)))
              `((key-table-bind +table-prefix+ ,(first pair) ,(second pair)))))
        pairs)
     (values)))

(define-initial-key-bindings
  (#\c :new-window)
  (#\n :next-window)
  (#\p :prev-window)
  (#\" :split-horizontal)
  (#\% :split-vertical)
  (#\o :next-pane)
  (#\d :detach)
  (#\? :list-keys)
  (#\[ :copy-mode-enter)
  (#\] :paste-buffer)
  (#\x :kill-pane-confirm)
  (#\& :kill-window-confirm)
  (#\, :rename-window)
  (#\H :resize-left)
  (#\J :resize-down)
  (#\K :resize-up)
  (#\L :resize-right)
  (#\Z :zoom-toggle)
  (#\$ :rename-session)
  (#\! :if-shell)
  (:digits :select-window))

;;; ── Key-binding accessors (thin wrappers over key-tables) ─────────────────

(defun lookup-key-binding (key)
  "Return the command keyword bound to KEY (a character or string), or NIL.
   Looks up the prefix key-table."
  (let ((entry (key-table-lookup +table-prefix+ key)))
    (and entry (key-table-command entry))))

(defun key-label (key)
  "Return a display string for KEY: characters become single-character strings,
   strings are returned as-is."
  (if (characterp key) (string key) key))

(defun %binding-label (command)
  "Human-readable label for a binding's COMMAND value: a reconstructed command
   line for a token list (`bind key cmd args`), or the lowercased keyword name
   for a built-in command."
  (if (consp command)
      (format nil "~{~A~^ ~}" command)
      (format nil "~(~A~)" command)))

(defun %sorted-table-names ()
  "Return all key-table names from *KEY-TABLES* in alphabetical order."
  (sort (loop for name being the hash-keys of *key-tables* collect name)
        #'string<))

(defun %table-binding-alist (inner)
  "Build an alist of (key . entry) from INNER (an inner key-table hash-table).
   The whole entry (command . flags) is kept so callers can read the -N note."
  (loop for key being the hash-keys of inner
        using (hash-value entry)
        collect (cons key entry)))

(defun %format-binding-line (table-name key entry)
  "Format one `bind-key -T TABLE-NAME [-N note] KEY COMMAND` line.
   When ENTRY carries an -N note it is emitted as `-N \"note\"` before the key,
   matching tmux's list-keys -N output."
  (let ((note (key-table-note entry)))
    (format nil "bind-key -T ~A ~@[-N \"~A\" ~]~A ~A~%"
            table-name
            note
            (key-label key)
            (%binding-label (key-table-command entry)))))

(defun %describe-one-table (out table-name)
  "Write bind-key lines for TABLE-NAME to OUT stream.  No-op when table is absent."
  (let* ((inner (gethash table-name *key-tables*)))
    (when inner
      (let ((bindings (sort (%table-binding-alist inner)
                            #'string< :key (lambda (b) (key-label (car b))))))
        (dolist (binding bindings)
          (write-string (%format-binding-line table-name (car binding) (cdr binding))
                        out))))))

(defun describe-key-bindings ()
  "Return bind-key -T table key command lines for all key tables.
   Output format matches real tmux list-keys: one binding per line,
   sorted by table name then by key within each table."
  (with-output-to-string (out)
    (dolist (table-name (%sorted-table-names))
      (%describe-one-table out table-name))))

(defun describe-key-bindings-for-table (table-name)
  "Return bind-key lines for TABLE-NAME only.
   When TABLE-NAME is NIL, returns all tables (same as DESCRIBE-KEY-BINDINGS).
   Returns an empty string when TABLE-NAME names a non-existent table."
  (if (null table-name)
      (describe-key-bindings)
      (with-output-to-string (out)
        (%describe-one-table out table-name))))

(defun set-key-binding (key command)
  "Bind KEY (a character or string) to COMMAND (a keyword) in the prefix table.
   Returns COMMAND."
  (key-table-bind +table-prefix+ key command)
  command)

(defun remove-key-binding (key)
  "Remove any binding for KEY (a character or string) from the prefix table."
  (let ((tbl (gethash +table-prefix+ *key-tables*)))
    (when tbl (remhash key tbl))))

;;; ── Initialisation ────────────────────────────────────────────────────────

(defun initialize-default-key-tables ()
  "Install the standard default prefix bindings, the C-b C-b → :send-prefix
   binding, and ensure the root/copy-mode tables exist.
   Called once at load time and by test isolation helpers (idempotent)."
  (install-default-prefix-bindings)
  (set-key-binding (code-char +prefix-key-code+) :send-prefix)
  (ensure-key-table +table-root+)
  (ensure-key-table +table-copy-mode+))

;;; Initialise tables at load time.
(initialize-default-key-tables)
