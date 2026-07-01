(in-package #:cl-tmux/config)

;;; ASCII 2 = ^B.  tmux uses C-b as the default prefix.
(defconstant +prefix-key-code+ 2)

;;; Control-character mask: (logand char-code +ctrl-mask+) yields the
;;; corresponding ASCII control byte.  Appears in prefix-key parsing and
;;; control-character table lookup; a single named constant avoids the magic
;;; literal #x1f from appearing in three separate source locations.
(defconstant +ctrl-mask+ #x1f
  "Bitmask used to convert an ASCII letter to its control-character code.
   (logand char-code +ctrl-mask+) = byte sent when Ctrl is held with that key.")

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

(defun %prefix-control-byte (rest)
  "Map REST (the part after a \"C-\"/\"^\" prefix) to its control BYTE, or NIL.
   C-a..C-z -> 1..26; C-Space / C-@ -> 0 (NUL); C-[ C-\\ C-] C-^ C-_ -> 27..31.
   Self-contained (config.lisp loads before config-tokenizer.lisp), so it does
   not call %parse-control-char."
  (cond
    ((string-equal rest "Space") 0)
    ((= (length rest) 1)
     (let ((c (char-upcase (char rest 0))))
       (cond
         ((char= c #\@) 0)
         ((char<= #\A c #\Z) (logand (char-code c) +ctrl-mask+))
         ((member c '(#\[ #\\ #\] #\^ #\_) :test #'char=)
          (logand (char-code c) +ctrl-mask+))
         (t nil))))
    (t nil)))

(defun %parse-prefix-key (key-str)
  "Parse a tmux key name KEY-STR into a byte value the single-byte event-loop
   prefix check can match.
   Supports: C-<key> and caret ^<key> control notation (incl. C-Space/C-@ -> NUL
   and C-[ C-\\ C-] C-^ C-_), and a single printable char.  \"None\"/\"Any\" and any
   other named key (M-a, F1, ...) return NIL — the byte event loop cannot match
   those; %bind-prefix-key treats \"None\" as an explicit disable.
   Returns an integer byte, or NIL for unmatchable/unrecognized names."
  (cond
    ((or (string-equal key-str "None") (string-equal key-str "Any")) nil)
    ;; "C-<key>" modifier notation.
    ((and (> (length key-str) 2)
          (char-equal (char key-str 0) #\C)
          (char= (char key-str 1) #\-))
     (%prefix-control-byte (subseq key-str 2)))
    ;; Caret control notation: ^A, ^[, ^@ (alias for C-<key>).
    ((and (> (length key-str) 1)
          (char= (char key-str 0) #\^))
     (%prefix-control-byte (subseq key-str 1)))
    ((= (length key-str) 1)
     (char-code (char key-str 0)))
    (t nil)))

;;; ── Key-table name constants ──────────────────────────────────────────────
;;;
;;; All references to the standard table names use these constants so a typo
;;; is caught at compile time and a rename touches only one place.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %defconstant-rebind-guard (symbol fallback)
    "Return SYMBOL's current value when it is already bound, else FALLBACK.
     Guards defconstant re-evaluation at image reload: SBCL signals an error when
     a defconstant is re-evaluated with a different value, so we preserve the
     already-bound value rather than supplying the literal again."
    (if (boundp symbol)
        (symbol-value symbol)
        fallback)))

(defconstant +table-prefix+
  (%defconstant-rebind-guard '+table-prefix+ "prefix")
  "Name of the default key-table (requires prefix key).")
(defconstant +table-root+
  (%defconstant-rebind-guard '+table-root+ "root")
  "Name of the root key-table (no prefix required).")
(defconstant +table-copy-mode+
  (%defconstant-rebind-guard '+table-copy-mode+ "copy-mode")
  "Name of the copy-mode key-table.")
(defconstant +table-copy-mode-vi+
  (%defconstant-rebind-guard '+table-copy-mode-vi+ "copy-mode-vi")
  "Name of the vi copy-mode key-table.")

;;; ── Shell default ─────────────────────────────────────────────────────────
;;;
;;; *default-shell* starts as "/bin/sh".  The ORCHESTRATE layer (main.lisp)
;;; calls init-default-shell at startup to read $SHELL from the environment.
;;; This keeps the DATA-layer defparameter free of I/O side-effects.

(defparameter *default-shell* "/bin/sh"
  "Shell binary launched for new panes.")

(defun init-default-shell ()
  "Set *DEFAULT-SHELL* from $SHELL if that variable is set and non-empty.
   Call this once at program startup (in main.lisp) before spawning any panes."
  (let ((shell (sb-ext:posix-getenv "SHELL")))
    (when (and shell (plusp (length shell)))
      (setf *default-shell* shell))))

(defparameter *status-height* 1
  "Number of rows reserved for the status bar at the bottom.")

(defconstant +max-status-lines+ 5
  "tmux's maximum status-line count; `set -g status N` clamps N to this cap.")

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
;;; Standard table names: +table-prefix+, +table-root+,
;;; +table-copy-mode+, +table-copy-mode-vi+.
;;;
;;; The prefix table is the main dispatch path; callers should mutate it
;;; through key-table-bind / key-table-unbind directly.

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
;;; After load, callers mutate the prefix table directly — no sync step needed.

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
  ;; These are the bootstrap defaults. events-loop.lisp installs the live
  ;; bindings after startup (#\L -> last-session, #\! -> break-pane).
  (#\L :resize-right)
  (#\Z :zoom-toggle)        ; uppercase alias; events-loop.lisp adds lowercase z
  (#\$ :rename-session)
  (#\! :if-shell)
  (:digits :select-window))

(defun install-default-prefix-string-bindings ()
  "Bind standard multi-byte prefix keys into the prefix key-table."
  (dolist (binding '(("Up"    :select-pane-up)
                     ("Down"  :select-pane-down)
                     ("Left"  :select-pane-left)
                     ("Right" :select-pane-right)))
    (destructuring-bind (key command) binding
      (key-table-bind +table-prefix+ key command)))
  (dolist (binding '(("C-Up"    ("resize-pane" "-U" "1"))
                     ("C-Down"  ("resize-pane" "-D" "1"))
                     ("C-Left"  ("resize-pane" "-L" "1"))
                     ("C-Right" ("resize-pane" "-R" "1"))
                     ("M-Up"    ("resize-pane" "-U" "5"))
                     ("M-Down"  ("resize-pane" "-D" "5"))
                     ("M-Left"  ("resize-pane" "-L" "5"))
                     ("M-Right" ("resize-pane" "-R" "5"))))
    (destructuring-bind (key command) binding
      (key-table-bind +table-prefix+ key command :repeatable t)))
  (values))

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; Resolve against the system source directory (robust to CWD and to ASDF
  ;; loading the compiled fasl from its cache), matching package.lisp.
  (let ((root (or (ignore-errors (asdf:system-source-directory :cl-tmux))
                  *load-pathname*
                  *compile-file-pathname*)))
    (load (merge-pathnames #P"src/application/config/config-listing.lisp" root))))

;;; ── Prefix-table helpers ─────────────────────────────────────────────────

(defun key-table-unbind (table key)
  "Remove any binding for KEY from TABLE and return T when TABLE existed."
  (let ((tbl (gethash table *key-tables*)))
    (when tbl
      (remhash key tbl)
      t)))

;;; ── Copy-mode binding helpers ────────────────────────────────────────────
;;;
;;; %bind-copy-mode-named-navigation and %bind-copy-mode-bindings are helper
;;; functions; the binding DATA tables (+default-copy-mode-bindings+,
;;; +default-copy-mode-vi-bindings+) and the install-* functions that use them
;;; live in config-copy-mode-defaults.lisp, loaded below.

(defun %bind-copy-mode-named-navigation (table-name)
  "Install common named-key copy-mode navigation bindings into TABLE-NAME."
  (dolist (binding '(("Up"       :copy-mode-cursor-up)
                     ("Down"     :copy-mode-cursor-down)
                     ("Left"     :copy-mode-cursor-left)
                     ("Right"    :copy-mode-cursor-right)
                     ("C-Up"     :copy-mode-scroll-up-line)
                     ("C-Down"   :copy-mode-scroll-down-line)
                     ("PageUp"   :copy-mode-page-up)
                     ("PageDown" :copy-mode-page-down)
                     ("Home"     :copy-mode-line-start)
                     ("End"      :copy-mode-line-end)))
    (destructuring-bind (key command) binding
      (key-table-bind table-name key command)))
  (values))

(defun %bind-copy-mode-bindings (table-name bindings)
  "Bind each (KEY COMMAND) pair in BINDINGS into TABLE-NAME."
  (dolist (binding bindings)
    (destructuring-bind (key command) binding
      (key-table-bind table-name key command)))
  (values))

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; Load copy-mode binding data: +default-copy-mode-bindings+,
  ;; +default-copy-mode-vi-bindings+, install-default-copy-mode-bindings,
  ;; install-default-copy-mode-vi-bindings.
  (let ((root (or (ignore-errors (asdf:system-source-directory :cl-tmux))
                  *load-pathname*
                  *compile-file-pathname*)))
    (load (merge-pathnames #P"src/application/config/config-copy-mode-defaults.lisp" root))))

;;; ── Initialisation ────────────────────────────────────────────────────────

(defun initialize-default-key-tables ()
  "Install the standard default prefix bindings, the C-b C-b -> :send-prefix
   binding, and ensure the root/copy-mode tables exist.
   Called once at load time and by test isolation helpers (idempotent)."
  (install-default-prefix-bindings)
  (install-default-prefix-string-bindings)
  (key-table-bind +table-prefix+ (code-char +prefix-key-code+) :send-prefix)
  (ensure-key-table +table-root+)
  (ensure-key-table +table-copy-mode+)
  (ensure-key-table +table-copy-mode-vi+)
  (install-default-copy-mode-bindings)
  (install-default-copy-mode-vi-bindings))

;;; Initialise tables at load time.
(initialize-default-key-tables)
