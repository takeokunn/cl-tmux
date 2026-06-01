(in-package #:cl-tmux/config)

;;; ASCII 2 = ^B.  tmux uses C-b as the default prefix.
(defconstant +prefix-key-code+ 2)

(defparameter *default-shell*
  (or (sb-ext:posix-getenv "SHELL")
      "/bin/sh")
  "Shell binary launched for new panes.")

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
;;; Standard table names: "prefix", "root", "copy-mode".
;;;
;;; *current-key-table* tracks which table is active during input processing.

(defparameter *key-tables*
  (make-hash-table :test #'equal)
  "Hash-table mapping table-name string → inner hash-table of chord → (keyword . flags).")

(defparameter *current-key-table* "root"
  "Name of the active key-table for input dispatch (dynamic variable).")

(defun ensure-key-table (name)
  "Return the inner hash-table for key-table NAME, creating it if absent."
  (or (gethash name *key-tables*)
      (setf (gethash name *key-tables*)
            (make-hash-table :test #'equal))))

(defun key-table-bind (table key command &key repeatable)
  "Add a binding for KEY → COMMAND in TABLE (a table-name string).
   :REPEATABLE T marks the binding so the prefix table stays active after dispatch."
  (let ((inner (ensure-key-table table)))
    (setf (gethash key inner) (cons command (list :repeatable repeatable)))))

(defun key-table-lookup (table key)
  "Return the (command . flags) cons for KEY in TABLE, or NIL."
  (let ((inner (gethash table *key-tables*)))
    (when inner (gethash key inner))))

(defun key-table-command (entry)
  "Extract the command keyword from a key-table entry (car)."
  (car entry))

(defun key-table-repeatable-p (entry)
  "Return T if the key-table entry is marked repeatable."
  (and entry (getf (cdr entry) :repeatable)))

;;; After receiving the prefix key, the next keystroke (a character or a
;;; multi-character string like \"M-1\") is looked up here.
;;; Each entry is (char-or-string . keyword).

(defmacro define-initial-key-bindings (&rest pairs)
  "Build the initial *key-bindings* alist from a declarative table.
   Each PAIR is (char-literal command-keyword).
   The special entry (:digits command) binds digit chars 0-9 to COMMAND."
  `(list ,@(mapcan
            (lambda (pair)
              (if (eq (first pair) :digits)
                  (loop for d from 0 to 9
                        collect `(cons (digit-char ,d) ,(second pair)))
                  `((cons ,(first pair) ,(second pair)))))
            pairs)))

(defparameter *key-bindings*
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
  "Prefix-key dispatch alist of (char-or-string . keyword).
   Built from the table above; mutated at runtime by set-key-binding.")

(defun lookup-key-binding (key)
  "Return the command keyword bound to KEY (a character or string), or NIL."
  (cdr (assoc key *key-bindings* :test #'equal)))

(defun describe-key-bindings ()
  "A newline-separated, key-sorted listing of the current prefix bindings
   (\"<key>  <command>\" per line) for the list-keys help overlay.
   Pure: reads *KEY-BINDINGS* without mutating it (copy-list before sort)."
  (flet ((key-label (k) (if (characterp k) (string k) k)))
    (with-output-to-string (out)
      (write-string "key bindings — press prefix (C-b) then:" out)
      (dolist (binding (sort (copy-list *key-bindings*) #'string<
                             :key (lambda (b) (key-label (car b)))))
        (format out "~%  ~A  ~(~A~)" (key-label (car binding)) (cdr binding))))))

(defun set-key-binding (key command)
  "Bind KEY (a character or string) to COMMAND (a keyword) in *KEY-BINDINGS*,
   replacing any existing binding for KEY.  Returns COMMAND."
  (let ((existing (assoc key *key-bindings* :test #'equal)))
    (if existing
        (setf (cdr existing) command)
        (push (cons key command) *key-bindings*)))
  command)

(defun remove-key-binding (key)
  "Remove any binding for KEY (a character or string) from *KEY-BINDINGS*."
  (setf *key-bindings* (remove key *key-bindings* :key #'car :test #'equal)))

;;; ── Populate key-tables from *key-bindings* ───────────────────────────────
;;;
;;; Called once at load time; also called from config-directives when bind/unbind
;;; is processed.  Copies the prefix-table alist into *key-tables* "prefix".

(defun sync-key-tables-from-bindings ()
  "Copy current *key-bindings* alist into the \"prefix\" key-table, replacing it."
  (let ((prefix-table (ensure-key-table "prefix")))
    ;; Clear existing entries
    (clrhash prefix-table)
    ;; Populate from *key-bindings* alist
    (dolist (binding *key-bindings*)
      (setf (gethash (car binding) prefix-table)
            (cons (cdr binding) '(:repeatable nil)))))
  ;; Ensure "root" and "copy-mode" tables exist
  (ensure-key-table "root")
  (ensure-key-table "copy-mode"))

;;; Bind C-b C-b (prefix then byte 2) to :send-prefix so the user can send
;;; a literal prefix byte to the running program.  This cannot be expressed
;;; as a character literal in define-initial-key-bindings because the
;;; character has code 2 (STX / Control-B), so we push it at load time.
(push (cons (code-char +prefix-key-code+) :send-prefix) *key-bindings*)

;;; Initialise tables at load time.
(sync-key-tables-from-bindings)

