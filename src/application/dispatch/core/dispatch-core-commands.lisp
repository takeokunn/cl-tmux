(in-package #:cl-tmux)

;;;; Copy-mode key overrides, format helpers, new-session factory,
;;;;  and named-command table (C-b : prompt resolution).

;;; ── Copy-mode key overrides macro ────────────────────────────────────────────

(defmacro define-copy-mode-key-overrides (&rest rules)
  "Build a copy-mode key-lookup function from a declarative override table.
   Each RULE is either (char keyword) or ((char ...) keyword). When in copy
   mode, CH is checked against the override table before the normal key-binding
   lookup.
   Generates %COPY-MODE-CMD that returns the override or the normal binding."
  `(defun %copy-mode-cmd (ch)
     "Return the command for CH when copy mode is active."
     (cond
       ,@(mapcar (lambda (rule)
                   (destructuring-bind (chars kw) rule
                     (if (listp chars)
                         `((and ch (member ch ',chars :test #'char=)) ,kw)
                         `((and ch (char= ch ,chars)) ,kw))))
                 rules)
       (t (and ch (lookup-key-binding ch))))))

(defun new-session (name rows cols &key start-dir)
  "Create a new session named NAME with a full-screen window of ROWS x COLS.
   START-DIR: when non-NIL, the initial shell starts in that directory.
   Registers the session in *server-sessions* and starts reader threads."
  (let ((session (create-initial-session rows cols :start-dir start-dir)))
    (setf (session-name session) name)
    (session-touch session)
    (server-add-session session)
    (dolist (pane (all-panes session))
      (start-reader-thread pane))
    (cl-tmux/hooks:run-hooks cl-tmux/hooks:+hook-session-created+ session)
    session))

(define-copy-mode-key-overrides
  ((#\q #\i) :copy-mode-exit)
  (#\Space :copy-mode-begin-selection)
  (#\v :copy-mode-begin-selection)
  (#\V :copy-mode-begin-line-selection)
  (#\y :copy-mode-yank)
  (#\w :copy-mode-word-forward)
  (#\b :copy-mode-word-backward)
  (#\e :copy-mode-word-end)
  (#\W :copy-mode-space-forward)
  (#\B :copy-mode-space-backward)
  (#\E :copy-mode-space-end)
  (#\0 :copy-mode-line-start)
  (#\^ :copy-mode-back-to-indentation)
  (#\$ :copy-mode-line-end)
  (#\g :copy-mode-top)
  (#\G :copy-mode-bottom)
  (#\H :copy-mode-high)
  (#\M :copy-mode-middle)
  (#\L :copy-mode-low)
  (#\D :copy-mode-copy-pipe-end-of-line-and-cancel)
  (#\Y :copy-mode-copy-line)
  (#\n :copy-mode-search-next)
  (#\N :copy-mode-search-prev)
  (#\/ :copy-mode-search-forward-prompt)
  (#\? :copy-mode-search-backward-prompt)
  (#\= :copy-mode-choose-buffer))

(defun %active-copy-mode-table ()
  "Return the copy-mode key table selected by the mode-keys option."
  (if (string= (cl-tmux/options:get-option "mode-keys" "emacs") "vi")
      "copy-mode-vi"
      +table-copy-mode+))

;;; -- Session list formatter helper -------------------------------------------
;;;
;;; :list-sessions, :list-sessions-full, :choose-session, and :choose-tree all
;;; produce the same "* N: name (W window[s])" line format.  A single helper
;;; keeps the loop in one place.

(defun %format-session-list (current-session)
  "Return a formatted string listing all sessions in *server-sessions*.
   The session matching CURRENT-SESSION is marked with an asterisk.
   Falls back to a single entry for CURRENT-SESSION when the registry is empty."
  (let ((current-name (session-name current-session)))
    (with-output-to-string (s)
      (loop for (name . sess) in (or *server-sessions*
                                     (list (cons current-name current-session)))
            for i from 0
            do (format s "~A~A: ~A (~D window~:P)~%"
                       (if (string= name current-name) "*" " ")
                       i name
                       (length (session-windows sess)))))))

;;; -- Choose-tree entry formatter helper --------------------------------------
;;;
;;; :choose-tree needs to render one session + its windows.  Both the
;;; *server-sessions* branch and the fallback branch share this logic.

(defun %format-tree-entry (stream session-name current-session-name windows active-window)
  "Write one session entry (SESSION-NAME + window list) to STREAM.
   Current session is marked with an asterisk.  ACTIVE-WINDOW marks the active
   window within that session."
  (format stream "~A~A~%"
          (if (string= session-name current-session-name) "* " "  ")
          session-name)
  (dolist (win windows)
    (format stream "    ~A~A: ~A~%"
            (if (eq win active-window) "*" " ")
            (window-id win)
            (window-name win))))

;;; -- Menu formatter helper ---------------------------------------------------

(defun %format-menu (menu)
  "Format a MENU struct into a displayable overlay string."
  (let ((title (menu-title menu))
        (items (menu-items menu))
        (sel   (menu-selected-index menu)))
    (with-output-to-string (s)
      (format s "┌─ ~A ─┐~%" title)
      (loop for (label . _cmd) in items
            for i from 0
            do (format s "~A ~A~%"
                       (if (= i sel) "▶" " ")
                       label))
      (format s "└~A┘"
              (make-string (+ 4 (length title)) :initial-element #\─)))))

;;; -- Signal-channel prompt helper --------------------------------------------
;;;
;;; :wait-for and :wait-for-signal had identical bodies; %signal-channel-prompt
;;; factors out the common logic so a single form removes the duplication.

(defun %signal-channel-prompt (prompt-label)
  "Open a prompt labelled PROMPT-LABEL; on submit signal the named channel
   and show a confirmation overlay."
  (prompt-nonempty prompt-label
                   (lambda (name)
                     (signal-channel name)
                     (%overlayf "signaled channel: ~A" name))))

;;; -- Toggle-synchronize-panes helper -----------------------------------------
;;;
;;; The :synchronize-panes handler mutates an option and shows an overlay.
;;; Extracting it as a named function keeps the handler table declarative and
;;; places the option-mutation logic where it belongs (a named function),
;;; separating it from the dispatch-layer rule.

(defun %toggle-synchronize-panes ()
  "Toggle the 'synchronize-panes' option and show a status overlay."
  (let ((current (cl-tmux/options:get-option "synchronize-panes")))
    (cl-tmux/options:set-option "synchronize-panes" (not current))
    (show-overlay (if (not current)
                      "synchronize-panes: ON"
                      "synchronize-panes: OFF"))))

;;; -- Option prompt helper -----------------------------------------------------
;;;
;;; :set-window-option and :set-session-option share the exact same body.
;;; %set-option-from-prompt factors out the common prompt+parse logic.

(defun %set-option-from-prompt (prompt-label)
  "Open a prompt labelled PROMPT-LABEL; on submit parse 'name value' and call set-option."
  (prompt-nonempty prompt-label
                   (lambda (input)
                     (let* ((parts (uiop:split-string input :separator " "))
                            (name  (first parts))
                            (value (second parts)))
                       (when (and name value)
                         (cl-tmux/options:set-option name value))))))

;;; -- Paste helper --------------------------------------------------------------
;;;
;;; :paste-buffer and :choose-buffer both need to write text to the active pane's
;;; PTY, honouring bracketed-paste mode.  %paste-to-pane factors that out.

(defconstant +bracketed-paste-begin+
  (if (boundp '+bracketed-paste-begin+)
      (symbol-value '+bracketed-paste-begin+)
      (format nil "~C[200~~" #\Escape))
  "Bracketed-paste begin escape sequence: ESC [ 2 0 0 ~")

(defconstant +bracketed-paste-end+
  (if (boundp '+bracketed-paste-end+)
      (symbol-value '+bracketed-paste-end+)
      (format nil "~C[201~~" #\Escape))
  "Bracketed-paste end escape sequence: ESC [ 2 0 1 ~")

(defun %paste-to-pane (pane text &optional (bracket-p t))
  "Write TEXT to PANE's PTY, wrapping in bracketed-paste sequences when the
   application enabled them (DECSET 2004) AND BRACKET-P is true.  BRACKET-P
   mirrors tmux's paste-buffer -p: the scriptable paste-buffer only brackets
   with -p, while the default interactive bindings (prefix ], mouse paste,
   buffer mode) all pass -p — hence BRACKET-P defaults to true for the
   interactive keyword-handler callers."
  (when (and text (cl-tmux/model:pane-live-p pane))
    (let* ((screen    (pane-screen pane))
           (bracketed (and bracket-p (screen-bracketed-paste screen))))
      (when bracketed
        (pty-write (pane-fd pane)
                   (babel:string-to-octets +bracketed-paste-begin+ :encoding :utf-8)))
      (pty-write (pane-fd pane) (babel:string-to-octets text :encoding :utf-8))
      (when bracketed
        (pty-write (pane-fd pane)
                   (babel:string-to-octets +bracketed-paste-end+ :encoding :utf-8))))))

;;; -- Shared command dispatch registry ----------------------------------------
;;;
;;; The prompt dispatcher and the tokenised arg-command runner both need the
;;; same command families. Keep the metadata in one place, then derive:
;;; - the named-command lookup table (string -> keyword)
;;; - the arg-command lookup table (string-list -> handler)
;;;
;;; The registry data itself lives in src/dispatch-command-specs.lisp so this
;;; file stays focused on helper code and table construction.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (let* ((root (or (ignore-errors (asdf:system-source-directory :cl-tmux))
                   *load-pathname*
                   *compile-file-pathname*
                   *default-pathname-defaults*))
         (src (merge-pathnames #P"src/" root)))
    (load (merge-pathnames #P"application/dispatch/core/dispatch-command-specs.lisp" src))))

(defun %make-dispatch-named-table (specs)
  "Build a hash-table mapping prompt-visible command names to dispatch keywords.
   SPECS is a list of plists; each plist may contain:
     :named-keyword KEYWORD — the dispatch keyword to register under these names
     :named-names   LIST    — strings to map to KEYWORD
   Specs that lack :named-keyword are silently ignored."
  (let ((table (make-hash-table :test #'equalp)))
    (dolist (spec specs table)
      (let ((keyword (getf spec :named-keyword)))
        (when keyword
          (dolist (name (getf spec :named-names))
            (setf (gethash name table) keyword)))))))

(defparameter *named-command-dispatch*
  (%make-dispatch-named-table *dispatch-command-specs-core*))

(defun %dispatch-named-command (session cmd-name)
  "Map CMD-NAME (a string) to a dispatch keyword and execute it on SESSION.
   Shows an error overlay for unknown command names."
  (let ((kw (gethash cmd-name *named-command-dispatch*)))
    (if kw
        (dispatch-command session kw nil)
        ;; Unknown name: show the error overlay AND return the :unknown-command
        ;; sentinel so callers (e.g. control mode's %error framing) can detect
        ;; the failure — the overlay value alone is not a reliable signal.
        (progn (%overlayf "unknown command: ~A" cmd-name)
               :unknown-command))))
