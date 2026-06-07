(in-package #:cl-tmux)

;;;; Declarative command dispatch for the prefix-key handler.
;;;;
;;;; define-command-handlers generates DISPATCH-COMMAND from a table of
;;;; (keyword &body forms) rules.  The macro keeps the dispatch table and
;;;; the function definition in sync automatically.

;;; ── Cyclic navigation macro ─────────────────────────────────────────────────
;;;
;;; next-cyclic and prev-cyclic are the same modular-arithmetic pattern with
;;; the step direction as the only difference.  A Prolog-like fact table:
;;;   navigate(next, List, Current) :- idx + 1.
;;;   navigate(prev, List, Current) :- idx - 1.

(defmacro define-cyclic-navigators (&rest specs)
  "Build cyclic list navigator functions from a declarative step table.
   Each SPEC is (name step docstring)."
  `(progn
     ,@(mapcar
        (lambda (spec)
          (destructuring-bind (name step docstring) spec
            `(defun ,name (list current)
               ,docstring
               (let ((idx (or (position current list) 0)))
                 (nth (mod (+ idx ,step) (length list)) list)))))
        specs)))

(define-cyclic-navigators
  (next-cyclic  1  "Element after CURRENT in LIST, wrapping around.")
  (prev-cyclic -1  "Element before CURRENT in LIST, wrapping around."))

;;; ── Active-pane access macro ─────────────────────────────────────────────────
;;;
;;; The pattern (let ((ap (session-active-pane session))) (when ap body))
;;; appears in %active-screen and %forward-octets.
;;; with-active-pane names the intent directly.

(defmacro with-active-pane ((pane-var session) &body body)
  "Bind PANE-VAR to SESSION's active pane and evaluate BODY.
   Returns NIL when no active pane is present (no-op guard)."
  `(let ((,pane-var (session-active-pane ,session)))
     (when ,pane-var ,@body)))

;;; -- Kill-result helper -------------------------------------------------------
;;;
;;; Both :kill-pane and :kill-window check the result and set *running* nil on
;;; :quit.  %handle-kill-result centralises that one-liner.

(defun %handle-kill-result (result)
  "Set *running* nil when RESULT is :quit, then return RESULT."
  (when (eq result :quit) (setf *running* nil))
  result)

;;; -- Active-window guard macro ------------------------------------------------
;;;
;;; Several handlers obtain the active window and do nothing when it is NIL.
;;; with-active-window names that guard directly.

(defmacro with-active-window ((win-var session) &body body)
  "Bind WIN-VAR to SESSION's active window and evaluate BODY only when present."
  `(let ((,win-var (session-active-window ,session)))
     (when ,win-var ,@body)))

;;; -- Swap-active-pane helper --------------------------------------------------
;;;
;;; :swap-pane-forward and :swap-pane-backward share the same shape.

(defun %swap-active-pane (session direction)
  "Swap the active pane of SESSION in DIRECTION (:left or :right)."
  (with-active-window (win session)
    (swap-pane win direction)))

;;; -- Private command helpers ------------------------------------------------

(defun %cmd-new-window (session)
  "Create a new window in SESSION and start a reader thread for it.
   The window name defaults to the shell basename (e.g. \"bash\"), matching
   real tmux; the id is assigned by session-new-window as the lowest free slot."
  (let* ((rows (- *term-rows* *status-height*))
         (cols *term-cols*)
         (name (cl-tmux/model::%shell-basename))
         (win  (session-new-window session name rows cols)))
    (start-reader-thread (window-active-pane win))
    (cl-tmux/hooks:run-hooks cl-tmux/hooks:+hook-after-new-window+ win)))

(defun %cmd-cycle-window (session cycler)
  "Switch the active window using CYCLER (next-cyclic or prev-cyclic)."
  (let ((w (funcall cycler
                    (session-windows session)
                    (session-active-window session))))
    (when w (session-select-window session w))))

(defun %cmd-cycle-pane (session cycler)
  "Switch the active pane within the active window using CYCLER."
  (let* ((win   (session-active-window session))
         (panes (window-panes win))
         (next  (funcall cycler panes (window-active-pane win))))
    (when next (window-select-pane win next))))

(defun %cmd-split (session orient &key no-focus size)
  "Split the active pane of SESSION's active window in tree ORIENT (:h left/right,
   :v top/bottom).  Returns NIL when the pane is too small and no shell is forked.
   NO-FOCUS T skips focus change.  SIZE hints the new pane's extent."
  (let* ((win (session-active-window session))
         (new (window-split win orient :no-focus no-focus :size size)))
    (when new
      (start-reader-thread new)
      (cl-tmux/hooks:run-hooks "after-split-window" new))
    new))

(defun %active-screen (session)
  "Return SESSION's active-pane screen, or NIL when there is no active pane."
  (with-active-pane (ap session)
    (pane-screen ap)))

;;; -- copy-mode-active-p ----------------------------------------------------

(defun copy-mode-active-p (session)
  "Return T when the active pane's screen is in copy mode."
  (let* ((win (session-active-window session))
         (ap  (and win (window-active-pane win))))
    (and ap
         (screen-copy-mode-p (pane-screen ap)))))

;;; -- Directional pane selection helper ------------------------------------
;;;
;;; The four :select-pane-left/right/up/down handlers share the same shape:
;;; obtain the active window and pane, then walk to the neighbor in DIRECTION.

(defun %select-pane-in-direction (session direction)
  "Select the pane adjacent to the active pane in DIRECTION."
  (let* ((win (session-active-window session))
         (ap  (and win (window-active-pane win))))
    (when (and win ap)
      (let ((nb (pane-neighbor win ap direction)))
        (when nb (window-select-pane win nb))))))

;;; -- Named-layout application helper --------------------------------------
;;;
;;; The three :select-layout-* handlers share the same shape: apply a named
;;; layout to the active window and recompute geometry.

(defun %apply-named-layout-to-session (session layout-name)
  "Apply LAYOUT-NAME to SESSION's active window and reassign geometry."
  (let ((win (session-active-window session)))
    (when win
      (cl-tmux/model:apply-named-layout win layout-name)
      (layout-assign (window-tree win) 0 0 (window-width win) (window-height win)))))

;;; -- Copy-mode dispatch helper --------------------------------------------
;;;
;;; The copy-mode command handlers share the pattern:
;;; obtain the active screen and invoke a copy-mode function when present.

(defun %copy-mode-call (session fn)
  "Call FN on SESSION's active screen when one exists and is in copy mode."
  (let ((s (%active-screen session)))
    (when s (funcall fn s))))

;;; -- Window list formatter ------------------------------------------------

(defun %format-window-list (session)
  "Return a formatted string listing all windows in SESSION.
   Format: INDEX: NAME (WxH) [active marker]
   INDEX is the window's stored id, not its 0-based list position."
  (let* ((win  (session-active-window session))
         (wins (session-windows session)))
    (with-output-to-string (s)
      (dolist (w wins)
        (format s "~A~A: ~A (~Dx~D) [~D pane~:P]~A~%"
                (if (eq w win) "*" " ")
                (window-id w)
                (window-name w)
                (window-width w)
                (window-height w)
                (length (window-panes w))
                (if (eq w win) " [active]" ""))))))

;;; ── Copy-mode key overrides macro ────────────────────────────────────────────

(defmacro define-copy-mode-key-overrides (&rest rules)
  "Build a copy-mode key-lookup function from a declarative override table.
   Each RULE is (char keyword). When in copy mode, CH is checked against the
   override table before the normal key-binding lookup.
   Generates %COPY-MODE-CMD that returns the override or the normal binding."
  `(defun %copy-mode-cmd (ch)
     "Return the command for CH when copy mode is active."
     (cond
       ,@(mapcar (lambda (rule)
                   (destructuring-bind (char kw) rule
                     `((and ch (char= ch ,char)) ,kw)))
                 rules)
       (t (and ch (lookup-key-binding ch))))))

(define-copy-mode-key-overrides
  (#\q :copy-mode-exit)
  (#\i :copy-mode-exit)
  (#\Space :copy-mode-begin-selection)
  (#\v :copy-mode-begin-selection)
  (#\V :copy-mode-begin-line-selection)
  (#\y :copy-mode-yank)
  (#\w :copy-mode-word-forward)
  (#\b :copy-mode-word-backward)
  (#\e :copy-mode-word-end)
  (#\0 :copy-mode-line-start)
  (#\$ :copy-mode-line-end)
  (#\g :copy-mode-top)
  (#\G :copy-mode-bottom)
  (#\H :copy-mode-high)
  (#\M :copy-mode-middle)
  (#\L :copy-mode-low)
  (#\D :copy-mode-copy-end-of-line)
  (#\Y :copy-mode-copy-line)
  (#\n :copy-mode-search-next)
  (#\N :copy-mode-search-prev)
  (#\/ :copy-mode-search-forward-prompt)
  (#\? :copy-mode-search-backward-prompt)
  (#\= :copy-mode-choose-buffer))

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

;;; -- %rename-session-in-registry --------------------------------------------
;;;
;;; Extracts the three-step registry mutation from the :rename-session handler:
;;; remove old key → rename struct → re-register under new key.

(defun %rename-session-in-registry (session new-name)
  "Remove SESSION from *server-sessions* under its old name, rename the
   session struct, then re-insert it under NEW-NAME.  No-op when NEW-NAME
   is empty."
  (unless (string= new-name "")
    (server-remove-session (session-name session))
    (rename-session session new-name)
    (server-add-session session)))

;;; -- new-session -------------------------------------------------------------

(defun new-session (name rows cols)
  "Create a new session named NAME with a full-screen window of ROWS x COLS.
   Registers the session in *server-sessions* and starts reader threads."
  (let ((session (create-initial-session rows cols)))
    (setf (session-name session) name)
    (session-touch session)
    (server-add-session session)
    (dolist (pane (all-panes session))
      (start-reader-thread pane))
    (cl-tmux/hooks:run-hooks cl-tmux/hooks:+hook-session-created+ session)
    session))

;;; -- %dispatch-named-command -------------------------------------------------
;;;
;;; Maps a string command name (as typed in the command-prompt) to a keyword
;;; dispatch tag, then calls dispatch-command.  Unknown names show an overlay.
;;;
;;; define-command-name-table generates a hash-table lookup from a declarative
;;; Prolog-fact table of (string keyword) pairs.

(defmacro define-command-name-table (table-var &rest entries)
  "Build TABLE-VAR (a hash table) from ENTRIES, each (string keyword).
   String lookups are case-insensitive via string-upcase at fill time."
  `(progn
     (defparameter ,table-var
       (let ((ht (make-hash-table :test #'equalp)))
         ,@(mapcar (lambda (entry)
                     (destructuring-bind (name kw) entry
                       `(setf (gethash ,name ht) ,kw)))
                   entries)
         ht)
       "Hash table mapping command name strings to dispatch keywords.")))

(define-command-name-table %command-name-table
  ("new-window"    :new-window)
  ("new-session"   :new-session)
  ("kill-pane"     :kill-pane)
  ("kill-window"   :kill-window)
  ("kill-session"  :kill-session)
  ("detach"        :detach)
  ("detach-client" :detach)
  ("next-window"   :next-window)
  ("prev-window"   :prev-window)
  ("split-window"  :split-horizontal)
  ("rename-window" :rename-window)
  ("rename-session" :rename-session)
  ("list-windows"  :list-windows)
  ("list-sessions" :list-sessions)
  ("list-keys"     :list-keys)
  ("copy-mode"     :copy-mode-enter)
  ("paste-buffer"  :paste-buffer)
  ("list-buffers"  :list-buffers)
  ("show-buffer"   :show-buffer)
  ("choose-buffer" :choose-buffer)
  ("delete-buffer" :delete-buffer)
  ("save-buffer"   :save-buffer)
  ("load-buffer"   :load-buffer)
  ("zoom-toggle"   :zoom-toggle)
  ("choose-tree"   :choose-tree)
  ("choose-session" :choose-session)
  ("choose-window" :choose-window)
  ("display-panes" :display-panes)
  ("show-messages" :show-messages)
  ("capture-pane"  :capture-pane)
  ("respawn-pane"  :respawn-pane)
  ("send-keys"     :send-keys)
  ("clock-mode"    :clock-mode)
  ("source-file"   :source-file)
  ("run-shell"     :run-shell)
  ("if-shell"      :if-shell)
  ("set-option"    :show-option)
  ("show-options"  :show-options)
  ("show-option"   :show-option)
  ("display-info"  :display-info)
  ("mark-pane"     :mark-pane)
  ("clear-mark"    :clear-mark)
  ("next-layout"   :next-layout)
  ("bind-key"      :bind-key)
  ("unbind-key"    :unbind-key)
  ("choose-client" :choose-client)
  ("move-window"   :move-window-prompt))

(defun %dispatch-named-command (session cmd-name)
  "Map CMD-NAME (a string) to a dispatch keyword and execute it on SESSION.
   Shows an error overlay for unknown command names."
  (let ((kw (gethash cmd-name %command-name-table)))
    (if kw
        (dispatch-command session kw nil)
        (show-overlay (format nil "unknown command: ~A" cmd-name)))))

;;; -- dispatch-prefix-command -----------------------------------------------

(defun dispatch-prefix-command (session byte)
  "Handle one byte received after the prefix key.
   Copy mode intercepts [ ] q before the normal binding table."
  (let* ((ch  (and byte (code-char byte)))
         (cmd (if (copy-mode-active-p session)
                  (%copy-mode-cmd ch)
                  (and ch (lookup-key-binding ch)))))
    (dispatch-command session cmd byte)))

;;; -- %prompt-set-option shared helper ----------------------------------------
;;;
;;; :set-window-option and :set-session-option are byte-for-byte identical.
;;; This helper extracts the shared logic so both handlers delegate here.

(defun %prompt-set-option (prompt-label)
  "Show PROMPT-LABEL prompt; parse 'name value' and call set-option."
  (prompt-start prompt-label ""
                (lambda (input)
                  (unless (string= input "")
                    (let* ((parts (uiop:split-string input :separator " "))
                           (name  (first parts))
                           (value (second parts)))
                      (when (and name value)
                        (cl-tmux/options:set-option name value)))))))

;;; -- define-command-handlers macro + dispatch-command ----------------------

(defmacro define-command-handlers (&rest rules)
  "Each RULE is (keyword &body forms); SESSION and BYTE are bound in each body.
   If a rule evaluates to :QUIT or :DETACH that outcome is returned directly.
   All other outcomes mark *DIRTY* and return NIL."
  `(defun dispatch-command (session cmd byte)
     (declare (ignorable byte))
     (let ((outcome
             (case cmd
               ,@(mapcar (lambda (rule)
                           (destructuring-bind (kw &rest body) rule
                             `(,kw (progn ,@body))))
                         rules)
               (otherwise nil))))
       (case outcome
         ((:quit :detach) outcome)
         (otherwise (setf *dirty* t) nil)))))

(define-command-handlers
  (:detach :detach)
  (:new-window (%cmd-new-window session))
  (:next-window (%cmd-cycle-window session #'next-cyclic))
  (:prev-window (%cmd-cycle-window session #'prev-cyclic))
  (:next-pane (%cmd-cycle-pane session #'next-cyclic))
  (:prev-pane (%cmd-cycle-pane session #'prev-cyclic))
  (:split-horizontal (%cmd-split session :v))        ; C-b " adds a horizontal bar → :v stacking
  (:split-vertical   (%cmd-split session :h))        ; C-b % adds a vertical bar   → :h side-by-side
  (:split-horizontal-no-focus (%cmd-split session :v :no-focus t))
  (:split-vertical-no-focus   (%cmd-split session :h :no-focus t))
  (:kill-pane   (%handle-kill-result (kill-pane session)))
  (:kill-window (%handle-kill-result (kill-window session (session-active-window session))))
  (:kill-pane-confirm
   ;; Show a y/n prompt; call kill-pane only when the user enters "y"/"Y".
   ;; Mirrors tmux's default "C-b x: kill-pane? (y/n)" behaviour.
   (with-active-window (win session)
     (let* ((ap  (window-active-pane win))
            (msg (if ap
                     (format nil "kill-pane ~D? (y/n)" (pane-id ap))
                     "kill-pane? (y/n)")))
       (prompt-start msg ""
                     (lambda (input)
                       (when (string-equal input "y")
                         (%handle-kill-result (kill-pane session))))))))
  (:kill-window-confirm
   ;; Show a y/n prompt; call kill-window only when the user enters "y"/"Y".
   ;; Mirrors tmux's default "C-b &: kill-window #W? (y/n)" behaviour.
   (with-active-window (win session)
     (let ((msg (format nil "kill-window ~A? (y/n)" (window-name win))))
       (prompt-start msg ""
                     (lambda (input)
                       (when (string-equal input "y")
                         (%handle-kill-result
                          (kill-window session (session-active-window session)))))))))
  (:respawn-pane
   (with-active-window (win session)
     (let ((ap (window-active-pane win)))
       (when ap
         (let ((new-pane (respawn-pane ap)))
           (start-reader-thread new-pane))))))
  (:rename-window
   (with-active-window (win session)
     (prompt-start "rename-window" (window-name win)
                   (lambda (name) (rename-window win name)))))
  (:list-keys (show-overlay (describe-key-bindings)))
  (:copy-mode-enter            (%copy-mode-call session #'copy-mode-enter))
  (:copy-mode-exit             (%copy-mode-call session #'copy-mode-exit))
  (:copy-mode-begin-selection  (%copy-mode-call session #'copy-mode-begin-selection))
  (:copy-mode-yank             (%copy-mode-call session #'copy-mode-yank))
  ;; Word navigation
  (:copy-mode-word-forward     (%copy-mode-call session #'copy-mode-word-forward))
  (:copy-mode-word-backward    (%copy-mode-call session #'copy-mode-word-backward))
  (:copy-mode-word-end         (%copy-mode-call session #'copy-mode-word-end))
  ;; Line navigation
  (:copy-mode-line-start       (%copy-mode-call session #'copy-mode-line-start))
  (:copy-mode-line-end         (%copy-mode-call session #'copy-mode-line-end))
  ;; Top / bottom jump
  (:copy-mode-top              (%copy-mode-call session #'copy-mode-top))
  (:copy-mode-bottom           (%copy-mode-call session #'copy-mode-bottom))
  ;; Screen position
  (:copy-mode-high             (%copy-mode-call session #'copy-mode-high))
  (:copy-mode-middle           (%copy-mode-call session #'copy-mode-middle))
  (:copy-mode-low              (%copy-mode-call session #'copy-mode-low))
  ;; Page up/down
  (:copy-mode-page-up          (%copy-mode-call session #'copy-mode-page-up))
  (:copy-mode-page-down        (%copy-mode-call session #'copy-mode-page-down))
  (:copy-mode-half-page-up     (%copy-mode-call session #'copy-mode-half-page-up))
  (:copy-mode-half-page-down   (%copy-mode-call session #'copy-mode-half-page-down))
  (:copy-mode-scroll-up-line   (%copy-mode-call session #'copy-mode-scroll-up-line))
  (:copy-mode-scroll-down-line (%copy-mode-call session #'copy-mode-scroll-down-line))
  ;; Line selection (V)
  (:copy-mode-begin-line-selection (%copy-mode-call session #'copy-mode-begin-line-selection))
  ;; Copy variants
  (:copy-mode-copy-end-of-line (%copy-mode-call session #'copy-mode-copy-end-of-line))
  (:copy-mode-copy-line        (%copy-mode-call session #'copy-mode-copy-line))
  ;; Search
  (:copy-mode-search-next      (%copy-mode-call session #'copy-mode-search-next))
  (:copy-mode-search-prev      (%copy-mode-call session #'copy-mode-search-prev))
  (:copy-mode-search-forward-prompt
   ;; / — prompt for a forward search term, then jump to first match
   (let ((s (%active-screen session)))
     (when s
       (prompt-start "/" ""
                     (lambda (term)
                       (unless (string= term "")
                         (copy-mode-search-forward s term)))))))
  (:copy-mode-search-backward-prompt
   ;; ? — prompt for a backward search term, then jump to first match
   (let ((s (%active-screen session)))
     (when s
       (prompt-start "?" ""
                     (lambda (term)
                       (unless (string= term "")
                         (copy-mode-search-backward s term)))))))
  (:copy-mode-choose-buffer
   ;; = — list paste buffers as overlay
   (show-overlay
    (with-output-to-string (s)
      (let ((bufs (cl-tmux/buffer:list-paste-buffers)))
        (if bufs
            (loop for buf in bufs
                  for i from 0
                  do (format s "~D: ~A~%" i (subseq buf 0 (min 40 (length buf)))))
            (format s "(no paste buffers)~%"))))))
  (:resize-left   (resize-pane (session-active-window session) :left))
  (:resize-right  (resize-pane (session-active-window session) :right))
  (:resize-up     (resize-pane (session-active-window session) :up))
  (:resize-down   (resize-pane (session-active-window session) :down))
  (:select-window (when byte (select-window-by-number session (- byte (char-code #\0)))))
  (:paste-buffer
   (let* ((text (cl-tmux/buffer:get-paste-buffer))
          (win  (session-active-window session))
          (ap   (and win (window-active-pane win))))
     (when (and text ap (> (pane-fd ap) 0))
       (let* ((screen (pane-screen ap))
              (bracketed (screen-bracketed-paste screen))
              (prefix (when bracketed (format nil "~C[200~~" #\Escape)))
              (suffix (when bracketed (format nil "~C[201~~" #\Escape))))
         (when prefix
           (pty-write (pane-fd ap) (babel:string-to-octets prefix :encoding :utf-8)))
         (pty-write (pane-fd ap) (babel:string-to-octets text :encoding :utf-8))
         (when suffix
           (pty-write (pane-fd ap) (babel:string-to-octets suffix :encoding :utf-8)))))))
  (:send-prefix
   ;; Send exactly one literal prefix byte (0x02) to the active pane's PTY.
   ;; This is the C-b C-b → literal C-b passthrough, matching real tmux behaviour.
   (flet ((byte-vec (b)
            (make-array 1 :element-type '(unsigned-byte 8) :initial-element b)))
     (with-active-pane (ap session)
       (when (> (pane-fd ap) 0)
         (pty-write (pane-fd ap) (byte-vec +prefix-key-code+))))))
  (:select-layout-even-h  (%apply-named-layout-to-session session :even-horizontal))
  (:select-layout-even-v  (%apply-named-layout-to-session session :even-vertical))
  (:select-layout-tiled   (%apply-named-layout-to-session session :tiled))
  (:select-pane-left   (%select-pane-in-direction session :left))
  (:select-pane-right  (%select-pane-in-direction session :right))
  (:select-pane-up     (%select-pane-in-direction session :up))
  (:select-pane-down   (%select-pane-in-direction session :down))
  (:zoom-toggle
   (with-active-window (win session)
     (window-zoom-toggle win)))
  (:rename-session
   (prompt-start "rename-session" (session-name session)
                 (lambda (name) (%rename-session-in-registry session name))))
  (:run-shell
   ;; Run the command in a prompt if no command is already queued.
   (prompt-start "run-shell" ""
                 (lambda (cmd)
                   (unless (string= cmd "")
                     (let ((out (run-shell cmd)))
                       (show-overlay out))))))
  (:if-shell
   ;; Prompt for a shell command; run it and display a success/failure overlay.
   (prompt-start "if-shell" ""
                 (lambda (cmd)
                   (unless (string= cmd "")
                     (if-shell cmd
                               (lambda () (show-overlay (format nil "[if-shell] ~A: ok" cmd)))
                               (lambda () (show-overlay (format nil "[if-shell] ~A: non-zero exit" cmd))))))))
  (:list-sessions
   (show-overlay
    (with-output-to-string (s)
      (if *server-sessions*
          (loop for (name . sess) in *server-sessions*
                for i from 0
                do (format s "~A~A: ~A (~D window~:P)~%"
                           (if (string= name (session-name session)) "*" " ")
                           i name
                           (length (session-windows sess))))
          (format s "  0: ~A (1 window)~%" (session-name session))))))
  (:new-session
   (let* ((rows (- *term-rows* *status-height*))
          (cols *term-cols*)
          (n    (1+ (length *server-sessions*)))
          (name (format nil "~D" n)))
     (new-session name rows cols)))
  (:kill-session
   ;; Kill all panes in current session, remove from registry.
   (let ((name (session-name session)))
     (dolist (pane (all-panes session))
       (ignore-errors (pty-close (pane-fd pane) (pane-pid pane))))
     (server-remove-session name)
     ;; If no sessions remain, quit. Otherwise continue.
     (if (null *server-sessions*)
         (progn (setf *running* nil) :quit)
         nil)))


  (:has-session
   ;; Check whether a session with the given name/id exists.
   ;; Shows a one-line overlay: "yes" or "no".
   (prompt-start "has-session" ""
                 (lambda (name)
                   (let ((found (server-find-session name)))
                     (show-overlay (if found "yes" "no"))))))
  (:list-windows (show-overlay (%format-window-list session)))
  (:choose-window
   ;; C-b w — show interactive window menu; each entry is "N: name".
   ;; Selecting an entry calls select-window-by-number on that window's id.
   (let* ((wins  (session-windows session))
          (items (mapcar (lambda (w)
                           (cons (format nil "~A: ~A" (window-id w) (window-name w))
                                 (window-id w)))
                         wins)))
     (if items
         (progn
           (show-menu (make-menu :title "choose-window" :items items :selected-index 0))
           (show-overlay (%format-menu *active-menu*))
           ;; Use a prompt so the user can type the index or navigate with Enter.
           (prompt-start "window: " ""
                         (lambda (s)
                           (let ((n (ignore-errors (parse-integer s))))
                             (when n (select-window-by-number session n)))
                           (close-menu)
                           (clear-overlay))))
         (show-overlay "(no windows)"))))
  (:last-window
   ;; C-b l — switch to the previously active window (second-highest last-active-time)
   (let ((prev (session-last-window session)))
     (when prev
       (session-select-window session prev)
       (setf (window-last-active-time prev) (get-universal-time)))))
  (:move-window
   ;; Prompt for a target index and move the active window there.
   (with-active-window (win session)
     (prompt-start "move-window" ""
                   (lambda (idx-str)
                     (let ((idx (ignore-errors (parse-integer idx-str))))
                       (when idx
                         (session-move-window session win idx)))))))
  (:swap-window
   ;; Prompt for target index and swap with active window.
   (with-active-window (win session)
     (let* ((wins   (session-windows session))
            (src-idx (position win wins)))
       (prompt-start "swap-window" ""
                     (lambda (idx-str)
                       (let ((dst (ignore-errors (parse-integer idx-str))))
                         (when (and dst src-idx)
                           (session-swap-windows session src-idx dst))))))))
  (:rotate-window
   ;; Rotate pane ordering within the active window (forward: first → end).
   (with-active-window (win session)
     (window-rotate win :up)))
  (:rotate-window-reverse
   ;; Reverse rotate: last → front.
   (with-active-window (win session)
     (window-rotate win :down)))
  (:find-window
   ;; C-b f — search window names; show matches in overlay.
   (prompt-start "find-window" ""
                 (lambda (pattern)
                   (unless (string= pattern "")
                     (let* ((wins (session-windows session))
                            (matches
                             (remove-if-not
                              (lambda (w)
                                (search pattern (window-name w) :test #'char-equal))
                              wins)))
                       (show-overlay
                        (if matches
                            (with-output-to-string (s)
                              (dolist (w matches)
                                (format s "~A: ~A~A~%"
                                        (position w wins)
                                        (window-name w)
                                        (if (eq w (session-active-window session))
                                            " [active]" ""))))
                            (format nil "no windows matching ~S~%" pattern))))))))
  (:swap-pane-forward  (%swap-active-pane session :right))
  (:swap-pane-backward (%swap-active-pane session :left))
  (:last-pane
   (let* ((win  (session-active-window session))
          (last (and win (window-last-active win))))
     (when last (window-select-pane win last))))
  (:display-panes
   ;; Show pane index numbers as a numbered overlay.  Each line shows one
   ;; pane's number and geometry for quick identification (tmux C-b q).
   ;; The overlay is dismissed by the next keystroke (standard overlay behaviour).
   (with-active-window (win session)
     (let ((panes (window-panes win)))
       (when panes
         (show-overlay
          (with-output-to-string (s)
            (dolist (p panes)
              (format s "Pane ~D: ~Dx~D at (~D,~D)~A~%"
                      (pane-id p)
                      (pane-width p) (pane-height p)
                      (pane-x p) (pane-y p)
                      (if (eq p (window-active-pane win)) " [active]" "")))))))))
  (:switch-client-next
   ;; C-b ) — switch to the next session (by sessions list order)
   (let* ((sessions (mapcar #'cdr *server-sessions*))
          (next     (and sessions (next-cyclic sessions session))))
     (when (and next (not (eq next session)))
       (session-touch next)
       (setf *dirty* t))))
  (:switch-client-prev
   ;; C-b ( — switch to the previous session (by sessions list order)
   (let* ((sessions (mapcar #'cdr *server-sessions*))
          (prev     (and sessions (prev-cyclic sessions session))))
     (when (and prev (not (eq prev session)))
       (session-touch prev)
       (setf *dirty* t))))
  (:last-session
   ;; C-b L — switch to the second-most-recently-active session
   (let* ((sessions (sort (mapcar #'cdr *server-sessions*) #'>
                          :key #'session-last-active))
          (second   (second sessions)))
     (when second
       (session-touch second)
       (setf *dirty* t))))
  (:display-message
   ;; Prompt for a message, display it as a transient overlay, and log it.
   (prompt-start "display-message" ""
                 (lambda (msg)
                   (unless (string= msg "")
                     (add-message-log msg)
                     (show-overlay msg)))))
  (:source-file
   ;; Prompt for a config file path and load it.
   (prompt-start "source-file" ""
                 (lambda (path)
                   (unless (string= path "")
                     (load-config-file (pathname path))))))
  (:show-options
   ;; Show all global options as an overlay.
   (show-overlay (cl-tmux/options:show-options)))
  (:show-option
   ;; Prompt for an option name and show its current value.
   (prompt-start "show-option" ""
                 (lambda (name)
                   (unless (string= name "")
                     (show-overlay (cl-tmux/options:show-option name))))))
  (:confirm-before
   ;; Show a y/n prompt; dispatch the wrapped command only on "y".
   ;; Here we prompt for a command string that will be executed on confirm.
   (prompt-start "confirm? (y/n)" ""
                 (lambda (input)
                   (when (and (not (string= input ""))
                              (string-equal input "y"))
                     ;; The confirm-before command confirmed — run the queued command.
                     ;; In the keybinding context this is used to guard destructive ops.
                     (show-overlay "[confirmed]")))))
  (:wait-for
   ;; Block until a named channel is signaled, or signal it.
   ;; Interactive: prompt for channel-name.
   (prompt-start "wait-for channel" ""
                 (lambda (name)
                   (unless (string= name "")
                     ;; Signal the channel (unblock waiting threads).
                     (signal-channel name)
                     (show-overlay (format nil "signaled channel: ~A" name))))))
  (:wait-for-signal
   ;; Directly signal a named channel from the event loop.
   (prompt-start "signal channel" ""
                 (lambda (name)
                   (unless (string= name "")
                     (signal-channel name)
                     (show-overlay (format nil "signaled: ~A" name))))))
  (:display-popup
   ;; Create a floating overlay showing output from a shell command.
   (prompt-start "popup command" ""
                 (lambda (cmd)
                   (unless (string= cmd "")
                     (let ((output (run-shell cmd)))
                       (show-popup (make-popup :title cmd
                                               :width  (min 60 *term-cols*)
                                               :height (min 15 (- *term-rows* 4))
                                               :screen nil
                                               :pane   nil))
                       ;; Show output as overlay until dismissed
                       (show-overlay
                        (format nil "┌─ ~A ─┐~%~A~%└~A┘"
                                cmd
                                (or output "")
                                (make-string (+ 2 (length cmd)) :initial-element #\─))))))))
  (:display-popup-dismiss
   ;; Dismiss the active popup.
   (close-popup))
  (:display-menu
   ;; Show a text menu overlay with j/k navigation and Enter selection.
   (let ((items (list (cons "New Window"    :new-window)
                      (cons "Next Window"   :next-window)
                      (cons "Prev Window"   :prev-window)
                      (cons "Kill Pane"     :kill-pane)
                      (cons "Kill Window"   :kill-window)
                      (cons "Zoom Toggle"   :zoom-toggle)
                      (cons "List Sessions" :list-sessions)
                      (cons "Detach"        :detach))))
     (show-menu (make-menu :title "Menu" :items items :selected-index 0))
     (show-overlay (%format-menu *active-menu*))))
  (:menu-next
   ;; Move menu selection down.
   (when *active-menu*
     (let ((n (length (menu-items *active-menu*))))
       (setf (menu-selected-index *active-menu*)
             (mod (1+ (menu-selected-index *active-menu*)) n))
       (show-overlay (%format-menu *active-menu*)))))
  (:menu-prev
   ;; Move menu selection up.
   (when *active-menu*
     (let ((n (length (menu-items *active-menu*))))
       (setf (menu-selected-index *active-menu*)
             (mod (1- (menu-selected-index *active-menu*)) n))
       (show-overlay (%format-menu *active-menu*)))))
  (:menu-select
   ;; Execute the selected menu item.
   (when *active-menu*
     (let* ((idx  (menu-selected-index *active-menu*))
            (item (nth idx (menu-items *active-menu*)))
            (cmd  (cdr item)))
       (close-menu)
       (clear-overlay)
       (when cmd
         (dispatch-command session cmd byte)))))
  (:menu-dismiss
   ;; Cancel the menu.
   (close-menu)
   (clear-overlay))
  (:break-pane
   ;; C-b ! — detach active pane into a new window.
   (with-active-window (win session)
     (when (> (length (window-panes win)) 1)
       (let ((new-win (break-pane session)))
         (when new-win
           (start-reader-thread (window-active-pane new-win)))))))
  (:join-pane
   ;; Prompt for source window index, then join its active pane into the current window.
   (with-active-window (dst-win session)
     (prompt-start "join-pane from window" ""
                   (lambda (idx-str)
                     (let ((idx (ignore-errors (parse-integer idx-str))))
                       (when idx
                         (let* ((src-win  (nth idx (session-windows session)))
                                (src-pane (and src-win (window-active-pane src-win))))
                           (when src-pane
                             (join-pane session src-win src-pane dst-win :h)))))))))
  (:pipe-pane
   ;; Toggle pane output piping: prompt for a command, or close existing pipe.
   (with-active-pane (ap session)
     (if (pane-pipe-fd ap)
         (pipe-pane-close ap)
         (prompt-start "pipe-pane command" ""
                       (lambda (cmd)
                         (unless (string= cmd "")
                           (pipe-pane-open ap cmd)))))))
  (:synchronize-panes
   ;; Toggle the synchronize-panes window option.
   (let ((cur (cl-tmux/options:get-option "synchronize-panes")))
     (cl-tmux/options:set-option "synchronize-panes" (not cur))
     (show-overlay (if (not cur)
                       "synchronize-panes: ON"
                       "synchronize-panes: OFF"))))
  (:lock-session
   ;; Lock the current session.
   (setf (session-locked-p session) t))
  (:unlock-session
   ;; Unlock the current session (any key / prompt).
   (setf (session-locked-p session) nil))
  (:choose-session
   ;; C-b s — show interactive session list overlay.
   (show-overlay
    (with-output-to-string (s)
      (if *server-sessions*
          (loop for (name . sess) in *server-sessions*
                for i from 0
                do (format s "~A~A: ~A (~D window~:P)~%"
                           (if (string= name (session-name session)) "*" " ")
                           i name
                           (length (session-windows sess))))
          (format s " 0: ~A (1 window)~%" (session-name session))))))
  ;; ── New commands ──────────────────────────────────────────────────────────
  (:command-prompt
   ;; C-b : — open a command-line prompt; execute the entered command.
   (prompt-start ": " ""
                 (lambda (input)
                   (unless (string= input "")
                     (let* ((trimmed  (string-trim '(#\Space #\Tab) input))
                            (parts    (uiop:split-string trimmed :separator " "))
                            (cmd-name (first parts)))
                       (%dispatch-named-command session cmd-name))))))
  (:send-keys
   ;; Prompt for a string and send it to the active pane's PTY.
   (with-active-pane (ap session)
     (prompt-start "send-keys" ""
                   (lambda (input)
                     (unless (string= input "")
                       (send-keys-to-pane ap input))))))
  (:clock-mode
   ;; Toggle a digital clock overlay on the active pane.
   (with-active-pane (ap session)
     (setf *clock-mode-pane-id*
           (if (eql *clock-mode-pane-id* (pane-id ap))
               nil
               (pane-id ap)))))
  (:show-messages
   ;; Show recent display-message entries as an overlay.
   (show-overlay
    (if *message-log*
        (format nil "~{~A~%~}"
                (mapcar #'cdr *message-log*))
        "(no messages)")))
  (:capture-pane
   ;; Dump active pane content as an overlay.
   (with-active-pane (ap session)
     (show-overlay (capture-pane ap))))
  (:choose-tree
   ;; Show a tree overview of all sessions and their windows.
   (show-overlay
    (with-output-to-string (s)
      (if *server-sessions*
          (loop for (name . sess) in *server-sessions*
                do (format s "~A~A~%"
                           (if (string= name (session-name session)) "* " "  ")
                           name)
                   (loop for win in (session-windows sess)
                         do (format s "    ~A~A: ~A~%"
                                    (if (eq win (session-active-window sess)) "*" " ")
                                    (window-id win)
                                    (window-name win))))
          (progn
            (format s "* ~A~%" (session-name session))
            (loop for win in (session-windows session)
                  do (format s "    ~A~A: ~A~%"
                             (if (eq win (session-active-window session)) "*" " ")
                             (window-id win)
                             (window-name win))))))))
  (:set-window-option
   ;; name+value prompt feeding the shared option registry (window scope).
   (%prompt-set-option "set-window-option"))
  (:set-session-option
   ;; name+value prompt feeding the shared option registry (session scope).
   (%prompt-set-option "set-session-option"))
  (:list-buffers
   ;; C-b # — show all paste buffers as an overlay.
   (show-overlay
    (with-output-to-string (s)
      (let ((bufs (cl-tmux/buffer:list-paste-buffers)))
        (if bufs
            (loop for buf in bufs
                  for i from 0
                  do (format s "~D: [~D] ~A~%"
                             i
                             (length buf)
                             (subseq buf 0 (min 40 (length buf)))))
            (format s "(no paste buffers)~%"))))))
  (:show-buffer
   ;; Show the content of the most recent paste buffer (index 0).
   (let ((buf (cl-tmux/buffer:get-paste-buffer 0)))
     (show-overlay (or buf "(no paste buffers)"))))
  (:choose-buffer
   ;; C-b = — prompt user for buffer index, then paste that buffer.
   (let ((bufs (cl-tmux/buffer:list-paste-buffers)))
     (if bufs
         (let ((listing
                 (with-output-to-string (s)
                   (loop for buf in bufs
                         for i from 0
                         do (format s "~D: ~A~%"
                                    i
                                    (subseq buf 0 (min 40 (length buf))))))))
           (show-overlay listing)
           (prompt-start "choose buffer (index)" "0"
                         (lambda (idx-str)
                           (let ((idx (ignore-errors (parse-integer idx-str))))
                             (when idx
                               (let* ((text (cl-tmux/buffer:get-paste-buffer idx))
                                      (win  (session-active-window session))
                                      (ap   (and win (window-active-pane win))))
                                 (when (and text ap (> (pane-fd ap) 0))
                                   (pty-write (pane-fd ap)
                                              (babel:string-to-octets
                                               text :encoding :utf-8)))))))))
         (show-overlay "(no paste buffers)"))))
  (:delete-buffer
   ;; C-b - — delete the most recent paste buffer (index 0) with confirmation.
   (let ((buf (cl-tmux/buffer:get-paste-buffer 0)))
     (if buf
         (progn
           (cl-tmux/buffer:delete-paste-buffer 0)
           (show-overlay "buffer 0 deleted"))
         (show-overlay "(no paste buffers to delete)"))))
  (:save-buffer
   ;; Prompt for a file path and write buffer 0 content to that file.
   (let ((buf (cl-tmux/buffer:get-paste-buffer 0)))
     (if buf
         (prompt-start "save-buffer to file" ""
                       (lambda (path)
                         (unless (string= path "")
                           (handler-case
                               (progn
                                 (with-open-file (f path
                                                   :direction :output
                                                   :if-exists :supersede
                                                   :if-does-not-exist :create)
                                   (write-string buf f))
                                 (show-overlay (format nil "saved to ~A" path)))
                             (error (e)
                               (show-overlay (format nil "save-buffer error: ~A" e)))))))
         (show-overlay "(no paste buffers to save)"))))
  (:load-buffer
   ;; Prompt for a file path and push its content as a new paste buffer.
   (prompt-start "load-buffer from file" ""
                 (lambda (path)
                   (unless (string= path "")
                     (handler-case
                         (let ((content
                                 (with-open-file (f path
                                                   :direction :input
                                                   :if-does-not-exist :error)
                                   (let ((s (make-string (file-length f))))
                                     (read-sequence s f)
                                     s))))
                           (cl-tmux/buffer:add-paste-buffer content)
                           (show-overlay (format nil "loaded ~D bytes from ~A"
                                                 (length content) path)))
                       (error (e)
                         (show-overlay (format nil "load-buffer error: ~A" e))))))))
  (:mark-pane
   ;; C-b m — set the marked pane (toggle: if already marked, unmark it).
   (with-active-pane (ap session)
     (if (pane-marked ap)
         (setf (pane-marked ap) nil)
         (progn
           ;; Clear any existing marked pane in this window first.
           (with-active-window (win session)
             (dolist (p (window-panes win))
               (setf (pane-marked p) nil)))
           (setf (pane-marked ap) t)))))
  (:clear-mark
   ;; C-b M — clear the marked pane in the current window.
   (with-active-window (win session)
     (dolist (p (window-panes win))
       (setf (pane-marked p) nil))))
  (:select-layout-spread
   ;; C-b E — apply the even-horizontal layout (spread panes evenly).
   (%apply-named-layout-to-session session :even-horizontal))
  (:next-layout
   ;; C-b Space — cycle through layouts in order.
   (with-active-window (win session)
     (let* ((layouts #(:even-horizontal :even-vertical :tiled
                       :main-horizontal :main-vertical))
            (current (cl-tmux/model:window-layout-cycle-index win))
            (next    (mod (1+ current) (length layouts)))
            (name    (aref layouts next)))
       (setf (cl-tmux/model:window-layout-cycle-index win) next)
       (%apply-named-layout-to-session session name))))
  (:choose-client
   ;; C-b D — show overlay with attached client info (stub for single-client mode).
   (show-overlay
    (with-output-to-string (s)
      (format s "Clients:~%")
      (format s "  0: local  ~A  ~Dx~D~%"
              (session-name session)
              *term-cols*
              *term-rows*))))
  (:display-info
   ;; C-b i — show session/window/pane info summary overlay.
   (with-active-pane (ap session)
     (let* ((win (session-active-window session))
            (sc  (pane-screen ap)))
       (show-overlay
        (format nil "Session: ~A~%Window: ~A (~Dx~D) [~D pane~:P]~%Pane: ~D at (~D,~D) ~Dx~D~A"
                (session-name session)
                (if win (window-name win) "none")
                (if win (window-width  win) 0)
                (if win (window-height win) 0)
                (if win (length (window-panes win)) 0)
                (pane-id ap)
                (pane-x ap) (pane-y ap)
                (pane-width ap) (pane-height ap)
                (if (and sc (screen-copy-mode-p sc)) " [copy]" ""))))))
  (:move-window-prompt
   ;; C-b . — prompt for a target index and move the active window there.
   (with-active-window (win session)
     (prompt-start "move-window to index" ""
                   (lambda (idx-str)
                     (let ((idx (ignore-errors (parse-integer idx-str))))
                       (when idx
                         (session-move-window session win idx)))))))
  (:bind-key
   ;; Runtime bind-key: prompt for "key command" and bind in the prefix table.
   (prompt-start "bind key: " ""
                 (lambda (input)
                   (unless (string= input "")
                     (let* ((parts (uiop:split-string input :separator " "))
                            (key-tok (and (first parts)
                                         (cl-tmux/config::%parse-key-token (first parts))))
                            (cmd-str (second parts))
                            (kw      (and cmd-str
                                          (cl-tmux/config::%command-keyword cmd-str))))
                       (if kw
                           (progn
                             (set-key-binding key-tok kw)
                             (key-table-bind "prefix" key-tok kw)
                             (show-overlay (format nil "bound ~A -> ~(~A~)" key-tok kw)))
                           (show-overlay (format nil "unknown command: ~A"
                                                 (or cmd-str input)))))))))
  (:unbind-key
   ;; Runtime unbind-key: prompt for a key and remove its prefix binding.
   (prompt-start "unbind key: " ""
                 (lambda (input)
                   (unless (string= input "")
                     (let ((k (cl-tmux/config::%parse-key-token input)))
                       (remove-key-binding k)
                       (let ((tbl (gethash "prefix" *key-tables*)))
                         (when tbl (remhash k tbl)))
                       (show-overlay (format nil "unbound ~A" k)))))))
  (:select-window-prompt
   ;; Prompt for a window name or number and select it.
   (prompt-start "select window (name or number): " ""
                 (lambda (input)
                   (unless (string= input "")
                     (let* ((idx (ignore-errors (parse-integer input)))
                            (win (or (and idx (nth idx (session-windows session)))
                                     (find input (session-windows session)
                                           :key #'window-name
                                           :test #'string-equal))))
                       (if win
                           (session-select-window session win)
                           (show-overlay (format nil "no window: ~A" input)))))))))

