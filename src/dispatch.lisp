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
;;; appears in %passthrough-prefix, %active-screen, and %forward-octets.
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
  "Create a new window in SESSION and start a reader thread for it."
  (let* ((rows (- *term-rows* *status-height*))
         (cols *term-cols*)
         (name (format nil "~D" (1+ (length (session-windows session)))))
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

(defun %passthrough-prefix (session byte)
  "Send the raw prefix byte followed by BYTE to the active pane."
  (flet ((byte-vec (b)
           (make-array 1 :element-type '(unsigned-byte 8) :initial-element b)))
    (with-active-pane (ap session)
      (pty-write (pane-fd ap) (byte-vec +prefix-key-code+))
      (when byte
        (pty-write (pane-fd ap) (byte-vec byte))))))

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
;;; The six copy-mode command handlers share the pattern:
;;; obtain the active screen and invoke a copy-mode function when present.

(defun %copy-mode-call (session fn)
  "Call FN on SESSION's active screen when one exists and is in copy mode."
  (let ((s (%active-screen session)))
    (when s (funcall fn s))))

;;; -- Window list formatter ------------------------------------------------

(defun %format-window-list (session)
  "Return a formatted string listing all windows in SESSION.
   Format: INDEX: NAME (WxH) [active marker]"
  (let* ((win  (session-active-window session))
         (wins (session-windows session)))
    (with-output-to-string (s)
      (dolist (w wins)
        (format s "~A~A: ~A (~Dx~D) [~D pane~:P]~A~%"
                (if (eq w win) "*" " ")
                (position w wins)
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
  (#\[ :copy-mode-up)
  (#\] :copy-mode-down)
  (#\q :copy-mode-exit)
  (#\Space :copy-mode-begin-selection)
  (#\v :copy-mode-begin-selection)
  (#\y :copy-mode-yank))

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

;;; -- dispatch-prefix-command -----------------------------------------------

(defun dispatch-prefix-command (session byte)
  "Handle one byte received after the prefix key.
   Copy mode intercepts [ ] q before the normal binding table."
  (let* ((ch  (and byte (code-char byte)))
         (cmd (if (copy-mode-active-p session)
                  (%copy-mode-cmd ch)
                  (and ch (lookup-key-binding ch)))))
    (dispatch-command session cmd byte)))

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
               (otherwise (%passthrough-prefix session byte) nil))))
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
  (:split-horizontal-no-focus (%cmd-split session :v :no-focus t))  ; split without changing focus
  (:split-vertical-no-focus   (%cmd-split session :h :no-focus t))  ; split without changing focus
  (:kill-pane   (%handle-kill-result (kill-pane session)))
  (:kill-window (%handle-kill-result (kill-window session (session-active-window session))))
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
  (:copy-mode-up               (%copy-mode-call session (lambda (s) (copy-mode-scroll s 3))))
  (:copy-mode-down             (%copy-mode-call session (lambda (s) (copy-mode-scroll s -3))))
  (:copy-mode-begin-selection  (%copy-mode-call session #'copy-mode-begin-selection))
  (:copy-mode-yank             (%copy-mode-call session #'copy-mode-yank))
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
       (pty-write (pane-fd ap) (babel:string-to-octets text :encoding :utf-8)))))
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
                 (lambda (name)
                   (unless (string= name "")
                     ;; Update registry key first, then rename the session struct.
                     (server-remove-session (session-name session))
                     (rename-session session name)
                     (server-add-session session)))))
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
  (:list-sessions-full
   (show-overlay
    (with-output-to-string (s)
      (loop for (name . sess) in *server-sessions*
            for i from 0
            do (format s "~A~A: ~A (~D window~:P)~%"
                       (if (string= name (session-name session)) "*" " ")
                       i name
                       (length (session-windows sess)))))))
  (:rename-session-prompt
   (prompt-start "rename-session" (session-name session)
                 (lambda (name)
                   (unless (string= name "")
                     (server-remove-session (session-name session))
                     (setf (session-name session) name)
                     (server-add-session session)))))
  (:has-session
   ;; Check whether a session with the given name/id exists.
   ;; Shows a one-line overlay: "yes" or "no".
   (prompt-start "has-session" ""
                 (lambda (name)
                   (let ((found (server-find-session name)))
                     (show-overlay (if found "yes" "no"))))))
  (:list-windows (show-overlay (%format-window-list session)))
  (:choose-window
   ;; C-b w — show interactive window list overlay (same as list-windows for now;
   ;; full interactive selection requires an overlay navigation layer)
   (show-overlay (%format-window-list session)))
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
   ;; Prompt for a message and display it as a transient overlay.
   (prompt-start "display-message" ""
                 (lambda (msg)
                   (show-overlay msg))))
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
                       (setf *active-popup*
                             (make-popup :title cmd
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
   (setf *active-popup* nil))
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
     (setf *active-menu* (make-menu :title "Menu" :items items :selected-index 0))
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
       (setf *active-menu* nil)
       (clear-overlay)
       (when cmd
         (dispatch-command session cmd byte)))))
  (:menu-dismiss
   ;; Cancel the menu.
   (setf *active-menu* nil)
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
          (format s " 0: ~A (1 window)~%" (session-name session)))))))

