(in-package #:cl-tmux)

;;; -- Named paste-buffer and overlay popup/menu %cmd-* handlers ---------------
;;;
;;; set-buffer, paste-buffer, delete-buffer, show-buffer (all -b name forms),
;;; and the display-popup / display-menu / confirm-before / list-keys / copy-mode
;;; overlay commands.

;;; ── Named paste-buffer commands (set/paste/delete/show -b name) ──────────────
;;;
;;; tmux's set-buffer/paste-buffer/delete-buffer/show-buffer all accept -b <name>
;;; to target a specific named buffer.  These arg-bearing handlers (registered in
;;; *arg-command-table*) layer over cl-tmux/buffer's named-buffer API; the no-arg
;;; keyword handlers (:set-buffer etc. in dispatch-handlers) remain for the C-b
;;; interactive bindings.

(defun %named-or-latest-paste-buffer (name)
  "Return NAME's paste buffer when NAME is non-NIL, otherwise the most recent
   paste buffer."
  (if name
      (cl-tmux/buffer:get-named-buffer name)
      (cl-tmux/buffer:get-paste-buffer 0)))

(defmacro define-flag-accessors (&rest specs)
  "Generate flag accessor functions from a fact table.
   Each SPEC is (fn-name doc :value flag-char [default]) or
                (fn-name doc :present flag-char).
   :value   — returns the flag value, or DEFAULT when the flag is absent.
   :present — returns the %flag-present-p result (truthy or NIL)."
  `(progn
     ,@(mapcar (lambda (spec)
                 (destructuring-bind (fn-name doc type char &optional default) spec
                   `(defun ,fn-name (flags)
                      ,doc
                      ,(ecase type
                         (:value  (if default
                                      `(or (%flag-value flags ,char) ,default)
                                      `(%flag-value flags ,char)))
                         (:present `(%flag-present-p flags ,char))))))
               specs)))

(define-flag-accessors
  (%buffer-name-from-flags
   "Return the named buffer selected by -b in FLAGS, or NIL when absent."
   :value #\b)
  (%buffer-append-p
   "Return T when the command FLAGS include -a."
   :present #\a)
  (%popup-title-from-flags
   "Return the popup title encoded by FLAGS, or the empty title when absent."
   :value #\T "")
  (%popup-width-from-flags
   "Return the popup width encoded by FLAGS, or NIL when absent."
   :value #\w)
  (%popup-height-from-flags
   "Return the popup height encoded by FLAGS, or NIL when absent."
   :value #\h)
  (%menu-title-from-flags
   "Return the menu title encoded by FLAGS, or the default menu title."
   :value #\T "Menu")
  (%confirm-prompt-from-flags
   "Return the custom confirm prompt encoded by FLAGS, or NIL when absent."
   :value #\p)
  (%list-keys-table-name-from-flags
   "Return the key table encoded by FLAGS, or NIL when absent."
   :value #\T)
  (%copy-mode-scroll-to-top-p
   "Return T when FLAGS request copy-mode to start at the top."
   :present #\u)
  (%copy-mode-exit-on-bottom-p
   "Return T when FLAGS request copy-mode to exit at the bottom."
   :present #\e))

(defun %buffer-positionals-text (positionals)
  "Join POSITIONALS with spaces, mirroring tmux's command-line token joining."
  (format nil "~{~A~^ ~}" positionals))

(defun %buffer-read-file (path)
  "Read PATH as a character stream and return its full contents."
  (with-open-file (in path :direction :input)
    (let* ((len (or (file-length in) 0))
           (text (make-string len))
           (count (read-sequence text in)))
      (subseq text 0 count))))

(defun %buffer-write-file (path text &key append-p)
  "Write TEXT to PATH, appending when APPEND-P is true, and return TEXT."
  (with-open-file (out path
                       :direction :output
                       :if-exists (if append-p :append :supersede)
                       :if-does-not-exist :create)
    (write-string text out))
  text)

(defun %set-buffer-send-to-clipboard (session text)
  "Honour set-buffer -w: enqueue an OSC 52 sequence on the active pane's screen
   so the host terminal copies TEXT to the system clipboard on the next frame.
   No-op when set-clipboard is off or there is no active pane."
  (let ((mode (or (ignore-errors (cl-tmux/options:get-option "set-clipboard")) "on"))
        (pane (and session (session-active-pane session))))
    (when (and pane text (not (string= mode "off")))
      (let ((screen (pane-screen pane)))
        (when screen
          (push (cl-tmux/terminal/parser:osc52-clipboard-sequence text)
                (screen-clipboard-queue screen)))))))

(defun %cmd-set-buffer-arg (session args)
  "set-buffer [-aw] [-b name] [-n new-name] [-t target-client] data...:
   set a paste buffer's contents.  -b name stores DATA under NAME; without -b
   an automatic name (bufferN) is assigned.  -n new-name renames the selected
   buffer (or the most recent one) to NEW-NAME and ignores DATA.  -w also sends
   the buffer to the host clipboard via OSC 52 (honouring set-clipboard); -t
   names the target client for that clipboard write (accepted; the standalone
   model has a single client so it routes to the active pane)."
  (with-command-input (flags positionals args "bnt"
                             :allowed-flags '(#\a #\b #\n #\w #\t)
                             :message "set-buffer: unsupported argument")
    (let* ((name     (%buffer-name-from-flags flags))
           (new-name (%flag-value flags #\n))
           (append-p (%buffer-append-p flags))
           (to-clip  (%flag-present-p flags #\w))
           (data     (%buffer-positionals-text positionals)))
      (cond
        (new-name
         (unless (cl-tmux/buffer:rename-paste-buffer name new-name)
           (show-overlay "no buffer")))
        (positionals
         (let ((stored data))
           (if append-p
               (let ((existing (or (%named-or-latest-paste-buffer name) "")))
                 (setf stored (concatenate 'string existing data))
                 (cl-tmux/buffer:add-paste-buffer stored name))
               (cl-tmux/buffer:add-paste-buffer data name))
           (when to-clip
             (%set-buffer-send-to-clipboard session stored))))))))

(defun %replace-newlines-with (text sep)
  "Return TEXT with every LF replaced by the string SEP (which may be empty or
   multi-character).  Used by paste-buffer's -s separator option."
  (with-output-to-string (s)
    (loop for ch across text
          do (if (char= ch #\Newline) (write-string sep s) (write-char ch s)))))

(defun %paste-buffer-text (raw no-replace &optional separator)
  "The text paste-buffer writes for buffer contents RAW.  tmux replaces LF with CR
   by default so each pasted line submits like Enter; SEPARATOR (-s) overrides the
   replacement string (LF → SEPARATOR); NO-REPLACE (-r) keeps the raw bytes and
   takes precedence over -s.  Returns NIL when RAW is NIL."
  (cond
    ((null raw)  nil)
    (no-replace  raw)
    (separator   (%replace-newlines-with raw separator))
    (t           (substitute #\Return #\Newline raw))))

(defun %cmd-paste-buffer-arg (session args)
  "paste-buffer [-d] [-p] [-r] [-b name] [-s sep] [-t target]: paste a buffer into
   the target pane.  -b name pastes the named buffer (else the most recent); -d
   deletes the buffer after pasting.  By default newlines (LF) are replaced with
   carriage returns (CR) so pasted lines act as Enter in a shell; -r disables that
   replacement.  -s sep replaces line endings (LF) with SEP instead of the default
   CR (e.g. `paste-buffer -s ' '` joins lines with spaces); -r still wins, pasting
   raw.  -p: wrap the paste in bracketed-paste sequences when the application
   has enabled them (tmux only brackets with -p on the scriptable command)."
  (with-command-input (flags positionals args "bst"
                                :allowed-flags '(#\d #\p #\r #\b #\s #\t)
                                :max-positionals 0
                                :message "paste-buffer: unsupported argument")
    (let* ((name       (%buffer-name-from-flags flags))
           (delete-p   (%flag-present-p flags #\d))
           (no-replace (%flag-present-p flags #\r))
           (separator  (%flag-value flags #\s))
           (target-str (%flag-value flags #\t))
           (raw        (%named-or-latest-paste-buffer name))
           ;; tmux default: LF → CR so a multi-line paste submits each line; -s
           ;; overrides the replacement, -r keeps the raw bytes.
           (text       (%paste-buffer-text raw no-replace separator)))
      (with-target-context (target-session target-window target-pane session target-str)
        (declare (ignore target-session target-window))
        (when text
          (%paste-to-pane target-pane text (%flag-present-p flags #\p))
          (when delete-p
            (if name
                (cl-tmux/buffer:delete-buffer-by-name name)
                (cl-tmux/buffer:delete-paste-buffer 0))))))))

(defun %cmd-delete-buffer-arg (session args)
  "delete-buffer [-b name]: delete the named buffer (or the most recent)."
  (declare (ignore session))
  (with-command-input (flags positionals args "b"
                             :allowed-flags '(#\b)
                             :max-positionals 0
                             :message "delete-buffer: unsupported argument")
    (let ((name (%buffer-name-from-flags flags)))
      (if name
          (cl-tmux/buffer:delete-buffer-by-name name)
          (cl-tmux/buffer:delete-paste-buffer 0)))))

(defun %cmd-show-buffer-arg (session args)
  "show-buffer [-b name]: show the named buffer's contents (or the most recent)."
  (declare (ignore session))
  (with-command-input (flags positionals args "b"
                             :allowed-flags '(#\b)
                             :max-positionals 0
                             :message "show-buffer: unsupported argument")
    (let* ((name (%buffer-name-from-flags flags))
           (text (%named-or-latest-paste-buffer name)))
      (show-overlay (or text "(no buffer)")))))

(defun %cmd-save-buffer-arg (session args)
  "save-buffer [-a] [-b name] path: save a paste buffer to PATH.
   -b name saves that named buffer; otherwise saves the most recent buffer.
   -a appends instead of overwriting."
  (declare (ignore session))
  (with-command-input (flags positionals args "b"
                             :allowed-flags '(#\a #\b)
                             :max-positionals 1
                             :message "save-buffer: unsupported argument")
    (let* ((name (%buffer-name-from-flags flags))
           (append-p (%buffer-append-p flags))
           (path (first positionals))
           (text (%named-or-latest-paste-buffer name)))
      (when (and path text)
        (%buffer-write-file path text :append-p append-p)))))

(defun %cmd-load-buffer-arg (session args)
  "load-buffer [-b name] path: load PATH into a paste buffer.
   -b name stores the data under NAME; otherwise an automatic buffer name is
   used. tmux also accepts -t target and -w for compatibility, so we parse
   them even though the loader only persists the file contents here."
  (declare (ignore session))
  (with-command-input (flags positionals args "bt"
                             :allowed-flags '(#\b #\t #\w)
                             :max-positionals 1
                             :message "load-buffer: unsupported argument")
    (let ((name (%buffer-name-from-flags flags))
          (path (first positionals)))
      (when path
        (cl-tmux/buffer:add-paste-buffer (%buffer-read-file path) name)))))

;;; ── Popup overlay constants + formatter ─────────────────────────────────────
;;;
;;; Moved here from dispatch-handlers so the arg-bearing %cmd-display-popup and
;;; the :display-popup keyword handler can share them.  These bounds cap the
;;; overlay geometry to the terminal size.

(defconstant +popup-max-width+  60 "Maximum column width of a popup overlay.")
(defconstant +popup-max-height+ 15 "Maximum row height of a popup overlay.")
(defconstant +popup-margin+      4 "Row margin subtracted from terminal height for popups.")

(defun %popup-border-chars ()
  "Return (values TOP-LEFT TOP-RIGHT BOTTOM-LEFT BOTTOM-RIGHT HORIZONTAL) box-
   drawing characters for popup-border-lines.  Delegates to the single source
   cl-tmux/renderer:%popup-border-charset (the text overlay has no sides, so the
   vertical character it also returns is dropped here)."
  (multiple-value-bind (tl tr bl br h v) (cl-tmux/renderer:%popup-border-charset)
    (declare (ignore v))
    (values tl tr bl br h)))

(defun %format-popup-overlay (title output)
  "Format a popup overlay string with box-drawing borders whose characters follow
   the popup-border-lines option.  TITLE is the header; OUTPUT is the body."
  (multiple-value-bind (tl tr bl br h) (%popup-border-chars)
    (format nil "~C~C ~A ~C~C~%~A~%~C~A~C"
            tl h title h tr
            (or output "")
            bl (make-string (+ 2 (length title)) :initial-element h) br)))

(defun %show-popup-command-output (title command width height)
  "Run COMMAND, open a popup sized WIDTH × HEIGHT, and render the command output."
  (let* ((label  (if (plusp (length title)) title command))
         (output (run-shell command)))
    (show-popup (make-popup :title label :width width :height height
                            :screen nil :pane nil))
    (show-overlay (%format-popup-overlay label output))))

(defun %popup-dimension (spec axis-total fallback)
  "Resolve a popup -w/-h dimension SPEC against AXIS-TOTAL (the terminal width or
   height).  SPEC may be NIL (use FALLBACK), an integer string (absolute cells),
   or an N% string (percentage of AXIS-TOTAL, which tmux accepts, e.g. -w 80%).
   Returns a positive integer clamped to [1, AXIS-TOTAL]."
  (let ((n (cond
             ((null spec) fallback)
             ((and (plusp (length spec))
                   (char= (char spec (1- (length spec))) #\%))
              (let ((pct (parse-integer spec :end (1- (length spec)) :junk-allowed t)))
                (if pct (max 1 (floor (* axis-total pct) 100)) fallback)))
             (t (or (parse-integer spec :junk-allowed t) fallback)))))
    (max 1 (min n axis-total))))

(defun %cmd-display-popup (session args)
  "display-popup [-E] [-w width] [-h height] [-x col] [-y row]
   [-d dir] [-t target] [-c client] [-b border] [-T title] [command]: show a popup.

   With a COMMAND, run it in a shell and display its output in the popup directly.
   With NO command, open the interactive popup-command prompt.
   -w/-h accept absolute cells or an N% of the terminal; -E/-EE (close on exit),
   -C (close an open popup), -e VAR=val (env, repeatable), -B (no border),
   -x/-y/-d/-t/-c/-b/-s/-S are parsed and tolerated.  Geometry is clamped to the
   overlay bounds (cl-tmux popups render command output, not a live embedded
   terminal)."
  (declare (ignore session))
  (with-command-flags+pos (flags positionals args "whxydtcbTesS")
    (let* ((title   (%popup-title-from-flags flags))
           (command (when positionals (format nil "~{~A~^ ~}" positionals)))
           (width   (%popup-dimension (%popup-width-from-flags flags) *term-cols* +popup-max-width+))
           (height  (%popup-dimension (%popup-height-from-flags flags) *term-rows*
                                      (min +popup-max-height+ (- *term-rows* +popup-margin+))))
           (clamp-w (min width  *term-cols*))
           (clamp-h (min height (max 1 (- *term-rows* +popup-margin+)))))
      (if command
          (%show-popup-command-output title command clamp-w clamp-h)
          ;; No command: fall back to the interactive popup-command prompt.
          (prompt-nonempty "popup command"
                           (lambda (cmd)
                             (%show-popup-command-output title cmd clamp-w clamp-h)))))))

(defun %cmd-display-menu-arg (session args)
  "display-menu [-T title] [-x x] [-y y] [label key command ...]: show an interactive menu.
   -T title: menu title (default: 'Menu').
   -x col / -y row: screen position (default: centred).  Clamped on screen.
   Item triples: label key command.  Empty label '' creates a visual separator.
   When selected, command is run via %run-command-line.
   Preconfigured commands as keyword tokens run directly.
   -O: the menu stays open after a selection runs its command (tmux -O).
   -C: close-existing control flag (accepted).  -b/-c/-s/-S/-t
   take arguments (border-lines/client/style/border-style/target) and are consumed
   so they do not leak into the item triples."
  (declare (ignore session))  ; session used via closure in item command
  (with-command-flags+pos (flags positionals args "TxybcsSt")
    (let* ((title (%menu-title-from-flags flags))
           (menu-x (%parse-flag-int flags #\x))
           (menu-y (%parse-flag-int flags #\y))
           ;; Build items from consecutive (label key command) triples.
           (items (loop for (label key cmd) on positionals by #'cdddr
                        when (and label key cmd)
                        collect (cons (if (and (plusp (length label))
                                               (plusp (length key)))
                                          (format nil "~A [~A]" label key)
                                          label)
                                      cmd))))
      (cond
        ((null positionals)
         (%overlayf "command display-menu: too few arguments (need at least 1)"))
        (items
         (show-menu (make-menu :title title :items items :selected-index 0
                               :x menu-x :y menu-y
                               :keep-open (%flag-present-p flags #\O)))
         (show-overlay (%format-menu *active-menu*)))))))

(defun %cmd-confirm-before-arg (session args)
  "confirm-before [-y] [-p prompt] [-c confirm-key] [-t target] command:
   prompt before running COMMAND.
   -p prompt: custom prompt text (default: 'command? (y/n)').
   -y: assume yes — run COMMAND immediately without prompting (tmux -y).
   -c confirm-key / -t target: the confirmation key / target client are consumed;
       cl-tmux confirms on 'y'/'Y'.
   COMMAND is the remaining positional tokens as a command line.
   Only executes COMMAND when the user confirms with 'y' or 'Y'."
  (with-command-flags+pos (flags positionals args "pct")
    (multiple-value-bind (window pane) (%active-window-pane session)
      (let* ((custom-prompt (%confirm-prompt-from-flags flags))
             (assume-yes    (%flag-present-p flags #\y))
             (cmd-line      (format nil "~{~A~^ ~}" positionals))
             (ctx           (cl-tmux/format:format-context-from-session
                             session window pane))
             (prompt-text   (if custom-prompt
                                (cl-tmux/format:expand-format-safe custom-prompt ctx)
                                (format nil "~A? (y/n)" cmd-line))))
        (when (plusp (length cmd-line))
          (if assume-yes
              ;; -y: skip the prompt and run COMMAND immediately.
              (%run-command-line session cmd-line)
              ;; Single-key prompt like tmux: one 'y'/'Y' keypress confirms (no
              ;; Enter); any other key cancels.
              (%confirm-prompt prompt-text
                               (lambda ()
                                 (%run-command-line session cmd-line)))))))))

(defun %cmd-list-keys-arg (session args)
  "list-keys [-1aN] [-P prefix] [-T table] [key]: list key bindings.
   -T table: show bindings for TABLE only (e.g. prefix, root, copy-mode-vi).
   Without -T: show all tables.  KEY filters the output to matching bindings.
   -1 keeps only the first line of output.
   -P prefix: a string to print before each key (consumed; accepted).
   -N: list key NOTES (bind -N descriptions) only; with -a, bindings without a
       note are included with their command as the description (tmux).
   The parser accepts -T/-P and an optional key filter."
  (declare (ignore session))
  (with-command-flags+pos (flags positionals args "T1P")
    (let* ((table-name (%list-keys-table-name-from-flags flags))
           (key        (first positionals))
           (output     (cond
                         ((%flag-present-p flags #\N)
                          (cl-tmux/config:describe-key-binding-notes
                           table-name (%flag-present-p flags #\a)))
                         (key
                          (cl-tmux/config:describe-key-bindings-for-key table-name key))
                         (t
                          (cl-tmux/config:describe-key-bindings-for-table table-name))))
           (output     (if (%flag-present-p flags #\1)
                           (let ((newline (position #\Newline output)))
                             (if newline
                                 (subseq output 0 newline)
                                 output))
                           output)))
      (show-overlay (if (plusp (length output))
                        output
                        (format nil "(no bindings in table ~A)"
                                (or table-name "all")))))))

(defun %resolve-copy-mode-screen (session target-str)
  "Return the screen copy-mode should act on: the screen of the pane named by
   TARGET-STR (tmux's -t target-pane convention) when non-NIL, otherwise
   SESSION's active screen."
  (if target-str
      (with-target-context (tsession twin tpane session target-str)
        (declare (ignore tsession twin))
        (and tpane (pane-screen tpane)))
      (%active-screen session)))

(defun %copy-mode-mouse-entry (session screen flags)
  "copy-mode -M: place the copy cursor at the current mouse position and begin
   a selection — tmux's MouseDrag1Pane entry (window_copy start-of-drag).
   No-op without -M, without an in-flight mouse event, or when the event is
   not over the pane owning SCREEN."
  (when (and (%flag-present-p flags #\M) *current-mouse-event*)
    (let* ((event *current-mouse-event*)
           (col   (getf event :col))
           (row   (getf event :row))
           (win   (session-active-window session))
           (pane  (and win col row (pane-at-position win col row))))
      (when (and pane (eq (pane-screen pane) screen))
        (setf (screen-copy-cursor screen)
              (cons (min (max 0 (- row (pane-y pane)))
                         (1- (screen-height screen)))
                    (min (max 0 (- col (pane-x pane)))
                         (1- (screen-width screen)))))
        (copy-mode-begin-selection screen)))))

(defun %cmd-copy-mode-arg (session args)
  "copy-mode [-eHMqu] [-s src-pane] [-t target-pane]: enter (or with -q, leave)
   copy mode on the target pane.
   -u: pre-scroll to the oldest scrollback content (e.g. bind PageUp copy-mode -u).
   -e: exit copy mode automatically when the viewport is scrolled back down to
       the live bottom (offset 0).  `bind -n WheelUpPane copy-mode -e`.
   -q: cancel copy mode on the target pane instead of entering it.
   -M: mouse-drag entry — the copy cursor jumps to the in-flight mouse position
       and a selection begins (the default MouseDrag1Pane binding shape).
   -t target-pane: act on a specific pane (default: the active pane), the
       universal tmux target convention (e.g. `copy-mode -t %3`).
   -s src-pane: view SRC-PANE's history — copy mode is entered on the source
       pane's screen (cl-tmux's per-screen copy mode shows the content there
       rather than mirroring it into the target pane).
   -H: hide the position indicator overlay for this copy-mode entry."
  (with-command-input (flags positionals args "ts"
                             :allowed-flags '(#\u #\e #\q #\t #\s #\M #\H #\d #\S)
                             :max-positionals 0
                             :message "copy-mode: unsupported argument")
    (let ((screen (%resolve-copy-mode-screen session
                                             (or (%flag-value flags #\s)
                                                 (%flag-value flags #\t)))))
      (when screen
        (if (%flag-present-p flags #\q)
            ;; -q cancels copy mode on the target pane (no-op when not active).
            (when (screen-copy-mode-p screen)
              (copy-mode-exit screen)
              (setf *dirty* t))
            (progn
              (copy-mode-enter screen
                               :scroll-to-top (%copy-mode-scroll-to-top-p flags)
                               :exit-on-bottom (%copy-mode-exit-on-bottom-p flags))
              ;; -H: suppress the position indicator for this entry; a later
              ;; plain entry shows it again.
              (setf (cl-tmux/terminal/types:screen-copy-hide-position screen)
                    (and (%flag-present-p flags #\H) t))
              (%copy-mode-mouse-entry session screen flags)
              (setf *dirty* t)))))))

;;; *set-option-command-names* removed — inlined into *arg-command-table* below.
