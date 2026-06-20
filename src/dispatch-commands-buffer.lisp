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
      (cl-tmux/buffer:get-buffer-by-name name)
      (cl-tmux/buffer:get-paste-buffer 0)))

(defun %buffer-name-from-flags (flags)
  "Return the named buffer selected by -b in FLAGS, or NIL when absent."
  (%flag-value flags #\b))

(defun %buffer-append-p (flags)
  "Return T when the command FLAGS include -a."
  (%flag-present-p flags #\a))

(defun %popup-title-from-flags (flags)
  "Return the popup title encoded by FLAGS, or the empty title when absent."
  (or (%flag-value flags #\T) ""))

(defun %popup-width-from-flags (flags)
  "Return the popup width encoded by FLAGS, or NIL when absent."
  (%flag-value flags #\w))

(defun %popup-height-from-flags (flags)
  "Return the popup height encoded by FLAGS, or NIL when absent."
  (%flag-value flags #\h))

(defun %menu-title-from-flags (flags)
  "Return the menu title encoded by FLAGS, or the default menu title."
  (or (%flag-value flags #\T) "Menu"))

(defun %confirm-prompt-from-flags (flags)
  "Return the custom confirm prompt encoded by FLAGS, or NIL when absent."
  (%flag-value flags #\p))

(defun %list-keys-table-name-from-flags (flags)
  "Return the key table encoded by FLAGS, or NIL when absent."
  (%flag-value flags #\T))

(defun %copy-mode-scroll-to-top-p (flags)
  "Return T when FLAGS request copy-mode to start at the top."
  (and (%flag-present-p flags #\u) t))

(defun %copy-mode-exit-on-bottom-p (flags)
  "Return T when FLAGS request copy-mode to exit at the bottom."
  (and (%flag-present-p flags #\e) t))

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

(defun %cmd-set-buffer-arg (session args)
  "set-buffer [-a] [-b name] [-n new-name] data...:
   set a paste buffer's contents.  -b name stores DATA under NAME; without -b
   an automatic name (bufferN) is assigned.  -n new-name renames the selected
   buffer (or the most recent one) to NEW-NAME and ignores DATA."
  (declare (ignore session))
  (with-command-input (flags positionals args "bn"
                             :allowed-flags '(#\a #\b #\n)
                             :message "set-buffer: unsupported argument")
    (let* ((name     (%buffer-name-from-flags flags))
           (new-name (%flag-value flags #\n))
           (append-p (%buffer-append-p flags))
           (data     (%buffer-positionals-text positionals)))
      (cond
        (new-name
         (unless (cl-tmux/buffer:rename-paste-buffer name new-name)
           (show-overlay "no buffer")))
        (positionals
         (if append-p
             (let ((existing (or (%named-or-latest-paste-buffer name) "")))
               (cl-tmux/buffer:add-paste-buffer
                (concatenate 'string existing data) name))
             (cl-tmux/buffer:add-paste-buffer data name)))))))

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
   raw.  Bracketed paste is applied automatically by %paste-to-pane when the
   application has enabled it.  -p is accepted but not specially handled."
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
          (%paste-to-pane target-pane text)
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
   -w/-h accept absolute cells or an N% of the terminal; -E/-EE and
   -x/-y/-d/-t/-c/-b are parsed and tolerated.  Geometry is clamped to the overlay
   bounds (cl-tmux popups render command output, not a live embedded terminal)."
  (declare (ignore session))
  (with-command-flags+pos (flags positionals args "whxydtcbT")
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
   Preconfigured commands as keyword tokens run directly."
  (declare (ignore session))  ; session used via closure in item command
  (with-command-flags+pos (flags positionals args "Txy")
    (let* ((title (%menu-title-from-flags flags))
           (menu-x (%parse-flag-int flags #\x))
           (menu-y (%parse-flag-int flags #\y))
           ;; Build items from consecutive (label key command) triples.
           ;; Silently skip incomplete triples (real tmux shows an error).
           (items (loop for (label key cmd) on positionals by #'cdddr
                        when (and label key cmd)
                        collect (cons (if (and (plusp (length label))
                                               (plusp (length key)))
                                          (format nil "~A [~A]" label key)
                                          label)
                                      cmd))))
      (when items
        (show-menu (make-menu :title title :items items :selected-index 0
                              :x menu-x :y menu-y))
        (show-overlay (%format-menu *active-menu*))))))

(defun %cmd-confirm-before-arg (session args)
  "confirm-before [-p prompt] command: prompt before running COMMAND.
   -p prompt: custom prompt text (default: 'command? (y/n)').
   COMMAND is the remaining positional tokens as a command line.
   Only executes COMMAND when the user confirms with 'y' or 'Y'."
  (with-command-flags+pos (flags positionals args "p")
    (multiple-value-bind (window pane) (%active-window-pane session)
      (let* ((custom-prompt (%confirm-prompt-from-flags flags))
             (cmd-line      (format nil "~{~A~^ ~}" positionals))
             (ctx           (cl-tmux/format:format-context-from-session
                             session window pane))
             (prompt-text   (if custom-prompt
                                (handler-case
                                    (cl-tmux/format:expand-format custom-prompt ctx)
                                  (error () custom-prompt))
                                (format nil "~A? (y/n)" cmd-line))))
        (when (plusp (length cmd-line))
          ;; Single-key prompt like tmux: one 'y'/'Y' keypress confirms (no Enter);
          ;; any other key cancels.
          (%confirm-prompt prompt-text
                           (lambda ()
                             (%run-command-line session cmd-line))))))))

(defun %cmd-list-keys-arg (session args)
  "list-keys [-T table] [-1] [key]: list key bindings.
   -T table: show bindings for TABLE only (e.g. prefix, root, copy-mode-vi).
   Without -T: show all tables.  KEY filters the output to matching bindings.
   -1 keeps only the first line of output.
   The parser accepts -T and an optional key filter."
  (declare (ignore session))
  (with-command-flags+pos (flags positionals args "T1")
    (let* ((table-name (%list-keys-table-name-from-flags flags))
           (key        (first positionals))
           (output     (if key
                           (cl-tmux/config:describe-key-bindings-for-key table-name key)
                           (cl-tmux/config:describe-key-bindings-for-table table-name)))
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

(defun %cmd-copy-mode-arg (session args)
  "copy-mode [-e] [-u]: enter copy mode.
   -u: pre-scroll to the oldest scrollback content (e.g. bind PageUp copy-mode -u).
   -e: exit copy mode automatically when the viewport is scrolled back down to
       the live bottom (offset 0).  Standard for mouse-wheel copy-mode entry:
       `bind -n WheelUpPane copy-mode -e` enters copy mode on scroll-up and
       leaves it once the user scrolls back to the live output."
  (with-command-input (flags positionals args ""
                             :allowed-flags '(#\u #\e)
                             :max-positionals 0
                             :message "copy-mode: unsupported argument")
    (let* ((scroll-to-top  (%copy-mode-scroll-to-top-p flags))
           (exit-on-bottom (%copy-mode-exit-on-bottom-p flags))
           (screen (%active-screen session)))
      (when screen
        (copy-mode-enter screen :scroll-to-top scroll-to-top
                                :exit-on-bottom exit-on-bottom)
        (setf *dirty* t)))))

;;; *set-option-command-names* removed — inlined into *arg-command-table* below.
