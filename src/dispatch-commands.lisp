(in-package #:cl-tmux)

;;; -- Arg-aware command-line runner -------------------------------------------
;;;
;;; The C-b : prompt may name a command WITH arguments (e.g.
;;; "display-message #{session_name}").  %run-command-line tokenises the line
;;; (shared shell-style lexer), routes arg-taking commands to their handlers, and
;;; falls through to the no-argument name table for everything else.

(defun %cmd-display-message (session args)
  "display-message [-l] [-d ms] [-t target] <fmt...>: expand the space-joined ARGS as a format string
   against the target (or active) session/window/pane, then log and show the result.
   -l: literal — show ARGS verbatim WITHOUT expanding #{...} format variables.
   -d ms: display duration in milliseconds (overrides display-time option).
   -t target: build the format context from the target's session/window/pane.
   -c target-client: accepted (consumes its argument) but a no-op — cl-tmux has a
   single client, so there is no other client to target.  This keeps
   `display-message -c <client> <fmt>` from mis-reading the client name as part of
   the format.
   -p/-v are tolerated (printing to stdout / verbose logging are no-ops in the
   single-client UI; the message is still shown as an overlay).
   Uses show-transient-overlay so it auto-dismisses after the configured duration."
  (multiple-value-bind (flags positionals) (%parse-command-flags args "dtc")
    (let* ((delay-str  (cdr (assoc #\d flags)))
           (delay-ms   (and delay-str (parse-integer delay-str :junk-allowed t)))
           (target-str (cdr (assoc #\t flags)))
           ;; -t: resolve to a target session/window/pane; fall back to active.
           (tgt-session session)
           (tgt-win    (session-active-window session))
           (tgt-pane   (session-active-pane session)))
      (when target-str
        (multiple-value-bind (rs rw rp)
            (resolve-target *server-sessions* target-str
                            :current-session session
                            :current-window  (session-active-window session)
                            :current-pane    (session-active-pane session))
          (when rs (setf tgt-session rs))
          (when rw (setf tgt-win rw))
          (when rp (setf tgt-pane rp))))
    (let* ((win       tgt-win)
           (pane      tgt-pane)
           (ctx       (cl-tmux/format:format-context-from-session tgt-session win pane))
           (raw       (format nil "~{~A~^ ~}" positionals))
           ;; -l: literal — emit ARGS unchanged, skipping #{...} expansion so a
           ;; message containing literal '#' / '#{' is shown as typed.
           (text      (if (assoc #\l flags)
                          raw
                          (cl-tmux/format:expand-format raw ctx))))
      (add-message-log text)
      (if delay-ms
          ;; Custom delay: temporarily override display-time for this message.
          (let ((saved (cl-tmux/options:get-option "display-time" 750)))
            (cl-tmux/options:set-option "display-time" delay-ms)
            (show-transient-overlay text)
            (cl-tmux/options:set-option "display-time" saved))
          (show-transient-overlay text))))))

(defun %resolve-pane-in-window (win target-str)
  "Resolve TARGET-STR to a pane in WIN by pane-id; default to WIN's active pane
   when TARGET-STR is NIL or names no pane in WIN.  Accepts both the bare id (\"2\")
   and the tmux %N sigil (\"%2\") — a leading '%' is stripped before parsing.
   Shared by select-pane, swap-pane and pipe-pane for -s/-t resolution."
  (or (and target-str win
           (let* ((digits (if (and (plusp (length target-str))
                                   (char= (char target-str 0) #\%))
                              (subseq target-str 1)
                              target-str))
                  (n      (parse-integer digits :junk-allowed t)))
             (and n (find n (window-panes win) :key #'pane-id))))
      (and win (window-active-pane win))))

(defun %cmd-swap-pane-arg (session args)
  "swap-pane [-dUDLRZ] [-s src-pane] [-t dst-pane]: swap two panes.
   -s src / -t dst: swap those two panes (pane-ids in the active window; each
     defaults to the active pane), e.g. swap-pane -s 1 -t 3.
   -U/-D/-L/-R: swap the active pane with the adjacent pane in that direction.
   -d (keep active) and -Z (keep zoom) are accepted.
   With neither -s/-t nor a direction: swap forward (same as C-b })."
  (multiple-value-bind (flags _positionals) (%parse-command-flags args "st")
    (declare (ignore _positionals))
    (with-active-window (win session)
      (cond
        ((assoc #\U flags) (swap-pane win :up))
        ((assoc #\D flags) (swap-pane win :down))
        ((assoc #\L flags) (swap-pane win :left))
        ((assoc #\R flags) (swap-pane win :right))
        ;; -s/-t: swap two specific panes (each defaults to the active pane).
        ((or (assoc #\s flags) (assoc #\t flags))
         (swap-two-panes win
                         (%resolve-pane-in-window win (cdr (assoc #\s flags)))
                         (%resolve-pane-in-window win (cdr (assoc #\t flags)))))
        ;; No direction, no -s/-t: swap forward (default tmux behaviour).
        (t (swap-pane win :right))))))

(defun %cmd-command-prompt-arg (session args)
  "command-prompt [-p prompts] [template]: open a command prompt with optional args.
   -p prompts: comma-separated list of prompt labels; each label becomes a
     separate sequential prompt.  On completion, each response replaces %%1, %%2,
     etc. in TEMPLATE and the expanded command is executed.
   Without -p: single prompt ':' that runs the typed command line (same as C-b :).
   Without TEMPLATE: input is executed directly as a command line.
   -1: single-key prompt — each prompt accepts ONE keypress (no Enter)."
  (multiple-value-bind (flags positionals) (%parse-command-flags args "p")
    (let* ((prompts-str (cdr (assoc #\p flags)))
           (single-key  (and (assoc #\1 flags) t))   ; -1: one-keypress prompts
           (template    (format nil "~{~A~^ ~}" positionals))
           (prompt-list (when prompts-str
                          (mapcar (lambda (s) (string-trim " " s))
                                  (uiop:split-string prompts-str :separator ","))))
           (num-prompts (length prompt-list)))
      (cond
        ;; -p with template: multi-prompt with %%N substitution
        ((and prompt-list (plusp (length template)))
         (let ((answers (make-array num-prompts :initial-element "")))
           (labels ((ask-prompt (idx)
                      (if (>= idx num-prompts)
                          ;; All prompts answered — substitute %%N → answer and run
                          (let ((cmd (%substitute-percent
                                      template
                                      (loop for i below num-prompts collect (aref answers i)))))
                            (%run-command-line session cmd))
                          ;; Ask next prompt
                          (let ((label (nth idx prompt-list)))
                            (prompt-start label "" (lambda (input)
                                                     (setf (aref answers idx) input)
                                                     (ask-prompt (1+ idx)))
                                          :single-key single-key)))))
             (ask-prompt 0))))
        ;; -p without template: each prompt result is concatenated
        (prompt-list
         (let ((label (first prompt-list)))
           (prompt-start (or label ": ") ""
                         (lambda (input)
                           (unless (string= input "")
                             (add-prompt-history input)
                             (%run-command-line session input)))
                         :single-key single-key)))
        ;; No -p: standard C-b : interactive prompt
        (t
         (prompt-start ": " ""
                       (lambda (input)
                         (unless (string= input "")
                           (add-prompt-history input)
                           (%run-command-line session input)))
                       :single-key single-key))))))

(defun %substitute-percent (template args)
  "Expand a command-prompt template: %1..%9 are replaced by the 1st..9th element
   of ARGS (an empty string when that arg is absent, matching tmux), %% is a
   literal percent, and any other %x is left verbatim.  Used by command-prompt -p.
   A single left-to-right pass so %1 never matches inside %10 and %% is not itself
   treated as an argument reference."
  (let ((out (make-string-output-stream))
        (n   (length template))
        (i   0))
    (loop while (< i n)
          for ch = (char template i)
          do (if (and (char= ch #\%) (< (1+ i) n))
                 (let ((next (char template (1+ i))))
                   (cond
                     ((char= next #\%)               ; %% → literal %
                      (write-char #\% out) (incf i 2))
                     ((and (digit-char-p next) (char/= next #\0)) ; %1..%9 → arg
                      (let ((idx (digit-char-p next)))
                        (when (<= idx (length args))
                          (write-string (nth (1- idx) args) out)))
                      (incf i 2))
                     (t                              ; %x (other) → verbatim
                      (write-char ch out) (incf i))))
                 (progn (write-char ch out) (incf i))))
    (get-output-stream-string out)))

(defun %cmd-last-pane-arg (session args)
  "last-pane [-Z]: jump to the previously active pane.
   -Z: zoom/unzoom the pane after selecting it (toggle zoom state)."
  (multiple-value-bind (flags _pos) (%parse-command-flags args "")
    (declare (ignore _pos))
    (let* ((win  (session-active-window session))
           (last (and win (window-last-active win))))
      (when last
        (%select-pane-with-focus win last)
        ;; -Z: toggle zoom on the newly selected pane's window.
        (when (assoc #\Z flags)
          (with-active-window (w session)
            (window-zoom-toggle w)))))))

(defun %cmd-has-session-arg (session args)
  "has-session [-t name]: check if a named session exists.
   Shows a transient overlay: 'has-session: yes' or 'has-session: no'.
   Without -t: checks if there is any session in *server-sessions*."
  (declare (ignore session))
  (multiple-value-bind (flags _positionals) (%parse-command-flags args "t")
    (declare (ignore _positionals))
    (let* ((target-name (cdr (assoc #\t flags)))
           (found       (if target-name
                            (server-find-session target-name)
                            (not (null *server-sessions*)))))
      (show-transient-overlay
       (if found
           (format nil "has-session ~A: yes" (or target-name ""))
           (format nil "has-session ~A: no"  (or target-name "")))))))

;;; ── Named paste-buffer commands (set/paste/delete/show -b name) ──────────────
;;;
;;; tmux's set-buffer/paste-buffer/delete-buffer/show-buffer all accept -b <name>
;;; to target a specific named buffer.  These arg-bearing handlers (registered in
;;; *arg-command-table*) layer over cl-tmux/buffer's named-buffer API; the no-arg
;;; keyword handlers (:set-buffer etc. in dispatch-handlers) remain for the C-b
;;; interactive bindings.

(defun %cmd-set-buffer-arg (session args)
  "set-buffer [-a] [-b name] [-t target] data: set a paste buffer's contents.
   -b name: name the buffer (retrievable via paste-buffer -b name, etc.); without
     -b an automatic name (bufferN) is assigned.
   -a: append DATA to the existing buffer (named NAME, or the most recent)."
  (declare (ignore session))
  (multiple-value-bind (flags positionals) (%parse-command-flags args "bt")
    (let* ((name     (cdr (assoc #\b flags)))
           (append-p (and (assoc #\a flags) t))
           (data     (format nil "~{~A~^ ~}" positionals)))
      (when positionals
        (if append-p
            (let ((existing (or (if name
                                    (cl-tmux/buffer:get-buffer-by-name name)
                                    (cl-tmux/buffer:get-paste-buffer 0))
                                "")))
              (cl-tmux/buffer:add-paste-buffer
               (concatenate 'string existing data) name))
            (cl-tmux/buffer:add-paste-buffer data name))))))

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
  (multiple-value-bind (flags _positionals) (%parse-command-flags args "bst")
    (declare (ignore _positionals))
    (let* ((name       (cdr (assoc #\b flags)))
           (delete-p   (and (assoc #\d flags) t))
           (no-replace (and (assoc #\r flags) t))
           (separator  (cdr (assoc #\s flags)))
           (target-str (cdr (assoc #\t flags)))
           (raw        (if name
                           (cl-tmux/buffer:get-buffer-by-name name)
                           (cl-tmux/buffer:get-paste-buffer 0)))
           ;; tmux default: LF → CR so a multi-line paste submits each line; -s
           ;; overrides the replacement, -r keeps the raw bytes.
           (text       (%paste-buffer-text raw no-replace separator))
           (target-pane (if target-str
                            (nth-value 2 (resolve-target
                                          *server-sessions* target-str
                                          :current-session session
                                          :current-window (session-active-window session)
                                          :current-pane (session-active-pane session)))
                            (session-active-pane session))))
      (when text
        (%paste-to-pane target-pane text)
        (when delete-p
          (if name
              (cl-tmux/buffer:delete-buffer-by-name name)
              (cl-tmux/buffer:delete-paste-buffer 0)))))))

(defun %cmd-delete-buffer-arg (session args)
  "delete-buffer [-b name]: delete the named buffer (or the most recent)."
  (declare (ignore session))
  (multiple-value-bind (flags _positionals) (%parse-command-flags args "b")
    (declare (ignore _positionals))
    (let ((name (cdr (assoc #\b flags))))
      (if name
          (cl-tmux/buffer:delete-buffer-by-name name)
          (cl-tmux/buffer:delete-paste-buffer 0)))))

(defun %cmd-show-buffer-arg (session args)
  "show-buffer [-b name]: show the named buffer's contents (or the most recent)."
  (declare (ignore session))
  (multiple-value-bind (flags _positionals) (%parse-command-flags args "b")
    (declare (ignore _positionals))
    (let* ((name (cdr (assoc #\b flags)))
           (text (if name
                     (cl-tmux/buffer:get-buffer-by-name name)
                     (cl-tmux/buffer:get-paste-buffer 0))))
      (show-overlay (or text "(no buffer)")))))

;;; ── Popup overlay constants + formatter ─────────────────────────────────────
;;;
;;; Moved here from dispatch-handlers so BOTH the arg-bearing %cmd-display-popup
;;; (below, registered in *arg-command-table*) and the legacy :display-popup
;;; keyword handler (in dispatch-handlers, which loads after dispatch-core) can
;;; share them.  These bounds cap the overlay geometry to the terminal size.

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
  "display-popup (alias: popup) [-E] [-w width] [-h height] [-x col] [-y row]
   [-d dir] [-t target] [-c client] [-b border] [-T title] [command]: show a popup.

   With a COMMAND (the common `bind C-p popup -E \"cmd\"` form), run it in a shell
   and display its output in the popup directly — no prompt.  With NO command, open
   the interactive popup-command prompt (the legacy :display-popup behaviour).
   -w/-h accept absolute cells or an N% of the terminal; -E/-EE and
   -x/-y/-d/-t/-c/-b are parsed and tolerated.  Geometry is clamped to the overlay
   bounds (cl-tmux popups render command output, not a live embedded terminal)."
  (declare (ignore session))
  (multiple-value-bind (flags positionals) (%parse-command-flags args "whxydtcbT")
    (let* ((title   (or (cdr (assoc #\T flags)) ""))
           (command (when positionals (format nil "~{~A~^ ~}" positionals)))
           (width   (%popup-dimension (cdr (assoc #\w flags)) *term-cols* +popup-max-width+))
           (height  (%popup-dimension (cdr (assoc #\h flags)) *term-rows*
                                      (min +popup-max-height+ (- *term-rows* +popup-margin+))))
           (clamp-w (min width  *term-cols*))
           (clamp-h (min height (max 1 (- *term-rows* +popup-margin+)))))
      (flet ((render (cmd)
               (let ((label  (if (plusp (length title)) title cmd))
                     (output (run-shell cmd)))
                 (show-popup (make-popup :title label :width clamp-w :height clamp-h
                                         :screen nil :pane nil))
                 (show-overlay (%format-popup-overlay label output)))))
        (if command
            (render command)
            ;; No command: fall back to the interactive popup-command prompt.
            (prompt-start "popup command" ""
                          (lambda (cmd)
                            (unless (string= cmd "") (render cmd)))))))))

(defun %cmd-display-menu-arg (session args)
  "display-menu [-T title] [-x x] [-y y] [label key command ...]: show an interactive menu.
   -T title: menu title (default: 'Menu').
   -x col / -y row: screen position (default: centred).  Clamped on screen.
   Item triples: label key command.  Empty label '' creates a visual separator.
   When selected, command is run via %run-command-line.
   Preconfigured commands as keyword tokens run directly (for compatibility)."
  (declare (ignore session))  ; session used via closure in item command
  (multiple-value-bind (flags positionals) (%parse-command-flags args "Txy")
    (let* ((title (or (cdr (assoc #\T flags)) "Menu"))
           (x-str (cdr (assoc #\x flags)))
           (y-str (cdr (assoc #\y flags)))
           (menu-x (and x-str (parse-integer x-str :junk-allowed t)))
           (menu-y (and y-str (parse-integer y-str :junk-allowed t)))
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
  (multiple-value-bind (flags positionals) (%parse-command-flags args "p")
    (let* ((custom-prompt (cdr (assoc #\p flags)))
           (cmd-line      (format nil "~{~A~^ ~}" positionals))
           (prompt-text   (or custom-prompt
                              (format nil "~A? (y/n)" cmd-line))))
      (when (plusp (length cmd-line))
        ;; Single-key prompt like tmux: one 'y'/'Y' keypress confirms (no Enter);
        ;; any other key cancels.
        (prompt-start prompt-text ""
                      (lambda (input)
                        (when (member input '("y" "Y") :test #'string=)
                          (%run-command-line session cmd-line)))
                      :single-key t)))))

(defun %cmd-list-keys-arg (session args)
  "list-keys [-T table] [-1] [key]: list key bindings.
   -T table: show bindings for TABLE only (e.g. prefix, root, copy-mode-vi).
   Without -T: show all tables.  Additional positionals and flags (-1) are accepted
   but ignored for simplicity (cl-tmux shows the full table always)."
  (declare (ignore session))
  (multiple-value-bind (flags _positionals) (%parse-command-flags args "T")
    (declare (ignore _positionals))
    (let* ((table-name (cdr (assoc #\T flags)))
           (output     (cl-tmux/config:describe-key-bindings-for-table table-name)))
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
  (let* ((flags (nth-value 0 (%parse-command-flags args "")))
         (scroll-to-top  (and (assoc #\u flags) t))
         (exit-on-bottom (and (assoc #\e flags) t))
         (screen (%active-screen session)))
    (when screen
      (copy-mode-enter screen :scroll-to-top scroll-to-top
                              :exit-on-bottom exit-on-bottom)
      (setf *dirty* t))))

;;; *set-option-command-names* removed — inlined into *arg-command-table* below.

;;; ── set-option scope helpers (CPS + data-logic separation) ─────────────────
;;;
;;; %cmd-set-option decomposes into three concerns:
;;;   1. Value expansion (-F flag) — data transformation before storage.
;;;   2. Scope resolution (-g/-w/-p/-t) — which store to use.
;;;   3. Operation dispatch (-u unset / -a append / -o guard / normal set).
;;;
;;; %with-option-scope resolves the scope ONCE and passes (scope target) to a
;;; continuation K.  The three %scope-* functions are pure scope→effect transforms
;;; with ecase — exhaustive, so the compiler warns on any missing scope kind.

(defun %expand-F-flag (flags session raw-value)
  "Expand RAW-VALUE as a format string when FLAGS contains -F; else return as-is."
  (if (assoc #\F flags)
      (cl-tmux/format:expand-format
       raw-value
       (cl-tmux/format:format-context-from-session
        session (session-active-window session) (session-active-pane session)))
      raw-value))

(defun %with-option-scope (session flags target-str k)
  "Resolve the option scope from FLAGS / TARGET-STR, then call K with (scope target).
   SCOPE is :pane, :window, or :global; TARGET is the resolved pane/window (NIL for
   :global).  Falls back to :global when -p/-w resolves to a NIL target."
  (let ((globalp (and (assoc #\g flags) t)))
    (cond
      ((and (assoc #\p flags) (not globalp))
       (let ((pane (if target-str
                       (%resolve-pane-in-window (session-active-window session) target-str)
                       (session-active-pane session))))
         (funcall k (if pane :pane :global) pane)))
      ((and (assoc #\w flags) (not globalp))
       (let ((win (if target-str
                      (%resolve-window-target session target-str)
                      (session-active-window session))))
         (funcall k (if win :window :global) win)))
      (t
       (funcall k :global nil)))))

(defun %scope-unset (name scope target)
  "Remove NAME from the option store identified by SCOPE / TARGET."
  (ecase scope
    (:pane   (remhash name (cl-tmux/model:pane-local-options target)))
    (:window (remhash name (cl-tmux/model:window-local-options target)))
    (:global (remhash name cl-tmux/options:*global-options*))))

(defun %scope-append (name value scope target)
  "Append VALUE to option NAME in the store identified by SCOPE / TARGET.
   Style options (e.g. status-style) join with ',' via append-option-value."
  (flet ((cur (v) (cl-tmux/options:append-option-value name v value)))
    (ecase scope
      (:pane
       (cl-tmux/options:set-option-for-pane
        name (cur (cl-tmux/options:get-option-for-pane name target)) target))
      (:window
       (cl-tmux/options:set-option-for-window
        name (cur (cl-tmux/options:get-option-for-window name target)) target))
      (:global
       (cl-tmux/options:set-option name (cur (cl-tmux/options:get-option name nil)))))))

(defun %scope-set (name value scope target)
  "Store VALUE for option NAME in the store identified by SCOPE / TARGET."
  (ecase scope
    (:pane   (cl-tmux/options:set-option-for-pane name value target))
    (:window (cl-tmux/options:set-option-for-window name value target))
    (:global (cl-tmux/options:set-option name value))))

(defun %cmd-set-option (session args)
  "set / set-option [-aFgopsuw] [-t target] <name> <value...>: set an option.
   Scope: -p pane-local, -w window-local, -g global (default), -s → global.
   Operation: -u unset, -a append, -o only-if-unset, default: set.
   -F expands #{...} in VALUE before storage (one-shot format resolution)."
  (multiple-value-bind (flags positionals) (%parse-command-flags args "t")
    (let* ((name       (first positionals))
           (raw-value  (format nil "~{~A~^ ~}" (rest positionals)))
           (value      (%expand-F-flag flags session raw-value))
           (target-str (cdr (assoc #\t flags))))
      (when name
        (%with-option-scope session flags target-str
          (lambda (scope target)
            (cond
              ((assoc #\u flags)
               (%scope-unset name scope target))
              ((assoc #\a flags)
               (%scope-append name value scope target))
              ((and (assoc #\o flags)
                    (nth-value 1 (gethash name cl-tmux/options:*global-options*)))
               nil)
              (t
               (%scope-set name value scope target)))
            ;; Side-effects for special options (prefix/status/escape-time etc.)
            ;; always run after the operation, even when -o skips the write.
            ;; Passes RAW value — side-effect parsers expect strings, not coerced types.
            (cl-tmux/config:%apply-option-side-effects name value)))))))

(defun %cmd-set-window-option (session args)
  "set-window-option / setw: like set-option but defaults to WINDOW scope (tmux
   `setw` is `set -w`).  Prepends -w so a bare `setw mode-keys vi` is window-local;
   an explicit -g still wins (global), since %cmd-set-option's (and windowp (not
   globalp)) gate lets -g override the injected -w."
  (%cmd-set-option session (cons "-w" args)))

;;; -- -e VAR=val environment flag parser ----------------------------------------
;;;
;;; new-window and split-window accept repeated -e VAR=val flags to set
;;; environment variables in the new pane.  This helper collects them from
;;; an already-parsed flags alist (produced by %parse-command-flags with "e"
;;; in value-flags) into an alist suitable for %fork-pane's :extra-env.

(defun %collect-env-flags (flags-alist)
  "Extract all (-e . \"VAR=val\") entries from FLAGS-ALIST and return an alist
   of (\"VAR\" . \"val\") pairs.  Entries without \"=\" are included as (\"NAME\" . \"\").
   Multiple -e flags are supported; all are collected."
  (loop for (char . value) in flags-alist
        when (and (char= char #\e) (stringp value))
        collect (let ((eq-pos (position #\= value)))
                  (if eq-pos
                      (cons (subseq value 0 eq-pos)
                            (subseq value (1+ eq-pos)))
                      (cons value "")))))

(defun %cmd-rename-window (session args)
  "rename-window [-t target-window] <name...>: rename the target window (default:
   the active window) to the joined remaining ARGS.  Without -t parsing, a bare
   `rename-window -t @2 foo` would fold the flag tokens into the name and rename
   the wrong (active) window."
  (multiple-value-bind (flags positionals) (%parse-command-flags args "t")
    (let* ((target-str (cdr (assoc #\t flags)))
           (win        (if target-str
                           (%resolve-window-target session target-str)
                           (session-active-window session)))
           (name       (format nil "~{~A~^ ~}" positionals)))
      (when (and win (plusp (length name)))
        (rename-window win name)))))

(defun %rename-session-checked (session new-name)
  "Rename SESSION to NEW-NAME, keeping *server-sessions* keyed by the new name and
   firing +hook-session-renamed+.  REFUSES (returns NIL) when NEW-NAME is empty or
   already used by a DIFFERENT session — tmux rejects a rename onto an existing name
   (`duplicate session`) rather than silently orphaning the other session; renaming
   to the session's CURRENT name is a harmless no-op that still succeeds.  The single
   chokepoint both rename paths (arg command + interactive prompt) route through.
   Returns T on success."
  (when (and new-name (not (string= new-name "")))
    (let ((existing (server-find-session new-name)))
      (unless (and existing (not (eq existing session)))   ; a different session owns it
        (server-remove-session (session-name session))
        (rename-session session new-name)
        (server-add-session session)
        (cl-tmux/hooks:run-hooks cl-tmux/hooks:+hook-session-renamed+ session)
        t))))

(defun %cmd-rename-session (session args)
  "rename-session [-t target-session] <name...>: rename the target session (default:
   the current one) to the joined remaining ARGS, updating the registry key.
   Refuses a name already used by another session (see %rename-session-checked).
   Without -t parsing, `rename-session -t old new` would fold the flag tokens into
   the name and rename the wrong (current) session."
  (multiple-value-bind (flags positionals) (%parse-command-flags args "t")
    (let* ((target-str (cdr (assoc #\t flags)))
           (target     (if target-str
                           (or (find-session-by-target *server-sessions* target-str)
                               session)
                           session))
           (name       (format nil "~{~A~^ ~}" positionals)))
      (when (plusp (length name))
        (%rename-session-checked target name)))))

;;; -- Flag parser (-t target, boolean flags) ----------------------------------
;;;
;;; Many tmux commands take a -t target plus boolean flags (-d, -p, ...).  This
;;; splits a token list into (alist-of-flags . positionals).  Flags whose char is
;;; in VALUE-FLAGS consume the next token (or an attached -Xvalue) as their value;
;;; the rest are boolean (T).  Used by select-window/-pane and any future -t cmd.

(defun %parse-flag-token (token value-flags remaining-tokens)
  "Parse one flag TOKEN (token[0] = #\\- and token[1] != #\\-) into one or more
   flag entries, supporting CLUSTERED boolean flags the way tmux does:
   -ga = -g -a, -dP = -d -P, -gF = -g -F.  Returns (values FLAG-ENTRIES
   NEW-REMAINING): FLAG-ENTRIES is a list of (char . value) conses; NEW-REMAINING
   is the residual token list after a value-flag consumes its argument.
   Clustering stops at the first value-flag char (one in VALUE-FLAGS): the rest of
   the token becomes its attached value (e.g. -p50 → (#\\p . \"50\")), or the next
   token is consumed when nothing is attached (e.g. -t target).  Boolean flags
   before it each become their own (char . T) entry."
  (let ((entries nil)
        (len     (length token))
        (i       1))
    (block scan
      (loop while (< i len) do
        (let ((ch (char token i)))
          (if (find ch value-flags)
              ;; Value-flag: the remainder of the token is its attached value;
              ;; otherwise consume the next whole token.  Ends the cluster.
              (let ((attached (when (< (1+ i) len) (subseq token (1+ i)))))
                (if attached
                    (push (cons ch attached) entries)
                    (progn
                      (push (cons ch (if remaining-tokens (first remaining-tokens) ""))
                            entries)
                      (setf remaining-tokens (if remaining-tokens
                                                 (rest remaining-tokens)
                                                 nil))))
                (return-from scan))
              ;; Boolean flag: record it and continue clustering.
              (progn (push (cons ch t) entries)
                     (incf i))))))
    (values (nreverse entries) remaining-tokens)))

(defun %parse-command-flags (tokens &optional (value-flags ""))
  "Split TOKENS into (values FLAGS POSITIONALS).  A -X token is a flag; when X is
   in VALUE-FLAGS it consumes the next token (or the attached -Xvalue) as its
   value, otherwise it is boolean (T).  FLAGS is an alist of (flag-char . value)
   (look up with ASSOC, which uses EQL on the character); POSITIONALS is the
   remaining non-flag tokens in order."
  (loop with flags = nil and positionals = nil and rest = tokens
        while rest
        for token = (first rest)
        do (setf rest (rest rest))
           (if (and (>= (length token) 2)
                    (char= (char token 0) #\-)
                    (char/= (char token 1) #\-))
               (multiple-value-bind (entries new-rest)
                   (%parse-flag-token token value-flags rest)
                 ;; ENTRIES is a list (clustered boolean flags expand to several);
                 ;; push each so the final NREVERSE restores declaration order.
                 (dolist (e entries) (push e flags))
                 (setf rest new-rest))
               (push token positionals))
        finally (return (values (nreverse flags) (nreverse positionals)))))

(defun %resolve-window-target (session target-str)
  "Resolve TARGET-STR to a window in SESSION.
   Supports special shorthands:
     :!  — last (previously active) window
     :+  — next window (wraps)
     :-  — previous window (wraps)
     :^  — first window
     :$  — last window
   Also accepts window-id (numeric) or window-name (string)."
  (let* ((wins (session-windows session))
         (act  (session-active-window session)))
    (cond
      ;; Special shorthands (with or without leading colon)
      ((member target-str '(":!" "!") :test #'string=)
       (session-last-window session))
      ((member target-str '(":+" "+") :test #'string=)
       (when wins
         (let ((idx (or (position act wins) 0)))
           (nth (mod (1+ idx) (length wins)) wins))))
      ((member target-str '(":-" "-") :test #'string=)
       (when wins
         (let ((idx (or (position act wins) 0)))
           (nth (mod (1- idx) (length wins)) wins))))
      ((member target-str '(":^" "^") :test #'string=)
       (first wins))
      ((member target-str '(":$" "$") :test #'string=)
       (car (last wins)))
      ;; Numeric window-id
      (t
       (let ((n (parse-integer target-str :junk-allowed t)))
         (if n
             (find n wins :key #'window-id)
             (find target-str wins :key #'window-name :test #'string-equal)))))))

(defun %cmd-select-window (session args)
  "select-window [-t target] [-l] [-n] [-p] [-T]: select a window.
   -t target: window-id, name, or special shorthand (:! last, :+ next, :- prev).
   -l: select the last (previously active) window (same as C-b l).
   -n: select the next window.
   -p: select the previous window.
   -T: toggle — when the target is ALREADY the current window, behave like
       last-window instead (the `bind Tab select-window -T` two-window toggle).
   Delivers ?1004 focus events on the switch."
  (multiple-value-bind (flags _pos) (%parse-command-flags args "t")
    (declare (ignore _pos))
    (cond
      ((assoc #\l flags)
       ;; -l: last window
       (let ((prev (session-last-window session)))
         (when prev
           (%with-window-focus-transition (session)
             (session-select-window session prev)))))
      ((assoc #\n flags)
       ;; -n: next window
       (%cmd-cycle-window session #'next-cyclic))
      ((assoc #\p flags)
       ;; -p: previous window
       (%cmd-cycle-window session #'prev-cyclic))
      (t
       ;; -t target or bare target
       (let ((target (cdr (assoc #\t flags))))
         (when target
           (%with-window-focus-transition (session)
             (let ((win (%resolve-window-target session target)))
               (when win
                 ;; -T toggle: already on the target → jump to last window instead.
                 (if (and (assoc #\T flags)
                          (eq win (session-active-window session))
                          (session-last-window session))
                     (session-select-window session (session-last-window session))
                     (session-select-window session win)))))))))
    ;; after-select-window: tmux's per-command hook (run-hooks now fires both the
    ;; add-hook and the .tmux.conf set-hook registries).
    (cl-tmux/hooks:run-hooks cl-tmux/hooks:+hook-after-select-window+ session)))

(defun %cmd-select-pane (session args)
  "select-pane [-L|-R|-U|-D|-l|-d|-e|-m|-M] [-t target] [-T title]: select or configure a pane.
   -L/-R/-U/-D: move in the given direction (relative to the active pane).
   -l: select the previously active (last) pane.
   -d/-e: disable / re-enable keyboard input to the TARGET pane.
   -T title: set the TARGET pane's title.
   -m: mark the TARGET pane; -M: clear the marked pane (unmark all).
   -t target: pane-id within the active window (default: the active pane).  The
     pane-configuring forms (-d/-e/-T/-m) and plain selection all act on -t's pane,
     not unconditionally the active one."
  (multiple-value-bind (flags _positionals) (%parse-command-flags args "tT")
    (declare (ignore _positionals))
    (let* ((win    (session-active-window session))
           ;; Resolve -t to a pane-id within the active window; default = active pane.
           (target-pane (%resolve-pane-in-window win (cdr (assoc #\t flags)))))
      (cond
        ((assoc #\L flags) (%select-pane-in-direction session :left))
        ((assoc #\R flags) (%select-pane-in-direction session :right))
        ((assoc #\U flags) (%select-pane-in-direction session :up))
        ((assoc #\D flags) (%select-pane-in-direction session :down))
        ;; -d/-e: disable / enable input to the target pane.
        ((assoc #\d flags) (when target-pane (setf (pane-input-disabled target-pane) t)))
        ((assoc #\e flags) (when target-pane (setf (pane-input-disabled target-pane) nil)))
        ;; -T title: set the target pane's title (and its screen title so
        ;; #{pane_title} reflects it).
        ((assoc #\T flags)
         (let ((title (cdr (assoc #\T flags))))
           (when (and target-pane title)
             (setf (pane-title target-pane) title)
             (let ((screen (pane-screen target-pane)))
               (when screen
                 (cl-tmux/terminal/actions:set-screen-title screen title))))))
        ;; -m: mark the target pane (unmark the others in its window first).
        ((assoc #\m flags)
         (when (and win target-pane)
           (dolist (p (window-panes win)) (setf (pane-marked p) nil))
           (setf (pane-marked target-pane) t)))
        ;; -M: clear the marked pane (unmark all panes in the active window).
        ((assoc #\M flags)
         (when win (dolist (p (window-panes win)) (setf (pane-marked p) nil))))
        ;; -l: select the previously active (last) pane in the active window.
        ((assoc #\l flags)
         (when win
           (let ((last (window-last-active win)))
             (when last (%select-pane-with-focus win last)))))
        ;; Default: select the target pane (no-op when it is already active).
        (t
         (when (and win target-pane (not (eq target-pane (window-active-pane win))))
           (%select-pane-with-focus win target-pane))))
      ;; after-select-pane fires once after the command (run-hooks now fires both
      ;; the add-hook and the .tmux.conf set-hook registries).
      (cl-tmux/hooks:run-hooks cl-tmux/hooks:+hook-after-select-pane+ session))))

