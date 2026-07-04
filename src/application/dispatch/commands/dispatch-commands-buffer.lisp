(in-package #:cl-tmux)

;;; -- Named paste-buffer %cmd-* handlers --------------------------------------
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
  "set-buffer [-aw] [-b name] [-n new-name] data...:
   set a paste buffer's contents.  -b name stores DATA under NAME; without -b
   an automatic name (bufferN) is assigned.  -n new-name renames the selected
   buffer (or the most recent one) to NEW-NAME and ignores DATA.  -w sends the
   buffer to the host clipboard via OSC 52 (honouring set-clipboard)."
  (with-command-input (flags positionals args "bn"
                             :allowed-flags '(#\a #\b #\n #\w)
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
   used."
  (declare (ignore session))
  (with-command-input (flags positionals args "b"
                             :allowed-flags '(#\b)
                             :max-positionals 1
                             :message "load-buffer: unsupported argument")
    (let ((name (%buffer-name-from-flags flags))
          (path (first positionals)))
      (when path
        (cl-tmux/buffer:add-paste-buffer (%buffer-read-file path) name)))))
