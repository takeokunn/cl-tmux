(in-package #:cl-tmux)

;;; -- Flag parser + shared resolver utilities ---------------------------------
;;;
;;; Defined first so all %cmd-* handlers below (and in the sibling
;;; dispatch-commands-*.lisp files) can use them without forward references.

(defmacro with-command-flags ((flags args &optional (value-flags "")) &body body)
  "Bind FLAGS to the parsed flag alist from ARGS, discarding positionals."
  (let ((pos (gensym "POS")))
    `(multiple-value-bind (,flags ,pos) (%parse-command-flags ,args ,value-flags)
       (declare (ignore ,pos))
       ,@body)))

(defmacro with-command-flags+pos ((flags pos args &optional (value-flags "")) &body body)
  "Bind FLAGS and POS to the parsed flag alist and positional tokens from ARGS."
  `(multiple-value-bind (,flags ,pos) (%parse-command-flags ,args ,value-flags)
     ,@body))

(defun %parse-flag-token (token value-flags remaining-tokens)
  "Parse one flag TOKEN into flag entries, supporting clustered boolean flags:
   -ga = -g -a, -gF = -g -F.  Returns (values FLAG-ENTRIES NEW-REMAINING)."
  (let ((entries nil)
        (len     (length token))
        (i       1))
    (block scan
      (loop while (< i len) do
        (let ((ch (char token i)))
          (if (find ch value-flags)
              (let ((attached (when (< (1+ i) len) (subseq token (1+ i)))))
                (if attached
                    (push (cons ch attached) entries)
                    (progn
                      (push (cons ch (if remaining-tokens (first remaining-tokens) ""))
                            entries)
                      (setf remaining-tokens (rest remaining-tokens))))
                (return-from scan))
              (progn (push (cons ch t) entries)
                     (incf i))))))
    (values (nreverse entries) remaining-tokens)))

(defun %parse-command-flags (tokens &optional (value-flags ""))
  "Split TOKENS into (values FLAGS POSITIONALS).  -X flags are parsed; those
   whose char is in VALUE-FLAGS consume the next token as their value."
  (loop with flags = nil and positionals = nil and rest = tokens
        while rest
        for token = (first rest)
        do (setf rest (rest rest))
           (if (and (>= (length token) 2)
                    (char= (char token 0) #\-)
                    (char/= (char token 1) #\-))
               (multiple-value-bind (entries new-rest)
                   (%parse-flag-token token value-flags rest)
                 (dolist (e entries) (push e flags))
                 (setf rest new-rest))
               (push token positionals))
        finally (return (values (nreverse flags) (nreverse positionals)))))

(defun %resolve-pane-in-window (win target-str)
  "Resolve TARGET-STR to a pane in WIN by pane-id; default to WIN's active pane.
   Accepts bare id (\"2\") and tmux %N sigil (\"%2\")."
  (or (and target-str win
           (let* ((digits (if (and (plusp (length target-str))
                                   (char= (char target-str 0) #\%))
                              (subseq target-str 1)
                              target-str))
                  (n (parse-integer digits :junk-allowed t)))
             (and n (find n (window-panes win) :key #'pane-id))))
      (and win (window-active-pane win))))

(defun %resolve-window-target (session target-str)
  "Resolve TARGET-STR to a window in SESSION.
   Shorthands: :! last, :+ next, :- prev, :^ first, :$ last."
  (let* ((wins (session-windows session))
         (act  (session-active-window session)))
    (cond
      ((member target-str '(":!" "!") :test #'string=)
       (session-last-window session))
      ((member target-str '(":+" "+") :test #'string=)
       (when wins
         (nth (mod (1+ (or (position act wins) 0)) (length wins)) wins)))
      ((member target-str '(":-" "-") :test #'string=)
       (when wins
         (nth (mod (1- (or (position act wins) 0)) (length wins)) wins)))
      ((member target-str '(":^" "^") :test #'string=) (first wins))
      ((member target-str '(":$" "$") :test #'string=) (car (last wins)))
      (t
       (let ((n (parse-integer target-str :junk-allowed t)))
         (if n
             (find n wins :key #'window-id)
             (find target-str wins :key #'window-name :test #'string-equal)))))))

;;; -- Arg-aware command-line handlers -----------------------------------------
;;;
;;; Each %cmd-*-arg function handles one tmux command that takes arguments.
;;; Flag parsing uses with-command-flags / with-command-flags+pos above.

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
  (with-command-flags+pos (flags positionals args "dtc")
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

(defun %cmd-swap-pane-arg (session args)
  "swap-pane [-dUDLRZ] [-s src-pane] [-t dst-pane]: swap two panes.
   -s src / -t dst: swap those two panes (pane-ids in the active window; each
     defaults to the active pane), e.g. swap-pane -s 1 -t 3.
   -U/-D/-L/-R: swap the active pane with the adjacent pane in that direction.
   -d (keep active) and -Z (keep zoom) are accepted.
   With neither -s/-t nor a direction: swap forward (same as C-b })."
  (with-command-flags (flags args "st")
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
  (with-command-flags+pos (flags positionals args "p")
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
  (with-command-flags (flags args "")
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
  (with-command-flags (flags args "t")
    (let* ((target-name (cdr (assoc #\t flags)))
           (found       (if target-name
                            (server-find-session target-name)
                            (not (null *server-sessions*)))))
      (show-transient-overlay
       (if found
           (format nil "has-session ~A: yes" (or target-name ""))
           (format nil "has-session ~A: no"  (or target-name "")))))))

