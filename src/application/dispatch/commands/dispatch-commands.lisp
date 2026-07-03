(in-package #:cl-tmux)

;;; -- Flag parser + shared resolver utilities ---------------------------------
;;;
;;; Defined first so all %cmd-* handlers below (and in the sibling
;;; dispatch-commands-*.lisp files) can use them without forward references.

(defmacro with-command-flags+pos ((flags pos args &optional (value-flags "")) &body body)
  "Bind FLAGS and POS to the parsed flag alist and positional tokens from ARGS."
  `(multiple-value-bind (,flags ,pos) (%parse-command-flags ,args ,value-flags)
     ,@body))

(defmacro with-command-input ((flags positionals args &optional (value-flags "") &rest options)
                              &body body)
  "Parse ARGS, validate FLAGS, and optionally constrain POSITIONALS before BODY."
  (let* ((allowed-flags-p (not (null (member :allowed-flags options))))
         (allowed-flags (when allowed-flags-p (getf options :allowed-flags)))
         (min-positionals-p (not (null (member :min-positionals options))))
         (min-positionals (when min-positionals-p (getf options :min-positionals)))
         (max-positionals-p (not (null (member :max-positionals options))))
         (max-positionals (when max-positionals-p (getf options :max-positionals)))
         (message (getf options :message "unsupported argument")))
    `(with-command-flags+pos (,flags ,positionals ,args ,value-flags)
       (if (%command-input-invalid-p ,flags ,positionals
                                     ,allowed-flags-p ,allowed-flags
                                     ,min-positionals-p ,min-positionals
                                     ,max-positionals-p ,max-positionals)
           (progn
             (show-overlay ,message)
             nil)
           (locally ,@body)))))

(defun %maybe-quote-form (form)
  "Return FORM unchanged when it is already quoted, otherwise quote it."
  (if (and (consp form) (eq (car form) 'quote))
      form
      (list 'quote form)))

(defun %command-input-invalid-p (flags positionals allowed-flags-p allowed-flags
                                 min-positionals-p min-positionals
                                 max-positionals-p max-positionals)
  "Return true when parsed command input violates shared flag or arity limits."
  (let ((positional-count (length positionals)))
    (or (and allowed-flags-p
             (find-if-not (lambda (flag)
                            (member (car flag) allowed-flags :test #'char=))
                          flags))
        (and min-positionals-p (< positional-count min-positionals))
        (and max-positionals-p (> positional-count max-positionals)))))

(defmacro define-command-input-handler (name (session args) docstring
                                        (flags positionals value-flags
                                         &rest options)
                                        &body body)
  "Define a %cmd-* handler with shared command-input plumbing."
  (let* ((allowed-flags-p (member :allowed-flags options))
         (allowed-flags (when allowed-flags-p (getf options :allowed-flags)))
         (min-positionals-p (member :min-positionals options))
         (min-positionals (when min-positionals-p (getf options :min-positionals)))
         (max-positionals-p (member :max-positionals options))
         (max-positionals (when max-positionals-p (getf options :max-positionals)))
         (message (getf options :message "unsupported argument")))
    `(defun ,name (,session ,args)
       ,docstring
       (with-command-input (,flags ,positionals ,args ,value-flags
                            :allowed-flags ,(%maybe-quote-form allowed-flags)
                            ,@(when min-positionals-p
                                `(:min-positionals ,min-positionals))
                            ,@(when max-positionals-p
                                `(:max-positionals ,max-positionals))
                            :message ,message)
         (declare (ignorable ,session ,flags ,positionals))
         ,@body))))

(defun %parse-flag-token (token value-flags remaining-tokens)
  "Parse one flag TOKEN into flag entries, supporting clustered boolean flags:
   -ga = -g -a, -gF = -g -F.  Returns (values FLAG-ENTRIES NEW-REMAINING)."
  (let ((entries nil)
        (len     (length token))
        (i       1))
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
              (return))
            (progn (push (cons ch t) entries)
                   (incf i)))))
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

(defun %command-prompt-ask-next (session template prompt-list answers idx num-prompts
                                 single-key initial)
  "Drive the sequential command-prompt -p flow."
  (labels ((finish ()
             (%run-command-line session
                                (%substitute-percent
                                 template
                                 (loop for i below num-prompts
                                       collect (aref answers i)))))
           (advance (i)
             (if (>= i num-prompts)
                 (finish)
                 (let ((label (nth i prompt-list))
                       (seed  (if (zerop i) initial "")))
                   (prompt-start label seed
                                 (lambda (input)
                                   (setf (aref answers i) input)
                                   (advance (1+ i)))
                                 :single-key single-key)))))
    (advance idx)))

(defun %parse-flag-int (flags char)
  "Return the integer value of flag CHAR in FLAGS, or NIL when the flag is absent.
   Uses parse-integer with :junk-allowed t so non-numeric values also return NIL."
  (let ((v (%flag-value flags char)))
    (and (stringp v) (%parse-integer-or-nil v))))

(defun %resolve-pane-in-window (win target-str)
  "Resolve TARGET-STR to a pane in WIN by pane-id; default to WIN's active pane.
   Accepts bare id (\"2\") and tmux %N sigil (\"%2\")."
  (or (and target-str win
           (let* ((digits (if (and (plusp (length target-str))
                                   (char= (char target-str 0) #\%))
                              (subseq target-str 1)
                              target-str))
                  (n (%parse-integer-or-nil digits)))
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
       (let ((n (%parse-integer-or-nil target-str)))
         (if n
             (find n wins :key #'window-id)
             (find target-str wins :key #'window-name :test #'string-equal)))))))

;;; -- Arg-aware command-line handlers -----------------------------------------
;;;
;;; Each %cmd-*-arg function handles one tmux command that takes arguments.
;;; Flag parsing uses with-command-flags+pos above.

(defun %resolve-client-target (target-str)
  "Resolve TARGET-STR to a client connection in *clients*.
   Accepts tmux-like names such as client-0 and client0, plus a bare numeric index."
  (when (and (stringp target-str) (plusp (length target-str)))
    (let* ((index-str (cond
                        ((and (>= (length target-str) 7)
                              (string-equal "client-" target-str :end2 7))
                         (subseq target-str 7))
                        ((and (>= (length target-str) 6)
                              (string-equal "client" target-str :end2 6))
                         (subseq target-str 6))
                        (t target-str)))
           (index (%parse-integer-or-nil index-str)))
      (and (integerp index)
           (>= index 0)
           (nth index *clients*)))))

(defun %format-message-log-overlay (&optional client-conn)
  "Return the show-messages overlay body for CLIENT-CONN, or the current client,
   or the global server message log when no client context is available."
  (let ((log (cond
               (client-conn (client-conn-message-log client-conn))
               (*current-client-conn* (client-conn-message-log *current-client-conn*))
               (t *message-log*))))
    (if log
        (%overlay-lines-string (mapcar #'cdr log))
        "(no messages)")))

(defun %cmd-display-message (session args)
  "display-message [-aClINpv] [-c client] [-d ms] [-F fmt] [-t target] <fmt...>:
   expand the space-joined ARGS (or -F fmt) as a format string against the target
   (or active) session/window/pane, then log and show the result.
   tmux args \"aCc:d:lINpt:F:v\".
   -l: literal — show ARGS verbatim WITHOUT expanding #{...} format variables.
   -d ms: display duration in milliseconds (overrides display-time option).
   -F fmt: use FMT as the format template instead of the positional ARGS.
   -t target: build the format context from the target's session/window/pane.
   -c client: target client (single-client standalone model: accepted).
   -a: list format variables; -C/-I/-N/-p/-v: client/print/verbose control flags —
       accepted; the standalone model shows the message in the overlay.
   Uses show-transient-overlay so it auto-dismisses after the configured duration."
  (with-command-input (flags positionals args "dtcF"
                             :allowed-flags '(#\a #\C #\c #\d #\l #\I #\N #\p #\t #\F #\v)
                             :message "display-message: unsupported argument")
    (let* ((delay-ms   (%parse-flag-int flags #\d))
           (target-str (%flag-value flags #\t)))
      (with-target-context (tgt-session tgt-win tgt-pane session target-str)
        (let* ((ctx       (cl-tmux/format:format-context-from-session tgt-session
                                                                     tgt-win
                                                                     tgt-pane))
               ;; -F fmt overrides the positional template.
               (raw       (or (%flag-value flags #\F)
                              (format nil "~{~A~^ ~}" positionals)))
               ;; -l: literal — emit ARGS unchanged, skipping #{...} expansion so a
               ;; message containing literal '#' / '#{' is shown as typed.
               (text      (if (%flag-present-p flags #\l)
                              raw
                              (cl-tmux/format:expand-format raw ctx))))
          (add-message-log text)
          (if delay-ms
              ;; Custom delay: temporarily override display-time for this message.
              (let ((saved (cl-tmux/options:get-option "display-time" 750)))
                (cl-tmux/options:set-option "display-time" delay-ms)
                (show-transient-overlay text)
                (cl-tmux/options:set-option "display-time" saved))
              (show-transient-overlay text)))))))

(defun %cmd-show-messages-arg (session args)
  "show-messages [-t target-client]: show server messages."
  (declare (ignore session))
  (with-command-input (flags positionals args "t"
                             :allowed-flags '(#\t)
                             :max-positionals 0
                             :message "show-messages: unsupported argument")
    (let* ((target-str (%flag-value flags #\t))
           (target-conn (and target-str (%resolve-client-target target-str))))
      (cond
        ((and target-str (null target-conn))
         (show-overlay (format nil "show-messages: no such client: ~A" target-str)))
        (t
         (show-overlay (%format-message-log-overlay target-conn)))))))

(defun %cmd-swap-pane-arg (session args)
  "swap-pane [-dUDLR] [-s src-pane] [-t dst-pane]: swap two panes.
   -s src / -t dst: swap those two panes (pane-ids in the active window; each
     defaults to the active pane), e.g. swap-pane -s 1 -t 3.
   -U/-D/-L/-R: swap the active pane with the adjacent pane in that direction.
   With neither -s/-t nor a direction: swap forward (same as C-b }).
   Unless -d is given, swap-pane makes the -t (destination) pane active, matching
   tmux (window_set_active_pane on dst_wp for a same-window swap).  In the
   directional/default paths the destination is the already-active pane, so it
   stays active either way and -d is a no-op there.
   -Z: keep the window zoomed if it was zoomed; without -Z, swapping in a
       zoomed window unzooms it first (tmux window_pop_zoom)."
  (with-command-input (flags positionals args "st"
                             :allowed-flags '(#\d #\U #\D #\L #\R #\s #\t #\Z)
                             :max-positionals 0
                             :message "swap-pane: unsupported argument")
    (with-active-window (win session)
      (%pane-navigation-unzoom win flags)
      (cond
        ((%flag-present-p flags #\U) (swap-pane win :up))
        ((%flag-present-p flags #\D) (swap-pane win :down))
        ((%flag-present-p flags #\L) (swap-pane win :left))
        ((%flag-present-p flags #\R) (swap-pane win :right))
        ;; -s/-t: swap two specific panes (each defaults to the active pane).
        ;; tmux activates the -t (dst) pane after a same-window swap unless -d.
        ((or (%flag-present-p flags #\s) (%flag-present-p flags #\t))
         (let ((dst (%resolve-pane-in-window win (%flag-value flags #\t))))
           (when (swap-two-panes win
                                 (%resolve-pane-in-window win (%flag-value flags #\s))
                                 dst)
             (unless (%flag-present-p flags #\d)
               (%select-pane-with-focus win dst)))))
        ;; No direction, no -s/-t: swap forward (default tmux behaviour).
        (t (swap-pane win :right))))))

(defun %cmd-command-prompt-arg (session args)
  "command-prompt [-p prompts] [template]: open a command prompt with optional args.
   -p prompts: comma-separated list of prompt labels; each label becomes a
     separate sequential prompt.  On completion, each response replaces %%1, %%2,
     etc. in TEMPLATE and the expanded command is executed.
   Without -p: single prompt ':' that runs the typed command line (same as C-b :).
   Without TEMPLATE: input is executed directly as a command line.
   -I initial: seed the prompt with INITIAL text before editing begins.
   -1: single-key prompt — each prompt accepts ONE keypress (no Enter).
   -k: key prompt — like -1, the prompt accepts a single keypress.
   -T type / -t target-client: accepted (their arguments are consumed so they do
       not leak into the template); -N/-F/-b/-e/-l are accepted as no-ops in the
       standalone prompt model.  tmux args \"1beFiklI:Np:t:T:\".
   -i: incremental — the template runs on EVERY prompt edit with %1/%% bound to
       the current input (tmux PROMPT_INCREMENTAL, used for live-search
       bindings), in addition to the final run on Enter."
  (with-command-flags+pos (flags positionals args "IptT")
    (let* ((prompts-str (%flag-value flags #\p))
           (initial     (or (%flag-value flags #\I) ""))
           ;; -1 and -k both request a one-keypress prompt.
           (single-key  (and (or (%flag-present-p flags #\1)
                                 (%flag-present-p flags #\k)) t))
           (template    (format nil "~{~A~^ ~}" positionals))
           (has-template (plusp (length template)))
           (prompt-list (when prompts-str
                          (mapcar (lambda (s) (string-trim " " s))
                                  (uiop:split-string prompts-str :separator ","))))
           (num-prompts (length prompt-list)))
      (flet ((run-input (input)
               (%run-command-line session input))
             (run-template (input)
               (%run-command-line session
                                  (%substitute-prompt-response template input))))
        (cond
          ;; -p with template: multi-prompt with %%N substitution
          ((and prompt-list has-template)
           (let ((answers (make-array num-prompts :initial-element "")))
             (%command-prompt-ask-next session template prompt-list answers 0
                                       num-prompts single-key initial)))
          ;; -p without template: each prompt result is concatenated
          (prompt-list
           (let ((label (first prompt-list)))
             (prompt-history-nonempty (or label ": ")
                                      #'run-input
                                      :single-key single-key
                                      :history *prompt-history*
                                      :initial initial)))
          ;; Template without -p: the response replaces %% / %1 in the template
          ;; (the classic `bind , command-prompt -I \"#W\" \"rename-window '%%'\"`
          ;; shape; previously the template was ignored and raw input ran).
          (has-template
           (prompt-history-nonempty ": "
                                    #'run-template
                                    :single-key single-key
                                    :history *prompt-history*
                                    :initial initial))
          ;; No -p, no template: standard C-b : interactive prompt
          (t
           (prompt-history-nonempty ": "
                                    #'run-input
                                    :single-key single-key
                                    :history *prompt-history*
                                    :initial initial)))
        ;; -i: run the template against the in-progress input on every edit.
        (when (and (%flag-present-p flags #\i)
                   has-template
                   cl-tmux/prompt:*prompt*)
          (setf (cl-tmux/prompt:prompt-on-change cl-tmux/prompt:*prompt*)
                #'run-template))))))

(defun %substitute-prompt-response (template input)
  "Expand a single-prompt command-prompt TEMPLATE: tmux replaces both %% and %1
   with the prompt response for a single prompt.  Rewrites each %% pair to %1
   left-to-right, then delegates to %substitute-percent with INPUT as arg 1."
  (let ((rewritten (with-output-to-string (out)
                     (let ((n (length template)) (i 0))
                       (loop while (< i n)
                             do (if (and (char= (char template i) #\%)
                                         (< (1+ i) n)
                                         (char= (char template (1+ i)) #\%))
                                    (progn (write-string "%1" out) (incf i 2))
                                    (progn (write-char (char template i) out)
                                           (incf i))))))))
    (%substitute-percent rewritten (list input))))

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
  "last-pane [-deZ] [-t target-window]: jump to the previously active pane.
   tmux args \"det:Z\".
   -d: disable keyboard input to the pane jumped to (PANE_INPUTOFF).
   -e: re-enable keyboard input to the pane jumped to.
   -Z: keep the window zoomed if it was zoomed; without -Z, jumping to the
       last pane in a zoomed window unzooms it first (tmux window_pop_zoom).
   -t target-window: the window whose last pane to select (default: active)."
  (with-command-input (flags positionals args "t"
                             :allowed-flags '(#\d #\e #\Z #\t)
                             :max-positionals 0
                             :message "last-pane: unsupported argument")
    (let* ((target (%flag-value flags #\t))
           (win    (if target
                       (%resolve-window-target session target)
                       (session-active-window session)))
           (last   (and win (window-last-active win))))
      (when (and win last)
        (%pane-navigation-unzoom win flags)
        (%select-pane-with-focus win last)
        ;; -d/-e: toggle input to the pane we just selected.
        (cond
          ((%flag-present-p flags #\d) (%select-pane-disable-input last t))
          ((%flag-present-p flags #\e) (%select-pane-disable-input last nil)))))))

(defun %cmd-has-session-arg (session args)
  "has-session [-t name]: check if a named session exists.
   Shows a transient overlay: 'has-session: yes' or 'has-session: no'.
   Without -t: checks if there is any session in *server-sessions*."
  (declare (ignore session))
  (with-command-input (flags positionals args "t"
                             :allowed-flags '(#\t)
                             :max-positionals 0
                             :message "has-session: unsupported argument")
    (let* ((target-name (%flag-value flags #\t))
           (found       (if target-name
                            (server-find-session target-name)
                            (not (null *server-sessions*)))))
      (show-transient-overlay
       (if found
           (format nil "has-session ~A: yes" (or target-name ""))
           (format nil "has-session ~A: no"  (or target-name "")))))))
