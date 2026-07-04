(in-package #:cl-tmux)

;;; Arg-aware display, message, prompt, and pane-selection command handlers.

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
  "display-message [-l] [-d ms] [-F fmt] [-t target] <fmt...>:
   expand the space-joined ARGS (or -F fmt) as a format string against the target
   (or active) session/window/pane, then log and show the result.
   -l: literal — show ARGS verbatim WITHOUT expanding #{...} format variables.
   -d ms: display duration in milliseconds (overrides display-time option).
   -F fmt: use FMT as the format template instead of the positional ARGS.
   -t target: build the format context from the target's session/window/pane.
   Uses show-transient-overlay so it auto-dismisses after the configured duration."
  (with-command-input (flags positionals args "dtF"
                             :allowed-flags '(#\d #\l #\t #\F)
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
  "swap-pane [-UDLR] [-s src-pane] [-t dst-pane]: swap two panes.
   -s src / -t dst: swap those two panes (pane-ids in the active window; each
     defaults to the active pane), e.g. swap-pane -s 1 -t 3.
   -U/-D/-L/-R: swap the active pane with the adjacent pane in that direction.
   With neither -s/-t nor a direction: swap forward (same as C-b }).
   For -s/-t, swap-pane always makes the -t (destination) pane active."
  (with-command-input (flags positionals args "st"
                             :allowed-flags '(#\U #\D #\L #\R #\s #\t)
                             :max-positionals 0
                             :message "swap-pane: unsupported argument")
    (with-active-window (win session)
      (%pane-navigation-unzoom win)
      (cond
        ((%flag-present-p flags #\U) (swap-pane win :up))
        ((%flag-present-p flags #\D) (swap-pane win :down))
        ((%flag-present-p flags #\L) (swap-pane win :left))
        ((%flag-present-p flags #\R) (swap-pane win :right))
        ;; -s/-t: swap two specific panes (each defaults to the active pane).
        ((or (%flag-present-p flags #\s) (%flag-present-p flags #\t))
         (let ((dst (%resolve-pane-in-window win (%flag-value flags #\t))))
            (when (swap-two-panes win
                                  (%resolve-pane-in-window win (%flag-value flags #\s))
                                  dst)
              (%select-pane-with-focus win dst))))
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
       not leak into the template).  tmux args \"1beFiklI:Np:t:T:\".
   -N: the prompt accepts numeric key presses only.
   -F: the substituted command line is expanded as a format string before it
       runs.
   -b: the prompt runs in the background without blocking — cl-tmux prompts
       are ALWAYS non-blocking overlays, so this is the native behaviour.
   -e: the prompt closes automatically when the client loses focus (wired to
       the ?1004 focus-out report).
   -l: like -1, the first key press is the answer; cl-tmux's -1 already
       submits the untranslated character, which is exactly -l's semantics.
   -i: incremental — the template runs on EVERY prompt edit with %1/%% bound to
       the current input (tmux PROMPT_INCREMENTAL, used for live-search
       bindings), in addition to the final run on Enter."
  (with-command-flags+pos (flags positionals args "IptT")
    (let* ((prompts-str (%flag-value flags #\p))
           (initial     (or (%flag-value flags #\I) ""))
           ;; -1, -k, and -l all request a one-keypress prompt (-k translates
           ;; to a key name in tmux; -l is explicitly untranslated — cl-tmux
           ;; submits the raw character for all three).
           (single-key  (and (or (%flag-present-p flags #\1)
                                 (%flag-present-p flags #\k)
                                 (%flag-present-p flags #\l)) t))
           (template    (format nil "~{~A~^ ~}" positionals))
           (has-template (plusp (length template)))
           (prompt-list (when prompts-str
                          (mapcar (lambda (s) (string-trim " " s))
                                  (uiop:split-string prompts-str :separator ","))))
           (num-prompts (length prompt-list)))
      (flet ((run-input (input)
               (%run-command-line session input))
             (run-template (input)
               (let ((cmd (%substitute-prompt-response template input)))
                 ;; -F: the substituted command line is expanded as a format
                 ;; string before running (tmux command-prompt -F).
                 (when (%flag-present-p flags #\F)
                   (setf cmd (or (ignore-errors
                                   (cl-tmux/format:expand-format
                                    cmd
                                    (cl-tmux/format:format-context-from-session
                                     session
                                     (session-active-window session)
                                     (session-active-pane session))))
                                 cmd)))
                 (%run-command-line session cmd))))
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
                #'run-template))
        ;; -N: the prompt accepts numeric key presses only.
        (when (and (%flag-present-p flags #\N)
                   cl-tmux/prompt:*prompt*)
          (setf (cl-tmux/prompt:prompt-numeric-only cl-tmux/prompt:*prompt*)
                t))
        ;; -e: close the prompt when the client loses focus (?1004 focus-out).
        (when (and (%flag-present-p flags #\e)
                   cl-tmux/prompt:*prompt*)
          (setf (cl-tmux/prompt:prompt-close-on-focus-out
                 cl-tmux/prompt:*prompt*)
                t))))))

(defun %cmd-last-pane-arg (session args)
  "last-pane [-de] [-t target-window]: jump to the previously active pane.
   -d: disable keyboard input to the pane jumped to (PANE_INPUTOFF).
   -e: re-enable keyboard input to the pane jumped to.
   -t target-window: the window whose last pane to select (default: active)."
  (with-command-input (flags positionals args "t"
                             :allowed-flags '(#\d #\e #\t)
                             :max-positionals 0
                             :message "last-pane: unsupported argument")
    (let* ((target (%flag-value flags #\t))
           (win    (if target
                       (%resolve-window-target session target)
                       (session-active-window session)))
           (last   (and win (window-last-active win))))
      (when (and win last)
        (%pane-navigation-unzoom win)
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
