(in-package #:cl-tmux)

;;; -- Capture-pane command ----------------------------------------------------

(defun %capture-pane-options-from-flags (flags)
  "Decode capture-pane flags into a plist used by the command handler."
  (list :print-p (%flag-present-p flags #\p)
        :include-scrollback (%flag-present-p flags #\S)
        :escapes (%flag-present-p flags #\e)
        :join (%flag-present-p flags #\J)
        :preserve (%flag-present-p flags #\N)
        :start (%capture-pane-parse-range-value (%flag-value flags #\S))
        :end   (%capture-pane-parse-range-value (%flag-value flags #\E))
        :target-str (%flag-value flags #\t)
        :buffer-name (%flag-value flags #\b)))

(defun %capture-pane-parse-range-value (raw)
  "Parse a capture-pane -S/-E value: NIL when absent, :edge for \"-\" (start of
   history / end of visible), or an integer line number (negative reaches into
   the scrollback)."
  (cond ((null raw) nil)
        ((string= raw "-") :edge)
        (t (%parse-integer-or-nil raw))))

(defun %capture-pane-slice-range (content height start end)
  "Slice CONTENT (captured text, one row per line, scrollback rows first) to
   the tmux -S/-E line range.  Line 0 is the first VISIBLE row; negative lines
   reach into the scrollback; :edge start = the beginning of history; :edge or
   absent end = the visible bottom."
  (if (and (null start) (null end))
      content
      (let* ((lines (uiop:split-string content :separator '(#\Newline)))
             ;; The final row terpri leaves one trailing empty pseudo-line.
             (lines (if (and lines (string= (first (last lines)) ""))
                        (butlast lines)
                        lines))
             (total (length lines))
             (vis0  (max 0 (- total height)))
             (from  (cond ((eq start :edge) 0)
                          ((integerp start) (max 0 (+ vis0 start)))
                          (t vis0)))
             (to    (cond ((or (null end) (eq end :edge)) total)
                          ((integerp end) (min total (+ vis0 end 1)))
                          (t total))))
        (if (< from to)
            (format nil "~{~A~%~}" (subseq lines from to))
            ""))))

(defun %capture-pane-deliver-content (content print-p buffer-name)
  "Send captured CONTENT to the overlay or a paste buffer."
  (if print-p
      ;; -p: stdout equivalent; show the content in an overlay.
      (show-overlay content)
      ;; Default: save to a paste buffer (silent), like tmux. -b names it.
      (cl-tmux/buffer:add-paste-buffer content buffer-name)))

(defun %cmd-capture-pane-arg (session args)
  "capture-pane [-p] [-S start] [-E end] [-b buffer] [-JeN] [-t target]: capture
   the pane's content.
   Default (no -p): SAVE the captured text to a paste buffer (retrievable with
     paste-buffer) — tmux's default behaviour, and the canonical capture→paste
     workflow.  Silent (no overlay).
   -p: print to stdout (shown as an overlay in standalone mode) instead of saving.
   -S start / -E end: the captured line range.  Line 0 is the first visible
     row; negative numbers reach into the scrollback; '-' means the start of
     history (-S) — the end defaults to the visible bottom (-E omitted/'-').
   -b name: store the capture in the buffer named NAME (retrievable with
     paste-buffer -b NAME); without -b an automatic name is assigned.
   -e: include SGR escape sequences so captured colours/attributes are preserved.
   -J: preserve trailing spaces AND rejoin lines that wrapped at the right margin
     into one logical line (default strips trailing spaces and keeps every row a
     separate line, like tmux).  Joining uses the screen's per-row wrap flags and
     applies to the visible region (scrollback rows are not joined).
  -N: preserve trailing spaces WITHOUT joining wrapped lines (the difference from -J).
  -t target: target pane by id (for example %2) or session:window.pane.
   -a: capture the alternate screen.  In this model the alternate IS the live
     grid while a full-screen app has it active (the saved cells hold the
     primary), so -a captures normally then; when no alternate screen is in
     use it reports tmux's \"no alternate screen\" error.
   -C: escape non-printable characters as octal \\nnn (accepted).
   -M/-P/-q/-T: hyperlink/pending/quiet/trailing-cell control flags (accepted;
     tmux args \"ab:CeE:JMNpPqS:Tt:\")."
  (with-command-input (flags positionals args "tSEb"
                             :max-positionals 0
                             :allowed-flags '(#\p #\S #\E #\b #\e #\J #\N #\t
                                              #\a #\C #\M #\P #\q #\T)
                             :message "capture-pane: unsupported argument")
    (let* ((options (%capture-pane-options-from-flags flags))
           (print-p (getf options :print-p))
           (include-scrollback (getf options :include-scrollback))
           (escapes (getf options :escapes))
           (join (getf options :join))
           (preserve (getf options :preserve))
           (target-str (getf options :target-str)))
      (with-target-context (target-session target-window pane session target-str)
        (declare (ignore target-session target-window))
        ;; -a: valid only while the pane's alternate screen is in use (the
        ;; alternate is then the live grid, so the normal capture reads it).
        ;; tmux -q suppresses the error and falls back to the normal screen.
        (when (and (%flag-present-p flags #\a)
                   (not (%flag-present-p flags #\q))
                   pane
                   (null (cl-tmux/terminal:screen-alt-cells (pane-screen pane))))
          (show-overlay "capture-pane: no alternate screen")
          (return-from %cmd-capture-pane-arg nil))
        (let ((content (and pane (capture-pane pane
                                               :include-scrollback (and include-scrollback t)
                                               :escapes (and escapes t)
                                               :join (and join t)
                                               :preserve-trailing (and preserve t)))))
          (when content
            ;; -S/-E line range: slice the assembled capture (scrollback rows
            ;; precede the visible region; line 0 = first visible row).
            (let ((start (getf options :start))
                  (end   (getf options :end)))
              (when (or start end)
                (setf content
                      (%capture-pane-slice-range
                       content
                       (cl-tmux/terminal/types:screen-height (pane-screen pane))
                       start end))))
            (%capture-pane-deliver-content content print-p
                                           (getf options :buffer-name))))))))
