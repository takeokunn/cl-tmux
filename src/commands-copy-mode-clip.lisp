(in-package #:cl-tmux/commands)

;;;; Rectangle selection text, copy-pipe helpers, yank, append-selection.

;;; ── Rectangle selection text ────────────────────────────────────────────────
;;;
;;; When rectangle select is active (screen-copy-rect-select-p), each row in
;;; the selection range is read between the same left and right column bounds
;;; (the canonical column range derived from mark and cursor column positions).
;;; Rows are joined with newlines; trailing spaces within each row are trimmed.

(defun %rectangle-selection-text (screen)
  "Compute the rectangle-selected text for SCREEN.
   Returns a string, or NIL when no valid selection exists.
   In rectangle mode each row between start-row and end-row is extracted
   between fixed column bounds (min/max of mark-col and cursor-col)."
  (unless (and (screen-copy-selecting screen)
               (screen-copy-mark   screen)
               (screen-copy-cursor screen))
    (return-from %rectangle-selection-text nil))
  (multiple-value-bind (start-vrow end-vrow start-col end-col)
      (%selection-bounds screen)
    (let* ((text (with-output-to-string (out)
                   (loop for vrow from start-vrow to end-vrow do
                     (let* ((row-str (%extract-vrow-chars screen vrow start-col end-col))
                            (trimmed (string-right-trim " " row-str)))
                       (write-string trimmed out)
                       (when (< vrow end-vrow)
                         (write-char #\Newline out)))))))
      (if (plusp (length text)) text nil))))

;;; ── copy-pipe helper ─────────────────────────────────────────────────────────
;;;
;;; When the "copy-command" option is set to a non-empty string, the yank text
;;; is also piped to that shell command via uiop:run-program.  Errors are
;;; silently swallowed so a misconfigured copy-command does not crash the session.

(defconstant +copy-command-timeout+ 30
  "Maximum seconds to wait for a copy-command subprocess before giving up.")

(defun %run-shell-cmd-with-input (command text)
  "Pipe TEXT as stdin to COMMAND (a shell string), bounded by +copy-command-timeout+.
   Errors are silently swallowed so a misconfigured command does not crash the session."
  (ignore-errors
    (bt:with-timeout (+copy-command-timeout+)
      (uiop:run-program (list "/bin/sh" "-c" command)
                        :input (make-string-input-stream text)
                        :ignore-error-status t))))

(defun %run-copy-command (text)
  "Pipe TEXT to the shell command stored in the \"copy-command\" option.
   No-op when the option is empty or TEXT is NIL/empty.
   The subprocess is bounded by +copy-command-timeout+ seconds so a hanging
   copy-command does not block the event loop indefinitely."
  (when (and text (plusp (length text)))
    (let ((cmd (ignore-errors (cl-tmux/options:get-option "copy-command"))))
      (when (and (stringp cmd) (plusp (length cmd)))
        (%run-shell-cmd-with-input cmd text)))))

(defun %maybe-copy-to-clipboard (screen text)
  "When the set-clipboard option is on/external, enqueue an OSC 52 sequence on
   SCREEN's clipboard-queue so the renderer copies TEXT to the host's system
   clipboard on the next frame.  No-op when set-clipboard is off."
  (let ((mode (or (ignore-errors (cl-tmux/options:get-option "set-clipboard")) "on")))
    (when (member mode '("on" "external") :test #'equal)
      (push (cl-tmux/terminal/parser:osc52-clipboard-sequence text)
            (screen-clipboard-queue screen)))))

(defun copy-mode-yank (screen)
  "Copy selected text to paste buffer (and pipe via copy-command if configured),
   then exit copy mode.  In rectangle-select mode the rectangular region is used.
   When set-clipboard is on/external, also emits OSC 52 to the host terminal so
   the selection reaches the system clipboard."
  (let ((text (if (screen-copy-rect-select-p screen)
                  (%rectangle-selection-text screen)
                  (%selection-text screen))))
    (when (and text (plusp (length text)))
      (cl-tmux/buffer:add-paste-buffer text)
      (%maybe-copy-to-clipboard screen text)
      (%run-copy-command text)))
  (copy-mode-cancel-selection screen)
  (copy-mode-exit screen))

;;; ── Rectangle-select toggle ─────────────────────────────────────────────────

(defun copy-mode-toggle-rectangle (screen)
  "Toggle rectangle-select mode for SCREEN.
   When toggled on, yank uses the rectangular region instead of stream selection.
   Marks the screen dirty."
  (when (screen-copy-mode-p screen)
    (setf (screen-copy-rect-select-p screen)
          (not (screen-copy-rect-select-p screen))
          (screen-dirty-p screen) t)))

;;; ── Append selection ────────────────────────────────────────────────────────
;;;
;;; append-selection appends the current selection to the *most recent* paste
;;; buffer entry (if one exists) instead of pushing a new entry.  If the paste
;;; buffer is empty, it behaves like a normal yank.

(defun copy-mode-append-selection (screen)
  "Append selected text to the most recent paste buffer entry; stay in copy mode.
   This matches tmux's `append-selection` send-keys -X command: the selection
   remains highlighted so the user can chain further append operations.
   If the paste buffer is empty the selection is pushed as a new entry.
   Rectangle-select mode is honoured."
  (when (screen-copy-mode-p screen)
    (let ((text (if (screen-copy-rect-select-p screen)
                    (%rectangle-selection-text screen)
                    (%selection-text screen))))
      (when (and text (plusp (length text)))
        (let ((existing (cl-tmux/buffer:get-paste-buffer 0)))
          (if existing
              (progn
                (cl-tmux/buffer:delete-paste-buffer 0)
                (cl-tmux/buffer:add-paste-buffer (concatenate 'string existing text)))
              (cl-tmux/buffer:add-paste-buffer text)))
        (%run-copy-command text)))
    ;; Do NOT exit copy mode — tmux append-selection keeps the user in copy mode.
    (setf (screen-dirty-p screen) t)))

(defun copy-mode-append-selection-and-cancel (screen)
  "Append selected text to the paste buffer, cancel selection, and exit copy mode.
   Equivalent to tmux's `append-selection-and-cancel` send-keys -X command."
  (copy-mode-append-selection screen)
  (copy-mode-cancel-selection screen)
  (copy-mode-exit screen))

;;; ── copy-pipe (yank + pipe) ─────────────────────────────────────────────────
;;;
;;; copy-mode-copy-pipe is the direct implementation of tmux's copy-pipe-and-cancel:
;;; it places the selection text into the paste buffer AND pipes it to CMD.
;;; CMD overrides the "copy-command" option for this single invocation.

(defun %resolve-copy-pipe-cmd (cmd)
  "Return the effective shell command string for copy-pipe.
   If CMD is a non-empty string, use it directly.
   Otherwise fall back to the \"copy-command\" global option.
   Returns NIL when neither source yields a usable command."
  (if (and (stringp cmd) (plusp (length cmd)))
      cmd
      (let ((option-cmd (ignore-errors (cl-tmux/options:get-option "copy-command"))))
        (when (and (stringp option-cmd) (plusp (length option-cmd)))
          option-cmd))))

(defun %do-copy-pipe (screen cmd)
  "Copy selection to paste buffer and pipe to CMD (or copy-command option if NIL).
   Shared logic for copy-pipe variants; does NOT exit copy mode."
  (when (screen-copy-mode-p screen)
    (let ((text (if (screen-copy-rect-select-p screen)
                    (%rectangle-selection-text screen)
                    (%selection-text screen))))
      (when (and text (plusp (length text)))
        (cl-tmux/buffer:add-paste-buffer text)
        (let ((effective-cmd (%resolve-copy-pipe-cmd cmd)))
          (when effective-cmd
            (%run-shell-cmd-with-input effective-cmd text)))))))

(defun copy-mode-copy-pipe (screen cmd)
  "Yank selected text to the paste buffer and pipe it to CMD (a shell string).
   If CMD is empty or NIL the global \"copy-command\" option is used.
   Exits copy mode after yanking (copy-pipe-and-cancel semantics)."
  (%do-copy-pipe screen cmd)
  (when (screen-copy-mode-p screen)
    (copy-mode-cancel-selection screen)
    (copy-mode-exit screen)))

(defun copy-mode-copy-pipe-no-cancel (screen cmd)
  "Yank selected text to the paste buffer and pipe it to CMD (a shell string).
   If CMD is empty or NIL the global \"copy-command\" option is used.
   Stays in copy mode after yanking (copy-pipe semantics, no cancel)."
  (%do-copy-pipe screen cmd)
  (when (screen-copy-mode-p screen)
    (setf (screen-dirty-p screen) t)))

;;; Navigation (word/line/screen jumps) and search are in separate files:
;;;   commands-copy-mode-nav.lisp    — word-forward/backward/end, line-start/end,
;;;                                    cursor-jump macros, page/half-page scroll,
;;;                                    begin-line-selection, copy-end-of-line, copy-line
;;;   commands-copy-mode-search.lisp — %copy-mode-row-string, find-forward/backward,
;;;                                    search-forward/backward, search-next/prev
