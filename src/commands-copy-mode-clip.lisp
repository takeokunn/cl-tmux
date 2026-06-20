(in-package #:cl-tmux/commands)

;;;; Rectangle selection text, copy-pipe helpers, yank, append-selection.
;;;; Uses selection helpers from commands-copy-mode-selection.lisp and search
;;;; helpers from commands-copy-mode-search.lisp.

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
  (when (and (screen-copy-selecting screen)
             (screen-copy-mark   screen)
             (screen-copy-cursor screen))
    (multiple-value-bind (start-vrow end-vrow start-col end-col)
        (%selection-bounds screen)
      (let* ((text (with-output-to-string (out)
                     (loop for vrow from start-vrow to end-vrow do
                       (let* ((row-str (%extract-vrow-chars screen vrow start-col end-col))
                              (trimmed (string-right-trim " " row-str)))
                         (write-string trimmed out)
                         (when (< vrow end-vrow)
                           (write-char #\Newline out)))))))
        (and (plusp (length text)) text)))))

;;; ── Selection-text dispatch helper ──────────────────────────────────────────

(defun %get-selection-text (screen)
  "Return the selected text for SCREEN, respecting rectangle-select mode.
   Delegates to %rectangle-selection-text when rect-select is active, else
   %selection-text."
  (if (screen-copy-rect-select-p screen)
      (%rectangle-selection-text screen)
      (%selection-text screen)))

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
                        :ignore-error-status t
                        :timeout +copy-command-timeout+))))

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

(defun %copy-mode-do-yank (screen)
  "Shared copy work for the yank/copy-selection family: place the current
   selection text into the paste buffer, emit OSC 52 when set-clipboard is
   on/external, and pipe via the copy-command option.  Does NOT touch the
   selection or copy-mode state.  No-op when there is no selection text."
  (let ((text (%get-selection-text screen)))
    (when (and text (plusp (length text)))
      (cl-tmux/buffer:add-paste-buffer text)
      (%maybe-copy-to-clipboard screen text)
      (%run-copy-command text))))

(defun copy-mode-yank (screen)
  "Copy selected text to paste buffer (and pipe via copy-command if configured),
   then exit copy mode.  In rectangle-select mode the rectangular region is used.
   When set-clipboard is on/external, also emits OSC 52 to the host terminal so
   the selection reaches the system clipboard.  This is the exit-on-yank path
   bound to vi y / Enter / emacs M-w / mouse-drag-release."
  (%copy-mode-do-yank screen)
  (copy-mode-cancel-selection screen)
  (copy-mode-exit screen))

(defun copy-mode-copy-selection-no-cancel (screen)
  "Copy selected text to the paste buffer (and clipboard / copy-command), clear
   the selection, but STAY in copy mode.  This is tmux's `copy-selection`
   send-keys -X command: window_copy_cmd_copy_selection returns
   WINDOW_COPY_CMD_REDRAW, so copy mode is preserved (only the selection is
   cleared and the screen is redrawn)."
  (when (screen-copy-mode-p screen)
    (%copy-mode-do-yank screen)
    (copy-mode-clear-selection screen)
    (setf (screen-dirty-p screen) t)))

(defun copy-mode-copy-selection-no-clear (screen)
  "Copy the current selection to the paste buffer (and clipboard / copy-command)
   but do NOT clear the selection and stay in copy mode.  This is tmux's
   `copy-selection-no-clear` send-keys -X command (window_copy_cmd_copy_selection
   with clear=NEVER, returning WINDOW_COPY_CMD_NOTHING)."
  (when (screen-copy-mode-p screen)
    (%copy-mode-do-yank screen)
    (setf (screen-dirty-p screen) t)))

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
    (let ((text (%get-selection-text screen)))
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

(defun %copy-pipe-text (cmd text)
  "Copy TEXT to the paste buffer and pipe it to CMD or copy-command.
   Returns T when TEXT was non-empty and processed."
  (when (and text (plusp (length text)))
    (cl-tmux/buffer:add-paste-buffer text)
    (let ((effective-cmd (%resolve-copy-pipe-cmd cmd)))
      (when effective-cmd
        (%run-shell-cmd-with-input effective-cmd text)))
    t))

(defun copy-mode-copy-pipe (screen cmd)
  "Yank selected text to the paste buffer and pipe it to CMD (a shell string).
   If CMD is empty or NIL the global \"copy-command\" option is used.
   Exits copy mode after yanking (copy-pipe-and-cancel semantics)."
  (when (screen-copy-mode-p screen)
    (%copy-pipe-text cmd (%get-selection-text screen))
    (copy-mode-cancel-selection screen)
    (copy-mode-exit screen)))

(defun copy-mode-copy-pipe-no-cancel (screen cmd)
  "Yank selected text to the paste buffer and pipe it to CMD (a shell string).
   If CMD is empty or NIL the global \"copy-command\" option is used.
   Stays in copy mode after yanking (copy-pipe semantics, no cancel)."
  (when (screen-copy-mode-p screen)
    (%copy-pipe-text cmd (%get-selection-text screen))
    (setf (screen-dirty-p screen) t)))

(defun copy-mode-copy-pipe-end-of-line (screen cmd)
  "Copy from the cursor to the end of the current line, pipe it, then exit copy mode.
   If CMD is empty or NIL the global \"copy-command\" option is used."
  (when (screen-copy-mode-p screen)
    (let* ((row (car (screen-copy-cursor screen)))
           (col (cdr (screen-copy-cursor screen)))
           (text (%copy-row-range-text screen row col (screen-width screen))))
      (%copy-pipe-text cmd text))
    (copy-mode-cancel-selection screen)
    (copy-mode-exit screen)))
