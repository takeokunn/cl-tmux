(in-package #:cl-tmux/prompt)

;;;; Single-line input prompt -- buffer editing and display.
;;;;
;;;; Overlay, popup, and menu state lives in overlay.lisp (same package).
;;;; Applying effects (e.g. renaming a window) is the caller's job; see events.lisp.
;;;; The renderer reads this state directly; it lives below the renderer.

;;; -- with-active-prompt guard macro ------------------------------------------

(defmacro with-active-prompt ((var) &body body)
  "Bind VAR to *prompt* and execute BODY only when a prompt is active.
   No-op when *prompt* is NIL.  Eliminates the repeated (let ((p *prompt*)) (when p ..)) guard."
  `(let ((,var *prompt*))
     (when ,var ,@body)))

;;; -- Prompt struct ------------------------------------------------------------

(defstruct prompt
  "An active single-line input prompt."
  (label        "" :type string)            ; shown before the buffer, e.g. "rename-window"
  (buffer       "" :type string)            ; the text typed so far
  (cursor-index  0 :type fixnum)            ; insertion point: 0..length-of-buffer
  (on-submit   nil :type (or null function)) ; called with the final buffer string on Enter
  ;; Incremental-search: called with the current buffer string after every edit.
  (on-change   nil :type (or null function))
  ;; Incremental-search cancel: called (no args) when the prompt is dismissed by
  ;; ESC / C-c / C-g so the caller can restore the pre-search cursor position.
  (on-cancel   nil :type (or null function))
  ;; Vi-mode: when status-keys = "vi", ESC enters normal mode instead of cancelling.
  (vi-normal-p nil :type boolean)           ; T when in vi normal mode
  ;; Single-key mode (confirm-before, command-prompt -1): the first printable key
  ;; IS the answer — submitted immediately, with no Enter required.
  (single-key  nil :type boolean))

(defvar *prompt* nil
  "The active PROMPT, or NIL when not prompting.
   Read/written only on the main thread (event loop + renderer); reader threads
   never touch it, so it needs no lock.")

(defun prompt-active-p ()
  "True when an input prompt is currently active."
  (and *prompt* t))

(defun prompt-start (label initial on-submit &key single-key on-change on-cancel)
  "Begin a prompt labelled LABEL, seeded with INITIAL text.  ON-SUBMIT is a
   function of one argument (the final buffer string) run when the user presses
   Enter.  The cursor starts at the end of INITIAL.
   SINGLE-KEY T (confirm-before, command-prompt -1) submits the first printable
   key immediately as a one-character string — no Enter needed.
   ON-CHANGE is a function of one argument called after every buffer edit —
   used by incremental search to jump to the nearest match while the user types.
   ON-CANCEL is a no-argument function called when the prompt is dismissed by
   ESC / C-c — used by incremental search to restore the pre-search cursor."
  (setf *prompt* (make-prompt :label label :buffer initial
                               :cursor-index (length initial)
                               :on-submit on-submit
                               :on-change  on-change
                               :on-cancel  on-cancel
                               :single-key single-key)))

;;; -- Change notification helper ----------------------------------------------

(defun prompt-notify-change ()
  "Call the active prompt's ON-CHANGE callback (if any) with the current buffer.
   Used by incremental search to jump to the nearest match after each edit."
  (with-active-prompt (p)
    (when (prompt-on-change p)
      (funcall (prompt-on-change p) (prompt-buffer p)))))

;;; -- Buffer editing -----------------------------------------------------------

(defun prompt-input (ch)
  "Insert character CH at the cursor position in the active prompt's buffer.
   Advances the cursor by one.  No-op when the prompt is inactive."
  (with-active-prompt (p)
    (let* ((buffer (prompt-buffer p))
           (index  (prompt-cursor-index p))
           (new    (concatenate 'string
                                (subseq buffer 0 index)
                                (string ch)
                                (subseq buffer index))))
      (setf (prompt-buffer       p) new)
      (setf (prompt-cursor-index p) (1+ index))))
  (prompt-notify-change))

(defun prompt-backspace ()
  "Delete the character immediately before the cursor, if any.
   The cursor moves back one position."
  (with-active-prompt (p)
    (let* ((buffer (prompt-buffer p))
           (index  (prompt-cursor-index p)))
      (when (plusp index)
        (setf (prompt-buffer       p)
              (concatenate 'string
                           (subseq buffer 0 (1- index))
                           (subseq buffer index)))
        (setf (prompt-cursor-index p) (1- index)))))
  (prompt-notify-change))

;;; -- Cursor navigation -- declarative table ----------------------------------
;;;
;;; All four cursor-movement functions are generated from a declarative table
;;; so the guard condition and setf target are never duplicated.
;;; with-active-prompt provides a uniform guard idiom throughout this file.

(defmacro define-prompt-cursor-ops (&rest specs)
  "Generate cursor-navigation defuns from a declarative fact table.
   Each SPEC has the form:
     (fn-name docstring :mode MODE ARGS...)
   where MODE is one of:
     :absolute EXPR   -- set cursor-index unconditionally to EXPR
     :step GUARD EXPR -- set cursor-index to EXPR only when GUARD is true
   EXPR and GUARD may reference the local variable P (the active prompt)."
  `(progn
     ,@(mapcar
        (lambda (spec)
          (destructuring-bind (fn-name docstring &rest rest) spec
            (let ((mode (first rest))
                  (args (rest rest)))
              `(defun ,fn-name ()
                 ,docstring
                 (with-active-prompt (p)
                   ,(ecase mode
                      (:absolute
                       `(setf (prompt-cursor-index p) ,(first args)))
                      (:step
                       (destructuring-bind (guard expr) args
                         `(when ,guard
                            (setf (prompt-cursor-index p) ,expr))))))))))
        specs)))

(define-prompt-cursor-ops
  (prompt-cursor-bol
   "Move the cursor to the beginning of the buffer (index 0)."
   :absolute 0)
  (prompt-cursor-eol
   "Move the cursor to the end of the buffer."
   :absolute (length (prompt-buffer p)))
  (prompt-cursor-back
   "Move the cursor one character to the left (no-op at beginning)."
   :step (plusp (prompt-cursor-index p))
         (1- (prompt-cursor-index p)))
  (prompt-cursor-forward
   "Move the cursor one character to the right (no-op at end)."
   :step (< (prompt-cursor-index p) (length (prompt-buffer p)))
         (1+ (prompt-cursor-index p))))

;;; -- Kill commands -----------------------------------------------------------

(defun prompt-kill-to-end ()
  "Kill (delete) all characters from the cursor to the end of the buffer."
  (with-active-prompt (p)
    (let ((index (prompt-cursor-index p)))
      (setf (prompt-buffer p) (subseq (prompt-buffer p) 0 index))))
  (prompt-notify-change))

(defun prompt-kill-to-start ()
  "Kill (delete) all characters from the start of the buffer to the cursor."
  (with-active-prompt (p)
    (let* ((buffer (prompt-buffer p))
           (index  (prompt-cursor-index p)))
      (setf (prompt-buffer       p) (subseq buffer index))
      (setf (prompt-cursor-index p) 0)))
  (prompt-notify-change))

(defun %skip-while-left (buffer scan predicate)
  "Walk SCAN leftward while PREDICATE holds for (char buffer (1- scan)).
   SCAN is a cursor position — it points between characters, so (1- scan) is
   the character immediately to the left.  Returns the new cursor index; clamps at 0."
  (loop while (and (> scan 0)
                   (funcall predicate (char buffer (1- scan))))
        do (decf scan)
        finally (return scan)))

(defun %word-kill-start (buffer end-index)
  "Return the new cursor index after a backward word-kill from END-INDEX.
   Matches readline/emacs C-w: skip trailing spaces left, then skip word chars left."
  (if (zerop end-index)
      0
      (let* ((after-spaces (%skip-while-left buffer end-index
                                             (lambda (c) (char= c #\Space))))
             (after-word   (%skip-while-left buffer after-spaces
                                             (lambda (c) (char/= c #\Space)))))
        after-word)))

(defun prompt-kill-word-back ()
  "Kill the word immediately before the cursor (C-w: back past spaces then word chars)."
  (with-active-prompt (p)
    (let* ((buffer      (prompt-buffer p))
           (end-index   (prompt-cursor-index p))
           (start-index (%word-kill-start buffer end-index)))
      (setf (prompt-buffer p)
            (concatenate 'string
                         (subseq buffer 0 start-index)
                         (subseq buffer end-index)))
      (setf (prompt-cursor-index p) start-index)))
  (prompt-notify-change))

;;; -- Vi-mode character deletion -----------------------------------------------

(defun prompt-delete-char ()
  "Delete the character at the cursor (vi `x`): removes the char under the cursor.
   Clamps cursor to stay within the buffer after deletion."
  (with-active-prompt (p)
    (let* ((buffer (prompt-buffer p))
           (index  (prompt-cursor-index p))
           (len    (length buffer)))
      (when (< index len)
        (setf (prompt-buffer p)
              (concatenate 'string
                           (subseq buffer 0 index)
                           (subseq buffer (1+ index))))
        ;; After deletion, clamp cursor so it doesn't go past end-1.
        (let ((new-len (1- len)))
          (when (> index new-len)
            (setf (prompt-cursor-index p) (max 0 new-len))))))))

;;; -- Dismiss and display -----------------------------------------------------

(defun prompt-clear ()
  "Dismiss the active prompt, calling on-cancel if set."
  (let ((p *prompt*))
    (setf *prompt* nil)
    (when (and p (prompt-on-cancel p))
      (funcall (prompt-on-cancel p)))))

(defun prompt-text ()
  "Status-bar display string with cursor indicator, or NIL when inactive.
   The cursor is shown as a '|' inserted at cursor-index in the buffer."
  (let ((p *prompt*))
    (when p
      (let* ((buffer       (prompt-buffer p))
             (cursor-index (prompt-cursor-index p)))
        (format nil "~A: ~A|~A"
                (prompt-label p)
                (subseq buffer 0 cursor-index)
                (subseq buffer cursor-index))))))

;;; Overlay, popup, and menu state continues in overlay.lisp (same package).
