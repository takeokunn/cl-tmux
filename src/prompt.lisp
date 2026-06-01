(in-package #:cl-tmux/prompt)

;;;; Transient view-state shown over the session: a single-line input prompt
;;;; (e.g. interactive rename-window) and a dismissible multi-line overlay
;;;; (e.g. the list-keys help).
;;;;
;;;; This module is pure UI state plus buffer-editing logic.  Applying any
;;;; effect (renaming a window, etc.) is the caller's job; see events.lisp.
;;;; It lives below the renderer so the renderer can read this state directly.

(defstruct prompt
  "An active single-line input prompt."
  (label        "" :type string)            ; shown before the buffer, e.g. "rename-window"
  (buffer       "" :type string)            ; the text typed so far
  (cursor-index  0 :type fixnum)            ; insertion point: 0..length-of-buffer
  (on-submit   nil :type (or null function))) ; called with the final buffer string on Enter

(defvar *prompt* nil
  "The active PROMPT, or NIL when not prompting.
   Read/written only on the main thread (event loop + renderer); reader threads
   never touch it, so it needs no lock.")

(defun prompt-active-p ()
  "True when an input prompt is currently active."
  (and *prompt* t))

(defun prompt-start (label initial on-submit)
  "Begin a prompt labelled LABEL, seeded with INITIAL text.  ON-SUBMIT is a
   function of one argument (the final buffer string) run when the user presses
   Enter.  The cursor starts at the end of INITIAL."
  (setf *prompt* (make-prompt :label label :buffer initial
                               :cursor-index (length initial)
                               :on-submit on-submit)))

(defun prompt-input (ch)
  "Insert character CH at the cursor position in the active prompt's buffer.
   Advances the cursor by one.  No-op when the prompt is inactive."
  (let ((p *prompt*))
    (when p
      (let* ((buf  (prompt-buffer p))
             (idx  (prompt-cursor-index p))
             (new  (concatenate 'string
                                (subseq buf 0 idx)
                                (string ch)
                                (subseq buf idx))))
        (setf (prompt-buffer       p) new)
        (setf (prompt-cursor-index p) (1+ idx))))))

(defun prompt-backspace ()
  "Delete the character immediately before the cursor, if any.
   The cursor moves back one position."
  (let ((p *prompt*))
    (when p
      (let* ((buf (prompt-buffer p))
             (idx (prompt-cursor-index p)))
        (when (plusp idx)
          (setf (prompt-buffer       p)
                (concatenate 'string
                             (subseq buf 0 (1- idx))
                             (subseq buf idx)))
          (setf (prompt-cursor-index p) (1- idx)))))))

(defun prompt-cursor-bol ()
  "Move the cursor to the beginning of the buffer (index 0)."
  (when *prompt*
    (setf (prompt-cursor-index *prompt*) 0)))

(defun prompt-cursor-eol ()
  "Move the cursor to the end of the buffer."
  (when *prompt*
    (setf (prompt-cursor-index *prompt*)
          (length (prompt-buffer *prompt*)))))

(defun prompt-cursor-back ()
  "Move the cursor one character to the left (no-op at beginning)."
  (let ((p *prompt*))
    (when (and p (plusp (prompt-cursor-index p)))
      (decf (prompt-cursor-index p)))))

(defun prompt-cursor-forward ()
  "Move the cursor one character to the right (no-op at end)."
  (let ((p *prompt*))
    (when (and p (< (prompt-cursor-index p)
                    (length (prompt-buffer p))))
      (incf (prompt-cursor-index p)))))

(defun prompt-kill-to-end ()
  "Kill (delete) all characters from the cursor to the end of the buffer."
  (let ((p *prompt*))
    (when p
      (let ((idx (prompt-cursor-index p)))
        (setf (prompt-buffer p) (subseq (prompt-buffer p) 0 idx))))))

(defun prompt-kill-to-start ()
  "Kill (delete) all characters from the start of the buffer to the cursor."
  (let ((p *prompt*))
    (when p
      (let* ((buf (prompt-buffer p))
             (idx (prompt-cursor-index p)))
        (setf (prompt-buffer       p) (subseq buf idx))
        (setf (prompt-cursor-index p) 0)))))

(defun prompt-kill-word-back ()
  "Kill the word immediately before the cursor (back to last space or start)."
  (let ((p *prompt*))
    (when p
      (let* ((buf (prompt-buffer p))
             (idx (prompt-cursor-index p))
             ;; Skip trailing spaces before the word
             (end idx)
             (start (if (zerop end)
                        0
                        (let ((i (1- end)))
                          ;; skip trailing spaces
                          (loop while (and (> i 0)
                                           (char= #\Space (char buf (1- i))))
                                do (decf i))
                          ;; skip word chars
                          (loop while (and (> i 0)
                                           (char/= #\Space (char buf (1- i))))
                                do (decf i))
                          i))))
        (setf (prompt-buffer p)
              (concatenate 'string
                           (subseq buf 0 start)
                           (subseq buf end)))
        (setf (prompt-cursor-index p) start)))))

(defun prompt-clear ()
  "Dismiss the active prompt."
  (setf *prompt* nil))

(defun prompt-text ()
  "Status-bar display string with cursor indicator, or NIL when inactive.
   The cursor is shown as a '|' inserted at cursor-index in the buffer."
  (let ((p *prompt*))
    (when p
      (let* ((buf (prompt-buffer p))
             (idx (prompt-cursor-index p)))
        (format nil "~A: ~A|~A"
                (prompt-label p)
                (subseq buf 0 idx)
                (subseq buf idx))))))

;;; ── Dismissible overlay (e.g. list-keys help) ───────────────────────────────

(defvar *overlay* nil
  "Active overlay text (a string, possibly multi-line) shown over the session,
   or NIL.  Dismissed by q or Esc; scrolled by j/k.  Main-thread-only, like *prompt*.")

(defvar *overlay-scroll-offset* 0
  "Current scroll offset (in lines) for the active overlay pager.
   0 means the first line is shown at the top.  Reset to 0 when a new overlay
   is displayed.  Main-thread-only.")

(defun overlay-active-p ()
  "True when an overlay is currently displayed."
  (and *overlay* t))

(defun show-overlay (text)
  "Display TEXT as an overlay; navigated with j/k, dismissed with q or Esc."
  (setf *overlay* text)
  (setf *overlay-scroll-offset* 0))

(defun clear-overlay ()
  "Dismiss the active overlay and reset the scroll offset."
  (setf *overlay* nil)
  (setf *overlay-scroll-offset* 0))

(defun overlay-lines ()
  "The active overlay split into a list of lines, or NIL when inactive."
  (when *overlay*
    (loop with text = *overlay*
          for start = 0 then (1+ nl)
          for nl = (position #\Newline text :start start)
          collect (subseq text start (or nl (length text)))
          while nl)))

(defun overlay-scroll (delta)
  "Scroll the active overlay by DELTA lines (positive = down, negative = up).
   Clamps to valid range.  No-op when no overlay is active."
  (when *overlay*
    (let* ((lines (overlay-lines))
           (max-offset (max 0 (1- (length lines)))))
      (setf *overlay-scroll-offset*
            (max 0 (min max-offset (+ *overlay-scroll-offset* delta)))))))

;;; ── Popup overlay ───────────────────────────────────────────────────────────

(defstruct popup
  "A floating overlay window with its own PTY and screen."
  (x 0 :type fixnum)
  (y 0 :type fixnum)
  (width 40 :type fixnum)
  (height 10 :type fixnum)
  (screen nil)             ; screen struct for the popup (or NIL for text-only)
  (pane   nil)             ; pane struct for the popup (or NIL for text-only)
  (title  "" :type string)
  (close-on-exit t :type boolean))

(defparameter *active-popup* nil
  "The currently displayed POPUP overlay, or NIL.")

;;; ── Menu overlay ────────────────────────────────────────────────────────────

(defstruct menu
  "An interactive text menu overlay."
  (title "" :type string)
  (items '() :type list)          ; list of (label . keyword) pairs
  (selected-index 0 :type fixnum))

(defparameter *active-menu* nil
  "The currently displayed MENU overlay, or NIL.")
