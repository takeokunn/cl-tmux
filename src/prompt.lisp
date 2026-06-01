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
  (label     "" :type string)            ; shown before the buffer, e.g. "rename-window"
  (buffer    "" :type string)            ; the text typed so far
  (on-submit nil :type (or null function))) ; called with the final buffer string on Enter

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
   Enter."
  (setf *prompt* (make-prompt :label label :buffer initial :on-submit on-submit)))

(defun prompt-input (ch)
  "Append character CH to the active prompt's buffer (no-op when inactive)."
  (when *prompt*
    (setf (prompt-buffer *prompt*)
          (concatenate 'string (prompt-buffer *prompt*) (string ch)))))

(defun prompt-backspace ()
  "Delete the last character of the active prompt's buffer, if any."
  (let ((p *prompt*))
    (when (and p (plusp (length (prompt-buffer p))))
      (setf (prompt-buffer p)
            (subseq (prompt-buffer p) 0 (1- (length (prompt-buffer p))))))))

(defun prompt-clear ()
  "Dismiss the active prompt."
  (setf *prompt* nil))

(defun prompt-text ()
  "Status-bar display string (\"LABEL: BUFFER\"), or NIL when inactive."
  (let ((p *prompt*))
    (when p (format nil "~A: ~A" (prompt-label p) (prompt-buffer p)))))

;;; ── Dismissible overlay (e.g. list-keys help) ───────────────────────────────

(defvar *overlay* nil
  "Active overlay text (a string, possibly multi-line) shown over the session,
   or NIL.  Dismissed by the next keystroke.  Main-thread-only, like *prompt*.")

(defun overlay-active-p ()
  "True when an overlay is currently displayed."
  (and *overlay* t))

(defun show-overlay (text)
  "Display TEXT as an overlay until the next keystroke."
  (setf *overlay* text))

(defun clear-overlay ()
  "Dismiss the active overlay."
  (setf *overlay* nil))

(defun overlay-lines ()
  "The active overlay split into a list of lines, or NIL when inactive."
  (when *overlay*
    (loop with text = *overlay*
          for start = 0 then (1+ nl)
          for nl = (position #\Newline text :start start)
          collect (subseq text start (or nl (length text)))
          while nl)))

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
