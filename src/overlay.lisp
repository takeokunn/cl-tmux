(in-package #:cl-tmux/prompt)

;;;; Overlay, popup, and menu state.
;;;;
;;;; This file holds the three overlay concerns that were originally bundled in
;;;; prompt.lisp.  They share the cl-tmux/prompt package and are driven by
;;;; dispatch.lisp / rendered by renderer.lisp.  Splitting them here keeps
;;;; prompt.lisp focused solely on single-line editing.

;;; -- Dismissible overlay (e.g. list-keys help) --------------------------------

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

;;; -- Popup overlay -----------------------------------------------------------

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

(defvar *active-popup* nil
  "The currently displayed POPUP overlay, or NIL.
   Session-persistent: use defvar so image reloads do not reset live state.")

;;; -- Menu overlay ------------------------------------------------------------

(defstruct menu
  "An interactive text menu overlay."
  (title "" :type string)
  (items '() :type list)          ; list of (label . keyword) pairs
  (selected-index 0 :type fixnum))

(defvar *active-menu* nil
  "The currently displayed MENU overlay, or NIL.
   Session-persistent: use defvar so image reloads do not reset live state.")
