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

(defvar *overlay-shown-at* 0
  "Universal-time when the most recent transient overlay was shown.
   Set by show-transient-overlay for display-time auto-dismiss.")

(defvar *display-panes-active* nil
  "T while display-panes (C-b q) is showing per-pane numbers.  Set by the
   :display-panes handler AFTER it opens its (timing) transient overlay; cleared by
   any other overlay (show-overlay / show-transient-overlay) and by clear-overlay,
   so it is T only for the display-panes overlay.  The renderer draws the big
   per-pane numbers while it is T and the overlay is still active.")

(defun overlay-active-p ()
  "True when an overlay is currently displayed."
  (and *overlay* t))

(defun overlay-shown-at ()
  "Return the universal-time when the most recent transient overlay was shown.
   Returns 0 when no transient overlay has been shown in this session.
   Provided as a package-boundary accessor so callers never read the private
   *overlay-shown-at* variable directly."
  *overlay-shown-at*)

(defun %set-overlay (text)
  "Internal helper: install TEXT as the active overlay and reset the scroll
   offset.  Callers are responsible for setting *overlay-shown-at* and
   *display-panes-active* to whatever the variant requires."
  (setf *overlay* text
        *overlay-scroll-offset* 0))

(defun show-overlay (text)
  "Display TEXT as an overlay; navigated with j/k, dismissed with q or Esc."
  (%set-overlay text)
  (setf *display-panes-active* nil))

(defun show-transient-overlay (text &key (timestamp (get-universal-time)))
  "Display TEXT as a transient overlay that auto-dismisses after display-time ms.
   Used for display-message and similar short notifications.
   TIMESTAMP defaults to (get-universal-time); supply an explicit value in tests
   so assertions can verify the recorded value deterministically."
  (%set-overlay text)
  (setf *overlay-shown-at* timestamp
        *display-panes-active* nil))

(defun show-display-panes-overlay (text &key (timestamp (get-universal-time)))
  "Like SHOW-TRANSIENT-OVERLAY but activates *DISPLAY-PANES-ACTIVE* so the
   renderer draws per-pane index numbers over the session frame.
   TIMESTAMP defaults to (get-universal-time); supply an explicit value in tests."
  (%set-overlay text)
  (setf *overlay-shown-at* timestamp
        *display-panes-active* t))

(defun clear-overlay ()
  "Dismiss the active overlay and reset the scroll offset."
  (setf *overlay* nil
        *overlay-scroll-offset* 0
        *display-panes-active* nil))

(defun overlay-lines ()
  "The active overlay split into a list of lines, or NIL when inactive."
  (when *overlay*
    (loop with text = *overlay*
          for start = 0 then (1+ newline-pos)
          for newline-pos = (position #\Newline text :start start)
          collect (subseq text start (or newline-pos (length text)))
          while newline-pos)))

(defun overlay-scroll (delta)
  "Scroll the active overlay by DELTA lines (positive = down, negative = up).
   Clamps to valid range.  No-op when no overlay is active."
  (when *overlay*
    (let* ((lines (overlay-lines))
           (max-offset (max 0 (1- (length lines)))))
      (setf *overlay-scroll-offset*
            (max 0 (min max-offset (+ *overlay-scroll-offset* delta)))))))

;;; -- Popup overlay -----------------------------------------------------------

(defconstant +default-popup-width+  40
  "Default width (columns) for a newly created popup overlay.")

(defconstant +default-popup-height+ 10
  "Default height (rows) for a newly created popup overlay.")

(defstruct popup
  "A floating overlay window with its own PTY and screen."
  (x 0 :type fixnum)
  (y 0 :type fixnum)
  (width  +default-popup-width+  :type fixnum)
  (height +default-popup-height+ :type fixnum)
  (screen nil)             ; screen struct for the popup (or NIL for text-only)
  (pane   nil)             ; pane struct for the popup (or NIL for text-only)
  (title  "" :type string)
  (close-on-exit t :type boolean))

(defvar *active-popup* nil
  "The currently displayed POPUP overlay, or NIL.
   Session-persistent: use defvar so image reloads do not reset live state.")

(defun show-popup (popup)
  "Register POPUP as the active popup overlay.
   If another popup is already active it is silently replaced — the old popup
   is not closed or cleaned up.  Callers requiring teardown must call close-popup first."
  (setf *active-popup* popup))

(defun close-popup ()
  "Dismiss the active popup overlay, setting *active-popup* to NIL.
   Safe to call when no popup is active (no-op)."
  (setf *active-popup* nil))

(defun popup-active-p ()
  "Return T when a popup overlay is currently displayed, NIL otherwise."
  (and *active-popup* t))

;;; -- Menu overlay ------------------------------------------------------------

(defstruct menu
  "An interactive text menu overlay."
  (title "" :type string)
  (items '() :type list)          ; list of (label . keyword) pairs
  (selected-index 0 :type fixnum)
  ;; Position (display-menu -x/-y): NIL = centre on screen, integer = fixed column/row.
  (x nil)
  (y nil))

(defvar *active-menu* nil
  "The currently displayed MENU overlay, or NIL.
   Session-persistent: use defvar so image reloads do not reset live state.")

(defun show-menu (menu)
  "Register MENU as the active menu overlay.
   Callers should use this instead of directly mutating *active-menu* so that
   the lifecycle boundary stays inside this module."
  (setf *active-menu* menu))

(defun close-menu ()
  "Dismiss the active menu overlay.
   Callers should use this instead of directly setting *active-menu* to NIL."
  (setf *active-menu* nil))

(defun menu-active-p ()
  "True when a menu overlay is currently displayed."
  (and *active-menu* t))
