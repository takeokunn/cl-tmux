(in-package #:cl-tmux/commands)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "src/commands-copy-mode-virtual.lisp")
  (load "src/commands-copy-mode-brackets.lisp"))

(declaim (special cl-tmux::*dirty*))

;;; ── Copy-mode search subsystem ──────────────────────────────────────────────
;;;
;;; copy_mode_search_forward(Screen, Term)  :- scan rows from cursor downward
;;;   through the *entire* virtual buffer (scrollback + live grid).
;;; copy_mode_search_backward(Screen, Term) :- scan rows from cursor upward.
;;; copy_mode_search_next(Screen)           :- repeat last search forward.
;;; copy_mode_search_prev(Screen)           :- repeat last search backward.
;;; copy_mode_search_forward_incremental    :- C-s live-update search.
;;; copy_mode_search_backward_incremental   :- C-r live-update search.
;;;
;;; Virtual row numbering (0 = oldest scrollback row, increasing toward live grid):
;;;   0 .. sb-count-1  : scrollback (oldest→newest)
;;;   sb-count .. sb-count+height-1 : live grid (top→bottom)
;;;
;;; Mapping from (copy-offset, viewport-row) to virtual row:
;;;   vrow = sb-count + viewport-row - copy-offset

;;; ── Incremental-search origin store ─────────────────────────────────────────
;;;
;;; When incremental search starts, the current cursor+offset are saved so they
;;; can be restored on cancel (C-g / ESC) or used as the search anchor on each
;;; on-change call.  A plain special is safe here: the event loop is single-threaded
;;; and incremental search is always entered and exited on the main thread.

(defvar *copy-mode-isearch-origin* nil
  "Saved (cons (cons row col) offset) when incremental search is active.
   NIL when no incremental search is in progress.")

;;; ── Matcher factory ──────────────────────────────────────────────────────────

(defun %copy-mode-make-matcher (term)
  "Return a matcher closure (row-string start) → match-start-column (or NIL).
   TERM is compiled as a cl-ppcre regex; on compile failure falls back to
   literal substring search so terms with unbalanced metacharacters still work."
  (let ((scanner (ignore-errors (cl-ppcre:create-scanner term))))
    (if scanner
        (lambda (str start) (cl-ppcre:scan scanner str :start start))
        (lambda (str start) (search term str :start2 start)))))

(defun %copy-mode-regex-escape (text)
  "Return TEXT with regex metacharacters escaped for literal search."
  (with-output-to-string (out)
    (loop for ch across text do
      (when (find ch "\\^$.|?*+()[]{}" :test #'char=)
        (write-char #\\ out))
      (write-char ch out))))

;;; ── Full-buffer directional search ──────────────────────────────────────────

(defun %copy-mode-find-forward (screen term start-vrow start-col)
  "Scan forward through the full virtual buffer from (START-VROW, START-COL).
   Returns (values vrow col) of the first match, or (values nil nil) when absent."
  (let ((total (%copy-mode-total-rows screen))
        (match (%copy-mode-make-matcher term)))
    (loop for vrow from start-vrow below total
          for row-str  = (%copy-mode-virtual-row-string screen vrow)
          for from-col = (if (= vrow start-vrow) start-col 0)
          for pos      = (and (<= from-col (length row-str))
                              (funcall match row-str from-col))
          when pos return (values vrow pos)
          finally (return (values nil nil)))))

(defun %copy-mode-find-backward (screen term start-vrow start-col)
  "Scan backward through the full virtual buffer from (START-VROW, START-COL).
   Within a row takes the LAST match whose start is strictly < START-COL (cursor-adjacent).
   Returns (values vrow col) or (values nil nil)."
  (let ((match (%copy-mode-make-matcher term)))
    (loop for vrow from start-vrow downto 0
          for row-str = (%copy-mode-virtual-row-string screen vrow)
          for end-col = (if (= vrow start-vrow) start-col (length row-str))
          ;; Walk all matches left-to-right, keep the last start < end-col.
          for best = (loop with b = nil and from = 0
                           for pos = (and (<= from (length row-str))
                                          (funcall match row-str from))
                           if (or (null pos) (>= pos end-col)) return b
                           else do (setf b pos from (1+ pos)))
          when best return (values vrow best)
          finally (return (values nil nil)))))

;;; ── Wrap-search option ───────────────────────────────────────────────────────

(defun %wrap-search-p ()
  "T when copy-mode search should wrap around the buffer ends."
  (cl-tmux/options:get-option "wrap-search" t))

;;; ── Public search commands ───────────────────────────────────────────────────

(defun %copy-mode-search-direction (screen term direction)
  "Shared search engine for copy-mode-search-{forward,backward}.
   DIRECTION is :forward or :backward.  Saves TERM; wraps when wrap-search is on.
   Forward starts one past the cursor col; backward starts at the cursor col."
  (when (and (screen-copy-mode-p screen) term (plusp (length term)))
    (setf (screen-copy-search-term screen) term)
    (let* ((cursor     (or (screen-copy-cursor screen) (cons 0 0)))
           (start-vrow (%copy-mode-cursor-virtual-row screen))
           (forwardp   (eq direction :forward))
           (finder     (if forwardp #'%copy-mode-find-forward #'%copy-mode-find-backward))
           (start-col  (if forwardp (1+ (cdr cursor)) (cdr cursor))))
      (multiple-value-bind (found-vrow found-col)
          (funcall finder screen term start-vrow start-col)
        (when (and (null found-vrow) (%wrap-search-p))
          (multiple-value-setq (found-vrow found-col)
            (if forwardp
                (funcall finder screen term 0 0)
                (funcall finder screen term
                         (1- (%copy-mode-total-rows screen))
                         (screen-width screen)))))
        (when found-vrow
          (%copy-mode-set-virtual-row screen found-vrow found-col))))))

(defun copy-mode-search-forward (screen term)
  "Search forward from the current cursor for TERM through the full scrollback + live grid.
   Saves TERM for n/N repeats.  Wraps to top when wrap-search is on."
  (%copy-mode-search-direction screen term :forward))

(defun copy-mode-search-backward (screen term)
  "Search backward from the current cursor for TERM through the full scrollback + live grid.
   Saves TERM for n/N repeats.  Wraps to bottom when wrap-search is on."
  (%copy-mode-search-direction screen term :backward))

(defun copy-mode-search-next (screen)
  "Repeat the last search in the forward direction."
  (when (screen-copy-mode-p screen)
    (let ((term (screen-copy-search-term screen)))
      (when term
        (copy-mode-search-forward screen term)))))

(defun copy-mode-search-prev (screen)
  "Repeat the last search in the backward direction."
  (when (screen-copy-mode-p screen)
    (let ((term (screen-copy-search-term screen)))
      (when term
        (copy-mode-search-backward screen term)))))

(defun copy-mode-search-forward-word (screen)
  "Search forward for the word under the copy-mode cursor, treating it literally."
  (let ((term (%copy-mode-word-at-cursor screen)))
    (when term
      (copy-mode-search-forward screen (%copy-mode-regex-escape term)))))

(defun copy-mode-search-backward-word (screen)
  "Search backward for the word under the copy-mode cursor, treating it literally."
  (let ((term (%copy-mode-word-at-cursor screen)))
    (when term
      (copy-mode-search-backward screen (%copy-mode-regex-escape term)))))

;;; ── Incremental search (C-s / C-r) ──────────────────────────────────────────
;;;
;;; Incremental search prompts the user for a search string while simultaneously
;;; moving the cursor to the first match after every keystroke.  On cancel (ESC /
;;; C-g) the cursor is restored to where it was before the search started.
;;; On submit (Enter) the match stays and the term is saved for n/N repeats.
;;;
;;; The prompt is opened by the dispatch-handlers entry; the on-change closure
;;; anchors each search at the saved origin, so deletion of characters returns the
;;; cursor to the previous (shorter) match rather than advancing past it.

(defun %copy-mode-isearch-from-origin (screen term direction)
  "Jump from the saved isearch origin to the nearest match for TERM.
   DIRECTION is :forward or :backward.  When TERM is empty the cursor is
   restored to the origin (\"nothing typed yet\" state)."
  (when (screen-copy-mode-p screen)
    (let ((origin *copy-mode-isearch-origin*))
      (cond
        ((null origin)
         ;; Origin not saved yet — save now and search from current position.
         (setf *copy-mode-isearch-origin*
               (cons (screen-copy-cursor screen) (screen-copy-offset screen)))
         (%copy-mode-isearch-from-origin screen term direction))
        ((zerop (length term))
         ;; Empty string: restore cursor to where search started.
         (setf (screen-copy-cursor screen) (car origin)
               (screen-copy-offset screen) (cdr origin)
               (screen-dirty-p screen) t))
        (t
         ;; Non-empty: temporarily restore origin then search from there.
         (setf (screen-copy-cursor screen) (car origin)
               (screen-copy-offset screen) (cdr origin))
         (if (eq direction :forward)
             (copy-mode-search-forward  screen term)
             (copy-mode-search-backward screen term)))))))

(defun %copy-mode-isearch-start (screen direction)
  "Launch an incremental search prompt in DIRECTION (:forward or :backward).
   Saves the current cursor/offset as the restore point; on-cancel restores it."
  (setf *copy-mode-isearch-origin*
        (cons (screen-copy-cursor screen) (screen-copy-offset screen)))
  (cl-tmux/prompt:prompt-start
   (if (eq direction :forward) "search-forward" "search-backward") ""
   (lambda (term)
     (setf *copy-mode-isearch-origin* nil)
     (when (and term (plusp (length term)))
       (setf (screen-copy-search-term screen) term)))
   :on-change
   (lambda (text)
     (%copy-mode-isearch-from-origin screen text direction)
     (setf cl-tmux::*dirty* t))
   :on-cancel
   (lambda ()
     (let ((origin *copy-mode-isearch-origin*))
       (setf *copy-mode-isearch-origin* nil)
       (when (and origin (screen-copy-mode-p screen))
         (setf (screen-copy-cursor screen) (car origin)
               (screen-copy-offset screen) (cdr origin)
               (screen-dirty-p screen) t))))))

(defun copy-mode-search-forward-incremental (screen)
  "Start a forward incremental search prompt (C-s in copy-mode).
   Each keystroke moves the cursor to the nearest forward match.
   ESC/C-g cancels and restores the original position."
  (when (screen-copy-mode-p screen)
    (%copy-mode-isearch-start screen :forward)))

(defun copy-mode-search-backward-incremental (screen)
  "Start a backward incremental search prompt (C-r in copy-mode).
   Each keystroke moves the cursor to the nearest backward match.
   ESC/C-g cancels and restores the original position."
  (when (screen-copy-mode-p screen)
    (%copy-mode-isearch-start screen :backward)))
