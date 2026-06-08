(in-package #:cl-tmux/commands)

;;; ── Copy-mode search subsystem ──────────────────────────────────────────────
;;;
;;; copy_mode_search_forward(Screen, Term)  :- scan rows from cursor downward.
;;; copy_mode_search_backward(Screen, Term) :- scan rows from cursor upward.
;;; copy_mode_search_next(Screen)           :- repeat last search forward.
;;; copy_mode_search_prev(Screen)           :- repeat last search backward.
;;;
;;; This file depends on the viewport primitives in commands-copy-mode.lisp and
;;; the row-string helper defined here.

(defun %copy-mode-row-string (screen row)
  "Return the string content of ROW in the visible viewport (honours copy-offset)."
  (with-output-to-string (out)
    (loop for col from 0 below (screen-width screen)
          do (write-char (cell-char (screen-display-cell screen col row)) out))))

(defun %copy-mode-make-matcher (term)
  "Return a matcher closure (row-string start) → match-start-column (or NIL) for
   TERM.  TERM is treated as a regular expression (cl-ppcre) — matching tmux's
   copy-mode regex search — when it compiles as a valid regex; otherwise it falls
   back to a literal substring search, so search terms containing unbalanced regex
   metacharacters (e.g. \"(\") still work.  A plain alphanumeric term compiles as
   a regex that matches itself, so literal searches behave identically."
  (let ((scanner (ignore-errors (cl-ppcre:create-scanner term))))
    (if scanner
        (lambda (str start) (cl-ppcre:scan scanner str :start start))
        (lambda (str start) (search term str :start2 start)))))

(defun %copy-mode-find-forward (screen term start-row start-col)
  "Scan forward from START-ROW/START-COL for TERM in SCREEN's visible viewport.
   TERM is matched as a regex (literal fallback).  Returns (values row col) of the
   first match, or (values nil nil) when absent."
  (let ((height (screen-height screen))
        (match  (%copy-mode-make-matcher term)))
    (loop for row from start-row below height
          do (let* ((row-string (%copy-mode-row-string screen row))
                    (from-col  (if (= row start-row) start-col 0)))
               (when (<= from-col (length row-string))
                 (let ((position (funcall match row-string from-col)))
                   (when position
                     (return-from %copy-mode-find-forward (values row position)))))))
    (values nil nil)))

(defun %copy-mode-find-backward (screen term start-row start-col)
  "Scan backward from START-ROW/START-COL for TERM in SCREEN's visible viewport.
   TERM is matched as a regex (literal fallback).  Returns (values row col) of the
   nearest match starting before the cursor, or (values nil nil).  Within a row the
   LAST match whose start is < END-COL is chosen (the occurrence nearest the cursor)."
  (let ((match (%copy-mode-make-matcher term)))
    (loop for row from start-row downto 0
          do (let* ((row-string (%copy-mode-row-string screen row))
                    (end-col    (if (= row start-row) start-col (length row-string)))
                    (best       nil)
                    (from       0))
               ;; Walk matches left-to-right, keeping the last start strictly
               ;; before END-COL; +1 advance guarantees progress on zero-width
               ;; matches.
               (loop
                 (let ((pos (and (<= from (length row-string))
                                 (funcall match row-string from))))
                   (cond
                     ((or (null pos) (>= pos end-col)) (return))
                     (t (setf best pos) (setf from (1+ pos))))))
               (when best
                 (return-from %copy-mode-find-backward (values row best)))))
    (values nil nil)))

(defun %wrap-search-p ()
  "T when copy-mode search should wrap around the buffer ends — tmux's wrap-search
   option, default on.  get-option's default argument distinguishes an explicit
   `set wrap-search off` (present → NIL) from an absent option (→ T)."
  (cl-tmux/options:get-option "wrap-search" t))

(defun copy-mode-search-forward (screen term)
  "Search forward from the current cursor for TERM.
   Saves TERM as the active search term for n/N repeats.
   Moves the cursor to the first match found.  When no match lies below the cursor
   and wrap-search is on, wraps to the top and takes the first match in the buffer."
  (when (and (screen-copy-mode-p screen) term (plusp (length term)))
    (setf (screen-copy-search-term screen) term)
    (let* ((cursor (or (screen-copy-cursor screen) (cons 0 0)))
           (row    (car cursor))
           (col    (1+ (cdr cursor))))    ; start one past current position to advance
      (multiple-value-bind (found-row found-col)
          (%copy-mode-find-forward screen term row col)
        ;; Wrap-around: nothing below → search again from the top of the buffer.
        (when (and (null found-row) (%wrap-search-p))
          (multiple-value-setq (found-row found-col)
            (%copy-mode-find-forward screen term 0 0)))
        (when found-row
          (setf (screen-copy-cursor screen) (cons found-row found-col)
                (screen-dirty-p screen) t))))))

(defun copy-mode-search-backward (screen term)
  "Search backward from the current cursor for TERM.
   Saves TERM as the active search term for n/N repeats.
   Moves the cursor to the nearest match going back.  When no match lies above the
   cursor and wrap-search is on, wraps to the bottom and takes the last match."
  (when (and (screen-copy-mode-p screen) term (plusp (length term)))
    (setf (screen-copy-search-term screen) term)
    (let* ((cursor (or (screen-copy-cursor screen) (cons 0 0)))
           (row    (car cursor))
           (col    (cdr cursor)))
      (multiple-value-bind (found-row found-col)
          (%copy-mode-find-backward screen term row col)
        ;; Wrap-around: nothing above → search again from the bottom of the buffer.
        (when (and (null found-row) (%wrap-search-p))
          (multiple-value-setq (found-row found-col)
            (%copy-mode-find-backward screen term
                                      (1- (screen-height screen))
                                      (screen-width screen))))
        (when found-row
          (setf (screen-copy-cursor screen) (cons found-row found-col)
                (screen-dirty-p screen) t))))))

(defun copy-mode-search-next (screen)
  "Repeat the last search in the forward direction using the saved search term."
  (when (screen-copy-mode-p screen)
    (let ((term (screen-copy-search-term screen)))
      (when term
        (copy-mode-search-forward screen term)))))

(defun copy-mode-search-prev (screen)
  "Repeat the last search in the backward direction using the saved search term."
  (when (screen-copy-mode-p screen)
    (let ((term (screen-copy-search-term screen)))
      (when term
        (copy-mode-search-backward screen term)))))
