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

(defun %copy-mode-find-forward (screen term start-row start-col)
  "Scan forward from START-ROW/START-COL for TERM in SCREEN's visible viewport.
   Returns (values row col) of the first match, or (values nil nil) when absent."
  (let ((height (screen-height screen)))
    (loop for row from start-row below height
          do (let* ((row-string (%copy-mode-row-string screen row))
                    (from-col  (if (= row start-row) start-col 0))
                    (position  (search term row-string :start2 from-col)))
               (when position
                 (return-from %copy-mode-find-forward (values row position)))))
    (values nil nil)))

(defun %copy-mode-find-backward (screen term start-row start-col)
  "Scan backward from START-ROW/START-COL for TERM in SCREEN's visible viewport.
   Returns (values row col) of the nearest match before the cursor, or (values nil nil)."
  (loop for row from start-row downto 0
        do (let* ((row-string (%copy-mode-row-string screen row))
                  (end-col   (if (= row start-row) start-col (length row-string)))
                  (position  (and (> end-col 0)
                                  (loop for i from (1- end-col) downto 0
                                        when (and (<= (+ i (length term)) (length row-string))
                                                  (string= term (subseq row-string i
                                                                        (+ i (length term)))))
                                          return i))))
             (when position
               (return-from %copy-mode-find-backward (values row position)))))
  (values nil nil))

(defun copy-mode-search-forward (screen term)
  "Search forward from the current cursor for TERM.
   Saves TERM as the active search term for n/N repeats.
   Moves the cursor to the first match found."
  (when (and (screen-copy-mode-p screen) term (plusp (length term)))
    (setf (screen-copy-search-term screen) term)
    (let* ((cursor (or (screen-copy-cursor screen) (cons 0 0)))
           (row    (car cursor))
           (col    (1+ (cdr cursor))))    ; start one past current position to advance
      (multiple-value-bind (found-row found-col)
          (%copy-mode-find-forward screen term row col)
        (when found-row
          (setf (screen-copy-cursor screen) (cons found-row found-col)
                (screen-dirty-p screen) t))))))

(defun copy-mode-search-backward (screen term)
  "Search backward from the current cursor for TERM.
   Saves TERM as the active search term for n/N repeats.
   Moves the cursor to the nearest match going back."
  (when (and (screen-copy-mode-p screen) term (plusp (length term)))
    (setf (screen-copy-search-term screen) term)
    (let* ((cursor (or (screen-copy-cursor screen) (cons 0 0)))
           (row    (car cursor))
           (col    (cdr cursor)))
      (multiple-value-bind (found-row found-col)
          (%copy-mode-find-backward screen term row col)
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
