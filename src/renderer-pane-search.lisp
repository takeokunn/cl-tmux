(in-package #:cl-tmux/renderer)

;;; Copy-mode search-match highlighting for pane rendering.

(defun %screen-row-display-string (screen row)
  "The visible (offset-aware) content of ROW as a string."
  (with-output-to-string (s)
    (dotimes (col (screen-width screen))
      (write-char (cell-char (screen-display-cell screen col row)) s))))

(defun %all-match-ranges (term row-str)
  "All (START . END) column ranges in ROW-STR matching TERM: regex via cl-ppcre,
   literal-substring fallback when TERM is not a valid regex.  Zero-width matches
   are skipped."
  (if (ignore-errors (cl-ppcre:create-scanner term))
      (loop for (s e) on (cl-ppcre:all-matches term row-str) by #'cddr
            when (and e (> e s)) collect (cons s e))
      (loop with tlen = (length term) and start = 0
            for pos = (search term row-str :start2 start)
            while pos
            collect (cons pos (+ pos tlen))
            do (setf start (+ pos (max 1 tlen))))))

(defun %copy-match-sgr (option-name default)
  "SGR string for the copy-mode-(current-)match-style OPTION-NAME, or NIL when the
   option is empty.  Parses the tmux style string via the renderer style pipeline."
  (let ((style (cl-tmux/options:get-option option-name default)))
    (when (and style (plusp (length style)))
      (style-to-sgr (parse-style-string style)))))

(defun %render-copy-search-matches (buffer pane)
  "When PANE's screen is in copy mode with an active search term, overdraw each
   matching span in copy-mode-match-style — the span under the copy cursor in
   copy-mode-current-match-style — over the already-rendered pane content."
  (let ((screen (pane-screen pane)))
    (when (and screen (screen-copy-mode-p screen))
      (let ((term (screen-copy-search-term screen)))
        (when (and term (plusp (length term)))
          (let* ((match-sgr   (%copy-match-sgr "copy-mode-match-style" "bg=green"))
                 (current-sgr  (%copy-match-sgr "copy-mode-current-match-style"
                                                "bg=magenta"))
                 (cursor       (screen-copy-cursor screen))
                 (cur-row      (and (consp cursor) (car cursor)))
                 (cur-col      (and (consp cursor) (cdr cursor)))
                 (ox           (pane-x pane))
                 (oy           (pane-y pane))
                 (w            (screen-width screen)))
            (when match-sgr
              (dotimes (row (screen-height screen))
                (let ((row-str (%screen-row-display-string screen row)))
                  (dolist (range (%all-match-ranges term row-str))
                    (let* ((start (car range))
                           (end   (min (cdr range) w))
                           (current-p (and (eql cur-row row) cur-col
                                           (<= start cur-col) (< cur-col end)))
                           (sgr   (if current-p (or current-sgr match-sgr) match-sgr)))
                      (move-to buffer (+ oy row) (+ ox start))
                      (%emit-sgr buffer sgr)
                      (write-string (subseq row-str start end) buffer)
                      (reset-attrs buffer))))))))))))
