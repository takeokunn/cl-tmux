(in-package #:cl-tmux/commands)

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

;;; ── Virtual buffer helpers ───────────────────────────────────────────────────

(defun %copy-mode-total-rows (screen)
  "Total row count in the virtual buffer (scrollback + live grid)."
  (+ (length (screen-scrollback screen)) (screen-height screen)))

(defun %copy-mode-virtual-row-string (screen vrow)
  "Content of virtual row VROW as a string.
   VROW 0 = oldest scrollback; VROW (total-1) = bottom of live grid."
  (let* ((sb    (screen-scrollback screen))
         (sb-n  (length sb))
         (width (screen-width screen)))
    (with-output-to-string (out)
      (if (< vrow sb-n)
          ;; Scrollback: vrow 0 = oldest = nth(sb-n-1), vrow sb-n-1 = newest = nth(0)
          (let ((vec (nth (- sb-n 1 vrow) sb)))
            (loop for col from 0 below width
                  do (write-char
                      (if (and vec (< col (length vec)))
                          (cell-char (aref vec col))
                          #\Space)
                      out)))
          ;; Live grid
          (loop for col from 0 below width
                do (write-char
                    (cell-char (screen-cell screen col (- vrow sb-n)))
                    out))))))

(defun %copy-mode-cursor-virtual-row (screen)
  "Virtual row index of the current copy-mode cursor."
  (let ((cursor (screen-copy-cursor screen)))
    (+ (length (screen-scrollback screen))
       (- (if cursor (car cursor) 0)
          (screen-copy-offset screen)))))

(defun %copy-mode-set-virtual-row (screen vrow col)
  "Position the copy-mode cursor at (VROW, COL), adjusting offset so VROW is visible."
  (let* ((sb-n   (length (screen-scrollback screen)))
         (offset (max 0 (min sb-n (- sb-n vrow))))
         (crow   (+ vrow offset (- sb-n))))
    (setf (screen-copy-offset screen) offset
          (screen-copy-cursor screen)  (cons crow col)
          (screen-dirty-p screen) t)))

;;; ── Viewport-relative row string (backward compatibility) ───────────────────

(defun %copy-mode-row-string (screen row)
  "Content of viewport row ROW as a string, honoring copy-offset.
   ROW is 0-based viewport-relative (0 = top of viewport); delegates to
   %copy-mode-virtual-row-string for the actual cell lookup."
  (let ((vrow (+ (length (screen-scrollback screen))
                 (- row (screen-copy-offset screen)))))
    (%copy-mode-virtual-row-string screen vrow)))

;;; ── Matcher factory ──────────────────────────────────────────────────────────

(defun %copy-mode-make-matcher (term)
  "Return a matcher closure (row-string start) → match-start-column (or NIL).
   TERM is compiled as a cl-ppcre regex; on compile failure falls back to
   literal substring search so terms with unbalanced metacharacters still work."
  (let ((scanner (ignore-errors (cl-ppcre:create-scanner term))))
    (if scanner
        (lambda (str start) (cl-ppcre:scan scanner str :start start))
        (lambda (str start) (search term str :start2 start)))))

;;; ── Full-buffer directional search ──────────────────────────────────────────

(defun %copy-mode-find-forward (screen term start-vrow start-col)
  "Scan forward through the full virtual buffer from (START-VROW, START-COL).
   Returns (values vrow col) of the first match, or (values nil nil) when absent."
  (let ((total (%copy-mode-total-rows screen))
        (match (%copy-mode-make-matcher term)))
    (loop for vrow from start-vrow below total
          do (let* ((row-str  (%copy-mode-virtual-row-string screen vrow))
                    (from-col (if (= vrow start-vrow) start-col 0)))
               (when (<= from-col (length row-str))
                 (let ((pos (funcall match row-str from-col)))
                   (when pos
                     (return-from %copy-mode-find-forward (values vrow pos)))))))
    (values nil nil)))

(defun %copy-mode-find-backward (screen term start-vrow start-col)
  "Scan backward through the full virtual buffer from (START-VROW, START-COL).
   Within a row takes the LAST match whose start is strictly < START-COL (cursor-adjacent).
   Returns (values vrow col) or (values nil nil)."
  (let ((match (%copy-mode-make-matcher term)))
    (loop for vrow from start-vrow downto 0
          do (let* ((row-str (%copy-mode-virtual-row-string screen vrow))
                    (end-col (if (= vrow start-vrow) start-col (length row-str)))
                    (best    nil)
                    (from    0))
               ;; Walk all matches left-to-right, keep the last start < end-col.
               (loop
                 (let ((pos (and (<= from (length row-str))
                                 (funcall match row-str from))))
                   (cond
                     ((or (null pos) (>= pos end-col)) (return))
                     (t (setf best pos) (setf from (1+ pos))))))
               (when best
                 (return-from %copy-mode-find-backward (values vrow best)))))
    (values nil nil)))

;;; ── Wrap-search option ───────────────────────────────────────────────────────

(defun %wrap-search-p ()
  "T when copy-mode search should wrap around the buffer ends."
  (cl-tmux/options:get-option "wrap-search" t))

;;; ── Public search commands ───────────────────────────────────────────────────

(defun copy-mode-search-forward (screen term)
  "Search forward from the current cursor for TERM through the full scrollback + live grid.
   Saves TERM for n/N repeats.  Wraps to top when wrap-search is on."
  (when (and (screen-copy-mode-p screen) term (plusp (length term)))
    (setf (screen-copy-search-term screen) term)
    (let* ((cursor    (or (screen-copy-cursor screen) (cons 0 0)))
           (start-vrow (%copy-mode-cursor-virtual-row screen))
           (start-col  (1+ (cdr cursor))))    ; advance past current position on same row
      (multiple-value-bind (found-vrow found-col)
          (%copy-mode-find-forward screen term start-vrow start-col)
        (when (and (null found-vrow) (%wrap-search-p))
          (multiple-value-setq (found-vrow found-col)
            (%copy-mode-find-forward screen term 0 0)))
        (when found-vrow
          (%copy-mode-set-virtual-row screen found-vrow found-col))))))

(defun copy-mode-search-backward (screen term)
  "Search backward from the current cursor for TERM through the full scrollback + live grid.
   Saves TERM for n/N repeats.  Wraps to bottom when wrap-search is on."
  (when (and (screen-copy-mode-p screen) term (plusp (length term)))
    (setf (screen-copy-search-term screen) term)
    (let* ((cursor    (or (screen-copy-cursor screen) (cons 0 0)))
           (start-vrow (%copy-mode-cursor-virtual-row screen))
           (start-col  (cdr cursor)))
      (multiple-value-bind (found-vrow found-col)
          (%copy-mode-find-backward screen term start-vrow start-col)
        (when (and (null found-vrow) (%wrap-search-p))
          (let ((bottom-vrow (1- (%copy-mode-total-rows screen))))
            (multiple-value-setq (found-vrow found-col)
              (%copy-mode-find-backward screen term bottom-vrow
                                        (screen-width screen)))))
        (when found-vrow
          (%copy-mode-set-virtual-row screen found-vrow found-col))))))

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

(defun copy-mode-search-forward-incremental (screen)
  "Start a forward incremental search prompt (C-s in copy-mode).
   Each keystroke moves the cursor to the nearest forward match.
   ESC/C-g cancels and restores the original position."
  (when (screen-copy-mode-p screen)
    ;; Save the origin BEFORE opening the prompt.
    (setf *copy-mode-isearch-origin*
          (cons (screen-copy-cursor screen) (screen-copy-offset screen)))
    (cl-tmux/prompt:prompt-start
     "search-forward" ""
     ;; on-submit: save term for n/N, clear origin
     (lambda (term)
       (setf *copy-mode-isearch-origin* nil)
       (when (and term (plusp (length term)))
         (setf (screen-copy-search-term screen) term)))
     :on-change
     (lambda (text)
       (%copy-mode-isearch-from-origin screen text :forward)
       (setf cl-tmux::*dirty* t))
     :on-cancel
     ;; Restore cursor to pre-search position.
     (lambda ()
       (let ((origin *copy-mode-isearch-origin*))
         (setf *copy-mode-isearch-origin* nil)
         (when (and origin (screen-copy-mode-p screen))
           (setf (screen-copy-cursor screen) (car origin)
                 (screen-copy-offset screen) (cdr origin)
                 (screen-dirty-p screen) t)))))))

(defun copy-mode-search-backward-incremental (screen)
  "Start a backward incremental search prompt (C-r in copy-mode).
   Each keystroke moves the cursor to the nearest backward match.
   ESC/C-g cancels and restores the original position."
  (when (screen-copy-mode-p screen)
    (setf *copy-mode-isearch-origin*
          (cons (screen-copy-cursor screen) (screen-copy-offset screen)))
    (cl-tmux/prompt:prompt-start
     "search-backward" ""
     (lambda (term)
       (setf *copy-mode-isearch-origin* nil)
       (when (and term (plusp (length term)))
         (setf (screen-copy-search-term screen) term)))
     :on-change
     (lambda (text)
       (%copy-mode-isearch-from-origin screen text :backward)
       (setf cl-tmux::*dirty* t))
     :on-cancel
     (lambda ()
       (let ((origin *copy-mode-isearch-origin*))
         (setf *copy-mode-isearch-origin* nil)
         (when (and origin (screen-copy-mode-p screen))
           (setf (screen-copy-cursor screen) (car origin)
                 (screen-copy-offset screen) (cdr origin)
                 (screen-dirty-p screen) t)))))))

;;; ── Bracket matching (vi %) ───────────────────────────────────────────────────
;;;
;;; copy_mode_next_matching_bracket(Screen):
;;;   Cursor on ( [ { → scan forward for matching ) ] }.
;;;   Cursor on ) ] } → scan backward for matching ( [ {.
;;;   Cursor on other → find next bracket forward, then match it.
;;;   Both tmux names (next/previous-matching-bracket) call the same function
;;;   because the direction is determined by what character the cursor is on.

(defun %bracket-pair (ch)
  "For bracket CH return (values partner direction) where direction :forward means
   CH is an opener and :backward means CH is a closer.
   Returns (values nil nil) when CH is not a bracket."
  (case ch
    (#\( (values #\) :forward))
    (#\[ (values #\] :forward))
    (#\{ (values #\} :forward))
    (#\) (values #\( :backward))
    (#\] (values #\[ :backward))
    (#\} (values #\{ :backward))
    (t   (values nil nil))))

(defun %bracket-char-p (ch)
  "True when CH is one of the six bracket characters."
  (multiple-value-bind (p d) (%bracket-pair ch)
    (declare (ignore d))
    (not (null p))))

(defun %bracket-scan-forward (screen start-vrow start-col open-ch close-ch)
  "Scan forward from column START-COL+1 of START-VROW for the CLOSE-CH that
   matches the OPEN-CH at the start position.  Respects nesting.
   Moves cursor on success and returns T; returns NIL when not found."
  (let ((total (%copy-mode-total-rows screen))
        (depth 1))
    (loop for vrow from start-vrow below total do
      (let* ((row-str  (%copy-mode-virtual-row-string screen vrow))
             (from-col (if (= vrow start-vrow) (1+ start-col) 0)))
        (loop for col from from-col below (length row-str) do
          (let ((c (char row-str col)))
            (cond ((char= c open-ch)  (incf depth))
                  ((char= c close-ch) (decf depth)
                                       (when (zerop depth)
                                         (%copy-mode-set-virtual-row screen vrow col)
                                         (return-from %bracket-scan-forward t))))))))
    nil))

(defun %bracket-scan-backward (screen start-vrow start-col open-ch close-ch)
  "Scan backward from column START-COL-1 of START-VROW for the OPEN-CH that
   matches the CLOSE-CH at the start position.  Respects nesting.
   Moves cursor on success and returns T; returns NIL when not found."
  (let ((depth 1))
    (loop for vrow from start-vrow downto 0 do
      (let* ((row-str (%copy-mode-virtual-row-string screen vrow))
             (to-col  (if (= vrow start-vrow)
                          (1- start-col)
                          (1- (length row-str)))))
        (loop for col from to-col downto 0 do
          (let ((c (if (< col (length row-str)) (char row-str col) #\Space)))
            (cond ((char= c close-ch) (incf depth))
                  ((char= c open-ch)  (decf depth)
                                       (when (zerop depth)
                                         (%copy-mode-set-virtual-row screen vrow col)
                                         (return-from %bracket-scan-backward t))))))))
    nil))

(defun %find-next-bracket (screen start-vrow start-col)
  "Scan forward from (START-VROW, START-COL) for the first bracket character.
   Returns (values vrow col ch) on success, or (values nil nil nil)."
  (let ((total (%copy-mode-total-rows screen)))
    (loop for vrow from start-vrow below total do
      (let* ((row-str  (%copy-mode-virtual-row-string screen vrow))
             (from-col (if (= vrow start-vrow) start-col 0)))
        (loop for col from from-col below (length row-str) do
          (let ((c (char row-str col)))
            (when (%bracket-char-p c)
              (return-from %find-next-bracket (values vrow col c)))))))
    (values nil nil nil)))

(defun copy-mode-next-matching-bracket (screen)
  "Jump to the bracket matching the char at the cursor (vi %).
   Open bracket → scan forward to close; close bracket → scan backward to open.
   Not on a bracket → find next bracket forward then jump to its match.
   Both next-matching-bracket and previous-matching-bracket map here."
  (when (screen-copy-mode-p screen)
    (let* ((cursor   (or (screen-copy-cursor screen) (cons 0 0)))
           (cur-vrow (%copy-mode-cursor-virtual-row screen))
           (cur-col  (cdr cursor))
           (row-str  (%copy-mode-virtual-row-string screen cur-vrow))
           (ch       (if (< cur-col (length row-str))
                         (char row-str cur-col)
                         #\Space)))
      (multiple-value-bind (partner direction) (%bracket-pair ch)
        (cond
          ((and partner (eq direction :forward))
           (%bracket-scan-forward  screen cur-vrow cur-col ch partner))
          ((and partner (eq direction :backward))
           (%bracket-scan-backward screen cur-vrow cur-col partner ch))
          (t
           ;; Not on a bracket: find the next bracket forward, then match it.
           (multiple-value-bind (next-vrow next-col next-ch)
               (%find-next-bracket screen cur-vrow (1+ cur-col))
             (when next-vrow
               (multiple-value-bind (next-partner next-dir) (%bracket-pair next-ch)
                 (when next-partner
                   (if (eq next-dir :forward)
                       (%bracket-scan-forward  screen next-vrow next-col next-ch next-partner)
                       (%bracket-scan-backward screen next-vrow next-col next-partner next-ch)))))))))
      (setf (screen-dirty-p screen) t))))
