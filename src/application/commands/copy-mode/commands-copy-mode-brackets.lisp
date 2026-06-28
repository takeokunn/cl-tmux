(in-package #:cl-tmux/commands)

;;;; Copy-mode bracket matching helpers.

;;; ── Bracket matching (vi %) ───────────────────────────────────────────────────
;;;
;;; copy_mode_next_matching_bracket(Screen):
;;;   Cursor on ( [ { → scan forward for matching ) ] }.
;;;   Cursor on ) ] } → scan backward for matching ( [ {.
;;;   Cursor on other → find next bracket forward, then match it.
;;; copy_mode_previous_matching_bracket(Screen):
;;;   Cursor on ) ] } → scan backward for matching ( [ {.
;;;   Cursor on other → find previous close bracket backward, then match it.

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

(defun %closing-bracket-p (ch)
  "True when CH is one of the three closing bracket characters."
  (multiple-value-bind (p d) (%bracket-pair ch)
    (and p (eq d :backward))))

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

(defun %find-previous-closing-bracket (screen start-vrow start-col)
  "Scan backward from (START-VROW, START-COL) for the first closing bracket.
   Returns (values vrow col ch) on success, or (values nil nil nil)."
  (loop for vrow from start-vrow downto 0 do
    (let* ((row-str (%copy-mode-virtual-row-string screen vrow))
           (to-col  (if (= vrow start-vrow)
                        (min start-col (1- (length row-str)))
                        (1- (length row-str)))))
      (loop for col from to-col downto 0 do
        (let ((c (char row-str col)))
            (when (%closing-bracket-p c)
              (return-from %find-previous-closing-bracket (values vrow col c)))))))
  (values nil nil nil))

(defun %copy-mode-bracket-state (screen)
  "Return the current copy-mode cursor state as (values cursor vrow col row-str ch)."
  (let* ((cursor   (or (screen-copy-cursor screen) (cons 0 0)))
         (cur-vrow (%copy-mode-cursor-virtual-row screen))
         (cur-col  (cdr cursor))
         (row-str  (%copy-mode-virtual-row-string screen cur-vrow))
         (ch       (if (< cur-col (length row-str))
                       (char row-str cur-col)
                       #\Space)))
    (values cursor cur-vrow cur-col row-str ch)))

(defun %copy-mode-match-bracket-at (screen vrow col ch)
  "Jump from bracket CH at VROW/COL to its matching bracket."
  (multiple-value-bind (partner direction) (%bracket-pair ch)
    (cond
      ((and partner (eq direction :forward))
       (%bracket-scan-forward screen vrow col ch partner))
      ((and partner (eq direction :backward))
       (%bracket-scan-backward screen vrow col partner ch))
      (t nil))))

(defun %copy-mode-match-next-bracket (screen cur-vrow cur-col)
  "Find the next bracket after CUR-VROW/CUR-COL and jump to its match."
  (multiple-value-bind (next-vrow next-col next-ch)
      (%find-next-bracket screen cur-vrow (1+ cur-col))
    (when next-vrow
      (%copy-mode-match-bracket-at screen next-vrow next-col next-ch))))

(defun %copy-mode-match-previous-closing-bracket-at (screen vrow col ch)
  "Jump backward from closing bracket CH at VROW/COL to its opener."
  (multiple-value-bind (partner direction) (%bracket-pair ch)
    (when (and partner (eq direction :backward))
      (%bracket-scan-backward screen vrow col partner ch))))

(defun %copy-mode-match-previous-closing-bracket (screen cur-vrow cur-col)
  "Find the previous closing bracket before CUR-VROW/CUR-COL and jump backward."
  (multiple-value-bind (prev-vrow prev-col prev-ch)
      (%find-previous-closing-bracket screen cur-vrow (1- cur-col))
    (when prev-vrow
      (%copy-mode-match-previous-closing-bracket-at screen prev-vrow prev-col prev-ch))))

(defun copy-mode-next-matching-bracket (screen)
  "Jump to the bracket matching the char at the cursor (vi %).
   Open bracket → scan forward to close; close bracket → scan backward to open.
   Not on a bracket → find next bracket forward then jump to its match."
  (when (screen-copy-mode-p screen)
    (multiple-value-bind (cursor cur-vrow cur-col row-str ch)
        (%copy-mode-bracket-state screen)
      (declare (ignore cursor row-str))
      (or (%copy-mode-match-bracket-at screen cur-vrow cur-col ch)
          (%copy-mode-match-next-bracket screen cur-vrow cur-col))
      (setf (screen-dirty-p screen) t))))

(defun copy-mode-previous-matching-bracket (screen)
  "Jump backward to the opener matching the current or previous close bracket.
   Cursor on a close bracket scans backward from it.  Otherwise the previous
   close bracket is found first, then matched backward."
  (when (screen-copy-mode-p screen)
    (multiple-value-bind (cursor cur-vrow cur-col row-str ch)
        (%copy-mode-bracket-state screen)
      (declare (ignore cursor row-str))
      (or (%copy-mode-match-previous-closing-bracket-at screen cur-vrow cur-col ch)
          (%copy-mode-match-previous-closing-bracket screen cur-vrow cur-col))
      (setf (screen-dirty-p screen) t))))
