(in-package #:cl-tmux/commands)

;;; Copy-mode paragraph motion (vi { and }).

(defun %copy-mode-row-blank-p (screen vrow)
  "Return T if VROW (virtual row, 0 = oldest scrollback) is entirely blank."
  (every (lambda (ch)
           (or (char= ch #\Space)
               (char= ch (code-char 0))))
         (%extract-vrow-chars screen vrow 0 (screen-width screen))))

(defun %cursor-vrow (screen)
  "Return the virtual row of the current copy-mode cursor."
  (%copy-mode-cursor-vrow screen))

(defun %set-cursor-vrow (screen vrow)
  "Move the copy-mode cursor to VROW (virtual row), adjusting the viewport offset as needed."
  (let* ((sb-n    (length (screen-scrollback screen)))
         (h       (screen-height screen))
         (total   (+ sb-n h))
         (clamped (max 0 (min (1- total) vrow)))
         (offset  (screen-copy-offset screen))
         (col     (cdr (screen-copy-cursor screen)))
         (nat-row (+ clamped (- sb-n) offset)))
    (if (and (>= nat-row 0) (< nat-row h))
        (setf (screen-copy-cursor screen) (cons nat-row col))
        (let* ((desired  (floor h 2))
               (new-off  (max 0 (min sb-n (+ desired sb-n (- clamped)))))
               (new-row  (max 0 (min (1- h) (+ clamped (- sb-n) new-off)))))
          (setf (screen-copy-offset screen) new-off
                (screen-copy-cursor screen)  (cons new-row col))))
    (setf (screen-dirty-p screen) t)))

(defun %find-paragraph-boundary (screen start direction total)
  "Scan from START in DIRECTION (:up or :down) for the nearest blank vrow."
  (if (eq direction :up)
      (loop for vrow downfrom start to 0
            when (%copy-mode-row-blank-p screen vrow) return vrow
            finally (return 0))
      (loop for vrow from start below total
            when (%copy-mode-row-blank-p screen vrow) return vrow
            finally (return (1- total)))))

(defun copy-mode-previous-paragraph (screen)
  "Jump to the nearest blank-line paragraph boundary above (vi {)."
  (with-copy-mode-dirty screen
    (%set-cursor-vrow screen
                      (%find-paragraph-boundary screen (1- (%cursor-vrow screen)) :up 0))))

(defun copy-mode-next-paragraph (screen)
  "Jump to the nearest blank-line paragraph boundary below (vi })."
  (with-copy-mode-dirty screen
    (let* ((sb-n  (length (screen-scrollback screen)))
           (total (+ sb-n (screen-height screen))))
      (%set-cursor-vrow screen
                        (%find-paragraph-boundary screen (1+ (%cursor-vrow screen)) :down total)))))
