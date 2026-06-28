(in-package #:cl-tmux/commands)

;;;; Copy-mode virtual-buffer helpers.

;;; ── Virtual buffer helpers ───────────────────────────────────────────────────

(defun %copy-mode-total-rows (screen)
  "Total row count in the virtual buffer (scrollback + live grid)."
  (+ (length (screen-scrollback screen)) (screen-height screen)))

(defun %copy-mode-virtual-row-string (screen vrow)
  "Content of virtual row VROW as a string.
   VROW 0 = oldest scrollback; VROW (total-1) = bottom of live grid."
  (%extract-vrow-chars screen vrow 0 (screen-width screen)))

;;; %copy-mode-cursor-vrow (canonical) is defined in commands-copy-mode-selection.lisp.

(defun %copy-mode-set-virtual-row (screen vrow col)
  "Position the copy-mode cursor at (VROW, COL), adjusting offset so VROW is visible."
  (let* ((sb-n   (length (screen-scrollback screen)))
         (offset (max 0 (min sb-n (- sb-n vrow))))
         (crow   (+ vrow offset (- sb-n))))
    (setf (screen-copy-offset screen) offset
          (screen-copy-cursor screen)  (cons crow col)
          (screen-dirty-p screen) t)))
