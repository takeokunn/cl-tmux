(in-package #:cl-tmux/terminal/csi)

;;;; CSI parameter interpretation.
;;;;
;;;; These helpers translate terminal-protocol parameter facts into the scalar
;;;; values expected by the screen action layer.  They stay separate from the
;;;; rule table so CSI actions remain declarative.

(declaim (inline %csi-leading-int))
(defun %csi-leading-int (param)
  "Return the leading integer of CSI PARAM for scalar P1/P2 bindings.
   A param carrying colon sub-parameters arrives as a list (sub0 sub1 ...);
   non-SGR handlers want only its leading value.  A plain integer is returned
   as-is, and NIL maps to 0.  APPLY-SGR keeps the raw list so it can apply
   colon-form extended colour."
  (cond ((consp param)    (or (first param) 0))
        ((integerp param) param)
        (t 0)))

(declaim (inline %csi-decstbm-params))
(defun %csi-decstbm-params (screen p1 p2)
  "Convert 1-based DECSTBM CSI parameters P1 and P2 to the 0-based inclusive
   (top bottom) pair expected by ACTIONS:DECSTBM.
   P2 = 0 means full screen: the bottom margin defaults to height-1.
   When top >= bottom (invalid margins), reset to full-screen (VT100 behaviour)."
  (let* ((top    (1- (max 1 p1)))
         (bottom (if (zerop p2) (1- (screen-height screen)) (1- p2))))
    (if (>= top bottom)
        (values 0 (1- (screen-height screen)))
        (values top bottom))))

(defun %cup-row (screen p1)
  "Translate a 1-based CUP/HVP row P1 to a 0-based screen row, honoring DECOM
   origin mode (?6): when set, the row is relative to the scroll-region top and
   clamped to the scroll region; otherwise it is absolute."
  (if (screen-origin-mode screen)
      (min (+ (screen-scroll-top screen) (1- p1)) (screen-scroll-bottom screen))
      (1- p1)))
