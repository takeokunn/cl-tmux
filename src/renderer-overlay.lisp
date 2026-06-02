(in-package #:cl-tmux/renderer)

;;;; Popup and menu box-drawing for the cl-tmux renderer.
;;;;
;;;; This file provides the concrete rendering functions for floating overlay
;;;; boxes: render-popup (for cl-tmux popup panes) and render-menu (for
;;;; interactive choice lists).  Each is split into border + content helpers
;;;; that renderer.lisp composes via %render-overlay-dispatch.
;;;;
;;;; Load order: renderer-format → renderer-style → renderer-pane
;;;;             → renderer-overlay → renderer
;;;; All files share the cl-tmux/renderer package (no defpackage here).

;;; ── Box-drawing shared helpers ──────────────────────────────────────────────

(defun %render-box-border-top (stream ox oy pw title)
  "Draw the top border of any box at (OX, OY) with width PW and TITLE label.
   Emits: ┌ TITLE ──...──┐ truncated to PW columns."
  (move-to stream oy ox)
  (write-char #\┌ stream)
  (let* ((inner  (- pw 2))
         (tlabel (format nil " ~A " title))
         (tlen   (min (length tlabel) inner))
         (fill   (max 0 (- inner tlen))))
    (write-string (subseq tlabel 0 tlen) stream)
    (loop repeat fill do (write-char #\─ stream)))
  (write-char #\┐ stream))

(defun %render-box-border-bottom (stream ox bottom-row pw)
  "Draw the bottom border of a box at (OX, BOTTOM-ROW) with width PW.
   Emits: └ ──...──┘"
  (move-to stream bottom-row ox)
  (write-char #\└ stream)
  (loop repeat (- pw 2) do (write-char #\─ stream))
  (write-char #\┘ stream))

;;; ── Popup rendering ─────────────────────────────────────────────────────────

(defun %render-popup-content-pane (stream ox oy pw ph popup-screen)
  "Render the live pane screen inside a popup box interior."
  (loop for row below (min ph (screen-height popup-screen)) do
    (move-to stream (+ oy 1 row) ox)
    (write-char #\│ stream)
    (loop for col below (- pw 2)
          for cell = (screen-display-cell popup-screen col row)
          do (write-char (cell-char cell) stream))
    (write-char #\│ stream)))

(defun %render-popup-content-empty (stream ox oy ph pw)
  "Render empty side bars inside a popup box that has no live pane."
  (loop for row below (- ph 2) do
    (move-to stream (+ oy 1 row) ox)
    (write-char #\│ stream)
    (loop repeat (- pw 2) do (write-char #\Space stream))
    (write-char #\│ stream)))

(defun render-popup (stream popup terminal-rows terminal-cols)
  "Draw the POPUP overlay box centered on the terminal.
   When the popup has a live pane, render it inside the box.
   Otherwise render an empty box with the popup title."
  (let* ((pw    (min (popup-width  popup) terminal-cols))
         (ph    (popup-height popup))
         (ox    (max 0 (floor (- terminal-cols pw) 2)))
         (oy    (max 0 (floor (- (1- terminal-rows) ph) 2)))
         (title (popup-title popup)))
    (reset-attrs stream)
    (%render-box-border-top stream ox oy pw title)
    (if (popup-pane popup)
        (let ((sc (popup-screen popup)))
          (when sc
            (%render-popup-content-pane stream ox oy pw ph sc)))
        (%render-popup-content-empty stream ox oy ph pw))
    (%render-box-border-bottom stream ox (+ oy ph -1) pw)))

;;; ── Menu rendering ──────────────────────────────────────────────────────────

(defun %render-menu-items (stream ox oy items pw sel)
  "Draw each menu item row with a selection indicator (▶ for selected, space for others)."
  (loop for (label . _cmd) in items
        for i from 0
        do (move-to stream (+ oy 1 i) ox)
           (write-char #\│ stream)
           (write-char (if (= i sel) #\▶ #\Space) stream)
           (let* ((inner (- pw 3))
                  (llen  (min (length label) inner))
                  (fill  (max 0 (- inner llen))))
             (write-string (subseq label 0 llen) stream)
             (loop repeat fill do (write-char #\Space stream)))
           (write-char #\│ stream)))

(defun render-menu (stream menu terminal-rows terminal-cols)
  "Draw the MENU overlay box centered on the terminal."
  (let* ((items  (menu-items menu))
         (n      (length items))
         (title  (menu-title menu))
         (pw     (min 40 terminal-cols))
         (ox     (max 0 (floor (- terminal-cols pw) 2)))
         (oy     (max 0 (floor (- terminal-rows (+ n 2)) 2)))
         (sel    (menu-selected-index menu)))
    (reset-attrs stream)
    (%render-box-border-top    stream ox oy pw title)
    (%render-menu-items        stream ox oy items pw sel)
    (%render-box-border-bottom stream ox (+ oy n 1) pw)))
