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

(defun %render-box-border-top (stream origin-x origin-y box-width title)
  "Draw the top border of any box.
   ORIGIN-X  — terminal column of the box left edge.
   ORIGIN-Y  — terminal row of the box top edge.
   BOX-WIDTH — total width of the box in columns (includes the two corner chars).
   TITLE     — title label rendered after the top-left corner.
   Emits: ┌ TITLE ──...──┐ truncated to BOX-WIDTH columns."
  (move-to stream origin-y origin-x)
  (write-char #\┌ stream)
  (let* ((inner  (- box-width 2))
         (tlabel (format nil " ~A " title))
         (tlen   (min (length tlabel) inner))
         (fill   (max 0 (- inner tlen))))
    (write-string (subseq tlabel 0 tlen) stream)
    (loop repeat fill do (write-char #\─ stream)))
  (write-char #\┐ stream))

(defun %render-box-border-bottom (stream origin-x bottom-row box-width)
  "Draw the bottom border of a box.
   ORIGIN-X   — terminal column of the box left edge.
   BOTTOM-ROW — terminal row of the bottom border line.
   BOX-WIDTH  — total width of the box in columns.
   Emits: └ ──...──┘"
  (move-to stream bottom-row origin-x)
  (write-char #\└ stream)
  (loop repeat (- box-width 2) do (write-char #\─ stream))
  (write-char #\┘ stream))

;;; ── Popup rendering ─────────────────────────────────────────────────────────

(defun %render-popup-content-pane (stream origin-x origin-y box-width box-height popup-screen)
  "Render the live pane screen inside a popup box interior.
   ORIGIN-X   — terminal column of the box left edge (where │ is drawn).
   ORIGIN-Y   — terminal row of the top border (content starts at ORIGIN-Y+1).
   BOX-WIDTH  — total width of the box (content width = BOX-WIDTH - 2).
   BOX-HEIGHT — total height of the box (content rows = min of BOX-HEIGHT, screen height).
   POPUP-SCREEN — the live screen to render inside the box."
  (loop for row below (min box-height (screen-height popup-screen)) do
    (move-to stream (+ origin-y 1 row) origin-x)
    (write-char #\│ stream)
    (loop for col below (- box-width 2)
          for cell = (screen-display-cell popup-screen col row)
          do (write-char (cell-char cell) stream))
    (write-char #\│ stream)))

(defun %render-popup-content-empty (stream origin-x origin-y box-height box-width)
  "Render empty side bars inside a popup box that has no live pane.
   ORIGIN-X   — terminal column of the box left edge.
   ORIGIN-Y   — terminal row of the top border (content starts at ORIGIN-Y+1).
   BOX-HEIGHT — total height of the box (content rows = BOX-HEIGHT - 2).
   BOX-WIDTH  — total width of the box (content width = BOX-WIDTH - 2)."
  (loop for row below (- box-height 2) do
    (move-to stream (+ origin-y 1 row) origin-x)
    (write-char #\│ stream)
    (loop repeat (- box-width 2) do (write-char #\Space stream))
    (write-char #\│ stream)))

(defun render-popup (stream popup terminal-rows terminal-cols)
  "Draw the POPUP overlay box centered on the terminal.
   When the popup has a live pane, render it inside the box.
   Otherwise render an empty box with the popup title."
  (let* ((box-width  (min (popup-width  popup) terminal-cols))
         (box-height (popup-height popup))
         (origin-x   (max 0 (floor (- terminal-cols box-width) 2)))
         (origin-y   (max 0 (floor (- (1- terminal-rows) box-height) 2)))
         (title      (popup-title popup)))
    (reset-attrs stream)
    (%render-box-border-top stream origin-x origin-y box-width title)
    (if (popup-pane popup)
        (let ((popup-screen (popup-screen popup)))
          (when popup-screen
            (%render-popup-content-pane stream origin-x origin-y
                                        box-width box-height popup-screen)))
        (%render-popup-content-empty stream origin-x origin-y box-height box-width))
    (%render-box-border-bottom stream origin-x (+ origin-y box-height -1) box-width)))

;;; ── Menu rendering ──────────────────────────────────────────────────────────

(defun %render-menu-items (stream origin-x origin-y items box-width selected-index)
  "Draw each menu item row with a selection indicator (▶ for selected, space for others).
   ORIGIN-X       — terminal column of the box left edge.
   ORIGIN-Y       — terminal row of the top border (items start at ORIGIN-Y+1).
   ITEMS          — alist of (label . command) pairs.
   BOX-WIDTH      — total width of the box (item content width = BOX-WIDTH - 3).
   SELECTED-INDEX — 0-based index of the highlighted item."
  (loop for (label . _cmd) in items
        for item-index from 0
        do (move-to stream (+ origin-y 1 item-index) origin-x)
           (write-char #\│ stream)
           (write-char (if (= item-index selected-index) #\▶ #\Space) stream)
           (let* ((inner-width (- box-width 3))
                  (label-len   (min (length label) inner-width))
                  (fill        (max 0 (- inner-width label-len))))
             (write-string (subseq label 0 label-len) stream)
             (loop repeat fill do (write-char #\Space stream)))
           (write-char #\│ stream)))

(defun render-menu (stream menu terminal-rows terminal-cols)
  "Draw the MENU overlay box.  Centred by default, or at MENU-X/MENU-Y when set
   (display-menu -x/-y).  Positions are clamped so the box stays on screen."
  (let* ((items          (menu-items menu))
         (item-count     (length items))
         (title          (menu-title menu))
         (box-width      (min 40 terminal-cols))
         (box-height     (+ item-count 2))
         ;; -x/-y: explicit position when set, else centre.  Clamp so the whole
         ;; box fits on screen (origin in [0, dim - box-extent]).
         (origin-x       (if (menu-x menu)
                             (max 0 (min (menu-x menu) (- terminal-cols box-width)))
                             (max 0 (floor (- terminal-cols box-width) 2))))
         (origin-y       (if (menu-y menu)
                             (max 0 (min (menu-y menu) (- terminal-rows box-height)))
                             (max 0 (floor (- terminal-rows box-height) 2))))
         (selected-index (menu-selected-index menu)))
    (reset-attrs stream)
    (%render-box-border-top    stream origin-x origin-y box-width title)
    (%render-menu-items        stream origin-x origin-y items box-width selected-index)
    (%render-box-border-bottom stream origin-x (+ origin-y item-count 1) box-width)))
