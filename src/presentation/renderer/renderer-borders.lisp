(in-package #:cl-tmux/renderer)

;;;; Split-tree separator and pane border rendering.

;;; ── Split-tree separators ───────────────────────────────────────────────────

(defun layout-subtree-rect (node)
  "Bounding rectangle of NODE's leaves as a plist (:x :y :w :h), derived from the
   already-laid-out pane geometry."
  (let* ((panes (layout-leaves node))
         (min-x (reduce #'min panes :key #'pane-x))
         (min-y (reduce #'min panes :key #'pane-y))
         (max-x (reduce #'max panes :key (lambda (p) (+ (pane-x p) (pane-width p)))))
         (max-y (reduce #'max panes :key (lambda (p) (+ (pane-y p) (pane-height p))))))
    (list :x min-x :y min-y :w (- max-x min-x) :h (- max-y min-y))))

(defun subtree-contains-p (node pane)
  "True when PANE is a leaf of NODE's subtree."
  (and pane (member pane (layout-leaves node))))

;;; ── Border style SGR helpers ────────────────────────────────────────────────

(defun %apply-border-style (stream style-string)
  "Emit the SGR code(s) for a pane border.
   Supported format: \"default\" → reset, \"fg=COLOR\" → foreground colour only."
  (cond
    ((or (null style-string)
         (string-equal style-string "default"))
     (reset-attrs stream))
    ((and (>= (length style-string) 3)
          (string-equal (subseq style-string 0 3) "fg="))
     (let* ((color-name (subseq style-string 3))
            (code       (%border-color-sgr color-name)))
       (reset-attrs stream)
       (when code
         (format stream "~C[~Dm" +esc+ code))))
    (t (reset-attrs stream))))

;;; ── Pane border character dispatch table (Prolog-like fact table) ───────────
;;;
;;; define-pane-border-chars-table builds %dispatch-pane-border-chars from a
;;; declarative (style-string vertical horizontal) fact table, following the
;;; define-border-charset-table pattern used in renderer-style.lisp.
;;; Unknown styles (including "number"/"padded") fall back to single-line glyphs.

(defmacro define-pane-border-chars-table (&rest rules)
  "Build %DISPATCH-PANE-BORDER-CHARS from a declarative (style-str vertical horizontal)
   fact table.  Unknown styles fall back to single-line box-drawing characters."
  `(defun %dispatch-pane-border-chars (style)
     "Return (values VERTICAL HORIZONTAL) border glyphs for pane border STYLE string.
      Falls back to single-line characters for unrecognised styles."
     (cond
       ,@(mapcar (lambda (rule)
                   (destructuring-bind (style-str vertical horizontal) rule
                     `((string-equal style ,style-str) (values ,vertical ,horizontal))))
                 rules)
       (t (values #\│ #\─)))))

(define-pane-border-chars-table
  ("double" #\║ #\═)
  ("heavy"  #\┃ #\━)
  ("simple" #\| #\-))

;;; ── Separator renderers (data layer — what each orientation draws) ──────────

(defun %pane-border-chars ()
  "Return (values VERTICAL HORIZONTAL) border glyphs for the pane-border-lines option.
   Delegates to %dispatch-pane-border-chars with the current option value.
   Unknown values — including number/padded — fall back to single-line glyphs."
  (%dispatch-pane-border-chars
   (cl-tmux/options:get-option "pane-border-lines" "single")))

(defun %border-indicators-colour-p ()
  "T unless pane-border-indicators is \"off\".  cl-tmux colours the active pane's
   border for \"colour\" (the default), \"both\", and \"arrows\" (the arrow glyphs
   are not drawn, so \"arrows\" degrades to colour); \"off\" disables the highlight."
  (string/= (cl-tmux/options:get-option "pane-border-indicators" "colour") "off"))

(defun %render-h-separator (stream node active-pane terminal-cols)
  "Draw the vertical column between the left and right children of an :h split.
   Glyph follows pane-border-lines; colour follows pane-border-style /
   pane-active-border-style (suppressed when pane-border-indicators is \"off\")."
  (let* ((a          (layout-split-first  node))
         (b          (layout-split-second node))
         (rect       (layout-subtree-rect a))
         (border-col (+ (getf rect :x) (getf rect :w)))
         ;; pane-border-indicators "off" suppresses the active-border highlight.
         (activep    (and (or (subtree-contains-p a active-pane)
                              (subtree-contains-p b active-pane))
                          (%border-indicators-colour-p)))
         (style      (if activep
                         (cl-tmux/options:get-option "pane-active-border-style" "fg=green")
                         (cl-tmux/options:get-option "pane-border-style" "default"))))
    (when (< border-col terminal-cols)
      (%apply-border-style stream style)
      (let ((v-char (%pane-border-chars)))
        (loop for row from (getf rect :y) below (+ (getf rect :y) (getf rect :h))
              do (move-to stream row border-col)
                 (write-char v-char stream)))
      (reset-attrs stream))))

(defun %render-v-separator (stream node terminal-cols)
  "Draw the horizontal row between the top and bottom children of a :v split.
   Glyph follows pane-border-lines."
  (let* ((rect       (layout-subtree-rect (layout-split-first node)))
         (border-row (+ (getf rect :y) (getf rect :h)))
         (x          (getf rect :x))
         (w          (min (getf rect :w) (- terminal-cols x))))
    (reset-attrs stream)
    (move-to stream border-row x)
    (multiple-value-bind (v-char h-char) (%pane-border-chars)
      (declare (ignore v-char))
      (loop repeat (max 0 w) do (write-char h-char stream)))))

;;; ── Pane border status line ──────────────────────────────────────────────────
;;;
;;; When pane-border-status is "top" or "bottom", each pane displays a title
;;; line on its border row showing the pane-border-format expansion.

(defun %render-pane-border-status (stream pane session win)
  "Render the pane-border-status title for PANE when pane-border-status is
   \"top\" or \"bottom\".  Expands pane-border-format as a format string.
   Does nothing when pane-border-status is \"off\" (the default)."
  (let ((status (cl-tmux/options:get-option "pane-border-status" "off")))
    (unless (member status '("off" "") :test #'string=)
      (let* ((fmt  (cl-tmux/options:get-option "pane-border-format" " #{pane_index} "))
             (ctx  (cl-tmux/format:format-context-from-session session win pane))
             (text (cl-tmux/format:expand-format-safe
                    fmt ctx (format nil " ~D " (pane-id pane))))
             ;; Truncate to pane width
             (maxw (pane-width pane))
             (disp (if (> (length text) maxw) (subseq text 0 maxw) text))
             ;; The title is drawn on the row RESERVED by pane-reposition, just
             ;; OUTSIDE the content rectangle — so it never overwrites pane
             ;; content: "top" on the row above (pane-y - 1), "bottom" on the row
             ;; below (pane-y + pane-height).  Clamped to row >= 0 for safety.
             (row  (max 0 (if (string= status "top")
                              (1- (pane-y pane))
                              (+ (pane-y pane) (pane-height pane))))))
        (reset-attrs stream)
        (move-to stream row (pane-x pane))
        ;; Overwrite cells with the status text (no SGR for now)
        (write-string disp stream)))))

;;; ── Tree border walk (logic layer) ──────────────────────────────────────────

(defun render-tree-borders (stream node active-pane terminal-cols)
  "Walk the split-tree NODE, drawing one separator per internal split node.
   :h (left|right) splits draw │ bars; :v (top/bottom) splits draw ─ bars.
   Recurses into both children after drawing the parent separator."
  (when (layout-split-p node)
    (ecase (layout-split-orientation node)
      (:h (%render-h-separator stream node active-pane terminal-cols))
      (:v (%render-v-separator stream node terminal-cols)))
    (render-tree-borders stream (layout-split-first  node) active-pane terminal-cols)
    (render-tree-borders stream (layout-split-second node) active-pane terminal-cols)))
