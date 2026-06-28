(in-package #:cl-tmux/renderer)

;;;; Copy-mode line-number gutter rendering.

(defun %copy-mode-line-number-mode ()
  "Return the current copy-mode line-number mode string."
  (string-downcase
   (string-trim " " (cl-tmux/options:get-option "copy-mode-line-numbers" "off"))))

(defun %copy-mode-line-number-style-spec (style-string)
  "Return NIL, :DEFAULT, or an SGR string for STYLE-STRING.
   Blank strings do not fall back to the status-bar default."
  (let ((trimmed (and style-string (string-trim " " style-string))))
    (cond
      ((or (null trimmed) (zerop (length trimmed))) nil)
      ((string-equal trimmed "default") :default)
      (t (let ((parsed (parse-style-string trimmed)))
           (and parsed (style-to-sgr parsed)))))))

(defun %copy-mode-cursor-row (screen)
  "Return the copy-mode cursor row, or NIL when the cursor is unset."
  (car (screen-copy-cursor screen)))

(defun %copy-mode-cursor-row-p (screen row)
  "True when ROW is the current copy-mode cursor row."
  (let ((cursor-row (%copy-mode-cursor-row screen)))
    (and cursor-row (= row cursor-row))))

(defun %copy-mode-absolute-line-number (screen row)
  "Absolute line number for ROW in SCREEN's copy-mode viewport."
  (+ (length (screen-scrollback screen))
     (- row (screen-copy-offset screen))
     1))

(defun %copy-mode-relative-line-number (screen row)
  "Relative line number for ROW in SCREEN's copy-mode viewport."
  (let ((cursor-row (%copy-mode-cursor-row screen)))
    (if cursor-row
        (abs (- row cursor-row))
        row)))

(defun %copy-mode-line-number-value (screen row mode)
  "Return the numeric line label for ROW under MODE."
  (cond
    ((string-equal mode "default") row)
    ((string-equal mode "absolute")
     (%copy-mode-absolute-line-number screen row))
    ((string-equal mode "relative")
     (%copy-mode-relative-line-number screen row))
    ((string-equal mode "hybrid")
     (if (%copy-mode-cursor-row-p screen row)
         (%copy-mode-absolute-line-number screen row)
         (%copy-mode-relative-line-number screen row)))
    (t nil)))

(defun %copy-mode-line-number-text (screen row mode)
  "Return the printable line-label string for ROW under MODE."
  (let ((value (%copy-mode-line-number-value screen row mode)))
    (when value
      (princ-to-string value))))

(defun %copy-mode-line-number-row-style-sgr (screen row base-style-sgr current-style-sgr)
  "Return the SGR string to use for ROW's line-number gutter."
  (if (%copy-mode-cursor-row-p screen row)
      (or current-style-sgr base-style-sgr)
      base-style-sgr))

(defun %copy-mode-line-number-field (text width)
  "Right-align TEXT in WIDTH columns, clipping from the left when necessary."
  (cond
    ((<= width 0) "")
    ((null text) (make-string width :initial-element #\Space))
    (t (let* ((len (length text))
              (clipped (if (> len width)
                           (subseq text (- len width))
                           text))
              (pad (max 0 (- width (length clipped)))))
         (concatenate 'string
                      (make-string pad :initial-element #\Space)
                      clipped)))))

(defun %copy-mode-line-number-gutter-width (screen pane-height pane-width)
  "Return the reserved gutter width for copy-mode line numbers."
  (let ((mode (%copy-mode-line-number-mode)))
    (cond
      ((or (<= pane-width 0)
           (screen-copy-mode-entered-by-mouse-p screen)
           (string-equal mode "off"))
       0)
      (t (let ((max-number-width
                (loop for row below pane-height
                      for text = (%copy-mode-line-number-text screen row mode)
                      maximize (length text) into width
                      finally (return width))))
           (min pane-width
                (if (plusp max-number-width)
                    (1+ max-number-width)
                    0)))))))

(defun %copy-mode-pane-geometry (screen origin-x pane-height pane-width)
  "Return the copy-mode gutter width, content origin, and content width."
  (let ((gutter-width (%copy-mode-line-number-gutter-width screen pane-height pane-width)))
    (values gutter-width
            (+ origin-x gutter-width)
            (max 0 (- pane-width gutter-width)))))

(defun %render-copy-mode-line-number-row (stream screen row origin-x origin-y gutter-width
                                         base-style-sgr current-style-sgr mode)
  "Render the copy-mode line-number gutter for one ROW."
  (let ((text (%copy-mode-line-number-text screen row mode)))
    (when text
      (move-to stream (+ origin-y row) origin-x)
      (reset-attrs stream)
      (let ((row-style-sgr (%copy-mode-line-number-row-style-sgr screen row
                                                                 base-style-sgr
                                                                 current-style-sgr)))
        (when (stringp row-style-sgr)
          (write-string (format nil "~C[~Am" #\Escape row-style-sgr) stream)))
      (write-string (%copy-mode-line-number-field text gutter-width) stream)
      (reset-attrs stream))))
