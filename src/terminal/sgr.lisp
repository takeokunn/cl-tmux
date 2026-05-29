(in-package #:cl-tmux/terminal/sgr)

;;;; SGR (Select Graphic Rendition) macro-driven dispatch.
;;;;
;;;; define-sgr-rules builds a COND-based dispatcher from a list of
;;;; (condition &body forms) clauses.  The public entry point apply-sgr
;;;; iterates over a params list and calls %dispatch-sgr-code for each value.

;;; ── Macro ──────────────────────────────────────────────────────────────────

(defmacro define-sgr-rules (&rest rules)
  "Each RULE is (condition-form &body forms).
   Available bindings in each rule: SCREEN (the screen struct), P (the SGR
   parameter integer).
   Expands into a DEFUN for %DISPATCH-SGR-CODE that dispatches via COND."
  `(defun %dispatch-sgr-code (screen p)
     (declare (type screen screen) (type fixnum p) (ignorable p))
     (cond
       ,@(mapcar (lambda (rule)
                   (destructuring-bind (condition &rest body) rule
                     `(,condition ,@body)))
                 rules)
       (t (values)))))

;;; ── SGR rule table ─────────────────────────────────────────────────────────

(define-sgr-rules
  ;; SGR 0 – reset all attributes to defaults
  ((= p 0)
   (setf (screen-cur-fg    screen) 7
         (screen-cur-bg    screen) 0
         (screen-cur-attrs screen) 0))

  ;; SGR 1 – bold on
  ((= p 1)
   (setf (screen-cur-attrs screen)
         (logior (screen-cur-attrs screen) +attr-bold+)))

  ;; SGR 2 – dim on
  ((= p 2)
   (setf (screen-cur-attrs screen)
         (logior (screen-cur-attrs screen) +attr-dim+)))

  ;; SGR 3 – italic (treated as dim for compatibility)
  ((= p 3)
   (setf (screen-cur-attrs screen)
         (logior (screen-cur-attrs screen) +attr-dim+)))

  ;; SGR 4 – underline on
  ((= p 4)
   (setf (screen-cur-attrs screen)
         (logior (screen-cur-attrs screen) +attr-underline+)))

  ;; SGR 5 – blink on
  ((= p 5)
   (setf (screen-cur-attrs screen)
         (logior (screen-cur-attrs screen) +attr-blink+)))

  ;; SGR 7 – reverse video on
  ((= p 7)
   (setf (screen-cur-attrs screen)
         (logior (screen-cur-attrs screen) +attr-reverse+)))

  ;; SGR 22 – bold + dim off
  ((= p 22)
   (setf (screen-cur-attrs screen)
         (logand (screen-cur-attrs screen)
                 (lognot (logior +attr-bold+ +attr-dim+)))))

  ;; SGR 24 – underline off
  ((= p 24)
   (setf (screen-cur-attrs screen)
         (logand (screen-cur-attrs screen) (lognot +attr-underline+))))

  ;; SGR 25 – blink off
  ((= p 25)
   (setf (screen-cur-attrs screen)
         (logand (screen-cur-attrs screen) (lognot +attr-blink+))))

  ;; SGR 27 – reverse video off
  ((= p 27)
   (setf (screen-cur-attrs screen)
         (logand (screen-cur-attrs screen) (lognot +attr-reverse+))))

  ;; SGR 30-37 – standard foreground colours 0-7
  ((<= 30 p 37)
   (setf (screen-cur-fg screen) (- p 30)))

  ;; SGR 39 – default foreground colour
  ((= p 39)
   (setf (screen-cur-fg screen) 7))

  ;; SGR 40-47 – standard background colours 0-7
  ((<= 40 p 47)
   (setf (screen-cur-bg screen) (- p 40)))

  ;; SGR 49 – default background colour
  ((= p 49)
   (setf (screen-cur-bg screen) 0))

  ;; SGR 90-97 – bright (high-intensity) foreground colours 8-15
  ((<= 90 p 97)
   (setf (screen-cur-fg screen) (+ 8 (- p 90))))

  ;; SGR 100-107 – bright (high-intensity) background colours 8-15
  ((<= 100 p 107)
   (setf (screen-cur-bg screen) (+ 8 (- p 100)))))

;;; ── Public entry point ─────────────────────────────────────────────────────

(defun apply-sgr (screen params)
  "Apply a sequence of SGR codes to SCREEN.
   PARAMS is a list of fixnum SGR parameter values; an empty or nil list is
   treated as (0) (i.e. a plain SGR reset)."
  (dolist (p (or params '(0)))
    (%dispatch-sgr-code screen p)))
