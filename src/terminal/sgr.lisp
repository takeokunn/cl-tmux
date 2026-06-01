(in-package #:cl-tmux/terminal/sgr)

;;;; SGR (Select Graphic Rendition) macro-driven dispatch.
;;;;
;;;; define-sgr-rules builds a COND-based dispatcher from a list of
;;;; (condition &body forms) clauses.  The public entry point apply-sgr
;;;; iterates over a params list and calls %dispatch-sgr-code for each value.

;;; ── Attribute mutation helpers (data layer) ────────────────────────────────
;;;
;;; These three inline functions separate the HOW (bit manipulation) from the
;;; WHAT (which SGR code means what), keeping the rule table below readable.

(declaim (inline reset-sgr attr-on attr-off attr2-on attr2-off))

(defun reset-sgr (screen)
  "Reset all SGR attributes to their default values."
  (setf (screen-cur-fg       screen) 7
        (screen-cur-bg       screen) 0
        (screen-cur-attrs    screen) 0
        (screen-cur-attrs2   screen) 0
        (screen-cur-ul-color screen) 0))

(defun attr-on (screen bit)
  "Enable SGR attribute BIT on SCREEN."
  (setf (screen-cur-attrs screen)
        (logior (screen-cur-attrs screen) bit)))

(defun attr-off (screen bit)
  "Disable SGR attribute BIT on SCREEN."
  (setf (screen-cur-attrs screen)
        (logand (screen-cur-attrs screen) (lognot bit))))

(defun attr2-on (screen bit)
  "Enable extended SGR attribute BIT (in cur-attrs2) on SCREEN."
  (setf (screen-cur-attrs2 screen)
        (logior (screen-cur-attrs2 screen) bit)))

(defun attr2-off (screen bit)
  "Disable extended SGR attribute BIT (in cur-attrs2) on SCREEN."
  (setf (screen-cur-attrs2 screen)
        (logand (screen-cur-attrs2 screen) (lognot bit))))

;;; ── Macro (logic layer) ─────────────────────────────────────────────────────

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
;;;
;;; Each rule reads as a Prolog clause:
;;;   sgr(0, Screen)   :- reset_sgr(Screen).
;;;   sgr(3, Screen)   :- attr_on(Screen, italic).
;;;   ...

(define-sgr-rules
  ;; ── Reset ─────────────────────────────────────────────────────────────────
  ((= p 0)   (reset-sgr screen))

  ;; ── Attributes on ─────────────────────────────────────────────────────────
  ((= p 1)   (attr-on screen +attr-bold+))
  ((= p 2)   (attr-on screen +attr-dim+))
  ((= p 3)   (attr-on screen +attr-italic+))
  ((= p 4)   (attr-on screen +attr-underline+))
  ((= p 5)   (attr-on screen +attr-blink+))
  ((= p 6)   (attr-on screen +attr-blink+))       ; rapid blink — mapped to blink bit
  ((= p 7)   (attr-on screen +attr-reverse+))
  ((= p 8)   (attr-on screen +attr-conceal+))
  ((= p 9)   (attr-on screen +attr-strikethrough+))
  ((= p 21)  (attr2-on screen +attr2-double-underline+))  ; doubly underlined

  ;; ── Framed / encircled / overlined (51-55) ────────────────────────────────
  ;; SGR 53 = overline on, SGR 55 = overline off.
  ;; SGR 51 (framed) and 52 (encircled) are silently accepted.
  ((= p 53)  (attr2-on  screen +attr2-overline+))
  ((= p 55)  (attr2-off screen +attr2-overline+))
  ((<= 51 p 52) (values))

  ;; ── Attributes off ────────────────────────────────────────────────────────
  ((= p 22)  (attr-off screen (logior +attr-bold+ +attr-dim+)))
  ((= p 23)  (attr-off screen +attr-italic+))
  ((= p 24)  (progn (attr-off  screen +attr-underline+)
                    (attr2-off screen +attr2-double-underline+)))
  ((= p 25)  (attr-off screen +attr-blink+))
  ((= p 27)  (attr-off screen +attr-reverse+))
  ((= p 28)  (attr-off screen +attr-conceal+))
  ((= p 29)  (attr-off screen +attr-strikethrough+))
  ;; SGR 59: reset underline color to default
  ((= p 59)  (setf (screen-cur-ul-color screen) 0))

  ;; ── Standard foreground (30-37) + default (39) ────────────────────────────
  ((<= 30 p 37)  (setf (screen-cur-fg screen) (- p 30)))
  ((= p 39)      (setf (screen-cur-fg screen) 7))

  ;; ── Standard background (40-47) + default (49) ────────────────────────────
  ((<= 40 p 47)  (setf (screen-cur-bg screen) (- p 40)))
  ((= p 49)      (setf (screen-cur-bg screen) 0))

  ;; ── Bright foreground (90-97) ─────────────────────────────────────────────
  ((<= 90 p 97)    (setf (screen-cur-fg screen) (+ 8 (- p 90))))

  ;; ── Bright background (100-107) ───────────────────────────────────────────
  ((<= 100 p 107)  (setf (screen-cur-bg screen) (+ 8 (- p 100)))))

;;; ── Public entry point ─────────────────────────────────────────────────────
;;;
;;; apply-sgr handles compound multi-parameter codes by consuming params ahead
;;; of the single-code dispatcher.  The recursive labels form is a CPS-like
;;; left-fold over the parameter list:
;;;   apply_sgr([], Screen)         :- true.
;;;   apply_sgr([38,5,N|T], S)      :- set_fg_256(S, N),        apply_sgr(T, S).
;;;   apply_sgr([38,2,R,G,B|T], S)  :- set_fg_truecolor(S,R,G,B), apply_sgr(T, S).
;;;   apply_sgr([48,5,N|T], S)      :- set_bg_256(S, N),        apply_sgr(T, S).
;;;   apply_sgr([48,2,R,G,B|T], S)  :- set_bg_truecolor(S,R,G,B), apply_sgr(T, S).
;;;   apply_sgr([P|T], S)           :- dispatch_sgr(S, P),      apply_sgr(T, S).

;;; %set-truecolor encodes a 38;2;R;G;B or 48;2;R;G;B run into #x1RRGGBB and
;;; stores it via the supplied SETTER closure.  Using a closure avoids
;;; duplicating the clamp/logior arithmetic for the fg and bg arms.

(declaim (inline %set-truecolor))
(defun %set-truecolor (screen setter ps)
  "Encode the R;G;B triple at positions 3-5 of PS as #x1RRGGBB and call
   SETTER with (SCREEN value) to store the result.  SETTER should be one of
   #'(setf screen-cur-fg) or #'(setf screen-cur-bg).
   Returns the tail of PS after the five consumed parameters."
  (let* ((r (clamp (or (third  ps) 0) 0 255))
         (g (clamp (or (fourth ps) 0) 0 255))
         (b (clamp (or (fifth  ps) 0) 0 255)))
    (funcall setter (logior #x1000000 (ash r 16) (ash g 8) b) screen))
  (nthcdr 5 ps))

(defun apply-sgr (screen params)
  "Apply a sequence of SGR codes to SCREEN.
   PARAMS is a list of fixnum SGR parameter values; an empty or nil list is
   treated as (0) (i.e. a plain SGR reset).
   Multi-parameter codes handled as a unit:
     38;5;N / 48;5;N   — 256-color fg/bg (N clamped to 0-255)
     38;2;R;G;B / 48;2;R;G;B — true-color fg/bg (stored as #x1RRGGBB;
                                bit 24 is the true-color flag)"
  (labels ((consume (ps)
             (when ps
               (let ((p (first ps)))
                 (cond
                   ;; 256-color foreground: 38;5;N
                   ((and (= p 38) (eql (second ps) 5) (third ps))
                    (setf (screen-cur-fg screen) (clamp (third ps) 0 255))
                    (consume (cdddr ps)))
                   ;; True-color foreground: 38;2;R;G;B → store as #x1RRGGBB
                   ;; Each component is clamped to 0-255 to stay within (unsigned-byte 25).
                   ((and (= p 38) (eql (second ps) 2) (cddr ps))
                    (consume (%set-truecolor screen #'(setf screen-cur-fg) ps)))
                   ;; 256-color background: 48;5;N
                   ((and (= p 48) (eql (second ps) 5) (third ps))
                    (setf (screen-cur-bg screen) (clamp (third ps) 0 255))
                    (consume (cdddr ps)))
                   ;; True-color background: 48;2;R;G;B → store as #x1RRGGBB
                   ((and (= p 48) (eql (second ps) 2) (cddr ps))
                    (consume (%set-truecolor screen #'(setf screen-cur-bg) ps)))
                   ;; Underline-color 256: 58;5;N
                   ((and (= p 58) (eql (second ps) 5) (third ps))
                    (setf (screen-cur-ul-color screen) (clamp (third ps) 0 255))
                    (consume (cdddr ps)))
                   ;; Underline-color true-color: 58;2;R;G;B
                   ((and (= p 58) (eql (second ps) 2) (cddr ps))
                    (consume (%set-truecolor screen #'(setf screen-cur-ul-color) ps)))
                   (t
                    (%dispatch-sgr-code screen p)
                    (consume (rest ps))))))))
    (consume (or params '(0)))))
