(in-package #:cl-tmux/terminal/sgr)

;;;; SGR (Select Graphic Rendition) macro-driven dispatch.
;;;;
;;;; define-sgr-rules builds a COND-based dispatcher from a list of
;;;; (condition &body forms) clauses.  The public entry point apply-sgr
;;;; iterates over a params list and calls %dispatch-sgr-code for each value.

;;; ── Attribute mutation helpers (data layer) ────────────────────────────────
;;;
;;; These four inline functions separate the HOW (bit manipulation) from the
;;; WHAT (which SGR code means what), keeping the rule table below readable.

(declaim (inline attr-on attr-off attr2-on attr2-off))

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
   Expands into a DEFUN for %DISPATCH-SGR-CODE that dispatches via COND.
   A generated docstring is injected so the exported symbol is documented."
  `(defun %dispatch-sgr-code (screen p)
     "Dispatch a single SGR parameter code P against SCREEN, applying the
      appropriate attribute or colour mutation.  Called by APPLY-SGR for each
      element of the parameter list.  Unknown codes are silently ignored."
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
  ((= p 0)   (reset-sgr-pen screen))

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
  ((= p 39)      (setf (screen-cur-fg screen) +default-color+))

  ;; ── Standard background (40-47) + default (49) ────────────────────────────
  ((<= 40 p 47)  (setf (screen-cur-bg screen) (- p 40)))
  ((= p 49)      (setf (screen-cur-bg screen) +default-color+))

  ;; ── Bright foreground (90-97) ─────────────────────────────────────────────
  ((<= 90 p 97)    (setf (screen-cur-fg screen) (+ 8 (- p 90))))

  ;; ── Bright background (100-107) ───────────────────────────────────────────
  ((<= 100 p 107)  (setf (screen-cur-bg screen) (+ 8 (- p 100)))))

;;; ── Public entry point ─────────────────────────────────────────────────────
;;;
;;; apply-sgr handles compound multi-parameter codes by consuming params ahead
;;; of the single-code dispatcher.  The helper loop is a left-fold over the
;;; parameter list:
;;;   apply_sgr([], Screen)              :- true.
;;;   apply_sgr([38,5,N|T], S)           :- set_fg_256(S, N),           apply_sgr(T, S).
;;;   apply_sgr([38,2,R,G,B|T], S)       :- set_fg_truecolor(S,R,G,B),  apply_sgr(T, S).
;;;   apply_sgr([48,5,N|T], S)           :- set_bg_256(S, N),           apply_sgr(T, S).
;;;   apply_sgr([48,2,R,G,B|T], S)       :- set_bg_truecolor(S,R,G,B),  apply_sgr(T, S).
;;;   apply_sgr([58,5,N|T], S)           :- set_ul_256(S, N),           apply_sgr(T, S).
;;;   apply_sgr([58,2,R,G,B|T], S)       :- set_ul_truecolor(S,R,G,B),  apply_sgr(T, S).
;;;   apply_sgr([P|T], S)                :- dispatch_sgr(S, P),         apply_sgr(T, S).

;;; %encode-truecolor-rgb and %set-truecolor encode a 38;2;R;G;B, 48;2;R;G;B, or
;;; 58;2;R;G;B run into +true-color-flag+ | (R<<16) | (G<<8) | B and store it via
;;; the supplied SETTER helper.  This keeps the clamp/logior arithmetic shared
;;; across all three colour slots AND across the semicolon (%set-truecolor) and
;;; colon-group (%apply-sgr-group) SGR syntaxes, which slice their R G B values
;;; out of differently-shaped lists but must encode them identically.

(declaim (inline %encode-truecolor-rgb))
(defun %encode-truecolor-rgb (red green blue)
  "Clamp RED, GREEN, and BLUE to 0-255 and encode them as #x1RRGGBB (bit 24 is
   the true-colour flag).  Shared by every SGR true-colour arm (semicolon and
   colon syntax alike) so the encoding arithmetic is written exactly once."
  (logior +true-color-flag+
          (ash (clamp (or red   0) 0 255) 16)
          (ash (clamp (or green 0) 0 255) 8)
          (clamp (or blue 0) 0 255)))

(declaim (inline %set-truecolor))
(defun %set-truecolor (screen setter parameter-list)
  "Encode the R;G;B triple at positions 3-5 of PARAMETER-LIST as #x1RRGGBB and
   call SETTER with (SCREEN value) to store the result.  SETTER should be one of
   #'(setf screen-cur-fg), #'(setf screen-cur-bg), or #'(setf screen-cur-ul-color).
   Returns the tail of PARAMETER-LIST after the five consumed parameters."
  (funcall setter
           (%encode-truecolor-rgb (third parameter-list) (fourth parameter-list)
                                   (fifth parameter-list))
           screen)
  (nthcdr 5 parameter-list))

;;; %consume-256-color-param handles the 38;5;N / 48;5;N / 58;5;N sub-protocol
;;; for a single selector code.  SETTER is the (setf screen-cur-XX) function to
;;; call; PARAMETER-TAIL is the full remaining list starting at the selector.
;;; Returns the tail after the three consumed elements (selector, 5, N).

(declaim (inline %consume-256-color-param))
(defun %consume-256-color-param (screen setter parameter-tail)
  "Apply a 256-color SGR selector arm: read the index at (third PARAMETER-TAIL),
   clamp it to 0-255, store it via SETTER, and return the tail after the three
   consumed parameter elements (selector; 5; N)."
  (funcall setter (clamp (third parameter-tail) 0 255) screen)
  (cdddr parameter-tail))

(defun %sgr-lead-setter (lead)
  "Return the color setter function for SGR extended-colour LEAD values (38/48/58)."
  (case lead
    (38 #'(setf screen-cur-fg))
    (48 #'(setf screen-cur-bg))
    (58 #'(setf screen-cur-ul-color))))

(defun %apply-sgr-group (screen group)
  "Apply ONE colon-delimited SGR sub-parameter GROUP (a list whose head is the
   leading SGR code), as produced by the parser for ISO 8613-6 colon syntax:
     (38|48|58 2 [cs] R G B) -> true-colour.  R G B are the LAST three values, so
        an optional colourspace-id field -- present (38:2:cs:R:G:B) or empty,
        which arrives as 0 (38:2::R:G:B) -- is skipped.
     (38|48|58 5 [cs] N)     -> 256-colour; N is the LAST value.
  Any other group applies its leading value as a plain SGR code, so e.g.
   4:3 (undercurl) -> underline (4)."
  (let ((lead   (first group))
        (kind   (second group))
        (setter (%sgr-lead-setter (first group))))
    (cond
      ((and setter (eql kind 2) (>= (length group) 5))
       (let ((rgb (last group 3)))
         (funcall setter (%encode-truecolor-rgb (first rgb) (second rgb) (third rgb))
                  screen)))
      ((and setter (eql kind 5) (>= (length group) 3))
       (funcall setter (clamp (or (car (last group)) 0) 0 255) screen))
      (t (%dispatch-sgr-code screen lead)))))

(defun %apply-sgr-color-arm (screen tail)
  "Apply a semicolon-protocol colour arm starting with the lead code at (first TAIL).
   The lead code must be 38, 48, or 58.  Returns the new tail after consumption.
   Handles kind-5 (256-colour) and kind-2 (true-colour) sub-protocols; falls back
   to dispatching the lead as a plain SGR code when the arm is malformed."
  (let* ((lead   (first tail))
         (kind   (second tail))
         (setter (%sgr-lead-setter lead)))
    (cond
      ((and setter (eql kind 5) (third tail))
       (%consume-256-color-param screen setter tail))
      ((and setter (eql kind 2) (cddr tail))
       (%set-truecolor screen setter tail))
      (t
       (%dispatch-sgr-code screen lead)
       (rest tail)))))

(defun %apply-sgr-parameters (screen parameters)
  "Consume PARAMETERS iteratively and apply each SGR arm to SCREEN.
   Uses a non-recursive loop to avoid stack overflow on pathologically long
   SGR sequences (e.g. sequences with thousands of parameters)."
  (let ((tail parameters))
    (loop while tail do
      (let ((p (first tail)))
        (cond
          ;; A colon-grouped parameter (list): a self-contained colour or
          ;; styled code.  MUST be checked first -- the integer branches
          ;; below would error on a list.
          ((consp p)
           (%apply-sgr-group screen p)
           (setf tail (rest tail)))
          ;; Semicolon-protocol colour arms: 38/48/58 with kind 5 (256-colour)
          ;; or kind 2 (true-colour).  Delegate to %apply-sgr-color-arm which
          ;; flattens the three-level nesting and returns the new tail.
          ((member p '(38 48 58))
           (setf tail (%apply-sgr-color-arm screen tail)))
          (t
           (%dispatch-sgr-code screen p)
           (setf tail (rest tail))))))
    (values)))

(defun apply-sgr (screen params)
  "Apply a sequence of SGR codes to SCREEN.
   PARAMS is a list of fixnum SGR parameter values; an empty or nil list is
   treated as (0) (i.e. a plain SGR reset).
   Multi-parameter codes handled as a unit:
     38;5;N / 48;5;N / 58;5;N     -- 256-color fg/bg/underline (N clamped to 0-255)
     38;2;R;G;B / 48;2;R;G;B / 58;2;R;G;B -- true-color fg/bg/underline
                                   (stored as #x1RRGGBB; bit 24 is the true-color flag)"
  (%apply-sgr-parameters screen (or params '(0))))

;;; ── Inverse: pen -> SGR parameter string (DECRQSS status report) ----------

(defun %emit-sgr-color (out color background-p)
  "Write the ';'-prefixed SGR colour fragment for cell COLOR to OUT;
   BACKGROUND-P selects the background variant (foreground when NIL).
   0-7 -> 30-37/40-47; 8-15 -> 90-97/100-107; 16-255 ->
   38;5;N / 48;5;N; +true-color-flag+ set -> 38;2;R;G;B / 48;2;R;G;B."
  (cond
    ((= color +default-color+) (format out ";~D" (if background-p 49 39)))
    ((logtest color +true-color-flag+)
     (format out ";~D;2;~D;~D;~D" (if background-p 48 38)
             (ldb (byte 8 16) color) (ldb (byte 8 8) color) (ldb (byte 8 0) color)))
    ((<= 0 color 7)    (format out ";~D" (+ (if background-p 40 30) color)))
    ((<= 8 color 15)   (format out ";~D" (+ (if background-p 100 90) (- color 8))))
    ((<= 16 color 255) (format out ";~D;5;~D" (if background-p 48 38) color))))

(defun %pen-to-sgr-params (fg bg attrs attrs2)
  "Reconstruct, from a reset, the SGR parameter string reproducing a pen with
   foreground FG, background BG (cell colour encoding) and attribute bitfields
   ATTRS / ATTRS2.  E.g. bold red on default -> \"0;1;31\".  The default fg/bg
   (+default-color+) are omitted (already produced by the leading reset).  This is
   the inverse of apply-sgr's pen mutation, used to answer DECRQSS 'm' queries."
  (with-output-to-string (out)
    (write-char #\0 out)                       ; always start from a reset
    (when (logtest attrs  +attr-bold+)              (format out ";~D" 1))
    (when (logtest attrs  +attr-dim+)               (format out ";~D" 2))
    (when (logtest attrs  +attr-italic+)            (format out ";~D" 3))
    (when (logtest attrs  +attr-underline+)         (format out ";~D" 4))
    (when (logtest attrs  +attr-blink+)             (format out ";~D" 5))
    (when (logtest attrs  +attr-reverse+)           (format out ";~D" 7))
    (when (logtest attrs  +attr-conceal+)           (format out ";~D" 8))
    (when (logtest attrs  +attr-strikethrough+)     (format out ";~D" 9))
    (when (logtest attrs2 +attr2-double-underline+) (format out ";~D" 21))
    (when (logtest attrs2 +attr2-overline+)         (format out ";~D" 53))
    (unless (= fg +default-color+) (%emit-sgr-color out fg nil))
    (unless (= bg +default-color+) (%emit-sgr-color out bg t))))
