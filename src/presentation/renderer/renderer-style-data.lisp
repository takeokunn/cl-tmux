(in-package #:cl-tmux/renderer)

;;;; Declarative dispatch tables backing the style-string parsing and SGR
;;;; emission logic in renderer-style.lisp.
;;;;
;;;; This file holds the three define-*-table macros (Prolog-like fact tables
;;;; that expand into dispatch functions) plus their invocations and the
;;;; colour-name alist, so renderer-style.lisp itself only has to hold the
;;;; parsing/emission logic that consumes them.
;;;;
;;;; Load order: renderer-format → renderer-style-data → renderer-style → renderer-pane → renderer.
;;;; All files share the cl-tmux/renderer package (no defpackage here).

;;; ── Style-token cond table (macro-generated) ─────────────────────────────────
;;;
;;; Define-style-token-table builds %dispatch-style-token from a Prolog-like
;;; fact table, replacing the 13-arm hand-written cond in parse-style-string:
;;;   style_token("bold",          :bold,          t)
;;;   style_token("nobold",        :bold,          nil)
;;;   ...

(defmacro define-style-token-table (&rest rules)
  "Build %DISPATCH-STYLE-TOKEN from a declarative (token key value) fact table.
   When the token matches, calls (SETF (GETF (CAR RESULT-CELL) KEY) VALUE)
   so the mutation is visible to the caller's plist stored in RESULT-CELL.
   Returns T on match, NIL otherwise."
  `(defun %dispatch-style-token (tok result-cell)
     "Apply style TOKEN, mutating (CAR RESULT-CELL).  Returns T on match, NIL otherwise."
     (cond
       ,@(mapcar (lambda (rule)
                   (destructuring-bind (token-str key value) rule
                     `((string= tok ,token-str)
                       (setf (getf (car result-cell) ,key) ,value)
                       t)))
                 rules)
       (t nil))))

(define-style-token-table
  ("bold"          :bold          t)
  ("dim"           :dim           t)
  ("reverse"       :reverse       t)
  ("underline"     :underline     t)
  ("italics"       :italics       t)
  ("blink"         :blink         t)
  ("conceal"       :conceal       t)
  ("strikethrough" :strikethrough t)
  ("nobold"        :bold          nil)
  ("nodim"         :dim           nil)
  ("noreverse"     :reverse       nil)
  ("nounderline"   :underline     nil)
  ("noitalics"     :italics       nil))

;;; ── SGR-code table (macro-generated) ────────────────────────────────────────
;;;
;;; Define-style-sgr-table builds %emit-style-attrs from a declarative
;;; (key sgr-code) fact table, replacing the 8 sequential (when ...) forms
;;; in style-to-sgr:
;;;   style_sgr(:bold,          "1")
;;;   style_sgr(:dim,           "2")
;;;   ...

(defmacro define-style-sgr-table (&rest rules)
  "Build %EMIT-STYLE-ATTRS from a declarative (key sgr-code) fact table.
   Pushes each applicable SGR code string onto the PARTS list and returns it."
  `(defun %emit-style-attrs (parsed-style parts)
     "Push SGR codes for each attribute set in PARSED-STYLE onto PARTS (prepended).
      Returns the updated PARTS list; caller must nreverse before use."
     ,@(mapcar (lambda (rule)
                 (destructuring-bind (key code) rule
                   `(when (getf parsed-style ,key) (push ,code parts))))
               rules)
     parts))

(define-style-sgr-table
  (:bold          "1")
  (:dim           "2")
  (:italics       "3")
  (:underline     "4")
  (:blink         "5")
  (:reverse       "7")
  (:conceal       "8")
  (:strikethrough "9"))

;;; ── Colour-name table ───────────────────────────────────────────────────────
;;;
;;; Defined here (before renderer-style.lisp) so SBCL sees it as a known
;;; special at compile time and does not emit an undefined-variable warning.

(defvar *%color-name-table*
  '(("black"    . "30")
    ("red"      . "31")
    ("green"    . "32")
    ("yellow"   . "33")
    ("blue"     . "34")
    ("magenta"  . "35")
    ("cyan"     . "36")
    ("white"    . "37")
    ("brightblack"   . "90")
    ("brightred"     . "91")
    ("brightgreen"   . "92")
    ("brightyellow"  . "93")
    ("brightblue"    . "94")
    ("brightmagenta" . "95")
    ("brightcyan"    . "96")
    ("brightwhite"   . "97"))
  "Read-only alist mapping color name strings to integer SGR base code strings (foreground codes).
Never rebound at runtime — use DEFVAR so image restarts do not reset the binding.")

;;; ── Border-charset declarative dispatch table ───────────────────────────────

(defmacro define-border-charset-table (&rest rules)
  "Build %DISPATCH-BORDER-CHARSET from a declarative (style tl tr bl br h v) fact table.
   Any unknown style falls back to single (┌┐└┘─│)."
  `(defun %dispatch-border-charset (style)
     "Return (values TL TR BL BR H V) box-drawing chars for the given border STYLE string."
     (cond
       ,@(mapcar (lambda (rule)
                   (destructuring-bind (style-str tl tr bl br h v) rule
                     `((string= style ,style-str)
                       (values ,tl ,tr ,bl ,br ,h ,v))))
                 rules)
       (t (values #\┌ #\┐ #\└ #\┘ #\─ #\│)))))

(define-border-charset-table
  ("rounded" #\╭ #\╮ #\╰ #\╯ #\─ #\│)
  ("double"  #\╔ #\╗ #\╚ #\╝ #\═ #\║)
  ("heavy"   #\┏ #\┓ #\┗ #\┛ #\━ #\┃)
  ("simple"  #\+ #\+ #\+ #\+ #\- #\|)
  ("padded"  #\Space #\Space #\Space #\Space #\Space #\Space)
  ("none"    #\Space #\Space #\Space #\Space #\Space #\Space))
