(in-package #:cl-tmux/renderer)

;;;; Style-string macro dispatch tables for the cl-tmux renderer.
;;;;
;;;; This file extends the style-string parsing in renderer.lisp with
;;;; macro-generated dispatch tables that replace the hand-written cond chains.
;;;; It also provides %border-color-sgr as a single-source-of-truth accessor
;;;; that renderer-pane.lisp uses instead of its own duplicated cond.
;;;;
;;;; Load order: renderer-format → renderer-style → renderer-pane → renderer.
;;;; All four files share the cl-tmux/renderer package (no defpackage here).

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
;;; Defined BEFORE %border-color-sgr so SBCL sees it as a known special at
;;; compile time and does not emit an undefined-variable warning.

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

;;; ── Single-source border-colour lookup ───────────────────────────────────────
;;;
;;; %border-color-sgr looks up a colour name in *%color-name-table* and returns
;;; the foreground SGR code integer.  renderer-pane.lisp calls this instead of
;;; its own duplicate cond table.

(defun %border-color-sgr (color-name)
  "Return the foreground SGR code integer for COLOR-NAME via *%color-name-table*.
   Returns NIL when COLOR-NAME is not in the table."
  (let ((entry (assoc (string-downcase color-name) *%color-name-table* :test #'string=)))
    (when entry
      (parse-integer (cdr entry)))))

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

(defun %border-charset-for (option-name)
  "Return box-drawing characters for the *-border-lines option OPTION-NAME as
   (values TOP-LEFT TOP-RIGHT BOTTOM-LEFT BOTTOM-RIGHT HORIZONTAL VERTICAL):
   single (default), rounded, double, heavy, simple (ASCII +/-|), padded/none
   (blank); an unknown value falls back to single.  Shared by the popup and menu
   box renderers."
  (%dispatch-border-charset (cl-tmux/options:get-option option-name "single")))

(defun %popup-border-charset ()
  "Return box-drawing characters for the popup-border-lines option.
   Delegates to %BORDER-CHARSET-FOR with \"popup-border-lines\"."
  (%border-charset-for "popup-border-lines"))


;;; ── Named SGR constants ──────────────────────────────────────────────────────
;;;
;;; +sgr-default-status+ : default status-bar and lock-screen style (blue bg / bright white)

(defconstant +sgr-default-status+
    (if (boundp '+sgr-default-status+)
        (symbol-value '+sgr-default-status+)
        "44;97")
  "Default status-bar SGR string: blue background (44) + bright white text (97).")

(defun %color-name-to-sgr-number (name is-bg)
  "Convert a color name string NAME to an SGR sequence fragment.
   IS-BG: T for background, NIL for foreground.
   Returns a string like \"31\" or \"41\" or \"38;5;N\" for colourN."
  (let ((lname (string-downcase name)))
    (cond
      ((and (>= (length lname) 7) (string= (subseq lname 0 6) "colour"))
       (let ((n (parse-integer lname :start 6 :junk-allowed t)))
         (if n
             (format nil "~D;5;~D" (if is-bg 48 38) n)
             (if is-bg "49" "39"))))
      ((string= lname "default") (if is-bg "49" "39"))
      (t (let ((entry (assoc lname *%color-name-table* :test #'string=)))
           (if entry
               (let ((base (parse-integer (cdr entry))))
                 (format nil "~D" (if is-bg (+ base 10) base)))
               (if is-bg "49" "39")))))))

(defun %color-name-to-cell-color (name)
  "Convert a tmux colour NAME to the cell fg/bg numeric encoding used by the
   screen model — a palette index 0-255, or true-colour with bit 24 set.  Returns
   NIL for an empty name or \"default\" (meaning: leave the cell's own colour as
   is).  Mirrors the cell colour model in terminal/cell.lisp (0-7 standard, 8-15
   bright, 16-255 extended, #x1000000+ true-colour)."
  (let ((lname (and name (string-downcase (string-trim " " name)))))
    (cond
      ((or (null lname) (string= lname "") (string= lname "default")) nil)
      ((and (> (length lname) 6) (string= (subseq lname 0 6) "colour"))
       (let ((n (parse-integer lname :start 6 :junk-allowed t)))
         (and n (<= 0 n 255) n)))
      ((and (= (length lname) 7) (char= (char lname 0) #\#))
       (let ((rgb (parse-integer lname :start 1 :radix 16 :junk-allowed t)))
         (and rgb (logior #x1000000 rgb))))   ; bit 24 marks true-colour
      (t (let ((entry (assoc lname *%color-name-table* :test #'string=)))
           (when entry
             ;; Table values are fg SGR codes: 30-37 → 0-7, 90-97 → 8-15.
             (let ((sgr (parse-integer (cdr entry))))
               (cond ((<= 30 sgr 37) (- sgr 30))
                     ((<= 90 sgr 97) (+ 8 (- sgr 90)))
                     (t nil)))))))))

(defun %window-style-default-colors (style-string)
  "Parse a window-style / window-active-style STYLE-STRING into the pane's default
   (values FG BG) as cell colour numbers, each NIL when the style does not set it.
   Used by the pane renderer to recolour cells that carry the model defaults
   (fg=7, bg=0) — the tmux \"dim inactive panes\" behaviour."
  (let ((parsed (parse-style-string style-string)))
    (if parsed
        (values (%color-name-to-cell-color (getf parsed :fg))
                (%color-name-to-cell-color (getf parsed :bg)))
        (values nil nil))))

(defun %split-style-tokens (style)
  "Internal: split STYLE on commas. Used by parse-style-string."
  (let ((tokens nil)
        (start  0))
    (loop for i from 0 below (length style)
          when (char= (char style i) #\,)
            do (push (subseq style start i) tokens)
               (setf start (1+ i))
          finally (push (subseq style start) tokens))
    (nreverse tokens)))

(defun parse-style-string (style)
  "Parse a tmux style string STYLE into a plist with keys:
   :fg :bg :bold :dim :reverse :underline :italics :blink :conceal :strikethrough
   Color values are strings (e.g. \"red\", \"colour4\"), attribute values are T/NIL.
   Returns NIL for NIL or empty STYLE."
  (unless (or (null style) (string= style ""))
    ;; Wrap the result plist in a cons cell so %dispatch-style-token can mutate
    ;; the binding via (setf (getf (car cell) key) value).
    (let ((result-cell (list nil)))
      (dolist (token (%split-style-tokens style))
        (let ((tok (string-downcase (string-trim " " token))))
          (cond
            ((and (>= (length tok) 3) (string= (subseq tok 0 3) "fg="))
             (setf (getf (car result-cell) :fg) (subseq tok 3)))
            ((and (>= (length tok) 3) (string= (subseq tok 0 3) "bg="))
             (setf (getf (car result-cell) :bg) (subseq tok 3)))
            (t (%dispatch-style-token tok result-cell)))))
      (car result-cell))))

(defun style-to-sgr (parsed-style)
  "Convert a parsed style plist (from PARSE-STYLE-STRING) to an SGR sequence string.
   Returns the default status-bar SGR \"44;97\" when PARSED-STYLE is NIL or empty."
  (if (null parsed-style)
      +sgr-default-status+
      (let ((parts nil))
        ;; Use the macro-generated %emit-style-attrs for attribute bits.
        (setf parts (%emit-style-attrs parsed-style parts))
        ;; Colour codes are position-dependent (fg before bg) so remain explicit.
        (when (getf parsed-style :fg)
          (push (%color-name-to-sgr-number (getf parsed-style :fg) nil) parts))
        (when (getf parsed-style :bg)
          (push (%color-name-to-sgr-number (getf parsed-style :bg) t) parts))
        (if parts
            (format nil "~{~A~^;~}" (nreverse parts))
            +sgr-default-status+))))

(defun %status-sgr-from-style (style-str)
  "Return a partial SGR string for STYLE-STR (e.g. \"fg=colour2,bg=colour4\").
   Parses the style string via PARSE-STYLE-STRING / STYLE-TO-SGR.
   Returns the default blue-on-white SGR \"44;97\" when style-str is empty/nil."
  (style-to-sgr (parse-style-string style-str)))

(defun %effective-status-style ()
  "Return the current status-bar style string from the `status-style` option.
   Returns an empty string when the option is unset."
  (cl-tmux/options:get-option "status-style" ""))
