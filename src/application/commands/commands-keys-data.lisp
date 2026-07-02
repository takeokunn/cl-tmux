(in-package #:cl-tmux/commands)

;;;; send-keys key-name data tables: the named-key alist and the CSI-modifier
;;;; shape table used by commands-keys.lisp's translation logic.

;;; ESC as a single-character string.  defparameter rather than defconstant because
;;; string literals are not EQL-comparable and SBCL would signal a redefinition
;;; error on every fasl reload with defconstant.
(defparameter *escape-string* (string (code-char 27))
  "The ESC character (ASCII 27) as a single-character string.")

(defun %escape-sequence (&rest tail)
  "Build a string beginning with ESC followed by TAIL strings concatenated."
  (apply #'concatenate 'string *escape-string* tail))

;;; tmux's send-keys interprets arguments that name a key (Enter, Tab, Up, C-c,
;;; M-x, F5, ...) and sends that key's byte sequence rather than the literal
;;; text.  *send-key-names* is the named-key table; C-<x> (control) and M-<x>
;;; (meta/alt) are handled algorithmically.  Escape sequences use the normal
;;; (non-application) xterm encodings, matching what send-keys emits by default.

(defparameter *send-key-names*
  (list
   ;; whitespace / control
   (cons "Enter"  (string #\Return)) (cons "C-m" (string #\Return))
   (cons "Tab"    (string #\Tab))    (cons "C-i" (string #\Tab))
   (cons "Space"  " ")
   (cons "Escape" *escape-string*)   (cons "Esc" *escape-string*)
   (cons "BSpace" (string (code-char 127)))
   (cons "BTab"   (%escape-sequence "[Z"))
   ;; arrows (normal cursor mode)
   (cons "Up"     (%escape-sequence "[A")) (cons "Down"  (%escape-sequence "[B"))
   (cons "Right"  (%escape-sequence "[C")) (cons "Left"  (%escape-sequence "[D"))
   ;; navigation block
   (cons "Home"     (%escape-sequence "[H")) (cons "End"      (%escape-sequence "[F"))
   (cons "PageUp"   (%escape-sequence "[5~")) (cons "PPage"   (%escape-sequence "[5~"))
   (cons "PageDown" (%escape-sequence "[6~")) (cons "NPage"   (%escape-sequence "[6~"))
   (cons "Insert"   (%escape-sequence "[2~")) (cons "IC"      (%escape-sequence "[2~"))
   (cons "Delete"   (%escape-sequence "[3~")) (cons "DC"      (%escape-sequence "[3~"))
   ;; function keys
   (cons "F1"  (%escape-sequence "OP"))   (cons "F2"  (%escape-sequence "OQ"))
   (cons "F3"  (%escape-sequence "OR"))   (cons "F4"  (%escape-sequence "OS"))
   (cons "F5"  (%escape-sequence "[15~")) (cons "F6"  (%escape-sequence "[17~"))
   (cons "F7"  (%escape-sequence "[18~")) (cons "F8"  (%escape-sequence "[19~"))
   (cons "F9"  (%escape-sequence "[20~")) (cons "F10" (%escape-sequence "[21~"))
   (cons "F11" (%escape-sequence "[23~")) (cons "F12" (%escape-sequence "[24~")))
  "Alist mapping tmux key-name strings to their literal byte sequence (as a
   string whose char-codes are the bytes — all < 128).")

(defparameter *modified-send-keys*
  '(;; :letter keys encode as ESC [ 1 ; <mod> <final-char>
    ;; e.g. C-Up → ESC[1;5A  (mod=5=Ctrl), S-Left → ESC[1;2D (mod=2=Shift)
    ("Up" :letter #\A) ("Down" :letter #\B) ("Right" :letter #\C) ("Left" :letter #\D)
    ("Home" :letter #\H) ("End" :letter #\F)
    ("F1" :letter #\P) ("F2" :letter #\Q) ("F3" :letter #\R) ("F4" :letter #\S)
    ;; :tilde keys encode as ESC [ <param> ; <mod> ~
    ;; e.g. C-F5 → ESC[15;5~  (param=15), S-PageUp → ESC[5;2~ (param=5)
    ("F5" :tilde 15) ("F6" :tilde 17) ("F7" :tilde 18) ("F8" :tilde 19)
    ("F9" :tilde 20) ("F10" :tilde 21) ("F11" :tilde 23) ("F12" :tilde 24)
    ("PageUp" :tilde 5) ("PPage" :tilde 5) ("PageDown" :tilde 6) ("NPage" :tilde 6)
    ("Insert" :tilde 2) ("IC" :tilde 2) ("Delete" :tilde 3) ("DC" :tilde 3))
  "Base special keys that accept a CSI modifier prefix, with their byte-sequence
   shape.  The modifier code is 1+Shift+2*Alt+4*Ctrl (so Ctrl=5, Shift=2, etc.).
   The inverse of the event loop's modifier decoding, so send-keys C-Up
   round-trips with `bind -n C-Up`.")
