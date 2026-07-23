(in-package #:cl-tmux/test)

;;;; config directive tests — key token parsing, command names, and listing labels

(describe "config-directives-suite"

  ;;; Navigation-key spellings
  ;;;
  ;;; cl-tmux config parsing is canonical-only.  PPage/PgUp/NPage/PgDn/IC/DC are
  ;;; not folded into PageUp/PageDown/Insert/Delete; they remain
  ;;; distinct string key names if a user binds them explicitly.

  ;; %parse-key-token keeps navigation spellings distinct instead of
  ;; normalizing them into canonical key names.
  (it "parse-key-token-keeps-navigation-spellings-verbatim"
    (check-table
     (loop for token in '("PPage" "PgUp" "NPage" "PgDn" "IC" "DC" "PageUp" "PageDown")
           collect (list (cl-tmux/config::%parse-key-token token)
                         token
                         (format nil "~A remains a distinct key name" token)))
     :test #'string=))

  ;; bind -n PPage <cmd> stores under PPage only; it must not create the canonical
  ;; PageUp binding as an implicit side effect.
  (it "bind-navigation-key-spelling-stays-distinct-from-canonical-binding"
    (with-isolated-config
      (cl-tmux/config:apply-config-directive '("bind" "-n" "PPage" "next-window"))
      (let ((entry (cl-tmux/config:key-table-lookup "root" "PPage")))
        (expect (eq :next-window (cl-tmux/config:key-table-command entry))))
      (expect (null (cl-tmux/config:key-table-lookup "root" "PageUp")))))

  ;;; %parse-control-char (config-tokenizer.lisp)
  ;;;
  ;;; Returns the control CHARACTER (not the byte %prefix-control-byte returns)
  ;;; for the part of a token after a "C-" prefix; used by %parse-key-token.

  ;; %parse-control-char maps C-a..C-z to ^A..^Z, C-Space/C-@ to NUL, the
  ;; bracket/backslash/caret/underscore control keys to their control chars, and
  ;; any other input to NIL.
  (it "parse-control-char-table"
    (check-table
     (loop for (input expected desc)
             in (list (list "Space" (code-char 0)  "C-Space -> NUL")
                      (list "a"     (code-char 1)  "C-a -> ^A")
                      (list "z"     (code-char 26) "C-z -> ^Z")
                      (list "A"     (code-char 1)  "C-A -> ^A (case-insensitive)")
                      (list "@"     (code-char 0)  "C-@ -> NUL")
                      (list "["     (code-char 27) "C-[ -> ESC")
                      (list "\\"    (code-char 28) "C-\\ -> FS")
                      (list "]"     (code-char 29) "C-] -> GS")
                      (list "^"     (code-char 30) "C-^ -> RS")
                      (list "_"     (code-char 31) "C-_ -> US")
                      (list "1"     nil            "C-1 is not a controllable key")
                      (list "ab"    nil            "multi-char rest is not a single key"))
           collect (list (cl-tmux/config::%parse-control-char input)
                         expected
                         desc))
     :test #'equal))

  ;;; %known-command-name-p (config-commands.lisp)

  ;; %known-command-name-p accepts bindable keywords and known canonical names; it
  ;; rejects tmux short aliases and genuine typos.
  (it "known-command-name-p-table"
    (check-table
     (loop for (input expected desc)
             in '(("new-window"        t   "a bindable keyword name is known")
                  ("neww"              nil "a tmux short alias is rejected")
                  ("previous-window"   t   "an arg-only canonical name is known")
                  ("breakp"            nil "break-pane's alias is rejected")
                  ("totally-bogus-xyz" nil "a genuine typo is not known")
                  (""                  nil "the empty string is not known"))
           collect (list (and (cl-tmux/config::%known-command-name-p input) t)
                         expected
                         desc))
     :test #'eq))

  ;;; key-label (config-listing.lisp)

  ;; key-label renders a character key as a one-character string and passes a
  ;; string key (named keys like "Up"/"C-Right") through unchanged.
  (it "key-label-table"
    (check-table
     (loop for (input expected desc)
             in (list (list #\c       "c"       "a character key becomes a 1-char string")
                      (list #\%       "%"       "a punctuation character key becomes a 1-char string")
                      (list "Up"      "Up"      "a string key passes through unchanged")
                      (list "C-Right" "C-Right" "a multi-char named key passes through unchanged"))
           collect (list (cl-tmux/config::key-label input) expected desc))
     :test #'string=)))
