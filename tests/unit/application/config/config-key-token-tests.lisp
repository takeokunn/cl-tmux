(in-package #:cl-tmux/test)

;;;; config directive tests — key token parsing, command names, and listing labels

(in-suite config-directives-suite)

;;; %normalize-key-alias / *key-name-aliases* (navigation-key spellings)
;;;
;;; PPage/PgUp/NPage/PgDn/IC/DC are alternate spellings tmux accepts for the
;;; canonical navigation-key names the event loop emits (see %csi-tilde-key-name).
;;; %parse-key-token consults these via %normalize-key-alias so `bind -n PPage`
;;; resolves to the same binding as `bind -n PageUp`.

(test normalize-key-alias-table
  "%normalize-key-alias maps tmux's alternate navigation-key spellings to the
   canonical name the event loop emits, case-insensitively; unknown tokens
   return NIL so the caller falls through to the verbatim token."
  (dolist (row '(("PPage"  "PageUp"   "PPage aliases PageUp")
                 ("PgUp"   "PageUp"   "PgUp aliases PageUp")
                 ("NPage"  "PageDown" "NPage aliases PageDown")
                 ("PgDn"   "PageDown" "PgDn aliases PageDown")
                 ("IC"     "Insert"   "IC aliases Insert")
                 ("DC"     "Delete"   "DC aliases Delete")
                 ("ppage"  "PageUp"   "alias lookup is case-insensitive")
                 ("PageUp" nil        "the canonical name itself is not an alias")
                 ("Up"     nil        "an unrelated key name returns NIL")))
    (destructuring-bind (input expected desc) row
      (is (equal expected (cl-tmux/config::%normalize-key-alias input)) "~A" desc))))

(test bind-navigation-key-alias-resolves-canonical-binding
  "bind -n PPage <cmd> and bind -n PageUp <cmd> store under the SAME canonical
   key, so either spelling in a .tmux.conf reaches the binding the event loop
   looks up."
  (with-isolated-config
    (cl-tmux/config:apply-config-directive '("bind" "-n" "PPage" "next-window"))
    (let ((entry (cl-tmux/config:key-table-lookup "root" "PageUp")))
      (is (eq :next-window (cl-tmux/config:key-table-command entry))
          "PPage must bind under the canonical \"PageUp\" key"))))

;;; %parse-control-char (config-tokenizer.lisp)
;;;
;;; Returns the control CHARACTER (not the byte %prefix-control-byte returns)
;;; for the part of a token after a "C-" prefix; used by %parse-key-token.

(test parse-control-char-table
  "%parse-control-char maps C-a..C-z to ^A..^Z, C-Space/C-@ to NUL, the
   bracket/backslash/caret/underscore control keys to their control chars, and
   any other input to NIL."
  (dolist (row (list (list "Space" (code-char 0)  "C-Space -> NUL")
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
                     (list "ab"    nil            "multi-char rest is not a single key")))
    (destructuring-bind (input expected desc) row
      (is (equal expected (cl-tmux/config::%parse-control-char input)) "~A" desc))))

;;; %canonical-command-name / %known-command-name-p (config-commands.lisp)

(test canonical-command-name-table
  "%canonical-command-name is identity: command aliases are not a compatibility
   layer, so canonical names and shorthand spellings pass through unchanged."
  (dolist (row '(("neww"        "neww"           "neww stays unresolved")
                 ("splitw"      "splitw"         "splitw stays unresolved")
                 ("NEWW"        "NEWW"           "case is not alias-normalized")
                 ("killp"       "killp"          "killp stays unresolved")
                 ("new-window"  "new-window"      "a canonical name passes through unchanged")
                 ("bogus-xyz"   "bogus-xyz"       "an unrecognised name passes through unchanged")))
    (destructuring-bind (input expected desc) row
      (is (string= expected (cl-tmux/config::%canonical-command-name input)) "~A" desc))))

(test known-command-name-p-table
  "%known-command-name-p accepts bindable keywords and known canonical names; it
   rejects tmux short aliases and genuine typos."
  (dolist (row '(("new-window"        t   "a bindable keyword name is known")
                 ("neww"              nil "a tmux short alias is rejected")
                 ("previous-window"   t   "an arg-only canonical name is known")
                 ("breakp"            nil "break-pane's alias is rejected")
                 ("totally-bogus-xyz" nil "a genuine typo is not known")
                 (""                  nil "the empty string is not known")))
    (destructuring-bind (input expected desc) row
      (is (eq expected (and (cl-tmux/config::%known-command-name-p input) t)) "~A" desc))))

;;; key-label (config-listing.lisp)

(test key-label-table
  "key-label renders a character key as a one-character string and passes a
   string key (named keys like \"Up\"/\"C-Right\") through unchanged."
  (dolist (row (list (list #\c      "c"       "a character key becomes a 1-char string")
                     (list #\%      "%"       "a punctuation character key becomes a 1-char string")
                     (list "Up"     "Up"      "a string key passes through unchanged")
                     (list "C-Right" "C-Right" "a multi-char named key passes through unchanged")))
    (destructuring-bind (input expected desc) row
      (is (string= expected (cl-tmux/config::key-label input)) "~A" desc))))
