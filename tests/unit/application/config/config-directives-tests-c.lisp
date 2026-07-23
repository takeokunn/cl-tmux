(in-package #:cl-tmux/test)

;;;; load-config-file, command-keyword, parse-bind-args, key-table edge cases — part III

(describe "config-directives-suite"

  ;;; load-config-file on a real temp file

  ;; load-config-file applies an existing file's directives and returns the count.
  (it "load-config-file-existing-temp-file"
    (with-isolated-config
      (with-temp-config-file (p "bind z next-window" "set-status-height 3")
        (expect (= 2 (load-config-file p)))
        (expect (eq :next-window (lookup-key-binding #\z)))
        (expect (= 3 *status-height*)))))

  ;;; %command-keyword: resolution and non-interning contract

  ;; %command-keyword must NOT intern an unknown command name into :keyword.
  (it "command-keyword-does-not-intern-unknown"
    (let ((name "CL-TMUX-NONEXISTENT-COMMAND-DO-NOT-INTERN-ME"))
      (expect (null (find-symbol name :keyword)))
      (expect (null (cl-tmux/config::%command-keyword name)))
      (expect (null (find-symbol name :keyword)))))

  ;; %command-keyword returns the command keyword for a recognized bindable name.
  (it "command-keyword-returns-bindable-keyword"
    (dolist (case '(("new-window" :new-window "new-window should resolve to :new-window")
                    ("NEW-WINDOW" :new-window "resolution should be case-insensitive")
                    ("split-horizontal" :split-horizontal
                     "split-horizontal should resolve to :split-horizontal")
                    ("prev-window" :prev-window "prev-window should resolve to :prev-window")
                    ("copy-mode-enter" :copy-mode-enter
                     "copy-mode-enter should resolve to :copy-mode-enter")
                    ("swap-pane-forward" :swap-pane-forward
                     "swap-pane-forward should resolve to :swap-pane-forward")
                    ("detach" :detach "detach should resolve to :detach")))
      (destructuring-bind (name expected desc) case
        (declare (ignore desc))
        (expect (eq expected (cl-tmux/config::%command-keyword name))))))

  ;; %command-keyword rejects shorthand tmux abbreviations so config sticks to
  ;; canonical command names.
  (it "command-keyword-rejects-standard-tmux-abbreviations"
    (dolist (name '("breakp" "killp" "next" "prev" "last" "displayp" "rotatew"))
      (expect (null (cl-tmux/config::%command-keyword name)))))

  ;; bind rejects tmux short aliases and stores arg-only canonical names as
  ;; deferred token lists for key-press dispatch.
  (it "bind-rejects-shorthand-and-stores-canonical-deferred-token-lists"
    (with-isolated-config
      (expect (= 0 (load-config-from-string "bind b breakp")))
      (expect (null (lookup-key-binding #\b)))
      (expect (= 1 (load-config-from-string "bind @ previous-window")))
      (expect (equal '("previous-window") (lookup-key-binding #\@)))
      (expect (= 0 (load-config-from-string "bind Q definitely-not-a-command")))
      (expect (null (lookup-key-binding #\Q)))))

  ;; %command-keyword returns NIL for interned-but-non-bindable keywords.
  (it "command-keyword-rejects-non-bindable-keyword"
    (let ((kw (intern "COPY-MODE-EXIT" :keyword)))
      (expect (eq :copy-mode-exit kw))
      (expect (not (member :copy-mode-exit cl-tmux/config::*bindable-commands*)))
      (expect (null (cl-tmux/config::%command-keyword "copy-mode-exit"))))
    (let ((cl-tmux/config::*bindable-commands* '(:detach)))
      (expect (null (cl-tmux/config::%command-keyword "new-window")))
      (expect (eq :detach (cl-tmux/config::%command-keyword "detach")))))

  ;;; %config-tokens (tokenizer)

  ;; %config-tokens splits a line into whitespace-separated tokens.
  (it "config-tokens-splits-on-whitespace"
    (expect (equal '("bind" "c" "new-window")
                   (cl-tmux/config::%config-tokens "bind c new-window")))
    (expect (equal '("set-shell" "/bin/bash")
                   (cl-tmux/config::%config-tokens "  set-shell  /bin/bash  ")))
    (expect (null (cl-tmux/config::%config-tokens "")))
    (expect (null (cl-tmux/config::%config-tokens "   "))))

  ;;; %parse-key-token

  ;; %parse-key-token: 1-char token → character; multi-char → string; C-<letter> →
  ;; control char; C-Space/C-@ → NUL; C-<named-key> kept as string.
  (it "parse-key-token-table"
    (dolist (row (list (list "c"       #\c            "single char 'c'")
                       (list "%"       #\%            "single char '%'")
                       (list "\""      #\"            "single char '\"'")
                       (list "M-1"     "M-1"          "multi-char → string")
                       (list "F1"      "F1"           "multi-char F1 → string")
                       (list "C-a"     (code-char 1)  "C-a → ^A (1)")
                       (list "C-z"     (code-char 26) "C-z → ^Z (26)")
                       (list "C-b"     (code-char 2)  "C-b → ^B (2)")
                       (list "C-A"     (code-char 1)  "C-A → ^A (case-insensitive)")
                       (list "C-Space" (code-char 0)  "C-Space → NUL")
                       (list "C-@"     (code-char 0)  "C-@ → NUL")
                       (list "C-["     (code-char 27) "C-[ → ESC (27)")
                       (list "C-\\"    (code-char 28) "C-\\ → FS (28)")
                       (list "C-]"     (code-char 29) "C-] → GS (29)")
                       (list "C-Left"  "C-Left"       "C-<named-key> stays string")
                       (list "C-Up"    "C-Up"         "C-<named-key> stays string")))
      (destructuring-bind (input expected desc) row
        (declare (ignore desc))
        (expect (equal expected (cl-tmux/config::%parse-key-token input))))))

  ;; %parse-key-token re-orders two-or-more modifier prefixes into canonical
  ;; C-/M-/S- order, so the spelling order in a binding does not matter and matches
  ;; what the event loop emits (audit #15).
  (it "parse-key-token-canonicalizes-multi-modifier-order"
    (dolist (row (list (list "M-C-x"    "C-M-x"    "M-C-x → C-M-x")
                       (list "C-M-x"    "C-M-x"    "C-M-x stays canonical")
                       (list "S-C-Up"   "C-S-Up"   "S-C-Up → C-S-Up")
                       (list "M-C-Left" "C-M-Left" "M-C-Left → C-M-Left")
                       (list "S-M-C-x"  "C-M-S-x"  "S-M-C-x → C-M-S-x (3 modifiers)")))
      (destructuring-bind (input expected desc) row
        (declare (ignore desc))
        (expect (equal expected (cl-tmux/config::%parse-key-token input))))))

  ;; bind C-a <cmd> binds the control character ^A (byte 1) so a real Ctrl-a
  ;; keypress (which the event loop reads as byte 1) resolves to the command.
  (it "bind-control-letter-fires-via-control-char"
    (with-isolated-config
      (cl-tmux/config:apply-config-directive '("bind" "C-a" "next-window"))
      (expect (eq :next-window (lookup-key-binding (code-char 1))))))

  ;; bind C-Up <cmd> stores the command under the string key "C-Up" in the
  ;; prefix table — the canonical key name the event loop reconstructs from the
  ;; ESC [ 1 ; 5 A wire sequence.  (Without this, modifier+arrow binds were dead.)
  (it "bind-modifier-arrow-stores-canonical-string-key"
    (with-isolated-config
      (cl-tmux/config:apply-config-directive '("bind" "C-Up" "next-window"))
      (let ((entry (cl-tmux/config:key-table-lookup "prefix" "C-Up")))
        (expect (eq :next-window (cl-tmux/config:key-table-command entry))))))

  ;; bind -n M-Left <cmd> stores under string key "M-Left" in the ROOT table so
  ;; a bare (no-prefix) Alt+Left fires it.
  (it "bind-n-modifier-arrow-stores-in-root-table"
    (with-isolated-config
      (cl-tmux/config:apply-config-directive '("bind" "-n" "M-Left" "next-window"))
      (let ((entry (cl-tmux/config:key-table-lookup "root" "M-Left")))
        (expect (eq :next-window (cl-tmux/config:key-table-command entry))))))

  ;; bind Up <cmd> stores under string key "Up" in the prefix table, matching
  ;; the name reconstructed from the ESC [ A wire sequence.
  (it "bind-plain-arrow-stores-canonical-string-key"
    (with-isolated-config
      (cl-tmux/config:apply-config-directive '("bind" "Up" "next-window"))
      (let ((entry (cl-tmux/config:key-table-lookup "prefix" "Up")))
        (expect (eq :next-window (cl-tmux/config:key-table-command entry))))))

  ;; bind -n M-h <cmd> stores under string key "M-h" in the ROOT table so a bare
  ;; (no-prefix) Alt+h, which arrives as ESC h, fires it.
  (it "bind-n-meta-key-stores-in-root-table"
    (with-isolated-config
      (cl-tmux/config:apply-config-directive '("bind" "-n" "M-h" "next-window"))
      (let ((entry (cl-tmux/config:key-table-lookup "root" "M-h")))
        (expect (eq :next-window (cl-tmux/config:key-table-command entry))))))

  ;;; status option: off / on / line-count parsing → *status-height*

  ;; `set-option -g status` maps string values to the expected status height.
  (it "set-status-directive-table"
    (dolist (case '(("off" 0 "status off → height 0")
                    ("0" 0 "status 0 → height 0")
                    ("on" 1 "status on → height 1")
                    ("2" 2 "status 2 must reserve 2 rows")
                    ("5" 5 "status 5 → 5 rows")
                    ("9" 5 "status 9 → clamped to 5 rows")))
      (destructuring-bind (value expected desc) case
        (declare (ignore desc))
        (with-isolated-config
          (cl-tmux/config:apply-config-directive (list "set-option" "-g" "status" value))
          (expect (= expected cl-tmux/config:*status-height*))))))

  ;;; apply-config-line

  ;; apply-config-line applies a directive line and returns T.
  (it "apply-config-line-applies-valid-directives"
    (with-isolated-config
      (expect (eq t (cl-tmux/config::apply-config-line "bind z new-window")))
      (expect (eq :new-window (lookup-key-binding #\z)))))

  ;; apply-config-line returns NIL for blank lines and # comments.
  (it "apply-config-line-ignores-blank-and-comments"
    (expect (null (cl-tmux/config::apply-config-line "")))
    (expect (null (cl-tmux/config::apply-config-line "   ")))
    (expect (null (cl-tmux/config::apply-config-line "# this is a comment")))))
