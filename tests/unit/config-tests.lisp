(in-package #:cl-tmux/test)

;;;; Configuration and key-binding tests.
;;;;
;;;; These tests are purely functional (no PTY, no threads) and cover:
;;;;   • the compile-time constant +prefix-key-code+,
;;;;   • known bindings in the default prefix key-table,
;;;;   • the lookup-key-binding helper, and
;;;;   • structural invariants of the prefix key-table itself.
;;;; The default-value, initialization, and parse coverage lives in
;;;; config-tests-defaults.lisp so this file stays focused on the core model.

(def-suite config-suite :description "Key bindings and configuration")
(in-suite config-suite)

;;; ── Import the config symbols we need ────────────────────────────────────

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(cl-tmux/config:lookup-key-binding
             cl-tmux/config:describe-key-bindings
             cl-tmux/config:+prefix-key-code+
            cl-tmux/config:+max-scrollback-lines+
            cl-tmux/config:+poll-timeout-us+
            cl-tmux/config:+accept-timeout-us+
            cl-tmux/config:+pty-buf-size+
            cl-tmux/config:+pty-poll-timeout-us+
            cl-tmux/config:key-table-bind
            cl-tmux/config:key-table-unbind)))

;;; ── Constant value ─────────────────────────────────────────────────────────

(test prefix-key-code
  "+prefix-key-code+ is 2 (ASCII STX / C-b)."
  (is (= 2 +prefix-key-code+)
      "+prefix-key-code+ should be 2, got ~A" +prefix-key-code+))

;;; ── Known default bindings ────────────────────────────────────────────────

(test lookup-known-bindings-table
  "C-b c creates a new window; C-b d detaches the client."
  (dolist (row '((#\c :new-window "#\\c → :new-window")
                 (#\d :detach     "#\\d → :detach")))
    (destructuring-bind (key expected desc) row
      (is (eq expected (lookup-key-binding key)) "~A" desc))))

(test lookup-unknown-returns-nil
  "An unbound key returns NIL.  #\\z is now bound to :zoom-toggle, so we
   use #\\@ (ASCII 64) which has no default binding."
  (is (null (lookup-key-binding #\@))
      "#\\@ should return NIL (unbound)"))

;;; ── Structural invariants of prefix key-table ──────────────────────────────

(test all-bindings-have-keyword-or-list-values
  "Every value in the prefix key-table is a keyword symbol or a command token form."
  (let* ((tbl (cl-tmux/config:ensure-key-table "prefix"))
         (keys nil))
    (maphash (lambda (k v) (declare (ignore k)) (push v keys)) tbl)
    (dolist (entry keys)
      (let ((cmd (cl-tmux/config:key-table-command entry)))
        (is (or (keywordp cmd)
                (and (consp cmd)
                     (or (every (lambda (part)
                                  (or (stringp part) (symbolp part)))
                                cmd)
                         (and (eq 'quote (first cmd))
                              (consp (second cmd))
                              (every (lambda (part)
                                       (or (stringp part) (symbolp part)))
                                     (second cmd))))))
            "entry ~A should have a keyword or token-list command, got ~A"
            entry cmd)))))

(test all-bindings-have-char-or-string-keys
  "Every key in the prefix key-table is a character or a string."
  (let* ((tbl (cl-tmux/config:ensure-key-table "prefix"))
         (keys nil))
    (maphash (lambda (k v) (declare (ignore v)) (push k keys)) tbl)
    (dolist (k keys)
      (is (or (characterp k)
              (stringp    k))
          "key ~A should be a character or string, got ~A"
          k (type-of k)))))

;;; ── define-initial-key-bindings macro ─────────────────────────────────────
;;;
;;; define-initial-key-bindings expands to side-effecting key-table-bind calls.
;;; It does NOT return an alist.  Tests verify the side effects via key-table-lookup.

(test define-initial-key-bindings-macro-populates-key-table
  "define-initial-key-bindings expands to install-default-prefix-bindings, which
   populates the prefix key-table for char and digit entries when called."
  ;; The macro now expands to (defun install-default-prefix-bindings ...) rather
  ;; than emitting side effects, so we must CALL the generated installer to
  ;; populate the table.  Because the macro redefines the GLOBAL installer with
  ;; this test's custom binding set, save and restore its real definition — else
  ;; later tests that rebuild defaults via initialize-default-key-tables would
  ;; inherit a prefix table missing #\d, #\x, etc. (a cross-test cascade).
  (let ((cl-tmux/config:*key-tables* (make-hash-table :test #'equal))
        (saved-installer
          (fdefinition 'cl-tmux/config::install-default-prefix-bindings)))
    (unwind-protect
         (progn
           (define-initial-key-bindings
             (#\c :new-window)
             (:digits :select-window))
           (cl-tmux/config::install-default-prefix-bindings)
           ;; #\c → :new-window
           (let ((entry (cl-tmux/config:key-table-lookup "prefix" #\c)))
             (is (not (null entry)) "#\\c must have a prefix binding")
             (is (eq :new-window (cl-tmux/config:key-table-command entry))
                 "char entry must bind :new-window"))
           ;; digits 0-9 → :select-window
           (dolist (d '(#\0 #\1 #\5 #\9))
             (let ((entry (cl-tmux/config:key-table-lookup "prefix" d)))
               (is (not (null entry)) "digit ~C must have a prefix binding" d)
               (is (eq :select-window (cl-tmux/config:key-table-command entry))
                   "digit ~C must bind :select-window" d)))
           ;; 11 total entries: 1 char + 10 digits
           (let ((tbl (cl-tmux/config:ensure-key-table "prefix")))
             (is (= 11 (hash-table-count tbl))
                 "prefix table must have exactly 11 entries (1 char + 10 digits)")))
      (setf (fdefinition 'cl-tmux/config::install-default-prefix-bindings)
            saved-installer))))

;;; ── key-table-bind / key-table-unbind ─────────────────────────────────────

(test key-table-bind-adds-new
  "key-table-bind adds a brand-new binding that lookup-key-binding finds.
   Uses #\\@ (ASCII 64) which has no default binding."
  (with-isolated-config
    (is (null (lookup-key-binding #\@))
        "#\\@ should start unbound")
    (key-table-bind "prefix" #\@ :new-window)
    (is (eq :new-window (lookup-key-binding #\@))
        "#\\@ should be bound to :new-window after key-table-bind")))

(test key-table-bind-replaces-existing
  "key-table-bind on an existing key replaces the command without duplicating."
  (with-isolated-config
    (key-table-bind "prefix" #\z :new-window)
    (is (eq :new-window (lookup-key-binding #\z))
        "#\\z should be bound to :new-window")
    (let* ((tbl (cl-tmux/config:ensure-key-table "prefix"))
           (before (hash-table-count tbl)))
      (key-table-bind "prefix" #\z :detach)
      (is (eq :detach (lookup-key-binding #\z))
          "#\\z should now be bound to :detach")
      (let ((after (hash-table-count tbl)))
        (is (= before after)
            "prefix table size should not grow (replace, not duplicate)")))))

(test key-table-unbind-removes
  "key-table-unbind removes a binding so lookup returns NIL afterward."
  (with-isolated-config
    (key-table-bind "prefix" #\z :new-window)
    (is (eq :new-window (lookup-key-binding #\z))
        "#\\z should be bound before removal")
    (key-table-unbind "prefix" #\z)
    (is (null (lookup-key-binding #\z))
        "#\\z should be unbound after key-table-unbind")))

;;; ── describe-key-bindings (list-keys help text) ─────────────────────────────

(test describe-key-bindings-lists-commands
  "describe-key-bindings produces help text naming the bound commands."
  (let ((text (describe-key-bindings)))
    (dolist (sub '("new-window" "detach" "select-window"))
      (is (search sub text) "should list ~A" sub))))

(test prefix-named-single-byte-key-reachable-at-runtime
  "Runtime prefix dispatch resolves a named single-byte key binding (Enter, byte
   13) via candidate probing, so `bind Enter <cmd>` is reachable — not just
   printable single characters (audit #35)."
  (with-isolated-config
    (cl-tmux/config:load-config-from-string "bind Enter next-window")
    (let ((entry (cl-tmux::%key-table-entry-by-candidates
                  cl-tmux/config:+table-prefix+
                  (cl-tmux::%single-byte-key-candidates 13))))
      (is (not (null entry))
          "Enter (byte 13) must resolve a prefix-table entry after bind Enter")
      (is (eq :next-window (cl-tmux/config:key-table-command entry))
          "the resolved entry must be the bound next-window command"))))

(test default-prefix-string-bindings-are-listed-and-repeatable
  "Default prefix multi-byte keys are present in the key table and resize arrows are repeatable."
  (with-isolated-config
    (let ((up-entry (cl-tmux/config:key-table-lookup "prefix" "Up"))
          (ctrl-right-entry (cl-tmux/config:key-table-lookup "prefix" "C-Right"))
          (meta-right-entry (cl-tmux/config:key-table-lookup "prefix" "M-Right"))
          (text (cl-tmux/config:describe-key-bindings-for-table "prefix")))
      (is (eq :select-pane-up (cl-tmux/config:key-table-command up-entry))
          "prefix Up must select the pane above")
      (is (equal '("resize-pane" "-R" "1")
                 (cl-tmux/config:key-table-command ctrl-right-entry))
          "prefix C-Right must resize right by 1")
      (is (equal '("resize-pane" "-R" "5")
                 (cl-tmux/config:key-table-command meta-right-entry))
          "prefix M-Right must resize right by 5")
      (is (cl-tmux/config:key-table-repeatable-p ctrl-right-entry)
          "prefix C-Right must be repeatable")
      (is (cl-tmux/config:key-table-repeatable-p meta-right-entry)
          "prefix M-Right must be repeatable")
      (is (search "bind-key -T prefix Up select-pane-up" text)
          "list-keys must show the prefix Up binding")
      (is (search "bind-key -T prefix -r C-Right resize-pane -R 1" text)
          "list-keys must show repeatable C-Right resize binding"))))

(test default-copy-mode-vi-bindings-are-listed
  "Default copy-mode-vi keys are present in the key table and list-keys output."
  (with-isolated-config
    (dolist (row '(("Escape"  :copy-mode-clear-selection              "Escape clears selection")
                   (#\q       :copy-mode-exit                         "q exits copy-mode")
                   (#\i       :copy-mode-exit                         "i exits copy-mode")
                   (#\j       :copy-mode-cursor-down                  "j moves down")
                   (#\h       :copy-mode-cursor-left                  "h moves left")
                   (#\%       :copy-mode-next-matching-bracket        "% jumps to matching bracket")
                   (#\#       :copy-mode-search-backward-word         "# searches backward for word")
                   (#\*       :copy-mode-search-forward-word          "* searches forward for word")
                   (#\,       :copy-mode-jump-reverse                 ", reverses last jump")
                   (#\;       :copy-mode-jump-again                   "; repeats last jump")
                   (#\A       :copy-mode-append-selection-and-cancel  "A appends selection and cancels")
                   (#\D       :copy-mode-copy-pipe-end-of-line-and-cancel "D copies to end of line")
                   (#\J       :copy-mode-scroll-down-line             "J scrolls down one line")
                   (#\K       :copy-mode-scroll-up-line               "K scrolls up one line")
                   (#\P       :copy-mode-other-end                    "P toggles position")
                   (#\R       :copy-mode-rectangle-toggle             "R toggles rectangle selection")
                   (#\X       :copy-mode-set-mark                     "X sets mark")
                   (#\o       :copy-mode-other-end                    "o moves to other selection end")
                   (#\z       :copy-mode-scroll-middle                "z centres the cursor")
                   (#\r       :copy-mode-refresh-from-pane            "r refreshes from pane")
                   (#\f       :copy-mode-jump-forward                 "f jumps forward to char")
                   (#\F       :copy-mode-jump-backward                "F jumps backward to char")
                   (#\t       :copy-mode-jump-to                      "t jumps to before char")
                   (#\T       :copy-mode-jump-to-backward             "T jumps backward to before char")
                   (#\:       :copy-mode-goto-line                    ": opens goto-line prompt")
                   (#\{       :copy-mode-prev-paragraph               "{ moves to prev paragraph")
                   (#\}       :copy-mode-next-paragraph               "} moves to next paragraph")
                   ("M-x"     :copy-mode-jump-to-mark                  "M-x jumps to mark")
                   ("C-b"     :copy-mode-page-up                       "C-b pages up")
                   ("C-v"     :copy-mode-rectangle-toggle              "C-v toggles rectangle selection")
                   ("C-Up"    :copy-mode-scroll-up-line                "C-Up scrolls up one line")
                   ("C-Down"  :copy-mode-scroll-down-line              "C-Down scrolls down one line")
                   ("Enter"   :copy-mode-copy-pipe-and-cancel          "Enter copy-pipe-and-cancel")
                   ("BSpace"  :copy-mode-cursor-left                   "BSpace moves left")
                   ("PageUp"  :copy-mode-page-up                       "PageUp pages up")))
      (destructuring-bind (key expected msg) row
        (let ((entry (cl-tmux/config:key-table-lookup "copy-mode-vi" key)))
          (is (eq expected (cl-tmux/config:key-table-command entry))
              (format nil "copy-mode-vi ~A: ~A" key msg)))))
    (let ((text (cl-tmux/config:describe-key-bindings-for-table "copy-mode-vi")))
      (dolist (fragment '("bind-key -T copy-mode-vi j copy-mode-cursor-down"
                          "bind-key -T copy-mode-vi Escape copy-mode-clear-selection"
                          "bind-key -T copy-mode-vi q copy-mode-exit"
                          "bind-key -T copy-mode-vi i copy-mode-exit"
                          "bind-key -T copy-mode-vi f copy-mode-jump-forward"
                          "bind-key -T copy-mode-vi F copy-mode-jump-backward"
                          "bind-key -T copy-mode-vi t copy-mode-jump-to"
                          "bind-key -T copy-mode-vi T copy-mode-jump-to-backward"
                          "bind-key -T copy-mode-vi r copy-mode-refresh-from-pane"
                          "bind-key -T copy-mode-vi : copy-mode-goto-line"
                          "bind-key -T copy-mode-vi % copy-mode-next-matching-bracket"
                          "bind-key -T copy-mode-vi # copy-mode-search-backward-word"
                          "bind-key -T copy-mode-vi * copy-mode-search-forward-word"
                          "bind-key -T copy-mode-vi A copy-mode-append-selection-and-cancel"
                          "bind-key -T copy-mode-vi M-x copy-mode-jump-to-mark"
                          "bind-key -T copy-mode-vi C-b copy-mode-page-up"
                          "bind-key -T copy-mode-vi C-Up copy-mode-scroll-up-line"
                          "bind-key -T copy-mode-vi Enter copy-mode-copy-pipe-and-cancel"
                          "bind-key -T copy-mode-vi PageUp copy-mode-page-up"))
        (is (search fragment text)
            (format nil "list-keys must show ~A" fragment))))))

(test default-copy-mode-emacs-control-bindings
  "Default copy-mode emacs control keys match tmux movement, selection and search defaults."
  (with-isolated-config
    (dolist (row '(("C-Space" :copy-mode-begin-selection "C-Space begins selection")
                   ("C-a" :copy-mode-line-start "C-a moves to line start")
                   ("C-c" :copy-mode-exit "C-c exits copy-mode")
                   ("C-e" :copy-mode-line-end "C-e moves to line end")
                   ("C-f" :copy-mode-cursor-right "C-f moves right")
                   ("C-b" :copy-mode-cursor-left "C-b moves left")
                   ("C-g" :copy-mode-clear-selection "C-g clears selection")
                   ("C-l" :copy-mode-cursor-centre-vertical
                    "C-l centres the cursor vertically")
                   ("C-k" :copy-mode-copy-pipe-end-of-line-and-cancel
                    "C-k copy-pipes to end of line and cancels")
                   ("C-n" :copy-mode-cursor-down "C-n moves down")
                   ("C-p" :copy-mode-cursor-up "C-p moves up")
                   ("C-r" :copy-mode-search-backward-incremental
                    "C-r starts backward incremental search")
                   ("C-s" :copy-mode-search-forward-incremental
                    "C-s starts forward incremental search")
                   ("C-v" :copy-mode-page-down "C-v pages down")
                   ("C-w" :copy-mode-copy-pipe-and-cancel
                    "C-w copy-pipes and cancels")))
      (destructuring-bind (key expected message) row
        (let ((entry (cl-tmux/config:key-table-lookup "copy-mode" key)))
          (is (eq expected (cl-tmux/config:key-table-command entry))
              "copy-mode ~A" message))))))

(test default-copy-mode-emacs-printable-and-meta-bindings
  "Default copy-mode emacs printable and meta keys expose tmux navigation defaults."
  (with-isolated-config
    (let ((text (cl-tmux/config:describe-key-bindings-for-table "copy-mode")))
      (dolist (row `((#\Space :copy-mode-page-down "Space pages down")
                     (#\, :copy-mode-jump-reverse ", reverses the last jump")
                     (#\; :copy-mode-jump-again "; repeats the last jump")
                     (#\N :copy-mode-search-prev "N repeats search backward")
                     (#\P :copy-mode-other-end "P toggles position")
                     (#\R :copy-mode-rectangle-toggle "R toggles rectangle")
                     (#\X :copy-mode-set-mark "X sets mark")
                     (#\n :copy-mode-search-next "n repeats search forward")
                     (#\r :copy-mode-refresh-from-pane "r refreshes from pane")
                     (#\f :copy-mode-jump-forward "f jumps forward")
                     (#\F :copy-mode-jump-backward "F jumps backward")
                     (#\t :copy-mode-jump-to "t jumps to a character")
                     (#\T :copy-mode-jump-to-backward
                      "T jumps backward to a character")
                     (#\g :copy-mode-goto-line "g opens the goto-line prompt")
                     ("M-f" :copy-mode-word-end "M-f moves to next word end")
                     ("M-l" :copy-mode-cursor-centre-horizontal
                      "M-l centres the cursor horizontally")
                     ("M-x" :copy-mode-jump-to-mark "M-x jumps to mark")
                     ("M-{" :copy-mode-prev-paragraph
                      "M-{ moves to the previous paragraph")
                     ("M-}" :copy-mode-next-paragraph
                      "M-} moves to the next paragraph")))
        (destructuring-bind (key expected message) row
          (let ((entry (cl-tmux/config:key-table-lookup "copy-mode" key)))
            (is (eq expected (cl-tmux/config:key-table-command entry))
                "copy-mode ~A" message))))
      (is (search "bind-key -T copy-mode C-Space copy-mode-begin-selection" text)
          "list-keys must show copy-mode C-Space")
      (is (search "bind-key -T copy-mode C-l copy-mode-cursor-centre-vertical" text)
          "list-keys must show copy-mode C-l")
      (is (search "bind-key -T copy-mode M-f copy-mode-word-end" text)
          "list-keys must show copy-mode M-f")
      (is (search "bind-key -T copy-mode M-l copy-mode-cursor-centre-horizontal"
                  text)
          "list-keys must show copy-mode M-l")
      (is (search "bind-key -T copy-mode f copy-mode-jump-forward" text)
          "list-keys must show copy-mode f")
      (is (search "bind-key -T copy-mode F copy-mode-jump-backward" text)
          "list-keys must show copy-mode F")
      (is (search "bind-key -T copy-mode t copy-mode-jump-to" text)
          "list-keys must show copy-mode t")
      (is (search "bind-key -T copy-mode T copy-mode-jump-to-backward" text)
          "list-keys must show copy-mode T")
      (is (search "bind-key -T copy-mode g copy-mode-goto-line" text)
          "list-keys must show copy-mode g")
      (is (search "bind-key -T copy-mode M-x copy-mode-jump-to-mark" text)
          "list-keys must show copy-mode M-x"))))

(test default-copy-mode-emacs-q-exits
  "Default copy-mode emacs q exits copy-mode and appears in list-keys output."
  (with-isolated-config
    (let ((escape-entry (cl-tmux/config:key-table-lookup "copy-mode" "Escape"))
          (q-entry (cl-tmux/config:key-table-lookup "copy-mode" #\q))
          (text (cl-tmux/config:describe-key-bindings-for-table "copy-mode")))
      (is (eq :copy-mode-exit
              (cl-tmux/config:key-table-command escape-entry))
          "copy-mode Escape must exit copy-mode")
      (is (eq :copy-mode-exit
              (cl-tmux/config:key-table-command q-entry))
          "copy-mode q must exit copy-mode")
      (is (search "bind-key -T copy-mode Escape copy-mode-exit" text)
          "list-keys must show copy-mode Escape")
      (is (search "bind-key -T copy-mode q copy-mode-exit" text)
          "list-keys must show copy-mode q"))))

(test default-copy-mode-emacs-control-meta-bracket-bindings
  "Default copy-mode emacs C-M-b/C-M-f jump between matching brackets."
  (with-isolated-config
    (let ((prev-entry (cl-tmux/config:key-table-lookup "copy-mode" "C-M-b"))
          (next-entry (cl-tmux/config:key-table-lookup "copy-mode" "C-M-f"))
          (text (cl-tmux/config:describe-key-bindings-for-table "copy-mode")))
      (is (eq :copy-mode-previous-matching-bracket
              (cl-tmux/config:key-table-command prev-entry))
          "copy-mode C-M-b must jump to the previous matching bracket")
      (is (eq :copy-mode-next-matching-bracket
              (cl-tmux/config:key-table-command next-entry))
          "copy-mode C-M-f must jump to the next matching bracket")
      (is (search "bind-key -T copy-mode C-M-b copy-mode-previous-matching-bracket"
                  text)
          "list-keys must show copy-mode C-M-b")
      (is (search "bind-key -T copy-mode C-M-f copy-mode-next-matching-bracket"
                  text)
          "list-keys must show copy-mode C-M-f"))))

(test default-copy-mode-emacs-modifier-arrow-bindings
  "Default copy-mode emacs C-Up/C-Down and M-Up/M-Down match tmux scrolling keys."
  (with-isolated-config
    (let ((c-up-entry (cl-tmux/config:key-table-lookup "copy-mode" "C-Up"))
          (c-down-entry (cl-tmux/config:key-table-lookup "copy-mode" "C-Down"))
          (m-up-entry (cl-tmux/config:key-table-lookup "copy-mode" "M-Up"))
          (m-down-entry (cl-tmux/config:key-table-lookup "copy-mode" "M-Down"))
          (text (cl-tmux/config:describe-key-bindings-for-table "copy-mode")))
      (is (eq :copy-mode-scroll-up-line
              (cl-tmux/config:key-table-command c-up-entry))
          "copy-mode C-Up must scroll up one line")
      (is (eq :copy-mode-scroll-down-line
              (cl-tmux/config:key-table-command c-down-entry))
          "copy-mode C-Down must scroll down one line")
      (is (eq :copy-mode-half-page-up
              (cl-tmux/config:key-table-command m-up-entry))
          "copy-mode M-Up must half-page up")
      (is (eq :copy-mode-half-page-down
              (cl-tmux/config:key-table-command m-down-entry))
          "copy-mode M-Down must half-page down")
      (is (search "bind-key -T copy-mode C-Up copy-mode-scroll-up-line" text)
          "list-keys must show copy-mode C-Up")
      (is (search "bind-key -T copy-mode M-Down copy-mode-half-page-down" text)
          "list-keys must show copy-mode M-Down"))))

(test describe-key-bindings-for-key-filters-by-key-label
  "describe-key-bindings-for-key returns only bindings whose key label matches."
  (with-isolated-config
    (let ((text (cl-tmux/config:describe-key-bindings-for-key "prefix" "C-Right")))
      (is (search "bind-key -T prefix -r C-Right resize-pane -R 1" text)
          "C-Right binding must be listed")
      (is (null (search "bind-key -T prefix Up select-pane-up" text))
          "different keys must be omitted"))))

;;; ── +max-scrollback-lines+ constant ───────────────────────────────────────

(test max-scrollback-lines-constant
  "+max-scrollback-lines+ equals 1000."
  (is (= 1000 +max-scrollback-lines+)
      "+max-scrollback-lines+ must be 1000, got ~A" +max-scrollback-lines+))

;;; ── Numeric compile-time constants ─────────────────────────────────────────

;;; ── Key-table system tests ────────────────────────────────────────────────

(test key-tables-initialized
  "*key-tables* is populated after load."
  (is (hash-table-p cl-tmux/config:*key-tables*)
      "*key-tables* must be a hash-table"))

(test key-tables-required-tables-exist
  "The standard key-tables are created by initialize-default-key-tables."
  (dolist (name '("prefix" "root" "copy-mode" "copy-mode-vi"))
    (is (not (null (gethash name cl-tmux/config:*key-tables*)))
        "\"~A\" table must exist in *key-tables*" name)))

(test key-table-bind-table
  "key-table-bind stores a binding retrievable by key-table-lookup in both 'root' and 'prefix' tables."
  (dolist (row '(("root"   #\a "root table binding")
                 ("prefix" #\c "prefix table binding")))
    (destructuring-bind (table-name key desc) row
      (let ((cl-tmux/config:*key-tables* (make-hash-table :test #'equal)))
        (cl-tmux/config:key-table-bind table-name key :new-window)
        (let ((entry (cl-tmux/config:key-table-lookup table-name key)))
          (is (not (null entry)) "~A: binding must be found" desc)
          (is (eq :new-window (cl-tmux/config:key-table-command entry))
              "~A: command must be :new-window" desc))))))

(test key-table-repeatable-flag-variants
  "key-table-bind with :repeatable T marks the entry repeatable; without the flag
   it defaults to not repeatable.  Each row: (key cmd rep expected description)."
  (dolist (row '((#\r :resize-left t   t   ":repeatable T must be repeatable")
                 (#\c :new-window  nil nil "no :repeatable flag must not be repeatable")))
    (destructuring-bind (key cmd rep expected desc) row
      (let ((cl-tmux/config:*key-tables* (make-hash-table :test #'equal)))
        (cl-tmux/config:key-table-bind "prefix" key cmd :repeatable rep)
        (let ((entry (cl-tmux/config:key-table-lookup "prefix" key)))
          (is (not (null entry)) "binding must be found: ~A" desc)
          (is (eql expected (cl-tmux/config:key-table-repeatable-p entry))
              desc))))))

(test key-table-lookup-missing-returns-nil
  "key-table-lookup returns NIL for an absent key and for a non-existent table."
  (let ((cl-tmux/config:*key-tables* (make-hash-table :test #'equal)))
    (is (null (cl-tmux/config:key-table-lookup "prefix"      #\z))
        "absent key must return NIL")
    (is (null (cl-tmux/config:key-table-lookup "nonexistent" #\a))
        "absent table must return NIL")))

;;; ── key-table-repeatable-p nil-safe guard ─────────────────────────────────

(test key-table-repeatable-p-nil-safe
  "key-table-repeatable-p returns NIL when passed NIL (nil-safe guard)."
  (is (null (cl-tmux/config:key-table-repeatable-p nil))
      "key-table-repeatable-p NIL must return NIL without signaling"))

(test key-table-command-nil-safe
  "key-table-command is the car of the entry; key-table-repeatable-p is nil-safe."
  (let ((cl-tmux/config:*key-tables* (make-hash-table :test #'equal)))
    ;; Absent key returns NIL; nil-safe guard means no error.
    (let ((absent (cl-tmux/config:key-table-lookup "prefix" #\@)))
      (is (null absent) "absent key must return NIL")
      (is (null (cl-tmux/config:key-table-repeatable-p absent))
          "key-table-repeatable-p on NIL must return NIL"))))

(test numeric-constants
  "Timeout and buffer-size constants have the expected values and are all positive."
  (dolist (row `((,+poll-timeout-us+     50000  "+poll-timeout-us+")
                 (,+accept-timeout-us+   100000 "+accept-timeout-us+")
                 (,+pty-buf-size+        4096   "+pty-buf-size+")
                 (,+pty-poll-timeout-us+ 50000  "+pty-poll-timeout-us+")))
    (destructuring-bind (value expected name) row
      (is (= expected value)  "~A must be ~A" name expected)
      (is (plusp value)       "~A must be positive" name))))

;;; ── ensure-key-table side effects ────────────────────────────────────────

(test ensure-key-table-creates-new-table
  "ensure-key-table creates a fresh hash-table for a previously unknown name."
  (let ((cl-tmux/config:*key-tables* (make-hash-table :test #'equal)))
    (let ((tbl (cl-tmux/config:ensure-key-table "my-table")))
      (is (hash-table-p tbl)
          "ensure-key-table must return a hash-table")
      (is (eq tbl (gethash "my-table" cl-tmux/config:*key-tables*))
          "the returned table must be stored in *key-tables*"))))

(test ensure-key-table-returns-existing-table
  "ensure-key-table returns the same table on repeated calls."
  (let ((cl-tmux/config:*key-tables* (make-hash-table :test #'equal)))
    (let* ((tbl1 (cl-tmux/config:ensure-key-table "my-table"))
           (tbl2 (cl-tmux/config:ensure-key-table "my-table")))
      (is (eq tbl1 tbl2)
          "ensure-key-table must return the same object on repeated calls"))))

;;; ── lookup-key-binding on digit keys ────────────────────────────────────

(test lookup-digit-keys-bind-select-window
  "The digit characters 0-9 all bind :select-window in the default prefix table."
  (dolist (d '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
    (is (eq :select-window (lookup-key-binding d))
        "digit ~C must be bound to :select-window" d)))

;;; ── send-prefix binding ──────────────────────────────────────────────────

(test send-prefix-binding
  "The prefix key itself (code-char 2 = C-b) is bound to :send-prefix."
  (is (eq :send-prefix (lookup-key-binding (code-char +prefix-key-code+)))
      "C-b (prefix key) must be bound to :send-prefix"))

;;; ── describe-key-bindings header ────────────────────────────────────────

(test describe-key-bindings-has-header
  "describe-key-bindings output uses bind-key -T table format (real tmux list-keys format)."
  (let ((text (describe-key-bindings)))
    (is (search "bind-key" text)
        "output must contain 'bind-key' (real tmux list-keys format)")
    (is (search "-T" text)
        "output must contain '-T' (table specifier)")))

;;; ── initialize-default-key-tables idempotency ─────────────────────────────

(test initialize-default-key-tables-idempotent
  "Calling initialize-default-key-tables twice does not duplicate bindings."
  (let ((cl-tmux/config:*key-tables* (make-hash-table :test #'equal)))
    (cl-tmux/config::initialize-default-key-tables)
    (let* ((tbl-after-first  (cl-tmux/config:ensure-key-table "prefix"))
           (count-after-first (hash-table-count tbl-after-first)))
      (cl-tmux/config::initialize-default-key-tables)
      (let ((count-after-second (hash-table-count tbl-after-first)))
        (is (= count-after-first count-after-second)
            "prefix table must not grow on second initialize-default-key-tables call")))
    (is (not (null (gethash "root"      cl-tmux/config:*key-tables*)))
        "\"root\" table must exist after double initialization")
    (is (not (null (gethash "copy-mode" cl-tmux/config:*key-tables*)))
        "\"copy-mode\" table must exist after double initialization")
    (is (not (null (gethash "copy-mode-vi" cl-tmux/config:*key-tables*)))
        "\"copy-mode-vi\" table must exist after double initialization")))

;;; ── Key-table name constants ──────────────────────────────────────────────

(test table-name-constants
  "Standard key-table constants have their expected string values."
  (dolist (c `(("prefix"       ,cl-tmux/config:+table-prefix+)
               ("root"         ,cl-tmux/config:+table-root+)
               ("copy-mode"    ,cl-tmux/config:+table-copy-mode+)
               ("copy-mode-vi" ,cl-tmux/config:+table-copy-mode-vi+)))
    (destructuring-bind (expected actual) c
      (is (string= expected actual) "constant must equal ~S" expected))))

;;; ── *default-shell* and *status-height* initial values ───────────────────

(test default-shell-is-string
  "*default-shell* is a non-empty string (set from $SHELL or /bin/sh)."
  (is (stringp cl-tmux/config:*default-shell*)
      "*default-shell* must be a string")
  (is (plusp (length cl-tmux/config:*default-shell*))
      "*default-shell* must not be empty"))

(test status-height-positive-integer
  "*status-height* is a positive integer (default 1)."
  (is (integerp cl-tmux/config:*status-height*)
      "*status-height* must be an integer")
  (is (plusp cl-tmux/config:*status-height*)
      "*status-height* must be positive"))
