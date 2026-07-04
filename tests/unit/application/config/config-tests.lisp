(in-package #:cl-tmux/test)

;;;; Configuration and key-binding core model tests.
;;;;
;;;; This file owns the small, direct invariants around prefix key constants,
;;;; binding lookup, key-table shape, and bind/unbind mutation.  List-keys
;;;; rendering coverage lives in config-key-description-tests.lisp; runtime
;;;; key-table state and default process options live in
;;;; config-key-table-runtime-tests.lisp.

(def-suite config-suite :description "Key bindings and configuration")
(in-suite config-suite)

;;; ── Import the config symbols we need ────────────────────────────────────

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(cl-tmux/config:lookup-key-binding
            cl-tmux/config:+prefix-key-code+
            cl-tmux/config:+ctrl-mask+
            cl-tmux/config:key-table-bind
            cl-tmux/config:key-table-unbind)))

;;; ── Constant value ─────────────────────────────────────────────────────────

(test prefix-key-code
  "+prefix-key-code+ is 2 (ASCII STX / C-b)."
  (is (= 2 +prefix-key-code+)
      "+prefix-key-code+ should be 2, got ~A" +prefix-key-code+))

(test ctrl-mask-constant
  "+ctrl-mask+ is #x1f, the bitmask mapping an ASCII letter to its control byte."
  (is (= #x1f +ctrl-mask+)
      "+ctrl-mask+ should be #x1f, got ~A" +ctrl-mask+)
  (is (= 2 (logand (char-code #\B) +ctrl-mask+))
      "C-b (#\\B masked) should be byte 2, matching +prefix-key-code+"))

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
