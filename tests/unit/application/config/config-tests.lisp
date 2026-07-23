(in-package #:cl-tmux/test)

;;;; Configuration and key-binding core model tests.
;;;;
;;;; This file owns the small, direct invariants around prefix key constants,
;;;; binding lookup, key-table shape, and bind/unbind mutation.  List-keys
;;;; rendering coverage lives in config-key-description-tests.lisp; runtime
;;;; key-table state and default process options live in
;;;; config-key-table-runtime-tests.lisp.

;;; ── Import the config symbols we need ────────────────────────────────────
;;;
;;; This must stay a top-level EVAL-WHEN before the DESCRIBE block below: the
;;; reader interns +prefix-key-code+ / +ctrl-mask+ / etc. as it reads each
;;; subsequent top-level form, so the IMPORT must have already run (at
;;; :compile-toplevel) by the time the reader reaches the DESCRIBE form that
;;; uses these symbols unqualified.  Nesting this inside DESCRIBE would only
;;; run it once the whole DESCRIBE form's body executes, which is too late —
;;; the reader would have already interned fresh, unrelated symbols in
;;; cl-tmux/test while reading the DESCRIBE form's nested IT bodies.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(cl-tmux/config:lookup-key-binding
            cl-tmux/config:+prefix-key-code+
            cl-tmux/config:+ctrl-mask+
            cl-tmux/config:key-table-bind
            cl-tmux/config:key-table-unbind)))

(describe "config-suite"

  ;; ── Constant value ─────────────────────────────────────────────────────────

  ;; +prefix-key-code+ is 2 (ASCII STX / C-b).
  (it "prefix-key-code"
    (expect (= 2 +prefix-key-code+)))

  ;; +ctrl-mask+ is #x1f, the bitmask mapping an ASCII letter to its control byte.
  (it "ctrl-mask-constant"
    (expect (= #x1f +ctrl-mask+))
    (expect (= 2 (logand (char-code #\B) +ctrl-mask+))))

  ;; ── Known default bindings ────────────────────────────────────────────────

  ;; C-b c creates a new window; C-b d detaches the client.
  (it "lookup-known-bindings-table"
    (dolist (row '((#\c :new-window "#\\c → :new-window")
                   (#\d :detach     "#\\d → :detach")))
      (destructuring-bind (key expected desc) row
        (declare (ignore desc))
        (expect (eq expected (lookup-key-binding key))))))

  ;; An unbound key returns NIL.  #\z is now bound to :zoom-toggle, so we
  ;; use #\@ (ASCII 64) which has no default binding.
  (it "lookup-unknown-returns-nil"
    (expect (null (lookup-key-binding #\@))))

  ;; ── Structural invariants of prefix key-table ──────────────────────────────

  ;; Every value in the prefix key-table is a keyword symbol or a command token form.
  (it "all-bindings-have-keyword-or-list-values"
    (let* ((tbl (cl-tmux/config:ensure-key-table "prefix"))
           (keys nil))
      (maphash (lambda (k v) (declare (ignore k)) (push v keys)) tbl)
      (dolist (entry keys)
        (let ((cmd (cl-tmux/config:key-table-command entry)))
          (expect (or (keywordp cmd)
                      (and (consp cmd)
                           (or (every (lambda (part)
                                        (or (stringp part) (symbolp part)))
                                      cmd)
                               (and (eq 'quote (first cmd))
                                    (consp (second cmd))
                                    (every (lambda (part)
                                             (or (stringp part) (symbolp part)))
                                           (second cmd)))))))))))

  ;; Every key in the prefix key-table is a character or a string.
  (it "all-bindings-have-char-or-string-keys"
    (let* ((tbl (cl-tmux/config:ensure-key-table "prefix"))
           (keys nil))
      (maphash (lambda (k v) (declare (ignore v)) (push k keys)) tbl)
      (dolist (k keys)
        (expect (or (characterp k)
                    (stringp    k))))))

  ;; ── define-initial-key-bindings macro ─────────────────────────────────────
  ;;
  ;; define-initial-key-bindings expands to side-effecting key-table-bind calls.
  ;; It does NOT return an alist.  Tests verify the side effects via key-table-lookup.

  ;; define-initial-key-bindings expands to install-default-prefix-bindings, which
  ;; populates the prefix key-table for char and digit entries when called.
  (it "define-initial-key-bindings-macro-populates-key-table"
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
               (expect (not (null entry)))
               (expect (eq :new-window (cl-tmux/config:key-table-command entry))))
             ;; digits 0-9 → :select-window
             (dolist (d '(#\0 #\1 #\5 #\9))
               (let ((entry (cl-tmux/config:key-table-lookup "prefix" d)))
                 (expect (not (null entry)))
                 (expect (eq :select-window (cl-tmux/config:key-table-command entry)))))
             ;; 11 total entries: 1 char + 10 digits
             (let ((tbl (cl-tmux/config:ensure-key-table "prefix")))
               (expect (= 11 (hash-table-count tbl)))))
        (setf (fdefinition 'cl-tmux/config::install-default-prefix-bindings)
              saved-installer))))

  ;; ── key-table-bind / key-table-unbind ─────────────────────────────────────

  ;; key-table-bind adds a brand-new binding that lookup-key-binding finds.
  ;; Uses #\@ (ASCII 64) which has no default binding.
  (it "key-table-bind-adds-new"
    (with-isolated-config
      (expect (null (lookup-key-binding #\@)))
      (key-table-bind "prefix" #\@ :new-window)
      (expect (eq :new-window (lookup-key-binding #\@)))))

  ;; key-table-bind on an existing key replaces the command without duplicating.
  (it "key-table-bind-replaces-existing"
    (with-isolated-config
      (key-table-bind "prefix" #\z :new-window)
      (expect (eq :new-window (lookup-key-binding #\z)))
      (let* ((tbl (cl-tmux/config:ensure-key-table "prefix"))
             (before (hash-table-count tbl)))
        (key-table-bind "prefix" #\z :detach)
        (expect (eq :detach (lookup-key-binding #\z)))
        (let ((after (hash-table-count tbl)))
          (expect (= before after))))))

  ;; key-table-unbind removes a binding so lookup returns NIL afterward.
  (it "key-table-unbind-removes"
    (with-isolated-config
      (key-table-bind "prefix" #\z :new-window)
      (expect (eq :new-window (lookup-key-binding #\z)))
      (key-table-unbind "prefix" #\z)
      (expect (null (lookup-key-binding #\z))))))
