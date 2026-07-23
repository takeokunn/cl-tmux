(in-package #:cl-tmux/test)

;;;; parser tests — part C2: parser-inline-predicates.

;;; ── SUITE: parser-inline-predicates ─────────────────────────────────────────
;;;
;;; These tests call the inline predicate helpers in cl-tmux/terminal/parser
;;; directly, verifying boundary conditions that the parser integration tests
;;; do not assert explicitly.

(describe "terminal-suite/parser-inline-predicates"

  ;; printable-ascii-p is T for #x20-#x7E and NIL outside that range.
  (it "printable-ascii-p-range"
    (expect (cl-tmux/terminal/parser::printable-ascii-p #x20) :to-be-truthy)
    (expect (cl-tmux/terminal/parser::printable-ascii-p #x41) :to-be-truthy) ; A
    (expect (cl-tmux/terminal/parser::printable-ascii-p #x7E) :to-be-truthy)
    (expect (cl-tmux/terminal/parser::printable-ascii-p #x1F) :to-be-falsy)
    (expect (cl-tmux/terminal/parser::printable-ascii-p #x7F) :to-be-falsy))

  ;; utf8-lead-p is T for #xC0-#xFE and NIL for ASCII or continuation bytes.
  (it "utf8-lead-p-identifies-lead-bytes"
    (expect (cl-tmux/terminal/parser::utf8-lead-p #xC2) :to-be-truthy)
    (expect (cl-tmux/terminal/parser::utf8-lead-p #xE3) :to-be-truthy)
    (expect (cl-tmux/terminal/parser::utf8-lead-p #xF0) :to-be-truthy)
    (expect (cl-tmux/terminal/parser::utf8-lead-p #x41) :to-be-falsy)  ; ASCII A
    (expect (cl-tmux/terminal/parser::utf8-lead-p #x80) :to-be-falsy)  ; continuation
    (expect (cl-tmux/terminal/parser::utf8-lead-p #xFF) :to-be-falsy)) ; excluded

  ;; utf8-continuation-p is T for #x80-#xBF.
  (it "utf8-continuation-p-identifies-continuation-bytes"
    (expect (cl-tmux/terminal/parser::utf8-continuation-p #x80) :to-be-truthy)
    (expect (cl-tmux/terminal/parser::utf8-continuation-p #xBF) :to-be-truthy)
    (expect (cl-tmux/terminal/parser::utf8-continuation-p #x41) :to-be-falsy)
    (expect (cl-tmux/terminal/parser::utf8-continuation-p #xC0) :to-be-falsy))

  ;; utf8-lead-decode gives (acc, remaining-bytes) for 2/3/4-byte sequences.
  (it "utf8-lead-decode-returns-initial-accumulators"
    (dolist (row '((#xC2 2 1 "2-byte leader")
                   (#xE3 3 2 "3-byte leader")
                   (#xF0 0 3 "4-byte leader")))
      (destructuring-bind (byte expected-acc expected-left desc) row
        (declare (ignore desc))
        (multiple-value-bind (acc left) (cl-tmux/terminal/parser::utf8-lead-decode byte)
          (expect (= expected-acc acc))
          (expect (= expected-left left)))))))
