;;;; CPS keystroke-pipeline helpers for cl-tmux tests.

(in-package #:cl-tmux/test)

;;; ── Custom matcher: CPS "reset to ground state" assertions ──────────────────
;;;
;;; Dozens of events/keystroke tests drive a CPS handler and then assert its
;;; two return values are the standard "sequence consumed, reset to ground"
;;; outcome — previously spelled out by hand at each call site as:
;;;   (multiple-value-bind (outcome next) (handler ...)
;;;     (expect (null outcome))
;;;     (expect (eq #'cl-tmux::%ground-input-state next)))
;;; A cl-weave custom matcher collapses that into a single readable assertion:
;;; (expect (multiple-value-list (handler ...)) :to-return-to-ground).
(cl-weave:defmatcher :to-return-to-ground (actual expected)
  "T when ACTUAL is the (multiple-value-list ...) of a CPS handler call whose
   result is (values NIL #'%GROUND-INPUT-STATE) — the standard outcome for a
   fully-consumed sequence that resets input to ground state."
  (declare (ignore expected))
  (destructuring-bind (outcome next) actual
    (values (and (null outcome) (eq next #'cl-tmux::%ground-input-state))
            (list :outcome outcome :next next)
            (list :outcome nil :next :ground-input-state))))
