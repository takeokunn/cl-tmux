(in-package #:cl-tmux/test)

;;;; channel helper and list capping contracts

(in-suite runtime-suite)

;;; ── %cap-list ─────────────────────────────────────────────────────────────────

(test cap-list-returns-list-unchanged-when-under-limit
  "%cap-list returns the list unchanged when its length is <= limit."
  (let ((lst '(1 2 3)))
    (is (equal '(1 2 3) (cl-tmux::%cap-list lst 5))
        "%cap-list must return list unchanged when length <= limit")
    (is (equal '(1 2 3) (cl-tmux::%cap-list lst 3))
        "%cap-list must return list unchanged when length == limit")))

(test cap-list-truncates-when-over-limit
  "%cap-list returns a subseq of at most LIMIT elements when the list is longer."
  (let ((lst '(a b c d e)))
    (is (equal '(a b c) (cl-tmux::%cap-list lst 3))
        "%cap-list must truncate to exactly LIMIT elements")))

(test cap-list-returns-nil-for-nil-input
  "%cap-list returns NIL for NIL input (empty list)."
  (is (null (cl-tmux::%cap-list nil 5))
      "%cap-list of NIL must return NIL"))

(test cap-list-returns-nil-for-zero-limit
  "%cap-list returns NIL when limit is 0."
  (is (null (cl-tmux::%cap-list '(1 2 3) 0))
      "%cap-list with limit 0 must return NIL"))

;;; ── with-channel-plist macro ──────────────────────────────────────────────────

(test with-channel-plist-binds-lock-and-cv
  "with-channel-plist binds LK and CV to the :lock and :cv fields of a channel plist."
  (let ((cl-tmux::*wait-channels* (make-hash-table :test #'equal)))
    (let ((ch (cl-tmux::%ensure-channel "wplist-test")))
      (cl-tmux::with-channel-plist (lk cv ch)
        (is (eq (getf ch :lock) lk) "LK must be the :lock field")
        (is (eq (getf ch :cv) cv) "CV must be the :cv field")))))

(test with-channel-plist-is-a-macro
  "with-channel-plist is defined as a macro."
  (is (macro-function 'cl-tmux::with-channel-plist)
      "with-channel-plist must be a macro"))
