(in-package #:cl-tmux/test)

;;;; Screen tests — part III: screen-clear-dirty, reset-sgr-pen, bell-pending, screen-consume-bell, miscellaneous slots, copy-mode extra slots.

(in-suite resize)

;;; ── screen-clear-dirty ───────────────────────────────────────────────────────

(test screen-clear-dirty-resets-and-is-idempotent
  "screen-clear-dirty sets dirty-p to NIL; calling it twice leaves the flag NIL."
  (with-screen (s 10 5)
    (is-true (cl-tmux/terminal/types:screen-dirty-p s) "new screen is dirty")
    (screen-clear-dirty s)
    (is-false (cl-tmux/terminal/types:screen-dirty-p s)
              "dirty flag must be NIL after screen-clear-dirty")
    (screen-clear-dirty s)
    (is-false (cl-tmux/terminal/types:screen-dirty-p s)
              "dirty flag must remain NIL after second screen-clear-dirty")))

;;; ── reset-sgr-pen ────────────────────────────────────────────────────────────

(def-suite reset-sgr-pen-suite
  :description "reset-sgr-pen: direct unit tests for all five SGR pen slots"
  :in terminal-suite)
(in-suite reset-sgr-pen-suite)

(test reset-sgr-pen-restores-all-five-slots
  :description "reset-sgr-pen sets all five SGR pen fields to VT100 defaults."
  (with-screen (s 10 5)
    ;; Dirty all five pen slots.
    (setf (cl-tmux/terminal/types:screen-cur-fg       s) 3
          (cl-tmux/terminal/types:screen-cur-bg       s) 4
          (cl-tmux/terminal/types:screen-cur-attrs    s) #b11111111
          (cl-tmux/terminal/types:screen-cur-attrs2   s) #b00000011
          (cl-tmux/terminal/types:screen-cur-ul-color s) 200)
    (cl-tmux/terminal/types:reset-sgr-pen s)
    (is (= cl-tmux/terminal/types:+default-color+ (cl-tmux/terminal/types:screen-cur-fg       s)) "fg must reset to the default sentinel")
    (is (= cl-tmux/terminal/types:+default-color+ (cl-tmux/terminal/types:screen-cur-bg       s)) "bg must reset to the default sentinel")
    (is (= 0 (cl-tmux/terminal/types:screen-cur-attrs    s)) "attrs must reset to 0")
    (is (= 0 (cl-tmux/terminal/types:screen-cur-attrs2   s)) "attrs2 must reset to 0")
    (is (= 0 (cl-tmux/terminal/types:screen-cur-ul-color s)) "ul-color must reset to 0")))

(test reset-sgr-pen-idempotent
  :description "Calling reset-sgr-pen twice leaves pen in the default state."
  (with-screen (s 10 5)
    (cl-tmux/terminal/types:reset-sgr-pen s)
    (cl-tmux/terminal/types:reset-sgr-pen s)
    (is (= cl-tmux/terminal/types:+default-color+ (cl-tmux/terminal/types:screen-cur-fg s)) "double-reset fg must be the default sentinel")
    (is (= cl-tmux/terminal/types:+default-color+ (cl-tmux/terminal/types:screen-cur-bg s)) "double-reset bg must be the default sentinel")))

;;; ── bell-pending slot ────────────────────────────────────────────────────────

(def-suite bell-pending-suite
  :description "screen-bell-pending slot: default value, set/clear"
  :in terminal-suite)
(in-suite bell-pending-suite)

(test bell-pending-default-and-toggle
  "bell-pending defaults to NIL and can be toggled via setf."
  (with-screen (s 10 5)
    (is-false (cl-tmux/terminal/types:screen-bell-pending s)
              "bell-pending must be NIL on a fresh screen")
    (setf (cl-tmux/terminal/types:screen-bell-pending s) t)
    (is (cl-tmux/terminal/types:screen-bell-pending s)
        "bell-pending must be T after setf t")
    (setf (cl-tmux/terminal/types:screen-bell-pending s) nil)
    (is-false (cl-tmux/terminal/types:screen-bell-pending s)
              "bell-pending must be NIL after setf nil")))

(test bel-byte-sets-bell-pending
  :description "Feeding a BEL byte (0x07) via screen-process-bytes sets bell-pending."
  (with-screen (s 10 5)
    (screen-clear-dirty s)
    ;; Feed BEL (7) directly
    (screen-process-bytes s (make-array 1 :element-type '(unsigned-byte 8)
                                          :initial-contents '(7)))
    (is-true (cl-tmux/terminal/types:screen-bell-pending s)
             "bell-pending must be T after feeding BEL byte")))

;;; ── screen-consume-bell ──────────────────────────────────────────────────────

(def-suite screen-consume-bell-suite
  :description "screen-consume-bell: consume and clear bell-pending atomically"
  :in terminal-suite)
(in-suite screen-consume-bell-suite)

(test screen-consume-bell-returns-nil-when-no-bell-pending
  :description "screen-consume-bell returns NIL and has no side effect when bell is not pending."
  (with-screen (s 10 5)
    ;; Fresh screen has no bell pending.
    (is-false (cl-tmux/terminal/types:screen-consume-bell s)
              "consume-bell must return NIL when bell is not pending")
    ;; Flag must still be NIL.
    (is-false (cl-tmux/terminal/types:screen-bell-pending s)
              "bell-pending must remain NIL after consume on no-bell screen")))

(test screen-consume-bell-returns-true-clears-then-nil
  "screen-consume-bell returns T when pending and clears the flag; second call returns NIL."
  (with-screen (s 10 5)
    (setf (cl-tmux/terminal/types:screen-bell-pending s) t)
    (is-true (cl-tmux/terminal/types:screen-consume-bell s)
             "first consume-bell must return T")
    (is-false (cl-tmux/terminal/types:screen-bell-pending s)
              "bell-pending must be NIL after consume-bell")
    (is-false (cl-tmux/terminal/types:screen-consume-bell s)
              "second consume-bell must return NIL")))

(test screen-consume-bell-after-bel-byte
  :description "screen-consume-bell clears the flag set by a real BEL byte."
  (with-screen (s 10 5)
    (screen-process-bytes s (make-array 1 :element-type '(unsigned-byte 8)
                                          :initial-contents '(7)))
    ;; Bell should be pending now.
    (is-true (cl-tmux/terminal/types:screen-bell-pending s)
             "pre-condition: bell-pending must be T after BEL byte")
    ;; Consume it.
    (is-true (cl-tmux/terminal/types:screen-consume-bell s)
             "consume-bell must return T")
    (is-false (cl-tmux/terminal/types:screen-bell-pending s)
              "bell-pending must be NIL after consume")))

;;; ── SUITE: miscellaneous screen slots ────────────────────────────────────────

(def-suite screen-slots
  :description "Miscellaneous screen slot defaults and setf contracts"
  :in terminal-suite)
(in-suite screen-slots)

(test screen-last-char-nil-then-updated
  "screen-last-char is NIL on a fresh screen and updates to the last character written."
  (with-screen (s 10 5)
    (is (null (cl-tmux/terminal/types:screen-last-char s))
        "last-char must be NIL on a fresh screen")
    (feed s "Z")
    (is (char= #\Z (cl-tmux/terminal/types:screen-last-char s))
        "last-char must be Z after feeding 'Z'")))

(test screen-slot-defaults-table
  "Fresh screens have expected default values: charset :ascii, autowrap T, cursor-shape 1, etc."
  (dolist (row (list (list #'cl-tmux/terminal/types:screen-charset        :ascii "charset defaults to :ascii")
                     (list #'cl-tmux/terminal/types:screen-autowrap        t      "autowrap defaults to T")
                     (list #'cl-tmux/terminal/types:screen-cursor-shape    1      "cursor-shape defaults to 1")
                     (list #'cl-tmux/terminal/types:screen-bracketed-paste nil    "bracketed-paste defaults to NIL")
                     (list #'cl-tmux/terminal/types:screen-app-cursor-keys nil    "app-cursor-keys defaults to NIL")
                     (list #'screen-title                                   ""     "title defaults to empty string")
                     (list #'screen-mouse-mode                              0      "mouse-mode defaults to 0")
                     (list #'screen-copy-mark                               nil    "copy-mark defaults to NIL")))
    (destructuring-bind (accessor expected desc) row
      (with-screen (s 10 5)
        (is (equal expected (funcall accessor s)) "~A" desc)))))

;;; ── SUITE: copy-mode extra slots ─────────────────────────────────────────────
