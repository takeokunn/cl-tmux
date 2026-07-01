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

(test screen-cur-fg-and-cur-bg-default-to-sentinel
  :description "A fresh screen's cur-fg and cur-bg SGR pen slots start at the default-colour sentinel."
  (with-screen (s 10 5)
    (is (= cl-tmux/terminal/types:+default-color+ (cl-tmux/terminal/types:screen-cur-fg s))
        "fresh screen cur-fg must be the default sentinel")
    (is (= cl-tmux/terminal/types:+default-color+ (cl-tmux/terminal/types:screen-cur-bg s))
        "fresh screen cur-bg must be the default sentinel")))

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

(def-suite copy-mode-extra-slots
  :description "copy-mode-entered-by-mouse-p and copy-exit-on-bottom slots"
  :in terminal-suite)
(in-suite copy-mode-extra-slots)

(test copy-mode-entered-by-mouse-defaults-nil
  "copy-mode-entered-by-mouse-p defaults to NIL on a fresh screen."
  (with-screen (s 10 5)
    (is-false (cl-tmux/terminal/types:screen-copy-mode-entered-by-mouse-p s)
              "copy-mode-entered-by-mouse-p must be NIL initially")))

(test copy-mode-entered-by-mouse-can-be-set
  "copy-mode-entered-by-mouse-p can be set and cleared via setf."
  (with-screen (s 10 5)
    (setf (cl-tmux/terminal/types:screen-copy-mode-entered-by-mouse-p s) t)
    (is-true (cl-tmux/terminal/types:screen-copy-mode-entered-by-mouse-p s)
             "must be T after setf t")
    (setf (cl-tmux/terminal/types:screen-copy-mode-entered-by-mouse-p s) nil)
    (is-false (cl-tmux/terminal/types:screen-copy-mode-entered-by-mouse-p s)
              "must be NIL after setf nil")))

(test copy-exit-on-bottom-defaults-nil
  "copy-exit-on-bottom defaults to NIL on a fresh screen."
  (with-screen (s 10 5)
    (is-false (cl-tmux/terminal/types:screen-copy-exit-on-bottom s)
              "copy-exit-on-bottom must be NIL initially")))

(test copy-exit-on-bottom-can-be-set
  "copy-exit-on-bottom can be set and cleared via setf."
  (with-screen (s 10 5)
    (setf (cl-tmux/terminal/types:screen-copy-exit-on-bottom s) t)
    (is-true (cl-tmux/terminal/types:screen-copy-exit-on-bottom s)
             "must be T after setf t")
    (setf (cl-tmux/terminal/types:screen-copy-exit-on-bottom s) nil)
    (is-false (cl-tmux/terminal/types:screen-copy-exit-on-bottom s)
              "must be NIL after setf nil")))

;;; ── SUITE: OSC 10/11 default colour slots ─────────────────────────────────

(def-suite osc-default-color-slots
  :description "screen-osc-default-fg and screen-osc-default-bg slot contracts"
  :in terminal-suite)
(in-suite osc-default-color-slots)

(test osc-default-fg-initial-value-matches-constant
  "screen-osc-default-fg on a fresh screen equals +osc-default-fg+ (white)."
  (with-screen (s 10 5)
    (is (= cl-tmux/terminal/types:+osc-default-fg+
           (cl-tmux/terminal/types:screen-osc-default-fg s))
        "osc-default-fg must equal +osc-default-fg+ (#xFFFFFF)")))

(test osc-default-bg-initial-value-matches-constant
  "screen-osc-default-bg on a fresh screen equals +osc-default-bg+ (black)."
  (with-screen (s 10 5)
    (is (= cl-tmux/terminal/types:+osc-default-bg+
           (cl-tmux/terminal/types:screen-osc-default-bg s))
        "osc-default-bg must equal +osc-default-bg+ (#x000000)")))

(test osc-default-fg-can-be-set-and-reset-via-sequence
  "OSC 10 ; #RRGGBB changes osc-default-fg; OSC 110 resets it to white."
  (with-screen (s 10 5)
    ;; Set foreground to a custom colour via OSC 10 ; #112233 ST
    (feed s (format nil "~C]10;#112233~C\\" #\Escape #\Escape))
    (is (= #x112233 (cl-tmux/terminal/types:screen-osc-default-fg s))
        "OSC 10 must update osc-default-fg to #x112233")
    ;; Reset via OSC 110 ST
    (feed s (format nil "~C]110~C\\" #\Escape #\Escape))
    (is (= cl-tmux/terminal/types:+osc-default-fg+
           (cl-tmux/terminal/types:screen-osc-default-fg s))
        "OSC 110 must reset osc-default-fg to +osc-default-fg+")))

(test osc-default-bg-can-be-set-and-reset-via-sequence
  "OSC 11 ; #RRGGBB changes osc-default-bg; OSC 111 resets it to black."
  (with-screen (s 10 5)
    ;; Set background to a custom colour via OSC 11 ; #AABBCC ST
    (feed s (format nil "~C]11;#aabbcc~C\\" #\Escape #\Escape))
    (is (= #xAABBCC (cl-tmux/terminal/types:screen-osc-default-bg s))
        "OSC 11 must update osc-default-bg to #xAABBCC")
    ;; Reset via OSC 111 ST
    (feed s (format nil "~C]111~C\\" #\Escape #\Escape))
    (is (= cl-tmux/terminal/types:+osc-default-bg+
           (cl-tmux/terminal/types:screen-osc-default-bg s))
        "OSC 111 must reset osc-default-bg to +osc-default-bg+")))

;;; ── SUITE: OSC 8 current-hyperlink slot ──────────────────────────────────

(def-suite current-hyperlink-slots
  :description "screen-current-hyperlink slot: nil default, set via OSC 8, clear via OSC 8 empty"
  :in terminal-suite)
(in-suite current-hyperlink-slots)

(test screen-current-hyperlink-defaults-nil
  "screen-current-hyperlink is NIL on a fresh screen."
  (with-screen (s 10 5)
    (is (null (cl-tmux/terminal/types:screen-current-hyperlink s))
        "current-hyperlink must be NIL on a fresh screen")))

(test screen-current-hyperlink-set-by-osc8
  "OSC 8 ; ; URI sets screen-current-hyperlink to the URI string."
  (with-screen (s 10 5)
    (feed s (format nil "~C]8;;https://example.com~C\\" #\Escape #\Escape))
    (is (string= "https://example.com"
                 (cl-tmux/terminal/types:screen-current-hyperlink s))
        "current-hyperlink must be the URI after OSC 8 ; ; URI")))

(test screen-current-hyperlink-cleared-by-osc8-empty
  "OSC 8 ; ; (empty URI) clears screen-current-hyperlink back to NIL."
  (with-screen (s 10 5)
    (feed s (format nil "~C]8;;https://example.com~C\\" #\Escape #\Escape))
    (is (cl-tmux/terminal/types:screen-current-hyperlink s)
        "pre-condition: hyperlink must be set")
    ;; Clear with empty URI
    (feed s (format nil "~C]8;;~C\\" #\Escape #\Escape))
    (is (null (cl-tmux/terminal/types:screen-current-hyperlink s))
        "current-hyperlink must be NIL after OSC 8 with empty URI")))

(test screen-current-hyperlink-stamped-onto-written-cells
  "Characters written while a hyperlink is active carry the URI in their cell hyperlink slot."
  (with-screen (s 10 5)
    (feed s (format nil "~C]8;;https://cl.org~C\\" #\Escape #\Escape))
    (feed s "AB")
    ;; The hyperlink must be stamped on both cells
    (is (string= "https://cl.org"
                 (cl-tmux/terminal/types:cell-hyperlink (cell-at s 0 0)))
        "cell (0,0) must carry the hyperlink URI")
    (is (string= "https://cl.org"
                 (cl-tmux/terminal/types:cell-hyperlink (cell-at s 1 0)))
        "cell (1,0) must carry the hyperlink URI")
    ;; After clearing the hyperlink, new cells must have NIL
    (feed s (format nil "~C]8;;~C\\" #\Escape #\Escape))
    (feed s "C")
    (is (null (cl-tmux/terminal/types:cell-hyperlink (cell-at s 2 0)))
        "cell (2,0) after clearing hyperlink must have NIL")))

;;; ── SUITE: %clear-line-wrapped ────────────────────────────────────────────

(def-suite clear-line-wrapped-suite
  :description "%clear-line-wrapped: removes wrap flag, is a no-op when absent"
  :in terminal-suite)
(in-suite clear-line-wrapped-suite)

(test clear-line-wrapped-removes-set-flag
  "%clear-line-wrapped removes a wrap flag that was previously set."
  (with-screen (s 10 5)
    (cl-tmux/terminal/types:%mark-line-wrapped s 2)
    (is-true (cl-tmux/terminal/types:%line-wrapped-p s 2)
             "pre-condition: row 2 must be marked wrapped")
    (cl-tmux/terminal/types:%clear-line-wrapped s 2)
    (is-false (cl-tmux/terminal/types:%line-wrapped-p s 2)
              "row 2 must not be wrapped after %clear-line-wrapped")))

(test clear-line-wrapped-is-noop-on-unmarked-row
  "%clear-line-wrapped on a row that was never marked is a no-op (no error)."
  (with-screen (s 10 5)
    ;; No hash-table exists yet; calling clear on an unmarked row must not signal.
    (finishes (cl-tmux/terminal/types:%clear-line-wrapped s 3))
    (is-false (cl-tmux/terminal/types:%line-wrapped-p s 3)
              "unmarked row must still be unwrapped after clear")))

(test clear-line-wrapped-does-not-disturb-other-rows
  "%clear-line-wrapped removes only the specified row's flag."
  (with-screen (s 10 5)
    (cl-tmux/terminal/types:%mark-line-wrapped s 0)
    (cl-tmux/terminal/types:%mark-line-wrapped s 1)
    (cl-tmux/terminal/types:%mark-line-wrapped s 2)
    (cl-tmux/terminal/types:%clear-line-wrapped s 1)
    (is-true  (cl-tmux/terminal/types:%line-wrapped-p s 0) "row 0 must remain wrapped")
    (is-false (cl-tmux/terminal/types:%line-wrapped-p s 1) "row 1 must be cleared")
    (is-true  (cl-tmux/terminal/types:%line-wrapped-p s 2) "row 2 must remain wrapped")))

(test clear-line-wrapped-is-noop-when-no-hash-table
  "%clear-line-wrapped on a screen with no wrapped-rows hash-table is silent."
  (with-screen (s 10 5)
    ;; Fresh screen has nil wrapped-rows — ensure there is no error.
    (is (null (cl-tmux/terminal/types:screen-wrapped-rows s))
        "pre-condition: fresh screen has no wrap table")
    (finishes (cl-tmux/terminal/types:%clear-line-wrapped s 0))
    (is-false (cl-tmux/terminal/types:%line-wrapped-p s 0)
              "row 0 must be unwrapped on a nil-table screen")))
