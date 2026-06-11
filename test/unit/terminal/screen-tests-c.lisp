(in-package #:cl-tmux/test)

;;;; Screen tests — part III: screen-clear-dirty, reset-sgr-pen, bell-pending, screen-consume-bell, miscellaneous slots, copy-mode extra slots.

(in-suite resize)

;;; ── screen-clear-dirty ───────────────────────────────────────────────────────

(test screen-clear-dirty-resets-flag
  "screen-clear-dirty sets screen-dirty-p to NIL."
  (with-screen (s 10 5)
    ;; A freshly created screen starts dirty.
    (is-true (cl-tmux/terminal/types:screen-dirty-p s) "new screen is dirty")
    (screen-clear-dirty s)
    (is-false (cl-tmux/terminal/types:screen-dirty-p s)
              "dirty flag must be NIL after screen-clear-dirty")))

(test screen-clear-dirty-idempotent
  :description "Calling screen-clear-dirty twice leaves the flag NIL."
  (with-screen (s 10 5)
    (screen-clear-dirty s)
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
    (is (= 7 (cl-tmux/terminal/types:screen-cur-fg       s)) "fg must reset to 7")
    (is (= 0 (cl-tmux/terminal/types:screen-cur-bg       s)) "bg must reset to 0")
    (is (= 0 (cl-tmux/terminal/types:screen-cur-attrs    s)) "attrs must reset to 0")
    (is (= 0 (cl-tmux/terminal/types:screen-cur-attrs2   s)) "attrs2 must reset to 0")
    (is (= 0 (cl-tmux/terminal/types:screen-cur-ul-color s)) "ul-color must reset to 0")))

(test reset-sgr-pen-idempotent
  :description "Calling reset-sgr-pen twice leaves pen in the default state."
  (with-screen (s 10 5)
    (cl-tmux/terminal/types:reset-sgr-pen s)
    (cl-tmux/terminal/types:reset-sgr-pen s)
    (is (= 7 (cl-tmux/terminal/types:screen-cur-fg s)) "double-reset fg must be 7")
    (is (= 0 (cl-tmux/terminal/types:screen-cur-bg s)) "double-reset bg must be 0")))

;;; ── bell-pending slot ────────────────────────────────────────────────────────

(def-suite bell-pending-suite
  :description "screen-bell-pending slot: default value, set/clear"
  :in terminal-suite)
(in-suite bell-pending-suite)

(test bell-pending-default-is-nil
  "A fresh screen has bell-pending NIL."
  (with-screen (s 10 5)
    (is-false (cl-tmux/terminal/types:screen-bell-pending s)
              "bell-pending must be NIL on a fresh screen")))

(test bell-pending-can-be-set-and-cleared
  "screen-bell-pending can be toggled via setf."
  (with-screen (s 10 5)
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

(test screen-consume-bell-returns-true-and-clears-flag
  :description "screen-consume-bell returns T and clears bell-pending when a bell is pending."
  (with-screen (s 10 5)
    (setf (cl-tmux/terminal/types:screen-bell-pending s) t)
    (is-true (cl-tmux/terminal/types:screen-consume-bell s)
             "consume-bell must return T when bell is pending")
    ;; Flag must be cleared now.
    (is-false (cl-tmux/terminal/types:screen-bell-pending s)
              "bell-pending must be NIL after consume-bell clears it")))

(test screen-consume-bell-idempotent-after-clear
  :description "Calling screen-consume-bell twice returns T then NIL."
  (with-screen (s 10 5)
    (setf (cl-tmux/terminal/types:screen-bell-pending s) t)
    ;; First call: consumes the bell.
    (is-true (cl-tmux/terminal/types:screen-consume-bell s)
             "first consume-bell must return T")
    ;; Second call: no bell pending.
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

(test screen-last-char-starts-nil
  :description "screen-last-char is NIL until a character is written."
  (with-screen (s 10 5)
    (is (null (cl-tmux/terminal/types:screen-last-char s))
        "last-char must be NIL on a fresh screen")))

(test screen-last-char-updated-after-write
  :description "screen-last-char holds the most recently written character."
  (with-screen (s 10 5)
    (feed s "Z")
    (is (char= #\Z (cl-tmux/terminal/types:screen-last-char s))
        "last-char must be Z after feeding 'Z'")))

(test screen-charset-defaults-to-ascii
  :description "A fresh screen uses the :ascii character set."
  (with-screen (s 10 5)
    (is (eq :ascii (cl-tmux/terminal/types:screen-charset s))
        "charset must default to :ascii")))

(test screen-autowrap-defaults-true
  :description "Auto-wrap mode is enabled on a fresh screen."
  (with-screen (s 10 5)
    (is-true (cl-tmux/terminal/types:screen-autowrap s)
             "autowrap must default to T")))

(test screen-cursor-shape-defaults-to-1
  :description "The cursor shape starts at 1 (block blink)."
  (with-screen (s 10 5)
    (is (= 1 (cl-tmux/terminal/types:screen-cursor-shape s))
        "cursor-shape must default to 1")))

(test screen-bracketed-paste-defaults-false
  :description "Bracketed paste mode is off by default."
  (with-screen (s 10 5)
    (is-false (cl-tmux/terminal/types:screen-bracketed-paste s)
              "bracketed-paste must default to NIL")))

(test screen-app-cursor-keys-defaults-false
  :description "Application cursor keys mode is off by default."
  (with-screen (s 10 5)
    (is-false (cl-tmux/terminal/types:screen-app-cursor-keys s)
              "app-cursor-keys must default to NIL")))

(test screen-title-defaults-empty-string
  :description "The window title slot starts as an empty string."
  (with-screen (s 10 5)
    (is (string= "" (screen-title s))
        "title must default to empty string")))

(test screen-mouse-mode-defaults-zero
  :description "Mouse reporting mode starts at 0 (off)."
  (with-screen (s 10 5)
    (is (= 0 (screen-mouse-mode s))
        "mouse-mode must default to 0")))

(test screen-copy-mark-defaults-nil
  :description "Copy-mode mark starts as NIL (no active selection)."
  (with-screen (s 10 5)
    (is (null (screen-copy-mark s))
        "copy-mark must start as NIL")))

;;; ── SUITE: copy-mode extra slots ─────────────────────────────────────────────
