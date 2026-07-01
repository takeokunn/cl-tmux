(in-package #:cl-tmux/test)

;;;; modes tests — part B: set-cursor-shape, bell-pending, designate-charset/title,
;;;; reset-terminal-modes, DECNKM, DECOM, origin-mode, screen-display-cell
;;;; continuation, DECSTBM, mouse/focus-reporting edge cases.

;;; ── SUITE: set-cursor-shape ──────────────────────────────────────────────────
;;;
;;; set-cursor-shape wraps DECSCUSR: clamps the shape value to [0,6] and stores
;;; it in screen-cursor-shape.

(def-suite set-cursor-shape-suite
  :description "set-cursor-shape: DECSCUSR clamping and storage"
  :in terminal-suite)
(in-suite set-cursor-shape-suite)

(test set-cursor-shape-stores-valid-values
  :description "set-cursor-shape stores values in [0,6] unchanged."
  ;; Table: (input expected-shape description)
  (let ((cases '((0 0 "default blinking block")
                 (1 1 "blinking block")
                 (2 2 "steady block")
                 (3 3 "blinking underline")
                 (4 4 "steady underline")
                 (5 5 "blinking bar")
                 (6 6 "steady bar"))))
    (dolist (c cases)
      (destructuring-bind (input expected desc) c
        (with-screen (s 10 5)
          (cl-tmux/terminal/actions:set-cursor-shape s input)
          (is (= expected (cl-tmux/terminal/types:screen-cursor-shape s))
              "set-cursor-shape ~D: expected ~D got ~D (~A)"
              input expected
              (cl-tmux/terminal/types:screen-cursor-shape s)
              desc))))))

(test set-cursor-shape-clamps-above-six-to-six
  :description "set-cursor-shape clamps values > 6 to 6."
  (with-screen (s 10 5)
    (cl-tmux/terminal/actions:set-cursor-shape s 99)
    (is (= 6 (cl-tmux/terminal/types:screen-cursor-shape s))
        "set-cursor-shape 99 must clamp to 6, got ~D"
        (cl-tmux/terminal/types:screen-cursor-shape s))))

(test set-cursor-shape-clamps-below-zero-to-zero
  :description "set-cursor-shape clamps negative values to 0."
  (with-screen (s 10 5)
    (cl-tmux/terminal/actions:set-cursor-shape s -1)
    (is (= 0 (cl-tmux/terminal/types:screen-cursor-shape s))
        "set-cursor-shape -1 must clamp to 0, got ~D"
        (cl-tmux/terminal/types:screen-cursor-shape s))))

;;; ── SUITE: set-bell-pending and screen-consume-bell ─────────────────────────
;;;
;;; set-bell-pending sets screen-bell-pending to T.
;;; screen-consume-bell returns T and clears the flag; returns NIL when not set.

(def-suite bell-pending-suite
  :description "set-bell-pending and screen-consume-bell"
  :in terminal-suite)
(in-suite bell-pending-suite)

(test set-bell-pending-sets-flag
  :description "set-bell-pending sets screen-bell-pending to T."
  (with-screen (s 10 5)
    (is-false (cl-tmux/terminal/types:screen-bell-pending s)
              "bell-pending must be NIL by default")
    (cl-tmux/terminal/actions:set-bell-pending s)
    (is (cl-tmux/terminal/types:screen-bell-pending s)
        "bell-pending must be T after set-bell-pending")))

(test screen-consume-bell-returns-true-and-clears-flag
  :description "screen-consume-bell returns T and clears the bell-pending flag."
  (with-screen (s 10 5)
    (cl-tmux/terminal/actions:set-bell-pending s)
    (let ((result (cl-tmux/terminal/types:screen-consume-bell s)))
      (is-true result "screen-consume-bell must return T when bell is pending")
      (is-false (cl-tmux/terminal/types:screen-bell-pending s)
                "bell-pending must be NIL after screen-consume-bell"))))

(test screen-consume-bell-returns-nil-when-not-pending
  :description "screen-consume-bell returns NIL without side effects when bell is not pending."
  (with-screen (s 10 5)
    (is-false (cl-tmux/terminal/types:screen-bell-pending s)
              "bell-pending must be NIL on a fresh screen")
    (is-false (cl-tmux/terminal/types:screen-consume-bell s)
              "screen-consume-bell must return NIL when bell is not pending")))

(test bell-byte-sets-pending-via-emulator
  :description "A BEL byte (0x07) fed to the emulator sets screen-bell-pending."
  (with-screen (s 10 5)
    (screen-process-bytes s (vector 7))  ; BEL = 0x07
    (is (cl-tmux/terminal/types:screen-bell-pending s)
        "BEL byte must set screen-bell-pending to T")))

;;; ── SUITE: designate-charset and set-screen-title ─────────────────────────────
;;;
;;; designate-charset (G0, the default active slot) stores the character set
;;; keyword into screen-charset.
;;; set-screen-title stores the OSC window title string.

(def-suite set-charset-set-title-suite
  :description "designate-charset and set-screen-title direct action tests"
  :in terminal-suite)
(in-suite set-charset-set-title-suite)

(test set-charset-stores-ascii-keyword
  :description "designate-charset :g0 :ascii sets screen-charset to :ascii."
  (with-screen (s 10 5)
    ;; Start with dec-graphics, then reset to ascii
    (setf (cl-tmux/terminal/types:screen-charset s) :dec-graphics)
    (cl-tmux/terminal/actions:designate-charset s :g0 :ascii)
    (is (eq :ascii (cl-tmux/terminal/types:screen-charset s))
        "screen-charset must be :ascii after designate-charset :g0 :ascii")))

(test set-charset-stores-dec-graphics-keyword
  :description "designate-charset :g0 :dec-graphics sets screen-charset to :dec-graphics."
  (with-screen (s 10 5)
    (cl-tmux/terminal/actions:designate-charset s :g0 :dec-graphics)
    (is (eq :dec-graphics (cl-tmux/terminal/types:screen-charset s))
        "screen-charset must be :dec-graphics after designate-charset :g0 :dec-graphics")))

(test set-screen-title-stores-string
  :description "set-screen-title stores the given title in screen-title."
  (with-screen (s 10 5)
    (cl-tmux/terminal/actions:set-screen-title s "my-window")
    (is (string= "my-window" (cl-tmux/terminal/types:screen-title s))
        "screen-title must be \"my-window\" after set-screen-title")))

(test set-screen-title-stores-empty-string
  :description "set-screen-title accepts an empty string."
  (with-screen (s 10 5)
    (cl-tmux/terminal/actions:set-screen-title s "first")
    (cl-tmux/terminal/actions:set-screen-title s "")
    (is (string= "" (cl-tmux/terminal/types:screen-title s))
        "screen-title must be empty string after set-screen-title \"\"")))

(test set-screen-title-via-osc-sequence
  :description "OSC 0 ; title ST sets screen-title via the emulator."
  (with-screen (s 20 5)
    ;; OSC 0 ; hello BEL — OSC title sequence
    (feed s (format nil "~C]0;hello~C" #\Escape (code-char 7)))
    (is (string= "hello" (cl-tmux/terminal/types:screen-title s))
        "screen-title must be \"hello\" after OSC 0;hello BEL sequence")))

;;; ── SUITE: reset-terminal-modes ──────────────────────────────────────────────
;;;
;;; reset-terminal-modes resets cursor visibility, autowrap, charset, and scroll
;;; region to VT100 defaults without touching the cell grid.

(def-suite reset-terminal-modes-suite
  :description "reset-terminal-modes: VT100 default mode restoration"
  :in terminal-suite)
(in-suite reset-terminal-modes-suite)

(test reset-terminal-modes-restores-flags
  "reset-terminal-modes restores cursor-visible and autowrap to T, and charset to :ascii."
  (with-screen (s 10 5)
    (setf (cl-tmux/terminal/types:screen-cursor-visible s) nil
          (cl-tmux/terminal/types:screen-autowrap s)        nil
          (cl-tmux/terminal/types:screen-charset s)         :dec-graphics)
    (cl-tmux/terminal/actions:reset-terminal-modes s)
    (is (cl-tmux/terminal/types:screen-cursor-visible s)
        "cursor-visible must be T after reset-terminal-modes")
    (is (cl-tmux/terminal/types:screen-autowrap s)
        "autowrap must be T after reset-terminal-modes")
    (is (eq :ascii (cl-tmux/terminal/types:screen-charset s))
        "charset must be :ascii after reset-terminal-modes")))

(test reset-terminal-modes-restores-scroll-region-to-full-screen
  :description "reset-terminal-modes resets scroll-top to 0 and scroll-bottom to height-1."
  (with-screen (s 10 8)
    ;; Restrict the scroll region
    (setf (cl-tmux/terminal/types:screen-scroll-top    s) 2
          (cl-tmux/terminal/types:screen-scroll-bottom s) 5)
    (cl-tmux/terminal/actions:reset-terminal-modes s)
    (is (= 0 (cl-tmux/terminal/types:screen-scroll-top s))
        "scroll-top must be 0 after reset-terminal-modes")
    (is (= 7 (cl-tmux/terminal/types:screen-scroll-bottom s))
        "scroll-bottom must be height-1 (7) after reset-terminal-modes")))

(test reset-terminal-modes-does-not-clear-cells
  :description "reset-terminal-modes does not erase the cell grid."
  (with-screen (s 10 5)
    (feed s "hello")
    (cl-tmux/terminal/actions:reset-terminal-modes s)
    ;; The text written before reset must survive
    (is (char= #\h (char-at s 0 0))
        "cell content must be preserved after reset-terminal-modes")))

;;; ── SUITE: enter/exit alt-screen direct actions ──────────────────────────────
;;;
;;; enter-alt-screen and exit-alt-screen are called directly to verify the
;;; no-op guard (enter when already in alt, exit when not in alt).

(def-suite alt-screen-direct-suite
  :description "enter-alt-screen / exit-alt-screen direct action tests"
  :in terminal-suite)
(in-suite alt-screen-direct-suite)

(test enter-alt-screen-is-noop-when-already-active
  :description "enter-alt-screen is a no-op when called while the alt screen is already active."
  (with-screen (s 10 5)
    (feed s "primary")
    (cl-tmux/terminal/actions:enter-alt-screen s)    ; first entry — saves grid
    ;; Capture the saved alt-cells reference before the second call
    (let ((saved-alt-cells (cl-tmux/terminal/types:screen-alt-cells s)))
      (cl-tmux/terminal/actions:enter-alt-screen s)  ; second call — no-op
      ;; alt-cells must still point to the same grid snapshot
      (is (eq saved-alt-cells (cl-tmux/terminal/types:screen-alt-cells s))
          "alt-cells reference must not change on second enter-alt-screen"))))

(test exit-alt-screen-clears-to-blank-when-no-saved-grid
  :description "exit-alt-screen with no prior save falls back to erase-display mode 2."
  (with-screen (s 10 5)
    (feed s "hello")
    ;; Call exit-alt-screen without ever entering: alt-cells is NIL
    (cl-tmux/terminal/actions:exit-alt-screen s)
    ;; The erase-display mode 2 fallback should have cleared all cells
    (dotimes (y 5)
      (is (row-blank-p s y)
          "row ~D must be blank after exit-alt-screen with no saved grid" y))))

;;; ── SUITE: enter/exit alt-screen direct content verification ─────────────────

(def-suite alt-screen-content-suite
  :description "enter/exit alt-screen verifies grid save and restore content"
  :in terminal-suite)
(in-suite alt-screen-content-suite)

(test enter-alt-screen-installs-blank-grid
  :description "enter-alt-screen replaces the live grid with a fresh blank grid."
  (with-screen (s 10 5)
    (feed s "primary")
    (cl-tmux/terminal/actions:enter-alt-screen s)
    ;; All cells in the new (alt) grid must be blank
    (dotimes (y 5)
      (is (row-blank-p s y)
          "alt screen row ~D must be blank after enter-alt-screen" y))))

(test exit-alt-screen-restores-primary-cursor-position
  :description "exit-alt-screen restores the cursor position saved at enter time."
  (with-screen (s 20 10)
    ;; Position cursor at (7, 3), enter alt, move cursor, exit
    (cl-tmux/terminal/actions:set-cursor s 7 3)
    (cl-tmux/terminal/actions:enter-alt-screen s)
    (cl-tmux/terminal/actions:set-cursor s 0 0)
    (cl-tmux/terminal/actions:exit-alt-screen s)
    (check-cursor s 7 3)))
