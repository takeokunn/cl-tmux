(in-package #:cl-tmux/test)

;;;; modes tests — part B: set-cursor-shape, bell-pending, designate-charset/title,
;;;; reset-terminal-modes, DECNKM, DECOM, origin-mode, screen-display-cell
;;;; continuation, DECSTBM, mouse/focus-reporting edge cases.

;;; ── SUITE: set-cursor-shape ──────────────────────────────────────────────────
;;;
;;; set-cursor-shape wraps DECSCUSR: clamps the shape value to [0,6] and stores
;;; it in screen-cursor-shape.

(describe "terminal-suite/set-cursor-shape-suite"

  ;; set-cursor-shape stores values in [0,6] unchanged.
  (it "set-cursor-shape-stores-valid-values"
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
          (declare (ignore desc))
          (with-screen (s 10 5)
            (cl-tmux/terminal/actions:set-cursor-shape s input)
            (expect (= expected (cl-tmux/terminal/types:screen-cursor-shape s))))))))

  ;; set-cursor-shape clamps values > 6 to 6.
  (it "set-cursor-shape-clamps-above-six-to-six"
    (with-screen (s 10 5)
      (cl-tmux/terminal/actions:set-cursor-shape s 99)
      (expect (= 6 (cl-tmux/terminal/types:screen-cursor-shape s)))))

  ;; set-cursor-shape clamps negative values to 0.
  (it "set-cursor-shape-clamps-below-zero-to-zero"
    (with-screen (s 10 5)
      (cl-tmux/terminal/actions:set-cursor-shape s -1)
      (expect (= 0 (cl-tmux/terminal/types:screen-cursor-shape s))))))

;;; ── SUITE: set-bell-pending and screen-consume-bell ─────────────────────────
;;;
;;; set-bell-pending sets screen-bell-pending to T.
;;; screen-consume-bell returns T and clears the flag; returns NIL when not set.

(describe "terminal-suite/bell-pending-suite"

  ;; set-bell-pending sets screen-bell-pending to T.
  (it "set-bell-pending-sets-flag"
    (with-screen (s 10 5)
      (expect (cl-tmux/terminal/types:screen-bell-pending s) :to-be-falsy)
      (cl-tmux/terminal/actions:set-bell-pending s)
      (expect (cl-tmux/terminal/types:screen-bell-pending s))))

  ;; screen-consume-bell returns T and clears the bell-pending flag.
  (it "screen-consume-bell-returns-true-and-clears-flag"
    (with-screen (s 10 5)
      (cl-tmux/terminal/actions:set-bell-pending s)
      (let ((result (cl-tmux/terminal/types:screen-consume-bell s)))
        (expect result :to-be-truthy)
        (expect (cl-tmux/terminal/types:screen-bell-pending s) :to-be-falsy))))

  ;; screen-consume-bell returns NIL without side effects when bell is not pending.
  (it "screen-consume-bell-returns-nil-when-not-pending"
    (with-screen (s 10 5)
      (expect (cl-tmux/terminal/types:screen-bell-pending s) :to-be-falsy)
      (expect (cl-tmux/terminal/types:screen-consume-bell s) :to-be-falsy)))

  ;; A BEL byte (0x07) fed to the emulator sets screen-bell-pending.
  (it "bell-byte-sets-pending-via-emulator"
    (with-screen (s 10 5)
      (screen-process-bytes s (vector 7))  ; BEL = 0x07
      (expect (cl-tmux/terminal/types:screen-bell-pending s)))))

;;; ── SUITE: designate-charset and set-screen-title ─────────────────────────────
;;;
;;; designate-charset (G0, the default active slot) stores the character set
;;; keyword into screen-charset.
;;; set-screen-title stores the OSC window title string.

(describe "terminal-suite/set-charset-set-title-suite"

  ;; designate-charset :g0 :ascii sets screen-charset to :ascii.
  (it "set-charset-stores-ascii-keyword"
    (with-screen (s 10 5)
      ;; Start with dec-graphics, then reset to ascii
      (setf (cl-tmux/terminal/types:screen-charset s) :dec-graphics)
      (cl-tmux/terminal/actions:designate-charset s :g0 :ascii)
      (expect (eq :ascii (cl-tmux/terminal/types:screen-charset s)))))

  ;; designate-charset :g0 :dec-graphics sets screen-charset to :dec-graphics.
  (it "set-charset-stores-dec-graphics-keyword"
    (with-screen (s 10 5)
      (cl-tmux/terminal/actions:designate-charset s :g0 :dec-graphics)
      (expect (eq :dec-graphics (cl-tmux/terminal/types:screen-charset s)))))

  ;; set-screen-title stores the given title in screen-title.
  (it "set-screen-title-stores-string"
    (with-screen (s 10 5)
      (cl-tmux/terminal/actions:set-screen-title s "my-window")
      (expect (string= "my-window" (cl-tmux/terminal/types:screen-title s)))))

  ;; set-screen-title accepts an empty string.
  (it "set-screen-title-stores-empty-string"
    (with-screen (s 10 5)
      (cl-tmux/terminal/actions:set-screen-title s "first")
      (cl-tmux/terminal/actions:set-screen-title s "")
      (expect (string= "" (cl-tmux/terminal/types:screen-title s)))))

  ;; OSC 0 ; title ST sets screen-title via the emulator.
  (it "set-screen-title-via-osc-sequence"
    (with-screen (s 20 5)
      ;; OSC 0 ; hello BEL — OSC title sequence
      (feed s (format nil "~C]0;hello~C" #\Escape (code-char 7)))
      (expect (string= "hello" (cl-tmux/terminal/types:screen-title s))))))

;;; ── SUITE: reset-terminal-modes ──────────────────────────────────────────────
;;;
;;; reset-terminal-modes resets cursor visibility, autowrap, charset, and scroll
;;; region to VT100 defaults without touching the cell grid.

(describe "terminal-suite/reset-terminal-modes-suite"

  ;; reset-terminal-modes restores cursor-visible and autowrap to T, and charset to :ascii.
  (it "reset-terminal-modes-restores-flags"
    (with-screen (s 10 5)
      (setf (cl-tmux/terminal/types:screen-cursor-visible s) nil
            (cl-tmux/terminal/types:screen-autowrap s)        nil
            (cl-tmux/terminal/types:screen-charset s)         :dec-graphics)
      (cl-tmux/terminal/actions:reset-terminal-modes s)
      (expect (cl-tmux/terminal/types:screen-cursor-visible s))
      (expect (cl-tmux/terminal/types:screen-autowrap s))
      (expect (eq :ascii (cl-tmux/terminal/types:screen-charset s)))))

  ;; reset-terminal-modes resets scroll-top to 0 and scroll-bottom to height-1.
  (it "reset-terminal-modes-restores-scroll-region-to-full-screen"
    (with-screen (s 10 8)
      ;; Restrict the scroll region
      (setf (cl-tmux/terminal/types:screen-scroll-top    s) 2
            (cl-tmux/terminal/types:screen-scroll-bottom s) 5)
      (cl-tmux/terminal/actions:reset-terminal-modes s)
      (expect (= 0 (cl-tmux/terminal/types:screen-scroll-top s)))
      (expect (= 7 (cl-tmux/terminal/types:screen-scroll-bottom s)))))

  ;; reset-terminal-modes does not erase the cell grid.
  (it "reset-terminal-modes-does-not-clear-cells"
    (with-screen (s 10 5)
      (feed s "hello")
      (cl-tmux/terminal/actions:reset-terminal-modes s)
      ;; The text written before reset must survive
      (expect (char= #\h (char-at s 0 0))))))

;;; ── SUITE: enter/exit alt-screen direct actions ──────────────────────────────
;;;
;;; enter-alt-screen and exit-alt-screen are called directly to verify the
;;; no-op guard (enter when already in alt, exit when not in alt).

(describe "terminal-suite/alt-screen-direct-suite"

  ;; enter-alt-screen is a no-op when called while the alt screen is already active.
  (it "enter-alt-screen-is-noop-when-already-active"
    (with-screen (s 10 5)
      (feed s "primary")
      (cl-tmux/terminal/actions:enter-alt-screen s)    ; first entry — saves grid
      ;; Capture the saved alt-cells reference before the second call
      (let ((saved-alt-cells (cl-tmux/terminal/types:screen-alt-cells s)))
        (cl-tmux/terminal/actions:enter-alt-screen s)  ; second call — no-op
        ;; alt-cells must still point to the same grid snapshot
        (expect (eq saved-alt-cells (cl-tmux/terminal/types:screen-alt-cells s))))))

  ;; exit-alt-screen with no prior save falls back to erase-display mode 2.
  (it "exit-alt-screen-clears-to-blank-when-no-saved-grid"
    (with-screen (s 10 5)
      (feed s "hello")
      ;; Call exit-alt-screen without ever entering: alt-cells is NIL
      (cl-tmux/terminal/actions:exit-alt-screen s)
      ;; The erase-display mode 2 fallback should have cleared all cells
      (dotimes (y 5)
        (expect (row-blank-p s y))))))

;;; ── SUITE: enter/exit alt-screen direct content verification ─────────────────

(describe "terminal-suite/alt-screen-content-suite"

  ;; enter-alt-screen replaces the live grid with a fresh blank grid.
  (it "enter-alt-screen-installs-blank-grid"
    (with-screen (s 10 5)
      (feed s "primary")
      (cl-tmux/terminal/actions:enter-alt-screen s)
      ;; All cells in the new (alt) grid must be blank
      (dotimes (y 5)
        (expect (row-blank-p s y)))))

  ;; exit-alt-screen restores the cursor position saved at enter time.
  (it "exit-alt-screen-restores-primary-cursor-position"
    (with-screen (s 20 10)
      ;; Position cursor at (7, 3), enter alt, move cursor, exit
      (cl-tmux/terminal/actions:set-cursor s 7 3)
      (cl-tmux/terminal/actions:enter-alt-screen s)
      (cl-tmux/terminal/actions:set-cursor s 0 0)
      (cl-tmux/terminal/actions:exit-alt-screen s)
      (check-cursor s 7 3))))
