(in-package #:cl-tmux/test)

;;;; screen tests — part B: copy-mode slots, alt-screen cursor save,
;;;; mouse-sgr-mode, response-queue, origin-mode, tab-stops,
;;;; screen-lock, screen-cells/screen-parser accessors.

(describe "terminal-suite/copy-mode-slots"

  ;; All copy-mode slots default to NIL/false on a fresh screen.
  (it "screen-copy-slots-default-to-nil"
    (with-screen (s 10 5)
      (expect (null (screen-copy-cursor s)))
      (expect (screen-copy-selecting s) :to-be-falsy)
      (expect (null (cl-tmux/terminal/types:screen-copy-search-term s)))
      (expect (cl-tmux/terminal/types:screen-copy-line-selection-p s) :to-be-falsy)))

  ;; copy-cursor can be set to a (row . col) pair via setf.
  (it "screen-copy-cursor-can-be-set"
    (with-screen (s 10 5)
      (setf (screen-copy-cursor s) (list 2 3))
      (expect (equal '(2 3) (screen-copy-cursor s)))))

  ;; copy-selecting can be set and cleared via setf.
  (it "screen-copy-selecting-can-be-toggled"
    (with-screen (s 10 5)
      (setf (screen-copy-selecting s) t)
      (expect (screen-copy-selecting s) :to-be-truthy)
      (setf (screen-copy-selecting s) nil)
      (expect (screen-copy-selecting s) :to-be-falsy)))

  ;; copy-search-term can hold an arbitrary search string.
  (it "screen-copy-search-term-can-be-set"
    (with-screen (s 10 5)
      (setf (cl-tmux/terminal/types:screen-copy-search-term s) "hello")
      (expect (string= "hello" (cl-tmux/terminal/types:screen-copy-search-term s)))))

  ;; copy-mark-offset is 0 on a fresh screen.
  (it "screen-copy-mark-offset-defaults-zero"
    (with-screen (s 10 5)
      (expect (= 0 (cl-tmux/terminal/types:screen-copy-mark-offset s)))))

  ;; copy-mark-offset can be set to an arbitrary integer via setf.
  (it "screen-copy-mark-offset-can-be-set"
    (with-screen (s 10 5)
      (setf (cl-tmux/terminal/types:screen-copy-mark-offset s) 7)
      (expect (= 7 (cl-tmux/terminal/types:screen-copy-mark-offset s))))))

;;; ── SUITE: alt-screen cursor save slots ─────────────────────────────────────

(describe "terminal-suite/alt-screen-slots"

  ;; alt-cursor-x and alt-cursor-y both start at 0 on a fresh screen.
  (it "screen-alt-cursor-defaults-zero"
    (with-screen (s 10 5)
      (expect (= 0 (cl-tmux/terminal/types:screen-alt-cursor-x s)))
      (expect (= 0 (cl-tmux/terminal/types:screen-alt-cursor-y s)))))

  ;; alt-cells slot is NIL before entering alt-screen mode.
  (it "screen-alt-cells-defaults-nil"
    (with-screen (s 10 5)
      (expect (null (cl-tmux/terminal/types:screen-alt-cells s)))))

  ;; Entering alt-screen via ESC[?1049h saves cursor position into alt-cursor-x/y.
  (it "screen-alt-cursor-saved-on-alt-screen-entry"
    (with-screen (s 20 10)
      (feed s (esc "[5;10H"))   ; move cursor to (row=4, col=9)
      (feed s (esc "[?1049h"))  ; enter alt screen
      ;; After entering alt-screen, alt-cursor-x/y should hold the saved cursor.
      (expect (= 9 (cl-tmux/terminal/types:screen-alt-cursor-x s)))
      (expect (= 4 (cl-tmux/terminal/types:screen-alt-cursor-y s)))))

  ;; Mode ?1049h saves the full cursor state (SGR attrs) and ?1049l restores it.
  ;; Apps like neovim rely on this to restore their primary-screen rendering state.
  (it "alt-screen-1049-saves-and-restores-full-cursor-state"
    (with-screen (s 40 24)
      ;; Set SGR: bold + foreground red (colour 1)
      (feed s (esc "[1;31m"))
      (let ((saved-attrs (cl-tmux/terminal/types:screen-cur-attrs s))
            (saved-fg    (cl-tmux/terminal/types:screen-cur-fg    s)))
        ;; Enter alt screen (1049h should save the SGR state via DECSC)
        (feed s (esc "[?1049h"))
        ;; In the alt screen: reset SGR and move cursor
        (feed s (esc "[m"))          ; reset attributes
        (feed s (esc "[10;5H"))      ; move cursor
        ;; Exit alt screen (1049l should restore SGR via DECRC)
        (feed s (esc "[?1049l"))
        ;; Cursor position restored from alt-cursor-x/y
        (expect (= 0 (cl-tmux/terminal/types:screen-cursor-x s)))
        ;; SGR state restored: bold + red should be back
        (expect (= saved-attrs (cl-tmux/terminal/types:screen-cur-attrs s)))
        (expect (= saved-fg (cl-tmux/terminal/types:screen-cur-fg s)))))))

;;; ── SUITE: mouse-sgr-mode slot ───────────────────────────────────────────────

(describe "terminal-suite/mouse-sgr-mode-suite"

  ;; mouse-sgr-mode is NIL on a fresh screen.
  (it "screen-mouse-sgr-mode-defaults-false"
    (with-screen (s 10 5)
      (expect (screen-mouse-sgr-mode s) :to-be-falsy)))

  ;; ESC[?1006h enables SGR extended mouse encoding.
  (it "screen-mouse-sgr-mode-enabled-by-1006h"
    (with-screen (s 10 5)
      (feed s (esc "[?1006h"))
      (expect (screen-mouse-sgr-mode s) :to-be-truthy)))

  ;; ESC[?1006l disables SGR extended mouse encoding.
  (it "screen-mouse-sgr-mode-disabled-by-1006l"
    (with-screen (s 10 5)
      (feed s (esc "[?1006h"))
      (feed s (esc "[?1006l"))
      (expect (screen-mouse-sgr-mode s) :to-be-falsy))))

;;; ── SUITE: response-queue ────────────────────────────────────────────────────

(describe "terminal-suite/response-queue-suite"

  ;; Response queue is NIL on a fresh screen.
  (it "response-queue-starts-nil"
    (with-screen (s 10 5)
      (expect (null (cl-tmux/terminal/types:screen-response-queue s)))))

  ;; Items pushed onto the response-queue can be nreversed to drain in order.
  (it "response-queue-can-be-pushed-and-drained"
    (with-screen (s 10 5)
      (push "response-a" (cl-tmux/terminal/types:screen-response-queue s))
      (push "response-b" (cl-tmux/terminal/types:screen-response-queue s))
      (let ((items (nreverse (cl-tmux/terminal/types:screen-response-queue s))))
        (setf (cl-tmux/terminal/types:screen-response-queue s) nil)
        (expect (equal '("response-a" "response-b") items)))))

  ;; Setting response-queue to NIL empties it.
  (it "response-queue-cleared-after-drain"
    (with-screen (s 10 5)
      (push "data" (cl-tmux/terminal/types:screen-response-queue s))
      (setf (cl-tmux/terminal/types:screen-response-queue s) nil)
      (expect (null (cl-tmux/terminal/types:screen-response-queue s))))))

;;; ── SUITE: origin-mode slot ──────────────────────────────────────────────────
;;;
;;; screen-origin-mode (DECOM ?6) is exercised indirectly by modes-tests via
;;; the ?6h/?6l sequences.  This suite verifies the raw slot contract directly.

(describe "terminal-suite/origin-mode-slot-suite"

  ;; A fresh screen has origin-mode NIL (absolute cursor positioning).
  (it "screen-origin-mode-defaults-false"
    (with-screen (s 10 5)
      (expect (cl-tmux/terminal/types:screen-origin-mode s) :to-be-falsy)))

  ;; screen-origin-mode can be set to T and cleared back to NIL via setf.
  (it "screen-origin-mode-can-be-set"
    (with-screen (s 10 5)
      (setf (cl-tmux/terminal/types:screen-origin-mode s) t)
      (expect (cl-tmux/terminal/types:screen-origin-mode s) :to-be-truthy)
      (setf (cl-tmux/terminal/types:screen-origin-mode s) nil)
      (expect (cl-tmux/terminal/types:screen-origin-mode s) :to-be-falsy)))

  ;; ESC[?6h (DECOM set) enables origin mode.
  (it "screen-origin-mode-enabled-by-sequence"
    (with-screen (s 10 5)
      (feed s (esc "[?6h"))
      (expect (cl-tmux/terminal/types:screen-origin-mode s) :to-be-truthy)))

  ;; ESC[?6l (DECOM reset) disables origin mode.
  (it "screen-origin-mode-disabled-by-sequence"
    (with-screen (s 10 5)
      (feed s (esc "[?6h"))
      (feed s (esc "[?6l"))
      (expect (cl-tmux/terminal/types:screen-origin-mode s) :to-be-falsy))))

;;; ── SUITE: tab-stops slot ────────────────────────────────────────────────────
;;;
;;; screen-tab-stops defaults to the :default sentinel meaning "standard
;;; every-8-columns stops."  HTS (ESC H) materialises it into an explicit list.

(describe "terminal-suite/tab-stops-slot-suite"

  ;; A fresh screen has tab-stops :default (standard every-8-column stops).
  (it "screen-tab-stops-defaults-to-sentinel"
    (with-screen (s 80 5)
      (expect (eq :default (cl-tmux/terminal/types:screen-tab-stops s)))))

  ;; screen-tab-stops can be replaced with an explicit sorted stop list.
  (it "screen-tab-stops-can-be-set-to-list"
    (with-screen (s 80 5)
      (setf (cl-tmux/terminal/types:screen-tab-stops s) '(0 8 16 24))
      (expect (equal '(0 8 16 24) (cl-tmux/terminal/types:screen-tab-stops s)))))

  ;; ESC H (HTS) at column 4 materialises the sentinel into an explicit list containing 4.
  (it "screen-tab-stops-hts-materialises-sentinel"
    (with-screen (s 80 5)
      ;; Move cursor to column 4 then set a tab stop there.
      ;; ESC H = 0x1B 0x48 — use (esc "H") which prepends ESC (char 27) before "H".
      (feed s (esc "[5G"))   ; CHA — move cursor to column 5 (1-based), i.e. 0-based col 4
      (feed s (esc "H"))     ; ESC H = HTS (set tab stop at current cursor column)
      (let ((stops (cl-tmux/terminal/types:screen-tab-stops s)))
        (expect (listp stops))
        (expect (member 4 stops))))))

;;; ── SUITE: screen-lock slot ──────────────────────────────────────────────────
;;;
;;; screen-lock is a bordeaux-threads lock created at construction time.
;;; Unit tests verify the slot is populated and has the expected type.

(describe "terminal-suite/screen-lock-suite"

  ;; A fresh screen's screen-lock slot is non-NIL.
  (it "screen-lock-is-present-and-non-nil"
    (with-screen (s 10 5)
      (expect (cl-tmux/terminal/types:screen-lock s) :to-be-truthy)))

  ;; The screen lock can be acquired and released without error.
  (it "screen-lock-can-be-acquired-and-released"
    (with-screen (s 10 5)
      (let ((lock (cl-tmux/terminal/types:screen-lock s)))
        (finishes
          (bordeaux-threads:acquire-lock lock)
          (bordeaux-threads:release-lock lock))))))

;;; ── SUITE: screen-cells and screen-parser accessors ─────────────────────────
;;;
;;; Both accessors are exported from cl-tmux/terminal/types but previously had
;;; no dedicated unit tests verifying their slot contracts.

(describe "terminal-suite/screen-cells-parser-suite"

  ;; screen-cells returns a simple-vector whose length equals width*height.
  (it "screen-cells-returns-simple-vector"
    (with-screen (s 8 4)
      (let ((cells (cl-tmux/terminal/types:screen-cells s)))
        (expect (simple-vector-p cells))
        (expect (= (* 8 4) (length cells))))))

  ;; Every element of screen-cells on a fresh screen is a blank cell.
  (it "screen-cells-all-elements-are-cells"
    (with-screen (s 4 3)
      (let ((cells (cl-tmux/terminal/types:screen-cells s)))
        (dotimes (i (* 4 3))
          (expect (cl-tmux/terminal/types:cell-p (aref cells i)))
          (expect (char= #\Space (cell-char (aref cells i))))))))

  ;; screen-parser is a function, and correctly processes printable ASCII (confirming ground-state).
  (it "screen-parser-is-wired-ground-state"
    (with-screen (s 10 5)
      (expect (functionp (cl-tmux/terminal/types:screen-parser s)))
      (screen-process-bytes s #(65))      ; 65 = #\A
      (expect (char= #\A (char-at s 0 0))))))
