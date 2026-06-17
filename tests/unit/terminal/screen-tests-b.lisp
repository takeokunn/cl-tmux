(in-package #:cl-tmux/test)

;;;; screen tests — part B: copy-mode slots, alt-screen cursor save,
;;;; mouse-sgr-mode, response-queue, origin-mode, tab-stops,
;;;; screen-lock, screen-cells/screen-parser accessors.

(def-suite copy-mode-slots
  :description "copy-mode selection, cursor, search-term, and line-selection slots"
  :in terminal-suite)
(in-suite copy-mode-slots)

(test screen-copy-slots-default-to-nil
  "All copy-mode slots default to NIL/false on a fresh screen."
  (with-screen (s 10 5)
    (is (null (screen-copy-cursor s))                                 "copy-cursor must start as NIL")
    (is-false (screen-copy-selecting s)                               "copy-selecting must default to NIL")
    (is (null (cl-tmux/terminal/types:screen-copy-search-term s))     "copy-search-term must start as NIL")
    (is-false (cl-tmux/terminal/types:screen-copy-line-selection-p s) "copy-line-selection-p must default to NIL")))

(test screen-copy-cursor-can-be-set
  :description "copy-cursor can be set to a (row . col) pair via setf."
  (with-screen (s 10 5)
    (setf (screen-copy-cursor s) (list 2 3))
    (is (equal '(2 3) (screen-copy-cursor s))
        "copy-cursor must hold the value after setf")))

(test screen-copy-selecting-can-be-toggled
  :description "copy-selecting can be set and cleared via setf."
  (with-screen (s 10 5)
    (setf (screen-copy-selecting s) t)
    (is-true (screen-copy-selecting s)
             "copy-selecting must be T after setf T")
    (setf (screen-copy-selecting s) nil)
    (is-false (screen-copy-selecting s)
              "copy-selecting must be NIL after setf NIL")))

(test screen-copy-search-term-can-be-set
  :description "copy-search-term can hold an arbitrary search string."
  (with-screen (s 10 5)
    (setf (cl-tmux/terminal/types:screen-copy-search-term s) "hello")
    (is (string= "hello" (cl-tmux/terminal/types:screen-copy-search-term s))
        "copy-search-term must hold the stored string")))

;;; ── SUITE: alt-screen cursor save slots ─────────────────────────────────────

(def-suite alt-screen-slots
  :description "Alt-screen cursor save/restore slot defaults"
  :in terminal-suite)
(in-suite alt-screen-slots)

(test screen-alt-cursor-defaults-zero
  "alt-cursor-x and alt-cursor-y both start at 0 on a fresh screen."
  (with-screen (s 10 5)
    (is (= 0 (cl-tmux/terminal/types:screen-alt-cursor-x s)) "alt-cursor-x must default to 0")
    (is (= 0 (cl-tmux/terminal/types:screen-alt-cursor-y s)) "alt-cursor-y must default to 0")))

(test screen-alt-cells-defaults-nil
  :description "alt-cells slot is NIL before entering alt-screen mode."
  (with-screen (s 10 5)
    (is (null (cl-tmux/terminal/types:screen-alt-cells s))
        "alt-cells must be NIL on a fresh screen")))

(test screen-alt-cursor-saved-on-alt-screen-entry
  :description "Entering alt-screen via ESC[?1049h saves cursor position into alt-cursor-x/y."
  (with-screen (s 20 10)
    (feed s (esc "[5;10H"))   ; move cursor to (row=4, col=9)
    (feed s (esc "[?1049h"))  ; enter alt screen
    ;; After entering alt-screen, alt-cursor-x/y should hold the saved cursor.
    (is (= 9 (cl-tmux/terminal/types:screen-alt-cursor-x s))
        "alt-cursor-x must be saved column 9")
    (is (= 4 (cl-tmux/terminal/types:screen-alt-cursor-y s))
        "alt-cursor-y must be saved row 4")))

(test alt-screen-1049-saves-and-restores-full-cursor-state
  "Mode ?1049h saves the full cursor state (SGR attrs) and ?1049l restores it.
   Apps like neovim rely on this to restore their primary-screen rendering state."
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
      (is (= 0 (cl-tmux/terminal/types:screen-cursor-x s))
          "cursor x must be restored to 0 (the saved primary position)")
      ;; SGR state restored: bold + red should be back
      (is (= saved-attrs (cl-tmux/terminal/types:screen-cur-attrs s))
          "screen-cur-attrs must be restored after ?1049l")
      (is (= saved-fg (cl-tmux/terminal/types:screen-cur-fg s))
          "screen-cur-fg must be restored to red after ?1049l"))))

;;; ── SUITE: mouse-sgr-mode slot ───────────────────────────────────────────────

(def-suite mouse-sgr-mode-suite
  :description "screen-mouse-sgr-mode slot default and toggle"
  :in terminal-suite)
(in-suite mouse-sgr-mode-suite)

(test screen-mouse-sgr-mode-defaults-false
  :description "mouse-sgr-mode is NIL on a fresh screen."
  (with-screen (s 10 5)
    (is-false (screen-mouse-sgr-mode s)
              "mouse-sgr-mode must default to NIL")))

(test screen-mouse-sgr-mode-enabled-by-1006h
  :description "ESC[?1006h enables SGR extended mouse encoding."
  (with-screen (s 10 5)
    (feed s (esc "[?1006h"))
    (is-true (screen-mouse-sgr-mode s)
             "mouse-sgr-mode must be T after ESC[?1006h")))

(test screen-mouse-sgr-mode-disabled-by-1006l
  :description "ESC[?1006l disables SGR extended mouse encoding."
  (with-screen (s 10 5)
    (feed s (esc "[?1006h"))
    (feed s (esc "[?1006l"))
    (is-false (screen-mouse-sgr-mode s)
              "mouse-sgr-mode must be NIL after ESC[?1006l")))

;;; ── SUITE: response-queue ────────────────────────────────────────────────────

(def-suite response-queue-suite
  :description "screen-response-queue: push and drain behaviour"
  :in terminal-suite)
(in-suite response-queue-suite)

(test response-queue-starts-nil
  :description "Response queue is NIL on a fresh screen."
  (with-screen (s 10 5)
    (is (null (cl-tmux/terminal/types:screen-response-queue s))
        "response-queue must be NIL initially")))

(test response-queue-can-be-pushed-and-drained
  :description "Items pushed onto the response-queue can be nreversed to drain in order."
  (with-screen (s 10 5)
    (push "response-a" (cl-tmux/terminal/types:screen-response-queue s))
    (push "response-b" (cl-tmux/terminal/types:screen-response-queue s))
    (let ((items (nreverse (cl-tmux/terminal/types:screen-response-queue s))))
      (setf (cl-tmux/terminal/types:screen-response-queue s) nil)
      (is (equal '("response-a" "response-b") items)
          "drained items must appear in push order"))))

(test response-queue-cleared-after-drain
  :description "Setting response-queue to NIL empties it."
  (with-screen (s 10 5)
    (push "data" (cl-tmux/terminal/types:screen-response-queue s))
    (setf (cl-tmux/terminal/types:screen-response-queue s) nil)
    (is (null (cl-tmux/terminal/types:screen-response-queue s))
        "response-queue must be NIL after explicit clear")))

;;; ── SUITE: origin-mode slot ──────────────────────────────────────────────────
;;;
;;; screen-origin-mode (DECOM ?6) is exercised indirectly by modes-tests via
;;; the ?6h/?6l sequences.  This suite verifies the raw slot contract directly.

(def-suite origin-mode-slot-suite
  :description "screen-origin-mode slot: default value and setf contract"
  :in terminal-suite)
(in-suite origin-mode-slot-suite)

(test screen-origin-mode-defaults-false
  :description "A fresh screen has origin-mode NIL (absolute cursor positioning)."
  (with-screen (s 10 5)
    (is-false (cl-tmux/terminal/types:screen-origin-mode s)
              "origin-mode must default to NIL")))

(test screen-origin-mode-can-be-set
  :description "screen-origin-mode can be set to T and cleared back to NIL via setf."
  (with-screen (s 10 5)
    (setf (cl-tmux/terminal/types:screen-origin-mode s) t)
    (is-true (cl-tmux/terminal/types:screen-origin-mode s)
             "origin-mode must be T after setf T")
    (setf (cl-tmux/terminal/types:screen-origin-mode s) nil)
    (is-false (cl-tmux/terminal/types:screen-origin-mode s)
              "origin-mode must be NIL after setf NIL")))

(test screen-origin-mode-enabled-by-sequence
  :description "ESC[?6h (DECOM set) enables origin mode."
  (with-screen (s 10 5)
    (feed s (esc "[?6h"))
    (is-true (cl-tmux/terminal/types:screen-origin-mode s)
             "origin-mode must be T after ESC[?6h")))

(test screen-origin-mode-disabled-by-sequence
  :description "ESC[?6l (DECOM reset) disables origin mode."
  (with-screen (s 10 5)
    (feed s (esc "[?6h"))
    (feed s (esc "[?6l"))
    (is-false (cl-tmux/terminal/types:screen-origin-mode s)
              "origin-mode must be NIL after ESC[?6l")))

;;; ── SUITE: tab-stops slot ────────────────────────────────────────────────────
;;;
;;; screen-tab-stops defaults to the :default sentinel meaning "standard
;;; every-8-columns stops."  HTS (ESC H) materialises it into an explicit list.

(def-suite tab-stops-slot-suite
  :description "screen-tab-stops slot: default sentinel and HTS materialisation"
  :in terminal-suite)
(in-suite tab-stops-slot-suite)

(test screen-tab-stops-defaults-to-sentinel
  :description "A fresh screen has tab-stops :default (standard every-8-column stops)."
  (with-screen (s 80 5)
    (is (eq :default (cl-tmux/terminal/types:screen-tab-stops s))
        "tab-stops must default to :default sentinel")))

(test screen-tab-stops-can-be-set-to-list
  :description "screen-tab-stops can be replaced with an explicit sorted stop list."
  (with-screen (s 80 5)
    (setf (cl-tmux/terminal/types:screen-tab-stops s) '(0 8 16 24))
    (is (equal '(0 8 16 24) (cl-tmux/terminal/types:screen-tab-stops s))
        "tab-stops must hold the explicit list after setf")))

(test screen-tab-stops-hts-materialises-sentinel
  :description "ESC H (HTS) at column 4 materialises the sentinel into an explicit list containing 4."
  (with-screen (s 80 5)
    ;; Move cursor to column 4 then set a tab stop there.
    ;; ESC H = 0x1B 0x48 — use (esc "H") which prepends ESC (char 27) before "H".
    (feed s (esc "[5G"))   ; CHA — move cursor to column 5 (1-based), i.e. 0-based col 4
    (feed s (esc "H"))     ; ESC H = HTS (set tab stop at current cursor column)
    (let ((stops (cl-tmux/terminal/types:screen-tab-stops s)))
      (is (listp stops) "tab-stops must be a list after HTS")
      (is (member 4 stops) "HTS at column 4 must add 4 to the stop list"))))

;;; ── SUITE: screen-lock slot ──────────────────────────────────────────────────
;;;
;;; screen-lock is a bordeaux-threads lock created at construction time.
;;; Unit tests verify the slot is populated and has the expected type.

(def-suite screen-lock-suite
  :description "screen-lock slot: construction and type contract"
  :in terminal-suite)
(in-suite screen-lock-suite)

(test screen-lock-is-present-and-non-nil
  "A fresh screen's screen-lock slot is non-NIL."
  (with-screen (s 10 5)
    (is-true (cl-tmux/terminal/types:screen-lock s)
             "screen-lock must be non-NIL after make-screen")))

(test screen-lock-can-be-acquired-and-released
  :description "The screen lock can be acquired and released without error."
  (with-screen (s 10 5)
    (let ((lock (cl-tmux/terminal/types:screen-lock s)))
      (finishes
        (bordeaux-threads:acquire-lock lock)
        (bordeaux-threads:release-lock lock)))))

;;; ── SUITE: screen-cells and screen-parser accessors ─────────────────────────
;;;
;;; Both accessors are exported from cl-tmux/terminal/types but previously had
;;; no dedicated unit tests verifying their slot contracts.

(def-suite screen-cells-parser-suite
  :description "screen-cells and screen-parser accessor contracts"
  :in terminal-suite)
(in-suite screen-cells-parser-suite)

(test screen-cells-returns-simple-vector
  :description "screen-cells returns a simple-vector whose length equals width*height."
  (with-screen (s 8 4)
    (let ((cells (cl-tmux/terminal/types:screen-cells s)))
      (is (simple-vector-p cells)
          "screen-cells must return a simple-vector")
      (is (= (* 8 4) (length cells))
          "screen-cells length must equal width*height"))))

(test screen-cells-all-elements-are-cells
  :description "Every element of screen-cells on a fresh screen is a blank cell."
  (with-screen (s 4 3)
    (let ((cells (cl-tmux/terminal/types:screen-cells s)))
      (dotimes (i (* 4 3))
        (is (cl-tmux/terminal/types:cell-p (aref cells i))
            "screen-cells element ~D must satisfy cell-p" i)
        (is (char= #\Space (cell-char (aref cells i)))
            "screen-cells element ~D must be a space cell" i)))))

(test screen-parser-is-wired-ground-state
  "screen-parser is a function, and correctly processes printable ASCII (confirming ground-state)."
  (with-screen (s 10 5)
    (is (functionp (cl-tmux/terminal/types:screen-parser s))
        "screen-parser must be a function")
    (screen-process-bytes s #(65))      ; 65 = #\A
    (is (char= #\A (char-at s 0 0))
        "parser must process 'A' correctly (ground-state wired in make-screen)")))
