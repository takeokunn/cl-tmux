(in-package #:cl-tmux/test)

;;;; VT100/ANSI emulator tests.  Feed byte sequences into a screen and assert
;;;; the resulting cell grid, cursor position, and SGR state.

(def-suite terminal-suite :description "VT100/ANSI terminal emulator")
(in-suite terminal-suite)

;;; ── Helpers ────────────────────────────────────────────────────────────────

(defun octets (string)
  "Convert STRING to an (unsigned-byte 8) vector (Latin-1; \\e = ESC #x1B)."
  (let ((v (make-array (length string) :element-type '(unsigned-byte 8))))
    (dotimes (i (length string) v)
      (setf (aref v i) (char-code (char string i))))))

(defun feed (screen string)
  "Process STRING (each char = one byte) through SCREEN."
  (screen-process-bytes screen (octets string))
  screen)

(defun row-string (screen y &key (start 0) end)
  "Return the characters of row Y from START to END (default: full width)."
  (let* ((w   (screen-width screen))
         (end (or end w)))
    (with-output-to-string (s)
      (loop for x from start below (min end w)
            do (write-char (cell-char (screen-cell screen x y)) s)))))

(defmacro with-screen ((var w h) &body body)
  `(let ((,var (make-screen ,w ,h))) ,@body))

;;; ── Printable text ─────────────────────────────────────────────────────────

(test plain-text
  (with-screen (s 20 5)
    (feed s "hello")
    (is (string= "hello" (row-string s 0 :end 5)))
    (is (= 5 (screen-cursor-x s)))
    (is (= 0 (screen-cursor-y s)))))

(test carriage-return-and-linefeed
  (with-screen (s 20 5)
    (feed s "ab")
    (feed s (format nil "~C~C" #\Return #\Linefeed))  ; CR LF
    (feed s "cd")
    (is (string= "ab" (row-string s 0 :end 2)))
    (is (string= "cd" (row-string s 1 :end 2)))
    (is (= 1 (screen-cursor-y s)))
    (is (= 2 (screen-cursor-x s)))))

(test line-wrap-at-right-margin
  (with-screen (s 4 3)
    (feed s "abcde")             ; 5 chars into a 4-wide screen
    (is (string= "abcd" (row-string s 0)))
    (is (string= "e"    (row-string s 1 :end 1)))
    (is (= 1 (screen-cursor-y s)))
    (is (= 1 (screen-cursor-x s)))))

(test backspace
  (with-screen (s 10 2)
    (feed s "abc")
    (feed s (string #\Backspace))
    (is (= 2 (screen-cursor-x s)))))

(test tab-advances-to-multiple-of-8
  (with-screen (s 40 2)
    (feed s "a")
    (feed s (string #\Tab))
    (is (= 8 (screen-cursor-x s)))))

;;; ── CSI cursor movement ──────────────────────────────────────────────────

(test cursor-position-csi-h
  (with-screen (s 20 10)
    (feed s (format nil "~C[3;5HX" #\Escape))  ; row 3, col 5 (1-based)
    (is (char= #\X (cell-char (screen-cell s 4 2))))))

(test cursor-up-down-left-right
  (with-screen (s 20 10)
    (feed s (format nil "~C[5;5H" #\Escape))   ; to (4,4) 0-based
    (feed s (format nil "~C[2A" #\Escape))     ; up 2  → y=2
    (is (= 2 (screen-cursor-y s)))
    (feed s (format nil "~C[3B" #\Escape))     ; down 3 → y=5
    (is (= 5 (screen-cursor-y s)))
    (feed s (format nil "~C[2C" #\Escape))     ; right 2 → x=6
    (is (= 6 (screen-cursor-x s)))
    (feed s (format nil "~C[4D" #\Escape))     ; left 4 → x=2
    (is (= 2 (screen-cursor-x s)))))

(test cursor-clamps-at-edges
  (with-screen (s 10 5)
    (feed s (format nil "~C[100;100H" #\Escape))  ; way past edges
    (is (= 9 (screen-cursor-x s)))
    (is (= 4 (screen-cursor-y s)))))

;;; ── Erase ────────────────────────────────────────────────────────────────

(test erase-entire-display
  (with-screen (s 10 3)
    (feed s "xxxxx")
    (feed s (format nil "~C[2J" #\Escape))
    (is (string= "          " (row-string s 0)))))

(test erase-line-to-end
  (with-screen (s 10 2)
    (feed s "abcdef")
    (feed s (format nil "~C[1;4H" #\Escape))   ; move to col 4 (0-based 3)
    (feed s (format nil "~C[0K" #\Escape))     ; erase to end of line
    (is (string= "abc" (row-string s 0 :end 3)))
    (is (char= #\Space (cell-char (screen-cell s 3 0))))))

;;; ── SGR colours / attributes ───────────────────────────────────────────────

(test sgr-foreground-colour
  (with-screen (s 10 2)
    (feed s (format nil "~C[31mR" #\Escape))   ; red foreground
    (is (= 1 (cell-fg (screen-cell s 0 0))))))

(test sgr-background-colour
  (with-screen (s 10 2)
    (feed s (format nil "~C[42mG" #\Escape))   ; green background
    (is (= 2 (cell-bg (screen-cell s 0 0))))))

(test sgr-bold-attribute
  (with-screen (s 10 2)
    (feed s (format nil "~C[1mB" #\Escape))    ; bold
    (is (logbitp 0 (cell-attrs (screen-cell s 0 0))))))

(test sgr-reset
  (with-screen (s 10 2)
    (feed s (format nil "~C[31;1mX~C[0mY" #\Escape #\Escape))
    (is (= 7 (cell-fg (screen-cell s 1 0))))     ; Y back to default fg
    (is (= 0 (cell-attrs (screen-cell s 1 0))))))

(test sgr-bright-foreground
  (with-screen (s 10 2)
    (feed s (format nil "~C[91mR" #\Escape))   ; bright red → index 9
    (is (= 9 (cell-fg (screen-cell s 0 0))))))

;;; ── UTF-8 decoding ─────────────────────────────────────────────────────────

(defun utf8-feed (screen lisp-string)
  "Encode LISP-STRING as UTF-8 octets and feed them to SCREEN."
  (screen-process-bytes screen (babel:string-to-octets lisp-string :encoding :utf-8))
  screen)

(test utf8-two-byte
  (with-screen (s 10 2)
    (utf8-feed s "é")                ; U+00E9, 2-byte UTF-8
    (is (char= #\é (cell-char (screen-cell s 0 0))))))

(test utf8-three-byte-cjk
  (with-screen (s 10 2)
    (utf8-feed s "あ")               ; U+3042, 3-byte UTF-8
    (is (char= #\あ (cell-char (screen-cell s 0 0))))))

(test utf8-box-drawing
  (with-screen (s 10 2)
    (utf8-feed s "│─")               ; box-drawing chars used for borders
    (is (char= #\│ (cell-char (screen-cell s 0 0))))
    (is (char= #\─ (cell-char (screen-cell s 1 0))))))

(test utf8-split-across-chunks
  (with-screen (s 10 2)
    ;; U+3042 = E3 81 82; deliver the bytes in two separate calls.
    (screen-process-bytes s (make-array 1 :element-type '(unsigned-byte 8)
                                          :initial-contents '(#xE3)))
    (screen-process-bytes s (make-array 2 :element-type '(unsigned-byte 8)
                                          :initial-contents '(#x81 #x82)))
    (is (char= #\あ (cell-char (screen-cell s 0 0))))))

(test utf8-mixed-with-ascii
  (with-screen (s 10 2)
    (utf8-feed s "aあb")
    (is (char= #\a (cell-char (screen-cell s 0 0))))
    (is (char= #\あ (cell-char (screen-cell s 1 0))))
    (is (char= #\b (cell-char (screen-cell s 2 0))))))

;;; ── Scrolling ──────────────────────────────────────────────────────────────

(test scroll-up-on-overflow
  (with-screen (s 5 3)
    ;; Fill 3 rows, then a 4th line forces a scroll.
    (feed s (format nil "L1~C~CL2~C~CL3~C~CL4"
                    #\Return #\Linefeed #\Return #\Linefeed #\Return #\Linefeed))
    ;; After scroll, L2 should now be on row 0, L4 on row 2.
    (is (string= "L2" (row-string s 0 :end 2)))
    (is (string= "L4" (row-string s 2 :end 2)))))

;;; ── Resize ─────────────────────────────────────────────────────────────────

(test resize-preserves-top-left-content
  (with-screen (s 10 5)
    (feed s "hello")
    (screen-resize s 20 8)
    (is (= 20 (screen-width s)))
    (is (= 8  (screen-height s)))
    (is (string= "hello" (row-string s 0 :end 5)))))

(test resize-shrink-clamps-cursor
  (with-screen (s 20 10)
    (feed s (format nil "~C[10;20H" #\Escape))  ; cursor near bottom-right
    (screen-resize s 5 3)
    (is (<= (screen-cursor-x s) 4))
    (is (<= (screen-cursor-y s) 2))))

(test resize-noop-when-unchanged
  (with-screen (s 10 5)
    (feed s "abc")
    (screen-resize s 10 5)        ; same size: must not clobber content
    (is (string= "abc" (row-string s 0 :end 3)))))
