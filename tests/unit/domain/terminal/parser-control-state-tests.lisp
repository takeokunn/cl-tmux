(in-package #:cl-tmux/test)

;;;; Terminal parser control-state and direct continuation tests.

;;; ── ground-state control-byte coverage ──────────────────────────────────────

(def-suite ground-state-control-bytes
  :description "ground-state handling of DEL, SO, SI, stray continuation bytes, and unhandled C0"
  :in terminal-suite)
(in-suite ground-state-control-bytes)

(test ground-state-ignored-bytes-table
  "ground-state on DEL (#x7F), SO (#x0E), and SI (#x0F) returns ground-state and writes nothing."
  (dolist (row '((#x7F "DEL must return ground-state"          "DEL must not write a visible character")
                 (#x0E "SO must return ground-state"           "SO must not write a visible character")
                 (#x0F "SI must return ground-state"           "SI must not write a visible character")))
    (destructuring-bind (byte state-desc char-desc) row
      (let ((s (make-screen 10 5)))
        (let ((next (cl-tmux/terminal/parser:ground-state s byte)))
          (is (eq #'cl-tmux/terminal/parser:ground-state next) "~A" state-desc)
          (is (char= #\Space (char-at s 0 0)) "~A" char-desc))))))

(test ground-state-stray-continuation-byte-emits-replacement
  "ground-state on a stray UTF-8 continuation byte (#x80) writes U+FFFD."
  (let ((s (make-screen 10 5)))
    (cl-tmux/terminal/parser:ground-state s #x80)
    (is (char= (code-char #xFFFD) (char-at s 0 0))
        "stray continuation byte must produce U+FFFD replacement character")))

(test ground-state-unhandled-c0-is-ignored
  "ground-state on unhandled C0 bytes (e.g. #x01, #x02) returns ground-state silently."
  (let ((s (make-screen 10 5)))
    (let ((next (cl-tmux/terminal/parser:ground-state s #x01)))
      (is (eq #'cl-tmux/terminal/parser:ground-state next)
          "unhandled C0 (#x01) must return ground-state"))
    (let ((next2 (cl-tmux/terminal/parser:ground-state s #x02)))
      (is (eq #'cl-tmux/terminal/parser:ground-state next2)
          "unhandled C0 (#x02) must return ground-state"))))

(test escape-state-unrecognized-byte-returns-ground
  "escape-state on an unrecognized byte (e.g. #x40 = '@') returns ground-state."
  (let ((s (make-screen 10 5)))
    (let ((next (cl-tmux/terminal/parser:escape-state s #x40)))
      (is (eq #'cl-tmux/terminal/parser:ground-state next)
          "unrecognized ESC byte must return ground-state"))))

(test escape-state-m-reverse-index-returns-ground
  "escape-state on #x4D ('M' = RI / reverse index) moves cursor up and returns ground-state."
  (with-screen (s 10 5)
    (feed s (esc "[3;1H"))    ; move to row 2 (0-based)
    (let ((next (cl-tmux/terminal/parser:escape-state s #x4D)))
      (is (eq #'cl-tmux/terminal/parser:ground-state next)
          "ESC M must return ground-state")
      ;; Cursor should have moved up one row (from 2 to 1).
      (is (= 1 (screen-cursor-y s))
          "ESC M (RI) must move cursor up one row"))))

(test escape-state-7-saves-cursor
  "escape-state on #x37 ('7' = DECSC) saves cursor and returns ground-state."
  (with-screen (s 10 5)
    (feed s (esc "[3;6H"))    ; cursor -> (5, 2)
    (let ((next (cl-tmux/terminal/parser:escape-state s #x37)))
      (is (eq #'cl-tmux/terminal/parser:ground-state next)
          "ESC 7 must return ground-state")
      ;; Saved cursor should be non-nil.
      (is (not (null (cl-tmux/terminal/types:screen-saved-cursor s)))
          "ESC 7 must have saved the cursor"))))

(test escape-state-8-restores-cursor
  "escape-state on #x38 ('8' = DECRC) restores cursor and returns ground-state."
  (with-screen (s 10 5)
    (feed s (esc "[3;6H"))    ; cursor -> (5, 2)
    (feed s (esc "7"))        ; ESC 7 -- save
    (feed s (esc "[1;1H"))    ; move to origin
    (let ((next (cl-tmux/terminal/parser:escape-state s #x38)))
      (is (eq #'cl-tmux/terminal/parser:ground-state next)
          "ESC 8 must return ground-state")
      ;; Cursor should be restored to (5, 2).
      (check-cursor s 5 2))))

(test escape-state-P-dcs-returns-continuation
  "escape-state on #x50 ('P' = DCS introducer) returns a DCS accumulator function."
  (with-screen (s 10 5)
    (let ((next (cl-tmux/terminal/parser:escape-state s #x50)))
      (is (functionp next)
          "ESC P must return a DCS accumulator continuation function"))))

(test escape-state-open-paren-returns-charset-designator
  "escape-state on #x28 ('(' = G0 designator introducer) returns a designator
   continuation that designates G0 to the next byte's charset."
  (with-screen (s 10 5)
    (let ((next (cl-tmux/terminal/parser:escape-state s #x28)))
      (is (functionp next) "ESC ( must return a charset-designator continuation")
      (funcall next s 48)                ; '0' -> DEC graphics
      (is (eq :dec-graphics (cl-tmux/terminal/types:screen-g0-charset s))
          "ESC ( 0 must designate G0 to :dec-graphics"))))

(test escape-state-close-bracket-returns-osc-state
  "escape-state on #x5D (']' = OSC introducer) returns osc-state."
  (with-screen (s 10 5)
    (let ((next (cl-tmux/terminal/parser:escape-state s #x5D)))
      (is (eq #'cl-tmux/terminal/parser:osc-state next)
          "ESC ] must return osc-state"))))

;;; ── make-dcs-k direct tests ──────────────────────────────────────────────────

(def-suite direct-dcs-suite
  :description "Direct calls to make-dcs-k DCS accumulator"
  :in terminal-suite)
(in-suite direct-dcs-suite)

(test make-dcs-k-consumes-payload-bytes
  "make-dcs-k continuation consumes non-ESC payload bytes and returns a continuation."
  (let* ((s  (make-screen 10 5))
         (k0 (cl-tmux/terminal/parser::make-dcs-k))
         ;; Feed a non-ESC payload byte
         (k1 (funcall k0 s (char-code #\H))))
    (is (functionp k1)
        "make-dcs-k must return a function after consuming a payload byte")))

(test make-dcs-k-terminates-on-esc-backslash
  "make-dcs-k returns ground-state after receiving ESC (#x1B) then backslash (#x5C)."
  (let* ((s   (make-screen 10 5))
         (k0  (cl-tmux/terminal/parser::make-dcs-k))
         ;; Feed some payload
         (k1  (funcall k0 s (char-code #\X)))
         ;; Feed ESC -> waiting for backslash
         (k2  (funcall k1 s #x1B))
         ;; Feed backslash = ST confirmed
         (result (funcall k2 s #x5C)))
    (is (eq #'cl-tmux/terminal/parser:ground-state result)
        "make-dcs-k must return ground-state after ESC+backslash ST")))

(test make-dcs-k-non-backslash-after-esc-continues
  "make-dcs-k after ESC followed by a non-backslash keeps consuming."
  (let* ((s   (make-screen 10 5))
         (k0  (cl-tmux/terminal/parser::make-dcs-k))
         (k1  (funcall k0 s #x1B))     ; ESC -> waiting for backslash
         ;; Feed a non-backslash byte -- should continue consuming DCS
         (k2  (funcall k1 s (char-code #\A))))
    (is (functionp k2)
        "non-backslash after ESC inside DCS must return a continuation, not ground-state")))

;;; ── G2/G3 designation + LS2/LS3 locking shifts + SS2/SS3 single shifts ───────

(test g2-g3-designation-and-locking-shifts
  "ESC * 0 designates G2 (without invoking it); ESC n (LS2) locks G2 in so
   line-drawing applies; ESC o (LS3, G3 still ASCII) switches back to plain."
  (with-screen (s 20 5)
    ;; Designate G2 = DEC graphics: nothing is invoked yet.
    (feed s (format nil "~C*0" #\Escape))
    (is (eq :dec-graphics (cl-tmux/terminal/types:screen-g2-charset s))
        "ESC * 0 must designate G2 to DEC graphics")
    (is (eq :ascii (cl-tmux/terminal/types:screen-charset s))
        "designating G2 must not change the effective charset")
    (feed s "q")
    (is (char= #\q (char-at s 0 0))
        "before LS2, 'q' must print as plain ASCII")
    ;; LS2: lock G2 in -- line drawing now applies.
    (feed s (format nil "~Cn" #\Escape))
    (is (eq :dec-graphics (cl-tmux/terminal/types:screen-charset s))
        "LS2 must make G2's designation effective")
    (feed s "q")
    (is (char= #\─ (char-at s 1 0))
        "after LS2, 'q' must map to the box-drawing dash")
    ;; LS3: G3 is still ASCII, so plain text resumes.
    (feed s (format nil "~Co" #\Escape))
    (feed s "q")
    (is (char= #\q (char-at s 2 0))
        "after LS3 (G3=ascii), 'q' must be plain again")))

(test ss2-single-shift-maps-one-character
  "ESC N (SS2) maps exactly ONE character through G2's designation, then the
   locking charset resumes."
  (with-screen (s 20 5)
    (feed s (format nil "~C*0" #\Escape))     ; G2 = DEC graphics
    (feed s (format nil "~CNq" #\Escape))     ; SS2 + q
    (feed s "q")                              ; plain q afterwards
    (is (char= #\─ (char-at s 0 0))
        "the SS2-shifted character must map through G2 (line drawing)")
    (is (char= #\q (char-at s 1 0))
        "the character AFTER the single shift must be plain ASCII")))

(test ris-resets-g2-g3-and-single-shift
  "RIS (ESC c) resets G2/G3 designations and any pending single shift."
  (with-screen (s 20 5)
    (feed s (format nil "~C*0~C+0~CN" #\Escape #\Escape #\Escape))
    (feed s (format nil "~Cc" #\Escape))
    (is (eq :ascii (cl-tmux/terminal/types:screen-g2-charset s))
        "RIS must reset G2 to ASCII")
    (is (eq :ascii (cl-tmux/terminal/types:screen-g3-charset s))
        "RIS must reset G3 to ASCII")
    (is (null (cl-tmux/terminal/types:screen-single-shift s))
        "RIS must clear a pending single shift")))
