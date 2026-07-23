(in-package #:cl-tmux/test)

;;;; Terminal parser control-state and direct continuation tests.

;;; ── ground-state control-byte coverage ──────────────────────────────────────

(describe "terminal-suite/ground-state-control-bytes"

  ;; ground-state on DEL (#x7F), SO (#x0E), and SI (#x0F) returns ground-state and writes nothing.
  (it "ground-state-ignored-bytes-table"
    (dolist (row '((#x7F "DEL must return ground-state"          "DEL must not write a visible character")
                   (#x0E "SO must return ground-state"           "SO must not write a visible character")
                   (#x0F "SI must return ground-state"           "SI must not write a visible character")))
      (destructuring-bind (byte state-desc char-desc) row
        (declare (ignore state-desc char-desc))
        (let ((s (make-screen 10 5)))
          (let ((next (cl-tmux/terminal/parser:ground-state s byte)))
            (expect (eq #'cl-tmux/terminal/parser:ground-state next))
            (expect (char= #\Space (char-at s 0 0))))))))

  ;; ground-state on a stray UTF-8 continuation byte (#x80) writes U+FFFD.
  (it "ground-state-stray-continuation-byte-emits-replacement"
    (let ((s (make-screen 10 5)))
      (cl-tmux/terminal/parser:ground-state s #x80)
      (expect (char= (code-char #xFFFD) (char-at s 0 0)))))

  ;; ground-state on unhandled C0 bytes (e.g. #x01, #x02) returns ground-state silently.
  (it "ground-state-unhandled-c0-is-ignored"
    (let ((s (make-screen 10 5)))
      (let ((next (cl-tmux/terminal/parser:ground-state s #x01)))
        (expect (eq #'cl-tmux/terminal/parser:ground-state next)))
      (let ((next2 (cl-tmux/terminal/parser:ground-state s #x02)))
        (expect (eq #'cl-tmux/terminal/parser:ground-state next2)))))

  ;; escape-state on an unrecognized byte (e.g. #x40 = '@') returns ground-state.
  (it "escape-state-unrecognized-byte-returns-ground"
    (let ((s (make-screen 10 5)))
      (let ((next (cl-tmux/terminal/parser:escape-state s #x40)))
        (expect (eq #'cl-tmux/terminal/parser:ground-state next)))))

  ;; escape-state on #x4D ('M' = RI / reverse index) moves cursor up and returns ground-state.
  (it "escape-state-m-reverse-index-returns-ground"
    (with-screen (s 10 5)
      (feed s (esc "[3;1H"))    ; move to row 2 (0-based)
      (let ((next (cl-tmux/terminal/parser:escape-state s #x4D)))
        (expect (eq #'cl-tmux/terminal/parser:ground-state next))
        ;; Cursor should have moved up one row (from 2 to 1).
        (expect (= 1 (screen-cursor-y s))))))

  ;; escape-state on #x37 ('7' = DECSC) saves cursor and returns ground-state.
  (it "escape-state-7-saves-cursor"
    (with-screen (s 10 5)
      (feed s (esc "[3;6H"))    ; cursor -> (5, 2)
      (let ((next (cl-tmux/terminal/parser:escape-state s #x37)))
        (expect (eq #'cl-tmux/terminal/parser:ground-state next))
        ;; Saved cursor should be non-nil.
        (expect (not (null (cl-tmux/terminal/types:screen-saved-cursor s)))))))

  ;; escape-state on #x38 ('8' = DECRC) restores cursor and returns ground-state.
  (it "escape-state-8-restores-cursor"
    (with-screen (s 10 5)
      (feed s (esc "[3;6H"))    ; cursor -> (5, 2)
      (feed s (esc "7"))        ; ESC 7 -- save
      (feed s (esc "[1;1H"))    ; move to origin
      (let ((next (cl-tmux/terminal/parser:escape-state s #x38)))
        (expect (eq #'cl-tmux/terminal/parser:ground-state next))
        ;; Cursor should be restored to (5, 2).
        (check-cursor s 5 2))))

  ;; escape-state on #x50 ('P' = DCS introducer) returns a DCS accumulator function.
  (it "escape-state-P-dcs-returns-continuation"
    (with-screen (s 10 5)
      (let ((next (cl-tmux/terminal/parser:escape-state s #x50)))
        (expect (functionp next)))))

  ;; escape-state on #x28 ('(' = G0 designator introducer) returns a designator
  ;; continuation that designates G0 to the next byte's charset.
  (it "escape-state-open-paren-returns-charset-designator"
    (with-screen (s 10 5)
      (let ((next (cl-tmux/terminal/parser:escape-state s #x28)))
        (expect (functionp next))
        (funcall next s 48)                ; '0' -> DEC graphics
        (expect (eq :dec-graphics (cl-tmux/terminal/types:screen-g0-charset s))))))

  ;; escape-state on #x5D (']' = OSC introducer) returns osc-state.
  (it "escape-state-close-bracket-returns-osc-state"
    (with-screen (s 10 5)
      (let ((next (cl-tmux/terminal/parser:escape-state s #x5D)))
        (expect (eq #'cl-tmux/terminal/parser:osc-state next))))))

;;; ── make-dcs-k direct tests ──────────────────────────────────────────────────

(describe "terminal-suite/direct-dcs-suite"

  ;; make-dcs-k continuation consumes non-ESC payload bytes and returns a continuation.
  (it "make-dcs-k-consumes-payload-bytes"
    (let* ((s  (make-screen 10 5))
           (k0 (cl-tmux/terminal/parser::make-dcs-k))
           ;; Feed a non-ESC payload byte
           (k1 (funcall k0 s (char-code #\H))))
      (expect (functionp k1))))

  ;; make-dcs-k returns ground-state after receiving ESC (#x1B) then backslash (#x5C).
  (it "make-dcs-k-terminates-on-esc-backslash"
    (let* ((s   (make-screen 10 5))
           (k0  (cl-tmux/terminal/parser::make-dcs-k))
           ;; Feed some payload
           (k1  (funcall k0 s (char-code #\X)))
           ;; Feed ESC -> waiting for backslash
           (k2  (funcall k1 s #x1B))
           ;; Feed backslash = ST confirmed
           (result (funcall k2 s #x5C)))
      (expect (eq #'cl-tmux/terminal/parser:ground-state result))))

  ;; make-dcs-k after ESC followed by a non-backslash keeps consuming.
  (it "make-dcs-k-non-backslash-after-esc-continues"
    (let* ((s   (make-screen 10 5))
           (k0  (cl-tmux/terminal/parser::make-dcs-k))
           (k1  (funcall k0 s #x1B))     ; ESC -> waiting for backslash
           ;; Feed a non-backslash byte -- should continue consuming DCS
           (k2  (funcall k1 s (char-code #\A))))
      (expect (functionp k2))))

  ;;; ── G2/G3 designation + LS2/LS3 locking shifts + SS2/SS3 single shifts ───────

  ;; ESC * 0 designates G2 (without invoking it); ESC n (LS2) locks G2 in so
  ;; line-drawing applies; ESC o (LS3, G3 still ASCII) switches back to plain.
  (it "g2-g3-designation-and-locking-shifts"
    (with-screen (s 20 5)
      ;; Designate G2 = DEC graphics: nothing is invoked yet.
      (feed s (format nil "~C*0" #\Escape))
      (expect (eq :dec-graphics (cl-tmux/terminal/types:screen-g2-charset s)))
      (expect (eq :ascii (cl-tmux/terminal/types:screen-charset s)))
      (feed s "q")
      (expect (char= #\q (char-at s 0 0)))
      ;; LS2: lock G2 in -- line drawing now applies.
      (feed s (format nil "~Cn" #\Escape))
      (expect (eq :dec-graphics (cl-tmux/terminal/types:screen-charset s)))
      (feed s "q")
      (expect (char= #\─ (char-at s 1 0)))
      ;; LS3: G3 is still ASCII, so plain text resumes.
      (feed s (format nil "~Co" #\Escape))
      (feed s "q")
      (expect (char= #\q (char-at s 2 0)))))

  ;; ESC N (SS2) maps exactly ONE character through G2's designation, then the
  ;; locking charset resumes.
  (it "ss2-single-shift-maps-one-character"
    (with-screen (s 20 5)
      (feed s (format nil "~C*0" #\Escape))     ; G2 = DEC graphics
      (feed s (format nil "~CNq" #\Escape))     ; SS2 + q
      (feed s "q")                              ; plain q afterwards
      (expect (char= #\─ (char-at s 0 0)))
      (expect (char= #\q (char-at s 1 0)))))

  ;; RIS (ESC c) resets G2/G3 designations and any pending single shift.
  (it "ris-resets-g2-g3-and-single-shift"
    (with-screen (s 20 5)
      (feed s (format nil "~C*0~C+0~CN" #\Escape #\Escape #\Escape))
      (feed s (format nil "~Cc" #\Escape))
      (expect (eq :ascii (cl-tmux/terminal/types:screen-g2-charset s)))
      (expect (eq :ascii (cl-tmux/terminal/types:screen-g3-charset s)))
      (expect (null (cl-tmux/terminal/types:screen-single-shift s))))))
