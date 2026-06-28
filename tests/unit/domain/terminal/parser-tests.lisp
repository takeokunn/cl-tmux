(in-package #:cl-tmux/test)

;;;; Parser tests (src/terminal/parser.lisp).
;;;; Tests: utf8, special, basic-text suites.

;;; ── SUITE: utf8 ─────────────────────────────────────────────────────────────

(def-suite utf8
  :description "Multi-byte UTF-8 character decoding"
  :in terminal-suite)
(in-suite utf8)

(test utf8-multibyte-table
  "Multi-byte UTF-8 characters decode and appear at the correct screen position."
  (dolist (row '((#\é "2-byte: U+00E9 é")
                 (#\あ "3-byte: U+3042 あ")))
    (destructuring-bind (char desc) row
      (with-screen (s 10 2)
        (utf8-feed s (string char))
        (is (char= char (char-at s 0 0)) "~A" desc)))))

(test utf8-4byte
  "A 4-byte UTF-8 code point is decoded correctly (e.g. U+1F600 if in limit)."
  ;; U+1F600 = 😀; only test if the Lisp runtime supports it.
  (when (< #x1F600 char-code-limit)
    (with-screen (s 10 2)
      ;; Feed the 4-byte UTF-8 sequence for U+1F600: F0 9F 98 80
      (screen-process-bytes s (make-array 4 :element-type '(unsigned-byte 8)
                                            :initial-contents '(#xF0 #x9F #x98 #x80)))
      (is (char= (code-char #x1F600) (char-at s 0 0))))))

(test utf8-split
  "U+3042 split across two feed calls (E3 | 81 82) assembles correctly."
  (with-screen (s 10 2)
    (screen-process-bytes s (make-array 1 :element-type '(unsigned-byte 8)
                                          :initial-contents '(#xE3)))
    (screen-process-bytes s (make-array 2 :element-type '(unsigned-byte 8)
                                          :initial-contents '(#x81 #x82)))
    (is (char= #\あ (char-at s 0 0)))))

(test utf8-mixed
  "ASCII + wide CJK + ASCII: the CJK char occupies two columns, so the
   trailing ASCII lands at column 3 (column 2 is the continuation cell)."
  (with-screen (s 10 2)
    (utf8-feed s "aあb")
    (is (char= #\a  (char-at s 0 0)))
    (is (char= #\あ (char-at s 1 0)))
    (is (= 2 (cell-width (cell-at s 1 0))) "あ must be a double-width lead cell")
    (is (= 0 (cell-width (cell-at s 2 0))) "column 2 must be a continuation cell")
    (is (char= #\b  (char-at s 3 0)) "trailing ASCII lands after the wide char")))

(test utf8-box-drawing
  "Box-drawing characters are decoded and placed correctly."
  (with-screen (s 10 2)
    (utf8-feed s "│─")
    (is (char= #\│ (char-at s 0 0)))
    (is (char= #\─ (char-at s 1 0)))))

(test utf8-malformed
  "A bare #xFF byte (invalid UTF-8) produces U+FFFD at the cursor."
  (with-screen (s 10 2)
    (screen-process-bytes s (make-array 1 :element-type '(unsigned-byte 8)
                                          :initial-contents '(#xFF)))
    (is (char= (code-char #xFFFD) (char-at s 0 0)))))

;;; ── SUITE: special ──────────────────────────────────────────────────────────
;;;
;;; Parser-level behaviour only: BEL, OSC, unknown CSI, DEC cursor-visibility.
;;; Mode/state tests (RIS, alt-screen, DECSC/DECRC) live in modes-tests.lisp.

(def-suite special
  :description "Parser behaviour: BEL, OSC, unknown CSI, DEC PM cursor visibility"
  :in terminal-suite)
(in-suite special)

(test bel-sets-bell-pending
  "BEL (byte #x07) sets screen-bell-pending to T without altering the screen or cursor."
  (with-screen (s 10 2)
    (feed s "ab")
    (is-false (cl-tmux/terminal/types:screen-bell-pending s)
              "bell-pending must be NIL before BEL")
    (screen-process-bytes s (make-array 1 :element-type '(unsigned-byte 8)
                                          :initial-contents '(#x07)))
    ;; Screen content and cursor must be unchanged.
    (is (char= #\a (char-at s 0 0)))
    (is (char= #\b (char-at s 1 0)))
    (check-cursor s 2 0)
    ;; bell-pending must now be set.
    (is (cl-tmux/terminal/types:screen-bell-pending s)
        "bell-pending must be T after BEL byte")))

;;; ── OSC 10/11 dynamic colours ────────────────────────────────────────────────

(defun %feed-osc (s payload)
  "Feed an OSC sequence (ESC ] PAYLOAD ST) to screen S via screen-process-bytes."
  (screen-process-bytes s
    (babel:string-to-octets (format nil "~C]~A~C\\" #\Escape payload #\Escape)
                            :encoding :utf-8)))

(test osc-0-1-2-set-screen-title
  "OSC 0, 1, and 2 all set screen-title (cl-tmux keeps a single title slot).
   Uses a table-driven loop to avoid repeating the identical 5-line pattern."
  (dolist (row '((0 "my-window"  "OSC 0 sets the window title")
                 (1 "icon-name"  "OSC 1 (icon name) also sets screen-title")
                 (2 "xterm-title" "OSC 2 also sets the window title")))
    (destructuring-bind (cmd title desc) row
      (with-screen (s 20 5)
        (%feed-osc s (format nil "~D;~A" cmd title))
        (is (string= title (cl-tmux/terminal/types:screen-title s)) "~A" desc)))))

(test parse-osc-color-forms
  "%parse-osc-color parses #RRGGBB, #RGB and rgb:R/G/B; rejects junk."
  (dolist (row '((#xFF8000 "#ff8000"              "#RRGGBB")
                 (#xFF0000 "#f00"                 "#RGB expands (0xF→0xFF)")
                 (#xFF0000 "rgb:ffff/0000/0000"   "rgb: with 16-bit channels scales down to 8-bit")
                 (#x00FF00 "rgb:00/ff/00"         "rgb: with 8-bit channels")))
    (destructuring-bind (expected input desc) row
      (is (= expected (cl-tmux/terminal/parser::%parse-osc-color input)) "~A" desc)))
  (dolist (input '("tomato" "rgb:zz/00/00"))
    (is (null (cl-tmux/terminal/parser::%parse-osc-color input))
        "invalid colour string → NIL: ~S" input)))

(test osc-color-helper-replies-format-correctly
  "%osc-color-reply and %osc4-reply build xterm-style ESC ] ... rgb:RRRR/GGGG/BBBB replies."
  (is (string= (format nil "~C]11;rgb:0101/0202/0303~C\\" #\Escape #\Escape)
               (cl-tmux/terminal/parser::%osc-color-reply 11 #x010203))
      "OSC 11 reply must double each 8-bit channel")
  (is (string= (format nil "~C]4;196;rgb:ffff/0000/0000~C\\" #\Escape #\Escape)
               (cl-tmux/terminal/parser::%osc4-reply 196 #xFF0000))
      "OSC 4 reply must include the palette index"))

(test osc-rgb-reply-channel-doubling-arithmetic
  "%osc-rgb-reply uses the 0x101 (= 257) scale factor to expand each 8-bit
   channel to a 16-bit xterm hex string: 0x00->\"0000\", 0xFF->\"ffff\",
   0x80->\"8080\".  This covers the (* byte #x101) idiom in %osc-hex-channel."
  (dolist (row '((#x000000 "0000" "0000" "0000" "black  #x00->\"0000\"")
                 (#xFFFFFF "ffff" "ffff" "ffff" "white  #xFF->\"ffff\"")
                 (#x800000 "8080" "0000" "0000" "maroon #x80->\"8080\"")
                 (#x010203 "0101" "0202" "0303" "mixed  #x01->\"0101\" #x02->\"0202\" #x03->\"0303\"")))
    (destructuring-bind (rgb er eg eb desc) row
      (let ((reply (cl-tmux/terminal/parser::%osc-rgb-reply "]11;rgb:" rgb)))
        (is (search (format nil "~A/~A/~A" er eg eb) reply) "~A" desc)))))

;;; ── OSC 4 / OSC 104 custom palette overrides (audit #23) ─────────────────────

(test osc-4-set-does-not-reply
  "OSC 4 ; 1 ; rgb:... (a SET, not a query) enqueues no reply."
  (with-screen (s 20 5)
    (%feed-osc s "4;1;rgb:ffff/0000/ffff")
    (is (null (cl-tmux/terminal/types:screen-response-queue s))
        "an OSC 4 SET must not enqueue a reply")))

(test osc-4-set-stores-custom-palette-override
  "OSC 4 ; 1 ; rgb:... stores a custom override for palette index 1."
  (with-screen (s 20 5)
    (%feed-osc s "4;1;rgb:ffff/0000/ffff")
    (is (= #xFF00FF (cl-tmux/terminal/types:%palette-override-get s 1))
        "OSC 4 set must store the parsed colour as a custom override")))

(test osc-4-query-reports-custom-override
  "After OSC 4 sets index 1, OSC 4 ; 1 ; ? reports the custom colour, not the
   built-in palette entry."
  (with-screen (s 20 5)
    (%feed-osc s "4;1;rgb:ffff/0000/ffff")
    (%feed-osc s "4;1;?")
    (is (string= (format nil "~C]4;1;rgb:ffff/0000/ffff~C\\" #\Escape #\Escape)
                 (first (cl-tmux/terminal/types:screen-response-queue s)))
        "OSC 4 query after a set must report the custom override")))

(test osc-4-set-and-query-in-one-sequence
  "OSC 4 ; 2 ; #00ff00 ; 2 ; ? sets then queries index 2 in one sequence,
   reporting the just-set colour."
  (with-screen (s 20 5)
    (%feed-osc s "4;2;#00ff00;2;?")
    (is (string= (format nil "~C]4;2;rgb:0000/ffff/0000~C\\" #\Escape #\Escape)
                 (first (cl-tmux/terminal/types:screen-response-queue s)))
        "set-then-query in one OSC 4 must report the set colour")))

(test osc-4-set-junk-spec-is-ignored
  "OSC 4 ; 3 ; tomato (an unparseable colour) stores no override and enqueues no
   reply."
  (with-screen (s 20 5)
    (%feed-osc s "4;3;tomato")
    (is (null (cl-tmux/terminal/types:%palette-override-get s 3))
        "an unparseable OSC 4 colour must not store an override")
    (is (null (cl-tmux/terminal/types:screen-response-queue s))
        "an unparseable OSC 4 set must not enqueue a reply")))

(test osc-104-resets-single-index
  "OSC 104 ; 1 reverts index 1 to the built-in palette; a subsequent query reports
   the standard xterm colour again."
  (with-screen (s 20 5)
    (%feed-osc s "4;1;rgb:ffff/0000/ffff")
    (%feed-osc s "104;1")
    (is (null (cl-tmux/terminal/types:%palette-override-get s 1))
        "OSC 104 ; 1 must clear the index-1 override")
    (%feed-osc s "4;1;?")
    (is (string= (format nil "~C]4;1;rgb:8080/0000/0000~C\\" #\Escape #\Escape)
                 (first (cl-tmux/terminal/types:screen-response-queue s)))
        "after OSC 104 the query reports built-in index 1 (maroon 0x800000)")))

(test osc-104-empty-body-resets-all
  "OSC 104 with an empty body drops every custom palette override."
  (with-screen (s 20 5)
    (%feed-osc s "4;1;rgb:ffff/0000/ffff")
    (%feed-osc s "4;200;#123456")
    (%feed-osc s "104")
    (is (null (cl-tmux/terminal/types:screen-palette-overrides s))
        "OSC 104 (empty body) must clear all overrides")
    (is (null (cl-tmux/terminal/types:%palette-override-get s 1))
        "index 1 override must be gone after a full reset")
    (is (null (cl-tmux/terminal/types:%palette-override-get s 200))
        "index 200 override must be gone after a full reset")))

(test osc-color-command-routes-query-and-set
  "%osc-color-command enqueues replies for '?' and delegates set bodies to the setter function."
  (with-screen (s 20 5)
    (let ((seen nil))
      (cl-tmux/terminal/parser::%osc-color-command
       s 10 "?" #xFFFFFF (lambda (rgb) (setf seen rgb)))
      (is (null seen) "query path must not call the setter")
      (is (string= (format nil "~C]10;rgb:ffff/ffff/ffff~C\\" #\Escape #\Escape)
                   (first (cl-tmux/terminal/types:screen-response-queue s)))
          "query path must enqueue a reply"))
    (let ((seen nil))
      (setf (cl-tmux/terminal/types:screen-response-queue s) nil)
      (cl-tmux/terminal/parser::%osc-color-command
       s 11 "#3366ff" #x000000 (lambda (rgb) (setf seen rgb)))
      (is (= #x3366FF seen) "set path must call the setter with the parsed colour")
      (is (null (cl-tmux/terminal/types:screen-response-queue s))
          "set path must not enqueue a reply"))))

(test osc-11-query-reports-default-background
  "OSC 11 ; ? queries the default background; cl-tmux replies on the response-queue
   with the stored colour (black by default), so apps can detect a dark theme."
  (with-screen (s 20 5)
    (%feed-osc s "11;?")
    (let ((replies (cl-tmux/terminal/types:screen-response-queue s)))
      (is (= 1 (length replies)) "exactly one OSC 11 reply is enqueued")
      (is (string= (format nil "~C]11;rgb:0000/0000/0000~C\\" #\Escape #\Escape)
                   (first replies))
          "reply reports black background (got ~S)" (first replies)))))

(test osc-10-query-reports-default-foreground
  "OSC 10 ; ? reports the default foreground (white) as rgb:ffff/ffff/ffff."
  (with-screen (s 20 5)
    (%feed-osc s "10;?")
    (is (string= (format nil "~C]10;rgb:ffff/ffff/ffff~C\\" #\Escape #\Escape)
                 (first (cl-tmux/terminal/types:screen-response-queue s)))
        "OSC 10 query reports white foreground")))

(test osc-11-set-updates-default-background
  "OSC 11 ; rgb:ffff/0000/0000 sets the stored background to 0xFF0000 and replies
   to nothing (only queries reply)."
  (with-screen (s 20 5)
    (%feed-osc s "11;rgb:ffff/0000/0000")
    (is (= #xFF0000 (cl-tmux/terminal/types:screen-osc-default-bg s))
        "OSC 11 set updates screen-osc-default-bg to 0xFF0000")
    (is (null (cl-tmux/terminal/types:screen-response-queue s))
        "a SET must not enqueue a reply")))

(test osc-11-query-after-set-roundtrips
  "After OSC 11 sets the background, OSC 11 ; ? reports the new colour back."
  (with-screen (s 20 5)
    (%feed-osc s "11;#3366ff")
    (%feed-osc s "11;?")
    (is (string= (format nil "~C]11;rgb:3333/6666/ffff~C\\" #\Escape #\Escape)
                 (first (cl-tmux/terminal/types:screen-response-queue s)))
        "query after #3366ff reports rgb:3333/6666/ffff")))

(test osc-111-resets-default-background
  "OSC 111 (no parameter) resets the default background to black after a set —
   exercising the parameterless OSC dispatch path."
  (with-screen (s 20 5)
    (%feed-osc s "11;#ffffff")
    (is (= #xFFFFFF (cl-tmux/terminal/types:screen-osc-default-bg s)) "bg set to white")
    (%feed-osc s "111")
    (is (= #x000000 (cl-tmux/terminal/types:screen-osc-default-bg s))
        "OSC 111 resets the background to black")))

;;; ── OSC 4 palette queries ────────────────────────────────────────────────────

(test xterm-palette-rgb-values
  "%xterm-palette-rgb maps indices to the standard xterm 256-colour palette."
  (dolist (c '((#x000000   0 "index 0 = black")
               (#xffffff  15 "index 15 = white")
               (#x000000  16 "cube origin = black")
               (#x0000ff  21 "index 21 = pure blue")
               (#xff0000 196 "index 196 = pure red")
               (#xffffff 231 "cube max = white")
               (#x080808 232 "grayscale ramp start")
               (#xeeeeee 255 "grayscale ramp end")))
    (destructuring-bind (expected idx desc) c
      (is (= expected (cl-tmux/terminal/parser::%xterm-palette-rgb idx)) "~A" desc)))
  (is (null (cl-tmux/terminal/parser::%xterm-palette-rgb 256)) "out of range → NIL"))

(test osc-4-query-reports-palette-colour
  "OSC 4 ; 196 ; ? reports palette index 196 as pure red (rgb:ffff/0000/0000)."
  (with-screen (s 20 5)
    (%feed-osc s "4;196;?")
    (is (string= (format nil "~C]4;196;rgb:ffff/0000/0000~C\\" #\Escape #\Escape)
                 (first (cl-tmux/terminal/types:screen-response-queue s)))
        "OSC 4 query must report index 196 as red")))

(test osc-4-query-multiple-indices
  "OSC 4 ; 0 ; ? ; 15 ; ? enqueues one reply per queried index."
  (with-screen (s 20 5)
    (%feed-osc s "4;0;?;15;?")
    (is (= 2 (length (cl-tmux/terminal/types:screen-response-queue s)))
        "two OSC 4 queries must enqueue two replies")))

(test osc-4-set-does-not-reply
  "OSC 4 ; 1 ; rgb:... (a SET, not a query) enqueues no reply (set is ignored)."
  (with-screen (s 20 5)
    (%feed-osc s "4;1;rgb:ffff/0000/ffff")
    (is (null (cl-tmux/terminal/types:screen-response-queue s))
        "an OSC 4 SET must not enqueue a reply")))

;;; ── OSC 8 hyperlinks ─────────────────────────────────────────────────────────

(test osc-8-stamps-cell-hyperlink
  "OSC 8 ; ; URI sets the current hyperlink so the next written cell carries it;
   OSC 8 ; ; clears it (the following cell has no hyperlink)."
  (with-screen (s 20 5)
    (%feed-osc s "8;;https://example.com")
    (feed s "X")
    (%feed-osc s "8;;")        ; clear the hyperlink
    (feed s "Y")
    (is (string= "https://example.com"
                 (cl-tmux/terminal/types:cell-hyperlink
                  (cl-tmux/terminal/types:screen-cell s 0 0)))
        "the cell written under OSC 8 must carry the hyperlink")
    (is (null (cl-tmux/terminal/types:cell-hyperlink
               (cl-tmux/terminal/types:screen-cell s 1 0)))
        "the cell after OSC 8 ; ; must have no hyperlink")))

(test osc-bel-no-crash
  "An OSC sequence terminated by BEL is consumed without crashing."
  (with-screen (s 10 2)
    (feed s "a")
    ;; OSC 0 ; title BEL -- common in xterm
    (screen-process-bytes s
      (babel:string-to-octets
        (format nil "~C]0;window title~C" #\Escape (code-char 7))
        :encoding :utf-8))
    (feed s "b")
    (is (char= #\a (char-at s 0 0)))
    (is (char= #\b (char-at s 1 0)))))

(test osc-st-ignored
  "An OSC sequence terminated by ESC \\ (ST) is consumed without crashing."
  (with-screen (s 10 2)
    (feed s "a")
    ;; OSC terminated by ST = ESC \
    (screen-process-bytes s
      (babel:string-to-octets
        (format nil "~C]0;title~C\\" #\Escape #\Escape)
        :encoding :utf-8))
    (feed s "b")
    (is (char= #\a (char-at s 0 0)))
    (is (char= #\b (char-at s 1 0)))))

(test csi-private-lt-marker-consumed-not-stray
  "CSI < t (XTPOPTITLE) and CSI = c (DA3) use the < / = private markers; the byte
   must route to the marker slot, not abort the sequence and print the final byte
   as a stray char."
  (with-screen (s 10 2)
    (feed s "a")
    (feed s (esc "[<t"))       ; XTPOPTITLE — pop title (no-op), prints nothing
    (feed s "b")
    (is (char= #\a (char-at s 0 0)) "'a' at column 0")
    (is (char= #\b (char-at s 1 0))
        "'b' at column 1 — no stray 't' printed between them")))

(test esc-hash-8-decaln-fills-screen-with-e
  "ESC # 8 (DECALN) fills the entire screen with 'E' (the VT100 alignment test)."
  (with-screen (s 4 2)
    (feed s (esc "#8"))
    (dotimes (y 2)
      (dotimes (x 4)
        (is (char= #\E (char-at s x y))
            "cell (~D,~D) must be 'E' after DECALN" x y)))))

(test esc-hash-selector-consumed-not-stray
  "ESC # <selector> consumes the selector byte; ESC # 5 (DECSWL, no-op) prints
   nothing — the byte must not abort the sequence and print as a stray char."
  (with-screen (s 10 2)
    (feed s "a")
    (feed s (esc "#5"))        ; DECSWL — single-width line, no-op
    (feed s "b")
    (is (char= #\a (char-at s 0 0)) "'a' at column 0")
    (is (char= #\b (char-at s 1 0))
        "'b' at column 1 — no stray '5' printed between them")))

(test esc-star-plus-g2-g3-designator-consumed-not-stray
  "ESC * X (designate G2) and ESC + X (designate G3) consume the designator byte
   without printing it as a stray char (G2/G3 accepted but not modeled)."
  (with-screen (s 10 2)
    (feed s "a")
    (feed s (esc "*0"))        ; designate G2 = DEC graphics (consumes '0')
    (feed s (esc "+B"))        ; designate G3 = ASCII (consumes 'B')
    (feed s "b")
    (is (char= #\a (char-at s 0 0)) "'a' at column 0")
    (is (char= #\b (char-at s 1 0))
        "'b' at column 1 — no stray '0' or 'B' from the G2/G3 designators")))

(test esc-space-and-percent-two-byte-seqs-consumed-not-stray
  "ESC SP F (S7C1T) and ESC % G (select UTF-8) consume their trailing byte without
   printing it as a stray char."
  (with-screen (s 10 2)
    (feed s "a")
    (feed s (esc " F"))        ; ESC SP F — S7C1T (consumes 'F')
    (feed s (esc "%G"))        ; ESC % G — select UTF-8 (consumes 'G')
    (feed s "b")
    (is (char= #\a (char-at s 0 0)) "'a' at column 0")
    (is (char= #\b (char-at s 1 0))
        "'b' at column 1 — no stray 'F'/'G' from the two-byte ESC sequences")))

(test csi-unknown
  "An unrecognised CSI final character is silently ignored; parser recovers."
  (with-screen (s 10 2)
    (feed s "a")
    ;; ESC [ z  -- 'z' is not a standard CSI final
    (feed s (esc "[z"))
    (feed s "b")
    (is (char= #\a (char-at s 0 0)))
    (is (char= #\b (char-at s 1 0)))))

(test dec-pm-hide-show-cursor
  "ESC[?25l (hide cursor) and ESC[?25h (show cursor) do not crash."
  (with-screen (s 10 2)
    (feed s "a")
    (feed s (esc "[?25l"))    ; hide cursor -- accepted silently
    (feed s "b")
    (feed s (esc "[?25h"))    ; show cursor -- accepted silently
    (feed s "c")
    ;; All three characters must be on screen.
    (is (char= #\a (char-at s 0 0)))
    (is (char= #\b (char-at s 1 0)))
    (is (char= #\c (char-at s 2 0)))))
