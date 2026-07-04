(in-package #:cl-tmux/test)

;;;; Parser tests (src/terminal/parser.lisp).
;;;; OSC coverage and the parser-special suite.

;;; ── SUITE: special ──────────────────────────────────────────────────────────
;;;
;;; Parser-level behaviour only: BEL, OSC, unknown CSI, DEC cursor-visibility.
;;; Mode/state tests (RIS, alt-screen, DECSC/DECRC) live in modes-tests.lisp.

(def-suite special
  :description "Parser behaviour: BEL, OSC, unknown CSI, DEC PM cursor visibility"
  :in terminal-suite)
(in-suite special)

;;; ── Coverage gap: DEFINE-STATE generated docstrings ──────────────────────────
;;;
;;; ground-state, escape-state, and osc-state are exported from
;;; cl-tmux/terminal/parser but, before DEFINE-STATE injected a generated
;;; docstring, carried no function-level documentation (only block comments).
;;; (define-state-macro-is-defined, verifying define-state is a macro, lives
;;; in parser-state-cps-tests.lisp alongside the other CPS state-function tests.)

(test ground-escape-osc-state-have-docstrings
  "The exported DEFINE-STATE entry points ground-state, escape-state, and
   osc-state each carry a non-empty function docstring."
  (dolist (fn-symbol '(cl-tmux/terminal/parser:ground-state
                       cl-tmux/terminal/parser:escape-state
                       cl-tmux/terminal/parser:osc-state))
    (let ((doc (documentation fn-symbol 'function)))
      (is (and (stringp doc) (plusp (length doc)))
          "~A must have a non-empty docstring" fn-symbol))))

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
                 (#xFF0000 "#f00"                 "#RGB expands (0xF->0xFF)")
                 (#xFF0000 "rgb:ffff/0000/0000"   "rgb: with 16-bit channels scales down to 8-bit")
                 (#x00FF00 "rgb:00/ff/00"         "rgb: with 8-bit channels")))
    (destructuring-bind (expected input desc) row
      (is (= expected (cl-tmux/terminal/parser::%parse-osc-color input)) "~A" desc)))
  (dolist (input '("tomato" "rgb:zz/00/00"))
    (is (null (cl-tmux/terminal/parser::%parse-osc-color input))
        "invalid colour string -> NIL: ~S" input)))

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
        "OSC 4 query must report the custom override, not palette index 1")))

(test osc-4-set-and-query-in-one-sequence
  "OSC 4 ; 1 ; rgb:... ; 1 ; ? both sets and then queries in one payload."
  (with-screen (s 20 5)
    (%feed-osc s "4;1;rgb:ffff/0000/ffff;1;?")
    (is (string= (format nil "~C]4;1;rgb:ffff/0000/ffff~C\\" #\Escape #\Escape)
                 (first (cl-tmux/terminal/types:screen-response-queue s)))
        "combined set+query must still yield the custom override reply")))

(test osc-4-set-junk-spec-is-ignored
  "An invalid OSC 4 spec such as 'junk' is ignored and enqueues no reply."
  (with-screen (s 20 5)
    (%feed-osc s "4;junk")
    (is (null (cl-tmux/terminal/types:screen-response-queue s))
        "junk OSC 4 spec must be ignored without reply")))

(test osc-104-resets-single-index
  "OSC 104 ; 1 resets only palette index 1 to its default."
  (with-screen (s 20 5)
    (%feed-osc s "4;1;rgb:ffff/0000/ffff")
    (%feed-osc s "104;1")
    (is (null (cl-tmux/terminal/types:%palette-override-get s 1))
        "OSC 104 ; 1 must clear the custom override at index 1")))

(test osc-104-empty-body-resets-all
  "OSC 104 with no parameters resets all palette overrides."
  (with-screen (s 20 5)
    (%feed-osc s "4;1;rgb:ffff/0000/ffff")
    (%feed-osc s "104")
    (is (null (cl-tmux/terminal/types:%palette-override-get s 1))
        "OSC 104 with no params must clear index 1")))

(test osc-color-command-routes-query-and-set
  "The OSC colour command dispatcher routes both query and set forms."
  (with-screen (s 20 5)
    (%feed-osc s "10;?")
    (is (not (null (cl-tmux/terminal/types:screen-response-queue s)))
        "OSC 10 query must enqueue a reply"))
  (with-screen (s 20 5)
    (%feed-osc s "10;#112233")
    (is (= #x112233 (cl-tmux/terminal/types:screen-osc-default-fg s))
        "OSC 10 set must update screen-osc-default-fg")))

(test osc-11-query-reports-default-background
  "OSC 11 ; ? reports the default background colour."
  (with-screen (s 20 5)
    (%feed-osc s "11;?")
    (is (string= (format nil "~C]11;rgb:0000/0000/0000~C\\" #\Escape #\Escape)
                 (first (cl-tmux/terminal/types:screen-response-queue s)))
        "OSC 11 query must report the default background")))

(test osc-10-query-reports-default-foreground
  "OSC 10 ; ? reports the default foreground colour."
  (with-screen (s 20 5)
    (%feed-osc s "10;?")
    (is (string= (format nil "~C]10;rgb:ffff/ffff/ffff~C\\" #\Escape #\Escape)
                 (first (cl-tmux/terminal/types:screen-response-queue s)))
        "OSC 10 query must report the default foreground")))

(test osc-11-set-updates-default-background
  "OSC 11 ; rgb:... updates the default background and sends no reply.
   This is the key 'set' path; the query path is exercised by osc-11-query...;
   the effect on the stored background is asserted here and the absence of reply
   keeps it true to terminal-set semantics."
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
  "OSC 111 (no parameter) resets the default background to black after a set -
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
  (is (null (cl-tmux/terminal/parser::%xterm-palette-rgb 256)) "out of range -> NIL"))

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
