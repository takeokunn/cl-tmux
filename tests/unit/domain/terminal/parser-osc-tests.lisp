(in-package #:cl-tmux/test)

;;;; Parser tests (src/terminal/parser.lisp).
;;;; OSC coverage and the parser-special suite.

(describe "terminal-suite/special"

  ;; ── SUITE: special ──────────────────────────────────────────────────────────
  ;;
  ;; Parser-level behaviour only: BEL, OSC, unknown CSI, DEC cursor-visibility.
  ;; Mode/state tests (RIS, alt-screen, DECSC/DECRC) live in modes-tests.lisp.

  ;; ── Coverage gap: DEFINE-STATE generated docstrings ──────────────────────────
  ;;
  ;; ground-state, escape-state, and osc-state are exported from
  ;; cl-tmux/terminal/parser but, before DEFINE-STATE injected a generated
  ;; docstring, carried no function-level documentation (only block comments).
  ;; (define-state-macro-is-defined, verifying define-state is a macro, lives
  ;; in parser-state-cps-tests.lisp alongside the other CPS state-function tests.)

  ;; The exported DEFINE-STATE entry points ground-state, escape-state, and
  ;; osc-state each carry a non-empty function docstring.
  (it "ground-escape-osc-state-have-docstrings"
    (dolist (fn-symbol '(cl-tmux/terminal/parser:ground-state
                         cl-tmux/terminal/parser:escape-state
                         cl-tmux/terminal/parser:osc-state))
      (let ((doc (documentation fn-symbol 'function)))
        (expect (and (stringp doc) (plusp (length doc)))))))

  ;; BEL (byte #x07) sets screen-bell-pending to T without altering the screen or cursor.
  (it "bel-sets-bell-pending"
    (with-screen (s 10 2)
      (feed s "ab")
      (expect (cl-tmux/terminal/types:screen-bell-pending s) :to-be-falsy)
      (screen-process-bytes s (make-array 1 :element-type '(unsigned-byte 8)
                                            :initial-contents '(#x07)))
      ;; Screen content and cursor must be unchanged.
      (expect (char= #\a (char-at s 0 0)))
      (expect (char= #\b (char-at s 1 0)))
      (check-cursor s 2 0)
      ;; bell-pending must now be set.
      (expect (cl-tmux/terminal/types:screen-bell-pending s))))

  ;; ── OSC 10/11 dynamic colours ────────────────────────────────────────────────

  (defun %feed-osc (s payload)
    "Feed an OSC sequence (ESC ] PAYLOAD ST) to screen S via screen-process-bytes."
    (screen-process-bytes s
      (babel:string-to-octets (format nil "~C]~A~C\\" #\Escape payload #\Escape)
                              :encoding :utf-8)))

  ;; OSC 0, 1, and 2 all set screen-title (cl-tmux keeps a single title slot).
  ;; Uses a table-driven loop to avoid repeating the identical 5-line pattern.
  (it "osc-0-1-2-set-screen-title"
    (dolist (row '((0 "my-window"  "OSC 0 sets the window title")
                   (1 "icon-name"  "OSC 1 (icon name) also sets screen-title")
                   (2 "xterm-title" "OSC 2 also sets the window title")))
      (destructuring-bind (cmd title desc) row
        (declare (ignore desc))
        (with-screen (s 20 5)
          (%feed-osc s (format nil "~D;~A" cmd title))
          (expect (string= title (cl-tmux/terminal/types:screen-title s)))))))

  ;; %parse-osc-color parses #RRGGBB, #RGB and rgb:R/G/B; rejects junk.
  (it "parse-osc-color-forms"
    (dolist (row '((#xFF8000 "#ff8000"              "#RRGGBB")
                   (#xFF0000 "#f00"                 "#RGB expands (0xF->0xFF)")
                   (#xFF0000 "rgb:ffff/0000/0000"   "rgb: with 16-bit channels scales down to 8-bit")
                   (#x00FF00 "rgb:00/ff/00"         "rgb: with 8-bit channels")
                   (#xAAABAB "rgb:a/abc/abcd"       "rgb: with mixed 4-bit/12-bit/16-bit channel widths")))
      (destructuring-bind (expected input desc) row
        (declare (ignore desc))
        (expect (= expected (cl-tmux/terminal/parser::%parse-osc-color input)))))
    (dolist (input '("tomato" "rgb:zz/00/00"))
      (expect (null (cl-tmux/terminal/parser::%parse-osc-color input)))))

  ;; %osc-color-reply and %osc4-reply build xterm-style ESC ] ... rgb:RRRR/GGGG/BBBB replies.
  (it "osc-color-helper-replies-format-correctly"
    (expect (string= (format nil "~C]11;rgb:0101/0202/0303~C\\" #\Escape #\Escape)
                     (cl-tmux/terminal/parser::%osc-color-reply 11 #x010203)))
    (expect (string= (format nil "~C]4;196;rgb:ffff/0000/0000~C\\" #\Escape #\Escape)
                     (cl-tmux/terminal/parser::%osc4-reply 196 #xFF0000))))

  ;; %osc-rgb-reply uses the 0x101 (= 257) scale factor to expand each 8-bit
  ;; channel to a 16-bit xterm hex string: 0x00->"0000", 0xFF->"ffff",
  ;; 0x80->"8080".  This covers the (* byte #x101) idiom in %osc-hex-channel.
  (it "osc-rgb-reply-channel-doubling-arithmetic"
    (dolist (row '((#x000000 "0000" "0000" "0000" "black  #x00->\"0000\"")
                   (#xFFFFFF "ffff" "ffff" "ffff" "white  #xFF->\"ffff\"")
                   (#x800000 "8080" "0000" "0000" "maroon #x80->\"8080\"")
                   (#x010203 "0101" "0202" "0303" "mixed  #x01->\"0101\" #x02->\"0202\" #x03->\"0303\"")))
      (destructuring-bind (rgb er eg eb desc) row
        (declare (ignore desc))
        (let ((reply (cl-tmux/terminal/parser::%osc-rgb-reply "]11;rgb:" rgb)))
          (expect (search (format nil "~A/~A/~A" er eg eb) reply))))))

  ;; ── OSC 4 / OSC 104 custom palette overrides (audit #23) ─────────────────────

  ;; OSC 4 ; 1 ; rgb:... (a SET, not a query) enqueues no reply.
  (it "osc-4-set-does-not-reply"
    (with-screen (s 20 5)
      (%feed-osc s "4;1;rgb:ffff/0000/ffff")
      (expect (null (cl-tmux/terminal/types:screen-response-queue s)))))

  ;; OSC 4 ; 1 ; rgb:... stores a custom override for palette index 1.
  (it "osc-4-set-stores-custom-palette-override"
    (with-screen (s 20 5)
      (%feed-osc s "4;1;rgb:ffff/0000/ffff")
      (expect (= #xFF00FF (cl-tmux/terminal/types:%palette-override-get s 1)))))

  ;; After OSC 4 sets index 1, OSC 4 ; 1 ; ? reports the custom colour, not the
  ;; built-in palette entry.
  (it "osc-4-query-reports-custom-override"
    (with-screen (s 20 5)
      (%feed-osc s "4;1;rgb:ffff/0000/ffff")
      (%feed-osc s "4;1;?")
      (expect (string= (format nil "~C]4;1;rgb:ffff/0000/ffff~C\\" #\Escape #\Escape)
                       (first (cl-tmux/terminal/types:screen-response-queue s))))))

  ;; OSC 4 ; 1 ; rgb:... ; 1 ; ? both sets and then queries in one payload.
  (it "osc-4-set-and-query-in-one-sequence"
    (with-screen (s 20 5)
      (%feed-osc s "4;1;rgb:ffff/0000/ffff;1;?")
      (expect (string= (format nil "~C]4;1;rgb:ffff/0000/ffff~C\\" #\Escape #\Escape)
                       (first (cl-tmux/terminal/types:screen-response-queue s))))))

  ;; An invalid OSC 4 spec such as 'junk' is ignored and enqueues no reply.
  (it "osc-4-set-junk-spec-is-ignored"
    (with-screen (s 20 5)
      (%feed-osc s "4;junk")
      (expect (null (cl-tmux/terminal/types:screen-response-queue s)))))

  ;; OSC 104 ; 1 resets only palette index 1 to its default.
  (it "osc-104-resets-single-index"
    (with-screen (s 20 5)
      (%feed-osc s "4;1;rgb:ffff/0000/ffff")
      (%feed-osc s "104;1")
      (expect (null (cl-tmux/terminal/types:%palette-override-get s 1)))))

  ;; OSC 104 with no parameters resets all palette overrides.
  (it "osc-104-empty-body-resets-all"
    (with-screen (s 20 5)
      (%feed-osc s "4;1;rgb:ffff/0000/ffff")
      (%feed-osc s "104")
      (expect (null (cl-tmux/terminal/types:%palette-override-get s 1)))))

  ;; The OSC colour command dispatcher routes both query and set forms.
  (it "osc-color-command-routes-query-and-set"
    (with-screen (s 20 5)
      (%feed-osc s "10;?")
      (expect (not (null (cl-tmux/terminal/types:screen-response-queue s)))))
    (with-screen (s 20 5)
      (%feed-osc s "10;#112233")
      (expect (= #x112233 (cl-tmux/terminal/types:screen-osc-default-fg s)))))

  ;; OSC 11 ; ? reports the default background colour.
  (it "osc-11-query-reports-default-background"
    (with-screen (s 20 5)
      (%feed-osc s "11;?")
      (expect (string= (format nil "~C]11;rgb:0000/0000/0000~C\\" #\Escape #\Escape)
                       (first (cl-tmux/terminal/types:screen-response-queue s))))))

  ;; OSC 10 ; ? reports the default foreground colour.
  (it "osc-10-query-reports-default-foreground"
    (with-screen (s 20 5)
      (%feed-osc s "10;?")
      (expect (string= (format nil "~C]10;rgb:ffff/ffff/ffff~C\\" #\Escape #\Escape)
                       (first (cl-tmux/terminal/types:screen-response-queue s))))))

  ;; OSC 11 ; rgb:... updates the default background and sends no reply.
  ;; This is the key 'set' path; the query path is exercised by osc-11-query...;
  ;; the effect on the stored background is asserted here and the absence of reply
  ;; keeps it true to terminal-set semantics.
  (it "osc-11-set-updates-default-background"
    (with-screen (s 20 5)
      (%feed-osc s "11;rgb:ffff/0000/0000")
      (expect (= #xFF0000 (cl-tmux/terminal/types:screen-osc-default-bg s)))
      (expect (null (cl-tmux/terminal/types:screen-response-queue s)))))

  ;; After OSC 11 sets the background, OSC 11 ; ? reports the new colour back.
  (it "osc-11-query-after-set-roundtrips"
    (with-screen (s 20 5)
      (%feed-osc s "11;#3366ff")
      (%feed-osc s "11;?")
      (expect (string= (format nil "~C]11;rgb:3333/6666/ffff~C\\" #\Escape #\Escape)
                       (first (cl-tmux/terminal/types:screen-response-queue s))))))

  ;; OSC 111 (no parameter) resets the default background to black after a set -
  ;; exercising the parameterless OSC dispatch path.
  (it "osc-111-resets-default-background"
    (with-screen (s 20 5)
      (%feed-osc s "11;#ffffff")
      (expect (= #xFFFFFF (cl-tmux/terminal/types:screen-osc-default-bg s)))
      (%feed-osc s "111")
      (expect (= #x000000 (cl-tmux/terminal/types:screen-osc-default-bg s)))))

  ;; ── OSC 4 palette queries ────────────────────────────────────────────────────

  ;; %xterm-palette-rgb maps indices to the standard xterm 256-colour palette.
  (it "xterm-palette-rgb-values"
    (dolist (c '((#x000000   0 "index 0 = black")
                 (#xffffff  15 "index 15 = white")
                 (#x000000  16 "cube origin = black")
                 (#x0000ff  21 "index 21 = pure blue")
                 (#xff0000 196 "index 196 = pure red")
                 (#xffffff 231 "cube max = white")
                 (#x080808 232 "grayscale ramp start")
                 (#xeeeeee 255 "grayscale ramp end")))
      (destructuring-bind (expected idx desc) c
        (declare (ignore desc))
        (expect (= expected (cl-tmux/terminal/parser::%xterm-palette-rgb idx)))))
    (expect (null (cl-tmux/terminal/parser::%xterm-palette-rgb 256))))

  ;; OSC 4 ; 196 ; ? reports palette index 196 as pure red (rgb:ffff/0000/0000).
  (it "osc-4-query-reports-palette-colour"
    (with-screen (s 20 5)
      (%feed-osc s "4;196;?")
      (expect (string= (format nil "~C]4;196;rgb:ffff/0000/0000~C\\" #\Escape #\Escape)
                       (first (cl-tmux/terminal/types:screen-response-queue s))))))

  ;; OSC 4 ; 0 ; ? ; 15 ; ? enqueues one reply per queried index.
  (it "osc-4-query-multiple-indices"
    (with-screen (s 20 5)
      (%feed-osc s "4;0;?;15;?")
      (expect (= 2 (length (cl-tmux/terminal/types:screen-response-queue s))))))

  ;; OSC 8 ; ; URI sets the current hyperlink so the next written cell carries it;
  ;; OSC 8 ; ; clears it (the following cell has no hyperlink).
  (it "osc-8-stamps-cell-hyperlink"
    (with-screen (s 20 5)
      (%feed-osc s "8;;https://example.com")
      (feed s "X")
      (%feed-osc s "8;;")        ; clear the hyperlink
      (feed s "Y")
      (expect (string= "https://example.com"
                       (cl-tmux/terminal/types:cell-hyperlink
                        (cl-tmux/terminal/types:screen-cell s 0 0))))
      (expect (null (cl-tmux/terminal/types:cell-hyperlink
                     (cl-tmux/terminal/types:screen-cell s 1 0))))))

  ;; An OSC sequence terminated by BEL is consumed without crashing.
  (it "osc-bel-no-crash"
    (with-screen (s 10 2)
      (feed s "a")
      ;; OSC 0 ; title BEL -- common in xterm
      (screen-process-bytes s
        (babel:string-to-octets
          (format nil "~C]0;window title~C" #\Escape (code-char 7))
          :encoding :utf-8))
      (feed s "b")
      (expect (char= #\a (char-at s 0 0)))
      (expect (char= #\b (char-at s 1 0)))))

  ;; An OSC sequence terminated by ESC \ (ST) is consumed without crashing.
  (it "osc-st-ignored"
    (with-screen (s 10 2)
      (feed s "a")
      ;; OSC terminated by ST = ESC \
      (screen-process-bytes s
        (babel:string-to-octets
          (format nil "~C]0;title~C\\" #\Escape #\Escape)
          :encoding :utf-8))
      (feed s "b")
      (expect (char= #\a (char-at s 0 0)))
      (expect (char= #\b (char-at s 1 0))))))
