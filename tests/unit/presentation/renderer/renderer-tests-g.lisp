(in-package #:cl-tmux/test)

;;;; renderer tests — part G: direct unit tests for %dispatch-align-token,
;;;; %split-align-attr, %status-align-buckets, %status-bar-default-segments,
;;;; and %content-search-match-p flag matrix.
;;;;
;;;; These helpers previously had no direct unit tests — they were covered only
;;;; transitively through %compose-aligned-line and #{C:} integration tests.

(describe "renderer-suite"

  ;;; ── %split-align-attr ────────────────────────────────────────────────────────
  ;;;
  ;;; %split-align-attr parses the body of a #[…] status block into an align
  ;;; keyword (:left/:centre/:right or NIL) and the remaining non-align attrs as a
  ;;; re-joined comma string (NIL when none survive).  Combined blocks like
  ;;; #[align=right,fg=red] must keep their colour.

  ;; %split-align-attr returns :left for 'align=left' (and its short form 'align=l').
  (it "split-align-attr-left-keyword"
    (multiple-value-bind (align rest)
        (cl-tmux/renderer::%split-align-attr "align=left")
      (expect (eq :left align))
      (expect (null rest)))
    (multiple-value-bind (align rest)
        (cl-tmux/renderer::%split-align-attr "align=l")
      (expect (eq :left align))
      (expect (null rest))))

  ;; %split-align-attr returns :centre for align=centre / align=center / align=c.
  (it "split-align-attr-centre-keyword"
    (dolist (body '("align=centre" "align=center" "align=c"))
      (multiple-value-bind (align rest)
          (cl-tmux/renderer::%split-align-attr body)
        (expect (eq :centre align))
        (expect (null rest)))))

  ;; %split-align-attr returns :right for 'align=right' and 'align=r'.
  (it "split-align-attr-right-keyword"
    (dolist (body '("align=right" "align=r"))
      (multiple-value-bind (align rest)
          (cl-tmux/renderer::%split-align-attr body)
        (expect (eq :right align))
        (expect (null rest)))))

  ;; %split-align-attr with no align= token returns NIL align and the body as REST.
  (it "split-align-attr-no-align-returns-nil"
    (multiple-value-bind (align rest)
        (cl-tmux/renderer::%split-align-attr "fg=red")
      (expect (null align))
      (expect (string= "fg=red" rest))))

  ;; %split-align-attr with 'align=right,fg=red' returns :right and 'fg=red'.
  (it "split-align-attr-combined-block-preserves-colour"
    (multiple-value-bind (align rest)
        (cl-tmux/renderer::%split-align-attr "align=right,fg=red")
      (expect (eq :right align))
      (expect (string= "fg=red" rest))))

  ;; %split-align-attr keeps ALL non-align attrs joined by comma in REST.
  (it "split-align-attr-multiple-style-attrs-preserved"
    (multiple-value-bind (align rest)
        (cl-tmux/renderer::%split-align-attr "align=centre,fg=blue,bold")
      (expect (eq :centre align))
      (expect (search "fg=blue" rest))
      (expect (search "bold" rest))))

  ;; %split-align-attr with an empty body returns NIL align and NIL rest.
  (it "split-align-attr-empty-body"
    (multiple-value-bind (align rest)
        (cl-tmux/renderer::%split-align-attr "")
      (expect (null align))
      (expect (null rest))))

  ;;; ── %status-align-buckets ────────────────────────────────────────────────────
  ;;;
  ;;; %status-align-buckets splits a raw status format string into (values LEFT
  ;;; CENTRE RIGHT) sub-strings using #[align=…] markers.  Text before any marker
  ;;; is LEFT; markers switch the current bucket; colour attrs survive within each
  ;;; bucket.

  ;; %status-align-buckets with no #[align=…] puts everything in the left bucket.
  (it "status-align-buckets-no-markers-all-left"
    (multiple-value-bind (left centre right)
        (cl-tmux/renderer::%status-align-buckets "hello world")
      (expect (string= "hello world" left))
      (expect (string= "" centre))
      (expect (string= "" right))))

  ;; %status-align-buckets splits on #[align=centre] and #[align=right].
  (it "status-align-buckets-basic-three-way-split"
    (multiple-value-bind (left centre right)
        (cl-tmux/renderer::%status-align-buckets "L#[align=centre]C#[align=right]R")
      (expect (string= "L" left))
      (expect (string= "C" centre))
      (expect (string= "R" right))))

  ;; %status-align-buckets on an empty string returns three empty strings.
  (it "status-align-buckets-empty-string"
    (multiple-value-bind (left centre right)
        (cl-tmux/renderer::%status-align-buckets "")
      (expect (string= "" left))
      (expect (string= "" centre))
      (expect (string= "" right))))

  ;; A combined #[align=right,fg=red] block switches to RIGHT and emits #[fg=red] there.
  (it "status-align-buckets-combined-block-emits-style-prefix"
    (multiple-value-bind (left centre right)
        (cl-tmux/renderer::%status-align-buckets "L#[align=right,fg=red]R")
      (expect (string= "L" left))
      (expect (search "fg=red" right))
      (expect (string= "" centre))))

  ;; %status-align-buckets plain text with no markers: everything lands in LEFT.
  (it "status-align-buckets-text-only-in-left"
    (multiple-value-bind (left centre right)
        (cl-tmux/renderer::%status-align-buckets "abc def")
      (expect (string= "abc def" left))
      (expect (string= "" centre))
      (expect (string= "" right))))

  ;;; ── %content-search-match-p flag matrix ──────────────────────────────────────
  ;;;
  ;;; %content-search-match-p tests LINE against TERM using the four flag
  ;;; combinations: plain-glob / regex (regex-p) × case-sensitive / insensitive
  ;;; (ci-p).  Plain-glob wraps TERM as *TERM* (contains-with-globbing); regex
  ;;; scans the whole line.  These direct unit tests complement the #{C:} end-to-
  ;;; end integration tests in format-tests-e.lisp.

  ;; %content-search-match-p (glob, case-sensitive) matches when TERM is a substring.
  (it "content-search-match-p-plain-glob-match"
    (expect (cl-tmux/format::%content-search-match-p "foo" "foobar" nil nil) :to-be-truthy)
    (expect (cl-tmux/format::%content-search-match-p "bar" "foobar" nil nil) :to-be-truthy)
    (expect (cl-tmux/format::%content-search-match-p "baz" "foobar" nil nil) :to-be-falsy))

  ;; %content-search-match-p (glob, ci-p=NIL) is case-sensitive.
  (it "content-search-match-p-plain-glob-case-sensitive"
    (expect (cl-tmux/format::%content-search-match-p "FOO" "foobar" nil nil) :to-be-falsy))

  ;; %content-search-match-p (glob, ci-p=T) folds case on both pattern and line.
  (it "content-search-match-p-glob-case-insensitive"
    (expect (cl-tmux/format::%content-search-match-p "FOO" "foobar" nil t) :to-be-truthy)
    (expect (cl-tmux/format::%content-search-match-p "foo" "FOOBAR" nil t) :to-be-truthy))

  ;; %content-search-match-p (regex, case-sensitive) matches regex against the full line.
  (it "content-search-match-p-regex-match"
    (expect (cl-tmux/format::%content-search-match-p "b.r" "foobar" t nil) :to-be-truthy)
    (expect (cl-tmux/format::%content-search-match-p "b.r" "foobaz" t nil) :to-be-falsy))

  ;; %content-search-match-p (regex) respects ^ anchor — ^foo matches line start only.
  (it "content-search-match-p-regex-anchor"
    (expect (cl-tmux/format::%content-search-match-p "^foo" "foobar" t nil) :to-be-truthy)
    (expect (cl-tmux/format::%content-search-match-p "^bar" "foobar" t nil) :to-be-falsy))

  ;; %content-search-match-p (regex, ci-p=T) folds case for the regex scan.
  (it "content-search-match-p-regex-case-insensitive"
    (expect (cl-tmux/format::%content-search-match-p "FOO" "foobar" t t) :to-be-truthy))

  ;;; ── %status-bar-default-segments ─────────────────────────────────────────────
  ;;;
  ;;; %status-bar-default-segments returns (values LEFT RIGHT JUSTIFY) from live
  ;;; session state and options.  We exercise the main paths: default clock right,
  ;;; custom status-right, and the justify option.

  ;; %status-bar-default-segments returns exactly three values: left, right, justify.
  (it "status-bar-default-segments-returns-three-values"
    (with-empty-status-bar-options ("status-justify" "left")
      (let* ((sess (make-renderer-test-session 80 6))
             (ctx  (cl-tmux/format:format-context-from-session
                    sess
                    (cl-tmux/model:session-active-window sess)
                    (cl-tmux/model:session-active-pane sess))))
        (multiple-value-bind (left right justify)
            (cl-tmux/renderer::%status-bar-default-segments sess ctx "44;97")
          (expect (stringp left))
          (expect (stringp right))
          (expect (stringp justify))))))

  ;; %status-bar-default-segments returns the status-justify option as the third value.
  (it "status-bar-default-segments-justify-option-propagated"
    (with-empty-status-bar-options ("status-justify" "centre")
      (let* ((sess (make-renderer-test-session 80 6))
             (ctx  (cl-tmux/format:format-context-from-session
                    sess
                    (cl-tmux/model:session-active-window sess)
                    (cl-tmux/model:session-active-pane sess))))
        (multiple-value-bind (_left _right justify)
            (cl-tmux/renderer::%status-bar-default-segments sess ctx "44;97")
          (declare (ignore _left _right))
          (expect (string= "centre" justify))))))

  ;; %status-bar-default-segments reflects a custom status-right option in RIGHT.
  (it "status-bar-default-segments-custom-right-appears"
    (with-isolated-options ("status-right" "MY-RIGHT" "status-left" nil)
      (let* ((sess (make-renderer-test-session 80 6))
             (ctx  (cl-tmux/format:format-context-from-session
                    sess
                    (cl-tmux/model:session-active-window sess)
                    (cl-tmux/model:session-active-pane sess))))
        (multiple-value-bind (_left right _justify)
            (cl-tmux/renderer::%status-bar-default-segments sess ctx "44;97")
          (declare (ignore _left _justify))
          (expect (search "MY-RIGHT" right)))))))
