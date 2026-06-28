(in-package #:cl-tmux/test)

;;;; renderer tests — part G: direct unit tests for %dispatch-align-token,
;;;; %split-align-attr, %status-align-buckets, %status-bar-default-segments,
;;;; and %content-search-match-p flag matrix.
;;;;
;;;; These helpers previously had no direct unit tests — they were covered only
;;;; transitively through %compose-aligned-line and #{C:} integration tests.

(in-suite renderer-suite)

;;; ── %split-align-attr ────────────────────────────────────────────────────────
;;;
;;; %split-align-attr parses the body of a #[…] status block into an align
;;; keyword (:left/:centre/:right or NIL) and the remaining non-align attrs as a
;;; re-joined comma string (NIL when none survive).  Combined blocks like
;;; #[align=right,fg=red] must keep their colour.

(test split-align-attr-left-keyword
  "%split-align-attr returns :left for 'align=left' (and its short form 'align=l')."
  (multiple-value-bind (align rest)
      (cl-tmux/renderer::%split-align-attr "align=left")
    (is (eq :left align) "align=left → :left (got ~S)" align)
    (is (null rest) "no remaining attrs (got ~S)" rest))
  (multiple-value-bind (align rest)
      (cl-tmux/renderer::%split-align-attr "align=l")
    (is (eq :left align) "align=l → :left (got ~S)" align)
    (is (null rest) "no remaining attrs (got ~S)" rest)))

(test split-align-attr-centre-keyword
  "%split-align-attr returns :centre for align=centre / align=center / align=c."
  (dolist (body '("align=centre" "align=center" "align=c"))
    (multiple-value-bind (align rest)
        (cl-tmux/renderer::%split-align-attr body)
      (is (eq :centre align) "~S → :centre (got ~S)" body align)
      (is (null rest) "no remaining attrs from ~S (got ~S)" body rest))))

(test split-align-attr-right-keyword
  "%split-align-attr returns :right for 'align=right' and 'align=r'."
  (dolist (body '("align=right" "align=r"))
    (multiple-value-bind (align rest)
        (cl-tmux/renderer::%split-align-attr body)
      (is (eq :right align) "~S → :right (got ~S)" body align)
      (is (null rest) "no remaining attrs from ~S (got ~S)" body rest))))

(test split-align-attr-no-align-returns-nil
  "%split-align-attr with no align= token returns NIL align and the body as REST."
  (multiple-value-bind (align rest)
      (cl-tmux/renderer::%split-align-attr "fg=red")
    (is (null align) "no align= token → NIL align (got ~S)" align)
    (is (string= "fg=red" rest) "non-align attr must survive in REST (got ~S)" rest)))

(test split-align-attr-combined-block-preserves-colour
  "%split-align-attr with 'align=right,fg=red' returns :right and 'fg=red'."
  (multiple-value-bind (align rest)
      (cl-tmux/renderer::%split-align-attr "align=right,fg=red")
    (is (eq :right align) "align extracted as :right (got ~S)" align)
    (is (string= "fg=red" rest) "fg=red survives as REST (got ~S)" rest)))

(test split-align-attr-multiple-style-attrs-preserved
  "%split-align-attr keeps ALL non-align attrs joined by comma in REST."
  (multiple-value-bind (align rest)
      (cl-tmux/renderer::%split-align-attr "align=centre,fg=blue,bold")
    (is (eq :centre align) "align=centre → :centre (got ~S)" align)
    (is (search "fg=blue" rest) "fg=blue must appear in REST (got ~S)" rest)
    (is (search "bold" rest) "bold must appear in REST (got ~S)" rest)))

(test split-align-attr-empty-body
  "%split-align-attr with an empty body returns NIL align and NIL rest."
  (multiple-value-bind (align rest)
      (cl-tmux/renderer::%split-align-attr "")
    (is (null align) "empty body → NIL align (got ~S)" align)
    (is (null rest) "empty body → NIL rest (got ~S)" rest)))

;;; ── %status-align-buckets ────────────────────────────────────────────────────
;;;
;;; %status-align-buckets splits a raw status format string into (values LEFT
;;; CENTRE RIGHT) sub-strings using #[align=…] markers.  Text before any marker
;;; is LEFT; markers switch the current bucket; colour attrs survive within each
;;; bucket.

(test status-align-buckets-no-markers-all-left
  "%status-align-buckets with no #[align=…] puts everything in the left bucket."
  (multiple-value-bind (left centre right)
      (cl-tmux/renderer::%status-align-buckets "hello world")
    (is (string= "hello world" left)
        "all text goes to LEFT when no align markers (got ~S)" left)
    (is (string= "" centre) "CENTRE must be empty (got ~S)" centre)
    (is (string= "" right)  "RIGHT must be empty (got ~S)" right)))

(test status-align-buckets-basic-three-way-split
  "%status-align-buckets splits on #[align=centre] and #[align=right]."
  (multiple-value-bind (left centre right)
      (cl-tmux/renderer::%status-align-buckets "L#[align=centre]C#[align=right]R")
    (is (string= "L" left)   "left bucket (got ~S)" left)
    (is (string= "C" centre) "centre bucket (got ~S)" centre)
    (is (string= "R" right)  "right bucket (got ~S)" right)))

(test status-align-buckets-empty-string
  "%status-align-buckets on an empty string returns three empty strings."
  (multiple-value-bind (left centre right)
      (cl-tmux/renderer::%status-align-buckets "")
    (is (string= "" left)   "LEFT must be empty (got ~S)" left)
    (is (string= "" centre) "CENTRE must be empty (got ~S)" centre)
    (is (string= "" right)  "RIGHT must be empty (got ~S)" right)))

(test status-align-buckets-combined-block-emits-style-prefix
  "A combined #[align=right,fg=red] block switches to RIGHT and emits #[fg=red] there."
  (multiple-value-bind (left centre right)
      (cl-tmux/renderer::%status-align-buckets "L#[align=right,fg=red]R")
    (is (string= "L" left)
        "left text must be 'L' (got ~S)" left)
    (is (search "fg=red" right)
        "colour attr must survive into right bucket (got ~S)" right)
    (is (string= "" centre) "centre must be empty (got ~S)" centre)))

(test status-align-buckets-text-only-in-left
  "%status-align-buckets plain text with no markers: everything lands in LEFT."
  (multiple-value-bind (left centre right)
      (cl-tmux/renderer::%status-align-buckets "abc def")
    (is (string= "abc def" left) "got ~S" left)
    (is (string= "" centre) "got ~S" centre)
    (is (string= "" right) "got ~S" right)))

;;; ── %content-search-match-p flag matrix ──────────────────────────────────────
;;;
;;; %content-search-match-p tests LINE against TERM using the four flag
;;; combinations: plain-glob / regex (regex-p) × case-sensitive / insensitive
;;; (ci-p).  Plain-glob wraps TERM as *TERM* (contains-with-globbing); regex
;;; scans the whole line.  These direct unit tests complement the #{C:} end-to-
;;; end integration tests in format-tests-e.lisp.

(test content-search-match-p-plain-glob-match
  "%content-search-match-p (glob, case-sensitive) matches when TERM is a substring."
  (is-true  (cl-tmux/format::%content-search-match-p "foo" "foobar" nil nil)
            "term 'foo' is contained in 'foobar'")
  (is-true  (cl-tmux/format::%content-search-match-p "bar" "foobar" nil nil)
            "term 'bar' is contained in 'foobar'")
  (is-false (cl-tmux/format::%content-search-match-p "baz" "foobar" nil nil)
            "term 'baz' is not in 'foobar'"))

(test content-search-match-p-plain-glob-case-sensitive
  "%content-search-match-p (glob, ci-p=NIL) is case-sensitive."
  (is-false (cl-tmux/format::%content-search-match-p "FOO" "foobar" nil nil)
            "upper-case FOO must not match lowercase 'foobar' when case-sensitive"))

(test content-search-match-p-glob-case-insensitive
  "%content-search-match-p (glob, ci-p=T) folds case on both pattern and line."
  (is-true  (cl-tmux/format::%content-search-match-p "FOO" "foobar" nil t)
            "FOO with ci-p=T must match 'foobar'")
  (is-true  (cl-tmux/format::%content-search-match-p "foo" "FOOBAR" nil t)
            "foo with ci-p=T must match 'FOOBAR'"))

(test content-search-match-p-regex-match
  "%content-search-match-p (regex, case-sensitive) matches regex against the full line."
  (is-true  (cl-tmux/format::%content-search-match-p "b.r" "foobar" t nil)
            "regex b.r matches 'foobar'")
  (is-false (cl-tmux/format::%content-search-match-p "b.r" "foobaz" t nil)
            "regex b.r does not match 'foobaz'"))

(test content-search-match-p-regex-anchor
  "%content-search-match-p (regex) respects ^ anchor — ^foo matches line start only."
  (is-true  (cl-tmux/format::%content-search-match-p "^foo" "foobar" t nil)
            "^foo matches 'foobar' (starts with foo)")
  (is-false (cl-tmux/format::%content-search-match-p "^bar" "foobar" t nil)
            "^bar does not match 'foobar' (bar is not at start)"))

(test content-search-match-p-regex-case-insensitive
  "%content-search-match-p (regex, ci-p=T) folds case for the regex scan."
  (is-true  (cl-tmux/format::%content-search-match-p "FOO" "foobar" t t)
            "regex FOO with ci-p=T matches 'foobar'"))

;;; ── %status-bar-default-segments ─────────────────────────────────────────────
;;;
;;; %status-bar-default-segments returns (values LEFT RIGHT JUSTIFY) from live
;;; session state and options.  We exercise the main paths: default clock right,
;;; custom status-right, and the justify option.

(test status-bar-default-segments-returns-three-values
  "%status-bar-default-segments returns exactly three values: left, right, justify."
  (with-isolated-options ("status-left" nil "status-right" nil "status-justify" "left")
    (let* ((sess (make-test-session 80 6))
           (ctx  (cl-tmux/format:format-context-from-session
                  sess
                  (cl-tmux/model:session-active-window sess)
                  (cl-tmux/model:session-active-pane sess))))
      (multiple-value-bind (left right justify)
          (cl-tmux/renderer::%status-bar-default-segments sess ctx "44;97")
        (is (stringp left)   "LEFT must be a string (got ~S)" left)
        (is (stringp right)  "RIGHT must be a string (got ~S)" right)
        (is (stringp justify) "JUSTIFY must be a string (got ~S)" justify)))))

(test status-bar-default-segments-justify-option-propagated
  "%status-bar-default-segments returns the status-justify option as the third value."
  (with-isolated-options ("status-justify" "centre" "status-left" nil "status-right" nil)
    (let* ((sess (make-test-session 80 6))
           (ctx  (cl-tmux/format:format-context-from-session
                  sess
                  (cl-tmux/model:session-active-window sess)
                  (cl-tmux/model:session-active-pane sess))))
      (multiple-value-bind (_left _right justify)
          (cl-tmux/renderer::%status-bar-default-segments sess ctx "44;97")
        (declare (ignore _left _right))
        (is (string= "centre" justify)
            "status-justify 'centre' must propagate as JUSTIFY (got ~S)" justify)))))

(test status-bar-default-segments-custom-right-appears
  "%status-bar-default-segments reflects a custom status-right option in RIGHT."
  (with-isolated-options ("status-right" "MY-RIGHT" "status-left" nil)
    (let* ((sess (make-test-session 80 6))
           (ctx  (cl-tmux/format:format-context-from-session
                  sess
                  (cl-tmux/model:session-active-window sess)
                  (cl-tmux/model:session-active-pane sess))))
      (multiple-value-bind (_left right _justify)
          (cl-tmux/renderer::%status-bar-default-segments sess ctx "44;97")
        (declare (ignore _left _justify))
        (is (search "MY-RIGHT" right)
            "custom status-right must appear in RIGHT (got ~S)" right)))))
