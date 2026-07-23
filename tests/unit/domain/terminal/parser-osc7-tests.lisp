(in-package #:cl-tmux/test)

;;;; parser tests - OSC 7 and percent decoding.

(describe "terminal-suite/osc7-cwd-coverage"

  ;;; ── OSC 7: current working directory (file://host/path) ─────────────────────

  ;; %osc7-path extracts the path from a file:// URL, with or without a host.
  (it "osc7-path-extraction"
    (flet ((p (s) (cl-tmux/terminal/parser::%osc7-path s)))
      (dolist (c '(("file://host/home/u" "/home/u"   "with host")
                   ("file:///home/u"     "/home/u"   "empty host")
                   ("file://host"        "/"         "host but no path -> /")
                   ("not-a-url"          "not-a-url" "non-file:// -> unchanged")))
        (destructuring-bind (input expected desc) c
          (declare (ignore desc))
          (expect (string= expected (p input)))))))

  ;; Feeding ESC ] 7 ; file://host/path BEL sets screen-cwd to the path.
  (it "osc7-sets-screen-cwd-end-to-end"
    (with-screen (s 20 5)
      (screen-process-bytes s
        (babel:string-to-octets
          (format nil "~C]7;file://myhost/home/user/project~C" #\Escape (code-char 7))
          :encoding :utf-8))
      (expect (string= "/home/user/project" (cl-tmux/terminal/types:screen-cwd s)))))

  ;; %percent-decode handles %20 spaces, UTF-8 multibyte, no-% passthrough, and an
  ;; incomplete trailing % (left literal).
  (it "percent-decode-cases"
    (flet ((d (s) (cl-tmux/terminal/parser::%percent-decode s)))
      (dolist (c '(("a%20b"     "a b" "%20 -> space")
                   ("abc"       "abc" "no % -> unchanged")
                   ("%2F"       "/"   "%2F -> /")
                   ("a%"        "a%"  "incomplete trailing % is literal")
                   ("a%zz"      "a%zz" "non-hex after % is literal")
                   ("%E2%9C%93" "✓"  "UTF-8 multibyte (U+2713) decodes")))
        (destructuring-bind (input expected desc) c
          (declare (ignore desc))
          (expect (string= expected (d input)))))))

  ;; OSC 7 paths are percent-decoded - e.g. macOS '/Application Support'.
  (it "osc7-path-percent-decoded"
    (dolist (c '(("file://host/My%20Docs"              "/My Docs")
                 ("file:///Library/Application%20Support" "/Library/Application Support")))
      (destructuring-bind (url expected) c
        (expect (string= expected (cl-tmux/terminal/parser::%osc7-path url))))))

  ;; screen-cwd is empty on a fresh screen (no OSC 7 reported yet).
  (it "screen-cwd-defaults-empty"
    (with-screen (s 20 5)
      (expect (string= "" (cl-tmux/terminal/types:screen-cwd s)))))

  ;;; ── Coverage gap: define-osc-rules macro ─────────────────────────────────────
  ;;;
  ;;; Audit finding: define-osc-rules was not tested as a macro in isolation.
  ;;; Symmetry with the define-state and define-dec-graphics-table assertions.

  ;; define-osc-rules is a defined macro in the parser package.
  (it "define-osc-rules-macro-is-defined"
    (expect (macro-function 'cl-tmux/terminal/parser::define-osc-rules))))
