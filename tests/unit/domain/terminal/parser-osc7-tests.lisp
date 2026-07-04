(in-package #:cl-tmux/test)

;;;; parser tests - OSC 7 and percent decoding.

(def-suite osc7-cwd-coverage
  :description "OSC 7 working-directory parsing and percent-decode coverage"
  :in terminal-suite)
(in-suite osc7-cwd-coverage)

;;; ── OSC 7: current working directory (file://host/path) ─────────────────────

(test osc7-path-extraction
  "%osc7-path extracts the path from a file:// URL, with or without a host."
  (flet ((p (s) (cl-tmux/terminal/parser::%osc7-path s)))
    (dolist (c '(("file://host/home/u" "/home/u"   "with host")
                 ("file:///home/u"     "/home/u"   "empty host")
                 ("file://host"        "/"         "host but no path -> /")
                 ("not-a-url"          "not-a-url" "non-file:// -> unchanged")))
      (destructuring-bind (input expected desc) c
        (is (string= expected (p input)) "~A" desc)))))

(test osc7-sets-screen-cwd-end-to-end
  "Feeding ESC ] 7 ; file://host/path BEL sets screen-cwd to the path."
  (with-screen (s 20 5)
    (screen-process-bytes s
      (babel:string-to-octets
        (format nil "~C]7;file://myhost/home/user/project~C" #\Escape (code-char 7))
        :encoding :utf-8))
    (is (string= "/home/user/project" (cl-tmux/terminal/types:screen-cwd s))
        "screen-cwd must be the OSC 7 path after the sequence (got ~S)"
        (cl-tmux/terminal/types:screen-cwd s))))

(test percent-decode-cases
  "%percent-decode handles %20 spaces, UTF-8 multibyte, no-% passthrough, and an
   incomplete trailing % (left literal)."
  (flet ((d (s) (cl-tmux/terminal/parser::%percent-decode s)))
    (dolist (c '(("a%20b"     "a b" "%20 -> space")
                 ("abc"       "abc" "no % -> unchanged")
                 ("%2F"       "/"   "%2F -> /")
                 ("a%"        "a%"  "incomplete trailing % is literal")
                 ("a%zz"      "a%zz" "non-hex after % is literal")
                 ("%E2%9C%93" "✓"  "UTF-8 multibyte (U+2713) decodes")))
      (destructuring-bind (input expected desc) c
        (is (string= expected (d input)) "~A" desc)))))

(test osc7-path-percent-decoded
  "OSC 7 paths are percent-decoded - e.g. macOS '/Application Support'."
  (dolist (c '(("file://host/My%20Docs"              "/My Docs")
               ("file:///Library/Application%20Support" "/Library/Application Support")))
    (destructuring-bind (url expected) c
      (is (string= expected (cl-tmux/terminal/parser::%osc7-path url))
          "~S" url))))

(test screen-cwd-defaults-empty
  "screen-cwd is empty on a fresh screen (no OSC 7 reported yet)."
  (with-screen (s 20 5)
    (is (string= "" (cl-tmux/terminal/types:screen-cwd s))
        "a fresh screen has no reported cwd")))

;;; ── Coverage gap: define-osc-rules macro ─────────────────────────────────────
;;;
;;; Audit finding: define-osc-rules was not tested as a macro in isolation.
;;; Symmetry with the define-state and define-dec-graphics-table assertions.

(test define-osc-rules-macro-is-defined
  "define-osc-rules is a defined macro in the parser package."
  (is (macro-function 'cl-tmux/terminal/parser::define-osc-rules)
      "define-osc-rules must be a macro"))
