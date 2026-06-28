(in-package #:cl-tmux)

;;;; customize-mode tree browser.
;;; tmux's customize-mode opens an interactive tree of every option / hook / key
;;; binding for editing in place.  cl-tmux renders it as a read-only customize
;;; tree overlay — the same depth as :choose-tree / :list-keys (the other "mode"
;;; commands here are informational overlays, not j/k-navigable panes); values
;;; are changed with set-option / bind.  The grouping (Server / Session+Window
;;; Options / Key Bindings) mirrors tmux's customize-mode categories so the same
;;; mental model and the same names appear.

(defun %customize-match-p (str filter)
  "T when FILTER is NIL or a case-insensitive substring of STR.  Used to filter
   the customize tree by customize-mode's -f option."
  (or (null filter)
      (search (string-downcase filter) (string-downcase str))))

(defun %customize-split-lines (text)
  "Split TEXT into a list of its lines (newline-separated), dropping the trailing
   empty line.  Used to filter pre-rendered multi-line blocks (key bindings)."
  (with-input-from-string (in text)
    (loop for line = (read-line in nil nil) while line collect line)))

(defun %customize-value-string (value)
  "Render an option VALUE for the customize tree: T->on, NIL->off, strings as-is,
   everything else via princ (mirrors cl-tmux/options' show-options formatter)."
  (cond ((eq value t) "on")
        ((null value) "off")
        ((stringp value) value)
        (t (princ-to-string value))))

(defun %customize-emit-options (s title ht-pairs &optional filter)
  "Write one customize-tree section to S, preserving the existing name sort and
   filtering behavior for options and hooks."
  (let ((shown (sort (remove-if-not
                      (lambda (p) (%customize-match-p (car p) filter))
                      ht-pairs)
                     #'string< :key #'car)))
    (when shown
      (format s "~A:~%" title)
      (dolist (p shown)
        (format s "  ~A: ~A~%"
                (car p) (%customize-value-string (cdr p)))))))

(defun %format-customize-tree (&optional filter)
  "Render the customize tree as an overlay string: Server Options, then
   Session/Window Options (each `  name: value`, name-sorted), then Key Bindings,
   restricted to entries matching FILTER (substring, case-insensitive).  A group
   with no surviving entries is omitted entirely."
  (with-output-to-string (s)
    (let (server-pairs)
      (maphash (lambda (k v) (push (cons k v) server-pairs))
               cl-tmux/options::*server-options*)
      (%customize-emit-options s "Server Options" server-pairs filter))
    (%customize-emit-options s "Session/Window Options"
                             (cl-tmux/options:all-options) filter)
    ;; Key bindings: filter the pre-rendered describe-key-bindings block by line.
    (let ((lines (remove-if
                  (lambda (l) (or (string= l "")
                                  (not (%customize-match-p l filter))))
                  (%customize-split-lines (cl-tmux/config:describe-key-bindings)))))
      (when lines
        (format s "Key Bindings:~%")
        (dolist (l lines) (format s "  ~A~%" l))))))

(defun %cmd-customize-mode (session args)
  "customize-mode [-NZ] [-f filter] [-F format] [-t target]: show the customize
   tree (options + key bindings) in an overlay.  -f FILTER limits the tree to
   entries whose name/line contains FILTER (case-insensitive substring).
   -N (no preview), -Z (zoom), -F format and -t target are accepted; their
   arguments are consumed.  cl-tmux's tree is read-only; edit with set-option /
   bind."
  (declare (ignore session))
  (with-command-input (flags positionals args "fFt"
                       :allowed-flags '(#\f #\F #\N #\Z #\t)
                       :max-positionals 0
                       :message "customize-mode: unsupported argument")
    (show-overlay (%format-customize-tree (%flag-value flags #\f)))
    t))
