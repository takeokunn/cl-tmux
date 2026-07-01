(in-package #:cl-tmux/format)

;;; -- Glob / regex matching and pane content search --------------------------
;;;
;;; Backs three related tmux format features:
;;;   #{m:pattern,string}   glob match       → %glob-match-p
;;;   #{m/r:pattern,string} regex match      → %regex-match-p
;;;   #{C:term}             pane content search → %format-content-search
;;;
;;; tmux's #{m:pattern,string} checks whether STRING matches PATTERN using Unix
;;; shell glob rules: * matches any sequence, ? matches any single char, and
;;; [...] matches a character class.  We implement the first two here
;;; (sufficient for 95% of real configs; [...] is left as a literal match).

(defun %glob-match-p (pattern string &key (start-p 0) (start-s 0))
  "Return T when STRING matches the shell glob PATTERN.
   Supported metacharacters: * (any sequence), ? (any one character).
   Case-sensitive.  Uses simple recursive backtracking."
  (let ((np (length pattern)) (ns (length string)))
    (loop
      (cond
        ((= start-p np) (return (= start-s ns)))
        ((char= (char pattern start-p) #\*)
         ;; Skip consecutive *s
         (loop while (and (< start-p np) (char= (char pattern start-p) #\*))
               do (incf start-p))
         (when (= start-p np) (return t))         ; trailing * matches rest
         ;; Try matching rest of pattern at each position in remaining string
         (when (loop for i from start-s to ns
                     thereis (%glob-match-p pattern string :start-p start-p :start-s i))
           (return t))
         (return nil))
        ((= start-s ns) (return nil))
        ((or (char= (char pattern start-p) #\?)
             (char= (char pattern start-p) (char string start-s)))
         (incf start-p) (incf start-s))
        (t (return nil))))))

(defun %regex-match-p (pattern string &optional ignore-case)
  "Return T when STRING matches the regular expression PATTERN (via cl-ppcre).
   IGNORE-CASE T compiles the pattern case-insensitively.  This backs the tmux
   #{m/r:pattern,string} match modifier.  A malformed PATTERN yields NIL (no
   match) rather than signaling — invalid regexes never break format expansion."
  (handler-case
      (let ((scanner (cl-ppcre:create-scanner pattern
                                              :case-insensitive-mode ignore-case)))
        (and (cl-ppcre:scan scanner string) t))
    (error () nil)))

(defun %pane-visible-lines (pane)
  "The visible (non-scrollback) rows of PANE's screen as a list of strings, top
   to bottom, with trailing spaces trimmed — the per-line content tmux's #{C:}
   search runs against.  Returns NIL when PANE has no live screen.  Read lazily
   (only when a #{C:} modifier actually fires), so non-search formats pay nothing."
  (let ((scr (and pane (cl-tmux/model:pane-screen pane))))
    (when scr
      (let ((w (cl-tmux/terminal:screen-width  scr))
            (h (cl-tmux/terminal:screen-height scr)))
        (loop for y below h
              collect (string-right-trim
                       '(#\Space)
                       (with-output-to-string (s)
                         (dotimes (x w)
                           (write-char (cl-tmux/terminal:cell-char
                                        (cl-tmux/terminal:screen-cell scr x y))
                                       s)))))))))

(defun %content-search-match-p (term line regex-p ci-p)
  "Does LINE match the #{C:} search TERM?  Mirrors tmux's window_pane_search:
   non-regex wraps TERM as the glob *TERM* and fnmatches the whole line (the
   stars turn %glob-match-p's anchored match into a contains-with-globbing
   search); regex scans LINE for TERM.  CI-P folds case on both branches."
  (if regex-p
      (%regex-match-p term line ci-p)
      (let ((pat (concatenate 'string "*" term "*")))
        (if ci-p
            (%glob-match-p (string-downcase pat) (string-downcase line))
            (%glob-match-p pat line)))))

(defun %format-content-search (mod rest context)
  "Evaluate a #{C[/r][/i]:term} content-search modifier.  TERM (REST) is first
   expanded as a format string, then matched against the visible content of the
   context pane line by line; returns the 1-based line number of the first match
   as a string, or \"0\" when there is no match (or no pane).  MOD is the modifier
   token (C, C/r, C/i, C/ri); r selects regex, i case-insensitivity — the same
   flag syntax as #{m/r:} and tmux's format_search."
  (let* ((term    (expand-format rest context))
         (regex-p (and (> (length mod) 1) (find #\r mod :start 1)))
         (ci-p    (and (> (length mod) 1) (find #\i mod :start 1)))
         (lines   (%pane-visible-lines (getf context :%c-search-pane))))
    (or (loop for line in lines
              for n from 1
              when (%content-search-match-p term line regex-p ci-p)
                do (return (format nil "~D" n)))
        "0")))
