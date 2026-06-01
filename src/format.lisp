(in-package #:cl-tmux/format)

;;;; tmux-style format string expansion.
;;;;
;;;; expand-format TEMPLATE CONTEXT → string
;;;;
;;;; Supported format specifiers:
;;;;   #S   — session name       (context key :session-name)
;;;;   #I   — window index       (context key :window-index)
;;;;   #W   — window name        (context key :window-name)
;;;;   #P   — pane index         (context key :pane-index)
;;;;   #H   — hostname           (context key :hostname)
;;;;   #{variable}               — look up :VARIABLE in context
;;;;                               (underscores → hyphens, upcased to keyword)
;;;;   #{?cond,true-text,false-text} — conditional expansion
;;;;   #[attrs]                  — pass through SGR attribute string literally
;;;;   ##                        — literal #
;;;;   Unknown specifier: keep literally (##X → #X for known single-char, else verbatim)

;;; ── Internal helpers ────────────────────────────────────────────────────────

(defun %lookup (context key)
  "Retrieve KEY from the plist CONTEXT, returning an empty string when absent."
  (let ((val (getf context key)))
    (if val (princ-to-string val) "")))

(defun %variable-to-keyword (name)
  "Convert a format variable name string to a context keyword.
   Underscores are replaced by hyphens and the result is upcased."
  (intern (string-upcase (substitute #\- #\_ name)) :keyword))

(defun %truthy-p (str)
  "Return T when STR is considered truthy: non-empty, not \"0\", not \"false\"."
  (and (plusp (length str))
       (not (string= str "0"))
       (not (string-equal str "false"))))

(defun %split-conditional (content)
  "Split CONTENT (the text after '?') on the first comma.
   Returns (values cond-str true-str false-str).
   true-str and false-str are split on the second comma; false-str defaults to \"\"."
  ;; content is like: cond,true-branch,false-branch
  (let ((comma1 (position #\, content)))
    (if (null comma1)
        (values content "" "")
        (let* ((cond-str   (subseq content 0 comma1))
               (rest       (subseq content (1+ comma1)))
               (comma2     (position #\, rest)))
          (if (null comma2)
              (values cond-str rest "")
              (values cond-str
                      (subseq rest 0 comma2)
                      (subseq rest (1+ comma2))))))))

;;; ── Core expander ───────────────────────────────────────────────────────────

(defun expand-format (template context)
  "Expand TEMPLATE using CONTEXT (a plist of keyword→value pairs).

   Supported format specifiers:
     #S  session-name   #I  window-index   #W  window-name
     #P  pane-index     #H  hostname
     #{variable}          look up :VARIABLE in context
     #{?cond,true,false}  conditional
     #[attrs]             pass through SGR attribute string literally
     ##                   literal #
     Unknown specifier:   kept literally as the two-character sequence"
  (with-output-to-string (out)
    (let ((i 0)
          (len (length template)))
      (loop while (< i len) do
        (let ((ch (char template i)))
          (cond
            ;; Format specifier: starts with #
            ((and (char= ch #\#) (< (1+ i) len))
             (let ((next (char template (1+ i))))
               (cond
                 ;; ## → literal #
                 ((char= next #\#)
                  (write-char #\# out)
                  (incf i 2))

                 ;; #S → session-name
                 ((char= next #\S)
                  (write-string (%lookup context :session-name) out)
                  (incf i 2))

                 ;; #I → window-index
                 ((char= next #\I)
                  (write-string (%lookup context :window-index) out)
                  (incf i 2))

                 ;; #W → window-name
                 ((char= next #\W)
                  (write-string (%lookup context :window-name) out)
                  (incf i 2))

                 ;; #P → pane-index
                 ((char= next #\P)
                  (write-string (%lookup context :pane-index) out)
                  (incf i 2))

                 ;; #H → hostname
                 ((char= next #\H)
                  (write-string (%lookup context :hostname) out)
                  (incf i 2))

                 ;; #{...} → variable reference or conditional
                 ((char= next #\{)
                  (let ((close (position #\} template :start (+ i 2))))
                    (if (null close)
                        ;; No closing brace — emit literally
                        (progn (write-char #\# out)
                               (incf i 1))
                        (let ((content (subseq template (+ i 2) close)))
                          (cond
                            ;; #{?cond,true,false} — conditional
                            ((and (plusp (length content))
                                  (char= (char content 0) #\?))
                             (multiple-value-bind (cond-str true-str false-str)
                                 (%split-conditional (subseq content 1))
                               (write-string
                                (if (%truthy-p cond-str) true-str false-str)
                                out)))
                            ;; #{variable} — context lookup
                            (t
                             (let ((kw (%variable-to-keyword content)))
                               (write-string (%lookup context kw) out))))
                          (setf i (1+ close))))))

                 ;; #[attrs] → pass through SGR attribute string literally
                 ((char= next #\[)
                  (let ((close (position #\] template :start (+ i 2))))
                    (if (null close)
                        ;; No closing bracket — emit literally
                        (progn (write-char #\# out)
                               (incf i 1))
                        (progn
                          (write-char #\# out)
                          (write-char #\[ out)
                          (write-string (subseq template (+ i 2) close) out)
                          (write-char #\] out)
                          (setf i (1+ close))))))

                 ;; Unknown specifier — keep literally
                 (t
                  (write-char #\# out)
                  (write-char next out)
                  (incf i 2)))))

            ;; Plain character — pass through
            (t
             (write-char ch out)
             (incf i 1))))))))

;;; ── Context builder ─────────────────────────────────────────────────────────

(defun format-context-from-session (session window pane)
  "Build a context plist for EXPAND-FORMAT from SESSION, WINDOW, and PANE.
   Any of the three arguments may be NIL; missing slots default to sensible
   empty or zero values.

   Keys returned:
     :session-name  — session name string (or \"\")
     :window-index  — 1-based window position within the session (or 0)
     :window-name   — window name string (or \"\")
     :window-count  — total number of windows in the session (or 0)
     :pane-index    — 1-based pane position within the window (or 0)
     :hostname      — (machine-instance) result"
  (let* ((session-name  (if session
                            (cl-tmux/model:session-name session)
                            ""))
         (session-wins  (if session
                            (cl-tmux/model:session-windows session)
                            nil))
         (window-count  (length session-wins))
         (window-index  (if (and window session-wins)
                            (let ((pos (position window session-wins)))
                              (if pos (1+ pos) 0))
                            0))
         (window-name   (if window
                            (cl-tmux/model:window-name window)
                            ""))
         (window-panes  (if window
                            (cl-tmux/model:window-panes window)
                            nil))
         (pane-index    (if (and pane window-panes)
                            (let ((pos (position pane window-panes)))
                              (if pos (1+ pos) 0))
                            0))
         (hostname      (machine-instance)))
    (list :session-name  session-name
          :window-index  window-index
          :window-name   window-name
          :window-count  window-count
          :pane-index    pane-index
          :hostname      hostname)))
