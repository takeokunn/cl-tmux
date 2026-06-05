(in-package #:cl-tmux/format)

;;;; tmux-style format string expansion.
;;;;
;;;; expand-format TEMPLATE CONTEXT → string
;;;;
;;;; Architecture (data / logic separation):
;;;;   DATA   — define-format-shorthands table maps chars to context keys
;;;;   LOGIC  — %expand-step: one character → next index (CPS-like)
;;;;   EFFECT — expand-format: loop over %expand-step, collect output

;;; ── Pure data helpers ────────────────────────────────────────────────────────

(defun %lookup (context key)
  "Retrieve KEY from the plist CONTEXT, returning an empty string when absent."
  (let ((val (getf context key)))
    (if val (princ-to-string val) "")))

(defun %variable-to-keyword (name)
  "Convert a variable name string to a context keyword.
   Underscores → hyphens, then upcase and intern in the KEYWORD package."
  (intern (string-upcase (substitute #\- #\_ name)) :keyword))

(defun %truthy-p (str)
  "T when STR is truthy: non-empty, not \"0\", not \"false\"."
  (and (plusp (length str))
       (not (string= str "0"))
       (not (string-equal str "false"))))

(defun %split-conditional (content)
  "Split CONTENT (text after '?') into (values cond true-branch false-branch)."
  (let ((comma1 (position #\, content)))
    (if (null comma1)
        (values content "" "")
        (let* ((cond-str (subseq content 0 comma1))
               (rest     (subseq content (1+ comma1)))
               (comma2   (position #\, rest)))
          (if (null comma2)
              (values cond-str rest "")
              (values cond-str (subseq rest 0 comma2) (subseq rest (1+ comma2))))))))

;;; ── Shorthand character table (data layer) ───────────────────────────────────
;;;
;;; Prolog-like fact table — each row is one format shorthand:
;;;   format_char(#\S) :- lookup(:session-name).
;;;   format_char(#\I) :- lookup(:window-index).
;;;   format_char(#\W) :- lookup(:window-name).
;;;   format_char(#\P) :- lookup(:pane-index).
;;;   format_char(#\H) :- lookup(:hostname).
;;;   format_char(#\#) :- write(#\#).        -- literal hash

(defmacro define-format-shorthands (&rest specs)
  "Build %EXPAND-SHORTHAND from a declarative (char context-key) fact table.
   Returns T when CH is a known shorthand (so the caller can advance by 2),
   NIL when unknown."
  `(defun %expand-shorthand (ch context out)
     "Expand single-character shorthand CH to OUT via CONTEXT lookup.
      Returns T on match, NIL when CH is not a recognized shorthand."
     (case ch
       ,@(mapcar (lambda (spec)
                   (destructuring-bind (char key) spec
                     `(,char (write-string (%lookup context ,key) out) t)))
                 specs)
       (#\# (write-char #\# out) t)
       (otherwise nil))))

(define-format-shorthands
  (#\S :session-name)
  (#\I :window-index)
  (#\W :window-name)
  (#\P :pane-index)
  (#\H :hostname))

;;; ── Brace and bracket form handlers (logic layer) ────────────────────────────
;;;
;;; These return the NEXT index to process (CPS convention: each step tells
;;; the caller where to resume).

(defun %expand-brace (template start context out)
  "Expand #{...} content starting at START (just past the '{').
   Writes to OUT and returns the index just past the closing '}'.
   Emits '#' literally when no closing brace is found."
  (let ((close (position #\} template :start start)))
    (if (null close)
        (progn (write-char #\# out) (1- start))   ; no close: treat # literally
        (let ((content (subseq template start close)))
          (cond
            ;; #{?cond,true,false} — conditional
            ;; cond-str may be a context variable name ("window_active") or a
            ;; literal value ("1", "0", "").  Perform a context lookup first: if
            ;; the keyword key is present in the context plist, use the looked-up
            ;; value; otherwise treat cond-str as the literal condition.
            ;; This makes #{?window_active,YES,NO} work for real-world use-cases
            ;; while preserving #{?1,yes,no} / #{?0,yes,no} / #{?,yes,no}.
            ((and (plusp (length content)) (char= (char content 0) #\?))
             (multiple-value-bind (cond-str true-str false-str)
                 (%split-conditional (subseq content 1))
               (let* ((kw       (%variable-to-keyword cond-str))
                      (ctx-val  (getf context kw))
                      (resolved (if ctx-val
                                    (princ-to-string ctx-val)
                                    cond-str)))
                 (write-string (if (%truthy-p resolved) true-str false-str) out))))
            ;; #{variable} — context lookup
            (t (write-string (%lookup context (%variable-to-keyword content)) out)))
          (1+ close)))))

(defun %expand-bracket (template start out)
  "Consume #[attrs] style directive starting at START (just past the '[').
   In real tmux these become SGR sequences; here we pass them through literally
   so the renderer can recognise and convert them (or ignore them safely).
   Writes the full #[...] literally and returns the index just past ']'.
   Emits '#' literally when no closing bracket is found."
  (let ((close (position #\] template :start start)))
    (if (null close)
        (progn (write-char #\# out) (1- start))
        (progn
          (write-char #\# out) (write-char #\[ out)
          (write-string (subseq template start close) out)
          (write-char #\] out)
          (1+ close)))))

(defun %expand-paren (template start out)
  "Expand #(shell-cmd) starting at START (just past the '(').
   Runs the command via uiop:run-program and writes its stdout to OUT.
   Returns the index just past the closing ')'.
   On any error (no closing paren, command failure) returns safely without
   crashing: missing ')' emits '#' literally; command errors emit empty string."
  (let ((close (position #\) template :start start)))
    (if (null close)
        (progn (write-char #\# out) (1- start))
        (let ((cmd (subseq template start close)))
          (let ((result
                  (handler-case
                      (uiop:run-program (list "/bin/sh" "-c" cmd)
                                        :output :string
                                        :ignore-error-status t)
                    (error () ""))))
            ;; Strip a single trailing newline (shell commands usually add one)
            (write-string
             (if (and (plusp (length result))
                      (char= (char result (1- (length result))) #\Newline))
                 (subseq result 0 (1- (length result)))
                 result)
             out))
          (1+ close)))))

;;; ── CPS-style character processor ───────────────────────────────────────────
;;;
;;; %expand-step processes template[I] and returns the NEXT index.
;;; It is the kernel of the CPS loop in expand-format.
;;;
;;; Prolog reading of each cond clause:
;;;   expand_step(#,{,...}, ctx, out) :- expand_brace(ctx, out).
;;;   expand_step(#,[,...}, ctx, out) :- expand_bracket(out).
;;;   expand_step(#,(,...}, ctx, out) :- expand_paren(out).
;;;   expand_step(#,X,     ctx, out) :- shorthand(X, ctx, out).
;;;   expand_step(#,?,     _,   out) :- write(#), write(?).   % unknown
;;;   expand_step(Ch,      _,   out) :- write(Ch).            % plain char

(defun %expand-step (template i context out)
  "Process TEMPLATE[I] and return the index of the next character to process."
  (declare (type string template) (type fixnum i))
  (let ((ch (char template i)))
    (cond
      ;; Format specifier: '#' followed by another character
      ((and (char= ch #\#) (< (1+ i) (length template)))
       (let ((next (char template (1+ i))))
         (cond
           ((char= next #\{) (%expand-brace   template (+ i 2) context out))
           ((char= next #\[) (%expand-bracket template (+ i 2) out))
           ((char= next #\() (%expand-paren   template (+ i 2) out))
           ((%expand-shorthand next context out) (+ i 2))
           ;; Unknown specifier: emit both characters literally
           (t (write-char #\# out) (write-char next out) (+ i 2)))))
      ;; Plain character: pass through unchanged
      (t (write-char ch out) (+ i 1)))))

;;; ── Public entry point ───────────────────────────────────────────────────────

(defun expand-format (template context)
  "Expand TEMPLATE using CONTEXT (a plist of keyword→value pairs).
   Processes one character position at a time via %expand-step (CPS-like):
   each call returns the next index, making the loop a pure iteration over steps.

   Supported specifiers:  #S #I #W #P #H ##  #{var}  #{?c,t,f}  #[sgr]  #(cmd)"
  (with-output-to-string (out)
    (loop for i = 0 then (%expand-step template i context out)
          while (< i (length template)))))

;;; ── Context builder ─────────────────────────────────────────────────────────

(defun %current-time-string ()
  "Return HH:MM string from the system clock."
  (multiple-value-bind (sec min hour) (get-decoded-time)
    (declare (ignore sec))
    (format nil "~2,'0D:~2,'0D" hour min)))

(defun %short-hostname (h)
  "Return the hostname up to the first dot, or the full string if no dot."
  (subseq h 0 (or (position #\. h) (length h))))

(defun format-context-from-session (session window pane
                                    &key (client-width 0) (client-height 0)
                                         (client-tty ""))
  "Build a context plist for EXPAND-FORMAT from SESSION, WINDOW, and PANE.
   Any of SESSION, WINDOW, PANE may be NIL; missing slots default to safe
   empty values.

   Optional keyword arguments supply client dimensions and tty path:
     :CLIENT-WIDTH   — terminal width reported to the client (default 0)
     :CLIENT-HEIGHT  — terminal height reported to the client (default 0)
     :CLIENT-TTY     — path to the client tty device (default \"\")

   Keys returned:
     :session-name :window-index :window-name :window-count
     :window-active :window-flags :pane-index :pane-title
     :hostname :host :host-short :time
     :client-width :client-height :client-tty"
  ;; session-active-window is the session's current window — distinct from
  ;; the WINDOW argument which is the window whose context we are building.
  ;; Naming it explicitly avoids confusion when both appear in the same binding.
  (let* ((session-name    (if session (cl-tmux/model:session-name session) ""))
         (session-wins    (if session (cl-tmux/model:session-windows session) nil))
         (session-active-window (if session (cl-tmux/model:session-active-window session) nil))
         (window-count    (length session-wins))
         (window-index    (if (and window session-wins)
                              (let ((pos (position window session-wins)))
                                (if pos (1+ pos) 0))
                              0))
         (window-name     (if window (cl-tmux/model:window-name window) ""))
         (window-active   (if (and window session-active-window
                                   (eq window session-active-window)) "1" "0"))
         (window-flags    (if (and window session-active-window
                                   (eq window session-active-window)) "*" " "))
         (window-panes    (if window (cl-tmux/model:window-panes window) nil))
         (pane-index      (if (and pane window-panes)
                              (let ((pos (position pane window-panes)))
                                (if pos (1+ pos) 0))
                              0))
         ;; pane-title: prefer the explicit pane-title slot; fall back to the
         ;; screen-title set via OSC 0/2 when the pane has a live screen.
         (pane-title      (cond
                            ((null pane) "")
                            ((and (plusp (length (cl-tmux/model:pane-title pane))))
                             (cl-tmux/model:pane-title pane))
                            ((cl-tmux/model:pane-screen pane)
                             (cl-tmux/terminal:screen-title
                              (cl-tmux/model:pane-screen pane)))
                            (t "")))
         (hostname        (machine-instance))
         (time-str        (%current-time-string))
         (host-short      (%short-hostname hostname)))
    (list :session-name  session-name
          :window-index  window-index
          :window-name   window-name
          :window-count  window-count
          :window-active window-active
          :window-flags  window-flags
          :pane-index    pane-index
          :pane-title    pane-title
          :hostname      hostname
          :host          hostname
          :host-short    host-short
          :time          time-str
          :client-width  client-width
          :client-height client-height
          :client-tty    client-tty)))

(defun format-context-from-window (session window
                                   &key (client-width 0) (client-height 0)
                                        (client-tty ""))
  "Build a context plist for per-window format strings (e.g. window-status-format).
   Like FORMAT-CONTEXT-FROM-SESSION but specialised for a single window.
   Any argument may be NIL.

   Keys: :session-name :window-index :window-name :window-count
         :window-active :window-flags :pane-index :pane-title
         :hostname :time :host :host-short
         :client-width :client-height :client-tty"
  (format-context-from-session session window
                               (when window
                                 (first (cl-tmux/model:window-panes window)))
                               :client-width  client-width
                               :client-height client-height
                               :client-tty    client-tty))
