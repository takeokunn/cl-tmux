(in-package #:cl-tmux/config)

;;; ── Config file parsing + directive processing ───────────────────────────
;;;
;;; This file depends on the key-table mutators defined in config.lisp
;;; (key-table-bind, key-table-unbind) and the mutable specials
;;; (*key-tables*, *default-shell*, *status-height*).

;;; ── Tokenizer phase helpers ──────────────────────────────────────────────

(defun %whitespace-p (ch)
  "True when CH is a configuration whitespace character (space or tab)."
  (or (char= ch #\Space) (char= ch #\Tab)))

;;; ── Tokenizer phase helpers ──────────────────────────────────────────────
;;;
;;; Each helper handles one tokenizer state and returns the updated character
;;; index.

(defun %tokenize-backslash-escape (line i len push-char)
  "Consume a backslash-escaped character starting at I.  Calls PUSH-CHAR on
   the escaped character.  Returns the new index past both characters."
  (let ((next (1+ i)))
    (if (< next len)
        (progn (funcall push-char (char line next))
               (+ next 1))
        (+ i 1))))

(defun %tokenize-double-quoted (line i len push-char)
  "Consume a double-quoted region beginning at I (the opening-quote position).
   Handles backslash escapes inside.  If no closing quote exists, treats the
   opening quote as a literal character.  Returns the new index."
  (let ((close-pos (position #\" line :start (1+ i))))
    (if (not close-pos)
        ;; No closing quote — treat the opening \" as a literal.
        (progn (funcall push-char (char line i))
               (1+ i))
        ;; Found a closing quote — process quoted content.
        (let ((j (1+ i)))            ; skip opening \"
          (loop while (and (< j len) (char/= (char line j) #\"))
                do (let ((quoted-char (char line j)))
                     (cond
                       ((and (char= quoted-char #\\) (< (1+ j) len))
                        (incf j)
                        (funcall push-char (char line j)))
                       (t
                        (funcall push-char quoted-char))))
                   (incf j))
          (when (< j len) (incf j))  ; skip closing \"
          j))))

(defun %tokenize-single-quoted (line i len push-char)
  "Consume a single-quoted region beginning at I.  No escapes inside.
   Returns the new index past the closing quote (or EOL if unmatched)."
  (let ((j (1+ i)))                  ; skip opening '
    (loop while (and (< j len) (char/= (char line j) #\'))
          do (funcall push-char (char line j))
             (incf j))
    (when (< j len) (incf j))        ; skip closing '
    j))

(defun %config-tokens (line)
  "Tokenize LINE into a list of strings, handling:
   - unquoted whitespace as delimiter
   - \"double quoted\" strings (spaces preserved, \\x escapes processed)
   - 'single quoted' strings (literal content, no escapes)
   - \\ (backslash) escaping of the next character outside quotes
   Returns a list of token strings."
  (let ((tokens   '())
        (current  (make-array 0 :element-type 'character :adjustable t :fill-pointer 0))
        (in-token nil)
        (len      (length line)))
    (flet ((push-char (ch)
             "Append CH to the in-progress token and mark it active.
              Shared by every character-class callback below so the
              append-and-flag dance is written once."
             (vector-push-extend ch current)
             (setf in-token t))
           (finish-token ()
             "Flush the in-progress token into TOKENS, when any is open."
             (when in-token
               (push (copy-seq current) tokens)
               (setf (fill-pointer current) 0
                     in-token nil))))
      (let ((i 0))
        (loop while (< i len) do
          (let ((ch (char line i)))
            (cond
              ((char= ch #\\)
               (setf i (%tokenize-backslash-escape line i len #'push-char)))
              ((char= ch #\")
               (setf i (%tokenize-double-quoted line i len #'push-char)
                     in-token t))
              ((char= ch #\')
               (setf i (%tokenize-single-quoted line i len #'push-char)
                     in-token t))
              ((char= ch #\;)
               ;; tmux cmd-parse: an unquoted, unescaped `;` is a command
               ;; separator even with no surrounding whitespace
               ;; (`set -g @a 1; set -g @b 2`), so it always lexes as its own
               ;; ";" token.  A literal `;` must be escaped (\;) or quoted —
               ;; both of those take the branches above and stay in-token.
               (finish-token)
               (push-char #\;)
               (finish-token)
               (incf i))
              ((%whitespace-p ch)
               (finish-token)
               (incf i))
              (t
               (push-char ch)
               (incf i))))))
      (finish-token))
    (nreverse tokens)))

(defun %parse-control-char (rest)
  "Map REST (the part after a \"C-\" prefix) to its control CHARACTER, or NIL
   when REST does not denote a single control-able key.
   C-a..C-z → ^A..^Z (1..26); C-Space / C-@ → NUL (0);
   C-[ C-\\ C-] C-^ C-_ → 27..31.  The control byte is (logand code #x1f)."
  (cond
    ((string-equal rest "Space") (code-char 0))
    ((= (length rest) 1)
     (let ((c (char-upcase (char rest 0))))
       (cond
         ((char= c #\@) (code-char 0))
         ((char<= #\A c #\Z) (code-char (logand (char-code c) +ctrl-mask+)))
         ((member c '(#\[ #\\ #\] #\^ #\_) :test #'char=)
          (code-char (logand (char-code c) +ctrl-mask+)))
         (t nil))))
    (t nil)))

(defun %canonicalize-multi-modifier-key (token)
  "When TOKEN is a chain of TWO OR MORE C-/M-/S- modifier prefixes (in any order
   or case) over a base key, return it with the modifiers re-emitted in canonical
   C-/M-/S- order (the order the event loop's %modifier-prefix produces), so
   `bind M-C-x` and `bind C-M-x` — or `bind S-C-Up` and `bind C-S-Up` — resolve to
   the same binding.  The base key is kept verbatim.  Returns NIL when TOKEN has
   fewer than two modifier prefixes (e.g. plain `C-x`, handled by the
   control-character branch) so the caller falls through."
  (let ((ctrl nil) (meta nil) (shift nil) (count 0) (i 0) (len (length token)))
    (loop while (and (<= (+ i 2) len) (char= (char token (1+ i)) #\-))
          for m = (char-upcase (char token i))
          do (case m
               (#\C (setf ctrl t))
               (#\M (setf meta t))
               (#\S (setf shift t))
               (otherwise (return)))
             (incf count)
             (incf i 2))
    (let ((base (subseq token i)))
      (when (and (>= count 2) (plusp (length base)))
        (concatenate 'string
                     (if ctrl "C-" "") (if meta "M-" "") (if shift "S-" "")
                     base)))))

(defun %parse-key-token (token)
  "Parse a bind-key key TOKEN into the key-table key.
   A single-character TOKEN denotes that character.  A \"C-<key>\" token denotes
   the corresponding control CHARACTER (C-a→^A, C-Space→NUL, ...) so that Ctrl
   bindings match the byte the event loop sees when the key is pressed (the loop
   looks keys up via (code-char byte)).  Any other multi-character token (named
   keys like F1, Up, Home, or modifier combos like M-x / C-Left that the event
   loop encodes as multi-byte sequences) is kept as the string itself, matching
   the key-table key format used by the lookup path."
  (cond
    ((= (length token) 1) (char token 0))
    ;; Two-or-more-modifier combos (C-M-x, M-C-Left, ...) are canonicalized to
    ;; C-/M-/S- order BEFORE the single C- control-character branch, so the
    ;; spelling order does not matter and matches what the event loop emits.
    ((%canonicalize-multi-modifier-key token))
    ((and (> (length token) 2)
          (char-equal (char token 0) #\C)
          (char= (char token 1) #\-))
     ;; "C-<key>": convert to the control char when single-key; otherwise (e.g.
     ;; "C-Left") fall back to the string for the deferred modifier-key path.
     (or (%parse-control-char (subseq token 2)) token))
    (t token)))

;;; ── Command-name registry ────────────────────────────────────────────────
;;;
;;; *bindable-commands*, *known-command-names*, %known-command-name-p, and
;;; %command-keyword
;;; have been extracted to config-commands.lisp to keep this tokenizer file
;;; focused on lexical analysis.  That file is loaded below after the key-
;;; parsing utilities it depends on are defined.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((root (or (ignore-errors (asdf:system-source-directory :cl-tmux))
                  *load-pathname*
                  *compile-file-pathname*)))
    (load (merge-pathnames #P"src/application/config/config-commands.lisp" root))))
