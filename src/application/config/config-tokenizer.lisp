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

(defun %config-token-finish (current tokens in-token)
  "Flush CURRENT into TOKENS when IN-TOKEN is true."
  (if in-token
      (values (cons (copy-seq current) tokens) nil)
      (values tokens in-token)))

(defun %config-token-append-char (current in-token ch)
  "Append CH to CURRENT and report the tokenizer as active."
  (vector-push-extend ch current)
  (values current t))

(defun %config-tokens (line)
  "Tokenize LINE into a list of strings, handling:
   - unquoted whitespace as delimiter
   - \"double quoted\" strings (spaces preserved, \\x escapes processed)
   - 'single quoted' strings (literal content, no escapes)
   - \\ (backslash) escaping of the next character outside quotes
   Returns a list of token strings."
  (let* ((tokens   '())
         (current  (make-array 0 :element-type 'character :adjustable t :fill-pointer 0))
         (in-token nil)
         (len      (length line)))
    (let ((i 0))
      (loop while (< i len) do
        (let ((ch (char line i)))
          (cond
            ((char= ch #\\)
             (setf i (%tokenize-backslash-escape line i len
                                                 (lambda (escaped-char)
                                                   (multiple-value-bind (next-current next-in-token)
                                                       (%config-token-append-char current in-token escaped-char)
                                                     (setf current next-current
                                                           in-token next-in-token))))))
            ((char= ch #\")
             (setf in-token t
                   i (%tokenize-double-quoted line i len
                                              (lambda (quoted-char)
                                                (multiple-value-bind (next-current next-in-token)
                                                    (%config-token-append-char current in-token quoted-char)
                                                  (setf current next-current
                                                        in-token next-in-token))))))
            ((char= ch #\')
             (setf in-token t
                   i (%tokenize-single-quoted line i len
                                              (lambda (quoted-char)
                                                (multiple-value-bind (next-current next-in-token)
                                                    (%config-token-append-char current in-token quoted-char)
                                                  (setf current next-current
                                                        in-token next-in-token))))))
            ((%whitespace-p ch)
             (multiple-value-setq (tokens in-token)
               (%config-token-finish current tokens in-token))
             (setf (fill-pointer current) 0)
             (incf i))
            (t
             (multiple-value-bind (next-current next-in-token)
                 (%config-token-append-char current in-token ch)
               (setf current next-current
                     in-token next-in-token))
             (incf i)))))
      (multiple-value-setq (tokens in-token)
        (%config-token-finish current tokens in-token))
      (setf (fill-pointer current) 0))
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

(defparameter *key-name-aliases*
  '(("PPage" . "PageUp")   ("PgUp" . "PageUp")
    ("NPage" . "PageDown") ("PgDn" . "PageDown")
    ("IC"    . "Insert")
    ("DC"    . "Delete"))
  "tmux navigation-key spellings that denote the same key as a canonical name.
   Both spellings must collapse to one string so the bind-side key and the
   event-loop's emitted key (see %csi-tilde-key-name) match in the key table.")

(defun %normalize-key-alias (token)
  "Return the canonical key name for TOKEN when it is a known alias (case-
   insensitively), else NIL.  Lets `bind -n PPage` and `bind -n PageUp` resolve
   to the same binding."
  (cdr (assoc token *key-name-aliases* :test #'string-equal)))

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
    ;; Navigation-key aliases → the canonical name the event loop emits for the
    ;; corresponding ESC [ N ~ sequence (see %csi-tilde-key-name).  Without this
    ;; `bind -n PPage <cmd>` would store "PPage" while the keypress resolves to
    ;; "PageUp", and the binding would never fire.
    ((%normalize-key-alias token))
    (t token)))

;;; ── Command-name registry ────────────────────────────────────────────────
;;;
;;; *bindable-commands*, *tmux-command-aliases*, *known-command-names*,
;;; %canonical-command-name, %known-command-name-p, and %command-keyword
;;; have been extracted to config-commands.lisp to keep this tokenizer file
;;; focused on lexical analysis.  That file is loaded below after the key-
;;; parsing utilities it depends on are defined.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((root (or (ignore-errors (asdf:system-source-directory :cl-tmux))
                  *load-pathname*
                  *compile-file-pathname*)))
    (load (merge-pathnames #P"src/application/config/config-commands.lisp" root))))
