(in-package #:cl-tmux/protocol)

;;;; +msg-command+ payload codec — NUL-delimited field encoding/decoding.
;;;;
;;;; This file is the pure, transport-agnostic codec for the command message
;;;; type.  It lives in the same package as protocol.lisp so all codec
;;;; primitives are co-located in cl-tmux/protocol.
;;;;
;;;; Payload format: NUL-delimited fields.
;;;;   [target NUL] command-keyword-name NUL [arg NUL ...]
;;;; When target is NIL the target field is omitted entirely.
;;;; The command keyword name is encoded without the leading colon.

(defconstant +field-delimiter+ 0
  "ASCII NUL byte used to separate fields in a +msg-command+ payload.
   Every field in the NUL-delimited encoding is terminated by this byte.")

;;; ── Target-sigil detection macro ─────────────────────────────────────────────
;;;
;;; define-target-sigils is a declarative table that drives the target-field-p
;;; predicate.  Each rule describes one detection policy:
;;;   (first-char CHAR)     — the field starts with CHAR (e.g. '$' for sessions)
;;;   (contains-char CHAR)  — the field contains CHAR anywhere (e.g. ':' or '.')
;;; Adding a new sigil never requires touching the function body.

(defmacro define-target-sigils (&rest rules)
  "Generate TARGET-FIELD-P from a declarative sigil/substring table.
   Each RULE is either (first-char CHAR) or (contains-char CHAR).
   Produces a DEFUN whose body is a flat OR over all rule tests."
  `(defun target-field-p (field)
     "Return true when FIELD looks like a tmux target rather than a command name.
   A field is a target when it starts with '$' (session sigil), contains ':'
   (session:window syntax), or contains '.' (window.pane syntax).
   This predicate is the sole policy point for target detection; keeping it
   separate from the NUL-field-splitting logic ensures that command names
   containing these characters are never misidentified."
     (and (plusp (length field))
          (or ,@(mapcar (lambda (rule)
                          (destructuring-bind (kind char) rule
                            (ecase kind
                              (first-char  `(char= (char field 0) ,char))
                              (contains-char `(find ,char field)))))
                        rules)))))

(define-target-sigils
  (first-char   #\$)
  (contains-char #\:)
  (contains-char #\.))

(defun command-name-to-string (command-name)
  "Convert COMMAND-NAME (keyword or string) to a lowercase string for wire encoding."
  (if (keywordp command-name)
      (string-downcase (symbol-name command-name))
      command-name))

(defun assemble-command-fields (name-str target args)
  "Build the ordered list of NUL-delimited field strings for a command payload.
   TARGET is prepended when non-NIL; ARGS are appended after NAME-STR."
  (append (when target (list target))
          (list name-str)
          args))

(defun encode-fields-to-buffer (field-octets)
  "Pack FIELD-OCTETS (a list of octet vectors) into a fresh buffer using
   CONCATENATE (no mutable writes).  Each field is followed by a
   +field-delimiter+ (NUL) byte; the total length equals the sum of all
   field lengths plus one delimiter per field."
  (let* ((field-count (length field-octets))
         (delimiters  (make-list field-count :initial-element (vector +field-delimiter+))))
    (apply #'concatenate
           '(simple-array (unsigned-byte 8) (*))
           (mapcan #'list field-octets delimiters))))

(defun encode-command-payload (command-name &key target args)
  "Encode a command message payload.
   COMMAND-NAME is a keyword or string naming the command.
   TARGET is an optional -t target string (NIL = current session).
   ARGS is an optional list of argument strings.
   Returns a fresh octet vector of NUL-delimited UTF-8 fields."
  (let* ((name-str      (command-name-to-string command-name))
         (field-strings (assemble-command-fields name-str target args))
         (field-octets  (mapcar (lambda (s)
                                  (babel:string-to-octets s :encoding :utf-8))
                                field-strings)))
    (encode-fields-to-buffer field-octets)))

(defun split-on-nul-bytes (octets)
  "Split OCTETS on NUL bytes and return a list of decoded UTF-8 strings.
   Each NUL-terminated region becomes one string; bytes after the final NUL
   (if any) are ignored.  Returns NIL for an empty or NUL-free input."
  (loop with start = 0
        for i from 0 below (length octets)
        when (zerop (aref octets i))
          collect (babel:octets-to-string octets :start start :end i :encoding :utf-8)
          and do (setf start (1+ i))))

(defun decode-command-payload (payload)
  "Decode a +msg-command+ PAYLOAD into (values command-keyword target args).
   COMMAND-KEYWORD is a keyword symbol of the command name.
   TARGET is a string or NIL when absent.
   ARGS is a list of argument strings (may be nil).
   The first NUL-delimited field is examined by TARGET-FIELD-P to determine
   whether it is a target or the command name; all remaining fields are args.
   Returns (values NIL NIL NIL) when the payload contains no NUL-terminated
   fields (empty or NUL-free input)."
  (let ((fields (split-on-nul-bytes (to-octets payload))))
    (cond
      ((null fields)
       (values nil nil nil))
      ((and (>= (length fields) 2)
            (target-field-p (first fields)))
       (values (intern (string-upcase (second fields)) :keyword)
               (first fields)
               (cddr fields)))
      (t
       (values (intern (string-upcase (first fields)) :keyword)
               nil
               (rest fields))))))
