(in-package #:cl-tmux)

;;;; Session/window/pane target resolution — the "-t session:window.pane" DSL.
;;;;
;;;; Architecture (data / logic separation):
;;;;   DATA  — parse-target splits a string into its three components
;;;;   LOGIC — find-*-by-target matches each component against the registry
;;;;   ENTRY — resolve-target is the single public entry point
;;;;
;;;; Target grammar:
;;;;   [SESSION][:WINDOW][.PANE]
;;;;     SESSION — session name prefix or $N (by id)
;;;;     WINDOW  — window name, @N (by id), or numeric index (0-based in list)
;;;;     PANE    — %N (by id) or numeric index (0-based in panes list)
;;;;   Any component may be absent; absent parts default to the current
;;;;   session/window/pane.

;;; ── Pure: parse a raw target string into three string components ─────────────

(defun %parse-session-component (target-string colon-pos dot-pos)
  "Derive the session component from TARGET-STRING given split positions.
   When a colon is present, the session is the text before it (possibly empty).
   When no colon is present, the session is the text before the dot (or the
   whole string when no dot is present either).
   Returns a non-empty string or NIL."
  (flet ((non-empty (s) (when (and s (plusp (length s))) s)))
    (if colon-pos
        (non-empty (subseq target-string 0 colon-pos))
        (non-empty (if dot-pos
                       (subseq target-string 0 dot-pos)
                       target-string)))))

(defun %parse-target (target-string)
  "Split TARGET-STRING into (values session-str window-str pane-str).
   Each value is NIL when the component is absent.
   Grammar: [SESSION][:WINDOW][.PANE]
   The SESSION portion is everything up to the first colon (if any).
   The WINDOW portion is between the first colon and the first dot (if any).
   The PANE portion is everything after the first dot."
  (when (or (null target-string) (string= target-string ""))
    (return-from %parse-target (values nil nil nil)))
  (let* ((colon-pos (position #\: target-string))
         (dot-pos   (position #\. target-string :start (or colon-pos 0))))
    (flet ((non-empty (s) (when (and s (plusp (length s))) s)))
      (let* ((win-raw  (cond
                         ;; Both colon and dot: window is between them.
                         ((and colon-pos dot-pos)
                          (subseq target-string (1+ colon-pos) dot-pos))
                         ;; Colon but no dot: window is everything after colon.
                         (colon-pos
                          (subseq target-string (1+ colon-pos)))
                         ;; No colon — no window component.
                         (t nil)))
             (pane-raw (when dot-pos
                         (subseq target-string (1+ dot-pos))))
             (sess-str (%parse-session-component target-string colon-pos dot-pos)))
        (values sess-str (non-empty win-raw) (non-empty pane-raw))))))

;;; ── define-target-lookup — Prolog-style sequential rule dispatch ─────────────
;;;
;;; Follows the same pattern as define-csi-rules / define-command-handlers.
;;; Each rule is a list: (test-expr) — return test-expr when it is non-NIL.
;;; The special rule (:nil-guard EXPR) exits early when EXPR is NIL.
;;; The generated dispatcher tries rules in order; returns NIL when none match.

(defmacro define-target-lookup (name lambda-list &rest rules)
  "Generate a target lookup function NAME with LAMBDA-LIST.
   An optional docstring may appear as the first element of RULES.
   Each remaining RULE is either:
     (:nil-guard EXPR)  -- return NIL early when EXPR is NIL
     (TEST-EXPR)        -- return TEST-EXPR when it is non-NIL
   Rules are tried in order.  Returns NIL when no rule matches."
  (let* ((docstring (when (stringp (first rules)) (first rules)))
         (actual-rules (if docstring (rest rules) rules)))
    `(defun ,name ,lambda-list
       ,@(when docstring (list docstring))
       ,@(mapcar
          (lambda (rule)
            (if (eq (car rule) :nil-guard)
                `(unless ,(cadr rule) (return-from ,name nil))
                `(let ((%result% ,(car rule)))
                   (when %result% (return-from ,name %result%)))))
          actual-rules)
       nil)))

;;; ── Sigil helpers (pure) ─────────────────────────────────────────────────────

(defun %sigil-id (target-str sigil-char)
  "If TARGET-STR starts with SIGIL-CHAR, parse the rest as an integer.
   Returns the integer or NIL."
  (when (and (plusp (length target-str))
             (char= (char target-str 0) sigil-char))
    (ignore-errors (parse-integer (subseq target-str 1)))))

(defun %name-prefix-p (prefix name)
  "T when NAME starts with PREFIX (both strings)."
  (and (>= (length name) (length prefix))
       (string= prefix name :end2 (min (length prefix) (length name)))))

;;; ── Find: session lookup ─────────────────────────────────────────────────────

(define-target-lookup find-session-by-target (server target-str)
  "Find a session in SERVER matching TARGET-STR.
   Rules: exact name, $N id, name prefix. Returns session or NIL."
  (:nil-guard target-str)
  ((cdr (assoc target-str server :test #'string=)))
  ((let ((id (%sigil-id target-str #\$)))
     (when id (find id (mapcar #'cdr server) :key #'session-id))))
  ((loop for (name . sess) in server
         when (and (stringp name) (plusp (length name))
                   (%name-prefix-p target-str name))
           return sess)))

;;; ── Find: window lookup ──────────────────────────────────────────────────────

(define-target-lookup find-window-by-target (session target-str)
  "Find a window in SESSION matching TARGET-STR.
   Rules: exact name, @N id, numeric index, name prefix. Returns window or NIL."
  (:nil-guard (and session target-str))
  ((find target-str (session-windows session)
         :key #'window-name :test #'string=))
  ((let ((id (%sigil-id target-str #\@)))
     (when id (find id (session-windows session) :key #'window-id))))
  ((let* ((wins (session-windows session))
          (idx  (ignore-errors (parse-integer target-str))))
     (when (and idx (>= idx 0) (< idx (length wins)))
       (nth idx wins))))
  ((let ((wins (session-windows session)))
     (loop for w in wins
           when (%name-prefix-p target-str (window-name w))
             return w))))

;;; ── Find: pane lookup ────────────────────────────────────────────────────────

(define-target-lookup find-pane-by-target (window target-str)
  "Find a pane in WINDOW matching TARGET-STR.
   Rules: %N id, numeric index. Returns pane or NIL."
  (:nil-guard (and window target-str))
  ((let ((id (%sigil-id target-str #\%)))
     (when id (find id (window-panes window) :key #'pane-id))))
  ((let* ((panes (window-panes window))
          (idx   (ignore-errors (parse-integer target-str))))
     (when (and idx (>= idx 0) (< idx (length panes)))
       (nth idx panes)))))

;;; ── Public: resolve-target ───────────────────────────────────────────────────

(defun resolve-target (server target-string &key current-session current-window current-pane)
  "Parse TARGET-STRING and resolve it to (values session window pane).
   SERVER is the *server-sessions* alist used for session lookup.
   CURRENT-SESSION / CURRENT-WINDOW / CURRENT-PANE are the defaults when
   a component is absent from TARGET-STRING or cannot be resolved."
  (multiple-value-bind (sess-str win-str pane-str)
      (%parse-target target-string)
    (let* ((session (or (when sess-str (find-session-by-target server sess-str))
                        current-session))
           (window  (or (when win-str (find-window-by-target session win-str))
                        (when (null win-str) current-window)
                        (session-active-window session)))
           (pane    (or (when pane-str (find-pane-by-target window pane-str))
                        (when (null pane-str) current-pane)
                        (when window (window-active-pane window)))))
      (values session window pane))))
