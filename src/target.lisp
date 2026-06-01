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

(defun %parse-target (target-string)
  "Split TARGET-STRING into (values session-str window-str pane-str).
   Each value is NIL when the component is absent.
   Grammar: [SESSION][:WINDOW][.PANE]
   The SESSION portion is everything up to the first colon (if any).
   The WINDOW portion is between the first colon and the first dot (if any).
   The PANE portion is everything after the first dot."
  (if (or (null target-string) (string= target-string ""))
      (values nil nil nil)
      (let* ((colon-pos (position #\: target-string))
             (dot-pos   (position #\. target-string :start (or colon-pos 0)))
             (sess-str  (when colon-pos
                          (let ((s (subseq target-string 0 colon-pos)))
                            (when (plusp (length s)) s))))
             (win-str   (cond
                          ;; Both colon and dot: window is between them
                          ((and colon-pos dot-pos)
                           (let ((s (subseq target-string (1+ colon-pos) dot-pos)))
                             (when (plusp (length s)) s)))
                          ;; Colon but no dot: window is everything after colon
                          (colon-pos
                           (let ((s (subseq target-string (1+ colon-pos))))
                             (when (plusp (length s)) s)))
                          ;; No colon but there's a dot — no window component
                          (t nil)))
             (pane-str  (when dot-pos
                          (let ((s (subseq target-string (1+ dot-pos))))
                            (when (plusp (length s)) s)))))
        ;; When no colon was found, the whole string without dot is the session
        (let ((sess-final (or sess-str
                              (when (not colon-pos)
                                (let ((s (if dot-pos
                                             (subseq target-string 0 dot-pos)
                                             target-string)))
                                  (when (plusp (length s)) s))))))
          (values sess-final win-str pane-str)))))

;;; ── Find: session lookup ─────────────────────────────────────────────────────

(defun find-session-by-target (server target-str)
  "Find a session in SERVER (the *server-sessions* alist) matching TARGET-STR.
   Match rules (in order):
     1. Exact name match
     2. Name prefix match (first matching session wins)
     3. $N notation (session id)
   Returns the session object or NIL when not found.
   SERVER is the *server-sessions* alist: ((name . session) ...)."
  (unless target-str (return-from find-session-by-target nil))
  (flet ((sessions () (mapcar #'cdr server)))
    ;; 1. Exact name match
    (let ((exact (cdr (assoc target-str server :test #'string=))))
      (when exact (return-from find-session-by-target exact)))
    ;; 2. $N: match by session id
    (when (and (plusp (length target-str))
               (char= (char target-str 0) #\$))
      (let ((id (ignore-errors
                  (parse-integer (subseq target-str 1)))))
        (when id
          (let ((by-id (find id (sessions) :key #'session-id)))
            (when by-id (return-from find-session-by-target by-id))))))
    ;; 3. Name prefix match
    (dolist (pair server)
      (when (and (stringp (car pair))
                 (> (length (car pair)) 0)
                 (<= (length target-str) (length (car pair)))
                 (string= target-str (car pair)
                          :end2 (min (length target-str) (length (car pair)))))
        (return-from find-session-by-target (cdr pair))))
    nil))

;;; ── Find: window lookup ──────────────────────────────────────────────────────

(defun find-window-by-target (session target-str)
  "Find a window in SESSION matching TARGET-STR.
   Match rules (in order):
     1. Exact name match
     2. @N: match by window id
     3. Numeric string: 0-based index into (session-windows session)
     4. Name prefix match
   Returns the window object or NIL."
  (unless (and session target-str) (return-from find-window-by-target nil))
  (let ((wins (session-windows session)))
    ;; 1. Exact name match
    (let ((by-name (find target-str wins :key #'window-name :test #'string=)))
      (when by-name (return-from find-window-by-target by-name)))
    ;; 2. @N: match by window id
    (when (and (plusp (length target-str))
               (char= (char target-str 0) #\@))
      (let ((id (ignore-errors (parse-integer (subseq target-str 1)))))
        (when id
          (let ((by-id (find id wins :key #'window-id)))
            (when by-id (return-from find-window-by-target by-id))))))
    ;; 3. Numeric index (0-based)
    (let ((idx (ignore-errors (parse-integer target-str))))
      (when (and idx (>= idx 0) (< idx (length wins)))
        (return-from find-window-by-target (nth idx wins))))
    ;; 4. Name prefix match
    (dolist (w wins)
      (when (and (>= (length (window-name w)) (length target-str))
                 (string= target-str (window-name w)
                          :end2 (min (length target-str) (length (window-name w)))))
        (return-from find-window-by-target w)))
    nil))

;;; ── Find: pane lookup ────────────────────────────────────────────────────────

(defun find-pane-by-target (window target-str)
  "Find a pane in WINDOW matching TARGET-STR.
   Match rules (in order):
     1. %N: match by pane id
     2. Numeric string: 0-based index into (window-panes window)
   Returns the pane object or NIL."
  (unless (and window target-str) (return-from find-pane-by-target nil))
  (let ((panes (window-panes window)))
    ;; 1. %N: match by pane id
    (when (and (plusp (length target-str))
               (char= (char target-str 0) #\%))
      (let ((id (ignore-errors (parse-integer (subseq target-str 1)))))
        (when id
          (let ((by-id (find id panes :key #'pane-id)))
            (when by-id (return-from find-pane-by-target by-id))))))
    ;; 2. Numeric index (0-based)
    (let ((idx (ignore-errors (parse-integer target-str))))
      (when (and idx (>= idx 0) (< idx (length panes)))
        (return-from find-pane-by-target (nth idx panes))))
    nil))

;;; ── Public: resolve-target ───────────────────────────────────────────────────

(defun resolve-target (server target-string &key current-session current-window current-pane)
  "Parse TARGET-STRING and resolve it to (values session window pane).
   SERVER is the *server-sessions* alist used for session lookup.
   CURRENT-SESSION / CURRENT-WINDOW / CURRENT-PANE are the defaults when
   a component is absent from TARGET-STRING or cannot be resolved.

   Target string format: [SESSION][:WINDOW][.PANE]
   Each component may be omitted to use the current default.

   Returns (values session window pane) — any component may be NIL when
   resolution fails and no current default was supplied."
  (multiple-value-bind (sess-str win-str pane-str)
      (%parse-target target-string)
    ;; Session resolution: found or fall back to current
    (let* ((session (or (when sess-str (find-session-by-target server sess-str))
                        current-session))
           ;; Window resolution within the resolved session
           (window  (or (when win-str (find-window-by-target session win-str))
                        (when (null win-str) current-window)
                        (session-active-window session)))
           ;; Pane resolution within the resolved window
           (pane    (or (when pane-str (find-pane-by-target window pane-str))
                        (when (null pane-str) current-pane)
                        (when window (window-active-pane window)))))
      (values session window pane))))
