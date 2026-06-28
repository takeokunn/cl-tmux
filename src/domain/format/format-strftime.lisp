(in-package #:cl-tmux/format)

;;; -- Strftime support (#{t:format}) -----------------------------------------
;;;
;;; %strftime-letter-p and the strftime formatting engine.  Loaded before
;;; format.lisp so %expand-step can call %strftime-letter-p and %strftime-format
;;; without a forward reference.

(defun %strftime-letter-p (ch)
  "Return T when CH is a single-character strftime code recognised by %strftime-format."
  (and (characterp ch)
       (member ch '(#\Y #\y #\m #\d #\e #\H #\M #\S #\I #\p #\P
                    #\A #\a #\B #\b #\T #\R #\F #\j #\Z #\%)
                :test #'char=)))


;;; ── Strftime support (#{t:format}) ──────────────────────────────────────────
;;;
;;; #{t:fmt} formats the CURRENT local time using strftime-style codes in FMT.
;;; Common codes: %Y (year) %m (month) %d (day) %H (hour) %M (min) %S (sec)
;;;               %T (HH:MM:SS) %R (HH:MM) %F (YYYY-MM-DD) %% (literal %)
;;; FMT is the REST part of the #{t:...} expression (after the first colon),
;;; so #{t:%H:%M} gives the current time as "15:30" without a variable lookup.

(defconstant +weekday-names+
    (if (boundp '+weekday-names+)
        (symbol-value '+weekday-names+)
        #("Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday" "Sunday"))
  "Full weekday names indexed 0=Monday..6=Sunday (CL decode-universal-time convention).")

(defconstant +weekday-abbrevs+
    (if (boundp '+weekday-abbrevs+)
        (symbol-value '+weekday-abbrevs+)
        #("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))
  "Three-letter weekday abbreviations indexed 0=Monday..6=Sunday.")

(defconstant +month-names+
    (if (boundp '+month-names+)
        (symbol-value '+month-names+)
        #("January" "February" "March" "April" "May" "June"
          "July" "August" "September" "October" "November" "December"))
  "Full month names indexed 0=January..11=December.")

(defconstant +month-abbrevs+
    (if (boundp '+month-abbrevs+)
        (symbol-value '+month-abbrevs+)
        #("Jan" "Feb" "Mar" "Apr" "May" "Jun"
          "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))
  "Three-letter month abbreviations indexed 0=January..11=December.")

(defun %days-in-month (month year)
  "Return the number of days in MONTH (1-12) of YEAR."
  (case month
    ((1 3 5 7 8 10 12) 31)
    ((4 6 9 11) 30)
    (2 (if (or (and (zerop (mod year 4)) (not (zerop (mod year 100))))
               (zerop (mod year 400))) 29 28))
    (otherwise 30)))

;;; — strftime dispatch table (Prolog-like fact table) ———————————————————————
;;;
;;; define-strftime-code-table builds %dispatch-strftime-code from a declarative
;;; (code-char &rest body) fact table, following the define-csi-rules pattern.
;;; The BODY forms receive the closed-over variables sec/min/hour/day/month/year/weekday
;;; from the enclosing let* in %strftime-format and write to OUT.

(defmacro define-strftime-code-table (&rest rules)
  "Build %DISPATCH-STRFTIME-CODE from a declarative (code-char &rest body) fact table.
   The generated function writes the appropriate output for CODE-CHAR to OUT,
   using the time variables (SEC MIN HOUR DAY MONTH YEAR WEEKDAY) in scope.
   Returns T when CODE-CHAR is recognised, NIL otherwise."
  `(defun %dispatch-strftime-code (code out sec min hour day month year weekday)
     "Write the strftime expansion for CODE-CHAR to OUT.  Returns T on match, NIL otherwise."
     (case code
       ,@(mapcar (lambda (rule)
                   `(,(first rule) ,@(rest rule) t))
                 rules)
       (otherwise nil))))

(define-strftime-code-table
  (#\Y (format out "~4,'0D" year))
  (#\y (format out "~2,'0D" (mod year 100)))
  (#\m (format out "~2,'0D" month))
  (#\d (format out "~2,'0D" day))
  (#\e (format out "~2D" day))
  (#\H (format out "~2,'0D" hour))
  (#\M (format out "~2,'0D" min))
  (#\S (format out "~2,'0D" sec))
  ;; 12-hour clock: 0 o'clock maps to 12
  (#\I (format out "~2,'0D" (let ((h (mod hour 12))) (if (zerop h) 12 h))))
  (#\p (write-string (if (< hour 12) "AM" "PM") out))
  (#\P (write-string (if (< hour 12) "am" "pm") out))
  ;; Weekday arrays indexed 0=Monday (CL decode-universal-time convention)
  (#\A (write-string (aref +weekday-names+  weekday) out))
  (#\a (write-string (aref +weekday-abbrevs+ weekday) out))
  (#\B (write-string (aref +month-names+  (1- month)) out))
  (#\b (write-string (aref +month-abbrevs+ (1- month)) out))
  (#\T (format out "~2,'0D:~2,'0D:~2,'0D" hour min sec))
  (#\R (format out "~2,'0D:~2,'0D" hour min))
  (#\F (format out "~4,'0D-~2,'0D-~2,'0D" year month day))
  (#\j (let ((day-of-year (loop for m from 1 below month
                                sum (%days-in-month m year))))
         (format out "~3,'0D" (+ day-of-year day))))
  (#\Z (write-string "UTC" out))
  (#\% (write-char #\% out)))

(defun %strftime-format-decoded (fmt sec min hour day month year weekday)
  "Format strftime-style FMT against already-decoded time components.
   Shared core of %strftime-format (current time) and %strftime-format-at (a
   specific universal-time).  Empty FMT uses the default '%a %b %e %H:%M:%S %Z %Y'.
   Unknown %-codes are emitted literally as %X."
  (when (zerop (length fmt))
    (setf fmt "%a %b %e %H:%M:%S %Z %Y"))
  (with-output-to-string (out)
    (let ((fmt-index 0)
          (fmt-length (length fmt)))
      (loop while (< fmt-index fmt-length) do
        (let ((current-char (char fmt fmt-index)))
          (cond
            ((and (char= current-char #\%)
                  (< (1+ fmt-index) fmt-length))
             (let ((code-char (char fmt (1+ fmt-index))))
               (incf fmt-index 2)
               (unless (%dispatch-strftime-code
                        code-char out sec min hour day month year weekday)
                 ;; Unknown code: emit literally as %X
                 (write-char #\% out)
                 (write-char code-char out))))
            (t
             (write-char current-char out)
             (incf fmt-index))))))))

(defun %strftime-format (fmt)
  "Format the current local time using strftime-style codes in FMT.
   Supported: %Y %y %m %d %e %H %M %S %I %p %P %A %a %B %b %T %R %F %j %Z %%.
   Unknown codes are kept literally (% + code char).
   Empty FMT uses the default '%a %b %e %H:%M:%S %Z %Y'."
  (multiple-value-bind (sec min hour day month year weekday dst tz)
      (get-decoded-time)
    (declare (ignore dst tz))
    (%strftime-format-decoded fmt sec min hour day month year weekday)))

(defun %strftime-format-at (fmt universal-time)
  "Format the given UNIVERSAL-TIME (a CL universal-time integer) in local time
   with strftime-style FMT.  Used by the #{t:VARIABLE} format modifier to render a
   timestamp variable (e.g. #{t:session_last_attached}).  Empty FMT uses the
   default format.  Returns the empty string when UNIVERSAL-TIME is not a usable
   positive integer."
  (if (and (integerp universal-time) (plusp universal-time))
      (multiple-value-bind (sec min hour day month year weekday dst tz)
          (decode-universal-time universal-time)
        (declare (ignore dst tz))
        (%strftime-format-decoded fmt sec min hour day month year weekday))
      ""))

