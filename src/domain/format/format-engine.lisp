(in-package #:cl-tmux/format)

;;;; CPS character processor and expand-format public entry points.

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
;;;   expand_step(%,X,     _,   out) :- strftime_letter(X), expand_strftime(%X).
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
      ;; Bare strftime code: %X where X is a recognised strftime letter.
      ;; Real tmux passes status strings through strftime() before #{} expansion.
      ;; Handling inline keeps the expansion composable and avoids a pre-pass.
      ((and (char= ch #\%)
            (< (1+ i) (length template))
            (%strftime-letter-p (char template (1+ i))))
       (write-string (%strftime-format (format nil "%~C" (char template (1+ i)))) out)
       (+ i 2))
      ;; Plain character: pass through unchanged
      (t (write-char ch out) (+ i 1)))))

;;; ── Public entry point ───────────────────────────────────────────────────────

(defun expand-format (template context)
  "Expand TEMPLATE using CONTEXT (a plist of keyword→value pairs).
   Processes one character position at a time via %expand-step (CPS-like):
   each call returns the next index, making the loop a pure iteration over steps.

   Supported specifiers:  #S #I #W #P #H ##  #{var}  #{?c,t,f}  #[sgr]  #(cmd)
                          #{t:var} (timestamp)  #{=N:var} #{=-N:var} (truncate)
                          #{pN:var} #{p-N:var} (pad)  #{b:var} #{d:var} (path)
                          #{U:var} #{L:var} (case)  #{n:var} (length)
                          #{l:var} (literal — emit operand unexpanded)
                          #{s/PAT/REP/[i]:var} (substitute)"
  (with-output-to-string (out)
    (loop for i = 0 then (%expand-step template i context out)
          while (< i (length template)))))

(defun expand-format-safe (template context &optional (fallback template))
  "Like EXPAND-FORMAT, but returns FALLBACK (default: TEMPLATE unexpanded)
   instead of signalling when expansion errors.  Consolidates the
   handler-case-around-expand-format shape duplicated across the renderer
   and dispatch layers."
  (handler-case (expand-format template context)
    (error () fallback)))
