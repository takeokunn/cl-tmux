(in-package #:cl-tmux/terminal/csi)

;;;; CSI dispatch macro.
;;;;
;;;; The terminal domain keeps CSI decoding declarative: each rule is a fact
;;;; whose predicate selects a protocol action.  The macro expands that table
;;;; into the single EXECUTE-CSI entry point consumed by the parser.

(defmacro define-csi-rules (&rest rules)
  "Each RULE is (condition-form &body forms).
   Available bindings in every rule body:
     SCREEN   - the screen struct
     FINAL    - the sequence final character (type character)
     INTERMED - intermediate character (character or nil; e.g. #\\? for DEC)
     PARAMS   - full parameter list (list of fixnum)
     P1       - first  parameter or 0
     P2       - second parameter or 0
     P1*      - (max 1 p1)
     P2*      - (max 1 p2)
   Expands into a DEFUN for EXECUTE-CSI that dispatches via COND.
   Unknown final bytes or unrecognized (INTERMED, FINAL) combinations are
   silently ignored and return (values), matching real-terminal behaviour."
  `(defun execute-csi (screen final intermed private params)
     "Dispatch one complete CSI escape sequence to its terminal action.
      SCREEN is the target screen struct.  FINAL is the sequence's final byte as
      a character.  INTERMED is the optional true intermediate byte (#x20-#x2F,
      e.g. #\\Space for DECSCUSR, #\\$ for DECRQM), or NIL.  PRIVATE is the optional
      private/marker byte (#\\? for DEC private sequences, #\\> for secondary DA),
      or NIL.  PARAMS is the list of integer parameters (possibly empty).
      Unknown sequences are silently ignored; no error is signalled."
     (declare (type screen screen)
              (type character final)
              (ignorable intermed private))
     (let* ((p1  (%csi-leading-int (first  params)))
            (p2  (%csi-leading-int (second params)))
            (p1* (max 1 p1))
            (p2* (max 1 p2)))
       (declare (type fixnum p1 p2 p1* p2*) (ignorable p1 p2 p1* p2*))
       (cond
         ,@(mapcar (lambda (rule)
                     (destructuring-bind (condition &rest body) rule
                       `(,condition ,@body)))
                   rules)
         (t (values))))))
