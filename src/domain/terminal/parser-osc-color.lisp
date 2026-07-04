(in-package #:cl-tmux/terminal/parser)

;;;; OSC color and palette helpers.

(defun %scale-hex-channel (channel)
  "Scale a 4-bit or 8-bit hex channel to 8-bit integer."
  (if (< channel 16)
      (* channel 17)
      channel))

(defun %parse-hash-color (hex)
  "Parse a #RGB or #RRGGBB hex string (without the leading #) to 0xRRGGBB, or NIL."
  (case (length hex)
    (6 (cl-tmux::%parse-integer-or-nil hex :radix 16))
    (3 (let ((r (cl-tmux::%parse-integer-or-nil (subseq hex 0 1) :radix 16))
             (g (cl-tmux::%parse-integer-or-nil (subseq hex 1 2) :radix 16))
             (b (cl-tmux::%parse-integer-or-nil (subseq hex 2 3) :radix 16)))
         (when (and r g b)
           (logior (ash (%scale-hex-channel r) 16)
                   (ash (%scale-hex-channel g) 8)
                   (%scale-hex-channel b)))))
    (otherwise nil)))

(defun %parse-rgb-color (spec)
  "Parse rgb:R/G/B where each channel is 1-4 hex digits."
  (let* ((parts (cl-ppcre:split "/" spec))
         (valid (= (length parts) 3)))
    (when valid
      (let ((channels
             (mapcar (lambda (s)
                       (and (> (length s) 0)
                            (<= (length s) 4)
                            (cl-tmux::%parse-integer-or-nil s :radix 16)))
                     parts)))
        (when (every #'integerp channels)
          (destructuring-bind (r g b) channels
            (labels ((scale (value digits)
                       (case digits
                         (1 (%scale-hex-channel value))
                         (2 value)
                         (3 (ldb (byte 8 4) value))
                         (4 (ldb (byte 8 8) value)))))
              (let ((r (scale r (length (first parts))))
                    (g (scale g (length (second parts))))
                    (b (scale b (length (third parts)))))
                (logior (ash r 16) (ash g 8) b)))))))))

(defun %parse-osc-color (spec)
  "Parse an X11/xterm colour SPEC to a 24-bit 0xRRGGBB integer, or NIL."
  (when (and (stringp spec) (> (length spec) 0))
    (cond
      ((char= (char spec 0) #\#)
       (%parse-hash-color (subseq spec 1)))
      ((and (>= (length spec) 4)
            (string-equal (subseq spec 0 4) "rgb:"))
       (%parse-rgb-color (subseq spec 4)))
      (t nil))))

(defun %osc-hex-channel (byte)
  "Format an 8-bit BYTE as a four-digit uppercase hex channel for OSC replies."
  (format nil "~(~4,'0X~)" (* byte #x101)))

(defun %osc-rgb-components (rgb)
  "Return the 8-bit R, G and B components of RGB (0xRRGGBB)."
  (values (ldb (byte 8 16) rgb)
          (ldb (byte 8 8) rgb)
          (ldb (byte 8 0) rgb)))

(defun %osc-rgb-reply (prefix rgb)
  "Build an OSC reply with PREFIX followed by rgb:RRRR/GGGG/BBBB data."
  (multiple-value-bind (r g b) (%osc-rgb-components rgb)
    (format nil "~C~A~A/~A/~A~C\\"
            #\Escape prefix
            (%osc-hex-channel r)
            (%osc-hex-channel g)
            (%osc-hex-channel b)
            #\Escape)))

(defun %osc-color-reply (command rgb)
  "Build the OSC reply reporting RGB (0xRRGGBB) for an OSC COMMAND query."
  (%osc-rgb-reply (format nil "]~D;rgb:" command) rgb))

(defun %osc-color-command (screen command body current-rgb set-fn)
  "Handle an OSC 10/11 colour command."
  (if (string= body "?")
      (push (%osc-color-reply command current-rgb)
            (screen-response-queue screen))
      (let ((rgb (%parse-osc-color body)))
        (when rgb (funcall set-fn rgb)))))

(defparameter +xterm-base16+
  #(#x000000 #x800000 #x008000 #x808000 #x000080 #x800080 #x008080 #xC0C0C0
    #x808080 #xFF0000 #x00FF00 #xFFFF00 #x0000FF #xFF00FF #x00FFFF #xFFFFFF))

(defun %xterm-palette-rgb (index)
  "Return the RGB colour for xterm palette INDEX as 0xRRGGBB, or NIL."
  (cond
    ((and (<= 0 index) (< index 16))
     (aref +xterm-base16+ index))
    ((and (<= 16 index) (< index 232))
     (let* ((i (- index 16))
            (r (floor i 36))
            (g (floor (mod i 36) 6))
            (b (mod i 6))
            (levels #(0 95 135 175 215 255)))
       (logior (ash (aref levels r) 16)
               (ash (aref levels g) 8)
               (aref levels b))))
    ((and (<= 232 index) (<= index 255))
     (let ((gray (+ 8 (* (- index 232) 10))))
       (logior (ash gray 16) (ash gray 8) gray)))
    (t nil)))

(defun %osc4-reply (index rgb)
  "Build the OSC 4 palette reply for INDEX with RGB value 0xRRGGBB."
  (%osc-rgb-reply (format nil "]4;~D;rgb:" index) rgb))

(defun %osc-split-fields (body)
  "Split OSC 4 BODY into a list of non-empty fields separated by ';'."
  (loop with start = 0
        for pos = (position #\; body :start start)
        collect (subseq body start pos)
        while pos
        do (setf start (1+ pos))))

(defun %palette-effective-rgb (screen index)
  "Return the effective 0xRRGGBB colour for palette INDEX."
  (or (%palette-override-get screen index)
      (%xterm-palette-rgb index)))

(defun %handle-osc-4 (screen body)
  "Handle OSC 4 (set/query palette colours)."
  (let ((fields (%osc-split-fields body)))
    (loop for (index-spec spec) on fields by #'cddr
          while spec
          for index = (cl-tmux::%parse-integer-or-nil index-spec :junk-allowed t)
          when index
            do (if (string= spec "?")
                   (let ((rgb (%palette-effective-rgb screen index)))
                     (when rgb
                       (push (%osc4-reply index rgb) (screen-response-queue screen))))
                   (let ((rgb (%parse-osc-color spec)))
                     (when rgb
                       (%palette-override-set screen index rgb)))))))

(defun %handle-osc-104 (screen body)
  "Handle OSC 104 (reset palette colours)."
  (let ((fields (%osc-split-fields body)))
    (if (or (null fields)
            (and (= (length fields) 1) (string= (first fields) "")))
        (%palette-override-clear-all screen)
        (dolist (index-spec fields)
          (let ((index (cl-tmux::%parse-integer-or-nil index-spec :junk-allowed t)))
            (when index
              (%palette-override-clear screen index)))))))
