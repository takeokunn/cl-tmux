(in-package #:cl-tmux/terminal/types)

;;;; Mutable screen struct and grid operations.
;;;;
;;;; Depends on cell.lisp (BLANK-CELL, CLAMP) being loaded first.

;;; ── Screen ─────────────────────────────────────────────────────────────────

(defstruct (screen (:constructor %make-screen))
  "Virtual terminal screen: cursor, cell grid, and CPS parser continuation."
  ;; Geometry
  (width    80 :type fixnum)
  (height   24 :type fixnum)
  ;; Row-major grid: index = y*width + x
  (cells    #() :type simple-vector)
  ;; Cursor position — screen-cursor-x / screen-cursor-y are the stable public names.
  (cursor-x 0 :type fixnum)
  (cursor-y 0 :type fixnum)
  ;; Current SGR state stamped on newly written cells.
  ;; Color encoding matches cell.fg / cell.bg: 0-255 palette, bit-24 = RGB true-color.
  (cur-fg    7 :type (unsigned-byte 25))
  (cur-bg    0 :type (unsigned-byte 25))
  (cur-attrs 0 :type (unsigned-byte 8))
  ;; Cursor visibility: toggled by DECTCEM (?25h = show, ?25l = hide).
  (cursor-visible t :type boolean)
  ;; Scroll region (inclusive 0-based row indices)
  (scroll-top    0  :type fixnum)
  (scroll-bottom 23 :type fixnum)
  ;; CPS parser: a closure (screen byte) -> next-state-fn.
  ;; The DATA defstruct carries a placeholder (#'identity) so that the
  ;; cl-tmux/terminal/parser package need not be present at defstruct compile time.
  ;; make-screen (below) overwrites this slot with the real ground-state function
  ;; after all packages are loaded (cl-tmux.asd guarantees parser loads before emulator).
  (parser #'identity :type function)
  ;; Dirty flag: set whenever a cell changes; cleared by renderer after paint
  (dirty-p t :type boolean)
  ;; Lock for thread safety (renderer <-> PTY-reader threads)
  (lock (make-lock "screen"))
  ;; Alt-screen support (?1049h / ?1049l)
  (alt-cells nil)                           ; saved normal-screen cell grid, or nil
  (alt-cursor-x 0 :type fixnum)            ; cursor column saved on alt-screen entry
  (alt-cursor-y 0 :type fixnum)            ; cursor row saved on alt-screen entry
  ;; DECSC/DECRC saved cursor: (cursor-x cursor-y fg bg attrs) or NIL when nothing saved
  (saved-cursor nil :type list)
  ;; Copy / scroll-back mode
  (copy-mode-p  nil  :type boolean)
  (copy-offset  0    :type fixnum)          ; lines scrolled back (0 = live view)
  (scrollback   nil  :type list)            ; list of row-vectors, newest first
  ;; Copy-mode selection state (nil when no selection is active)
  (copy-mark    nil  :type list)            ; (row . col) mark position, NIL = no selection
  (copy-cursor  nil  :type list)            ; (row . col) cursor position in copy mode, NIL = not in copy mode
  (copy-selecting nil :type boolean)        ; T when selection is being built
  ;; copy-mode -e: when T, scrolling down to the live bottom (offset 0) auto-exits
  ;; copy mode.  Set by `copy-mode -e`; cleared on copy-mode entry/exit.
  (copy-exit-on-bottom nil :type boolean)
  ;; Last printed character — used by CSI REP (repeat preceding char, final byte 'b').
  ;; NIL until the first character has been written to the screen.
  (last-char nil :type (or null character))
  ;; DECSCUSR cursor shape: 0/1=block blink, 2=block steady, 3=underline blink,
  ;; 4=underline steady, 5=bar blink, 6=bar steady
  (cursor-shape 1 :type (unsigned-byte 8))
  ;; Bracketed paste mode (?2004h = on, ?2004l = off)
  (bracketed-paste nil :type boolean)
  ;; Application cursor keys (?1h = on, ?1l = off)
  (app-cursor-keys nil :type boolean)
  ;; OSC 0/2 window title
  (title "" :type string)
  ;; OSC 7 current working directory (file://host/path reported by the shell);
  ;; surfaces as #{pane_current_path}.  Empty until the shell reports it.
  (cwd "" :type string)
  ;; Mouse reporting mode: 0=off, 1=basic-1000, 2=button-1002, 3=all-motion-1003
  (mouse-mode 0 :type (unsigned-byte 8))
  ;; SGR extended mouse encoding: T when ?1006h is set
  (mouse-sgr-mode nil :type boolean)
  ;; Auto-wrap mode: T = wrap at right margin (?7h default), NIL = no wrap (?7l)
  (autowrap t :type boolean)
  ;; Deferred (pending) wrap, a.k.a. the VT100 "last column flag": set after a
  ;; character is written into the last column with autowrap on.  The cursor stays
  ;; parked at the last column; the wrap to the next row happens only when the NEXT
  ;; printable character arrives.  Any explicit cursor movement (set-cursor, CR,
  ;; LF, BS, HT, …) cancels it.  Without this, writing exactly WIDTH characters
  ;; then a newline inserts a spurious blank line.
  (pending-wrap nil :type boolean)
  ;; Origin mode (DECOM, ?6): T = CUP/HVP rows are relative to the scroll region
  ;; (and the cursor is confined to it); NIL (default) = absolute positioning.
  (origin-mode nil :type boolean)
  ;; Focus event reporting (?1004h = on, ?1004l = off).  When on, the pane's
  ;; application is sent ESC[I on focus gained / ESC[O on focus lost (e.g. when
  ;; the active pane changes), so TUIs like vim can redraw or pause.
  (focus-events nil :type boolean)
  ;; Effective (currently-invoked) character set: :ascii or :dec-graphics.
  ;; Derived from whichever of G0/G1 is active; read by %remap-charset-char so
  ;; that direct (setf screen-charset) in tests keeps working.
  (charset :ascii :type (member :ascii :dec-graphics))
  ;; VT100 charset state: G0/G1 designations (ESC ( X / ESC ) X) plus the active
  ;; locking-shift selector toggled by SO (0x0E → G1) / SI (0x0F → G0).  G0 is
  ;; the invoked set on reset.  CHARSET above mirrors the active set's designation.
  (g0-charset :ascii :type (member :ascii :dec-graphics))
  (g1-charset :ascii :type (member :ascii :dec-graphics))
  (active-g :g0 :type (member :g0 :g1))
  ;; Horizontal tab stops.  The :DEFAULT sentinel means "the standard fixed
  ;; every-8-columns stops" (so the common path needs no per-screen list and is
  ;; resize-proof); HTS (ESC H) / TBC (CSI g) materialize it into an explicit
  ;; sorted list of stop columns.
  (tab-stops :default)
  ;; Current underline color pen (same encoding as fg/bg; 0 = default)
  (cur-ul-color 0 :type (unsigned-byte 25))
  ;; Current extended attribute pen (attrs2 bits: double-underline, overline)
  (cur-attrs2 0 :type (unsigned-byte 8))
  ;; Response buffer: a list of strings that the emulator wants to write back
  ;; to the PTY (e.g. DA1/DA2 device attribute responses).  The PTY loop drains
  ;; this and writes the bytes to the master fd.  A list is used as a simple
  ;; FIFO: new entries are pushed to the front (nreverse to drain in order).
  (response-queue nil :type list)
  ;; Passthrough buffer: a list of strings the pane emitted via the tmux DCS
  ;; passthrough sequence (\ePtmux;...\e\\) for the OUTER terminal (not the PTY).
  ;; Used for tmux-in-tmux and image protocols (iTerm2 \e]1337, kitty graphics).
  ;; The renderer drains this and writes to the outer terminal when the
  ;; allow-passthrough option is enabled.  FIFO: push front, nreverse to drain.
  (passthrough-queue nil :type list)
  ;; Clipboard buffer: a list of OSC 52 sequences (ESC ] 52 ; c ; <base64> ST)
  ;; the copy-mode yank enqueues for the OUTER terminal so the host's system
  ;; clipboard is updated.  The renderer drains this when the set-clipboard
  ;; option is on/external (distinct from passthrough-queue's allow-passthrough
  ;; gating).  FIFO: push front, nreverse to drain.
  (clipboard-queue nil :type list)
  ;; BEL (0x07) pending: set to T when the emulator receives a BEL byte.
  ;; The renderer emits an outer-terminal BEL on the next frame and clears the flag.
  (bell-pending nil :type boolean)
  ;; Copy-mode search state: the last search term entered via / or ?
  (copy-search-term nil :type (or null string))
  ;; Copy-mode line-selection flag: T when V (line-select) mode is active
  (copy-line-selection-p nil :type boolean)
  ;; Copy-mode rectangle-select flag: T when 'r' toggles rectangle mode
  (copy-rect-select-p nil :type boolean)
  ;; OSC 10 / OSC 11 default foreground / background colour, as 0xRRGGBB.
  ;; Apps query these (OSC 10 ; ? / OSC 11 ; ?) to detect the terminal's
  ;; light/dark theme and SET them (OSC 10 ; <colour>); OSC 110 / 111 reset to
  ;; the defaults below.  Reported back through response-queue.  Defaults are the
  ;; conventional white-on-black (must match +osc-default-fg+ / +osc-default-bg+
  ;; in parser-osc.lisp, used by the 110/111 reset path).
  (osc-default-fg #xFFFFFF :type (unsigned-byte 24))
  (osc-default-bg #x000000 :type (unsigned-byte 24)))

(defun %make-blank-cells (cell-count)
  "Allocate a simple vector of CELL-COUNT blank cells (space, default colour, no attrs).
   Each call to BLANK-CELL allocates a fresh struct; MAKE-ARRAY :initial-element
   cannot be used here because that would share a single sentinel object across
   all positions — mutations on one cell would silently corrupt others."
  (make-array cell-count :initial-contents (loop repeat cell-count collect (blank-cell))))

(defun make-screen (width height)
  "Create a blank WIDTH x HEIGHT screen with cursor at origin and the CPS parser
   wired to CL-TMUX/TERMINAL/PARSER:GROUND-STATE.
   The parser slot is updated after construction so that the DATA-layer defstruct
   carries no compile-time forward-reference to the CPS layer."
  (let ((screen (%make-screen :width         width
                               :height        height
                               :cells         (%make-blank-cells (* width height))
                               :scroll-bottom (1- height))))
    ;; Wire the real ground-state now that all packages are loaded.
    (setf (screen-parser screen)
          (lambda (s byte) (cl-tmux/terminal/parser:ground-state s byte)))
    screen))

;;; ── Grid helpers ───────────────────────────────────────────────────────────

(defun screen-cell (screen x y)
  "Return the cell at column X, row Y."
  (aref (screen-cells screen)
        (+ (* y (screen-width screen)) x)))

(defun (setf screen-cell) (cell screen x y)
  "Store CELL at column X, row Y in SCREEN's grid.
   Dirty-marking is the responsibility of the action layer; this setter is a
   pure grid accessor — it does NOT set screen-dirty-p."
  (setf (aref (screen-cells screen)
              (+ (* y (screen-width screen)) x))
        cell))

(defun screen-clear-dirty (screen)
  "Clear the dirty flag on SCREEN."
  (setf (screen-dirty-p screen) nil))

(defun screen-consume-bell (screen)
  "Return T and atomically clear SCREEN's bell-pending flag when a BEL is pending.
   Returns NIL without side effects when no bell is pending.

   Canonical placement rationale: this function mutates a single flag slot and is
   called exclusively by the renderer (cl-tmux/renderer-compose) to consume a BEL
   without reaching into the struct directly.  It lives here so both the renderer
   and the LOGIC layer share one definition without a load-order circularity."
  (when (screen-bell-pending screen)
    (setf (screen-bell-pending screen) nil)
    t))

;;; ── SGR pen reset (canonical, data layer) ─────────────────────────────────
;;;
;;; Both cl-tmux/terminal/actions (modes.lisp) and cl-tmux/terminal/sgr perform
;;; an identical five-slot SGR reset.  The canonical definition lives here, in
;;; the TYPES layer that both packages depend on, to eliminate duplication without
;;; creating a load-order circularity between the DISPATCH and LOGIC layers.

(declaim (inline reset-sgr-pen))
(defun reset-sgr-pen (screen)
  "Reset all five SGR pen slots of SCREEN to their VT100 power-on defaults:
   foreground 7 (white), background 0 (black), all attribute bits clear.
   Inlined canonical helper shared by cl-tmux/terminal/sgr and
   cl-tmux/terminal/actions (modes.lisp) to ensure a single source of truth."
  (setf (screen-cur-fg       screen) 7
        (screen-cur-bg       screen) 0
        (screen-cur-attrs    screen) 0
        (screen-cur-attrs2   screen) 0
        (screen-cur-ul-color screen) 0))

;;; ── Resize ─────────────────────────────────────────────────────────────────

(defun screen-resize (screen new-width new-height)
  "Resize SCREEN to NEW-WIDTH x NEW-HEIGHT in place, preserving the
   overlapping top-left rectangle of content.  Resets the scroll region to
   the full new height and clamps the cursor into bounds.

   Alt-cells geometry is not resized; callers that need alt-screen consistency
   should exit alt-screen mode before resizing.

   Callers that share the screen with a reader thread must hold SCREEN's
   lock; this function does no locking of its own."
  (when (and (= new-width  (screen-width  screen))
             (= new-height (screen-height screen)))
    (return-from screen-resize screen))
  (let* ((old-width  (screen-width  screen))
         (old-height (screen-height screen))
         (old-cells  (screen-cells  screen))
         (new-cells  (%make-blank-cells (* new-width new-height)))
         (copy-rows  (min old-height new-height))
         (copy-cols  (min old-width  new-width)))
    ;; Install the new grid before using screen-cell so the index arithmetic
    ;; uses new-width.  Copy the old content via the raw old-cells vector
    ;; (old-width stride) into the new grid using the screen-cell abstraction.
    (setf (screen-cells  screen) new-cells
          (screen-width  screen) new-width
          (screen-height screen) new-height)
    (dotimes (y copy-rows)
      (dotimes (x copy-cols)
        (setf (screen-cell screen x y)
              (aref old-cells (+ (* y old-width) x)))))
    (setf (screen-scroll-top    screen) 0
          (screen-scroll-bottom screen) (1- new-height)
          (screen-cursor-x      screen) (clamp (screen-cursor-x screen) 0 (1- new-width))
          (screen-cursor-y      screen) (clamp (screen-cursor-y screen) 0 (1- new-height))
          (screen-dirty-p       screen) t)
    screen))
