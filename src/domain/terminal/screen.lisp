(in-package #:cl-tmux/terminal/types)

;;;; Screen struct definition and pure grid operations (DATA layer).
;;;;
;;;; Depends on cell.lisp (BLANK-CELL, CLAMP, +default-color+) being loaded first.
;;;;
;;;; Layer boundaries:
;;;;   DATA:      defstruct definition, slot defaults, %make-blank-cells
;;;;   CONSTRUCT: make-screen — allocates lock, cells, scroll-bottom, parser
;;;;   GRID:      screen-cell, setf screen-cell
;;;;
;;;; screen-resize → screen-resize.lisp
;;;; screen-clear-dirty, screen-consume-bell, reset-sgr-pen → screen-logic.lisp
;;;; Line-wrap and palette metadata helpers → screen-metadata.lisp

;;; ── Screen ─────────────────────────────────────────────────────────────────

(defstruct (screen (:constructor %make-screen))
  "Virtual terminal screen: cursor, cell grid, and CPS parser continuation."
  ;; Geometry — defaults match the VT100 standard 80×24 terminal.
  (width    +default-screen-width+  :type fixnum)
  (height   +default-screen-height+ :type fixnum)
  ;; Row-major grid: index = y*width + x
  (cells    #() :type simple-vector)
  ;; Cursor position — screen-cursor-x / screen-cursor-y are the stable public names.
  (cursor-x 0 :type fixnum)
  (cursor-y 0 :type fixnum)
  ;; Current SGR state stamped on newly written cells.
  ;; Color encoding matches cell.fg / cell.bg: 0-255 palette, bit-24 = RGB true-color.
  (cur-fg    +default-color+ :type (unsigned-byte 25))
  (cur-bg    +default-color+ :type (unsigned-byte 25))
  (cur-attrs 0 :type (unsigned-byte 8))
  ;; Cursor visibility: toggled by DECTCEM (?25h = show, ?25l = hide).
  (cursor-visible t :type boolean)
  ;; Scroll region (inclusive 0-based row indices).
  ;; Default scroll-bottom = height-1; matches VT100 power-on state.
  (scroll-top    0                             :type fixnum)
  (scroll-bottom (1- +default-screen-height+) :type fixnum)
  ;; CPS parser: a closure (screen byte) -> next-state-fn.
  ;; The DATA defstruct carries a placeholder (#'identity) so that the
  ;; cl-tmux/terminal/parser package need not be present at defstruct compile time.
  ;; make-screen (below) overwrites this slot with the real ground-state function
  ;; after all packages are loaded (cl-tmux.asd guarantees parser loads before emulator).
  (parser #'identity :type function)
  ;; Dirty flag: set whenever a cell changes; cleared by renderer after paint
  (dirty-p t :type boolean)
  ;; Lock for thread safety (renderer <-> PTY-reader threads).
  ;; Allocated by make-screen (CONSTRUCT layer), not here, so this DATA-layer
  ;; defstruct remains free of side-effecting allocations at load time.
  (lock nil :type (or null bordeaux-threads:lock))
  ;; Alt-screen support (?1049h / ?1049l)
  (alt-cells nil)                           ; saved normal-screen cell grid, or nil
  (alt-cursor-x 0 :type fixnum)            ; cursor column saved on alt-screen entry
  (alt-cursor-y 0 :type fixnum)            ; cursor row saved on alt-screen entry
  ;; DECSC/DECRC saved state, or NIL when nothing saved.  Mirrors tmux input_save_state:
  ;; (cursor-x cursor-y fg bg attrs attrs2 ul-color g0-charset g1-charset active-g charset origin-mode)
  (saved-cursor nil :type list)
  ;; Copy / scroll-back mode
  (copy-mode-p  nil  :type boolean)
  ;; copy-mode -H: suppress the position indicator overlay for this entry.
  (copy-hide-position nil :type boolean)
  (copy-offset  0    :type fixnum)          ; lines scrolled back (0 = live view)
  (scrollback   nil  :type list)            ; list of row-vectors, newest first
  ;; Copy-mode selection state (nil when no selection is active)
  ;; Wrap flags for scrollback rows (newest-first, parallel to scrollback):
  ;; T when that history row wrapped into the row below it (capture-pane -J).
  (scrollback-wrapped nil :type list)
  ;; OSC 133 shell-integration prompt marks: absolute row indexes (counted
  ;; from the start of the output stream) where a prompt began ('133;A').
  ;; history-trimmed counts rows dropped from the scrollback forever, so
  ;; absolute = history-trimmed + scrollback position.
  (prompt-marks nil :type list)
  (history-trimmed 0 :type integer)
  ;; DECDHL/DECDWL per-row line sizes: row -> #\3 (double-height top),
  ;; #\4 (double-height bottom), #\6 (double-width); absent = single (#\5).
  ;; The renderer re-emits ESC # <char> so the OUTER terminal draws the size.
  (line-sizes (make-hash-table) :type hash-table)
  (copy-mark    nil  :type list)            ; (row . col) mark position, NIL = no selection
  (copy-mark-offset 0 :type fixnum)         ; copy-offset in effect when copy-mark was set
  (copy-cursor  nil  :type list)            ; (row . col) cursor position in copy mode, NIL = not in copy mode
  (copy-selecting nil :type boolean)        ; T when selection is being built
  ;; copy-mode -e: when T, scrolling down to the live bottom (offset 0) auto-exits
  ;; copy mode.  Set by `copy-mode -e`; cleared on copy-mode entry/exit.
  (copy-exit-on-bottom nil :type boolean)
  ;; copy-mode entered by mouse: suppress gutter line numbers while copy mode
  ;; was opened via wheel/click, matching tmux's mouse-enter behavior.
  (copy-mode-entered-by-mouse-p nil :type boolean)
  ;; Last printed character — used by CSI REP (repeat preceding char, final byte 'b').
  ;; NIL until the first character has been written to the screen.
  (last-char nil :type (or null character))
  ;; DECSCUSR cursor shape: 0/1=block blink, 2=block steady, 3=underline blink,
  ;; 4=underline steady, 5=bar blink, 6=bar steady
  (cursor-shape 1 :type (unsigned-byte 8))
  ;; IRM — Insert/Replace Mode (CSI 4 h = insert, CSI 4 l = replace; default off).
  ;; When T, a printed character inserts at the cursor, shifting the rest of the
  ;; line right (rather than overwriting).  Reset by RIS / DECSTR.
  (insert-mode nil :type boolean)
  ;; LNM — Line Feed/New Line Mode (CSI 20 h = newline, CSI 20 l = line feed;
  ;; default off).  When T, a C0 line-feed (LF/VT/FF) also performs a carriage
  ;; return (cursor to column 0).  Reset by RIS / DECSTR.
  (newline-mode nil :type boolean)
  ;; DECSCNM — reverse-video screen (?5h on / ?5l off; default off).  When T the
  ;; whole grid renders with fg/bg swapped (a global reverse, XORed per cell with
  ;; the cell's own reverse attribute).  Reset by RIS.
  (reverse-screen nil :type boolean)
  ;; Bracketed paste mode (?2004h = on, ?2004l = off)
  (bracketed-paste nil :type boolean)
  ;; Application cursor keys (?1h = on, ?1l = off)
  (app-cursor-keys nil :type boolean)
  ;; OSC 0/2 window title
  (title "" :type string)
  ;; XTPUSHTITLE / XTPOPTITLE (CSI > Ps t / CSI < Ps t): a stack of saved
  ;; title strings.  Push saves the current title; pop restores the most
  ;; recently saved one.  The stack is bounded to 8 entries to match xterm.
  (title-stack nil :type list)
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
  ;; G2/G3 (ESC * X / ESC + X), invoked via LS2/LS3 (ESC n / ESC o) locking
  ;; shifts or SS2/SS3 (ESC N / ESC O) single shifts.
  (g2-charset :ascii :type (member :ascii :dec-graphics))
  (g3-charset :ascii :type (member :ascii :dec-graphics))
  (active-g :g0 :type (member :g0 :g1 :g2 :g3))
  ;; Pending single shift: the NEXT printable character only is mapped through
  ;; this G set (SS2 → :g2, SS3 → :g3), then the shift clears.
  (single-shift nil :type (member nil :g2 :g3))
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
  ;; Direction of the last explicit search (/ → :forward, ? → :backward), so n
  ;; repeats in that direction and N reverses it.  NIL until the first search;
  ;; the n/N commands treat NIL as :forward.
  (copy-search-direction nil :type (or null keyword))
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
  (osc-default-fg +osc-default-fg+ :type (unsigned-byte 24))
  (osc-default-bg +osc-default-bg+ :type (unsigned-byte 24))
  ;; OSC 8 current hyperlink URI: set by OSC 8 ; params ; URI, cleared by OSC 8 ; ;.
  ;; Stamped onto each cell written while non-NIL (see %write-normal-cell).
  (current-hyperlink nil :type (or null string))
  ;; OSC 4 / OSC 104 custom palette overrides.  NIL means "no overrides — use the
  ;; built-in xterm 256-colour palette".  Otherwise a lazily-allocated simple-vector
  ;; of 256 entries; each entry is an 0xRRGGBB integer override or NIL (use built-in).
  ;; Mirrors tmux's per-pane colour_palette override array (input_osc_4 /
  ;; colour_palette_set); OSC 104 clears entries back to NIL.
  (palette-overrides nil :type (or null simple-vector))
  ;; Per-row line-wrap flags (a lazily-created hash-table row→T, or NIL): a row is
  ;; marked when an autowrap actually carries its line onto the next row, so
  ;; capture-pane -J can rejoin lines that wrapped at the right margin.  Cleared
  ;; when a row is repositioned/erased; shifted on scroll.  Pure capture metadata
  ;; — it never affects rendering or emulation.
  (wrapped-rows nil :type (or null hash-table)))

(defun %make-blank-cells (cell-count)
  "Allocate a simple vector of CELL-COUNT blank cells (space, default colour, no attrs).
   Each call to BLANK-CELL allocates a fresh struct; MAKE-ARRAY :initial-element
   cannot be used here because that would share a single sentinel object across
   all positions — mutations on one cell would silently corrupt others."
  (make-array cell-count :initial-contents (loop repeat cell-count collect (blank-cell))))

(defun make-screen (width height)
  "Create a blank WIDTH x HEIGHT screen with cursor at origin.
   Allocates the bordeaux-threads mutex here (CONSTRUCT layer) so that the
   defstruct default for the lock slot can be NIL, keeping the DATA layer free
   of side-effecting allocations at load time.
   The CPS parser is wired to CL-TMUX/TERMINAL/PARSER:GROUND-STATE after
   construction so that the DATA-layer defstruct carries no compile-time
   forward-reference to the CPS layer."
  (let ((screen (%make-screen :width         width
                               :height        height
                               :cells         (%make-blank-cells (* width height))
                               :scroll-bottom (1- height)
                               :lock          (make-lock "screen"))))
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

;;; screen-clear-dirty, screen-consume-bell, and reset-sgr-pen are defined in
;;; screen-logic.lisp (loaded immediately after this file).  They mutate screen
;;; slots and belong in the LOGIC layer, not in this DATA file.
