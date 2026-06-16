(:schema-version 1
 :generated-date "2026-06-14"
 :tmux-version "tmux 3.6a"
 :scope (:commands 90
         :named-bindings 87
         :root-bindings 19
         :prefix-bindings 87
         :copy-mode-bindings 74
         :copy-mode-vi-bindings 87
         :default-key-bindings 267
         :global-options 61
         :window-options 67
         :hooks 57
         :formats 226)
 :status-counts (:match 52
                 :partial 405
                 :missing 332
                 :intentionally-unsupported 0
                 :unknown 0)
 :entries
 ((:kind :inventory
   :name "tmux command inventory"
   :status :partial
   :tmux-command "tmux -L <clean> -f /dev/null list-commands"
   :tmux-count 90
   :cl-tmux-count 211
   :cl-tmux-matched-count 90
   :cl-tmux-extra-count 121
   :cl-tmux-evidence "All tmux 3.6a command names are present in cl-tmux's combined bindable-command and argv-command inventories, but behavior, flags, aliases, output, and side effects are not yet exhaustively proven."
   :next-step "Compare command signatures, aliases, flags, output, server behavior, side effects, and error behavior against real tmux.")
  (:kind :inventory
   :name "tmux named binding inventory"
   :status :partial
   :tmux-command "tmux -L <clean> -f /dev/null list-keys -N"
   :tmux-count 87
   :cl-tmux-evidence "Named binding notes are not yet compared row-by-row against cl-tmux list-keys output."
   :next-step "Generate binding rows from tmux list-keys -N and compare notes, tables, keys, repeat flags, and commands.")
  (:kind :inventory
   :name "tmux root binding inventory"
   :status :partial
   :tmux-command "tmux -L <clean> -f /dev/null list-keys -T root"
   :tmux-count 19
   :cl-tmux-evidence "Root key table exists in cl-tmux, but all 19 tmux 3.6a default root bindings are absent from the cl-tmux table."
   :next-step "Implement or intentionally exclude root mouse/status bindings, then compare flags and command behavior.")
  (:kind :inventory
   :name "tmux prefix binding inventory"
   :status :partial
   :tmux-command "tmux -L <clean> -f /dev/null list-keys -T prefix"
   :tmux-count 87
   :cl-tmux-evidence "Default prefix table is row-expanded below; 39 of 87 tmux 3.6a keys have a cl-tmux table/key entry. Command strings and behavior are not yet proven."
   :next-step "Compare repeat flags, notes, command strings, dispatch effects, and error behavior.")
 (:kind :inventory
   :name "tmux copy-mode binding inventory"
   :status :partial
   :tmux-command "tmux -L <clean> -f /dev/null list-keys -T copy-mode"
   :tmux-count 74
   :cl-tmux-evidence "Default copy-mode table is row-expanded below; 55 of 74 tmux 3.6a keys have a cl-tmux table/key entry. Numeric repeat handling is modeled separately in runtime and should not be read as a missing static binding."
   :next-step "Compare repeat flags, notes, command strings, copy-mode state transitions, output behavior, and count-prefix semantics separately from static list-keys rows.")
  (:kind :inventory
   :name "tmux copy-mode-vi binding inventory"
   :status :partial
   :tmux-command "tmux -L <clean> -f /dev/null list-keys -T copy-mode-vi"
   :tmux-count 87
   :cl-tmux-evidence "Default copy-mode-vi table is row-expanded below; 71 of 87 tmux 3.6a keys have a cl-tmux table/key entry. Numeric repeat handling is modeled separately in runtime and should not be read as a missing static binding."
   :next-step "Compare repeat flags, notes, command strings, copy-mode state transitions, output behavior, and count-prefix semantics separately from static list-keys rows.")
  ;; Default key binding rows generated from tmux 3.6a list-keys -T.
  (:kind :binding :name "binding root MouseDown1Pane" :table "root" :key "MouseDown1Pane" :status :missing :cl-tmux-binding nil :tmux-version "tmux 3.6a" :source "list-keys -T root")
  (:kind :binding :name "binding root MouseDown1Status" :table "root" :key "MouseDown1Status" :status :missing :cl-tmux-binding nil :tmux-version "tmux 3.6a" :source "list-keys -T root")
  (:kind :binding :name "binding root MouseDown1ScrollbarUp" :table "root" :key "MouseDown1ScrollbarUp" :status :missing :cl-tmux-binding nil :tmux-version "tmux 3.6a" :source "list-keys -T root")
  (:kind :binding :name "binding root MouseDown1ScrollbarDown" :table "root" :key "MouseDown1ScrollbarDown" :status :missing :cl-tmux-binding nil :tmux-version "tmux 3.6a" :source "list-keys -T root")
  (:kind :binding :name "binding root MouseDown2Pane" :table "root" :key "MouseDown2Pane" :status :missing :cl-tmux-binding nil :tmux-version "tmux 3.6a" :source "list-keys -T root")
  (:kind :binding :name "binding root MouseDown3Pane" :table "root" :key "MouseDown3Pane" :status :missing :cl-tmux-binding nil :tmux-version "tmux 3.6a" :source "list-keys -T root")
  (:kind :binding :name "binding root MouseDown3Status" :table "root" :key "MouseDown3Status" :status :missing :cl-tmux-binding nil :tmux-version "tmux 3.6a" :source "list-keys -T root")
  (:kind :binding :name "binding root MouseDown3StatusLeft" :table "root" :key "MouseDown3StatusLeft" :status :missing :cl-tmux-binding nil :tmux-version "tmux 3.6a" :source "list-keys -T root")
  (:kind :binding :name "binding root MouseDrag1Pane" :table "root" :key "MouseDrag1Pane" :status :missing :cl-tmux-binding nil :tmux-version "tmux 3.6a" :source "list-keys -T root")
  (:kind :binding :name "binding root MouseDrag1ScrollbarSlider" :table "root" :key "MouseDrag1ScrollbarSlider" :status :missing :cl-tmux-binding nil :tmux-version "tmux 3.6a" :source "list-keys -T root")
  (:kind :binding :name "binding root MouseDrag1Border" :table "root" :key "MouseDrag1Border" :status :missing :cl-tmux-binding nil :tmux-version "tmux 3.6a" :source "list-keys -T root")
  (:kind :binding :name "binding root WheelUpPane" :table "root" :key "WheelUpPane" :status :missing :cl-tmux-binding nil :tmux-version "tmux 3.6a" :source "list-keys -T root")
  (:kind :binding :name "binding root WheelUpStatus" :table "root" :key "WheelUpStatus" :status :missing :cl-tmux-binding nil :tmux-version "tmux 3.6a" :source "list-keys -T root")
  (:kind :binding :name "binding root WheelDownStatus" :table "root" :key "WheelDownStatus" :status :missing :cl-tmux-binding nil :tmux-version "tmux 3.6a" :source "list-keys -T root")
  (:kind :binding :name "binding root DoubleClick1Pane" :table "root" :key "DoubleClick1Pane" :status :missing :cl-tmux-binding nil :tmux-version "tmux 3.6a" :source "list-keys -T root")
  (:kind :binding :name "binding root TripleClick1Pane" :table "root" :key "TripleClick1Pane" :status :missing :cl-tmux-binding nil :tmux-version "tmux 3.6a" :source "list-keys -T root")
  (:kind :binding :name "binding root M-MouseDown3Pane" :table "root" :key "M-MouseDown3Pane" :status :missing :cl-tmux-binding nil :tmux-version "tmux 3.6a" :source "list-keys -T root")
  (:kind :binding :name "binding root M-MouseDown3Status" :table "root" :key "M-MouseDown3Status" :status :missing :cl-tmux-binding nil :tmux-version "tmux 3.6a" :source "list-keys -T root")
  (:kind :binding :name "binding root M-MouseDown3StatusLeft" :table "root" :key "M-MouseDown3StatusLeft" :status :missing :cl-tmux-binding nil :tmux-version "tmux 3.6a" :source "list-keys -T root")
  (:kind :binding :name "binding prefix Space" :table "prefix" :key "Space" :status :missing :cl-tmux-binding nil :tmux-version "tmux 3.6a" :source "list-keys -T prefix")
  (:kind :binding :name "binding prefix !" :table "prefix" :key "!" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T prefix")
  (:kind :binding :name "binding prefix \"" :table "prefix" :key "\"" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T prefix")
  (:kind :binding :name "binding prefix #" :table "prefix" :key "#" :status :missing :cl-tmux-binding nil :tmux-version "tmux 3.6a" :source "list-keys -T prefix")
  (:kind :binding :name "binding prefix $" :table "prefix" :key "$" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T prefix")
  (:kind :binding :name "binding prefix %" :table "prefix" :key "%" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T prefix")
  (:kind :binding :name "binding prefix &" :table "prefix" :key "&" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T prefix")
  (:kind :binding :name "binding prefix '" :table "prefix" :key "'" :status :missing :cl-tmux-binding nil :tmux-version "tmux 3.6a" :source "list-keys -T prefix")
  (:kind :binding :name "binding prefix (" :table "prefix" :key "(" :status :missing :cl-tmux-binding nil :tmux-version "tmux 3.6a" :source "list-keys -T prefix")
  (:kind :binding :name "binding prefix )" :table "prefix" :key ")" :status :missing :cl-tmux-binding nil :tmux-version "tmux 3.6a" :source "list-keys -T prefix")
  (:kind :binding :name "binding prefix ," :table "prefix" :key "," :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T prefix")
  (:kind :binding :name "binding prefix -" :table "prefix" :key "-" :status :missing :cl-tmux-binding nil :tmux-version "tmux 3.6a" :source "list-keys -T prefix")
  (:kind :binding :name "binding prefix ." :table "prefix" :key "." :status :missing :cl-tmux-binding nil :tmux-version "tmux 3.6a" :source "list-keys -T prefix")
  (:kind :binding :name "binding prefix /" :table "prefix" :key "/" :status :missing :cl-tmux-binding nil :tmux-version "tmux 3.6a" :source "list-keys -T prefix")
  (:kind :binding :name "binding prefix 0" :table "prefix" :key "0" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T prefix")
  (:kind :binding :name "binding prefix 1" :table "prefix" :key "1" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T prefix")
  (:kind :binding :name "binding prefix 2" :table "prefix" :key "2" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T prefix")
  (:kind :binding :name "binding prefix 3" :table "prefix" :key "3" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T prefix")
  (:kind :binding :name "binding prefix 4" :table "prefix" :key "4" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T prefix")
  (:kind :binding :name "binding prefix 5" :table "prefix" :key "5" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T prefix")
  (:kind :binding :name "binding prefix 6" :table "prefix" :key "6" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T prefix")
  (:kind :binding :name "binding prefix 7" :table "prefix" :key "7" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T prefix")
  (:kind :binding :name "binding prefix 8" :table "prefix" :key "8" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T prefix")
  (:kind :binding :name "binding prefix 9" :table "prefix" :key "9" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T prefix")
  (:kind :binding :name "binding prefix :" :table "prefix" :key ":" :status :missing :cl-tmux-binding nil :tmux-version "tmux 3.6a" :source "list-keys -T prefix")
  (:kind :binding :name "binding prefix ;" :table "prefix" :key ";" :status :missing :cl-tmux-binding nil :tmux-version "tmux 3.6a" :source "list-keys -T prefix")
  (:kind :binding :name "binding prefix <" :table "prefix" :key "<" :status :missing :cl-tmux-binding nil :tmux-version "tmux 3.6a" :source "list-keys -T prefix")
  (:kind :binding :name "binding prefix =" :table "prefix" :key "=" :status :missing :cl-tmux-binding nil :tmux-version "tmux 3.6a" :source "list-keys -T prefix")
  (:kind :binding :name "binding prefix >" :table "prefix" :key ">" :status :missing :cl-tmux-binding nil :tmux-version "tmux 3.6a" :source "list-keys -T prefix")
  (:kind :binding :name "binding prefix ?" :table "prefix" :key "?" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T prefix")
  (:kind :binding :name "binding prefix C" :table "prefix" :key "C" :status :missing :cl-tmux-binding nil :tmux-version "tmux 3.6a" :source "list-keys -T prefix")
  (:kind :binding :name "binding prefix D" :table "prefix" :key "D" :status :missing :cl-tmux-binding nil :tmux-version "tmux 3.6a" :source "list-keys -T prefix")
  (:kind :binding :name "binding prefix E" :table "prefix" :key "E" :status :missing :cl-tmux-binding nil :tmux-version "tmux 3.6a" :source "list-keys -T prefix")
  (:kind :binding :name "binding prefix L" :table "prefix" :key "L" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T prefix")
  (:kind :binding :name "binding prefix M" :table "prefix" :key "M" :status :missing :cl-tmux-binding nil :tmux-version "tmux 3.6a" :source "list-keys -T prefix")
  (:kind :binding :name "binding prefix [" :table "prefix" :key "[" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T prefix")
  (:kind :binding :name "binding prefix ]" :table "prefix" :key "]" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T prefix")
  (:kind :binding :name "binding prefix c" :table "prefix" :key "c" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T prefix")
  (:kind :binding :name "binding prefix d" :table "prefix" :key "d" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T prefix")
  (:kind :binding :name "binding prefix f" :table "prefix" :key "f" :status :missing :cl-tmux-binding nil :tmux-version "tmux 3.6a" :source "list-keys -T prefix")
  (:kind :binding :name "binding prefix i" :table "prefix" :key "i" :status :missing :cl-tmux-binding nil :tmux-version "tmux 3.6a" :source "list-keys -T prefix")
  (:kind :binding :name "binding prefix l" :table "prefix" :key "l" :status :missing :cl-tmux-binding nil :tmux-version "tmux 3.6a" :source "list-keys -T prefix")
  (:kind :binding :name "binding prefix m" :table "prefix" :key "m" :status :missing :cl-tmux-binding nil :tmux-version "tmux 3.6a" :source "list-keys -T prefix")
  (:kind :binding :name "binding prefix n" :table "prefix" :key "n" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T prefix")
  (:kind :binding :name "binding prefix o" :table "prefix" :key "o" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T prefix")
  (:kind :binding :name "binding prefix p" :table "prefix" :key "p" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T prefix")
  (:kind :binding :name "binding prefix q" :table "prefix" :key "q" :status :missing :cl-tmux-binding nil :tmux-version "tmux 3.6a" :source "list-keys -T prefix")
  (:kind :binding :name "binding prefix r" :table "prefix" :key "r" :status :missing :cl-tmux-binding nil :tmux-version "tmux 3.6a" :source "list-keys -T prefix")
  (:kind :binding :name "binding prefix s" :table "prefix" :key "s" :status :missing :cl-tmux-binding nil :tmux-version "tmux 3.6a" :source "list-keys -T prefix")
  (:kind :binding :name "binding prefix t" :table "prefix" :key "t" :status :missing :cl-tmux-binding nil :tmux-version "tmux 3.6a" :source "list-keys -T prefix")
  (:kind :binding :name "binding prefix w" :table "prefix" :key "w" :status :missing :cl-tmux-binding nil :tmux-version "tmux 3.6a" :source "list-keys -T prefix")
  (:kind :binding :name "binding prefix x" :table "prefix" :key "x" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T prefix")
  (:kind :binding :name "binding prefix z" :table "prefix" :key "z" :status :missing :cl-tmux-binding nil :tmux-version "tmux 3.6a" :source "list-keys -T prefix")
  (:kind :binding :name "binding prefix {" :table "prefix" :key "{" :status :missing :cl-tmux-binding nil :tmux-version "tmux 3.6a" :source "list-keys -T prefix")
  (:kind :binding :name "binding prefix }" :table "prefix" :key "}" :status :missing :cl-tmux-binding nil :tmux-version "tmux 3.6a" :source "list-keys -T prefix")
  (:kind :binding :name "binding prefix ~" :table "prefix" :key "~" :status :missing :cl-tmux-binding nil :tmux-version "tmux 3.6a" :source "list-keys -T prefix")
  (:kind :binding :name "binding prefix DC" :table "prefix" :key "DC" :status :missing :cl-tmux-binding nil :tmux-version "tmux 3.6a" :source "list-keys -T prefix")
  (:kind :binding :name "binding prefix PPage" :table "prefix" :key "PPage" :status :missing :cl-tmux-binding nil :tmux-version "tmux 3.6a" :source "list-keys -T prefix")
  (:kind :binding :name "binding prefix Up" :table "prefix" :key "Up" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T prefix")
  (:kind :binding :name "binding prefix Down" :table "prefix" :key "Down" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T prefix")
  (:kind :binding :name "binding prefix Left" :table "prefix" :key "Left" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T prefix")
  (:kind :binding :name "binding prefix Right" :table "prefix" :key "Right" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T prefix")
  (:kind :binding :name "binding prefix M-1" :table "prefix" :key "M-1" :status :missing :cl-tmux-binding nil :tmux-version "tmux 3.6a" :source "list-keys -T prefix")
  (:kind :binding :name "binding prefix M-2" :table "prefix" :key "M-2" :status :missing :cl-tmux-binding nil :tmux-version "tmux 3.6a" :source "list-keys -T prefix")
  (:kind :binding :name "binding prefix M-3" :table "prefix" :key "M-3" :status :missing :cl-tmux-binding nil :tmux-version "tmux 3.6a" :source "list-keys -T prefix")
  (:kind :binding :name "binding prefix M-4" :table "prefix" :key "M-4" :status :missing :cl-tmux-binding nil :tmux-version "tmux 3.6a" :source "list-keys -T prefix")
  (:kind :binding :name "binding prefix M-5" :table "prefix" :key "M-5" :status :missing :cl-tmux-binding nil :tmux-version "tmux 3.6a" :source "list-keys -T prefix")
  (:kind :binding :name "binding prefix M-6" :table "prefix" :key "M-6" :status :missing :cl-tmux-binding nil :tmux-version "tmux 3.6a" :source "list-keys -T prefix")
  (:kind :binding :name "binding prefix M-7" :table "prefix" :key "M-7" :status :missing :cl-tmux-binding nil :tmux-version "tmux 3.6a" :source "list-keys -T prefix")
  (:kind :binding :name "binding prefix M-n" :table "prefix" :key "M-n" :status :missing :cl-tmux-binding nil :tmux-version "tmux 3.6a" :source "list-keys -T prefix")
  (:kind :binding :name "binding prefix M-o" :table "prefix" :key "M-o" :status :missing :cl-tmux-binding nil :tmux-version "tmux 3.6a" :source "list-keys -T prefix")
  (:kind :binding :name "binding prefix M-p" :table "prefix" :key "M-p" :status :missing :cl-tmux-binding nil :tmux-version "tmux 3.6a" :source "list-keys -T prefix")
  (:kind :binding :name "binding prefix M-Up" :table "prefix" :key "M-Up" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T prefix")
  (:kind :binding :name "binding prefix M-Down" :table "prefix" :key "M-Down" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T prefix")
  (:kind :binding :name "binding prefix M-Left" :table "prefix" :key "M-Left" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T prefix")
  (:kind :binding :name "binding prefix M-Right" :table "prefix" :key "M-Right" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T prefix")
  (:kind :binding :name "binding prefix C-b" :table "prefix" :key "C-b" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T prefix")
  (:kind :binding :name "binding prefix C-o" :table "prefix" :key "C-o" :status :missing :cl-tmux-binding nil :tmux-version "tmux 3.6a" :source "list-keys -T prefix")
  (:kind :binding :name "binding prefix C-z" :table "prefix" :key "C-z" :status :missing :cl-tmux-binding nil :tmux-version "tmux 3.6a" :source "list-keys -T prefix")
  (:kind :binding :name "binding prefix C-Up" :table "prefix" :key "C-Up" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T prefix")
  (:kind :binding :name "binding prefix C-Down" :table "prefix" :key "C-Down" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T prefix")
  (:kind :binding :name "binding prefix C-Left" :table "prefix" :key "C-Left" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T prefix")
  (:kind :binding :name "binding prefix C-Right" :table "prefix" :key "C-Right" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T prefix")
  (:kind :binding :name "binding prefix S-Up" :table "prefix" :key "S-Up" :status :missing :cl-tmux-binding nil :tmux-version "tmux 3.6a" :source "list-keys -T prefix")
  (:kind :binding :name "binding prefix S-Down" :table "prefix" :key "S-Down" :status :missing :cl-tmux-binding nil :tmux-version "tmux 3.6a" :source "list-keys -T prefix")
  (:kind :binding :name "binding prefix S-Left" :table "prefix" :key "S-Left" :status :missing :cl-tmux-binding nil :tmux-version "tmux 3.6a" :source "list-keys -T prefix")
  (:kind :binding :name "binding prefix S-Right" :table "prefix" :key "S-Right" :status :missing :cl-tmux-binding nil :tmux-version "tmux 3.6a" :source "list-keys -T prefix")
  (:kind :binding :name "binding copy-mode Escape" :table "copy-mode" :key "Escape" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode")
  (:kind :binding :name "binding copy-mode Space" :table "copy-mode" :key "Space" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode")
  (:kind :binding :name "binding copy-mode ," :table "copy-mode" :key "," :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode")
  (:kind :binding :name "binding copy-mode ;" :table "copy-mode" :key ";" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode")
  (:kind :binding :name "binding copy-mode F" :table "copy-mode" :key "F" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode")
  (:kind :binding :name "binding copy-mode N" :table "copy-mode" :key "N" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode")
  (:kind :binding :name "binding copy-mode P" :table "copy-mode" :key "P" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode")
  (:kind :binding :name "binding copy-mode R" :table "copy-mode" :key "R" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode")
  (:kind :binding :name "binding copy-mode T" :table "copy-mode" :key "T" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode")
  (:kind :binding :name "binding copy-mode X" :table "copy-mode" :key "X" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode")
  (:kind :binding :name "binding copy-mode f" :table "copy-mode" :key "f" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode")
  (:kind :binding :name "binding copy-mode g" :table "copy-mode" :key "g" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode")
  (:kind :binding :name "binding copy-mode n" :table "copy-mode" :key "n" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode")
  (:kind :binding :name "binding copy-mode q" :table "copy-mode" :key "q" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode")
  (:kind :binding :name "binding copy-mode r" :table "copy-mode" :key "r" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode")
  (:kind :binding :name "binding copy-mode t" :table "copy-mode" :key "t" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode")
  (:kind :binding :name "binding copy-mode MouseDown1Pane" :table "copy-mode" :key "MouseDown1Pane" :status :missing :cl-tmux-binding nil :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode")
  (:kind :binding :name "binding copy-mode MouseDrag1Pane" :table "copy-mode" :key "MouseDrag1Pane" :status :missing :cl-tmux-binding nil :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode")
  (:kind :binding :name "binding copy-mode MouseDragEnd1Pane" :table "copy-mode" :key "MouseDragEnd1Pane" :status :missing :cl-tmux-binding nil :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode")
  (:kind :binding :name "binding copy-mode WheelUpPane" :table "copy-mode" :key "WheelUpPane" :status :missing :cl-tmux-binding nil :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode")
  (:kind :binding :name "binding copy-mode WheelDownPane" :table "copy-mode" :key "WheelDownPane" :status :missing :cl-tmux-binding nil :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode")
  (:kind :binding :name "binding copy-mode DoubleClick1Pane" :table "copy-mode" :key "DoubleClick1Pane" :status :missing :cl-tmux-binding nil :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode")
  (:kind :binding :name "binding copy-mode TripleClick1Pane" :table "copy-mode" :key "TripleClick1Pane" :status :missing :cl-tmux-binding nil :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode")
  (:kind :binding :name "binding copy-mode Home" :table "copy-mode" :key "Home" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode")
  (:kind :binding :name "binding copy-mode End" :table "copy-mode" :key "End" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode")
  (:kind :binding :name "binding copy-mode NPage" :table "copy-mode" :key "NPage" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode")
  (:kind :binding :name "binding copy-mode PPage" :table "copy-mode" :key "PPage" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode")
  (:kind :binding :name "binding copy-mode Up" :table "copy-mode" :key "Up" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode")
  (:kind :binding :name "binding copy-mode Down" :table "copy-mode" :key "Down" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode")
  (:kind :binding :name "binding copy-mode Left" :table "copy-mode" :key "Left" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode")
  (:kind :binding :name "binding copy-mode Right" :table "copy-mode" :key "Right" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode")
  (:kind :binding :name "binding copy-mode M-1" :table "copy-mode" :key "M-1" :status :missing :cl-tmux-binding nil :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode")
  (:kind :binding :name "binding copy-mode M-2" :table "copy-mode" :key "M-2" :status :missing :cl-tmux-binding nil :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode")
  (:kind :binding :name "binding copy-mode M-3" :table "copy-mode" :key "M-3" :status :missing :cl-tmux-binding nil :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode")
  (:kind :binding :name "binding copy-mode M-4" :table "copy-mode" :key "M-4" :status :missing :cl-tmux-binding nil :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode")
  (:kind :binding :name "binding copy-mode M-5" :table "copy-mode" :key "M-5" :status :missing :cl-tmux-binding nil :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode")
  (:kind :binding :name "binding copy-mode M-6" :table "copy-mode" :key "M-6" :status :missing :cl-tmux-binding nil :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode")
  (:kind :binding :name "binding copy-mode M-7" :table "copy-mode" :key "M-7" :status :missing :cl-tmux-binding nil :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode")
  (:kind :binding :name "binding copy-mode M-8" :table "copy-mode" :key "M-8" :status :missing :cl-tmux-binding nil :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode")
  (:kind :binding :name "binding copy-mode M-9" :table "copy-mode" :key "M-9" :status :missing :cl-tmux-binding nil :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode")
  (:kind :binding :name "binding copy-mode M-<" :table "copy-mode" :key "M-<" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode")
  (:kind :binding :name "binding copy-mode M->" :table "copy-mode" :key "M->" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode")
  (:kind :binding :name "binding copy-mode M-R" :table "copy-mode" :key "M-R" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode")
  (:kind :binding :name "binding copy-mode M-b" :table "copy-mode" :key "M-b" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode")
  (:kind :binding :name "binding copy-mode M-f" :table "copy-mode" :key "M-f" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode")
  (:kind :binding :name "binding copy-mode M-l" :table "copy-mode" :key "M-l" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode")
  (:kind :binding :name "binding copy-mode M-m" :table "copy-mode" :key "M-m" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode")
  (:kind :binding :name "binding copy-mode M-r" :table "copy-mode" :key "M-r" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode")
  (:kind :binding :name "binding copy-mode M-v" :table "copy-mode" :key "M-v" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode")
  (:kind :binding :name "binding copy-mode M-w" :table "copy-mode" :key "M-w" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode")
  (:kind :binding :name "binding copy-mode M-x" :table "copy-mode" :key "M-x" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode")
  (:kind :binding :name "binding copy-mode M-{" :table "copy-mode" :key "M-{" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode")
  (:kind :binding :name "binding copy-mode M-}" :table "copy-mode" :key "M-}" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode")
  (:kind :binding :name "binding copy-mode M-Up" :table "copy-mode" :key "M-Up" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode")
  (:kind :binding :name "binding copy-mode M-Down" :table "copy-mode" :key "M-Down" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode")
  (:kind :binding :name "binding copy-mode C-Space" :table "copy-mode" :key "C-Space" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode")
  (:kind :binding :name "binding copy-mode C-a" :table "copy-mode" :key "C-a" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode")
  (:kind :binding :name "binding copy-mode C-b" :table "copy-mode" :key "C-b" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode")
  (:kind :binding :name "binding copy-mode C-c" :table "copy-mode" :key "C-c" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode")
  (:kind :binding :name "binding copy-mode C-e" :table "copy-mode" :key "C-e" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode")
  (:kind :binding :name "binding copy-mode C-f" :table "copy-mode" :key "C-f" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode")
  (:kind :binding :name "binding copy-mode C-g" :table "copy-mode" :key "C-g" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode")
  (:kind :binding :name "binding copy-mode C-k" :table "copy-mode" :key "C-k" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode")
  (:kind :binding :name "binding copy-mode C-l" :table "copy-mode" :key "C-l" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode")
  (:kind :binding :name "binding copy-mode C-n" :table "copy-mode" :key "C-n" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode")
  (:kind :binding :name "binding copy-mode C-p" :table "copy-mode" :key "C-p" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode")
  (:kind :binding :name "binding copy-mode C-r" :table "copy-mode" :key "C-r" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode")
  (:kind :binding :name "binding copy-mode C-s" :table "copy-mode" :key "C-s" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode")
  (:kind :binding :name "binding copy-mode C-v" :table "copy-mode" :key "C-v" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode")
  (:kind :binding :name "binding copy-mode C-w" :table "copy-mode" :key "C-w" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode")
  (:kind :binding :name "binding copy-mode C-Up" :table "copy-mode" :key "C-Up" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode")
  (:kind :binding :name "binding copy-mode C-Down" :table "copy-mode" :key "C-Down" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode")
  (:kind :binding :name "binding copy-mode C-M-b" :table "copy-mode" :key "C-M-b" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode")
  (:kind :binding :name "binding copy-mode C-M-f" :table "copy-mode" :key "C-M-f" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode")
  (:kind :binding :name "binding copy-mode-vi Enter" :table "copy-mode-vi" :key "Enter" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode-vi")
  (:kind :binding :name "binding copy-mode-vi Escape" :table "copy-mode-vi" :key "Escape" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode-vi")
  (:kind :binding :name "binding copy-mode-vi Space" :table "copy-mode-vi" :key "Space" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode-vi")
  (:kind :binding :name "binding copy-mode-vi #" :table "copy-mode-vi" :key "#" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode-vi")
  (:kind :binding :name "binding copy-mode-vi $" :table "copy-mode-vi" :key "$" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode-vi")
  (:kind :binding :name "binding copy-mode-vi %" :table "copy-mode-vi" :key "%" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode-vi")
  (:kind :binding :name "binding copy-mode-vi *" :table "copy-mode-vi" :key "*" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode-vi")
  (:kind :binding :name "binding copy-mode-vi ," :table "copy-mode-vi" :key "," :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode-vi")
  (:kind :binding :name "binding copy-mode-vi /" :table "copy-mode-vi" :key "/" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode-vi")
  (:kind :binding :name "binding copy-mode-vi 0" :table "copy-mode-vi" :key "0" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode-vi")
  (:kind :binding :name "binding copy-mode-vi 1" :table "copy-mode-vi" :key "1" :status :missing :cl-tmux-binding nil :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode-vi")
  (:kind :binding :name "binding copy-mode-vi 2" :table "copy-mode-vi" :key "2" :status :missing :cl-tmux-binding nil :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode-vi")
  (:kind :binding :name "binding copy-mode-vi 3" :table "copy-mode-vi" :key "3" :status :missing :cl-tmux-binding nil :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode-vi")
  (:kind :binding :name "binding copy-mode-vi 4" :table "copy-mode-vi" :key "4" :status :missing :cl-tmux-binding nil :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode-vi")
  (:kind :binding :name "binding copy-mode-vi 5" :table "copy-mode-vi" :key "5" :status :missing :cl-tmux-binding nil :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode-vi")
  (:kind :binding :name "binding copy-mode-vi 6" :table "copy-mode-vi" :key "6" :status :missing :cl-tmux-binding nil :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode-vi")
  (:kind :binding :name "binding copy-mode-vi 7" :table "copy-mode-vi" :key "7" :status :missing :cl-tmux-binding nil :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode-vi")
  (:kind :binding :name "binding copy-mode-vi 8" :table "copy-mode-vi" :key "8" :status :missing :cl-tmux-binding nil :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode-vi")
  (:kind :binding :name "binding copy-mode-vi 9" :table "copy-mode-vi" :key "9" :status :missing :cl-tmux-binding nil :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode-vi")
  (:kind :binding :name "binding copy-mode-vi :" :table "copy-mode-vi" :key ":" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode-vi")
  (:kind :binding :name "binding copy-mode-vi ;" :table "copy-mode-vi" :key ";" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode-vi")
  (:kind :binding :name "binding copy-mode-vi ?" :table "copy-mode-vi" :key "?" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode-vi")
  (:kind :binding :name "binding copy-mode-vi A" :table "copy-mode-vi" :key "A" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode-vi")
  (:kind :binding :name "binding copy-mode-vi B" :table "copy-mode-vi" :key "B" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode-vi")
  (:kind :binding :name "binding copy-mode-vi D" :table "copy-mode-vi" :key "D" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode-vi")
  (:kind :binding :name "binding copy-mode-vi E" :table "copy-mode-vi" :key "E" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode-vi")
  (:kind :binding :name "binding copy-mode-vi F" :table "copy-mode-vi" :key "F" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode-vi")
  (:kind :binding :name "binding copy-mode-vi G" :table "copy-mode-vi" :key "G" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode-vi")
  (:kind :binding :name "binding copy-mode-vi H" :table "copy-mode-vi" :key "H" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode-vi")
  (:kind :binding :name "binding copy-mode-vi J" :table "copy-mode-vi" :key "J" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode-vi")
  (:kind :binding :name "binding copy-mode-vi K" :table "copy-mode-vi" :key "K" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode-vi")
  (:kind :binding :name "binding copy-mode-vi L" :table "copy-mode-vi" :key "L" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode-vi")
  (:kind :binding :name "binding copy-mode-vi M" :table "copy-mode-vi" :key "M" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode-vi")
  (:kind :binding :name "binding copy-mode-vi N" :table "copy-mode-vi" :key "N" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode-vi")
  (:kind :binding :name "binding copy-mode-vi P" :table "copy-mode-vi" :key "P" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode-vi")
  (:kind :binding :name "binding copy-mode-vi T" :table "copy-mode-vi" :key "T" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode-vi")
  (:kind :binding :name "binding copy-mode-vi V" :table "copy-mode-vi" :key "V" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode-vi")
  (:kind :binding :name "binding copy-mode-vi W" :table "copy-mode-vi" :key "W" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode-vi")
  (:kind :binding :name "binding copy-mode-vi X" :table "copy-mode-vi" :key "X" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode-vi")
  (:kind :binding :name "binding copy-mode-vi ^" :table "copy-mode-vi" :key "^" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode-vi")
  (:kind :binding :name "binding copy-mode-vi b" :table "copy-mode-vi" :key "b" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode-vi")
  (:kind :binding :name "binding copy-mode-vi e" :table "copy-mode-vi" :key "e" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode-vi")
  (:kind :binding :name "binding copy-mode-vi f" :table "copy-mode-vi" :key "f" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode-vi")
  (:kind :binding :name "binding copy-mode-vi g" :table "copy-mode-vi" :key "g" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode-vi")
  (:kind :binding :name "binding copy-mode-vi h" :table "copy-mode-vi" :key "h" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode-vi")
  (:kind :binding :name "binding copy-mode-vi j" :table "copy-mode-vi" :key "j" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode-vi")
  (:kind :binding :name "binding copy-mode-vi k" :table "copy-mode-vi" :key "k" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode-vi")
  (:kind :binding :name "binding copy-mode-vi l" :table "copy-mode-vi" :key "l" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode-vi")
  (:kind :binding :name "binding copy-mode-vi n" :table "copy-mode-vi" :key "n" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode-vi")
  (:kind :binding :name "binding copy-mode-vi o" :table "copy-mode-vi" :key "o" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode-vi")
  (:kind :binding :name "binding copy-mode-vi q" :table "copy-mode-vi" :key "q" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode-vi")
  (:kind :binding :name "binding copy-mode-vi r" :table "copy-mode-vi" :key "r" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode-vi")
  (:kind :binding :name "binding copy-mode-vi t" :table "copy-mode-vi" :key "t" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode-vi")
  (:kind :binding :name "binding copy-mode-vi v" :table "copy-mode-vi" :key "v" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode-vi")
  (:kind :binding :name "binding copy-mode-vi w" :table "copy-mode-vi" :key "w" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode-vi")
  (:kind :binding :name "binding copy-mode-vi z" :table "copy-mode-vi" :key "z" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode-vi")
  (:kind :binding :name "binding copy-mode-vi {" :table "copy-mode-vi" :key "{" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode-vi")
  (:kind :binding :name "binding copy-mode-vi }" :table "copy-mode-vi" :key "}" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode-vi")
  (:kind :binding :name "binding copy-mode-vi MouseDown1Pane" :table "copy-mode-vi" :key "MouseDown1Pane" :status :missing :cl-tmux-binding nil :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode-vi")
  (:kind :binding :name "binding copy-mode-vi MouseDrag1Pane" :table "copy-mode-vi" :key "MouseDrag1Pane" :status :missing :cl-tmux-binding nil :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode-vi")
  (:kind :binding :name "binding copy-mode-vi MouseDragEnd1Pane" :table "copy-mode-vi" :key "MouseDragEnd1Pane" :status :missing :cl-tmux-binding nil :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode-vi")
  (:kind :binding :name "binding copy-mode-vi WheelUpPane" :table "copy-mode-vi" :key "WheelUpPane" :status :missing :cl-tmux-binding nil :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode-vi")
  (:kind :binding :name "binding copy-mode-vi WheelDownPane" :table "copy-mode-vi" :key "WheelDownPane" :status :missing :cl-tmux-binding nil :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode-vi")
  (:kind :binding :name "binding copy-mode-vi DoubleClick1Pane" :table "copy-mode-vi" :key "DoubleClick1Pane" :status :missing :cl-tmux-binding nil :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode-vi")
  (:kind :binding :name "binding copy-mode-vi TripleClick1Pane" :table "copy-mode-vi" :key "TripleClick1Pane" :status :missing :cl-tmux-binding nil :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode-vi")
  (:kind :binding :name "binding copy-mode-vi BSpace" :table "copy-mode-vi" :key "BSpace" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode-vi")
  (:kind :binding :name "binding copy-mode-vi Home" :table "copy-mode-vi" :key "Home" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode-vi")
  (:kind :binding :name "binding copy-mode-vi End" :table "copy-mode-vi" :key "End" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode-vi")
  (:kind :binding :name "binding copy-mode-vi NPage" :table "copy-mode-vi" :key "NPage" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode-vi")
  (:kind :binding :name "binding copy-mode-vi PPage" :table "copy-mode-vi" :key "PPage" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode-vi")
  (:kind :binding :name "binding copy-mode-vi Up" :table "copy-mode-vi" :key "Up" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode-vi")
  (:kind :binding :name "binding copy-mode-vi Down" :table "copy-mode-vi" :key "Down" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode-vi")
  (:kind :binding :name "binding copy-mode-vi Left" :table "copy-mode-vi" :key "Left" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode-vi")
  (:kind :binding :name "binding copy-mode-vi Right" :table "copy-mode-vi" :key "Right" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode-vi")
  (:kind :binding :name "binding copy-mode-vi M-x" :table "copy-mode-vi" :key "M-x" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode-vi")
  (:kind :binding :name "binding copy-mode-vi C-b" :table "copy-mode-vi" :key "C-b" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode-vi")
  (:kind :binding :name "binding copy-mode-vi C-c" :table "copy-mode-vi" :key "C-c" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode-vi")
  (:kind :binding :name "binding copy-mode-vi C-d" :table "copy-mode-vi" :key "C-d" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode-vi")
  (:kind :binding :name "binding copy-mode-vi C-e" :table "copy-mode-vi" :key "C-e" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode-vi")
  (:kind :binding :name "binding copy-mode-vi C-f" :table "copy-mode-vi" :key "C-f" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode-vi")
  (:kind :binding :name "binding copy-mode-vi C-h" :table "copy-mode-vi" :key "C-h" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode-vi")
  (:kind :binding :name "binding copy-mode-vi C-j" :table "copy-mode-vi" :key "C-j" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode-vi")
  (:kind :binding :name "binding copy-mode-vi C-u" :table "copy-mode-vi" :key "C-u" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode-vi")
  (:kind :binding :name "binding copy-mode-vi C-v" :table "copy-mode-vi" :key "C-v" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode-vi")
  (:kind :binding :name "binding copy-mode-vi C-y" :table "copy-mode-vi" :key "C-y" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode-vi")
  (:kind :binding :name "binding copy-mode-vi C-Up" :table "copy-mode-vi" :key "C-Up" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode-vi")
  (:kind :binding :name "binding copy-mode-vi C-Down" :table "copy-mode-vi" :key "C-Down" :status :partial :cl-tmux-binding t :tmux-version "tmux 3.6a" :source "list-keys -T copy-mode-vi")
  ;; End default key binding rows.
  (:kind :inventory
   :name "tmux global option inventory"
   :status :partial
   :tmux-command "tmux -L <clean> -f /dev/null show-options -g"
   :tmux-count 61
   :cl-tmux-evidence "cl-tmux options are not yet mapped row-by-row against tmux global/session defaults and types."
   :next-step "Generate option rows from tmux show-options -g and classify each option.")
  (:kind :inventory
   :name "tmux window option inventory"
   :status :partial
   :tmux-command "tmux show-window-options -g"
   :tmux-count 67
   :cl-tmux-evidence "cl-tmux window options are not yet mapped row-by-row against tmux defaults and types."
   :next-step "Generate option rows from tmux show-window-options -g and classify each option.")
  (:kind :inventory
   :name "tmux hook inventory"
   :status :partial
   :tmux-command "tmux show-hooks -g"
   :tmux-count 57
   :cl-tmux-count 27
   :cl-tmux-only-count 8
   :cl-tmux-evidence "tmux 3.6a show-hooks -g lists 57 hook names; 19 are present in cl-tmux hook event constants, 38 are absent, and 8 cl-tmux hook constants are not tmux global hook names. Firing semantics and hook format variables are not differentially proven."
   :next-step "Promote hook rows only after set-hook, show-hooks, run-hook, firing semantics, and hook format variable differential tests.")
  (:kind :hook :name "after-bind-key" :status :missing :cl-tmux-event nil :evidence "listed by tmux 3.6a show-hooks -g; absent from cl-tmux hook event constants")
  (:kind :hook :name "after-capture-pane" :status :missing :cl-tmux-event nil :evidence "listed by tmux 3.6a show-hooks -g; absent from cl-tmux hook event constants")
  (:kind :hook :name "after-copy-mode" :status :missing :cl-tmux-event nil :evidence "listed by tmux 3.6a show-hooks -g; absent from cl-tmux hook event constants")
  (:kind :hook :name "after-display-message" :status :missing :cl-tmux-event nil :evidence "listed by tmux 3.6a show-hooks -g; absent from cl-tmux hook event constants")
  (:kind :hook :name "after-display-panes" :status :missing :cl-tmux-event nil :evidence "listed by tmux 3.6a show-hooks -g; absent from cl-tmux hook event constants")
  (:kind :hook :name "after-kill-pane" :status :partial :cl-tmux-event t :evidence "present in cl-tmux hook event constants; firing semantics and hook format variables are not differentially proven")
  (:kind :hook :name "after-list-buffers" :status :missing :cl-tmux-event nil :evidence "listed by tmux 3.6a show-hooks -g; absent from cl-tmux hook event constants")
  (:kind :hook :name "after-list-clients" :status :missing :cl-tmux-event nil :evidence "listed by tmux 3.6a show-hooks -g; absent from cl-tmux hook event constants")
  (:kind :hook :name "after-list-keys" :status :missing :cl-tmux-event nil :evidence "listed by tmux 3.6a show-hooks -g; absent from cl-tmux hook event constants")
  (:kind :hook :name "after-list-panes" :status :missing :cl-tmux-event nil :evidence "listed by tmux 3.6a show-hooks -g; absent from cl-tmux hook event constants")
  (:kind :hook :name "after-list-sessions" :status :missing :cl-tmux-event nil :evidence "listed by tmux 3.6a show-hooks -g; absent from cl-tmux hook event constants")
  (:kind :hook :name "after-list-windows" :status :missing :cl-tmux-event nil :evidence "listed by tmux 3.6a show-hooks -g; absent from cl-tmux hook event constants")
  (:kind :hook :name "after-load-buffer" :status :missing :cl-tmux-event nil :evidence "listed by tmux 3.6a show-hooks -g; absent from cl-tmux hook event constants")
  (:kind :hook :name "after-lock-server" :status :missing :cl-tmux-event nil :evidence "listed by tmux 3.6a show-hooks -g; absent from cl-tmux hook event constants")
  (:kind :hook :name "after-new-session" :status :missing :cl-tmux-event nil :evidence "listed by tmux 3.6a show-hooks -g; absent from cl-tmux hook event constants")
  (:kind :hook :name "after-new-window" :status :partial :cl-tmux-event t :evidence "present in cl-tmux hook event constants; firing semantics and hook format variables are not differentially proven")
  (:kind :hook :name "after-paste-buffer" :status :missing :cl-tmux-event nil :evidence "listed by tmux 3.6a show-hooks -g; absent from cl-tmux hook event constants")
  (:kind :hook :name "after-pipe-pane" :status :missing :cl-tmux-event nil :evidence "listed by tmux 3.6a show-hooks -g; absent from cl-tmux hook event constants")
  (:kind :hook :name "after-queue" :status :missing :cl-tmux-event nil :evidence "listed by tmux 3.6a show-hooks -g; absent from cl-tmux hook event constants")
  (:kind :hook :name "after-refresh-client" :status :missing :cl-tmux-event nil :evidence "listed by tmux 3.6a show-hooks -g; absent from cl-tmux hook event constants")
  (:kind :hook :name "after-rename-session" :status :missing :cl-tmux-event nil :evidence "listed by tmux 3.6a show-hooks -g; absent from cl-tmux hook event constants")
  (:kind :hook :name "after-rename-window" :status :partial :cl-tmux-event t :evidence "present in cl-tmux hook event constants; firing semantics and hook format variables are not differentially proven")
  (:kind :hook :name "after-resize-pane" :status :partial :cl-tmux-event t :evidence "present in cl-tmux hook event constants; firing semantics and hook format variables are not differentially proven")
  (:kind :hook :name "after-resize-window" :status :missing :cl-tmux-event nil :evidence "listed by tmux 3.6a show-hooks -g; absent from cl-tmux hook event constants")
  (:kind :hook :name "after-save-buffer" :status :missing :cl-tmux-event nil :evidence "listed by tmux 3.6a show-hooks -g; absent from cl-tmux hook event constants")
  (:kind :hook :name "after-select-layout" :status :missing :cl-tmux-event nil :evidence "listed by tmux 3.6a show-hooks -g; absent from cl-tmux hook event constants")
  (:kind :hook :name "after-select-pane" :status :partial :cl-tmux-event t :evidence "present in cl-tmux hook event constants; firing semantics and hook format variables are not differentially proven")
  (:kind :hook :name "after-select-window" :status :partial :cl-tmux-event t :evidence "present in cl-tmux hook event constants; firing semantics and hook format variables are not differentially proven")
  (:kind :hook :name "after-send-keys" :status :missing :cl-tmux-event nil :evidence "listed by tmux 3.6a show-hooks -g; absent from cl-tmux hook event constants")
  (:kind :hook :name "after-set-buffer" :status :missing :cl-tmux-event nil :evidence "listed by tmux 3.6a show-hooks -g; absent from cl-tmux hook event constants")
  (:kind :hook :name "after-set-environment" :status :missing :cl-tmux-event nil :evidence "listed by tmux 3.6a show-hooks -g; absent from cl-tmux hook event constants")
  (:kind :hook :name "after-set-hook" :status :missing :cl-tmux-event nil :evidence "listed by tmux 3.6a show-hooks -g; absent from cl-tmux hook event constants")
  (:kind :hook :name "after-set-option" :status :missing :cl-tmux-event nil :evidence "listed by tmux 3.6a show-hooks -g; absent from cl-tmux hook event constants")
  (:kind :hook :name "after-show-environment" :status :missing :cl-tmux-event nil :evidence "listed by tmux 3.6a show-hooks -g; absent from cl-tmux hook event constants")
  (:kind :hook :name "after-show-messages" :status :missing :cl-tmux-event nil :evidence "listed by tmux 3.6a show-hooks -g; absent from cl-tmux hook event constants")
  (:kind :hook :name "after-show-options" :status :missing :cl-tmux-event nil :evidence "listed by tmux 3.6a show-hooks -g; absent from cl-tmux hook event constants")
  (:kind :hook :name "after-split-window" :status :partial :cl-tmux-event t :evidence "present in cl-tmux hook event constants; firing semantics and hook format variables are not differentially proven")
  (:kind :hook :name "after-unbind-key" :status :missing :cl-tmux-event nil :evidence "listed by tmux 3.6a show-hooks -g; absent from cl-tmux hook event constants")
  (:kind :hook :name "alert-activity" :status :partial :cl-tmux-event t :evidence "present in cl-tmux hook event constants; firing semantics and hook format variables are not differentially proven")
  (:kind :hook :name "alert-bell" :status :partial :cl-tmux-event t :evidence "present in cl-tmux hook event constants; firing semantics and hook format variables are not differentially proven")
  (:kind :hook :name "alert-silence" :status :partial :cl-tmux-event t :evidence "present in cl-tmux hook event constants; firing semantics and hook format variables are not differentially proven")
  (:kind :hook :name "client-active" :status :missing :cl-tmux-event nil :evidence "listed by tmux 3.6a show-hooks -g; absent from cl-tmux hook event constants")
  (:kind :hook :name "client-attached" :status :partial :cl-tmux-event t :evidence "present in cl-tmux hook event constants; firing semantics and hook format variables are not differentially proven")
  (:kind :hook :name "client-detached" :status :partial :cl-tmux-event t :evidence "present in cl-tmux hook event constants; firing semantics and hook format variables are not differentially proven")
  (:kind :hook :name "client-focus-in" :status :missing :cl-tmux-event nil :evidence "listed by tmux 3.6a show-hooks -g; absent from cl-tmux hook event constants")
  (:kind :hook :name "client-focus-out" :status :missing :cl-tmux-event nil :evidence "listed by tmux 3.6a show-hooks -g; absent from cl-tmux hook event constants")
  (:kind :hook :name "client-resized" :status :partial :cl-tmux-event t :evidence "present in cl-tmux hook event constants; firing semantics and hook format variables are not differentially proven")
  (:kind :hook :name "client-session-changed" :status :missing :cl-tmux-event nil :evidence "listed by tmux 3.6a show-hooks -g; absent from cl-tmux hook event constants")
  (:kind :hook :name "client-light-theme" :status :missing :cl-tmux-event nil :evidence "listed by tmux 3.6a show-hooks -g; absent from cl-tmux hook event constants")
  (:kind :hook :name "client-dark-theme" :status :missing :cl-tmux-event nil :evidence "listed by tmux 3.6a show-hooks -g; absent from cl-tmux hook event constants")
  (:kind :hook :name "command-error" :status :missing :cl-tmux-event nil :evidence "listed by tmux 3.6a show-hooks -g; absent from cl-tmux hook event constants")
  (:kind :hook :name "session-closed" :status :partial :cl-tmux-event t :evidence "present in cl-tmux hook event constants; firing semantics and hook format variables are not differentially proven")
  (:kind :hook :name "session-created" :status :partial :cl-tmux-event t :evidence "present in cl-tmux hook event constants; firing semantics and hook format variables are not differentially proven")
  (:kind :hook :name "session-renamed" :status :partial :cl-tmux-event t :evidence "present in cl-tmux hook event constants; firing semantics and hook format variables are not differentially proven")
  (:kind :hook :name "session-window-changed" :status :partial :cl-tmux-event t :evidence "present in cl-tmux hook event constants; firing semantics and hook format variables are not differentially proven")
  (:kind :hook :name "window-linked" :status :partial :cl-tmux-event t :evidence "present in cl-tmux hook event constants; firing semantics and hook format variables are not differentially proven")
  (:kind :hook :name "window-unlinked" :status :partial :cl-tmux-event t :evidence "present in cl-tmux hook event constants; firing semantics and hook format variables are not differentially proven")
  (:kind :inventory
   :name "tmux format variable inventory"
   :status :partial
   :tmux-command "tmux 3.6a man FORMATS plus display-menu popup variable table"
   :tmux-count 226
   :cl-tmux-count 81
   :cl-tmux-evidence "tmux 3.6a documents 226 format variables; 74 are present in cl-tmux format-context-from-session and 152 are absent. Value behavior is not yet differentially proven."
   :next-step "Promote format rows only after sampled display-message evaluations prove values, aliases, modifiers, and context-specific behavior.")
   (:kind :format :name "active_window_index" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "alternate_on" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "alternate_saved_x" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "alternate_saved_y" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "buffer_created" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "buffer_full" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "buffer_name" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "buffer_sample" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "buffer_size" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "client_activity" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "client_cell_height" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "client_cell_width" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "client_control_mode" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "client_created" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "client_discarded" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "client_flags" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "client_height" :status :partial :cl-tmux-context t :evidence "present in cl-tmux format context; value behavior is not differentially proven")
   (:kind :format :name "client_key_table" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "client_last_session" :status :partial :cl-tmux-context t :evidence "present in cl-tmux format context; value behavior is not differentially proven")
   (:kind :format :name "client_name" :status :partial :cl-tmux-context t :evidence "present in cl-tmux format context; value behavior is not differentially proven")
   (:kind :format :name "client_pid" :status :partial :cl-tmux-context t :evidence "present in cl-tmux format context; value behavior is not differentially proven")
   (:kind :format :name "client_prefix" :status :partial :cl-tmux-context t :evidence "present in cl-tmux format context; value behavior is not differentially proven")
   (:kind :format :name "client_readonly" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "client_session" :status :partial :cl-tmux-context t :evidence "present in cl-tmux format context; value behavior is not differentially proven")
   (:kind :format :name "client_termfeatures" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "client_termname" :status :partial :cl-tmux-context t :evidence "present in cl-tmux format context; value behavior is not differentially proven")
   (:kind :format :name "client_termtype" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "client_tty" :status :partial :cl-tmux-context t :evidence "present in cl-tmux format context; value behavior is not differentially proven")
   (:kind :format :name "client_uid" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "client_user" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "client_utf8" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "client_width" :status :partial :cl-tmux-context t :evidence "present in cl-tmux format context; value behavior is not differentially proven")
   (:kind :format :name "client_written" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "command" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "command_list_alias" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "command_list_name" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "command_list_usage" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "config_files" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "copy_cursor_hyperlink" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "copy_cursor_line" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "copy_cursor_word" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "copy_cursor_x" :status :partial :cl-tmux-context t :evidence "present in cl-tmux format context; value behavior is not differentially proven")
   (:kind :format :name "copy_cursor_y" :status :partial :cl-tmux-context t :evidence "present in cl-tmux format context; value behavior is not differentially proven")
   (:kind :format :name "current_file" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "cursor_blinking" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "cursor_character" :status :partial :cl-tmux-context t :evidence "present in cl-tmux format context; value behavior is not differentially proven")
   (:kind :format :name "cursor_colour" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "cursor_flag" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "cursor_shape" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "cursor_very_visible" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "cursor_x" :status :partial :cl-tmux-context t :evidence "present in cl-tmux format context; value behavior is not differentially proven")
   (:kind :format :name "cursor_y" :status :partial :cl-tmux-context t :evidence "present in cl-tmux format context; value behavior is not differentially proven")
   (:kind :format :name "history_bytes" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "history_limit" :status :partial :cl-tmux-context t :evidence "present in cl-tmux format context; value behavior is not differentially proven")
   (:kind :format :name "history_size" :status :partial :cl-tmux-context t :evidence "present in cl-tmux format context; value behavior is not differentially proven")
   (:kind :format :name "hook" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "hook_client" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "hook_pane" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "hook_session" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "hook_session_name" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "hook_window" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "hook_window_name" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "host" :status :partial :cl-tmux-context t :evidence "present in cl-tmux format context; value behavior is not differentially proven")
   (:kind :format :name "host_short" :status :partial :cl-tmux-context t :evidence "present in cl-tmux format context; value behavior is not differentially proven")
   (:kind :format :name "insert_flag" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "keypad_cursor_flag" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "keypad_flag" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "last_session_index" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "last_window_index" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "line" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "loop_last_flag" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "mouse_all_flag" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "mouse_any_flag" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "mouse_button_flag" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "mouse_hyperlink" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "mouse_line" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "mouse_sgr_flag" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "mouse_standard_flag" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "mouse_status_line" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "mouse_status_range" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "mouse_utf8_flag" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "mouse_word" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "mouse_x" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "mouse_y" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "next_session_id" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "origin_flag" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "pane_active" :status :partial :cl-tmux-context t :evidence "present in cl-tmux format context; value behavior is not differentially proven")
   (:kind :format :name "pane_at_bottom" :status :partial :cl-tmux-context t :evidence "present in cl-tmux format context; value behavior is not differentially proven")
   (:kind :format :name "pane_at_left" :status :partial :cl-tmux-context t :evidence "present in cl-tmux format context; value behavior is not differentially proven")
   (:kind :format :name "pane_at_right" :status :partial :cl-tmux-context t :evidence "present in cl-tmux format context; value behavior is not differentially proven")
   (:kind :format :name "pane_at_top" :status :partial :cl-tmux-context t :evidence "present in cl-tmux format context; value behavior is not differentially proven")
   (:kind :format :name "pane_bg" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "pane_bottom" :status :partial :cl-tmux-context t :evidence "present in cl-tmux format context; value behavior is not differentially proven")
   (:kind :format :name "pane_current_command" :status :partial :cl-tmux-context t :evidence "present in cl-tmux format context; value behavior is not differentially proven")
   (:kind :format :name "pane_current_path" :status :partial :cl-tmux-context t :evidence "present in cl-tmux format context; value behavior is not differentially proven")
   (:kind :format :name "pane_dead" :status :partial :cl-tmux-context t :evidence "present in cl-tmux format context; value behavior is not differentially proven")
   (:kind :format :name "pane_dead_signal" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "pane_dead_status" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "pane_dead_time" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "pane_fg" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "pane_format" :status :partial :cl-tmux-context t :evidence "present in cl-tmux format context; value behavior is not differentially proven")
   (:kind :format :name "pane_height" :status :partial :cl-tmux-context t :evidence "present in cl-tmux format context; value behavior is not differentially proven")
   (:kind :format :name "pane_id" :status :partial :cl-tmux-context t :evidence "present in cl-tmux format context; value behavior is not differentially proven")
   (:kind :format :name "pane_in_mode" :status :partial :cl-tmux-context t :evidence "present in cl-tmux format context; value behavior is not differentially proven")
   (:kind :format :name "pane_index" :status :partial :cl-tmux-context t :evidence "present in cl-tmux format context; value behavior is not differentially proven")
   (:kind :format :name "pane_input_off" :status :partial :cl-tmux-context t :evidence "present in cl-tmux format context; value behavior is not differentially proven")
   (:kind :format :name "pane_key_mode" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "pane_last" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "pane_left" :status :partial :cl-tmux-context t :evidence "present in cl-tmux format context; value behavior is not differentially proven")
   (:kind :format :name "pane_marked" :status :partial :cl-tmux-context t :evidence "present in cl-tmux format context; value behavior is not differentially proven")
   (:kind :format :name "pane_marked_set" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "pane_mode" :status :partial :cl-tmux-context t :evidence "present in cl-tmux format context; value behavior is not differentially proven")
   (:kind :format :name "pane_path" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "pane_pid" :status :partial :cl-tmux-context t :evidence "present in cl-tmux format context; value behavior is not differentially proven")
   (:kind :format :name "pane_pipe" :status :partial :cl-tmux-context t :evidence "present in cl-tmux format context; value behavior is not differentially proven")
   (:kind :format :name "pane_right" :status :partial :cl-tmux-context t :evidence "present in cl-tmux format context; value behavior is not differentially proven")
   (:kind :format :name "pane_search_string" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "pane_start_command" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "pane_start_path" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "pane_synchronized" :status :partial :cl-tmux-context t :evidence "present in cl-tmux format context; value behavior is not differentially proven")
   (:kind :format :name "pane_tabs" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "pane_title" :status :partial :cl-tmux-context t :evidence "present in cl-tmux format context; value behavior is not differentially proven")
   (:kind :format :name "pane_top" :status :partial :cl-tmux-context t :evidence "present in cl-tmux format context; value behavior is not differentially proven")
   (:kind :format :name "pane_tty" :status :partial :cl-tmux-context t :evidence "present in cl-tmux format context; value behavior is not differentially proven")
   (:kind :format :name "pane_unseen_changes" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "pane_width" :status :partial :cl-tmux-context t :evidence "present in cl-tmux format context; value behavior is not differentially proven")
   (:kind :format :name "pid" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "popup_centre_x" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "popup_centre_y" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "popup_height" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "popup_mouse_bottom" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "popup_mouse_centre_x" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "popup_mouse_centre_y" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "popup_mouse_top" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "popup_mouse_x" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "popup_mouse_y" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "popup_pane_bottom" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "popup_pane_left" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "popup_pane_right" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "popup_pane_top" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "popup_status_line_y" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "popup_width" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "popup_window_status_line_x" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "popup_window_status_line_y" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "rectangle_toggle" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "scroll_position" :status :partial :cl-tmux-context t :evidence "present in cl-tmux format context; value behavior is not differentially proven")
   (:kind :format :name "scroll_region_lower" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "scroll_region_upper" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "search_count" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "search_count_partial" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "search_match" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "search_present" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "selection_active" :status :partial :cl-tmux-context t :evidence "present in cl-tmux format context; value behavior is not differentially proven")
   (:kind :format :name "selection_end_x" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "selection_end_y" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "selection_present" :status :partial :cl-tmux-context t :evidence "present in cl-tmux format context; value behavior is not differentially proven")
   (:kind :format :name "selection_start_x" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "selection_start_y" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "server_sessions" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "session_active" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "session_activity" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "session_activity_flag" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "session_alerts" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "session_attached" :status :partial :cl-tmux-context t :evidence "present in cl-tmux format context; value behavior is not differentially proven")
   (:kind :format :name "session_attached_list" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "session_bell_flag" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "session_created" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "session_format" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "session_group" :status :partial :cl-tmux-context t :evidence "present in cl-tmux format context; value behavior is not differentially proven")
   (:kind :format :name "session_group_attached" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "session_group_attached_list" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "session_group_list" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "session_group_many_attached" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "session_group_size" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "session_grouped" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "session_id" :status :partial :cl-tmux-context t :evidence "present in cl-tmux format context; value behavior is not differentially proven")
   (:kind :format :name "session_index" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "session_last_attached" :status :partial :cl-tmux-context t :evidence "present in cl-tmux format context; value behavior is not differentially proven")
   (:kind :format :name "session_many_attached" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "session_marked" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "session_name" :status :partial :cl-tmux-context t :evidence "present in cl-tmux format context; value behavior is not differentially proven")
   (:kind :format :name "session_path" :status :partial :cl-tmux-context t :evidence "present in cl-tmux format context; value behavior is not differentially proven")
   (:kind :format :name "session_silence_flag" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "session_stack" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "session_windows" :status :partial :cl-tmux-context t :evidence "present in cl-tmux format context; value behavior is not differentially proven")
   (:kind :format :name "sixel_support" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "socket_path" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "start_time" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "uid" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "user" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "version" :status :partial :cl-tmux-context t :evidence "present in cl-tmux format context; value behavior is not differentially proven")
   (:kind :format :name "window_active" :status :partial :cl-tmux-context t :evidence "present in cl-tmux format context; value behavior is not differentially proven")
   (:kind :format :name "window_active_clients" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "window_active_clients_list" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "window_active_sessions" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "window_active_sessions_list" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "window_activity" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "window_activity_flag" :status :partial :cl-tmux-context t :evidence "present in cl-tmux format context; value behavior is not differentially proven")
   (:kind :format :name "window_bell_flag" :status :partial :cl-tmux-context t :evidence "present in cl-tmux format context; value behavior is not differentially proven")
   (:kind :format :name "window_bigger" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "window_cell_height" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "window_cell_width" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "window_end_flag" :status :partial :cl-tmux-context t :evidence "present in cl-tmux format context; value behavior is not differentially proven")
   (:kind :format :name "window_flags" :status :partial :cl-tmux-context t :evidence "present in cl-tmux format context; value behavior is not differentially proven")
   (:kind :format :name "window_format" :status :partial :cl-tmux-context t :evidence "present in cl-tmux format context; value behavior is not differentially proven")
   (:kind :format :name "window_height" :status :partial :cl-tmux-context t :evidence "present in cl-tmux format context; value behavior is not differentially proven")
   (:kind :format :name "window_id" :status :partial :cl-tmux-context t :evidence "present in cl-tmux format context; value behavior is not differentially proven")
   (:kind :format :name "window_index" :status :partial :cl-tmux-context t :evidence "present in cl-tmux format context; value behavior is not differentially proven")
   (:kind :format :name "window_last_flag" :status :partial :cl-tmux-context t :evidence "present in cl-tmux format context; value behavior is not differentially proven")
   (:kind :format :name "window_layout" :status :partial :cl-tmux-context t :evidence "present in cl-tmux format context; value behavior is not differentially proven")
   (:kind :format :name "window_linked" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "window_linked_sessions" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "window_linked_sessions_list" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "window_marked_flag" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "window_name" :status :partial :cl-tmux-context t :evidence "present in cl-tmux format context; value behavior is not differentially proven")
   (:kind :format :name "window_offset_x" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "window_offset_y" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "window_panes" :status :partial :cl-tmux-context t :evidence "present in cl-tmux format context; value behavior is not differentially proven")
   (:kind :format :name "window_raw_flags" :status :partial :cl-tmux-context t :evidence "present in cl-tmux format context; value behavior is not differentially proven")
   (:kind :format :name "window_silence_flag" :status :partial :cl-tmux-context t :evidence "present in cl-tmux format context; value behavior is not differentially proven")
   (:kind :format :name "window_stack_index" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")
   (:kind :format :name "window_start_flag" :status :partial :cl-tmux-context t :evidence "present in cl-tmux format context; value behavior is not differentially proven")
   (:kind :format :name "window_visible_layout" :status :partial :cl-tmux-context t :evidence "present in cl-tmux format context; value behavior is not differentially proven")
   (:kind :format :name "window_width" :status :partial :cl-tmux-context t :evidence "present in cl-tmux format context; value behavior is not differentially proven")
   (:kind :format :name "window_zoomed_flag" :status :partial :cl-tmux-context t :evidence "present in cl-tmux format context; value behavior is not differentially proven")
   (:kind :format :name "wrap_flag" :status :missing :cl-tmux-context nil :evidence "listed by tmux 3.6a manpage; absent from cl-tmux format context")

  (:kind :option
   :name "activity-action"
   :scope :global
   :status :match
   :tmux-command "tmux show-options -g activity-action"
   :tmux-default "other"
   :cl-tmux-default "other")
  (:kind :option
   :name "assume-paste-time"
   :scope :global
   :status :missing
   :tmux-command "tmux show-options -g assume-paste-time"
   :tmux-default "1"
   :cl-tmux-default nil)
  (:kind :option
   :name "base-index"
   :scope :global
   :status :match
   :tmux-command "tmux show-options -g base-index"
   :tmux-default "0"
   :cl-tmux-default "0")
  (:kind :option
   :name "bell-action"
   :scope :global
   :status :match
   :tmux-command "tmux show-options -g bell-action"
   :tmux-default "any"
   :cl-tmux-default "any")
  (:kind :option
   :name "default-command"
   :scope :global
   :status :match
   :tmux-command "tmux show-options -g default-command"
   :tmux-default ""
   :cl-tmux-default "")
  (:kind :option
   :name "default-shell"
   :scope :global
   :status :partial
   :tmux-command "tmux show-options -g default-shell"
   :tmux-default "/nix/store/k2ym45laiqq93jj5dxr342yz493vlb55-fish-4.7.1/bin/fish"
   :cl-tmux-default "/bin/sh")
  (:kind :option
   :name "default-size"
   :scope :global
   :status :match
   :tmux-command "tmux show-options -g default-size"
   :tmux-default "80x24"
   :cl-tmux-default "80x24")
  (:kind :option
   :name "destroy-unattached"
   :scope :global
   :status :match
   :tmux-command "tmux show-options -g destroy-unattached"
   :tmux-default "off"
   :cl-tmux-default "off")
  (:kind :option
   :name "detach-on-destroy"
   :scope :global
   :status :match
   :tmux-command "tmux show-options -g detach-on-destroy"
   :tmux-default "on"
   :cl-tmux-default "on")
  (:kind :option
   :name "display-panes-active-colour"
   :scope :global
   :status :match
   :tmux-command "tmux show-options -g display-panes-active-colour"
   :tmux-default "red"
   :cl-tmux-default "red")
  (:kind :option
   :name "display-panes-colour"
   :scope :global
   :status :match
   :tmux-command "tmux show-options -g display-panes-colour"
   :tmux-default "blue"
   :cl-tmux-default "blue")
  (:kind :option
   :name "display-panes-time"
   :scope :global
   :status :match
   :tmux-command "tmux show-options -g display-panes-time"
   :tmux-default "1000"
   :cl-tmux-default "1000")
  (:kind :option
   :name "display-time"
   :scope :global
   :status :match
   :tmux-command "tmux show-options -g display-time"
   :tmux-default "750"
   :cl-tmux-default "750")
  (:kind :option
   :name "history-limit"
   :scope :global
   :status :match
   :tmux-command "tmux show-options -g history-limit"
   :tmux-default "2000"
   :cl-tmux-default "2000")
  (:kind :option
   :name "initial-repeat-time"
   :scope :global
   :status :missing
   :tmux-command "tmux show-options -g initial-repeat-time"
   :tmux-default "0"
   :cl-tmux-default nil)
  (:kind :option
   :name "key-table"
   :scope :global
   :status :partial
   :tmux-command "tmux show-options -g key-table"
   :tmux-default "root"
   :cl-tmux-default "prefix")
  (:kind :option
   :name "lock-after-time"
   :scope :global
   :status :match
   :tmux-command "tmux show-options -g lock-after-time"
   :tmux-default "0"
   :cl-tmux-default "0")
  (:kind :option
   :name "lock-command"
   :scope :global
   :status :missing
   :tmux-command "tmux show-options -g lock-command"
   :tmux-default "lock -np"
   :cl-tmux-default nil)
  (:kind :option
   :name "message-command-style"
   :scope :global
   :status :partial
   :tmux-command "tmux show-options -g message-command-style"
   :tmux-default "bg=black,fg=yellow"
   :cl-tmux-default "")
  (:kind :option
   :name "message-line"
   :scope :global
   :status :missing
   :tmux-command "tmux show-options -g message-line"
   :tmux-default "0"
   :cl-tmux-default nil)
  (:kind :option
   :name "message-style"
   :scope :global
   :status :partial
   :tmux-command "tmux show-options -g message-style"
   :tmux-default "bg=yellow,fg=black"
   :cl-tmux-default "")
  (:kind :option
   :name "mouse"
   :scope :global
   :status :match
   :tmux-command "tmux show-options -g mouse"
   :tmux-default "off"
   :cl-tmux-default "off")
  (:kind :option
   :name "prefix"
   :scope :global
   :status :missing
   :tmux-command "tmux show-options -g prefix"
   :tmux-default "C-b"
   :cl-tmux-default nil)
  (:kind :option
   :name "prefix2"
   :scope :global
   :status :partial
   :tmux-command "tmux show-options -g prefix2"
   :tmux-default "None"
   :cl-tmux-default "")
  (:kind :option
   :name "renumber-windows"
   :scope :global
   :status :match
   :tmux-command "tmux show-options -g renumber-windows"
   :tmux-default "off"
   :cl-tmux-default "off")
  (:kind :option
   :name "repeat-time"
   :scope :global
   :status :match
   :tmux-command "tmux show-options -g repeat-time"
   :tmux-default "500"
   :cl-tmux-default "500")
  (:kind :option
   :name "set-titles"
   :scope :global
   :status :match
   :tmux-command "tmux show-options -g set-titles"
   :tmux-default "off"
   :cl-tmux-default "off")
  (:kind :option
   :name "set-titles-string"
   :scope :global
   :status :partial
   :tmux-command "tmux show-options -g set-titles-string"
   :tmux-default "#S:#I:#W - \"#T\" #{session_alerts}"
   :cl-tmux-default "#S:#I:#W")
  (:kind :option
   :name "silence-action"
   :scope :global
   :status :match
   :tmux-command "tmux show-options -g silence-action"
   :tmux-default "other"
   :cl-tmux-default "other")
  (:kind :option
   :name "status"
   :scope :global
   :status :match
   :tmux-command "tmux show-options -g status"
   :tmux-default "on"
   :cl-tmux-default "on")
  (:kind :option
   :name "status-bg"
   :scope :global
   :status :missing
   :tmux-command "tmux show-options -g status-bg"
   :tmux-default "default"
   :cl-tmux-default nil)
  (:kind :option
   :name "status-fg"
   :scope :global
   :status :missing
   :tmux-command "tmux show-options -g status-fg"
   :tmux-default "default"
   :cl-tmux-default nil)
  (:kind :option
   :name "status-format[0]"
   :scope :global
   :status :missing
   :tmux-command "tmux show-options -g status-format[0]"
   :tmux-default "#[align=left range=left #{E:status-left-style}]#[push-default]#{T;=/#{status-left-length}:status-left}#[pop-default]#[norange default]#[list=on align=#{status-justify}]#[list=left-marker]<#[list=right-marker]>#[list=on]#{W:#[range=window|#{window_index} #{E:window-status-style}#{?#{&&:#{window_last_flag},#{!=:#{E:window-status-last-style},default}}, #{E:window-status-last-style},}#{?#{&&:#{window_bell_flag},#{!=:#{E:window-status-bell-style},default}}, #{E:window-status-bell-style},#{?#{&&:#{||:#{window_activity_flag},#{window_silence_flag}},#{!=:#{E:window-status-activity-style},default}}, #{E:window-status-activity-style},}}]#[push-default]#{T:window-status-format}#[pop-default]#[norange default]#{?loop_last_flag,,#{window-status-separator}},#[range=window|#{window_index} list=focus #{?#{!=:#{E:window-status-current-style},default},#{E:window-status-current-style},#{E:window-status-style}}#{?#{&&:#{window_last_flag},#{!=:#{E:window-status-last-style},default}}, #{E:window-status-last-style},}#{?#{&&:#{window_bell_flag},#{!=:#{E:window-status-bell-style},default}}, #{E:window-status-bell-style},#{?#{&&:#{||:#{window_activity_flag},#{window_silence_flag}},#{!=:#{E:window-status-activity-style},default}}, #{E:window-status-activity-style},}}]#[push-default]#{T:window-status-current-format}#[pop-default]#[norange list=on default]#{?loop_last_flag,,#{window-status-separator}}}#[nolist align=right range=right #{E:status-right-style}]#[push-default]#{T;=/#{status-right-length}:status-right}#[pop-default]#[norange default]"
   :cl-tmux-default nil)
  (:kind :option
   :name "status-format[1]"
   :scope :global
   :status :missing
   :tmux-command "tmux show-options -g status-format[1]"
   :tmux-default "#[align=left]#{R: ,#{n:#{session_name}}}P: #[norange default]#[list=on align=#{status-justify}]#[list=left-marker]<#[list=right-marker]>#[list=on]#{P:#[range=pane|#{pane_id} #{E:pane-status-style}]#[push-default]#P[#{pane_width}x#{pane_height}]#[pop-default]#[norange list=on default]  ,#[range=pane|#{pane_id} list=focus #{?#{!=:#{E:pane-status-current-style},default},#{E:pane-status-current-style},#{E:pane-status-style}}]#[push-default]#P[#{pane_width}x#{pane_height}]*#[pop-default]#[norange list=on default] }"
   :cl-tmux-default nil)
  (:kind :option
   :name "status-format[2]"
   :scope :global
   :status :missing
   :tmux-command "tmux show-options -g status-format[2]"
   :tmux-default "#[align=left]#{R: ,#{n:#{session_name}}}S: #[norange default]#[list=on align=#{status-justify}]#[list=left-marker]<#[list=right-marker]>#[list=on]#{S:#[range=session|#{session_id} #{E:session-status-style}]#[push-default]#S#{session_alert}#[pop-default]#[norange list=on default]  ,#[range=session|#{session_id} list=focus #{?#{!=:#{E:session-status-current-style},default},#{E:session-status-current-style},#{E:session-status-style}}]#[push-default]#S*#{session_alert}#[pop-default]#[norange list=on default] }"
   :cl-tmux-default nil)
  (:kind :option
   :name "status-interval"
   :scope :global
   :status :match
   :tmux-command "tmux show-options -g status-interval"
   :tmux-default "15"
   :cl-tmux-default "15")
  (:kind :option
   :name "status-justify"
   :scope :global
   :status :match
   :tmux-command "tmux show-options -g status-justify"
   :tmux-default "left"
   :cl-tmux-default "left")
  (:kind :option
   :name "status-keys"
   :scope :global
   :status :match
   :tmux-command "tmux show-options -g status-keys"
   :tmux-default "emacs"
   :cl-tmux-default "emacs")
  (:kind :option
   :name "status-left"
   :scope :global
   :status :partial
   :tmux-command "tmux show-options -g status-left"
   :tmux-default "[#{session_name}] "
   :cl-tmux-default "[#{session_name}]")
  (:kind :option
   :name "status-left-length"
   :scope :global
   :status :partial
   :tmux-command "tmux show-options -g status-left-length"
   :tmux-default "10"
   :cl-tmux-default "40")
  (:kind :option
   :name "status-left-style"
   :scope :global
   :status :partial
   :tmux-command "tmux show-options -g status-left-style"
   :tmux-default "default"
   :cl-tmux-default "")
  (:kind :option
   :name "status-position"
   :scope :global
   :status :match
   :tmux-command "tmux show-options -g status-position"
   :tmux-default "bottom"
   :cl-tmux-default "bottom")
  (:kind :option
   :name "status-right"
   :scope :global
   :status :partial
   :tmux-command "tmux show-options -g status-right"
   :tmux-default "#{?window_bigger,[#{window_offset_x}#,#{window_offset_y}] ,}\"#{=21:pane_title}\" %H:%M %d-%b-%y"
   :cl-tmux-default "#{time}")
  (:kind :option
   :name "status-right-length"
   :scope :global
   :status :match
   :tmux-command "tmux show-options -g status-right-length"
   :tmux-default "40"
   :cl-tmux-default "40")
  (:kind :option
   :name "status-right-style"
   :scope :global
   :status :partial
   :tmux-command "tmux show-options -g status-right-style"
   :tmux-default "default"
   :cl-tmux-default "")
  (:kind :option
   :name "status-style"
   :scope :global
   :status :partial
   :tmux-command "tmux show-options -g status-style"
   :tmux-default "bg=green,fg=black"
   :cl-tmux-default "")
  (:kind :option
   :name "prompt-cursor-colour"
   :scope :global
   :status :missing
   :tmux-command "tmux show-options -g prompt-cursor-colour"
   :tmux-default "cyan"
   :cl-tmux-default nil)
  (:kind :option
   :name "prompt-cursor-style"
   :scope :global
   :status :missing
   :tmux-command "tmux show-options -g prompt-cursor-style"
   :tmux-default "default"
   :cl-tmux-default nil)
  (:kind :option
   :name "update-environment[0]"
   :scope :global
   :status :missing
   :tmux-command "tmux show-options -g update-environment[0]"
   :tmux-default "DISPLAY"
   :cl-tmux-default nil)
  (:kind :option
   :name "update-environment[1]"
   :scope :global
   :status :missing
   :tmux-command "tmux show-options -g update-environment[1]"
   :tmux-default "KRB5CCNAME"
   :cl-tmux-default nil)
  (:kind :option
   :name "update-environment[2]"
   :scope :global
   :status :missing
   :tmux-command "tmux show-options -g update-environment[2]"
   :tmux-default "MSYSTEM"
   :cl-tmux-default nil)
  (:kind :option
   :name "update-environment[3]"
   :scope :global
   :status :missing
   :tmux-command "tmux show-options -g update-environment[3]"
   :tmux-default "SSH_ASKPASS"
   :cl-tmux-default nil)
  (:kind :option
   :name "update-environment[4]"
   :scope :global
   :status :missing
   :tmux-command "tmux show-options -g update-environment[4]"
   :tmux-default "SSH_AUTH_SOCK"
   :cl-tmux-default nil)
  (:kind :option
   :name "update-environment[5]"
   :scope :global
   :status :missing
   :tmux-command "tmux show-options -g update-environment[5]"
   :tmux-default "SSH_AGENT_PID"
   :cl-tmux-default nil)
  (:kind :option
   :name "update-environment[6]"
   :scope :global
   :status :missing
   :tmux-command "tmux show-options -g update-environment[6]"
   :tmux-default "SSH_CONNECTION"
   :cl-tmux-default nil)
  (:kind :option
   :name "update-environment[7]"
   :scope :global
   :status :missing
   :tmux-command "tmux show-options -g update-environment[7]"
   :tmux-default "WINDOWID"
   :cl-tmux-default nil)
  (:kind :option
   :name "update-environment[8]"
   :scope :global
   :status :missing
   :tmux-command "tmux show-options -g update-environment[8]"
   :tmux-default "XAUTHORITY"
   :cl-tmux-default nil)
  (:kind :option
   :name "visual-activity"
   :scope :global
   :status :match
   :tmux-command "tmux show-options -g visual-activity"
   :tmux-default "off"
   :cl-tmux-default "off")
  (:kind :option
   :name "visual-bell"
   :scope :global
   :status :match
   :tmux-command "tmux show-options -g visual-bell"
   :tmux-default "off"
   :cl-tmux-default "off")
  (:kind :option
   :name "visual-silence"
   :scope :global
   :status :match
   :tmux-command "tmux show-options -g visual-silence"
   :tmux-default "off"
   :cl-tmux-default "off")
  (:kind :option
   :name "word-separators"
   :scope :global
   :status :partial
   :tmux-command "tmux show-options -g word-separators"
   :tmux-default "!\"#$%&'()*+,-./:;<=>?@[\\]^`{|}~"
   :cl-tmux-default " -_@")
  (:kind :option
   :name "cursor-colour"
   :scope :window
   :status :missing
   :tmux-command "tmux show-window-options -g cursor-colour"
   :tmux-default "none"
   :cl-tmux-default nil)
  (:kind :option
   :name "cursor-style"
   :scope :window
   :status :missing
   :tmux-command "tmux show-window-options -g cursor-style"
   :tmux-default "default"
   :cl-tmux-default nil)
  (:kind :option
   :name "menu-style"
   :scope :window
   :status :partial
   :tmux-command "tmux show-window-options -g menu-style"
   :tmux-default "default"
   :cl-tmux-default "")
  (:kind :option
   :name "menu-selected-style"
   :scope :window
   :status :partial
   :tmux-command "tmux show-window-options -g menu-selected-style"
   :tmux-default "bg=yellow,fg=black"
   :cl-tmux-default "")
  (:kind :option
   :name "menu-border-style"
   :scope :window
   :status :partial
   :tmux-command "tmux show-window-options -g menu-border-style"
   :tmux-default "default"
   :cl-tmux-default "")
  (:kind :option
   :name "menu-border-lines"
   :scope :window
   :status :match
   :tmux-command "tmux show-window-options -g menu-border-lines"
   :tmux-default "single"
   :cl-tmux-default "single")
  (:kind :option
   :name "pane-status-current-style"
   :scope :window
   :status :missing
   :tmux-command "tmux show-window-options -g pane-status-current-style"
   :tmux-default "default"
   :cl-tmux-default nil)
  (:kind :option
   :name "pane-status-style"
   :scope :window
   :status :missing
   :tmux-command "tmux show-window-options -g pane-status-style"
   :tmux-default "default"
   :cl-tmux-default nil)
  (:kind :option
   :name "session-status-current-style"
   :scope :window
   :status :missing
   :tmux-command "tmux show-window-options -g session-status-current-style"
   :tmux-default "default"
   :cl-tmux-default nil)
  (:kind :option
   :name "session-status-style"
   :scope :window
   :status :missing
   :tmux-command "tmux show-window-options -g session-status-style"
   :tmux-default "default"
   :cl-tmux-default nil)
  (:kind :option
   :name "aggressive-resize"
   :scope :window
   :status :missing
   :tmux-command "tmux show-window-options -g aggressive-resize"
   :tmux-default "off"
   :cl-tmux-default nil)
  (:kind :option
   :name "allow-passthrough"
   :scope :window
   :status :match
   :tmux-command "tmux show-window-options -g allow-passthrough"
   :tmux-default "off"
   :cl-tmux-default "off")
  (:kind :option
   :name "allow-rename"
   :scope :window
   :status :partial
   :tmux-command "tmux show-window-options -g allow-rename"
   :tmux-default "off"
   :cl-tmux-default "on")
  (:kind :option
   :name "allow-set-title"
   :scope :window
   :status :missing
   :tmux-command "tmux show-window-options -g allow-set-title"
   :tmux-default "on"
   :cl-tmux-default nil)
  (:kind :option
   :name "alternate-screen"
   :scope :window
   :status :match
   :tmux-command "tmux show-window-options -g alternate-screen"
   :tmux-default "on"
   :cl-tmux-default "on")
  (:kind :option
   :name "automatic-rename"
   :scope :window
   :status :match
   :tmux-command "tmux show-window-options -g automatic-rename"
   :tmux-default "on"
   :cl-tmux-default "on")
  (:kind :option
   :name "automatic-rename-format"
   :scope :window
   :status :partial
   :tmux-command "tmux show-window-options -g automatic-rename-format"
   :tmux-default "#{?pane_in_mode,[tmux],#{pane_current_command}}#{?pane_dead,[dead],}"
   :cl-tmux-default "#{pane_current_command}")
  (:kind :option
   :name "clock-mode-colour"
   :scope :window
   :status :match
   :tmux-command "tmux show-window-options -g clock-mode-colour"
   :tmux-default "blue"
   :cl-tmux-default "blue")
  (:kind :option
   :name "clock-mode-style"
   :scope :window
   :status :match
   :tmux-command "tmux show-window-options -g clock-mode-style"
   :tmux-default "24"
   :cl-tmux-default "24")
  (:kind :option
   :name "copy-mode-match-style"
   :scope :window
   :status :partial
   :tmux-command "tmux show-window-options -g copy-mode-match-style"
   :tmux-default "bg=cyan,fg=black"
   :cl-tmux-default "bg=green")
  (:kind :option
   :name "copy-mode-current-match-style"
   :scope :window
   :status :partial
   :tmux-command "tmux show-window-options -g copy-mode-current-match-style"
   :tmux-default "bg=magenta,fg=black"
   :cl-tmux-default "bg=magenta")
  (:kind :option
   :name "copy-mode-mark-style"
   :scope :window
   :status :missing
   :tmux-command "tmux show-window-options -g copy-mode-mark-style"
   :tmux-default "bg=red,fg=black"
   :cl-tmux-default nil)
  (:kind :option
   :name "copy-mode-position-format"
   :scope :window
   :status :missing
   :tmux-command "tmux show-window-options -g copy-mode-position-format"
   :tmux-default "#[align=right]#{t/p:top_line_time}#{?#{e|>:#{top_line_time},0}, ,}[#{scroll_position}/#{history_size}]#{?search_timed_out, (timed out),#{?search_count, (#{search_count}#{?search_count_partial,+,} results),}}"
   :cl-tmux-default nil)
  (:kind :option
   :name "copy-mode-position-style"
   :scope :window
   :status :missing
   :tmux-command "tmux show-window-options -g copy-mode-position-style"
   :tmux-default "#{E:mode-style}"
   :cl-tmux-default nil)
  (:kind :option
   :name "copy-mode-selection-style"
   :scope :window
   :status :missing
   :tmux-command "tmux show-window-options -g copy-mode-selection-style"
   :tmux-default "#{E:mode-style}"
   :cl-tmux-default nil)
  (:kind :option
   :name "fill-character"
   :scope :window
   :status :missing
   :tmux-command "tmux show-window-options -g fill-character"
   :tmux-default ""
   :cl-tmux-default nil)
  (:kind :option
   :name "main-pane-height"
   :scope :window
   :status :match
   :tmux-command "tmux show-window-options -g main-pane-height"
   :tmux-default "24"
   :cl-tmux-default "24")
  (:kind :option
   :name "main-pane-width"
   :scope :window
   :status :match
   :tmux-command "tmux show-window-options -g main-pane-width"
   :tmux-default "80"
   :cl-tmux-default "80")
  (:kind :option
   :name "mode-keys"
   :scope :window
   :status :partial
   :tmux-command "tmux show-window-options -g mode-keys"
   :tmux-default "emacs"
   :cl-tmux-default "vi")
  (:kind :option
   :name "mode-style"
   :scope :window
   :status :partial
   :tmux-command "tmux show-window-options -g mode-style"
   :tmux-default "noattr,bg=yellow,fg=black"
   :cl-tmux-default "reverse")
  (:kind :option
   :name "monitor-activity"
   :scope :window
   :status :match
   :tmux-command "tmux show-window-options -g monitor-activity"
   :tmux-default "off"
   :cl-tmux-default "off")
  (:kind :option
   :name "monitor-bell"
   :scope :window
   :status :match
   :tmux-command "tmux show-window-options -g monitor-bell"
   :tmux-default "on"
   :cl-tmux-default "on")
  (:kind :option
   :name "monitor-silence"
   :scope :window
   :status :match
   :tmux-command "tmux show-window-options -g monitor-silence"
   :tmux-default "0"
   :cl-tmux-default "0")
  (:kind :option
   :name "other-pane-height"
   :scope :window
   :status :match
   :tmux-command "tmux show-window-options -g other-pane-height"
   :tmux-default "0"
   :cl-tmux-default "0")
  (:kind :option
   :name "other-pane-width"
   :scope :window
   :status :match
   :tmux-command "tmux show-window-options -g other-pane-width"
   :tmux-default "0"
   :cl-tmux-default "0")
  (:kind :option
   :name "pane-active-border-style"
   :scope :window
   :status :partial
   :tmux-command "tmux show-window-options -g pane-active-border-style"
   :tmux-default "#{?pane_in_mode,fg=yellow,#{?synchronize-panes,fg=red,fg=green}}"
   :cl-tmux-default "fg=green")
  (:kind :option
   :name "pane-base-index"
   :scope :window
   :status :match
   :tmux-command "tmux show-window-options -g pane-base-index"
   :tmux-default "0"
   :cl-tmux-default "0")
  (:kind :option
   :name "pane-border-format"
   :scope :window
   :status :partial
   :tmux-command "tmux show-window-options -g pane-border-format"
   :tmux-default "#{?pane_active,#[reverse],}#{pane_index}#[default] \"#{pane_title}\""
   :cl-tmux-default " #{pane_index} ")
  (:kind :option
   :name "pane-border-indicators"
   :scope :window
   :status :match
   :tmux-command "tmux show-window-options -g pane-border-indicators"
   :tmux-default "colour"
   :cl-tmux-default "colour")
  (:kind :option
   :name "pane-border-lines"
   :scope :window
   :status :match
   :tmux-command "tmux show-window-options -g pane-border-lines"
   :tmux-default "single"
   :cl-tmux-default "single")
  (:kind :option
   :name "pane-border-status"
   :scope :window
   :status :match
   :tmux-command "tmux show-window-options -g pane-border-status"
   :tmux-default "off"
   :cl-tmux-default "off")
  (:kind :option
   :name "pane-border-style"
   :scope :window
   :status :partial
   :tmux-command "tmux show-window-options -g pane-border-style"
   :tmux-default "default"
   :cl-tmux-default "")
  (:kind :option
   :name "pane-colours"
   :scope :window
   :status :missing
   :tmux-command "tmux show-window-options -g pane-colours"
   :tmux-default ""
   :cl-tmux-default nil)
  (:kind :option
   :name "pane-scrollbars"
   :scope :window
   :status :missing
   :tmux-command "tmux show-window-options -g pane-scrollbars"
   :tmux-default "off"
   :cl-tmux-default nil)
  (:kind :option
   :name "pane-scrollbars-style"
   :scope :window
   :status :missing
   :tmux-command "tmux show-window-options -g pane-scrollbars-style"
   :tmux-default "bg=black,fg=white,width=1,pad=0"
   :cl-tmux-default nil)
  (:kind :option
   :name "pane-scrollbars-position"
   :scope :window
   :status :missing
   :tmux-command "tmux show-window-options -g pane-scrollbars-position"
   :tmux-default "right"
   :cl-tmux-default nil)
  (:kind :option
   :name "popup-style"
   :scope :window
   :status :partial
   :tmux-command "tmux show-window-options -g popup-style"
   :tmux-default "default"
   :cl-tmux-default "")
  (:kind :option
   :name "popup-border-style"
   :scope :window
   :status :partial
   :tmux-command "tmux show-window-options -g popup-border-style"
   :tmux-default "default"
   :cl-tmux-default "")
  (:kind :option
   :name "popup-border-lines"
   :scope :window
   :status :match
   :tmux-command "tmux show-window-options -g popup-border-lines"
   :tmux-default "single"
   :cl-tmux-default "single")
  (:kind :option
   :name "remain-on-exit"
   :scope :window
   :status :match
   :tmux-command "tmux show-window-options -g remain-on-exit"
   :tmux-default "off"
   :cl-tmux-default "off")
  (:kind :option
   :name "remain-on-exit-format"
   :scope :window
   :status :partial
   :tmux-command "tmux show-window-options -g remain-on-exit-format"
   :tmux-default "Pane is dead (#{?#{!=:#{pane_dead_status},},status #{pane_dead_status},}#{?#{!=:#{pane_dead_signal},},signal #{pane_dead_signal},}, #{t:pane_dead_time})"
   :cl-tmux-default "Pane is dead")
  (:kind :option
   :name "scroll-on-clear"
   :scope :window
   :status :match
   :tmux-command "tmux show-window-options -g scroll-on-clear"
   :tmux-default "on"
   :cl-tmux-default "on")
  (:kind :option
   :name "synchronize-panes"
   :scope :window
   :status :match
   :tmux-command "tmux show-window-options -g synchronize-panes"
   :tmux-default "off"
   :cl-tmux-default "off")
  (:kind :option
   :name "tiled-layout-max-columns"
   :scope :window
   :status :missing
   :tmux-command "tmux show-window-options -g tiled-layout-max-columns"
   :tmux-default "0"
   :cl-tmux-default nil)
  (:kind :option
   :name "window-active-style"
   :scope :window
   :status :partial
   :tmux-command "tmux show-window-options -g window-active-style"
   :tmux-default "default"
   :cl-tmux-default "")
  (:kind :option
   :name "window-size"
   :scope :window
   :status :partial
   :tmux-command "tmux show-window-options -g window-size"
   :tmux-default "latest"
   :cl-tmux-default "smallest")
  (:kind :option
   :name "window-style"
   :scope :window
   :status :partial
   :tmux-command "tmux show-window-options -g window-style"
   :tmux-default "default"
   :cl-tmux-default "")
  (:kind :option
   :name "window-status-activity-style"
   :scope :window
   :status :match
   :tmux-command "tmux show-window-options -g window-status-activity-style"
   :tmux-default "reverse"
   :cl-tmux-default "reverse")
  (:kind :option
   :name "window-status-bell-style"
   :scope :window
   :status :match
   :tmux-command "tmux show-window-options -g window-status-bell-style"
   :tmux-default "reverse"
   :cl-tmux-default "reverse")
  (:kind :option
   :name "window-status-current-format"
   :scope :window
   :status :partial
   :tmux-command "tmux show-window-options -g window-status-current-format"
   :tmux-default "#I:#W#{?window_flags,#{window_flags}, }"
   :cl-tmux-default " #{window_index}:#{window_name}* ")
  (:kind :option
   :name "window-status-current-style"
   :scope :window
   :status :partial
   :tmux-command "tmux show-window-options -g window-status-current-style"
   :tmux-default "default"
   :cl-tmux-default "reverse")
  (:kind :option
   :name "window-status-format"
   :scope :window
   :status :partial
   :tmux-command "tmux show-window-options -g window-status-format"
   :tmux-default "#I:#W#{?window_flags,#{window_flags}, }"
   :cl-tmux-default " #{window_index}:#{window_name} ")
  (:kind :option
   :name "window-status-last-style"
   :scope :window
   :status :partial
   :tmux-command "tmux show-window-options -g window-status-last-style"
   :tmux-default "default"
   :cl-tmux-default "")
  (:kind :option
   :name "window-status-separator"
   :scope :window
   :status :match
   :tmux-command "tmux show-window-options -g window-status-separator"
   :tmux-default " "
   :cl-tmux-default " ")
  (:kind :option
   :name "window-status-style"
   :scope :window
   :status :partial
   :tmux-command "tmux show-window-options -g window-status-style"
   :tmux-default "default"
   :cl-tmux-default "")
  (:kind :option
   :name "wrap-search"
   :scope :window
   :status :match
   :tmux-command "tmux show-window-options -g wrap-search"
   :tmux-default "on"
   :cl-tmux-default "on")
  (:kind :option
   :name "xterm-keys"
   :scope :window
   :status :missing
   :tmux-command "tmux show-window-options -g xterm-keys"
   :tmux-default "on"
   :cl-tmux-default nil)

 (:kind :cli
  :name "list-commands lscm alias without a server"
  :status :partial
  :tmux-command "tmux lscm -F '#{command_list_name}'"
  :cl-tmux-evidence "Differential test compares the lscm alias stdout, stderr, and exit code for #{command_list_name} against tmux."
 :next-step "Compare lscm usage output, flag errors, alias parsing inside command sequences, and command_list_usage formatting against real tmux.")
 (:kind :cli
  :name "display alias -p without a server"
  :status :partial
  :tmux-command "tmux display -p hello"
  :cl-tmux-evidence "Differential test compares no-server stdout, normalized stderr connection failure, and exit code for the display alias -p path."
  :next-step "Prove display-message long name, target/client flags, format expansion context, literal/verbose flags, hooks, in-session overlay behavior, and error cases against real tmux.")
 (:kind :cli
  :name "list-sessions without a terminal"
   :status :partial
  :tmux-command "tmux list-sessions"
  :cl-tmux-evidence "Differential test compares no-server non-TTY stdout, normalized stderr connection failure, and exit code against tmux."
  :next-step "Prove server-present list-sessions output, -F formatting, filters, aliases, and socket selection under a live cl-tmux server.")
 (:kind :cli
  :name "has-session without a server"
  :status :partial
  :tmux-command "tmux has-session -t no-such-session-xyz"
 :cl-tmux-evidence "Differential test compares no-server stdout, normalized stderr connection failure, and exit code against tmux."
  :next-step "Prove live-server has-session behavior, target parsing, aliases, stale socket handling, and socket selection.")
 (:kind :cli
  :name "has alias without a server"
  :status :partial
  :tmux-command "tmux has -t no-such-session-xyz"
  :cl-tmux-evidence "Differential test compares the has alias no-server stdout, normalized stderr connection failure, and exit code against tmux."
  :next-step "Prove live-server has alias behavior, target parsing, stale socket handling, and socket selection.")
 (:kind :cli
  :name "kill-server without a server"
  :status :partial
  :tmux-command "tmux kill-server"
 :cl-tmux-evidence "Differential test compares no-server stdout, normalized stderr connection failure, and exit code against tmux."
  :next-step "Prove live-server kill-server behavior, flags, stale socket handling, and socket selection.")
 (:kind :cli
  :name "list-windows without a server"
  :status :partial
  :tmux-command "tmux list-windows"
  :cl-tmux-evidence "Differential test compares no-server stdout, normalized stderr connection failure, and exit code against tmux."
  :next-step "Prove server-present list-windows output, -a/-F/-f/-t semantics, aliases, stale socket handling, and socket selection.")
 (:kind :cli
  :name "show-options -g without a server"
  :status :partial
 :tmux-command "tmux show-options -g"
 :cl-tmux-evidence "Differential test compares no-server stdout, normalized stderr connection failure, and exit code against tmux."
  :next-step "Prove live-server show-options output, server/window/global scopes, quiet flags, option formatting, stale socket handling, and socket selection.")
 (:kind :cli
  :name "show-window-options -g without a server"
  :status :partial
  :tmux-command "tmux show-window-options -g"
  :cl-tmux-evidence "Differential test compares no-server stdout, normalized stderr connection failure, and exit code against tmux."
  :next-step "Prove live-server show-window-options output, target-window semantics, flags, option formatting, stale socket handling, and socket selection.")
 (:kind :cli
 :name "new-session -d against an existing server"
 :status :partial
 :tmux-command "tmux new-session -d -s beta -n two"
  :cl-tmux-evidence "Differential test starts isolated live servers, runs detached new-session, then compares list-sessions -F '#{session_name}' and the beta list-windows -a -F '#{session_name}:#{window_name}' row against tmux. Under threaded SBCL this cl-tmux path registers a query-visible no-PTY placeholder pane, not a proven shell-backed pane."
  :next-step "Prove attached new-session behavior, shell-backed pane usability for forwarded detached sessions, -A/-c/-x/-y/-t grouping, duplicate-name errors, current-session selection, target semantics, and socket-name semantics.")
 (:kind :command :name "attach-session" :status :partial :cl-tmux-command t)
  (:kind :command :name "bind-key" :status :partial :cl-tmux-command t)
  (:kind :command :name "break-pane" :status :partial :cl-tmux-command t)
  (:kind :command :name "capture-pane" :status :partial :cl-tmux-command t)
  (:kind :command :name "choose-buffer" :status :partial :cl-tmux-command t)
  (:kind :command :name "choose-client" :status :partial :cl-tmux-command t)
  (:kind :command :name "choose-tree" :status :partial :cl-tmux-command t)
  (:kind :command :name "clear-history" :status :partial :cl-tmux-command t)
  (:kind :command :name "clear-prompt-history" :status :partial :cl-tmux-command t)
  (:kind :command :name "clock-mode" :status :partial :cl-tmux-command t)
  (:kind :command :name "command-prompt" :status :partial :cl-tmux-command t)
  (:kind :command :name "confirm-before" :status :partial :cl-tmux-command t)
  (:kind :command :name "copy-mode" :status :partial :cl-tmux-command t)
  (:kind :command :name "customize-mode" :status :partial :cl-tmux-command t)
  (:kind :command :name "delete-buffer" :status :partial :cl-tmux-command t)
  (:kind :command :name "detach-client" :status :partial :cl-tmux-command t)
  (:kind :command :name "display-menu" :status :partial :cl-tmux-command t)
  (:kind :command :name "display-message" :status :partial :cl-tmux-command t)
  (:kind :command :name "display-popup" :status :partial :cl-tmux-command t)
  (:kind :command :name "display-panes" :status :partial :cl-tmux-command t)
  (:kind :command :name "find-window" :status :partial :cl-tmux-command t)
  (:kind :command :name "has-session" :status :partial :cl-tmux-command t)
  (:kind :command :name "if-shell" :status :partial :cl-tmux-command t)
  (:kind :command :name "join-pane" :status :partial :cl-tmux-command t)
  (:kind :command :name "kill-pane" :status :partial :cl-tmux-command t)
  (:kind :command :name "kill-server" :status :partial :cl-tmux-command t)
  (:kind :command :name "kill-session" :status :partial :cl-tmux-command t)
  (:kind :command :name "kill-window" :status :partial :cl-tmux-command t)
  (:kind :command :name "last-pane" :status :partial :cl-tmux-command t)
  (:kind :command :name "last-window" :status :partial :cl-tmux-command t)
  (:kind :command :name "link-window" :status :partial :cl-tmux-command t)
  (:kind :command :name "list-buffers" :status :partial :cl-tmux-command t)
  (:kind :command :name "list-clients" :status :partial :cl-tmux-command t)
  (:kind :command
   :name "list-commands"
   :status :partial
   :cl-tmux-command t
   :tmux-command "tmux list-commands -F '#{command_list_name}'"
   :cl-tmux-command-line "cl-tmux list-commands -F '#{command_list_name}'"
   :cl-tmux-evidence "No-server and in-session list-commands now use the tmux 3.6a public command-name inventory instead of cl-tmux internal bindable helper names. The differential test compares stdout, stderr, and exit code for #{command_list_name} when CL_TMUX_COMPAT_BINARY or result/bin/cl-tmux is available."
   :next-step "Compare default signatures, aliases, flags, filters, error behavior, and command_list_usage formatting against real tmux.")
  (:kind :command :name "list-keys" :status :partial :cl-tmux-command t)
  (:kind :command :name "list-panes" :status :partial :cl-tmux-command t)
  (:kind :command :name "list-sessions" :status :partial :cl-tmux-command t)
  (:kind :command :name "list-windows" :status :partial :cl-tmux-command t)
  (:kind :command :name "load-buffer" :status :partial :cl-tmux-command t)
  (:kind :command :name "lock-client" :status :partial :cl-tmux-command t)
  (:kind :command :name "lock-server" :status :partial :cl-tmux-command t)
  (:kind :command :name "lock-session" :status :partial :cl-tmux-command t)
  (:kind :command :name "move-pane" :status :partial :cl-tmux-command t)
  (:kind :command :name "move-window" :status :partial :cl-tmux-command t)
  (:kind :command :name "new-session" :status :partial :cl-tmux-command t)
  (:kind :command :name "new-window" :status :partial :cl-tmux-command t)
  (:kind :command :name "next-layout" :status :partial :cl-tmux-command t)
  (:kind :command :name "next-window" :status :partial :cl-tmux-command t)
  (:kind :command :name "paste-buffer" :status :partial :cl-tmux-command t)
  (:kind :command :name "pipe-pane" :status :partial :cl-tmux-command t)
  (:kind :command :name "previous-layout" :status :partial :cl-tmux-command t)
  (:kind :command :name "previous-window" :status :partial :cl-tmux-command t)
  (:kind :command :name "refresh-client" :status :partial :cl-tmux-command t)
  (:kind :command :name "rename-session" :status :partial :cl-tmux-command t)
  (:kind :command :name "rename-window" :status :partial :cl-tmux-command t)
  (:kind :command :name "resize-pane" :status :partial :cl-tmux-command t)
  (:kind :command :name "resize-window" :status :partial :cl-tmux-command t)
  (:kind :command :name "respawn-pane" :status :partial :cl-tmux-command t)
  (:kind :command :name "respawn-window" :status :partial :cl-tmux-command t)
  (:kind :command :name "rotate-window" :status :partial :cl-tmux-command t)
  (:kind :command :name "run-shell" :status :partial :cl-tmux-command t)
  (:kind :command :name "save-buffer" :status :partial :cl-tmux-command t)
  (:kind :command :name "select-layout" :status :partial :cl-tmux-command t)
  (:kind :command :name "select-pane" :status :partial :cl-tmux-command t)
  (:kind :command :name "select-window" :status :partial :cl-tmux-command t)
  (:kind :command :name "send-keys" :status :partial :cl-tmux-command t)
  (:kind :command :name "send-prefix" :status :partial :cl-tmux-command t)
  (:kind :command :name "server-access" :status :partial :cl-tmux-command t)
  (:kind :command :name "set-buffer" :status :partial :cl-tmux-command t)
  (:kind :command :name "set-environment" :status :partial :cl-tmux-command t)
  (:kind :command :name "set-hook" :status :partial :cl-tmux-command t)
  (:kind :command :name "set-option" :status :partial :cl-tmux-command t)
  (:kind :command :name "set-window-option" :status :partial :cl-tmux-command t)
  (:kind :command :name "show-buffer" :status :partial :cl-tmux-command t)
  (:kind :command :name "show-environment" :status :partial :cl-tmux-command t)
  (:kind :command :name "show-hooks" :status :partial :cl-tmux-command t)
  (:kind :command :name "show-messages" :status :partial :cl-tmux-command t)
  (:kind :command :name "show-options" :status :partial :cl-tmux-command t)
  (:kind :command :name "show-prompt-history" :status :partial :cl-tmux-command t)
  (:kind :command :name "show-window-options" :status :partial :cl-tmux-command t)
  (:kind :command :name "source-file" :status :partial :cl-tmux-command t)
  (:kind :command :name "split-window" :status :partial :cl-tmux-command t)
  (:kind :command :name "start-server" :status :partial :cl-tmux-command t)
  (:kind :command :name "suspend-client" :status :partial :cl-tmux-command t)
  (:kind :command :name "swap-pane" :status :partial :cl-tmux-command t)
  (:kind :command :name "swap-window" :status :partial :cl-tmux-command t)
  (:kind :command :name "switch-client" :status :partial :cl-tmux-command t)
  (:kind :command :name "unbind-key" :status :partial :cl-tmux-command t)
  (:kind :command :name "unlink-window" :status :partial :cl-tmux-command t)
  (:kind :command :name "wait-for" :status :partial :cl-tmux-command t)))
