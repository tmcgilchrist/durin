(* TODO This wrapper could be better organised.
   Keep Dwarf_* modules for spec compliant DWARF.
   Eh_frame_header / Compact_unwind / Elf_symbols are high-level format readers
   exposed alongside Dwarf.
   Then SFrame in PR#12 would also be a high-level module. *)
module Dwarf = Dwarf
module Dwarf_write = Dwarf_write
module Dwarf_asm = Dwarf_asm
module Elf_symbols = Elf_symbols
module Eh_frame_header = Eh_frame_header
module Compact_unwind = Compact_unwind
