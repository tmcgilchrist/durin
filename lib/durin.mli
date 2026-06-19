(* TODO This wrapper could be better organised.
   Keep Dwarf_* modules for spec compliant DWARF.
   Compact_unwind as additional high level module here, since it's macOS specific.
   Same logic for Elf_symbols, I think but needs confirmation.
   Then SFrame in PR#12 would also be a high-level module. *)
module Dwarf = Dwarf
module Dwarf_write = Dwarf_write
module Dwarf_asm = Dwarf_asm
module Elf_symbols = Elf_symbols
