# Explanation

Conceptual background on DWARF and how Durin models it. These documents are
for *understanding* the domain rather than completing a specific task — read
them when you want to know how DWARF fits together, not when you are looking
up a particular function (see the [API reference](https://ocaml.org/p/durin))
or following a worked example (see the `example/` programs).

## Contents

- [DWARF 5 terminology](./terminology.md) — the relationship between
  compilation units and Debugging Information Entries (DIEs), what the root
  DIE is, and how the `.debug_str_offsets` table and `DW_FORM_strx` indices
  resolve to strings.
- [Object files and sections](./object-files.md) — which DWARF sections exist
  in ELF (Linux) versus Mach-O (macOS), what each section holds, and the
  toolchain commands for inspecting them.
- [Module architecture](./architecture.md) — how the library's modules depend
  on one another, as a layered diagram.

## Reference material

- [DWARF 5 standard](./DWARF5.pdf) and [DWARF 4 standard](./DWARF4.pdf) — the
  upstream specifications Durin implements.
