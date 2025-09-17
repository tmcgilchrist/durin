# Durin

Durin is a library for reading and writing the [Dwarf debugging format](https://dwarfstd.org/).

It aims to support:
 * Reading DWARF 5 encoded information from ELF and MachO object files.
 * Writing DWARF 5 information into ELF and MachO object files.
 * Writing DWARF 5 information into assembly files.

In future it could support DWARF 4 or newer versions of the DWARF standard.

It should provide:

 * Cross-platform: `durin` makes no assumptions about what kind of object file
   you're working with. Provide your own Buffer or use the `object` library.
 * Lazy: you can iterate compilation units without parsing their contents.
   Parse only as many debugging information entry (DIE) trees as you iterate
   over. `durin` also uses `DW_AT_sibling` references to avoid parsing a DIE's
   children to find it's next sibling where possible.

## Install

To install `durin` as a dependency, run:

``` shell
$ opam install durin
```

And add `durin` to your project's `dune-project` or `*.opam` files.

## Documentation

 * Documentation on [ocaml.org](https://ocaml.org/p/durin)
 * Example programs in `example` directory
   - A simple [.debug_info](./example/simple_debug_info.ml) parser
   - A simple [.debug_line](./example/simple_debug_line.ml) parser
   - A [dwarfdump clone](./example/dwarfdump.ml)
   - An [addr2line clone](./example/addr2line.ml)
   - A small utility [dwprod](./example/dwprod.ml) to list the compilers
     used to create each compilation unit within a shared library or
     executable (via `DW_AT_producer`).
   - A [dwarf-valiate clone](./example/dwarf_validate.ml), a program to
     validate the integrity of some DWARF information and the references
     between sections and compilation units.

## Resources

 * Apple Compact Unwinding Format is defined by the LLVM implementation.
   https://github.com/llvm/llvm-project/blob/main/libunwind/include/mach-o/compact_unwind_encoding.h
   https://faultlore.com/blah/compact-unwinding/
   https://github.com/getsentry/symbolic/blob/master/symbolic-debuginfo/src/macho/compact.rs
