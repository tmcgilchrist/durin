# Durin

Durin is a library for reading and writing the [Dwarf debugging format](https://dwarfstd.org/).

It supports:
 * Reading and writing DWARF 4 and DWARF 5 from ELF and MachO object files.
 * Writing DWARF 5 into GNU assembler-compatible assembly files.
 * Writing DWARF 5 into object files.
 * Split DWARF (`.dwo` files, `.dwp` package files, skeleton units).

It provides:

 * Cross-platform: `durin` supports both ELF (Linux) and MachO (macOS) object
   files via the [`object`](https://github.com/mbarbin/object) library.
 * Lazy: you can iterate compilation units without parsing their contents.
   Parse only as many debugging information entry (DIE) trees as you iterate
   over. `durin` also uses `DW_AT_sibling` references to avoid parsing a DIE's
   children to find its next sibling where possible.

### Section Support

| Section              | Read | Write | DWARF Version |
|----------------------|------|-------|---------------|
| `.debug_info`        | Yes  | Yes   | 4, 5          |
| `.debug_abbrev`      | Yes  | Yes   | 4, 5          |
| `.debug_line`        | Yes  | Yes   | 4, 5          |
| `.debug_line_str`    | Yes  | Yes   | 5             |
| `.debug_str`         | Yes  | Yes   | 4, 5          |
| `.debug_str_offsets` | Yes  | Yes   | 5             |
| `.debug_addr`        | Yes  | Yes   | 5             |
| `.debug_frame`       | Yes  | Yes   | 4, 5          |
| `.eh_frame`          | Yes  | Yes   | 4, 5          |
| `.debug_aranges`     | Yes  | Yes   | 4, 5          |
| `.debug_loclists`    | Yes  | Yes   | 5             |
| `.debug_rnglists`    | Yes  | Yes   | 5             |
| `.debug_names`       | Yes  | Yes   | 5             |
| `.debug_macro`       | Yes  | Yes   | 5             |
| `.debug_cu_index`    | Yes  | Yes   | 5             |
| `.debug_tu_index`    | Yes  | Yes   | 5             |
| `.debug_loc`         | Yes  | Yes   | 4             |
| `.debug_ranges`      | Yes  | Yes   | 4             |
| `.debug_pubnames`    | Yes  | Yes   | 4             |
| `.debug_pubtypes`    | Yes  | Yes   | 4             |
| `.debug_macinfo`     | Yes  | Yes   | 4             |
| `.debug_types`       | Yes  | Yes   | 4             |

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
   - https://github.com/llvm/llvm-project/blob/main/libunwind/include/mach-o/compact_unwind_encoding.h
   - https://faultlore.com/blah/compact-unwinding/
   - https://github.com/getsentry/symbolic/blob/master/symbolic-debuginfo/src/macho/compact.rs

 * Vendor extensions from GCC
   https://sourceware.org/elfutils/DwarfExtensions

### Future work / Known limitations

**Big-endian is unsupported.** Both read and write paths assume
  little-endian throughout. The `Object.Buffer.Read` functions are
  hardcoded LE, and all `dwarf_write` primitives are LE-only. This
  affects PowerPC 64, s390x, and big-endian MIPS targets. Fixing
  this requires coordinated changes: making `Object.Buffer.Read`
  endianness-aware, threading byte order through the `encoding`
  type, and adding `_be` write variants. All current targets
  (x86-64, ARM64 on Linux/macOS) are little-endian so this is
  low priority.