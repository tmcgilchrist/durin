DWARF 5 can be used in different object file formats, here we mainly care about Linux ELF and macOS Mach-O.

DWARF 5 specifies the following set of sections for ELF:

 * .debug_info - The core section with DWARF information entries (DIEs)
 * .debug_abbrev - Abbreviations used to simplify the `.debug_info` section
 * .debug_aranges - Debug address ranges, provides a lookup table to find information about an address range.
 * .debug_frame - Information about how to perform stack unwinding, usually empty. SYSV ABI uses .eh_frame section instead.
 * .eh_frame - Stack unwinding information.
 * .debug_line - Mappings from machine instructions to source-level instructions.
 * .debug_line_str - String section specific to the line number table.
 * .debug_loc  - Descriptions of where variables live
 * .debug_ranges - Address ranges referenced by DIEs
 * .debug_str - String table used by .debug_info; uses the same format as ELF string tables
 * .debug_types - Type descriptions introduced in DWARF 4 moved to .debug_info section in DWARF 5
 * .debug_pubnames - Lookup table for global objects and functions; Legacy and essentially unused.
 * .debug_pubtypes - Lookup table for global types; Legacy and essentially unused.

DWARF 5 uses the following sections for Mach-O:

 * __debug_info - The core section with DWARF information entries (DIEs)
 * __debug_abbrev - Abbreviations used to simplify the `__debug_info` section
 * __debug_addr - Debug address table
 * __debug_aranges - Debug address ranges, provides a lookup table to find information about an address range.
 * __unwind_info - Information about how to perform stack unwinding
 * __debug_line - Mappings from machine instructions to source-level instructions.
 * __debug_line_str - Strings for file names used in combination with `__debug_line`.
 * __debug_loclists - Descriptions of where variables live
 * __debug_rnglists - Address ranges referenced by DIEs
 * __debug_str - String table used by __debug_info; uses the same format as ELF string tables???
 * __debug_str_offs - The string offsets table for the strings in `__debug_str`.
 * __debug_names - Lookup table by name for global objects, types and functions. Replaces the .debug_pubnames and .debug_pubtypes from DWARF 4.

Initially GCC and Clang both emit DWARF 5 32 bit format so we will support that first.

## MacOS commands

 * llvm-objdump - LLVM's object file dumper
 * dwarfdump - dump and verify DWARF debug information
 * nm - display name list (symbol table)

``` shell
# Display section headers
$ gcc -gdwarf-5 -o prog prog.c
$ objdump --section-headers prog

prog:	file format mach-o arm64

Sections:
Idx Name          Size     VMA              Type
  0 __text        00000068 0000000100000460 TEXT
  1 __stubs       00000018 00000001000004c8 TEXT
  2 __cstring     00000011 00000001000004e0 DATA
  3 __unwind_info 00000060 00000001000004f4 DATA
  4 __got         00000010 0000000100004000 DATA

$ objdump --section-headers prog.dSYM/Contents/Resources/DWARF/prog

prog.dSYM/Contents/Resources/DWARF/prog:	file format mach-o arm64

Sections:
Idx Name             Size     VMA              Type
  0 __text           00000068 0000000100000460 TEXT
  1 __stubs          00000018 00000001000004c8 TEXT
  2 __cstring        00000011 00000001000004e0 DATA
  3 __unwind_info    00000060 00000001000004f4 DATA
  4 __got            00000010 0000000100004000 DATA
  5 __debug_line     00000084 0000000100009000 DATA, DEBUG
  6 __debug_aranges  00000030 0000000100009084 DATA, DEBUG
  7 __debug_addr     00000018 00000001000090b4 DATA, DEBUG
  8 __debug_info     00000068 00000001000090cc DATA, DEBUG
  9 __debug_abbrev   00000083 0000000100009134 DATA, DEBUG
 10 __debug_str      000000ed 00000001000091b7 DATA, DEBUG
 11 __debug_str_offs 00000034 00000001000092a4 DATA, DEBUG
 12 __debug_line_str 00000028 00000001000092d8 DATA, DEBUG
 13 __debug_names    000000b0 0000000100009300 DATA, DEBUG
```


## Linux commands

 * readelf - display information about ELF files
 * dwarfdump - dumps DWARF debug information of an ELF object
 * nm - list symbols from object files

``` shell
# Display section headers
$ readelf -S prog

```
