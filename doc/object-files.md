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

DWARF 5 used the following sections for Mach-O:

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

Initally GCC and Clang both emit DWARF 5 32 bit format so we will support that first.

## MacOS commands

 * llvm-objdump - LLVM's object file dumper
 * dwarfdump - dump and verify DWARF debug information
 * nm - display name list (symbol table)

## Linux commands

 * readelf - display information about ELF files
 * dwarfdump - dumps DWARF debug information of an ELF object
 * nm - list symbols from object files
