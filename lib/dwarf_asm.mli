open Dwarf_types

(** Emit DWARF as assembler text (via a {!Format.formatter}), the textual
    counterpart of the binary {!Dwarf_write} *)

(** {2 Primitive Assembly Directives} *)

val emit_byte : Format.formatter -> int -> unit
(** Emit a [.byte] directive for an 8-bit value. *)

val emit_2byte : Format.formatter -> int -> unit
(** Emit a [.2byte] directive for a 16-bit value. *)

val emit_4byte : Format.formatter -> int -> unit
(** Emit a [.4byte] directive for a 32-bit value. *)

val emit_8byte : Format.formatter -> int64 -> unit
(** Emit an [.8byte] directive for a 64-bit value. *)

val emit_uleb128 : Format.formatter -> u64 -> unit
(** Emit a [.uleb128] directive (unsigned LEB128 variable-length encoding). *)

val emit_sleb128 : Format.formatter -> i64 -> unit
(** Emit a [.sleb128] directive (signed LEB128 variable-length encoding). *)

val emit_asciz : Format.formatter -> string -> unit
(** Emit a [.asciz] directive: a NUL-terminated string literal. *)

val emit_label : Format.formatter -> string -> unit
(** Emit a label definition ([name:]) at the current position. *)

val emit_comment : Format.formatter -> string -> unit
(** Emit an assembler comment ([# ...]). *)

val emit_section : Format.formatter -> string -> unit
(** Emit a [.section] directive switching subsequent output to the named
    section. *)

(** {2 DWARF-specific Emission} *)

val emit_initial_length :
  Format.formatter ->
  Dwarf.dwarf_format ->
  start_label:string ->
  end_label:string ->
  unit
(** Emit a DWARF initial-length field as the assembler-computed difference
    [end_label - start_label], sized by {!Dwarf.dwarf_format}. Using labels lets
    the assembler fill in the length, so the section body need not be measured
    up front. *)

val emit_offset : Format.formatter -> Dwarf.dwarf_format -> string -> unit
(** Emit a section offset as a reference to the given label, sized by
    {!Dwarf.dwarf_format} (4 bytes for [DWARF32], 8 for [DWARF64]). *)

val emit_attribute_value :
  Format.formatter ->
  Dwarf.DIE.attribute_value ->
  Dwarf.attribute_form_encoding ->
  Dwarf.encoding ->
  unit
(** Emit an {!Dwarf.DIE.attribute_value} with the given
    {!Dwarf.attribute_form_encoding} under the {!Dwarf.encoding} as assembly
    directives. The assembler-text counterpart of
    {!Dwarf_write.write_attribute_value}.

    @raise Dwarf.Parse_error
      if the form/value combination or address size is unsupported. *)

val emit_die :
  Format.formatter -> Dwarf.DIE.t -> Dwarf.encoding -> (int -> u64) -> unit
(** Emit a DIE and its child subtree as assembly. The [(int -> u64)] argument
    looks up a DIE's abbreviation code by its offset. The assembler-text
    counterpart of {!Dwarf_write.write_die}.

    @raise Dwarf.Parse_error
      if a DIE attribute uses an unsupported form/value combination. *)

val emit_abbrev_table : Format.formatter -> Dwarf.abbrev array -> unit
(** Emit the contents of an abbreviation table as assembly directives,
    terminated by a null entry. *)

(** {2 Section-Level Emission} *)

val emit_debug_abbrev : Format.formatter -> Dwarf.abbrev array -> unit
(** Emit a complete [.debug_abbrev] section, including its [.section] directive.
*)

val emit_debug_info :
  Format.formatter -> Dwarf.encoding -> Dwarf.DIE.t list -> (int -> u64) -> unit
(** Emit a complete [.debug_info] section: its [.section] directive followed by
    one compilation unit per root DIE. The [(int -> u64)] argument is the
    abbreviation-code lookup.

    @raise Dwarf.Parse_error
      if a DIE attribute uses an unsupported form/value combination. *)

val emit_debug_str : Format.formatter -> Dwarf_write.string_table -> unit
(** Emit a [.debug_str] section from a {!Dwarf_write.string_table}. *)

val emit_debug_line_str : Format.formatter -> Dwarf_write.string_table -> unit
(** Emit a [.debug_line_str] section from a {!Dwarf_write.string_table}. *)

val emit_all : Format.formatter -> Dwarf.encoding -> Dwarf.DIE.t list -> unit
(** Emit the [.debug_abbrev] and [.debug_info] sections for a forest of
    compilation-unit root DIEs, assigning and deduplicating abbreviations
    automatically. The assembler-text counterpart of
    {!Dwarf_write.write_debug_info}.

    @raise Dwarf.Parse_error
      if a DIE attribute uses an unsupported form/value combination. *)
