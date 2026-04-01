open Dwarf_types

(** {2 Primitive Assembly Directives} *)

val emit_byte : Format.formatter -> int -> unit
val emit_2byte : Format.formatter -> int -> unit
val emit_4byte : Format.formatter -> int -> unit
val emit_8byte : Format.formatter -> int64 -> unit
val emit_uleb128 : Format.formatter -> u64 -> unit
val emit_sleb128 : Format.formatter -> i64 -> unit
val emit_asciz : Format.formatter -> string -> unit
val emit_label : Format.formatter -> string -> unit
val emit_comment : Format.formatter -> string -> unit
val emit_section : Format.formatter -> string -> unit

(** {2 DWARF-specific Emission} *)

val emit_initial_length :
  Format.formatter ->
  Dwarf.dwarf_format ->
  start_label:string ->
  end_label:string ->
  unit

val emit_offset : Format.formatter -> Dwarf.dwarf_format -> string -> unit

val emit_attribute_value :
  Format.formatter ->
  Dwarf.DIE.attribute_value ->
  Dwarf.attribute_form_encoding ->
  Dwarf.encoding ->
  unit

val emit_die :
  Format.formatter -> Dwarf.DIE.t -> Dwarf.encoding -> (int -> u64) -> unit

val emit_abbrev_table : Format.formatter -> Dwarf.abbrev array -> unit

(** {2 Section-Level Emission} *)

val emit_debug_abbrev : Format.formatter -> Dwarf.abbrev array -> unit

val emit_debug_info :
  Format.formatter -> Dwarf.encoding -> Dwarf.DIE.t list -> (int -> u64) -> unit

val emit_debug_str : Format.formatter -> Dwarf_write.string_table -> unit
val emit_debug_line_str : Format.formatter -> Dwarf_write.string_table -> unit
val emit_all : Format.formatter -> Dwarf.encoding -> Dwarf.DIE.t list -> unit
