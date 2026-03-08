open Types
module Buffer = Stdlib.Buffer

val write_u8 : Buffer.t -> u8 -> unit
val write_u16_le : Buffer.t -> u16 -> unit
val write_u32_le : Buffer.t -> u32 -> unit
val write_u64_le : Buffer.t -> u64 -> unit
val write_i64_le : Buffer.t -> i64 -> unit
val write_uleb128 : Buffer.t -> u64 -> unit
val write_sleb128 : Buffer.t -> i64 -> unit
val write_initial_length : Buffer.t -> Dwarf.dwarf_format -> int -> unit
val write_offset : Buffer.t -> Dwarf.dwarf_format -> u64 -> unit
val write_address : Buffer.t -> int -> u64 -> unit
val write_null_terminated_string : Buffer.t -> string -> unit

(** {2 Stage 2: Abbreviation Table} *)

val form_for_attribute_value :
  Dwarf.DIE.attribute_value -> Dwarf.attribute_form_encoding

type die_shape = {
  tag : Dwarf.abbreviation_tag;
  has_children : bool;
  attr_forms : (Dwarf.attribute_encoding * Dwarf.attribute_form_encoding) list;
}

val shape_of_die : Dwarf.DIE.t -> die_shape
val assign_abbreviations : Dwarf.DIE.t list -> Dwarf.abbrev array * (int -> u64)
val write_abbrev_table : Buffer.t -> Dwarf.abbrev array -> unit
val abbrev_table_size : Dwarf.abbrev array -> int
val uleb128_size : u64 -> int
val sleb128_size : i64 -> int

(** {2 Stage 3: Attribute Value Serialisation} *)

val write_attribute_value :
  Buffer.t ->
  Dwarf.DIE.attribute_value ->
  Dwarf.attribute_form_encoding ->
  Dwarf.encoding ->
  unit

val attribute_value_size :
  Dwarf.DIE.attribute_value ->
  Dwarf.attribute_form_encoding ->
  Dwarf.encoding ->
  int

(** {2 Stage 4: DIE Tree Serialisation} *)

val write_die :
  Buffer.t -> Dwarf.DIE.t -> Dwarf.encoding -> (int -> u64) -> unit

val die_size : Dwarf.DIE.t -> Dwarf.encoding -> (int -> u64) -> int

val write_die_forest :
  Buffer.t -> Dwarf.DIE.t list -> Dwarf.encoding -> (int -> u64) -> unit

(** {2 Stage 5: Compilation Unit & Top-Level} *)

val write_compile_unit :
  Buffer.t -> Dwarf.encoding -> Dwarf.DIE.t -> (int -> u64) -> u64 -> unit

val write_debug_info : Dwarf.encoding -> Dwarf.DIE.t list -> string * string

(** {2 Stage 6: String Table} *)

type string_table

val create_string_table : unit -> string_table
val add_string : string_table -> string -> int
val write_string_table : Buffer.t -> string_table -> unit
val string_table_size : string_table -> int

(** {2 Stage 7: Expression Encoding} *)

val write_expression :
  Buffer.t -> Dwarf.dwarf_expression_operation list -> Dwarf.encoding -> unit

(** {2 Stage 9: Location/Range Lists} *)

val write_location_entry :
  Buffer.t -> Dwarf.DebugLoclists.location_entry -> int -> unit

val write_location_list :
  Buffer.t -> Dwarf.DebugLoclists.location_list -> int -> unit

val write_range_entry :
  Buffer.t -> Dwarf.DebugRnglists.range_entry -> int -> unit

val write_range_list : Buffer.t -> Dwarf.DebugRnglists.range_list -> int -> unit
val write_loclists_header : Buffer.t -> Dwarf.encoding -> int -> int -> unit
val write_rnglists_header : Buffer.t -> Dwarf.encoding -> int -> int -> unit
val write_debug_loc : Buffer.t -> Dwarf.DebugLoc.entry list -> int -> unit
val write_debug_ranges : Buffer.t -> Dwarf.DebugRanges.entry list -> int -> unit

(** {2 Stage 10: Line Program Writer} *)

val write_debug_line :
  Buffer.t ->
  Dwarf.DebugLine.line_program_header ->
  Dwarf.DebugLine.line_table_entry list ->
  unit
