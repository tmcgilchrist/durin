open Dwarf_types
module Buffer = Stdlib.Buffer

(** {2 Low-level write helpers} *)

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
(** @raise Dwarf.Parse_error if the address size is unsupported. *)

val write_null_terminated_string : Buffer.t -> string -> unit

(** {2 Abbreviation Table} *)

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

(** {2 Attribute Value Serialisation} *)

val write_attribute_value :
  Buffer.t ->
  Dwarf.DIE.attribute_value ->
  Dwarf.attribute_form_encoding ->
  Dwarf.encoding ->
  unit
(** @raise Dwarf.Parse_error
      if the form/value combination or address size is unsupported. *)

val attribute_value_size :
  Dwarf.DIE.attribute_value ->
  Dwarf.attribute_form_encoding ->
  Dwarf.encoding ->
  int
(** @raise Dwarf.Parse_error if the form/value combination is unsupported. *)

(** {2 DIE Tree Serialisation} *)

val write_die :
  Buffer.t -> Dwarf.DIE.t -> Dwarf.encoding -> (int -> u64) -> unit
(** @raise Dwarf.Parse_error
      if a DIE attribute uses an unsupported form/value combination. *)

val die_size : Dwarf.DIE.t -> Dwarf.encoding -> (int -> u64) -> int
(** @raise Dwarf.Parse_error
      if a DIE attribute uses an unsupported form/value combination. *)

val write_die_forest :
  Buffer.t -> Dwarf.DIE.t list -> Dwarf.encoding -> (int -> u64) -> unit
(** @raise Dwarf.Parse_error
      if a DIE attribute uses an unsupported form/value combination. *)

(** {2 Compilation Unit & Top-Level} *)

val write_compile_unit :
  Buffer.t -> Dwarf.encoding -> Dwarf.DIE.t -> (int -> u64) -> u64 -> unit
(** @raise Dwarf.Parse_error
      if a DIE attribute uses an unsupported form/value combination. *)

val write_debug_info : Dwarf.encoding -> Dwarf.DIE.t list -> string * string
(** @raise Dwarf.Parse_error
      if a DIE attribute uses an unsupported form/value combination. *)

(* TODO Maybe explain the string_table or reference documentation about
   what the string table is and which sections it is stored into.
   Perhaps also why is this exported as a type but the read side doesn't
   have an equivalent? Does it? *)
type string_table
(** {2 String Table} *)

val create_string_table : unit -> string_table
val add_string : string_table -> string -> int
val write_string_table : Buffer.t -> string_table -> unit
val string_table_size : string_table -> int
val write_debug_str : Buffer.t -> string_table -> unit
val write_debug_line_str : Buffer.t -> string_table -> unit

(** {2 Expression Encoding} *)

val write_expression :
  Buffer.t -> Dwarf.dwarf_expression_operation list -> Dwarf.encoding -> unit
(** @raise Dwarf.Parse_error if an operation is unsupported for writing. *)

(** {2 Location/Range Lists} *)

val write_location_entry :
  Buffer.t -> Dwarf.DebugLoclists.location_entry -> int -> unit
(** @raise Dwarf.Parse_error if an unsupported address size is encountered. *)

val write_location_list :
  Buffer.t -> Dwarf.DebugLoclists.location_list -> int -> unit
(** @raise Dwarf.Parse_error if an unsupported address size is encountered. *)

val write_range_entry :
  Buffer.t -> Dwarf.DebugRnglists.range_entry -> int -> unit
(** @raise Dwarf.Parse_error if an unsupported address size is encountered. *)

val write_range_list : Buffer.t -> Dwarf.DebugRnglists.range_list -> int -> unit
(** @raise Dwarf.Parse_error if an unsupported address size is encountered. *)

val write_loclists_header : Buffer.t -> Dwarf.encoding -> int -> int -> unit
val write_rnglists_header : Buffer.t -> Dwarf.encoding -> int -> int -> unit

val write_debug_loc : Buffer.t -> Dwarf.DebugLoc.entry list -> int -> unit
(** @raise Dwarf.Parse_error if an unsupported address size is encountered. *)

val write_debug_ranges : Buffer.t -> Dwarf.DebugRanges.entry list -> int -> unit
(** @raise Dwarf.Parse_error if an unsupported address size is encountered. *)

(** {2 Line Program Writer} *)

val write_debug_line :
  Buffer.t ->
  ?line_str_table:string_table ->
  Dwarf.DebugLine.line_program_header ->
  Dwarf.DebugLine.line_table_entry list ->
  unit
(** @raise Dwarf.Parse_error
      if a directory or file entry format, or an address size, is unsupported.
*)

(** {2 CFI Writer} *)

(* TODO Why is this in write but not dwarf.mli for reading. Explain the
   difference. *)

(** A call frame instruction emitted into a [.debug_frame] or [.eh_frame]
    section.

    DWARF 5 specification, section 6.4.2 "Call Frame Instructions". *)
type cfi_op =
  | CFA_advance_loc of int
  | CFA_offset of int * int
  | CFA_restore of int
  | CFA_nop
  | CFA_set_loc of int
  | CFA_advance_loc1 of int
  | CFA_advance_loc2 of int
  | CFA_advance_loc4 of int
  | CFA_offset_extended of int * int
  | CFA_restore_extended of int
  | CFA_undefined of int
  | CFA_same_value of int
  | CFA_register of int * int
  | CFA_remember_state
  | CFA_restore_state
  | CFA_def_cfa of int * int
  | CFA_def_cfa_register of int
  | CFA_def_cfa_offset of int
  | CFA_def_cfa_expression of string
  | CFA_expression of int * string
  | CFA_offset_extended_sf of int * int
  | CFA_def_cfa_sf of int * int
  | CFA_def_cfa_offset_sf of int
  | CFA_val_offset of int * int
  | CFA_val_offset_sf of int * int
  | CFA_val_expression of int * string

val write_cfi_instruction : Buffer.t -> cfi_op -> unit
val write_cfi_instructions : Buffer.t -> cfi_op list -> unit
val write_cie : Buffer.t -> Dwarf.CallFrame.common_information_entry -> unit
val write_fde : Buffer.t -> Dwarf.CallFrame.frame_description_entry -> unit

val write_debug_frame :
  Buffer.t -> Dwarf.CallFrame.debug_frame_entry list -> unit

(** {2 .eh_frame Writer} *)

val write_eh_cie : Buffer.t -> Dwarf.CallFrame.common_information_entry -> unit

val write_eh_fde :
  Buffer.t ->
  Dwarf.CallFrame.frame_description_entry ->
  Eh_encoding.encoding ->
  int ->
  unit
(** [write_eh_fde buf fde encoding cie_offset] writes an FDE. [encoding] is the
    pointer encoding for [initial_location] declared by the owning CIE (see
    {!Eh_frame.fde_encoding_of_cie}); [cie_offset] is that CIE's byte offset in
    the section. {!write_eh_frame} supplies both automatically. *)

val write_eh_frame : Buffer.t -> Eh_frame.eh_frame_entry list -> unit

(** {2 .debug_aranges Writer} *)

val write_aranges_set : Buffer.t -> Dwarf.DebugAranges.aranges_set -> unit
(** @raise Dwarf.Parse_error if an unsupported address size is encountered. *)

(** {2 .debug_addr and .debug_str_offsets Writers} *)

val write_debug_addr : Buffer.t -> Dwarf.DebugAddr.t -> unit
(** @raise Dwarf.Parse_error if an unsupported address size is encountered. *)

val write_debug_str_offsets : Buffer.t -> Dwarf.DebugStrOffsets.t -> unit

(** {2 .debug_names Writer} *)

val write_debug_names_abbrev_table :
  Buffer.t -> Dwarf.DebugNames.debug_names_abbrev list -> unit

val write_debug_names : Buffer.t -> Dwarf.DebugNames.debug_names_section -> unit

(** {2 Split DWARF Index Writer} *)

val write_unit_index : Buffer.t -> Dwarf.SplitDwarf.unit_index -> unit

(** {2 .debug_macro Writer} *)

val write_debug_macro_entry :
  Buffer.t -> Dwarf.dwarf_format -> Dwarf.DebugMacro.entry -> unit

val write_debug_macro_unit : Buffer.t -> Dwarf.DebugMacro.macro_unit -> unit
val write_debug_macro : Buffer.t -> Dwarf.DebugMacro.section -> unit

(** {2 .debug_pubnames/.debug_pubtypes Writers} *)

val write_pubnames_set :
  Buffer.t ->
  Dwarf.DebugPubnames.header ->
  Dwarf.DebugPubnames.entry list ->
  unit

val write_pubtypes_set :
  Buffer.t ->
  Dwarf.DebugPubtypes.header ->
  Dwarf.DebugPubtypes.entry list ->
  unit

(** {2 .debug_types Writer} *)

val write_type_unit :
  Buffer.t ->
  Dwarf.encoding ->
  Dwarf.DIE.t ->
  (int -> u64) ->
  u64 ->
  u64 ->
  u64 ->
  unit
(** @raise Dwarf.Parse_error
      if a DIE attribute uses an unsupported form/value combination. *)
