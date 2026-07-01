open Dwarf_types
module Buffer = Stdlib.Buffer

(** {2 Low-level write helpers} *)

val write_u8 : Buffer.t -> u8 -> unit
(** Append a single byte. *)

val write_u16_le : Buffer.t -> u16 -> unit
(** Append a 16-bit value in little-endian byte order. *)

val write_u32_le : Buffer.t -> u32 -> unit
(** Append a 32-bit value in little-endian byte order. *)

val write_u64_le : Buffer.t -> u64 -> unit
(** Append a 64-bit value in little-endian byte order. *)

val write_i64_le : Buffer.t -> i64 -> unit
(** Append a signed 64-bit value in little-endian byte order. *)

val write_uleb128 : Buffer.t -> u64 -> unit
(** Append a value in unsigned LEB128 variable-length encoding. *)

val write_sleb128 : Buffer.t -> i64 -> unit
(** Append a value in signed LEB128 variable-length encoding. *)

val write_initial_length : Buffer.t -> Dwarf.dwarf_format -> int -> unit
(** Append a DWARF initial-length field sized by {!type:Dwarf.dwarf_format}. *)

val write_offset : Buffer.t -> Dwarf.dwarf_format -> u64 -> unit
(** Append a section offset, sized by {!type:Dwarf.dwarf_format}. *)

val write_address : Buffer.t -> int -> u64 -> unit
(** Append a target address whose width in bytes is given by the [int] argument
    (4 or 8).

    @raise Dwarf.Parse_error if the address size is unsupported. *)

val write_null_terminated_string : Buffer.t -> string -> unit
(** Append a string followed by a terminating NUL byte. *)

(** {2 Abbreviation Table} *)

val form_for_attribute_value :
  Dwarf.DIE.attribute_value -> Dwarf.attribute_form_encoding
(** Pick an {!type:Dwarf.attribute_form_encoding} capable of encoding the given
    {!Dwarf.DIE.attribute_value}. *)

type die_shape = {
  tag : Dwarf.abbreviation_tag;
  has_children : bool;
  attr_forms : (Dwarf.attribute_encoding * Dwarf.attribute_form_encoding) list;
}
(** The abbreviation-relevant shape of a DIE: its tag, whether it has children,
    and the (attribute, form) pairs of its attributes in order. DIEs with an
    identical shape can share a single abbreviation. *)

val shape_of_die : Dwarf.DIE.t -> die_shape
(** Compute the {!die_shape} of a DIE, choosing a form for each attribute via
    {!form_for_attribute_value}. *)

val assign_abbreviations : Dwarf.DIE.t list -> Dwarf.abbrev array * (int -> u64)
(** Assign abbreviation codes across a forest of DIEs, deduplicating identical
    shapes. Returns the abbreviation table and a lookup mapping a DIE's
    section-relative offset to its assigned abbreviation code. *)

val write_abbrev_table : Buffer.t -> Dwarf.abbrev array -> unit
(** Serialise an abbreviation table into [.debug_abbrev] form, terminated by a
    null entry. *)

val abbrev_table_size : Dwarf.abbrev array -> int
(** Number of bytes {!write_abbrev_table} would emit for the table. *)

val uleb128_size : u64 -> int
(** Number of bytes the unsigned LEB128 encoding of the value occupies. *)

val sleb128_size : i64 -> int
(** Number of bytes the signed LEB128 encoding of the value occupies. *)

(** {2 Attribute Value Serialisation} *)

val write_attribute_value :
  Buffer.t ->
  Dwarf.DIE.attribute_value ->
  Dwarf.attribute_form_encoding ->
  Dwarf.encoding ->
  unit
(** Serialise an {!Dwarf.DIE.attribute_value} with the given
    {!type:Dwarf.attribute_form_encoding} under the {!Dwarf.encoding}, appending
    the bytes to the buffer.

    @raise Dwarf.Parse_error
      if the form/value combination or address size is unsupported. *)

val attribute_value_size :
  Dwarf.DIE.attribute_value ->
  Dwarf.attribute_form_encoding ->
  Dwarf.encoding ->
  int
(** Number of bytes {!write_attribute_value} would emit for this value, form and
    encoding.

    @raise Dwarf.Parse_error if the form/value combination is unsupported. *)

(** {2 DIE Tree Serialisation} *)

val write_die :
  Buffer.t -> Dwarf.DIE.t -> Dwarf.encoding -> (int -> u64) -> unit
(** Serialise a DIE and its child subtree: the abbreviation code (from the
    [(int -> u64)] lookup keyed on the DIE's offset), its attribute values, and
    recursively its children terminated by a null entry.

    @raise Dwarf.Parse_error
      if a DIE attribute uses an unsupported form/value combination. *)

val die_size : Dwarf.DIE.t -> Dwarf.encoding -> (int -> u64) -> int
(** Number of bytes {!write_die} would emit for the DIE and its subtree.

    @raise Dwarf.Parse_error
      if a DIE attribute uses an unsupported form/value combination. *)

val write_die_forest :
  Buffer.t -> Dwarf.DIE.t list -> Dwarf.encoding -> (int -> u64) -> unit
(** Serialise a list of sibling DIEs, each with its subtree, via {!write_die}.

    @raise Dwarf.Parse_error
      if a DIE attribute uses an unsupported form/value combination. *)

(** {2 Compilation Unit & Top-Level} *)

val write_compile_unit :
  Buffer.t -> Dwarf.encoding -> Dwarf.DIE.t -> (int -> u64) -> u64 -> unit
(** Serialise a complete compilation unit: the unit header (with the given
    [.debug_abbrev] offset) followed by the root DIE and its subtree. The
    [(int -> u64)] argument is the abbreviation-code lookup.

    @raise Dwarf.Parse_error
      if a DIE attribute uses an unsupported form/value combination. *)

val write_debug_info : Dwarf.encoding -> Dwarf.DIE.t list -> string * string
(** Serialise a forest of compilation-unit root DIEs, returning the
    [(.debug_info, .debug_abbrev)] section contents. Abbreviations are assigned
    and deduplicated across all units (see {!assign_abbreviations}).

    @raise Dwarf.Parse_error
      if a DIE attribute uses an unsupported form/value combination. *)

(** {2 String Table} *)

type string_table
(** A builder for a string section ([.debug_str] or [.debug_line_str]). It
    accumulates strings, deduplicating equal ones, and assigns each a byte
    offset within the section. Reading resolves strings directly by offset (see
    {!Dwarf.DebugStr}). *)

val create_string_table : unit -> string_table
(** Create an empty string table. *)

val add_string : string_table -> string -> int
(** Add [s] to the table — reusing an equal string already present — and return
    its byte offset within the section. *)

val write_string_table : Buffer.t -> string_table -> unit
(** Append the accumulated string blob to the buffer. *)

val string_table_size : string_table -> int
(** Total size in bytes of the accumulated string blob. *)

val write_debug_str : Buffer.t -> string_table -> unit
(** Write the table as a [.debug_str] section (same layout as
    {!write_string_table}). *)

val write_debug_line_str : Buffer.t -> string_table -> unit
(** Write the table as a [.debug_line_str] section (same layout as
    {!write_string_table}). *)

(** {2 Expression Encoding} *)

val write_expression :
  Buffer.t -> Dwarf.dwarf_expression_operation list -> Dwarf.encoding -> unit
(** Serialise a DWARF expression — a sequence of operations — to its byte form.

    @raise Dwarf.Parse_error if an operation is unsupported for writing. *)

(** {2 Location/Range Lists} *)

val write_location_entry :
  Buffer.t -> Dwarf.DebugLoclists.location_entry -> int -> unit
(** Serialise a single [.debug_loclists] entry (a DW_LLE_ record) using the
    given address size in bytes.

    @raise Dwarf.Parse_error if an unsupported address size is encountered. *)

val write_location_list :
  Buffer.t -> Dwarf.DebugLoclists.location_list -> int -> unit
(** Serialise a complete location list (its entries followed by an end-of-list
    marker) using the given address size in bytes.

    @raise Dwarf.Parse_error if an unsupported address size is encountered. *)

val write_range_entry :
  Buffer.t -> Dwarf.DebugRnglists.range_entry -> int -> unit
(** Serialise a single [.debug_rnglists] entry (a DW_RLE_ record) using the
    given address size in bytes.

    @raise Dwarf.Parse_error if an unsupported address size is encountered. *)

val write_range_list : Buffer.t -> Dwarf.DebugRnglists.range_list -> int -> unit
(** Serialise a complete range list (its entries followed by an end-of-list
    marker) using the given address size in bytes.

    @raise Dwarf.Parse_error if an unsupported address size is encountered. *)

val write_loclists_header : Buffer.t -> Dwarf.encoding -> int -> int -> unit
(** Write a [.debug_loclists] contribution header. The [int] arguments are the
    number of offset-table entries and the size in bytes of the lists body. *)

val write_rnglists_header : Buffer.t -> Dwarf.encoding -> int -> int -> unit
(** Write a [.debug_rnglists] contribution header. The [int] arguments are the
    number of offset-table entries and the size in bytes of the lists body. *)

val write_debug_loc : Buffer.t -> Dwarf.DebugLoc.entry list -> int -> unit
(** Serialise a legacy DWARF 4 [.debug_loc] location list using the given
    address size in bytes.

    @raise Dwarf.Parse_error if an unsupported address size is encountered. *)

val write_debug_ranges : Buffer.t -> Dwarf.DebugRanges.entry list -> int -> unit
(** Serialise a legacy DWARF 4 [.debug_ranges] range list using the given
    address size in bytes.

    @raise Dwarf.Parse_error if an unsupported address size is encountered. *)

(** {2 Line Program Writer} *)

val write_debug_line :
  Buffer.t ->
  ?line_str_table:string_table ->
  Dwarf.DebugLine.line_program_header ->
  Dwarf.DebugLine.line_table_entry list ->
  unit
(** Serialise a [.debug_line] line-number program: the header (directory and
    file tables) followed by the line-number state-machine opcodes for the given
    entries. [?line_str_table] supplies the [.debug_line_str] offsets for
    file/directory names encoded with DW_FORM_line_strp.

    @raise Dwarf.Parse_error
      if a directory or file entry format, or an address size, is unsupported.
*)

(** {2 CFI Writer} *)

(** Write call frame instructions into a [.debug_frame] or [.eh_frame] buffer.
    The instruction type {!Dwarf.CallFrame.cfi_op} and its byte encoding
    ({!Dwarf.CallFrame.encode_instructions}) live with the other CFI types in
    {!Dwarf.CallFrame}; these are the buffer-writing wrappers.

    DWARF 5 specification, section 6.4.2 "Call Frame Instructions". *)

val write_cfi_instruction : Buffer.t -> Dwarf.CallFrame.cfi_op -> unit
(** Encode a single call frame instruction (see
    {!Dwarf.CallFrame.encode_instructions}) and append it to the buffer. *)

val write_cfi_instructions : Buffer.t -> Dwarf.CallFrame.cfi_op list -> unit
(** Encode a sequence of call frame instructions and append them to the buffer.
*)

val write_cie : Buffer.t -> Dwarf.CallFrame.common_information_entry -> unit
(** Write a Common Information Entry (CIE) in [.debug_frame] format. *)

val write_fde : Buffer.t -> Dwarf.CallFrame.frame_description_entry -> unit
(** Write a Frame Description Entry (FDE) in [.debug_frame] format. *)

val write_debug_frame :
  Buffer.t -> Dwarf.CallFrame.debug_frame_entry list -> unit
(** Write a complete [.debug_frame] section: its CIEs and FDEs in order. *)

(** {2 .eh_frame Writer} *)

val write_eh_cie : Buffer.t -> Dwarf.CallFrame.common_information_entry -> unit
(** Write a Common Information Entry (CIE) in [.eh_frame] format, which differs
    from [.debug_frame] in its length and CIE-pointer conventions. *)

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
(** Write a complete [.eh_frame] section from its CIE/FDE entries, computing CIE
    back-pointers and pointer encodings automatically. *)

(** {2 .debug_aranges Writer} *)

val write_aranges_set : Buffer.t -> Dwarf.DebugAranges.aranges_set -> unit
(** Serialise a [.debug_aranges] set: its header followed by the address-range
    descriptors, terminated by a zero entry.

    @raise Dwarf.Parse_error if an unsupported address size is encountered. *)

(** {2 .debug_addr and .debug_str_offsets Writers} *)

val write_debug_addr : Buffer.t -> Dwarf.DebugAddr.t -> unit
(** Serialise a [.debug_addr] contribution: its header followed by the address
    table.

    @raise Dwarf.Parse_error if an unsupported address size is encountered. *)

val write_debug_str_offsets : Buffer.t -> Dwarf.DebugStrOffsets.t -> unit
(** Serialise a [.debug_str_offsets] contribution: its header followed by the
    array of offsets into [.debug_str]. *)

(** {2 .debug_names Writer} *)

val write_debug_names_abbrev_table :
  Buffer.t -> Dwarf.DebugNames.debug_names_abbrev list -> unit
(** Write the abbreviation table of a [.debug_names] index — the per-entry
    formats describing each name-index entry. *)

val write_debug_names : Buffer.t -> Dwarf.DebugNames.debug_names_section -> unit
(** Serialise a complete [.debug_names] accelerated name index: its header, the
    hash and offset tables, the abbreviation table, and the entry pool. *)

(** {2 Split DWARF Index Writer} *)

val write_unit_index : Buffer.t -> Dwarf.SplitDwarf.unit_index -> unit
(** Serialise a DWARF package index ([.debug_cu_index] or [.debug_tu_index])
    mapping unit signatures to their section contributions within a [.dwp] file.
*)

(** {2 .debug_macro Writer} *)

val write_debug_macro_entry :
  Buffer.t -> Dwarf.dwarf_format -> Dwarf.DebugMacro.entry -> unit
(** Serialise a single [.debug_macro] entry (a DW_MACRO_ record). *)

val write_debug_macro_unit : Buffer.t -> Dwarf.DebugMacro.macro_unit -> unit
(** Serialise one macro unit: its header followed by its entries, terminated by
    a null entry. *)

val write_debug_macro : Buffer.t -> Dwarf.DebugMacro.section -> unit
(** Serialise a complete [.debug_macro] section — all of its macro units. *)

(** {2 .debug_pubnames/.debug_pubtypes Writers} *)

val write_pubnames_set :
  Buffer.t ->
  Dwarf.DebugPubnames.header ->
  Dwarf.DebugPubnames.entry list ->
  unit
(** Serialise a [.debug_pubnames] set: its header followed by the (offset, name)
    pairs, terminated by a zero entry. *)

val write_pubtypes_set :
  Buffer.t ->
  Dwarf.DebugPubtypes.header ->
  Dwarf.DebugPubtypes.entry list ->
  unit
(** Serialise a [.debug_pubtypes] set: its header followed by the (offset, name)
    pairs, terminated by a zero entry. *)

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
(** Serialise a [.debug_types] type unit: the type-unit header followed by the
    root DIE and its subtree. After the abbreviation-code lookup, the three
    [u64] arguments are the type signature, the offset of the type DIE within
    the unit, and the [.debug_abbrev] offset.

    @raise Dwarf.Parse_error
      if a DIE attribute uses an unsupported form/value combination. *)
