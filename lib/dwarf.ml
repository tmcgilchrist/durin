(* DWARF 5 *)

open Types

type dwarf_section =
  | Debug_info
  | Debug_abbrev
  | Debug_aranges
  | Debug_line
  | Debug_line_str
  | Debug_loclists
  | Debug_rnglists
  | Debug_str
  | Debug_str_offs
  | Debug_names
  | Debug_addr

type object_format = MachO | ELF

let string_of_object_format = function MachO -> "Mach-O" | ELF -> "ELF"

(* TODO Move some of this into "object" library *)

(** Detect file format from buffer using magic numbers *)
let detect_format (buf : Object.Buffer.t) : object_format =
  let cursor = Object.Buffer.cursor buf in
  let magic = Object.Buffer.Read.u32 cursor in
  let magic_int = Unsigned.UInt32.to_int magic in
  match magic_int with
  | 0x7f454c46 -> ELF (* ELF magic: \x7fELF big-endian *)
  | 0x464c457f -> ELF (* ELF magic: \x7fELF little-endian *)
  | 0xFEEDFACE | 0xFEEDFACF | 0xCEFAEDFE | 0xCFFAEDFE ->
      MachO (* Mach-O magics *)
  | _ -> failwith "Unsupported file format"

(** Convert Mach-O cpu_type to architecture string *)
let string_of_cpu_type = function
  | `X86 -> "i386"
  | `X86_64 -> "x86_64"
  | `ARM -> "arm"
  | `ARM64 -> "arm64"
  | `ARM64_32 -> "arm64_32"
  | `POWERPC -> "ppc"
  | `POWERPC64 -> "ppc64"
  | `Unknown x -> Printf.sprintf "unknown_%d" x

(** Detect file format and architecture from buffer *)
let detect_format_and_arch (buf : Object.Buffer.t) : string =
  let format = detect_format buf in
  match format with
  | ELF ->
      (* For ELF, we would need to read the e_machine field from the ELF header
         For now, we'll default to a generic ELF string *)
      "ELF"
  | MachO ->
      let header, _commands = Object.Macho.read buf in
      let arch_str = string_of_cpu_type header.cpu_type in
      Printf.sprintf "Mach-O %s" arch_str

type unit_type =
  | DW_UT_compile
  | DW_UT_type
  | DW_UT_partial
  | DW_UT_skeleton
  | DW_UT_split_compile
  | DW_UT_split_type
  | DW_UT_lo_user
  | DW_UT_hi_user

let unit_type_of_u8 u =
  match Unsigned.UInt8.to_int u with
  | 0x01 -> DW_UT_compile
  | 0x02 -> DW_UT_type
  | 0x03 -> DW_UT_partial
  | 0x04 -> DW_UT_skeleton
  | 0x05 -> DW_UT_split_compile
  | 0x06 -> DW_UT_split_type
  | 0x80 -> DW_UT_lo_user
  | 0xff -> DW_UT_hi_user
  | n -> failwith (Printf.sprintf "Unknown unit type: 0x%02x" n)

let string_of_unit_type = function
  | DW_UT_compile -> "DW_UT_compile"
  | DW_UT_type -> "DW_UT_type"
  | DW_UT_partial -> "DW_UT_partial"
  | DW_UT_skeleton -> "DW_UT_skeleton"
  | DW_UT_split_compile -> "DW_UT_split_compile"
  | DW_UT_split_type -> "DW_UT_split_type"
  | DW_UT_lo_user -> "DW_UT_lo_user"
  | DW_UT_hi_user -> "DW_UT_hi_user"

let object_format_to_section_name format section =
  match format with
  | MachO -> (
      match section with
      | Debug_info -> "__debug_info"
      | Debug_abbrev -> "__debug_abbrev"
      | Debug_aranges -> "__debug_aranges"
      | Debug_line -> "__debug_line"
      | Debug_line_str -> "__debug_line_str"
      | Debug_loclists -> "__debug_loclists"
      | Debug_rnglists -> "__debug_rnglists"
      | Debug_str -> "__debug_str"
      | Debug_str_offs -> "__debug_str_offs"
      | Debug_names -> "__debug_names"
      | Debug_addr -> "__debug_addr")
  | ELF -> (
      match section with
      | Debug_info -> ".debug_info"
      | Debug_abbrev -> ".debug_abbrev"
      | Debug_aranges -> ".debug_aranges"
      | Debug_line -> ".debug_line"
      | Debug_line_str -> ".debug_line_str"
      | Debug_loclists -> ".debug_loclists"
      | Debug_rnglists -> ".debug_rnglists"
      | Debug_str -> ".debug_str"
      | Debug_str_offs -> ".debug_str_offs"
      | Debug_names -> ".debug_names"
      | Debug_addr -> ".debug_addr")

type abbreviation_tag =
  | DW_TAG_array_type
  | DW_TAG_class_type
  | DW_TAG_entry_point
  | DW_TAG_enumeration_type
  | DW_TAG_formal_parameter
  | DW_TAG_imported_declaration
  | DW_TAG_label
  | DW_TAG_lexical_block
  | DW_TAG_member
  | DW_TAG_pointer_type
  | DW_TAG_reference_type
  | DW_TAG_compile_unit
  | DW_TAG_string_type
  | DW_TAG_structure_type
  | DW_TAG_subroutine_type
  | DW_TAG_typedef
  | DW_TAG_union_type
  | DW_TAG_unspecified_parameters
  | DW_TAG_variant
  | DW_TAG_common_block
  | DW_TAG_common_inclusion
  | DW_TAG_inheritance
  | DW_TAG_inlined_subroutine
  | DW_TAG_module
  | DW_TAG_ptr_to_member_type
  | DW_TAG_set_type
  | DW_TAG_subrange_type
  | DW_TAG_with_stmt
  | DW_TAG_access_declaration
  | DW_TAG_base_type
  | DW_TAG_catch_block
  | DW_TAG_const_type
  | DW_TAG_constant
  | DW_TAG_enumerator
  | DW_TAG_file_type
  | DW_TAG_friend
  | DW_TAG_namelist
  | DW_TAG_namelist_item
  | DW_TAG_packed_type
  | DW_TAG_subprogram
  | DW_TAG_template_type_parameter
  | DW_TAG_template_value_parameter
  | DW_TAG_thrown_type
  | DW_TAG_try_block
  | DW_TAG_variant_part
  | DW_TAG_variable
  | DW_TAG_volatile_type
  | DW_TAG_dwarf_procedure
  | DW_TAG_restrict_type
  | DW_TAG_interface_type
  | DW_TAG_namespace
  | DW_TAG_imported_module
  | DW_TAG_unspecified_type
  | DW_TAG_partial_unit
  | DW_TAG_imported_unit
  | DW_TAG_condition
  | DW_TAG_shared_type
  | DW_TAG_type_unit
  | DW_TAG_rvalue_reference_type
  | DW_TAG_template_alias
  (* New in DWARF Version 5 *)
  | DW_TAG_coarray_type
  | DW_TAG_generic_subrange
  | DW_TAG_dynamic_type
  | DW_TAG_atomic_type
  | DW_TAG_call_site
  | DW_TAG_call_site_parameter
  | DW_TAG_skeleton_unit
  | DW_TAG_immutable_type
  | DW_TAG_lo_user
  | DW_TAG_hi_user

let abbreviation_tag_of_int tag_code =
  match Unsigned.UInt64.to_int tag_code with
  | 0x01 -> DW_TAG_array_type
  | 0x02 -> DW_TAG_class_type
  | 0x03 -> DW_TAG_entry_point
  | 0x04 -> DW_TAG_enumeration_type
  | 0x05 -> DW_TAG_formal_parameter
  | 0x08 -> DW_TAG_imported_declaration
  | 0x0a -> DW_TAG_label
  | 0x0b -> DW_TAG_lexical_block
  | 0x0d -> DW_TAG_member
  | 0x0f -> DW_TAG_pointer_type
  | 0x10 -> DW_TAG_reference_type
  | 0x11 -> DW_TAG_compile_unit
  | 0x12 -> DW_TAG_string_type
  | 0x13 -> DW_TAG_structure_type
  | 0x15 -> DW_TAG_subroutine_type
  | 0x16 -> DW_TAG_typedef
  | 0x17 -> DW_TAG_union_type
  | 0x18 -> DW_TAG_unspecified_parameters
  | 0x19 -> DW_TAG_variant
  | 0x1a -> DW_TAG_common_block
  | 0x1b -> DW_TAG_common_inclusion
  | 0x1c -> DW_TAG_inheritance
  | 0x1d -> DW_TAG_inlined_subroutine
  | 0x1e -> DW_TAG_module
  | 0x1f -> DW_TAG_ptr_to_member_type
  | 0x20 -> DW_TAG_set_type
  | 0x21 -> DW_TAG_subrange_type
  | 0x22 -> DW_TAG_with_stmt
  | 0x23 -> DW_TAG_access_declaration
  | 0x24 -> DW_TAG_base_type
  | 0x25 -> DW_TAG_catch_block
  | 0x26 -> DW_TAG_const_type
  | 0x27 -> DW_TAG_constant
  | 0x28 -> DW_TAG_enumerator
  | 0x29 -> DW_TAG_file_type
  | 0x2a -> DW_TAG_friend
  | 0x2b -> DW_TAG_namelist
  | 0x2c -> DW_TAG_namelist_item
  | 0x2d -> DW_TAG_packed_type
  | 0x2e -> DW_TAG_subprogram
  | 0x2f -> DW_TAG_template_type_parameter
  | 0x30 -> DW_TAG_template_value_parameter
  | 0x31 -> DW_TAG_thrown_type
  | 0x32 -> DW_TAG_try_block
  | 0x33 -> DW_TAG_variant_part
  | 0x34 -> DW_TAG_variable
  | 0x35 -> DW_TAG_volatile_type
  | 0x36 -> DW_TAG_dwarf_procedure
  | 0x37 -> DW_TAG_restrict_type
  | 0x38 -> DW_TAG_interface_type
  | 0x39 -> DW_TAG_namespace
  | 0x3a -> DW_TAG_imported_module
  | 0x3b -> DW_TAG_unspecified_type
  | 0x3c -> DW_TAG_partial_unit
  | 0x3d -> DW_TAG_imported_unit
  | 0x3f -> DW_TAG_condition
  | 0x40 -> DW_TAG_shared_type
  | 0x41 -> DW_TAG_type_unit
  | 0x42 -> DW_TAG_rvalue_reference_type
  | 0x43 -> DW_TAG_template_alias
  | 0x44 -> DW_TAG_coarray_type
  | 0x45 -> DW_TAG_generic_subrange
  | 0x46 -> DW_TAG_dynamic_type
  | 0x47 -> DW_TAG_atomic_type
  | 0x48 -> DW_TAG_call_site
  | 0x49 -> DW_TAG_call_site_parameter
  | 0x4a -> DW_TAG_skeleton_unit
  | 0x4b -> DW_TAG_immutable_type
  | 0x4080 -> DW_TAG_lo_user
  | 0xffff -> DW_TAG_hi_user
  | n -> failwith (Printf.sprintf "Unknown tag encoding: 0x%02x" n)

(* TODO Rename various conversion functions consistently. *)
let uint64_of_abbreviation_tag tag =
  let code =
    match tag with
    | DW_TAG_array_type -> 0x01
    | DW_TAG_class_type -> 0x02
    | DW_TAG_entry_point -> 0x03
    | DW_TAG_enumeration_type -> 0x04
    | DW_TAG_formal_parameter -> 0x05
    | DW_TAG_imported_declaration -> 0x08
    | DW_TAG_label -> 0x0a
    | DW_TAG_lexical_block -> 0x0b
    | DW_TAG_member -> 0x0d
    | DW_TAG_pointer_type -> 0x0f
    | DW_TAG_reference_type -> 0x10
    | DW_TAG_compile_unit -> 0x11
    | DW_TAG_string_type -> 0x12
    | DW_TAG_structure_type -> 0x13
    | DW_TAG_subroutine_type -> 0x15
    | DW_TAG_typedef -> 0x16
    | DW_TAG_union_type -> 0x17
    | DW_TAG_unspecified_parameters -> 0x18
    | DW_TAG_variant -> 0x19
    | DW_TAG_common_block -> 0x1a
    | DW_TAG_common_inclusion -> 0x1b
    | DW_TAG_inheritance -> 0x1c
    | DW_TAG_inlined_subroutine -> 0x1d
    | DW_TAG_module -> 0x1e
    | DW_TAG_ptr_to_member_type -> 0x1f
    | DW_TAG_set_type -> 0x20
    | DW_TAG_subrange_type -> 0x21
    | DW_TAG_with_stmt -> 0x22
    | DW_TAG_access_declaration -> 0x23
    | DW_TAG_base_type -> 0x24
    | DW_TAG_catch_block -> 0x25
    | DW_TAG_const_type -> 0x26
    | DW_TAG_constant -> 0x27
    | DW_TAG_enumerator -> 0x28
    | DW_TAG_file_type -> 0x29
    | DW_TAG_friend -> 0x2a
    | DW_TAG_namelist -> 0x2b
    | DW_TAG_namelist_item -> 0x2c
    | DW_TAG_packed_type -> 0x2d
    | DW_TAG_subprogram -> 0x2e
    | DW_TAG_template_type_parameter -> 0x2f
    | DW_TAG_template_value_parameter -> 0x30
    | DW_TAG_thrown_type -> 0x31
    | DW_TAG_try_block -> 0x32
    | DW_TAG_variant_part -> 0x33
    | DW_TAG_variable -> 0x34
    | DW_TAG_volatile_type -> 0x35
    | _ -> 0 (* Unknown tag *)
  in
  Unsigned.UInt64.of_int code

let string_of_abbreviation_tag tag_code =
  match Unsigned.UInt64.to_int tag_code with
  | 0x01 -> "DW_TAG_array_type"
  | 0x02 -> "DW_TAG_class_type"
  | 0x03 -> "DW_TAG_entry_point"
  | 0x04 -> "DW_TAG_enumeration_type"
  | 0x05 -> "DW_TAG_formal_parameter"
  | 0x08 -> "DW_TAG_imported_declaration"
  | 0x0a -> "DW_TAG_label"
  | 0x0b -> "DW_TAG_lexical_block"
  | 0x0d -> "DW_TAG_member"
  | 0x0f -> "DW_TAG_pointer_type"
  | 0x10 -> "DW_TAG_reference_type"
  | 0x11 -> "DW_TAG_compile_unit"
  | 0x12 -> "DW_TAG_string_type"
  | 0x13 -> "DW_TAG_structure_type"
  | 0x15 -> "DW_TAG_subroutine_type"
  | 0x16 -> "DW_TAG_typedef"
  | 0x17 -> "DW_TAG_union_type"
  | 0x18 -> "DW_TAG_unspecified_parameters"
  | 0x19 -> "DW_TAG_variant"
  | 0x1a -> "DW_TAG_common_block"
  | 0x1b -> "DW_TAG_common_inclusion"
  | 0x1c -> "DW_TAG_inheritance"
  | 0x1d -> "DW_TAG_inlined_subroutine"
  | 0x1e -> "DW_TAG_module"
  | 0x1f -> "DW_TAG_ptr_to_member_type"
  | 0x20 -> "DW_TAG_set_type"
  | 0x21 -> "DW_TAG_subrange_type"
  | 0x22 -> "DW_TAG_with_stmt"
  | 0x23 -> "DW_TAG_access_declaration"
  | 0x24 -> "DW_TAG_base_type"
  | 0x25 -> "DW_TAG_catch_block"
  | 0x26 -> "DW_TAG_const_type"
  | 0x27 -> "DW_TAG_constant"
  | 0x28 -> "DW_TAG_enumerator"
  | 0x29 -> "DW_TAG_file_type"
  | 0x2a -> "DW_TAG_friend"
  | 0x2b -> "DW_TAG_namelist"
  | 0x2c -> "DW_TAG_namelist_item"
  | 0x2d -> "DW_TAG_packed_type"
  | 0x2e -> "DW_TAG_subprogram"
  | 0x2f -> "DW_TAG_template_type_parameter"
  | 0x30 -> "DW_TAG_template_value_parameter"
  | 0x31 -> "DW_TAG_thrown_type"
  | 0x32 -> "DW_TAG_try_block"
  | 0x33 -> "DW_TAG_variant_part"
  | 0x34 -> "DW_TAG_variable"
  | 0x35 -> "DW_TAG_volatile_type"
  | 0x36 -> "DW_TAG_dwarf_procedure"
  | 0x37 -> "DW_TAG_restrict_type"
  | 0x38 -> "DW_TAG_interface_type"
  | 0x39 -> "DW_TAG_namespace"
  | 0x3a -> "DW_TAG_imported_module"
  | 0x3b -> "DW_TAG_unspecified_type"
  | 0x3c -> "DW_TAG_partial_unit"
  | 0x3d -> "DW_TAG_imported_unit"
  | 0x3f -> "DW_TAG_condition"
  | 0x40 -> "DW_TAG_shared_type"
  | 0x41 -> "DW_TAG_type_unit"
  | 0x42 -> "DW_TAG_rvalue_reference_type"
  | 0x43 -> "DW_TAG_template_alias"
  | 0x4107 -> "DW_TAG_GNU_template_parameter_pack"
  | code -> Printf.sprintf "DW_TAG_<0x%02x>" code

type children_determination = DW_CHILDREN_no | DW_CHILDREN_yes

let children_determination = function
  | 0x00 -> DW_CHILDREN_no
  | 0x01 -> DW_CHILDREN_yes

type attribute_encoding =
  | DW_AT_sibling
  | DW_AT_location
  | DW_AT_name
  | DW_AT_ordering
  | DW_AT_byte_size
  | DW_AT_bit_size
  | DW_AT_stmt_list
  | DW_AT_low_pc
  | DW_AT_high_pc
  | DW_AT_language
  | DW_AT_discr
  | DW_AT_discr_value
  | DW_AT_visibility
  | DW_AT_import
  | DW_AT_string_length
  | DW_AT_common_reference
  | DW_AT_comp_dir
  | DW_AT_const_value
  | DW_AT_containing_type
  | DW_AT_default_value
  | DW_AT_inline
  | DW_AT_is_optional
  | DW_AT_lower_bound
  | DW_AT_producer
  | DW_AT_prototyped
  | DW_AT_return_addr
  | DW_AT_start_scope
  | DW_AT_bit_stride
  | DW_AT_upper_bound
  | DW_AT_abstract_origin
  | DW_AT_accessibility
  | DW_AT_address_class
  | DW_AT_artificial
  | DW_AT_base_types
  | DW_AT_calling_convention
  | DW_AT_count
  | DW_AT_data_member_location
  | DW_AT_decl_column
  | DW_AT_decl_file
  | DW_AT_decl_line
  | DW_AT_declaration
  | DW_AT_discr_list
  | DW_AT_encoding
  | DW_AT_external
  | DW_AT_frame_base
  | DW_AT_friend
  | DW_AT_identifier_case
  | DW_AT_namelist_item
  | DW_AT_priority
  | DW_AT_segment
  | DW_AT_specification
  | DW_AT_static_link
  | DW_AT_type
  | DW_AT_use_location
  | DW_AT_variable_parameter
  | DW_AT_virtuality
  | DW_AT_vtable_elem_location
  | DW_AT_allocated
  | DW_AT_associated
  | DW_AT_data_location
  | DW_AT_byte_stride
  | DW_AT_entry_pc
  | DW_AT_use_UTF8
  | DW_AT_extension
  | DW_AT_ranges
  | DW_AT_trampoline
  | DW_AT_call_column
  | DW_AT_call_file
  | DW_AT_call_line
  | DW_AT_description
  | DW_AT_binary_scale
  | DW_AT_decimal_scale
  | DW_AT_small
  | DW_AT_decimal_sign
  | DW_AT_digit_count
  | DW_AT_picture_string
  | DW_AT_mutable
  | DW_AT_threads_scaled
  | DW_AT_explicit
  | DW_AT_object_pointer
  | DW_AT_endianity
  | DW_AT_elemental
  | DW_AT_pure
  | DW_AT_recursive
  | DW_AT_signature
  | DW_AT_main_subprogram
  | DW_AT_data_bit_offset
  | DW_AT_const_expr
  | DW_AT_enum_class
  | DW_AT_linkage_name
  (* New in DWARF Version 5 *)
  | DW_AT_string_length_bit_size
  | DW_AT_string_length_byte_size
  | DW_AT_rank
  | DW_AT_str_offsets_base
  | DW_AT_addr_base
  | DW_AT_rnglists_base
  | DW_AT_dwo_name
  | DW_AT_reference
  | DW_AT_rvalue_reference
  | DW_AT_macros
  | DW_AT_call_all_calls
  | DW_AT_call_all_source_calls
  | DW_AT_call_all_tail_calls
  | DW_AT_call_return_pc
  | DW_AT_call_value
  | DW_AT_call_origin
  | DW_AT_call_parameter
  | DW_AT_call_pc
  | DW_AT_call_tail_call
  | DW_AT_call_target
  | DW_AT_call_target_clobbered
  | DW_AT_call_data_location
  | DW_AT_call_data_value
  | DW_AT_noreturn
  | DW_AT_alignment
  | DW_AT_export_symbols
  | DW_AT_deleted
  | DW_AT_defaulted
  | DW_AT_loclists_base
  | DW_AT_lo_user
  | DW_AT_hi_user
  (* LLVM and Apple extensions *)
  | DW_AT_LLVM_sysroot
  | DW_AT_APPLE_omit_frame_ptr
  | DW_AT_APPLE_sdk

let string_of_attribute_code attr_code =
  match Unsigned.UInt64.to_int attr_code with
  | 0x01 -> "DW_AT_sibling"
  | 0x02 -> "DW_AT_location"
  | 0x03 -> "DW_AT_name"
  | 0x04 -> "DW_AT_reserved_4"
  | 0x05 -> "DW_AT_fund_type"
  | 0x06 -> "DW_AT_mod_fund_type"
  | 0x07 -> "DW_AT_user_def_type"
  | 0x08 -> "DW_AT_mod_u_d_type"
  | 0x09 -> "DW_AT_ordering"
  | 0x0a -> "DW_AT_subscr_data"
  | 0x0b -> "DW_AT_byte_size"
  | 0x0c -> "DW_AT_bit_offset"
  | 0x0d -> "DW_AT_bit_size"
  | 0x0e -> "DW_AT_reserved_0e"
  | 0x0f -> "DW_AT_element_list"
  | 0x10 -> "DW_AT_stmt_list"
  | 0x11 -> "DW_AT_low_pc"
  | 0x12 -> "DW_AT_high_pc"
  | 0x13 -> "DW_AT_language"
  | 0x14 -> "DW_AT_member"
  | 0x15 -> "DW_AT_discr"
  | 0x16 -> "DW_AT_discr_value"
  | 0x17 -> "DW_AT_visibility"
  | 0x18 -> "DW_AT_import"
  | 0x19 -> "DW_AT_string_length"
  | 0x1a -> "DW_AT_common_reference"
  | 0x1b -> "DW_AT_comp_dir"
  | 0x1c -> "DW_AT_const_value"
  | 0x1d -> "DW_AT_containing_type"
  | 0x1e -> "DW_AT_default_value"
  | 0x1f -> "DW_AT_reserved_1f"
  | 0x20 -> "DW_AT_inline"
  | 0x21 -> "DW_AT_is_optional"
  | 0x22 -> "DW_AT_lower_bound"
  | 0x23 -> "DW_AT_reserved_23"
  | 0x24 -> "DW_AT_reserved_24"
  | 0x25 -> "DW_AT_producer"
  | 0x26 -> "DW_AT_reserved_26"
  | 0x27 -> "DW_AT_prototyped"
  | 0x28 -> "DW_AT_reserved_28"
  | 0x29 -> "DW_AT_reserved_29"
  | 0x2a -> "DW_AT_return_addr"
  | 0x2b -> "DW_AT_reserved_2b"
  | 0x2c -> "DW_AT_start_scope"
  | 0x2d -> "DW_AT_reserved_2d"
  | 0x2e -> "DW_AT_bit_stride"
  | 0x2f -> "DW_AT_upper_bound"
  | 0x30 -> "DW_AT_reserved_30"
  | 0x31 -> "DW_AT_abstract_origin"
  | 0x32 -> "DW_AT_accessibility"
  | 0x33 -> "DW_AT_address_class"
  | 0x34 -> "DW_AT_artificial"
  | 0x35 -> "DW_AT_base_types"
  | 0x36 -> "DW_AT_calling_convention"
  | 0x37 -> "DW_AT_count"
  | 0x38 -> "DW_AT_data_member_location"
  | 0x39 -> "DW_AT_decl_column"
  | 0x3a -> "DW_AT_decl_file"
  | 0x3b -> "DW_AT_decl_line"
  | 0x3c -> "DW_AT_declaration"
  | 0x3d -> "DW_AT_discr_list"
  | 0x3e -> "DW_AT_encoding"
  | 0x3f -> "DW_AT_external"
  | 0x40 -> "DW_AT_frame_base"
  | 0x41 -> "DW_AT_friend"
  | 0x42 -> "DW_AT_identifier_case"
  | 0x43 -> "DW_AT_macro_info"
  | 0x44 -> "DW_AT_namelist_item"
  | 0x45 -> "DW_AT_priority"
  | 0x46 -> "DW_AT_segment"
  | 0x47 -> "DW_AT_specification"
  | 0x48 -> "DW_AT_static_link"
  | 0x49 -> "DW_AT_type"
  | 0x4a -> "DW_AT_use_location"
  | 0x4b -> "DW_AT_variable_parameter"
  | 0x4c -> "DW_AT_virtuality"
  | 0x4d -> "DW_AT_vtable_elem_location"
  | 0x4e -> "DW_AT_allocated"
  | 0x4f -> "DW_AT_associated"
  | 0x50 -> "DW_AT_data_location"
  | 0x51 -> "DW_AT_byte_stride"
  | 0x52 -> "DW_AT_entry_pc"
  | 0x53 -> "DW_AT_use_UTF8"
  | 0x54 -> "DW_AT_extension"
  | 0x55 -> "DW_AT_ranges"
  | 0x56 -> "DW_AT_trampoline"
  | 0x57 -> "DW_AT_call_column"
  | 0x58 -> "DW_AT_call_file"
  | 0x59 -> "DW_AT_call_line"
  | 0x5a -> "DW_AT_description"
  | 0x5b -> "DW_AT_binary_scale"
  | 0x5c -> "DW_AT_decimal_scale"
  | 0x5d -> "DW_AT_small"
  | 0x5e -> "DW_AT_decimal_sign"
  | 0x5f -> "DW_AT_digit_count"
  | 0x60 -> "DW_AT_picture_string"
  | 0x61 -> "DW_AT_mutable"
  | 0x62 -> "DW_AT_threads_scaled"
  | 0x63 -> "DW_AT_explicit"
  | 0x64 -> "DW_AT_object_pointer"
  | 0x65 -> "DW_AT_endianity"
  | 0x66 -> "DW_AT_elemental"
  | 0x67 -> "DW_AT_pure"
  | 0x68 -> "DW_AT_recursive"
  | 0x69 -> "DW_AT_signature"
  | 0x6a -> "DW_AT_main_subprogram"
  | 0x6b -> "DW_AT_data_bit_offset"
  | 0x6c -> "DW_AT_const_expr"
  | 0x6d -> "DW_AT_enum_class"
  | 0x6e -> "DW_AT_linkage_name"
  | 0x6f -> "DW_AT_string_length_bit_size"
  | 0x70 -> "DW_AT_string_length_byte_size"
  | 0x71 -> "DW_AT_rank"
  | 0x72 -> "DW_AT_str_offsets_base"
  | 0x73 -> "DW_AT_addr_base"
  | 0x74 -> "DW_AT_rnglists_base"
  | 0x75 -> "DW_AT_reserved_75"
  | 0x76 -> "DW_AT_dwo_name"
  | 0x77 -> "DW_AT_reference"
  | 0x78 -> "DW_AT_rvalue_reference"
  | 0x79 -> "DW_AT_macros"
  | 0x7a -> "DW_AT_call_all_calls"
  | 0x7b -> "DW_AT_call_all_source_calls"
  | 0x7c -> "DW_AT_call_all_tail_calls"
  | 0x7d -> "DW_AT_call_return_pc"
  | 0x7e -> "DW_AT_call_value"
  | 0x7f -> "DW_AT_call_origin"
  | 0x80 -> "DW_AT_call_parameter"
  | 0x81 -> "DW_AT_call_pc"
  | 0x82 -> "DW_AT_call_tail_call"
  | 0x83 -> "DW_AT_call_target"
  | 0x84 -> "DW_AT_call_target_clobbered"
  | 0x85 -> "DW_AT_call_data_location"
  | 0x86 -> "DW_AT_call_data_value"
  | 0x87 -> "DW_AT_noreturn"
  | 0x88 -> "DW_AT_alignment"
  | 0x89 -> "DW_AT_export_symbols"
  | 0x8a -> "DW_AT_deleted"
  | 0x8b -> "DW_AT_defaulted"
  | 0x8c -> "DW_AT_loclists_base"
  | 0x3e02 -> "DW_AT_LLVM_sysroot"
  | 0x3fe7 -> "DW_AT_APPLE_omit_frame_ptr"
  | 0x3fef -> "DW_AT_APPLE_sdk"
  | code -> Printf.sprintf "DW_AT_<0x%04x>" code

let attribute_encoding x =
  match Unsigned.UInt64.to_int x with
  | 0x01 -> DW_AT_sibling
  | 0x02 -> DW_AT_location
  | 0x03 -> DW_AT_name
  | 0x09 -> DW_AT_ordering
  | 0x0b -> DW_AT_byte_size
  | 0x0d -> DW_AT_bit_size
  | 0x10 -> DW_AT_stmt_list
  | 0x11 -> DW_AT_low_pc
  | 0x12 -> DW_AT_high_pc
  | 0x13 -> DW_AT_language
  | 0x15 -> DW_AT_discr
  | 0x16 -> DW_AT_discr_value
  | 0x17 -> DW_AT_visibility
  | 0x18 -> DW_AT_import
  | 0x19 -> DW_AT_string_length
  | 0x1a -> DW_AT_common_reference
  | 0x1b -> DW_AT_comp_dir
  | 0x1c -> DW_AT_const_value
  | 0x1d -> DW_AT_containing_type
  | 0x1e -> DW_AT_default_value
  | 0x20 -> DW_AT_inline
  | 0x21 -> DW_AT_is_optional
  | 0x22 -> DW_AT_lower_bound
  | 0x25 -> DW_AT_producer
  | 0x27 -> DW_AT_prototyped
  | 0x2a -> DW_AT_return_addr
  | 0x2c -> DW_AT_start_scope
  | 0x2e -> DW_AT_bit_stride
  | 0x2f -> DW_AT_upper_bound
  | 0x31 -> DW_AT_abstract_origin
  | 0x32 -> DW_AT_accessibility
  | 0x33 -> DW_AT_address_class
  | 0x34 -> DW_AT_artificial
  | 0x35 -> DW_AT_base_types
  | 0x36 -> DW_AT_calling_convention
  | 0x37 -> DW_AT_count
  | 0x38 -> DW_AT_data_member_location
  | 0x39 -> DW_AT_decl_column
  | 0x3a -> DW_AT_decl_file
  | 0x3b -> DW_AT_decl_line
  | 0x3c -> DW_AT_declaration
  | 0x3d -> DW_AT_discr_list
  | 0x3e -> DW_AT_encoding
  | 0x3f -> DW_AT_external
  | 0x40 -> DW_AT_frame_base
  | 0x41 -> DW_AT_friend
  | 0x42 -> DW_AT_identifier_case
  | 0x44 -> DW_AT_namelist_item
  | 0x45 -> DW_AT_priority
  | 0x46 -> DW_AT_segment
  | 0x47 -> DW_AT_specification
  | 0x48 -> DW_AT_static_link
  | 0x49 -> DW_AT_type
  | 0x4a -> DW_AT_use_location
  | 0x4b -> DW_AT_variable_parameter
  | 0x4c -> DW_AT_virtuality
  | 0x4d -> DW_AT_vtable_elem_location
  | 0x4e -> DW_AT_allocated
  | 0x4f -> DW_AT_associated
  | 0x50 -> DW_AT_data_location
  | 0x51 -> DW_AT_byte_stride
  | 0x52 -> DW_AT_entry_pc
  | 0x53 -> DW_AT_use_UTF8
  | 0x54 -> DW_AT_extension
  | 0x55 -> DW_AT_ranges
  | 0x56 -> DW_AT_trampoline
  | 0x57 -> DW_AT_call_column
  | 0x58 -> DW_AT_call_file
  | 0x59 -> DW_AT_call_line
  | 0x5a -> DW_AT_description
  | 0x5b -> DW_AT_binary_scale
  | 0x5c -> DW_AT_decimal_scale
  | 0x5d -> DW_AT_small
  | 0x5e -> DW_AT_decimal_sign
  | 0x5f -> DW_AT_digit_count
  | 0x60 -> DW_AT_picture_string
  | 0x61 -> DW_AT_mutable
  | 0x62 -> DW_AT_threads_scaled
  | 0x63 -> DW_AT_explicit
  | 0x64 -> DW_AT_object_pointer
  | 0x65 -> DW_AT_endianity
  | 0x66 -> DW_AT_elemental
  | 0x67 -> DW_AT_pure
  | 0x68 -> DW_AT_recursive
  | 0x69 -> DW_AT_signature
  | 0x6a -> DW_AT_main_subprogram
  | 0x6b -> DW_AT_data_bit_offset
  | 0x6c -> DW_AT_const_expr
  | 0x6d -> DW_AT_enum_class
  | 0x6e -> DW_AT_linkage_name
  | 0x6f -> DW_AT_string_length_bit_size
  | 0x70 -> DW_AT_string_length_byte_size
  | 0x71 -> DW_AT_rank
  | 0x72 -> DW_AT_str_offsets_base
  | 0x73 -> DW_AT_addr_base
  | 0x74 -> DW_AT_rnglists_base
  | 0x76 -> DW_AT_dwo_name
  | 0x77 -> DW_AT_reference
  | 0x78 -> DW_AT_rvalue_reference
  | 0x79 -> DW_AT_macros
  | 0x7a -> DW_AT_call_all_calls
  | 0x7b -> DW_AT_call_all_source_calls
  | 0x7c -> DW_AT_call_all_tail_calls
  | 0x7d -> DW_AT_call_return_pc
  | 0x7e -> DW_AT_call_value
  | 0x7f -> DW_AT_call_origin
  | 0x80 -> DW_AT_call_parameter
  | 0x81 -> DW_AT_call_pc
  | 0x82 -> DW_AT_call_tail_call
  | 0x83 -> DW_AT_call_target
  | 0x84 -> DW_AT_call_target_clobbered
  | 0x85 -> DW_AT_call_data_location
  | 0x86 -> DW_AT_call_data_value
  | 0x87 -> DW_AT_noreturn
  | 0x88 -> DW_AT_alignment
  | 0x89 -> DW_AT_export_symbols
  | 0x8a -> DW_AT_deleted
  | 0x8b -> DW_AT_defaulted
  | 0x8c -> DW_AT_loclists_base
  | 0x2000 -> DW_AT_lo_user
  | 0x3fff -> DW_AT_hi_user
  (* LLVM and Apple extensions *)
  | 0x3e02 -> DW_AT_LLVM_sysroot
  | 0x3fe7 -> DW_AT_APPLE_omit_frame_ptr
  | 0x3fef -> DW_AT_APPLE_sdk
  | n -> failwith (Printf.sprintf "Unknown attribute_encoding: 0x%04x" n)

let string_of_attribute_encoding = function
  | DW_AT_sibling -> "DW_AT_sibling"
  | DW_AT_location -> "DW_AT_location"
  | DW_AT_name -> "DW_AT_name"
  | DW_AT_ordering -> "DW_AT_ordering"
  | DW_AT_byte_size -> "DW_AT_byte_size"
  | DW_AT_bit_size -> "DW_AT_bit_size"
  | DW_AT_stmt_list -> "DW_AT_stmt_list"
  | DW_AT_low_pc -> "DW_AT_low_pc"
  | DW_AT_high_pc -> "DW_AT_high_pc"
  | DW_AT_language -> "DW_AT_language"
  | DW_AT_discr -> "DW_AT_discr"
  | DW_AT_discr_value -> "DW_AT_discr_value"
  | DW_AT_visibility -> "DW_AT_visibility"
  | DW_AT_import -> "DW_AT_import"
  | DW_AT_string_length -> "DW_AT_string_length"
  | DW_AT_common_reference -> "DW_AT_common_reference"
  | DW_AT_comp_dir -> "DW_AT_comp_dir"
  | DW_AT_const_value -> "DW_AT_const_value"
  | DW_AT_containing_type -> "DW_AT_containing_type"
  | DW_AT_default_value -> "DW_AT_default_value"
  | DW_AT_inline -> "DW_AT_inline"
  | DW_AT_is_optional -> "DW_AT_is_optional"
  | DW_AT_lower_bound -> "DW_AT_lower_bound"
  | DW_AT_producer -> "DW_AT_producer"
  | DW_AT_prototyped -> "DW_AT_prototyped"
  | DW_AT_return_addr -> "DW_AT_return_addr"
  | DW_AT_start_scope -> "DW_AT_start_scope"
  | DW_AT_bit_stride -> "DW_AT_bit_stride"
  | DW_AT_upper_bound -> "DW_AT_upper_bound"
  | DW_AT_abstract_origin -> "DW_AT_abstract_origin"
  | DW_AT_accessibility -> "DW_AT_accessibility"
  | DW_AT_address_class -> "DW_AT_address_class"
  | DW_AT_artificial -> "DW_AT_artificial"
  | DW_AT_base_types -> "DW_AT_base_types"
  | DW_AT_calling_convention -> "DW_AT_calling_convention"
  | DW_AT_count -> "DW_AT_count"
  | DW_AT_data_member_location -> "DW_AT_data_member_location"
  | DW_AT_decl_column -> "DW_AT_decl_column"
  | DW_AT_decl_file -> "DW_AT_decl_file"
  | DW_AT_decl_line -> "DW_AT_decl_line"
  | DW_AT_declaration -> "DW_AT_declaration"
  | DW_AT_discr_list -> "DW_AT_discr_list"
  | DW_AT_encoding -> "DW_AT_encoding"
  | DW_AT_external -> "DW_AT_external"
  | DW_AT_frame_base -> "DW_AT_frame_base"
  | DW_AT_friend -> "DW_AT_friend"
  | DW_AT_identifier_case -> "DW_AT_identifier_case"
  | DW_AT_namelist_item -> "DW_AT_namelist_item"
  | DW_AT_priority -> "DW_AT_priority"
  | DW_AT_segment -> "DW_AT_segment"
  | DW_AT_specification -> "DW_AT_specification"
  | DW_AT_static_link -> "DW_AT_static_link"
  | DW_AT_type -> "DW_AT_type"
  | DW_AT_use_location -> "DW_AT_use_location"
  | DW_AT_variable_parameter -> "DW_AT_variable_parameter"
  | DW_AT_virtuality -> "DW_AT_virtuality"
  | DW_AT_vtable_elem_location -> "DW_AT_vtable_elem_location"
  | DW_AT_allocated -> "DW_AT_allocated"
  | DW_AT_associated -> "DW_AT_associated"
  | DW_AT_data_location -> "DW_AT_data_location"
  | DW_AT_byte_stride -> "DW_AT_byte_stride"
  | DW_AT_entry_pc -> "DW_AT_entry_pc"
  | DW_AT_use_UTF8 -> "DW_AT_use_UTF8"
  | DW_AT_extension -> "DW_AT_extension"
  | DW_AT_ranges -> "DW_AT_ranges"
  | DW_AT_trampoline -> "DW_AT_trampoline"
  | DW_AT_call_column -> "DW_AT_call_column"
  | DW_AT_call_file -> "DW_AT_call_file"
  | DW_AT_call_line -> "DW_AT_call_line"
  | DW_AT_description -> "DW_AT_description"
  | DW_AT_binary_scale -> "DW_AT_binary_scale"
  | DW_AT_decimal_scale -> "DW_AT_decimal_scale"
  | DW_AT_small -> "DW_AT_small"
  | DW_AT_decimal_sign -> "DW_AT_decimal_sign"
  | DW_AT_digit_count -> "DW_AT_digit_count"
  | DW_AT_picture_string -> "DW_AT_picture_string"
  | DW_AT_mutable -> "DW_AT_mutable"
  | DW_AT_threads_scaled -> "DW_AT_threads_scaled"
  | DW_AT_explicit -> "DW_AT_explicit"
  | DW_AT_object_pointer -> "DW_AT_object_pointer"
  | DW_AT_endianity -> "DW_AT_endianity"
  | DW_AT_elemental -> "DW_AT_elemental"
  | DW_AT_pure -> "DW_AT_pure"
  | DW_AT_recursive -> "DW_AT_recursive"
  | DW_AT_signature -> "DW_AT_signature"
  | DW_AT_main_subprogram -> "DW_AT_main_subprogram"
  | DW_AT_data_bit_offset -> "DW_AT_data_bit_offset"
  | DW_AT_const_expr -> "DW_AT_const_expr"
  | DW_AT_enum_class -> "DW_AT_enum_class"
  | DW_AT_linkage_name -> "DW_AT_linkage_name"
  | DW_AT_string_length_bit_size -> "DW_AT_string_length_bit_size"
  | DW_AT_string_length_byte_size -> "DW_AT_string_length_byte_size"
  | DW_AT_rank -> "DW_AT_rank"
  | DW_AT_str_offsets_base -> "DW_AT_str_offsets_base"
  | DW_AT_addr_base -> "DW_AT_addr_base"
  | DW_AT_rnglists_base -> "DW_AT_rnglists_base"
  | DW_AT_dwo_name -> "DW_AT_dwo_name"
  | DW_AT_reference -> "DW_AT_reference"
  | DW_AT_rvalue_reference -> "DW_AT_rvalue_reference"
  | DW_AT_macros -> "DW_AT_macros"
  | DW_AT_call_all_calls -> "DW_AT_call_all_calls"
  | DW_AT_call_all_source_calls -> "DW_AT_call_all_source_calls"
  | DW_AT_call_all_tail_calls -> "DW_AT_call_all_tail_calls"
  | DW_AT_call_return_pc -> "DW_AT_call_return_pc"
  | DW_AT_call_value -> "DW_AT_call_value"
  | DW_AT_call_origin -> "DW_AT_call_origin"
  | DW_AT_call_parameter -> "DW_AT_call_parameter"
  | DW_AT_call_pc -> "DW_AT_call_pc"
  | DW_AT_call_tail_call -> "DW_AT_call_tail_call"
  | DW_AT_call_target -> "DW_AT_call_target"
  | DW_AT_call_target_clobbered -> "DW_AT_call_target_clobbered"
  | DW_AT_call_data_location -> "DW_AT_call_data_location"
  | DW_AT_call_data_value -> "DW_AT_call_data_value"
  | DW_AT_noreturn -> "DW_AT_noreturn"
  | DW_AT_alignment -> "DW_AT_alignment"
  | DW_AT_export_symbols -> "DW_AT_export_symbols"
  | DW_AT_deleted -> "DW_AT_deleted"
  | DW_AT_defaulted -> "DW_AT_defaulted"
  | DW_AT_loclists_base -> "DW_AT_loclists_base"
  | DW_AT_lo_user -> "DW_AT_lo_user"
  | DW_AT_hi_user -> "DW_AT_hi_user"
  (* LLVM and Apple extensions *)
  | DW_AT_LLVM_sysroot -> "DW_AT_LLVM_sysroot"
  | DW_AT_APPLE_omit_frame_ptr -> "DW_AT_APPLE_omit_frame_ptr"
  | DW_AT_APPLE_sdk -> "DW_AT_APPLE_sdk"

type attribute_form_encoding =
  | DW_FORM_addr
  | DW_FORM_block2
  | DW_FORM_block4
  | DW_FORM_data2
  | DW_FORM_data4
  | DW_FORM_data8
  | DW_FORM_string
  | DW_FORM_block
  | DW_FORM_block1
  | DW_FORM_data1
  | DW_FORM_flag
  | DW_FORM_sdata
  | DW_FORM_strp
  | DW_FORM_udata
  | DW_FORM_ref_addr
  | DW_FORM_ref1
  | DW_FORM_ref2
  | DW_FORM_ref4
  | DW_FORM_ref8
  | DW_FORM_ref_udata
  | DW_FORM_indirect
  | DW_FORM_sec_offset
  | DW_FORM_exprloc
  | DW_FORM_flag_present  (** New in DWARF Version 5 *)
  | DW_FORM_strx
  | DW_FORM_addrx
  | DW_FORM_ref_sup4
  | DW_FORM_strp_sup
  | DW_FORM_data16
  | DW_FORM_line_strp
  | DW_FORM_ref_sig8
  | DW_FORM_implicit_const
  | DW_FORM_loclistx
  | DW_FORM_rnglistx
  | DW_FORM_ref_sup8
  | DW_FORM_strx1
  | DW_FORM_strx2
  | DW_FORM_strx3
  | DW_FORM_strx4
  | DW_FORM_addrx1
  | DW_FORM_addrx2
  | DW_FORM_addrx3
  | DW_FORM_addrx4

let attribute_form_encoding x =
  match Unsigned.UInt64.to_int x with
  | 0x01 -> DW_FORM_addr
  | 0x03 -> DW_FORM_block2
  | 0x04 -> DW_FORM_block4
  | 0x05 -> DW_FORM_data2
  | 0x06 -> DW_FORM_data4
  | 0x07 -> DW_FORM_data8
  | 0x08 -> DW_FORM_string
  | 0x09 -> DW_FORM_block
  | 0x0a -> DW_FORM_block1
  | 0x0b -> DW_FORM_data1
  | 0x0c -> DW_FORM_flag
  | 0x0d -> DW_FORM_sdata
  | 0x0e -> DW_FORM_strp
  | 0x0f -> DW_FORM_udata
  | 0x10 -> DW_FORM_ref_addr
  | 0x11 -> DW_FORM_ref1
  | 0x12 -> DW_FORM_ref2
  | 0x13 -> DW_FORM_ref4
  | 0x14 -> DW_FORM_ref8
  | 0x15 -> DW_FORM_ref_udata
  | 0x16 -> DW_FORM_indirect
  | 0x17 -> DW_FORM_sec_offset
  | 0x18 -> DW_FORM_exprloc
  | 0x19 -> DW_FORM_flag_present
  | 0x1a -> DW_FORM_strx
  | 0x1b -> DW_FORM_addrx
  | 0x1c -> DW_FORM_ref_sup4
  | 0x1d -> DW_FORM_strp_sup
  | 0x1e -> DW_FORM_data16
  | 0x1f -> DW_FORM_line_strp
  | 0x20 -> DW_FORM_ref_sig8
  | 0x21 -> DW_FORM_implicit_const
  | 0x22 -> DW_FORM_loclistx
  | 0x23 -> DW_FORM_rnglistx
  | 0x24 -> DW_FORM_ref_sup8
  | 0x25 -> DW_FORM_strx1
  | 0x26 -> DW_FORM_strx2
  | 0x27 -> DW_FORM_strx3
  | 0x28 -> DW_FORM_strx4
  | 0x29 -> DW_FORM_addrx1
  | 0x2a -> DW_FORM_addrx2
  | 0x2b -> DW_FORM_addrx3
  | 0x2c -> DW_FORM_addrx4
  | n -> failwith (Printf.sprintf "Unknown attribute_form_encoding : 0x%02x" n)

let string_of_attribute_form_encoding form_code =
  match Unsigned.UInt64.to_int form_code with
  | 0x01 -> "DW_FORM_addr"
  | 0x02 -> "DW_FORM_reserved_02"
  | 0x03 -> "DW_FORM_block2"
  | 0x04 -> "DW_FORM_block4"
  | 0x05 -> "DW_FORM_data2"
  | 0x06 -> "DW_FORM_data4"
  | 0x07 -> "DW_FORM_data8"
  | 0x08 -> "DW_FORM_string"
  | 0x09 -> "DW_FORM_block"
  | 0x0a -> "DW_FORM_block1"
  | 0x0b -> "DW_FORM_data1"
  | 0x0c -> "DW_FORM_flag"
  | 0x0d -> "DW_FORM_sdata"
  | 0x0e -> "DW_FORM_strp"
  | 0x0f -> "DW_FORM_udata"
  | 0x10 -> "DW_FORM_ref_addr"
  | 0x11 -> "DW_FORM_ref1"
  | 0x12 -> "DW_FORM_ref2"
  | 0x13 -> "DW_FORM_ref4"
  | 0x14 -> "DW_FORM_ref8"
  | 0x15 -> "DW_FORM_ref_udata"
  | 0x16 -> "DW_FORM_indirect"
  | 0x17 -> "DW_FORM_sec_offset"
  | 0x18 -> "DW_FORM_exprloc"
  | 0x19 -> "DW_FORM_flag_present"
  | 0x1a -> "DW_FORM_strx"
  | 0x1b -> "DW_FORM_addrx"
  | 0x1c -> "DW_FORM_ref_sup4"
  | 0x1d -> "DW_FORM_strp_sup"
  | 0x1e -> "DW_FORM_data16"
  | 0x1f -> "DW_FORM_line_strp"
  | 0x20 -> "DW_FORM_ref_sig8"
  | 0x21 -> "DW_FORM_implicit_const"
  | 0x22 -> "DW_FORM_loclistx"
  | 0x23 -> "DW_FORM_rnglistx"
  | 0x24 -> "DW_FORM_ref_sup8"
  | 0x25 -> "DW_FORM_strx1"
  | 0x26 -> "DW_FORM_strx2"
  | 0x27 -> "DW_FORM_strx3"
  | 0x28 -> "DW_FORM_strx4"
  | 0x29 -> "DW_FORM_addrx1"
  | 0x2a -> "DW_FORM_addrx2"
  | 0x2b -> "DW_FORM_addrx3"
  | 0x2c -> "DW_FORM_addrx4"
  | code -> Printf.sprintf "DW_FORM_<0x%02x>" code

type operation_encoding =
  | DW_OP_addr (* constant address *)
  | DW_OP_deref
  | DW_OP_const1u (* 1-byte unsigned constant *)
  | DW_OP_const1s (* 1-byte signed constant *)
  | DW_OP_const2u (* 2-byte unsigned constant *)
  | DW_OP_const2s (* 2-byte signed constant *)
  | DW_OP_const4u (* 4-byte unsigned constant *)
  | DW_OP_const4s (* 4-byte signed constant *)
  | DW_OP_const8u (* 8-byte unsigned constant *)
  | DW_OP_const8s (* 8-byte signed constant *)
  | DW_OP_constu (* ULEB128 unsigned constant *)
  | DW_OP_consts (* SLEB128 signed constant *)
  | DW_OP_dup
  | DW_OP_drop
  | DW_OP_over
  | DW_OP_pick (* 1-byte stack index *)
  | DW_OP_swap
  | DW_OP_rot
  | DW_OP_xderef
  | DW_OP_abs
  | DW_OP_and
  | DW_OP_div
  | DW_OP_minus
  | DW_OP_mod
  | DW_OP_mul
  | DW_OP_neg
  | DW_OP_not
  | DW_OP_or
  | DW_OP_plus
  | DW_OP_plus_uconst
  | DW_OP_shl
  | DW_OP_shr
  | DW_OP_shra
  | DW_OP_xor
  | DW_OP_bra (* signed 2-byte constant *)
  | DW_OP_eq
  | DW_OP_ge
  | DW_OP_gt
  | DW_OP_le
  | DW_OP_lt
  | DW_OP_ne
  | DW_OP_skip (* signed 2-byte constant *)
  | DW_OP_lit0
  | DW_OP_lit1
  | DW_OP_lit2
  | DW_OP_lit3
  | DW_OP_lit4
  | DW_OP_lit5
  | DW_OP_lit6
  | DW_OP_lit7
  | DW_OP_lit8
  | DW_OP_lit9
  | DW_OP_lit10
  | DW_OP_lit11
  | DW_OP_lit12
  | DW_OP_lit13
  | DW_OP_lit14
  | DW_OP_lit15
  | DW_OP_lit16
  | DW_OP_lit17
  | DW_OP_lit18
  | DW_OP_lit19
  | DW_OP_lit20
  | DW_OP_lit21
  | DW_OP_lit22
  | DW_OP_lit23
  | DW_OP_lit24
  | DW_OP_lit25
  | DW_OP_lit26
  | DW_OP_lit27
  | DW_OP_lit28
  | DW_OP_lit29
  | DW_OP_lit30
  | DW_OP_lit31
  | DW_OP_reg0
  | DW_OP_reg1
  | DW_OP_reg2
  | DW_OP_reg3
  | DW_OP_reg4
  | DW_OP_reg5
  | DW_OP_reg6
  | DW_OP_reg7
  | DW_OP_reg8
  | DW_OP_reg9
  | DW_OP_reg10
  | DW_OP_reg11
  | DW_OP_reg12
  | DW_OP_reg13
  | DW_OP_reg14
  | DW_OP_reg15
  | DW_OP_reg16
  | DW_OP_reg17
  | DW_OP_reg18
  | DW_OP_reg19
  | DW_OP_reg20
  | DW_OP_reg21
  | DW_OP_reg22
  | DW_OP_reg23
  | DW_OP_reg24
  | DW_OP_reg25
  | DW_OP_reg26
  | DW_OP_reg27
  | DW_OP_reg28
  | DW_OP_reg29
  | DW_OP_reg30
  | DW_OP_reg31
  | DW_OP_breg0 (* SLEB128 offset *)
  | DW_OP_breg1
  | DW_OP_breg2
  | DW_OP_breg3
  | DW_OP_breg4
  | DW_OP_breg5
  | DW_OP_breg6
  | DW_OP_breg7
  | DW_OP_breg8
  | DW_OP_breg9
  | DW_OP_breg10
  | DW_OP_breg11
  | DW_OP_breg12
  | DW_OP_breg13
  | DW_OP_breg14
  | DW_OP_breg15
  | DW_OP_breg16
  | DW_OP_breg17
  | DW_OP_breg18
  | DW_OP_breg19
  | DW_OP_breg20
  | DW_OP_breg21
  | DW_OP_breg22
  | DW_OP_breg23
  | DW_OP_breg24
  | DW_OP_breg25
  | DW_OP_breg26
  | DW_OP_breg27
  | DW_OP_breg28
  | DW_OP_breg29
  | DW_OP_breg30
  | DW_OP_breg31
  | DW_OP_regx (* ULEB128 register *)
  | DW_OP_fbreg (* SLEB128 offset *)
  | DW_OP_bregx (* ULEB128 register, SLEB128 offset *)
  | DW_OP_piece (* ULEB128 size of piece *)
  | DW_OP_deref_size (* 1-byte size of data retrieved *)
  | DW_OP_xderef_size (* 1-byte size of data retrieved *)
  | DW_OP_nop
  | DW_OP_push_object_address
  | DW_OP_call2 (* 2-byte offset of DIE *)
  | DW_OP_call4 (* 4-byte offset of DIE *)
  | DW_OP_call_ref (* 4- or 8-byte offset of DIE *)
  | DW_OP_form_tls_address
  | DW_OP_call_frame_cfa
  | DW_OP_bit_piece (* ULEB128 size, ULEB128 offset *)
  | DW_OP_implicit_value (* ULEB128 size, block of that size *)
  | DW_OP_stack_value  (** New in DWARF Version 5 *)
  | DW_OP_implicit_pointer
    (* 4- or 8-byte offset of DIE, SLEB128 constant offset *)
  | DW_OP_addrx (* ULEB128 indirect address *)
  | DW_OP_constx (* ULEB128 indirect constant *)
  | DW_OP_entry_value (* ULEB128 size, block of that size *)
  | DW_OP_const_type
    (* ULEB128 type entry offset, 1-byte size, constant value *)
  | DW_OP_regval_type (* ULEB128 register number, ULEB128 constant offset *)
  | DW_OP_deref_type (* 1-byte size, ULEB128 type entry offset *)
  | DW_OP_xderef_type (* 1-byte size, ULEB128 type entry offset *)
  | DW_OP_convert (* ULEB128 type entry offset *)
  | DW_OP_reinterpret (* DW_OP_lo_user 0xe0 ULEB128 type entry offset *)
  | DW_OP_hi_user

let operation_encoding = function
  | 0x03 -> DW_OP_addr
  | 0x06 -> DW_OP_deref
  | 0x08 -> DW_OP_const1u
  | 0x09 -> DW_OP_const1s
  | 0x0a -> DW_OP_const2u
  | 0x0b -> DW_OP_const2s
  | 0x0c -> DW_OP_const4u
  | 0x0d -> DW_OP_const4s
  | 0x0e -> DW_OP_const8u
  | 0x0f -> DW_OP_const8s
  | 0x10 -> DW_OP_constu
  | 0x11 -> DW_OP_consts
  | 0x12 -> DW_OP_dup
  | 0x13 -> DW_OP_drop
  | 0x14 -> DW_OP_over
  | 0x15 -> DW_OP_pick
  | 0x16 -> DW_OP_swap
  | 0x17 -> DW_OP_rot
  | 0x18 -> DW_OP_xderef
  | 0x19 -> DW_OP_abs
  | 0x1a -> DW_OP_and
  | 0x1b -> DW_OP_div
  | 0x1c -> DW_OP_minus
  | 0x1d -> DW_OP_mod
  | 0x1e -> DW_OP_mul
  | 0x1f -> DW_OP_neg
  | 0x20 -> DW_OP_not
  | 0x21 -> DW_OP_or
  | 0x22 -> DW_OP_plus
  | 0x23 -> DW_OP_plus_uconst
  | 0x24 -> DW_OP_shl
  | 0x25 -> DW_OP_shr
  | 0x26 -> DW_OP_shra
  | 0x27 -> DW_OP_xor
  | 0x28 -> DW_OP_bra
  | 0x29 -> DW_OP_eq
  | 0x2a -> DW_OP_ge
  | 0x2b -> DW_OP_gt
  | 0x2c -> DW_OP_le
  | 0x2d -> DW_OP_lt
  | 0x2e -> DW_OP_ne
  | 0x2f -> DW_OP_skip
  | 0x30 -> DW_OP_lit0
  | 0x31 -> DW_OP_lit1
  | 0x32 -> DW_OP_lit2
  | 0x33 -> DW_OP_lit3
  | 0x34 -> DW_OP_lit4
  | 0x35 -> DW_OP_lit5
  | 0x36 -> DW_OP_lit6
  | 0x37 -> DW_OP_lit7
  | 0x38 -> DW_OP_lit8
  | 0x39 -> DW_OP_lit9
  | 0x3a -> DW_OP_lit10
  | 0x3b -> DW_OP_lit11
  | 0x3c -> DW_OP_lit12
  | 0x3d -> DW_OP_lit13
  | 0x3e -> DW_OP_lit14
  | 0x3f -> DW_OP_lit15
  | 0x40 -> DW_OP_lit16
  | 0x41 -> DW_OP_lit17
  | 0x42 -> DW_OP_lit18
  | 0x43 -> DW_OP_lit19
  | 0x44 -> DW_OP_lit20
  | 0x45 -> DW_OP_lit21
  | 0x46 -> DW_OP_lit22
  | 0x47 -> DW_OP_lit23
  | 0x48 -> DW_OP_lit24
  | 0x49 -> DW_OP_lit25
  | 0x4a -> DW_OP_lit26
  | 0x4b -> DW_OP_lit27
  | 0x4c -> DW_OP_lit28
  | 0x4d -> DW_OP_lit29
  | 0x4e -> DW_OP_lit30
  | 0x4f -> DW_OP_lit31
  | 0x50 -> DW_OP_reg0
  | 0x51 -> DW_OP_reg1
  | 0x52 -> DW_OP_reg2
  | 0x53 -> DW_OP_reg3
  | 0x54 -> DW_OP_reg4
  | 0x55 -> DW_OP_reg5
  | 0x56 -> DW_OP_reg6
  | 0x57 -> DW_OP_reg7
  | 0x58 -> DW_OP_reg8
  | 0x59 -> DW_OP_reg9
  | 0x5a -> DW_OP_reg10
  | 0x5b -> DW_OP_reg11
  | 0x5c -> DW_OP_reg12
  | 0x5d -> DW_OP_reg13
  | 0x5e -> DW_OP_reg14
  | 0x5f -> DW_OP_reg15
  | 0x60 -> DW_OP_reg16
  | 0x61 -> DW_OP_reg17
  | 0x62 -> DW_OP_reg18
  | 0x63 -> DW_OP_reg19
  | 0x64 -> DW_OP_reg20
  | 0x65 -> DW_OP_reg21
  | 0x66 -> DW_OP_reg22
  | 0x67 -> DW_OP_reg23
  | 0x68 -> DW_OP_reg24
  | 0x69 -> DW_OP_reg25
  | 0x6a -> DW_OP_reg26
  | 0x6b -> DW_OP_reg27
  | 0x6c -> DW_OP_reg28
  | 0x6d -> DW_OP_reg29
  | 0x6e -> DW_OP_reg30
  | 0x6f -> DW_OP_reg31
  | 0x70 -> DW_OP_breg0
  | 0x71 -> DW_OP_breg1
  | 0x8f -> DW_OP_breg31
  | 0x90 -> DW_OP_regx
  | 0x91 -> DW_OP_fbreg
  | 0x92 -> DW_OP_bregx
  | 0x93 -> DW_OP_piece
  | 0x94 -> DW_OP_deref_size
  | 0x95 -> DW_OP_xderef_size
  | 0x96 -> DW_OP_nop
  | 0x97 -> DW_OP_push_object_address
  | 0x98 -> DW_OP_call2
  | 0x99 -> DW_OP_call4
  | 0x9a -> DW_OP_call_ref
  | 0x9b -> DW_OP_form_tls_address
  | 0x9c -> DW_OP_call_frame_cfa
  | 0x9d -> DW_OP_bit_piece
  | 0x9e -> DW_OP_implicit_value
  | 0x9f -> DW_OP_stack_value
  | 0xa0 -> DW_OP_implicit_pointer
  | 0xa1 -> DW_OP_addrx
  | 0xa2 -> DW_OP_constx
  | 0xa3 -> DW_OP_entry_value
  | 0xa4 -> DW_OP_const_type
  | 0xa5 -> DW_OP_regval_type
  | 0xa6 -> DW_OP_deref_type
  | 0xa7 -> DW_OP_xderef_type
  | 0xa8 -> DW_OP_convert
  | 0xa9 -> DW_OP_reinterpret
  | 0xff -> DW_OP_hi_user
  | n -> failwith (Printf.sprintf "Unknown operation_encoding: 0x%02x" n)

type location_list_entry =
  | DW_LLE_end_of_list
  | DW_LLE_base_addressx
  | DW_LLE_startx_endx
  | DW_LLE_startx_length
  | DW_LLE_offset_pair
  | DW_LLE_default_location
  | DW_LLE_base_address
  | DW_LLE_start_end
  | DW_LLE_start_length

let location_list_entry = function
  | 0x00 -> DW_LLE_end_of_list
  | 0x01 -> DW_LLE_base_addressx
  | 0x02 -> DW_LLE_startx_endx
  | 0x03 -> DW_LLE_startx_length
  | 0x04 -> DW_LLE_offset_pair
  | 0x05 -> DW_LLE_default_location
  | 0x06 -> DW_LLE_base_address
  | 0x07 -> DW_LLE_start_end
  | 0x08 -> DW_LLE_start_length
  | n -> failwith (Printf.sprintf "Unknown location_list_entry: 0x%02x" n)

type base_type =
  | DW_ATE_address
  | DW_ATE_boolean
  | DW_ATE_complex_float
  | DW_ATE_float
  | DW_ATE_signed
  | DW_ATE_signed_char
  | DW_ATE_unsigned
  | DW_ATE_unsigned_char
  | DW_ATE_imaginary_float
  | DW_ATE_packed_decimal
  | DW_ATE_numeric_string
  | DW_ATE_edited
  | DW_ATE_signed_fixed
  | DW_ATE_unsigned_fixed
  | DW_ATE_decimal_float
  | DW_ATE_UTF
  | DW_ATE_UCS
  | DW_ATE_ASCII
  | DW_ATE_lo_user
  | DW_ATE_hi_user

let base_type = function
  | 0x01 -> DW_ATE_address
  | 0x02 -> DW_ATE_boolean
  | 0x03 -> DW_ATE_complex_float
  | 0x04 -> DW_ATE_float
  | 0x05 -> DW_ATE_signed
  | 0x06 -> DW_ATE_signed_char
  | 0x07 -> DW_ATE_unsigned
  | 0x08 -> DW_ATE_unsigned_char
  | 0x09 -> DW_ATE_imaginary_float
  | 0x0a -> DW_ATE_packed_decimal
  | 0x0b -> DW_ATE_numeric_string
  | 0x0c -> DW_ATE_edited
  | 0x0d -> DW_ATE_signed_fixed
  | 0x0e -> DW_ATE_unsigned_fixed
  | 0x0f -> DW_ATE_decimal_float
  | 0x10 -> DW_ATE_UTF
  | 0x11 -> DW_ATE_UCS
  | 0x12 -> DW_ATE_ASCII
  | 0x80 -> DW_ATE_lo_user
  | 0xff -> DW_ATE_hi_user
  | n -> failwith (Printf.sprintf "Unknown base_type: 0x%02x" n)

let string_of_base_type = function
  | DW_ATE_address -> "DW_ATE_address"
  | DW_ATE_boolean -> "DW_ATE_boolean"
  | DW_ATE_complex_float -> "DW_ATE_complex_float"
  | DW_ATE_float -> "DW_ATE_float"
  | DW_ATE_signed -> "DW_ATE_signed"
  | DW_ATE_signed_char -> "DW_ATE_signed_char"
  | DW_ATE_unsigned -> "DW_ATE_unsigned"
  | DW_ATE_unsigned_char -> "DW_ATE_unsigned_char"
  | DW_ATE_imaginary_float -> "DW_ATE_imaginary_float"
  | DW_ATE_packed_decimal -> "DW_ATE_packed_decimal"
  | DW_ATE_numeric_string -> "DW_ATE_numeric_string"
  | DW_ATE_edited -> "DW_ATE_edited"
  | DW_ATE_signed_fixed -> "DW_ATE_signed_fixed"
  | DW_ATE_unsigned_fixed -> "DW_ATE_unsigned_fixed"
  | DW_ATE_decimal_float -> "DW_ATE_decimal_float"
  | DW_ATE_UTF -> "DW_ATE_UTF"
  | DW_ATE_UCS -> "DW_ATE_UCS"
  | DW_ATE_ASCII -> "DW_ATE_ASCII"
  | DW_ATE_lo_user -> "DW_ATE_lo_user"
  | DW_ATE_hi_user -> "DW_ATE_hi_user"

type decimal_sign =
  | DW_DS_unsigned
  | DW_DS_leading_overpunch
  | DW_DS_trailing_overpunch
  | DW_DS_leading_separate
  | DW_DS_trailing_separate

let decimal_sign = function
  | 0x01 -> DW_DS_unsigned
  | 0x02 -> DW_DS_leading_overpunch
  | 0x03 -> DW_DS_trailing_overpunch
  | 0x04 -> DW_DS_leading_separate
  | 0x05 -> DW_DS_trailing_separate
  | n -> failwith (Printf.sprintf "Unknown decimal_sign: 0x%02x" n)

type endianity =
  | DW_END_default
  | DW_END_big
  | DW_END_little
  | DW_END_lo_user
  | DW_END_hi_user

let endianity = function
  | 0x00 -> DW_END_default
  | 0x01 -> DW_END_big
  | 0x02 -> DW_END_little
  | 0x40 -> DW_END_lo_user
  | 0xff -> DW_END_hi_user
  | n -> failwith (Printf.sprintf "Unknown endianity: 0x%02x" n)

type accessibility =
  | DW_ACCESS_public
  | DW_ACCESS_protected
  | DW_ACCESS_private

let accessibility = function
  | 0x01 -> DW_ACCESS_public
  | 0x02 -> DW_ACCESS_protected
  | 0x03 -> DW_ACCESS_private
  | n -> failwith (Printf.sprintf "Unknown accessibility: 0x%02x" n)

type visibility = DW_VIS_local | DW_VIS_exported | DW_VIS_qualified

let visibility = function
  | 0x01 -> DW_VIS_local
  | 0x02 -> DW_VIS_exported
  | 0x03 -> DW_VIS_qualified
  | n -> failwith (Printf.sprintf "Unknown visibility: 0x%02x" n)

type virtuality =
  | DW_VIRTUALITY_none
  | DW_VIRTUALITY_virtual
  | DW_VIRTUALITY_pure_virtual

let virtuality = function
  | 0x00 -> DW_VIRTUALITY_none
  | 0x01 -> DW_VIRTUALITY_virtual
  | 0x02 -> DW_VIRTUALITY_pure_virtual
  | n -> failwith (Printf.sprintf "Unknown virtuality: 0x%02x" n)

type dwarf_language =
  | DW_LANG_C89
  | DW_LANG_C
  | DW_LANG_Ada83
  | DW_LANG_C_plus_plus
  | DW_LANG_Cobol74
  | DW_LANG_Cobol85
  | DW_LANG_Fortran77
  | DW_LANG_Fortran90
  | DW_LANG_Pascal83
  | DW_LANG_Modula2
  | DW_LANG_Java
  | DW_LANG_C99
  | DW_LANG_Ada95
  | DW_LANG_Fortran95
  | DW_LANG_PLI
  | DW_LANG_ObjC
  | DW_LANG_ObjC_plus_plus
  | DW_LANG_UPC
  | DW_LANG_D
  | DW_LANG_Python  (** New in DWARF Version 5 *)
  | DW_LANG_OpenCL
  | DW_LANG_Go
  | DW_LANG_Modula3
  | DW_LANG_Haskell
  | DW_LANG_C_plus_plus_03
  | DW_LANG_C_plus_plus_11
  | DW_LANG_OCaml
  | DW_LANG_Rust
  | DW_LANG_C11
  | DW_LANG_Swift
  | DW_LANG_Julia
  | DW_LANG_Dylan
  | DW_LANG_C_plus_plus_14
  | DW_LANG_Fortran03
  | DW_LANG_Fortran08
  | DW_LANG_RenderScript
  | DW_LANG_BLISS
  | DW_LANG_lo_user
  | DW_LANG_hi_user

let dwarf_language = function
  | 0x0001 -> DW_LANG_C89
  | 0x0002 -> DW_LANG_C
  | 0x0003 -> DW_LANG_Ada83
  | 0x0004 -> DW_LANG_C_plus_plus
  | 0x0005 -> DW_LANG_Cobol74
  | 0x0006 -> DW_LANG_Cobol85
  | 0x0007 -> DW_LANG_Fortran77
  | 0x0008 -> DW_LANG_Fortran90
  | 0x0009 -> DW_LANG_Pascal83
  | 0x000a -> DW_LANG_Modula2
  | 0x000b -> DW_LANG_Java
  | 0x000c -> DW_LANG_C99
  | 0x000d -> DW_LANG_Ada95
  | 0x000e -> DW_LANG_Fortran95
  | 0x000f -> DW_LANG_PLI
  | 0x0010 -> DW_LANG_ObjC
  | 0x0011 -> DW_LANG_ObjC_plus_plus
  | 0x0012 -> DW_LANG_UPC
  | 0x0013 -> DW_LANG_D
  | 0x0014 -> DW_LANG_Python
  | 0x0015 -> DW_LANG_OpenCL
  | 0x0016 -> DW_LANG_Go
  | 0x0017 -> DW_LANG_Modula3
  | 0x0018 -> DW_LANG_Haskell
  | 0x0019 -> DW_LANG_C_plus_plus_03
  | 0x001a -> DW_LANG_C_plus_plus_11
  | 0x001b -> DW_LANG_OCaml
  | 0x001c -> DW_LANG_Rust
  | 0x001d -> DW_LANG_C11
  | 0x001e -> DW_LANG_Swift
  | 0x001f -> DW_LANG_Julia
  | 0x0020 -> DW_LANG_Dylan
  | 0x0021 -> DW_LANG_C_plus_plus_14
  | 0x0022 -> DW_LANG_Fortran03
  | 0x0023 -> DW_LANG_Fortran08
  | 0x0024 -> DW_LANG_RenderScript
  | 0x0025 -> DW_LANG_BLISS
  | 0x8000 -> DW_LANG_lo_user
  | 0xffff -> DW_LANG_hi_user
  | n -> failwith (Printf.sprintf "Unknown dwarf_language: 0x%04x" n)

let string_of_dwarf_language = function
  | DW_LANG_C89 -> "DW_LANG_C89"
  | DW_LANG_C -> "DW_LANG_C"
  | DW_LANG_Ada83 -> "DW_LANG_Ada83"
  | DW_LANG_C_plus_plus -> "DW_LANG_C_plus_plus"
  | DW_LANG_Cobol74 -> "DW_LANG_Cobol74"
  | DW_LANG_Cobol85 -> "DW_LANG_Cobol85"
  | DW_LANG_Fortran77 -> "DW_LANG_Fortran77"
  | DW_LANG_Fortran90 -> "DW_LANG_Fortran90"
  | DW_LANG_Pascal83 -> "DW_LANG_Pascal83"
  | DW_LANG_Modula2 -> "DW_LANG_Modula2"
  | DW_LANG_Java -> "DW_LANG_Java"
  | DW_LANG_C99 -> "DW_LANG_C99"
  | DW_LANG_Ada95 -> "DW_LANG_Ada95"
  | DW_LANG_Fortran95 -> "DW_LANG_Fortran95"
  | DW_LANG_PLI -> "DW_LANG_PLI"
  | DW_LANG_ObjC -> "DW_LANG_ObjC"
  | DW_LANG_ObjC_plus_plus -> "DW_LANG_ObjC_plus_plus"
  | DW_LANG_UPC -> "DW_LANG_UPC"
  | DW_LANG_D -> "DW_LANG_D"
  | DW_LANG_Python -> "DW_LANG_Python"
  | DW_LANG_OpenCL -> "DW_LANG_OpenCL"
  | DW_LANG_Go -> "DW_LANG_Go"
  | DW_LANG_Modula3 -> "DW_LANG_Modula3"
  | DW_LANG_Haskell -> "DW_LANG_Haskell"
  | DW_LANG_C_plus_plus_03 -> "DW_LANG_C_plus_plus_03"
  | DW_LANG_C_plus_plus_11 -> "DW_LANG_C_plus_plus_11"
  | DW_LANG_OCaml -> "DW_LANG_OCaml"
  | DW_LANG_Rust -> "DW_LANG_Rust"
  | DW_LANG_C11 -> "DW_LANG_C11"
  | DW_LANG_Swift -> "DW_LANG_Swift"
  | DW_LANG_Julia -> "DW_LANG_Julia"
  | DW_LANG_Dylan -> "DW_LANG_Dylan"
  | DW_LANG_C_plus_plus_14 -> "DW_LANG_C_plus_plus_14"
  | DW_LANG_Fortran03 -> "DW_LANG_Fortran03"
  | DW_LANG_Fortran08 -> "DW_LANG_Fortran08"
  | DW_LANG_RenderScript -> "DW_LANG_RenderScript"
  | DW_LANG_BLISS -> "DW_LANG_BLISS"
  | DW_LANG_lo_user -> "DW_LANG_lo_user"
  | DW_LANG_hi_user -> "DW_LANG_hi_user"

type identifier =
  | DW_ID_case_sensitive
  | DW_ID_up_case
  | DW_ID_down_case
  | DW_ID_case_insensitive

let identifier = function
  | 0x00 -> DW_ID_case_sensitive
  | 0x01 -> DW_ID_up_case
  | 0x02 -> DW_ID_down_case
  | 0x03 -> DW_ID_case_insensitive
  | n -> failwith (Printf.sprintf "Unknown identifier: 0x%02x" n)

type calling_convention =
  | DW_CC_normal
  | DW_CC_program
  | DW_CC_nocall  (** New in DWARF Version 5 *)
  | DW_CC_pass_by_reference
  | DW_CC_pass_by_value
  | DW_CC_lo_user
  | DW_CC_hi_user

let calling_convention = function
  | 0x01 -> DW_CC_normal
  | 0x02 -> DW_CC_program
  | 0x03 -> DW_CC_nocall
  | 0x04 -> DW_CC_pass_by_reference
  | 0x05 -> DW_CC_pass_by_value
  | 0x40 -> DW_CC_lo_user
  | 0xff -> DW_CC_hi_user
  | n -> failwith (Printf.sprintf "Unknown calling_convention: 0x%02x" n)

type inlined =
  | DW_INL_not_inlined
  | DW_INL_inlined
  | DW_INL_declared_not_inlined
  | DW_INL_declared_inlined

let inlined = function
  | 0x00 -> DW_INL_not_inlined
  | 0x01 -> DW_INL_inlined
  | 0x02 -> DW_INL_declared_not_inlined
  | 0x03 -> DW_INL_declared_inlined
  | n -> failwith (Printf.sprintf "Unknown inlined: 0x%02x" n)

type array_ordering = DW_ORD_row_major | DW_ORD_col_major

let array_ordering = function
  | 0x00 -> DW_ORD_row_major
  | 0x01 -> DW_ORD_col_major

type discriminant = DW_DSC_label | DW_DSC_range

let discriminant = function 0x00 -> DW_DSC_label | 0x01 -> DW_DSC_range

type name_index_attribute =
  | DW_IDX_compile_unit
  | DW_IDX_type_unit
  | DW_IDX_die_offset
  | DW_IDX_parent
  | DW_IDX_type_hash
  | DW_IDX_lo_user
  | DW_IDX_hi_user

let name_index_attribute = function
  | 1 -> DW_IDX_compile_unit
  | 2 -> DW_IDX_type_unit
  | 3 -> DW_IDX_die_offset
  | 4 -> DW_IDX_parent
  | 5 -> DW_IDX_type_hash
  | 0x2000 -> DW_IDX_lo_user
  | 0x3fff -> DW_IDX_hi_user
  | n -> failwith (Printf.sprintf "Unknown name_index_attribute: 0x%02x" n)

type defaulted_attribute =
  | DW_DEFAULTED_no
  | DW_DEFAULTED_in_class
  | DW_DEFAULTED_out_of_class

let defaulted_attribute = function
  | 0x00 -> DW_DEFAULTED_no
  | 0x01 -> DW_DEFAULTED_in_class
  | 0x02 -> DW_DEFAULTED_out_of_class
  | n -> failwith (Printf.sprintf "Unknown defaulted_attribute: 0x%02x" n)

type line_number_opcode =
  | DW_LNS_copy
  | DW_LNS_advance_pc
  | DW_LNS_advance_line
  | DW_LNS_set_file
  | DW_LNS_set_column
  | DW_LNS_negate_stmt
  | DW_LNS_set_basic_block
  | DW_LNS_const_add_pc
  | DW_LNS_fixed_advance_pc
  | DW_LNS_set_prologue_end
  | DW_LNS_set_epilogue_begin
  | DW_LNS_set_isa

let line_number_opcode = function
  | 0x01 -> DW_LNS_copy
  | 0x02 -> DW_LNS_advance_pc
  | 0x03 -> DW_LNS_advance_line
  | 0x04 -> DW_LNS_set_file
  | 0x05 -> DW_LNS_set_column
  | 0x06 -> DW_LNS_negate_stmt
  | 0x07 -> DW_LNS_set_basic_block
  | 0x08 -> DW_LNS_const_add_pc
  | 0x09 -> DW_LNS_fixed_advance_pc
  | 0x0a -> DW_LNS_set_prologue_end
  | 0x0b -> DW_LNS_set_epilogue_begin
  | 0x0c -> DW_LNS_set_isa
  | n -> failwith (Printf.sprintf "Unknown line_number_opcode: 0x%02x" n)

type line_number_extended_opcode =
  | DW_LNE_end_sequence
  | DW_LNE_set_address
  | DW_LNE_set_discriminator
  | DW_LNE_lo_user
  | DW_LNE_hi_user

let line_number_extended_opcode = function
  | 0x01 -> DW_LNE_end_sequence
  | 0x02 -> DW_LNE_set_address
  | 0x04 -> DW_LNE_set_discriminator
  | 0x80 -> DW_LNE_lo_user
  | 0xff -> DW_LNE_hi_user
  | n ->
      failwith (Printf.sprintf "Unknown line_number_extended_opcode: 0x%02x" n)

type line_number_header_entry =
  | DW_LNCT_path
  | DW_LNCT_directory_index
  | DW_LNCT_timestamp
  | DW_LNCT_size
  | DW_LNCT_MD5
  | DW_LNCT_lo_user
  | DW_LNCT_hi_user

let line_number_header_entry = function
  | 0x1 -> DW_LNCT_path
  | 0x2 -> DW_LNCT_directory_index
  | 0x3 -> DW_LNCT_timestamp
  | 0x4 -> DW_LNCT_size
  | 0x5 -> DW_LNCT_MD5
  | 0x2000 -> DW_LNCT_lo_user
  | 0x3fff -> DW_LNCT_hi_user
  | n -> failwith (Printf.sprintf "Unknown line_number_header_entry: 0x%04x" n)

type macro_info_entry_type =
  | DW_MACRO_define
  | DW_MACRO_undef
  | DW_MACRO_start_file
  | DW_MACRO_end_file
  | DW_MACRO_define_strp
  | DW_MACRO_undef_strp
  | DW_MACRO_import
  | DW_MACRO_define_sup
  | DW_MACRO_undef_sup
  | DW_MACRO_import_sup
  | DW_MACRO_define_strx
  | DW_MACRO_undef_strx
  | DW_MACRO_lo_user
  | DW_MACRO_hi_user

let macro_info_entry_type = function
  | 0x01 -> DW_MACRO_define
  | 0x02 -> DW_MACRO_undef
  | 0x03 -> DW_MACRO_start_file
  | 0x04 -> DW_MACRO_end_file
  | 0x05 -> DW_MACRO_define_strp
  | 0x06 -> DW_MACRO_undef_strp
  | 0x07 -> DW_MACRO_import
  | 0x08 -> DW_MACRO_define_sup
  | 0x09 -> DW_MACRO_undef_sup
  | 0x0a -> DW_MACRO_import_sup
  | 0x0b -> DW_MACRO_define_strx
  | 0x0c -> DW_MACRO_undef_strx
  | 0xe0 -> DW_MACRO_lo_user
  | 0xff -> DW_MACRO_hi_user
  | n -> failwith (Printf.sprintf "Unknown macro_info_entry_type: 0x%02x" n)

type call_frame_instruction =
  | DW_CFA_advance_loc
  | DW_CFA_offset
  | DW_CFA_restore
  | DW_CFA_nop
  | DW_CFA_set_loc
  | DW_CFA_advance_loc1
  | DW_CFA_advance_loc2
  | DW_CFA_advance_loc4
  | DW_CFA_offset_extended
  | DW_CFA_restore_extended
  | DW_CFA_undefined
  | DW_CFA_same_value
  | DW_CFA_register
  | DW_CFA_remember_state
  | DW_CFA_restore_state
  | DW_CFA_def_cfa
  | DW_CFA_def_cfa_register
  | DW_CFA_def_cfa_offset
  | DW_CFA_def_cfa_expression
  | DW_CFA_expression
  | DW_CFA_offset_extended_sf
  | DW_CFA_def_cfa_sf
  | DW_CFA_def_cfa_offset_sf
  | DW_CFA_val_offset
  | DW_CFA_val_offset_sf
  | DW_CFA_val_expression
  | DW_CFA_lo_user
  | DW_CFA_hi_user

type range_list_entry =
  | DW_RLE_end_of_list
  | DW_RLE_base_addressx
  | DW_RLE_startx_endx
  | DW_RLE_startx_length
  | DW_RLE_offset_pair
  | DW_RLE_base_address
  | DW_RLE_start_end
  | DW_RLE_start_length

let range_list_entry = function
  | 0x00 -> DW_RLE_end_of_list
  | 0x01 -> DW_RLE_base_addressx
  | 0x02 -> DW_RLE_startx_endx
  | 0x03 -> DW_RLE_startx_length
  | 0x04 -> DW_RLE_offset_pair
  | 0x05 -> DW_RLE_base_address
  | 0x06 -> DW_RLE_start_end
  | 0x07 -> DW_RLE_start_length
  | n -> failwith (Printf.sprintf "Unknown range_list_entry: 0x%02x" n)

module Object_file = struct
  type t = Buffer.t
end

type attr_spec = { attr : u64; form : u64 }

type abbrev = {
  code : u64;
  tag : u64;
  has_children : bool;
  attr_specs : attr_spec list;
}

(* Convert int to bool in C-style. *)
let bool_of_int = function 0 -> false | _ -> true

open Object.Buffer

let parse_abbrev_table (elf : Object_file.t) (offset : u32) :
    (u64, abbrev) Hashtbl.t =
  (* Try MachO first, then fall back to ELF *)
  (* TODO This should probably take a buffer that you already know is ELF or MachO. *)
  let try_format format =
    try
      let s = object_format_to_section_name format Debug_abbrev in
      match format with
      | MachO -> (
          (* TODO Use a find section helper here *)
          let _, commands = Object.Macho.read elf in
          let sections =
            List.fold_left
              (fun acc command ->
                match command with
                | Object.Macho.LC_SEGMENT_64 lazy_seg ->
                    let seg = Lazy.force lazy_seg in
                    Array.fold_left
                      (fun acc section ->
                        if section.Object.Macho.sec_sectname = s then
                          Some section :: acc
                        else acc)
                      acc seg.Object.Macho.seg_sections
                | _ -> acc)
              [] commands
          in
          match List.filter_map (fun x -> x) sections with
          | section :: _ -> Some (Object.Macho.section_body elf section)
          | [] -> None)
      | ELF ->
          let _, sa = Object.Elf.read_elf elf in
          Object.Elf.read_section_contents elf sa s
    with _ -> None
  in
  let buf =
    match try_format MachO with
    | Some buf -> buf
    | None -> (
        match try_format ELF with
        | Some buf -> buf
        | None ->
            failwith
              "Could not find debug_abbrev section in MachO or ELF format")
  in
  let cur = cursor buf ~at:(Unsigned.UInt32.to_int offset) in
  let table = Hashtbl.create 100 in

  let rec parse_abbrevs () =
    let code = Read.uleb128 cur in
    if code = 0 then () (* End of abbreviations *)
    else
      let tag = Read.uleb128 cur in
      let has_children = Read.u8 cur |> Unsigned.UInt8.to_int |> bool_of_int in

      let rec parse_attr_specs acc =
        let attr = Read.uleb128 cur in
        let form = Read.uleb128 cur in
        if attr = 0 && form = 0 then List.rev acc (* End of attributes *)
        else
          let attr_spec =
            {
              attr = Unsigned.UInt64.of_int attr;
              form = Unsigned.UInt64.of_int form;
            }
          in
          parse_attr_specs (attr_spec :: acc)
      in

      let attr_specs = parse_attr_specs [] in
      let abbrev =
        {
          code = Unsigned.UInt64.of_int code;
          tag = Unsigned.UInt64.of_int tag;
          has_children;
          attr_specs;
        }
      in
      Hashtbl.add table (Unsigned.UInt64.of_int code) abbrev;
      parse_abbrevs ()
  in

  parse_abbrevs ();
  table

(* String table helper functions *)
let find_debug_section buffer section_name =
  try
    let open Object.Macho in
    let _header, commands = read buffer in

    (* Look for __DWARF segment *)
    let dwarf_segment_opt =
      List.find_map
        (function
          | LC_SEGMENT_64 (lazy seg) when seg.seg_segname = "__DWARF" ->
              Some seg
          | _ -> None)
        commands
    in

    match dwarf_segment_opt with
    | None -> None
    | Some dwarf_segment ->
        (* Find the specified section within __DWARF segment *)
        Array.find_map
          (fun section ->
            if section.sec_sectname = section_name then
              Some (section.sec_offset, section.sec_size)
            else None)
          dwarf_segment.seg_sections
  with _ -> None

let read_string_from_section buffer offset section_offset : string option =
  try
    let actual_offset = section_offset + offset in
    let cursor = Object.Buffer.cursor buffer ~at:actual_offset in
    Object.Buffer.Read.zero_string cursor ()
  with _ -> None

(* TODO Provide tests for this function *)
let resolve_string_index (buffer : Object.Buffer.t) (index : int) : string =
  (* Try to resolve string index using debug_str_offs and debug_str sections *)
  match
    ( find_debug_section buffer "__debug_str_offs",
      find_debug_section buffer "__debug_str" )
  with
  | Some (str_offs_offset, _), Some (str_offset, _) -> (
      try
        (* Read offset from string offsets table *)
        (* Skip the 8-byte header: unit_length(4) + version(2) + padding(2) *)
        let str_offs_cursor =
          Object.Buffer.cursor buffer
            ~at:(Unsigned.UInt32.to_int str_offs_offset + 8 + (index * 4))
        in
        let string_offset =
          Object.Buffer.Read.u32 str_offs_cursor |> Unsigned.UInt32.to_int
        in

        (* Read actual string from debug_str section *)
        match
          read_string_from_section buffer string_offset
            (Unsigned.UInt32.to_int str_offset)
        with
        | Some s -> s
        | None -> Printf.sprintf "<strx_error:%d>" index
      with _ -> Printf.sprintf "<strx_error:%d>" index)
  | Some (_, _), None -> Printf.sprintf "<strx_no_str:%d>" index
  | None, Some (_, _) -> Printf.sprintf "<strx_no_offs:%d>" index
  | None, None -> Printf.sprintf "<strx_no_sections:%d>" index

module DIE = struct
  type attribute_value =
    | String of string
    | UData of u64
    | SData of i64
    | Address of u64
    | Flag of bool
    | Reference of u64
    | Block of string
    | Language of dwarf_language
    | Encoding of base_type

  type attribute = { attr : attribute_encoding; value : attribute_value }

  type t = {
    tag : abbreviation_tag;
    attributes : attribute list;
    children : t Seq.t;
    offset : int;
  }

  let find_attribute die attr_name =
    List.find_map
      (fun attr -> if attr.attr = attr_name then Some attr.value else None)
      die.attributes

  let parse_attribute_value (cur : Object.Buffer.cursor)
      (form : attribute_form_encoding) (full_buffer : Object.Buffer.t) :
      attribute_value =
    match form with
    | DW_FORM_string ->
        let str = Object.Buffer.Read.zero_string cur () in
        String (match str with Some s -> s | None -> "")
    | DW_FORM_strp ->
        (* String pointer to Debug_str section - simplified for now *)
        let offset = Object.Buffer.Read.u32 cur |> Unsigned.UInt32.to_int in
        String (Printf.sprintf "<strp_offset:%d>" offset)
    | DW_FORM_udata ->
        let value = Object.Buffer.Read.uleb128 cur in
        UData (Unsigned.UInt64.of_int value)
    | DW_FORM_sdata ->
        let value = Object.Buffer.Read.sleb128 cur in
        SData (Signed.Int64.of_int value)
    | DW_FORM_addr ->
        let addr = Object.Buffer.Read.u64 cur in
        Address addr
    | DW_FORM_flag ->
        let flag = Object.Buffer.Read.u8 cur |> Unsigned.UInt8.to_int in
        Flag (flag != 0)
    | DW_FORM_flag_present -> Flag true
    | DW_FORM_data1 ->
        let value = Object.Buffer.Read.u8 cur in
        UData (Unsigned.UInt64.of_int (Unsigned.UInt8.to_int value))
    | DW_FORM_data2 ->
        let value = Object.Buffer.Read.u16 cur in
        UData (Unsigned.UInt64.of_int (Unsigned.UInt16.to_int value))
    | DW_FORM_data4 ->
        let value = Object.Buffer.Read.u32 cur in
        UData (Unsigned.UInt64.of_uint32 value)
    | DW_FORM_data8 ->
        let value = Object.Buffer.Read.u64 cur in
        UData value
    | DW_FORM_strx ->
        (* String index form - reads ULEB128 index into string offsets table *)
        let index = Object.Buffer.Read.uleb128 cur in
        let resolved_string = resolve_string_index full_buffer index in
        String resolved_string
    | DW_FORM_sec_offset ->
        (* Section offset - 4 or 8 bytes depending on DWARF format *)
        let offset = Object.Buffer.Read.u32 cur in
        UData (Unsigned.UInt64.of_uint32 offset)
    | DW_FORM_addrx ->
        (* Address index - ULEB128 index into address table *)
        let index = Object.Buffer.Read.uleb128 cur in
        (* TODO: Implement proper address table resolution using DW_AT_addr_base *)
        (* For now, treat as an address placeholder that needs resolution *)
        Address (Unsigned.UInt64.of_int index)
    | DW_FORM_ref4 ->
        (* 4-byte offset reference within same compilation unit *)
        let offset = Object.Buffer.Read.u32 cur in
        Reference (Unsigned.UInt64.of_uint32 offset)
    | DW_FORM_ref8 ->
        (* 8-byte offset reference within same compilation unit *)
        let offset = Object.Buffer.Read.u64 cur in
        Reference offset
    | DW_FORM_ref1 ->
        (* 1-byte offset reference within same compilation unit *)
        let offset = Object.Buffer.Read.u8 cur in
        Reference (Unsigned.UInt64.of_int (Unsigned.UInt8.to_int offset))
    | DW_FORM_ref2 ->
        (* 2-byte offset reference within same compilation unit *)
        let offset = Object.Buffer.Read.u16 cur in
        Reference (Unsigned.UInt64.of_int (Unsigned.UInt16.to_int offset))
    | DW_FORM_exprloc ->
        (* DWARF expression/location - ULEB128 length followed by expression *)
        let length = Object.Buffer.Read.uleb128 cur in
        let expr_data = Object.Buffer.Read.fixed_string cur length in
        Block expr_data
    | DW_FORM_block ->
        (* Variable length block - ULEB128 length followed by data *)
        let length = Object.Buffer.Read.uleb128 cur in
        let block_data = Object.Buffer.Read.fixed_string cur length in
        Block block_data
    | DW_FORM_block1 ->
        (* 1-byte length followed by data *)
        let length = Object.Buffer.Read.u8 cur |> Unsigned.UInt8.to_int in
        let block_data = Object.Buffer.Read.fixed_string cur length in
        Block block_data
    | DW_FORM_block2 ->
        (* 2-byte length followed by data *)
        let length = Object.Buffer.Read.u16 cur |> Unsigned.UInt16.to_int in
        let block_data = Object.Buffer.Read.fixed_string cur length in
        Block block_data
    | DW_FORM_block4 ->
        (* 4-byte length followed by data *)
        let length = Object.Buffer.Read.u32 cur |> Unsigned.UInt32.to_int in
        let block_data = Object.Buffer.Read.fixed_string cur length in
        Block block_data
    | _ ->
        (* For unsupported forms, skip and return placeholder *)
        String "<unsupported_form>"

  let process_language_attribute (raw_value : attribute_value) : attribute_value
      =
    match raw_value with
    | UData lang_code -> (
        let lang_int = Unsigned.UInt64.to_int lang_code in
        (* Language codes are parsed correctly by Object.Buffer based on file endianness *)
        try Language (dwarf_language lang_int) with _ -> raw_value)
    | _ -> raw_value

  let process_encoding_attribute (raw_value : attribute_value) : attribute_value
      =
    match raw_value with
    | UData encoding_code -> (
        let encoding_int = Unsigned.UInt64.to_int encoding_code in
        (* Encoding codes are parsed correctly by Object.Buffer based on file endianness *)
        try Encoding (base_type encoding_int) with _ -> raw_value)
    | _ -> raw_value

  let rec parse_children_seq (cur : Object.Buffer.cursor)
      (abbrev_table : (u64, abbrev) Hashtbl.t) (full_buffer : Object.Buffer.t) :
      t Seq.t =
   fun () ->
    match parse_die cur abbrev_table full_buffer with
    | None -> Seq.Nil (* End of children marker or error *)
    | Some die -> Seq.Cons (die, parse_children_seq cur abbrev_table full_buffer)

  and parse_die (cur : Object.Buffer.cursor)
      (abbrev_table : (u64, abbrev) Hashtbl.t) (full_buffer : Object.Buffer.t) :
      t option =
    try
      (* Capture the position at the start of this DIE *)
      let die_offset = cur.position in
      let abbrev_code = Object.Buffer.Read.uleb128 cur in
      if abbrev_code = 0 then None (* End of children marker *)
      else
        let abbrev_code_u64 = Unsigned.UInt64.of_int abbrev_code in
        match Hashtbl.find_opt abbrev_table abbrev_code_u64 with
        | None -> None (* Invalid abbreviation code *)
        | Some abbrev ->
            (* Convert tag from u64 to abbreviation_tag *)
            let tag = abbreviation_tag_of_int abbrev.tag in

            (* Parse attributes according to abbreviation specification *)
            let attributes =
              List.map
                (fun (spec : attr_spec) ->
                  let attr_encoding = attribute_encoding spec.attr in
                  let form_encoding = attribute_form_encoding spec.form in
                  let raw_value =
                    parse_attribute_value cur form_encoding full_buffer
                  in
                  let value =
                    (* Process special attributes that need conversion *)
                    if attr_encoding = DW_AT_language then
                      process_language_attribute raw_value
                    else if attr_encoding = DW_AT_encoding then
                      process_encoding_attribute raw_value
                    else raw_value
                  in
                  { attr = attr_encoding; value })
                abbrev.attr_specs
            in
            (* Parse children if the abbreviation indicates this DIE has children *)
            let children =
              if abbrev.has_children then
                parse_children_seq cur abbrev_table full_buffer
              else Seq.empty
            in
            Some { tag; attributes; children; offset = die_offset }
    with _ -> None
end

(* Represents a section of the binary that corresponds to an
   element e.g. [Die.t] *)
type span = { start : size_t; size : size_t }

module CompileUnit = struct
  type header = {
    unit_length : u32;
    version : u16;
    unit_type : u8;
    debug_abbrev_offset : u32;
    address_size : u8;
  }

  type t = {
    parent_ : int;
    span : span;
    raw_buffer_ : Object_file.t;
    header : header;
  }

  let make parent_ span raw_buffer_ header =
    { parent_; span; raw_buffer_; header }

  let dwarf_info t = t.parent_
  let data t = t.span
  let header t = t.header

  let root_die t abbrev_table full_buffer =
    (* Create cursor positioned after the compilation unit header *)
    (* let _parsed = parsed_data t in *)
    (* DWARF 5 header: unit_length(4) + version(2) + unit_type(1) + address_size(1) + debug_abbrev_offset(4) = 12 bytes *)
    let cur =
      Object.Buffer.cursor t.raw_buffer_
        ~at:
          (Unsigned.UInt64.to_int
             (Unsigned.UInt64.add t.span.start (Unsigned.UInt64.of_int 12)))
    in
    (* Skip header *)
    DIE.parse_die cur abbrev_table full_buffer
end

(* TODO Record to keep the different parsed areas of an object file together.
   Perhaps this belongs in a consumer? *)
type t = {
  abbrev_tables_ : (size_t, (u64, abbrev) Hashtbl.t) Hashtbl.t;
  compile_units_ : CompileUnit.t Array.t;
  object_ : Object_file.t;
}

let parse_compile_unit_header (cur : Object.Buffer.cursor) :
    span * CompileUnit.header =
  let start = cur.position in
  (* Parse DWARF 5 compile unit header *)
  let unit_length = Object.Buffer.Read.u32 cur in
  let version = Object.Buffer.Read.u16 cur in
  let unit_type = Object.Buffer.Read.u8 cur in
  let address_size = Object.Buffer.Read.u8 cur in
  let debug_abbrev_offset = Object.Buffer.Read.u32 cur in

  let parsed_unit_type = unit_type_of_u8 unit_type in

  (* Only support DWARF32 format for now *)
  if unit_length = Unsigned.UInt32.of_int 0xffffffff then
    failwith "Only DWARF32 is supported";

  (* Only support DWARF version 5 *)
  if version <> Unsigned.UInt16.of_int 5 then
    failwith "Only DWARF version 5 is supported";

  (* Only support compile unit type DW_UT_compile *)
  if parsed_unit_type <> DW_UT_compile then
    failwith
      ("Only DW_UT_compile unit type is supported, got: "
      ^ string_of_unit_type parsed_unit_type);

  (* Validate address size *)
  if address_size <> Unsigned.UInt8.of_int 8 then
    failwith "Invalid address size of DWARF";

  (* Total size includes the length field itself *)
  let total_size =
    start + 4
    (* sizeof(uint32_t) *)
  in
  let span =
    {
      start = Unsigned.UInt64.of_int start;
      size = Unsigned.UInt64.of_int total_size;
    }
  in
  (span, { unit_length; version; unit_type; debug_abbrev_offset; address_size })

let parse_compile_unit (cur : Object.Buffer.cursor) : CompileUnit.t =
  (* Start by parsing just the header to get size *)
  let start = cur.position in

  (* Reset cursor to start for consistent parsing *)
  Object.Buffer.seek cur start;

  let data, parsed = parse_compile_unit_header cur in
  CompileUnit.make 0 data cur.buffer parsed

let parse_compile_units (dwarf : t) : CompileUnit.t Seq.t =
  let s = object_format_to_section_name MachO Debug_info in
  let _header, commands = Object.Macho.read dwarf.object_ in

  (* Find the debug_info section *)
  (* TODO Add helper function for finding a section within a MachO binary.  *)
  let debug_info_section =
    List.fold_left
      (fun acc command ->
        match command with
        | Object.Macho.LC_SEGMENT_64 lazy_seg ->
            let seg = Lazy.force lazy_seg in
            Array.fold_left
              (fun acc section ->
                if section.Object.Macho.sec_sectname = s then Some section
                else acc)
              acc seg.Object.Macho.seg_sections
        | _ -> acc)
      None commands
  in

  match debug_info_section with
  | None -> Seq.empty
  | Some section ->
      let buf = Object.Macho.section_body dwarf.object_ section in
      let section_end = Unsigned.UInt64.to_int section.Object.Macho.sec_size in

      (* Create a lazy sequence generator *)
      let rec parse_units cursor_pos () =
        if cursor_pos >= section_end then Seq.Nil
        else
          try
            let cur = Object.Buffer.cursor buf ~at:cursor_pos in
            let span, parsed_header = parse_compile_unit_header cur in
            let unit = CompileUnit.make 0 span cur.buffer parsed_header in

            (* Calculate next position: current + unit_length + 4 (for length field) *)
            let unit_length =
              Unsigned.UInt32.to_int parsed_header.unit_length
            in
            let next_pos = cursor_pos + unit_length + 4 in

            Seq.Cons (unit, parse_units next_pos)
          with exn ->
            Printf.eprintf "Error parsing compile unit at offset %d: %s\n"
              cursor_pos (Printexc.to_string exn);
            Seq.Nil
      in
      parse_units 0

module LineTable = struct
  type t = { cu : CompileUnit.t }

  module File = struct
    type t = { path : string; modification_time : u64; file_length : u64 }
  end

  type line_program_header = {
    unit_length : u32;
    version : u16;
    address_size : u8;
    segment_selector_size : u8;
    header_length : u32;
    minimum_instruction_length : u8;
    maximum_operations_per_instruction : u8;
    default_is_stmt : bool;
    line_base : int;
    line_range : u8;
    opcode_base : u8;
    standard_opcode_lengths : u8 array;
    directory_entry_format_count : u8;
    directory_entry_formats :
      (line_number_header_entry * attribute_form_encoding) array;
    directories_count : u32;
    directories : string array;
    file_name_entry_format_count : u8;
    file_name_entry_formats :
      (line_number_header_entry * attribute_form_encoding) array;
    file_names_count : u32;
    file_names : (string * u64 * u64 * string * string option) array;
  }

  let resolve_line_strp_offset buffer offset =
    (* Find the debug_line_str section and read the string at the given offset *)
    match find_debug_section buffer "__debug_line_str" with
    | None ->
        Printf.sprintf "<line_strp:0x%08lx>" (Unsigned.UInt32.to_int32 offset)
    | Some (section_offset, _size) -> (
        try
          let string_cursor =
            Object.Buffer.cursor buffer
              ~at:
                (Unsigned.UInt32.to_int section_offset
                + Unsigned.UInt32.to_int offset)
          in
          match Object.Buffer.Read.zero_string string_cursor () with
          | Some s -> s
          | None ->
              Printf.sprintf "<line_strp:0x%08lx>"
                (Unsigned.UInt32.to_int32 offset)
        with _ ->
          Printf.sprintf "<line_strp:0x%08lx>" (Unsigned.UInt32.to_int32 offset)
        )

  let parse_line_program_header (cur : Object.Buffer.cursor) buffer :
      line_program_header =
    let unit_length = Object.Buffer.Read.u32 cur in
    let version = Object.Buffer.Read.u16 cur in
    let address_size = Object.Buffer.Read.u8 cur in
    let segment_selector_size = Object.Buffer.Read.u8 cur in
    let header_length = Object.Buffer.Read.u32 cur in
    let minimum_instruction_length = Object.Buffer.Read.u8 cur in
    let maximum_operations_per_instruction = Object.Buffer.Read.u8 cur in
    let default_is_stmt =
      Object.Buffer.Read.u8 cur |> Unsigned.UInt8.to_int |> ( <> ) 0
    in
    let line_base_u8 = Object.Buffer.Read.u8 cur in
    let line_base =
      if Unsigned.UInt8.to_int line_base_u8 > 127 then
        Unsigned.UInt8.to_int line_base_u8 - 256
      else Unsigned.UInt8.to_int line_base_u8
    in
    let line_range = Object.Buffer.Read.u8 cur in
    let opcode_base = Object.Buffer.Read.u8 cur in

    let standard_opcode_lengths =
      Array.make
        (Unsigned.UInt8.to_int opcode_base - 1)
        (Unsigned.UInt8.of_int 0)
    in
    for i = 0 to Array.length standard_opcode_lengths - 1 do
      standard_opcode_lengths.(i) <- Object.Buffer.Read.u8 cur
    done;

    let directory_entry_format_count = Object.Buffer.Read.u8 cur in
    let directory_entry_formats =
      Array.make
        (Unsigned.UInt8.to_int directory_entry_format_count)
        (DW_LNCT_path, DW_FORM_string)
    in
    for i = 0 to Array.length directory_entry_formats - 1 do
      let content_type_code = Object.Buffer.Read.uleb128 cur in
      let form_code = Object.Buffer.Read.uleb128 cur in
      let content_type = line_number_header_entry content_type_code in
      let form = attribute_form_encoding (Unsigned.UInt64.of_int form_code) in
      directory_entry_formats.(i) <- (content_type, form)
    done;

    let directories_count =
      Object.Buffer.Read.uleb128 cur |> Unsigned.UInt32.of_int
    in
    let directories =
      Array.make (Unsigned.UInt32.to_int directories_count) ""
    in
    for i = 0 to Array.length directories - 1 do
      (* Parse directory entry according to the format descriptors *)
      let path_ref = ref "" in
      for j = 0 to Array.length directory_entry_formats - 1 do
        let content_type, form = directory_entry_formats.(j) in
        match (content_type, form) with
        | DW_LNCT_path, DW_FORM_string -> (
            path_ref :=
              match Object.Buffer.Read.zero_string cur () with
              | Some s -> s
              | None -> "")
        | DW_LNCT_path, DW_FORM_line_strp ->
            (* Read offset into .debug_line_str section *)
            let offset = Object.Buffer.Read.u32 cur in
            (* Resolve line_strp offset to actual string *)
            path_ref := resolve_line_strp_offset buffer offset
        | _, _ -> (
            (* Skip other content types or forms we don't handle yet *)
            match form with
            | DW_FORM_string -> ignore (Object.Buffer.Read.zero_string cur ())
            | DW_FORM_data1 -> ignore (Object.Buffer.Read.u8 cur)
            | DW_FORM_data2 -> ignore (Object.Buffer.Read.u16 cur)
            | DW_FORM_data4 -> ignore (Object.Buffer.Read.u32 cur)
            | DW_FORM_data8 -> ignore (Object.Buffer.Read.u64 cur)
            | _ -> failwith "Unsupported directory entry form")
      done;
      directories.(i) <- !path_ref
    done;

    let file_name_entry_format_count = Object.Buffer.Read.u8 cur in
    let file_name_entry_formats =
      Array.make
        (Unsigned.UInt8.to_int file_name_entry_format_count)
        (DW_LNCT_path, DW_FORM_string)
    in
    for i = 0 to Array.length file_name_entry_formats - 1 do
      let content_type_code = Object.Buffer.Read.uleb128 cur in
      let form_code = Object.Buffer.Read.uleb128 cur in
      (* TODO Validate this type is supposed to be uleb128. *)
      let content_type = line_number_header_entry content_type_code in
      let form = attribute_form_encoding (Unsigned.UInt64.of_int form_code) in
      file_name_entry_formats.(i) <- (content_type, form)
    done;

    let file_names_count =
      Object.Buffer.Read.uleb128 cur |> Unsigned.UInt32.of_int
    in
    let file_names =
      Array.make
        (Unsigned.UInt32.to_int file_names_count)
        ("", Unsigned.UInt64.of_int 0, Unsigned.UInt64.of_int 0, "", None)
    in
    for i = 0 to Array.length file_names - 1 do
      (* Parse file entry according to the format descriptors *)
      let path_ref = ref "" in
      let dir_index_ref = ref 0 in
      let timestamp_ref = ref (Unsigned.UInt64.of_int 0) in
      let file_size_ref = ref (Unsigned.UInt64.of_int 0) in
      let md5_ref = ref None in

      for j = 0 to Array.length file_name_entry_formats - 1 do
        let content_type, form = file_name_entry_formats.(j) in
        match (content_type, form) with
        | DW_LNCT_path, DW_FORM_string -> (
            path_ref :=
              match Object.Buffer.Read.zero_string cur () with
              | Some s -> s
              | None -> "")
        | DW_LNCT_path, DW_FORM_line_strp ->
            (* Read offset into .debug_line_str section and resolve *)
            let offset = Object.Buffer.Read.u32 cur in
            path_ref := resolve_line_strp_offset buffer offset
        | DW_LNCT_directory_index, DW_FORM_udata ->
            dir_index_ref := Object.Buffer.Read.uleb128 cur
        | DW_LNCT_timestamp, DW_FORM_udata ->
            timestamp_ref :=
              Object.Buffer.Read.uleb128 cur |> Unsigned.UInt64.of_int
        | DW_LNCT_size, DW_FORM_udata ->
            file_size_ref :=
              Object.Buffer.Read.uleb128 cur |> Unsigned.UInt64.of_int
        | DW_LNCT_MD5, DW_FORM_data16 ->
            (* Read MD5 hash (16 bytes) *)
            let md5_bytes = Array.make 16 (Unsigned.UInt8.of_int 0) in
            for k = 0 to 15 do
              md5_bytes.(k) <- Object.Buffer.Read.u8 cur
            done;
            (* Convert to hex string *)
            let md5_hex =
              String.concat ""
                (Array.to_list
                   (Array.map
                      (fun b -> Printf.sprintf "%02x" (Unsigned.UInt8.to_int b))
                      md5_bytes))
            in
            md5_ref := Some md5_hex
        | _, _ -> (
            (* Skip other content types or forms we don't handle yet *)
            match form with
            | DW_FORM_string -> ignore (Object.Buffer.Read.zero_string cur ())
            | DW_FORM_data1 -> ignore (Object.Buffer.Read.u8 cur)
            | DW_FORM_data2 -> ignore (Object.Buffer.Read.u16 cur)
            | DW_FORM_data4 -> ignore (Object.Buffer.Read.u32 cur)
            | DW_FORM_data8 -> ignore (Object.Buffer.Read.u64 cur)
            | DW_FORM_data16 ->
                for _k = 0 to 15 do
                  ignore (Object.Buffer.Read.u8 cur)
                done
            | DW_FORM_udata -> ignore (Object.Buffer.Read.uleb128 cur)
            | _ -> failwith "Unsupported file name entry form")
      done;

      let dir_name =
        if !dir_index_ref < Array.length directories then
          directories.(!dir_index_ref)
        else ""
      in
      file_names.(i) <-
        (!path_ref, !timestamp_ref, !file_size_ref, dir_name, !md5_ref)
    done;

    {
      unit_length;
      version;
      address_size;
      segment_selector_size;
      header_length;
      minimum_instruction_length;
      maximum_operations_per_instruction;
      default_is_stmt;
      line_base;
      line_range;
      opcode_base;
      standard_opcode_lengths;
      directory_entry_format_count;
      directory_entry_formats;
      directories_count;
      directories;
      file_name_entry_format_count;
      file_name_entry_formats;
      file_names_count;
      file_names;
    }
end

(** Call frame information parsing for Debug_frame section *)
module CallFrame = struct
  type common_information_entry = {
    length : u32;
    cie_id : u32;
    version : u8;
    augmentation : string;
    address_size : u8;
    segment_selector_size : u8;
    code_alignment_factor : u64;
    data_alignment_factor : i64;
    return_address_register : u64;
    augmentation_length : u64 option;
    augmentation_data : string option;
    initial_instructions : string;
  }

  type frame_description_entry = {
    length : u32;
    cie_pointer : u32;
    initial_location : u64;
    address_range : u64;
    augmentation_length : u64 option;
    augmentation_data : string option;
    instructions : string;
  }

  (** Parse a null-terminated augmentation string from a cursor *)
  let parse_augmentation_string (cur : Object.Buffer.cursor) : string =
    match Object.Buffer.Read.zero_string cur () with Some s -> s | None -> ""

  (** Parse augmentation data if present based on augmentation string *)
  let parse_augmentation_data (cur : Object.Buffer.cursor)
      (augmentation : string) : (u64 * string) option =
    if String.length augmentation > 0 && augmentation.[0] = 'z' then (
      let length_int = Object.Buffer.Read.uleb128 cur in
      let length = Unsigned.UInt64.of_int length_int in
      let data = Bytes.create length_int in
      for i = 0 to Bytes.length data - 1 do
        Bytes.set data i
          (Char.chr (Unsigned.UInt8.to_int (Object.Buffer.Read.u8 cur)))
      done;
      Some (length, Bytes.to_string data))
    else None

  (** Parse call frame instructions as raw bytes *)
  let parse_instructions (cur : Object.Buffer.cursor) (length : int) : string =
    let data = Bytes.create length in
    for i = 0 to length - 1 do
      Bytes.set data i
        (Char.chr (Unsigned.UInt8.to_int (Object.Buffer.Read.u8 cur)))
    done;
    Bytes.to_string data

  (** Parse a Common Information Entry from the Debug_frame section *)
  let parse_common_information_entry (cur : Object.Buffer.cursor) :
      common_information_entry =
    let length = Object.Buffer.Read.u32 cur in
    let cie_id = Object.Buffer.Read.u32 cur in

    (* Verify this is actually a CIE (cie_id should be 0xffffffff) *)
    if Unsigned.UInt32.to_int32 cie_id <> 0xffffffffl then
      failwith "Invalid CIE: cie_id is not 0xffffffff";

    let version = Object.Buffer.Read.u8 cur in
    let augmentation = parse_augmentation_string cur in
    let address_size = Object.Buffer.Read.u8 cur in
    let segment_selector_size = Object.Buffer.Read.u8 cur in
    let code_alignment_factor_int = Object.Buffer.Read.uleb128 cur in
    let code_alignment_factor =
      Unsigned.UInt64.of_int code_alignment_factor_int
    in
    let data_alignment_factor_int = Object.Buffer.Read.sleb128 cur in
    let data_alignment_factor = Signed.Int64.of_int data_alignment_factor_int in
    let return_address_register_int = Object.Buffer.Read.uleb128 cur in
    let return_address_register =
      Unsigned.UInt64.of_int return_address_register_int
    in

    (* Parse augmentation data if present *)
    let augmentation_length, augmentation_data =
      match parse_augmentation_data cur augmentation with
      | Some (len, data) -> (Some len, Some data)
      | None -> (None, None)
    in

    (* Calculate approximate size for initial instructions.
       This is a simplified calculation - in a full implementation,
       we would need to track the exact cursor position. *)
    let base_header_size = 9 + String.length augmentation + 1 in
    let aug_data_size =
      match augmentation_length with
      | Some len -> Unsigned.UInt64.to_int len
      | None -> 0
    in
    (* Approximate size allowing for ULEB128/SLEB128 values (typically 1-5 bytes each) *)
    let uleb_sleb_size = 15 in
    (* Conservative estimate for 3 variable-length integers *)
    let instructions_length =
      max 0
        (Unsigned.UInt32.to_int length
        - base_header_size - aug_data_size - uleb_sleb_size)
    in
    let initial_instructions =
      if instructions_length > 0 then parse_instructions cur instructions_length
      else ""
    in

    {
      length;
      cie_id;
      version;
      augmentation;
      address_size;
      segment_selector_size;
      code_alignment_factor;
      data_alignment_factor;
      return_address_register;
      augmentation_length;
      augmentation_data;
      initial_instructions;
    }
end

(** Debug names section parsing for Debug_names section *)
module DebugNames = struct
  type name_index_header = {
    unit_length : u32;
    version : u16;
    padding : u16;
    comp_unit_count : u32;
    local_type_unit_count : u32;
    foreign_type_unit_count : u32;
    bucket_count : u32;
    name_count : u32;
    abbrev_table_size : u32;
    augmentation_string_size : u32;
    augmentation_string : string;
  }

  type name_index_entry = {
    name_offset : u32;
    die_offset : u32;
    attributes : (name_index_attribute * u64) list;
  }

  type debug_names_section = {
    header : name_index_header;
    comp_unit_offsets : u32 array;
    local_type_unit_offsets : u32 array;
    foreign_type_unit_signatures : u64 array;
    hash_table : u32 array;
    name_table : string array;
    entry_pool : name_index_entry array;
  }

  (** Parse augmentation string from debug_names header *)
  let parse_augmentation_string (cur : Object.Buffer.cursor) (size : u32) :
      string =
    if Unsigned.UInt32.to_int size = 0 then ""
    else
      let data = Bytes.create (Unsigned.UInt32.to_int size) in
      for i = 0 to Bytes.length data - 1 do
        Bytes.set data i
          (Char.chr (Unsigned.UInt8.to_int (Object.Buffer.Read.u8 cur)))
      done;
      Bytes.to_string data

  (** Parse name index header *)
  let parse_name_index_header (cur : Object.Buffer.cursor) : name_index_header =
    let unit_length = Object.Buffer.Read.u32 cur in
    let version = Object.Buffer.Read.u16 cur in
    let padding = Object.Buffer.Read.u16 cur in
    let comp_unit_count = Object.Buffer.Read.u32 cur in
    let local_type_unit_count = Object.Buffer.Read.u32 cur in
    let foreign_type_unit_count = Object.Buffer.Read.u32 cur in
    let bucket_count = Object.Buffer.Read.u32 cur in
    let name_count = Object.Buffer.Read.u32 cur in
    let abbrev_table_size = Object.Buffer.Read.u32 cur in
    let augmentation_string_size = Object.Buffer.Read.u32 cur in
    let augmentation_string =
      parse_augmentation_string cur augmentation_string_size
    in

    {
      unit_length;
      version;
      padding;
      comp_unit_count;
      local_type_unit_count;
      foreign_type_unit_count;
      bucket_count;
      name_count;
      abbrev_table_size;
      augmentation_string_size;
      augmentation_string;
    }

  (** Parse array of 32-bit offsets *)
  let parse_u32_array (cur : Object.Buffer.cursor) (count : u32) : u32 array =
    let arr =
      Array.make (Unsigned.UInt32.to_int count) (Unsigned.UInt32.of_int 0)
    in
    for i = 0 to Array.length arr - 1 do
      arr.(i) <- Object.Buffer.Read.u32 cur
    done;
    arr

  (** Parse array of 64-bit signatures *)
  let parse_u64_array (cur : Object.Buffer.cursor) (count : u32) : u64 array =
    let arr =
      Array.make (Unsigned.UInt32.to_int count) (Unsigned.UInt64.of_int 0)
    in
    for i = 0 to Array.length arr - 1 do
      arr.(i) <- Object.Buffer.Read.u64 cur
    done;
    arr

  (** Parse name table as array of null-terminated strings *)
  let parse_name_table (cur : Object.Buffer.cursor) (count : u32) : string array
      =
    let arr = Array.make (Unsigned.UInt32.to_int count) "" in
    for i = 0 to Array.length arr - 1 do
      arr.(i) <-
        (match Object.Buffer.Read.zero_string cur () with
        | Some s -> s
        | None -> "")
    done;
    arr

  (** Parse a complete debug_names section *)
  let parse_debug_names_section (cur : Object.Buffer.cursor) :
      debug_names_section =
    let header = parse_name_index_header cur in

    let comp_unit_offsets = parse_u32_array cur header.comp_unit_count in
    let local_type_unit_offsets =
      parse_u32_array cur header.local_type_unit_count
    in
    let foreign_type_unit_signatures =
      parse_u64_array cur header.foreign_type_unit_count
    in
    let hash_table = parse_u32_array cur header.bucket_count in
    let name_table = parse_name_table cur header.name_count in

    (* TODO For now, entry pool parsing is simplified - in a full implementation
       we would need to parse the abbreviation table and entry pool *)
    let entry_pool =
      Array.make 0
        {
          name_offset = Unsigned.UInt32.of_int 0;
          die_offset = Unsigned.UInt32.of_int 0;
          attributes = [];
        }
    in

    {
      header;
      comp_unit_offsets;
      local_type_unit_offsets;
      foreign_type_unit_signatures;
      hash_table;
      name_table;
      entry_pool;
    }
end

let get_abbrev_table t (offset : size_t) =
  let a = Hashtbl.find_opt t.abbrev_tables_ offset in
  match a with
  | Some a -> (t, a)
  | None ->
      let a =
        parse_abbrev_table t.object_
          (Unsigned.UInt32.of_int64 (Unsigned.UInt64.to_int64 offset))
      in
      Hashtbl.add t.abbrev_tables_ offset a;
      (t, a)

let create object_ =
  { abbrev_tables_ = Hashtbl.create 10; compile_units_ = [||]; object_ }

let get_compile_units t =
  let compile_units = parse_compile_units t |> List.of_seq |> Array.of_list in
  { t with compile_units_ = compile_units }

module DebugStrOffsets = struct
  type header = { unit_length : u32; version : u16; padding : u16 }
  type offset_entry = { offset : u32; resolved_string : string option }
  type t = { header : header; offsets : offset_entry array }

  let parse_header (cursor : Object.Buffer.cursor) : header =
    let unit_length = Object.Buffer.Read.u32 cursor in
    let version = Object.Buffer.Read.u16 cursor in
    let padding = Object.Buffer.Read.u16 cursor in

    (* Validate DWARF version 5 *)
    if Unsigned.UInt16.to_int version != 5 then
      failwith
        (Printf.sprintf "Expected DWARF version 5, got %d"
           (Unsigned.UInt16.to_int version));

    { unit_length; version; padding }

  let parse_offsets (cursor : Object.Buffer.cursor) (header : header)
      (debug_str_section : (u32 * u64) option) (buffer : Object.Buffer.t) :
      offset_entry array =
    let _header_size = 8 in
    (* unit_length(4) + version(2) + padding(2) *)
    let offset_size = 4 in
    (* 4-byte offsets for DWARF32 *)
    let data_size = Unsigned.UInt32.to_int header.unit_length - 4 in
    (* unit_length excludes itself *)
    let num_offsets = data_size / offset_size in

    Array.init num_offsets (fun _i ->
        let offset = Object.Buffer.Read.u32 cursor in
        let resolved_string =
          match debug_str_section with
          | Some (str_section_offset, _) -> (
              try
                let str_cursor =
                  Object.Buffer.cursor buffer
                    ~at:
                      (Unsigned.UInt32.to_int str_section_offset
                      + Unsigned.UInt32.to_int offset)
                in
                Object.Buffer.Read.zero_string str_cursor ()
              with _ -> None)
          | None -> None
        in
        { offset; resolved_string })

  let parse (buffer : Object.Buffer.t) (section_offset : u32) : t =
    let cursor =
      Object.Buffer.cursor buffer ~at:(Unsigned.UInt32.to_int section_offset)
    in
    let header = parse_header cursor in

    (* Find debug_str section for string resolution *)
    let debug_str_section = find_debug_section buffer "__debug_str" in

    let offsets = parse_offsets cursor header debug_str_section buffer in
    { header; offsets }
end

module DebugAddr = struct
  type header = {
    unit_length : u32;
    version : u16;
    address_size : u8;
    segment_selector_size : u8;
  }

  type entry = {
    segment : u64 option; (* Present only if segment_selector_size > 0 *)
    address : u64;
  }

  type t = { header : header; entries : entry array }

  let parse_header cursor =
    let unit_length = Object.Buffer.Read.u32 cursor in
    let version = Object.Buffer.Read.u16 cursor in
    let address_size = Object.Buffer.Read.u8 cursor in
    let segment_selector_size = Object.Buffer.Read.u8 cursor in
    { unit_length; version; address_size; segment_selector_size }

  let parse_entries cursor header =
    (* Calculate number of entries from unit_length *)
    (* unit_length includes everything after the length field itself *)
    (* header takes 4 bytes (version + address_size + segment_selector_size) *)
    let header_size = 4 in
    let remaining_length =
      Unsigned.UInt32.to_int header.unit_length - header_size
    in
    let entry_size =
      (if Unsigned.UInt8.to_int header.segment_selector_size > 0 then
         Unsigned.UInt8.to_int header.segment_selector_size
       else 0)
      + Unsigned.UInt8.to_int header.address_size
    in
    let num_entries = remaining_length / entry_size in

    Array.init num_entries (fun _i ->
        let segment =
          if Unsigned.UInt8.to_int header.segment_selector_size > 0 then
            (* Read segment selector based on its size *)
            match Unsigned.UInt8.to_int header.segment_selector_size with
            | 1 ->
                Some
                  (Unsigned.UInt64.of_int
                     (Unsigned.UInt8.to_int (Object.Buffer.Read.u8 cursor)))
            | 2 ->
                Some
                  (Unsigned.UInt64.of_int
                     (Unsigned.UInt16.to_int (Object.Buffer.Read.u16 cursor)))
            | 4 ->
                Some (Unsigned.UInt64.of_uint32 (Object.Buffer.Read.u32 cursor))
            | 8 -> Some (Object.Buffer.Read.u64 cursor)
            | _ -> failwith "Invalid segment_selector_size"
          else None
        in
        let address =
          (* Read address based on address_size *)
          match Unsigned.UInt8.to_int header.address_size with
          | 1 ->
              Unsigned.UInt64.of_int
                (Unsigned.UInt8.to_int (Object.Buffer.Read.u8 cursor))
          | 2 ->
              Unsigned.UInt64.of_int
                (Unsigned.UInt16.to_int (Object.Buffer.Read.u16 cursor))
          | 4 -> Unsigned.UInt64.of_uint32 (Object.Buffer.Read.u32 cursor)
          | 8 -> Object.Buffer.Read.u64 cursor
          | _ -> failwith "Invalid address_size"
        in
        { segment; address })

  let parse buffer section_offset =
    let cursor =
      Object.Buffer.cursor buffer ~at:(Unsigned.UInt32.to_int section_offset)
    in
    let header = parse_header cursor in
    let entries = parse_entries cursor header in
    { header; entries }
end

let lookup_address_in_debug_addr (buffer : Object.Buffer.t) (_addr_base : u64)
    (index : int) : u64 option =
  (* Find the debug_addr section *)
  match find_debug_section buffer "__debug_addr" with
  | None -> None
  | Some (section_offset, _) -> (
      try
        (* addr_base is an offset from the beginning of the debug_addr section to the address data *)
        (* We need to parse the entire section, not start at addr_base *)
        let parsed_addr = DebugAddr.parse buffer section_offset in

        (* Check if index is within bounds *)
        if index >= 0 && index < Array.length parsed_addr.entries then
          Some parsed_addr.entries.(index).address
        else None
      with _ -> None)

let resolve_address_index (buffer : Object.Buffer.t) (index : int)
    (addr_base : u64) : u64 =
  (* Try to resolve address index using debug_addr section *)
  match lookup_address_in_debug_addr buffer addr_base index with
  | Some address -> address
  | None ->
      (* Fall back to returning the index value if resolution fails *)
      Unsigned.UInt64.of_int index
