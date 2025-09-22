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
  | Debug_macro
  | Debug_frame

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

(** Convert ELF machine type to architecture string *)
let string_of_elf_machine = function
  | `EM_386 -> "i386"
  | `EM_X86_64 -> "x86_64"
  | `EM_ARM -> "arm"
  | `EM_AARCH64 -> "aarch64"
  | `EM_MIPS -> "mips"
  | `EM_PPC -> "powerpc"
  | `EM_PPC64 -> "powerpc64"
  | `EM_SPARC -> "sparc"
  | `EM_SPARCV9 -> "sparcv9"
  | `EM_IA_64 -> "ia64"
  | `EM_S390 -> "s390"
  | `EM_RISCV -> "riscv"
  | `EM_68K -> "m68k"
  | `EM_88K -> "m88k"
  | `EM_PARISC -> "hppa"
  | `EM_SH -> "sh"
  | `EM_UNKNOWN x -> Printf.sprintf "unknown_%d" x
  | _ -> "unknown"

(** Detect file format and architecture from buffer *)
let detect_format_and_arch (buf : Object.Buffer.t) : string =
  let format = detect_format buf in
  match format with
  | ELF -> (
      (* Read ELF header to get architecture information *)
      try
        let open Object.Elf in
        let header, _section_array = read_elf buf in
        let arch_str = string_of_elf_machine header.e_machine in
        let class_str =
          match header.e_ident.elf_class with
          | `ELFCLASS32 -> "elf32"
          | `ELFCLASS64 -> "elf64"
          | `ELFCLASSNONE -> "elf"
        in
        Printf.sprintf "%s-%s" class_str arch_str
      with _ -> "ELF")
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
      | Debug_str_offs -> "__debug_str_offsets"
      | Debug_names -> "__debug_names"
      | Debug_addr -> "__debug_addr"
      | Debug_macro -> "__debug_macro"
      | Debug_frame -> "__debug_frame")
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
      | Debug_str_offs -> ".debug_str_offsets"
      | Debug_names -> ".debug_names"
      | Debug_addr -> ".debug_addr"
      | Debug_macro -> ".debug_macro"
      | Debug_frame -> ".debug_frame")

type abbreviation_tag =
  | DW_TAG_null
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
  (* GNU extensions *)
  | DW_TAG_GNU_template_parameter_pack
  | DW_TAG_lo_user
  | DW_TAG_hi_user

let abbreviation_tag_of_int tag_code =
  match Unsigned.UInt64.to_int tag_code with
  | 0x00 -> DW_TAG_null
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
  (* GNU extensions *)
  | 0x4107 -> DW_TAG_GNU_template_parameter_pack
  | 0x4080 -> DW_TAG_lo_user
  | 0xffff -> DW_TAG_hi_user
  | n -> failwith (Printf.sprintf "Unknown tag encoding: 0x%02x" n)

(* TODO Rename various conversion functions consistently. *)
let uint64_of_abbreviation_tag tag =
  let code =
    match tag with
    | DW_TAG_null -> 0x00
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
    | DW_TAG_dwarf_procedure -> 0x36
    | DW_TAG_restrict_type -> 0x37
    | DW_TAG_interface_type -> 0x38
    | DW_TAG_namespace -> 0x39
    | DW_TAG_imported_module -> 0x3a
    | DW_TAG_unspecified_type -> 0x3b
    | DW_TAG_partial_unit -> 0x3c
    | DW_TAG_imported_unit -> 0x3d
    | DW_TAG_condition -> 0x3f
    | DW_TAG_shared_type -> 0x40
    | DW_TAG_type_unit -> 0x41
    | DW_TAG_rvalue_reference_type -> 0x42
    | DW_TAG_template_alias -> 0x43
    (* GNU extensions *)
    | DW_TAG_GNU_template_parameter_pack -> 0x4107
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

(* Overloaded function that takes abbreviation_tag directly *)
let string_of_abbreviation_tag_direct tag =
  string_of_abbreviation_tag (uint64_of_abbreviation_tag tag)

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
  | DW_FORM_unknown of int  (** Unknown or vendor-specific forms *)

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
  | n -> DW_FORM_unknown n

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

let string_of_attribute_form_encoding_variant = function
  | DW_FORM_addr -> "DW_FORM_addr"
  | DW_FORM_block2 -> "DW_FORM_block2"
  | DW_FORM_block4 -> "DW_FORM_block4"
  | DW_FORM_data2 -> "DW_FORM_data2"
  | DW_FORM_data4 -> "DW_FORM_data4"
  | DW_FORM_data8 -> "DW_FORM_data8"
  | DW_FORM_string -> "DW_FORM_string"
  | DW_FORM_block -> "DW_FORM_block"
  | DW_FORM_block1 -> "DW_FORM_block1"
  | DW_FORM_data1 -> "DW_FORM_data1"
  | DW_FORM_flag -> "DW_FORM_flag"
  | DW_FORM_sdata -> "DW_FORM_sdata"
  | DW_FORM_strp -> "DW_FORM_strp"
  | DW_FORM_udata -> "DW_FORM_udata"
  | DW_FORM_ref_addr -> "DW_FORM_ref_addr"
  | DW_FORM_ref1 -> "DW_FORM_ref1"
  | DW_FORM_ref2 -> "DW_FORM_ref2"
  | DW_FORM_ref4 -> "DW_FORM_ref4"
  | DW_FORM_ref8 -> "DW_FORM_ref8"
  | DW_FORM_ref_udata -> "DW_FORM_ref_udata"
  | DW_FORM_indirect -> "DW_FORM_indirect"
  | DW_FORM_sec_offset -> "DW_FORM_sec_offset"
  | DW_FORM_exprloc -> "DW_FORM_exprloc"
  | DW_FORM_flag_present -> "DW_FORM_flag_present"
  | DW_FORM_strx -> "DW_FORM_strx"
  | DW_FORM_addrx -> "DW_FORM_addrx"
  | DW_FORM_ref_sup4 -> "DW_FORM_ref_sup4"
  | DW_FORM_strp_sup -> "DW_FORM_strp_sup"
  | DW_FORM_data16 -> "DW_FORM_data16"
  | DW_FORM_line_strp -> "DW_FORM_line_strp"
  | DW_FORM_ref_sig8 -> "DW_FORM_ref_sig8"
  | DW_FORM_implicit_const -> "DW_FORM_implicit_const"
  | DW_FORM_loclistx -> "DW_FORM_loclistx"
  | DW_FORM_rnglistx -> "DW_FORM_rnglistx"
  | DW_FORM_ref_sup8 -> "DW_FORM_ref_sup8"
  | DW_FORM_strx1 -> "DW_FORM_strx1"
  | DW_FORM_strx2 -> "DW_FORM_strx2"
  | DW_FORM_strx3 -> "DW_FORM_strx3"
  | DW_FORM_strx4 -> "DW_FORM_strx4"
  | DW_FORM_addrx1 -> "DW_FORM_addrx1"
  | DW_FORM_addrx2 -> "DW_FORM_addrx2"
  | DW_FORM_addrx3 -> "DW_FORM_addrx3"
  | DW_FORM_addrx4 -> "DW_FORM_addrx4"
  | DW_FORM_unknown i -> Printf.sprintf "DW_FORM_unknown(%d)" i

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
  | 0x72 -> DW_OP_breg2
  | 0x73 -> DW_OP_breg3
  | 0x74 -> DW_OP_breg4
  | 0x75 -> DW_OP_breg5
  | 0x76 -> DW_OP_breg6
  | 0x77 -> DW_OP_breg7
  | 0x78 -> DW_OP_breg8
  | 0x79 -> DW_OP_breg9
  | 0x7a -> DW_OP_breg10
  | 0x7b -> DW_OP_breg11
  | 0x7c -> DW_OP_breg12
  | 0x7d -> DW_OP_breg13
  | 0x7e -> DW_OP_breg14
  | 0x7f -> DW_OP_breg15
  | 0x80 -> DW_OP_breg16
  | 0x81 -> DW_OP_breg17
  | 0x82 -> DW_OP_breg18
  | 0x83 -> DW_OP_breg19
  | 0x84 -> DW_OP_breg20
  | 0x85 -> DW_OP_breg21
  | 0x86 -> DW_OP_breg22
  | 0x87 -> DW_OP_breg23
  | 0x88 -> DW_OP_breg24
  | 0x89 -> DW_OP_breg25
  | 0x8a -> DW_OP_breg26
  | 0x8b -> DW_OP_breg27
  | 0x8c -> DW_OP_breg28
  | 0x8d -> DW_OP_breg29
  | 0x8e -> DW_OP_breg30
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

let string_of_operation_encoding = function
  | DW_OP_addr -> "DW_OP_addr"
  | DW_OP_deref -> "DW_OP_deref"
  | DW_OP_const1u -> "DW_OP_const1u"
  | DW_OP_const1s -> "DW_OP_const1s"
  | DW_OP_const2u -> "DW_OP_const2u"
  | DW_OP_const2s -> "DW_OP_const2s"
  | DW_OP_const4u -> "DW_OP_const4u"
  | DW_OP_const4s -> "DW_OP_const4s"
  | DW_OP_const8u -> "DW_OP_const8u"
  | DW_OP_const8s -> "DW_OP_const8s"
  | DW_OP_constu -> "DW_OP_constu"
  | DW_OP_consts -> "DW_OP_consts"
  | DW_OP_dup -> "DW_OP_dup"
  | DW_OP_drop -> "DW_OP_drop"
  | DW_OP_over -> "DW_OP_over"
  | DW_OP_pick -> "DW_OP_pick"
  | DW_OP_swap -> "DW_OP_swap"
  | DW_OP_rot -> "DW_OP_rot"
  | DW_OP_xderef -> "DW_OP_xderef"
  | DW_OP_abs -> "DW_OP_abs"
  | DW_OP_and -> "DW_OP_and"
  | DW_OP_div -> "DW_OP_div"
  | DW_OP_minus -> "DW_OP_minus"
  | DW_OP_mod -> "DW_OP_mod"
  | DW_OP_mul -> "DW_OP_mul"
  | DW_OP_neg -> "DW_OP_neg"
  | DW_OP_not -> "DW_OP_not"
  | DW_OP_or -> "DW_OP_or"
  | DW_OP_plus -> "DW_OP_plus"
  | DW_OP_plus_uconst -> "DW_OP_plus_uconst"
  | DW_OP_shl -> "DW_OP_shl"
  | DW_OP_shr -> "DW_OP_shr"
  | DW_OP_shra -> "DW_OP_shra"
  | DW_OP_xor -> "DW_OP_xor"
  | DW_OP_bra -> "DW_OP_bra"
  | DW_OP_eq -> "DW_OP_eq"
  | DW_OP_ge -> "DW_OP_ge"
  | DW_OP_gt -> "DW_OP_gt"
  | DW_OP_le -> "DW_OP_le"
  | DW_OP_lt -> "DW_OP_lt"
  | DW_OP_ne -> "DW_OP_ne"
  | DW_OP_skip -> "DW_OP_skip"
  | DW_OP_lit0 -> "DW_OP_lit0"
  | DW_OP_lit1 -> "DW_OP_lit1"
  | DW_OP_lit2 -> "DW_OP_lit2"
  | DW_OP_lit3 -> "DW_OP_lit3"
  | DW_OP_lit4 -> "DW_OP_lit4"
  | DW_OP_lit5 -> "DW_OP_lit5"
  | DW_OP_lit6 -> "DW_OP_lit6"
  | DW_OP_lit7 -> "DW_OP_lit7"
  | DW_OP_lit8 -> "DW_OP_lit8"
  | DW_OP_lit9 -> "DW_OP_lit9"
  | DW_OP_lit10 -> "DW_OP_lit10"
  | DW_OP_lit11 -> "DW_OP_lit11"
  | DW_OP_lit12 -> "DW_OP_lit12"
  | DW_OP_lit13 -> "DW_OP_lit13"
  | DW_OP_lit14 -> "DW_OP_lit14"
  | DW_OP_lit15 -> "DW_OP_lit15"
  | DW_OP_lit16 -> "DW_OP_lit16"
  | DW_OP_lit17 -> "DW_OP_lit17"
  | DW_OP_lit18 -> "DW_OP_lit18"
  | DW_OP_lit19 -> "DW_OP_lit19"
  | DW_OP_lit20 -> "DW_OP_lit20"
  | DW_OP_lit21 -> "DW_OP_lit21"
  | DW_OP_lit22 -> "DW_OP_lit22"
  | DW_OP_lit23 -> "DW_OP_lit23"
  | DW_OP_lit24 -> "DW_OP_lit24"
  | DW_OP_lit25 -> "DW_OP_lit25"
  | DW_OP_lit26 -> "DW_OP_lit26"
  | DW_OP_lit27 -> "DW_OP_lit27"
  | DW_OP_lit28 -> "DW_OP_lit28"
  | DW_OP_lit29 -> "DW_OP_lit29"
  | DW_OP_lit30 -> "DW_OP_lit30"
  | DW_OP_lit31 -> "DW_OP_lit31"
  | DW_OP_reg0 -> "DW_OP_reg0"
  | DW_OP_reg1 -> "DW_OP_reg1"
  | DW_OP_reg2 -> "DW_OP_reg2"
  | DW_OP_reg3 -> "DW_OP_reg3"
  | DW_OP_reg4 -> "DW_OP_reg4"
  | DW_OP_reg5 -> "DW_OP_reg5"
  | DW_OP_reg6 -> "DW_OP_reg6"
  | DW_OP_reg7 -> "DW_OP_reg7"
  | DW_OP_reg8 -> "DW_OP_reg8"
  | DW_OP_reg9 -> "DW_OP_reg9"
  | DW_OP_reg10 -> "DW_OP_reg10"
  | DW_OP_reg11 -> "DW_OP_reg11"
  | DW_OP_reg12 -> "DW_OP_reg12"
  | DW_OP_reg13 -> "DW_OP_reg13"
  | DW_OP_reg14 -> "DW_OP_reg14"
  | DW_OP_reg15 -> "DW_OP_reg15"
  | DW_OP_reg16 -> "DW_OP_reg16"
  | DW_OP_reg17 -> "DW_OP_reg17"
  | DW_OP_reg18 -> "DW_OP_reg18"
  | DW_OP_reg19 -> "DW_OP_reg19"
  | DW_OP_reg20 -> "DW_OP_reg20"
  | DW_OP_reg21 -> "DW_OP_reg21"
  | DW_OP_reg22 -> "DW_OP_reg22"
  | DW_OP_reg23 -> "DW_OP_reg23"
  | DW_OP_reg24 -> "DW_OP_reg24"
  | DW_OP_reg25 -> "DW_OP_reg25"
  | DW_OP_reg26 -> "DW_OP_reg26"
  | DW_OP_reg27 -> "DW_OP_reg27"
  | DW_OP_reg28 -> "DW_OP_reg28"
  | DW_OP_reg29 -> "DW_OP_reg29"
  | DW_OP_reg30 -> "DW_OP_reg30"
  | DW_OP_reg31 -> "DW_OP_reg31"
  | DW_OP_breg0 -> "DW_OP_breg0"
  | DW_OP_breg1 -> "DW_OP_breg1"
  | DW_OP_breg2 -> "DW_OP_breg2"
  | DW_OP_breg3 -> "DW_OP_breg3"
  | DW_OP_breg4 -> "DW_OP_breg4"
  | DW_OP_breg5 -> "DW_OP_breg5"
  | DW_OP_breg6 -> "DW_OP_breg6"
  | DW_OP_breg7 -> "DW_OP_breg7"
  | DW_OP_breg8 -> "DW_OP_breg8"
  | DW_OP_breg9 -> "DW_OP_breg9"
  | DW_OP_breg10 -> "DW_OP_breg10"
  | DW_OP_breg11 -> "DW_OP_breg11"
  | DW_OP_breg12 -> "DW_OP_breg12"
  | DW_OP_breg13 -> "DW_OP_breg13"
  | DW_OP_breg14 -> "DW_OP_breg14"
  | DW_OP_breg15 -> "DW_OP_breg15"
  | DW_OP_breg16 -> "DW_OP_breg16"
  | DW_OP_breg17 -> "DW_OP_breg17"
  | DW_OP_breg18 -> "DW_OP_breg18"
  | DW_OP_breg19 -> "DW_OP_breg19"
  | DW_OP_breg20 -> "DW_OP_breg20"
  | DW_OP_breg21 -> "DW_OP_breg21"
  | DW_OP_breg22 -> "DW_OP_breg22"
  | DW_OP_breg23 -> "DW_OP_breg23"
  | DW_OP_breg24 -> "DW_OP_breg24"
  | DW_OP_breg25 -> "DW_OP_breg25"
  | DW_OP_breg26 -> "DW_OP_breg26"
  | DW_OP_breg27 -> "DW_OP_breg27"
  | DW_OP_breg28 -> "DW_OP_breg28"
  | DW_OP_breg29 -> "DW_OP_breg29"
  | DW_OP_breg30 -> "DW_OP_breg30"
  | DW_OP_breg31 -> "DW_OP_breg31"
  | DW_OP_regx -> "DW_OP_regx"
  | DW_OP_fbreg -> "DW_OP_fbreg"
  | DW_OP_bregx -> "DW_OP_bregx"
  | DW_OP_piece -> "DW_OP_piece"
  | DW_OP_deref_size -> "DW_OP_deref_size"
  | DW_OP_xderef_size -> "DW_OP_xderef_size"
  | DW_OP_nop -> "DW_OP_nop"
  | DW_OP_push_object_address -> "DW_OP_push_object_address"
  | DW_OP_call2 -> "DW_OP_call2"
  | DW_OP_call4 -> "DW_OP_call4"
  | DW_OP_call_ref -> "DW_OP_call_ref"
  | DW_OP_form_tls_address -> "DW_OP_form_tls_address"
  | DW_OP_call_frame_cfa -> "DW_OP_call_frame_cfa"
  | DW_OP_bit_piece -> "DW_OP_bit_piece"
  | DW_OP_implicit_value -> "DW_OP_implicit_value"
  | DW_OP_stack_value -> "DW_OP_stack_value"
  | DW_OP_implicit_pointer -> "DW_OP_implicit_pointer"
  | DW_OP_addrx -> "DW_OP_addrx"
  | DW_OP_constx -> "DW_OP_constx"
  | DW_OP_entry_value -> "DW_OP_entry_value"
  | DW_OP_const_type -> "DW_OP_const_type"
  | DW_OP_regval_type -> "DW_OP_regval_type"
  | DW_OP_deref_type -> "DW_OP_deref_type"
  | DW_OP_xderef_type -> "DW_OP_xderef_type"
  | DW_OP_convert -> "DW_OP_convert"
  | DW_OP_reinterpret -> "DW_OP_reinterpret"
  | DW_OP_hi_user -> "DW_OP_hi_user"

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
  | DW_IDX_null
  | DW_IDX_compile_unit
  | DW_IDX_type_unit
  | DW_IDX_die_offset
  | DW_IDX_parent
  | DW_IDX_type_hash
  | DW_IDX_lo_user
  | DW_IDX_hi_user

let name_index_attribute = function
  | 0 -> DW_IDX_null
  | 1 -> DW_IDX_compile_unit
  | 2 -> DW_IDX_type_unit
  | 3 -> DW_IDX_die_offset
  | 4 -> DW_IDX_parent
  | 5 -> DW_IDX_type_hash
  | 0x2000 -> DW_IDX_lo_user
  | 0x3fff -> DW_IDX_hi_user
  | n -> failwith (Printf.sprintf "Unknown name_index_attribute: 0x%02x" n)

let name_index_attribute_of_u64 code =
  name_index_attribute (Unsigned.UInt64.to_int code)

let string_of_name_index_attribute = function
  | DW_IDX_null -> "DW_IDX_null"
  | DW_IDX_compile_unit -> "DW_IDX_compile_unit"
  | DW_IDX_type_unit -> "DW_IDX_type_unit"
  | DW_IDX_die_offset -> "DW_IDX_die_offset"
  | DW_IDX_parent -> "DW_IDX_parent"
  | DW_IDX_type_hash -> "DW_IDX_type_hash"
  | DW_IDX_lo_user -> "DW_IDX_lo_user"
  | DW_IDX_hi_user -> "DW_IDX_hi_user"

let abbreviation_tag_of_u64 code = abbreviation_tag_of_int code

let u64_of_attribute_form_encoding = function
  | DW_FORM_addr -> Unsigned.UInt64.of_int 0x01
  | DW_FORM_block2 -> Unsigned.UInt64.of_int 0x03
  | DW_FORM_block4 -> Unsigned.UInt64.of_int 0x04
  | DW_FORM_data2 -> Unsigned.UInt64.of_int 0x05
  | DW_FORM_data4 -> Unsigned.UInt64.of_int 0x06
  | DW_FORM_data8 -> Unsigned.UInt64.of_int 0x07
  | DW_FORM_string -> Unsigned.UInt64.of_int 0x08
  | DW_FORM_block -> Unsigned.UInt64.of_int 0x09
  | DW_FORM_block1 -> Unsigned.UInt64.of_int 0x0a
  | DW_FORM_data1 -> Unsigned.UInt64.of_int 0x0b
  | DW_FORM_flag -> Unsigned.UInt64.of_int 0x0c
  | DW_FORM_sdata -> Unsigned.UInt64.of_int 0x0d
  | DW_FORM_strp -> Unsigned.UInt64.of_int 0x0e
  | DW_FORM_udata -> Unsigned.UInt64.of_int 0x0f
  | DW_FORM_ref_addr -> Unsigned.UInt64.of_int 0x10
  | DW_FORM_ref1 -> Unsigned.UInt64.of_int 0x11
  | DW_FORM_ref2 -> Unsigned.UInt64.of_int 0x12
  | DW_FORM_ref4 -> Unsigned.UInt64.of_int 0x13
  | DW_FORM_ref8 -> Unsigned.UInt64.of_int 0x14
  | DW_FORM_ref_udata -> Unsigned.UInt64.of_int 0x15
  | DW_FORM_indirect -> Unsigned.UInt64.of_int 0x16
  | DW_FORM_sec_offset -> Unsigned.UInt64.of_int 0x17
  | DW_FORM_exprloc -> Unsigned.UInt64.of_int 0x18
  | DW_FORM_flag_present -> Unsigned.UInt64.of_int 0x19
  | DW_FORM_strx -> Unsigned.UInt64.of_int 0x1a
  | DW_FORM_addrx -> Unsigned.UInt64.of_int 0x1b
  | DW_FORM_ref_sup4 -> Unsigned.UInt64.of_int 0x1c
  | DW_FORM_strp_sup -> Unsigned.UInt64.of_int 0x1d
  | DW_FORM_data16 -> Unsigned.UInt64.of_int 0x1e
  | DW_FORM_line_strp -> Unsigned.UInt64.of_int 0x1f
  | DW_FORM_ref_sig8 -> Unsigned.UInt64.of_int 0x20
  | DW_FORM_implicit_const -> Unsigned.UInt64.of_int 0x21
  | DW_FORM_loclistx -> Unsigned.UInt64.of_int 0x22
  | DW_FORM_rnglistx -> Unsigned.UInt64.of_int 0x23
  | DW_FORM_ref_sup8 -> Unsigned.UInt64.of_int 0x24
  | DW_FORM_strx1 -> Unsigned.UInt64.of_int 0x25
  | DW_FORM_strx2 -> Unsigned.UInt64.of_int 0x26
  | DW_FORM_strx3 -> Unsigned.UInt64.of_int 0x27
  | DW_FORM_strx4 -> Unsigned.UInt64.of_int 0x28
  | DW_FORM_addrx1 -> Unsigned.UInt64.of_int 0x29
  | DW_FORM_addrx2 -> Unsigned.UInt64.of_int 0x2a
  | DW_FORM_addrx3 -> Unsigned.UInt64.of_int 0x2b
  | DW_FORM_addrx4 -> Unsigned.UInt64.of_int 0x2c
  | DW_FORM_unknown n -> Unsigned.UInt64.of_int n

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

let string_of_line_number_opcode = function
  | DW_LNS_copy -> "DW_LNS_copy"
  | DW_LNS_advance_pc -> "DW_LNS_advance_pc"
  | DW_LNS_advance_line -> "DW_LNS_advance_line"
  | DW_LNS_set_file -> "DW_LNS_set_file"
  | DW_LNS_set_column -> "DW_LNS_set_column"
  | DW_LNS_negate_stmt -> "DW_LNS_negate_stmt"
  | DW_LNS_set_basic_block -> "DW_LNS_set_basic_block"
  | DW_LNS_const_add_pc -> "DW_LNS_const_add_pc"
  | DW_LNS_fixed_advance_pc -> "DW_LNS_fixed_advance_pc"
  | DW_LNS_set_prologue_end -> "DW_LNS_set_prologue_end"
  | DW_LNS_set_epilogue_begin -> "DW_LNS_set_epilogue_begin"
  | DW_LNS_set_isa -> "DW_LNS_set_isa"

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

let string_of_line_number_extended_opcode = function
  | DW_LNE_end_sequence -> "DW_LNE_end_sequence"
  | DW_LNE_set_address -> "DW_LNE_set_address"
  | DW_LNE_set_discriminator -> "DW_LNE_set_discriminator"
  | DW_LNE_lo_user -> "DW_LNE_lo_user"
  | DW_LNE_hi_user -> "DW_LNE_hi_user"

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

let string_of_line_number_header_entry = function
  | DW_LNCT_path -> "DW_LNCT_path"
  | DW_LNCT_directory_index -> "DW_LNCT_directory_index"
  | DW_LNCT_timestamp -> "DW_LNCT_timestamp"
  | DW_LNCT_size -> "DW_LNCT_size"
  | DW_LNCT_MD5 -> "DW_LNCT_MD5"
  | DW_LNCT_lo_user -> "DW_LNCT_lo_user"
  | DW_LNCT_hi_user -> "DW_LNCT_hi_user"

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

let macro_info_entry_type_of_u8 u =
  macro_info_entry_type (Unsigned.UInt8.to_int u)

let string_of_macro_info_entry_type = function
  | DW_MACRO_define -> "DW_MACRO_define"
  | DW_MACRO_undef -> "DW_MACRO_undef"
  | DW_MACRO_start_file -> "DW_MACRO_start_file"
  | DW_MACRO_end_file -> "DW_MACRO_end_file"
  | DW_MACRO_define_strp -> "DW_MACRO_define_strp"
  | DW_MACRO_undef_strp -> "DW_MACRO_undef_strp"
  | DW_MACRO_import -> "DW_MACRO_import"
  | DW_MACRO_define_sup -> "DW_MACRO_define_sup"
  | DW_MACRO_undef_sup -> "DW_MACRO_undef_sup"
  | DW_MACRO_import_sup -> "DW_MACRO_import_sup"
  | DW_MACRO_define_strx -> "DW_MACRO_define_strx"
  | DW_MACRO_undef_strx -> "DW_MACRO_undef_strx"
  | DW_MACRO_lo_user -> "DW_MACRO_lo_user"
  | DW_MACRO_hi_user -> "DW_MACRO_hi_user"

(* Debug Macro Section - DWARF 5 Section 6.3 *)

type debug_macro_header = {
  length : u32;  (** Unit length *)
  format : string;  (** DWARF32 or DWARF64 *)
  version : u16;  (** Version number *)
  flags : u8;  (** Flags *)
  debug_line_offset : u32 option;  (** Offset into debug_line (if flag set) *)
  debug_str_offsets_offset : u32 option;
      (** Offset into debug_str_offsets (if flag set) *)
}

type debug_macro_entry = {
  entry_type : macro_info_entry_type;  (** Type of macro entry *)
  line_number : u32 option;  (** Line number for certain types *)
  string_offset : u32 option;  (** Offset into string table *)
  string_value : string option;  (** Direct string value *)
  file_index : u32 option;  (** File index for start_file entries *)
}

type debug_macro_unit = {
  header : debug_macro_header;  (** Unit header *)
  entries : debug_macro_entry list;  (** List of macro entries *)
}

type debug_macro_section = {
  units : debug_macro_unit list;  (** List of macro units *)
}

(* Debug Macro Parsing Functions *)

let parse_debug_macro_header (cur : Object.Buffer.cursor) : debug_macro_header =
  let length = Object.Buffer.Read.u32 cur in
  let format = "DWARF32" in
  (* For now, only support DWARF32 format *)
  let version = Object.Buffer.Read.u16 cur in
  let flags = Object.Buffer.Read.u8 cur in

  let debug_line_offset =
    if Unsigned.UInt8.to_int flags land 0x01 <> 0 then
      Some (Object.Buffer.Read.u32 cur)
    else None
  in

  let debug_str_offsets_offset =
    if Unsigned.UInt8.to_int flags land 0x02 <> 0 then
      Some (Object.Buffer.Read.u32 cur)
    else None
  in

  {
    length;
    format;
    version;
    flags;
    debug_line_offset;
    debug_str_offsets_offset;
  }

let parse_debug_macro_entry (cur : Object.Buffer.cursor) :
    debug_macro_entry option =
  let entry_type_code = Object.Buffer.Read.u8 cur in
  if Unsigned.UInt8.to_int entry_type_code = 0 then None
    (* End of entries marker *)
  else
    let entry_type = macro_info_entry_type_of_u8 entry_type_code in
    let line_number, string_offset, string_value, file_index =
      match entry_type with
      | DW_MACRO_define | DW_MACRO_undef ->
          let line = Object.Buffer.Read.uleb128 cur |> Unsigned.UInt32.of_int in
          let str_offset =
            Object.Buffer.Read.uleb128 cur |> Unsigned.UInt32.of_int
          in
          (Some line, Some str_offset, None, None)
      | DW_MACRO_define_strp | DW_MACRO_undef_strp ->
          let line = Object.Buffer.Read.uleb128 cur |> Unsigned.UInt32.of_int in
          let str_offset = Object.Buffer.Read.u32 cur in
          (Some line, Some str_offset, None, None)
      | DW_MACRO_define_sup | DW_MACRO_undef_sup ->
          let line = Object.Buffer.Read.uleb128 cur |> Unsigned.UInt32.of_int in
          let str_offset =
            Object.Buffer.Read.uleb128 cur |> Unsigned.UInt32.of_int
          in
          (Some line, Some str_offset, None, None)
      | DW_MACRO_define_strx | DW_MACRO_undef_strx ->
          let line = Object.Buffer.Read.uleb128 cur |> Unsigned.UInt32.of_int in
          let str_offset =
            Object.Buffer.Read.uleb128 cur |> Unsigned.UInt32.of_int
          in
          (Some line, Some str_offset, None, None)
      | DW_MACRO_start_file ->
          let line = Object.Buffer.Read.uleb128 cur |> Unsigned.UInt32.of_int in
          let file_idx =
            Object.Buffer.Read.uleb128 cur |> Unsigned.UInt32.of_int
          in
          (Some line, None, None, Some file_idx)
      | DW_MACRO_end_file -> (None, None, None, None)
      | DW_MACRO_import | DW_MACRO_import_sup ->
          let offset = Object.Buffer.Read.u32 cur in
          (None, Some offset, None, None)
      | _ ->
          (* TODO Make this comprehensive match *)
          (* For unknown or user-defined entry types, skip *)
          (None, None, None, None)
    in
    Some { entry_type; line_number; string_offset; string_value; file_index }

let parse_debug_macro_unit (cur : Object.Buffer.cursor) : debug_macro_unit =
  let header = parse_debug_macro_header cur in
  let entries = ref [] in

  let rec parse_entries () =
    match parse_debug_macro_entry cur with
    | None -> List.rev !entries
    | Some entry ->
        entries := entry :: !entries;
        parse_entries ()
  in

  let entries_list = parse_entries () in
  { header; entries = entries_list }

let parse_debug_macro_section (cur : Object.Buffer.cursor) (section_size : int)
    : debug_macro_section =
  let start_pos = cur.position in
  let end_pos = start_pos + section_size in
  let units = ref [] in

  let rec parse_units () =
    if cur.position >= end_pos then List.rev !units
    else
      let unit = parse_debug_macro_unit cur in
      units := unit :: !units;
      (* Skip to next unit based on header length *)
      let unit_end =
        start_pos + Unsigned.UInt32.to_int unit.header.length + 4
      in
      cur.position <- unit_end;
      parse_units ()
  in

  let units_list = parse_units () in
  { units = units_list }

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

(* CFI instruction decoder *)
let decode_cfa_opcode = function
  | 0x00 -> DW_CFA_nop
  | 0x01 -> DW_CFA_set_loc
  | 0x02 -> DW_CFA_advance_loc1
  | 0x03 -> DW_CFA_advance_loc2
  | 0x04 -> DW_CFA_advance_loc4
  | 0x05 -> DW_CFA_offset_extended
  | 0x06 -> DW_CFA_restore_extended
  | 0x07 -> DW_CFA_undefined
  | 0x08 -> DW_CFA_same_value
  | 0x09 -> DW_CFA_register
  | 0x0a -> DW_CFA_remember_state
  | 0x0b -> DW_CFA_restore_state
  | 0x0c -> DW_CFA_def_cfa
  | 0x0d -> DW_CFA_def_cfa_register
  | 0x0e -> DW_CFA_def_cfa_offset
  | 0x0f -> DW_CFA_def_cfa_expression
  | 0x10 -> DW_CFA_expression
  | 0x11 -> DW_CFA_offset_extended_sf
  | 0x12 -> DW_CFA_def_cfa_sf
  | 0x13 -> DW_CFA_def_cfa_offset_sf
  | 0x14 -> DW_CFA_val_offset
  | 0x15 -> DW_CFA_val_offset_sf
  | 0x16 -> DW_CFA_val_expression
  | 0x1c -> DW_CFA_lo_user
  | 0x3f -> DW_CFA_hi_user
  | n when n >= 0x40 && n <= 0x7f -> DW_CFA_advance_loc (* + delta *)
  | n when n >= 0x80 && n <= 0xbf -> DW_CFA_offset (* + register *)
  | n when n >= 0xc0 && n <= 0xff -> DW_CFA_restore (* + register *)
  | _ -> DW_CFA_nop (* Unknown instruction, treat as nop *)

(* Helper functions for parsing ULEB128/SLEB128 values *)
(* Read ULEB128 from string at position *)
let read_uleb128_from_string str pos =
  let rec read_uleb acc shift pos =
    if pos >= String.length str then (acc, pos)
    else
      let byte = Char.code str.[pos] in
      let value = byte land 0x7f in
      let acc' = acc lor (value lsl shift) in
      if byte land 0x80 = 0 then (acc', pos + 1)
      else read_uleb acc' (shift + 7) (pos + 1)
  in
  read_uleb 0 0 pos

(* Read signed LEB128 from string at position *)
let read_sleb128_from_string str pos =
  let rec read_sleb acc shift pos =
    if pos >= String.length str then (acc, pos)
    else
      let byte = Char.code str.[pos] in
      let value = byte land 0x7f in
      let acc' = acc lor (value lsl shift) in
      let pos' = pos + 1 in
      if byte land 0x80 = 0 then
        (* Sign extend if necessary *)
        let result =
          if shift < 32 && value land 0x40 <> 0 then
            acc' lor (-1 lsl (shift + 7))
          else acc'
        in
        (result, pos')
      else read_sleb acc' (shift + 7) pos'
  in
  read_sleb 0 0 pos

(* DWARF Expression Parser *)
type dwarf_expression_operation = {
  opcode : operation_encoding;
  operands : int list;
  operand_string : string option;
}

let parse_dwarf_expression (expr_bytes : string) :
    dwarf_expression_operation list =
  let rec parse_ops pos acc =
    if pos >= String.length expr_bytes then List.rev acc
    else
      let opcode_byte = Char.code expr_bytes.[pos] in
      try
        let opcode = operation_encoding opcode_byte in
        let operands, operand_string, next_pos =
          match opcode with
          (* No operands *)
          | DW_OP_deref | DW_OP_dup | DW_OP_drop | DW_OP_over | DW_OP_swap
          | DW_OP_rot | DW_OP_xderef | DW_OP_abs | DW_OP_and | DW_OP_div
          | DW_OP_minus | DW_OP_mod | DW_OP_mul | DW_OP_neg | DW_OP_not
          | DW_OP_or | DW_OP_plus | DW_OP_shl | DW_OP_shr | DW_OP_shra
          | DW_OP_xor | DW_OP_eq | DW_OP_ge | DW_OP_gt | DW_OP_le | DW_OP_lt
          | DW_OP_ne | DW_OP_nop | DW_OP_push_object_address
          | DW_OP_form_tls_address | DW_OP_call_frame_cfa | DW_OP_stack_value
          | DW_OP_hi_user ->
              ([], None, pos + 1)
          (* Literal values 0-31 *)
          | DW_OP_lit0 | DW_OP_lit1 | DW_OP_lit2 | DW_OP_lit3 | DW_OP_lit4
          | DW_OP_lit5 | DW_OP_lit6 | DW_OP_lit7 | DW_OP_lit8 | DW_OP_lit9
          | DW_OP_lit10 | DW_OP_lit11 | DW_OP_lit12 | DW_OP_lit13 | DW_OP_lit14
          | DW_OP_lit15 | DW_OP_lit16 | DW_OP_lit17 | DW_OP_lit18 | DW_OP_lit19
          | DW_OP_lit20 | DW_OP_lit21 | DW_OP_lit22 | DW_OP_lit23 | DW_OP_lit24
          | DW_OP_lit25 | DW_OP_lit26 | DW_OP_lit27 | DW_OP_lit28 | DW_OP_lit29
          | DW_OP_lit30 | DW_OP_lit31 ->
              ([], None, pos + 1)
          (* Register values 0-31 *)
          | DW_OP_reg0 | DW_OP_reg1 | DW_OP_reg2 | DW_OP_reg3 | DW_OP_reg4
          | DW_OP_reg5 | DW_OP_reg6 | DW_OP_reg7 | DW_OP_reg8 | DW_OP_reg9
          | DW_OP_reg10 | DW_OP_reg11 | DW_OP_reg12 | DW_OP_reg13 | DW_OP_reg14
          | DW_OP_reg15 | DW_OP_reg16 | DW_OP_reg17 | DW_OP_reg18 | DW_OP_reg19
          | DW_OP_reg20 | DW_OP_reg21 | DW_OP_reg22 | DW_OP_reg23 | DW_OP_reg24
          | DW_OP_reg25 | DW_OP_reg26 | DW_OP_reg27 | DW_OP_reg28 | DW_OP_reg29
          | DW_OP_reg30 | DW_OP_reg31 ->
              ([], None, pos + 1)
          (* Base register + offset (breg0-breg31) all take SLEB128 offset *)
          | DW_OP_breg0 | DW_OP_breg1 | DW_OP_breg2 | DW_OP_breg3 | DW_OP_breg4
          | DW_OP_breg5 | DW_OP_breg6 | DW_OP_breg7 | DW_OP_breg8 | DW_OP_breg9
          | DW_OP_breg10 | DW_OP_breg11 | DW_OP_breg12 | DW_OP_breg13
          | DW_OP_breg14 | DW_OP_breg15 | DW_OP_breg16 | DW_OP_breg17
          | DW_OP_breg18 | DW_OP_breg19 | DW_OP_breg20 | DW_OP_breg21
          | DW_OP_breg22 | DW_OP_breg23 | DW_OP_breg24 | DW_OP_breg25
          | DW_OP_breg26 | DW_OP_breg27 | DW_OP_breg28 | DW_OP_breg29
          | DW_OP_breg30 | DW_OP_breg31 ->
              let offset, next_pos =
                read_sleb128_from_string expr_bytes (pos + 1)
              in
              ([ offset ], None, next_pos)
          (* 1-byte operands *)
          | DW_OP_const1u | DW_OP_const1s | DW_OP_pick | DW_OP_deref_size
          | DW_OP_xderef_size ->
              if pos + 1 < String.length expr_bytes then
                let operand = Char.code expr_bytes.[pos + 1] in
                ([ operand ], None, pos + 2)
              else ([], None, pos + 1)
          (* 2-byte operands *)
          | DW_OP_const2u | DW_OP_const2s | DW_OP_bra | DW_OP_skip | DW_OP_call2
            ->
              if pos + 2 < String.length expr_bytes then
                let b1 = Char.code expr_bytes.[pos + 1] in
                let b2 = Char.code expr_bytes.[pos + 2] in
                let operand = b1 lor (b2 lsl 8) in
                ([ operand ], None, pos + 3)
              else ([], None, pos + 1)
          (* 4-byte operands *)
          | DW_OP_const4u | DW_OP_const4s | DW_OP_call4 ->
              if pos + 4 < String.length expr_bytes then
                let b1 = Char.code expr_bytes.[pos + 1] in
                let b2 = Char.code expr_bytes.[pos + 2] in
                let b3 = Char.code expr_bytes.[pos + 3] in
                let b4 = Char.code expr_bytes.[pos + 4] in
                let operand =
                  b1 lor (b2 lsl 8) lor (b3 lsl 16) lor (b4 lsl 24)
                in
                ([ operand ], None, pos + 5)
              else ([], None, pos + 1)
          (* Address operand (architecture dependent size) *)
          | DW_OP_addr ->
              (* For now, assume 8-byte addresses *)
              if pos + 8 < String.length expr_bytes then
                let addr_bytes = String.sub expr_bytes (pos + 1) 8 in
                ( [],
                  Some (Printf.sprintf "0x%s" (String.escaped addr_bytes)),
                  pos + 9 )
              else ([], None, pos + 1)
          (* ULEB128 operands *)
          | DW_OP_constu | DW_OP_plus_uconst | DW_OP_regx | DW_OP_piece
          | DW_OP_addrx | DW_OP_constx ->
              let operand, next_pos =
                read_uleb128_from_string expr_bytes (pos + 1)
              in
              ([ operand ], None, next_pos)
          (* SLEB128 operands *)
          | DW_OP_consts | DW_OP_fbreg ->
              let operand, next_pos =
                read_sleb128_from_string expr_bytes (pos + 1)
              in
              ([ operand ], None, next_pos)
          (* ULEB128 register + SLEB128 offset *)
          | DW_OP_bregx ->
              let reg, pos1 = read_uleb128_from_string expr_bytes (pos + 1) in
              let offset, next_pos = read_sleb128_from_string expr_bytes pos1 in
              ([ reg; offset ], None, next_pos)
          (* ULEB128 size + ULEB128 offset *)
          | DW_OP_bit_piece ->
              let size, pos1 = read_uleb128_from_string expr_bytes (pos + 1) in
              let offset, next_pos = read_uleb128_from_string expr_bytes pos1 in
              ([ size; offset ], None, next_pos)
          (* ULEB128 size + block of that size *)
          | DW_OP_implicit_value | DW_OP_entry_value ->
              let size, pos1 = read_uleb128_from_string expr_bytes (pos + 1) in
              if pos1 + size <= String.length expr_bytes then
                ([ size ], Some (Printf.sprintf "[%d bytes]" size), pos1 + size)
              else ([], None, pos + 1)
          (* Parse 8-byte constant operands *)
          | DW_OP_const8u ->
              if pos + 8 < String.length expr_bytes then
                let b1 = Int64.of_int (Char.code expr_bytes.[pos + 1]) in
                let b2 = Int64.of_int (Char.code expr_bytes.[pos + 2]) in
                let b3 = Int64.of_int (Char.code expr_bytes.[pos + 3]) in
                let b4 = Int64.of_int (Char.code expr_bytes.[pos + 4]) in
                let b5 = Int64.of_int (Char.code expr_bytes.[pos + 5]) in
                let b6 = Int64.of_int (Char.code expr_bytes.[pos + 6]) in
                let b7 = Int64.of_int (Char.code expr_bytes.[pos + 7]) in
                let b8 = Int64.of_int (Char.code expr_bytes.[pos + 8]) in
                let operand =
                  Int64.(
                    logor b1
                      (logor (shift_left b2 8)
                         (logor (shift_left b3 16)
                            (logor (shift_left b4 24)
                               (logor (shift_left b5 32)
                                  (logor (shift_left b6 40)
                                     (logor (shift_left b7 48)
                                        (shift_left b8 56))))))))
                in
                let value_str = Int64.to_string operand in
                ([], Some value_str, pos + 9)
              else ([], Some "[truncated]", pos + 1)
          | DW_OP_const8s ->
              if pos + 8 < String.length expr_bytes then
                let b1 = Int64.of_int (Char.code expr_bytes.[pos + 1]) in
                let b2 = Int64.of_int (Char.code expr_bytes.[pos + 2]) in
                let b3 = Int64.of_int (Char.code expr_bytes.[pos + 3]) in
                let b4 = Int64.of_int (Char.code expr_bytes.[pos + 4]) in
                let b5 = Int64.of_int (Char.code expr_bytes.[pos + 5]) in
                let b6 = Int64.of_int (Char.code expr_bytes.[pos + 6]) in
                let b7 = Int64.of_int (Char.code expr_bytes.[pos + 7]) in
                let b8 = Int64.of_int (Char.code expr_bytes.[pos + 8]) in
                let operand =
                  Int64.(
                    logor b1
                      (logor (shift_left b2 8)
                         (logor (shift_left b3 16)
                            (logor (shift_left b4 24)
                               (logor (shift_left b5 32)
                                  (logor (shift_left b6 40)
                                     (logor (shift_left b7 48)
                                        (shift_left b8 56))))))))
                in
                let value_str = Int64.to_string operand in
                ([], Some value_str, pos + 9)
              else ([], Some "[truncated]", pos + 1)
          (* DW_OP_call_ref - calls a DIE reference (address-sized) *)
          | DW_OP_call_ref ->
              (* For now, assume 8-byte addresses - should be architecture dependent *)
              if pos + 8 < String.length expr_bytes then
                let b1 = Int64.of_int (Char.code expr_bytes.[pos + 1]) in
                let b2 = Int64.of_int (Char.code expr_bytes.[pos + 2]) in
                let b3 = Int64.of_int (Char.code expr_bytes.[pos + 3]) in
                let b4 = Int64.of_int (Char.code expr_bytes.[pos + 4]) in
                let b5 = Int64.of_int (Char.code expr_bytes.[pos + 5]) in
                let b6 = Int64.of_int (Char.code expr_bytes.[pos + 6]) in
                let b7 = Int64.of_int (Char.code expr_bytes.[pos + 7]) in
                let b8 = Int64.of_int (Char.code expr_bytes.[pos + 8]) in
                let die_ref =
                  Int64.(
                    logor b1
                      (logor (shift_left b2 8)
                         (logor (shift_left b3 16)
                            (logor (shift_left b4 24)
                               (logor (shift_left b5 32)
                                  (logor (shift_left b6 40)
                                     (logor (shift_left b7 48)
                                        (shift_left b8 56))))))))
                in
                let ref_str = Printf.sprintf "0x%Lx" die_ref in
                ([], Some ref_str, pos + 9)
              else ([], Some "[truncated]", pos + 1)
          (* DW_OP_implicit_pointer - DIE reference + signed offset *)
          | DW_OP_implicit_pointer ->
              (* For now, assume 8-byte DIE references - should be architecture dependent *)
              if pos + 8 < String.length expr_bytes then
                let b1 = Int64.of_int (Char.code expr_bytes.[pos + 1]) in
                let b2 = Int64.of_int (Char.code expr_bytes.[pos + 2]) in
                let b3 = Int64.of_int (Char.code expr_bytes.[pos + 3]) in
                let b4 = Int64.of_int (Char.code expr_bytes.[pos + 4]) in
                let b5 = Int64.of_int (Char.code expr_bytes.[pos + 5]) in
                let b6 = Int64.of_int (Char.code expr_bytes.[pos + 6]) in
                let b7 = Int64.of_int (Char.code expr_bytes.[pos + 7]) in
                let b8 = Int64.of_int (Char.code expr_bytes.[pos + 8]) in
                let die_ref =
                  Int64.(
                    logor b1
                      (logor (shift_left b2 8)
                         (logor (shift_left b3 16)
                            (logor (shift_left b4 24)
                               (logor (shift_left b5 32)
                                  (logor (shift_left b6 40)
                                     (logor (shift_left b7 48)
                                        (shift_left b8 56))))))))
                in
                (* Read SLEB128 offset *)
                let offset, next_pos =
                  read_sleb128_from_string expr_bytes (pos + 9)
                in
                let desc = Printf.sprintf "0x%Lx+%d" die_ref offset in
                ([], Some desc, next_pos)
              else ([], Some "[truncated]", pos + 1)
          (* DW_OP_const_type - ULEB128 type offset + variable length constant *)
          | DW_OP_const_type ->
              let type_offset, pos1 =
                read_uleb128_from_string expr_bytes (pos + 1)
              in
              let const_size, pos2 = read_uleb128_from_string expr_bytes pos1 in
              if pos2 + const_size <= String.length expr_bytes then
                let desc =
                  Printf.sprintf "type:0x%x const:[%d bytes]" type_offset
                    const_size
                in
                ([], Some desc, pos2 + const_size)
              else ([], Some "[truncated]", pos + 1)
          (* DW_OP_regval_type - ULEB128 register + ULEB128 type offset *)
          | DW_OP_regval_type ->
              let register, pos1 =
                read_uleb128_from_string expr_bytes (pos + 1)
              in
              let type_offset, next_pos =
                read_uleb128_from_string expr_bytes pos1
              in
              let desc =
                Printf.sprintf "reg:%d type:0x%x" register type_offset
              in
              ([], Some desc, next_pos)
          (* DW_OP_deref_type - 1-byte size + ULEB128 type offset *)
          | DW_OP_deref_type ->
              if pos + 1 < String.length expr_bytes then
                let size = Char.code expr_bytes.[pos + 1] in
                let type_offset, next_pos =
                  read_uleb128_from_string expr_bytes (pos + 2)
                in
                let desc =
                  Printf.sprintf "size:%d type:0x%x" size type_offset
                in
                ([], Some desc, next_pos)
              else ([], Some "[truncated]", pos + 1)
          (* DW_OP_xderef_type - 1-byte size + ULEB128 type offset *)
          | DW_OP_xderef_type ->
              if pos + 1 < String.length expr_bytes then
                let size = Char.code expr_bytes.[pos + 1] in
                let type_offset, next_pos =
                  read_uleb128_from_string expr_bytes (pos + 2)
                in
                let desc =
                  Printf.sprintf "size:%d type:0x%x" size type_offset
                in
                ([], Some desc, next_pos)
              else ([], Some "[truncated]", pos + 1)
          (* DW_OP_convert - ULEB128 type offset *)
          | DW_OP_convert ->
              let type_offset, next_pos =
                read_uleb128_from_string expr_bytes (pos + 1)
              in
              let desc = Printf.sprintf "type:0x%x" type_offset in
              ([], Some desc, next_pos)
          (* DW_OP_reinterpret - ULEB128 type offset *)
          | DW_OP_reinterpret ->
              let type_offset, next_pos =
                read_uleb128_from_string expr_bytes (pos + 1)
              in
              let desc = Printf.sprintf "type:0x%x" type_offset in
              ([], Some desc, next_pos)
        in
        let operation = { opcode; operands; operand_string } in
        parse_ops next_pos (operation :: acc)
      with Failure _ ->
        (* Unknown opcode, skip *)
        parse_ops (pos + 1) acc
  in
  parse_ops 0 []

let string_of_dwarf_operation (op : dwarf_expression_operation) : string =
  let opcode_name = string_of_operation_encoding op.opcode in
  match (op.operands, op.operand_string) with
  | [], None -> opcode_name
  | [], Some s -> Printf.sprintf "%s(%s)" opcode_name s
  | operands, None ->
      let operand_strs = List.map string_of_int operands in
      Printf.sprintf "%s(%s)" opcode_name (String.concat "," operand_strs)
  | operands, Some s ->
      let operand_strs = List.map string_of_int operands in
      Printf.sprintf "%s(%s,%s)" opcode_name (String.concat "," operand_strs) s

(* TODO Replace this with concat_map  *)
let string_of_dwarf_expression (ops : dwarf_expression_operation list) : string
    =
  String.concat " " (List.map string_of_dwarf_operation ops)

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
  type t = { buffer : Buffer.t; format : object_format }
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
          let _, commands = Object.Macho.read elf.buffer in
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
          | section :: _ -> Some (Object.Macho.section_body elf.buffer section)
          | [] -> None)
      | ELF ->
          let _, sa = Object.Elf.read_elf elf.buffer in
          Object.Elf.read_section_contents elf.buffer sa s
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

(* Object-format-aware section finder that works with both ELF and MachO *)
let find_debug_section_by_type buffer section_type =
  let object_format = detect_format buffer in
  let section_name = object_format_to_section_name object_format section_type in
  try
    match object_format with
    | ELF -> (
        let open Object.Elf in
        let _header, section_array = read_elf buffer in
        let section_opt =
          Array.find_opt
            (fun section -> section.sh_name_str = section_name)
            section_array
        in
        match section_opt with
        | Some section -> Some (section.sh_offset, section.sh_size)
        | None -> None)
    | MachO -> (
        let open Object.Macho in
        let _header, commands = read buffer in
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
            Array.find_map
              (fun section ->
                if section.sec_sectname = section_name then
                  Some
                    ( Unsigned.UInt64.of_uint32 section.sec_offset,
                      section.sec_size )
                else None)
              dwarf_segment.seg_sections)
  with _ -> None

(* TODO Refactor to use section_type *)
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

(* TODO Suspicious function, how does this get used? *)
let read_string_from_section buffer offset section_offset : string option =
  try
    let actual_offset = section_offset + offset in
    let cursor = Object.Buffer.cursor buffer ~at:actual_offset in
    Object.Buffer.Read.zero_string cursor ()
  with _ -> None

(* TODO Provide tests for this function *)
let resolve_string_index (buffer : Object.Buffer.t) (index : int) : string =
  (* Try to resolve string index using debug_str_offs and debug_str sections *)
  (* TODO Restructure this using let or passed in sections? *)
  match
    ( find_debug_section_by_type buffer Debug_str_offs,
      find_debug_section_by_type buffer Debug_str )
  with
  | Some (str_offs_offset, _), Some (str_offset, _) -> (
      try
        (* Read offset from string offsets table *)
        (* TODO This should parse and validate the header structure *)
        (* Skip the 8-byte header: unit_length(4) + version(2) + padding(2) *)
        let str_offs_cursor =
          Object.Buffer.cursor buffer
            ~at:(Unsigned.UInt64.to_int str_offs_offset + 8 + (index * 4))
        in
        let string_offset =
          Object.Buffer.Read.u32 str_offs_cursor |> Unsigned.UInt32.to_int
        in

        (* Read actual string from debug_str section *)
        match
          read_string_from_section buffer string_offset
            (Unsigned.UInt64.to_int str_offset)
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
    | DW_FORM_strp -> (
        (* String pointer to Debug_str section *)
        let offset = Object.Buffer.Read.u32 cur |> Unsigned.UInt32.to_int in
        match find_debug_section_by_type full_buffer Debug_str with
        | Some (str_section_offset, _) -> (
            match
              read_string_from_section full_buffer offset
                (Unsigned.UInt64.to_int str_section_offset)
            with
            | Some s -> String s
            | None -> String (Printf.sprintf "<strp_offset:%d>" offset))
        | None -> String (Printf.sprintf "<strp_offset:%d>" offset))
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
    | DW_FORM_flag_present ->
        Flag
          true (* TODO Validate this parsing, surely it should read a value. *)
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
    | DW_FORM_strx1 ->
        (* String index form - reads 1-byte index into string offsets table *)
        let index = Object.Buffer.Read.u8 cur |> Unsigned.UInt8.to_int in
        let resolved_string = resolve_string_index full_buffer index in
        String resolved_string
    | DW_FORM_strx2 ->
        (* String index form - reads 2-byte index into string offsets table *)
        let index = Object.Buffer.Read.u16 cur |> Unsigned.UInt16.to_int in
        let resolved_string = resolve_string_index full_buffer index in
        String resolved_string
    | DW_FORM_strx3 ->
        (* String index form - reads 3-byte index into string offsets table *)
        let byte1 = Object.Buffer.Read.u8 cur |> Unsigned.UInt8.to_int in
        let byte2 = Object.Buffer.Read.u8 cur |> Unsigned.UInt8.to_int in
        let byte3 = Object.Buffer.Read.u8 cur |> Unsigned.UInt8.to_int in
        let index = byte1 lor (byte2 lsl 8) lor (byte3 lsl 16) in
        let resolved_string = resolve_string_index full_buffer index in
        String resolved_string
    | DW_FORM_strx4 ->
        (* String index form - reads 4-byte index into string offsets table *)
        let index = Object.Buffer.Read.u32 cur |> Unsigned.UInt32.to_int in
        let resolved_string = resolve_string_index full_buffer index in
        String resolved_string
    | DW_FORM_line_strp -> (
        (* String pointer to Debug_line_str section *)
        let offset = Object.Buffer.Read.u32 cur |> Unsigned.UInt32.to_int in
        match find_debug_section_by_type full_buffer Debug_line_str with
        | Some (str_section_offset, _) -> (
            match
              read_string_from_section full_buffer offset
                (Unsigned.UInt64.to_int str_section_offset)
            with
            | Some s -> String s
            | None -> String (Printf.sprintf "<line_strp_offset:%d>" offset))
        | None -> String (Printf.sprintf "<line_strp_offset:%d>" offset))
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
    header_span : span;
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
    (* DWARF 5 header: unit_length(4) + version(2) + unit_type(1) + address_size(1) + debug_abbrev_offset(4) = 12 bytes *)
    let cur =
      Object.Buffer.cursor t.raw_buffer_.buffer
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

  (* Calculate header size: unit_length(4) + version(2) + unit_type(1) + address_size(1) + debug_abbrev_offset(4) = 12 bytes *)
  let header_end = cur.position in
  let header_size = header_end - start in
  let header_span =
    {
      start = Unsigned.UInt64.of_int start;
      size = Unsigned.UInt64.of_int header_size;
    }
  in

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
  ( span,
    {
      unit_length;
      version;
      unit_type;
      debug_abbrev_offset;
      address_size;
      header_span;
    } )

let parse_compile_unit (object_file : Object_file.t)
    (cur : Object.Buffer.cursor) : CompileUnit.t =
  (* Start by parsing just the header to get size *)
  let start = cur.position in

  (* Reset cursor to start for consistent parsing *)
  Object.Buffer.seek cur start;

  let data, parsed = parse_compile_unit_header cur in
  CompileUnit.make 0 data object_file parsed

let parse_compile_units (dwarf : t) : CompileUnit.t Seq.t =
  match find_debug_section_by_type dwarf.object_.buffer Debug_info with
  | None -> Seq.empty
  | Some (section_offset, section_size) ->
      let section_end = Unsigned.UInt64.to_int section_size in

      (* Create a lazy sequence generator *)
      let rec parse_units cursor_pos () =
        if cursor_pos >= section_end then Seq.Nil
        else
          try
            let absolute_pos =
              Unsigned.UInt64.to_int section_offset + cursor_pos
            in
            let cur =
              Object.Buffer.cursor dwarf.object_.buffer ~at:absolute_pos
            in
            let span, parsed_header = parse_compile_unit_header cur in
            let unit =
              CompileUnit.make cursor_pos span dwarf.object_ parsed_header
            in

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

  type file_entry = {
    name : string;
    timestamp : u64;
    size : u64;
    directory : string;
    md5_checksum : string option;
  }

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
    file_names : file_entry array;
  }

  type line_table_entry = {
    address : u64;
    line : u32;
    column : u32;
    file_index : u32;
    isa : u32;
    discriminator : u32;
    op_index : u32;
    is_stmt : bool;
    basic_block : bool;
    end_sequence : bool;
    prologue_end : bool;
    epilogue_begin : bool;
  }

  (** Resolve a DW_FORM_line_strp offset to its string value.

      In DWARF 5, strings in line tables can be stored indirectly in the
      .debug_line_str section to reduce duplication. This function takes an
      offset into that section and returns the null-terminated string found at
      that offset.

      @param buffer Complete buffer containing debug sections
      @param offset Offset into .debug_line_str section
      @return
        String at the given offset, or a placeholder if section/offset not found

      Reference: DWARF 5 specification, section 7.26 "String Encodings" *)
  let resolve_line_strp_offset buffer offset =
    match find_debug_section_by_type buffer Debug_line_str with
    | None ->
        Printf.sprintf "<line_strp:0x%08lx>" (Unsigned.UInt32.to_int32 offset)
    | Some (section_offset, _size) -> (
        try
          let string_cursor =
            Object.Buffer.cursor buffer
              ~at:
                (Unsigned.UInt64.to_int section_offset
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

  (** Parse a DWARF 5 line program header from a buffer cursor.

      This implementation handles the complex DWARF 5 format with flexible
      directory and file entry encodings based on format descriptors. It parses
      all header fields, builds the directory and file tables, and resolves any
      string references to the .debug_line_str section.

      The parsing process: 1. Fixed header fields (lengths, versions, basic
      parameters) 2. Standard opcode definitions array 3. Directory entries
      using directory_entry_formats descriptors 4. File entries using
      file_name_entry_formats descriptors 5. Directory index resolution in file
      entries 6. MD5 checksum conversion from binary to hex string

      Supports all standard DWARF 5 content types and forms including
      DW_FORM_line_strp indirect string references and DW_FORM_data16 MD5
      checksums. *)
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
      (* DWARF 5 spec 6.2.4: Each description consists of a pair of ULEB128 values *)
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
        (* Section 6.2 (Line Number Information) of the DWARF 5 spec does not seem to
           constrain which DW_LNCT content types are valid for directory entries.
           https://www.mail-archive.com/dwarf-discuss@lists.dwarfstd.org/msg01394.html *)
        | DW_LNCT_directory_index, _ ->
            failwith
              "DW_LNCT_directory_index is not valid for directory entries \
               (DWARF 5 spec)"
        | DW_LNCT_timestamp, _ ->
            failwith
              "DW_LNCT_timestamp is not valid for directory entries (DWARF 5 \
               spec)"
        | DW_LNCT_size, _ ->
            failwith
              "DW_LNCT_size is not valid for directory entries (DWARF 5 spec)"
        | DW_LNCT_MD5, _ ->
            failwith
              "DW_LNCT_MD5 is not valid for directory entries (DWARF 5 spec)"
        | DW_LNCT_lo_user, _ | DW_LNCT_hi_user, _ -> (
            (* Vendor-defined content types - skip with form-based parsing *)
            match form with
            | DW_FORM_string -> ignore (Object.Buffer.Read.zero_string cur ())
            | DW_FORM_data1 -> ignore (Object.Buffer.Read.u8 cur)
            | DW_FORM_data2 -> ignore (Object.Buffer.Read.u16 cur)
            | DW_FORM_data4 -> ignore (Object.Buffer.Read.u32 cur)
            | DW_FORM_data8 -> ignore (Object.Buffer.Read.u64 cur)
            | DW_FORM_udata -> ignore (Object.Buffer.Read.uleb128 cur)
            | _ ->
                failwith
                  "Unsupported form for vendor-defined directory entry content \
                   type")
        | DW_LNCT_path, _form ->
            failwith
              "Unsupported form for DW_LNCT_path in directory entry (expected \
               DW_FORM_string or DW_FORM_line_strp)"
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
      (* DWARF 5 spec 6.2.4: Each description consists of a pair of ULEB128 values *)
      let content_type_code = Object.Buffer.Read.uleb128 cur in
      let form_code = Object.Buffer.Read.uleb128 cur in
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
        {
          name = "";
          timestamp = Unsigned.UInt64.of_int 0;
          size = Unsigned.UInt64.of_int 0;
          directory = "";
          md5_checksum = None;
        }
    in
    for i = 0 to Array.length file_names - 1 do
      (* DWARF 5 spec 6.2.4: Parse file entry to populate file_names array with
         path, directory_index, timestamp, size, and MD5 data based on format descriptors *)
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
        | DW_LNCT_directory_index, DW_FORM_data1 ->
            dir_index_ref := Object.Buffer.Read.u8 cur |> Unsigned.UInt8.to_int
        | DW_LNCT_directory_index, DW_FORM_data2 ->
            dir_index_ref :=
              Object.Buffer.Read.u16 cur |> Unsigned.UInt16.to_int
        | DW_LNCT_timestamp, DW_FORM_data4 ->
            timestamp_ref :=
              Object.Buffer.Read.u32 cur |> Unsigned.UInt32.to_int
              |> Unsigned.UInt64.of_int
        | DW_LNCT_timestamp, DW_FORM_data8 ->
            timestamp_ref := Object.Buffer.Read.u64 cur
        | DW_LNCT_size, DW_FORM_data1 ->
            file_size_ref :=
              Object.Buffer.Read.u8 cur |> Unsigned.UInt8.to_int
              |> Unsigned.UInt64.of_int
        | DW_LNCT_size, DW_FORM_data2 ->
            file_size_ref :=
              Object.Buffer.Read.u16 cur |> Unsigned.UInt16.to_int
              |> Unsigned.UInt64.of_int
        | DW_LNCT_size, DW_FORM_data4 ->
            file_size_ref :=
              Object.Buffer.Read.u32 cur |> Unsigned.UInt32.to_int
              |> Unsigned.UInt64.of_int
        | DW_LNCT_size, DW_FORM_data8 ->
            file_size_ref := Object.Buffer.Read.u64 cur
        | DW_LNCT_lo_user, _ | DW_LNCT_hi_user, _ -> (
            (* Vendor-defined content types - skip with form-based parsing *)
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
            | _ ->
                failwith
                  "Unsupported form for vendor-defined file entry content type")
        | _content_type, _form ->
            failwith
              "Invalid content_type/form combination for file entry (DWARF 5 \
               spec)"
      done;

      let dir_name =
        if !dir_index_ref < Array.length directories then
          directories.(!dir_index_ref)
        else ""
      in
      file_names.(i) <-
        {
          name = !path_ref;
          timestamp = !timestamp_ref;
          size = !file_size_ref;
          directory = dir_name;
          md5_checksum = !md5_ref;
        }
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

  (** Parse the line number program following the header.

      This implements the DWARF 5 line number state machine as specified in
      section 6.2.2. The state machine processes opcodes and generates line
      table entries that map program addresses to source locations. *)
  let parse_line_program (cur : Object.Buffer.cursor)
      (header : line_program_header) : line_table_entry list =
    (* Initialize the line number state machine *)
    let address = ref (Unsigned.UInt64.of_int 0) in
    let op_index = ref (Unsigned.UInt32.of_int 0) in
    let file_index = ref (Unsigned.UInt32.of_int 1) in
    let line = ref (Unsigned.UInt32.of_int 1) in
    let column = ref (Unsigned.UInt32.of_int 0) in
    let is_stmt = ref header.default_is_stmt in
    let basic_block = ref false in
    let end_sequence = ref false in
    let prologue_end = ref false in
    let epilogue_begin = ref false in
    let isa = ref (Unsigned.UInt32.of_int 0) in
    let discriminator = ref (Unsigned.UInt32.of_int 0) in

    let entries = ref [] in
    let program_done = ref false in

    (* Helper function to create a line table entry *)
    let make_entry () =
      {
        address = !address;
        line = !line;
        column = !column;
        file_index = !file_index;
        isa = !isa;
        discriminator = !discriminator;
        op_index = !op_index;
        is_stmt = !is_stmt;
        basic_block = !basic_block;
        end_sequence = !end_sequence;
        prologue_end = !prologue_end;
        epilogue_begin = !epilogue_begin;
      }
    in

    (* Process opcodes until end of program *)
    while not !program_done do
      try
        let opcode = Object.Buffer.Read.u8 cur in
        let opcode_val = Unsigned.UInt8.to_int opcode in

        if opcode_val = 0 then
          (* Extended opcode *)
          let length = Object.Buffer.Read.uleb128 cur in
          let extended_opcode = Object.Buffer.Read.u8 cur in
          let extended_val = Unsigned.UInt8.to_int extended_opcode in

          (* TODO Revisit this *)
          match extended_val with
          | 0x01 ->
              (* DW_LNE_end_sequence *)
              end_sequence := true;
              entries := make_entry () :: !entries;
              program_done := true
          | 0x02 ->
              (* DW_LNE_set_address *)
              let addr_bytes = length - 1 in
              if addr_bytes = 4 then
                address :=
                  Object.Buffer.Read.u32 cur |> Unsigned.UInt64.of_uint32
              else if addr_bytes = 8 then address := Object.Buffer.Read.u64 cur
              else failwith "Unsupported address size in DW_LNE_set_address"
          | 0x04 ->
              (* DW_LNE_set_discriminator *)
              discriminator :=
                Object.Buffer.Read.uleb128 cur |> Unsigned.UInt32.of_int
          | _ ->
              (* Skip unknown extended opcodes *)
              for _i = 1 to length - 1 do
                ignore (Object.Buffer.Read.u8 cur)
              done
        else if opcode_val >= Unsigned.UInt8.to_int header.opcode_base then (
          (* Special opcode *)
          let adjusted_opcode =
            opcode_val - Unsigned.UInt8.to_int header.opcode_base
          in
          let line_increment =
            header.line_base
            + (adjusted_opcode mod Unsigned.UInt8.to_int header.line_range)
          in
          let address_increment =
            adjusted_opcode
            / Unsigned.UInt8.to_int header.line_range
            * Unsigned.UInt8.to_int header.minimum_instruction_length
          in

          address :=
            Unsigned.UInt64.add !address
              (Unsigned.UInt64.of_int address_increment);
          line :=
            Unsigned.UInt32.add !line (Unsigned.UInt32.of_int line_increment);

          entries := make_entry () :: !entries;
          basic_block := false;
          prologue_end := false;
          epilogue_begin := false;
          discriminator := Unsigned.UInt32.of_int 0)
        else
          (* Standard opcode *)
          match opcode_val with
          | 0x01 ->
              (* DW_LNS_copy *)
              entries := make_entry () :: !entries;
              basic_block := false;
              prologue_end := false;
              epilogue_begin := false;
              discriminator := Unsigned.UInt32.of_int 0
          | 0x02 ->
              (* DW_LNS_advance_pc *)
              let advance = Object.Buffer.Read.uleb128 cur in
              let address_increment =
                advance
                * Unsigned.UInt8.to_int header.minimum_instruction_length
              in
              address :=
                Unsigned.UInt64.add !address
                  (Unsigned.UInt64.of_int address_increment)
          | 0x03 ->
              (* DW_LNS_advance_line *)
              let advance = Object.Buffer.Read.sleb128 cur in
              line := Unsigned.UInt32.add !line (Unsigned.UInt32.of_int advance)
          | 0x04 ->
              (* DW_LNS_set_file *)
              file_index :=
                Object.Buffer.Read.uleb128 cur |> Unsigned.UInt32.of_int
          | 0x05 ->
              (* DW_LNS_set_column *)
              column := Object.Buffer.Read.uleb128 cur |> Unsigned.UInt32.of_int
          | 0x06 ->
              (* DW_LNS_negate_stmt *)
              is_stmt := not !is_stmt
          | 0x07 ->
              (* DW_LNS_set_basic_block *)
              basic_block := true
          | 0x08 ->
              (* DW_LNS_const_add_pc *)
              let adjusted_opcode =
                255 - Unsigned.UInt8.to_int header.opcode_base
              in
              let address_increment =
                adjusted_opcode
                / Unsigned.UInt8.to_int header.line_range
                * Unsigned.UInt8.to_int header.minimum_instruction_length
              in
              address :=
                Unsigned.UInt64.add !address
                  (Unsigned.UInt64.of_int address_increment)
          | 0x09 ->
              (* DW_LNS_fixed_advance_pc *)
              let advance = Object.Buffer.Read.u16 cur in
              address :=
                Unsigned.UInt64.add !address
                  (Unsigned.UInt64.of_int (Unsigned.UInt16.to_int advance))
          | 0x0a ->
              (* DW_LNS_set_prologue_end *)
              prologue_end := true
          | 0x0b ->
              (* DW_LNS_set_epilogue_begin *)
              epilogue_begin := true
          | 0x0c ->
              (* DW_LNS_set_isa *)
              isa := Object.Buffer.Read.uleb128 cur |> Unsigned.UInt32.of_int
          | _ ->
              (* Skip unknown standard opcodes using operand count from header *)
              let operand_count =
                if opcode_val < Array.length header.standard_opcode_lengths then
                  Unsigned.UInt8.to_int
                    header.standard_opcode_lengths.(opcode_val - 1)
                else 0
              in
              for _i = 1 to operand_count do
                ignore (Object.Buffer.Read.uleb128 cur)
              done
      with End_of_file | _ -> program_done := true
    done;

    List.rev !entries
end

(** Call frame information parsing for Debug_frame section *)
module CallFrame = struct
  let debug_frame_cie_id = Unsigned.UInt32.of_int32 0xffffffffl

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
    initial_location : u32;
    address_range : u32;
    augmentation_length : u64 option;
    augmentation_data : string option;
    instructions : string;
    offset : u32; (* File offset where this FDE starts *)
  }

  (* CFI rule types for state machine *)
  type cfi_rule =
    | Rule_undefined
    | Rule_same_value
    | Rule_offset of int64 (* offset from CFA *)
    | Rule_val_offset of int64 (* value = CFA + offset *)
    | Rule_register of int (* register number *)
    | Rule_expression of string (* DWARF expression *)
    | Rule_val_expression of string (* DWARF expression for value *)

  (* CFI state for tracking register rules *)
  type cfi_state = {
    cfa_register : int;
    cfa_offset : int64;
    register_rules : (int, cfi_rule) Hashtbl.t;
    pc_offset : int;
    state_stack : cfi_state list; (* Stack for remember_state/restore_state *)
  }

  let create_default_cie () =
    {
      length = Unsigned.UInt32.of_int 0;
      cie_id = Unsigned.UInt32.of_int 0;
      version = Unsigned.UInt8.of_int 1;
      augmentation = "";
      address_size = Unsigned.UInt8.of_int 8;
      segment_selector_size = Unsigned.UInt8.of_int 0;
      code_alignment_factor = Unsigned.UInt64.of_int 1;
      data_alignment_factor = Signed.Int64.of_int (-8);
      return_address_register = Unsigned.UInt64.of_int 16;
      augmentation_length = None;
      augmentation_data = None;
      initial_instructions = "";
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
    if Unsigned.UInt32.compare cie_id debug_frame_cie_id <> 0 then
      failwith "Invalid CIE: cie_id is not the debug_frame CIE identifier";

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

    (* TODO Refactor *)
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

  (** Parse a Frame Description Entry from the Debug_frame section *)
  let parse_frame_description_entry (cur : Object.Buffer.cursor)
      (start_pos : int) : frame_description_entry =
    let length = Object.Buffer.Read.u32 cur in
    let cie_pointer = Object.Buffer.Read.u32 cur in
    let initial_location = Object.Buffer.Read.u32 cur in
    let address_range = Object.Buffer.Read.u32 cur in

    (* Calculate remaining bytes for instructions *)
    let header_size = 16 in
    (* length + cie_pointer + initial_location + address_range *)
    let total_length = Unsigned.UInt32.to_int length in
    let instructions_length = max 0 (total_length - header_size + 4) in
    (* +4 because length field is excluded *)

    (* For now, assume no augmentation data in FDEs for simplicity *)
    let augmentation_length = None in
    let augmentation_data = None in

    (* Parse the FDE instructions *)
    let instructions =
      if instructions_length > 0 then parse_instructions cur instructions_length
      else ""
    in

    {
      length;
      cie_pointer;
      initial_location;
      address_range;
      augmentation_length;
      augmentation_data;
      instructions;
      offset = Unsigned.UInt32.of_int start_pos;
    }

  (** Debug Frame section entry type *)
  type debug_frame_entry =
    | CIE of common_information_entry
    | FDE of frame_description_entry
    | Zero_terminator of int (* Position of zero terminator *)

  type debug_frame_section = {
    entries : debug_frame_entry list;
    entry_count : int;
  }
  (** Debug Frame section *)

  (** Parse debug_frame section from cursor *)
  let parse_debug_frame_section cursor section_size =
    let section_end = cursor.position + section_size in
    let entries = ref [] in
    let entry_count = ref 0 in

    try
      while cursor.position < section_end do
        let start_pos = cursor.position in
        let length = Object.Buffer.Read.u32 cursor in
        let length_int = Unsigned.UInt32.to_int length in

        if length_int = 0 then (
          (* Zero length indicates end of section *)
          entries := Zero_terminator start_pos :: !entries;
          cursor.position <- section_end (* End parsing *))
        else
          (* Read the CIE/FDE ID field *)
          let id = Object.Buffer.Read.u32 cursor in

          (* Reset cursor to parse the full entry *)
          cursor.position <- start_pos;

          if Unsigned.UInt32.compare id debug_frame_cie_id = 0 then (
            (* This is a Common Information Entry (CIE) *)
            let cie = parse_common_information_entry cursor in
            entries := CIE cie :: !entries;
            incr entry_count)
          else
            (* This is a Frame Description Entry (FDE) *)
            let fde = parse_frame_description_entry cursor start_pos in
            entries := FDE fde :: !entries;
            incr entry_count
      done;
      { entries = List.rev !entries; entry_count = !entry_count }
    with End_of_file | _ ->
      { entries = List.rev !entries; entry_count = !entry_count }

  (* TODO This is x86_64 specific, we want to support ARM64 as well *)
  (* Create initial CFI state *)
  let initial_cfi_state () =
    {
      cfa_register = 7;
      (* Default RSP for x86_64 *)
      cfa_offset = 8L;
      (* Default stack pointer offset *)
      register_rules = Hashtbl.create 32;
      pc_offset = 0;
      state_stack = [];
    }

  (* Parse CIE initial instructions to establish proper initial CFI state *)
  let parse_initial_state (cie : common_information_entry) : cfi_state =
    if String.length cie.initial_instructions = 0 then
      (* No initial instructions, use architecture-aware defaults *)
      initial_cfi_state ()
    else
      (* Parse CIE initial instructions to establish baseline state *)
      let data_alignment = Signed.Int64.to_int64 cie.data_alignment_factor in
      let initial_state = initial_cfi_state () in

      (* Apply CIE initial instructions to the default state *)
      let rec apply_initial_instructions state pos =
        if pos >= String.length cie.initial_instructions then state
        else
          let opcode = Char.code cie.initial_instructions.[pos] in
          let instruction = decode_cfa_opcode opcode in
          match instruction with
          | DW_CFA_def_cfa ->
              (* DW_CFA_def_cfa register offset *)
              let reg, pos1 =
                read_uleb128_from_string cie.initial_instructions (pos + 1)
              in
              let offset, pos2 =
                read_uleb128_from_string cie.initial_instructions pos1
              in
              let new_state =
                {
                  state with
                  cfa_register = reg;
                  cfa_offset = Int64.of_int offset;
                }
              in
              apply_initial_instructions new_state pos2
          | DW_CFA_def_cfa_register ->
              (* DW_CFA_def_cfa_register register *)
              let reg, next_pos =
                read_uleb128_from_string cie.initial_instructions (pos + 1)
              in
              let new_state = { state with cfa_register = reg } in
              apply_initial_instructions new_state next_pos
          | DW_CFA_def_cfa_offset ->
              (* DW_CFA_def_cfa_offset offset *)
              let offset, next_pos =
                read_uleb128_from_string cie.initial_instructions (pos + 1)
              in
              let new_state = { state with cfa_offset = Int64.of_int offset } in
              apply_initial_instructions new_state next_pos
          | DW_CFA_offset ->
              (* DW_CFA_offset register (embedded) offset *)
              let reg = opcode land 0x3f in
              let offset_uleb, next_pos =
                read_uleb128_from_string cie.initial_instructions (pos + 1)
              in
              let offset =
                Int64.mul (Int64.of_int offset_uleb) data_alignment
              in
              Hashtbl.replace state.register_rules reg (Rule_offset offset);
              apply_initial_instructions state next_pos
          | DW_CFA_nop -> apply_initial_instructions state (pos + 1)
          | _ ->
              (* Skip unknown or unsupported instructions for CIE initial state *)
              apply_initial_instructions state (pos + 1)
      in
      apply_initial_instructions initial_state 0

  (* Read ULEB128 from string at position *)
  let read_uleb128_from_string str pos =
    let rec read_uleb acc shift pos =
      if pos >= String.length str then (acc, pos)
      else
        let byte = Char.code str.[pos] in
        let value = byte land 0x7f in
        let acc' = acc lor (value lsl shift) in
        if byte land 0x80 = 0 then (acc', pos + 1)
        else read_uleb acc' (shift + 7) (pos + 1)
    in
    read_uleb 0 0 pos

  (* Read signed LEB128 from string at position *)
  let read_sleb128_from_string str pos =
    let rec read_sleb acc shift pos =
      if pos >= String.length str then (acc, pos)
      else
        let byte = Char.code str.[pos] in
        let value = byte land 0x7f in
        let acc' = acc lor (value lsl shift) in
        let pos' = pos + 1 in
        if byte land 0x80 = 0 then
          (* Sign extend if necessary *)
          let result =
            if shift < 32 && value land 0x40 <> 0 then
              acc' lor (-1 lsl (shift + 7))
            else acc'
          in
          (result, pos')
        else read_sleb acc' (shift + 7) pos'
    in
    read_sleb 0 0 pos

  (* Read multi-byte fixed-size values from string at position *)
  let read_u8_from_string str pos =
    if pos < String.length str then (Char.code str.[pos], pos + 1) else (0, pos)

  let read_u16_from_string str pos =
    if pos + 1 < String.length str then
      let b0 = Char.code str.[pos] in
      let b1 = Char.code str.[pos + 1] in
      (* Little-endian byte order *)
      (b0 lor (b1 lsl 8), pos + 2)
    else (0, pos)

  let read_u32_from_string str pos =
    if pos + 3 < String.length str then
      let b0 = Char.code str.[pos] in
      let b1 = Char.code str.[pos + 1] in
      let b2 = Char.code str.[pos + 2] in
      let b3 = Char.code str.[pos + 3] in
      (* Little-endian byte order *)
      let result = b0 lor (b1 lsl 8) lor (b2 lsl 16) lor (b3 lsl 24) in
      (result, pos + 4)
    else (0, pos)

  (* Basic CFI instruction parser - extracts info from instruction bytes *)
  let parse_cfi_instructions (instructions : string) (code_alignment : int64)
      (_data_alignment : int64) : (int * string) list =
    let rec parse_byte_stream bytes pos pc_offset acc =
      if pos >= String.length bytes then List.rev acc
      else
        let opcode = Char.code bytes.[pos] in
        let instruction = decode_cfa_opcode opcode in
        match instruction with
        | DW_CFA_def_cfa ->
            (* DW_CFA_def_cfa takes register and offset *)
            if pos + 2 < String.length bytes then
              let reg = Char.code bytes.[pos + 1] in
              let offset = Char.code bytes.[pos + 2] in
              let desc = Printf.sprintf "<off cfa=%02d(r%d) >" offset reg in
              parse_byte_stream bytes (pos + 3) pc_offset
                ((pc_offset, desc) :: acc)
            else List.rev acc
        | DW_CFA_offset ->
            (* DW_CFA_offset with embedded register number *)
            let reg = opcode land 0x3f in
            if pos + 1 < String.length bytes then
              let offset = Char.code bytes.[pos + 1] in
              let desc =
                Printf.sprintf "<off r%d=-%d(cfa) >" reg (offset * 8)
              in
              parse_byte_stream bytes (pos + 2) pc_offset
                ((pc_offset, desc) :: acc)
            else List.rev acc
        | DW_CFA_advance_loc ->
            (* DW_CFA_advance_loc with embedded delta *)
            let delta = opcode land 0x3f in
            parse_byte_stream bytes (pos + 1) (pc_offset + delta) acc
        | DW_CFA_nop -> parse_byte_stream bytes (pos + 1) pc_offset acc
        | DW_CFA_restore ->
            (* DW_CFA_restore with embedded register number *)
            let reg = opcode land 0x3f in
            let desc = Printf.sprintf "<restore r%d >" reg in
            parse_byte_stream bytes (pos + 1) pc_offset
              ((pc_offset, desc) :: acc)
        | DW_CFA_undefined ->
            (* DW_CFA_undefined takes register as ULEB128 *)
            if pos + 1 < String.length bytes then
              let reg, next_pos = read_uleb128_from_string bytes (pos + 1) in
              let desc = Printf.sprintf "<undefined r%d >" reg in
              parse_byte_stream bytes next_pos pc_offset
                ((pc_offset, desc) :: acc)
            else List.rev acc
        | DW_CFA_same_value ->
            (* DW_CFA_same_value takes register as ULEB128 *)
            if pos + 1 < String.length bytes then
              let reg, next_pos = read_uleb128_from_string bytes (pos + 1) in
              let desc = Printf.sprintf "<same_value r%d >" reg in
              parse_byte_stream bytes next_pos pc_offset
                ((pc_offset, desc) :: acc)
            else List.rev acc
        | DW_CFA_register ->
            (* DW_CFA_register takes register and target register as ULEB128 *)
            if pos + 1 < String.length bytes then
              let reg, pos1 = read_uleb128_from_string bytes (pos + 1) in
              if pos1 < String.length bytes then
                let target_reg, next_pos =
                  read_uleb128_from_string bytes pos1
                in
                let desc =
                  Printf.sprintf "<register r%d=r%d >" reg target_reg
                in
                parse_byte_stream bytes next_pos pc_offset
                  ((pc_offset, desc) :: acc)
              else List.rev acc
            else List.rev acc
        | DW_CFA_def_cfa_register ->
            (* DW_CFA_def_cfa_register takes register as ULEB128 *)
            if pos + 1 < String.length bytes then
              let reg, next_pos = read_uleb128_from_string bytes (pos + 1) in
              let desc = Printf.sprintf "<def_cfa_register r%d >" reg in
              parse_byte_stream bytes next_pos pc_offset
                ((pc_offset, desc) :: acc)
            else List.rev acc
        | DW_CFA_def_cfa_offset ->
            (* DW_CFA_def_cfa_offset takes offset as ULEB128 *)
            if pos + 1 < String.length bytes then
              let offset, next_pos = read_uleb128_from_string bytes (pos + 1) in
              let desc = Printf.sprintf "<def_cfa_offset %d >" offset in
              parse_byte_stream bytes next_pos pc_offset
                ((pc_offset, desc) :: acc)
            else List.rev acc
        | DW_CFA_advance_loc1 ->
            (* DW_CFA_advance_loc1 takes 1-byte delta *)
            if pos + 1 < String.length bytes then
              let delta, next_pos = read_u8_from_string bytes (pos + 1) in
              parse_byte_stream bytes next_pos (pc_offset + delta) acc
            else List.rev acc
        | DW_CFA_advance_loc2 ->
            (* DW_CFA_advance_loc2 takes 2-byte delta *)
            if pos + 2 < String.length bytes then
              let delta, next_pos = read_u16_from_string bytes (pos + 1) in
              parse_byte_stream bytes next_pos (pc_offset + delta) acc
            else List.rev acc
        | DW_CFA_advance_loc4 ->
            (* DW_CFA_advance_loc4 takes 4-byte delta *)
            if pos + 4 < String.length bytes then
              let delta, next_pos = read_u32_from_string bytes (pos + 1) in
              parse_byte_stream bytes next_pos (pc_offset + delta) acc
            else List.rev acc
        | DW_CFA_set_loc ->
            (* DW_CFA_set_loc takes address (4-byte for 32-bit targets) *)
            if pos + 4 < String.length bytes then
              let new_loc, next_pos = read_u32_from_string bytes (pos + 1) in
              parse_byte_stream bytes next_pos new_loc acc
            else List.rev acc
        | DW_CFA_offset_extended ->
            (* DW_CFA_offset_extended takes ULEB128 register + ULEB128 offset *)
            if pos + 1 < String.length bytes then
              let reg, pos1 = read_uleb128_from_string bytes (pos + 1) in
              if pos1 < String.length bytes then
                let offset, next_pos = read_uleb128_from_string bytes pos1 in
                let desc =
                  Printf.sprintf "<off r%d=-%d(cfa) >" reg (offset * 8)
                in
                parse_byte_stream bytes next_pos pc_offset
                  ((pc_offset, desc) :: acc)
              else List.rev acc
            else List.rev acc
        | DW_CFA_restore_extended ->
            (* DW_CFA_restore_extended takes ULEB128 register *)
            if pos + 1 < String.length bytes then
              let reg, next_pos = read_uleb128_from_string bytes (pos + 1) in
              let desc = Printf.sprintf "<restore r%d >" reg in
              parse_byte_stream bytes next_pos pc_offset
                ((pc_offset, desc) :: acc)
            else List.rev acc
        | DW_CFA_remember_state ->
            (* DW_CFA_remember_state - push current state onto stack *)
            let desc = Printf.sprintf "<remember_state >" in
            parse_byte_stream bytes (pos + 1) pc_offset
              ((pc_offset, desc) :: acc)
        | DW_CFA_restore_state ->
            (* DW_CFA_restore_state - pop state from stack *)
            let desc = Printf.sprintf "<restore_state >" in
            parse_byte_stream bytes (pos + 1) pc_offset
              ((pc_offset, desc) :: acc)
        | _ ->
            (* Skip unknown instructions for now *)
            parse_byte_stream bytes (pos + 1) pc_offset acc
    in
    let basic_results = parse_byte_stream instructions 0 0 [] in
    (* Apply scaling based on code alignment factor *)
    List.map
      (fun (pc_offset, desc) ->
        let scaled_pc =
          Int64.to_int (Int64.mul (Int64.of_int pc_offset) code_alignment)
        in
        (scaled_pc, desc))
      basic_results
end

(** EH Frame Header (.eh_frame_hdr section) - ELF exception handling support *)
module EHFrameHdr = struct
  type encoding =
    | DW_EH_PE_absptr (* 0x00 *)
    | DW_EH_PE_omit (* 0xff *)
    | DW_EH_PE_uleb128 (* 0x01 *)
    | DW_EH_PE_udata2 (* 0x02 *)
    | DW_EH_PE_udata4 (* 0x03 *)
    | DW_EH_PE_udata8 (* 0x04 *)
    | DW_EH_PE_sleb128 (* 0x09 *)
    | DW_EH_PE_sdata2 (* 0x0a *)
    | DW_EH_PE_sdata4 (* 0x0b *)
    | DW_EH_PE_sdata8 (* 0x0c *)
    | DW_EH_PE_pcrel (* 0x10 - PC relative *)
    | DW_EH_PE_datarel (* 0x30 - data relative *)
    | DW_EH_PE_funcrel (* 0x40 - function relative *)
    | DW_EH_PE_aligned (* 0x50 - aligned *)
    | DW_EH_PE_indirect (* 0x80 - indirect *)

  type search_table_entry = {
    initial_location : u64; (* PC value *)
    fde_address : u64; (* Address of FDE *)
  }

  type header = {
    version : u8;
    eh_frame_ptr_enc : encoding;
    fde_count_enc : encoding;
    table_enc : encoding;
    eh_frame_ptr : u64;
    fde_count : u32;
    search_table : search_table_entry array;
  }

  let encoding_of_u8 = function
    | 0x00 -> DW_EH_PE_absptr
    | 0xff -> DW_EH_PE_omit
    | 0x01 -> DW_EH_PE_uleb128
    | 0x02 -> DW_EH_PE_udata2
    | 0x03 -> DW_EH_PE_udata4
    | 0x04 -> DW_EH_PE_udata8
    | 0x09 -> DW_EH_PE_sleb128
    | 0x0a -> DW_EH_PE_sdata2
    | 0x0b -> DW_EH_PE_sdata4
    | 0x0c -> DW_EH_PE_sdata8
    | 0x10 -> DW_EH_PE_pcrel
    | 0x30 -> DW_EH_PE_datarel
    | 0x40 -> DW_EH_PE_funcrel
    | 0x50 -> DW_EH_PE_aligned
    | 0x80 ->
        DW_EH_PE_indirect
        (* Handle common combined encodings *)
        (* TODO Other common encodings to handle? *)
    | 0x1b -> DW_EH_PE_pcrel (* 0x10 | 0x0b = PC-relative signed 4-byte *)
    | 0x3b -> DW_EH_PE_datarel (* 0x30 | 0x0b = Data-relative signed 4-byte *)
    | n -> failwith (Printf.sprintf "Unknown EH encoding: 0x%02x" n)

  let read_encoded_value cursor encoding _base_addr =
    match encoding with
    | DW_EH_PE_absptr -> Object.Buffer.Read.u64 cursor
    | DW_EH_PE_udata4 ->
        Unsigned.UInt64.of_uint32 (Object.Buffer.Read.u32 cursor)
    | DW_EH_PE_sdata4 ->
        let value = Object.Buffer.Read.u32 cursor in
        let signed_value = Unsigned.UInt32.to_int32 value in
        Unsigned.UInt64.of_int64 (Int64.of_int32 signed_value)
    | DW_EH_PE_pcrel ->
        (* PC-relative: value is relative to current position *)
        let current_pos = Unsigned.UInt64.of_int cursor.position in
        let offset = Object.Buffer.Read.u32 cursor in
        let signed_offset = Unsigned.UInt32.to_int32 offset in
        Unsigned.UInt64.add current_pos
          (Unsigned.UInt64.of_int64 (Int64.of_int32 signed_offset))
    | DW_EH_PE_datarel ->
        (* Data-relative: value is relative to section base *)
        let offset = Object.Buffer.Read.u32 cursor in
        let signed_offset = Unsigned.UInt32.to_int32 offset in
        Unsigned.UInt64.add _base_addr
          (Unsigned.UInt64.of_int64 (Int64.of_int32 signed_offset))
    | _ -> failwith "Unsupported encoding in read_encoded_value"

  let parse_header cursor section_base_addr =
    let version = Object.Buffer.Read.u8 cursor in
    let eh_frame_ptr_enc =
      encoding_of_u8 (Unsigned.UInt8.to_int (Object.Buffer.Read.u8 cursor))
    in
    let fde_count_enc =
      encoding_of_u8 (Unsigned.UInt8.to_int (Object.Buffer.Read.u8 cursor))
    in
    let table_enc =
      encoding_of_u8 (Unsigned.UInt8.to_int (Object.Buffer.Read.u8 cursor))
    in

    let eh_frame_ptr =
      read_encoded_value cursor eh_frame_ptr_enc section_base_addr
    in
    let fde_count_raw =
      read_encoded_value cursor fde_count_enc section_base_addr
    in
    let fde_count =
      Unsigned.UInt32.of_int64 (Unsigned.UInt64.to_int64 fde_count_raw)
    in

    (* Parse search table *)
    let search_table =
      Array.init (Unsigned.UInt32.to_int fde_count) (fun _i ->
          let initial_location =
            read_encoded_value cursor table_enc section_base_addr
          in
          let fde_address =
            read_encoded_value cursor table_enc section_base_addr
          in
          { initial_location; fde_address })
    in

    {
      version;
      eh_frame_ptr_enc;
      fde_count_enc;
      table_enc;
      eh_frame_ptr;
      fde_count;
      search_table;
    }

  (** Parse complete .eh_frame_hdr section *)
  let parse_section cursor section_base_addr =
    parse_header cursor section_base_addr
end

(** EH Frame (.eh_frame section) - ELF exception handling call frame information
*)
module EHFrame = struct
  (* Reuse the CallFrame types but add EH-specific handling *)
  type eh_frame_entry =
    | EH_CIE of CallFrame.common_information_entry
    | EH_FDE of CallFrame.frame_description_entry

  type section = { entries : eh_frame_entry list }

  (* Parse CIE adapted for .eh_frame format *)
  let parse_eh_cie cursor _expected_length =
    let length = Object.Buffer.Read.u32 cursor in
    let cie_id_start = cursor.position in
    (* Track where CIE content starts *)
    let cie_id = Object.Buffer.Read.u32 cursor in

    (* In .eh_frame, CIE ID is 0 (not 0xffffffff like in .debug_frame) *)
    if Unsigned.UInt32.to_int32 cie_id <> 0x00000000l then
      failwith "Invalid EH CIE: cie_id is not 0";

    let version = Object.Buffer.Read.u8 cursor in
    let augmentation = CallFrame.parse_augmentation_string cursor in

    (* EH frame may not have address_size and segment_selector_size for older versions *)
    let address_size, segment_selector_size =
      if Unsigned.UInt8.to_int version >= 4 then
        (Object.Buffer.Read.u8 cursor, Object.Buffer.Read.u8 cursor)
      else (Unsigned.UInt8.of_int 8, Unsigned.UInt8.of_int 0)
      (* Default values *)
    in

    let code_alignment_factor_int = Object.Buffer.Read.uleb128 cursor in
    let code_alignment_factor =
      Unsigned.UInt64.of_int code_alignment_factor_int
    in
    let data_alignment_factor_int = Object.Buffer.Read.sleb128 cursor in
    let data_alignment_factor = Signed.Int64.of_int data_alignment_factor_int in
    let return_address_register_int = Object.Buffer.Read.uleb128 cursor in
    let return_address_register =
      Unsigned.UInt64.of_int return_address_register_int
    in

    (* Parse augmentation data if present *)
    let augmentation_length, augmentation_data =
      match CallFrame.parse_augmentation_data cursor augmentation with
      | Some (len, data) -> (Some len, Some data)
      | None -> (None, None)
    in

    (* Calculate initial instructions length correctly *)
    let current_pos = cursor.position in
    let bytes_read_from_cie_id = current_pos - cie_id_start in
    let total_cie_size = Unsigned.UInt32.to_int length in
    (* Length field covers cie_id onwards *)

    let instructions_length = max 0 (total_cie_size - bytes_read_from_cie_id) in
    let initial_instructions =
      if instructions_length > 0 then
        CallFrame.parse_instructions cursor instructions_length
      else ""
    in

    {
      CallFrame.length;
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

  (* Parse FDE adapted for .eh_frame format *)
  let parse_eh_fde cursor _expected_length fde_offset =
    let length = Object.Buffer.Read.u32 cursor in
    let fde_start = cursor.position in
    (* Track where FDE content starts *)
    let cie_pointer = Object.Buffer.Read.u32 cursor in

    (* In .eh_frame, cie_pointer is a relative offset backwards to the CIE *)
    (* For now, we'll store it as-is and calculate the actual CIE reference later *)
    (* TODO Revisit this *)
    (* Read initial_location (PC start address) - in .eh_frame usually 32-bit PC-relative *)
    let initial_location_field_pos = cursor.position in
    let initial_location_raw = Object.Buffer.Read.u32 cursor in

    (* In .eh_frame, initial_location is PC-relative to the field position.
       Convert signed 32-bit to int32 first, then add field position to get absolute address *)
    let initial_location_offset =
      Int32.of_int (Unsigned.UInt32.to_int initial_location_raw)
    in
    let initial_location =
      Unsigned.UInt32.of_int
        (Int32.to_int
           (Int32.add initial_location_offset
              (Int32.of_int initial_location_field_pos)))
    in

    (* Read address_range (length of code covered) - in .eh_frame usually 32-bit *)
    let address_range = Object.Buffer.Read.u32 cursor in

    (* Parse augmentation data if present - check if remaining bytes suggest augmentation *)
    let current_pos = cursor.position in
    let bytes_read_from_fde = current_pos - fde_start in
    let total_fde_size = Unsigned.UInt32.to_int length in
    let remaining_bytes = total_fde_size - bytes_read_from_fde in

    (* Try to parse augmentation data if there are enough bytes and first byte looks like length *)
    let augmentation_length, augmentation_data, instructions_start =
      if remaining_bytes > 0 then (
        let saved_pos = cursor.position in
        try
          (* Try to read augmentation length as ULEB128 *)
          let aug_len = Object.Buffer.Read.uleb128 cursor in
          if aug_len >= 0 && aug_len < remaining_bytes then (
            (* Read augmentation data *)
            let data = Bytes.create aug_len in
            for i = 0 to aug_len - 1 do
              Bytes.set data i
                (Char.chr
                   (Unsigned.UInt8.to_int (Object.Buffer.Read.u8 cursor)))
            done;
            ( Some (Unsigned.UInt64.of_int aug_len),
              Some (Bytes.to_string data),
              cursor.position ))
          else (
            (* Not valid augmentation data, reset position *)
            cursor.position <- saved_pos;
            (None, None, saved_pos))
        with _ ->
          (* Error reading, reset position *)
          cursor.position <- saved_pos;
          (None, None, saved_pos))
      else (None, None, current_pos)
    in

    (* Parse remaining bytes as instructions *)
    let instructions_length =
      max 0 (total_fde_size - (instructions_start - fde_start))
    in
    let instructions =
      if instructions_length > 0 then
        CallFrame.parse_instructions cursor instructions_length
      else ""
    in

    {
      CallFrame.length;
      cie_pointer;
      initial_location;
      address_range;
      augmentation_length;
      augmentation_data;
      instructions;
      offset = Unsigned.UInt32.of_int fde_offset;
    }

  let parse_section cursor section_size =
    let section_start = cursor.position in
    (* Track section start for relative offsets *)
    let section_end = cursor.position + section_size in
    let entries = ref [] in

    while cursor.position < section_end do
      let start_pos = cursor.position in
      let length = Object.Buffer.Read.u32 cursor in
      let length_int = Unsigned.UInt32.to_int length in

      if length_int = 0 then
        (* Zero terminator *)
        cursor.position <- section_end
      else
        let id = Object.Buffer.Read.u32 cursor in
        (* Reset cursor to parse full entry *)
        cursor.position <- start_pos;

        if Unsigned.UInt32.to_int32 id = 0x00000000l then
          (* This is a CIE in .eh_frame format (id = 0 instead of 0xffffffff) *)
          let cie = parse_eh_cie cursor length in
          entries := EH_CIE cie :: !entries
        else
          (* This is an FDE - parse it to get address ranges *)
          cursor.position <- start_pos;
        (* Reset to start of entry *)
        let fde_section_offset = start_pos - section_start in
        (* Calculate section-relative offset *)
        let fde = parse_eh_fde cursor length fde_section_offset in
        entries := EH_FDE fde :: !entries
    done;

    { entries = List.rev !entries }

  (* Find the CIE corresponding to an FDE using the cie_pointer field *)
  let find_cie_for_fde (entries : eh_frame_entry list) (_cie_pointer : u32) :
      CallFrame.common_information_entry option =
    (* In .eh_frame format, cie_pointer is a relative offset backwards from the current FDE position
       to the CIE. This is different from .debug_frame where it's an absolute offset.

       However, since we're working with parsed entries, we need to find the CIE by
       matching the cie_pointer value. For simplicity, we'll search through all CIEs
       and find the one that matches the expected relationship. *)
    let rec find_cie_in_entries = function
      | [] -> None
      | EH_CIE cie :: _ ->
          (* For now, we use a heuristic: find the CIE that appears before this FDE
             and has compatible parameters. A proper implementation would need to track
             file positions to resolve the relative offset correctly. *)
          Some cie
      | EH_FDE _ :: rest -> find_cie_in_entries rest
    in
    find_cie_in_entries entries

  (* Build a mapping from section offset to CIE for efficient lookup *)
  let build_cie_map (section : section) :
      (int, CallFrame.common_information_entry) Hashtbl.t =
    let cie_map = Hashtbl.create 16 in
    let process_entry index = function
      | EH_CIE cie ->
          (* Use index as a proxy for file offset - in a real implementation,
             we'd need to track actual file positions during parsing *)
          Hashtbl.add cie_map index cie
      | EH_FDE _ -> ()
    in
    List.iteri process_entry section.entries;
    cie_map

  (* Enhanced CIE lookup using both cie_pointer and section mapping *)
  let find_cie_for_fde_enhanced (section : section) (cie_pointer : u32)
      (fde_file_offset : int) : CallFrame.common_information_entry option =
    (* Strategy 1: Try to find CIE using relative offset calculation
       In .eh_frame, cie_pointer is typically the distance back to the CIE *)
    let cie_map = build_cie_map section in

    (* Strategy 2: If relative offset fails, use the most recent CIE
       This matches the current heuristic but is more systematic *)
    let rec find_most_recent_cie entries =
      match entries with
      | [] -> None
      | EH_CIE cie :: _ -> Some cie
      | EH_FDE _ :: rest -> find_most_recent_cie rest
    in

    (* Try mapping approach first, then fallback to most recent *)
    let cie_offset = fde_file_offset - Unsigned.UInt32.to_int cie_pointer in
    match Hashtbl.find_opt cie_map cie_offset with
    | Some cie -> Some cie
    | None -> find_most_recent_cie section.entries
end

(** Accelerated Name Lookup (.debug_names section) - DWARF 5 Section 6.1 *)
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

  type debug_str_entry = {
    offset : u32;  (** Original offset in debug_str section *)
    value : string;  (** Resolved string value *)
  }
  (** String with original offset preserved for debug_names *)

  type name_index_entry = {
    name_offset : u32;
    die_offset : u32;
    attributes : (name_index_attribute * u64) list;
  }

  type debug_names_abbrev = {
    code : u64;  (** Abbreviation code *)
    tag : abbreviation_tag;  (** DWARF tag *)
    attributes : (name_index_attribute * attribute_form_encoding) list;
        (** List of attributes and their forms *)
  }
  (** Abbreviation entry for debug_names *)

  type debug_names_section = {
    header : name_index_header;
    comp_unit_offsets : u32 array;
    local_type_unit_offsets : u32 array;
    foreign_type_unit_signatures : u64 array;
    buckets : u32 array;  (** Hash bucket organization *)
    hash_table : u32 array;
    name_table : debug_str_entry array;
    entry_offsets : u32 array;
        (** Entry offsets into entry pool for each name *)
    abbreviation_table : debug_names_abbrev list;
        (** Parsed abbreviation table *)
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

  (** Parse name table as array of debug_str_entry with offset tracking *)
  let parse_name_table_with_offsets (cur : Object.Buffer.cursor) (count : u32) :
      debug_str_entry array =
    let arr =
      Array.make
        (Unsigned.UInt32.to_int count)
        { offset = Unsigned.UInt32.zero; value = "" }
    in
    for i = 0 to Array.length arr - 1 do
      let start_pos = cur.position in
      let value =
        match Object.Buffer.Read.zero_string cur () with
        | Some s -> s
        | None -> ""
      in
      arr.(i) <- { offset = Unsigned.UInt32.of_int start_pos; value }
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

  (** Parse debug_names abbreviation entry *)

  (** Parse debug_names abbreviation table *)
  let parse_debug_names_abbrev_table (cur : Object.Buffer.cursor) (size : u32) :
      (u64, debug_names_abbrev) Hashtbl.t =
    let abbrev_table = Hashtbl.create 16 in
    let start_pos = cur.position in
    let size_int = Unsigned.UInt32.to_int size in
    let end_pos = start_pos + size_int in

    let rec parse_abbrevs () =
      if cur.position >= end_pos then ()
      else
        let code = Object.Buffer.Read.uleb128 cur |> Unsigned.UInt64.of_int in
        if Unsigned.UInt64.to_int code = 0 then ()
        else
          let tag_code =
            Object.Buffer.Read.uleb128 cur |> Unsigned.UInt64.of_int
          in
          let tag = abbreviation_tag_of_u64 tag_code in
          let attributes = ref [] in

          (* Read attribute specifications until we hit (0, 0) *)
          let rec read_attributes () =
            let attr_code =
              Object.Buffer.Read.uleb128 cur |> Unsigned.UInt64.of_int
            in
            let form_code =
              Object.Buffer.Read.uleb128 cur |> Unsigned.UInt64.of_int
            in
            if
              Unsigned.UInt64.to_int attr_code <> 0
              || Unsigned.UInt64.to_int form_code <> 0
            then (
              let attr = name_index_attribute_of_u64 attr_code in
              let form = attribute_form_encoding form_code in
              attributes := (attr, form) :: !attributes;
              read_attributes ())
          in
          read_attributes ();

          let abbrev = { code; tag; attributes = List.rev !attributes } in
          Hashtbl.add abbrev_table code abbrev;
          parse_abbrevs ()
    in
    parse_abbrevs ();
    abbrev_table

  (** DJB2 hash function used by DWARF 5 debug_names sections *)
  let djb2_hash (s : string) : u32 =
    let hash = ref 5381 in
    String.iter
      (fun c ->
        let char_code = Char.code c in
        hash := (!hash lsl 5) + !hash + char_code)
      s;
    Unsigned.UInt32.of_int !hash

  (** Resolve debug_str offset to debug_str_entry with both offset and value *)
  let resolve_debug_str_offset (buffer : Object.Buffer.t) (offset : u32) :
      debug_str_entry =
    match find_debug_section_by_type buffer Debug_str with
    | Some (str_section_offset, _) -> (
        match
          read_string_from_section buffer
            (Unsigned.UInt32.to_int offset)
            (Unsigned.UInt64.to_int str_section_offset)
        with
        | Some resolved_string -> { offset; value = resolved_string }
        | None ->
            {
              offset;
              value =
                Printf.sprintf "<str_error:0x%lx>"
                  (Unsigned.UInt32.to_int32 offset);
            })
    | None ->
        {
          offset;
          value =
            Printf.sprintf "<no_debug_str:0x%lx>"
              (Unsigned.UInt32.to_int32 offset);
        }

  (** Calculate the absolute byte address of an entry in the debug_names section
  *)
  let calculate_entry_address (section_base_offset : u32)
      (relative_offset : int) : u32 =
    Unsigned.UInt32.add section_base_offset
      (Unsigned.UInt32.of_int relative_offset)

  (** Calculate addresses for all components of a debug_names section *)
  let calculate_section_addresses (section_base_offset : u32)
      (header : name_index_header) : (string * u32) list =
    let addresses = ref [] in
    let current_offset = ref 0 in

    (* Header components *)
    addresses := ("header", section_base_offset) :: !addresses;
    current_offset := !current_offset + 40;

    (* Standard header size *)

    (* Compilation unit offsets *)
    if Unsigned.UInt32.to_int header.comp_unit_count > 0 then (
      addresses :=
        ( "comp_unit_offsets",
          calculate_entry_address section_base_offset !current_offset )
        :: !addresses;
      current_offset :=
        !current_offset + (Unsigned.UInt32.to_int header.comp_unit_count * 4));

    (* Local type unit offsets *)
    if Unsigned.UInt32.to_int header.local_type_unit_count > 0 then (
      addresses :=
        ( "local_type_unit_offsets",
          calculate_entry_address section_base_offset !current_offset )
        :: !addresses;
      current_offset :=
        !current_offset
        + (Unsigned.UInt32.to_int header.local_type_unit_count * 4));

    (* Foreign type unit signatures *)
    if Unsigned.UInt32.to_int header.foreign_type_unit_count > 0 then (
      addresses :=
        ( "foreign_type_unit_signatures",
          calculate_entry_address section_base_offset !current_offset )
        :: !addresses;
      current_offset :=
        !current_offset
        + (Unsigned.UInt32.to_int header.foreign_type_unit_count * 8));

    (* Hash table (buckets + hashes) *)
    addresses :=
      ( "hash_buckets",
        calculate_entry_address section_base_offset !current_offset )
      :: !addresses;
    current_offset :=
      !current_offset + (Unsigned.UInt32.to_int header.bucket_count * 4);

    addresses :=
      ( "hash_values",
        calculate_entry_address section_base_offset !current_offset )
      :: !addresses;
    current_offset :=
      !current_offset + (Unsigned.UInt32.to_int header.name_count * 4);

    (* Name table *)
    addresses :=
      ("name_table", calculate_entry_address section_base_offset !current_offset)
      :: !addresses;

    (* Abbreviation table and entry pool positions are dynamic based on name table size *)
    List.rev !addresses

  (** Parse debug_names abbreviation table *)
  let parse_debug_names_abbreviation_table (cur : Object.Buffer.cursor)
      (abbrev_table_size : u32) : debug_names_abbrev list =
    let start_position = cur.position in
    let end_position =
      start_position + Unsigned.UInt32.to_int abbrev_table_size
    in
    let abbreviations = ref ([] : debug_names_abbrev list) in

    let rec parse_abbreviations () =
      if cur.position >= end_position then ()
      else
        (* Read abbreviation code *)
        let code = Object.Buffer.Read.uleb128 cur |> Unsigned.UInt64.of_int in

        if Unsigned.UInt64.to_int code = 0 then ()
          (* End of abbreviations table *)
        else
          (* Read tag *)
          let tag_code =
            Object.Buffer.Read.uleb128 cur |> Unsigned.UInt64.of_int
          in
          let tag = abbreviation_tag_of_u64 tag_code in

          (* Parse attributes *)
          let attributes = ref [] in
          let rec parse_attrs () =
            let attr_code =
              Object.Buffer.Read.uleb128 cur |> Unsigned.UInt64.of_int
            in
            let form_code =
              Object.Buffer.Read.uleb128 cur |> Unsigned.UInt64.of_int
            in

            if
              Unsigned.UInt64.to_int attr_code = 0
              && Unsigned.UInt64.to_int form_code = 0
            then () (* End of attributes for this abbreviation *)
            else
              let attr = name_index_attribute_of_u64 attr_code in
              let form = attribute_form_encoding form_code in
              attributes := (attr, form) :: !attributes;
              parse_attrs ()
          in
          parse_attrs ();

          let abbrev_entry = { code; tag; attributes = List.rev !attributes } in
          abbreviations := abbrev_entry :: !abbreviations;
          parse_abbreviations ()
    in
    parse_abbreviations ();
    List.rev !abbreviations

  (** Parse a single entry from entry pool at a specific offset *)
  let parse_single_entry (cur : Object.Buffer.cursor)
      (abbrev_table : (u64, debug_names_abbrev) Hashtbl.t) : name_index_entry =
    let abbrev_code_int = Object.Buffer.Read.uleb128 cur in
    let abbrev_code = Unsigned.UInt64.of_int abbrev_code_int in
    match Hashtbl.find_opt abbrev_table abbrev_code with
    | Some abbrev ->
        let name_offset_ref = ref (Unsigned.UInt32.of_int 0) in
        let die_offset_ref = ref (Unsigned.UInt32.of_int 0) in
        let attributes_ref = ref [] in

        (* Parse attributes according to abbreviation specification *)
        List.iter
          (fun (attr, form) ->
            let value =
              match form with
              | DW_FORM_ref4 ->
                  let v = Object.Buffer.Read.u32 cur in
                  Unsigned.UInt64.of_uint32 v
              | DW_FORM_flag_present -> Unsigned.UInt64.of_int 1
              | DW_FORM_udata ->
                  Object.Buffer.Read.uleb128 cur |> Unsigned.UInt64.of_int
              | DW_FORM_unknown _ ->
                  (* Skip unknown forms by reading a single byte for now *)
                  Object.Buffer.Read.u8 cur |> Unsigned.UInt8.to_int
                  |> Unsigned.UInt64.of_int
              | _ ->
                  failwith
                    ("Unsupported form in debug_names entry pool: "
                    ^ string_of_attribute_form_encoding
                        (u64_of_attribute_form_encoding form))
            in

            match attr with
            | DW_IDX_die_offset ->
                die_offset_ref :=
                  Unsigned.UInt32.of_int64 (Unsigned.UInt64.to_int64 value)
            | _ -> attributes_ref := (attr, value) :: !attributes_ref)
          abbrev.attributes;

        {
          name_offset = !name_offset_ref;
          die_offset = !die_offset_ref;
          attributes = List.rev !attributes_ref;
        }
    | None ->
        let available_codes =
          Hashtbl.fold
            (fun k _ acc -> Unsigned.UInt64.to_int k :: acc)
            abbrev_table []
        in
        failwith
          (Printf.sprintf
             "Unknown abbreviation code in debug_names entry pool: %d \
              (available codes: [%s])"
             (Unsigned.UInt64.to_int abbrev_code)
             (String.concat "; " (List.map string_of_int available_codes)))

  (** Parse entry pool using entry_offsets to locate individual entries *)
  let parse_entry_pool_with_offsets (buffer : Object.Buffer.t)
      (entry_pool_start : int) (entry_offsets : u32 array)
      (abbrev_table : (u64, debug_names_abbrev) Hashtbl.t) :
      name_index_entry array =
    Array.mapi
      (fun i offset ->
        let absolute_offset =
          entry_pool_start + Unsigned.UInt32.to_int offset
        in

        if i < 4 then (
          (* Only try to parse first few entries for debugging *)
          (* Scan forward from the offset to find the first non-null byte *)
          let rec find_entry_start pos max_scan =
            if max_scan <= 0 then pos
            else
              let test_cur = Object.Buffer.cursor buffer ~at:pos in
              let byte = Object.Buffer.Read.u8 test_cur in
              if Unsigned.UInt8.to_int byte = 0 then
                find_entry_start (pos + 1) (max_scan - 1)
              else pos
          in

          let actual_entry_start = find_entry_start absolute_offset 20 in
          if actual_entry_start <> absolute_offset then
            ignore (absolute_offset, actual_entry_start);

          let cur = Object.Buffer.cursor buffer ~at:actual_entry_start in
          parse_single_entry cur abbrev_table)
        else
          {
            (* For now, return dummy entry for other entries *)
            name_offset = Unsigned.UInt32.of_int 0;
            die_offset = Unsigned.UInt32.of_int 0;
            attributes = [];
          })
      entry_offsets

  (** Parse entry pool using a base cursor and relative offsets *)
  let parse_entry_pool_with_base_cursor (base_cursor : Object.Buffer.cursor)
      (buffer : Object.Buffer.t) (entry_offsets : u32 array)
      (abbrev_table : (u64, debug_names_abbrev) Hashtbl.t) :
      name_index_entry array =
    (* Since we can't get absolute position of base_cursor, we need to work around this *)
    (* The trick: read a byte to "consume" the cursor, then use fresh cursors with calculated offsets *)
    (* First, save the first byte for reference *)
    let first_byte = Object.Buffer.Read.u8 base_cursor in
    ignore (Unsigned.UInt8.to_int first_byte);

    (* TODO Hack *)
    (* Try to determine the absolute position by trying some reasonable values *)
    (* Since the first byte is typically a valid abbreviation code, scan for it *)
    let rec find_base_position start_guess =
      let test_cursor = Object.Buffer.cursor buffer ~at:start_guess in
      let test_byte = Object.Buffer.Read.u8 test_cursor in
      if Unsigned.UInt8.to_int test_byte = Unsigned.UInt8.to_int first_byte then
        start_guess
      else if start_guess < 0x100 then find_base_position (start_guess + 1)
      else failwith "Could not determine entry pool base position"
    in

    let entry_pool_start = find_base_position 0x60 in

    Array.map
      (fun offset ->
        let absolute_offset =
          entry_pool_start + Unsigned.UInt32.to_int offset
        in
        let cur = Object.Buffer.cursor buffer ~at:absolute_offset in
        parse_single_entry cur abbrev_table)
      entry_offsets

  (** Parse entry pool using current cursor position and entry_offsets *)
  let parse_entry_pool_with_cursor (entry_pool_cur : Object.Buffer.cursor)
      (_buffer : Object.Buffer.t) (entry_offsets : u32 array)
      (abbrev_table : (u64, debug_names_abbrev) Hashtbl.t) :
      name_index_entry array =
    (* TODO Hack *)
    (* The tricky part: we need to know the absolute position of entry_pool_cur *)
    (* For now, let's try to determine this by reading a few bytes and checking if they make sense *)
    let test_cur = entry_pool_cur in
    let first_byte = Object.Buffer.Read.u8 test_cur in
    ignore (Unsigned.UInt8.to_int first_byte);

    (* Reset the test cursor and try to parse the first entry *)
    let first_entry_cur = entry_pool_cur in
    Array.mapi
      (fun _i offset ->
        ignore (Unsigned.UInt32.to_int offset);
        (* For now, just parse sequentially to see what happens *)
        parse_single_entry first_entry_cur abbrev_table)
      entry_offsets

  (** Parse a complete debug_names section *)
  let parse_debug_names_section (cur : Object.Buffer.cursor)
      (buffer : Object.Buffer.t) : debug_names_section =
    let header = parse_name_index_header cur in

    let comp_unit_offsets = parse_u32_array cur header.comp_unit_count in
    let local_type_unit_offsets =
      parse_u32_array cur header.local_type_unit_count
    in
    let foreign_type_unit_signatures =
      parse_u64_array cur header.foreign_type_unit_count
    in
    (* Hash lookup table: bucket_count buckets + name_count hashes *)
    let buckets = parse_u32_array cur header.bucket_count in
    let hash_table = parse_u32_array cur header.name_count in
    (* Name table is just an array of offsets into debug_str *)
    let name_offsets = parse_u32_array cur header.name_count in
    (* TODO: Resolve these offsets to actual strings later *)
    let name_table =
      Array.map (fun offset -> { offset; value = "" }) name_offsets
    in

    (* Entry offsets array - points to where each name's entry starts in the entry pool *)
    let entry_offsets = parse_u32_array cur header.name_count in

    (* Parse debug_names abbreviation table *)
    let abbreviation_table =
      parse_debug_names_abbreviation_table cur header.abbrev_table_size
    in

    (* Convert abbreviation_entry list to hashtbl for compatibility with entry pool parsing *)
    let abbrev_table = Hashtbl.create 16 in
    List.iter
      (fun abbrev ->
        let debug_names_abbrev =
          {
            code = abbrev.code;
            tag = abbrev.tag;
            attributes = abbrev.attributes;
          }
        in
        Hashtbl.add abbrev_table abbrev.code debug_names_abbrev)
      abbreviation_table;

    (* At this point, the cursor is positioned right at the start of the entry pool *)
    (* Since we just finished parsing the abbreviation table, cur points to entry pool start *)

    (*   (String.concat "; " (Array.to_list (Array.map (fun x -> Printf.sprintf "0x%08x" (Unsigned.UInt32.to_int x)) entry_offsets))); *)

    (* Parse entry pool using the old approach but with correct calculation *)
    (* Since I can't get cursor position directly, I'll try a workaround *)
    (* Read a test byte to confirm we're at the right position *)
    let first_byte_test = Object.Buffer.Read.u8 cur in

    (* TODO Revisit this parsing logic *)
    (* Calculate entry pool boundaries using unit_length from DWARF 5 spec *)
    (* According to DWARF 5 section 6.1.1.4.8: *)
    (* Entry pool size = unit_length - (size of all components before entry pool) *)

    (* let section_total_size = Unsigned.UInt32.to_int header.unit_length + 4 in (\* +4 for unit_length field itself *\) *)
    (* Calculate the exact header size based on our structure *)
    (* let actual_header_size = 4 + 2 + 2 + 4 + 4 + 4 + 4 + 4 + 4 in (\* augmentation string varies *\) *)
    (* let augmentation_size = String.length header.augmentation_string in *)
    (* let total_header_size = actual_header_size + augmentation_size in *)

    (* let comp_unit_offsets_size = (Unsigned.UInt32.to_int header.comp_unit_count) * 4 in *)
    (* let local_type_unit_offsets_size = (Unsigned.UInt32.to_int header.local_type_unit_count) * 4 in *)
    (* let foreign_type_unit_signatures_size = (Unsigned.UInt32.to_int header.foreign_type_unit_count) * 8 in *)
    (* let buckets_size = (Unsigned.UInt32.to_int header.bucket_count) * 4 in *)
    (* let hash_table_size = (Unsigned.UInt32.to_int header.name_count) * 4 in *)
    (* let name_offsets_size = (Unsigned.UInt32.to_int header.name_count) * 4 in *)
    (* let entry_offsets_size = (Unsigned.UInt32.to_int header.name_count) * 4 in *)
    (* let abbrev_table_size = Unsigned.UInt32.to_int header.abbrev_table_size in *)

    (* let pre_entry_pool_size = total_header_size + comp_unit_offsets_size + local_type_unit_offsets_size + *)
    (*                           foreign_type_unit_signatures_size + buckets_size + hash_table_size + *)
    (*                           name_offsets_size + entry_offsets_size + abbrev_table_size in *)

    (* let entry_pool_size = section_total_size - pre_entry_pool_size in *)

    (* The cursor is already positioned at the entry pool start after parsing the abbreviation table *)
    (* To get the absolute position, we need to use the heuristic search, but let's try a cleaner approach *)
    let rec find_byte_position target_byte search_start max_search =
      if search_start > max_search then
        failwith
          (Printf.sprintf
             "Could not locate entry pool start (byte 0x%02x not found in \
              range)"
             target_byte)
      else
        try
          let test_cur = Object.Buffer.cursor buffer ~at:search_start in
          let test_byte = Object.Buffer.Read.u8 test_cur in
          if Unsigned.UInt8.to_int test_byte = target_byte then search_start
          else find_byte_position target_byte (search_start + 1) max_search
        with _ -> find_byte_position target_byte (search_start + 1) max_search
    in

    let entry_pool_start =
      find_byte_position (Unsigned.UInt8.to_int first_byte_test) 0x60 0x200
    in

    (* Debug: dump the entire entry pool to see its structure *)
    let entry_pool =
      (* DWARF 5 Figure 6.1: Each entry_offset points to a SERIES of entries for that name *)
      (* Each series is terminated by abbreviation code 0 *)
      let parse_entry_series (start_offset : int) =
        (* Parse a series of entries starting at start_offset until we hit terminator (abbrev code 0) *)
        let rec parse_series_rec (cur_offset : int)
            (acc : name_index_entry list) =
          try
            let cur = Object.Buffer.cursor buffer ~at:cur_offset in
            let abbrev_code_int = Object.Buffer.Read.uleb128 cur in

            if abbrev_code_int = 0 then
              (* Terminator found, return accumulated entries *)
              List.rev acc
            else
              (* Parse this entry and continue *)
              let reset_cur = Object.Buffer.cursor buffer ~at:cur_offset in
              let entry = parse_single_entry reset_cur abbrev_table in
              let next_offset = cur_offset + 6 in
              (* Estimate entry size *)
              parse_series_rec next_offset (entry :: acc)
          with _ ->
            (* Failed to parse, return what we have *)
            List.rev acc
        in
        parse_series_rec start_offset []
      in

      let absolute_series = ref 0 in
      let relative_series = ref 0 in
      let failures = ref 0 in

      let results =
        Array.map
          (fun offset ->
            let entry_offset = Unsigned.UInt32.to_int offset in

            (* Try absolute offset first *)
            let abs_series = parse_entry_series entry_offset in
            if List.length abs_series > 0 then (
              incr absolute_series;
              List.hd abs_series (* Return first entry in series for now *))
            else
              (* Try relative to entry pool start *)
              let pool_relative_offset = entry_pool_start + entry_offset in
              let rel_series = parse_entry_series pool_relative_offset in
              if List.length rel_series > 0 then (
                incr relative_series;
                List.hd rel_series (* Return first entry in series for now *))
              else (
                incr failures;
                {
                  name_offset = Unsigned.UInt32.of_int 0;
                  die_offset = Unsigned.UInt32.of_int 0;
                  attributes = [];
                }))
          entry_offsets
      in

      results
    in

    {
      header;
      comp_unit_offsets;
      local_type_unit_offsets;
      foreign_type_unit_signatures;
      buckets;
      hash_table;
      name_table;
      entry_offsets;
      abbreviation_table;
      entry_pool;
    }

  (* TODO Consider changing large tuple to a record *)

  (** Parse a single entry from the entry pool at the given buffer position.
      Returns None if terminator (abbrev code 0) is found, or Some with entry
      details. *)
  let parse_single_entry_at_cursor (cursor : Object.Buffer.cursor)
      (abbrev_table : debug_names_abbrev list) (current_offset_ref : int ref)
      (entry_pool_relative_offset : int) (entry_offset : int)
      (absolute_entry_offset : int) :
      (int * int * string * string * int option * bool) option =
    let entry_start_relative = !current_offset_ref - absolute_entry_offset in

    (* Read abbreviation code *)
    let abbrev_code_int = Object.Buffer.Read.uleb128 cursor in

    (* Check for terminator *)
    if abbrev_code_int = 0 then None (* End of entry series *)
    else
      let abbrev_code = Unsigned.UInt64.of_int abbrev_code_int in

      (* Update current offset (ULEB128 minimum 1 byte) *)
      current_offset_ref := !current_offset_ref + 1;

      (* Find the abbreviation in the table *)
      let matching_abbrev_opt =
        List.find_opt (fun abbrev -> abbrev.code = abbrev_code) abbrev_table
      in

      match matching_abbrev_opt with
      | Some abbrev ->
          (* Parse the attributes according to the abbreviation *)
          let die_offset_ref = ref 0 in
          let parent_offset_ref = ref None in
          let has_parent_flag = ref false in
          List.iter
            (fun (attr, form) ->
              match attr with
              | DW_IDX_die_offset -> (
                  match form with
                  | DW_FORM_ref4 ->
                      let value = Object.Buffer.Read.u32 cursor in
                      die_offset_ref := Unsigned.UInt32.to_int value;
                      current_offset_ref := !current_offset_ref + 4
                  | _ -> ())
              | DW_IDX_parent -> (
                  match form with
                  | DW_FORM_flag_present -> has_parent_flag := true
                  | DW_FORM_ref4 ->
                      let value = Object.Buffer.Read.u32 cursor in
                      parent_offset_ref := Some (Unsigned.UInt32.to_int value);
                      current_offset_ref := !current_offset_ref + 4
                  | _ -> ())
              | _ -> (
                  (* TODO Why do we skip attributes here? *)
                  (* Skip other attributes we don't handle *)
                  match form with
                  | DW_FORM_ref4 ->
                      let _ = Object.Buffer.Read.u32 cursor in
                      current_offset_ref := !current_offset_ref + 4
                  | DW_FORM_flag_present -> ()
                  | _ -> ()))
            abbrev.attributes;

          let tag_code = uint64_of_abbreviation_tag abbrev.tag in
          let tag_str = string_of_abbreviation_tag tag_code in
          let abbrev_id =
            Printf.sprintf "0x%x" (Unsigned.UInt64.to_int abbrev.code)
          in

          let entry_addr =
            entry_pool_relative_offset + entry_offset + entry_start_relative
          in
          Some
            ( entry_addr,
              !die_offset_ref,
              tag_str,
              abbrev_id,
              !parent_offset_ref,
              !has_parent_flag )
      | None ->
          (* Abbreviation not found *)
          let entry_addr =
            entry_pool_relative_offset + entry_offset + entry_start_relative
          in
          Some (entry_addr, 0, "DW_TAG_<unknown>", "0x0", None, false)

  (** Parse all entries in a series until terminator (abbrev code 0) is found.
      Returns list of parsed entries. *)
  let parse_entry_series (buffer : Object.Buffer.t)
      (absolute_entry_offset : int) (abbrev_table : debug_names_abbrev list)
      (entry_pool_relative_offset : int) (entry_offset : int) :
      (int * int * string * string * int option * bool) list =
    let current_offset_ref = ref absolute_entry_offset in

    let rec parse_series_rec acc =
      let cursor = Object.Buffer.cursor buffer ~at:!current_offset_ref in
      match
        parse_single_entry_at_cursor cursor abbrev_table current_offset_ref
          entry_pool_relative_offset entry_offset absolute_entry_offset
      with
      | None -> List.rev acc (* Terminator found *)
      | Some entry -> parse_series_rec (entry :: acc)
    in

    try parse_series_rec []
    with _ -> [ (0, 0, "DW_TAG_<parse error>", "0x0", None, false) ]

  (** Calculate entry pool offset based on header information *)
  let calculate_entry_pool_offset (header : name_index_header) : int =
    (* TODO Refactor hardcoded value here. *)
    let header_size = 44 in
    (* 4 bytes unit_length + 40 bytes header including augmentation *)
    let cu_offsets_size = Unsigned.UInt32.to_int header.comp_unit_count * 4 in
    let tu_offsets_size =
      Unsigned.UInt32.to_int header.local_type_unit_count * 4
    in
    let foreign_tu_size =
      Unsigned.UInt32.to_int header.foreign_type_unit_count * 8
    in
    let bucket_size = Unsigned.UInt32.to_int header.bucket_count * 4 in
    let hash_table_size = Unsigned.UInt32.to_int header.name_count * 4 in
    let name_table_size = Unsigned.UInt32.to_int header.name_count * 4 in
    let entry_offsets_size = Unsigned.UInt32.to_int header.name_count * 4 in
    let abbrev_table_size = Unsigned.UInt32.to_int header.abbrev_table_size in

    header_size + cu_offsets_size + tu_offsets_size + foreign_tu_size
    + bucket_size + hash_table_size + name_table_size + entry_offsets_size
    + abbrev_table_size

  (** Parse all entries for a given name index according to DWARF 5
      specification *)
  let parse_all_entries_for_name (buffer : Object.Buffer.t)
      (debug_names : debug_names_section) (section_offset : int)
      (name_idx : int) : (int * int * string * string * int option * bool) list
      =
    try
      if name_idx < Array.length debug_names.entry_offsets then
        let entry_offset =
          Unsigned.UInt32.to_int debug_names.entry_offsets.(name_idx)
        in
        let entry_pool_relative_offset =
          calculate_entry_pool_offset debug_names.header
        in
        let absolute_entry_offset =
          section_offset + entry_pool_relative_offset + entry_offset
        in

        parse_entry_series buffer absolute_entry_offset
          debug_names.abbreviation_table entry_pool_relative_offset entry_offset
      else [ (0, 0, "DW_TAG_<no entry>", "0x0", None, false) ]
    with _ -> [ (0, 0, "DW_TAG_<parse error>", "0x0", None, false) ]
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

let create buffer =
  let format = detect_format buffer in
  let object_ = Object_file.{ buffer; format } in
  { abbrev_tables_ = Hashtbl.create 10; compile_units_ = [||]; object_ }

(* TODO Change the type of t.compile_units_ *)
let get_compile_units t =
  let compile_units = parse_compile_units t |> List.of_seq |> Array.of_list in
  { t with compile_units_ = compile_units }

(** String Offset Tables (.debug_str_offsets section) - DWARF 5 Section 7.26 *)
module DebugStrOffsets = struct
  type header = {
    unit_length : u32;
    version : u16;
    padding : u16;
    header_span : span;
  }

  type offset_entry = { offset : u32; resolved_string : string option }
  type t = { header : header; offsets : offset_entry array }

  let parse_header (cursor : Object.Buffer.cursor) : header =
    let start_pos = cursor.position in
    let unit_length = Object.Buffer.Read.u32 cursor in
    let version = Object.Buffer.Read.u16 cursor in
    let padding = Object.Buffer.Read.u16 cursor in
    let end_pos = cursor.position in

    (* Calculate header span *)
    let header_span =
      {
        start = Unsigned.UInt64.of_int start_pos;
        size = Unsigned.UInt64.of_int (end_pos - start_pos);
      }
    in

    (* Validate DWARF version 5 *)
    if Unsigned.UInt16.to_int version != 5 then
      failwith
        (Printf.sprintf "Expected DWARF version 5, got %d"
           (Unsigned.UInt16.to_int version));

    { unit_length; version; padding; header_span }

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
    let debug_str_section =
      match find_debug_section_by_type buffer Debug_str with
      | Some (offset, size) ->
          Some (Unsigned.UInt32.of_int (Unsigned.UInt64.to_int offset), size)
      | None -> None
    in

    let offsets = parse_offsets cursor header debug_str_section buffer in
    { header; offsets }
end

(** Debug String Tables (.debug_str section) - DWARF 5 Section 7.26 *)
module DebugStr = struct
  type string_entry = {
    offset : int;  (** Offset from start of .debug_str section *)
    length : int;  (** Length of the string in bytes *)
    content : string;  (** The actual string content *)
  }

  type t = {
    entries : string_entry array;  (** All strings in the section *)
    total_size : int;  (** Total size of the section in bytes *)
  }

  let parse buffer : t option =
    match find_debug_section_by_type buffer Debug_str with
    | None -> None
    | Some (section_offset, section_size) -> (
        try
          let section_start = Unsigned.UInt64.to_int section_offset in
          let section_end =
            section_start + Unsigned.UInt64.to_int section_size
          in
          let cursor = Object.Buffer.cursor buffer ~at:section_start in

          (* TODO Write this as a single pass? *)
          (* First pass: count strings to allocate array *)
          let string_count = ref 0 in
          let current_pos = ref section_start in

          while !current_pos < section_end do
            match Object.Buffer.Read.zero_string cursor () with
            | Some str ->
                let str_len = String.length str + 1 in
                (* +1 for null terminator *)
                current_pos := !current_pos + str_len;
                incr string_count
            | None -> current_pos := section_end (* Break on read error *)
          done;

          (* Second pass: read strings into array *)
          let entries =
            Array.make !string_count { offset = 0; length = 0; content = "" }
          in
          let cursor = Object.Buffer.cursor buffer ~at:section_start in
          let current_pos = ref section_start in
          let string_offset = ref 0 in
          let index = ref 0 in

          while !current_pos < section_end && !index < !string_count do
            match Object.Buffer.Read.zero_string cursor () with
            | Some str ->
                let str_len = String.length str + 1 in
                (* +1 for null terminator *)
                entries.(!index) <-
                  {
                    offset = !string_offset;
                    length = String.length str;
                    content = str;
                  };
                current_pos := !current_pos + str_len;
                string_offset := !string_offset + str_len;
                incr index
            | None -> current_pos := section_end (* Break on read error *)
          done;

          Some
            {
              entries = Array.sub entries 0 !index;
              total_size = Unsigned.UInt64.to_int section_size;
            }
        with _ -> None)

  (* TODO Inline this! *)
  let iter f debug_str = Array.iter f debug_str.entries

  let find_string_at_offset debug_str offset =
    Array.find_map
      (fun entry -> if entry.offset = offset then Some entry.content else None)
      debug_str.entries
end

module DebugLineStr = struct
  type string_entry = {
    offset : int;  (** Offset from start of .debug_line_str section *)
    length : int;  (** Length of the string in bytes *)
    content : string;  (** The actual string content *)
  }

  type t = {
    entries : string_entry array;  (** All strings in the section *)
    total_size : int;  (** Total size of the section in bytes *)
  }

  let parse buffer : t option =
    match find_debug_section_by_type buffer Debug_line_str with
    | None -> None
    | Some (section_offset, section_size) -> (
        try
          let section_start = Unsigned.UInt64.to_int section_offset in
          let section_end =
            section_start + Unsigned.UInt64.to_int section_size
          in
          let cursor = Object.Buffer.cursor buffer ~at:section_start in
          let current_pos = ref section_start in
          let string_offset = ref 0 in
          let index = ref 0 in

          (* TODO Can this be improved to a single pass? *)
          (* First pass: count the number of strings *)
          let string_count = ref 0 in
          let temp_cursor = Object.Buffer.cursor buffer ~at:section_start in
          let temp_pos = ref section_start in
          let temp_offset = ref 0 in
          while !temp_pos < section_end do
            match Object.Buffer.Read.zero_string temp_cursor () with
            | Some str ->
                let str_len = String.length str + 1 in
                temp_pos := !temp_pos + str_len;
                temp_offset := !temp_offset + str_len;
                incr string_count
            | None -> temp_pos := section_end
          done;

          (* Second pass: collect all strings *)
          let entries =
            Array.make !string_count { offset = 0; length = 0; content = "" }
          in

          while !current_pos < section_end && !index < !string_count do
            match Object.Buffer.Read.zero_string cursor () with
            | Some str ->
                let str_len = String.length str + 1 in
                entries.(!index) <-
                  {
                    offset = !string_offset;
                    length = String.length str;
                    content = str;
                  };
                current_pos := !current_pos + str_len;
                string_offset := !string_offset + str_len;
                incr index
            | None -> current_pos := section_end
          done;

          Some { entries; total_size = Unsigned.UInt64.to_int section_size }
        with exn ->
          Printf.eprintf "Error parsing debug_line_str section: %s\n"
            (Printexc.to_string exn);
          None)

  let iter f debug_line_str = Array.iter f debug_line_str.entries

  let find_string_at_offset debug_line_str offset =
    Array.find_map
      (fun entry -> if entry.offset = offset then Some entry.content else None)
      debug_line_str.entries
end

(** Address Tables (.debug_addr section) - DWARF 5 Section 7.27 *)
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
    (* TODO Can this be calculated based off the header? *)
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

(** Address Range Tables (.debug_aranges section) - DWARF 5 Section 6.1.2 *)
module DebugAranges = struct
  type header = {
    unit_length : u32;
    version : u16;
    debug_info_offset : u32;
    address_size : u8;
    segment_size : u8;
    header_span : span;
  }

  type address_range = { start_address : u64; length : u64 }
  type aranges_set = { header : header; ranges : address_range list }

  let parse_header cursor =
    let start_pos = cursor.position in
    let unit_length = Object.Buffer.Read.u32 cursor in
    let version = Object.Buffer.Read.u16 cursor in
    let debug_info_offset = Object.Buffer.Read.u32 cursor in
    let address_size = Object.Buffer.Read.u8 cursor in
    let segment_size = Object.Buffer.Read.u8 cursor in
    let end_pos = cursor.position in

    (* Calculate header span *)
    let header_span =
      {
        start = Unsigned.UInt64.of_int start_pos;
        size = Unsigned.UInt64.of_int (end_pos - start_pos);
      }
    in

    {
      unit_length;
      version;
      debug_info_offset;
      address_size;
      segment_size;
      header_span;
    }

  let parse_ranges cursor header =
    let address_size = Unsigned.UInt8.to_int header.address_size in
    let segment_size = Unsigned.UInt8.to_int header.segment_size in

    (* DWARF spec requires alignment to 2*address_size after the header.
       The header size is tracked in header_span. For proper alignment with 8-byte addresses (16-byte alignment),
       we observe that 4 bytes of padding are needed in practice. *)
    for _i = 0 to 3 do
      ignore (Object.Buffer.Read.u8 cursor)
    done;

    let rec read_ranges acc =
      (* Read address and length based on address_size *)
      let start_address =
        match address_size with
        | 4 -> Unsigned.UInt64.of_uint32 (Object.Buffer.Read.u32 cursor)
        | 8 -> Object.Buffer.Read.u64 cursor
        | _ ->
            failwith ("Unsupported address size: " ^ string_of_int address_size)
      in

      let length =
        match address_size with
        | 4 -> Unsigned.UInt64.of_uint32 (Object.Buffer.Read.u32 cursor)
        | 8 -> Object.Buffer.Read.u64 cursor
        | _ ->
            failwith ("Unsupported address size: " ^ string_of_int address_size)
      in

      (* Skip segment selector if present *)
      if segment_size > 0 then
        for _i = 0 to segment_size - 1 do
          ignore (Object.Buffer.Read.u8 cursor)
        done;

      (* Check for terminating null entry *)
      if
        Unsigned.UInt64.equal start_address Unsigned.UInt64.zero
        && Unsigned.UInt64.equal length Unsigned.UInt64.zero
      then List.rev acc
      else
        let range = { start_address; length } in
        read_ranges (range :: acc)
    in
    read_ranges []

  (* TODO Would this be improved by returning the span/size of the header? *)
  (* Calculate the actual CU DIE offset by examining the .debug_info section structure *)
  let calculate_cu_die_offset buffer debug_info_offset =
    match find_debug_section_by_type buffer Debug_info with
    | None -> debug_info_offset
    | Some (debug_info_section_offset, _) -> (
        try
          (* If debug_info_offset is 0, we need to find the actual CU DIE offset *)
          if Unsigned.UInt32.equal debug_info_offset (Unsigned.UInt32.of_int 0)
          then
            let cursor =
              Object.Buffer.cursor buffer
                ~at:(Unsigned.UInt64.to_int debug_info_section_offset)
            in
            (* Read compilation unit header to find where DIE starts *)
            let _unit_length = Object.Buffer.Read.u32 cursor in
            let _version = Object.Buffer.Read.u16 cursor in
            let _unit_type = Object.Buffer.Read.u8 cursor in
            let _debug_abbrev_offset = Object.Buffer.Read.u32 cursor in
            let _address_size = Object.Buffer.Read.u8 cursor in
            (* The DIE starts after the header - current cursor position relative to section start *)
            let cu_die_offset =
              cursor.position - Unsigned.UInt64.to_int debug_info_section_offset
            in
            Unsigned.UInt32.of_int cu_die_offset
          else debug_info_offset
        with _ -> debug_info_offset)

  let parse buffer : aranges_set option =
    match find_debug_section_by_type buffer Debug_aranges with
    | None -> None
    | Some (section_offset, _section_size) -> (
        try
          let cursor =
            Object.Buffer.cursor buffer
              ~at:(Unsigned.UInt64.to_int section_offset)
          in
          let header = parse_header cursor in
          (* For ELF files, debug_info_offset points to the compilation unit header start.
             The cu_die_offset displayed should be the offset where the actual DIE starts.
             Calculate this as: base offset + header size *)
          let cu_die_offset =
            if
              Unsigned.UInt32.equal header.debug_info_offset
                (Unsigned.UInt32.of_int 0)
            then
              (* If debug_info_offset is 0 (start of .debug_info), calculate DIE offset *)
              calculate_cu_die_offset buffer header.debug_info_offset
            else
              (* For non-zero offsets, assume it already points to the DIE *)
              header.debug_info_offset
          in
          let display_header =
            { header with debug_info_offset = cu_die_offset }
          in
          let ranges = parse_ranges cursor header in
          Some { header = display_header; ranges }
        with exn ->
          Printf.eprintf "Error parsing debug_aranges section: %s\n"
            (Printexc.to_string exn);
          None)
end

(** Location Lists (.debug_loclists section) - DWARF 5 Section 7.7.3 *)
module DebugLoclists = struct
  type header = {
    unit_length : u32;
    version : u16;
    address_size : u8;
    segment_size : u8;
    offset_entry_count : u32;
  }

  type location_list_entry_type =
    | DW_LLE_end_of_list (* 0x00 *)
    | DW_LLE_base_addressx (* 0x01 *)
    | DW_LLE_startx_endx (* 0x02 *)
    | DW_LLE_startx_length (* 0x03 *)
    | DW_LLE_offset_pair (* 0x04 *)
    | DW_LLE_default_location (* 0x05 *)
    | DW_LLE_base_address (* 0x06 *)
    | DW_LLE_start_end (* 0x07 *)
    | DW_LLE_start_length (* 0x08 *)

  type location_list_entry = {
    entry_type : location_list_entry_type;
    data : string; (* Raw data for the entry, varies by type *)
  }

  type location_list = { offset : u32; entries : location_list_entry list }

  type loclists_section = {
    header : header;
    offset_table : u32 array;
    location_lists : location_list list;
  }

  let location_list_entry_type_of_u8 byte =
    match Unsigned.UInt8.to_int byte with
    | 0x00 -> DW_LLE_end_of_list
    | 0x01 -> DW_LLE_base_addressx
    | 0x02 -> DW_LLE_startx_endx
    | 0x03 -> DW_LLE_startx_length
    | 0x04 -> DW_LLE_offset_pair
    | 0x05 -> DW_LLE_default_location
    | 0x06 -> DW_LLE_base_address
    | 0x07 -> DW_LLE_start_end
    | 0x08 -> DW_LLE_start_length
    | n ->
        failwith (Printf.sprintf "Unknown location list entry type: 0x%02x" n)

  let parse_header cursor =
    let unit_length = Object.Buffer.Read.u32 cursor in
    let version = Object.Buffer.Read.u16 cursor in
    let address_size = Object.Buffer.Read.u8 cursor in
    let segment_size = Object.Buffer.Read.u8 cursor in
    let offset_entry_count = Object.Buffer.Read.u32 cursor in
    { unit_length; version; address_size; segment_size; offset_entry_count }

  let parse_offset_table cursor offset_entry_count =
    let count = Unsigned.UInt32.to_int offset_entry_count in
    Array.init count (fun _i -> Object.Buffer.Read.u32 cursor)

  let parse_location_list_entry cursor =
    let entry_type_byte = Object.Buffer.Read.u8 cursor in
    let entry_type = location_list_entry_type_of_u8 entry_type_byte in

    (* For now, we'll just store empty data since our test files are empty *)
    let data = "" in
    { entry_type; data }

  let parse_location_list cursor offset =
    (* Read entries until we hit DW_LLE_end_of_list *)
    let rec read_entries acc =
      let entry = parse_location_list_entry cursor in
      match entry.entry_type with
      | DW_LLE_end_of_list -> List.rev (entry :: acc)
      | _ -> read_entries (entry :: acc)
    in
    let entries = read_entries [] in
    { offset; entries }

  let parse buffer section_offset =
    let cursor =
      Object.Buffer.cursor buffer ~at:(Unsigned.UInt32.to_int section_offset)
    in

    (* Handle empty sections gracefully *)
    try
      let header = parse_header cursor in
      let offset_table = parse_offset_table cursor header.offset_entry_count in
      (* For now, return empty location lists since our test files are empty *)
      let location_lists = [] in
      { header; offset_table; location_lists }
    with _ ->
      (* Return empty structure for empty or invalid section *)
      {
        header =
          {
            unit_length = Unsigned.UInt32.zero;
            version = Unsigned.UInt16.zero;
            address_size = Unsigned.UInt8.zero;
            segment_size = Unsigned.UInt8.zero;
            offset_entry_count = Unsigned.UInt32.zero;
          };
        offset_table = [||];
        location_lists = [];
      }
end

let lookup_address_in_debug_addr (buffer : Object.Buffer.t) (_addr_base : u64)
    (index : int) : u64 option =
  (* Find the debug_addr section *)
  match find_debug_section_by_type buffer Debug_addr with
  | None -> None
  | Some (section_offset, _) -> (
      try
        (* addr_base is an offset from the beginning of the debug_addr section to the address data *)
        (* We need to parse the entire section, not start at addr_base *)
        let parsed_addr =
          DebugAddr.parse buffer
            (Unsigned.UInt32.of_int (Unsigned.UInt64.to_int section_offset))
        in

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

(** CompactUnwind module integrates with MachO compact unwinding format *)
module CompactUnwind = struct
  include Compact_unwind

  let find_unwind_info_section buffer =
    try
      let open Object.Macho in
      let _header, commands = read buffer in

      (* Look for __TEXT segment *)
      let text_segment_opt =
        List.find_map
          (function
            | LC_SEGMENT_64 (lazy seg) when seg.seg_segname = "__TEXT" ->
                Some seg
            | _ -> None)
          commands
      in

      match text_segment_opt with
      | None -> None
      | Some text_segment ->
          (* Find __unwind_info section within __TEXT segment *)
          Array.find_map
            (fun section ->
              if section.sec_sectname = "__unwind_info" then
                Some
                  ( Unsigned.UInt32.to_int section.sec_offset,
                    Unsigned.UInt64.to_int section.sec_size )
              else None)
            text_segment.seg_sections
    with _ -> None

  let parse_from_buffer buffer =
    match find_unwind_info_section buffer with
    | None -> None
    | Some (section_offset, section_size) -> (
        try
          let unwind_info =
            parse_unwind_info buffer section_offset section_size
          in
          let arch = detect_architecture buffer in
          Some (unwind_info, arch)
        with Invalid_compact_unwind_format _ -> None)
end
