open Dwarf_types

(** DWARF 5 unit type encodings.

    Each compilation unit header begins with a unit type that identifies the
    purpose and structure of the unit. The unit type determines which sections
    the unit uses and how the unit's header is formatted.

    DWARF 5 specification, Table 7.2 "Unit header unit type encodings". *)
type unit_type =
  | DW_UT_compile  (** A normal compilation unit *)
  | DW_UT_type  (** A type unit *)
  | DW_UT_partial  (** A partial unit *)
  | DW_UT_skeleton  (** A skeleton compilation unit *)
  | DW_UT_split_compile  (** A split compilation unit *)
  | DW_UT_split_type  (** A split type unit *)
  | DW_UT_lo_user  (** Reserved for user-defined types *)
  | DW_UT_hi_user  (** Reserved for user-defined types *)

val unit_type : u8 -> unit_type
(** Convert a u8 value to a unit_type

    @raise Parse_error if the byte is not a recognized DWARF unit type. *)

val u8_of_unit_type : unit_type -> u8
(** Convert a [unit_type] to its u8 encoding. *)

val string_of_unit_type : unit_type -> string
(** Convert a unit_type to its string representation *)

(** DWARF format discriminator.

    DWARF 5 specification, section 7.4 "32-Bit and 64-Bit DWARF Formats". *)
type dwarf_format =
  | DWARF32  (** 32-bit format with 4-byte lengths and offsets. *)
  | DWARF64  (** 64-bit format with 8-byte lengths and offsets. *)

val dwarf_format : int -> dwarf_format
(** Convert an offset size in bytes to the corresponding DWARF format. The
    inverse of {!offset_size_for_format}.

    @raise Parse_error if the size is neither 4 nor 8. *)

val string_of_dwarf_format : dwarf_format -> string
(** Convert a dwarf_format to its string representation. *)

type encoding = {
  format : dwarf_format;
  address_size : u8;  (** Size of addresses in bytes (typically 8 for 64-bit) *)
  version : u16;  (** DWARF version number *)
}
(** Encoding parameters that affect how DWARF data is parsed.

    DWARF 5 specification, section 7.5.1 "Unit Headers". *)

val offset_size_for_format : dwarf_format -> int
(** The size in bytes of an offset or length in the given format: 4 for
    {!DWARF32}, 8 for {!DWARF64}. Use it to compute the width of section-offset
    fields; it is the inverse of {!val:dwarf_format}. *)

(** Abbreviation tag encoding.

    The abbreviations table for a single compilation unit consists of a series
    of abbreviation declarations. Each declaration specifies the tag and
    attributes for a particular form of Debugging Information Entry (DIE).

    DWARF 5 specification, Table 7.3 "Tag encodings". *)
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
  | DW_TAG_GNU_template_template_param
  | DW_TAG_GNU_template_parameter_pack
  | DW_TAG_GNU_formal_parameter_pack
  | DW_TAG_GNU_call_site
  | DW_TAG_GNU_call_site_parameter
  | DW_TAG_lo_user
  | DW_TAG_hi_user
  | DW_TAG_unknown of int
      (** A value not in this enumeration, e.g. a vendor extension; carries the
          raw code. *)

val abbreviation_tag : u64 -> abbreviation_tag
(** Convert a u64 to an [abbreviation_tag].

    Unrecognised values decode to [DW_TAG_unknown]. *)

val u64_of_abbreviation_tag : abbreviation_tag -> u64
(** Convert an [abbreviation_tag] to its u64 tag encoding. *)

val string_of_abbreviation_tag : abbreviation_tag -> string
(** Convert an [abbreviation_tag] to a string representation.

    Follows the format of the abbreviation_tag type variants. *)

(** The encodings for the child determination byte.

    DWARF 5 specification, Table 7.4 "Child determination encodings". *)
type children_determination = DW_CHILDREN_no | DW_CHILDREN_yes

val children_determination : int -> children_determination
(** Convert int to [children_determination].

    @raise Parse_error
      if the value is not a recognized child determination code. *)

val int_of_children_determination : children_determination -> int
(** Convert [children_determination] to int. *)

val string_of_children_determination : children_determination -> string
(** Convert [children_determination] to a string representation.

    Follows the format of the children_determination type variants.*)

(** The encodings for the attribute names.

    DWARF 5 specification, Table 7.5 "Attribute encodings". *)
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
  (* DWARF 6 *)
  | DW_AT_language_name
  | DW_AT_language_version
  | DW_AT_lo_user
  | DW_AT_hi_user
  (* LLVM and Apple extensions *)
  | DW_AT_LLVM_sysroot
  | DW_AT_APPLE_omit_frame_ptr
  | DW_AT_APPLE_sdk
  | DW_AT_unknown of int
      (** A value not in this enumeration, e.g. a vendor extension; carries the
          raw code. *)

val string_of_attribute_encoding : attribute_encoding -> string
(** Convert an [attribute_encoding] to its string representation.

    Follows the format of the attribute_encoding type variants. *)

val attribute_encoding : u64 -> attribute_encoding
(** Convert a u64 to an [attribute_encoding].

    Unrecognised values decode to [DW_AT_unknown]. *)

val u64_of_attribute_encoding : attribute_encoding -> u64
(** Convert an attribute_encoding to its numeric DWARF code. *)

(** Attribute form encoding.

    DWARF 5 specification, Table 7.6 "Attribute form encodings". *)
type attribute_form_encoding =
  | DW_FORM_addr  (** Target-address-sized address *)
  | DW_FORM_block2  (** Block with a 2-byte length prefix *)
  | DW_FORM_block4  (** Block with a 4-byte length prefix *)
  | DW_FORM_data2  (** 2-byte constant *)
  | DW_FORM_data4  (** 4-byte constant *)
  | DW_FORM_data8  (** 8-byte constant *)
  | DW_FORM_string  (** Inline null-terminated string *)
  | DW_FORM_block  (** Block with a ULEB128 length prefix *)
  | DW_FORM_block1  (** Block with a 1-byte length prefix *)
  | DW_FORM_data1  (** 1-byte constant *)
  | DW_FORM_flag  (** 1-byte boolean flag *)
  | DW_FORM_sdata  (** SLEB128 signed constant *)
  | DW_FORM_strp  (** Offset of a string in .debug_str *)
  | DW_FORM_udata  (** ULEB128 unsigned constant *)
  | DW_FORM_ref_addr  (** Offset-sized reference to any unit's DIE *)
  | DW_FORM_ref1  (** 1-byte reference within the current unit *)
  | DW_FORM_ref2  (** 2-byte reference within the current unit *)
  | DW_FORM_ref4  (** 4-byte reference within the current unit *)
  | DW_FORM_ref8  (** 8-byte reference within the current unit *)
  | DW_FORM_ref_udata  (** ULEB128 reference within the current unit *)
  | DW_FORM_indirect  (** Form is itself given by a ULEB128 in the data *)
  | DW_FORM_sec_offset  (** Offset-sized offset into a DWARF section *)
  | DW_FORM_exprloc  (** DWARF expression with a ULEB128 length prefix *)
  | DW_FORM_flag_present  (** Implicit true flag; no value stored *)
  (* New in DWARF Version 5 *)
  | DW_FORM_strx  (** ULEB128 index into .debug_str_offsets *)
  | DW_FORM_addrx  (** ULEB128 index into .debug_addr *)
  | DW_FORM_ref_sup4  (** 4-byte reference into the supplementary .debug_info *)
  | DW_FORM_strp_sup  (** Offset of a string in the supplementary .debug_str *)
  | DW_FORM_data16  (** 16-byte constant (e.g. an MD5 digest) *)
  | DW_FORM_line_strp  (** Offset of a string in .debug_line_str *)
  | DW_FORM_ref_sig8  (** 8-byte type-unit signature *)
  | DW_FORM_implicit_const
      (** Constant stored in the abbreviation, not the DIE *)
  | DW_FORM_loclistx  (** ULEB128 index into .debug_loclists *)
  | DW_FORM_rnglistx  (** ULEB128 index into .debug_rnglists *)
  | DW_FORM_ref_sup8  (** 8-byte reference into the supplementary .debug_info *)
  | DW_FORM_strx1  (** 1-byte index into .debug_str_offsets *)
  | DW_FORM_strx2  (** 2-byte index into .debug_str_offsets *)
  | DW_FORM_strx3  (** 3-byte index into .debug_str_offsets *)
  | DW_FORM_strx4  (** 4-byte index into .debug_str_offsets *)
  | DW_FORM_addrx1  (** 1-byte index into .debug_addr *)
  | DW_FORM_addrx2  (** 2-byte index into .debug_addr *)
  | DW_FORM_addrx3  (** 3-byte index into .debug_addr *)
  | DW_FORM_addrx4  (** 4-byte index into .debug_addr *)
  (* GNU extensions *)
  | DW_FORM_GNU_addr_index  (** GNU Fission: ULEB128 index into .debug_addr *)
  | DW_FORM_GNU_str_index
      (** GNU Fission: ULEB128 index into .debug_str_offsets *)
  | DW_FORM_GNU_ref_alt  (** GNU: offset into alternate .debug_info *)
  | DW_FORM_GNU_strp_alt  (** GNU: offset into alternate .debug_str *)
  | DW_FORM_unknown of int  (** Unknown or vendor-specific forms *)

val string_of_attribute_form_encoding : attribute_form_encoding -> string
(** Convert an [attribute_form_encoding] to its string representation. *)

val attribute_form_encoding : u64 -> attribute_form_encoding
(** Convert a u64 to an [attribute_form_encoding]. *)

val u64_of_attribute_form_encoding : attribute_form_encoding -> u64
(** Convert an attribute_form_encoding to an u64 *)

(** DWARF expression operations

    DWARF 5 specification, Table 7.9 "DWARF operation encodings". *)
type operation_encoding =
  | DW_OP_addr  (** Push a constant target-address-sized address. *)
  | DW_OP_deref
      (** Pop an address and push the address-sized value stored there. *)
  | DW_OP_const1u  (** Push a 1-byte unsigned constant. *)
  | DW_OP_const1s  (** Push a 1-byte signed constant. *)
  | DW_OP_const2u  (** Push a 2-byte unsigned constant. *)
  | DW_OP_const2s  (** Push a 2-byte signed constant. *)
  | DW_OP_const4u  (** Push a 4-byte unsigned constant. *)
  | DW_OP_const4s  (** Push a 4-byte signed constant. *)
  | DW_OP_const8u  (** Push an 8-byte unsigned constant. *)
  | DW_OP_const8s  (** Push an 8-byte signed constant. *)
  | DW_OP_constu  (** Push a ULEB128 unsigned constant. *)
  | DW_OP_consts  (** Push an SLEB128 signed constant. *)
  | DW_OP_dup  (** Duplicate the top stack entry. *)
  | DW_OP_drop  (** Pop the top stack entry. *)
  | DW_OP_over
      (** Push a copy of the entry below the top (equivalent to pick 1). *)
  | DW_OP_pick  (** Push a copy of the stack entry at the 1-byte index. *)
  | DW_OP_swap  (** Swap the top two stack entries. *)
  | DW_OP_rot  (** Rotate the top three stack entries. *)
  | DW_OP_xderef
      (** Pop an address and an address-space identifier and push the value read
          from that multi-address-space location. *)
  | DW_OP_abs  (** Replace the top entry with its absolute value. *)
  | DW_OP_and  (** Bitwise AND of the top two entries. *)
  | DW_OP_div  (** Signed division of the second entry by the top. *)
  | DW_OP_minus  (** Subtract the top entry from the one below it. *)
  | DW_OP_mod  (** Modulo of the second entry by the top. *)
  | DW_OP_mul  (** Multiply the top two entries. *)
  | DW_OP_neg  (** Arithmetic negation of the top entry. *)
  | DW_OP_not  (** Bitwise complement of the top entry. *)
  | DW_OP_or  (** Bitwise OR of the top two entries. *)
  | DW_OP_plus  (** Add the top two entries. *)
  | DW_OP_plus_uconst  (** Add a ULEB128 constant to the top entry. *)
  | DW_OP_shl  (** Left-shift the second entry by the top entry. *)
  | DW_OP_shr  (** Logical right-shift the second entry by the top entry. *)
  | DW_OP_shra  (** Arithmetic right-shift the second entry by the top entry. *)
  | DW_OP_xor  (** Bitwise XOR of the top two entries. *)
  | DW_OP_bra
      (** Conditional branch: pop the top entry and, if non-zero, branch by the
          signed 2-byte offset. *)
  | DW_OP_eq  (** Push 1 if the top two entries are equal, else 0. *)
  | DW_OP_ge  (** Push 1 if the second entry is >= the top, else 0. *)
  | DW_OP_gt  (** Push 1 if the second entry is > the top, else 0. *)
  | DW_OP_le  (** Push 1 if the second entry is <= the top, else 0. *)
  | DW_OP_lt  (** Push 1 if the second entry is < the top, else 0. *)
  | DW_OP_ne  (** Push 1 if the top two entries are not equal, else 0. *)
  | DW_OP_skip  (** Unconditional branch by the signed 2-byte offset. *)
  | DW_OP_lit0  (** Push the literal value N (DW_OP_lit0..lit31 push 0..31). *)
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
      (** Register location: the value is in register N (DW_OP_reg0..reg31 cover
          registers 0..31). *)
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
  | DW_OP_breg0
      (** Push register N's contents plus an SLEB128 offset (DW_OP_breg0..breg31
          cover registers 0..31). *)
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
  | DW_OP_regx  (** Register location for the ULEB128 register number. *)
  | DW_OP_fbreg
      (** Push the frame base (DW_AT_frame_base) plus an SLEB128 offset. *)
  | DW_OP_bregx
      (** Push the ULEB128 register's contents plus an SLEB128 offset. *)
  | DW_OP_piece
      (** The preceding location holds a ULEB128-byte piece of the object
          (composite location description). *)
  | DW_OP_deref_size
      (** Like DW_OP_deref but read the 1-byte-specified number of bytes. *)
  | DW_OP_xderef_size
      (** Like DW_OP_xderef but read the 1-byte-specified number of bytes. *)
  | DW_OP_nop  (** No operation. *)
  | DW_OP_push_object_address
      (** Push the address of the object being described. *)
  | DW_OP_call2
      (** Evaluate the expression of the DIE at the 2-byte unit offset. *)
  | DW_OP_call4
      (** Evaluate the expression of the DIE at the 4-byte unit offset. *)
  | DW_OP_call_ref
      (** Evaluate the expression of the DIE at the offset-sized reference. *)
  | DW_OP_form_tls_address
      (** Pop a TLS offset and push the corresponding thread-local address. *)
  | DW_OP_call_frame_cfa
      (** Push the call frame CFA (canonical frame address). *)
  | DW_OP_bit_piece
      (** Like DW_OP_piece but a ULEB128 bit size and ULEB128 bit offset. *)
  | DW_OP_implicit_value
      (** The object has no location; its value is the given block (ULEB128 size
          followed by that many bytes). *)
  | DW_OP_stack_value
      (** The object has no location; its value is the top stack entry. New in
          DWARF Version 5. *)
  | DW_OP_implicit_pointer
      (** A pointer that was optimised away: an offset-sized DIE reference and
          an SLEB128 byte offset into the pointed-to object. *)
  | DW_OP_addrx  (** Push an address from .debug_addr at the ULEB128 index. *)
  | DW_OP_constx  (** Push a constant from .debug_addr at the ULEB128 index. *)
  | DW_OP_entry_value
      (** Push the value a sub-expression had on entry to the current function
          (ULEB128 size followed by that many bytes of expression). *)
  | DW_OP_const_type
      (** Push a typed constant: ULEB128 type-DIE offset, 1-byte size, then the
          constant value of that size. *)
  | DW_OP_regval_type
      (** Push the typed contents of a register: ULEB128 register number and
          ULEB128 type-DIE offset. *)
  | DW_OP_deref_type
      (** Like DW_OP_deref but typed: 1-byte size and ULEB128 type-DIE offset.
      *)
  | DW_OP_xderef_type
      (** Like DW_OP_xderef but typed: 1-byte size and ULEB128 type-DIE offset.
      *)
  | DW_OP_convert
      (** Convert the top entry to the type at the ULEB128 offset (0 = generic
          type). *)
  | DW_OP_reinterpret
      (** Reinterpret the top entry's bits as the type at the ULEB128 offset. *)
  | DW_OP_GNU_parameter_ref
      (** GNU extension: reference a parameter DIE by 4-byte offset. *)
  | DW_OP_GNU_variable_value
      (** GNU extension: push the value of the variable at the address-sized DIE
          reference. *)
  | DW_OP_hi_user
      (** Top of the vendor-extension opcode range (DW_OP_lo_user 0xe0 ..
          DW_OP_hi_user 0xff). *)
  | DW_OP_unknown of int
      (** An opcode not in this enumeration, e.g. a vendor extension; carries
          the raw code. *)

val operation_encoding : int -> operation_encoding
(** Convert an int to an [operation_encoding]. Unrecognised values decode to
    [DW_OP_unknown]. *)

val int_of_operation_encoding : operation_encoding -> int
(** Convert an [operation_encoding] to its integer encoding. *)

val string_of_operation_encoding : operation_encoding -> string
(** Convert an [operation_encoding] to its string representation.

    Follows the format of the operation_encoding type variants. *)

type dwarf_expression_operation = {
  opcode : operation_encoding;
  operands : int list;
  operand_string : string option;
}
(** A single decoded DWARF expression operation with its operands.

    DWARF 5 specification, section 7.7.1 "DWARF Expressions". *)

val parse_dwarf_expression :
  ?encoding:encoding -> string -> dwarf_expression_operation list
(** Decode a DWARF expression bytecode string (as carried raw in
    [DW_AT_location], location- and range-list entries, [DW_CFA_expression],
    etc.) into its operations. [encoding] sets the width of address- and
    offset-sized operands; without it they default to 8 bytes. *)

val string_of_dwarf_operation : dwarf_expression_operation -> string
(** Convert a single DWARF operation to its string representation. *)

val string_of_dwarf_expression : dwarf_expression_operation list -> string
(** Convert a list of DWARF operations to a string representation. *)

(** Location List entry.

    DWARF 5 specification, Table 7.10 "Location list entry encoding values". *)
type location_list_entry =
  | DW_LLE_end_of_list  (** Marks the end of the location list *)
  | DW_LLE_base_addressx  (** New base address, as a .debug_addr index *)
  | DW_LLE_startx_endx  (** Range: start and end as .debug_addr indices *)
  | DW_LLE_startx_length
      (** Range: start as a .debug_addr index, plus length *)
  | DW_LLE_offset_pair
      (** Range: start and end offsets from the base address *)
  | DW_LLE_default_location  (** Default location when no other entry applies *)
  | DW_LLE_base_address  (** New base address, given directly *)
  | DW_LLE_start_end  (** Range: explicit start and end addresses *)
  | DW_LLE_start_length  (** Range: explicit start address, plus length *)

val location_list_entry : int -> location_list_entry
(** Convert an int to a [location_list_entry].

    @raise Parse_error
      if the value is not a recognized location list entry kind. *)

val int_of_location_list_entry : location_list_entry -> int
(** Convert a [location_list_entry] to its integer encoding. *)

val string_of_location_list_entry : location_list_entry -> string
(** Convert a [location_list_entry] to its string representation. *)

(** Base type encoding.

    The encodings of the constants used in the [DW_AT_encoding] attributes.

    A base type is a data type that is not defined in terms of other data types.
    Each programming language has a set of base types that are considered to be
    built into that language.

    DWARF 5 specification, Table 7.11 "Base type encoding values". *)
type base_type =
  | DW_ATE_address  (** Linear machine address *)
  | DW_ATE_boolean  (** True or false *)
  | DW_ATE_complex_float  (** Complex floating-point number *)
  | DW_ATE_float  (** Floating-point number *)
  | DW_ATE_signed  (** Signed binary integer *)
  | DW_ATE_signed_char  (** Signed character *)
  | DW_ATE_unsigned  (** Unsigned binary integer *)
  | DW_ATE_unsigned_char  (** Unsigned character *)
  | DW_ATE_imaginary_float  (** Imaginary floating-point number *)
  | DW_ATE_packed_decimal  (** Packed decimal *)
  | DW_ATE_numeric_string  (** Numeric string *)
  | DW_ATE_edited  (** Edited string or number *)
  | DW_ATE_signed_fixed  (** Signed fixed-point *)
  | DW_ATE_unsigned_fixed  (** Unsigned fixed-point *)
  | DW_ATE_decimal_float  (** Decimal floating-point number *)
  | DW_ATE_UTF  (** Unicode character; new in DWARF Version 5 *)
  | DW_ATE_UCS  (** ISO/IEC 10646-1 character *)
  | DW_ATE_ASCII  (** ASCII character *)
  | DW_ATE_lo_user  (** Start of the vendor-extension range *)
  | DW_ATE_hi_user  (** End of the vendor-extension range *)
  | DW_ATE_unknown of int
      (** A value not in this enumeration, e.g. a vendor extension; carries the
          raw code. *)

val base_type : int -> base_type
(** Convert an int to a [base_type].

    Unrecognised values decode to [DW_ATE_unknown]. *)

val int_of_base_type : base_type -> int
(** Convert a [base_type] to its integer encoding. *)

val string_of_base_type : base_type -> string
(** Convert a [base_type] to its string representation *)

(** Decimal sign encoding.

    The encodings of the constants used in the [DW_AT_decimal_sign] attribute.

    DWARF 5 specification, Table 7.12 "Decimal sign encodings". *)
type decimal_sign =
  | DW_DS_unsigned  (** Unsigned *)
  | DW_DS_leading_overpunch  (** Sign encoded in the leading digit *)
  | DW_DS_trailing_overpunch  (** Sign encoded in the trailing digit *)
  | DW_DS_leading_separate  (** Separate leading sign byte *)
  | DW_DS_trailing_separate  (** Separate trailing sign byte *)

val decimal_sign : int -> decimal_sign
(** Convert an int to a [decimal_sign].

    @raise Parse_error if the value is not a recognized decimal sign code. *)

val int_of_decimal_sign : decimal_sign -> int
(** Convert a [decimal_sign] to its integer encoding. *)

val string_of_decimal_sign : decimal_sign -> string
(** Convert a [decimal_sign] to its string representation. *)

(** Endianity encoding.

    The encodings of the constants used in the [DW_AT_endianity] attribute.

    DWARF 5 specification, Table 7.13 "Endianity encodings". *)
type endianity =
  | DW_END_default  (** Target's default byte order *)
  | DW_END_big  (** Big-endian byte order *)
  | DW_END_little  (** Little-endian byte order *)
  | DW_END_lo_user  (** Start of the vendor-extension range *)
  | DW_END_hi_user  (** End of the vendor-extension range *)
  | DW_END_unknown of int
      (** A value not in this enumeration, e.g. a vendor extension; carries the
          raw code. *)

val endianity : int -> endianity
(** Convert an int to an [endianity].

    Unrecognised values decode to [DW_END_unknown]. *)

val int_of_endianity : endianity -> int
(** Convert an [endianity] to its integer encoding. *)

val string_of_endianity : endianity -> string
(** Convert an [endianity] to its string representation.

    Follows the format of the endianity type variants. *)

(** Accessibility encoding.

    The encodings of the constants used in the [DW_AT_accessibility] attribute.

    DWARF 5 specification, Table 7.14 "Accessibility encodings". *)
type accessibility =
  | DW_ACCESS_public  (** Public member *)
  | DW_ACCESS_protected  (** Protected member *)
  | DW_ACCESS_private  (** Private member *)

val accessibility : int -> accessibility
(** Convert an int to an [accessibility].

    @raise Parse_error if the value is not a recognized accessibility code. *)

val int_of_accessibility : accessibility -> int
(** Convert an [accessibility] to its integer encoding. *)

val string_of_accessibility : accessibility -> string
(** Convert an [accessibility] to its string representation.

    Follows the format of the accessibility type variants. *)

(** Visibility encoding.

    The encoding of the constants used in the [DW_AT_visibility] attribute.

    DWARF 5 specification, Table 7.15 "Visibility encodings". *)
type visibility =
  | DW_VIS_local  (** Visible only within its compilation unit *)
  | DW_VIS_exported  (** Visible outside its compilation unit *)
  | DW_VIS_qualified  (** Visible only through qualification *)

val visibility : int -> visibility
(** Convert an int to a [visibility].

    @raise Parse_error if the value is not a recognized visibility code. *)

val int_of_visibility : visibility -> int
(** Convert a [visibility] to its integer encoding. *)

val string_of_visibility : visibility -> string
(** Convert a [visibility] to its string representation.

    Follows the format of the visibility type variants. *)

(** Virtuality encoding.

    The encoding of the constants used in the [DW_AT_virtuality] attribute.

    DWARF 5 specification, Table 7.16 "Virtuality encodings". *)
type virtuality =
  | DW_VIRTUALITY_none  (** Not virtual *)
  | DW_VIRTUALITY_virtual  (** Virtual *)
  | DW_VIRTUALITY_pure_virtual  (** Pure virtual *)

val virtuality : int -> virtuality
(** Convert an int to a [virtuality].

    @raise Parse_error if the value is not a recognized virtuality code. *)

val int_of_virtuality : virtuality -> int
(** Convert a [virtuality] to its integer encoding. *)

val string_of_virtuality : virtuality -> string
(** Convert a [virtuality] to its string representation.

    Follows the format of the virtuality type variants. *)

(** Language encoding.

    The encoding of the constants used in the [DW_AT_language] attribute.

    DWARF 5 specification, Table 7.17 "Language encodings". *)
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
  (* New in DWARF Version 5 *)
  | DW_LANG_Python
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
  | DW_LANG_unknown of int
      (** A value not in this enumeration, e.g. a vendor extension; carries the
          raw code. *)

val dwarf_language : int -> dwarf_language
(** Convert an int to a [dwarf_language]. Unrecognised values decode to
    [DW_LANG_unknown]. *)

val int_of_dwarf_language : dwarf_language -> int
(** Convert a [dwarf_language] to its integer encoding. *)

val string_of_dwarf_language : dwarf_language -> string
(** Convert a dwarf_language to its string representation *)

(** Identifier case.

    The encoding of the constants used in the [DW_AT_identifier_case] attribute.

    DWARF 5 specification, Table 7.18 "Identifier case encodings". *)
type identifier =
  | DW_ID_case_sensitive  (** Case-sensitive identifiers *)
  | DW_ID_up_case  (** Folded to upper case *)
  | DW_ID_down_case  (** Folded to lower case *)
  | DW_ID_case_insensitive  (** Case-insensitive identifiers *)

val identifier : int -> identifier
(** Convert an int to an [identifier].

    @raise Parse_error if the value is not a recognized identifier case code. *)

val int_of_identifier : identifier -> int
(** Convert an [identifier] to its integer encoding. *)

val string_of_identifier : identifier -> string
(** Convert an [identifier] to its string representation.

    Follows the format of the [identifier] type variants. *)

(** Calling convention.

    The encoding of the constants used in the [DW_AT_calling_convention]
    attribute.

    DWARF 5 specification, Table 7.19 "Calling convention encodings". *)
type calling_convention =
  | DW_CC_normal  (** Normal calling convention *)
  | DW_CC_program  (** Program's main subprogram *)
  | DW_CC_nocall  (** Subroutine that is never called *)
  (* New in DWARF Version 5 *)
  | DW_CC_pass_by_reference  (** Aggregate passed by reference *)
  | DW_CC_pass_by_value  (** Aggregate passed by value *)
  | DW_CC_lo_user  (** Start of the vendor-extension range *)
  | DW_CC_hi_user  (** End of the vendor-extension range *)
  | DW_CC_unknown of int
      (** A value outside the standard set, e.g. a vendor extension in the
          [lo_user]..[hi_user] range; carries the raw code. *)

val calling_convention : int -> calling_convention
(** Convert an int to a [calling_convention].

    Unrecognised values decode to [DW_CC_unknown]. *)

val int_of_calling_convention : calling_convention -> int
(** Convert a [calling_convention] to its integer encoding. *)

val string_of_calling_convention : calling_convention -> string
(** Convert a [calling_convention] to its string representation. *)

(** Inlined encoding. The encodings of the constants used in the [DW_AT_inline]
    attribute.

    DWARF 5 specification, Table 7.20 "Inline encodings". *)
type inlined =
  | DW_INL_not_inlined  (** Neither declared inline nor inlined *)
  | DW_INL_inlined  (** Inlined but not declared inline *)
  | DW_INL_declared_not_inlined  (** Declared inline but not inlined *)
  | DW_INL_declared_inlined  (** Declared inline and inlined *)

val inlined : int -> inlined
(** Convert an int to an [inlined].

    @raise Parse_error if the value is not a recognized inline code. *)

val int_of_inlined : inlined -> int
(** Convert an [inlined] to its integer encoding. *)

val string_of_inlined : inlined -> string
(** Convert an [inlined] to its string representation.

    Follows the format of the [inlined] type variants. *)

(** Array ordering.

    The encoding of the constants used in the [DW_AT_ordering] attribute.

    DWARF 5 specification, Table 7.21 "Ordering encodings". *)
type array_ordering =
  | DW_ORD_row_major  (** Row-major array ordering *)
  | DW_ORD_col_major  (** Column-major array ordering *)

val array_ordering : int -> array_ordering
(** Convert an int to an [array_ordering].

    @raise Parse_error if the value is not a recognized array ordering. *)

val int_of_array_ordering : array_ordering -> int
(** Convert an [array_ordering] to its integer encoding. *)

val string_of_array_ordering : array_ordering -> string
(** Convert an [array_ordering] to its string representation.

    Follows the format of the [array_ordering] type variants. *)

(** Discriminant.

    The descriptors used in the [DW_AT_discr_list] attribute, which are encoded
    as 1-byte constants.

    DWARF 5 specification, Table 7.22 "Discriminant descriptor encodings". *)
type discriminant =
  | DW_DSC_label  (** Discriminant is a single label *)
  | DW_DSC_range  (** Discriminant is a range of values *)

val discriminant : int -> discriminant
(** Convert an int to a [discriminant].

    @raise Parse_error if the value is not a recognized discriminant descriptor.
*)

val int_of_discriminant : discriminant -> int
(** Convert a [discriminant] to its integer encoding. *)

val string_of_discriminant : discriminant -> string
(** Convert a [discriminant] to its string representation. *)

(** Name index attribute encodings. New in DWARF 5.

    DWARF 5 specification, Table 7.23 "Name index attribute encodings". *)
type name_index_attribute =
  | DW_IDX_null  (** Null terminator for the attribute list *)
  | DW_IDX_compile_unit  (** Index of the CU in the CU list *)
  | DW_IDX_type_unit  (** Index of the type unit in the local TU list *)
  | DW_IDX_die_offset  (** Offset of the DIE within its unit *)
  | DW_IDX_parent  (** Index of the parent entry in the name index *)
  | DW_IDX_type_hash  (** Hash of the type, for type units *)
  | DW_IDX_lo_user  (** Start of the vendor-extension range *)
  | DW_IDX_hi_user  (** End of the vendor-extension range *)
  | DW_IDX_unknown of int
      (** A value not in this enumeration, e.g. a vendor extension; carries the
          raw code. *)

val name_index_attribute : int -> name_index_attribute
(** Convert an int to a [name_index_attribute].

    Unrecognised values decode to [DW_IDX_unknown]. *)

val int_of_name_index_attribute : name_index_attribute -> int
(** Convert a [name_index_attribute] to its integer encoding. *)

val string_of_name_index_attribute : name_index_attribute -> string
(** Convert a [name_index_attribute] to its string representation. *)

(** Defaulted attribute.

    The encoding of the constants used in the [DW_AT_defaulted] attribute. New
    in DWARF 5.

    DWARF 5 specification, Table 7.24 "Defaulted attribute encodings". *)
type defaulted_attribute =
  | DW_DEFAULTED_no  (** Not defaulted *)
  | DW_DEFAULTED_in_class  (** Defaulted inside the class body *)
  | DW_DEFAULTED_out_of_class  (** Defaulted outside the class body *)

val defaulted_attribute : int -> defaulted_attribute
(** Convert an int to a [defaulted_attribute].

    @raise Parse_error
      if the value is not a recognized defaulted attribute code. *)

val int_of_defaulted_attribute : defaulted_attribute -> int
(** Convert a [defaulted_attribute] to its integer encoding. *)

val string_of_defaulted_attribute : defaulted_attribute -> string
(** Convert a [defaulted_attribute] to its string representation.

    Follows the format of the [defaulted_attribute] type variants. *)

(** Line number opcodes.

    The encoding for the standard opcodes.

    DWARF 5 specification, Table 7.25 "Line number standard opcode encodings".
*)
type line_number_opcode =
  | DW_LNS_copy  (** Append a row to the line table *)
  | DW_LNS_advance_pc  (** Advance the address by an operand *)
  | DW_LNS_advance_line  (** Advance the line by a signed operand *)
  | DW_LNS_set_file  (** Set the current file *)
  | DW_LNS_set_column  (** Set the current column *)
  | DW_LNS_negate_stmt  (** Toggle the is_stmt flag *)
  | DW_LNS_set_basic_block  (** Mark the next row as a basic-block start *)
  | DW_LNS_const_add_pc  (** Advance the address by a constant *)
  | DW_LNS_fixed_advance_pc
      (** Advance the address by a fixed 2-byte operand *)
  | DW_LNS_set_prologue_end  (** Mark the row as a prologue end *)
  | DW_LNS_set_epilogue_begin  (** Mark the row as an epilogue start *)
  | DW_LNS_set_isa  (** Set the instruction set architecture *)

val line_number_opcode : int -> line_number_opcode
(** Convert an int to a [line_number_opcode]. *)

val int_of_line_number_opcode : line_number_opcode -> int
(** Convert a [line_number_opcode] to its integer encoding. *)

val string_of_line_number_opcode : line_number_opcode -> string
(** Convert a [line_number_opcode] to its string representation.

    Follows the format of the [line_number_opcode] type variants. *)

(** Line number extended opcodes.

    The encoding for the extended opcodes.

    DWARF 5 specification, Table 7.26 "Line number extended opcode encodings".
*)
type line_number_extended_opcode =
  | DW_LNE_end_sequence  (** End the current sequence of rows *)
  | DW_LNE_set_address  (** Set the address to a relocatable operand *)
  | DW_LNE_set_discriminator  (** Set the discriminator value *)
  | DW_LNE_lo_user  (** Start of the vendor-extension range *)
  | DW_LNE_hi_user  (** End of the vendor-extension range *)
  | DW_LNE_unknown of int
      (** A value not in this enumeration, e.g. a vendor extension; carries the
          raw code. *)

val line_number_extended_opcode : int -> line_number_extended_opcode
(** Convert an int to a [line_number_extended_opcode]. *)

val int_of_line_number_extended_opcode : line_number_extended_opcode -> int
(** Convert a [line_number_extended_opcode] to its integer encoding. *)

val string_of_line_number_extended_opcode :
  line_number_extended_opcode -> string
(** Convert a [line_number_extended_opcode] to its string representation.

    Follows the format of the [line_number_extended_opcode] type variants. *)

(** Line number header entry.

    The encoding for the line number header entry formats. New in DWARF 5.

    DWARF 5 specification, Table 7.27 "Line number header entry format
    encodings". *)
type line_number_header_entry =
  | DW_LNCT_path  (** File or directory path *)
  | DW_LNCT_directory_index  (** Index into the directory list *)
  | DW_LNCT_timestamp  (** Last-modification timestamp *)
  | DW_LNCT_size  (** File size in bytes *)
  | DW_LNCT_MD5  (** 16-byte MD5 digest of the file *)
  | DW_LNCT_lo_user  (** Start of the vendor-extension range *)
  | DW_LNCT_hi_user  (** End of the vendor-extension range *)
  | DW_LNCT_unknown of int
      (** A value not in this enumeration, e.g. a vendor extension; carries the
          raw code. *)

val line_number_header_entry : int -> line_number_header_entry
(** Convert an int to a [line_number_header_entry]. *)

val int_of_line_number_header_entry : line_number_header_entry -> int
(** Convert a [line_number_header_entry] to its integer encoding. *)

val string_of_line_number_header_entry : line_number_header_entry -> string
(** Convert a [line_number_header_entry] to its string representation.

    Follows the format of the [line_number_header_entry] type variants.*)

(** Macro information entry type. New in DWARF 5.

    DWARF 5 specification, Table 7.28 "Macro information entry type encodings".
*)
type macro_info_entry_type =
  | DW_MACRO_define  (** Macro definition, inline string *)
  | DW_MACRO_undef  (** Macro undefinition, inline string *)
  | DW_MACRO_start_file  (** Start of a source file *)
  | DW_MACRO_end_file  (** End of a source file *)
  | DW_MACRO_define_strp  (** Macro definition, string via .debug_str *)
  | DW_MACRO_undef_strp  (** Macro undefinition, string via .debug_str *)
  | DW_MACRO_import  (** Import another macro unit *)
  | DW_MACRO_define_sup
      (** Macro definition, string in the supplementary file *)
  | DW_MACRO_undef_sup
      (** Macro undefinition, string in the supplementary file *)
  | DW_MACRO_import_sup  (** Import a macro unit from the supplementary file *)
  | DW_MACRO_define_strx  (** Macro definition, string via .debug_str_offsets *)
  | DW_MACRO_undef_strx
      (** Macro undefinition, string via .debug_str_offsets *)
  | DW_MACRO_lo_user  (** Start of the vendor-extension range *)
  | DW_MACRO_hi_user  (** End of the vendor-extension range *)

val macro_info_entry_type : u8 -> macro_info_entry_type
(** Convert a u8 to a [macro_info_entry_type]. *)

val u8_of_macro_info_entry_type : macro_info_entry_type -> u8
(** Convert a [macro_info_entry_type] to its u8 encoding. *)

val string_of_macro_info_entry_type : macro_info_entry_type -> string
(** Convert a [macro_info_entry_type] to its string representation. *)

(** Parser for the .debug_macro section. New in DWARF 5.

    The debug_macro section contains macro definition and undefinition
    information used by the preprocessor. Each compilation unit may have its own
    macro unit describing the macros active during compilation.

    DWARF 5 specification, section 6.3 "Macro Information". *)
module DebugMacro : sig
  type header = {
    format : dwarf_format;  (** Either DWARF32 or DWARF64. *)
    version : u16;  (** Version number *)
    flags : u8;  (** Flags *)
    debug_line_offset : u64 option;  (** Offset into debug_line (flags bit 1) *)
    debug_str_offsets_offset : u64 option;
        (** Offset into debug_str_offsets (flags bit 2) *)
  }
  (** Header structure for a debug_macro contribution. *)

  type entry = {
    entry_type : macro_info_entry_type;  (** Type of macro entry *)
    line_number : u32 option;  (** Line number for certain types *)
    string_offset : u64 option;  (** Offset into string table *)
    string_value : string option;  (** Direct string value *)
    file_index : u32 option;  (** File index for start_file entries *)
  }
  (** A single macro entry. *)

  type macro_unit = {
    header : header;  (** Unit header *)
    entries : entry list;  (** List of macro entries *)
  }
  (** A complete macro unit with header and entries. *)

  type section = { units : macro_unit list }
  (** Complete parsed .debug_macro section. *)

  val parse_header : Object.Buffer.cursor -> header
  (** Parse the header of a debug_macro contribution. *)

  val parse_entry : Object.Buffer.cursor -> dwarf_format -> entry option
  (** Parse a single debug_macro entry. Returns [None] at end of entries marker.

      @raise Parse_error if a macro entry type is unknown. *)

  val parse_unit : Object.Buffer.cursor -> macro_unit
  (** Parse a complete debug_macro unit (header + entries).

      @raise Parse_error if the unit is malformed. *)

  val parse_section : Object.Buffer.cursor -> int -> section
  (** Parse the entire debug_macro section.

      @raise Parse_error if the section is malformed. *)
end

(** Parser for the .debug_macinfo section (DWARF 4).

    The debug_macinfo section is the DWARF 4 predecessor to .debug_macro. It
    contains macro definition and undefinition information in a simpler format
    without string table indirection.

    DWARF 4 specification, section 6.3 "Macro Information". *)
module DebugMacinfo : sig
  (** Macro info entry type codes. *)
  type macinfo_type =
    | DW_MACINFO_define  (** Macro definition *)
    | DW_MACINFO_undef  (** Macro undefinition *)
    | DW_MACINFO_start_file  (** Start of a source file *)
    | DW_MACINFO_end_file  (** End of a source file *)
    | DW_MACINFO_vendor_ext  (** Vendor-specific extension *)

  val macinfo_type_of_int : int -> macinfo_type
  (** Convert an int to a [macinfo_type].

      @raise Parse_error if the value is not a recognized macinfo type. *)

  val string_of_macinfo_type : macinfo_type -> string
  (** Convert a [macinfo_type] to its string representation. *)

  type entry = {
    macinfo_type : macinfo_type;  (** Kind of macro entry *)
    line_number : u32 option;  (** Source line, for define/undef entries *)
    string_value : string option;  (** Macro text or file name *)
    file_index : u32 option;  (** File index, for start_file entries *)
    constant : u64 option;  (** Constant, for vendor extensions *)
  }
  (** A single macinfo entry. *)

  type section = { entries : entry list }
  (** Complete parsed .debug_macinfo section. *)

  val parse_entry : Object.Buffer.cursor -> entry option
  (** Parse a single macinfo entry. Returns [None] at end marker.

      @raise Parse_error if a macinfo entry type is unknown. *)

  val parse_section : Object.Buffer.cursor -> int -> section
  (** Parse the entire .debug_macinfo section. The [int] parameter is the
      section size in bytes.

      @raise Parse_error if the section is malformed. *)
end

(** Sections that hold DWARF information.

    DWARF 5 specification, section 7.5 "Format of Debugging Information". *)
type dwarf_section = Dwarf_types.dwarf_section =
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
  | Debug_loc
  | Debug_ranges
  | Debug_pubnames
  | Debug_pubtypes
  | Debug_types
  | Debug_info_dwo
  | Debug_abbrev_dwo
  | Debug_line_dwo
  | Debug_loclists_dwo
  | Debug_rnglists_dwo
  | Debug_str_dwo
  | Debug_str_offs_dwo
  | Debug_macro_dwo
  | Debug_macinfo
  | Debug_cu_index
  | Debug_tu_index

type parse_error = Dwarf_types.parse_error = {
  section : dwarf_section option;
      (** The DWARF section being parsed, if known. *)
  offset : int option;  (** Byte offset within the section, if known. *)
  message : string;  (** Human-readable description of the failure. *)
}
(** Structured description of a DWARF parsing failure.

    Raised by the reader, writers and helper functions when input cannot be
    decoded. *)

exception Parse_error of parse_error

(** Call frame instruction encodings.

    DWARF 5 specification, Table 7.29 "Call frame instruction encodings". *)
type call_frame_instruction =
  | DW_CFA_advance_loc  (** Advance location by the delta in the opcode *)
  | DW_CFA_offset  (** Register saved at CFA + offset *)
  | DW_CFA_restore  (** Restore register to its initial rule *)
  | DW_CFA_nop  (** No operation; padding *)
  | DW_CFA_set_loc  (** Set location to an explicit address *)
  | DW_CFA_advance_loc1  (** Advance location by a 1-byte delta *)
  | DW_CFA_advance_loc2  (** Advance location by a 2-byte delta *)
  | DW_CFA_advance_loc4  (** Advance location by a 4-byte delta *)
  | DW_CFA_offset_extended
      (** Register saved at CFA + offset (ULEB register) *)
  | DW_CFA_restore_extended  (** Restore register to its initial rule (ULEB) *)
  | DW_CFA_undefined  (** Register has no recoverable value *)
  | DW_CFA_same_value  (** Register's value is unchanged *)
  | DW_CFA_register  (** Register saved in another register *)
  | DW_CFA_remember_state  (** Push the register rules onto a stack *)
  | DW_CFA_restore_state  (** Pop the register rules from the stack *)
  | DW_CFA_def_cfa  (** Define the CFA as register + offset *)
  | DW_CFA_def_cfa_register  (** Change the CFA register, keep the offset *)
  | DW_CFA_def_cfa_offset  (** Change the CFA offset, keep the register *)
  | DW_CFA_def_cfa_expression  (** Define the CFA via a DWARF expression *)
  | DW_CFA_expression  (** Register value located via a DWARF expression *)
  | DW_CFA_offset_extended_sf  (** Register saved at CFA + signed offset *)
  | DW_CFA_def_cfa_sf  (** Define the CFA as register + signed offset *)
  | DW_CFA_def_cfa_offset_sf  (** Change the CFA offset to a signed value *)
  | DW_CFA_val_offset  (** Register value is CFA + offset (not in memory) *)
  | DW_CFA_val_offset_sf  (** Register value is CFA + signed offset *)
  | DW_CFA_val_expression  (** Register value computed by a DWARF expression *)
  | DW_CFA_lo_user  (** Start of the vendor-extension range *)
  | DW_CFA_hi_user  (** End of the vendor-extension range *)
  | DW_CFA_unknown of int
      (** An instruction not in this enumeration, e.g. a vendor extension;
          carries the raw opcode. *)

val string_of_call_frame_instruction : call_frame_instruction -> string
(** Convert a [call_frame_instruction] to its string representation. *)

(** Range list entry encoding values. Each entry in a range list begins with one
    of these codes to indicate the kind of entry that follows. Range lists
    describe the non-contiguous address ranges owned by a DIE, such as the code
    addresses belonging to a function that has been split by the optimiser.

    DWARF 5 specification, Table 7.30 "Range list entry encoding values". *)
type range_list_entry =
  | DW_RLE_end_of_list  (** Marks the end of the range list *)
  | DW_RLE_base_addressx  (** New base address, as a .debug_addr index *)
  | DW_RLE_startx_endx  (** Range: start and end as .debug_addr indices *)
  | DW_RLE_startx_length
      (** Range: start as a .debug_addr index, plus length *)
  | DW_RLE_offset_pair
      (** Range: start and end offsets from the base address *)
  | DW_RLE_base_address  (** New base address, given directly *)
  | DW_RLE_start_end  (** Range: explicit start and end addresses *)
  | DW_RLE_start_length  (** Range: explicit start address, plus length *)

val range_list_entry : int -> range_list_entry
(** Convert an int to a [range_list_entry].

    @raise Parse_error if the value is not a recognized range list entry kind.
*)

val int_of_range_list_entry : range_list_entry -> int
(** Convert a [range_list_entry] to its integer encoding. *)

val string_of_range_list_entry : range_list_entry -> string
(** Convert a [range_list_entry] to its string representation. *)

type t
(** A DWARF context: a handle over an object buffer that lazily parses and
    caches sections. This is the {b high-level} API and the entry point for
    reading DWARF; create one with {!create}.

    For one-shot, stateless parsing of an individual section without a context,
    use the per-section modules directly (the {b low-level} API) — e.g.
    {!DebugAbbrev}, {!DebugAranges}, {!DebugLoclists}. The context's cached
    accessors are the memoizing counterparts of those. *)

type span = { start : size_t; size : size_t }
(** A byte range within a buffer, used to track the position and size of parsed
    DWARF structures. *)

type attr_spec = {
  attr : attribute_encoding;
      (** The attribute name (e.g. [DW_AT_name], [DW_AT_type]) *)
  form : attribute_form_encoding;
      (** The encoding form (e.g. [DW_FORM_strp], [DW_FORM_data4]) *)
  implicit_const : int64 option;
      (** Value for [DW_FORM_implicit_const] attributes, [None] otherwise *)
}
(** A single attribute specification within an abbreviation declaration.

    Each attribute specification pairs an attribute name with a form that
    describes how the attribute value is encoded.

    DWARF 5 specification, section 7.5.3 "Abbreviations Tables". *)

type abbrev = {
  code : u64;  (** Abbreviation code referenced by DIEs *)
  tag : abbreviation_tag;  (** The DIE tag (e.g. [DW_TAG_compile_unit]) *)
  has_children : bool;
      (** Whether DIEs using this abbreviation have children *)
  attr_specs : attr_spec list;  (** List of attribute specifications *)
}
(** An abbreviation declaration from the [.debug_abbrev] section.

    Each abbreviation maps a compact code used in [.debug_info] to the full tag,
    children flag, and attribute specifications for a DIE. *)

val object_format_to_section_name :
  Object_format.format -> dwarf_section -> string
(** Returns the dwarf_section name specific for the object_format. *)

val detect_format_and_arch : Object.Buffer.t -> string
(** Detect file format and architecture from buffer, returning a string like
    "Mach-O arm64" *)

val resolve_string_index : Object.Buffer.t -> dwarf_format -> int -> string
(** Resolve a string index to its actual string value using debug_str sections
*)

val lookup_address_in_debug_addr : Object.Buffer.t -> u64 -> int -> u64 option
(** Look up an address by index in the debug_addr section at given offset.
    Returns Some address if found, None if not found or section missing *)

val resolve_address_index : Object.Buffer.t -> int -> u64 -> u64
(** Resolve an address index to its actual address value using debug_addr
    section. Returns the resolved address if found, or the index value as
    fallback *)

(** Debugging Information Entry (DIE) represent low-level information about a
    source program.

    The debugging information entries are contained in the [debug_info] and/or
    .debug_info.dwo sections of an object file.

    DWARF 5 specification, section 2.1 "The Debugging Information Entry (DIE)"
*)
module DIE : sig
  (** Attribute values that can appear in DWARF Debug Information Entries.

      DWARF 5 specification, section 7.5.5 "Classes and Forms". *)
  type attribute_value =
    | String of string  (** String value from DW_FORM_string or DW_FORM_strp *)
    | IndexedString of int * string
        (** Indexed string from DW_FORM_strx* with index and resolved value *)
    | UData of u64  (** Unsigned integer from DW_FORM_udata, DW_FORM_data* *)
    | SData of i64  (** Signed integer from DW_FORM_sdata *)
    | Address of u64  (** Address from DW_FORM_addr *)
    | IndexedAddress of int * u64
        (** Indexed address from DW_FORM_addrx* with index and resolved address
        *)
    | Flag of bool  (** Boolean from DW_FORM_flag or DW_FORM_flag_present *)
    | Reference of u64  (** Reference from DW_FORM_ref* *)
    | Block of string  (** Block of data from DW_FORM_block* *)
    | Language of dwarf_language  (** Language from DW_AT_language attribute *)
    | Encoding of base_type
        (** Data type encoding from DW_AT_encoding attribute *)
    | Ordering of array_ordering
        (** Array ordering from DW_AT_ordering attribute *)
    | DecimalSign of decimal_sign
        (** Decimal sign from DW_AT_decimal_sign attribute *)
    | Endianity of endianity  (** Byte order from DW_AT_endianity attribute *)
    | Accessibility of accessibility
        (** Access control from DW_AT_accessibility attribute *)
    | Visibility of visibility
        (** Visibility from DW_AT_visibility attribute *)
    | Virtuality of virtuality
        (** Virtuality from DW_AT_virtuality attribute *)
    | IdentifierCase of identifier
        (** Identifier case from DW_AT_identifier_case attribute *)
    | CallingConvention of calling_convention
        (** Calling convention from DW_AT_calling_convention attribute *)
    | Inline of inlined  (** Inline status from DW_AT_inline attribute *)
    | Defaulted of defaulted_attribute
        (** Defaulted status from DW_AT_defaulted attribute *)

  type attribute = { attr : attribute_encoding; value : attribute_value }
  (** A DWARF attribute consisting of name and value.

      DWARF 5 specification, section 7.5.4 "Attribute Encodings". *)

  type t = {
    tag : abbreviation_tag;  (** The DIE's tag (its kind) *)
    attributes : attribute list;  (** Decoded attributes *)
    children : t Seq.t;  (** Child DIEs, parsed lazily *)
    offset : int;  (** Section-relative offset of this DIE *)
  }
  (** A Debug Information Entry containing tag, attributes, and children *)

  val parse_die :
    Object.Buffer.cursor ->
    (u64, abbrev) Hashtbl.t ->
    encoding ->
    Object.Buffer.t ->
    t option
  (** Parse a single DIE from a buffer using abbreviation table. *)

  val parse_attribute_value :
    Object.Buffer.cursor ->
    attribute_form_encoding ->
    encoding ->
    Object.Buffer.t ->
    ?implicit_const:int64 ->
    unit ->
    attribute_value
  (** Parse a single attribute value from a buffer. *)

  val find_attribute : t -> attribute_encoding -> attribute_value option
  (** Find an attribute by name in a DIE *)
end

(** Compilation units represent individual source files and their debugging
    information in DWARF.

    A compilation unit is a fundamental organizational structure in DWARF that
    contains all debugging information associated with the compilation of a
    single source file.

    Each compilation unit includes:
    - Source file information (original file that was compiled)
    - Debug Information Entries (DIEs) describing types, variables, functions,
      etc.
    - Compilation metadata (compiler version, language, compilation directory)
    - Address ranges where the compiled code resides
    - Line number mappings between source and machine code

    DWARF 5 specification, Chapter 3 "Program Scope Entries" introduces the
    concept of Unit Entries and Compilation Units. *)
module CompileUnit : sig
  type header = {
    format : dwarf_format;  (** Either DWARF32 or DWARF64. *)
    unit_length : u64;  (** Length of this unit excluding the length field *)
    version : u16;  (** DWARF version (4 or 5) *)
    unit_type : unit_type;
        (** Unit type (compile, type, partial, skeleton, split_compile,
            split_type). Synthesized as DW_UT_compile for DWARF 4. *)
    debug_abbrev_offset : u64;  (** Offset into debug abbreviation table *)
    address_size : u8;  (** Size of addresses in bytes (4 or 8) *)
    span : span;  (** Span indicating the header's position and size *)
    addr_base : u64 option;
        (** Address table base offset from DW_AT_addr_base *)
    type_signature : u64 option;
        (** Type signature for DW_UT_type and DW_UT_split_type units *)
    type_offset : u64 option;
        (** Offset to the type DIE within the unit, for type units *)
    dwo_id : u64 option;
        (** DWO id for DW_UT_skeleton and DW_UT_split_compile units *)
  }
  (** Parsed header data from a compilation unit. This contains the essential
      metadata needed to interpret the unit's content.

      DWARF 5 specification, section 7.5.1.1 "Full and Partial Compilation Unit
      Headers". *)

  type t
  (** A compilation unit with parsed content. *)

  val make : position:int -> offset:int -> Object.Buffer.t -> header -> t
  (** Create a new compilation unit, given its section-relative [position] and
      its absolute buffer [offset]. *)

  val dwarf_info : t -> int
  (** Get the section-relative position of this unit. *)

  val offset : t -> int
  (** Get the absolute buffer offset where this unit starts. *)

  val header : t -> header
  (** Force parsing of the compilation unit header and return parsed data. *)

  val encoding : t -> encoding
  (** Extract encoding parameters (format, address_size, version) from the unit.
      This provides the context needed for parsing DIE attributes. *)

  val root_die : t -> (u64, abbrev) Hashtbl.t -> Object.Buffer.t -> DIE.t option
  (** Get the root DIE for this compilation unit. *)

  val die_cursor :
    t ->
    (u64, abbrev) Hashtbl.t ->
    Object.Buffer.t ->
    Object.Buffer.cursor * (u64, abbrev) Hashtbl.t * encoding * Object.Buffer.t
  (** Get cursor, abbrev table, encoding, and buffer positioned at the first DIE
      of this compilation unit. *)
end

val skip_attribute_value :
  Object.Buffer.cursor ->
  attribute_form_encoding ->
  encoding ->
  Object.Buffer.t ->
  unit
(** Skip past a single attribute value in the buffer without allocating.

    @raise Parse_error if the attribute form is unknown. *)

val skip_die :
  Object.Buffer.cursor ->
  (u64, abbrev) Hashtbl.t ->
  encoding ->
  Object.Buffer.t ->
  unit
(** Skip an entire DIE (attributes + children if any).

    @raise Parse_error if a DIE attribute form is unknown. *)

val skip_children :
  Object.Buffer.cursor ->
  (u64, abbrev) Hashtbl.t ->
  encoding ->
  Object.Buffer.t ->
  unit
(** Skip all remaining children at the current nesting level.

    @raise Parse_error if a DIE attribute form is unknown. *)

(** Forward-only cursor for iterating over sibling DIEs at a single level.

    Use {!DieCursor} when you need explicit control over tree traversal, for
    example to skip subtrees selectively. For a higher-level zipper interface
    with up/down/right navigation, see {!DieZipper}. *)
module DieCursor : sig
  type t

  val create :
    Object.Buffer.t -> (u64, abbrev) Hashtbl.t -> encoding -> int -> t
  (** Create cursor starting at buffer offset. *)

  val next : t -> (DIE.t * bool) option
  (** Parse next DIE. Returns (die, has_children). Returns None at
      end-of-children (null entry). *)

  val skip_children : t -> unit
  (** Skip remaining children at current depth.

      @raise Parse_error if a DIE attribute form is unknown. *)

  val position : t -> int
  (** Current cursor position in the buffer. *)
end

(** Zipper-based navigator for the DIE tree.

    {!DieZipper} provides a cursor that can move {!DieZipper.down} to the first
    child, {!DieZipper.right} to the next sibling, and {!DieZipper.up} to the
    parent without re-parsing already-visited nodes. Internally it maintains a
    breadcrumb trail of parent DIEs so that upward navigation is O(1).

    This is the recommended interface for interactive or selective traversal of
    the DIE tree. For simple linear iteration over siblings at a single level,
    {!DieCursor} is lighter weight.

    Typical usage:
    {@ocaml skip[
      let dc = DieCursor.create buffer abbrev_table encoding offset in
      match DieZipper.of_die_cursor dc with
      | None -> ()
      | Some z ->
          DieZipper.children z
          |> Seq.iter (fun child ->
              let _die = DieZipper.current child in
              ())
    ]} *)
module DieZipper : sig
  type t
  (** A position in the DIE tree with context for navigating in any direction.
  *)

  val of_die_cursor : DieCursor.t -> t option
  (** Create a zipper focused on the first DIE from a cursor. Returns [None] if
      the cursor is at the end of its siblings. *)

  val current : t -> DIE.t
  (** Get the DIE at the focus. *)

  val tag : t -> abbreviation_tag
  (** Shorthand for (current t).tag. *)

  val down : t -> t option
  (** Navigate to first child. None if no children. *)

  val right : t -> t option
  (** Navigate to next sibling. None if last sibling.

      @raise Parse_error if a DIE attribute form is unknown. *)

  val up : t -> t option
  (** Navigate to parent. None if at root. *)

  val children : t -> t Seq.t
  (** Lazy sequence of child zippers.

      @raise Parse_error
        when the sequence is forced, if a DIE attribute form is unknown. *)

  val siblings : t -> t Seq.t
  (** Lazy sequence of remaining sibling zippers.

      @raise Parse_error
        when the sequence is forced, if a DIE attribute form is unknown. *)

  val fold_children : ('a -> t -> 'a) -> 'a -> t -> 'a
  (** Fold over immediate children.

      @raise Parse_error if a DIE attribute form is unknown. *)

  val find_child : (t -> bool) -> t -> t option
  (** Find first child matching predicate.

      @raise Parse_error if a DIE attribute form is unknown. *)

  val depth : t -> int
  (** Current depth from root (root = 0). *)
end

val parse_compile_unit_header :
  Object.Buffer.cursor -> span * CompileUnit.header
(** Parse the header of a compilation unit from a buffer cursor. Returns both
    the unit span and the parsed header with span included.

    @raise Parse_error if the header is malformed or the version is unsupported.
*)

(** Line number information parsing.

    The line number information in DWARF 5 provides a mapping between source
    code positions (file, line, column) and machine instruction addresses. This
    enables debuggers to set breakpoints on source lines and show source context
    during debugging.

    The line number information is encoded as a sequence of special opcodes and
    standard opcodes that build a matrix of program counter values and source
    positions. Each row in this matrix corresponds to a machine instruction and
    contains:
    - The program counter (address) of the instruction
    - The source file name (as an index into the file names table)
    - The line number within that source file
    - The column number within that line
    - Various flags (is_stmt, basic_block, end_sequence, etc.)

    The line table consists of: 1. A line program header (parsed by this module)
    containing metadata 2. A line program (sequence of opcodes that generate the
    line table matrix)

    The header contains essential information needed to decode the line program,
    including file names, directory names, and opcode definitions.

    DWARF 5 specification, section 6.2 "Line Number Information". *)
module DebugLine : sig
  type t = { cu : CompileUnit.t }
  (** A line program associated with a compilation unit. *)

  (** A DWARF 4 style file entry with path and metadata. *)
  module File : sig
    type t = { path : string; modification_time : u64; file_length : u64 }
  end

  type file_entry = {
    name : string;  (** Source file name, without directory path *)
    timestamp : u64;
        (** File modification timestamp (UNIX epoch), or 0 if unknown *)
    size : u64;  (** File size in bytes, or 0 if unknown *)
    directory : string;  (** Directory path containing this file *)
    md5_checksum : string option;
        (** Optional MD5 checksum of file contents (32 hex chars) *)
  }
  (** File entry in the line number program header.

      Each file entry describes a source file referenced by the line number
      program. In DWARF 5, file entries use a flexible format where different
      content types can be present or absent based on the
      file_name_entry_formats array in the header.

      Supported content types (DW_LNCT_ * constants):
      - {!DW_LNCT_path}: The file name (required)
      - {!DW_LNCT_directory_index}: Directory index (directory field resolved
        from index)
      - {!DW_LNCT_timestamp}: File modification time
      - {!DW_LNCT_size}: File size in bytes
      - {!DW_LNCT_MD5}: MD5 checksum as 16-byte hash converted to hex string

      The directory field contains the resolved directory path rather than the
      raw directory index, making it easier to work with file paths.

      DWARF 5 specification, section 6.2.4 "The Line Number Program Header". *)

  type line_program_header = {
    format : dwarf_format;  (** Either DWARF32 or DWARF64. *)
    unit_length : u64;
        (** Total length of line table entry excluding the length field *)
    version : u16;  (** DWARF version number *)
    address_size : u8;
        (** Size of target address in bytes (typically 4 or 8) *)
    segment_selector_size : u8;
        (** Size of segment selector in bytes (usually 0) *)
    header_length : u64;
        (** Length of line program header excluding initial_length and version
        *)
    minimum_instruction_length : u8;
        (** Smallest target machine instruction length *)
    maximum_operations_per_instruction : u8;
        (** Max operations per instruction (VLIW support) *)
    default_is_stmt : bool;
        (** Initial value of is_stmt register (true = line is statement) *)
    line_base : int;
        (** Base value for special opcode line advance calculations *)
    line_range : u8;  (** Range of line number advances for special opcodes *)
    opcode_base : u8;
        (** First non-standard opcode (standard opcodes are 1 to opcode_base-1)
        *)
    standard_opcode_lengths : u8 array;
        (** Number of operands for each standard opcode *)
    directory_entry_format_count : u8;
        (** Number of format descriptors for directories *)
    directory_entry_formats :
      (line_number_header_entry * attribute_form_encoding) array;
        (** Format descriptors for directory entries *)
    directories_count : u32;  (** Number of directories in directories array *)
    directories : string array;  (** Directory names (include directories) *)
    file_name_entry_format_count : u8;
        (** Number of format descriptors for file names *)
    file_name_entry_formats :
      (line_number_header_entry * attribute_form_encoding) array;
        (** Format descriptors for file entries *)
    file_names_count : u32;  (** Number of file entries in file_names array *)
    file_names : file_entry array;  (** Source file information *)
  }
  (** Line number program header.

      The line number program header contains metadata required to interpret the
      line number program that follows. It defines the opcodes, their operand
      counts, the file and directory tables, and parameters for the line number
      state machine.

      DWARF 5 specification, section 6.2.4 "The Line Number Program Header". *)

  type line_table_entry = {
    address : u64;  (** Program counter address *)
    line : u32;  (** Source line number (1-based) *)
    column : u32;  (** Source column number (1-based, 0 = no column info) *)
    file_index : u32;  (** Index into file_names array *)
    isa : u32;  (** Instruction Set Architecture identifier *)
    discriminator : u32;  (** Discriminator for multiple blocks on same line *)
    op_index : u32;  (** Operation index within VLIW instruction *)
    is_stmt : bool;
        (** True if this instruction is a recommended breakpoint location *)
    basic_block : bool;  (** True if this instruction begins a basic block *)
    end_sequence : bool;  (** True if this instruction ends a sequence *)
    prologue_end : bool;  (** True if this instruction ends function prologue *)
    epilogue_begin : bool;
        (** True if this instruction begins function epilogue *)
  }
  (** Line table entry representing one row in the line number matrix.

      Each entry maps a program counter address to source location information
      and various flags that help debuggers understand the code structure.

      DWARF 5 specification, section 6.2.2 "State Machine Registers". *)

  val parse_line_program_header :
    Object.Buffer.cursor -> Object.Buffer.t -> line_program_header
  (** Parse a complete line program header from the [Debug_line] section.

      Handles both DWARF 4 and DWARF 5 formats. In DWARF 5, directory and file
      entries use a flexible format described by format descriptors, with string
      references resolved from [.debug_line_str] via the [buffer] parameter.

      @param cursor Buffer cursor positioned at start of line program header
      @param buffer
        Complete buffer containing debug sections (needed for
        [DW_FORM_line_strp] resolution)
      @return Parsed line program header with all tables resolved
      @raise Parse_error
        if header format is invalid or unsupported content types are encountered
  *)

  val parse_line_program :
    Object.Buffer.cursor -> line_program_header -> line_table_entry Seq.t
  (** Parse the line number program following the header.

      Returns a lazy sequence of [line_table_entry] produced by executing the
      DWARF line number state machine. Entries are generated on demand as the
      sequence is consumed.

      The state machine processes three types of opcodes:
      - Special opcodes (values opcode_base to 255): Efficiently encode common
        address/line advance combinations
      - Standard opcodes (values 1 to opcode_base-1): Individual operations like
        DW_LNS_copy, DW_LNS_advance_pc
      - Extended opcodes (value 0 followed by length and extended opcode):
        Operations like DW_LNE_end_sequence, DW_LNE_set_address,
        DW_LNE_set_discriminator

      @param cursor Buffer cursor positioned after the line program header
      @param header Parsed line program header containing opcode definitions
      @return Lazy sequence of line table entries *)

  val parse :
    Object.Buffer.cursor ->
    Object.Buffer.t ->
    line_program_header * line_table_entry Seq.t
  (** Parse a complete line program (header + entries). Convenience function
      combining [parse_line_program_header] and [parse_line_program].

      @raise Parse_error
        if the line program header is malformed or the version is unsupported.
  *)
end

(** DWARF 4 location list parsing for .debug_loc section.

    The .debug_loc section contains location lists that describe where a
    variable resides at different points during program execution. Each location
    list is a series of entries terminated by an end-of-list marker. Location
    lists are referenced from DIE attributes encoded with [DW_FORM_sec_offset]
    in DWARF 4.

    Superseded by .debug_loclists in DWARF 5.

    DWARF 4 specification, section 7.7.3 "Location Lists". *)
module DebugLoc : sig
  (** A single location list entry.

      DWARF 4 specification, section 2.6.2 "Location Lists". *)
  type entry =
    | EndOfList  (** Marks the end of the location list *)
    | BaseAddress of u64  (** New base address for subsequent entries *)
    | Location of { begin_addr : u64; end_addr : u64; expr : string }
        (** Location described by [expr], for addresses [begin_addr] up to
            [end_addr] (exclusive) *)

  val parse_list : Object.Buffer.cursor -> int -> entry list
  (** Parse a location list from the .debug_loc section. The [int] parameter is
      the address size (4 or 8). *)
end

(** DWARF 4 range list parsing for .debug_ranges section.

    The .debug_ranges section contains range lists that describe non-contiguous
    address ranges associated with a DIE. Each range list is a series of entries
    terminated by an end-of-list marker. Range lists are referenced from
    [DW_AT_ranges] attributes in DWARF 4.

    Superseded by .debug_rnglists in DWARF 5.

    DWARF 4 specification, section 7.24 "Non-contiguous Address Ranges". *)
module DebugRanges : sig
  (** A single range list entry.

      DWARF 4 specification, section 2.17.3 "Non-Contiguous Address Ranges". *)
  type entry =
    | EndOfList  (** Marks the end of the range list *)
    | BaseAddress of u64  (** New base address for subsequent entries *)
    | Range of { begin_addr : u64; end_addr : u64 }
        (** A range from [begin_addr] up to [end_addr] (exclusive) *)

  val parse_list : Object.Buffer.cursor -> int -> entry list
  (** Parse a range list from the .debug_ranges section. The [int] parameter is
      the address size (4 or 8). *)
end

(** DWARF 4 type unit header parsing for .debug_types section.

    The .debug_types section contains type units that allow type descriptions to
    be shared across compilation units via type signatures. Each type unit has a
    64-bit type signature and an offset to the type DIE within the unit.

    In DWARF 5, type units are folded into .debug_info with [DW_UT_type] unit
    headers.

    DWARF 4 specification, section 7.5.1.2 "Type Unit Header". *)
module DebugTypes : sig
  type type_unit_header = {
    format : dwarf_format;  (** Either DWARF32 or DWARF64. *)
    unit_length : u64;  (** Length of this unit excluding the length field *)
    version : u16;  (** DWARF version number *)
    debug_abbrev_offset : u64;  (** Offset into .debug_abbrev section *)
    address_size : u8;  (** Size of addresses in bytes *)
    type_signature : u64;  (** 64-bit type signature *)
    type_offset : u64;  (** Offset to the type DIE within this unit *)
    span : span;  (** Span indicating the header's position and size *)
  }
  (** Parsed type unit header.

      DWARF 4 specification, section 7.5.1.2 "Type Unit Header". *)

  val parse_type_unit_header : Object.Buffer.cursor -> span * type_unit_header
  (** Parse a type unit header from the .debug_types section.

      @raise Parse_error
        if the header is malformed or the version is unsupported. *)

  val parse_type_units : Object.Buffer.t -> (span * type_unit_header) Seq.t
  (** Iterate all type units in the .debug_types section. *)
end

val resolve_location_list :
  Object.Buffer.t -> u64 -> int -> DebugLoc.entry list option
(** [resolve_location_list buffer offset address_size] resolves a
    DW_FORM_sec_offset value into a parsed DWARF 4 location list from the
    .debug_loc section. Returns [None] if the section is absent. *)

val resolve_range_list :
  Object.Buffer.t -> u64 -> int -> DebugRanges.entry list option
(** [resolve_range_list buffer offset address_size] resolves a
    DW_FORM_sec_offset value into a parsed DWARF 4 range list from the
    .debug_ranges section. Returns [None] if the section is absent. *)

(** DWARF 4 .debug_pubnames section parsing.

    The .debug_pubnames section contains a lookup table mapping globally visible
    names to DIE offsets in .debug_info. Each set covers one compilation unit
    and contains a header followed by (offset, name) pairs terminated by a zero
    offset.

    Superseded by .debug_names in DWARF 5.

    DWARF 4 specification, section 6.1.1 "Lookup by Name". *)
module DebugPubnames : sig
  type header = {
    format : dwarf_format;  (** Either DWARF32 or DWARF64. *)
    unit_length : u64;  (** Length of this set excluding the length field *)
    version : u16;  (** DWARF version number (typically 2) *)
    debug_info_offset : u64;
        (** Offset of the compilation unit in .debug_info *)
    debug_info_length : u64;  (** Size of the compilation unit in .debug_info *)
    span : span;  (** Position and size of this header *)
  }
  (** Header for a pubnames set.

      DWARF 4 specification, section 7.19 "Name Lookup Tables". *)

  type entry = { offset : u64; name : string }
  (** A name entry mapping a DIE offset to a symbol name.

      DWARF 4 specification, section 7.19 "Name Lookup Tables". *)

  val parse_header : Object.Buffer.cursor -> header
  (** Parse the header of a pubnames set.

      @raise Parse_error if the length field uses a reserved value. *)

  val parse_entries : Object.Buffer.cursor -> header -> entry list
  (** Parse entries following a pubnames header until a zero offset terminator.
  *)

  val parse_set : Object.Buffer.cursor -> header * entry list
  (** Parse one pubnames set (header + entries). Convenience function combining
      [parse_header] and [parse_entries].

      @raise Parse_error if the set is malformed. *)
end

(** DWARF 4 .debug_pubtypes section parsing.

    The .debug_pubtypes section contains a lookup table mapping globally visible
    type names to DIE offsets in .debug_info. The format is identical to
    .debug_pubnames but indexes type definitions instead of object and function
    names.

    Superseded by .debug_names in DWARF 5.

    DWARF 4 specification, section 6.1.1 "Lookup by Name". *)
module DebugPubtypes : sig
  type header = {
    format : dwarf_format;  (** Either DWARF32 or DWARF64. *)
    unit_length : u64;  (** Length of this set excluding the length field *)
    version : u16;  (** DWARF version number (typically 2) *)
    debug_info_offset : u64;
        (** Offset of the compilation unit in .debug_info *)
    debug_info_length : u64;  (** Size of the compilation unit in .debug_info *)
    span : span;  (** Position and size of this header *)
  }
  (** Header for a pubtypes set.

      DWARF 4 specification, section 7.19 "Name Lookup Tables". *)

  type entry = { offset : u64; name : string }
  (** A type entry mapping a DIE offset to a type name.

      DWARF 4 specification, section 7.19 "Name Lookup Tables". *)

  val parse_header : Object.Buffer.cursor -> header
  (** Parse the header of a pubtypes set.

      @raise Parse_error if the length field uses a reserved value. *)

  val parse_entries : Object.Buffer.cursor -> header -> entry list
  (** Parse entries following a pubtypes header until a zero offset terminator.
  *)

  val parse_set : Object.Buffer.cursor -> header * entry list
  (** Parse one pubtypes set (header + entries). Convenience function combining
      [parse_header] and [parse_entries].

      @raise Parse_error if the set is malformed. *)
end

(** Call frame information parsing for .debug_frame section.

    The .debug_frame section describes how to unwind the call stack at any
    program counter, enabling debuggers to produce accurate backtraces and
    exception handlers to unwind correctly.

    The section contains two kinds of entries:
    - Common Information Entries (CIEs): shared parameters and initial register
      rules for a group of functions
    - Frame Description Entries (FDEs): per-function instructions that modify
      CIE defaults to describe register saves and the Canonical Frame Address
      (CFA) at each instruction

    DWARF 5 specification, section 6.4 "Call Frame Information". *)
module CallFrame : sig
  type common_information_entry = {
    format : dwarf_format;  (** Either DWARF32 or DWARF64. *)
    length : u64;  (** Length of the CIE excluding this field *)
    cie_id : u64;  (** Distinguished CIE identifier (0xffffffff) *)
    version : u8;  (** DWARF version number (5 for DWARF 5) *)
    augmentation : string;  (** Null-terminated augmentation string *)
    address_size : u8;  (** Size of target address in bytes (4 or 8) *)
    segment_selector_size : u8;
        (** Size of segment selector in bytes (usually 0) *)
    code_alignment_factor : u64;  (** Alignment factor for code addresses *)
    data_alignment_factor : i64;
        (** Signed alignment factor for stack offsets *)
    return_address_register : u64;
        (** Register number containing return address *)
    augmentation_length : u64 option;
        (** Length of augmentation data if present *)
    augmentation_data : string option;
        (** Implementation-specific augmentation data *)
    initial_instructions : string;  (** Call frame instructions for this CIE *)
    span : span;  (** Tracks exact header size for precise parsing *)
    offset : u64;  (** Section-relative offset where CIE starts *)
  }
  (** Common Information Entry (CIE).

      A CIE contains information shared among many Frame Description Entries
      (FDEs). There is at least one CIE in every non-empty [.debug_frame]
      section. CIEs define common parameters and initial call frame state that
      multiple functions can share.

      The alignment factors enable compact encoding of frame instructions:
      [code_alignment_factor] scales PC advance values (often set to the minimum
      instruction size), while [data_alignment_factor] scales stack offsets
      (often negative, e.g. -8 on 64-bit). The [initial_instructions] establish
      the baseline register rules that FDEs modify.

      DWARF 5 specification, section 6.4.1 "Structure of Call Frame
      Information". *)

  val create_default_cie : unit -> common_information_entry
  (** Create a CIE with default values for x86-64 architecture. *)
  (* TODO Maybe default to x86-64 but take an optional architecture here? *)

  type frame_description_entry = {
    format : dwarf_format;  (** Either DWARF32 or DWARF64. *)
    length : u64;  (** Length of the FDE excluding this field *)
    cie_pointer : u64;  (** Offset to associated CIE in [Debug_frame] section *)
    initial_location : u64;
        (** Address of first instruction covered by this FDE *)
    address_range : u64;  (** Number of bytes of code covered by this FDE *)
    augmentation_length : u64 option;
        (** Length of augmentation data, if present *)
    augmentation_data : string option;
        (** Vendor-specific augmentation data, if present *)
    instructions : string;
        (** Call frame instructions for this address range *)
    span : span;  (** Position and size of this FDE *)
    offset : u64;  (** Section-relative offset where this FDE starts *)
  }
  (** Frame Description Entry (FDE).

      An FDE contains call frame information for a specific range of program
      addresses. Each FDE references a CIE for shared parameters, then provides
      its own instructions to describe register saves and CFA changes within its
      address range.

      DWARF 5 specification, section 6.4.1 "Structure of Call Frame
      Information". *)

  val parse_common_information_entry :
    Object.Buffer.cursor -> common_information_entry
  (** Parse a Common Information Entry from the [Debug_frame] section. The
      cursor should be positioned at the start of a CIE entry.

      @raise Parse_error if the CIE is malformed. *)

  (** Debug Frame section entry type. *)
  type debug_frame_entry =
    | CIE of common_information_entry  (** Common Information Entry *)
    | FDE of frame_description_entry  (** Frame Description Entry *)
    | Zero_terminator of int  (** Zero terminator at given position *)

  type debug_frame_section = {
    entries : debug_frame_entry list;  (** All entries in the section *)
    entry_count : int;  (** Number of CIE/FDE entries *)
  }
  (** Debug Frame section. *)

  val parse_debug_frame_section :
    Object.Buffer.cursor -> int -> debug_frame_section
  (** Parse complete debug_frame section from cursor.

      @param cursor Buffer cursor positioned at the start of debug_frame section
      @param section_size Size of the debug_frame section in bytes
      @return Parsed debug_frame section containing all CIE and FDE entries *)

  (** CFI rule types for state machine. *)
  type cfi_rule =
    | Rule_undefined
    | Rule_same_value
    | Rule_offset of int64  (** offset from CFA *)
    | Rule_val_offset of int64  (** value = CFA + offset *)
    | Rule_register of int  (** register number *)
    | Rule_expression of string  (** DWARF expression *)
    | Rule_val_expression of string  (** DWARF expression for value *)

  type cfi_state = {
    cfa_register : int;  (** Register defining the CFA *)
    cfa_offset : int64;  (** Offset from [cfa_register] to the CFA *)
    register_rules : (int, cfi_rule) Hashtbl.t;
        (** Recovery rule per register number *)
    pc_offset : int;  (** Current location within the frame's code range *)
    state_stack : cfi_state list;
        (** Saved states for DW_CFA_remember_state/restore_state *)
  }
  (** CFI state for tracking register rules. *)

  val initial_cfi_state : unit -> cfi_state
  (** Create initial CFI state with architecture defaults *)

  val parse_initial_state : common_information_entry -> cfi_state
  (** Parse CIE initial instructions to establish proper initial CFI state.

      This function processes the initial_instructions field of a CIE to
      establish the baseline Call Frame Information state that FDEs can modify.
  *)

  val parse_cfi_instructions : string -> int64 -> int64 -> (int * string) list
  (** Parse CFI instructions into human-readable descriptions.

      @param instructions Raw CFI instruction bytes
      @param code_alignment Code alignment factor from CIE (scales PC offsets)
      @param data_alignment
        Data alignment factor from CIE (scales stack offsets)

      Returns list of (pc_offset, description) pairs showing CFI rules. *)

  val parse_augmentation_string : Object.Buffer.cursor -> string
  (** Parse a CIE augmentation string (a NUL-terminated string). *)

  val parse_augmentation_data :
    Object.Buffer.cursor -> string -> (u64 * string) option
  (** Parse CIE augmentation data when the augmentation string begins with 'z'.
      Returns the [(length, data)] pair, or [None] when absent. *)

  val parse_instructions : Object.Buffer.cursor -> int -> string
  (** Read [length] bytes of call frame instructions as a raw string. *)
end

(** Accelerated Name Lookup parsing for .debug_names section.

    This module provides support for parsing DWARF 5 accelerated name lookup
    tables, which enable efficient access to debugging information by name. The
    debug_names section contains hash tables and indices that allow fast lookup
    of DIEs (Debug Information Entries) by name, eliminating the need for linear
    scanning through all debugging information.

    The .debug_names section is crucial for debugger performance, enabling:
    - Setting breakpoints by function name
    - Looking up global variables by name
    - Resolving type names during expression evaluation
    - Locating compilation units containing specific symbols
    - Fast symbol table operations

    Structure of the debug_names section:
    + Name Index Header: Contains sizes, counts, and format information
    + Compilation Unit Offsets: Array of offsets to compilation unit headers
    + Type Unit Information: Local and foreign type unit references
    + Hash Table: Bucket array for efficient name hashing using DJB2 algorithm
    + Name Table: Array of string offsets into .debug_str section
    + Entry Pool: Encoded entries with DIE offsets and attributes

    The hash table uses a simple chaining mechanism where each bucket contains
    an index into the name table. Multiple names can hash to the same bucket,
    forming a chain that must be traversed during lookup. Each name table entry
    corresponds to one or more entries in the entry pool, which contain the
    actual DIE offsets and associated attributes like DW_IDX_compile_unit,
    DW_IDX_type_unit, DW_IDX_die_offset, DW_IDX_parent, DW_IDX_type_hash, etc.

    This unified approach replaced the older debug_pubnames, debug_pubtypes,
    debug_gnu_pubnames, and debug_gnu_pubtypes sections with a more flexible and
    efficient design.

    DWARF 5 specification, section 6.1 "Accelerated Access". *)
module DebugNames : sig
  type name_index_header = {
    format : dwarf_format;  (** Either DWARF32 or DWARF64. *)
    unit_length : u64;
        (** Total length of name index excluding the length field *)
    version : u16;  (** Version number (5 for DWARF 5) *)
    padding : u16;  (** Reserved padding field, must be zero *)
    comp_unit_count : u32;  (** Number of compilation units indexed *)
    local_type_unit_count : u32;  (** Number of local type units indexed *)
    foreign_type_unit_count : u32;
        (** Number of foreign type units referenced *)
    bucket_count : u32;  (** Number of hash table buckets *)
    name_count : u32;  (** Number of names in the name table *)
    abbrev_table_size : u32;  (** Size in bytes of abbreviation table *)
    augmentation_string_size : u32;  (** Size of augmentation string *)
    augmentation_string : string;
        (** Implementation-specific augmentation data *)
    span : int;  (** Total size of header in bytes *)
  }
  (** Name index header.

      The name index header contains metadata about the structure and contents
      of the debug_names section. It defines the sizes of the various tables and
      arrays that follow the header.

      DWARF 5 specification, section 6.1.1.4.1 "Section Header". *)

  type debug_str_entry = {
    offset : u32;  (** Original offset in debug_str section *)
    value : string;  (** Resolved string value *)
  }
  (** String with original offset preserved for debug_names *)

  type entry_parse_result = {
    name_offset : u32;  (** Offset of name in debug_str section *)
    die_offset : u32;  (** Offset of DIE in debug_info section *)
    tag_name : string;  (** Human-readable tag name *)
    offset_hex : string;  (** Hexadecimal representation of offset *)
    unit_index : int option;  (** Index of compilation unit *)
    is_declaration : bool;  (** Whether this is a declaration *)
    compile_unit_index : u32 option;  (** DWARF 5 DW_IDX_compile_unit *)
    type_unit_index : u32 option;  (** DWARF 5 DW_IDX_type_unit *)
    type_hash : u64 option;  (** DWARF 5 DW_IDX_type_hash *)
  }
  (** Result of parsing a single entry from the entry pool *)

  type name_index_entry = {
    name_offset : u32;  (** Offset into .debug_str section for symbol name *)
    die_offset : u32;  (** Offset to Debug Information Entry in .debug_info *)
    attributes : (name_index_attribute * u64) list;
        (** Additional DIE attributes and indices *)
  }
  (** Name index entry from the entry pool.

      Each name index entry represents one named symbol (function, variable,
      type, etc.) and provides the information needed to locate its Debug
      Information Entry (DIE).

      The entry contains:
      - name_offset: Points to the symbol's name string in the .debug_str
        section
      - die_offset: Points to the actual DIE in the .debug_info section
      - attributes: Additional metadata encoded as DW_IDX_ * attribute pairs

      Common attributes include:
      - DW_IDX_compile_unit: Index of compilation unit containing this DIE
      - DW_IDX_type_unit: Index of type unit (for type entries)
      - DW_IDX_die_offset: Offset within compilation/type unit
      - DW_IDX_parent: Index of parent entry for nested scopes
      - DW_IDX_type_hash: Hash signature for type units

      Multiple entries can share the same name_offset when symbols have
      identical names (e.g., overloaded functions, variables in different
      scopes).

      DWARF 5 specification, section 6.1.1.4.8 "Entry Pool". *)

  type debug_names_abbrev = {
    code : u64;  (** Abbreviation code *)
    tag : abbreviation_tag;  (** DWARF tag *)
    attributes : (name_index_attribute * attribute_form_encoding) list;
        (** List of attributes and their forms *)
  }
  (** Abbreviation entry for debug_names *)

  type debug_names_section = {
    header : name_index_header;  (** Section header with counts and sizes *)
    comp_unit_offsets : u32 array;
        (** Offsets to compilation unit headers in .debug_info *)
    local_type_unit_offsets : u32 array;
        (** Offsets to local type unit headers *)
    foreign_type_unit_signatures : u64 array;
        (** Type signatures for foreign type units *)
    buckets : u32 array;  (** Hash bucket organization *)
    hash_table : u32 array;
        (** Hash buckets containing indices into name_table *)
    name_table : debug_str_entry array;
        (** Symbol names with original offsets preserved *)
    entry_offsets : u32 array;
        (** Entry offsets into entry pool for each name *)
    abbreviation_table : debug_names_abbrev list;
        (** Parsed abbreviation table *)
    entry_pool : name_index_entry array;
        (** All name index entries with DIE information *)
  }
  (** Complete parsed debug_names section with all components resolved.

      DWARF 5 specification, section 6.1.1 "Lookup by Name". *)

  val djb2_hash : string -> u32
  (** Compute DJB2 hash value used by DWARF 5 debug_names sections *)

  val resolve_debug_str_offset : Object.Buffer.t -> u32 -> debug_str_entry
  (** Resolve debug_str offset to debug_str_entry with both offset and resolved
      string *)

  val calculate_entry_address : u32 -> int -> u32
  (** Calculate absolute byte address given section base offset and relative
      offset *)

  val calculate_section_addresses :
    u32 -> name_index_header -> (string * u32) list
  (** Calculate byte addresses for all components of a debug_names section *)

  val parse_debug_names_section :
    Object.Buffer.cursor -> Object.Buffer.t -> debug_names_section
  (** Parse a complete debug_names section from the [Debug_names] section.

      This function parses all components of a DWARF 5 debug_names section,
      building the complete acceleration structure for efficient name-based
      lookups.

      The parsing process:
      + Read name index header with table sizes and counts
      + Parse compilation unit offset array
      + Parse local and foreign type unit arrays
      + Read hash table buckets for name lookup acceleration
      + Parse name table (resolve string offsets from .debug_str section)
      + Decode entry pool using abbreviation table format
      + Associate entries with names and hash buckets

      @param cursor Buffer cursor positioned at start of debug_names section
      @return
        Complete debug_names_section with all tables populated and resolved

      @raise Parse_error if section format is invalid or unsupported version

      Performance note: The resulting structure enables O(1) name lookups via
      the hash table, significantly faster than linear DIE scanning.

      DWARF 5 specification, section 6.1.1 "Lookup by Name". *)

  val parse_all_entries_for_name :
    Object.Buffer.t ->
    debug_names_section ->
    int ->
    int ->
    entry_parse_result list
  (** Parse all entries for a given name index according to DWARF 5
      specification. Returns a list of entry parse results with structured data.
  *)

  val calculate_entry_pool_offset : name_index_header -> int
  (** Calculate entry pool offset based on header information *)

  val find_bucket_index : string -> int -> int
  (** Find bucket index for a given name using DJB2 hash algorithm.

      @param name The symbol name to hash
      @param bucket_count Total number of buckets in the hash table
      @return Bucket index (0-based) where this name should be found *)

  val get_name_indices_for_bucket : debug_names_section -> int -> int list
  (** Get all name indices for entries in a specific hash bucket.

      @param debug_names Parsed debug_names section
      @param bucket_index Bucket index to search
      @return List of name table indices that hash to this bucket *)

  val find_name_indices : debug_names_section -> string -> int list
  (** Find all name table indices that match a given name exactly.

      @param debug_names Parsed debug_names section
      @param name Symbol name to search for
      @return List of matching name table indices *)

  val find_entries_by_name :
    Object.Buffer.t ->
    debug_names_section ->
    int ->
    string ->
    entry_parse_result list
  (** Find all entries (DIEs) that match a given name.

      This is the core name lookup function that uses the hash table to
      efficiently locate all DIEs with the specified name.

      @param buffer Object buffer containing DWARF data
      @param debug_names Parsed debug_names section
      @param section_offset Offset of debug_names section in buffer
      @param name Symbol name to search for
      @return List of matching entry_parse_result structures *)

  val lookup_symbols_by_name :
    Object.Buffer.t ->
    debug_names_section ->
    int ->
    string ->
    entry_parse_result list
  (** Find all symbols (any kind of DIE) matching a name.

      This function searches for any symbol with the given name, regardless of
      type (functions, variables, types, etc.). It's an alias for
      find_entries_by_name with a more descriptive name for general symbol
      lookup.

      @param buffer Object buffer containing DWARF data
      @param debug_names Parsed debug_names section
      @param section_offset Offset of debug_names section in buffer
      @param name Symbol name to search for
      @return List of matching entry_parse_result structures *)

  val find_functions_by_name :
    Object.Buffer.t ->
    debug_names_section ->
    int ->
    string ->
    entry_parse_result list
  (** Find specifically function DIEs by name.

      This function filters results to only include function-like DIEs such as
      DW_TAG_subprogram entries. Useful for debuggers looking specifically for
      callable functions.

      @param buffer Object buffer containing DWARF data
      @param debug_names Parsed debug_names section
      @param section_offset Offset of debug_names section in buffer
      @param name Function name to search for
      @return List of matching function entry_parse_result structures *)

  val find_types_by_name :
    Object.Buffer.t ->
    debug_names_section ->
    int ->
    string ->
    entry_parse_result list
  (** Find specifically type DIEs by name.

      This function filters results to only include type definition DIEs such as
      DW_TAG_structure_type, DW_TAG_class_type, DW_TAG_typedef, etc. Useful for
      type lookup in debuggers and analysis tools.

      @param buffer Object buffer containing DWARF data
      @param debug_names Parsed debug_names section
      @param section_offset Offset of debug_names section in buffer
      @param name Type name to search for
      @return List of matching type entry_parse_result structures *)

  val get_all_symbol_names : debug_names_section -> string list
  (** Get all symbol names available in the debug_names section.

      This function extracts all symbol names from the name table, which can be
      useful for symbol completion, browsing available symbols, or building
      symbol indices.

      @param debug_names Parsed debug_names section
      @return List of all symbol names in the section *)

  val search_names_with_prefix : debug_names_section -> string -> string list
  (** Find names matching a prefix.

      This function searches for all symbol names that start with the given
      prefix. Useful for implementing symbol name completion in debuggers and
      IDEs.

      @param debug_names Parsed debug_names section
      @param prefix Prefix to search for
      @return List of symbol names that start with the given prefix *)

  val filter_entries_by_tag :
    abbreviation_tag -> entry_parse_result list -> entry_parse_result list
  (** Filter entries by a specific abbreviation tag.

      @param tag Target DWARF tag to filter by
      @param entries List of entries to filter
      @return Filtered list containing only entries with the specified tag *)

  val filter_entries_by_tags :
    abbreviation_tag list -> entry_parse_result list -> entry_parse_result list
  (** Filter entries by multiple abbreviation tags.

      @param tags List of DWARF tags to filter by
      @param entries List of entries to filter
      @return
        Filtered list containing only entries with any of the specified tags *)

  val find_variables_by_name :
    Object.Buffer.t ->
    debug_names_section ->
    int ->
    string ->
    entry_parse_result list
  (** Find specifically variable DIEs by name.

      This function filters results to only include variable-like DIEs such as
      DW_TAG_variable, DW_TAG_formal_parameter, and DW_TAG_constant entries.

      @param buffer Object buffer containing DWARF data
      @param debug_names Parsed debug_names section
      @param section_offset Offset of debug_names section in buffer
      @param name Variable name to search for
      @return List of matching variable entry_parse_result structures *)

  val find_namespaces_by_name :
    Object.Buffer.t ->
    debug_names_section ->
    int ->
    string ->
    entry_parse_result list
  (** Find specifically namespace or module DIEs by name.

      This function filters results to only include namespace-like DIEs such as
      DW_TAG_namespace and DW_TAG_module entries.

      @param buffer Object buffer containing DWARF data
      @param debug_names Parsed debug_names section
      @param section_offset Offset of debug_names section in buffer
      @param name Namespace/module name to search for
      @return List of matching namespace entry_parse_result structures *)

  val search_entries_with_pattern :
    Object.Buffer.t ->
    debug_names_section ->
    int ->
    string ->
    entry_parse_result list
  (** Search entries with regex pattern matching on names.

      This function allows for complex pattern-based searches using regular
      expressions. Useful for finding symbols with complex naming patterns.

      @param buffer Object buffer containing DWARF data
      @param debug_names Parsed debug_names section
      @param section_offset Offset of debug_names section in buffer
      @param pattern Regular expression pattern to match against symbol names
      @return List of matching entry_parse_result structures *)

  val find_entries_in_compilation_unit :
    entry_parse_result list -> u32 -> entry_parse_result list
  (** Find entries within a specific compilation unit.

      This function filters entries to only include those belonging to a
      specific compilation unit index, leveraging the DWARF 5
      DW_IDX_compile_unit attribute.

      @param entries List of entries to filter
      @param cu_index Compilation unit index to filter by
      @return
        Filtered list containing only entries from the specified compilation
        unit *)

  val find_entries_in_type_unit :
    entry_parse_result list -> u32 -> entry_parse_result list
  (** Find entries within a specific type unit.

      This function filters entries to only include those belonging to a
      specific type unit index, leveraging the DWARF 5 DW_IDX_type_unit
      attribute.

      @param entries List of entries to filter
      @param tu_index Type unit index to filter by
      @return Filtered list containing only entries from the specified type unit
  *)

  val find_entries_with_type_hash :
    entry_parse_result list -> u64 -> entry_parse_result list
  (** Find entries with a specific type hash.

      This function filters entries to only include those with a specific type
      hash, leveraging the DWARF 5 DW_IDX_type_hash attribute for type
      deduplication.

      @param entries List of entries to filter
      @param type_hash Type hash value to filter by
      @return Filtered list containing only entries with the specified type hash
  *)

  val find_children_entries :
    Object.Buffer.t ->
    debug_names_section ->
    int ->
    entry_parse_result ->
    entry_parse_result list
  (** Find children of a given entry using parent offset relationships.

      This function searches for all entries that reference the given entry as
      their parent, leveraging the DWARF 5 parent-child relationships in the
      debug_names section.

      @param buffer Object buffer containing DWARF data
      @param debug_names Parsed debug_names section
      @param section_offset Offset of debug_names section in buffer
      @param parent_entry Entry to find children for
      @return List of child entries *)

  val find_sibling_entries :
    entry_parse_result list -> entry_parse_result -> entry_parse_result list
  (** Find all entries that belong to the same compilation unit as a given
      entry.

      This function finds sibling entries by matching compilation unit indices,
      excluding the target entry itself.

      @param entries List of entries to search within
      @param target_entry Entry to find siblings for
      @return List of sibling entries from the same compilation unit *)

  val group_entries_by_compilation_unit :
    entry_parse_result list -> (u32 * entry_parse_result list) list
  (** Group entries by their compilation unit.

      This function organizes entries into groups based on their compilation
      unit index, leveraging the DWARF 5 DW_IDX_compile_unit attribute.

      @param entries List of entries to group
      @return List of (compilation_unit_index, entries) pairs *)

  val group_entries_by_type_unit :
    entry_parse_result list -> (u32 * entry_parse_result list) list
  (** Group entries by their type unit.

      This function organizes entries into groups based on their type unit
      index, leveraging the DWARF 5 DW_IDX_type_unit attribute.

      @param entries List of entries to group
      @return List of (type_unit_index, entries) pairs *)

  val group_entries_by_type_hash :
    entry_parse_result list -> (u64 * entry_parse_result list) list
  (** Group entries by their type hash.

      This function groups entries with the same type hash, which is useful for
      type deduplication and finding equivalent types across compilation units.

      @param entries List of entries to group
      @return List of (type_hash, entries) pairs *)

  type entry_tree = { entry : entry_parse_result; children : entry_tree list }
  (** Hierarchical tree structure representing parent-child relationships
      between entries *)

  val build_entry_hierarchy :
    Object.Buffer.t ->
    debug_names_section ->
    int ->
    entry_parse_result list ->
    entry_tree list
  (** Build a hierarchical tree structure from entries using parent
      relationships.

      This function constructs a tree representation of the symbol hierarchy,
      showing parent-child relationships between debug information entries.

      @param buffer Object buffer containing DWARF data
      @param debug_names Parsed debug_names section
      @param section_offset Offset of debug_names section in buffer
      @param root_entries List of root entries to build trees from
      @return List of entry trees showing hierarchical relationships *)

  val find_root_entries_in_compilation_unit :
    entry_parse_result list -> u32 -> entry_parse_result list
  (** Find root entries (entries with no parent) in a compilation unit.

      This function identifies top-level entries within a specific compilation
      unit that have no parent references.

      @param entries List of entries to search within
      @param cu_index Compilation unit index to search in
      @return List of root entries in the specified compilation unit *)

  val find_compilation_unit_for_die : debug_names_section -> u32 -> u32 option
  (** Find the compilation unit index that contains a specific DIE offset.

      This function determines which compilation unit contains a given DIE
      offset by searching through the compilation unit offset array.

      @param debug_names Parsed debug_names section
      @param die_offset DIE offset to search for
      @return Optional compilation unit index containing the DIE *)

  val get_compilation_unit_offset : debug_names_section -> u32 -> u32 option
  (** Get the compilation unit offset for a given index.

      @param debug_names Parsed debug_names section
      @param cu_index Compilation unit index
      @return Optional compilation unit offset *)

  val get_all_compilation_unit_offsets : debug_names_section -> u32 array
  (** Get all compilation unit offsets from debug_names section.

      @param debug_names Parsed debug_names section
      @return Array of all compilation unit offsets *)
end

(** String table parsing for .debug_str section.

    Provides support for parsing DWARF string tables, which contain
    null-terminated strings referenced by other sections via offsets. The string
    table centralises string storage and enables efficient string sharing across
    debug information entries.

    DWARF 5 specification, section 7.5 "Format of Debugging Information". *)
module DebugStr : sig
  type string_entry = {
    offset : int;  (** Offset of string in debug_str section *)
    length : int;  (** Length of string excluding null terminator *)
    content : string;  (** The actual string content *)
  }
  (** Individual string entry with location and content information. *)

  (* TODO Can we load this on-demand? All the strings in the section will
     be very large! *)
  type t = {
    entries : string_entry array;  (** Array of all strings in the section *)
    total_size : int;  (** Total size of the debug_str section *)
  }
  (** Complete parsed debug_str section *)

  val parse : Object.Buffer.t -> t option
  (** Parse the complete .debug_str section from buffer.

      @param buffer Object buffer containing the DWARF data
      @return Optional parsed string table, None if section not found
      @raise Parse_error if section format is invalid *)
end

(** Line string table parsing for .debug_line_str section.

    This module provides support for parsing DWARF line string tables, which
    contain null-terminated strings specifically used by the line number
    program. These strings are typically file names and directory paths
    referenced by line table entries via DW_FORM_line_strp form.

    DWARF 5 specification, section 6.2 "Line Number Information". *)
module DebugLineStr : sig
  type string_entry = {
    offset : int;  (** Offset of string in debug_line_str section *)
    length : int;  (** Length of string excluding null terminator *)
    content : string;  (** The actual string content *)
  }
  (** Individual string entry with location and content information. *)

  type t = {
    entries : string_entry array;  (** Array of all strings in the section *)
    total_size : int;  (** Total size of the debug_line_str section *)
  }
  (** Complete parsed debug_line_str section *)

  val parse : Object.Buffer.t -> t option
  (** Parse the complete .debug_line_str section from buffer.

      @param buffer Object buffer containing the DWARF data
      @return Optional parsed line string table, None if section not found
      @raise Parse_error if section format is invalid *)

  val iter : (string_entry -> unit) -> t -> unit
  (** Iterate over every string entry in the section. *)

  val find_string_at_offset : t -> int -> string option
  (** [find_string_at_offset t offset] returns the string whose entry begins at
      [offset] in the [.debug_line_str] section, or [None] if no entry matches.
  *)
end

val create : Object.Buffer.t -> t
(** Create a DWARF context over an object buffer. The context holds no parsed
    data initially; sections are parsed and cached on first access. *)

val parse_compile_units : t -> CompileUnit.t Seq.t
(** Parse all compile units from the [Debug_info] section lazily *)

val get_abbrev_table : t -> size_t -> (u64, abbrev) Hashtbl.t
(** Return the abbreviation table at the given [.debug_abbrev] offset, parsing
    it on first request and caching it in the context for reuse (many compile
    units share a table). The cached, high-level counterpart of the stateless
    {!DebugAbbrev.parse}.

    @raise Parse_error if the abbreviation table is malformed. *)

(** Abbreviation table parsing for .debug_abbrev section.

    The .debug_abbrev section contains abbreviation declarations used by
    .debug_info to encode DIEs compactly. Each compilation unit references an
    abbreviation table at a specific offset within this section. *)
module DebugAbbrev : sig
  val parse :
    Object.Buffer.t -> Unsigned.UInt32.t -> (u64, abbrev) Hashtbl.t option
  (** Parse the abbreviation table at the given offset within the .debug_abbrev
      section. Returns [None] if the section is not found.

      @raise Parse_error if the abbreviation table is malformed. *)

  val parse_all : Object.Buffer.t -> (u64, abbrev) Hashtbl.t list
  (** Parse all abbreviation tables from the .debug_abbrev section.

      @raise Parse_error if an abbreviation table is malformed. *)
end

(** String Offset Tables parsing for .debug_str_offsets section.

    This module provides support for parsing DWARF 5 string offset tables, which
    enable indirect access to strings in the .debug_str section. Instead of
    embedding string offsets directly in DIEs (Debug Information Entries), DWARF
    5 uses string indices that reference this offset table, resulting in more
    compact representation and better string sharing across compilation units.

    The .debug_str_offsets section provides several benefits:
    - Reduced DIE size by using smaller indices instead of full offsets
    - Better string deduplication across compilation units
    - More efficient linking when combining object files
    - Improved compression of debug information

    The offset table is typically referenced via DW_FORM_strx* forms in
    attribute values, where the form value is an index into this table rather
    than a direct offset into .debug_str.

    DWARF 5 specification, section 7.26 "String Offsets Table". *)
module DebugStrOffsets : sig
  type header = {
    format : dwarf_format;  (** Either DWARF32 or DWARF64. *)
    unit_length : u64;
        (** Length of this contribution excluding the length field *)
    version : u16;  (** DWARF version number (typically 5) *)
    padding : u16;  (** Reserved padding field, must be zero *)
    span : span;  (** Span indicating the header's position and size *)
  }
  (** Header structure for a string offsets contribution.

      Each contribution starts with a header containing the total length, DWARF
      version, and padding for alignment. Multiple contributions can exist in a
      single [.debug_str_offsets] section. *)

  type offset_entry = {
    offset : u64;  (** Offset into .debug_str section *)
    resolved_string : string option;  (** Resolved string content if available *)
  }
  (** An individual offset entry with optional resolved string content.

      Each entry contains a 32-bit offset into the [.debug_str] section. The
      [resolved_string] field is populated when the parser has access to the
      [.debug_str] section and can resolve the actual string content. *)

  type t = {
    header : header;  (** Section header *)
    offsets : offset_entry array;  (** Array of string offset entries *)
  }
  (** Complete parsed string offsets table.

      Contains the header information and an array of all offset entries for
      this contribution. The array index corresponds to the string index used in
      DWARF 5 forms like [DW_FORM_strx]. *)

  val parse_header : Object.Buffer.cursor -> header
  (** Parse the header of a debug_str_offsets contribution.

      @param cursor Buffer cursor positioned at start of contribution header
      @return Parsed header structure
      @raise Parse_error if header format is invalid or unsupported version *)

  val parse_offsets :
    Object.Buffer.cursor ->
    header ->
    (u32 * u64) option ->
    Object.Buffer.t ->
    offset_entry array
  (** Parse the offset array with optional string resolution.

      Reads the array of 32-bit offsets following the header. If debug_str
      section information is provided, attempts to resolve each offset to its
      actual string content.

      @param cursor Buffer cursor positioned after header
      @param header Previously parsed header for size calculation
      @param debug_str_info Optional (offset, size) of .debug_str section
      @param buffer Full buffer for string resolution
      @return Array of offset entries with optional resolved strings *)

  val parse : Object.Buffer.t -> u32 -> t
  (** Parse a complete debug_str_offsets contribution from buffer.

      This is the main entry point for parsing a string offsets table. It
      combines header parsing and offset array parsing with automatic string
      resolution if the [.debug_str] section is available.

      @param buffer Object buffer containing the DWARF data
      @param section_offset Offset to start of .debug_str_offsets section
      @return Complete parsed string offsets table
      @raise Parse_error if section format is invalid

      Usage example:
      {@ocaml skip[
        let str_offsets = DebugStrOffsets.parse buffer section_offset in
        let first_string = str_offsets.offsets.(0).resolved_string
      ]}

      Performance note: String resolution is performed eagerly during parsing.
  *)
end

(** Address table parsing for .debug_addr section.

    This module provides support for parsing DWARF 5 address tables, which
    enable indirect access to addresses in debug information. Instead of
    embedding full addresses directly in DIEs (Debug Information Entries), DWARF
    5 uses address indices that reference this address table via forms like
    DW_FORM_addrx*, enabling more compact representation and better
    relocatability.

    The .debug_addr section provides several advantages:
    - Reduced DIE size by using smaller indices instead of full addresses
    - Better support for position-independent code (PIC)
    - Improved linking performance for shared libraries
    - More efficient relocation processing
    - Enhanced support for split DWARF packages

    This indirection is particularly valuable for position-independent code and
    shared libraries where actual addresses are determined at load time rather
    than link time. The address table allows the debug information to remain
    compact while supporting full address resolution.

    DWARF 5 specification, section 7.27 "Address Table". *)
module DebugAddr : sig
  type header = {
    format : dwarf_format;  (** Either DWARF32 or DWARF64. *)
    unit_length : u64;
        (** Length of this contribution excluding length field *)
    version : u16;  (** DWARF version number (typically 5) *)
    address_size : u8;
        (** Size of addresses in bytes (4 for 32-bit, 8 for 64-bit) *)
    segment_selector_size : u8;
        (** Size of segment selectors in bytes (0 if no segments) *)
    span : span;  (** Header location and size information *)
  }
  (** Header structure for an address table contribution.

      Each contribution starts with a header containing the total length, DWARF
      version, and size information for addresses and segments. Multiple
      contributions can exist in a single [.debug_addr] section, each
      potentially with different address sizes for different architectures. *)

  type entry = {
    segment : u64 option;  (** Segment selector if segment_selector_size > 0 *)
    address : u64;  (** The actual address value *)
  }
  (** An individual address entry with optional segment information.

      Each entry contains an address value and an optional segment selector. The
      segment is only present when [segment_selector_size > 0] in the header.
      Most modern systems use flat memory models with no segments. *)

  type t = {
    header : header;  (** Section header with size information *)
    entries : entry array;  (** Array of address entries *)
  }
  (** Complete parsed address table.

      Contains the header information and an array of all address entries for
      this contribution. The array index corresponds to the address index used
      in DWARF 5 forms like [DW_FORM_addrx]. *)

  val parse_header : Object.Buffer.cursor -> header
  (** Parse the header of a debug_addr contribution.

      @param cursor Buffer cursor positioned at start of contribution header
      @return Parsed header structure with size information
      @raise Parse_error if header format is invalid or unsupported version *)

  val parse_entries : Object.Buffer.cursor -> header -> entry array
  (** Parse the address entries following the header.

      Reads the array of addresses using the size information from the header.
      Each address is read according to [address_size] and segment selectors are
      included if [segment_selector_size > 0].

      @param cursor Buffer cursor positioned after header
      @param header Previously parsed header for size calculation
      @return Array of address entries
      @raise Parse_error
        if entries cannot be parsed or address sizes are invalid *)

  val parse : Object.Buffer.t -> u64 -> t
  (** Parse a complete debug_addr contribution from buffer.

      This is the main entry point for parsing an address table. It combines
      header parsing and address entry parsing to provide a complete address
      lookup table.

      @param buffer Object buffer containing the DWARF data
      @param section_offset Offset to start of .debug_addr section
      @return Complete parsed address table
      @raise Parse_error if section format is invalid

      Usage example:
      {@ocaml skip[
        let addr_table = DebugAddr.parse buffer section_offset in
        let first_address = addr_table.entries.(0).address
      ]}

      Performance note: This function parses the entire contribution eagerly.
      For large address tables, consider lazy parsing if only specific indices
      are needed. TODO Do we have a lazy parsing option in the library?

      Architecture note: The [address_size] field determines the width of
      addresses read from the section. This allows the same DWARF data to
      support both 32-bit and 64-bit architectures. *)
end

(** Address range table parsing for .debug_aranges section.

    This module provides support for parsing DWARF Address Range Tables, which
    enable efficient mapping from memory addresses to compilation units. Address
    range tables are crucial for debuggers and profilers to quickly determine
    which compilation unit contains a given program counter address without
    scanning through all debug information.

    The .debug_aranges section contains address range tables that specify:
    - Contiguous ranges of machine code addresses
    - Which compilation unit each address range belongs to
    - Fast lookup capability for address-to-unit mapping

    Address range tables are particularly useful for:
    - Stack unwinding and backtrace generation
    - Setting breakpoints by address
    - Source-level debugging when only addresses are available
    - Performance profiling and address attribution

    Each compilation unit can have its own address range table describing all
    the non-contiguous memory regions that contain code belonging to that unit.
    This is essential for optimized code where functions may be split across
    multiple memory regions.

    DWARF 5 specification, section 6.1.2 "Lookup by Address". *)
module DebugAranges : sig
  type header = {
    format : dwarf_format;  (** Either DWARF32 or DWARF64. *)
    unit_length : u64;
        (** Length of this aranges set excluding the length field *)
    version : u16;  (** DWARF version number (typically 2) *)
    debug_info_offset : u64;  (** Offset into .debug_info section *)
    address_size : u8;  (** Size of addresses in bytes *)
    segment_size : u8;  (** Size of segment selectors in bytes (usually 0) *)
    span : span;  (** Span indicating the header's position and size *)
  }
  (** Header structure for an address range table.

      Each compilation unit can have its own address range table describing the
      non-contiguous address ranges of code belonging to that unit.

      DWARF 5 specification, section 7.21 "Address Range Table". *)

  type address_range = {
    start_address : u64;  (** Beginning of address range *)
    length : u64;  (** Length of address range *)
  }
  (** A single address range entry.

      Represents a contiguous range of addresses belonging to a compilation
      unit. The range covers addresses from [start_address] to
      [start_address + length - 1].

      DWARF 5 specification, section 7.21 "Address Range Table". *)

  type aranges_set = {
    header : header;  (** Header information *)
    ranges : address_range list;  (** List of address ranges *)
  }
  (** Complete address range table for one compilation unit.

      Contains header information identifying the compilation unit and a list of
      all address ranges belonging to that unit.

      DWARF 5 specification, section 7.21 "Address Range Table". *)

  val parse_header : Object.Buffer.cursor -> header
  (** Parse the header of an aranges set.

      @raise Parse_error if the length field uses a reserved value. *)

  val parse_entries : Object.Buffer.cursor -> header -> address_range list
  (** Parse address range entries following an aranges header.

      @raise Parse_error if an unsupported address size is encountered. *)

  val parse : Object.Buffer.t -> aranges_set option
  (** Parse an address range table from buffer.

      Automatically detects and handles both ELF (.debug_aranges) and MachO
      (__debug_aranges) section naming conventions.

      @param buffer Object buffer containing the DWARF data
      @return Optional parsed address range table, None if section not found *)
end

(** Location list parsing for .debug_loclists section.

    This module provides support for parsing DWARF 5 Location Lists, which
    describe where variables and parameters can be found during program
    execution. Location lists are essential for debuggers to track variable
    locations as they move between registers, memory locations, or become
    temporarily unavailable during optimization.

    The .debug_loclists section contains location descriptions that specify:
    - Address ranges where variables are valid
    - How to find variables (register, memory address, computed location)
    - When variables are not available (optimized out, between scopes)

    Each location list consists of a series of location list entries (LLE) that
    describe contiguous ranges of program counter values and the corresponding
    location expressions. This replaces the older .debug_loc section format used
    in DWARF 4 and earlier.

    DWARF 5 specification, section 7.7.3 "Location Lists". *)
module DebugLoclists : sig
  type header = {
    format : dwarf_format;  (** Either DWARF32 or DWARF64. *)
    unit_length : u64;  (** Length of the location lists contribution *)
    version : u16;  (** Version identifier (DWARF 5) *)
    address_size : u8;  (** Size of addresses in bytes *)
    segment_size : u8;  (** Size of segment selectors in bytes (usually 0) *)
    offset_entry_count : u32;  (** Number of entries in the offset table *)
  }
  (** Header structure for a location lists contribution.

      The location lists section contains location descriptions that are
      referenced by debug information entries via DW_AT_location attributes.

      DWARF 5 specification, section 7.29 "Location List Table". *)

  (** A decoded location list entry (a DW_LLE_ encoding). Entries that describe
      a location carry the DWARF expression bytes in [expr].

      DWARF 5 specification, section 7.7.3 "Location Lists". *)
  type location_entry =
    | LLE_end_of_list  (** Marks the end of the location list *)
    | LLE_base_addressx of { index : int }
        (** New base address, as a .debug_addr index *)
    | LLE_startx_endx of { start_index : int; end_index : int; expr : string }
        (** Range with start and end as .debug_addr indices *)
    | LLE_startx_length of { start_index : int; length : u64; expr : string }
        (** Range with start as a .debug_addr index, plus a length *)
    | LLE_offset_pair of { start_offset : u64; end_offset : u64; expr : string }
        (** Range as start and end offsets from the current base address *)
    | LLE_default_location of { expr : string }
        (** Default location used when no other entry matches *)
    | LLE_base_address of { address : u64 }
        (** New base address, given directly *)
    | LLE_start_end of { start_addr : u64; end_addr : u64; expr : string }
        (** Range with explicit start and end addresses *)
    | LLE_start_length of { start_addr : u64; length : u64; expr : string }
        (** Range with an explicit start address, plus a length *)

  type location_list = { entries : location_entry list }
  (** A single decoded location list — the entries found at one offset. *)

  (* TODO [parse] populates [lists] eagerly; consider lazy parsing for large
     sections. *)
  type loclists_section = {
    header : header;
    offset_table : u64 array;  (** Section-relative offset of each list. *)
    lists : location_list array;  (** The decoded lists, one per offset. *)
  }
  (** A parsed [.debug_loclists] contribution: its [header], the [offset_table]
      indexing each list, and the decoded [lists] themselves. *)

  val parse_header : Object.Buffer.cursor -> header
  (** Parse the header of a loclists contribution.

      @raise Parse_error if the length field uses a reserved value. *)

  val parse_offset_table : Object.Buffer.cursor -> header -> u64 array
  (** Parse the offset table following a loclists header. *)

  val parse_location_list : Object.Buffer.cursor -> u8 -> location_list
  (** [parse_location_list cursor address_size] reads a single location list
      from [cursor] until a DW_LLE_end_of_list.

      @raise Parse_error if an unknown location list entry kind is encountered.
  *)

  val parse : Object.Buffer.t -> loclists_section option
  (** Parse the .debug_loclists section from a buffer. Convenience function
      combining [parse_header] and [parse_offset_table]. Returns [None] if the
      section is absent or malformed. *)

  val resolve_location_list :
    Object.Buffer.t -> u64 -> u8 -> location_list option
  (** [resolve_location_list buffer offset address_size] parses a single
      location list starting at [offset] bytes from the start of the
      .debug_loclists section. Returns [None] if the section is absent.

      @raise Parse_error if an unknown location list entry kind is encountered.
  *)
end

(** Parser for the .debug_rnglists section (DWARF 5).

    The range lists section contains range descriptions referenced by debug
    information entries via [DW_AT_ranges] attributes. It replaces .debug_ranges
    from DWARF 4.

    DWARF 5 specification, section 2.17.3 "Non-Contiguous Address Ranges". DWARF
    5 specification, section 7.28 "Range List Table". *)
module DebugRnglists : sig
  type header = {
    format : dwarf_format;  (** Either DWARF32 or DWARF64. *)
    unit_length : u64;  (** Length of the range lists contribution *)
    version : u16;  (** Version identifier (DWARF 5) *)
    address_size : u8;  (** Size of addresses in bytes *)
    segment_size : u8;  (** Size of segment selectors in bytes (usually 0) *)
    offset_entry_count : u32;  (** Number of entries in the offset table *)
  }
  (** Header for a range lists contribution.*)

  (** A decoded range list entry (a DW_RLE_ encoding).

      DWARF 5 specification, section 7.25. *)
  type range_entry =
    | RLE_end_of_list  (** Marks the end of the range list *)
    | RLE_base_addressx of { index : int }
        (** New base address, as a .debug_addr index *)
    | RLE_startx_endx of { start_index : int; end_index : int }
        (** Range with start and end as .debug_addr indices *)
    | RLE_startx_length of { start_index : int; length : u64 }
        (** Range with start as a .debug_addr index, plus a length *)
    | RLE_offset_pair of { start_offset : u64; end_offset : u64 }
        (** Range as start and end offsets from the current base address *)
    | RLE_base_address of { address : u64 }
        (** New base address, given directly *)
    | RLE_start_end of { start_addr : u64; end_addr : u64 }
        (** Range with explicit start and end addresses *)
    | RLE_start_length of { start_addr : u64; length : u64 }
        (** Range with an explicit start address, plus a length *)

  type range_list = { entries : range_entry list }
  (** A single decoded range list — the entries found at one offset. *)

  (* TODO [parse] populates [lists] eagerly; consider lazy parsing for large
     sections. *)
  type rnglists_section = {
    header : header;
    offset_table : u64 array;  (** Section-relative offset of each list. *)
    lists : range_list array;  (** The decoded lists, one per offset. *)
  }
  (** A parsed [.debug_rnglists] contribution: its [header], the [offset_table]
      indexing each list, and the decoded [lists] themselves. *)

  val parse_header : Object.Buffer.cursor -> header
  (** Parse the header of a rnglists contribution.

      @raise Parse_error if the length field uses a reserved value. *)

  val parse_offset_table : Object.Buffer.cursor -> header -> u64 array
  (** Parse the offset table following a rnglists header. *)

  val parse_range_list : Object.Buffer.cursor -> u8 -> range_list
  (** [parse_range_list cursor address_size] reads a single range list from
      [cursor] until a DW_RLE_end_of_list entry.

      @raise Parse_error if an unknown range list entry kind is encountered. *)

  val parse : Object.Buffer.t -> rnglists_section option
  (** Parse the .debug_rnglists section from a buffer. Convenience function
      combining [parse_header] and [parse_offset_table]. Returns [None] if the
      section is absent or malformed. *)

  val resolve_range_list : Object.Buffer.t -> u64 -> u8 -> range_list option
  (** [resolve_range_list buffer offset address_size] parses a single range list
      starting at [offset] bytes from the start of the .debug_rnglists section.
      Returns [None] if the section is absent.

      @raise Parse_error if an unknown range list entry kind is encountered. *)
end

(** Split DWARF support for .dwo files and .dwp packages.

    Split DWARF moves bulky debug information out of the main executable into
    separate .dwo (DWARF object) files or .dwp (DWARF package) files. This
    reduces link times and executable sizes while keeping full debug information
    available.

    A skeleton compilation unit ([DW_UT_skeleton]) in the main executable
    contains a [DW_AT_dwo_name] path and a [dwo_id] that identifies the matching
    split compilation unit ([DW_UT_split_compile]) in the .dwo file. The .dwp
    format aggregates multiple .dwo files into a single archive with index
    tables for efficient lookup.

    DWARF 5 specification, section 7.3.2 "Split DWARF Object Files". *)
module SplitDwarf : sig
  type dwo_context = {
    dwo_buffer : Object.Buffer.t;
        (** Buffer containing the .dwo or .dwp file *)
    parent_buffer : Object.Buffer.t;
        (** Buffer containing the main executable *)
    dwo_id : u64;  (** DWO identifier matching skeleton and split units *)
    contributions : (dwarf_section * int * int) list;
        (** Section contributions as (section, offset, size) from .dwp index *)
  }
  (** Context for accessing a single .dwo file paired with its parent
      executable. The [contributions] list is empty for standalone .dwo files
      and populated from the .dwp index for packaged units. *)

  val find_section : dwo_context -> dwarf_section -> (u64 * u64) option
  (** Find a DWARF section within the .dwo context. For [Debug_addr], looks in
      the parent buffer; for other sections, checks contributions first, then
      falls back to scanning the .dwo buffer. Returns [(offset, size)] if found.
  *)

  val resolve_string_index_dwo :
    Object.Buffer.t -> dwarf_format -> int -> string
  (** Resolve a string index via the .debug_str_offsets.dwo and .debug_str.dwo
      sections in a .dwo buffer. *)

  val dwo_abbrev_table : dwo_context -> u64 -> (u64, abbrev) Hashtbl.t
  (** Parse the abbreviation table from .debug_abbrev.dwo at the given offset.

      @raise Parse_error if the .dwo abbreviation table is malformed. *)

  val dwo_compile_units : dwo_context -> CompileUnit.t Seq.t
  (** Lazily parse all compilation units from .debug_info.dwo. *)

  val load_dwo :
    parent_buffer:Object.Buffer.t ->
    dwo_path:string ->
    dwo_id:u64 ->
    dwo_context option
  (** Load a .dwo file and verify it contains a [DW_UT_split_compile] unit
      matching [dwo_id]. Returns [None] if the file cannot be loaded or the DWO
      id does not match. *)

  val fixup_dwo_die : dwo_context -> dwarf_format -> u64 -> DIE.t -> DIE.t
  (** Resolve indexed strings and addresses in a DIE parsed from a .dwo file.
      Strings are resolved via the .dwo string tables; addresses are resolved
      via the parent executable's [.debug_addr] section using [addr_base]. *)

  val dwo_root_die : dwo_context -> CompileUnit.t -> u64 -> DIE.t option
  (** Parse and fix up the root DIE of a .dwo compilation unit. The [u64]
      parameter is the [addr_base] from the skeleton unit's [DW_AT_addr_base]
      attribute.

      @raise Parse_error if the .dwo unit is malformed. *)

  (** DWARF package section identifiers.

      These identify the section types within a .dwp package file's contribution
      index.

      DWARF 5 specification, Table 7.1 "DWARF package file section identifier
      encodings". *)
  type dw_sect =
    | DW_SECT_INFO  (** .debug_info contribution *)
    | DW_SECT_ABBREV  (** .debug_abbrev contribution *)
    | DW_SECT_LINE  (** .debug_line contribution *)
    | DW_SECT_LOCLISTS  (** .debug_loclists contribution *)
    | DW_SECT_STR_OFFSETS  (** .debug_str_offsets contribution *)
    | DW_SECT_MACRO  (** .debug_macro contribution *)
    | DW_SECT_RNGLISTS  (** .debug_rnglists contribution *)

  val int_of_dw_sect : dw_sect -> int
  (** Convert a [dw_sect] to its integer encoding. *)

  type index_entry = {
    dwo_id : u64;  (** DWO identifier for this unit *)
    contributions : (dw_sect * int * int) list;
        (** Section contributions as (section, offset, size) *)
  }
  (** An entry from the .dwp index mapping a DWO id to its section contributions
      within the package.

      DWARF 5 specification, section 7.3.5 "DWARF Package Files". *)

  type unit_index = {
    version : int;  (** Index table version *)
    unit_count : int;  (** Number of units in the index *)
    entries : index_entry array;  (** Index entries for each unit *)
  }
  (** A parsed .dwp unit index (.debug_cu_index or .debug_tu_index).

      DWARF 5 specification, section 7.3.5 "DWARF Package Files". *)

  type dwp_context = {
    dwp_buffer : Object.Buffer.t;  (** Buffer containing the .dwp file *)
    parent_buffer : Object.Buffer.t;
        (** Buffer containing the main executable *)
    cu_index : unit_index;  (** Compilation unit index from .debug_cu_index *)
    tu_index : unit_index option;
        (** Type unit index from .debug_tu_index, if present *)
  }
  (** Context for accessing a .dwp (DWARF package) file. *)

  val load_dwp :
    parent_buffer:Object.Buffer.t -> dwp_path:string -> dwp_context option
  (** Load a .dwp package file and parse its unit indices. Returns [None] if the
      file cannot be loaded or the .debug_cu_index section is missing. *)

  val find_cu_by_dwo_id : dwp_context -> u64 -> index_entry option
  (** Look up a compilation unit entry in the .dwp index by DWO id. Returns
      [None] if no matching entry is found. *)

  val dwp_dwo_context : dwp_context -> index_entry -> dwo_context
  (** Create a [dwo_context] from a .dwp package entry, populating the
      contributions list from the index entry. *)
end

(** DWARF Expression Evaluator.

    This module provides an architecture-independent stack-based evaluator for
    DWARF expressions (DW_OP_* operations) used in variable locations and other
    DWARF attributes.

    The evaluator uses a callback-based design where external data (registers,
    memory, frame base, CFA) is requested via result types rather than passed
    directly. This keeps the evaluator architecture-independent - it works only
    with abstract register numbers.

    For architecture-specific register name mappings, see the Dwarf_arch module.

    DWARF 5 specification, section 2.5 "DWARF Expressions" and section 7.7
    "DWARF Expressions and Location Descriptions". *)
module Expression : sig
  (** Architecture-independent register - just a number. Architecture modules
      map these to names. *)
  type register = Dwarf_arch.register = Register of int

  (** Values on the DWARF expression stack *)
  type value =
    | Generic of int64  (** Generic integer value *)
    | Address of int64  (** Address value (for DW_OP_stack_value) *)
    | ImplicitValue of string  (** Implicit value (raw bytes) *)

  type piece = {
    size_in_bits : int option;  (** Size of this piece in bits, if known *)
    bit_offset : int option;  (** Bit offset of this piece within the value *)
    location : value option;  (** Where this piece lives, if it has a location *)
  }
  (** Location pieces for composite locations.

      DWARF 5 specification, section 2.6.1.2 "Composite Location Descriptions".
  *)

  (** Results from evaluation - used for callback pattern *)
  type evaluation_result =
    | Complete  (** Evaluation finished successfully *)
    | RequiresRegister of { register : register; offset : int64 option }
        (** Need caller to provide register value. If offset is Some, add it to
            the register value. *)
    | RequiresMemory of { address : int64; size : int }
        (** Need caller to provide memory at address *)
    | RequiresFrameBase of { offset : int64 }
        (** Need caller to provide frame base address. The offset will be added
            to the frame base. *)
    | RequiresCFA  (** Need caller to provide canonical frame address *)
    | RequiresTLSAddress of { address : int64 }
        (** Need caller to provide TLS address for the given offset *)
    | RequiresObjectAddress  (** Need caller to provide the object address *)
    | RequiresIndexedAddress of { index : int; is_constant : bool }
        (** Need caller to provide an address from the address table.
            is_constant is true for DW_OP_constx, false for DW_OP_addrx *)
    | RequiresSubExpression of { offset : int64 }
        (** Need caller to evaluate a sub-expression at the given offset *)
    | RequiresEntryValue of { bytecode : string }
        (** Need caller to evaluate the entry value expression *)

  type evaluation_state
  (** Evaluation state *)

  val start_evaluation :
    bytecode:string -> encoding:encoding -> evaluation_state
  (** Start evaluating a DWARF expression.

      @param bytecode The DWARF expression bytecode
      @param encoding The compilation unit encoding (for address_size)
      @return Initial evaluation state *)

  val evaluate : evaluation_state -> evaluation_result
  (** Evaluate the expression until completion or until external data is needed.

      @param state Current evaluation state
      @return Evaluation result indicating completion or required data

      @raise Parse_error
        if the expression contains an invalid or unsupported operation. *)

  val resume_with_register : evaluation_state -> value -> evaluation_result
  (** Resume evaluation after providing a register value.

      @param state Current evaluation state
      @param value The register value (Generic or Address)
      @return Next evaluation result

      @raise Parse_error if the evaluator is not waiting for a register value.
  *)

  val resume_with_memory : evaluation_state -> string -> evaluation_result
  (** Resume evaluation after providing memory contents.

      @param state Current evaluation state
      @param bytes The memory contents (as string)
      @return Next evaluation result

      @raise Parse_error
        if the evaluator is not waiting for memory, or the read size is
        unsupported. *)

  val resume_with_frame_base : evaluation_state -> int64 -> evaluation_result
  (** Resume evaluation after providing frame base address.

      @param state Current evaluation state
      @param address The frame base address
      @return Next evaluation result

      @raise Parse_error if the evaluator is not waiting for a frame base. *)

  val resume_with_cfa : evaluation_state -> int64 -> evaluation_result
  (** Resume evaluation after providing canonical frame address.

      @param state Current evaluation state
      @param address The CFA
      @return Next evaluation result

      @raise Parse_error if the evaluator is not waiting for a CFA. *)

  val resume_with_tls_address : evaluation_state -> int64 -> evaluation_result
  (** Resume evaluation after providing a TLS address.

      @param state Current evaluation state
      @param address The resolved TLS address
      @return Next evaluation result

      @raise Parse_error if the evaluator is not waiting for a TLS address. *)

  val resume_with_object_address :
    evaluation_state -> int64 -> evaluation_result
  (** Resume evaluation after providing the object address.

      @param state Current evaluation state
      @param address The object address
      @return Next evaluation result

      @raise Parse_error if the evaluator is not waiting for an object address.
  *)

  val resume_with_indexed_address :
    evaluation_state -> int64 -> evaluation_result
  (** Resume evaluation after providing an indexed address. Pushes as Address
      for DW_OP_addrx, Generic for DW_OP_constx.

      @param state Current evaluation state
      @param value The resolved address or constant value
      @return Next evaluation result

      @raise Parse_error if the evaluator is not waiting for an indexed address.
  *)

  val resume_with_sub_expression :
    evaluation_state -> value list -> evaluation_result
  (** Resume evaluation after evaluating a sub-expression.

      @param state Current evaluation state
      @param values The sub-expression result stack
      @return Next evaluation result

      @raise Parse_error if the evaluator is not waiting for a sub-expression.
  *)

  val resume_with_entry_value : evaluation_state -> value -> evaluation_result
  (** Resume evaluation after computing an entry value.

      @param state Current evaluation state
      @param value The entry value result
      @return Next evaluation result

      @raise Parse_error if the evaluator is not waiting for an entry value. *)

  val result : evaluation_state -> value list
  (** Get the final result stack after Complete evaluation.

      @param state Completed evaluation state
      @return Values on the stack (top = head) *)

  val pieces : evaluation_state -> piece list
  (** Get location pieces for composite locations.

      @param state Completed evaluation state
      @return List of location pieces *)
end
