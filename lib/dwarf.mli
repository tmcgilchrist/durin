(* DWARF 5 *)
open Types

(** DWARF 5 unit type encodings.

    Each compilation unit header begins with a unit type that identifies the
    purpose and structure of the unit. The unit type determines which sections
    the unit uses and how the unit's header is formatted.

    Table 7.5.1: Unit type encodings *)
type unit_type =
  | DW_UT_compile (* 0x01 - A normal compilation unit *)
  | DW_UT_type (* 0x02 - A type unit *)
  | DW_UT_partial (* 0x03 - A partial unit *)
  | DW_UT_skeleton (* 0x04 - A skeleton compilation unit *)
  | DW_UT_split_compile (* 0x05 - A split compilation unit *)
  | DW_UT_split_type (* 0x06 - A split type unit *)
  | DW_UT_lo_user (* 0x80 - Reserved for user-defined types *)
  | DW_UT_hi_user (* 0xff - Reserved for user-defined types *)

val unit_type_of_u8 : Unsigned.UInt8.t -> unit_type
(** Convert a u8 value to a unit_type *)

val string_of_unit_type : unit_type -> string
(** Convert a unit_type to its string representation *)

val string_of_attribute_code : u64 -> string
(** Convert a numeric DWARF attribute code to its string representation *)

val string_of_attribute_form_encoding : u64 -> string
(** Convert a numeric DWARF form code to its string representation *)

(** Abbreviation tag encoding.

    The abbreviations table for a single compilation unit consists of a series
    of abbreviation declarations. Each declaration specifies the tag and
    attributes for a particular form of debugging information entry.

    Table 7.3: Tag encodings *)
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

val uint64_of_abbreviation_tag : abbreviation_tag -> u64
(** Convert a DWARF [abbreviation_tag] tag to a string *)

val string_of_abbreviation_tag : u64 -> string
(** Convert a numeric DWARF [abbreviation_tag] code to a string representation
*)

(** The encodings for the child determination byte. Table 7.4: Child
    determination encodings *)
type children_determination = DW_CHILDREN_no | DW_CHILDREN_yes

(** The encodings for the attribute names.

    Table 7.5: Attribute encodings *)

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

val string_of_attribute_encoding : attribute_encoding -> string
(** Convert an attribute_encoding type to its string representation *)

(** Attribute form encoding. Table 7.6: Attribute form encodings *)
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

(** DWARF expression operations

    Table 7.9: DWARF operation encodings *)
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

(** Location List entry. Table 7.10: Location list entry encoding values New in
    DWARF Version 5 *)
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

(* Base type encoding.
   The encodings of the constants used in the [DW_AT_encoding] attribute.

   Table 7.11: Base type encoding values *)
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
  | DW_ATE_UTF  (** New in DWARF Version 5 *)
  | DW_ATE_UCS
  | DW_ATE_ASCII
  | DW_ATE_lo_user
  | DW_ATE_hi_user

(* Decimal sign encoding.
   The encodings of the constants used in the [DW_AT_decimal_sign] attribute.
   Table 7.12: Decimal sign encodings. *)
type decimal_sign =
  | DW_DS_unsigned
  | DW_DS_leading_overpunch
  | DW_DS_trailing_overpunch
  | DW_DS_leading_separate
  | DW_DS_trailing_separate

(** Endianity encoding. The encodings of the constants used in the
    [DW_AT_endianity] attribute. Table 7.13: Endianity encodings *)
type endianity =
  | DW_END_default
  | DW_END_big
  | DW_END_little
  | DW_END_lo_user
  | DW_END_hi_user

(** Accessibility encoding. The encodings of the constants used in the
    [DW_AT_accessibility] attribute. Table 7.14: Accessibility encodings *)
type accessibility =
  | DW_ACCESS_public
  | DW_ACCESS_protected
  | DW_ACCESS_private

(** Visibility encoding. The encodings of the constants used in the
    DW_AT_visibility attribute.

    Table 7.15: Visibility encodings *)
type visibility = DW_VIS_local | DW_VIS_exported | DW_VIS_qualified

(** Virtuality encoding. The encodings of the constants used in the
    [DW_AT_virtuality] attribute Table 7.16: Virtuality encodings *)
type virtuality =
  | DW_VIRTUALITY_none
  | DW_VIRTUALITY_virtual
  | DW_VIRTUALITY_pure_virtual

(** Language encoding. The encodings of the constants used in the
    [DW_AT_language] attribute Table 7.17: Language encodings *)
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

val string_of_dwarf_language : dwarf_language -> string
(** Convert a dwarf_language to its string representation *)

(** Identifier case. The encodings of the constants used in the
    [DW_AT_identifier_case] attribute. Table 7.18: Identifier case encodings *)
type identifier =
  | DW_ID_case_sensitive
  | DW_ID_up_case
  | DW_ID_down_case
  | DW_ID_case_insensitive

(** Calling convention. The encodings of the constants used in the
    [DW_AT_calling_convention] attribute. Table 7.19: Calling convention
    encodings *)
type calling_convention =
  | DW_CC_normal
  | DW_CC_program
  | DW_CC_nocall  (** New in DWARF Version 5 *)
  | DW_CC_pass_by_reference
  | DW_CC_pass_by_value
  | DW_CC_lo_user
  | DW_CC_hi_user

(** Inlined encoding. The encodings of the constants used in the DW_AT_inline
    attribute. Table 7.20: Inline encodings *)
type inlined =
  | DW_INL_not_inlined
  | DW_INL_inlined
  | DW_INL_declared_not_inlined
  | DW_INL_declared_inlined

(** Array ordering. The encodings of the constants used in the DW_AT_ordering
    attribute. Table 7.21: Ordering encodings *)
type array_ordering = DW_ORD_row_major | DW_ORD_col_major

(** Discriminant. The descriptors used in the [DW_AT_discr_list] attribute are
    encoded as 1-byte constants. Table 7.22: Discriminant descriptor encodings
*)
type discriminant = DW_DSC_label | DW_DSC_range

(** Name index attribute Table 7.23: Name index attribute encodings

    New in DWARF Version 5 *)
type name_index_attribute =
  | DW_IDX_compile_unit
  | DW_IDX_type_unit
  | DW_IDX_die_offset
  | DW_IDX_parent
  | DW_IDX_type_hash
  | DW_IDX_lo_user
  | DW_IDX_hi_user

(** Defaulted attribute. The encodings of the constants used in the
    [DW_AT_defaulted] attribute. Table 7.24: Defaulted attribute encodings

    New in DWARF Version 5 *)
type defaulted_attribute =
  | DW_DEFAULTED_no
  | DW_DEFAULTED_in_class
  | DW_DEFAULTED_out_of_class

(** Line number opcodes. The encodings for the standard opcodes. Table 7.25:
    Line number standard opcode encodings *)
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

(** Line number extended opcodes. The encodings for the extended opcodes. Table
    7.26: Line number extended opcode encodings *)
type line_number_extended_opcode =
  | DW_LNE_end_sequence
  | DW_LNE_set_address
  | DW_LNE_set_discriminator
  | DW_LNE_lo_user
  | DW_LNE_hi_user

(** Line number header entry. The encodings for the line number header entry
    formats. Table 7.27: Line number header entry format encodings. New in DWARF
    Version 5. *)
type line_number_header_entry =
  | DW_LNCT_path
  | DW_LNCT_directory_index
  | DW_LNCT_timestamp
  | DW_LNCT_size
  | DW_LNCT_MD5
  | DW_LNCT_lo_user
  | DW_LNCT_hi_user

(** The macro information entry type is encoded as a single unsigned byte. Table
    7.28: Macro information entry type encodings. New in DWARF Version 5. *)
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

(** Sections that hold DWARF 5 debugging information. *)
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

(** Call frame instructions. Table 7.29: Call frame instruction encodings *)
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
      (** Range list entry Each entry in a range list is either a range list
          entry, a base address selection entry, or an end-of-list entry.

          Each entry begins with an unsigned 1-byte code that indicates the kind
          of entry that follows. The encodings for these constants are given in
          Table 7.30: Range list entry encoding values *)

type t

type object_format =
  | MachO
  | ELF
      (** Object file formats supported.

          Primarily used to derive the correct section name for different
          formats. *)

val string_of_object_format : object_format -> string
(** Human readable string for [object_format] type *)

(* TODO This could be stored alongside the parsed representation or
   to provide a lazy way to parse out the values on demand. *)
type span = { start : size_t; size : size_t }

(* TODO Improve this to use variant types? *)
type attr_spec = { attr : u64; form : u64 }

(* TODO Improve this to use variant types? *)
type abbrev = {
  code : u64;
  tag : u64;
  has_children : bool;
  attr_specs : attr_spec list;
}

val object_format_to_section_name : object_format -> dwarf_section -> string
(** Returns the dwarf_section name specific for the object_format. *)

val detect_format : Object.Buffer.t -> object_format
(** Detect file format from buffer using magic numbers *)

val detect_format_and_arch : Object.Buffer.t -> string
(** Detect file format and architecture from buffer, returning a string like
    "Mach-O arm64" *)

val resolve_string_index : Object.Buffer.t -> int -> string
(** Resolve a string index to its actual string value using debug_str sections
*)

val resolve_address_index : Object.Buffer.t -> int -> u64 -> u64
(** Resolve an address index to its actual address value using debug_addr
    section. Parameters: buffer, address_index, addr_base_offset Returns the
    resolved address or the index value if resolution fails *)

val lookup_address_in_debug_addr : Object.Buffer.t -> u64 -> int -> u64 option
(** Look up an address by index in the debug_addr section at given offset.
    Returns Some address if found, None if not found or section missing *)

(** Object module as a wrapper around a buffer for an object file. *)
module Object_file : sig
  type t
end

(** Debugging Information Entry (DIE) represent low-level information about a
    source program.

    The debugging information entries are contained in the [debug_info] and/or
    .debug_info.dwo sections of an object file. *)
module DIE : sig
  (** Attribute values that can appear in DWARF Debug Information Entries *)
  type attribute_value =
    | String of string  (** String value from DW_FORM_string or DW_FORM_strp *)
    | UData of u64  (** Unsigned integer from DW_FORM_udata, DW_FORM_data* *)
    | SData of i64  (** Signed integer from DW_FORM_sdata *)
    | Address of u64  (** Address from DW_FORM_addr *)
    | Flag of bool  (** Boolean from DW_FORM_flag or DW_FORM_flag_present *)
    | Reference of u64  (** Reference from DW_FORM_ref* *)
    | Block of string  (** Block of data from DW_FORM_block* *)
    | Language of dwarf_language  (** Language from DW_AT_language attribute *)

  type attribute = { attr : attribute_encoding; value : attribute_value }
  (** A DWARF attribute consisting of name and value *)

  type t = {
    tag : abbreviation_tag;
    attributes : attribute list;
    children : t list;
  }
  (** A Debug Information Entry containing tag, attributes, and children *)

  val parse_die : Object.Buffer.cursor -> (u64, abbrev) Hashtbl.t -> Object.Buffer.t -> t option
  (** Parse a single DIE from a buffer using abbreviation table *)

  val find_attribute : t -> attribute_encoding -> attribute_value option
  (** Find an attribute by name in a DIE *)
end

(** Compilation units represent individual source files and their debugging
    information in DWARF 5.

    A compilation unit is a fundamental organizational structure in DWARF that
    contains all debugging information associated with the compilation of a
    single source file. Each compilation unit includes:

    - Source file information (original file that was compiled)
    - Debug Information Entries (DIEs) describing types, variables, functions,
      etc.
    - Compilation metadata (compiler version, language, compilation directory)
    - Address ranges where the compiled code resides
    - Line number mappings between source and machine code

    The DWARF 5 compilation unit structure consists of:
    - Unit Length (4 bytes) - Size of the compilation unit
    - Version (2 bytes) - DWARF version (5 for DWARF 5)
    - Unit Type (1 byte) - DW_UT_compile for normal compilation units
    - Address Size (1 byte) - Size of addresses (typically 8 for 64-bit)
    - Debug Abbrev Offset (4 bytes) - Offset into [Debug_abbrev] section
    - DIE Data (variable length) - Tree of debugging information entries *)
module CompileUnit : sig
  type header = {
    unit_length : u32;
    version : u16;  (** DWARF version (must be 5) *)
    unit_type : u8;
    debug_abbrev_offset : u32;  (** Offset into debug abbreviation table *)
    address_size : u8;  (** Size of addresses in bytes (must be 8) *)
  }
  (** Parsed header data from a compilation unit. This contains the essential
      metadata needed to interpret the unit's content. *)

  type t
  (** A compilation unit with parsed content. *)

  val make : int -> span -> Object.Buffer.t -> header -> t
  (** Create a new compilation unit. *)

  val dwarf_info : t -> int
  (** Get the parent DWARF info identifier. *)

  val data : t -> span
  (** Get the span indicating location/size of unit data. *)

  val header : t -> header
  (** Force parsing of the compilation unit header and return parsed data. *)

  val root_die : t -> (u64, abbrev) Hashtbl.t -> Object.Buffer.t -> DIE.t option
  (** Get the root DIE for this compilation unit. *)

  (* val abbrev_table : t -> unit *)
  (* (\** Get abbreviation table for this compilation unit. TODO: Implementation *)
  (*     needs to return actual abbreviation table. *\) *)
end

(** Line number information in DWARF 5 section 6.2. *)
module LineTable : sig
  type t = { cu : CompileUnit.t }

  module File : sig
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
    file_names : (string * u64 * u64 * string) array;
  }
  (** Line number program header as specified in DWARF 5 section 6.2.4

      The line number program header provides information used by consumers in
      decoding the line number program instructions for a particular compilation
      unit and also provides information used throughout the rest of the line
      number program. *)

  val parse_line_program_header : Object.Buffer.cursor -> line_program_header
  (** Parse the line number program header from the [Debug_line] section. *)

  (* TODO This module should provide an iterator to
     get a line table entry based on an address. *)
  (* TODO Provide access to lookup entries by line. *)
end

(** Call frame information parsing for [Debug_frame] section *)
module CallFrame : sig
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
  (** Common Information Entry (CIE) from DWARF 5 section 6.4.1.

      A CIE contains information that is shared among many Frame Description
      Entries. There is at least one CIE in every non-empty [Debug_frame]
      section. A CIE entry describes information that is common to FDE entries
      associated with that CIE.

      The CIE format in DWARF 5 is:
      - Length (4 bytes) - Length of the CIE (not including this field)
      - CIE_id (4 bytes) - Distinguished value 0xffffffff for CIEs
      - Version (1 byte) - DWARF version (5 for DWARF 5)
      - Augmentation (string) - Null-terminated string
      - Address Size (1 byte) - Size of target address in bytes
      - Segment Selector Size (1 byte) - Size of segment selector in bytes
      - Code Alignment Factor (ULEB128) - Alignment of code addresses
      - Data Alignment Factor (SLEB128) - Alignment for data
      - Return Address Register (ULEB128) - Register containing return address
      - Augmentation Length (ULEB128) - Length of augmentation data (if present)
      - Augmentation Data (bytes) - Vendor-specific data (if present)
      - Initial Instructions (bytes) - Call frame instructions *)

  type frame_description_entry = {
    length : u32;
    cie_pointer : u32;
    initial_location : u64;
    address_range : u64;
    augmentation_length : u64 option;
    augmentation_data : string option;
    instructions : string;
  }
  (** Frame Description Entry (FDE) from DWARF 5 section 6.4.1.

      An FDE contains call frame information for a specific range of program
      addresses. Each FDE references a CIE that contains common information.
      Together, the CIE and FDE provide the information necessary to unwind the
      call frame for any given program counter value within the address range
      covered by the FDE.

      The FDE format in DWARF 5 is:
      - Length (4 bytes) - Length of the FDE (not including this field)
      - CIE_pointer (4 bytes) - Offset to associated CIE in [Debug_frame]
        section
      - Initial_location (address_size bytes) - Address of first instruction
        covered
      - Address_range (address_size bytes) - Number of bytes covered by this FDE
      - Augmentation_length (ULEB128) - Length of augmentation data (if present)
      - Augmentation_data (bytes) - Vendor-specific data (if present)
      - Instructions (bytes) - Call frame instructions specific to this address
        range *)

  val parse_common_information_entry :
    Object.Buffer.cursor -> common_information_entry
  (** Parse a Common Information Entry from the [Debug_frame] section.

      A CIE contains information shared among many Frame Description Entries.
      The cursor should be positioned at the start of a CIE entry in the
      [Debug_frame] section. *)
end

(** Debug names section parsing for [Debug_names] section *)
module DebugNames : sig
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
  (** Name index header as specified in DWARF 5 section 6.1.1 *)

  type name_index_entry = {
    name_offset : u32;
    die_offset : u32;
    attributes : (name_index_attribute * u64) list;
  }
  (** Name index entry *)

  type debug_names_section = {
    header : name_index_header;
    comp_unit_offsets : u32 array;
    local_type_unit_offsets : u32 array;
    foreign_type_unit_signatures : u64 array;
    hash_table : u32 array;
    name_table : string array;
    entry_pool : name_index_entry array;
  }
  (** Complete debug names section *)

  val parse_debug_names_section : Object.Buffer.cursor -> debug_names_section
  (** Parse a complete debug_names section from the [Debug_names] section.

      The debug_names section contains name lookup tables for compilation units,
      type units, function names, variable names, and type names. This allows
      efficient name-based lookups in DWARF debugging information.

      The cursor should be positioned at the start of a [Debug_names] section.
  *)
end

val object_format_to_section_name : object_format -> dwarf_section -> string
(** Textual representation of section name in [object_format] *)

val create : Object.Buffer.t -> t
(** Create a new DWARF context from an object file *)

val parse_compile_units : t -> CompileUnit.t Seq.t
(** Parse all compile units from the [Debug_info] section lazily *)

val get_abbrev_table : t -> size_t -> t * (u64, abbrev) Hashtbl.t
(** Retrieve the abbreviation table at offset [size_t]. *)

module DebugStrOffsets : sig
  type header = { unit_length : u32; version : u16; padding : u16 }
  type offset_entry = { offset : u32; resolved_string : string option }
  type t = { header : header; offsets : offset_entry array }

  val parse_header : Object.Buffer.cursor -> header
  (** Parse the header of a debug_str_offsets section *)

  val parse_offsets :
    Object.Buffer.cursor ->
    header ->
    (u32 * u64) option ->
    Object.Buffer.t ->
    offset_entry array
  (** Parse the offset array with optional string resolution *)

  val parse : Object.Buffer.t -> u32 -> t
  (** Parse a complete debug_str_offsets section from buffer at given offset *)
end

module DebugAddr : sig
  type header = {
    unit_length : u32;
    version : u16;
    address_size : u8;
    segment_selector_size : u8;
  }

  type entry = {
    segment : u64 option;  (** Present only if segment_selector_size > 0 *)
    address : u64;
  }

  type t = { header : header; entries : entry array }

  val parse_header : Object.Buffer.cursor -> header
  (** Parse the header of a debug_addr section *)

  val parse_entries : Object.Buffer.cursor -> header -> entry array
  (** Parse the entries following the header *)

  val parse : Object.Buffer.t -> u32 -> t
  (** Parse a complete debug_addr section from buffer at given offset *)
end
