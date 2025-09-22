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

val uint64_of_abbreviation_tag : abbreviation_tag -> u64
(** Convert a DWARF [abbreviation_tag] tag to a string *)

val string_of_abbreviation_tag : u64 -> string
(** Convert a numeric DWARF [abbreviation_tag] code to a string representation
*)

val string_of_abbreviation_tag_direct : abbreviation_tag -> string
(** Convert a DWARF [abbreviation_tag] directly to a string representation *)

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
  | DW_FORM_flag_present
  (* New in DWARF Version 5 *)
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

val string_of_attribute_form_encoding_variant :
  attribute_form_encoding -> string

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

val string_of_operation_encoding : operation_encoding -> string

type dwarf_expression_operation = {
  opcode : operation_encoding;
  operands : int list;
  operand_string : string option;
}
(** DWARF Expression Parser *)

val parse_dwarf_expression : string -> dwarf_expression_operation list
val string_of_dwarf_operation : dwarf_expression_operation -> string
val string_of_dwarf_expression : dwarf_expression_operation list -> string

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

   A base type is a data type that is not defined in terms of other data
   types. Each programming language has a set of base types that are
   considered to be built into that language.

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

val string_of_base_type : base_type -> string
(** Convert a [base_type] to its string representation *)

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
  | DW_IDX_null
  | DW_IDX_compile_unit
  | DW_IDX_type_unit
  | DW_IDX_die_offset
  | DW_IDX_parent
  | DW_IDX_type_hash
  | DW_IDX_lo_user
  | DW_IDX_hi_user

val string_of_name_index_attribute : name_index_attribute -> string

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

val line_number_opcode : int -> line_number_opcode
val string_of_line_number_opcode : line_number_opcode -> string

(** Line number extended opcodes. The encodings for the extended opcodes. Table
    7.26: Line number extended opcode encodings *)
type line_number_extended_opcode =
  | DW_LNE_end_sequence
  | DW_LNE_set_address
  | DW_LNE_set_discriminator
  | DW_LNE_lo_user
  | DW_LNE_hi_user

val string_of_line_number_extended_opcode :
  line_number_extended_opcode -> string

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

val string_of_line_number_header_entry : line_number_header_entry -> string

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

val macro_info_entry_type_of_u8 : Types.u8 -> macro_info_entry_type
val string_of_macro_info_entry_type : macro_info_entry_type -> string

(** Debug Macro Section - DWARF 5 Section 6.3 *)

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

val parse_debug_macro_header : Object.Buffer.cursor -> debug_macro_header
(** Parse debug_macro header from binary data *)

val parse_debug_macro_entry : Object.Buffer.cursor -> debug_macro_entry option
(** Parse a single debug_macro entry from binary data *)

val parse_debug_macro_unit : Object.Buffer.cursor -> debug_macro_unit
(** Parse a complete debug_macro unit from binary data *)

val parse_debug_macro_section :
  Object.Buffer.cursor -> int -> debug_macro_section
(** Parse the entire debug_macro section from binary data *)

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
  | Debug_macro
  | Debug_frame

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
    | Encoding of base_type
        (** Data type encoding from DW_AT_encoding attribute *)

  type attribute = { attr : attribute_encoding; value : attribute_value }
  (** A DWARF attribute consisting of name and value *)

  type t = {
    tag : abbreviation_tag;
    attributes : attribute list;
    children : t Seq.t;
    offset : int;
  }
  (** A Debug Information Entry containing tag, attributes, and children *)

  val parse_die :
    Object.Buffer.cursor ->
    (u64, abbrev) Hashtbl.t ->
    Object.Buffer.t ->
    t option
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
    header_span : span;  (** Span indicating the header's position and size *)
  }
  (** Parsed header data from a compilation unit. This contains the essential
      metadata needed to interpret the unit's content. *)

  type t
  (** A compilation unit with parsed content. *)

  val make : int -> span -> Object_file.t -> header -> t
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

val parse_compile_unit_header :
  Object.Buffer.cursor -> span * CompileUnit.header
(** Parse the header of a compilation unit from a buffer cursor. Returns both
    the unit span and the parsed header with header_span included. *)

(** Line number information parsing for DWARF 5 section 6.2.

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

    This module currently implements parsing of the line program header only.
    The header contains essential information needed to decode the line program,
    including file names, directory names, and opcode definitions.

    Reference: DWARF 5 specification, section 6.2 "Line Number Information" *)
module LineTable : sig
  type t = { cu : CompileUnit.t }

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

      Reference: DWARF 5 specification, section 6.2.4.1 "The Line Number Program
      Header" *)

  type line_program_header = {
    unit_length : u32;
        (** Total length of line table entry excluding this field *)
    version : u16;  (** DWARF version number (5 for DWARF 5) *)
    address_size : u8;
        (** Size of target address in bytes (typically 4 or 8) *)
    segment_selector_size : u8;
        (** Size of segment selector in bytes (usually 0) *)
    header_length : u32;
        (** Length of line program header excluding unit_length and version *)
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
  (** Line number program header as specified in DWARF 5 section 6.2.4.

      The line number program header contains metadata required to interpret the
      line number program that follows. It defines the opcodes, their operand
      counts, the file and directory tables, and parameters for the line number
      state machine.

      Key components:
      - Basic parameters: address size, instruction lengths, version info
      - Opcode definitions: which opcodes exist and how many operands they take
      - Line advance parameters: line_base and line_range define special opcode
        behavior
      - File/directory tables: map indices used in line program to actual paths
      - Format descriptors: define flexible encoding of directories and files
        (DWARF 5)

      The header is followed by the line number program itself, which consists
      of opcodes that build a matrix mapping addresses to source locations.

      Reference: DWARF 5 specification, section 6.2.4 "The Line Number Program
      Header" *)

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

      Reference: DWARF 5 specification, section 6.2.2 "State Machine Registers"
  *)

  val parse_line_program_header :
    Object.Buffer.cursor -> Object.Buffer.t -> line_program_header
  (** Parse the line number program header from the [Debug_line] section.

      This function parses a complete DWARF 5 line program header from the
      debug_line section. It handles the flexible format introduced in DWARF 5
      where directory and file entries can contain different combinations of
      content based on format descriptors.

      The parser:
      - Reads all fixed-size header fields (lengths, versions, parameters)
      - Parses standard opcode length definitions
      - Uses directory_entry_formats to parse directory entries with support for
        DW_FORM_line_strp references
      - Uses file_name_entry_formats to parse file entries with support for all
        DW_LNCT content types
      - Resolves directory indices in file entries to actual directory paths
      - Converts MD5 checksums from binary to hex string representation

      @param cursor Buffer cursor positioned at start of line program header
      @param buffer
        Complete buffer containing debug sections (needed for line_strp
        resolution)
      @return Parsed line program header with all tables resolved

      @raise Failure
        if header format is invalid or unsupported content types are encountered

      Reference: DWARF 5 specification, section 6.2.4 "The Line Number Program
      Header" *)

  val parse_line_program :
    Object.Buffer.cursor -> line_program_header -> line_table_entry list
  (** Parse the line number program following the header.

      This function executes the line number program (sequence of opcodes) that
      follows the line program header. It runs the DWARF 5 line number state
      machine to produce the complete line table matrix.

      The state machine processes three types of opcodes:
      - Special opcodes (values opcode_base to 255): Efficiently encode common
        address/line advance combinations
      - Standard opcodes (values 1 to opcode_base-1): Individual operations like
        DW_LNS_copy, DW_LNS_advance_pc, DW_LNS_advance_line
      - Extended opcodes (value 0 followed by length and extended opcode):
        Operations like DW_LNE_end_sequence, DW_LNE_set_address,
        DW_LNE_set_discriminator

      @param cursor Buffer cursor positioned after the line program header
      @param header Parsed line program header containing opcode definitions
      @return List of line table entries representing the complete line table

      @raise Failure
        if program contains invalid opcodes or malformed instructions

      Reference: DWARF 5 specification, section 6.2 "Line Number Information" *)
  (* TODO Combine the header and line_program parsing into a single function
     That parses the header and lazily parses the line_table_entry Seq.t
  *)
  (* TODO This module should provide an iterator to
     get a line table entry based on an address. *)
  (* TODO Provide access to lookup entries by line. *)
end

(** Call frame information parsing for DWARF 5 section 6.4.

    The debug_frame section contains call frame information that describes how
    to unwind the call stack during program execution. This is essential for
    debuggers to:
    - Display accurate stack traces during debugging
    - Implement proper exception handling unwinding
    - Support profiling tools that need to walk the call stack
    - Enable correct stack frame reconstruction for crash analysis

    Call frame information solves the fundamental problem that modern optimizing
    compilers can make it impossible to determine the call stack structure at
    runtime. Registers may be reused, stack frames may be optimized away, and
    the frame pointer may be eliminated for performance.

    DWARF call frame information provides a complete description of how to
    reconstruct the call stack at any program counter by describing:
    - Which registers have been saved and where
    - How to compute the Canonical Frame Address (CFA)
    - Where the return address is stored
    - How register values change throughout function execution

    Structure of debug_frame section: 1. Common Information Entries (CIEs):
    Shared information for multiple functions 2. Frame Description Entries
    (FDEs): Function-specific unwinding information 3. Call Frame Instructions:
    Opcodes that describe register save/restore operations

    The format uses a compact instruction encoding to efficiently represent
    register state changes throughout function execution. Instructions describe
    operations like:
    - DW_CFA_offset: Register saved at CFA + offset
    - DW_CFA_register: Register saved in another register
    - DW_CFA_def_cfa: Define how to compute CFA
    - DW_CFA_advance_loc: Advance program counter

    Each FDE references a CIE for common information, then provides specific
    instructions for unwinding that function's call frame. The combination
    allows precise stack reconstruction at any program counter within the
    function's address range.

    This implementation follows the DWARF 5 standard which unified the
    previously separate .debug_frame and .eh_frame formats, providing better
    interoperability between different tools and compilers.

    Reference: DWARF 5 specification, section 6.4 "Call Frame Information" *)
module CallFrame : sig
  val debug_frame_cie_id : u32
  (** Distinguished CIE identifier (0xffffffff) for .debug_frame sections *)

  type common_information_entry = {
    length : u32;  (** Length of CIE excluding this length field *)
    cie_id : u32;  (** Distinguished CIE identifier (0xffffffff) *)
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
  }
  (** Common Information Entry (CIE) from DWARF 5 section 6.4.1.

      A CIE contains information that is shared among many Frame Description
      Entries (FDEs). There is at least one CIE in every non-empty debug_frame
      section. CIEs define common parameters and initial call frame state that
      multiple functions can share, reducing redundancy.

      Key fields and their purposes:
      - length: Total size of CIE entry (excluding the length field itself)
      - cie_id: Always 0xffffffff to distinguish CIEs from FDEs
      - version: DWARF format version for compatibility checking
      - augmentation: Extension mechanism (empty string for standard format)
      - address_size: Architecture word size (32-bit = 4, 64-bit = 8)
      - segment_selector_size: Usually 0 except for segmented architectures
      - code_alignment_factor: Used to encode program counter advances
        efficiently
      - data_alignment_factor: Used to encode stack offset calculations (often
        negative)
      - return_address_register: Architecture-specific return address register
      - augmentation_length/data: Optional vendor-specific extensions
      - initial_instructions: Call frame opcodes defining default register rules

      The alignment factors are critical for compact encoding:
      - code_alignment_factor: Often set to minimum instruction size (1, 2, 4)
      - data_alignment_factor: Often set to stack slot size (e.g., -8 for
        64-bit)

      The initial_instructions establish the baseline call frame state that FDEs
      can modify with their own instruction sequences.

      Reference: DWARF 5 specification, section 6.4.1 "Structure of Call Frame
      Information" *)

  val create_default_cie : unit -> common_information_entry
  (** Create a CIE with sensible default values for x86-64 architecture *)

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

  (** Debug Frame section entry type *)
  type debug_frame_entry =
    | CIE of common_information_entry  (** Common Information Entry *)
    | FDE of frame_description_entry  (** Frame Description Entry *)
    | Zero_terminator of int  (** Zero terminator at given position *)

  type debug_frame_section = {
    entries : debug_frame_entry list;  (** All entries in the section *)
    entry_count : int;  (** Number of CIE/FDE entries *)
  }
  (** Debug Frame section *)

  val parse_debug_frame_section :
    Object.Buffer.cursor -> int -> debug_frame_section
  (** Parse complete debug_frame section from cursor.

      @param cursor Buffer cursor positioned at the start of debug_frame section
      @param section_size Size of the debug_frame section in bytes
      @return Parsed debug_frame section containing all CIE and FDE entries *)
end

(** EH Frame Header parsing for .eh_frame_hdr section.

    The .eh_frame_hdr section provides a sorted table for fast lookup of Frame
    Description Entries in the .eh_frame section. This is used by the runtime
    exception handling mechanism for efficient stack unwinding.

    Reference: LSB specification and ELF exception handling ABI *)
module EHFrameHdr : sig
  type encoding =
    | DW_EH_PE_absptr
    | DW_EH_PE_omit
    | DW_EH_PE_uleb128
    | DW_EH_PE_udata2
    | DW_EH_PE_udata4
    | DW_EH_PE_udata8
    | DW_EH_PE_sleb128
    | DW_EH_PE_sdata2
    | DW_EH_PE_sdata4
    | DW_EH_PE_sdata8
    | DW_EH_PE_pcrel
    | DW_EH_PE_datarel
    | DW_EH_PE_funcrel
    | DW_EH_PE_aligned
    | DW_EH_PE_indirect

  type search_table_entry = { initial_location : u64; fde_address : u64 }

  type header = {
    version : u8;
    eh_frame_ptr_enc : encoding;
    fde_count_enc : encoding;
    table_enc : encoding;
    eh_frame_ptr : u64;
    fde_count : u32;
    search_table : search_table_entry array;
  }

  val parse_header : Object.Buffer.cursor -> u64 -> header
  (** Parse the .eh_frame_hdr header and search table.

      The second parameter is the base address of the section for relative
      address calculations. *)

  val parse_section : Object.Buffer.cursor -> u64 -> header
  (** Parse complete .eh_frame_hdr section - alias for parse_header. *)

  val encoding_of_u8 : int -> encoding
  (** Convert a byte value to its corresponding encoding type *)
end

(** EH Frame parsing for .eh_frame section.

    The .eh_frame section contains Call Frame Information used by the runtime
    exception handling mechanism. While similar to .debug_frame, it has some
    format differences optimized for runtime performance.

    Reference: LSB specification and ELF exception handling ABI *)
module EHFrame : sig
  type eh_frame_entry =
    | EH_CIE of CallFrame.common_information_entry
    | EH_FDE of CallFrame.frame_description_entry

  type section = { entries : eh_frame_entry list }

  val parse_eh_cie :
    Object.Buffer.cursor -> u32 -> CallFrame.common_information_entry
  (** Parse a Common Information Entry adapted for .eh_frame format.

      The second parameter is the expected length from the length field. *)

  val parse_eh_fde :
    Object.Buffer.cursor -> u32 -> int -> CallFrame.frame_description_entry
  (** Parse a Frame Description Entry adapted for .eh_frame format.

      The second parameter is the expected length from the length field. The
      third parameter is the file offset where this FDE starts. *)

  val parse_section : Object.Buffer.cursor -> int -> section
  (** Parse the .eh_frame section.

      The second parameter is the section size in bytes. *)
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

    DWARF 5 Specification Reference: Section 6.1 (Accelerated Access) *)
module DebugNames : sig
  type name_index_header = {
    unit_length : u32;  (** Total length of name index excluding this field *)
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
  }
  (** Name index header as specified in DWARF 5 section 6.1.1.

      The name index header contains metadata about the structure and contents
      of the debug_names section. It defines the sizes of various tables and
      arrays that follow the header.

      Key fields:
      - Counts (comp_unit_count, etc.): Define sizes of offset arrays
      - bucket_count: Determines hash table size for name lookup performance
      - name_count: Size of name table containing string offsets
      - abbrev_table_size: Size of abbreviation table for entry pool encoding

      The augmentation string allows producers to indicate
      implementation-specific extensions to the format. Standard producers use
      an empty string.

      Hash table performance depends on bucket_count being appropriately sized
      relative to name_count to minimize collisions during name lookup.

      Reference: DWARF 5 specification, section 6.1.1.1 "Name Index Header" *)

  type debug_str_entry = {
    offset : u32;  (** Original offset in debug_str section *)
    value : string;  (** Resolved string value *)
  }
  (** String with original offset preserved for debug_names *)

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

      Reference: DWARF 5 specification, section 6.1.1.4.2 "Name Index Entries"
  *)

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
  (** Complete parsed debug_names section.

      This structure contains all components of a debug_names section after
      parsing, with string offsets resolved to actual strings and all tables
      populated.

      Structure layout (in section order):
      + header: Metadata about table sizes and format
      + comp_unit_offsets: Maps CU indices to .debug_info offsets
      + local_type_unit_offsets: Maps TU indices to local type unit offsets
      + foreign_type_unit_signatures: Maps TU indices to foreign type signatures
      + hash_table: Bucket array for efficient name hashing
      + name_table: Resolved symbol names (originally offsets into .debug_str)
      + entry_pool: All entries with DIE offsets and attributes

      Name lookup algorithm:
      + Hash the target name to get bucket index
      + Use hash_table[bucket] to get name_table index
      + Compare target name with name_table[index]
      + If match, use index to find corresponding entries in entry_pool
      + Use entries to locate DIEs in compilation/type units

      The arrays are sized according to the counts in the header, providing O(1)
      access to compilation units and type units by index.

      Reference: DWARF 5 specification, section 6.1.1 "Name Index Format" *)

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
      lookups. It handles the complex multi-table format and resolves all string
      references.

      The parsing process:
      + Read name index header with table sizes and counts
      + Parse compilation unit offset array
      + Parse local and foreign type unit arrays
      + Read hash table buckets for name lookup acceleration
      + Parse name table (resolve string offsets from .debug_str section)
      + Decode entry pool using abbreviation table format
      + Associate entries with names and hash buckets

      The parser handles:
      - Variable-length encoding of entry pool using abbreviations
      - String offset resolution from .debug_str section
      - Proper sizing of all arrays based on header counts
      - Validation of format version and field alignments

      @param cursor Buffer cursor positioned at start of debug_names section
      @return
        Complete debug_names_section with all tables populated and resolved

      @raise Failure if section format is invalid or unsupported version

      Performance note: The resulting structure enables O(1) name lookups via
      the hash table, significantly faster than linear DIE scanning.

      Reference: DWARF 5 specification, section 6.1.1 "Name Index Format" *)

  val parse_all_entries_for_name :
    Object.Buffer.t ->
    debug_names_section ->
    int ->
    int ->
    (int * int * string * string * int option * bool) list
  (** Parse all entries for a given name index according to DWARF 5
      specification. Returns a list of tuples: (entry_addr, die_offset, tag_str,
      abbrev_id, parent_offset_opt, has_parent_flag) *)

  val calculate_entry_pool_offset : name_index_header -> int
  (** Calculate entry pool offset based on header information *)
end

(** String table parsing for .debug_str section.

    This module provides support for parsing DWARF string tables, which contain
    null-terminated strings referenced by other sections via offsets. The string
    table centralizes string storage and enables efficient string sharing across
    debug information entries. *)
module DebugStr : sig
  type string_entry = {
    offset : int;  (** Offset of string in debug_str section *)
    length : int;  (** Length of string excluding null terminator *)
    content : string;  (** The actual string content *)
  }
  (** Individual string entry with location and content information *)

  type t = {
    entries : string_entry array;  (** Array of all strings in the section *)
    total_size : int;  (** Total size of the debug_str section *)
  }
  (** Complete parsed debug_str section *)

  val parse : Object.Buffer.t -> t option
  (** Parse the complete .debug_str section from buffer.

      @param buffer Object buffer containing the DWARF data
      @return Optional parsed string table, None if section not found
      @raise Failure if section format is invalid *)
end

(** Line string table parsing for .debug_line_str section.

    This module provides support for parsing DWARF line string tables, which
    contain null-terminated strings specifically used by the line number
    program. These strings are typically file names and directory paths
    referenced by line table entries via DW_FORM_line_strp form. *)
module DebugLineStr : sig
  type string_entry = {
    offset : int;  (** Offset of string in debug_line_str section *)
    length : int;  (** Length of string excluding null terminator *)
    content : string;  (** The actual string content *)
  }
  (** Individual string entry with location and content information *)

  type t = {
    entries : string_entry array;  (** Array of all strings in the section *)
    total_size : int;  (** Total size of the debug_line_str section *)
  }
  (** Complete parsed debug_line_str section *)

  val parse : Object.Buffer.t -> t option
  (** Parse the complete .debug_line_str section from buffer.

      @param buffer Object buffer containing the DWARF data
      @return Optional parsed line string table, None if section not found
      @raise Failure if section format is invalid *)
end

val object_format_to_section_name : object_format -> dwarf_section -> string
(** Textual representation of section name in [object_format] *)

val create : Object.Buffer.t -> t
(** Create a new DWARF context from an object file *)

val parse_compile_units : t -> CompileUnit.t Seq.t
(** Parse all compile units from the [Debug_info] section lazily *)

val get_abbrev_table : t -> size_t -> t * (u64, abbrev) Hashtbl.t
(** Retrieve the abbreviation table at offset [size_t]. *)

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

    Section structure:
    - Header with length, version, and alignment padding
    - Array of 32-bit offsets into the .debug_str section
    - Each offset can be resolved to retrieve actual string content

    The offset table is typically referenced via DW_FORM_strx* forms in
    attribute values, where the form value is an index into this table rather
    than a direct offset into .debug_str.

    DWARF 5 Specification Reference: Section 7.26 (String Offsets Table) *)
module DebugStrOffsets : sig
  type header = {
    unit_length : u32;  (** Length of this contribution excluding this field *)
    version : u16;  (** DWARF version number (typically 5) *)
    padding : u16;  (** Reserved padding field, must be zero *)
    header_span : span;  (** Span indicating the header's position and size *)
  }
  (** Header structure for a string offsets contribution.

      Each contribution starts with a header containing the total length, DWARF
      version, and padding for alignment. Multiple contributions can exist in a
      single [.debug_str_offsets] section. *)

  type offset_entry = {
    offset : u32;  (** Offset into .debug_str section *)
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
      @raise Failure if header format is invalid or unsupported version *)

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
      @raise Failure if section format is invalid

      Usage example:
      {[
        let str_offsets = DebugStrOffsets.parse buffer section_offset in
        let first_string = str_offsets.offsets.(0).resolved_string
      ]}

      Performance note: String resolution is performed eagerly during parsing
      for better cache locality and simpler API usage. *)
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

    Section structure:
    - Header with length, version, address size, and segment size
    - Array of addresses (size determined by address_size field)
    - Each address can be resolved using an index and optional addr_base offset

    This indirection is particularly valuable for position-independent code and
    shared libraries where actual addresses are determined at load time rather
    than link time. The address table allows the debug information to remain
    compact while supporting full address resolution.

    DWARF 5 Specification Reference: Section 7.27 (Address Table) *)
module DebugAddr : sig
  type header = {
    unit_length : u32;  (** Length of this contribution excluding this field *)
    version : u16;  (** DWARF version number (typically 5) *)
    address_size : u8;
        (** Size of addresses in bytes (4 for 32-bit, 8 for 64-bit) *)
    segment_selector_size : u8;
        (** Size of segment selectors in bytes (0 if no segments) *)
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
      @raise Failure if header format is invalid or unsupported version *)

  val parse_entries : Object.Buffer.cursor -> header -> entry array
  (** Parse the address entries following the header.

      Reads the array of addresses using the size information from the header.
      Each address is read according to [address_size] and segment selectors are
      included if [segment_selector_size > 0].

      @param cursor Buffer cursor positioned after header
      @param header Previously parsed header for size calculation
      @return Array of address entries
      @raise Failure if entries cannot be parsed or address sizes are invalid *)

  val parse : Object.Buffer.t -> u32 -> t
  (** Parse a complete debug_addr contribution from buffer.

      This is the main entry point for parsing an address table. It combines
      header parsing and address entry parsing to provide a complete address
      lookup table.

      @param buffer Object buffer containing the DWARF data
      @param section_offset Offset to start of .debug_addr section
      @return Complete parsed address table
      @raise Failure if section format is invalid

      Usage example:
      {[
        let addr_table = DebugAddr.parse buffer section_offset in
        let first_address = addr_table.entries.(0).address
      ]}

      Performance note: This function parses the entire contribution eagerly.
      For large address tables, consider lazy parsing if only specific indices
      are needed.

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

    DWARF 5 Specification Reference: Section 6.1.2 (Address Range Table) *)
module DebugAranges : sig
  type header = {
    unit_length : u32;  (** Length of this aranges set excluding this field *)
    version : u16;  (** DWARF version number (typically 2) *)
    debug_info_offset : u32;  (** Offset into .debug_info section *)
    address_size : u8;  (** Size of addresses in bytes *)
    segment_size : u8;  (** Size of segment selectors in bytes (usually 0) *)
    header_span : span;  (** Span indicating the header's position and size *)
  }
  (** Header structure for an address range table.

      Each compilation unit can have its own address range table describing the
      non-contiguous address ranges of code belonging to that unit. *)

  type address_range = {
    start_address : u64;  (** Beginning of address range *)
    length : u64;  (** Length of address range *)
  }
  (** A single address range entry.

      Represents a contiguous range of addresses belonging to a compilation
      unit. The range covers addresses from [start_address] to
      [start_address + length - 1]. *)

  type aranges_set = {
    header : header;  (** Header information *)
    ranges : address_range list;  (** List of address ranges *)
  }
  (** Complete address range table for one compilation unit.

      Contains header information identifying the compilation unit and a list of
      all address ranges belonging to that unit. *)

  val parse : Object.Buffer.t -> aranges_set option
  (** Parse an address range table from buffer.

      Automatically detects and handles both ELF (.debug_aranges) and MachO
      (__debug_aranges) section naming conventions.

      @param buffer Object buffer containing the DWARF data
      @return Optional parsed address range table, None if section not found
      @raise Failure if section format is invalid *)
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

    DWARF 5 Specification Reference: Section 7.7.3 (Location Lists) *)
module DebugLoclists : sig
  type header = {
    unit_length : u32;  (** Length of the location lists contribution *)
    version : u16;  (** Version identifier (DWARF 5) *)
    address_size : u8;  (** Size of addresses in bytes *)
    segment_size : u8;  (** Size of segment selectors in bytes (usually 0) *)
    offset_entry_count : u32;  (** Number of entries in the offset table *)
  }
  (** Header structure for a location lists contribution.

      The location lists section contains location descriptions that are
      referenced by debug information entries via DW_AT_location attributes. *)

  (** Location list entry types as defined in DWARF 5 Section 7.7.3 *)
  type location_list_entry_type =
    | DW_LLE_end_of_list  (** 0x00 - End of location list *)
    | DW_LLE_base_addressx  (** 0x01 - Base address from address table *)
    | DW_LLE_startx_endx  (** 0x02 - Start/end addresses from address table *)
    | DW_LLE_startx_length  (** 0x03 - Start from address table + length *)
    | DW_LLE_offset_pair  (** 0x04 - Offset pair from base address *)
    | DW_LLE_default_location  (** 0x05 - Default location for object *)
    | DW_LLE_base_address  (** 0x06 - Base address (direct) *)
    | DW_LLE_start_end  (** 0x07 - Start/end addresses (direct) *)
    | DW_LLE_start_length  (** 0x08 - Start address + length (direct) *)

  type location_list_entry = {
    entry_type : location_list_entry_type;  (** Type of this entry *)
    data : string;  (** Raw data for the entry (varies by type) *)
  }
  (** Individual location list entry.

      Each entry describes a range of program counter values and the
      corresponding location where a variable can be found. *)

  type location_list = {
    offset : u32;  (** Offset within the section *)
    entries : location_list_entry list;  (** List of location entries *)
  }
  (** Complete location list for one object.

      Contains all location entries that describe where an object can be found
      throughout the program's execution. *)

  type loclists_section = {
    header : header;  (** Section header *)
    offset_table : u32 array;  (** Table of offsets to location lists *)
    location_lists : location_list list;  (** All location lists in section *)
  }
  (** Complete .debug_loclists section.

      Contains header information and all location lists for the compilation
      unit. *)

  val parse : Object.Buffer.t -> u32 -> loclists_section
  (** Parse location lists section from buffer.

      @param buffer Object buffer containing the DWARF data
      @param section_offset Offset to start of .debug_loclists section
      @return Parsed location lists section
      @raise Failure if section format is invalid *)
end

(** CompactUnwind module for Apple's Compact Unwinding Format.

    This module provides support for parsing and interpreting Apple's
    non-standard Compact Unwinding Format found in MachO binaries. The compact
    format provides efficient stack unwinding information stored in the
    __unwind_info section within the __TEXT segment.

    This is complementary to standard DWARF CFI and is used by Apple platforms
    for improved performance and reduced memory usage during stack unwinding. *)
module CompactUnwind : sig
  include module type of Compact_unwind

  val find_unwind_info_section : Object.Buffer.t -> (int * int) option
  (** Find the __unwind_info section in a MachO binary
      @param buffer Object buffer containing the MachO binary
      @return Optional tuple of (section_offset, section_size) *)

  val parse_from_buffer : Object.Buffer.t -> (unwind_info * architecture) option
  (** Parse compact unwind information from a MachO binary
      @param buffer Object buffer containing the MachO binary
      @return Optional tuple of (unwind_info, architecture) if parsing succeeds
  *)
end

(* TODO Why do we need both functions and why do they take string values rather than cursor? *)
val parse_cfi_instructions_basic : string -> (int * string) list
(** Parse basic CFI instructions from instruction bytes *)

val parse_cfi_instructions : string -> int64 -> int64 -> (int * string) list
(** Parse comprehensive CFI instructions with full DWARF 5 support.

    Parameters:
    - instructions: Raw CFI instruction bytes
    - code_alignment: Code alignment factor from CIE
    - data_alignment: Data alignment factor from CIE

    Returns list of (pc_offset, description) pairs showing CFI rules at each PC
    location. *)
