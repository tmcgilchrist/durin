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
    (* New in DWARF 5 *)
    | DW_TAG_coarray_type -> 0x44
    | DW_TAG_generic_subrange -> 0x45
    | DW_TAG_dynamic_type -> 0x46
    | DW_TAG_atomic_type -> 0x47
    | DW_TAG_call_site -> 0x48
    | DW_TAG_call_site_parameter -> 0x49
    | DW_TAG_skeleton_unit -> 0x4a
    | DW_TAG_immutable_type -> 0x4b
    (* GNU extensions *)
    | DW_TAG_GNU_template_parameter_pack -> 0x4107
    | DW_TAG_lo_user -> 0x4080
    | DW_TAG_hi_user -> 0xffff
  in
  Unsigned.UInt64.of_int code

let string_of_abbreviation_tag = function
  | DW_TAG_null -> "DW_TAG_null"
  | DW_TAG_array_type -> "DW_TAG_array_type"
  | DW_TAG_class_type -> "DW_TAG_class_type"
  | DW_TAG_entry_point -> "DW_TAG_entry_point"
  | DW_TAG_enumeration_type -> "DW_TAG_enumeration_type"
  | DW_TAG_formal_parameter -> "DW_TAG_formal_parameter"
  | DW_TAG_imported_declaration -> "DW_TAG_imported_declaration"
  | DW_TAG_label -> "DW_TAG_label"
  | DW_TAG_lexical_block -> "DW_TAG_lexical_block"
  | DW_TAG_member -> "DW_TAG_member"
  | DW_TAG_pointer_type -> "DW_TAG_pointer_type"
  | DW_TAG_reference_type -> "DW_TAG_reference_type"
  | DW_TAG_compile_unit -> "DW_TAG_compile_unit"
  | DW_TAG_string_type -> "DW_TAG_string_type"
  | DW_TAG_structure_type -> "DW_TAG_structure_type"
  | DW_TAG_subroutine_type -> "DW_TAG_subroutine_type"
  | DW_TAG_typedef -> "DW_TAG_typedef"
  | DW_TAG_union_type -> "DW_TAG_union_type"
  | DW_TAG_unspecified_parameters -> "DW_TAG_unspecified_parameters"
  | DW_TAG_variant -> "DW_TAG_variant"
  | DW_TAG_common_block -> "DW_TAG_common_block"
  | DW_TAG_common_inclusion -> "DW_TAG_common_inclusion"
  | DW_TAG_inheritance -> "DW_TAG_inheritance"
  | DW_TAG_inlined_subroutine -> "DW_TAG_inlined_subroutine"
  | DW_TAG_module -> "DW_TAG_module"
  | DW_TAG_ptr_to_member_type -> "DW_TAG_ptr_to_member_type"
  | DW_TAG_set_type -> "DW_TAG_set_type"
  | DW_TAG_subrange_type -> "DW_TAG_subrange_type"
  | DW_TAG_with_stmt -> "DW_TAG_with_stmt"
  | DW_TAG_access_declaration -> "DW_TAG_access_declaration"
  | DW_TAG_base_type -> "DW_TAG_base_type"
  | DW_TAG_catch_block -> "DW_TAG_catch_block"
  | DW_TAG_const_type -> "DW_TAG_const_type"
  | DW_TAG_constant -> "DW_TAG_constant"
  | DW_TAG_enumerator -> "DW_TAG_enumerator"
  | DW_TAG_file_type -> "DW_TAG_file_type"
  | DW_TAG_friend -> "DW_TAG_friend"
  | DW_TAG_namelist -> "DW_TAG_namelist"
  | DW_TAG_namelist_item -> "DW_TAG_namelist_item"
  | DW_TAG_packed_type -> "DW_TAG_packed_type"
  | DW_TAG_subprogram -> "DW_TAG_subprogram"
  | DW_TAG_template_type_parameter -> "DW_TAG_template_type_parameter"
  | DW_TAG_template_value_parameter -> "DW_TAG_template_value_parameter"
  | DW_TAG_thrown_type -> "DW_TAG_thrown_type"
  | DW_TAG_try_block -> "DW_TAG_try_block"
  | DW_TAG_variant_part -> "DW_TAG_variant_part"
  | DW_TAG_variable -> "DW_TAG_variable"
  | DW_TAG_volatile_type -> "DW_TAG_volatile_type"
  | DW_TAG_dwarf_procedure -> "DW_TAG_dwarf_procedure"
  | DW_TAG_restrict_type -> "DW_TAG_restrict_type"
  | DW_TAG_interface_type -> "DW_TAG_interface_type"
  | DW_TAG_namespace -> "DW_TAG_namespace"
  | DW_TAG_imported_module -> "DW_TAG_imported_module"
  | DW_TAG_unspecified_type -> "DW_TAG_unspecified_type"
  | DW_TAG_partial_unit -> "DW_TAG_partial_unit"
  | DW_TAG_imported_unit -> "DW_TAG_imported_unit"
  | DW_TAG_condition -> "DW_TAG_condition"
  | DW_TAG_shared_type -> "DW_TAG_shared_type"
  | DW_TAG_type_unit -> "DW_TAG_type_unit"
  | DW_TAG_rvalue_reference_type -> "DW_TAG_rvalue_reference_type"
  | DW_TAG_template_alias -> "DW_TAG_template_alias"
  | DW_TAG_coarray_type -> "DW_TAG_coarray_type"
  | DW_TAG_generic_subrange -> "DW_TAG_generic_subrange"
  | DW_TAG_dynamic_type -> "DW_TAG_dynamic_type"
  | DW_TAG_atomic_type -> "DW_TAG_atomic_type"
  | DW_TAG_call_site -> "DW_TAG_call_site"
  | DW_TAG_call_site_parameter -> "DW_TAG_call_site_parameter"
  | DW_TAG_skeleton_unit -> "DW_TAG_skeleton_unit"
  | DW_TAG_immutable_type -> "DW_TAG_immutable_type"
  | DW_TAG_GNU_template_parameter_pack -> "DW_TAG_GNU_template_parameter_pack"
  | DW_TAG_lo_user -> "DW_TAG_lo_user"
  | DW_TAG_hi_user -> "DW_TAG_hi_user"

type children_determination = DW_CHILDREN_no | DW_CHILDREN_yes

let children_determination = function
  | 0x00 -> DW_CHILDREN_no
  | 0x01 -> DW_CHILDREN_yes
  | n -> failwith (Printf.sprintf "Unknown children_determination: 0x%02x" n)

let int_of_children_determination = function
  | DW_CHILDREN_no -> 0x00
  | DW_CHILDREN_yes -> 0x01
