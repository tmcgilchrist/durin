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

let string_of_calling_convention = function
  | DW_CC_normal -> "DW_CC_normal"
  | DW_CC_program -> "DW_CC_program"
  | DW_CC_nocall -> "DW_CC_nocall"
  | DW_CC_pass_by_reference -> "DW_CC_pass_by_reference"
  | DW_CC_pass_by_value -> "DW_CC_pass_by_value"
  | DW_CC_lo_user -> "DW_CC_lo_user"
  | DW_CC_hi_user -> "DW_CC_hi_user"

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

type defaulted_attribute =
  | DW_DEFAULTED_no
  | DW_DEFAULTED_in_class
  | DW_DEFAULTED_out_of_class

let defaulted_attribute = function
  | 0x00 -> DW_DEFAULTED_no
  | 0x01 -> DW_DEFAULTED_in_class
  | 0x02 -> DW_DEFAULTED_out_of_class
  | n -> failwith (Printf.sprintf "Unknown defaulted_attribute: 0x%02x" n)

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
