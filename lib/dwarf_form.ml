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
  | DW_FORM_GNU_addr_index
  | DW_FORM_GNU_str_index
  | DW_FORM_GNU_ref_alt
  | DW_FORM_GNU_strp_alt
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
  | 0x1f01 -> DW_FORM_GNU_addr_index
  | 0x1f02 -> DW_FORM_GNU_str_index
  | 0x1f20 -> DW_FORM_GNU_ref_alt
  | 0x1f21 -> DW_FORM_GNU_strp_alt
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
  | 0x1f01 -> "DW_FORM_GNU_addr_index"
  | 0x1f02 -> "DW_FORM_GNU_str_index"
  | 0x1f20 -> "DW_FORM_GNU_ref_alt"
  | 0x1f21 -> "DW_FORM_GNU_strp_alt"
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
  | DW_FORM_GNU_addr_index -> "DW_FORM_GNU_addr_index"
  | DW_FORM_GNU_str_index -> "DW_FORM_GNU_str_index"
  | DW_FORM_GNU_ref_alt -> "DW_FORM_GNU_ref_alt"
  | DW_FORM_GNU_strp_alt -> "DW_FORM_GNU_strp_alt"
  | DW_FORM_unknown i -> Printf.sprintf "DW_FORM_unknown(%d)" i

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
  | DW_FORM_GNU_addr_index -> Unsigned.UInt64.of_int 0x1f01
  | DW_FORM_GNU_str_index -> Unsigned.UInt64.of_int 0x1f02
  | DW_FORM_GNU_ref_alt -> Unsigned.UInt64.of_int 0x1f20
  | DW_FORM_GNU_strp_alt -> Unsigned.UInt64.of_int 0x1f21
  | DW_FORM_unknown n -> Unsigned.UInt64.of_int n
