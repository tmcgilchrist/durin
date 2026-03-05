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
  | DW_OP_GNU_parameter_ref (* 4-byte DIE offset *)
  | DW_OP_GNU_variable_value (* address-sized DIE ref *)
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
  | 0xe0 -> DW_OP_form_tls_address
  | 0xf2 -> DW_OP_implicit_pointer
  | 0xf3 -> DW_OP_entry_value
  | 0xf4 -> DW_OP_const_type
  | 0xf5 -> DW_OP_regval_type
  | 0xf6 -> DW_OP_deref_type
  | 0xf7 -> DW_OP_convert
  | 0xf9 -> DW_OP_reinterpret
  | 0xfa -> DW_OP_GNU_parameter_ref
  | 0xfb -> DW_OP_addrx
  | 0xfc -> DW_OP_constx
  | 0xfd -> DW_OP_GNU_variable_value
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
  | DW_OP_GNU_parameter_ref -> "DW_OP_GNU_parameter_ref"
  | DW_OP_GNU_variable_value -> "DW_OP_GNU_variable_value"
  | DW_OP_hi_user -> "DW_OP_hi_user"
