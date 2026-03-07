open Durin

let check_expr ?encoding ~name ~count ~expected bytes () =
  let parsed = Dwarf.parse_dwarf_expression ?encoding bytes in
  Alcotest.(check int) (name ^ " count") count (List.length parsed);
  let result_str = Dwarf.string_of_dwarf_expression parsed in
  Alcotest.(check string) (name ^ " string") expected result_str

let check_single_op ?encoding ~name ~expected bytes () =
  let parsed = Dwarf.parse_dwarf_expression ?encoding bytes in
  Alcotest.(check int) (name ^ " count") 1 (List.length parsed);
  let op = List.hd parsed in
  Alcotest.(check string)
    name expected
    (Dwarf.string_of_operation_encoding op.opcode);
  op

let dwarf4_encoding : Dwarf.encoding =
  {
    format = DWARF32;
    address_size = Unsigned.UInt8.of_int 8;
    version = Unsigned.UInt16.of_int 4;
  }

let dwarf5_64_encoding : Dwarf.encoding =
  {
    format = DWARF64;
    address_size = Unsigned.UInt8.of_int 8;
    version = Unsigned.UInt16.of_int 5;
  }

let test_gnu_implicit_pointer () =
  ignore
  @@ check_single_op ~encoding:dwarf4_encoding ~name:"gnu_implicit_pointer"
       ~expected:"DW_OP_implicit_pointer" "\xf2\x2a\x00\x00\x00\x05" ()

let test_gnu_parameter_ref () =
  let op =
    check_single_op ~name:"gnu_parameter_ref"
      ~expected:"DW_OP_GNU_parameter_ref" "\xfa\x42\x00\x00\x00" ()
  in
  Alcotest.(check int) "operand is 0x42" 0x42 (List.hd op.operands)

let test_gnu_variable_value () =
  ignore
  @@ check_single_op ~name:"gnu_variable_value"
       ~expected:"DW_OP_GNU_variable_value"
       "\xfd\x01\x02\x03\x04\x05\x06\x07\x08" ()

let test_implicit_pointer_dwarf4_encoding () =
  ignore
  @@ check_single_op ~encoding:dwarf4_encoding ~name:"implicit_pointer_dwarf4"
       ~expected:"DW_OP_implicit_pointer" "\xa0\x2a\x00\x00\x00\x00" ()

let test_implicit_pointer_dwarf64_encoding () =
  ignore
  @@ check_single_op ~encoding:dwarf5_64_encoding
       ~name:"implicit_pointer_dwarf64" ~expected:"DW_OP_implicit_pointer"
       "\xa0\x2a\x00\x00\x00\x00\x00\x00\x00\x00" ()

let () =
  Alcotest.run "DWARF_Expression"
    [
      ( "expression_parsing",
        [
          ( "simple_expression",
            `Quick,
            check_expr ~name:"simple" ~count:3
              ~expected:"DW_OP_breg7(0) DW_OP_const1u(8) DW_OP_plus"
              "\x77\x00\x08\x08\x22" );
          ( "literal_operations",
            `Quick,
            check_expr ~name:"literal" ~count:3
              ~expected:"DW_OP_lit5 DW_OP_lit10 DW_OP_plus" "\x35\x3a\x22" );
          ( "register_operations",
            `Quick,
            check_expr ~name:"register" ~count:2
              ~expected:"DW_OP_reg6 DW_OP_reg16" "\x56\x60" );
          ( "stack_operations",
            `Quick,
            check_expr ~name:"stack" ~count:3
              ~expected:"DW_OP_dup DW_OP_drop DW_OP_swap" "\x12\x13\x16" );
          ( "uleb128_operations",
            `Quick,
            check_expr ~name:"uleb128" ~count:2
              ~expected:"DW_OP_constu(128) DW_OP_plus_uconst(255)"
              "\x10\x80\x01\x23\xff\x01" );
          ( "empty_expression",
            `Quick,
            check_expr ~name:"empty" ~count:0 ~expected:"" "" );
          ( "unknown_opcode",
            `Quick,
            check_expr ~name:"unknown" ~count:1 ~expected:"DW_OP_lit5"
              "\x01\x35" );
        ] );
      ( "gnu_ops",
        [
          ( "gnu_push_tls_address",
            `Quick,
            fun () ->
              ignore
              @@ check_single_op ~name:"gnu_push_tls_address"
                   ~expected:"DW_OP_form_tls_address" "\xe0" () );
          ( "gnu_entry_value",
            `Quick,
            fun () ->
              ignore
              @@ check_single_op ~name:"gnu_entry_value"
                   ~expected:"DW_OP_entry_value" "\xf3\x01\x50" () );
          ( "gnu_convert",
            `Quick,
            fun () ->
              ignore
              @@ check_single_op ~name:"gnu_convert" ~expected:"DW_OP_convert"
                   "\xf7\x2a" () );
          ( "gnu_addr_index",
            `Quick,
            fun () ->
              ignore
              @@ check_single_op ~name:"gnu_addr_index" ~expected:"DW_OP_addrx"
                   "\xfb\x03" () );
          ( "gnu_const_index",
            `Quick,
            fun () ->
              ignore
              @@ check_single_op ~name:"gnu_const_index"
                   ~expected:"DW_OP_constx" "\xfc\x07" () );
          ("gnu_implicit_pointer", `Quick, test_gnu_implicit_pointer);
          ("gnu_parameter_ref", `Quick, test_gnu_parameter_ref);
          ("gnu_variable_value", `Quick, test_gnu_variable_value);
          ( "implicit_pointer_dwarf4",
            `Quick,
            test_implicit_pointer_dwarf4_encoding );
          ( "implicit_pointer_dwarf64",
            `Quick,
            test_implicit_pointer_dwarf64_encoding );
        ] );
    ]
