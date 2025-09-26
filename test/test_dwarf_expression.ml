open Durin

let test_simple_expression () =
  (* Test simple expression: DW_OP_breg7(0) DW_OP_const1u(8) DW_OP_plus *)
  (* Bytecode: 0x77 0x00 0x08 0x08 0x22 *)
  let test_expr = "\x77\x00\x08\x08\x22" in
  let parsed = Dwarf.parse_dwarf_expression test_expr in
  Alcotest.(check int) "parsed operation count" 3 (List.length parsed);
  let result_str = Dwarf.string_of_dwarf_expression parsed in
  let expected_str = "DW_OP_breg7(0) DW_OP_const1u(8) DW_OP_plus" in
  Alcotest.(check string)
    "expression string representation" expected_str result_str

let test_literal_operations () =
  (* Test literal values: DW_OP_lit5 DW_OP_lit10 DW_OP_plus *)
  (* Bytecode: 0x35 0x3a 0x22 *)
  let test_expr = "\x35\x3a\x22" in
  let parsed = Dwarf.parse_dwarf_expression test_expr in
  Alcotest.(check int) "literal ops count" 3 (List.length parsed);
  let result_str = Dwarf.string_of_dwarf_expression parsed in
  let expected_str = "DW_OP_lit5 DW_OP_lit10 DW_OP_plus" in
  Alcotest.(check string) "literal expression string" expected_str result_str

let test_register_operations () =
  (* Test register operations: DW_OP_reg6 DW_OP_reg16 *)
  (* Bytecode: 0x56 0x60 *)
  let test_expr = "\x56\x60" in
  let parsed = Dwarf.parse_dwarf_expression test_expr in
  Alcotest.(check int) "register ops count" 2 (List.length parsed);
  let result_str = Dwarf.string_of_dwarf_expression parsed in
  let expected_str = "DW_OP_reg6 DW_OP_reg16" in
  Alcotest.(check string) "register expression string" expected_str result_str

let test_stack_operations () =
  (* Test stack operations: DW_OP_dup DW_OP_drop DW_OP_swap *)
  (* Bytecode: 0x12 0x13 0x16 *)
  let test_expr = "\x12\x13\x16" in
  let parsed = Dwarf.parse_dwarf_expression test_expr in
  Alcotest.(check int) "stack ops count" 3 (List.length parsed);
  let result_str = Dwarf.string_of_dwarf_expression parsed in
  let expected_str = "DW_OP_dup DW_OP_drop DW_OP_swap" in
  Alcotest.(check string) "stack expression string" expected_str result_str

let test_uleb128_operations () =
  (* Test ULEB128 operations: DW_OP_constu(128) DW_OP_plus_uconst(255) *)
  (* 128 in ULEB128 = 0x80 0x01, 255 in ULEB128 = 0xff 0x01 *)
  (* Bytecode: 0x10 0x80 0x01 0x23 0xff 0x01 *)
  let test_expr = "\x10\x80\x01\x23\xff\x01" in
  let parsed = Dwarf.parse_dwarf_expression test_expr in
  Alcotest.(check int) "uleb128 ops count" 2 (List.length parsed);
  let result_str = Dwarf.string_of_dwarf_expression parsed in
  let expected_str = "DW_OP_constu(128) DW_OP_plus_uconst(255)" in
  Alcotest.(check string) "uleb128 expression string" expected_str result_str

let test_empty_expression () =
  (* Test empty expression *)
  let test_expr = "" in
  let parsed = Dwarf.parse_dwarf_expression test_expr in
  Alcotest.(check int) "empty expression count" 0 (List.length parsed);
  let result_str = Dwarf.string_of_dwarf_expression parsed in
  let expected_str = "" in
  Alcotest.(check string) "empty expression string" expected_str result_str

let test_unknown_opcode () =
  (* Test unknown opcode handling *)
  let test_expr = "\x01\x35" in
  (* 0x01 is unknown, 0x35 is DW_OP_lit5 *)
  let parsed = Dwarf.parse_dwarf_expression test_expr in
  (* Should skip unknown opcode and parse DW_OP_lit5 *)
  Alcotest.(check int) "unknown opcode handling" 1 (List.length parsed);
  let result_str = Dwarf.string_of_dwarf_expression parsed in
  let expected_str = "DW_OP_lit5" in
  Alcotest.(check string) "unknown opcode expression" expected_str result_str

let () =
  let tests =
    [
      ("simple_expression", `Quick, test_simple_expression);
      ("literal_operations", `Quick, test_literal_operations);
      ("register_operations", `Quick, test_register_operations);
      ("stack_operations", `Quick, test_stack_operations);
      ("uleb128_operations", `Quick, test_uleb128_operations);
      ("empty_expression", `Quick, test_empty_expression);
      ("unknown_opcode", `Quick, test_unknown_opcode);
    ]
  in
  Alcotest.run "DWARF_Expression" [ ("expression_parsing", tests) ]
