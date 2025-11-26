open Durin

(* Helper to create a basic encoding for 64-bit addresses *)
let make_encoding ?(address_size = 8) () =
  {
    Dwarf.format = Dwarf.DWARF32;
    version = Unsigned.UInt16.of_int 5;
    address_size = Unsigned.UInt8.of_int address_size;
  }

(* Test literal operations *)
let test_literals () =
  (* DW_OP_lit5: Push literal 5 *)
  let bytecode = "\x35" in
  let state =
    Dwarf.Expression.start_evaluation ~bytecode ~encoding:(make_encoding ())
  in
  let result = Dwarf.Expression.evaluate state in
  Alcotest.(check bool)
    "DW_OP_lit5 completes" true
    (result = Dwarf.Expression.Complete);
  let stack = Dwarf.Expression.result state in
  Alcotest.(check int) "stack has one value" 1 (List.length stack);
  match stack with
  | [ Dwarf.Expression.Generic v ] ->
      Alcotest.(check int64) "literal value is 5" 5L v
  | _ -> Alcotest.fail "unexpected stack contents"

(* Test multiple literals *)
let test_multiple_literals () =
  (* DW_OP_lit0 DW_OP_lit31: Push 0 and 31 *)
  let bytecode = "\x30\x4f" in
  let state =
    Dwarf.Expression.start_evaluation ~bytecode ~encoding:(make_encoding ())
  in
  let result = Dwarf.Expression.evaluate state in
  Alcotest.(check bool) "completes" true (result = Dwarf.Expression.Complete);
  let stack = Dwarf.Expression.result state in
  Alcotest.(check int) "stack has two values" 2 (List.length stack);
  match stack with
  | [ Dwarf.Expression.Generic v1; Dwarf.Expression.Generic v0 ] ->
      Alcotest.(check int64) "top is 31" 31L v1;
      Alcotest.(check int64) "bottom is 0" 0L v0
  | _ -> Alcotest.fail "unexpected stack contents"

(* Test arithmetic: addition *)
let test_addition () =
  (* DW_OP_lit5 DW_OP_lit10 DW_OP_plus *)
  let bytecode = "\x35\x3a\x22" in
  let state =
    Dwarf.Expression.start_evaluation ~bytecode ~encoding:(make_encoding ())
  in
  let result = Dwarf.Expression.evaluate state in
  Alcotest.(check bool) "completes" true (result = Dwarf.Expression.Complete);
  let stack = Dwarf.Expression.result state in
  match stack with
  | [ Dwarf.Expression.Generic v ] -> Alcotest.(check int64) "5 + 10 = 15" 15L v
  | _ -> Alcotest.fail "unexpected stack contents"

(* Test arithmetic: subtraction *)
let test_subtraction () =
  (* DW_OP_lit20 DW_OP_lit7 DW_OP_minus *)
  let bytecode = "\x44\x37\x1c" in
  let state =
    Dwarf.Expression.start_evaluation ~bytecode ~encoding:(make_encoding ())
  in
  let result = Dwarf.Expression.evaluate state in
  Alcotest.(check bool) "completes" true (result = Dwarf.Expression.Complete);
  let stack = Dwarf.Expression.result state in
  match stack with
  | [ Dwarf.Expression.Generic v ] -> Alcotest.(check int64) "20 - 7 = 13" 13L v
  | _ -> Alcotest.fail "unexpected stack contents"

(* Test arithmetic: multiplication *)
let test_multiplication () =
  (* DW_OP_lit6 DW_OP_lit7 DW_OP_mul *)
  let bytecode = "\x36\x37\x1e" in
  let state =
    Dwarf.Expression.start_evaluation ~bytecode ~encoding:(make_encoding ())
  in
  let result = Dwarf.Expression.evaluate state in
  Alcotest.(check bool) "completes" true (result = Dwarf.Expression.Complete);
  let stack = Dwarf.Expression.result state in
  match stack with
  | [ Dwarf.Expression.Generic v ] -> Alcotest.(check int64) "6 * 7 = 42" 42L v
  | _ -> Alcotest.fail "unexpected stack contents"

(* Test stack operations: dup *)
let test_dup () =
  (* DW_OP_lit5 DW_OP_dup *)
  let bytecode = "\x35\x12" in
  let state =
    Dwarf.Expression.start_evaluation ~bytecode ~encoding:(make_encoding ())
  in
  let result = Dwarf.Expression.evaluate state in
  Alcotest.(check bool) "completes" true (result = Dwarf.Expression.Complete);
  let stack = Dwarf.Expression.result state in
  Alcotest.(check int) "stack has two values" 2 (List.length stack);
  match stack with
  | [ Dwarf.Expression.Generic v1; Dwarf.Expression.Generic v0 ] ->
      Alcotest.(check int64) "both are 5" 5L v1;
      Alcotest.(check int64) "both are 5" 5L v0
  | _ -> Alcotest.fail "unexpected stack contents"

(* Test stack operations: swap *)
let test_swap () =
  (* DW_OP_lit5 DW_OP_lit10 DW_OP_swap *)
  let bytecode = "\x35\x3a\x16" in
  let state =
    Dwarf.Expression.start_evaluation ~bytecode ~encoding:(make_encoding ())
  in
  let result = Dwarf.Expression.evaluate state in
  Alcotest.(check bool) "completes" true (result = Dwarf.Expression.Complete);
  let stack = Dwarf.Expression.result state in
  match stack with
  | [ Dwarf.Expression.Generic v1; Dwarf.Expression.Generic v0 ] ->
      Alcotest.(check int64) "top is now 5" 5L v1;
      Alcotest.(check int64) "bottom is now 10" 10L v0
  | _ -> Alcotest.fail "unexpected stack contents"

(* Test comparison: equal *)
let test_eq () =
  (* DW_OP_lit5 DW_OP_lit5 DW_OP_eq *)
  let bytecode = "\x35\x35\x29" in
  let state =
    Dwarf.Expression.start_evaluation ~bytecode ~encoding:(make_encoding ())
  in
  let result = Dwarf.Expression.evaluate state in
  Alcotest.(check bool) "completes" true (result = Dwarf.Expression.Complete);
  let stack = Dwarf.Expression.result state in
  match stack with
  | [ Dwarf.Expression.Generic v ] ->
      Alcotest.(check int64) "5 == 5 is true (1)" 1L v
  | _ -> Alcotest.fail "unexpected stack contents"

(* Test comparison: not equal *)
let test_ne () =
  (* DW_OP_lit5 DW_OP_lit10 DW_OP_ne *)
  let bytecode = "\x35\x3a\x2e" in
  let state =
    Dwarf.Expression.start_evaluation ~bytecode ~encoding:(make_encoding ())
  in
  let result = Dwarf.Expression.evaluate state in
  Alcotest.(check bool) "completes" true (result = Dwarf.Expression.Complete);
  let stack = Dwarf.Expression.result state in
  match stack with
  | [ Dwarf.Expression.Generic v ] ->
      Alcotest.(check int64) "5 != 10 is true (1)" 1L v
  | _ -> Alcotest.fail "unexpected stack contents"

(* Test register operations - callback pattern *)
let test_register_callback () =
  (* DW_OP_reg5: Requires register 5 *)
  let bytecode = "\x55" in
  let state =
    Dwarf.Expression.start_evaluation ~bytecode ~encoding:(make_encoding ())
  in
  let result = Dwarf.Expression.evaluate state in
  match result with
  | Dwarf.Expression.RequiresRegister { register; offset } -> (
      match (register, offset) with
      | Dwarf.Expression.Register 5, None -> (
          (* Resume with register value 42 *)
          let result2 =
            Dwarf.Expression.resume_with_register state
              (Dwarf.Expression.Generic 42L)
          in
          Alcotest.(check bool)
            "completes after register" true
            (result2 = Dwarf.Expression.Complete);
          let stack = Dwarf.Expression.result state in
          match stack with
          | [ Dwarf.Expression.Generic v ] ->
              Alcotest.(check int64) "register value is 42" 42L v
          | _ -> Alcotest.fail "unexpected stack after resume")
      | _ -> Alcotest.fail "wrong register or offset")
  | _ -> Alcotest.fail "expected RequiresRegister"

(* Test breg operations - register with offset *)
let test_breg_callback () =
  (* DW_OP_breg7 with offset 16 (SLEB128: 0x10) *)
  let bytecode = "\x77\x10" in
  let state =
    Dwarf.Expression.start_evaluation ~bytecode ~encoding:(make_encoding ())
  in
  let result = Dwarf.Expression.evaluate state in
  match result with
  | Dwarf.Expression.RequiresRegister { register; offset } -> (
      match (register, offset) with
      | Dwarf.Expression.Register 7, Some off -> (
          Alcotest.(check int64) "offset is 16" 16L off;
          (* Resume with register value 100 *)
          let result2 =
            Dwarf.Expression.resume_with_register state
              (Dwarf.Expression.Generic 100L)
          in
          Alcotest.(check bool)
            "completes after register" true
            (result2 = Dwarf.Expression.Complete);
          let stack = Dwarf.Expression.result state in
          match stack with
          | [ Dwarf.Expression.Generic v ] ->
              Alcotest.(check int64) "100 + 16 = 116" 116L v
          | _ -> Alcotest.fail "unexpected stack after resume")
      | _ -> Alcotest.fail "wrong register or offset")
  | _ -> Alcotest.fail "expected RequiresRegister with offset"

(* Test const operations *)
let test_const1u () =
  (* DW_OP_const1u(255) *)
  let bytecode = "\x08\xff" in
  let state =
    Dwarf.Expression.start_evaluation ~bytecode ~encoding:(make_encoding ())
  in
  let result = Dwarf.Expression.evaluate state in
  Alcotest.(check bool) "completes" true (result = Dwarf.Expression.Complete);
  let stack = Dwarf.Expression.result state in
  match stack with
  | [ Dwarf.Expression.Generic v ] ->
      Alcotest.(check int64) "const1u value is 255" 255L v
  | _ -> Alcotest.fail "unexpected stack contents"

(* Test const2u *)
let test_const2u () =
  (* DW_OP_const2u(1000) = 0x03e8 little-endian *)
  let bytecode = "\x0a\xe8\x03" in
  let state =
    Dwarf.Expression.start_evaluation ~bytecode ~encoding:(make_encoding ())
  in
  let result = Dwarf.Expression.evaluate state in
  Alcotest.(check bool) "completes" true (result = Dwarf.Expression.Complete);
  let stack = Dwarf.Expression.result state in
  match stack with
  | [ Dwarf.Expression.Generic v ] ->
      Alcotest.(check int64) "const2u value is 1000" 1000L v
  | _ -> Alcotest.fail "unexpected stack contents"

(* Test plus_uconst *)
let test_plus_uconst () =
  (* DW_OP_lit5 DW_OP_plus_uconst(10) - ULEB128: 0x0a *)
  let bytecode = "\x35\x23\x0a" in
  let state =
    Dwarf.Expression.start_evaluation ~bytecode ~encoding:(make_encoding ())
  in
  let result = Dwarf.Expression.evaluate state in
  Alcotest.(check bool) "completes" true (result = Dwarf.Expression.Complete);
  let stack = Dwarf.Expression.result state in
  match stack with
  | [ Dwarf.Expression.Generic v ] -> Alcotest.(check int64) "5 + 10 = 15" 15L v
  | _ -> Alcotest.fail "unexpected stack contents"

(* Test nop *)
let test_nop () =
  (* DW_OP_lit5 DW_OP_nop DW_OP_lit10 *)
  let bytecode = "\x35\x96\x3a" in
  let state =
    Dwarf.Expression.start_evaluation ~bytecode ~encoding:(make_encoding ())
  in
  let result = Dwarf.Expression.evaluate state in
  Alcotest.(check bool) "completes" true (result = Dwarf.Expression.Complete);
  let stack = Dwarf.Expression.result state in
  Alcotest.(check int) "stack has two values" 2 (List.length stack);
  match stack with
  | [ Dwarf.Expression.Generic v1; Dwarf.Expression.Generic v0 ] ->
      Alcotest.(check int64) "top is 10" 10L v1;
      Alcotest.(check int64) "bottom is 5" 5L v0
  | _ -> Alcotest.fail "unexpected stack contents"

(* Test stack_value *)
let test_stack_value () =
  (* DW_OP_const1u(42) DW_OP_stack_value *)
  let bytecode = "\x08\x2a\x9f" in
  let state =
    Dwarf.Expression.start_evaluation ~bytecode ~encoding:(make_encoding ())
  in
  let result = Dwarf.Expression.evaluate state in
  Alcotest.(check bool) "completes" true (result = Dwarf.Expression.Complete);
  let stack = Dwarf.Expression.result state in
  match stack with
  | [ Dwarf.Expression.Address v ] ->
      Alcotest.(check int64) "address value is 42" 42L v
  | _ -> Alcotest.fail "expected Address value"

(* Test frame base callback *)
let test_fbreg_callback () =
  (* DW_OP_fbreg with offset -8 (SLEB128: 0x78 = -8 in SLEB128) *)
  let bytecode = "\x91\x78" in
  let state =
    Dwarf.Expression.start_evaluation ~bytecode ~encoding:(make_encoding ())
  in
  let result = Dwarf.Expression.evaluate state in
  match result with
  | Dwarf.Expression.RequiresFrameBase { offset } when offset = -8L -> (
      (* Resume with frame base 1000 *)
      let result2 = Dwarf.Expression.resume_with_frame_base state 1000L in
      Alcotest.(check bool)
        "completes after frame base" true
        (result2 = Dwarf.Expression.Complete);
      let stack = Dwarf.Expression.result state in
      match stack with
      | [ Dwarf.Expression.Generic v ] ->
          Alcotest.(check int64) "1000 + (-8) = 992" 992L v
      | _ -> Alcotest.fail "unexpected stack after resume")
  | _ -> Alcotest.fail "expected RequiresFrameBase with offset -8"

(* Test CFA callback *)
let test_cfa_callback () =
  (* DW_OP_call_frame_cfa *)
  let bytecode = "\x9c" in
  let state =
    Dwarf.Expression.start_evaluation ~bytecode ~encoding:(make_encoding ())
  in
  let result = Dwarf.Expression.evaluate state in
  match result with
  | Dwarf.Expression.RequiresCFA -> (
      (* Resume with CFA 2000 *)
      let result2 = Dwarf.Expression.resume_with_cfa state 2000L in
      Alcotest.(check bool)
        "completes after CFA" true
        (result2 = Dwarf.Expression.Complete);
      let stack = Dwarf.Expression.result state in
      match stack with
      | [ Dwarf.Expression.Address v ] ->
          Alcotest.(check int64) "CFA is 2000" 2000L v
      | _ -> Alcotest.fail "unexpected stack after resume")
  | Complete | RequiresFrameBase _ | RequiresRegister _ | RequiresMemory _ ->
      Alcotest.fail "expected RequiresCFA"

(* Test address size masking for 32-bit *)
let test_address_size_32bit () =
  (* DW_OP_const4u(0xFFFFFFFF) with 32-bit address size *)
  let bytecode = "\x0c\xff\xff\xff\xff" in
  let state =
    Dwarf.Expression.start_evaluation ~bytecode
      ~encoding:(make_encoding ~address_size:4 ())
  in
  let result = Dwarf.Expression.evaluate state in
  Alcotest.(check bool) "completes" true (result = Dwarf.Expression.Complete);
  let stack = Dwarf.Expression.result state in
  match stack with
  | [ Dwarf.Expression.Generic v ] ->
      Alcotest.(check int64) "32-bit max value" 0xFFFFFFFFL v
  | _ -> Alcotest.fail "unexpected stack contents"

(* Test complex expression: (reg5 + 16) * 8 *)
let test_complex_expression () =
  (* DW_OP_breg5(16) DW_OP_lit8 DW_OP_mul *)
  let bytecode = "\x75\x10\x38\x1e" in
  let state =
    Dwarf.Expression.start_evaluation ~bytecode ~encoding:(make_encoding ())
  in
  let result = Dwarf.Expression.evaluate state in
  match result with
  | Dwarf.Expression.RequiresRegister { register; offset } -> (
      match (register, offset) with
      | Dwarf.Expression.Register 5, Some 16L -> (
          (* Provide register value 100 *)
          let result2 =
            Dwarf.Expression.resume_with_register state
              (Dwarf.Expression.Generic 100L)
          in
          Alcotest.(check bool)
            "completes after register" true
            (result2 = Dwarf.Expression.Complete);
          let stack = Dwarf.Expression.result state in
          match stack with
          | [ Dwarf.Expression.Generic v ] ->
              Alcotest.(check int64) "(100 + 16) * 8 = 928" 928L v
          | _ -> Alcotest.fail "unexpected stack after complex eval")
      | _ -> Alcotest.fail "wrong register or offset")
  | _ -> Alcotest.fail "expected RequiresRegister"

let () =
  let tests =
    [
      ("literals", `Quick, test_literals);
      ("multiple_literals", `Quick, test_multiple_literals);
      ("addition", `Quick, test_addition);
      ("subtraction", `Quick, test_subtraction);
      ("multiplication", `Quick, test_multiplication);
      ("dup", `Quick, test_dup);
      ("swap", `Quick, test_swap);
      ("eq", `Quick, test_eq);
      ("ne", `Quick, test_ne);
      ("register_callback", `Quick, test_register_callback);
      ("breg_callback", `Quick, test_breg_callback);
      ("const1u", `Quick, test_const1u);
      ("const2u", `Quick, test_const2u);
      ("plus_uconst", `Quick, test_plus_uconst);
      ("nop", `Quick, test_nop);
      ("stack_value", `Quick, test_stack_value);
      ("fbreg_callback", `Quick, test_fbreg_callback);
      ("cfa_callback", `Quick, test_cfa_callback);
      ("address_size_32bit", `Quick, test_address_size_32bit);
      ("complex_expression", `Quick, test_complex_expression);
    ]
  in
  Alcotest.run "Expression_Evaluator" [ ("evaluator", tests) ]
