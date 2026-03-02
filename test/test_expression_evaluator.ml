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

(* Test DW_OP_const4s with -1 (regression for overflow fix) *)
let test_const4s () =
  (* DW_OP_const4s(0xFFFFFFFF) = -1 *)
  let bytecode = "\x0d\xff\xff\xff\xff" in
  let state =
    Dwarf.Expression.start_evaluation ~bytecode ~encoding:(make_encoding ())
  in
  let result = Dwarf.Expression.evaluate state in
  Alcotest.(check bool) "completes" true (result = Dwarf.Expression.Complete);
  let stack = Dwarf.Expression.result state in
  match stack with
  | [ Dwarf.Expression.Generic v ] ->
      Alcotest.(check int64) "const4s(-1) = -1" (-1L) v
  | _ -> Alcotest.fail "unexpected stack contents"

(* Test DW_OP_addr: push 8-byte address *)
let test_addr () =
  (* DW_OP_addr followed by 8 bytes LE for 0x0000000000400080 *)
  let bytecode = "\x03\x80\x00\x40\x00\x00\x00\x00\x00" in
  let state =
    Dwarf.Expression.start_evaluation ~bytecode ~encoding:(make_encoding ())
  in
  let result = Dwarf.Expression.evaluate state in
  Alcotest.(check bool) "completes" true (result = Dwarf.Expression.Complete);
  let stack = Dwarf.Expression.result state in
  match stack with
  | [ Dwarf.Expression.Address v ] ->
      Alcotest.(check int64) "address is 0x400080" 0x400080L v
  | _ -> Alcotest.fail "unexpected stack contents"

(* Test DW_OP_deref: pop address, return RequiresMemory *)
let test_deref () =
  (* DW_OP_const4u(0x1000) DW_OP_deref *)
  let bytecode = "\x0c\x00\x10\x00\x00\x06" in
  let state =
    Dwarf.Expression.start_evaluation ~bytecode ~encoding:(make_encoding ())
  in
  let result = Dwarf.Expression.evaluate state in
  match result with
  | Dwarf.Expression.RequiresMemory { address; size } -> (
      Alcotest.(check int64) "address is 0x1000" 0x1000L address;
      Alcotest.(check int) "size is 8" 8 size;
      let mem_bytes = "\x42\x00\x00\x00\x00\x00\x00\x00" in
      let result2 = Dwarf.Expression.resume_with_memory state mem_bytes in
      Alcotest.(check bool)
        "completes after memory" true
        (result2 = Dwarf.Expression.Complete);
      let stack = Dwarf.Expression.result state in
      match stack with
      | [ Dwarf.Expression.Generic v ] ->
          Alcotest.(check int64) "deref value is 0x42" 0x42L v
      | _ -> Alcotest.fail "unexpected stack after deref")
  | _ -> Alcotest.fail "expected RequiresMemory"

(* Test DW_OP_deref_size: 4-byte read *)
let test_deref_size () =
  (* DW_OP_const4u(0x2000) DW_OP_deref_size(4) *)
  let bytecode = "\x0c\x00\x20\x00\x00\x94\x04" in
  let state =
    Dwarf.Expression.start_evaluation ~bytecode ~encoding:(make_encoding ())
  in
  let result = Dwarf.Expression.evaluate state in
  match result with
  | Dwarf.Expression.RequiresMemory { address; size } -> (
      Alcotest.(check int64) "address is 0x2000" 0x2000L address;
      Alcotest.(check int) "size is 4" 4 size;
      let mem_bytes = "\xef\xbe\xad\xde" in
      let result2 = Dwarf.Expression.resume_with_memory state mem_bytes in
      Alcotest.(check bool)
        "completes after memory" true
        (result2 = Dwarf.Expression.Complete);
      let stack = Dwarf.Expression.result state in
      match stack with
      | [ Dwarf.Expression.Generic v ] ->
          Alcotest.(check int64) "deref_size value is 0xDEADBEEF" 0xDEADBEEFL v
      | _ -> Alcotest.fail "unexpected stack after deref_size")
  | _ -> Alcotest.fail "expected RequiresMemory"

(* Test DW_OP_pick: copy Nth stack item to top *)
let test_pick () =
  (* DW_OP_lit10 DW_OP_lit20 DW_OP_lit30 DW_OP_pick(2) *)
  (* Stack before pick: [30, 20, 10], pick index 2 -> 10 *)
  let bytecode = "\x3a\x44\x4e\x15\x02" in
  let state =
    Dwarf.Expression.start_evaluation ~bytecode ~encoding:(make_encoding ())
  in
  let result = Dwarf.Expression.evaluate state in
  Alcotest.(check bool) "completes" true (result = Dwarf.Expression.Complete);
  let stack = Dwarf.Expression.result state in
  Alcotest.(check int) "stack has 4 values" 4 (List.length stack);
  match stack with
  | Dwarf.Expression.Generic top :: _ ->
      Alcotest.(check int64) "picked value is 10" 10L top
  | _ -> Alcotest.fail "unexpected stack contents"

(* Test DW_OP_skip: skip over opcodes *)
let test_skip () =
  (* DW_OP_lit5 DW_OP_skip(+2) DW_OP_lit99 DW_OP_lit99
     DW_OP_lit10 *)
  (* skip jumps over the two lit99 opcodes *)
  let bytecode = "\x35\x2f\x02\x00\x93\x93\x3a" in
  let state =
    Dwarf.Expression.start_evaluation ~bytecode ~encoding:(make_encoding ())
  in
  let result = Dwarf.Expression.evaluate state in
  Alcotest.(check bool) "completes" true (result = Dwarf.Expression.Complete);
  let stack = Dwarf.Expression.result state in
  Alcotest.(check int) "stack has 2 values" 2 (List.length stack);
  match stack with
  | [ Dwarf.Expression.Generic top; Dwarf.Expression.Generic bot ] ->
      Alcotest.(check int64) "top is 10" 10L top;
      Alcotest.(check int64) "bottom is 5" 5L bot
  | _ -> Alcotest.fail "unexpected stack contents"

(* Test DW_OP_bra: branch taken when TOS non-zero *)
let test_bra_taken () =
  (* DW_OP_lit5 DW_OP_lit1 DW_OP_bra(+1) DW_OP_nop DW_OP_lit10 *)
  (* TOS=1 (non-zero), branch skips the nop *)
  let bytecode = "\x35\x31\x28\x01\x00\x96\x3a" in
  let state =
    Dwarf.Expression.start_evaluation ~bytecode ~encoding:(make_encoding ())
  in
  let result = Dwarf.Expression.evaluate state in
  Alcotest.(check bool) "completes" true (result = Dwarf.Expression.Complete);
  let stack = Dwarf.Expression.result state in
  Alcotest.(check int) "stack has 2 values" 2 (List.length stack);
  match stack with
  | [ Dwarf.Expression.Generic top; Dwarf.Expression.Generic bot ] ->
      Alcotest.(check int64) "top is 10" 10L top;
      Alcotest.(check int64) "bottom is 5" 5L bot
  | _ -> Alcotest.fail "unexpected stack contents"

(* Test DW_OP_bra: branch not taken when TOS is zero *)
let test_bra_not_taken () =
  (* DW_OP_lit5 DW_OP_lit0 DW_OP_bra(+1) DW_OP_lit20 *)
  (* TOS=0, no branch, so lit20 executes *)
  let bytecode = "\x35\x30\x28\x01\x00\x44" in
  let state =
    Dwarf.Expression.start_evaluation ~bytecode ~encoding:(make_encoding ())
  in
  let result = Dwarf.Expression.evaluate state in
  Alcotest.(check bool) "completes" true (result = Dwarf.Expression.Complete);
  let stack = Dwarf.Expression.result state in
  Alcotest.(check int) "stack has 2 values" 2 (List.length stack);
  match stack with
  | [ Dwarf.Expression.Generic top; Dwarf.Expression.Generic bot ] ->
      Alcotest.(check int64) "top is 20" 20L top;
      Alcotest.(check int64) "bottom is 5" 5L bot
  | _ -> Alcotest.fail "unexpected stack contents"

(* Test DW_OP_piece: populate pieces list *)
let test_piece () =
  (* DW_OP_lit5 DW_OP_piece(4) *)
  let bytecode = "\x35\x93\x04" in
  let state =
    Dwarf.Expression.start_evaluation ~bytecode ~encoding:(make_encoding ())
  in
  let result = Dwarf.Expression.evaluate state in
  Alcotest.(check bool) "completes" true (result = Dwarf.Expression.Complete);
  let pieces = Dwarf.Expression.pieces state in
  Alcotest.(check int) "one piece" 1 (List.length pieces);
  match pieces with
  | [ p ] -> (
      Alcotest.(check (option int))
        "size_in_bits is 32" (Some 32) p.size_in_bits;
      Alcotest.(check (option int)) "bit_offset is None" None p.bit_offset;
      match p.location with
      | Some (Dwarf.Expression.Generic v) ->
          Alcotest.(check int64) "location is 5" 5L v
      | _ -> Alcotest.fail "expected Generic location")
  | _ -> Alcotest.fail "unexpected pieces"

(* Helper: evaluate bytecode and return top-of-stack int64 *)
let eval_to_int64 bytecode =
  let state =
    Dwarf.Expression.start_evaluation ~bytecode ~encoding:(make_encoding ())
  in
  let result = Dwarf.Expression.evaluate state in
  Alcotest.(check bool) "completes" true (result = Dwarf.Expression.Complete);
  match Dwarf.Expression.result state with
  | [ Dwarf.Expression.Generic v ] -> v
  | _ -> Alcotest.fail "expected single Generic on stack"

(* Helper: check that evaluation raises a Failure *)
let expect_failure bytecode msg_substring =
  let state =
    Dwarf.Expression.start_evaluation ~bytecode ~encoding:(make_encoding ())
  in
  match Dwarf.Expression.evaluate state with
  | _ -> Alcotest.fail ("expected failure: " ^ msg_substring)
  | exception Failure m ->
      let contains =
        try
          let _ = Str.search_forward (Str.regexp_string msg_substring) m 0 in
          true
        with Not_found -> false
      in
      Alcotest.(check bool) ("error contains: " ^ msg_substring) true contains

(* ================================================================ *)
(* Error handling tests                                              *)
(* ================================================================ *)

let test_error_stack_underflow_drop () =
  (* DW_OP_drop on empty stack *)
  expect_failure "\x13" "Stack underflow"

let test_error_stack_underflow_binary () =
  (* DW_OP_lit5 DW_OP_plus — only 1 item for binary op *)
  expect_failure "\x35\x22" "Stack underflow"

let test_error_dup_empty () =
  (* DW_OP_dup on empty stack *)
  expect_failure "\x12" "DW_OP_dup on empty stack"

let test_error_over_insufficient () =
  (* DW_OP_lit5 DW_OP_over — only 1 item *)
  expect_failure "\x35\x14" "DW_OP_over requires at least 2"

let test_error_swap_insufficient () =
  (* DW_OP_lit5 DW_OP_swap — only 1 item *)
  expect_failure "\x35\x16" "DW_OP_swap requires at least 2"

let test_error_rot_insufficient () =
  (* DW_OP_lit5 DW_OP_lit6 DW_OP_rot — only 2 items *)
  expect_failure "\x35\x36\x17" "DW_OP_rot requires at least 3"

let test_error_pick_out_of_range () =
  (* DW_OP_lit5 DW_OP_pick(5) — only 1 item, index 5 *)
  expect_failure "\x35\x15\x05" "DW_OP_pick index out of range"

let test_error_unknown_opcode () =
  (* opcode 0xff is not defined *)
  expect_failure "\xff" "Expression evaluation error"

let test_error_skip_out_of_bounds () =
  (* DW_OP_skip with offset that goes past end *)
  (* skip offset = 0x7F00 (way past end) *)
  expect_failure "\x2f\x00\x7f" "target out of bounds"

let test_error_bra_out_of_bounds () =
  (* DW_OP_lit1 DW_OP_bra with offset past end *)
  expect_failure "\x31\x28\x00\x7f" "target out of bounds"

let test_error_div_by_zero () =
  (* DW_OP_lit5 DW_OP_lit0 DW_OP_div(0x1b) *)
  expect_failure "\x35\x30\x1b" "Division by zero"

let test_error_mod_by_zero () =
  (* DW_OP_lit5 DW_OP_lit0 DW_OP_mod *)
  expect_failure "\x35\x30\x1d" "Modulo by zero"

let test_error_invalid_memory_size () =
  (* DW_OP_lit0 DW_OP_deref, then resume with 3 bytes *)
  let bytecode = "\x30\x06" in
  let state =
    Dwarf.Expression.start_evaluation ~bytecode ~encoding:(make_encoding ())
  in
  let result = Dwarf.Expression.evaluate state in
  match result with
  | Dwarf.Expression.RequiresMemory _ -> (
      match Dwarf.Expression.resume_with_memory state "abc" with
      | _ -> Alcotest.fail "expected failure for 3-byte memory"
      | exception Failure m ->
          let contains =
            try
              let _ =
                Str.search_forward
                  (Str.regexp_string "Unsupported memory read size")
                  m 0
              in
              true
            with Not_found -> false
          in
          Alcotest.(check bool) "error mentions unsupported size" true contains)
  | _ -> Alcotest.fail "expected RequiresMemory"

let test_error_deref_size_exceeds_addr_size () =
  (* DW_OP_lit0 DW_OP_deref_size(8) in 32-bit mode *)
  (* deref_size=8 > address_size=4 should fail *)
  let bytecode = "\x30\x94\x08" in
  let state =
    Dwarf.Expression.start_evaluation ~bytecode
      ~encoding:(make_encoding ~address_size:4 ())
  in
  match Dwarf.Expression.evaluate state with
  | _ -> Alcotest.fail "expected failure for deref_size > addr_size"
  | exception Failure m ->
      let contains =
        try
          let _ =
            Str.search_forward (Str.regexp_string "exceeds address size") m 0
          in
          true
        with Not_found -> false
      in
      Alcotest.(check bool) "error mentions exceeds" true contains

let test_error_resume_register_wrong_state () =
  (* Try resume_with_register when not waiting *)
  let bytecode = "\x35" in
  let state =
    Dwarf.Expression.start_evaluation ~bytecode ~encoding:(make_encoding ())
  in
  let _ = Dwarf.Expression.evaluate state in
  match
    Dwarf.Expression.resume_with_register state (Dwarf.Expression.Generic 0L)
  with
  | _ -> Alcotest.fail "expected failure"
  | exception Failure _ -> ()

let test_error_resume_memory_wrong_state () =
  let bytecode = "\x35" in
  let state =
    Dwarf.Expression.start_evaluation ~bytecode ~encoding:(make_encoding ())
  in
  let _ = Dwarf.Expression.evaluate state in
  match Dwarf.Expression.resume_with_memory state "\x00" with
  | _ -> Alcotest.fail "expected failure"
  | exception Failure _ -> ()

let test_error_resume_frame_base_wrong_state () =
  let bytecode = "\x35" in
  let state =
    Dwarf.Expression.start_evaluation ~bytecode ~encoding:(make_encoding ())
  in
  let _ = Dwarf.Expression.evaluate state in
  match Dwarf.Expression.resume_with_frame_base state 0L with
  | _ -> Alcotest.fail "expected failure"
  | exception Failure _ -> ()

let test_error_resume_cfa_wrong_state () =
  let bytecode = "\x35" in
  let state =
    Dwarf.Expression.start_evaluation ~bytecode ~encoding:(make_encoding ())
  in
  let _ = Dwarf.Expression.evaluate state in
  match Dwarf.Expression.resume_with_cfa state 0L with
  | _ -> Alcotest.fail "expected failure"
  | exception Failure _ -> ()

(* ================================================================ *)
(* Arithmetic edge case tests                                       *)
(* ================================================================ *)

let test_addition_overflow () =
  (* 0xFFFFFFFFFFFFFFFF + 1 should wrap to 0 (mod addr_mask) *)
  (* DW_OP_const8u(0xFFFFFFFFFFFFFFFF) DW_OP_lit1 DW_OP_plus *)
  let bytecode = "\x0e\xff\xff\xff\xff\xff\xff\xff\xff\x31\x22" in
  let v = eval_to_int64 bytecode in
  Alcotest.(check int64) "overflow wraps to 0" 0L v

let test_subtraction_underflow () =
  (* 0 - 1 wraps to 0xFFFFFFFFFFFFFFFF *)
  (* DW_OP_lit0 DW_OP_lit1 DW_OP_minus *)
  let bytecode = "\x30\x31\x1c" in
  let v = eval_to_int64 bytecode in
  Alcotest.(check int64) "underflow wraps to -1" (-1L) v

let test_signed_division () =
  (* -6 / 2 = -3 *)
  (* DW_OP_consts(-6) DW_OP_lit2 DW_OP_div(0x1b) *)
  (* SLEB128 of -6 = 0x7a *)
  let bytecode = "\x11\x7a\x32\x1b" in
  let v = eval_to_int64 bytecode in
  Alcotest.(check int64) "-6 / 2 = -3" (-3L) v

let test_abs_negative () =
  (* abs(-5) = 5 *)
  (* DW_OP_consts(-5) DW_OP_abs *)
  (* SLEB128 of -5 = 0x7b *)
  let bytecode = "\x11\x7b\x19" in
  let v = eval_to_int64 bytecode in
  Alcotest.(check int64) "abs(-5) = 5" 5L v

let test_neg_positive () =
  (* neg(5) = -5 *)
  (* DW_OP_lit5 DW_OP_neg *)
  let bytecode = "\x35\x1f" in
  let v = eval_to_int64 bytecode in
  Alcotest.(check int64) "neg(5) = -5" (-5L) v

let test_not_zero () =
  (* not(0) = -1 (all bits set) *)
  (* DW_OP_lit0 DW_OP_not *)
  let bytecode = "\x30\x20" in
  let v = eval_to_int64 bytecode in
  Alcotest.(check int64) "not(0) = -1" (-1L) v

let test_shift_by_zero () =
  (* 5 << 0 = 5 *)
  (* DW_OP_lit5 DW_OP_lit0 DW_OP_shl *)
  let bytecode = "\x35\x30\x24" in
  let v = eval_to_int64 bytecode in
  Alcotest.(check int64) "5 << 0 = 5" 5L v

let test_shr_logical () =
  (* 0xFF >> 1 = 0x7F (logical, zero-fill) *)
  (* DW_OP_const1u(0xFF) DW_OP_lit1 DW_OP_shr *)
  let bytecode = "\x08\xff\x31\x25" in
  let v = eval_to_int64 bytecode in
  (* const1u pushes 255 as unsigned, shr is logical *)
  Alcotest.(check int64) "0xFF >> 1 = 0x7F" 0x7FL v

let test_shra_negative () =
  (* Arithmetic right shift of -2 by 1 = -1 *)
  (* DW_OP_consts(-2) DW_OP_lit1 DW_OP_shra *)
  (* SLEB128 of -2 = 0x7e *)
  let bytecode = "\x11\x7e\x31\x26" in
  let v = eval_to_int64 bytecode in
  Alcotest.(check int64) "shra(-2, 1) = -1" (-1L) v

let test_mul_overflow () =
  (* Large multiplication that overflows *)
  (* DW_OP_const8u(0x8000000000000001) DW_OP_lit2 DW_OP_mul *)
  let bytecode = "\x0e\x01\x00\x00\x00\x00\x00\x00\x80\x32\x1e" in
  let v = eval_to_int64 bytecode in
  (* Wrapping: 0x8000000000000001 * 2 = 0x0000000000000002 *)
  Alcotest.(check int64) "overflow mul wraps" 2L v

(* ================================================================ *)
(* Untested operations: constants                                   *)
(* ================================================================ *)

let test_const1s () =
  (* DW_OP_const1s(0xFF) = -1 *)
  let bytecode = "\x09\xff" in
  let v = eval_to_int64 bytecode in
  Alcotest.(check int64) "const1s(0xFF) = -1" (-1L) v

let test_const1s_boundary () =
  (* DW_OP_const1s(0x80) = -128 *)
  let bytecode = "\x09\x80" in
  let v = eval_to_int64 bytecode in
  Alcotest.(check int64) "const1s(0x80) = -128" (-128L) v

let test_const2s () =
  (* DW_OP_const2s(0x8000) = -32768, LE: 0x00 0x80 *)
  let bytecode = "\x0b\x00\x80" in
  let v = eval_to_int64 bytecode in
  Alcotest.(check int64) "const2s(0x8000) = -32768" (-32768L) v

let test_const2s_negative_one () =
  (* DW_OP_const2s(0xFFFF) = -1 *)
  let bytecode = "\x0b\xff\xff" in
  let v = eval_to_int64 bytecode in
  Alcotest.(check int64) "const2s(0xFFFF) = -1" (-1L) v

let test_const4u () =
  (* DW_OP_const4u(0x12345678) LE: 0x78 0x56 0x34 0x12 *)
  let bytecode = "\x0c\x78\x56\x34\x12" in
  let v = eval_to_int64 bytecode in
  Alcotest.(check int64) "const4u = 0x12345678" 0x12345678L v

let test_const4s_boundary () =
  (* DW_OP_const4s(0x80000000) = -2147483648 *)
  let bytecode = "\x0d\x00\x00\x00\x80" in
  let v = eval_to_int64 bytecode in
  Alcotest.(check int64) "const4s(0x80000000) = -2147483648" (-2147483648L) v

let test_const8u () =
  (* DW_OP_const8u(0x0102030405060708) *)
  let bytecode = "\x0e\x08\x07\x06\x05\x04\x03\x02\x01" in
  let v = eval_to_int64 bytecode in
  Alcotest.(check int64) "const8u = 0x0102030405060708" 0x0102030405060708L v

let test_const8s () =
  (* DW_OP_const8s(0xFFFFFFFFFFFFFFFF) = -1 *)
  let bytecode = "\x0f\xff\xff\xff\xff\xff\xff\xff\xff" in
  let v = eval_to_int64 bytecode in
  Alcotest.(check int64) "const8s(-1) = -1" (-1L) v

let test_constu () =
  (* DW_OP_constu(300) ULEB128: 0xAC 0x02 *)
  let bytecode = "\x10\xac\x02" in
  let v = eval_to_int64 bytecode in
  Alcotest.(check int64) "constu(300) = 300" 300L v

let test_consts () =
  (* DW_OP_consts(-100) SLEB128: 0x9C 0x7F *)
  let bytecode = "\x11\x9c\x7f" in
  let v = eval_to_int64 bytecode in
  Alcotest.(check int64) "consts(-100) = -100" (-100L) v

(* ================================================================ *)
(* Untested operations: stack manipulation                          *)
(* ================================================================ *)

let test_drop () =
  (* DW_OP_lit5 DW_OP_lit10 DW_OP_drop *)
  let bytecode = "\x35\x3a\x13" in
  let state =
    Dwarf.Expression.start_evaluation ~bytecode ~encoding:(make_encoding ())
  in
  let result = Dwarf.Expression.evaluate state in
  Alcotest.(check bool) "completes" true (result = Dwarf.Expression.Complete);
  let stack = Dwarf.Expression.result state in
  Alcotest.(check int) "stack has 1 value" 1 (List.length stack);
  match stack with
  | [ Dwarf.Expression.Generic v ] ->
      Alcotest.(check int64) "remaining is 5" 5L v
  | _ -> Alcotest.fail "unexpected stack"

let test_over () =
  (* DW_OP_lit5 DW_OP_lit10 DW_OP_over *)
  let bytecode = "\x35\x3a\x14" in
  let state =
    Dwarf.Expression.start_evaluation ~bytecode ~encoding:(make_encoding ())
  in
  let result = Dwarf.Expression.evaluate state in
  Alcotest.(check bool) "completes" true (result = Dwarf.Expression.Complete);
  let stack = Dwarf.Expression.result state in
  Alcotest.(check int) "stack has 3 values" 3 (List.length stack);
  match stack with
  | [ Dwarf.Expression.Generic top; _; _ ] ->
      Alcotest.(check int64) "over copied 5 to top" 5L top
  | _ -> Alcotest.fail "unexpected stack"

let test_rot () =
  (* DW_OP_lit1 DW_OP_lit2 DW_OP_lit3 DW_OP_rot *)
  (* Stack before: [3, 2, 1]. After rot: [1, 3, 2] *)
  let bytecode = "\x31\x32\x33\x17" in
  let state =
    Dwarf.Expression.start_evaluation ~bytecode ~encoding:(make_encoding ())
  in
  let result = Dwarf.Expression.evaluate state in
  Alcotest.(check bool) "completes" true (result = Dwarf.Expression.Complete);
  let stack = Dwarf.Expression.result state in
  match stack with
  | [
   Dwarf.Expression.Generic a;
   Dwarf.Expression.Generic b;
   Dwarf.Expression.Generic c;
  ] ->
      Alcotest.(check int64) "top is 1" 1L a;
      Alcotest.(check int64) "mid is 3" 3L b;
      Alcotest.(check int64) "bot is 2" 2L c
  | _ -> Alcotest.fail "unexpected stack"

(* ================================================================ *)
(* Untested operations: arithmetic                                  *)
(* ================================================================ *)

let test_div () =
  (* DW_OP_lit20 DW_OP_lit4 DW_OP_div(0x1b) *)
  let bytecode = "\x44\x34\x1b" in
  let v = eval_to_int64 bytecode in
  Alcotest.(check int64) "20 / 4 = 5" 5L v

let test_mod () =
  (* DW_OP_lit17 DW_OP_lit5 DW_OP_mod *)
  let bytecode = "\x41\x35\x1d" in
  let v = eval_to_int64 bytecode in
  Alcotest.(check int64) "17 mod 5 = 2" 2L v

let test_mod_unsigned () =
  (* DWARF spec: DW_OP_mod is unsigned modulus *)
  (* consts(-3) = 0xFFFFFFFFFFFFFFFD as unsigned, mod 2 = 1 *)
  (* DW_OP_consts(-3) DW_OP_lit2 DW_OP_mod(0x1d) *)
  (* SLEB128 of -3 = 0x7d *)
  let bytecode = "\x11\x7d\x32\x1d" in
  let v = eval_to_int64 bytecode in
  Alcotest.(check int64) "unsigned: 0xFFFFFFFFFFFFFFFD mod 2 = 1" 1L v

let test_abs_positive () =
  (* abs(5) = 5, no change *)
  let bytecode = "\x35\x19" in
  let v = eval_to_int64 bytecode in
  Alcotest.(check int64) "abs(5) = 5" 5L v

let test_and () =
  (* 0xFF AND 0x0F = 0x0F *)
  (* DW_OP_const1u(0xFF) DW_OP_const1u(0x0F) DW_OP_and(0x1a) *)
  let bytecode = "\x08\xff\x08\x0f\x1a" in
  let v = eval_to_int64 bytecode in
  Alcotest.(check int64) "0xFF & 0x0F = 0x0F" 0x0FL v

let test_or () =
  (* 0xF0 OR 0x0F = 0xFF *)
  let bytecode = "\x08\xf0\x08\x0f\x21" in
  let v = eval_to_int64 bytecode in
  Alcotest.(check int64) "0xF0 | 0x0F = 0xFF" 0xFFL v

let test_xor () =
  (* 0xFF XOR 0xFF = 0 *)
  let bytecode = "\x08\xff\x08\xff\x27" in
  let v = eval_to_int64 bytecode in
  Alcotest.(check int64) "0xFF ^ 0xFF = 0" 0L v

let test_shl () =
  (* 1 << 8 = 256 *)
  (* DW_OP_lit1 DW_OP_const1u(8) DW_OP_shl *)
  let bytecode = "\x31\x08\x08\x24" in
  let v = eval_to_int64 bytecode in
  Alcotest.(check int64) "1 << 8 = 256" 256L v

let test_shr () =
  (* 256 >> 4 = 16 (logical) *)
  (* DW_OP_const2u(256) DW_OP_lit4 DW_OP_shr *)
  let bytecode = "\x0a\x00\x01\x34\x25" in
  let v = eval_to_int64 bytecode in
  Alcotest.(check int64) "256 >> 4 = 16" 16L v

let test_shra () =
  (* 256 >> 4 = 16 (arithmetic, positive value same as logical) *)
  let bytecode = "\x0a\x00\x01\x34\x26" in
  let v = eval_to_int64 bytecode in
  Alcotest.(check int64) "shra(256, 4) = 16" 16L v

(* ================================================================ *)
(* Untested operations: comparisons                                 *)
(* ================================================================ *)

let test_ge_true () =
  (* 10 >= 5 = 1 *)
  let bytecode = "\x3a\x35\x2a" in
  let v = eval_to_int64 bytecode in
  Alcotest.(check int64) "10 >= 5 = 1" 1L v

let test_ge_equal () =
  (* 5 >= 5 = 1 *)
  let bytecode = "\x35\x35\x2a" in
  let v = eval_to_int64 bytecode in
  Alcotest.(check int64) "5 >= 5 = 1" 1L v

let test_ge_false () =
  (* 4 >= 5 = 0 *)
  let bytecode = "\x34\x35\x2a" in
  let v = eval_to_int64 bytecode in
  Alcotest.(check int64) "4 >= 5 = 0" 0L v

let test_gt () =
  (* 10 > 5 = 1 *)
  let bytecode = "\x3a\x35\x2b" in
  let v = eval_to_int64 bytecode in
  Alcotest.(check int64) "10 > 5 = 1" 1L v

let test_gt_equal () =
  (* 5 > 5 = 0 *)
  let bytecode = "\x35\x35\x2b" in
  let v = eval_to_int64 bytecode in
  Alcotest.(check int64) "5 > 5 = 0" 0L v

let test_le () =
  (* 5 <= 10 = 1 *)
  let bytecode = "\x35\x3a\x2c" in
  let v = eval_to_int64 bytecode in
  Alcotest.(check int64) "5 <= 10 = 1" 1L v

let test_lt () =
  (* 5 < 10 = 1 *)
  let bytecode = "\x35\x3a\x2d" in
  let v = eval_to_int64 bytecode in
  Alcotest.(check int64) "5 < 10 = 1" 1L v

let test_lt_false () =
  (* 10 < 5 = 0 *)
  let bytecode = "\x3a\x35\x2d" in
  let v = eval_to_int64 bytecode in
  Alcotest.(check int64) "10 < 5 = 0" 0L v

let test_eq_false () =
  (* 5 == 10 = 0 *)
  let bytecode = "\x35\x3a\x29" in
  let v = eval_to_int64 bytecode in
  Alcotest.(check int64) "5 == 10 = 0" 0L v

(* ================================================================ *)
(* Untested operations: register variants                           *)
(* ================================================================ *)

let test_regx () =
  (* DW_OP_regx with ULEB128 register number 100 (0x64) *)
  let bytecode = "\x90\xe4\x00" in
  let state =
    Dwarf.Expression.start_evaluation ~bytecode ~encoding:(make_encoding ())
  in
  let result = Dwarf.Expression.evaluate state in
  match result with
  | Dwarf.Expression.RequiresRegister
      { register = Dwarf.Expression.Register n; offset = None } ->
      Alcotest.(check int) "regx register is 100" 100 n
  | _ -> Alcotest.fail "expected RequiresRegister"

let test_bregx () =
  (* DW_OP_bregx with ULEB128 register 50 (0x32),
     SLEB128 offset -16 (0x70) *)
  let bytecode = "\x92\x32\x70" in
  let state =
    Dwarf.Expression.start_evaluation ~bytecode ~encoding:(make_encoding ())
  in
  let result = Dwarf.Expression.evaluate state in
  match result with
  | Dwarf.Expression.RequiresRegister
      { register = Dwarf.Expression.Register n; offset = Some off } ->
      Alcotest.(check int) "bregx register is 50" 50 n;
      Alcotest.(check int64) "bregx offset is -16" (-16L) off
  | _ -> Alcotest.fail "expected RequiresRegister with offset"

(* ================================================================ *)
(* Composite location tests                                         *)
(* ================================================================ *)

let test_bit_piece () =
  (* DW_OP_lit7 DW_OP_bit_piece(size=16, offset=4) *)
  (* ULEB128 16 = 0x10, ULEB128 4 = 0x04 *)
  let bytecode = "\x37\x9d\x10\x04" in
  let state =
    Dwarf.Expression.start_evaluation ~bytecode ~encoding:(make_encoding ())
  in
  let result = Dwarf.Expression.evaluate state in
  Alcotest.(check bool) "completes" true (result = Dwarf.Expression.Complete);
  let pieces = Dwarf.Expression.pieces state in
  Alcotest.(check int) "one piece" 1 (List.length pieces);
  match pieces with
  | [ p ] -> (
      Alcotest.(check (option int))
        "size_in_bits is 16" (Some 16) p.size_in_bits;
      Alcotest.(check (option int)) "bit_offset is 4" (Some 4) p.bit_offset;
      match p.location with
      | Some (Dwarf.Expression.Generic v) ->
          Alcotest.(check int64) "location is 7" 7L v
      | _ -> Alcotest.fail "expected Generic location")
  | _ -> Alcotest.fail "unexpected pieces"

let test_multi_piece () =
  (* DW_OP_lit5 DW_OP_piece(2) DW_OP_lit10 DW_OP_piece(2) *)
  let bytecode = "\x35\x93\x02\x3a\x93\x02" in
  let state =
    Dwarf.Expression.start_evaluation ~bytecode ~encoding:(make_encoding ())
  in
  let result = Dwarf.Expression.evaluate state in
  Alcotest.(check bool) "completes" true (result = Dwarf.Expression.Complete);
  let pieces = Dwarf.Expression.pieces state in
  Alcotest.(check int) "two pieces" 2 (List.length pieces);
  match pieces with
  | [ p1; p2 ] -> (
      Alcotest.(check (option int))
        "p1 size_in_bits is 16" (Some 16) p1.size_in_bits;
      Alcotest.(check (option int))
        "p2 size_in_bits is 16" (Some 16) p2.size_in_bits;
      match (p1.location, p2.location) with
      | Some (Dwarf.Expression.Generic v1), Some (Dwarf.Expression.Generic v2)
        ->
          Alcotest.(check int64) "p1 location is 5" 5L v1;
          Alcotest.(check int64) "p2 location is 10" 10L v2
      | _ -> Alcotest.fail "expected Generic locations")
  | _ -> Alcotest.fail "unexpected pieces"

let test_piece_empty () =
  (* DW_OP_piece(4) with empty stack = empty piece *)
  let bytecode = "\x93\x04" in
  let state =
    Dwarf.Expression.start_evaluation ~bytecode ~encoding:(make_encoding ())
  in
  let result = Dwarf.Expression.evaluate state in
  Alcotest.(check bool) "completes" true (result = Dwarf.Expression.Complete);
  let pieces = Dwarf.Expression.pieces state in
  Alcotest.(check int) "one piece" 1 (List.length pieces);
  match pieces with
  | [ p ] ->
      Alcotest.(check (option int)) "size is 32" (Some 32) p.size_in_bits;
      Alcotest.(check bool) "location is None" true (p.location = None)
  | _ -> Alcotest.fail "unexpected pieces"

(* ================================================================ *)
(* 32-bit address mode tests                                        *)
(* ================================================================ *)

let test_32bit_addition_masking () =
  (* In 32-bit mode, plus should mask to 32 bits *)
  (* DW_OP_const4u(0xFFFFFFFF) DW_OP_lit1 DW_OP_plus *)
  let bytecode = "\x0c\xff\xff\xff\xff\x31\x22" in
  let state =
    Dwarf.Expression.start_evaluation ~bytecode
      ~encoding:(make_encoding ~address_size:4 ())
  in
  let result = Dwarf.Expression.evaluate state in
  Alcotest.(check bool) "completes" true (result = Dwarf.Expression.Complete);
  match Dwarf.Expression.result state with
  | [ Dwarf.Expression.Generic v ] ->
      Alcotest.(check int64) "32-bit overflow wraps to 0" 0L v
  | _ -> Alcotest.fail "unexpected stack"

let test_32bit_deref_size () =
  (* In 32-bit mode, deref requests 4 bytes *)
  let bytecode = "\x30\x06" in
  let state =
    Dwarf.Expression.start_evaluation ~bytecode
      ~encoding:(make_encoding ~address_size:4 ())
  in
  let result = Dwarf.Expression.evaluate state in
  match result with
  | Dwarf.Expression.RequiresMemory { size; _ } ->
      Alcotest.(check int) "32-bit deref size is 4" 4 size
  | _ -> Alcotest.fail "expected RequiresMemory"

let test_32bit_addr () =
  (* DW_OP_addr with 4-byte address in 32-bit mode *)
  (* address = 0x00400080 LE *)
  let bytecode = "\x03\x80\x00\x40\x00" in
  let state =
    Dwarf.Expression.start_evaluation ~bytecode
      ~encoding:(make_encoding ~address_size:4 ())
  in
  let result = Dwarf.Expression.evaluate state in
  Alcotest.(check bool) "completes" true (result = Dwarf.Expression.Complete);
  match Dwarf.Expression.result state with
  | [ Dwarf.Expression.Address v ] ->
      Alcotest.(check int64) "32-bit addr is 0x400080" 0x400080L v
  | _ -> Alcotest.fail "unexpected stack"

(* ================================================================ *)
(* Multi-step evaluation tests                                      *)
(* ================================================================ *)

let test_deref_then_computation () =
  (* DW_OP_const4u(0x1000) DW_OP_deref DW_OP_lit5 DW_OP_plus *)
  let bytecode = "\x0c\x00\x10\x00\x00\x06\x35\x22" in
  let state =
    Dwarf.Expression.start_evaluation ~bytecode ~encoding:(make_encoding ())
  in
  let result = Dwarf.Expression.evaluate state in
  match result with
  | Dwarf.Expression.RequiresMemory { address; _ } -> (
      Alcotest.(check int64) "deref addr" 0x1000L address;
      let mem = "\x0a\x00\x00\x00\x00\x00\x00\x00" in
      let result2 = Dwarf.Expression.resume_with_memory state mem in
      Alcotest.(check bool)
        "completes" true
        (result2 = Dwarf.Expression.Complete);
      match Dwarf.Expression.result state with
      | [ Dwarf.Expression.Generic v ] ->
          Alcotest.(check int64) "10 + 5 = 15" 15L v
      | _ -> Alcotest.fail "unexpected stack")
  | _ -> Alcotest.fail "expected RequiresMemory"

let test_fbreg_then_deref () =
  (* DW_OP_fbreg(-8) DW_OP_deref *)
  let bytecode = "\x91\x78\x06" in
  let state =
    Dwarf.Expression.start_evaluation ~bytecode ~encoding:(make_encoding ())
  in
  (* First: needs frame base *)
  let result = Dwarf.Expression.evaluate state in
  (match result with
  | Dwarf.Expression.RequiresFrameBase { offset } ->
      Alcotest.(check int64) "offset is -8" (-8L) offset
  | _ -> Alcotest.fail "expected RequiresFrameBase");
  (* Provide frame base 1000, so 1000 + (-8) = 992 pushed *)
  let result2 = Dwarf.Expression.resume_with_frame_base state 1000L in
  (* Now it should need memory at address 992 *)
  (match result2 with
  | Dwarf.Expression.RequiresMemory { address; size } ->
      Alcotest.(check int64) "deref addr is 992" 992L address;
      Alcotest.(check int) "deref size is 8" 8 size
  | _ -> Alcotest.fail "expected RequiresMemory after fbreg");
  (* Provide memory value *)
  let mem = "\x42\x00\x00\x00\x00\x00\x00\x00" in
  let result3 = Dwarf.Expression.resume_with_memory state mem in
  Alcotest.(check bool) "completes" true (result3 = Dwarf.Expression.Complete);
  match Dwarf.Expression.result state with
  | [ Dwarf.Expression.Generic v ] ->
      Alcotest.(check int64) "deref result is 0x42" 0x42L v
  | _ -> Alcotest.fail "unexpected stack"

(* ================================================================ *)
(* Empty expression                                                 *)
(* ================================================================ *)

let test_empty_expression () =
  let bytecode = "" in
  let state =
    Dwarf.Expression.start_evaluation ~bytecode ~encoding:(make_encoding ())
  in
  let result = Dwarf.Expression.evaluate state in
  Alcotest.(check bool)
    "empty completes" true
    (result = Dwarf.Expression.Complete);
  let stack = Dwarf.Expression.result state in
  Alcotest.(check int) "empty stack" 0 (List.length stack)

let () =
  let existing_tests =
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
      ("const4s", `Quick, test_const4s);
      ("addr", `Quick, test_addr);
      ("deref", `Quick, test_deref);
      ("deref_size", `Quick, test_deref_size);
      ("pick", `Quick, test_pick);
      ("skip", `Quick, test_skip);
      ("bra_taken", `Quick, test_bra_taken);
      ("bra_not_taken", `Quick, test_bra_not_taken);
      ("piece", `Quick, test_piece);
    ]
  in
  let error_tests =
    [
      ("error_stack_underflow_drop", `Quick, test_error_stack_underflow_drop);
      ("error_stack_underflow_binary", `Quick, test_error_stack_underflow_binary);
      ("error_dup_empty", `Quick, test_error_dup_empty);
      ("error_over_insufficient", `Quick, test_error_over_insufficient);
      ("error_swap_insufficient", `Quick, test_error_swap_insufficient);
      ("error_rot_insufficient", `Quick, test_error_rot_insufficient);
      ("error_pick_out_of_range", `Quick, test_error_pick_out_of_range);
      ("error_unknown_opcode", `Quick, test_error_unknown_opcode);
      ("error_skip_out_of_bounds", `Quick, test_error_skip_out_of_bounds);
      ("error_bra_out_of_bounds", `Quick, test_error_bra_out_of_bounds);
      ("error_div_by_zero", `Quick, test_error_div_by_zero);
      ("error_mod_by_zero", `Quick, test_error_mod_by_zero);
      ("error_invalid_memory_size", `Quick, test_error_invalid_memory_size);
      ( "error_deref_size_exceeds_addr_size",
        `Quick,
        test_error_deref_size_exceeds_addr_size );
      ( "error_resume_register_wrong_state",
        `Quick,
        test_error_resume_register_wrong_state );
      ( "error_resume_memory_wrong_state",
        `Quick,
        test_error_resume_memory_wrong_state );
      ( "error_resume_frame_base_wrong_state",
        `Quick,
        test_error_resume_frame_base_wrong_state );
      ("error_resume_cfa_wrong_state", `Quick, test_error_resume_cfa_wrong_state);
    ]
  in
  let arithmetic_edge_tests =
    [
      ("addition_overflow", `Quick, test_addition_overflow);
      ("subtraction_underflow", `Quick, test_subtraction_underflow);
      ("signed_division", `Quick, test_signed_division);
      ("abs_negative", `Quick, test_abs_negative);
      ("neg_positive", `Quick, test_neg_positive);
      ("not_zero", `Quick, test_not_zero);
      ("shift_by_zero", `Quick, test_shift_by_zero);
      ("shr_logical", `Quick, test_shr_logical);
      ("shra_negative", `Quick, test_shra_negative);
      ("mul_overflow", `Quick, test_mul_overflow);
    ]
  in
  let constant_tests =
    [
      ("const1s", `Quick, test_const1s);
      ("const1s_boundary", `Quick, test_const1s_boundary);
      ("const2s", `Quick, test_const2s);
      ("const2s_negative_one", `Quick, test_const2s_negative_one);
      ("const4u", `Quick, test_const4u);
      ("const4s_boundary", `Quick, test_const4s_boundary);
      ("const8u", `Quick, test_const8u);
      ("const8s", `Quick, test_const8s);
      ("constu", `Quick, test_constu);
      ("consts", `Quick, test_consts);
    ]
  in
  let stack_tests =
    [
      ("drop", `Quick, test_drop);
      ("over", `Quick, test_over);
      ("rot", `Quick, test_rot);
    ]
  in
  let arith_op_tests =
    [
      ("div", `Quick, test_div);
      ("mod", `Quick, test_mod);
      ("mod_unsigned", `Quick, test_mod_unsigned);
      ("abs_positive", `Quick, test_abs_positive);
      ("and", `Quick, test_and);
      ("or", `Quick, test_or);
      ("xor", `Quick, test_xor);
      ("shl", `Quick, test_shl);
      ("shr", `Quick, test_shr);
      ("shra", `Quick, test_shra);
    ]
  in
  let comparison_tests =
    [
      ("ge_true", `Quick, test_ge_true);
      ("ge_equal", `Quick, test_ge_equal);
      ("ge_false", `Quick, test_ge_false);
      ("gt", `Quick, test_gt);
      ("gt_equal", `Quick, test_gt_equal);
      ("le", `Quick, test_le);
      ("lt", `Quick, test_lt);
      ("lt_false", `Quick, test_lt_false);
      ("eq_false", `Quick, test_eq_false);
    ]
  in
  let register_tests =
    [ ("regx", `Quick, test_regx); ("bregx", `Quick, test_bregx) ]
  in
  let composite_tests =
    [
      ("bit_piece", `Quick, test_bit_piece);
      ("multi_piece", `Quick, test_multi_piece);
      ("piece_empty", `Quick, test_piece_empty);
    ]
  in
  let addr_32bit_tests =
    [
      ("32bit_addition_masking", `Quick, test_32bit_addition_masking);
      ("32bit_deref_size", `Quick, test_32bit_deref_size);
      ("32bit_addr", `Quick, test_32bit_addr);
    ]
  in
  let multi_step_tests =
    [
      ("deref_then_computation", `Quick, test_deref_then_computation);
      ("fbreg_then_deref", `Quick, test_fbreg_then_deref);
    ]
  in
  let misc_tests = [ ("empty_expression", `Quick, test_empty_expression) ] in
  Alcotest.run "Expression_Evaluator"
    [
      ("evaluator", existing_tests);
      ("errors", error_tests);
      ("arithmetic_edge_cases", arithmetic_edge_tests);
      ("constants", constant_tests);
      ("stack_ops", stack_tests);
      ("arithmetic_ops", arith_op_tests);
      ("comparisons", comparison_tests);
      ("registers", register_tests);
      ("composite_locations", composite_tests);
      ("32bit_mode", addr_32bit_tests);
      ("multi_step", multi_step_tests);
      ("misc", misc_tests);
    ]
