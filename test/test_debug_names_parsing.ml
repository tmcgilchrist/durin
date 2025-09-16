open Durin

(* TODO Add more examples here of parsing debug names. Look at C++ binaries
   for more interesting names. *)

let test_debug_str_entry_creation () =
  let entry =
    {
      Dwarf.DebugNames.offset = Unsigned.UInt32.of_int 0x100;
      Dwarf.DebugNames.value = "test_string";
    }
  in
  Alcotest.(check int)
    "offset matches" 0x100
    (Unsigned.UInt32.to_int entry.offset);
  Alcotest.(check string) "value matches" "test_string" entry.value

let test_calculate_entry_address () =
  let base_offset = Unsigned.UInt32.of_int 0x1000 in
  let relative_offset = 0x50 in
  let result =
    Dwarf.DebugNames.calculate_entry_address base_offset relative_offset
  in
  let expected = 0x1050 in
  Alcotest.(check int)
    "calculated address" expected
    (Unsigned.UInt32.to_int result)

let test_calculate_section_addresses () =
  let header =
    {
      Dwarf.DebugNames.unit_length = Unsigned.UInt32.of_int 0x98;
      version = Unsigned.UInt16.of_int 5;
      padding = Unsigned.UInt16.of_int 0;
      comp_unit_count = Unsigned.UInt32.of_int 1;
      local_type_unit_count = Unsigned.UInt32.of_int 0;
      foreign_type_unit_count = Unsigned.UInt32.of_int 0;
      bucket_count = Unsigned.UInt32.of_int 4;
      name_count = Unsigned.UInt32.of_int 4;
      abbrev_table_size = Unsigned.UInt32.of_int 0x11;
      augmentation_string_size = Unsigned.UInt32.of_int 8;
      augmentation_string = "LLVM0700";
    }
  in
  let base_offset = Unsigned.UInt32.of_int 0x2000 in
  let addresses =
    Dwarf.DebugNames.calculate_section_addresses base_offset header
  in

  (* Check that we get expected number of address entries *)
  Alcotest.(check bool) "has multiple addresses" true (List.length addresses > 3);

  (* Check that header address is the base offset *)
  let header_addr = List.assoc "header" addresses in
  Alcotest.(check int)
    "header address" 0x2000
    (Unsigned.UInt32.to_int header_addr);

  (* Check that comp_unit_offsets comes after header (header = 40 bytes) *)
  let cu_addr = List.assoc "comp_unit_offsets" addresses in
  Alcotest.(check int)
    "CU offsets address" (0x2000 + 40)
    (Unsigned.UInt32.to_int cu_addr)

let test_hash_consistency () =
  (* Test that the same string always produces the same hash *)
  let test_strings = [ "main"; "int"; "char"; "void"; "" ] in
  List.iter
    (fun s ->
      let hash1 = Dwarf.DebugNames.djb2_hash s in
      let hash2 = Dwarf.DebugNames.djb2_hash s in
      Alcotest.(check bool)
        ("hash consistency for '" ^ s ^ "'")
        true
        (Unsigned.UInt32.equal hash1 hash2))
    test_strings

let test_address_calculator_edge_cases () =
  (* Test with zero base offset *)
  let result1 =
    Dwarf.DebugNames.calculate_entry_address (Unsigned.UInt32.of_int 0) 100
  in
  Alcotest.(check int) "zero base offset" 100 (Unsigned.UInt32.to_int result1);

  (* Test with zero relative offset *)
  let result2 =
    Dwarf.DebugNames.calculate_entry_address (Unsigned.UInt32.of_int 1000) 0
  in
  Alcotest.(check int)
    "zero relative offset" 1000
    (Unsigned.UInt32.to_int result2);

  (* Test with both zero *)
  let result3 =
    Dwarf.DebugNames.calculate_entry_address (Unsigned.UInt32.of_int 0) 0
  in
  Alcotest.(check int) "both zero" 0 (Unsigned.UInt32.to_int result3)

let () =
  let tests =
    [
      ("debug_str_entry_creation", `Quick, test_debug_str_entry_creation);
      ("calculate_entry_address", `Quick, test_calculate_entry_address);
      ("calculate_section_addresses", `Quick, test_calculate_section_addresses);
      ("hash_consistency", `Quick, test_hash_consistency);
      ( "address_calculator_edge_cases",
        `Quick,
        test_address_calculator_edge_cases );
    ]
  in
  Alcotest.run "DebugNames_Parsing" [ ("parsing", tests) ]
