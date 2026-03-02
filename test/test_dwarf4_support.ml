open Alcotest
open Durin

let buffer_of_bytes bytes =
  let filename = Filename.temp_file "dwarf4_test_" ".bin" in
  let oc = open_out_bin filename in
  List.iter (fun b -> output_char oc (Char.chr b)) bytes;
  close_out oc;
  let buffer = Object.Buffer.parse filename in
  Sys.remove filename;
  buffer

let u16 v = Unsigned.UInt16.of_int v
let u8 v = Unsigned.UInt8.of_int v

(* ---- PR 1: Compile unit header tests ---- *)

let test_dwarf5_cu_header_still_works () =
  (* DWARF 5 CU header: DWARF32
     initial_length: 4 bytes (value=25)
     version: 2 bytes (5)
     unit_type: 1 byte (0x01 = DW_UT_compile)
     address_size: 1 byte (8)
     abbrev_offset: 4 bytes (0) *)
  let bytes =
    [
      (* initial_length: 25 in LE *)
      0x19;
      0x00;
      0x00;
      0x00;
      (* version: 5 *)
      0x05;
      0x00;
      (* unit_type: DW_UT_compile *)
      0x01;
      (* address_size: 8 *)
      0x08;
      (* debug_abbrev_offset: 0 *)
      0x00;
      0x00;
      0x00;
      0x00;
    ]
  in
  let buffer = buffer_of_bytes bytes in
  let cursor = Object.Buffer.cursor buffer ~at:0 in
  let _span, header = Dwarf.parse_compile_unit_header cursor in
  check int "version is 5" (Unsigned.UInt16.to_int header.version) 5;
  check int "address_size is 8" (Unsigned.UInt8.to_int header.address_size) 8;
  check int "unit_type is 0x01" (Unsigned.UInt8.to_int header.unit_type) 0x01;
  check int64 "abbrev_offset is 0"
    (Unsigned.UInt64.to_int64 header.debug_abbrev_offset)
    0L

let test_dwarf4_cu_header () =
  (* DWARF 4 CU header: DWARF32
     initial_length: 4 bytes (value=11)
     version: 2 bytes (4)
     abbrev_offset: 4 bytes (0x20)
     address_size: 1 byte (8) *)
  let bytes =
    [
      (* initial_length: 11 in LE *)
      0x0b;
      0x00;
      0x00;
      0x00;
      (* version: 4 *)
      0x04;
      0x00;
      (* debug_abbrev_offset: 0x20 *)
      0x20;
      0x00;
      0x00;
      0x00;
      (* address_size: 8 *)
      0x08;
    ]
  in
  let buffer = buffer_of_bytes bytes in
  let cursor = Object.Buffer.cursor buffer ~at:0 in
  let _span, header = Dwarf.parse_compile_unit_header cursor in
  check int "version is 4" (Unsigned.UInt16.to_int header.version) 4;
  check int "address_size is 8" (Unsigned.UInt8.to_int header.address_size) 8;
  check int "unit_type is 0x01 (synthetic DW_UT_compile)"
    (Unsigned.UInt8.to_int header.unit_type)
    0x01;
  check int64 "abbrev_offset is 0x20"
    (Unsigned.UInt64.to_int64 header.debug_abbrev_offset)
    0x20L

let test_dwarf4_cu_header_32bit () =
  (* DWARF 4 with 4-byte address size *)
  let bytes =
    [
      0x0b;
      0x00;
      0x00;
      0x00;
      (* initial_length: 11 *)
      0x04;
      0x00;
      (* version: 4 *)
      0x00;
      0x00;
      0x00;
      0x00;
      (* abbrev_offset: 0 *)
      0x04;
      (* address_size: 4 *)
    ]
  in
  let buffer = buffer_of_bytes bytes in
  let cursor = Object.Buffer.cursor buffer ~at:0 in
  let _span, header = Dwarf.parse_compile_unit_header cursor in
  check int "address_size is 4" (Unsigned.UInt8.to_int header.address_size) 4

let test_unsupported_version_rejected () =
  let bytes =
    [
      0x0b;
      0x00;
      0x00;
      0x00;
      (* initial_length *)
      0x03;
      0x00;
      (* version: 3 - unsupported *)
      0x00;
      0x00;
      0x00;
      0x00;
      0x08;
    ]
  in
  let buffer = buffer_of_bytes bytes in
  let cursor = Object.Buffer.cursor buffer ~at:0 in
  check_raises "version 3 rejected"
    (Failure "Unsupported DWARF version: 3 (only 4 and 5 supported)") (fun () ->
      ignore (Dwarf.parse_compile_unit_header cursor))

(* ---- PR 3: Section type tests ---- *)

let test_dwarf4_section_names () =
  check string "ELF debug_loc" ".debug_loc"
    (Dwarf.object_format_to_section_name Object_format.ELF Dwarf.Debug_loc);
  check string "ELF debug_ranges" ".debug_ranges"
    (Dwarf.object_format_to_section_name Object_format.ELF Dwarf.Debug_ranges);
  check string "ELF debug_pubnames" ".debug_pubnames"
    (Dwarf.object_format_to_section_name Object_format.ELF Dwarf.Debug_pubnames);
  check string "ELF debug_pubtypes" ".debug_pubtypes"
    (Dwarf.object_format_to_section_name Object_format.ELF Dwarf.Debug_pubtypes);
  check string "MachO debug_loc" "__debug_loc"
    (Dwarf.object_format_to_section_name Object_format.MACHO Dwarf.Debug_loc);
  check string "MachO debug_ranges" "__debug_ranges"
    (Dwarf.object_format_to_section_name Object_format.MACHO Dwarf.Debug_ranges)

(* ---- PR 4: DebugLoc tests ---- *)

let test_debug_loc_end_of_list () =
  (* 8-byte address: two zero addresses = end of list *)
  let bytes =
    [
      0x00;
      0x00;
      0x00;
      0x00;
      0x00;
      0x00;
      0x00;
      0x00;
      0x00;
      0x00;
      0x00;
      0x00;
      0x00;
      0x00;
      0x00;
      0x00;
    ]
  in
  let buffer = buffer_of_bytes bytes in
  let cursor = Object.Buffer.cursor buffer ~at:0 in
  let entries = Dwarf.DebugLoc.parse_list cursor 8 in
  check int "one entry" 1 (List.length entries);
  match entries with
  | [ Dwarf.DebugLoc.EndOfList ] -> ()
  | _ -> fail "expected EndOfList"

let test_debug_loc_base_address () =
  (* 8-byte: base address selection (max_addr, new_base)
     then end of list *)
  let bytes =
    [
      (* max addr for 8 bytes *)
      0xff;
      0xff;
      0xff;
      0xff;
      0xff;
      0xff;
      0xff;
      0xff;
      (* base address: 0x1000 *)
      0x00;
      0x10;
      0x00;
      0x00;
      0x00;
      0x00;
      0x00;
      0x00;
      (* end of list *)
      0x00;
      0x00;
      0x00;
      0x00;
      0x00;
      0x00;
      0x00;
      0x00;
      0x00;
      0x00;
      0x00;
      0x00;
      0x00;
      0x00;
      0x00;
      0x00;
    ]
  in
  let buffer = buffer_of_bytes bytes in
  let cursor = Object.Buffer.cursor buffer ~at:0 in
  let entries = Dwarf.DebugLoc.parse_list cursor 8 in
  check int "two entries" 2 (List.length entries);
  (match List.hd entries with
  | Dwarf.DebugLoc.BaseAddress addr ->
      check int64 "base addr is 0x1000" (Unsigned.UInt64.to_int64 addr) 0x1000L
  | _ -> fail "expected BaseAddress");
  match List.nth entries 1 with
  | Dwarf.DebugLoc.EndOfList -> ()
  | _ -> fail "expected EndOfList"

let test_debug_loc_location_entry () =
  (* 4-byte address: location entry
     begin=0x100, end=0x200, length=2, expr=[0xAB, 0xCD]
     then end of list *)
  let bytes =
    [
      (* begin: 0x100 *)
      0x00;
      0x01;
      0x00;
      0x00;
      (* end: 0x200 *)
      0x00;
      0x02;
      0x00;
      0x00;
      (* expr length: 2 *)
      0x02;
      0x00;
      (* expr bytes *)
      0xAB;
      0xCD;
      (* end of list *)
      0x00;
      0x00;
      0x00;
      0x00;
      0x00;
      0x00;
      0x00;
      0x00;
    ]
  in
  let buffer = buffer_of_bytes bytes in
  let cursor = Object.Buffer.cursor buffer ~at:0 in
  let entries = Dwarf.DebugLoc.parse_list cursor 4 in
  check int "two entries" 2 (List.length entries);
  (match List.hd entries with
  | Dwarf.DebugLoc.Location { begin_addr; end_addr; expr } ->
      check int64 "begin 0x100" (Unsigned.UInt64.to_int64 begin_addr) 0x100L;
      check int64 "end 0x200" (Unsigned.UInt64.to_int64 end_addr) 0x200L;
      check int "expr len 2" 2 (String.length expr)
  | _ -> fail "expected Location");
  match List.nth entries 1 with
  | Dwarf.DebugLoc.EndOfList -> ()
  | _ -> fail "expected EndOfList"

(* ---- PR 4: DebugRanges tests ---- *)

let test_debug_ranges_end_of_list () =
  let bytes =
    [
      0x00;
      0x00;
      0x00;
      0x00;
      0x00;
      0x00;
      0x00;
      0x00;
      0x00;
      0x00;
      0x00;
      0x00;
      0x00;
      0x00;
      0x00;
      0x00;
    ]
  in
  let buffer = buffer_of_bytes bytes in
  let cursor = Object.Buffer.cursor buffer ~at:0 in
  let entries = Dwarf.DebugRanges.parse_list cursor 8 in
  check int "one entry" 1 (List.length entries);
  match entries with
  | [ Dwarf.DebugRanges.EndOfList ] -> ()
  | _ -> fail "expected EndOfList"

let test_debug_ranges_range_entry () =
  (* 4-byte: range 0x100..0x200, then end *)
  let bytes =
    [
      0x00;
      0x01;
      0x00;
      0x00;
      0x00;
      0x02;
      0x00;
      0x00;
      0x00;
      0x00;
      0x00;
      0x00;
      0x00;
      0x00;
      0x00;
      0x00;
    ]
  in
  let buffer = buffer_of_bytes bytes in
  let cursor = Object.Buffer.cursor buffer ~at:0 in
  let entries = Dwarf.DebugRanges.parse_list cursor 4 in
  check int "two entries" 2 (List.length entries);
  (match List.hd entries with
  | Dwarf.DebugRanges.Range { begin_addr; end_addr } ->
      check int64 "begin 0x100" (Unsigned.UInt64.to_int64 begin_addr) 0x100L;
      check int64 "end 0x200" (Unsigned.UInt64.to_int64 end_addr) 0x200L
  | _ -> fail "expected Range");
  match List.nth entries 1 with
  | Dwarf.DebugRanges.EndOfList -> ()
  | _ -> fail "expected EndOfList"

let test_debug_ranges_base_address_32bit () =
  (* 4-byte: base address selection, then range, then end *)
  let bytes =
    [
      (* max addr for 4 bytes *)
      0xff;
      0xff;
      0xff;
      0xff;
      (* base: 0x8000 *)
      0x00;
      0x80;
      0x00;
      0x00;
      (* range: 0x10..0x20 *)
      0x10;
      0x00;
      0x00;
      0x00;
      0x20;
      0x00;
      0x00;
      0x00;
      (* end *)
      0x00;
      0x00;
      0x00;
      0x00;
      0x00;
      0x00;
      0x00;
      0x00;
    ]
  in
  let buffer = buffer_of_bytes bytes in
  let cursor = Object.Buffer.cursor buffer ~at:0 in
  let entries = Dwarf.DebugRanges.parse_list cursor 4 in
  check int "three entries" 3 (List.length entries);
  (match List.hd entries with
  | Dwarf.DebugRanges.BaseAddress addr ->
      check int64 "base 0x8000" (Unsigned.UInt64.to_int64 addr) 0x8000L
  | _ -> fail "expected BaseAddress");
  (match List.nth entries 1 with
  | Dwarf.DebugRanges.Range { begin_addr; end_addr } ->
      check int64 "begin 0x10" (Unsigned.UInt64.to_int64 begin_addr) 0x10L;
      check int64 "end 0x20" (Unsigned.UInt64.to_int64 end_addr) 0x20L
  | _ -> fail "expected Range");
  match List.nth entries 2 with
  | Dwarf.DebugRanges.EndOfList -> ()
  | _ -> fail "expected EndOfList"

(* ---- PR 5: Form handling tests ---- *)

let test_dw_form_ref_addr () =
  (* DWARF32: ref_addr is 4 bytes *)
  let bytes = [ 0x78; 0x56; 0x34; 0x12 ] in
  let buffer = buffer_of_bytes bytes in
  let cursor = Object.Buffer.cursor buffer ~at:0 in
  let encoding : Dwarf.encoding =
    { format = Dwarf.DWARF32; address_size = u8 8; version = u16 4 }
  in
  let value =
    Dwarf.DIE.parse_attribute_value cursor Dwarf.DW_FORM_ref_addr encoding
      buffer
  in
  match value with
  | Dwarf.DIE.Reference offset ->
      check int64 "ref_addr offset"
        (Unsigned.UInt64.to_int64 offset)
        0x12345678L
  | _ -> fail "expected Reference"

let test_dw_form_indirect () =
  (* DW_FORM_indirect: uleb128 form code,
     then the actual data.
     form code 0x08 = DW_FORM_data4,
     then 4 bytes of data *)
  let bytes =
    [
      0x08;
      (* uleb128: DW_FORM_data4 = 0x08 -- but wait *)
      (* Actually DW_FORM_data4 is 0x06 in DWARF *)
      (* Let me use DW_FORM_data1 = 0x0b,
         value = 0x42 *)
    ]
  in
  ignore bytes;
  (* DW_FORM_data1 = 0x0b, then one byte of data *)
  let bytes = [ 0x0b; 0x42 ] in
  let buffer = buffer_of_bytes bytes in
  let cursor = Object.Buffer.cursor buffer ~at:0 in
  let encoding : Dwarf.encoding =
    { format = Dwarf.DWARF32; address_size = u8 8; version = u16 4 }
  in
  let value =
    Dwarf.DIE.parse_attribute_value cursor Dwarf.DW_FORM_indirect encoding
      buffer
  in
  match value with
  | Dwarf.DIE.UData v ->
      check int64 "indirect data1 value" (Unsigned.UInt64.to_int64 v) 0x42L
  | _ -> fail "expected UData"

(* ---- DWARF 4 line table header test ---- *)

let test_dwarf4_line_table_header () =
  (* Construct a minimal DWARF 4 line table header:
     - initial_length (DWARF32): 4 bytes
     - version: 2 bytes (4)
     - header_length: 4 bytes
     - min_inst_length: 1 byte (1)
     - max_ops_per_inst: 1 byte (1)
     - default_is_stmt: 1 byte (1)
     - line_base: 1 byte (0xfb = -5 signed)
     - line_range: 1 byte (14)
     - opcode_base: 1 byte (13)
     - std_opcode_lengths: 12 bytes
     - directories: "mydir\0" then "\0" (end)
     - files: "test.c\0" dir_idx=1 mtime=0 len=0
       then "\0" (end) *)
  let std_opcodes = [ 0; 1; 1; 1; 1; 0; 0; 0; 1; 0; 0; 1 ] in
  let dir_bytes =
    List.map Char.code (List.of_seq (String.to_seq "mydir")) @ [ 0; 0 ]
  in
  let file_bytes =
    List.map Char.code (List.of_seq (String.to_seq "test.c"))
    @ [
        0;
        (* null terminator for name *)
        1;
        (* dir_idx = 1 (uleb128) *)
        0;
        (* mtime = 0 (uleb128) *)
        0;
        (* file length = 0 (uleb128) *)
      ]
    @ [ 0 ]
  in
  let header_content =
    [
      1;
      (* min_inst_length *)
      1;
      (* max_ops_per_inst *)
      1;
      (* default_is_stmt *)
      0xfb;
      (* line_base: -5 as unsigned byte *)
      14;
      (* line_range *)
      13;
      (* opcode_base *)
    ]
    @ std_opcodes @ dir_bytes @ file_bytes
  in
  let header_length = List.length header_content in
  (* unit_length = 2 (version) + 4 (header_length field)
     + header_content_length *)
  let unit_length = 2 + 4 + header_length in
  let bytes =
    [
      (* initial_length LE *)
      unit_length land 0xff;
      (unit_length lsr 8) land 0xff;
      (unit_length lsr 16) land 0xff;
      (unit_length lsr 24) land 0xff;
      (* version: 4 *)
      0x04;
      0x00;
      (* header_length LE *)
      header_length land 0xff;
      (header_length lsr 8) land 0xff;
      (header_length lsr 16) land 0xff;
      (header_length lsr 24) land 0xff;
    ]
    @ header_content
  in
  let buffer = buffer_of_bytes bytes in
  let cursor = Object.Buffer.cursor buffer ~at:0 in
  let header = Dwarf.LineTable.parse_line_program_header cursor buffer in
  check int "version is 4" (Unsigned.UInt16.to_int header.version) 4;
  check int "min_inst_length is 1"
    (Unsigned.UInt8.to_int header.minimum_instruction_length)
    1;
  check bool "default_is_stmt" true header.default_is_stmt;
  check int "line_base is -5" (-5) header.line_base;
  check int "line_range is 14" (Unsigned.UInt8.to_int header.line_range) 14;
  check int "opcode_base is 13" (Unsigned.UInt8.to_int header.opcode_base) 13;
  check int "1 directory" 1 (Array.length header.directories);
  check string "directory is mydir" "mydir" header.directories.(0);
  check int "1 file" 1 (Array.length header.file_names);
  check string "file name" "test.c" header.file_names.(0).name;
  check string "file directory" "mydir" header.file_names.(0).directory

let () =
  run "DWARF 4 Support"
    [
      ( "compile_unit_header",
        [
          test_case "DWARF 5 CU header still works" `Quick
            test_dwarf5_cu_header_still_works;
          test_case "DWARF 4 CU header" `Quick test_dwarf4_cu_header;
          test_case "DWARF 4 CU header 32-bit" `Quick
            test_dwarf4_cu_header_32bit;
          test_case "unsupported version rejected" `Quick
            test_unsupported_version_rejected;
        ] );
      ( "section_types",
        [ test_case "DWARF 4 section names" `Quick test_dwarf4_section_names ]
      );
      ( "debug_loc",
        [
          test_case "end of list" `Quick test_debug_loc_end_of_list;
          test_case "base address" `Quick test_debug_loc_base_address;
          test_case "location entry" `Quick test_debug_loc_location_entry;
        ] );
      ( "debug_ranges",
        [
          test_case "end of list" `Quick test_debug_ranges_end_of_list;
          test_case "range entry" `Quick test_debug_ranges_range_entry;
          test_case "base address 32-bit" `Quick
            test_debug_ranges_base_address_32bit;
        ] );
      ( "form_handling",
        [
          test_case "DW_FORM_ref_addr" `Quick test_dw_form_ref_addr;
          test_case "DW_FORM_indirect" `Quick test_dw_form_indirect;
        ] );
      ( "line_table",
        [
          test_case "DWARF 4 line table header" `Quick
            test_dwarf4_line_table_header;
        ] );
    ]
