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

(* Compile unit header tests *)

let test_dwarf5_cu_header_still_works () =
  let bytes =
    [ 0x19; 0x00; 0x00; 0x00; 0x05; 0x00; 0x01; 0x08; 0x00; 0x00; 0x00; 0x00 ]
  in
  let buffer = buffer_of_bytes bytes in
  let cursor = Object.Buffer.cursor buffer ~at:0 in
  let _span, header = Dwarf.parse_compile_unit_header cursor in
  check int "version is 5" 5 (Unsigned.UInt16.to_int header.version);
  check int "address_size is 8" 8 (Unsigned.UInt8.to_int header.address_size);
  check int "unit_type is 0x01" 0x01 (Unsigned.UInt8.to_int header.unit_type);
  check int64 "abbrev_offset is 0" 0L
    (Unsigned.UInt64.to_int64 header.debug_abbrev_offset)

let test_dwarf4_cu_header () =
  let bytes =
    [ 0x0b; 0x00; 0x00; 0x00; 0x04; 0x00; 0x20; 0x00; 0x00; 0x00; 0x08 ]
  in
  let buffer = buffer_of_bytes bytes in
  let cursor = Object.Buffer.cursor buffer ~at:0 in
  let _span, header = Dwarf.parse_compile_unit_header cursor in
  check int "version is 4" 4 (Unsigned.UInt16.to_int header.version);
  check int "address_size is 8" 8 (Unsigned.UInt8.to_int header.address_size);
  check int "unit_type is 0x01 (synthetic DW_UT_compile)" 0x01
    (Unsigned.UInt8.to_int header.unit_type);
  check int64 "abbrev_offset is 0x20" 0x20L
    (Unsigned.UInt64.to_int64 header.debug_abbrev_offset)

let test_dwarf4_cu_header_32bit () =
  let bytes =
    [ 0x0b; 0x00; 0x00; 0x00; 0x04; 0x00; 0x00; 0x00; 0x00; 0x00; 0x04 ]
  in
  let buffer = buffer_of_bytes bytes in
  let cursor = Object.Buffer.cursor buffer ~at:0 in
  let _span, header = Dwarf.parse_compile_unit_header cursor in
  check int "address_size is 4" 4 (Unsigned.UInt8.to_int header.address_size)

let test_unsupported_version_rejected () =
  let bytes =
    [ 0x0b; 0x00; 0x00; 0x00; 0x03; 0x00; 0x00; 0x00; 0x00; 0x00; 0x08 ]
  in
  let buffer = buffer_of_bytes bytes in
  let cursor = Object.Buffer.cursor buffer ~at:0 in
  check_raises "version 3 rejected"
    (Failure "Unsupported DWARF version: 3 (only 4 and 5 supported)") (fun () ->
      ignore (Dwarf.parse_compile_unit_header cursor))

(* ---- Non-compile unit type tests ---- *)

let test_dwarf5_type_unit_header () =
  let bytes =
    [
      0x1d;
      0x00;
      0x00;
      0x00;
      0x05;
      0x00;
      0x02;
      0x08;
      0x00;
      0x00;
      0x00;
      0x00;
      0xBE;
      0xBA;
      0xFE;
      0xCA;
      0xEF;
      0xBE;
      0xAD;
      0xDE;
      0x20;
      0x00;
      0x00;
      0x00;
    ]
  in
  let buffer = buffer_of_bytes bytes in
  let cursor = Object.Buffer.cursor buffer ~at:0 in
  let _span, header = Dwarf.parse_compile_unit_header cursor in
  check int "unit_type is 0x02" 0x02 (Unsigned.UInt8.to_int header.unit_type);
  check int "version is 5" 5 (Unsigned.UInt16.to_int header.version);
  (match header.type_signature with
  | Some sig8 ->
      check int64 "type_signature"
        (Int64.of_string "-0x2152411035014542")
        (Unsigned.UInt64.to_int64 sig8)
  | None -> fail "expected type_signature");
  (match header.type_offset with
  | Some toff ->
      check int64 "type_offset 0x20" 0x20L (Unsigned.UInt64.to_int64 toff)
  | None -> fail "expected type_offset");
  check bool "no dwo_id" true (header.dwo_id = None)

let test_dwarf5_partial_unit_header () =
  let bytes =
    [ 0x19; 0x00; 0x00; 0x00; 0x05; 0x00; 0x03; 0x08; 0x00; 0x00; 0x00; 0x00 ]
  in
  let buffer = buffer_of_bytes bytes in
  let cursor = Object.Buffer.cursor buffer ~at:0 in
  let _span, header = Dwarf.parse_compile_unit_header cursor in
  check int "unit_type is 0x03" 0x03 (Unsigned.UInt8.to_int header.unit_type);
  check bool "no type_signature" true (header.type_signature = None);
  check bool "no dwo_id" true (header.dwo_id = None)

let test_dwarf5_skeleton_unit_header () =
  let bytes =
    [
      0x19;
      0x00;
      0x00;
      0x00;
      0x05;
      0x00;
      0x04;
      0x08;
      0x00;
      0x00;
      0x00;
      0x00;
      0x08;
      0x07;
      0x06;
      0x05;
      0x04;
      0x03;
      0x02;
      0x01;
    ]
  in
  let buffer = buffer_of_bytes bytes in
  let cursor = Object.Buffer.cursor buffer ~at:0 in
  let _span, header = Dwarf.parse_compile_unit_header cursor in
  check int "unit_type is 0x04" 0x04 (Unsigned.UInt8.to_int header.unit_type);
  (match header.dwo_id with
  | Some id ->
      check int64 "dwo_id"
        (Int64.of_string "0x0102030405060708")
        (Unsigned.UInt64.to_int64 id)
  | None -> fail "expected dwo_id");
  check bool "no type_signature" true (header.type_signature = None)

let test_dwarf5_split_compile_unit_header () =
  let bytes =
    [
      0x19;
      0x00;
      0x00;
      0x00;
      0x05;
      0x00;
      0x05;
      0x04;
      0x10;
      0x00;
      0x00;
      0x00;
      0xAA;
      0xBB;
      0xCC;
      0xDD;
      0xEE;
      0xFF;
      0x00;
      0x11;
    ]
  in
  let buffer = buffer_of_bytes bytes in
  let cursor = Object.Buffer.cursor buffer ~at:0 in
  let _span, header = Dwarf.parse_compile_unit_header cursor in
  check int "unit_type is 0x05" 0x05 (Unsigned.UInt8.to_int header.unit_type);
  check int "address_size is 4" 4 (Unsigned.UInt8.to_int header.address_size);
  check int64 "abbrev_offset 0x10" 0x10L
    (Unsigned.UInt64.to_int64 header.debug_abbrev_offset);
  match header.dwo_id with
  | Some id ->
      check int64 "dwo_id"
        (Int64.of_string "0x1100FFEEDDCCBBAA")
        (Unsigned.UInt64.to_int64 id)
  | None -> fail "expected dwo_id"

(* ---- Section type tests ---- *)

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

(* ---- Form handling tests ---- *)

let test_dw_form_ref_addr () =
  let bytes = [ 0x78; 0x56; 0x34; 0x12 ] in
  let buffer = buffer_of_bytes bytes in
  let cursor = Object.Buffer.cursor buffer ~at:0 in
  let encoding : Dwarf.encoding =
    { format = Dwarf.DWARF32; address_size = u8 8; version = u16 4 }
  in
  let value =
    Dwarf.DIE.parse_attribute_value cursor Dwarf.DW_FORM_ref_addr encoding
      buffer ()
  in
  match value with
  | Dwarf.DIE.Reference offset ->
      check int64 "ref_addr offset" 0x12345678L
        (Unsigned.UInt64.to_int64 offset)
  | _ -> fail "expected Reference"

let test_dw_form_indirect () =
  let bytes = [ 0x0b; 0x42 ] in
  let buffer = buffer_of_bytes bytes in
  let cursor = Object.Buffer.cursor buffer ~at:0 in
  let encoding : Dwarf.encoding =
    { format = Dwarf.DWARF32; address_size = u8 8; version = u16 4 }
  in
  let value =
    Dwarf.DIE.parse_attribute_value cursor Dwarf.DW_FORM_indirect encoding
      buffer ()
  in
  match value with
  | Dwarf.DIE.UData v ->
      check int64 "indirect data1 value" 0x42L (Unsigned.UInt64.to_int64 v)
  | _ -> fail "expected UData"

(* ---- DWARF 4 line table header test ---- *)

let test_dwarf4_line_table_header () =
  let std_opcodes = [ 0; 1; 1; 1; 1; 0; 0; 0; 1; 0; 0; 1 ] in
  let dir_bytes =
    List.map Char.code (List.of_seq (String.to_seq "mydir")) @ [ 0; 0 ]
  in
  let file_bytes =
    List.map Char.code (List.of_seq (String.to_seq "test.c"))
    @ [ 0; 1; 0; 0 ] @ [ 0 ]
  in
  let header_content =
    [ 1; 1; 1; 0xfb; 14; 13 ] @ std_opcodes @ dir_bytes @ file_bytes
  in
  let header_length = List.length header_content in
  let unit_length = 2 + 4 + header_length in
  let bytes =
    [
      unit_length land 0xff;
      (unit_length lsr 8) land 0xff;
      (unit_length lsr 16) land 0xff;
      (unit_length lsr 24) land 0xff;
      0x04;
      0x00;
      header_length land 0xff;
      (header_length lsr 8) land 0xff;
      (header_length lsr 16) land 0xff;
      (header_length lsr 24) land 0xff;
    ]
    @ header_content
  in
  let buffer = buffer_of_bytes bytes in
  let cursor = Object.Buffer.cursor buffer ~at:0 in
  let header = Dwarf.DebugLine.parse_line_program_header cursor buffer in
  check int "version is 4" 4 (Unsigned.UInt16.to_int header.version);
  check int "min_inst_length is 1" 1
    (Unsigned.UInt8.to_int header.minimum_instruction_length);
  check bool "default_is_stmt" true header.default_is_stmt;
  check int "line_base is -5" (-5) header.line_base;
  check int "line_range is 14" 14 (Unsigned.UInt8.to_int header.line_range);
  check int "opcode_base is 13" 13 (Unsigned.UInt8.to_int header.opcode_base);
  check int "1 directory" 1 (Array.length header.directories);
  check string "directory is mydir" "mydir" header.directories.(0);
  check int "1 file" 1 (Array.length header.file_names);
  check string "file name" "test.c" header.file_names.(0).name;
  check string "file directory" "mydir" header.file_names.(0).directory

(* ---- DW_FORM_addr with 4-byte address ---- *)

let test_dw_form_addr_4byte () =
  let bytes = [ 0x78; 0x56; 0x34; 0x12 ] in
  let buffer = buffer_of_bytes bytes in
  let cursor = Object.Buffer.cursor buffer ~at:0 in
  let encoding : Dwarf.encoding =
    { format = Dwarf.DWARF32; address_size = u8 4; version = u16 4 }
  in
  let value =
    Dwarf.DIE.parse_attribute_value cursor Dwarf.DW_FORM_addr encoding buffer ()
  in
  match value with
  | Dwarf.DIE.Address addr ->
      check int64 "addr 0x12345678" 0x12345678L (Unsigned.UInt64.to_int64 addr)
  | _ -> fail "expected Address"

let test_dw_form_addr_8byte () =
  let bytes = [ 0xf0; 0xde; 0xbc; 0x9a; 0x78; 0x56; 0x34; 0x12 ] in
  let buffer = buffer_of_bytes bytes in
  let cursor = Object.Buffer.cursor buffer ~at:0 in
  let encoding : Dwarf.encoding =
    { format = Dwarf.DWARF32; address_size = u8 8; version = u16 5 }
  in
  let value =
    Dwarf.DIE.parse_attribute_value cursor Dwarf.DW_FORM_addr encoding buffer ()
  in
  match value with
  | Dwarf.DIE.Address addr ->
      check int64 "addr 8-byte"
        (Int64.of_string "0x123456789abcdef0")
        (Unsigned.UInt64.to_int64 addr)
  | _ -> fail "expected Address"

(* ---- DW_FORM_implicit_const test ---- *)

let test_dw_form_implicit_const () =
  let buffer = buffer_of_bytes [ 0x00 ] in
  let cursor = Object.Buffer.cursor buffer ~at:0 in
  let encoding : Dwarf.encoding =
    { format = Dwarf.DWARF32; address_size = u8 8; version = u16 5 }
  in
  let value =
    Dwarf.DIE.parse_attribute_value cursor Dwarf.DW_FORM_implicit_const encoding
      buffer ~implicit_const:42L ()
  in
  match value with
  | Dwarf.DIE.SData v ->
      check int64 "implicit_const value" 42L (Signed.Int64.to_int64 v)
  | _ -> fail "expected SData"

let test_dw_form_implicit_const_negative () =
  let buffer = buffer_of_bytes [ 0x00 ] in
  let cursor = Object.Buffer.cursor buffer ~at:0 in
  let encoding : Dwarf.encoding =
    { format = Dwarf.DWARF32; address_size = u8 8; version = u16 5 }
  in
  let value =
    Dwarf.DIE.parse_attribute_value cursor Dwarf.DW_FORM_implicit_const encoding
      buffer ~implicit_const:(-5L) ()
  in
  match value with
  | Dwarf.DIE.SData v ->
      check int64 "negative implicit_const" (-5L) (Signed.Int64.to_int64 v)
  | _ -> fail "expected SData"

(* ---- DW_FORM_loclistx / rnglistx / ref_udata / ref_sig8 ---- *)

let test_dw_form_loclistx () =
  let bytes = [ 0x03 ] in
  let buffer = buffer_of_bytes bytes in
  let cursor = Object.Buffer.cursor buffer ~at:0 in
  let encoding : Dwarf.encoding =
    { format = Dwarf.DWARF32; address_size = u8 8; version = u16 5 }
  in
  let value =
    Dwarf.DIE.parse_attribute_value cursor Dwarf.DW_FORM_loclistx encoding
      buffer ()
  in
  match value with
  | Dwarf.DIE.UData v ->
      check int64 "loclistx index 3" 3L (Unsigned.UInt64.to_int64 v)
  | _ -> fail "expected UData"

let test_dw_form_rnglistx () =
  let bytes = [ 0x07 ] in
  let buffer = buffer_of_bytes bytes in
  let cursor = Object.Buffer.cursor buffer ~at:0 in
  let encoding : Dwarf.encoding =
    { format = Dwarf.DWARF32; address_size = u8 8; version = u16 5 }
  in
  let value =
    Dwarf.DIE.parse_attribute_value cursor Dwarf.DW_FORM_rnglistx encoding
      buffer ()
  in
  match value with
  | Dwarf.DIE.UData v ->
      check int64 "rnglistx index 7" 7L (Unsigned.UInt64.to_int64 v)
  | _ -> fail "expected UData"

let test_dw_form_ref_udata () =
  let bytes = [ 0x80; 0x01 ] in
  let buffer = buffer_of_bytes bytes in
  let cursor = Object.Buffer.cursor buffer ~at:0 in
  let encoding : Dwarf.encoding =
    { format = Dwarf.DWARF32; address_size = u8 8; version = u16 5 }
  in
  let value =
    Dwarf.DIE.parse_attribute_value cursor Dwarf.DW_FORM_ref_udata encoding
      buffer ()
  in
  match value with
  | Dwarf.DIE.Reference v ->
      check int64 "ref_udata 128" 128L (Unsigned.UInt64.to_int64 v)
  | _ -> fail "expected Reference"

let test_dw_form_ref_sig8 () =
  let bytes = [ 0xBE; 0xBA; 0xFE; 0xCA; 0xEF; 0xBE; 0xAD; 0xDE ] in
  let buffer = buffer_of_bytes bytes in
  let cursor = Object.Buffer.cursor buffer ~at:0 in
  let encoding : Dwarf.encoding =
    { format = Dwarf.DWARF32; address_size = u8 8; version = u16 5 }
  in
  let value =
    Dwarf.DIE.parse_attribute_value cursor Dwarf.DW_FORM_ref_sig8 encoding
      buffer ()
  in
  match value with
  | Dwarf.DIE.Reference v ->
      check int64 "ref_sig8"
        (Int64.of_string "-0x2152411035014542")
        (Unsigned.UInt64.to_int64 v)
  | _ -> fail "expected Reference"

let test_resolve_functions_exist () =
  check bool "resolve_location_list exists" true
    (match Dwarf.resolve_location_list with _ -> true);
  check bool "resolve_range_list exists" true
    (match Dwarf.resolve_range_list with _ -> true);
  check bool "parse_type_units exists" true
    (match Dwarf.DebugTypes.parse_type_units with _ -> true)

let () =
  run "debug_info"
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
          test_case "DWARF 5 type unit" `Quick test_dwarf5_type_unit_header;
          test_case "DWARF 5 partial unit" `Quick
            test_dwarf5_partial_unit_header;
          test_case "DWARF 5 skeleton unit" `Quick
            test_dwarf5_skeleton_unit_header;
          test_case "DWARF 5 split compile unit" `Quick
            test_dwarf5_split_compile_unit_header;
        ] );
      ( "section_types",
        [ test_case "DWARF 4 section names" `Quick test_dwarf4_section_names ]
      );
      ( "form_handling",
        [
          test_case "DW_FORM_ref_addr" `Quick test_dw_form_ref_addr;
          test_case "DW_FORM_indirect" `Quick test_dw_form_indirect;
          test_case "DW_FORM_addr 4-byte" `Quick test_dw_form_addr_4byte;
          test_case "DW_FORM_addr 8-byte" `Quick test_dw_form_addr_8byte;
          test_case "DW_FORM_implicit_const" `Quick test_dw_form_implicit_const;
          test_case "DW_FORM_implicit_const negative" `Quick
            test_dw_form_implicit_const_negative;
          test_case "DW_FORM_loclistx" `Quick test_dw_form_loclistx;
          test_case "DW_FORM_rnglistx" `Quick test_dw_form_rnglistx;
          test_case "DW_FORM_ref_udata" `Quick test_dw_form_ref_udata;
          test_case "DW_FORM_ref_sig8" `Quick test_dw_form_ref_sig8;
        ] );
      ( "line_table",
        [
          test_case "DWARF 4 line table header" `Quick
            test_dwarf4_line_table_header;
        ] );
      ( "resolution",
        [
          test_case "resolve functions exist" `Quick
            test_resolve_functions_exist;
        ] );
    ]
