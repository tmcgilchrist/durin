open Alcotest
open Durin

let buffer_of_bytes bytes =
  let filename = Filename.temp_file "debug_types_test_" ".bin" in
  let oc = open_out_bin filename in
  List.iter (fun b -> output_char oc (Char.chr b)) bytes;
  close_out oc;
  let buffer = Object.Buffer.parse filename in
  Sys.remove filename;
  buffer

let test_debug_types_header () =
  let bytes =
    [
      0x17;
      0x00;
      0x00;
      0x00;
      0x04;
      0x00;
      0x30;
      0x00;
      0x00;
      0x00;
      0x08;
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
  let _span, header = Dwarf.DebugTypes.parse_type_unit_header cursor in
  check int "version is 4" (Unsigned.UInt16.to_int header.version) 4;
  check int64 "abbrev_offset is 0x30"
    (Unsigned.UInt64.to_int64 header.debug_abbrev_offset)
    0x30L;
  check int "address_size is 8" (Unsigned.UInt8.to_int header.address_size) 8;
  check int64 "type_signature"
    (Unsigned.UInt64.to_int64 header.type_signature)
    (Int64.of_string "-0x2152411035014542");
  check int64 "type_offset is 0x20"
    (Unsigned.UInt64.to_int64 header.type_offset)
    0x20L

let test_debug_types_section_name () =
  check string "ELF debug_types" ".debug_types"
    (Dwarf.object_format_to_section_name Object_format.ELF Dwarf.Debug_types);
  check string "MachO debug_types" "__debug_types"
    (Dwarf.object_format_to_section_name Object_format.MACHO Dwarf.Debug_types)

let test_parse_type_units_no_section () =
  let bytes = [ 0x00; 0x00; 0x00; 0x00 ] in
  let buffer = buffer_of_bytes bytes in
  let units =
    try Dwarf.DebugTypes.parse_type_units buffer with Failure _ -> Seq.empty
  in
  let count = Seq.fold_left (fun acc _ -> acc + 1) 0 units in
  check int "no type units in non-ELF buffer" 0 count

let () =
  run "debug_types"
    [
      ( "debug_types",
        [
          test_case "type unit header" `Quick test_debug_types_header;
          test_case "section name" `Quick test_debug_types_section_name;
          test_case "parse_type_units no section" `Quick
            test_parse_type_units_no_section;
        ] );
    ]
