open Alcotest
open Durin

let buffer_of_bytes bytes =
  let filename = Filename.temp_file "debug_aranges_test_" ".bin" in
  let oc = open_out_bin filename in
  List.iter (fun b -> output_char oc (Char.chr b)) bytes;
  close_out oc;
  let buffer = Object.Buffer.parse filename in
  Sys.remove filename;
  buffer

(* ---- DebugAranges type tests ---- *)

let test_aranges_parse_no_section () =
  let bytes = [ 0x00; 0x00; 0x00; 0x00 ] in
  let buffer = buffer_of_bytes bytes in
  let result = try Dwarf.DebugAranges.parse buffer with Failure _ -> None in
  check bool "returns None when no .debug_aranges section" true (result = None)

let test_aranges_section_name () =
  check string "ELF debug_aranges" ".debug_aranges"
    (Dwarf.object_format_to_section_name Object_format.ELF Dwarf.Debug_aranges);
  check string "MachO debug_aranges" "__debug_aranges"
    (Dwarf.object_format_to_section_name Object_format.MACHO Dwarf.Debug_aranges)

(* Test address_range record construction *)
let test_aranges_address_range_record () =
  let range : Dwarf.DebugAranges.address_range =
    {
      start_address = Unsigned.UInt64.of_int 0x1000;
      length = Unsigned.UInt64.of_int 0x200;
    }
  in
  check int64 "start_address"
    (Unsigned.UInt64.to_int64 range.start_address)
    0x1000L;
  check int64 "length" (Unsigned.UInt64.to_int64 range.length) 0x200L

(* Test header record construction *)
let test_aranges_header_record () =
  let header : Dwarf.DebugAranges.header =
    {
      format = Dwarf.DWARF32;
      unit_length = Unsigned.UInt64.of_int 44;
      version = Unsigned.UInt16.of_int 2;
      debug_info_offset = Unsigned.UInt64.of_int 0;
      address_size = Unsigned.UInt8.of_int 8;
      segment_size = Unsigned.UInt8.of_int 0;
      header_span =
        { start = Unsigned.UInt64.of_int 0; size = Unsigned.UInt64.of_int 12 };
    }
  in
  check int "version is 2" (Unsigned.UInt16.to_int header.version) 2;
  check int "address_size is 8" (Unsigned.UInt8.to_int header.address_size) 8;
  check int "segment_size is 0" (Unsigned.UInt8.to_int header.segment_size) 0

(* Test aranges_set record construction *)
let test_aranges_set_record () =
  let header : Dwarf.DebugAranges.header =
    {
      format = Dwarf.DWARF32;
      unit_length = Unsigned.UInt64.of_int 44;
      version = Unsigned.UInt16.of_int 2;
      debug_info_offset = Unsigned.UInt64.of_int 0x0c;
      address_size = Unsigned.UInt8.of_int 8;
      segment_size = Unsigned.UInt8.of_int 0;
      header_span =
        { start = Unsigned.UInt64.of_int 0; size = Unsigned.UInt64.of_int 12 };
    }
  in
  let ranges : Dwarf.DebugAranges.address_range list =
    [
      {
        start_address = Unsigned.UInt64.of_int 0x1149;
        length = Unsigned.UInt64.of_int 0x1e;
      };
    ]
  in
  let set : Dwarf.DebugAranges.aranges_set = { header; ranges } in
  check int "one range" 1 (List.length set.ranges);
  let r = List.hd set.ranges in
  check int64 "start 0x1149" (Unsigned.UInt64.to_int64 r.start_address) 0x1149L;
  check int64 "length 0x1e" (Unsigned.UInt64.to_int64 r.length) 0x1eL;
  check int64 "debug_info_offset 0x0c"
    (Unsigned.UInt64.to_int64 set.header.debug_info_offset)
    0x0cL

(* Test multiple ranges in a set *)
let test_aranges_multiple_ranges () =
  let header : Dwarf.DebugAranges.header =
    {
      format = Dwarf.DWARF32;
      unit_length = Unsigned.UInt64.of_int 60;
      version = Unsigned.UInt16.of_int 2;
      debug_info_offset = Unsigned.UInt64.of_int 0;
      address_size = Unsigned.UInt8.of_int 4;
      segment_size = Unsigned.UInt8.of_int 0;
      header_span =
        { start = Unsigned.UInt64.of_int 0; size = Unsigned.UInt64.of_int 12 };
    }
  in
  let ranges : Dwarf.DebugAranges.address_range list =
    [
      {
        start_address = Unsigned.UInt64.of_int 0x1000;
        length = Unsigned.UInt64.of_int 0x100;
      };
      {
        start_address = Unsigned.UInt64.of_int 0x2000;
        length = Unsigned.UInt64.of_int 0x200;
      };
      {
        start_address = Unsigned.UInt64.of_int 0x4000;
        length = Unsigned.UInt64.of_int 0x50;
      };
    ]
  in
  let set : Dwarf.DebugAranges.aranges_set = { header; ranges } in
  check int "three ranges" 3 (List.length set.ranges);
  let r2 = List.nth set.ranges 1 in
  check int64 "second start" (Unsigned.UInt64.to_int64 r2.start_address) 0x2000L;
  check int64 "second length" (Unsigned.UInt64.to_int64 r2.length) 0x200L

let () =
  run "debug_aranges"
    [
      ( "debug_aranges",
        [
          test_case "parse returns None without section" `Quick
            test_aranges_parse_no_section;
          test_case "section names" `Quick test_aranges_section_name;
          test_case "address_range record" `Quick
            test_aranges_address_range_record;
          test_case "header record" `Quick test_aranges_header_record;
          test_case "aranges_set record" `Quick test_aranges_set_record;
          test_case "multiple ranges" `Quick test_aranges_multiple_ranges;
        ] );
    ]
