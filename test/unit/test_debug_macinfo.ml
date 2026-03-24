open Alcotest
open Durin

let buffer_of_bytes bytes =
  let filename = Filename.temp_file "debug_macinfo_test_" ".bin" in
  let oc = open_out_bin filename in
  List.iter (fun b -> output_char oc (Char.chr b)) bytes;
  close_out oc;
  let buffer = Object.Buffer.parse filename in
  Sys.remove filename;
  buffer

let test_macinfo_define () =
  let bytes = [ 0x01; 0x03; 0x46; 0x4f; 0x4f; 0x20; 0x31; 0x00; 0x00 ] in
  let buffer = buffer_of_bytes bytes in
  let cursor = Object.Buffer.cursor buffer ~at:0 in
  let section = Dwarf.DebugMacinfo.parse_section cursor (List.length bytes) in
  check int "one entry" 1 (List.length section.entries);
  let entry = List.hd section.entries in
  check string "type is define" "DW_MACINFO_define"
    (Dwarf.DebugMacinfo.string_of_macinfo_type entry.macinfo_type);
  (match entry.line_number with
  | Some ln -> check int "line is 3" 3 (Unsigned.UInt32.to_int ln)
  | None -> fail "expected line_number");
  match entry.string_value with
  | Some s -> check string "string is FOO 1" "FOO 1" s
  | None -> fail "expected string_value"

let test_macinfo_start_end_file () =
  let bytes = [ 0x03; 0x01; 0x00; 0x04; 0x00 ] in
  let buffer = buffer_of_bytes bytes in
  let cursor = Object.Buffer.cursor buffer ~at:0 in
  let section = Dwarf.DebugMacinfo.parse_section cursor (List.length bytes) in
  check int "two entries" 2 (List.length section.entries);
  let first = List.nth section.entries 0 in
  check string "first is start_file" "DW_MACINFO_start_file"
    (Dwarf.DebugMacinfo.string_of_macinfo_type first.macinfo_type);
  (match first.file_index with
  | Some fi -> check int "file index is 0" 0 (Unsigned.UInt32.to_int fi)
  | None -> fail "expected file_index");
  let second = List.nth section.entries 1 in
  check string "second is end_file" "DW_MACINFO_end_file"
    (Dwarf.DebugMacinfo.string_of_macinfo_type second.macinfo_type)

let test_macinfo_vendor_ext () =
  let bytes = [ 0xff; 0x2a; 0x65; 0x78; 0x74; 0x00; 0x00 ] in
  let buffer = buffer_of_bytes bytes in
  let cursor = Object.Buffer.cursor buffer ~at:0 in
  let section = Dwarf.DebugMacinfo.parse_section cursor (List.length bytes) in
  check int "one entry" 1 (List.length section.entries);
  let entry = List.hd section.entries in
  check string "type is vendor_ext" "DW_MACINFO_vendor_ext"
    (Dwarf.DebugMacinfo.string_of_macinfo_type entry.macinfo_type);
  (match entry.constant with
  | Some c -> check int64 "constant is 42" 42L (Unsigned.UInt64.to_int64 c)
  | None -> fail "expected constant");
  match entry.string_value with
  | Some s -> check string "string is ext" "ext" s
  | None -> fail "expected string_value"

let test_macinfo_empty () =
  let bytes = [ 0x00 ] in
  let buffer = buffer_of_bytes bytes in
  let cursor = Object.Buffer.cursor buffer ~at:0 in
  let section = Dwarf.DebugMacinfo.parse_section cursor (List.length bytes) in
  check int "no entries" 0 (List.length section.entries)

let () =
  run "debug_macinfo"
    [
      ( "debug_macinfo",
        [
          test_case "macinfo define" `Quick test_macinfo_define;
          test_case "macinfo start_file end_file" `Quick
            test_macinfo_start_end_file;
          test_case "macinfo vendor_ext" `Quick test_macinfo_vendor_ext;
          test_case "macinfo empty section" `Quick test_macinfo_empty;
        ] );
    ]
