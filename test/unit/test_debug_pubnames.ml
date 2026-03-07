open Alcotest
open Durin

let buffer_of_bytes bytes =
  let filename = Filename.temp_file "debug_pubnames_test_" ".bin" in
  let oc = open_out_bin filename in
  List.iter (fun b -> output_char oc (Char.chr b)) bytes;
  close_out oc;
  let buffer = Object.Buffer.parse filename in
  Sys.remove filename;
  buffer

let test_debug_pubnames_set () =
  let name_bytes = List.map Char.code (List.of_seq (String.to_seq "main")) in
  let bytes =
    [
      0x1a;
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
      0x01;
      0x00;
      0x00;
      0x2a;
      0x00;
      0x00;
      0x00;
    ]
    @ name_bytes
    @ [ 0x00; 0x00; 0x00; 0x00; 0x00 ]
  in
  let buffer = buffer_of_bytes bytes in
  let cursor = Object.Buffer.cursor buffer ~at:0 in
  let header, entries = Dwarf.DebugPubnames.parse_set cursor in
  check int "version is 2" (Unsigned.UInt16.to_int header.version) 2;
  check int64 "info_offset is 0"
    (Unsigned.UInt64.to_int64 header.debug_info_offset)
    0L;
  check int64 "info_length is 0x100"
    (Unsigned.UInt64.to_int64 header.debug_info_length)
    0x100L;
  check int "one entry" 1 (List.length entries);
  let e = List.hd entries in
  check int64 "entry offset 0x2a" (Unsigned.UInt64.to_int64 e.offset) 0x2aL;
  check string "entry name" "main" e.name

let test_debug_pubtypes_set () =
  let name_bytes = List.map Char.code (List.of_seq (String.to_seq "int")) in
  let bytes =
    [
      0x19;
      0x00;
      0x00;
      0x00;
      0x02;
      0x00;
      0x00;
      0x00;
      0x00;
      0x00;
      0x80;
      0x00;
      0x00;
      0x00;
      0x10;
      0x00;
      0x00;
      0x00;
    ]
    @ name_bytes
    @ [ 0x00; 0x00; 0x00; 0x00; 0x00 ]
  in
  let buffer = buffer_of_bytes bytes in
  let cursor = Object.Buffer.cursor buffer ~at:0 in
  let header, entries = Dwarf.DebugPubtypes.parse_set cursor in
  check int "version is 2" (Unsigned.UInt16.to_int header.version) 2;
  check int64 "info_length is 0x80"
    (Unsigned.UInt64.to_int64 header.debug_info_length)
    0x80L;
  check int "one entry" 1 (List.length entries);
  let e = List.hd entries in
  check int64 "entry offset 0x10" (Unsigned.UInt64.to_int64 e.offset) 0x10L;
  check string "entry name" "int" e.name

let () =
  run "debug_pubnames"
    [
      ( "debug_pubnames",
        [ test_case "pubnames set" `Quick test_debug_pubnames_set ] );
      ( "debug_pubtypes",
        [ test_case "pubtypes set" `Quick test_debug_pubtypes_set ] );
    ]
