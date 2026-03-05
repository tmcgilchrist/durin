open Alcotest
open Durin

(* Helper to create a buffer from byte list by writing to a temp file *)
let buffer_of_bytes bytes =
  let filename = Filename.temp_file "dwarf64_test_" ".bin" in
  let oc = open_out_bin filename in
  List.iter (fun b -> output_char oc (Char.chr b)) bytes;
  close_out oc;
  let buffer = Object.Buffer.parse filename in
  Sys.remove filename;
  buffer

(* Test parse_initial_length for DWARF32 *)
let test_parse_initial_length_dwarf32 () =
  (* DWARF32: length = 0x00000100 (256 bytes) *)
  let bytes = [ 0x00; 0x01; 0x00; 0x00 ] in
  let buffer = buffer_of_bytes bytes in
  let cursor = Object.Buffer.cursor buffer ~at:0 in
  let format, length = Dwarf.parse_initial_length cursor in
  check
    (module struct
      type t = Dwarf.dwarf_format

      let equal a b = a = b
      let pp fmt v = Format.fprintf fmt "%s" (Dwarf.string_of_dwarf_format v)
    end)
    "format is DWARF32" Dwarf.DWARF32 format;
  check int64 "length is 256"
    (Unsigned.UInt64.to_int64 length)
    (Int64.of_int 256)

(* Test parse_initial_length for DWARF64 *)
let test_parse_initial_length_dwarf64 () =
  (* DWARF64: marker 0xffffffff + 8-byte length = 0x0000000000000200 (512 bytes) *)
  let bytes =
    [ 0xff; 0xff; 0xff; 0xff; 0x00; 0x02; 0x00; 0x00; 0x00; 0x00; 0x00; 0x00 ]
  in
  let buffer = buffer_of_bytes bytes in
  let cursor = Object.Buffer.cursor buffer ~at:0 in
  let format, length = Dwarf.parse_initial_length cursor in
  check
    (module struct
      type t = Dwarf.dwarf_format

      let equal a b = a = b
      let pp fmt v = Format.fprintf fmt "%s" (Dwarf.string_of_dwarf_format v)
    end)
    "format is DWARF64" Dwarf.DWARF64 format;
  check int64 "length is 512"
    (Unsigned.UInt64.to_int64 length)
    (Int64.of_int 512)

(* Test read_offset_for_format with DWARF32 *)
let test_read_offset_dwarf32 () =
  (* 4-byte offset: 0x12345678 *)
  let bytes = [ 0x78; 0x56; 0x34; 0x12 ] in
  let buffer = buffer_of_bytes bytes in
  let cursor = Object.Buffer.cursor buffer ~at:0 in
  let offset = Dwarf.read_offset_for_format Dwarf.DWARF32 cursor in
  check int64 "offset is 0x12345678"
    (Unsigned.UInt64.to_int64 offset)
    (Int64.of_string "0x12345678")

(* Test read_offset_for_format with DWARF64 *)
let test_read_offset_dwarf64 () =
  (* 8-byte offset: 0x123456789abcdef0 *)
  let bytes = [ 0xf0; 0xde; 0xbc; 0x9a; 0x78; 0x56; 0x34; 0x12 ] in
  let buffer = buffer_of_bytes bytes in
  let cursor = Object.Buffer.cursor buffer ~at:0 in
  let offset = Dwarf.read_offset_for_format Dwarf.DWARF64 cursor in
  check int64 "offset is 0x123456789abcdef0"
    (Unsigned.UInt64.to_int64 offset)
    (Int64.of_string "0x123456789abcdef0")

(* Test offset_size_for_format *)
let test_offset_size_for_format () =
  check int "DWARF32 offset size is 4"
    (Dwarf.offset_size_for_format Dwarf.DWARF32)
    4;
  check int "DWARF64 offset size is 8"
    (Dwarf.offset_size_for_format Dwarf.DWARF64)
    8

(* Test string_of_dwarf_format *)
let test_string_of_dwarf_format () =
  check string "DWARF32 string representation"
    (Dwarf.string_of_dwarf_format Dwarf.DWARF32)
    "DWARF32";
  check string "DWARF64 string representation"
    (Dwarf.string_of_dwarf_format Dwarf.DWARF64)
    "DWARF64"

let () =
  run "DWARF64 Parsing"
    [
      ( "parse_initial_length",
        [
          test_case "DWARF32 format" `Quick test_parse_initial_length_dwarf32;
          test_case "DWARF64 format" `Quick test_parse_initial_length_dwarf64;
        ] );
      ( "read_offset",
        [
          test_case "DWARF32 4-byte offset" `Quick test_read_offset_dwarf32;
          test_case "DWARF64 8-byte offset" `Quick test_read_offset_dwarf64;
        ] );
      ( "offset_size",
        [
          test_case "offset size for format" `Quick test_offset_size_for_format;
        ] );
      ( "string_conversion",
        [
          test_case "string_of_dwarf_format" `Quick test_string_of_dwarf_format;
        ] );
    ]
