open Alcotest
open Durin
module Buffer = Stdlib.Buffer

let object_buffer_of_buffer buf =
  let contents = Buffer.contents buf in
  let filename = Filename.temp_file "dwarf_write_test_" ".bin" in
  let oc = open_out_bin filename in
  output_string oc contents;
  close_out oc;
  let obj_buf = Object.Buffer.parse filename in
  Sys.remove filename;
  obj_buf

let test_write_u8 () =
  let buf = Buffer.create 1 in
  Dwarf_write.write_u8 buf (Unsigned.UInt8.of_int 0x42);
  let obj_buf = object_buffer_of_buffer buf in
  let cur = Object.Buffer.cursor obj_buf ~at:0 in
  let v = Object.Buffer.Read.u8 cur in
  check int "u8 roundtrip" 0x42 (Unsigned.UInt8.to_int v)

let test_write_u16_le () =
  let buf = Buffer.create 2 in
  Dwarf_write.write_u16_le buf (Unsigned.UInt16.of_int 0x1234);
  let obj_buf = object_buffer_of_buffer buf in
  let cur = Object.Buffer.cursor obj_buf ~at:0 in
  let v = Object.Buffer.Read.u16 cur in
  check int "u16 roundtrip" 0x1234 (Unsigned.UInt16.to_int v)

let test_write_u32_le () =
  let buf = Buffer.create 4 in
  Dwarf_write.write_u32_le buf (Unsigned.UInt32.of_int 0x12345678);
  let obj_buf = object_buffer_of_buffer buf in
  let cur = Object.Buffer.cursor obj_buf ~at:0 in
  let v = Object.Buffer.Read.u32 cur in
  check int64 "u32 roundtrip"
    (Int64.of_string "0x12345678")
    (Unsigned.UInt32.to_int64 v)

let test_write_u64_le () =
  let buf = Buffer.create 8 in
  Dwarf_write.write_u64_le buf
    (Unsigned.UInt64.of_int64 (Int64.of_string "0x123456789abcdef0"));
  let obj_buf = object_buffer_of_buffer buf in
  let cur = Object.Buffer.cursor obj_buf ~at:0 in
  let v = Object.Buffer.Read.u64 cur in
  check int64 "u64 roundtrip"
    (Int64.of_string "0x123456789abcdef0")
    (Unsigned.UInt64.to_int64 v)

let test_write_i64_le () =
  let buf = Buffer.create 8 in
  Dwarf_write.write_i64_le buf (Signed.Int64.of_int64 (-42L));
  let obj_buf = object_buffer_of_buffer buf in
  let cur = Object.Buffer.cursor obj_buf ~at:0 in
  let v = Object.Buffer.Read.i64 cur in
  check int64 "i64 roundtrip" (-42L) (Signed.Int64.to_int64 v)

let roundtrip_uleb128 value =
  let buf = Buffer.create 10 in
  let u64_val = Unsigned.UInt64.of_int value in
  Dwarf_write.write_uleb128 buf u64_val;
  let obj_buf = object_buffer_of_buffer buf in
  let cur = Object.Buffer.cursor obj_buf ~at:0 in
  let read_back = Object.Buffer.Read.uleb128 cur in
  check int (Printf.sprintf "uleb128 roundtrip %d" value) value read_back

let test_write_uleb128 () =
  roundtrip_uleb128 0;
  roundtrip_uleb128 127;
  roundtrip_uleb128 128;
  roundtrip_uleb128 16384;
  roundtrip_uleb128 0x0102030405

let roundtrip_sleb128 value =
  let buf = Buffer.create 10 in
  let i64_val = Signed.Int64.of_int value in
  Dwarf_write.write_sleb128 buf i64_val;
  let obj_buf = object_buffer_of_buffer buf in
  let cur = Object.Buffer.cursor obj_buf ~at:0 in
  let read_back = Object.Buffer.Read.sleb128 cur in
  check int (Printf.sprintf "sleb128 roundtrip %d" value) value read_back

let test_write_sleb128 () =
  roundtrip_sleb128 0;
  roundtrip_sleb128 (-1);
  roundtrip_sleb128 63;
  roundtrip_sleb128 (-64);
  roundtrip_sleb128 128;
  roundtrip_sleb128 (-129)

let test_write_initial_length_dwarf32 () =
  let buf = Buffer.create 4 in
  Dwarf_write.write_initial_length buf Dwarf.DWARF32 256;
  let obj_buf = object_buffer_of_buffer buf in
  let cur = Object.Buffer.cursor obj_buf ~at:0 in
  let format, length = Dwarf.parse_initial_length cur in
  check
    (module struct
      type t = Dwarf.dwarf_format

      let equal a b = a = b
      let pp fmt v = Format.fprintf fmt "%s" (Dwarf.string_of_dwarf_format v)
    end)
    "format is DWARF32" Dwarf.DWARF32 format;
  check int64 "length is 256" (Int64.of_int 256)
    (Unsigned.UInt64.to_int64 length)

let test_write_initial_length_dwarf64 () =
  let buf = Buffer.create 12 in
  Dwarf_write.write_initial_length buf Dwarf.DWARF64 512;
  let obj_buf = object_buffer_of_buffer buf in
  let cur = Object.Buffer.cursor obj_buf ~at:0 in
  let format, length = Dwarf.parse_initial_length cur in
  check
    (module struct
      type t = Dwarf.dwarf_format

      let equal a b = a = b
      let pp fmt v = Format.fprintf fmt "%s" (Dwarf.string_of_dwarf_format v)
    end)
    "format is DWARF64" Dwarf.DWARF64 format;
  check int64 "length is 512" (Int64.of_int 512)
    (Unsigned.UInt64.to_int64 length)

let test_write_offset_dwarf32 () =
  let buf = Buffer.create 4 in
  Dwarf_write.write_offset buf Dwarf.DWARF32 (Unsigned.UInt64.of_int 0x12345678);
  let obj_buf = object_buffer_of_buffer buf in
  let cur = Object.Buffer.cursor obj_buf ~at:0 in
  let v = Dwarf.read_offset_for_format Dwarf.DWARF32 cur in
  check int64 "offset DWARF32 roundtrip"
    (Int64.of_string "0x12345678")
    (Unsigned.UInt64.to_int64 v)

let test_write_offset_dwarf64 () =
  let buf = Buffer.create 8 in
  Dwarf_write.write_offset buf Dwarf.DWARF64
    (Unsigned.UInt64.of_int64 (Int64.of_string "0x123456789abcdef0"));
  let obj_buf = object_buffer_of_buffer buf in
  let cur = Object.Buffer.cursor obj_buf ~at:0 in
  let v = Dwarf.read_offset_for_format Dwarf.DWARF64 cur in
  check int64 "offset DWARF64 roundtrip"
    (Int64.of_string "0x123456789abcdef0")
    (Unsigned.UInt64.to_int64 v)

let test_write_address_4 () =
  let buf = Buffer.create 4 in
  Dwarf_write.write_address buf 4 (Unsigned.UInt64.of_int 0x12345678);
  let obj_buf = object_buffer_of_buffer buf in
  let cur = Object.Buffer.cursor obj_buf ~at:0 in
  let v = Object.Buffer.Read.u32 cur |> Unsigned.UInt64.of_uint32 in
  check int64 "4-byte address roundtrip"
    (Int64.of_string "0x12345678")
    (Unsigned.UInt64.to_int64 v)

let test_write_address_8 () =
  let buf = Buffer.create 8 in
  Dwarf_write.write_address buf 8
    (Unsigned.UInt64.of_int64 (Int64.of_string "0x123456789abcdef0"));
  let obj_buf = object_buffer_of_buffer buf in
  let cur = Object.Buffer.cursor obj_buf ~at:0 in
  let v = Object.Buffer.Read.u64 cur in
  check int64 "8-byte address roundtrip"
    (Int64.of_string "0x123456789abcdef0")
    (Unsigned.UInt64.to_int64 v)

let test_write_null_terminated_string () =
  let buf = Buffer.create 16 in
  Dwarf_write.write_null_terminated_string buf "hello";
  let obj_buf = object_buffer_of_buffer buf in
  let cur = Object.Buffer.cursor obj_buf ~at:0 in
  let s = Object.Buffer.Read.zero_string cur () in
  check (option string) "null terminated string roundtrip" (Some "hello") s

let () =
  run "Dwarf_write"
    [
      ( "fixed-width",
        [
          test_case "write_u8" `Quick test_write_u8;
          test_case "write_u16_le" `Quick test_write_u16_le;
          test_case "write_u32_le" `Quick test_write_u32_le;
          test_case "write_u64_le" `Quick test_write_u64_le;
          test_case "write_i64_le" `Quick test_write_i64_le;
        ] );
      ( "variable-length",
        [
          test_case "write_uleb128" `Quick test_write_uleb128;
          test_case "write_sleb128" `Quick test_write_sleb128;
        ] );
      ( "dwarf-compound",
        [
          test_case "initial_length DWARF32" `Quick
            test_write_initial_length_dwarf32;
          test_case "initial_length DWARF64" `Quick
            test_write_initial_length_dwarf64;
          test_case "offset DWARF32" `Quick test_write_offset_dwarf32;
          test_case "offset DWARF64" `Quick test_write_offset_dwarf64;
          test_case "address 4-byte" `Quick test_write_address_4;
          test_case "address 8-byte" `Quick test_write_address_8;
          test_case "null_terminated_string" `Quick
            test_write_null_terminated_string;
        ] );
    ]
