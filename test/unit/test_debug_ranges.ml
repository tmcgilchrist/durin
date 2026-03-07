open Alcotest
open Durin

let buffer_of_bytes bytes =
  let filename = Filename.temp_file "debug_ranges_test_" ".bin" in
  let oc = open_out_bin filename in
  List.iter (fun b -> output_char oc (Char.chr b)) bytes;
  close_out oc;
  let buffer = Object.Buffer.parse filename in
  Sys.remove filename;
  buffer

let u8 v = Unsigned.UInt8.of_int v

(* ---- DebugRanges tests (DWARF 4) ---- *)

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
  let bytes =
    [
      0xff;
      0xff;
      0xff;
      0xff;
      0x00;
      0x80;
      0x00;
      0x00;
      0x10;
      0x00;
      0x00;
      0x00;
      0x20;
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

(* ---- DebugRnglists tests (DWARF 5) ---- *)

let test_rnglists_end_of_list () =
  let bytes = [ 0x00 ] in
  let buffer = buffer_of_bytes bytes in
  let cursor = Object.Buffer.cursor buffer ~at:0 in
  let rl = Dwarf.DebugRnglists.parse_range_list cursor (u8 8) in
  check int "one entry" 1 (List.length rl.entries);
  match rl.entries with
  | [ Dwarf.DebugRnglists.RLE_end_of_list ] -> ()
  | _ -> fail "expected RLE_end_of_list"

let test_rnglists_offset_pair () =
  let bytes = [ 0x04; 0x10; 0x30; 0x00 ] in
  let buffer = buffer_of_bytes bytes in
  let cursor = Object.Buffer.cursor buffer ~at:0 in
  let rl = Dwarf.DebugRnglists.parse_range_list cursor (u8 8) in
  check int "two entries" 2 (List.length rl.entries);
  match List.hd rl.entries with
  | Dwarf.DebugRnglists.RLE_offset_pair { start_offset; end_offset } ->
      check int64 "start 0x10" (Unsigned.UInt64.to_int64 start_offset) 0x10L;
      check int64 "end 0x30" (Unsigned.UInt64.to_int64 end_offset) 0x30L
  | _ -> fail "expected RLE_offset_pair"

let test_rnglists_base_address () =
  let bytes =
    [
      0x05;
      0x00;
      0x40;
      0x00;
      0x00;
      0x00;
      0x00;
      0x00;
      0x00;
      0x04;
      0x00;
      0x20;
      0x00;
    ]
  in
  let buffer = buffer_of_bytes bytes in
  let cursor = Object.Buffer.cursor buffer ~at:0 in
  let rl = Dwarf.DebugRnglists.parse_range_list cursor (u8 8) in
  check int "three entries" 3 (List.length rl.entries);
  match List.hd rl.entries with
  | Dwarf.DebugRnglists.RLE_base_address { address } ->
      check int64 "base 0x4000" (Unsigned.UInt64.to_int64 address) 0x4000L
  | _ -> fail "expected RLE_base_address"

let test_rnglists_start_end () =
  let bytes = [ 0x06; 0x00; 0x10; 0x00; 0x00; 0x00; 0x20; 0x00; 0x00; 0x00 ] in
  let buffer = buffer_of_bytes bytes in
  let cursor = Object.Buffer.cursor buffer ~at:0 in
  let rl = Dwarf.DebugRnglists.parse_range_list cursor (u8 4) in
  check int "two entries" 2 (List.length rl.entries);
  match List.hd rl.entries with
  | Dwarf.DebugRnglists.RLE_start_end { start_addr; end_addr } ->
      check int64 "start 0x1000" (Unsigned.UInt64.to_int64 start_addr) 0x1000L;
      check int64 "end 0x2000" (Unsigned.UInt64.to_int64 end_addr) 0x2000L
  | _ -> fail "expected RLE_start_end"

let test_rnglists_startx_length () =
  let bytes = [ 0x03; 0x05; 0x80; 0x01; 0x00 ] in
  let buffer = buffer_of_bytes bytes in
  let cursor = Object.Buffer.cursor buffer ~at:0 in
  let rl = Dwarf.DebugRnglists.parse_range_list cursor (u8 8) in
  check int "two entries" 2 (List.length rl.entries);
  match List.hd rl.entries with
  | Dwarf.DebugRnglists.RLE_startx_length { start_index; length } ->
      check int "start_index 5" start_index 5;
      check int64 "length 128" (Unsigned.UInt64.to_int64 length) 128L
  | _ -> fail "expected RLE_startx_length"

let test_rnglists_unknown_kind () =
  let bytes = [ 0xFF ] in
  let buffer = buffer_of_bytes bytes in
  let cursor = Object.Buffer.cursor buffer ~at:0 in
  check_raises "unknown kind" (Failure "Unknown DW_RLE entry kind: 0xff")
    (fun () -> ignore (Dwarf.DebugRnglists.parse_range_list cursor (u8 8)))

let test_resolve_range_list_no_section () =
  let bytes = [ 0x00; 0x00; 0x00; 0x00 ] in
  let buffer = buffer_of_bytes bytes in
  let result =
    try Dwarf.resolve_range_list buffer (Unsigned.UInt64.of_int 0) 8
    with Failure _ -> None
  in
  check bool "returns None when no .debug_ranges section" true (result = None)

let () =
  run "debug_ranges"
    [
      ( "debug_ranges",
        [
          test_case "end of list" `Quick test_debug_ranges_end_of_list;
          test_case "range entry" `Quick test_debug_ranges_range_entry;
          test_case "base address 32-bit" `Quick
            test_debug_ranges_base_address_32bit;
        ] );
      ( "debug_rnglists",
        [
          test_case "end of list" `Quick test_rnglists_end_of_list;
          test_case "offset pair" `Quick test_rnglists_offset_pair;
          test_case "base address" `Quick test_rnglists_base_address;
          test_case "start end" `Quick test_rnglists_start_end;
          test_case "startx length" `Quick test_rnglists_startx_length;
          test_case "unknown kind" `Quick test_rnglists_unknown_kind;
        ] );
      ( "resolution",
        [
          test_case "resolve_range_list no section" `Quick
            test_resolve_range_list_no_section;
        ] );
    ]
