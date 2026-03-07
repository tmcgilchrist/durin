open Alcotest
open Durin

let buffer_of_bytes bytes =
  let filename = Filename.temp_file "debug_loc_test_" ".bin" in
  let oc = open_out_bin filename in
  List.iter (fun b -> output_char oc (Char.chr b)) bytes;
  close_out oc;
  let buffer = Object.Buffer.parse filename in
  Sys.remove filename;
  buffer

let u8 v = Unsigned.UInt8.of_int v

(* ---- DebugLoc tests (DWARF 4) ---- *)

let test_debug_loc_end_of_list () =
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
  let bytes =
    [
      0xff;
      0xff;
      0xff;
      0xff;
      0xff;
      0xff;
      0xff;
      0xff;
      0x00;
      0x10;
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
      0x02;
      0x00;
      0xAB;
      0xCD;
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

(* ---- DebugLoclists tests (DWARF 5) ---- *)

let test_loclists_end_of_list () =
  let bytes = [ 0x00 ] in
  let buffer = buffer_of_bytes bytes in
  let cursor = Object.Buffer.cursor buffer ~at:0 in
  let ll = Dwarf.DebugLoclists.parse_location_list cursor (u8 8) in
  check int "one entry" 1 (List.length ll.entries);
  match ll.entries with
  | [ Dwarf.DebugLoclists.LLE_end_of_list ] -> ()
  | _ -> fail "expected LLE_end_of_list"

let test_loclists_offset_pair () =
  let bytes = [ 0x04; 0x10; 0x30; 0x02; 0x50; 0x00; 0x00 ] in
  let buffer = buffer_of_bytes bytes in
  let cursor = Object.Buffer.cursor buffer ~at:0 in
  let ll = Dwarf.DebugLoclists.parse_location_list cursor (u8 8) in
  check int "two entries" 2 (List.length ll.entries);
  match List.hd ll.entries with
  | Dwarf.DebugLoclists.LLE_offset_pair { start_offset; end_offset; expr } ->
      check int64 "start 0x10" (Unsigned.UInt64.to_int64 start_offset) 0x10L;
      check int64 "end 0x30" (Unsigned.UInt64.to_int64 end_offset) 0x30L;
      check int "expr len 2" 2 (String.length expr)
  | _ -> fail "expected LLE_offset_pair"

let test_loclists_base_address () =
  let bytes = [ 0x06; 0x00; 0x40; 0x00; 0x00; 0x00; 0x00; 0x00; 0x00; 0x00 ] in
  let buffer = buffer_of_bytes bytes in
  let cursor = Object.Buffer.cursor buffer ~at:0 in
  let ll = Dwarf.DebugLoclists.parse_location_list cursor (u8 8) in
  check int "two entries" 2 (List.length ll.entries);
  match List.hd ll.entries with
  | Dwarf.DebugLoclists.LLE_base_address { address } ->
      check int64 "base 0x4000" (Unsigned.UInt64.to_int64 address) 0x4000L
  | _ -> fail "expected LLE_base_address"

let test_loclists_start_end () =
  let bytes =
    [ 0x07; 0x00; 0x10; 0x00; 0x00; 0x00; 0x20; 0x00; 0x00; 0x01; 0x50; 0x00 ]
  in
  let buffer = buffer_of_bytes bytes in
  let cursor = Object.Buffer.cursor buffer ~at:0 in
  let ll = Dwarf.DebugLoclists.parse_location_list cursor (u8 4) in
  check int "two entries" 2 (List.length ll.entries);
  match List.hd ll.entries with
  | Dwarf.DebugLoclists.LLE_start_end { start_addr; end_addr; expr } ->
      check int64 "start 0x1000" (Unsigned.UInt64.to_int64 start_addr) 0x1000L;
      check int64 "end 0x2000" (Unsigned.UInt64.to_int64 end_addr) 0x2000L;
      check int "expr len 1" 1 (String.length expr)
  | _ -> fail "expected LLE_start_end"

let test_loclists_default_location () =
  let bytes = [ 0x05; 0x01; 0x50; 0x00 ] in
  let buffer = buffer_of_bytes bytes in
  let cursor = Object.Buffer.cursor buffer ~at:0 in
  let ll = Dwarf.DebugLoclists.parse_location_list cursor (u8 8) in
  check int "two entries" 2 (List.length ll.entries);
  match List.hd ll.entries with
  | Dwarf.DebugLoclists.LLE_default_location { expr } ->
      check int "expr len 1" 1 (String.length expr)
  | _ -> fail "expected LLE_default_location"

let test_loclists_unknown_kind () =
  let bytes = [ 0xFF ] in
  let buffer = buffer_of_bytes bytes in
  let cursor = Object.Buffer.cursor buffer ~at:0 in
  check_raises "unknown kind" (Failure "Unknown DW_LLE entry kind: 0xff")
    (fun () -> ignore (Dwarf.DebugLoclists.parse_location_list cursor (u8 8)))

let test_resolve_location_list_no_section () =
  let bytes = [ 0x00; 0x00; 0x00; 0x00 ] in
  let buffer = buffer_of_bytes bytes in
  let result =
    try Dwarf.resolve_location_list buffer (Unsigned.UInt64.of_int 0) 8
    with Failure _ -> None
  in
  check bool "returns None when no .debug_loc section" true (result = None)

let () =
  run "debug_loc"
    [
      ( "debug_loc",
        [
          test_case "end of list" `Quick test_debug_loc_end_of_list;
          test_case "base address" `Quick test_debug_loc_base_address;
          test_case "location entry" `Quick test_debug_loc_location_entry;
        ] );
      ( "debug_loclists",
        [
          test_case "end of list" `Quick test_loclists_end_of_list;
          test_case "offset pair" `Quick test_loclists_offset_pair;
          test_case "base address" `Quick test_loclists_base_address;
          test_case "start end" `Quick test_loclists_start_end;
          test_case "default location" `Quick test_loclists_default_location;
          test_case "unknown kind" `Quick test_loclists_unknown_kind;
        ] );
      ( "resolution",
        [
          test_case "resolve_location_list no section" `Quick
            test_resolve_location_list_no_section;
        ] );
    ]
