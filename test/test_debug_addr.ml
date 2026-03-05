open Alcotest
open Durin

(* TODO Make this a common test helper file *)
let buffer_of_bytes bytes =
  let filename = Filename.temp_file "debug_addr_test_" ".bin" in
  let oc = open_out_bin filename in
  List.iter (fun b -> output_char oc (Char.chr b)) bytes;
  close_out oc;
  let buffer = Object.Buffer.parse filename in
  Sys.remove filename;
  buffer

(* ---- DebugAddr header parsing ---- *)

(* TODO Add DWARF64 header example *)

let test_debug_addr_header_dwarf32 () =
  (* DWARF32 header:
     unit_length: 4 bytes (value = 12: 2 version + 1 addr_size
       + 1 seg_sel_size + 8 for one 8-byte addr)
     version: 2 bytes (5)
     address_size: 1 byte (8)
     segment_selector_size: 1 byte (0) *)
  let bytes =
    [
      0x0c; 0x00; 0x00; 0x00; 0x05; 0x00; 0x08; 0x00;
    ]
  in
  let buffer = buffer_of_bytes bytes in
  let cursor = Object.Buffer.cursor buffer ~at:0 in
  let header = Dwarf.DebugAddr.parse_header cursor in
  check int "version is 5" (Unsigned.UInt16.to_int header.version) 5;
  check int "address_size is 8" (Unsigned.UInt8.to_int header.address_size) 8;
  check int "segment_selector_size is 0"
    (Unsigned.UInt8.to_int header.segment_selector_size)
    0;
  check bool "format is DWARF32" true (header.format = Dwarf.DWARF32)

let test_debug_addr_header_4byte_addr () =
  (* DWARF32 with 4-byte addresses *)
  let bytes =
    [
      0x0c; 0x00; 0x00; 0x00; 0x05; 0x00; 0x04; 0x00;
    ]
  in
  let buffer = buffer_of_bytes bytes in
  let cursor = Object.Buffer.cursor buffer ~at:0 in
  let header = Dwarf.DebugAddr.parse_header cursor in
  check int "address_size is 4" (Unsigned.UInt8.to_int header.address_size) 4

(* ---- DebugAddr entry parsing ---- *)

let test_debug_addr_single_entry_8byte () =
  (* Header + one 8-byte address entry (0x00401000) *)
  let bytes =
    [
      (* unit_length: 12 = 2+1+1+8 *)
      0x0c; 0x00; 0x00; 0x00;
      (* version: 5 *)
      0x05; 0x00;
      (* address_size: 8 *)
      0x08;
      (* segment_selector_size: 0 *)
      0x00;
      (* address: 0x00401000 LE *)
      0x00; 0x10; 0x40; 0x00; 0x00; 0x00; 0x00; 0x00;
    ]
  in
  let buffer = buffer_of_bytes bytes in
  let cursor = Object.Buffer.cursor buffer ~at:0 in
  let header = Dwarf.DebugAddr.parse_header cursor in
  let entries = Dwarf.DebugAddr.parse_entries cursor header in
  check int "one entry" 1 (Array.length entries);
  check int64 "address is 0x401000"
    (Unsigned.UInt64.to_int64 entries.(0).address)
    0x401000L;
  check bool "no segment" true (entries.(0).segment = None)

let test_debug_addr_multiple_entries_4byte () =
  (* Header + three 4-byte address entries *)
  let bytes =
    [
      (* unit_length: 16 = 2+1+1+3*4 *)
      0x10; 0x00; 0x00; 0x00;
      (* version: 5 *)
      0x05; 0x00;
      (* address_size: 4 *)
      0x04;
      (* segment_selector_size: 0 *)
      0x00;
      (* addr 0: 0x1000 *)
      0x00; 0x10; 0x00; 0x00;
      (* addr 1: 0x2000 *)
      0x00; 0x20; 0x00; 0x00;
      (* addr 2: 0x3000 *)
      0x00; 0x30; 0x00; 0x00;
    ]
  in
  let buffer = buffer_of_bytes bytes in
  let cursor = Object.Buffer.cursor buffer ~at:0 in
  let header = Dwarf.DebugAddr.parse_header cursor in
  let entries = Dwarf.DebugAddr.parse_entries cursor header in
  check int "three entries" 3 (Array.length entries);
  check int64 "addr 0" (Unsigned.UInt64.to_int64 entries.(0).address) 0x1000L;
  check int64 "addr 1" (Unsigned.UInt64.to_int64 entries.(1).address) 0x2000L;
  check int64 "addr 2" (Unsigned.UInt64.to_int64 entries.(2).address) 0x3000L

let test_debug_addr_empty () =
  (* Header with unit_length covering only the header fields (no entries) *)
  let bytes =
    [
      (* unit_length: 4 = 2+1+1 (header fields only) *)
      0x04; 0x00; 0x00; 0x00;
      (* version: 5 *)
      0x05; 0x00;
      (* address_size: 8 *)
      0x08;
      (* segment_selector_size: 0 *)
      0x00;
    ]
  in
  let buffer = buffer_of_bytes bytes in
  let cursor = Object.Buffer.cursor buffer ~at:0 in
  let header = Dwarf.DebugAddr.parse_header cursor in
  let entries = Dwarf.DebugAddr.parse_entries cursor header in
  check int "no entries" 0 (Array.length entries)

let test_debug_addr_parse_combined () =
  (* Test the combined parse function *)
  let bytes =
    [
      (* unit_length: 20 = 2+1+1+2*8 *)
      0x14; 0x00; 0x00; 0x00;
      (* version: 5 *)
      0x05; 0x00;
      (* address_size: 8 *)
      0x08;
      (* segment_selector_size: 0 *)
      0x00;
      (* addr 0: 0xDEADBEEF *)
      0xEF; 0xBE; 0xAD; 0xDE; 0x00; 0x00; 0x00; 0x00;
      (* addr 1: 0xCAFEBABE *)
      0xBE; 0xBA; 0xFE; 0xCA; 0x00; 0x00; 0x00; 0x00;
    ]
  in
  let buffer = buffer_of_bytes bytes in
  let t = Dwarf.DebugAddr.parse buffer (Unsigned.UInt64.of_int 0) in
  check int "two entries" 2 (Array.length t.entries);
  check int64 "first addr"
    (Unsigned.UInt64.to_int64 t.entries.(0).address)
    0xDEADBEEFL;
  check int64 "second addr"
    (Unsigned.UInt64.to_int64 t.entries.(1).address)
    0xCAFEBABEL

let test_debug_addr_large_addresses () =
  (* 8-byte addresses with high bits set *)
  let bytes =
    [
      (* unit_length: 12 = 2+1+1+8 *)
      0x0c; 0x00; 0x00; 0x00;
      (* version: 5 *)
      0x05; 0x00;
      (* address_size: 8 *)
      0x08;
      (* segment_selector_size: 0 *)
      0x00;
      (* addr: 0x7FFFFFFFFFFFFFFF *)
      0xFF; 0xFF; 0xFF; 0xFF; 0xFF; 0xFF; 0xFF; 0x7F;
    ]
  in
  let buffer = buffer_of_bytes bytes in
  let cursor = Object.Buffer.cursor buffer ~at:0 in
  let header = Dwarf.DebugAddr.parse_header cursor in
  let entries = Dwarf.DebugAddr.parse_entries cursor header in
  check int "one entry" 1 (Array.length entries);
  check int64 "large address"
    (Unsigned.UInt64.to_int64 entries.(0).address)
    0x7FFFFFFFFFFFFFFFL

let test_debug_addr_section_name () =
  check string "ELF debug_addr" ".debug_addr"
    (Dwarf.object_format_to_section_name Object_format.ELF Dwarf.Debug_addr);
  check string "MachO debug_addr" "__debug_addr"
    (Dwarf.object_format_to_section_name Object_format.MACHO Dwarf.Debug_addr)

let () =
  run "debug_addr"
    [
      ( "header",
        [
          test_case "DWARF32 header" `Quick test_debug_addr_header_dwarf32;
          test_case "4-byte address header" `Quick
            test_debug_addr_header_4byte_addr;
        ] );
      ( "entries",
        [
          test_case "single 8-byte entry" `Quick
            test_debug_addr_single_entry_8byte;
          test_case "multiple 4-byte entries" `Quick
            test_debug_addr_multiple_entries_4byte;
          test_case "empty entries" `Quick test_debug_addr_empty;
          test_case "combined parse" `Quick test_debug_addr_parse_combined;
          test_case "large addresses" `Quick test_debug_addr_large_addresses;
        ] );
      ( "section",
        [ test_case "section names" `Quick test_debug_addr_section_name ] );
    ]
