open Alcotest
open Durin

let buffer_of_bytes bytes =
  let filename = Filename.temp_file "debug_macro_test_" ".bin" in
  let oc = open_out_bin filename in
  List.iter (fun b -> output_char oc (Char.chr b)) bytes;
  close_out oc;
  let buffer = Object.Buffer.parse filename in
  Sys.remove filename;
  buffer

(* ---- Header parsing ---- *)

let test_macro_header_no_flags () =
  (* debug_macro header per DWARF 5 Section 6.3.1:
     version: 2 bytes (5)
     flags: 1 byte (0x00 — DWARF32, no optional fields) *)
  let bytes = [ 0x05; 0x00; 0x00 ] in
  let buffer = buffer_of_bytes bytes in
  let cursor = Object.Buffer.cursor buffer ~at:0 in
  let header = Dwarf.DebugMacro.parse_header cursor in
  check int "version is 5" (Unsigned.UInt16.to_int header.version) 5;
  check int "flags is 0" (Unsigned.UInt8.to_int header.flags) 0;
  check bool "no debug_line_offset" true (header.debug_line_offset = None);
  check bool "no debug_str_offsets_offset" true
    (header.debug_str_offsets_offset = None);
  check bool "format is DWARF32" true (header.format = Dwarf.DWARF32)

let test_macro_header_with_line_offset () =
  (* flags = 0x02: bit 1 set = debug_line_offset present *)
  let bytes =
    [ 0x05; 0x00; 0x02; (* debug_line_offset: 0x100 *) 0x00; 0x01; 0x00; 0x00 ]
  in
  let buffer = buffer_of_bytes bytes in
  let cursor = Object.Buffer.cursor buffer ~at:0 in
  let header = Dwarf.DebugMacro.parse_header cursor in
  check int "flags is 2" (Unsigned.UInt8.to_int header.flags) 2;
  (match header.debug_line_offset with
  | Some off ->
      check int64 "debug_line_offset 0x100"
        (Unsigned.UInt64.to_int64 off)
        0x100L
  | None -> fail "expected debug_line_offset");
  check bool "no debug_str_offsets_offset" true
    (header.debug_str_offsets_offset = None)

let test_macro_header_with_both_offsets () =
  (* flags = 0x06: bit 1 + bit 2 = both offsets present *)
  let bytes =
    [
      0x05;
      0x00;
      0x06;
      (* debug_line_offset: 0x200 *)
      0x00;
      0x02;
      0x00;
      0x00;
      (* debug_str_offsets_offset: 0x300 *)
      0x00;
      0x03;
      0x00;
      0x00;
    ]
  in
  let buffer = buffer_of_bytes bytes in
  let cursor = Object.Buffer.cursor buffer ~at:0 in
  let header = Dwarf.DebugMacro.parse_header cursor in
  check int "flags is 6" (Unsigned.UInt8.to_int header.flags) 6;
  (match header.debug_line_offset with
  | Some off ->
      check int64 "debug_line_offset 0x200"
        (Unsigned.UInt64.to_int64 off)
        0x200L
  | None -> fail "expected debug_line_offset");
  match header.debug_str_offsets_offset with
  | Some off ->
      check int64 "str_offsets_offset 0x300"
        (Unsigned.UInt64.to_int64 off)
        0x300L
  | None -> fail "expected debug_str_offsets_offset"

(* ---- Entry parsing ---- *)

let test_macro_entry_define () =
  (* DW_MACRO_define (0x01): line=5 (uleb128),
     null-terminated string "foo" *)
  let bytes = [ 0x01; 0x05; 0x66; 0x6f; 0x6f; 0x00 ] in
  let buffer = buffer_of_bytes bytes in
  let cursor = Object.Buffer.cursor buffer ~at:0 in
  let entry = Dwarf.DebugMacro.parse_entry cursor Dwarf.DWARF32 in
  match entry with
  | Some e -> (
      check string "type is DW_MACRO_define" "DW_MACRO_define"
        (Dwarf.string_of_macro_info_entry_type e.entry_type);
      (match e.line_number with
      | Some ln -> check int "line is 5" 5 (Unsigned.UInt32.to_int ln)
      | None -> fail "expected line_number");
      match e.string_value with
      | Some s -> check string "string is foo" "foo" s
      | None -> fail "expected string_value")
  | None -> fail "expected entry"

let test_macro_entry_undef () =
  (* DW_MACRO_undef (0x02): line=3,
     null-terminated string "X" *)
  let bytes = [ 0x02; 0x03; 0x58; 0x00 ] in
  let buffer = buffer_of_bytes bytes in
  let cursor = Object.Buffer.cursor buffer ~at:0 in
  let entry = Dwarf.DebugMacro.parse_entry cursor Dwarf.DWARF32 in
  match entry with
  | Some e -> (
      check string "type is DW_MACRO_undef" "DW_MACRO_undef"
        (Dwarf.string_of_macro_info_entry_type e.entry_type);
      match e.line_number with
      | Some ln -> check int "line is 3" 3 (Unsigned.UInt32.to_int ln)
      | None -> fail "expected line_number")
  | None -> fail "expected entry"

let test_macro_entry_start_file () =
  (* DW_MACRO_start_file (0x03): line=1, file_index=2 *)
  let bytes = [ 0x03; 0x01; 0x02 ] in
  let buffer = buffer_of_bytes bytes in
  let cursor = Object.Buffer.cursor buffer ~at:0 in
  let entry = Dwarf.DebugMacro.parse_entry cursor Dwarf.DWARF32 in
  match entry with
  | Some e -> (
      check string "type is DW_MACRO_start_file" "DW_MACRO_start_file"
        (Dwarf.string_of_macro_info_entry_type e.entry_type);
      (match e.line_number with
      | Some ln -> check int "line is 1" 1 (Unsigned.UInt32.to_int ln)
      | None -> fail "expected line_number");
      match e.file_index with
      | Some fi -> check int "file_index is 2" 2 (Unsigned.UInt32.to_int fi)
      | None -> fail "expected file_index")
  | None -> fail "expected entry"

let test_macro_entry_end_file () =
  (* DW_MACRO_end_file (0x04) *)
  let bytes = [ 0x04 ] in
  let buffer = buffer_of_bytes bytes in
  let cursor = Object.Buffer.cursor buffer ~at:0 in
  let entry = Dwarf.DebugMacro.parse_entry cursor Dwarf.DWARF32 in
  match entry with
  | Some e ->
      check string "type is DW_MACRO_end_file" "DW_MACRO_end_file"
        (Dwarf.string_of_macro_info_entry_type e.entry_type);
      check bool "no line_number" true (e.line_number = None);
      check bool "no file_index" true (e.file_index = None)
  | None -> fail "expected entry"

let test_macro_entry_define_strp () =
  (* DW_MACRO_define_strp (0x05): line=10,
     string_offset=0x20 (DWARF32 = 4 bytes) *)
  let bytes = [ 0x05; 0x0a; 0x20; 0x00; 0x00; 0x00 ] in
  let buffer = buffer_of_bytes bytes in
  let cursor = Object.Buffer.cursor buffer ~at:0 in
  let entry = Dwarf.DebugMacro.parse_entry cursor Dwarf.DWARF32 in
  match entry with
  | Some e -> (
      check string "type is DW_MACRO_define_strp" "DW_MACRO_define_strp"
        (Dwarf.string_of_macro_info_entry_type e.entry_type);
      (match e.line_number with
      | Some ln -> check int "line is 10" 10 (Unsigned.UInt32.to_int ln)
      | None -> fail "expected line_number");
      match e.string_offset with
      | Some off ->
          check int64 "string_offset 0x20" (Unsigned.UInt64.to_int64 off) 0x20L
      | None -> fail "expected string_offset")
  | None -> fail "expected entry"

let test_macro_entry_import () =
  (* DW_MACRO_import (0x07): offset=0x50 (DWARF32 = 4 bytes) *)
  let bytes = [ 0x07; 0x50; 0x00; 0x00; 0x00 ] in
  let buffer = buffer_of_bytes bytes in
  let cursor = Object.Buffer.cursor buffer ~at:0 in
  let entry = Dwarf.DebugMacro.parse_entry cursor Dwarf.DWARF32 in
  match entry with
  | Some e -> (
      check string "type is DW_MACRO_import" "DW_MACRO_import"
        (Dwarf.string_of_macro_info_entry_type e.entry_type);
      check bool "no line_number" true (e.line_number = None);
      match e.string_offset with
      | Some off ->
          check int64 "offset 0x50" (Unsigned.UInt64.to_int64 off) 0x50L
      | None -> fail "expected string_offset (used for import offset)")
  | None -> fail "expected entry"

let test_macro_entry_define_strx () =
  (* DW_MACRO_define_strx (0x0b): line=1, string_offset=3 (uleb128) *)
  let bytes = [ 0x0b; 0x01; 0x03 ] in
  let buffer = buffer_of_bytes bytes in
  let cursor = Object.Buffer.cursor buffer ~at:0 in
  let entry = Dwarf.DebugMacro.parse_entry cursor Dwarf.DWARF32 in
  match entry with
  | Some e -> (
      check string "type is DW_MACRO_define_strx" "DW_MACRO_define_strx"
        (Dwarf.string_of_macro_info_entry_type e.entry_type);
      (match e.line_number with
      | Some ln -> check int "line is 1" 1 (Unsigned.UInt32.to_int ln)
      | None -> fail "expected line_number");
      match e.string_offset with
      | Some off ->
          check int64 "string_offset 3" (Unsigned.UInt64.to_int64 off) 3L
      | None -> fail "expected string_offset")
  | None -> fail "expected entry"

let test_macro_entry_terminator () =
  (* 0x00 = end of entries *)
  let bytes = [ 0x00 ] in
  let buffer = buffer_of_bytes bytes in
  let cursor = Object.Buffer.cursor buffer ~at:0 in
  let entry = Dwarf.DebugMacro.parse_entry cursor Dwarf.DWARF32 in
  check bool "terminator returns None" true (entry = None)

(* ---- Unit parsing ---- *)

let test_macro_unit_define_and_end () =
  (* A complete macro unit:
     version(2) + flags(1) + no optional offsets
     + DW_MACRO_define (line=1, string="x\0")
     + terminator *)
  let bytes =
    [
      (* version: 5 *)
      0x05;
      0x00;
      (* flags: 0 *)
      0x00;
      (* DW_MACRO_define: line=1, string "x\0" *)
      0x01;
      0x01;
      0x78;
      0x00;
      (* terminator *)
      0x00;
    ]
  in
  let buffer = buffer_of_bytes bytes in
  let cursor = Object.Buffer.cursor buffer ~at:0 in
  let unit = Dwarf.DebugMacro.parse_unit cursor in
  check int "version is 5" (Unsigned.UInt16.to_int unit.header.version) 5;
  check int "one entry" 1 (List.length unit.entries);
  let e = List.hd unit.entries in
  check string "type is DW_MACRO_define" "DW_MACRO_define"
    (Dwarf.string_of_macro_info_entry_type e.entry_type)

let test_macro_unit_start_end_file () =
  (* A macro unit with start_file + end_file *)
  let bytes =
    [
      (* version: 5 *)
      0x05;
      0x00;
      (* flags: 0 *)
      0x00;
      (* DW_MACRO_start_file: line=1, file=0 *)
      0x03;
      0x01;
      0x00;
      (* DW_MACRO_end_file *)
      0x04;
      (* terminator *)
      0x00;
    ]
  in
  let buffer = buffer_of_bytes bytes in
  let cursor = Object.Buffer.cursor buffer ~at:0 in
  let unit = Dwarf.DebugMacro.parse_unit cursor in
  check int "two entries" 2 (List.length unit.entries);
  let first = List.nth unit.entries 0 in
  check string "first is start_file" "DW_MACRO_start_file"
    (Dwarf.string_of_macro_info_entry_type first.entry_type);
  let second = List.nth unit.entries 1 in
  check string "second is end_file" "DW_MACRO_end_file"
    (Dwarf.string_of_macro_info_entry_type second.entry_type)

let test_macro_unit_empty () =
  (* A macro unit with only a terminator *)
  let bytes =
    [ (* version: 5 *) 0x05; 0x00; (* flags: 0 *) 0x00; (* terminator *) 0x00 ]
  in
  let buffer = buffer_of_bytes bytes in
  let cursor = Object.Buffer.cursor buffer ~at:0 in
  let unit = Dwarf.DebugMacro.parse_unit cursor in
  check int "no entries" 0 (List.length unit.entries)

(* ---- Type conversion tests ---- *)

let test_macro_entry_type_strings () =
  check string "DW_MACRO_define" "DW_MACRO_define"
    (Dwarf.string_of_macro_info_entry_type Dwarf.DW_MACRO_define);
  check string "DW_MACRO_undef" "DW_MACRO_undef"
    (Dwarf.string_of_macro_info_entry_type Dwarf.DW_MACRO_undef);
  check string "DW_MACRO_start_file" "DW_MACRO_start_file"
    (Dwarf.string_of_macro_info_entry_type Dwarf.DW_MACRO_start_file);
  check string "DW_MACRO_end_file" "DW_MACRO_end_file"
    (Dwarf.string_of_macro_info_entry_type Dwarf.DW_MACRO_end_file);
  check string "DW_MACRO_define_strp" "DW_MACRO_define_strp"
    (Dwarf.string_of_macro_info_entry_type Dwarf.DW_MACRO_define_strp);
  check string "DW_MACRO_import" "DW_MACRO_import"
    (Dwarf.string_of_macro_info_entry_type Dwarf.DW_MACRO_import);
  check string "DW_MACRO_define_strx" "DW_MACRO_define_strx"
    (Dwarf.string_of_macro_info_entry_type Dwarf.DW_MACRO_define_strx)

let test_macro_entry_type_of_u8 () =
  let check_type byte expected_str =
    let t = Dwarf.macro_info_entry_type_of_u8 (Unsigned.UInt8.of_int byte) in
    check string
      (Printf.sprintf "byte 0x%02x" byte)
      expected_str
      (Dwarf.string_of_macro_info_entry_type t)
  in
  check_type 0x01 "DW_MACRO_define";
  check_type 0x02 "DW_MACRO_undef";
  check_type 0x03 "DW_MACRO_start_file";
  check_type 0x04 "DW_MACRO_end_file";
  check_type 0x05 "DW_MACRO_define_strp";
  check_type 0x06 "DW_MACRO_undef_strp";
  check_type 0x07 "DW_MACRO_import";
  check_type 0x08 "DW_MACRO_define_sup";
  check_type 0x09 "DW_MACRO_undef_sup";
  check_type 0x0a "DW_MACRO_import_sup";
  check_type 0x0b "DW_MACRO_define_strx";
  check_type 0x0c "DW_MACRO_undef_strx"

let test_macro_section_name () =
  check string "ELF debug_macro" ".debug_macro"
    (Dwarf.object_format_to_section_name Object_format.ELF Dwarf.Debug_macro);
  check string "MachO debug_macro" "__debug_macro"
    (Dwarf.object_format_to_section_name Object_format.MACHO Dwarf.Debug_macro)

let () =
  run "debug_macro"
    [
      ( "header",
        [
          test_case "header with no flags" `Quick test_macro_header_no_flags;
          test_case "header with line offset" `Quick
            test_macro_header_with_line_offset;
          test_case "header with both offsets" `Quick
            test_macro_header_with_both_offsets;
        ] );
      ( "entries",
        [
          test_case "DW_MACRO_define" `Quick test_macro_entry_define;
          test_case "DW_MACRO_undef" `Quick test_macro_entry_undef;
          test_case "DW_MACRO_start_file" `Quick test_macro_entry_start_file;
          test_case "DW_MACRO_end_file" `Quick test_macro_entry_end_file;
          test_case "DW_MACRO_define_strp" `Quick test_macro_entry_define_strp;
          test_case "DW_MACRO_import" `Quick test_macro_entry_import;
          test_case "DW_MACRO_define_strx" `Quick test_macro_entry_define_strx;
          test_case "terminator" `Quick test_macro_entry_terminator;
        ] );
      ( "unit",
        [
          test_case "define and end" `Quick test_macro_unit_define_and_end;
          test_case "start_file end_file" `Quick test_macro_unit_start_end_file;
          test_case "empty unit" `Quick test_macro_unit_empty;
        ] );
      ( "types",
        [
          test_case "entry type strings" `Quick test_macro_entry_type_strings;
          test_case "entry type of u8" `Quick test_macro_entry_type_of_u8;
          test_case "section name" `Quick test_macro_section_name;
        ] );
    ]
