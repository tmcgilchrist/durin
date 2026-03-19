open Alcotest
open Durin

let test_pubnames_parse_succeeds binary_path =
  match Test_helpers.find_section binary_path ".debug_pubnames" with
  | None -> fail "expected .debug_pubnames section"
  | Some (buffer, section) ->
      let offset = Unsigned.UInt64.to_int section.sh_offset in
      let cursor = Object.Buffer.cursor buffer ~at:offset in
      let header, entries = Dwarf.DebugPubnames.parse_set cursor in
      check int "version is 2" 2 (Unsigned.UInt16.to_int header.version);
      check bool "has entries" true (List.length entries > 0)

let test_pubnames_header_valid binary_path =
  match Test_helpers.find_section binary_path ".debug_pubnames" with
  | None -> fail "expected .debug_pubnames section"
  | Some (buffer, section) ->
      let offset = Unsigned.UInt64.to_int section.sh_offset in
      let cursor = Object.Buffer.cursor buffer ~at:offset in
      let header, _ = Dwarf.DebugPubnames.parse_set cursor in
      check bool "format is DWARF32" true (header.format = Dwarf.DWARF32);
      check bool "unit_length > 0" true
        (Unsigned.UInt64.to_int64 header.unit_length > 0L);
      check bool "debug_info_length > 0" true
        (Unsigned.UInt64.to_int64 header.debug_info_length > 0L)

let test_pubnames_contains_main binary_path =
  match Test_helpers.find_section binary_path ".debug_pubnames" with
  | None -> fail "expected .debug_pubnames section"
  | Some (buffer, section) ->
      let offset = Unsigned.UInt64.to_int section.sh_offset in
      let cursor = Object.Buffer.cursor buffer ~at:offset in
      let _, entries = Dwarf.DebugPubnames.parse_set cursor in
      let has_main =
        List.exists
          (fun (e : Dwarf.DebugPubnames.entry) -> e.name = "main")
          entries
      in
      check bool "contains main" true has_main

let test_pubtypes_parse_succeeds binary_path =
  match Test_helpers.find_section binary_path ".debug_pubtypes" with
  | None -> fail "expected .debug_pubtypes section"
  | Some (buffer, section) ->
      let offset = Unsigned.UInt64.to_int section.sh_offset in
      let cursor = Object.Buffer.cursor buffer ~at:offset in
      let header, entries = Dwarf.DebugPubtypes.parse_set cursor in
      check int "version is 2" 2 (Unsigned.UInt16.to_int header.version);
      check bool "has entries" true (List.length entries > 0)

let test_pubtypes_header_valid binary_path =
  match Test_helpers.find_section binary_path ".debug_pubtypes" with
  | None -> fail "expected .debug_pubtypes section"
  | Some (buffer, section) ->
      let offset = Unsigned.UInt64.to_int section.sh_offset in
      let cursor = Object.Buffer.cursor buffer ~at:offset in
      let header, _ = Dwarf.DebugPubtypes.parse_set cursor in
      check bool "format is DWARF32" true (header.format = Dwarf.DWARF32);
      check bool "unit_length > 0" true
        (Unsigned.UInt64.to_int64 header.unit_length > 0L);
      check bool "debug_info_length > 0" true
        (Unsigned.UInt64.to_int64 header.debug_info_length > 0L)

let test_pubtypes_contains_int binary_path =
  match Test_helpers.find_section binary_path ".debug_pubtypes" with
  | None -> fail "expected .debug_pubtypes section"
  | Some (buffer, section) ->
      let offset = Unsigned.UInt64.to_int section.sh_offset in
      let cursor = Object.Buffer.cursor buffer ~at:offset in
      let _, entries = Dwarf.DebugPubtypes.parse_set cursor in
      let has_int =
        List.exists
          (fun (e : Dwarf.DebugPubtypes.entry) -> e.name = "int")
          entries
      in
      check bool "contains int" true has_int

let binary_path =
  Test_helpers.binary_path ~doc:"Path to DWARF 4 test binary with -gpubnames"

let () =
  run_with_args "debug_pubnames integration" binary_path
    [
      ( "pubnames",
        [
          ("parse succeeds", `Quick, test_pubnames_parse_succeeds);
          ("header valid", `Quick, test_pubnames_header_valid);
          ("contains main", `Quick, test_pubnames_contains_main);
        ] );
      ( "pubtypes",
        [
          ("parse succeeds", `Quick, test_pubtypes_parse_succeeds);
          ("header valid", `Quick, test_pubtypes_header_valid);
          ("contains int", `Quick, test_pubtypes_contains_int);
        ] );
    ]
