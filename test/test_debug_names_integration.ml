open Alcotest
open Durin

let find_section buffer name =
  let _header, sections = Object.Elf.read_elf buffer in
  Array.find_opt (fun (s : Object.Elf.section) -> s.sh_name_str = name) sections

let parse_names binary_path =
  let buffer = Object.Buffer.parse binary_path in
  match find_section buffer ".debug_names" with
  | None -> None
  | Some section ->
      let offset = Unsigned.UInt64.to_int section.sh_offset in
      let cursor = Object.Buffer.cursor buffer ~at:offset in
      Some (Dwarf.DebugNames.parse_debug_names_section cursor buffer)

let test_parse_succeeds binary_path =
  match parse_names binary_path with
  | None -> fail "expected .debug_names section"
  | Some _ -> ()

let test_header_valid binary_path =
  match parse_names binary_path with
  | None -> fail "expected .debug_names section"
  | Some names ->
      let h = names.header in
      check int "version is 5" 5 (Unsigned.UInt16.to_int h.version);
      check bool "format is DWARF32" true (h.format = Dwarf.DWARF32);
      check bool "unit_length > 0" true
        (Unsigned.UInt64.to_int64 h.unit_length > 0L);
      check bool "comp_unit_count > 0" true
        (Unsigned.UInt32.to_int h.comp_unit_count > 0);
      check bool "name_count > 0" true (Unsigned.UInt32.to_int h.name_count > 0);
      check bool "bucket_count > 0" true
        (Unsigned.UInt32.to_int h.bucket_count > 0)

let test_has_comp_unit_offsets binary_path =
  match parse_names binary_path with
  | None -> fail "expected .debug_names section"
  | Some names ->
      check bool "has CU offsets" true (Array.length names.comp_unit_offsets > 0)

let test_has_name_table binary_path =
  match parse_names binary_path with
  | None -> fail "expected .debug_names section"
  | Some names ->
      check bool "has name table entries" true
        (Array.length names.name_table > 0)

let test_contains_main binary_path =
  match parse_names binary_path with
  | None -> fail "expected .debug_names section"
  | Some names ->
      let has_main =
        Array.exists
          (fun (e : Dwarf.DebugNames.debug_str_entry) -> e.value = "main")
          names.name_table
      in
      check bool "name table contains main" true has_main

let test_has_abbreviations binary_path =
  match parse_names binary_path with
  | None -> fail "expected .debug_names section"
  | Some names ->
      check bool "has abbreviations" true
        (List.length names.abbreviation_table > 0)

let test_has_entry_pool binary_path =
  match parse_names binary_path with
  | None -> fail "expected .debug_names section"
  | Some names ->
      check bool "has entry pool entries" true
        (Array.length names.entry_pool > 0)

let binary_path =
  let doc = "Path to DWARF 5 test binary with -gpubnames" in
  Cmdliner.Arg.(
    required & opt (some file) None & info [ "binary"; "b" ] ~doc ~docv:"BINARY")

let () =
  run_with_args "debug_names integration" binary_path
    [
      ( "parse",
        [
          ("parse succeeds", `Quick, test_parse_succeeds);
          ("header valid", `Quick, test_header_valid);
        ] );
      ( "tables",
        [
          ("has CU offsets", `Quick, test_has_comp_unit_offsets);
          ("has name table", `Quick, test_has_name_table);
          ("contains main", `Quick, test_contains_main);
          ("has abbreviations", `Quick, test_has_abbreviations);
          ("has entry pool", `Quick, test_has_entry_pool);
        ] );
    ]
