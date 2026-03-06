open Alcotest
open Durin

let find_debug_line binary_path =
  let buffer = Object.Buffer.parse binary_path in
  let _header, sections = Object.Elf.read_elf buffer in
  let section_opt =
    Array.find_opt
      (fun (s : Object.Elf.section) -> s.sh_name_str = ".debug_line")
      sections
  in
  match section_opt with
  | None -> None
  | Some section ->
      let offset = Unsigned.UInt64.to_int section.sh_offset in
      Some (buffer, offset)

let test_header_parse_succeeds binary_path =
  match find_debug_line binary_path with
  | None -> fail "expected .debug_line section"
  | Some (buffer, offset) ->
      let cur = Object.Buffer.cursor ~at:offset buffer in
      let _header = Dwarf.DebugLine.parse_line_program_header cur buffer in
      ()

let test_header_fields_valid binary_path =
  match find_debug_line binary_path with
  | None -> fail "expected .debug_line section"
  | Some (buffer, offset) ->
      let cur = Object.Buffer.cursor ~at:offset buffer in
      let header = Dwarf.DebugLine.parse_line_program_header cur buffer in
      check int "version is 5" 5 (Unsigned.UInt16.to_int header.version);
      check int "address_size is 8" 8
        (Unsigned.UInt8.to_int header.address_size);
      check bool "format is DWARF32" true (header.format = Dwarf.DWARF32);
      check bool "minimum_instruction_length >= 1" true
        (Unsigned.UInt8.to_int header.minimum_instruction_length >= 1)

let test_header_has_files binary_path =
  match find_debug_line binary_path with
  | None -> fail "expected .debug_line section"
  | Some (buffer, offset) ->
      let cur = Object.Buffer.cursor ~at:offset buffer in
      let header = Dwarf.DebugLine.parse_line_program_header cur buffer in
      check bool "has file names" true (Array.length header.file_names > 0);
      let has_source =
        Array.exists
          (fun (f : Dwarf.DebugLine.file_entry) ->
            String.equal f.name "hello_world.c")
          header.file_names
      in
      check bool "contains hello_world.c" true has_source

let test_header_has_directories binary_path =
  match find_debug_line binary_path with
  | None -> fail "expected .debug_line section"
  | Some (buffer, offset) ->
      let cur = Object.Buffer.cursor ~at:offset buffer in
      let header = Dwarf.DebugLine.parse_line_program_header cur buffer in
      check bool "has directories" true (Array.length header.directories > 0)

let test_line_program_non_empty binary_path =
  match find_debug_line binary_path with
  | None -> fail "expected .debug_line section"
  | Some (buffer, offset) ->
      let cur = Object.Buffer.cursor ~at:offset buffer in
      let header = Dwarf.DebugLine.parse_line_program_header cur buffer in
      let entries = Dwarf.DebugLine.parse_line_program cur header in
      check bool "has line entries" true (List.length entries > 0)

let test_line_entries_valid binary_path =
  match find_debug_line binary_path with
  | None -> fail "expected .debug_line section"
  | Some (buffer, offset) ->
      let cur = Object.Buffer.cursor ~at:offset buffer in
      let header = Dwarf.DebugLine.parse_line_program_header cur buffer in
      let entries = Dwarf.DebugLine.parse_line_program cur header in
      List.iter
        (fun (e : Dwarf.DebugLine.line_table_entry) ->
          check bool "address > 0" true (Unsigned.UInt64.to_int64 e.address > 0L))
        entries

let test_has_end_sequence binary_path =
  match find_debug_line binary_path with
  | None -> fail "expected .debug_line section"
  | Some (buffer, offset) ->
      let cur = Object.Buffer.cursor ~at:offset buffer in
      let header = Dwarf.DebugLine.parse_line_program_header cur buffer in
      let entries = Dwarf.DebugLine.parse_line_program cur header in
      let has_end =
        List.exists
          (fun (e : Dwarf.DebugLine.line_table_entry) -> e.end_sequence)
          entries
      in
      check bool "has end_sequence" true has_end

let binary_path =
  let doc = "Path to DWARF 5 test binary" in
  Cmdliner.Arg.(
    required & opt (some file) None & info [ "binary"; "b" ] ~doc ~docv:"BINARY")

let () =
  run_with_args "debug_line integration" binary_path
    [
      ( "header",
        [
          ("parse succeeds", `Quick, test_header_parse_succeeds);
          ("header fields valid", `Quick, test_header_fields_valid);
          ("has files", `Quick, test_header_has_files);
          ("has directories", `Quick, test_header_has_directories);
        ] );
      ( "line_program",
        [
          ("non-empty", `Quick, test_line_program_non_empty);
          ("entries valid", `Quick, test_line_entries_valid);
          ("has end_sequence", `Quick, test_has_end_sequence);
        ] );
    ]
