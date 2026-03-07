open Alcotest
open Durin

let test_section_exists binary_path =
  match Test_helpers.find_section binary_path ".debug_macro" with
  | None -> fail "expected .debug_macro section"
  | Some (_, section) ->
      let size = Unsigned.UInt64.to_int section.sh_size in
      check bool "size > 0" true (size > 0)

let test_header binary_path =
  match Test_helpers.find_section binary_path ".debug_macro" with
  | None -> fail "expected .debug_macro section"
  | Some (buffer, section) ->
      let offset = Unsigned.UInt64.to_int section.sh_offset in
      let cur = Object.Buffer.cursor ~at:offset buffer in
      let header = Dwarf.parse_debug_macro_header cur in
      check int "version is 5" 5 (Unsigned.UInt16.to_int header.version);
      check bool "format is DWARF32" true (header.format = Dwarf.DWARF32);
      let flags = Unsigned.UInt8.to_int header.flags in
      check bool "debug_line_offset flag set" true (flags land 0x02 <> 0);
      check bool "debug_line_offset is Some" true
        (Option.is_some header.debug_line_offset)

let test_entries_valid binary_path =
  match Test_helpers.find_section binary_path ".debug_macro" with
  | None -> fail "expected .debug_macro section"
  | Some (buffer, section) ->
      let offset = Unsigned.UInt64.to_int section.sh_offset in
      let cur = Object.Buffer.cursor ~at:offset buffer in
      let unit = Dwarf.parse_debug_macro_unit cur in
      List.iter
        (fun (e : Dwarf.debug_macro_entry) ->
          match e.entry_type with
          | DW_MACRO_define_strp | DW_MACRO_undef_strp ->
              check bool "line_number present" true
                (Option.is_some e.line_number);
              check bool "string_offset present" true
                (Option.is_some e.string_offset)
          | DW_MACRO_start_file ->
              check bool "line_number present" true
                (Option.is_some e.line_number);
              check bool "file_index present" true (Option.is_some e.file_index)
          | DW_MACRO_end_file ->
              check bool "no line_number" true (Option.is_none e.line_number);
              check bool "no string_offset" true
                (Option.is_none e.string_offset);
              check bool "no file_index" true (Option.is_none e.file_index)
          | DW_MACRO_import ->
              check bool "string_offset present" true
                (Option.is_some e.string_offset)
          | _ -> ())
        unit.entries

let test_entry_types_present binary_path =
  match Test_helpers.find_section binary_path ".debug_macro" with
  | None -> fail "expected .debug_macro section"
  | Some (buffer, section) ->
      let offset = Unsigned.UInt64.to_int section.sh_offset in
      let cur = Object.Buffer.cursor ~at:offset buffer in
      let unit = Dwarf.parse_debug_macro_unit cur in
      let has_start_file =
        List.exists
          (fun (e : Dwarf.debug_macro_entry) ->
            e.entry_type = DW_MACRO_start_file)
          unit.entries
      in
      let has_end_file =
        List.exists
          (fun (e : Dwarf.debug_macro_entry) ->
            e.entry_type = DW_MACRO_end_file)
          unit.entries
      in
      let has_define_strp =
        List.exists
          (fun (e : Dwarf.debug_macro_entry) ->
            e.entry_type = DW_MACRO_define_strp)
          unit.entries
      in
      check bool "has DW_MACRO_start_file" true has_start_file;
      check bool "has DW_MACRO_end_file" true has_end_file;
      check bool "has DW_MACRO_define_strp" true has_define_strp

let test_full_section_traversal binary_path =
  match Test_helpers.find_section binary_path ".debug_macro" with
  | None -> fail "expected .debug_macro section"
  | Some (buffer, section) ->
      let offset = Unsigned.UInt64.to_int section.sh_offset in
      let size = Unsigned.UInt64.to_int section.sh_size in
      let cur = Object.Buffer.cursor ~at:offset buffer in
      let parsed = Dwarf.parse_debug_macro_section cur size in
      check bool "has units" true (List.length parsed.units > 0)

let binary_path =
  Test_helpers.binary_path ~doc:"Path to DWARF 5 macro test binary"

let () =
  run_with_args "debug_macro integration" binary_path
    [
      ("section", [ ("section exists", `Quick, test_section_exists) ]);
      ("header", [ ("header fields valid", `Quick, test_header) ]);
      ( "entries",
        [
          ("entry fields valid", `Quick, test_entries_valid);
          ("expected types present", `Quick, test_entry_types_present);
        ] );
      ( "full_section",
        [ ("full section traversal", `Quick, test_full_section_traversal) ] );
    ]
