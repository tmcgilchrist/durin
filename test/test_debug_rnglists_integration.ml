open Alcotest
open Durin

let parse_rnglists binary_path =
  let buffer = Object.Buffer.parse binary_path in
  Dwarf.DebugRnglists.parse buffer

let test_parse_succeeds binary_path =
  match parse_rnglists binary_path with
  | None -> fail "expected DebugRnglists.parse to return Some"
  | Some _ -> ()

let test_header_valid binary_path =
  match parse_rnglists binary_path with
  | None -> fail "expected DebugRnglists.parse to return Some"
  | Some section ->
      let h = section.header in
      check int "version is 5" 5 (Unsigned.UInt16.to_int h.version);
      check int "address_size is 8" 8 (Unsigned.UInt8.to_int h.address_size);
      check int "segment_size is 0" 0 (Unsigned.UInt8.to_int h.segment_size);
      check bool "format is DWARF32" true (h.format = Dwarf.DWARF32)

let test_unit_length_positive binary_path =
  match parse_rnglists binary_path with
  | None -> fail "expected DebugRnglists.parse to return Some"
  | Some section ->
      check bool "unit_length > 0" true
        (Unsigned.UInt64.to_int64 section.header.unit_length > 0L)

let test_offset_table binary_path =
  match parse_rnglists binary_path with
  | None -> fail "expected DebugRnglists.parse to return Some"
  | Some section ->
      let count = Unsigned.UInt32.to_int section.header.offset_entry_count in
      check int "offset_table length matches count" count
        (Array.length section.offset_table)

let test_resolve_first_list binary_path =
  let buffer = Object.Buffer.parse binary_path in
  match Dwarf.DebugRnglists.parse buffer with
  | None -> fail "expected DebugRnglists.parse to return Some"
  | Some section ->
      if Array.length section.offset_table > 0 then
        let offset = section.offset_table.(0) in
        let addr_size = section.header.address_size in
        let result =
          Dwarf.DebugRnglists.resolve_range_list buffer offset addr_size
        in
        check bool "resolve_range_list returns Some" true
          (Option.is_some result)

let binary_path =
  let doc = "Path to DWARF 5 test binary" in
  Cmdliner.Arg.(
    required & opt (some file) None & info [ "binary"; "b" ] ~doc ~docv:"BINARY")

let () =
  run_with_args "debug_rnglists integration" binary_path
    [
      ("parse", [ ("parse succeeds", `Quick, test_parse_succeeds) ]);
      ( "header",
        [
          ("header fields valid", `Quick, test_header_valid);
          ("unit_length positive", `Quick, test_unit_length_positive);
        ] );
      ( "offset_table",
        [ ("offset table matches count", `Quick, test_offset_table) ] );
      ( "resolve",
        [ ("resolve first range list", `Quick, test_resolve_first_list) ] );
    ]
