open Alcotest
open Durin

let parse_aranges binary_path =
  let buffer = Object.Buffer.parse binary_path in
  Dwarf.DebugAranges.parse buffer

let test_parse_succeeds binary_path =
  match parse_aranges binary_path with
  | None -> fail "expected DebugAranges.parse to return Some"
  | Some _ -> ()

let test_header_valid binary_path =
  match parse_aranges binary_path with
  | None -> fail "expected DebugAranges.parse to return Some"
  | Some set ->
      let h = set.header in
      check int "version is 2" 2 (Unsigned.UInt16.to_int h.version);
      check int "address_size is 8" 8 (Unsigned.UInt8.to_int h.address_size);
      check int "segment_size is 0" 0 (Unsigned.UInt8.to_int h.segment_size);
      check bool "format is DWARF32" true (h.format = Dwarf.DWARF32)

let test_ranges_non_empty binary_path =
  match parse_aranges binary_path with
  | None -> fail "expected DebugAranges.parse to return Some"
  | Some set -> check bool "ranges non-empty" true (List.length set.ranges > 0)

let test_range_values_valid binary_path =
  match parse_aranges binary_path with
  | None -> fail "expected DebugAranges.parse to return Some"
  | Some set ->
      List.iter
        (fun (r : Dwarf.DebugAranges.address_range) ->
          check bool "start_address > 0" true
            (Unsigned.UInt64.to_int64 r.start_address > 0L);
          check bool "length > 0" true (Unsigned.UInt64.to_int64 r.length > 0L))
        set.ranges

let test_debug_info_offset binary_path =
  match parse_aranges binary_path with
  | None -> fail "expected DebugAranges.parse to return Some"
  | Some set ->
      check bool "debug_info_offset < unit_length" true
        (set.header.debug_info_offset < set.header.unit_length)

let binary_path =
  let doc = "Path to DWARF 5 test binary" in
  Cmdliner.Arg.(
    required & opt (some file) None & info [ "binary"; "b" ] ~doc ~docv:"BINARY")

let () =
  run_with_args "debug_aranges integration" binary_path
    [
      ("parse", [ ("parse succeeds", `Quick, test_parse_succeeds) ]);
      ("header", [ ("header fields valid", `Quick, test_header_valid) ]);
      ( "ranges",
        [
          ("ranges non-empty", `Quick, test_ranges_non_empty);
          ("range values valid", `Quick, test_range_values_valid);
        ] );
      ( "debug_info_offset",
        [ ("debug_info_offset is 0", `Quick, test_debug_info_offset) ] );
    ]
