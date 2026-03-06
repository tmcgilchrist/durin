open Alcotest
open Durin

let test_parse_succeeds binary_path =
  let buffer = Object.Buffer.parse binary_path in
  let units = Dwarf.DebugTypes.parse_type_units buffer in
  match Seq.uncons units with
  | None -> fail "expected at least one type unit"
  | Some _ -> ()

let test_header_valid binary_path =
  let buffer = Object.Buffer.parse binary_path in
  let units = Dwarf.DebugTypes.parse_type_units buffer in
  match Seq.uncons units with
  | None -> fail "expected at least one type unit"
  | Some ((_span, h), _) ->
      check int "version is 4" 4 (Unsigned.UInt16.to_int h.version);
      check int "address_size is 8" 8 (Unsigned.UInt8.to_int h.address_size);
      check bool "format is DWARF32" true (h.format = Dwarf.DWARF32);
      check bool "unit_length > 0" true
        (Unsigned.UInt64.to_int64 h.unit_length > 0L)

let test_type_signature_nonzero binary_path =
  let buffer = Object.Buffer.parse binary_path in
  let units = Dwarf.DebugTypes.parse_type_units buffer in
  match Seq.uncons units with
  | None -> fail "expected at least one type unit"
  | Some ((_span, h), _) ->
      check bool "type_signature nonzero" true
        (Unsigned.UInt64.to_int64 h.type_signature <> 0L)

let test_type_offset_valid binary_path =
  let buffer = Object.Buffer.parse binary_path in
  let units = Dwarf.DebugTypes.parse_type_units buffer in
  match Seq.uncons units with
  | None -> fail "expected at least one type unit"
  | Some ((_span, h), _) ->
      check bool "type_offset > 0" true
        (Unsigned.UInt64.to_int64 h.type_offset > 0L)

let test_span_valid binary_path =
  let buffer = Object.Buffer.parse binary_path in
  let units = Dwarf.DebugTypes.parse_type_units buffer in
  match Seq.uncons units with
  | None -> fail "expected at least one type unit"
  | Some ((span, _), _) ->
      check bool "span size > 0" true (Unsigned.UInt64.to_int64 span.size > 0L)

let binary_path =
  let doc = "Path to DWARF 4 test binary with -fdebug-types-section" in
  Cmdliner.Arg.(
    required & opt (some file) None & info [ "binary"; "b" ] ~doc ~docv:"BINARY")

let () =
  run_with_args "debug_types integration" binary_path
    [
      ( "type_units",
        [
          ("parse succeeds", `Quick, test_parse_succeeds);
          ("header valid", `Quick, test_header_valid);
          ("type signature nonzero", `Quick, test_type_signature_nonzero);
          ("type offset valid", `Quick, test_type_offset_valid);
          ("span valid", `Quick, test_span_valid);
        ] );
    ]
