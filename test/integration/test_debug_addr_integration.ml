open Alcotest
open Durin

let find_debug_addr binary_path =
  match Test_helpers.find_section binary_path ".debug_addr" with
  | None -> None
  | Some (buffer, section) ->
      let offset = Unsigned.UInt64.to_int section.sh_offset in
      Some (buffer, offset)

let test_parse_succeeds binary_path =
  match find_debug_addr binary_path with
  | None -> fail "expected .debug_addr section"
  | Some (buffer, offset) ->
      let t = Dwarf.DebugAddr.parse buffer (Unsigned.UInt64.of_int offset) in
      check bool "has entries" true (Array.length t.entries > 0)

let test_header_valid binary_path =
  match find_debug_addr binary_path with
  | None -> fail "expected .debug_addr section"
  | Some (buffer, offset) ->
      let t = Dwarf.DebugAddr.parse buffer (Unsigned.UInt64.of_int offset) in
      let h = t.header in
      check int "version is 5" 5 (Unsigned.UInt16.to_int h.version);
      check int "address_size is 8" 8 (Unsigned.UInt8.to_int h.address_size);
      check int "segment_selector_size is 0" 0
        (Unsigned.UInt8.to_int h.segment_selector_size);
      check bool "format is DWARF32" true (h.format = Dwarf.DWARF32);
      check bool "unit_length > 0" true
        (Unsigned.UInt64.to_int64 h.unit_length > 0L)

let test_addresses_nonzero binary_path =
  match find_debug_addr binary_path with
  | None -> fail "expected .debug_addr section"
  | Some (buffer, offset) ->
      let t = Dwarf.DebugAddr.parse buffer (Unsigned.UInt64.of_int offset) in
      Array.iteri
        (fun i (e : Dwarf.DebugAddr.entry) ->
          check bool
            (Printf.sprintf "address[%d] > 0" i)
            true
            (Unsigned.UInt64.to_int64 e.address > 0L))
        t.entries

let test_no_segments binary_path =
  match find_debug_addr binary_path with
  | None -> fail "expected .debug_addr section"
  | Some (buffer, offset) ->
      let t = Dwarf.DebugAddr.parse buffer (Unsigned.UInt64.of_int offset) in
      Array.iteri
        (fun i (e : Dwarf.DebugAddr.entry) ->
          check bool
            (Printf.sprintf "segment[%d] is None" i)
            true (Option.is_none e.segment))
        t.entries

let binary_path = Test_helpers.binary_path ~doc:"Path to DWARF 5 test binary"

let () =
  run_with_args "debug_addr integration" binary_path
    [
      ("parse", [ ("parse succeeds", `Quick, test_parse_succeeds) ]);
      ("header", [ ("header fields valid", `Quick, test_header_valid) ]);
      ( "entries",
        [
          ("addresses nonzero", `Quick, test_addresses_nonzero);
          ("no segments", `Quick, test_no_segments);
        ] );
    ]
