open Alcotest
open Durin

let test_resolve_location_list binary_path =
  let buffer = Object.Buffer.parse binary_path in
  let result =
    Dwarf.resolve_location_list buffer (Unsigned.UInt64.of_int 0) 8
  in
  match result with
  | None -> fail "expected Some entries from .debug_loc"
  | Some entries ->
      check bool "has entries" true (List.length entries > 0);
      let has_end =
        List.exists
          (function Dwarf.DebugLoc.EndOfList -> true | _ -> false)
          entries
      in
      check bool "ends with EndOfList" true has_end;
      let has_location =
        List.exists
          (function Dwarf.DebugLoc.Location _ -> true | _ -> false)
          entries
      in
      check bool "contains at least one Location entry" true has_location

let test_resolve_location_list_offset binary_path =
  let buffer = Object.Buffer.parse binary_path in
  let result0 =
    Dwarf.resolve_location_list buffer (Unsigned.UInt64.of_int 0) 8
  in
  let result1 =
    Dwarf.resolve_location_list buffer (Unsigned.UInt64.of_int 0x5c) 8
  in
  (match result0 with
  | None -> fail "expected entries at offset 0"
  | Some _ -> ());
  match result1 with
  | None -> fail "expected entries at offset 0x5c"
  | Some entries ->
      check bool "offset 0x5c has entries" true (List.length entries > 0)

let test_resolve_location_list_values binary_path =
  let buffer = Object.Buffer.parse binary_path in
  match Dwarf.resolve_location_list buffer (Unsigned.UInt64.of_int 0) 8 with
  | None -> fail "expected Some"
  | Some entries ->
      List.iter
        (function
          | Dwarf.DebugLoc.Location { begin_addr; end_addr; expr } ->
              let b = Unsigned.UInt64.to_int64 begin_addr in
              let e = Unsigned.UInt64.to_int64 end_addr in
              check bool "begin <= end" true (b <= e);
              check bool "expr non-empty" true (String.length expr > 0)
          | Dwarf.DebugLoc.BaseAddress addr ->
              check bool "base addr nonzero" true
                (Unsigned.UInt64.to_int64 addr > 0L)
          | Dwarf.DebugLoc.EndOfList -> ())
        entries

let test_resolve_range_list binary_path =
  let buffer = Object.Buffer.parse binary_path in
  let result = Dwarf.resolve_range_list buffer (Unsigned.UInt64.of_int 0) 8 in
  match result with
  | None -> fail "expected Some entries from .debug_ranges"
  | Some entries ->
      check bool "has entries" true (List.length entries > 0);
      let has_end =
        List.exists
          (function Dwarf.DebugRanges.EndOfList -> true | _ -> false)
          entries
      in
      check bool "ends with EndOfList" true has_end;
      let has_range =
        List.exists
          (function Dwarf.DebugRanges.Range _ -> true | _ -> false)
          entries
      in
      check bool "contains at least one Range entry" true has_range

let test_resolve_range_list_values binary_path =
  let buffer = Object.Buffer.parse binary_path in
  match Dwarf.resolve_range_list buffer (Unsigned.UInt64.of_int 0) 8 with
  | None -> fail "expected Some"
  | Some entries ->
      List.iter
        (function
          | Dwarf.DebugRanges.Range { begin_addr; end_addr } ->
              let b = Unsigned.UInt64.to_int64 begin_addr in
              let e = Unsigned.UInt64.to_int64 end_addr in
              check bool "begin < end" true (b < e)
          | Dwarf.DebugRanges.BaseAddress addr ->
              check bool "base addr nonzero" true
                (Unsigned.UInt64.to_int64 addr > 0L)
          | Dwarf.DebugRanges.EndOfList -> ())
        entries

let test_resolve_range_list_multiple_offsets binary_path =
  let buffer = Object.Buffer.parse binary_path in
  let offsets = [ 0x00; 0x30; 0x60 ] in
  List.iter
    (fun off ->
      match Dwarf.resolve_range_list buffer (Unsigned.UInt64.of_int off) 8 with
      | None -> fail (Printf.sprintf "expected entries at offset 0x%x" off)
      | Some entries ->
          check bool
            (Printf.sprintf "offset 0x%x has entries" off)
            true
            (List.length entries > 0))
    offsets

let test_parse_type_units binary_path =
  let buffer = Object.Buffer.parse binary_path in
  let units = Dwarf.DebugTypes.parse_type_units buffer in
  let unit_list = List.of_seq units in
  check bool "found at least one type unit" true (List.length unit_list > 0);
  List.iter
    (fun (span, (header : Dwarf.DebugTypes.type_unit_header)) ->
      check int "version is 4" (Unsigned.UInt16.to_int header.version) 4;
      check int "address_size is 8"
        (Unsigned.UInt8.to_int header.address_size)
        8;
      check bool "format is DWARF32" true (header.format = Dwarf.DWARF32);
      check bool "unit_length > 0" true
        (Unsigned.UInt64.to_int64 header.unit_length > 0L);
      check bool "type_signature nonzero" true
        (Unsigned.UInt64.compare header.type_signature
           (Unsigned.UInt64.of_int 0)
        <> 0);
      check bool "type_offset > 0" true
        (Unsigned.UInt64.to_int header.type_offset > 0);
      check bool "span size > 0" true (Unsigned.UInt64.to_int64 span.size > 0L))
    unit_list

let test_parse_type_units_signature binary_path =
  let buffer = Object.Buffer.parse binary_path in
  let units = Dwarf.DebugTypes.parse_type_units buffer in
  let unit_list = List.of_seq units in
  let _span, header = List.hd unit_list in
  check int64 "type_signature matches dwarfdump"
    (Unsigned.UInt64.to_int64 header.type_signature)
    (Int64.of_string "0x060ef4604c886824");
  check int64 "abbrev_offset is 0"
    (Unsigned.UInt64.to_int64 header.debug_abbrev_offset)
    0L

let binary_path =
  let doc = "Path to DWARF 4 test binary" in
  Cmdliner.Arg.(
    required & opt (some file) None & info [ "binary"; "b" ] ~doc ~docv:"BINARY")

let () =
  run_with_args "DWARF 4 Integration" binary_path
    [
      ( "resolve_location_list",
        [
          ("parses .debug_loc entries", `Quick, test_resolve_location_list);
          ( "parses at different offsets",
            `Quick,
            test_resolve_location_list_offset );
          ( "location entry values valid",
            `Quick,
            test_resolve_location_list_values );
        ] );
      ( "resolve_range_list",
        [
          ("parses .debug_ranges entries", `Quick, test_resolve_range_list);
          ("range entry values valid", `Quick, test_resolve_range_list_values);
          ( "parses at multiple offsets",
            `Quick,
            test_resolve_range_list_multiple_offsets );
        ] );
      ( "parse_type_units",
        [
          ("iterates .debug_types", `Quick, test_parse_type_units);
          ("type signature matches", `Quick, test_parse_type_units_signature);
        ] );
    ]
