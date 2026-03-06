open Alcotest
open Durin

(* .debug_loc tests *)

let test_loc_resolve_succeeds binary_path =
  let buffer = Object.Buffer.parse binary_path in
  let result = Dwarf.resolve_location_list buffer Unsigned.UInt64.zero 8 in
  check bool "resolve succeeds" true (Option.is_some result)

let test_loc_has_entries binary_path =
  let buffer = Object.Buffer.parse binary_path in
  match Dwarf.resolve_location_list buffer Unsigned.UInt64.zero 8 with
  | None -> fail "expected location list"
  | Some entries -> check bool "has entries" true (List.length entries > 0)

let test_loc_has_end_of_list binary_path =
  let buffer = Object.Buffer.parse binary_path in
  match Dwarf.resolve_location_list buffer Unsigned.UInt64.zero 8 with
  | None -> fail "expected location list"
  | Some entries ->
      let has_eol =
        List.exists (fun e -> e = Dwarf.DebugLoc.EndOfList) entries
      in
      check bool "has EndOfList" true has_eol

let test_loc_has_location_entry binary_path =
  let buffer = Object.Buffer.parse binary_path in
  match Dwarf.resolve_location_list buffer Unsigned.UInt64.zero 8 with
  | None -> fail "expected location list"
  | Some entries ->
      let has_loc =
        List.exists
          (fun e ->
            match e with Dwarf.DebugLoc.Location _ -> true | _ -> false)
          entries
      in
      check bool "has Location entry" true has_loc

(* .debug_ranges tests *)

let test_ranges_resolve_succeeds binary_path =
  let buffer = Object.Buffer.parse binary_path in
  let result = Dwarf.resolve_range_list buffer Unsigned.UInt64.zero 8 in
  check bool "resolve succeeds" true (Option.is_some result)

let test_ranges_has_entries binary_path =
  let buffer = Object.Buffer.parse binary_path in
  match Dwarf.resolve_range_list buffer Unsigned.UInt64.zero 8 with
  | None -> fail "expected range list"
  | Some entries -> check bool "has entries" true (List.length entries > 0)

let test_ranges_has_end_of_list binary_path =
  let buffer = Object.Buffer.parse binary_path in
  match Dwarf.resolve_range_list buffer Unsigned.UInt64.zero 8 with
  | None -> fail "expected range list"
  | Some entries ->
      let has_eol =
        List.exists (fun e -> e = Dwarf.DebugRanges.EndOfList) entries
      in
      check bool "has EndOfList" true has_eol

let test_ranges_has_range_entry binary_path =
  let buffer = Object.Buffer.parse binary_path in
  match Dwarf.resolve_range_list buffer Unsigned.UInt64.zero 8 with
  | None -> fail "expected range list"
  | Some entries ->
      let has_range =
        List.exists
          (fun e ->
            match e with Dwarf.DebugRanges.Range _ -> true | _ -> false)
          entries
      in
      check bool "has Range entry" true has_range

let binary_path =
  let doc = "Path to DWARF 4 test binary with -O2" in
  Cmdliner.Arg.(
    required & opt (some file) None & info [ "binary"; "b" ] ~doc ~docv:"BINARY")

let () =
  run_with_args "debug_loc_ranges integration" binary_path
    [
      ( "debug_loc",
        [
          ("resolve succeeds", `Quick, test_loc_resolve_succeeds);
          ("has entries", `Quick, test_loc_has_entries);
          ("has EndOfList", `Quick, test_loc_has_end_of_list);
          ("has Location entry", `Quick, test_loc_has_location_entry);
        ] );
      ( "debug_ranges",
        [
          ("resolve succeeds", `Quick, test_ranges_resolve_succeeds);
          ("has entries", `Quick, test_ranges_has_entries);
          ("has EndOfList", `Quick, test_ranges_has_end_of_list);
          ("has Range entry", `Quick, test_ranges_has_range_entry);
        ] );
    ]
