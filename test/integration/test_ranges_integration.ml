open Alcotest
open Durin

(* Exercises attr_ranges / attr_locations on the non-contiguous fixture. On macOS
   the DWARF lives in the .dSYM bundle; on Linux it is embedded in the ELF. *)
let dwarf_file binary_path =
  let dsym =
    binary_path ^ ".dSYM/Contents/Resources/DWARF/"
    ^ Filename.basename binary_path
  in
  if Sys.file_exists dsym then dsym else binary_path

let context binary_path =
  Dwarf.create (Object.Buffer.parse (dwarf_file binary_path))

let has_attr attr (die : Dwarf.DIE.t) =
  Option.is_some (Dwarf.DIE.find_attribute die attr)

(* First (unit handle, DIE) across all units whose DIE satisfies [pred]. *)
let first_die pred ctx =
  Seq.find_map
    (fun cu ->
      let u = Dwarf.unit ctx cu in
      match Dwarf.root_die u with
      | None -> None
      | Some root ->
          if pred root then Some (u, root)
          else
            Option.map (fun d -> (u, d)) (Dwarf.DIE.find_descendant pred root))
    (Dwarf.compile_units ctx)

(* The compile unit is non-contiguous (a cold path in its own section), so a DIE
   carries DW_AT_ranges resolving to more than one range. *)
let test_attr_ranges_noncontiguous binary_path =
  let ctx = context binary_path in
  match first_die (has_attr Dwarf.DW_AT_ranges) ctx with
  | None -> fail "expected a DIE with DW_AT_ranges"
  | Some (u, die) -> (
      match Dwarf.attr_ranges u die with
      | None -> fail "expected attr_ranges to resolve"
      | Some ranges ->
          check bool "more than one range (non-contiguous)" true
            (List.length ranges >= 2);
          List.iter
            (fun (r : Dwarf.range) ->
              check bool "range is non-empty" true
                (Unsigned.UInt64.compare r.start r.stop < 0))
            ranges)

let is_loclist (die : Dwarf.DIE.t) =
  match Dwarf.DIE.find_attribute die Dwarf.DW_AT_location with
  | Some (Dwarf.DIE.UData _) | Some (Dwarf.DIE.LoclistIndex _) -> true
  | _ -> false

(* A variable whose DW_AT_location is a location list resolves to entries with
   ranges and non-empty DWARF expressions. *)
let test_attr_locations binary_path =
  let ctx = context binary_path in
  match first_die is_loclist ctx with
  | None -> fail "expected a DIE with a location list"
  | Some (u, die) -> (
      match Dwarf.attr_locations u die with
      | None -> fail "expected attr_locations to resolve"
      | Some locs ->
          check bool "at least one location entry" true (locs <> []);
          check bool "every entry has a DWARF expression" true
            (List.for_all
               (fun (l : Dwarf.location) -> String.length l.expr > 0)
               locs);
          check bool "at least one entry has a range" true
            (List.exists
               (fun (l : Dwarf.location) -> Option.is_some l.range)
               locs))

let binary_path = Test_helpers.binary_path ~doc:"Path to a DWARF 5 test binary"

let () =
  run_with_args "ranges/locations integration" binary_path
    [
      ( "ranges",
        [
          ("attr_ranges non-contiguous", `Quick, test_attr_ranges_noncontiguous);
        ] );
      ("locations", [ ("attr_locations", `Quick, test_attr_locations) ]);
    ]
