open Alcotest
open Durin

(* Exercises the [Dwarf.line_table] context accessor on a real binary. On macOS
   the DWARF lives in the .dSYM bundle; on Linux it is embedded in the ELF, so
   resolve whichever file actually holds the debug sections. *)
let dwarf_file binary_path =
  let dsym =
    binary_path ^ ".dSYM/Contents/Resources/DWARF/"
    ^ Filename.basename binary_path
  in
  if Sys.file_exists dsym then dsym else binary_path

let context binary_path =
  Dwarf.create (Object.Buffer.parse (dwarf_file binary_path))

(* First compilation unit that has a line table, paired with the table. *)
let first_line_table ctx =
  Seq.fold_left
    (fun acc cu ->
      match acc with
      | Some _ -> acc
      | None -> Option.map (fun lt -> (cu, lt)) (Dwarf.line_table ctx cu))
    None (Dwarf.compile_units ctx)

(* A real (non-terminal) code address taken from the first line table. *)
let first_code_address ctx =
  match first_line_table ctx with
  | None -> None
  | Some (_cu, lt) ->
      Dwarf.DebugLine.entries lt |> List.of_seq
      |> List.find_opt (fun (e : Dwarf.DebugLine.line_table_entry) ->
          (not e.end_sequence) && Unsigned.UInt64.to_int e.address > 0)
      |> Option.map (fun (e : Dwarf.DebugLine.line_table_entry) -> e.address)

let test_line_table_present binary_path =
  let ctx = context binary_path in
  match first_line_table ctx with
  | None -> fail "expected a compilation unit with a line table"
  | Some (_cu, lt) ->
      let header = Dwarf.DebugLine.header lt in
      let version = Unsigned.UInt16.to_int header.version in
      check bool "version is 4 or 5" true (version = 4 || version = 5);
      check bool "has file names" true (Array.length header.file_names > 0);
      let rows = Dwarf.DebugLine.entries lt |> List.of_seq in
      check bool "has rows" true (List.length rows > 0);
      check bool "has an end_sequence row" true
        (List.exists
           (fun (e : Dwarf.DebugLine.line_table_entry) -> e.end_sequence)
           rows)

let test_find_by_address_roundtrip binary_path =
  let ctx = context binary_path in
  match first_line_table ctx with
  | None -> fail "expected a compilation unit with a line table"
  | Some (_cu, lt) -> (
      let rows = Dwarf.DebugLine.entries lt |> List.of_seq in
      match
        List.find_opt
          (fun (e : Dwarf.DebugLine.line_table_entry) ->
            (not e.end_sequence) && Unsigned.UInt64.to_int e.address > 0)
          rows
      with
      | None -> fail "expected a real (non-terminal) row"
      | Some row -> (
          match Dwarf.DebugLine.find_by_address lt row.address with
          | None -> fail "row address did not resolve"
          | Some found ->
              check bool "resolves to a row on the same line" true
                (Unsigned.UInt32.equal found.line row.line)))

let test_line_table_cached binary_path =
  let ctx = context binary_path in
  match first_line_table ctx with
  | None -> fail "expected a compilation unit with a line table"
  | Some (cu, lt1) -> (
      match Dwarf.line_table ctx cu with
      | None -> fail "line table vanished on second call"
      | Some lt2 ->
          check bool "second call returns the cached table" true (lt1 == lt2))

let test_line_info_for_address binary_path =
  let ctx = context binary_path in
  match first_code_address ctx with
  | None -> fail "expected a code address"
  | Some addr -> (
      match Dwarf.line_info_for_address ctx addr with
      | None -> fail "expected line info for a real code address"
      | Some info ->
          check bool "source file is a .c file" true
            (Filename.check_suffix info.Dwarf.file ".c");
          check bool "line > 0" true (info.Dwarf.line > 0);
          check bool "row address matches the query" true
            (Unsigned.UInt64.equal info.Dwarf.address addr))

let test_unit_for_address binary_path =
  let ctx = context binary_path in
  match first_code_address ctx with
  | None -> fail "expected a code address"
  | Some addr ->
      check bool "unit found for code address" true
        (Option.is_some (Dwarf.unit_for_address ctx addr));
      check bool "no unit for out-of-range address" true
        (Option.is_none (Dwarf.unit_for_address ctx Unsigned.UInt64.max_int))

let test_subprogram_for_address binary_path =
  let ctx = context binary_path in
  match first_code_address ctx with
  | None -> fail "expected a code address"
  | Some addr -> (
      match Dwarf.subprogram_for_address ctx addr with
      | None -> fail "expected a subprogram for a real code address"
      | Some die ->
          check bool "DIE is a subprogram" true
            (die.Dwarf.DIE.tag = Dwarf.DW_TAG_subprogram);
          check bool "subprogram has a name" true
            (Option.is_some (Dwarf.DIE.find_attribute die Dwarf.DW_AT_name));
          check bool "no subprogram for out-of-range address" true
            (Option.is_none
               (Dwarf.subprogram_for_address ctx Unsigned.UInt64.max_int)))

let binary_path = Test_helpers.binary_path ~doc:"Path to a DWARF 5 test binary"

let () =
  run_with_args "line_table integration" binary_path
    [
      ( "line_table",
        [
          ("present and non-empty", `Quick, test_line_table_present);
          ("find_by_address round-trip", `Quick, test_find_by_address_roundtrip);
          ("cached", `Quick, test_line_table_cached);
        ] );
      ( "address_queries",
        [
          ("line_info_for_address", `Quick, test_line_info_for_address);
          ("unit_for_address", `Quick, test_unit_for_address);
          ("subprogram_for_address", `Quick, test_subprogram_for_address);
        ] );
    ]
