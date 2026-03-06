open Alcotest
open Durin

let test_parse_succeeds binary_path =
  let buffer = Object.Buffer.parse binary_path in
  let table = Dwarf.DebugAbbrev.parse buffer (Unsigned.UInt32.of_int 0) in
  check bool "parse succeeds" true (Option.is_some table)

let test_table_non_empty binary_path =
  let buffer = Object.Buffer.parse binary_path in
  match Dwarf.DebugAbbrev.parse buffer (Unsigned.UInt32.of_int 0) with
  | None -> fail "expected abbreviation table"
  | Some table -> check bool "table is non-empty" true (Hashtbl.length table > 0)

let test_codes_positive binary_path =
  let buffer = Object.Buffer.parse binary_path in
  match Dwarf.DebugAbbrev.parse buffer (Unsigned.UInt32.of_int 0) with
  | None -> fail "expected abbreviation table"
  | Some table ->
      Hashtbl.iter
        (fun code _abbrev ->
          check bool "code > 0" true (Unsigned.UInt64.to_int code > 0))
        table

let test_has_compile_unit_tag binary_path =
  let buffer = Object.Buffer.parse binary_path in
  match Dwarf.DebugAbbrev.parse buffer (Unsigned.UInt32.of_int 0) with
  | None -> fail "expected abbreviation table"
  | Some table ->
      let has_cu =
        Hashtbl.fold
          (fun _code (abbrev : Dwarf.abbrev) acc ->
            acc
            || Unsigned.UInt64.to_int abbrev.tag
               = Unsigned.UInt64.to_int
                   (Dwarf.uint64_of_abbreviation_tag Dwarf.DW_TAG_compile_unit))
          table false
      in
      check bool "has DW_TAG_compile_unit" true has_cu

let test_abbrevs_have_attr_specs binary_path =
  let buffer = Object.Buffer.parse binary_path in
  match Dwarf.DebugAbbrev.parse buffer (Unsigned.UInt32.of_int 0) with
  | None -> fail "expected abbreviation table"
  | Some table ->
      Hashtbl.iter
        (fun _code (abbrev : Dwarf.abbrev) ->
          check bool "has attr_specs" true (List.length abbrev.attr_specs > 0))
        table

let test_parse_all binary_path =
  let buffer = Object.Buffer.parse binary_path in
  let tables = Dwarf.DebugAbbrev.parse_all buffer in
  check bool "parse_all returns at least one table" true (List.length tables > 0)

let test_matches_get_abbrev_table binary_path =
  let buffer = Object.Buffer.parse binary_path in
  let ctx = Dwarf.create buffer in
  let cus = Dwarf.parse_compile_units ctx in
  match Seq.uncons cus with
  | None -> fail "expected at least one compile unit"
  | Some (cu, _) -> (
      let h = Dwarf.CompileUnit.header cu in
      let _ctx, via_context =
        Dwarf.get_abbrev_table ctx h.debug_abbrev_offset
      in
      let via_module =
        Dwarf.DebugAbbrev.parse buffer
          (Unsigned.UInt32.of_int
             (Unsigned.UInt64.to_int h.debug_abbrev_offset))
      in
      match via_module with
      | None -> fail "expected abbreviation table from module"
      | Some via_mod ->
          check int "same number of entries"
            (Hashtbl.length via_context)
            (Hashtbl.length via_mod))

let binary_path =
  let doc = "Path to DWARF 5 test binary" in
  Cmdliner.Arg.(
    required & opt (some file) None & info [ "binary"; "b" ] ~doc ~docv:"BINARY")

let () =
  run_with_args "debug_abbrev integration" binary_path
    [
      ( "parse",
        [
          ("parse succeeds", `Quick, test_parse_succeeds);
          ("table non-empty", `Quick, test_table_non_empty);
          ("codes positive", `Quick, test_codes_positive);
          ("has DW_TAG_compile_unit", `Quick, test_has_compile_unit_tag);
          ("abbrevs have attr_specs", `Quick, test_abbrevs_have_attr_specs);
        ] );
      ("parse_all", [ ("returns tables", `Quick, test_parse_all) ]);
      ( "consistency",
        [ ("matches get_abbrev_table", `Quick, test_matches_get_abbrev_table) ]
      );
    ]
