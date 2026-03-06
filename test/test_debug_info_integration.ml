open Alcotest
open Durin

let create_context binary_path =
  let buffer = Object.Buffer.parse binary_path in
  Dwarf.create buffer

let test_create_succeeds binary_path =
  let _ctx = create_context binary_path in
  ()

let test_has_compile_units binary_path =
  let ctx = create_context binary_path in
  let cus = Dwarf.parse_compile_units ctx in
  match Seq.uncons cus with
  | None -> fail "expected at least one compile unit"
  | Some _ -> ()

let test_cu_header_valid binary_path =
  let ctx = create_context binary_path in
  let cus = Dwarf.parse_compile_units ctx in
  match Seq.uncons cus with
  | None -> fail "expected at least one compile unit"
  | Some (cu, _) ->
      let h = Dwarf.CompileUnit.header cu in
      check int "version is 5" 5 (Unsigned.UInt16.to_int h.version);
      check int "address_size is 8" 8 (Unsigned.UInt8.to_int h.address_size);
      check bool "format is DWARF32" true (h.format = Dwarf.DWARF32);
      check bool "unit_length > 0" true
        (Unsigned.UInt64.to_int64 h.unit_length > 0L)

let test_root_die_exists binary_path =
  let buffer = Object.Buffer.parse binary_path in
  let ctx = Dwarf.create buffer in
  let cus = Dwarf.parse_compile_units ctx in
  match Seq.uncons cus with
  | None -> fail "expected at least one compile unit"
  | Some (cu, _) ->
      let h = Dwarf.CompileUnit.header cu in
      let _ctx, abbrev_table =
        Dwarf.get_abbrev_table ctx h.debug_abbrev_offset
      in
      let root = Dwarf.CompileUnit.root_die cu abbrev_table buffer in
      check bool "root DIE exists" true (Option.is_some root)

let test_root_die_is_compile_unit binary_path =
  let buffer = Object.Buffer.parse binary_path in
  let ctx = Dwarf.create buffer in
  let cus = Dwarf.parse_compile_units ctx in
  match Seq.uncons cus with
  | None -> fail "expected at least one compile unit"
  | Some (cu, _) -> (
      let h = Dwarf.CompileUnit.header cu in
      let _ctx, abbrev_table =
        Dwarf.get_abbrev_table ctx h.debug_abbrev_offset
      in
      match Dwarf.CompileUnit.root_die cu abbrev_table buffer with
      | None -> fail "expected root DIE"
      | Some die ->
          check bool "tag is DW_TAG_compile_unit" true
            (die.tag = Dwarf.DW_TAG_compile_unit))

let test_root_die_has_attributes binary_path =
  let buffer = Object.Buffer.parse binary_path in
  let ctx = Dwarf.create buffer in
  let cus = Dwarf.parse_compile_units ctx in
  match Seq.uncons cus with
  | None -> fail "expected at least one compile unit"
  | Some (cu, _) -> (
      let h = Dwarf.CompileUnit.header cu in
      let _ctx, abbrev_table =
        Dwarf.get_abbrev_table ctx h.debug_abbrev_offset
      in
      match Dwarf.CompileUnit.root_die cu abbrev_table buffer with
      | None -> fail "expected root DIE"
      | Some die ->
          check bool "has attributes" true (List.length die.attributes > 0))

let binary_path =
  let doc = "Path to DWARF 5 test binary" in
  Cmdliner.Arg.(
    required & opt (some file) None & info [ "binary"; "b" ] ~doc ~docv:"BINARY")

let () =
  run_with_args "debug_info integration" binary_path
    [
      ("context", [ ("create succeeds", `Quick, test_create_succeeds) ]);
      ( "compile_units",
        [
          ("has compile units", `Quick, test_has_compile_units);
          ("CU header valid", `Quick, test_cu_header_valid);
        ] );
      ( "die",
        [
          ("root DIE exists", `Quick, test_root_die_exists);
          ("root is DW_TAG_compile_unit", `Quick, test_root_die_is_compile_unit);
          ("root has attributes", `Quick, test_root_die_has_attributes);
        ] );
    ]
