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
      let abbrev_table = Dwarf.get_abbrev_table ctx h.debug_abbrev_offset in
      let root =
        Dwarf.CompileUnit.root_die cu abbrev_table
          (Dwarf.context_str_resolver ctx)
      in
      check bool "root DIE exists" true (Option.is_some root)

let test_root_die_is_compile_unit binary_path =
  let buffer = Object.Buffer.parse binary_path in
  let ctx = Dwarf.create buffer in
  let cus = Dwarf.parse_compile_units ctx in
  match Seq.uncons cus with
  | None -> fail "expected at least one compile unit"
  | Some (cu, _) -> (
      let h = Dwarf.CompileUnit.header cu in
      let abbrev_table = Dwarf.get_abbrev_table ctx h.debug_abbrev_offset in
      match
        Dwarf.CompileUnit.root_die cu abbrev_table
          (Dwarf.context_str_resolver ctx)
      with
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
      let abbrev_table = Dwarf.get_abbrev_table ctx h.debug_abbrev_offset in
      match
        Dwarf.CompileUnit.root_die cu abbrev_table
          (Dwarf.context_str_resolver ctx)
      with
      | None -> fail "expected root DIE"
      | Some die ->
          check bool "has attributes" true (List.length die.attributes > 0))

let test_root_die_attribute_values binary_path =
  let buffer = Object.Buffer.parse binary_path in
  let ctx = Dwarf.create buffer in
  let cus = Dwarf.parse_compile_units ctx in
  match Seq.uncons cus with
  | None -> fail "expected at least one compile unit"
  | Some (cu, _) -> (
      let h = Dwarf.CompileUnit.header cu in
      let abbrev_table = Dwarf.get_abbrev_table ctx h.debug_abbrev_offset in
      match
        Dwarf.CompileUnit.root_die cu abbrev_table
          (Dwarf.context_str_resolver ctx)
      with
      | None -> fail "expected root DIE"
      | Some die -> (
          (match Dwarf.DIE.find_attribute die Dwarf.DW_AT_name with
          | Some (String s) ->
              check bool "DW_AT_name contains hello_world" true
                (String.equal s "hello_world.c")
          | Some (IndexedString (_, s)) ->
              check bool "DW_AT_name contains hello_world" true
                (String.equal s "hello_world.c")
          | _ -> fail "expected DW_AT_name to be a string");
          match Dwarf.DIE.find_attribute die Dwarf.DW_AT_language with
          | Some (Language _) -> ()
          | _ -> fail "expected DW_AT_language to be a Language value"))

(* The [unit_ref] handle reads the same root DIE without the caller threading the
   abbrev table and string resolver that the tests above pass by hand. *)
let test_unit_handle_root_die binary_path =
  let ctx = create_context binary_path in
  match Seq.uncons (Dwarf.compile_units ctx) with
  | None -> fail "expected at least one compile unit"
  | Some (cu, _) -> (
      let u = Dwarf.unit ctx cu in
      check bool "handle round-trips its unit" true (Dwarf.cu u == cu);
      match Dwarf.root_die u with
      | None -> fail "expected root DIE"
      | Some die ->
          check bool "root is DW_TAG_compile_unit" true
            (die.tag = Dwarf.DW_TAG_compile_unit))

let test_unit_handle_name binary_path =
  let ctx = create_context binary_path in
  match Seq.uncons (Dwarf.compile_units ctx) with
  | None -> fail "expected at least one compile unit"
  | Some (cu, _) ->
      let u = Dwarf.unit ctx cu in
      check (option string) "unit_name is hello_world.c" (Some "hello_world.c")
        (Dwarf.unit_name u);
      check bool "comp_dir is present" true (Option.is_some (Dwarf.comp_dir u))

(* First DIE in a subtree satisfying [pred], depth-first. *)
let rec find_die pred (die : Dwarf.DIE.t) =
  if pred die then Some die
  else
    Seq.fold_left
      (fun acc child ->
        match acc with Some _ -> acc | None -> find_die pred child)
      None die.children

let has_attr attr (die : Dwarf.DIE.t) =
  Option.is_some (Dwarf.DIE.find_attribute die attr)

(* The handle and root DIE of the first compilation unit. *)
let first_unit_root ctx =
  match Seq.uncons (Dwarf.compile_units ctx) with
  | None -> None
  | Some (cu, _) ->
      let u = Dwarf.unit ctx cu in
      Option.map (fun root -> (u, root)) (Dwarf.root_die u)

let test_attr_string binary_path =
  let ctx = create_context binary_path in
  match first_unit_root ctx with
  | None -> fail "expected a root DIE"
  | Some (u, root) ->
      check (option string) "attr_string reads DW_AT_name"
        (Some "hello_world.c")
        (Dwarf.attr_string u root Dwarf.DW_AT_name)

(* attr_die follows a within-unit reference (DW_AT_type) to its target DIE. *)
let test_attr_die_follows_reference binary_path =
  let ctx = create_context binary_path in
  match first_unit_root ctx with
  | None -> fail "expected a root DIE"
  | Some (u, root) -> (
      match find_die (has_attr Dwarf.DW_AT_type) root with
      | None -> fail "expected a DIE with DW_AT_type"
      | Some die -> (
          match Dwarf.attr_die u die Dwarf.DW_AT_type with
          | None -> fail "attr_die did not resolve DW_AT_type"
          | Some target ->
              check bool "resolved a distinct target DIE" true
                (target.Dwarf.DIE.offset <> die.Dwarf.DIE.offset)))

let test_attr_address binary_path =
  let ctx = create_context binary_path in
  match first_unit_root ctx with
  | None -> fail "expected a root DIE"
  | Some (u, root) -> (
      match find_die (has_attr Dwarf.DW_AT_low_pc) root with
      | None -> fail "expected a DIE with DW_AT_low_pc"
      | Some die ->
          check bool "attr_address resolves DW_AT_low_pc" true
            (Option.is_some (Dwarf.attr_address u die Dwarf.DW_AT_low_pc)))

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
          ("root attribute values", `Quick, test_root_die_attribute_values);
        ] );
      ( "unit_handle",
        [
          ("root DIE via handle", `Quick, test_unit_handle_root_die);
          ("name and comp_dir", `Quick, test_unit_handle_name);
        ] );
      ( "typed_attrs",
        [
          ("attr_string", `Quick, test_attr_string);
          ("attr_die follows reference", `Quick, test_attr_die_follows_reference);
          ("attr_address", `Quick, test_attr_address);
        ] );
    ]
