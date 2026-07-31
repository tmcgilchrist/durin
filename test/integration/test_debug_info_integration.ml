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
  let cus = Dwarf.compile_units ctx in
  match Seq.uncons cus with
  | None -> fail "expected at least one compile unit"
  | Some _ -> ()

let test_cu_header_valid binary_path =
  let ctx = create_context binary_path in
  let cus = Dwarf.compile_units ctx in
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
  let cus = Dwarf.compile_units ctx in
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
  let cus = Dwarf.compile_units ctx in
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
  let cus = Dwarf.compile_units ctx in
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
  let cus = Dwarf.compile_units ctx in
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
          check (option string) "DW_AT_name is hello_world.c"
            (Some "hello_world.c")
            (Dwarf.attr_string (Dwarf.unit ctx cu) die Dwarf.DW_AT_name);
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

(* find_attribute returns the raw (indexed) value; resolve_string resolves it
   against the unit's DW_AT_str_offsets_base. *)
let test_resolve_string binary_path =
  let ctx = create_context binary_path in
  match first_unit_root ctx with
  | None -> fail "expected a root DIE"
  | Some (u, root) -> (
      match Dwarf.DIE.find_attribute root Dwarf.DW_AT_name with
      | None -> fail "expected DW_AT_name"
      | Some value ->
          check (option string) "resolve_string resolves the raw value"
            (Some "hello_world.c")
            (Dwarf.resolve_string u value))

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

(* die_ranges falls back to the contiguous [low_pc, high_pc) pair when a DIE has
   no DW_AT_ranges. *)
let test_die_ranges_contiguous binary_path =
  let ctx = create_context binary_path in
  match first_unit_root ctx with
  | None -> fail "expected a root DIE"
  | Some (u, root) -> (
      match find_die (has_attr Dwarf.DW_AT_low_pc) root with
      | None -> fail "expected a DIE with DW_AT_low_pc"
      | Some die -> (
          check bool "no DW_AT_ranges on this DIE" true
            (Option.is_none (Dwarf.attr_ranges u die));
          match Dwarf.die_ranges u die with
          | Some [ r ] ->
              check bool "range is non-empty" true
                (Unsigned.UInt64.compare r.Dwarf.start r.Dwarf.stop < 0);
              check bool "range start is DW_AT_low_pc" true
                (match Dwarf.attr_address u die Dwarf.DW_AT_low_pc with
                | Some low -> Unsigned.UInt64.equal low r.Dwarf.start
                | None -> false)
          | _ -> fail "expected a single contiguous range"))

(* The context builds its string resolver once and shares it. *)
let test_str_resolver_shared binary_path =
  let ctx = create_context binary_path in
  check bool "str resolver is shared" true
    (Dwarf.context_str_resolver ctx == Dwarf.context_str_resolver ctx)

(* The root DIE is parsed once and cached, so two handles over the same unit
   return the physically same DIE. *)
let test_root_die_cached binary_path =
  let ctx = create_context binary_path in
  match Seq.uncons (Dwarf.compile_units ctx) with
  | None -> fail "expected at least one compile unit"
  | Some (cu, _) ->
      let d1 = Dwarf.root_die (Dwarf.unit ctx cu) in
      let d2 = Dwarf.root_die (Dwarf.unit ctx cu) in
      check bool "root DIE is cached across handles" true (d1 == d2)

let rec count_dies (die : Dwarf.DIE.t) =
  Seq.fold_left (fun n child -> n + count_dies child) 1 die.children

(* Children are re-traversable: walking the same DIE twice yields the same tree,
   which is what makes a cached root DIE safe to reuse. *)
let test_children_retraversable binary_path =
  let ctx = create_context binary_path in
  match first_unit_root ctx with
  | None -> fail "expected a root DIE"
  | Some (_u, root) ->
      let n1 = count_dies root in
      let n2 = count_dies root in
      check int "second full traversal matches the first" n1 n2;
      check bool "traversed beyond the root" true (n1 > 1)

let test_unit_entries binary_path =
  let ctx = create_context binary_path in
  match first_unit_root ctx with
  | None -> fail "expected a root DIE"
  | Some (u, root) ->
      let n = count_dies root in
      let entries = List.of_seq (Dwarf.unit_entries u) in
      check int "unit_entries covers the whole unit" n (List.length entries);
      (match entries with
      | first :: _ -> check bool "root DIE is first" true (first == root)
      | [] -> fail "expected entries");
      check int "unit_entries is re-traversable" n
        (List.length (List.of_seq (Dwarf.unit_entries u)))

let test_descendants binary_path =
  let ctx = create_context binary_path in
  match first_unit_root ctx with
  | None -> fail "expected a root DIE"
  | Some (_u, root) ->
      check int "descendants excludes the root"
        (count_dies root - 1)
        (List.length (List.of_seq (Dwarf.DIE.descendants root)))

let test_find_descendant binary_path =
  let ctx = create_context binary_path in
  match first_unit_root ctx with
  | None -> fail "expected a root DIE"
  | Some (_u, root) ->
      check bool "find_descendant locates a typed DIE" true
        (Option.is_some
           (Dwarf.DIE.find_descendant (has_attr Dwarf.DW_AT_type) root))

(* die_cursor / die_zipper built from a handle start at the root DIE. *)
let test_die_cursor_from_unit binary_path =
  let ctx = create_context binary_path in
  match Seq.uncons (Dwarf.compile_units ctx) with
  | None -> fail "expected at least one compile unit"
  | Some (cu, _) -> (
      let u = Dwarf.unit ctx cu in
      (match Dwarf.DieCursor.next (Dwarf.die_cursor u) with
      | Some (die, _) ->
          check bool "cursor starts at the compile-unit DIE" true
            (die.Dwarf.DIE.tag = Dwarf.DW_TAG_compile_unit)
      | None -> fail "expected the root DIE from the cursor");
      match Dwarf.die_zipper u with
      | None -> fail "expected a zipper"
      | Some z ->
          check bool "zipper focuses the compile-unit DIE" true
            (Dwarf.DieZipper.tag z = Dwarf.DW_TAG_compile_unit))

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
          ("resolve_string", `Quick, test_resolve_string);
          ("attr_die follows reference", `Quick, test_attr_die_follows_reference);
          ("attr_address", `Quick, test_attr_address);
          ("die_ranges contiguous", `Quick, test_die_ranges_contiguous);
        ] );
      ( "caching",
        [
          ("str resolver shared", `Quick, test_str_resolver_shared);
          ("root DIE cached", `Quick, test_root_die_cached);
          ("children re-traversable", `Quick, test_children_retraversable);
        ] );
      ( "traversal",
        [
          ("unit_entries", `Quick, test_unit_entries);
          ("descendants", `Quick, test_descendants);
          ("find_descendant", `Quick, test_find_descendant);
          ("die_cursor from unit", `Quick, test_die_cursor_from_unit);
        ] );
    ]
