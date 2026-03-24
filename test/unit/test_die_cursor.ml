open Alcotest
open Durin

let buffer_of_bytes bytes =
  let filename = Filename.temp_file "die_cursor_test_" ".bin" in
  let oc = open_out_bin filename in
  List.iter (fun b -> output_char oc (Char.chr b)) bytes;
  close_out oc;
  let buffer = Object.Buffer.parse filename in
  Sys.remove filename;
  buffer

let make_test_abbrev_table () =
  let tbl = Hashtbl.create 4 in
  let spec attr form : Dwarf.attr_spec =
    { attr; form; implicit_const = None }
  in
  Hashtbl.add tbl (Unsigned.UInt64.of_int 1)
    Dwarf.
      {
        code = Unsigned.UInt64.of_int 1;
        tag = DW_TAG_compile_unit;
        has_children = true;
        attr_specs = [ spec DW_AT_name DW_FORM_string ];
      };
  Hashtbl.add tbl (Unsigned.UInt64.of_int 2)
    Dwarf.
      {
        code = Unsigned.UInt64.of_int 2;
        tag = DW_TAG_variable;
        has_children = false;
        attr_specs = [ spec DW_AT_name DW_FORM_string ];
      };
  tbl

let make_test_encoding () : Dwarf.encoding =
  {
    format = Dwarf.DWARF32;
    address_size = Unsigned.UInt8.of_int 8;
    version = Unsigned.UInt16.of_int 5;
  }

let make_test_die_bytes () =
  [
    0x01;
    0x68;
    0x65;
    0x6c;
    0x6c;
    0x6f;
    0x00;
    0x02;
    0x78;
    0x00;
    0x02;
    0x79;
    0x00;
    0x00;
  ]

let test_die_cursor_basic () =
  let bytes = make_test_die_bytes () in
  let buffer = buffer_of_bytes bytes in
  let abbrev_table = make_test_abbrev_table () in
  let encoding = make_test_encoding () in
  let dc = Dwarf.DieCursor.create buffer abbrev_table encoding 0 in
  match Dwarf.DieCursor.next dc with
  | None -> fail "expected a DIE"
  | Some (die, has_children) ->
      check bool "compile_unit has children" true has_children;
      check string "tag is compile_unit" "DW_TAG_compile_unit"
        (Dwarf.string_of_abbreviation_tag die.tag)

let test_die_cursor_skip_children () =
  let bytes = make_test_die_bytes () in
  let buffer = buffer_of_bytes bytes in
  let abbrev_table = make_test_abbrev_table () in
  let encoding = make_test_encoding () in
  let dc = Dwarf.DieCursor.create buffer abbrev_table encoding 0 in
  (match Dwarf.DieCursor.next dc with
  | None -> fail "expected compile_unit DIE"
  | Some (_, has_children) ->
      check bool "has children" true has_children;
      Dwarf.DieCursor.skip_children dc);
  let result = Dwarf.DieCursor.next dc in
  check bool "no more DIEs after skip" true (result = None)

(* ---- DieZipper tests ---- *)

let test_die_zipper_down_up () =
  let bytes = make_test_die_bytes () in
  let buffer = buffer_of_bytes bytes in
  let abbrev_table = make_test_abbrev_table () in
  let encoding = make_test_encoding () in
  let dc = Dwarf.DieCursor.create buffer abbrev_table encoding 0 in
  match Dwarf.DieZipper.of_die_cursor dc with
  | None -> fail "expected zipper"
  | Some z -> (
      check string "root is compile_unit" "DW_TAG_compile_unit"
        (Dwarf.string_of_abbreviation_tag (Dwarf.DieZipper.tag z));
      check int "root depth is 0" 0 (Dwarf.DieZipper.depth z);
      match Dwarf.DieZipper.down z with
      | None -> fail "expected child"
      | Some child -> (
          check string "child is variable" "DW_TAG_variable"
            (Dwarf.string_of_abbreviation_tag (Dwarf.DieZipper.tag child));
          check int "child depth is 1" 1 (Dwarf.DieZipper.depth child);
          match Dwarf.DieZipper.up child with
          | None -> fail "expected parent"
          | Some parent ->
              check string "parent is compile_unit" "DW_TAG_compile_unit"
                (Dwarf.string_of_abbreviation_tag (Dwarf.DieZipper.tag parent)))
      )

let test_die_zipper_right () =
  let bytes = make_test_die_bytes () in
  let buffer = buffer_of_bytes bytes in
  let abbrev_table = make_test_abbrev_table () in
  let encoding = make_test_encoding () in
  let dc = Dwarf.DieCursor.create buffer abbrev_table encoding 0 in
  match Dwarf.DieZipper.of_die_cursor dc with
  | None -> fail "expected zipper"
  | Some z -> (
      match Dwarf.DieZipper.down z with
      | None -> fail "expected first child"
      | Some first -> (
          check string "first child is variable" "DW_TAG_variable"
            (Dwarf.string_of_abbreviation_tag (Dwarf.DieZipper.tag first));
          match Dwarf.DieZipper.right first with
          | None -> fail "expected second child"
          | Some second ->
              check string "second child is variable" "DW_TAG_variable"
                (Dwarf.string_of_abbreviation_tag (Dwarf.DieZipper.tag second));
              let no_more = Dwarf.DieZipper.right second in
              check bool "no third child" true (no_more = None)))

let test_die_zipper_children () =
  let bytes = make_test_die_bytes () in
  let buffer = buffer_of_bytes bytes in
  let abbrev_table = make_test_abbrev_table () in
  let encoding = make_test_encoding () in
  let dc = Dwarf.DieCursor.create buffer abbrev_table encoding 0 in
  match Dwarf.DieZipper.of_die_cursor dc with
  | None -> fail "expected zipper"
  | Some z ->
      let child_count =
        Dwarf.DieZipper.fold_children (fun acc _ -> acc + 1) 0 z
      in
      check int "two children" 2 child_count

let () =
  run "die_cursor"
    [
      ( "die_cursor",
        [
          test_case "cursor next parses DIE" `Quick test_die_cursor_basic;
          test_case "cursor skip_children" `Quick test_die_cursor_skip_children;
        ] );
      ( "die_zipper",
        [
          test_case "zipper down/up" `Quick test_die_zipper_down_up;
          test_case "zipper right" `Quick test_die_zipper_right;
          test_case "zipper children seq" `Quick test_die_zipper_children;
        ] );
    ]
