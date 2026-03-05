open Alcotest
open Durin

(* ---- Abbreviation tag conversion tests ---- *)

let test_tag_to_string () =
  check string "DW_TAG_compile_unit" "DW_TAG_compile_unit"
    (Dwarf.string_of_abbreviation_tag (Unsigned.UInt64.of_int 0x11));
  check string "DW_TAG_subprogram" "DW_TAG_subprogram"
    (Dwarf.string_of_abbreviation_tag (Unsigned.UInt64.of_int 0x2e));
  check string "DW_TAG_variable" "DW_TAG_variable"
    (Dwarf.string_of_abbreviation_tag (Unsigned.UInt64.of_int 0x34));
  check string "DW_TAG_base_type" "DW_TAG_base_type"
    (Dwarf.string_of_abbreviation_tag (Unsigned.UInt64.of_int 0x24));
  check string "DW_TAG_formal_parameter" "DW_TAG_formal_parameter"
    (Dwarf.string_of_abbreviation_tag (Unsigned.UInt64.of_int 0x05));
  check string "DW_TAG_typedef" "DW_TAG_typedef"
    (Dwarf.string_of_abbreviation_tag (Unsigned.UInt64.of_int 0x16));
  check string "DW_TAG_structure_type" "DW_TAG_structure_type"
    (Dwarf.string_of_abbreviation_tag (Unsigned.UInt64.of_int 0x13));
  check string "DW_TAG_member" "DW_TAG_member"
    (Dwarf.string_of_abbreviation_tag (Unsigned.UInt64.of_int 0x0d));
  check string "DW_TAG_pointer_type" "DW_TAG_pointer_type"
    (Dwarf.string_of_abbreviation_tag (Unsigned.UInt64.of_int 0x0f));
  check string "DW_TAG_array_type" "DW_TAG_array_type"
    (Dwarf.string_of_abbreviation_tag (Unsigned.UInt64.of_int 0x01));
  check string "DW_TAG_enumeration_type" "DW_TAG_enumeration_type"
    (Dwarf.string_of_abbreviation_tag (Unsigned.UInt64.of_int 0x04));
  check string "DW_TAG_namespace" "DW_TAG_namespace"
    (Dwarf.string_of_abbreviation_tag (Unsigned.UInt64.of_int 0x39))

let test_tag_direct_to_string () =
  check string "DW_TAG_compile_unit direct" "DW_TAG_compile_unit"
    (Dwarf.string_of_abbreviation_tag_direct Dwarf.DW_TAG_compile_unit);
  check string "DW_TAG_subprogram direct" "DW_TAG_subprogram"
    (Dwarf.string_of_abbreviation_tag_direct Dwarf.DW_TAG_subprogram);
  check string "DW_TAG_variable direct" "DW_TAG_variable"
    (Dwarf.string_of_abbreviation_tag_direct Dwarf.DW_TAG_variable);
  check string "DW_TAG_type_unit direct" "DW_TAG_type_unit"
    (Dwarf.string_of_abbreviation_tag_direct Dwarf.DW_TAG_type_unit);
  check string "DW_TAG_template_alias direct" "DW_TAG_template_alias"
    (Dwarf.string_of_abbreviation_tag_direct Dwarf.DW_TAG_template_alias)

let test_tag_to_uint64_roundtrip () =
  let check_roundtrip tag expected_name =
    let code = Dwarf.uint64_of_abbreviation_tag tag in
    let name = Dwarf.string_of_abbreviation_tag code in
    check string expected_name expected_name name
  in
  check_roundtrip Dwarf.DW_TAG_compile_unit "DW_TAG_compile_unit";
  check_roundtrip Dwarf.DW_TAG_subprogram "DW_TAG_subprogram";
  check_roundtrip Dwarf.DW_TAG_variable "DW_TAG_variable";
  check_roundtrip Dwarf.DW_TAG_base_type "DW_TAG_base_type";
  check_roundtrip Dwarf.DW_TAG_formal_parameter "DW_TAG_formal_parameter";
  check_roundtrip Dwarf.DW_TAG_lexical_block "DW_TAG_lexical_block";
  check_roundtrip Dwarf.DW_TAG_inlined_subroutine "DW_TAG_inlined_subroutine";
  check_roundtrip Dwarf.DW_TAG_type_unit "DW_TAG_type_unit";
  check_roundtrip Dwarf.DW_TAG_template_alias "DW_TAG_template_alias"

(* ---- DWARF 5 specific tags ---- *)

let test_dwarf5_tags () =
  check string "DW_TAG_type_unit" "DW_TAG_type_unit"
    (Dwarf.string_of_abbreviation_tag (Unsigned.UInt64.of_int 0x41));
  check string "DW_TAG_rvalue_reference_type" "DW_TAG_rvalue_reference_type"
    (Dwarf.string_of_abbreviation_tag (Unsigned.UInt64.of_int 0x42));
  check string "DW_TAG_template_alias" "DW_TAG_template_alias"
    (Dwarf.string_of_abbreviation_tag (Unsigned.UInt64.of_int 0x43));
  check string "DW_TAG_GNU_template_parameter_pack"
    "DW_TAG_GNU_template_parameter_pack"
    (Dwarf.string_of_abbreviation_tag (Unsigned.UInt64.of_int 0x4107));
  (* Tags not yet in the string table produce fallback *)
  check string "unknown tag fallback" "DW_TAG_<0x48>"
    (Dwarf.string_of_abbreviation_tag (Unsigned.UInt64.of_int 0x48))

(* ---- Attribute code string conversion ---- *)

let test_attribute_code_strings () =
  check string "DW_AT_name" "DW_AT_name"
    (Dwarf.string_of_attribute_code (Unsigned.UInt64.of_int 0x03));
  check string "DW_AT_type" "DW_AT_type"
    (Dwarf.string_of_attribute_code (Unsigned.UInt64.of_int 0x49));
  check string "DW_AT_low_pc" "DW_AT_low_pc"
    (Dwarf.string_of_attribute_code (Unsigned.UInt64.of_int 0x11));
  check string "DW_AT_high_pc" "DW_AT_high_pc"
    (Dwarf.string_of_attribute_code (Unsigned.UInt64.of_int 0x12));
  check string "DW_AT_language" "DW_AT_language"
    (Dwarf.string_of_attribute_code (Unsigned.UInt64.of_int 0x13));
  check string "DW_AT_comp_dir" "DW_AT_comp_dir"
    (Dwarf.string_of_attribute_code (Unsigned.UInt64.of_int 0x1b));
  check string "DW_AT_stmt_list" "DW_AT_stmt_list"
    (Dwarf.string_of_attribute_code (Unsigned.UInt64.of_int 0x10));
  check string "DW_AT_producer" "DW_AT_producer"
    (Dwarf.string_of_attribute_code (Unsigned.UInt64.of_int 0x25));
  check string "DW_AT_decl_file" "DW_AT_decl_file"
    (Dwarf.string_of_attribute_code (Unsigned.UInt64.of_int 0x3a));
  check string "DW_AT_decl_line" "DW_AT_decl_line"
    (Dwarf.string_of_attribute_code (Unsigned.UInt64.of_int 0x3b))

(* ---- Form encoding string conversion ---- *)

let test_form_encoding_strings () =
  check string "DW_FORM_addr" "DW_FORM_addr"
    (Dwarf.string_of_attribute_form_encoding (Unsigned.UInt64.of_int 0x01));
  check string "DW_FORM_data1" "DW_FORM_data1"
    (Dwarf.string_of_attribute_form_encoding (Unsigned.UInt64.of_int 0x0b));
  check string "DW_FORM_data2" "DW_FORM_data2"
    (Dwarf.string_of_attribute_form_encoding (Unsigned.UInt64.of_int 0x05));
  check string "DW_FORM_data4" "DW_FORM_data4"
    (Dwarf.string_of_attribute_form_encoding (Unsigned.UInt64.of_int 0x06));
  check string "DW_FORM_data8" "DW_FORM_data8"
    (Dwarf.string_of_attribute_form_encoding (Unsigned.UInt64.of_int 0x07));
  check string "DW_FORM_string" "DW_FORM_string"
    (Dwarf.string_of_attribute_form_encoding (Unsigned.UInt64.of_int 0x08));
  check string "DW_FORM_strp" "DW_FORM_strp"
    (Dwarf.string_of_attribute_form_encoding (Unsigned.UInt64.of_int 0x0e));
  check string "DW_FORM_ref_addr" "DW_FORM_ref_addr"
    (Dwarf.string_of_attribute_form_encoding (Unsigned.UInt64.of_int 0x10));
  check string "DW_FORM_exprloc" "DW_FORM_exprloc"
    (Dwarf.string_of_attribute_form_encoding (Unsigned.UInt64.of_int 0x18));
  check string "DW_FORM_implicit_const" "DW_FORM_implicit_const"
    (Dwarf.string_of_attribute_form_encoding (Unsigned.UInt64.of_int 0x21));
  check string "DW_FORM_strx1" "DW_FORM_strx1"
    (Dwarf.string_of_attribute_form_encoding (Unsigned.UInt64.of_int 0x25));
  check string "DW_FORM_addrx" "DW_FORM_addrx"
    (Dwarf.string_of_attribute_form_encoding (Unsigned.UInt64.of_int 0x1b))

(* ---- Abbreviation record construction ---- *)

let test_abbrev_record () =
  let spec : Dwarf.attr_spec =
    {
      attr = Unsigned.UInt64.of_int 0x03;
      form = Unsigned.UInt64.of_int 0x08;
      implicit_const = None;
    }
  in
  let abbrev : Dwarf.abbrev =
    {
      code = Unsigned.UInt64.of_int 1;
      tag = Unsigned.UInt64.of_int 0x11;
      has_children = true;
      attr_specs = [ spec ];
    }
  in
  check int "code is 1" (Unsigned.UInt64.to_int abbrev.code) 1;
  check string "tag is compile_unit" "DW_TAG_compile_unit"
    (Dwarf.string_of_abbreviation_tag abbrev.tag);
  check bool "has_children" true abbrev.has_children;
  check int "one attr spec" 1 (List.length abbrev.attr_specs);
  let s = List.hd abbrev.attr_specs in
  check string "attr is DW_AT_name" "DW_AT_name"
    (Dwarf.string_of_attribute_code s.attr);
  check string "form is DW_FORM_string" "DW_FORM_string"
    (Dwarf.string_of_attribute_form_encoding s.form);
  check bool "no implicit_const" true (s.implicit_const = None)

let test_abbrev_with_implicit_const () =
  let spec : Dwarf.attr_spec =
    {
      attr = Unsigned.UInt64.of_int 0x03;
      form = Unsigned.UInt64.of_int 0x21;
      implicit_const = Some 42L;
    }
  in
  check string "form is DW_FORM_implicit_const" "DW_FORM_implicit_const"
    (Dwarf.string_of_attribute_form_encoding spec.form);
  match spec.implicit_const with
  | Some v -> check int64 "implicit_const is 42" v 42L
  | None -> fail "expected implicit_const"

let test_abbrev_multiple_attrs () =
  let specs : Dwarf.attr_spec list =
    [
      {
        attr = Unsigned.UInt64.of_int 0x03;
        form = Unsigned.UInt64.of_int 0x08;
        implicit_const = None;
      };
      {
        attr = Unsigned.UInt64.of_int 0x11;
        form = Unsigned.UInt64.of_int 0x01;
        implicit_const = None;
      };
      {
        attr = Unsigned.UInt64.of_int 0x12;
        form = Unsigned.UInt64.of_int 0x07;
        implicit_const = None;
      };
    ]
  in
  let abbrev : Dwarf.abbrev =
    {
      code = Unsigned.UInt64.of_int 1;
      tag = Unsigned.UInt64.of_int 0x2e;
      has_children = true;
      attr_specs = specs;
    }
  in
  check string "tag is subprogram" "DW_TAG_subprogram"
    (Dwarf.string_of_abbreviation_tag abbrev.tag);
  check int "three attr specs" 3 (List.length abbrev.attr_specs);
  let s0 = List.nth abbrev.attr_specs 0 in
  let s1 = List.nth abbrev.attr_specs 1 in
  let s2 = List.nth abbrev.attr_specs 2 in
  check string "attr 0 is DW_AT_name" "DW_AT_name"
    (Dwarf.string_of_attribute_code s0.attr);
  check string "attr 1 is DW_AT_low_pc" "DW_AT_low_pc"
    (Dwarf.string_of_attribute_code s1.attr);
  check string "attr 2 is DW_AT_high_pc" "DW_AT_high_pc"
    (Dwarf.string_of_attribute_code s2.attr);
  check string "form 0 is DW_FORM_string" "DW_FORM_string"
    (Dwarf.string_of_attribute_form_encoding s0.form);
  check string "form 1 is DW_FORM_addr" "DW_FORM_addr"
    (Dwarf.string_of_attribute_form_encoding s1.form);
  check string "form 2 is DW_FORM_data8" "DW_FORM_data8"
    (Dwarf.string_of_attribute_form_encoding s2.form)

let test_abbrev_no_children () =
  let abbrev : Dwarf.abbrev =
    {
      code = Unsigned.UInt64.of_int 2;
      tag = Unsigned.UInt64.of_int 0x34;
      has_children = false;
      attr_specs = [];
    }
  in
  check string "tag is variable" "DW_TAG_variable"
    (Dwarf.string_of_abbreviation_tag abbrev.tag);
  check bool "no children" false abbrev.has_children;
  check int "no attr specs" 0 (List.length abbrev.attr_specs)

(* ---- Hashtbl-based abbreviation table ---- *)

let test_abbrev_table_lookup () =
  let tbl = Hashtbl.create 4 in
  let spec name_attr form =
    Dwarf.
      {
        attr = Unsigned.UInt64.of_int name_attr;
        form = Unsigned.UInt64.of_int form;
        implicit_const = None;
      }
  in
  Hashtbl.add tbl (Unsigned.UInt64.of_int 1)
    Dwarf.
      {
        code = Unsigned.UInt64.of_int 1;
        tag = Unsigned.UInt64.of_int 0x11;
        has_children = true;
        attr_specs = [ spec 0x03 0x08; spec 0x25 0x0e ];
      };
  Hashtbl.add tbl (Unsigned.UInt64.of_int 2)
    Dwarf.
      {
        code = Unsigned.UInt64.of_int 2;
        tag = Unsigned.UInt64.of_int 0x2e;
        has_children = true;
        attr_specs = [ spec 0x03 0x08; spec 0x11 0x01; spec 0x12 0x07 ];
      };
  Hashtbl.add tbl (Unsigned.UInt64.of_int 3)
    Dwarf.
      {
        code = Unsigned.UInt64.of_int 3;
        tag = Unsigned.UInt64.of_int 0x34;
        has_children = false;
        attr_specs = [ spec 0x03 0x08 ];
      };

  let a1 = Hashtbl.find tbl (Unsigned.UInt64.of_int 1) in
  check string "code 1 is compile_unit" "DW_TAG_compile_unit"
    (Dwarf.string_of_abbreviation_tag a1.tag);
  check int "code 1 has 2 attrs" 2 (List.length a1.attr_specs);

  let a2 = Hashtbl.find tbl (Unsigned.UInt64.of_int 2) in
  check string "code 2 is subprogram" "DW_TAG_subprogram"
    (Dwarf.string_of_abbreviation_tag a2.tag);
  check int "code 2 has 3 attrs" 3 (List.length a2.attr_specs);

  let a3 = Hashtbl.find tbl (Unsigned.UInt64.of_int 3) in
  check string "code 3 is variable" "DW_TAG_variable"
    (Dwarf.string_of_abbreviation_tag a3.tag);
  check bool "code 3 no children" false a3.has_children;

  let missing = Hashtbl.find_opt tbl (Unsigned.UInt64.of_int 99) in
  check bool "code 99 not found" true (missing = None)

(* TODO Add tests of real debug_abbrev from some example C/C++ programs *)

let () =
  run "debug_abbrev"
    [
      ( "tag_conversion",
        [
          test_case "tag code to string" `Quick test_tag_to_string;
          test_case "tag direct to string" `Quick test_tag_direct_to_string;
          test_case "tag to uint64 roundtrip" `Quick
            test_tag_to_uint64_roundtrip;
          test_case "DWARF 5 tags" `Quick test_dwarf5_tags;
        ] );
      ( "attribute_codes",
        [
          test_case "attribute code strings" `Quick test_attribute_code_strings;
          test_case "form encoding strings" `Quick test_form_encoding_strings;
        ] );
      ( "abbrev_record",
        [
          test_case "basic record" `Quick test_abbrev_record;
          test_case "implicit const" `Quick test_abbrev_with_implicit_const;
          test_case "multiple attributes" `Quick test_abbrev_multiple_attrs;
          test_case "no children" `Quick test_abbrev_no_children;
        ] );
      ( "abbrev_table",
        [
          test_case "table lookup" `Quick test_abbrev_table_lookup;
          test_case "section name" `Quick test_abbrev_section_name;
        ] );
    ]
