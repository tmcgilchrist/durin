open Durin.Dwarf

(* Import types and derive QCheck generators.
   ppx_import copies the type definition from Durin.Dwarf;
   ppx_deriving_qcheck generates gen_<type> : <type> QCheck.Gen.t
   for each. When variants are added to the library, the
   generators update automatically — no scan ranges to maintain. *)

type abbreviation_tag = [%import: Durin.Dwarf.abbreviation_tag]
[@@deriving qcheck]

type attribute_encoding = [%import: Durin.Dwarf.attribute_encoding]
[@@deriving qcheck]

type attribute_form_encoding = [%import: Durin.Dwarf.attribute_form_encoding]
[@@deriving qcheck]

type operation_encoding = [%import: Durin.Dwarf.operation_encoding]
[@@deriving qcheck]

type location_list_entry = [%import: Durin.Dwarf.location_list_entry]
[@@deriving qcheck]

type base_type = [%import: Durin.Dwarf.base_type] [@@deriving qcheck]
type dwarf_language = [%import: Durin.Dwarf.dwarf_language] [@@deriving qcheck]

type name_index_attribute = [%import: Durin.Dwarf.name_index_attribute]
[@@deriving qcheck]

type range_list_entry = [%import: Durin.Dwarf.range_list_entry]
[@@deriving qcheck]

type macro_info_entry_type = [%import: Durin.Dwarf.macro_info_entry_type]
[@@deriving qcheck]

type unit_type = [%import: Durin.Dwarf.unit_type] [@@deriving qcheck]

type children_determination = [%import: Durin.Dwarf.children_determination]
[@@deriving qcheck]

type decimal_sign = [%import: Durin.Dwarf.decimal_sign] [@@deriving qcheck]
type endianity = [%import: Durin.Dwarf.endianity] [@@deriving qcheck]
type accessibility = [%import: Durin.Dwarf.accessibility] [@@deriving qcheck]
type visibility = [%import: Durin.Dwarf.visibility] [@@deriving qcheck]
type virtuality = [%import: Durin.Dwarf.virtuality] [@@deriving qcheck]
type identifier = [%import: Durin.Dwarf.identifier] [@@deriving qcheck]

type calling_convention = [%import: Durin.Dwarf.calling_convention]
[@@deriving qcheck]

type inlined = [%import: Durin.Dwarf.inlined] [@@deriving qcheck]
type array_ordering = [%import: Durin.Dwarf.array_ordering] [@@deriving qcheck]
type discriminant = [%import: Durin.Dwarf.discriminant] [@@deriving qcheck]

type defaulted_attribute = [%import: Durin.Dwarf.defaulted_attribute]
[@@deriving qcheck]

type line_number_opcode = [%import: Durin.Dwarf.line_number_opcode]
[@@deriving qcheck]

type line_number_extended_opcode =
  [%import: Durin.Dwarf.line_number_extended_opcode]
[@@deriving qcheck]

type line_number_header_entry = [%import: Durin.Dwarf.line_number_header_entry]
[@@deriving qcheck]

(* --- Helpers --- *)

let arb gen = QCheck.make gen

let roundtrip name gen encode decode =
  QCheck.Test.make ~name:(name ^ " roundtrip") (arb gen) (fun v ->
      decode (encode v) = v)

(* --- Roundtrip tests --- *)

let roundtrip_tests =
  [
    roundtrip "abbreviation_tag" gen_abbreviation_tag uint64_of_abbreviation_tag
      abbreviation_tag_of_int;
    roundtrip "attribute_encoding" gen_attribute_encoding
      u64_of_attribute_encoding attribute_encoding;
    roundtrip "attribute_form_encoding" gen_attribute_form_encoding
      u64_of_attribute_form_encoding attribute_form_encoding;
    roundtrip "operation_encoding" gen_operation_encoding
      int_of_operation_encoding operation_encoding;
    roundtrip "location_list_entry" gen_location_list_entry
      int_of_location_list_entry location_list_entry;
    roundtrip "base_type" gen_base_type int_of_base_type base_type;
    roundtrip "dwarf_language" gen_dwarf_language int_of_dwarf_language
      dwarf_language;
    roundtrip "name_index_attribute" gen_name_index_attribute
      int_of_name_index_attribute name_index_attribute;
    roundtrip "range_list_entry" gen_range_list_entry int_of_range_list_entry
      range_list_entry;
    roundtrip "macro_info_entry_type" gen_macro_info_entry_type
      (fun v -> Unsigned.UInt8.to_int (u8_of_macro_info_entry_type v))
      (fun i -> macro_info_entry_type_of_u8 (Unsigned.UInt8.of_int i));
    roundtrip "unit_type" gen_unit_type int_of_unit_type (fun i ->
        unit_type_of_u8 (Unsigned.UInt8.of_int i));
    roundtrip "children_determination" gen_children_determination
      int_of_children_determination children_determination;
    roundtrip "decimal_sign" gen_decimal_sign int_of_decimal_sign decimal_sign;
    roundtrip "endianity" gen_endianity int_of_endianity endianity;
    roundtrip "accessibility" gen_accessibility int_of_accessibility
      accessibility;
    roundtrip "visibility" gen_visibility int_of_visibility visibility;
    roundtrip "virtuality" gen_virtuality int_of_virtuality virtuality;
    roundtrip "identifier" gen_identifier int_of_identifier identifier;
    roundtrip "calling_convention" gen_calling_convention
      int_of_calling_convention calling_convention;
    roundtrip "inlined" gen_inlined int_of_inlined inlined;
    roundtrip "array_ordering" gen_array_ordering int_of_array_ordering
      array_ordering;
    roundtrip "discriminant" gen_discriminant int_of_discriminant discriminant;
    roundtrip "defaulted_attribute" gen_defaulted_attribute
      int_of_defaulted_attribute defaulted_attribute;
    roundtrip "line_number_opcode" gen_line_number_opcode
      int_of_line_number_opcode line_number_opcode;
    roundtrip "line_number_extended_opcode" gen_line_number_extended_opcode
      int_of_line_number_extended_opcode line_number_extended_opcode;
    roundtrip "line_number_header_entry" gen_line_number_header_entry
      int_of_line_number_header_entry line_number_header_entry;
  ]

(* --- String consistency tests --- *)

let () =
  let open Alcotest in
  run "DWARF constants roundtrip"
    [ ("roundtrip", List.map QCheck_alcotest.to_alcotest roundtrip_tests) ]
