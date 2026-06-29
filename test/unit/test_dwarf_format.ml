open Alcotest
open Durin

(* Test offset_size_for_format *)
let test_offset_size_for_format () =
  check int "DWARF32 offset size is 4"
    (Dwarf.offset_size_for_format Dwarf.DWARF32)
    4;
  check int "DWARF64 offset size is 8"
    (Dwarf.offset_size_for_format Dwarf.DWARF64)
    8

(* Test string_of_dwarf_format *)
let test_string_of_dwarf_format () =
  check string "DWARF32 string representation"
    (Dwarf.string_of_dwarf_format Dwarf.DWARF32)
    "DWARF32";
  check string "DWARF64 string representation"
    (Dwarf.string_of_dwarf_format Dwarf.DWARF64)
    "DWARF64"

let () =
  run "DWARF format"
    [
      ( "offset_size",
        [
          test_case "offset size for format" `Quick test_offset_size_for_format;
        ] );
      ( "string_conversion",
        [
          test_case "string_of_dwarf_format" `Quick test_string_of_dwarf_format;
        ] );
    ]
