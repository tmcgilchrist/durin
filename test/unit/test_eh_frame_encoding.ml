open Durin
module H = Dwarf.EHFrameHdr

(* Describe a decoded encoding via the library's own pretty-printer, so the
   tests exercise both [encoding_of_u8] and [string_of_encoding]. *)
let desc byte = H.string_of_encoding (H.encoding_of_u8 byte)

let check_desc ~name byte expected () =
  Alcotest.(check string) name expected (desc byte)

(* Structural checks: decode the byte and assert the three components. *)
let parts byte =
  match H.encoding_of_u8 byte with
  | H.DW_EH_PE_omit -> ("omit", "omit", false)
  | H.DW_EH_PE_encoding { format; application; indirect } ->
      let fmt =
        match format with
        | DW_EH_PE_absptr -> "absptr"
        | DW_EH_PE_uleb128 -> "uleb128"
        | DW_EH_PE_udata2 -> "udata2"
        | DW_EH_PE_udata4 -> "udata4"
        | DW_EH_PE_udata8 -> "udata8"
        | DW_EH_PE_sleb128 -> "sleb128"
        | DW_EH_PE_sdata2 -> "sdata2"
        | DW_EH_PE_sdata4 -> "sdata4"
        | DW_EH_PE_sdata8 -> "sdata8"
      in
      let app =
        match application with
        | None -> "abs"
        | Some DW_EH_PE_pcrel -> "pcrel"
        | Some DW_EH_PE_textrel -> "textrel"
        | Some DW_EH_PE_datarel -> "datarel"
        | Some DW_EH_PE_funcrel -> "funcrel"
        | Some DW_EH_PE_aligned -> "aligned"
      in
      (fmt, app, indirect)

let triple_t = Alcotest.(triple string string bool)

let check_parts ~name byte expected () =
  Alcotest.check triple_t name expected (parts byte)

(* The two nibbles combine freely; before the fix only 0x1b and 0x3b of the
   combined bytes decoded and the rest raised. *)
let combined_tests =
  [
    ("absptr/abs", `Q (0x00, ("absptr", "abs", false)));
    ("omit", `Q (0xff, ("omit", "omit", false)));
    ("udata4/abs", `Q (0x03, ("udata4", "abs", false)));
    ("sdata4/abs", `Q (0x0b, ("sdata4", "abs", false)));
    ("pcrel|sdata4", `Q (0x1b, ("sdata4", "pcrel", false)));
    ("datarel|sdata4", `Q (0x3b, ("sdata4", "datarel", false)));
    ("pcrel|udata4", `Q (0x13, ("udata4", "pcrel", false)));
    ("pcrel|sdata8", `Q (0x1c, ("sdata8", "pcrel", false)));
    ("funcrel|uleb128", `Q (0x41, ("uleb128", "funcrel", false)));
    (* indirect | pcrel | sdata4 - previously crashed on decode *)
    ("indirect|pcrel|sdata4", `Q (0x9b, ("sdata4", "pcrel", true)));
  ]

let part_cases =
  List.map
    (fun (name, `Q (byte, expected)) ->
      Alcotest.test_case name `Quick (check_parts ~name byte expected))
    combined_tests

let desc_cases =
  [
    Alcotest.test_case "desc absptr" `Quick
      (check_desc ~name:"absptr" 0x00 "absolute pointer");
    Alcotest.test_case "desc omit" `Quick (check_desc ~name:"omit" 0xff "omit");
    Alcotest.test_case "desc pcrel sdata4" `Quick
      (check_desc ~name:"pcrel sdata4" 0x1b "PC-relative signed 4-byte");
    Alcotest.test_case "desc datarel sdata4" `Quick
      (check_desc ~name:"datarel sdata4" 0x3b "data-relative signed 4-byte");
    Alcotest.test_case "desc indirect pcrel sdata4" `Quick
      (check_desc ~name:"indirect pcrel sdata4" 0x9b
         "indirect PC-relative signed 4-byte");
  ]

(* Reserved nibble values must be rejected, not silently mapped. *)
let reject byte () =
  let raised =
    match H.encoding_of_u8 byte with
    | _ -> false
    | exception Dwarf.Parse_error _ -> true
  in
  Alcotest.(check bool) "raises Parse_error" true raised

let reject_cases =
  [
    Alcotest.test_case "reserved format 0x0f" `Quick (reject 0x0f);
    Alcotest.test_case "reserved application 0x60" `Quick (reject 0x60);
  ]

let () =
  Alcotest.run "EH frame pointer encoding"
    [
      ("decode", part_cases);
      ("describe", desc_cases);
      ("reject reserved", reject_cases);
    ]
