open Alcotest
open Durin
module Buffer = Stdlib.Buffer

let object_buffer_of_buffer buf =
  let contents = Buffer.contents buf in
  let filename = Filename.temp_file "dwarf_write_test_" ".bin" in
  let oc = open_out_bin filename in
  output_string oc contents;
  close_out oc;
  let obj_buf = Object.Buffer.parse filename in
  Sys.remove filename;
  obj_buf

let test_write_u8 () =
  let buf = Buffer.create 1 in
  Dwarf_write.write_u8 buf (Unsigned.UInt8.of_int 0x42);
  let obj_buf = object_buffer_of_buffer buf in
  let cur = Object.Buffer.cursor obj_buf ~at:0 in
  let v = Object.Buffer.Read.u8 cur in
  check int "u8 roundtrip" 0x42 (Unsigned.UInt8.to_int v)

let test_write_u16_le () =
  let buf = Buffer.create 2 in
  Dwarf_write.write_u16_le buf (Unsigned.UInt16.of_int 0x1234);
  let obj_buf = object_buffer_of_buffer buf in
  let cur = Object.Buffer.cursor obj_buf ~at:0 in
  let v = Object.Buffer.Read.u16 cur in
  check int "u16 roundtrip" 0x1234 (Unsigned.UInt16.to_int v)

let test_write_u32_le () =
  let buf = Buffer.create 4 in
  Dwarf_write.write_u32_le buf (Unsigned.UInt32.of_int 0x12345678);
  let obj_buf = object_buffer_of_buffer buf in
  let cur = Object.Buffer.cursor obj_buf ~at:0 in
  let v = Object.Buffer.Read.u32 cur in
  check int64 "u32 roundtrip"
    (Int64.of_string "0x12345678")
    (Unsigned.UInt32.to_int64 v)

let test_write_u64_le () =
  let buf = Buffer.create 8 in
  Dwarf_write.write_u64_le buf
    (Unsigned.UInt64.of_int64 (Int64.of_string "0x123456789abcdef0"));
  let obj_buf = object_buffer_of_buffer buf in
  let cur = Object.Buffer.cursor obj_buf ~at:0 in
  let v = Object.Buffer.Read.u64 cur in
  check int64 "u64 roundtrip"
    (Int64.of_string "0x123456789abcdef0")
    (Unsigned.UInt64.to_int64 v)

let test_write_i64_le () =
  let buf = Buffer.create 8 in
  Dwarf_write.write_i64_le buf (Signed.Int64.of_int64 (-42L));
  let obj_buf = object_buffer_of_buffer buf in
  let cur = Object.Buffer.cursor obj_buf ~at:0 in
  let v = Object.Buffer.Read.i64 cur in
  check int64 "i64 roundtrip" (-42L) (Signed.Int64.to_int64 v)

let roundtrip_uleb128 value =
  let buf = Buffer.create 10 in
  let u64_val = Unsigned.UInt64.of_int value in
  Dwarf_write.write_uleb128 buf u64_val;
  let obj_buf = object_buffer_of_buffer buf in
  let cur = Object.Buffer.cursor obj_buf ~at:0 in
  let read_back = Object.Buffer.Read.uleb128 cur in
  check int (Printf.sprintf "uleb128 roundtrip %d" value) value read_back

let test_write_uleb128 () =
  roundtrip_uleb128 0;
  roundtrip_uleb128 127;
  roundtrip_uleb128 128;
  roundtrip_uleb128 16384;
  roundtrip_uleb128 0x0102030405

let roundtrip_sleb128 value =
  let buf = Buffer.create 10 in
  let i64_val = Signed.Int64.of_int value in
  Dwarf_write.write_sleb128 buf i64_val;
  let obj_buf = object_buffer_of_buffer buf in
  let cur = Object.Buffer.cursor obj_buf ~at:0 in
  let read_back = Object.Buffer.Read.sleb128 cur in
  check int (Printf.sprintf "sleb128 roundtrip %d" value) value read_back

let test_write_sleb128 () =
  roundtrip_sleb128 0;
  roundtrip_sleb128 (-1);
  roundtrip_sleb128 63;
  roundtrip_sleb128 (-64);
  roundtrip_sleb128 128;
  roundtrip_sleb128 (-129)

let test_write_initial_length_dwarf32 () =
  let buf = Buffer.create 4 in
  Dwarf_write.write_initial_length buf Dwarf.DWARF32 256;
  let obj_buf = object_buffer_of_buffer buf in
  let cur = Object.Buffer.cursor obj_buf ~at:0 in
  let format, length = Dwarf.parse_initial_length cur in
  check
    (module struct
      type t = Dwarf.dwarf_format

      let equal a b = a = b
      let pp fmt v = Format.fprintf fmt "%s" (Dwarf.string_of_dwarf_format v)
    end)
    "format is DWARF32" Dwarf.DWARF32 format;
  check int64 "length is 256" (Int64.of_int 256)
    (Unsigned.UInt64.to_int64 length)

let test_write_initial_length_dwarf64 () =
  let buf = Buffer.create 12 in
  Dwarf_write.write_initial_length buf Dwarf.DWARF64 512;
  let obj_buf = object_buffer_of_buffer buf in
  let cur = Object.Buffer.cursor obj_buf ~at:0 in
  let format, length = Dwarf.parse_initial_length cur in
  check
    (module struct
      type t = Dwarf.dwarf_format

      let equal a b = a = b
      let pp fmt v = Format.fprintf fmt "%s" (Dwarf.string_of_dwarf_format v)
    end)
    "format is DWARF64" Dwarf.DWARF64 format;
  check int64 "length is 512" (Int64.of_int 512)
    (Unsigned.UInt64.to_int64 length)

let test_write_offset_dwarf32 () =
  let buf = Buffer.create 4 in
  Dwarf_write.write_offset buf Dwarf.DWARF32 (Unsigned.UInt64.of_int 0x12345678);
  let obj_buf = object_buffer_of_buffer buf in
  let cur = Object.Buffer.cursor obj_buf ~at:0 in
  let v = Dwarf.read_offset_for_format Dwarf.DWARF32 cur in
  check int64 "offset DWARF32 roundtrip"
    (Int64.of_string "0x12345678")
    (Unsigned.UInt64.to_int64 v)

let test_write_offset_dwarf64 () =
  let buf = Buffer.create 8 in
  Dwarf_write.write_offset buf Dwarf.DWARF64
    (Unsigned.UInt64.of_int64 (Int64.of_string "0x123456789abcdef0"));
  let obj_buf = object_buffer_of_buffer buf in
  let cur = Object.Buffer.cursor obj_buf ~at:0 in
  let v = Dwarf.read_offset_for_format Dwarf.DWARF64 cur in
  check int64 "offset DWARF64 roundtrip"
    (Int64.of_string "0x123456789abcdef0")
    (Unsigned.UInt64.to_int64 v)

let test_write_address_4 () =
  let buf = Buffer.create 4 in
  Dwarf_write.write_address buf 4 (Unsigned.UInt64.of_int 0x12345678);
  let obj_buf = object_buffer_of_buffer buf in
  let cur = Object.Buffer.cursor obj_buf ~at:0 in
  let v = Object.Buffer.Read.u32 cur |> Unsigned.UInt64.of_uint32 in
  check int64 "4-byte address roundtrip"
    (Int64.of_string "0x12345678")
    (Unsigned.UInt64.to_int64 v)

let test_write_address_8 () =
  let buf = Buffer.create 8 in
  Dwarf_write.write_address buf 8
    (Unsigned.UInt64.of_int64 (Int64.of_string "0x123456789abcdef0"));
  let obj_buf = object_buffer_of_buffer buf in
  let cur = Object.Buffer.cursor obj_buf ~at:0 in
  let v = Object.Buffer.Read.u64 cur in
  check int64 "8-byte address roundtrip"
    (Int64.of_string "0x123456789abcdef0")
    (Unsigned.UInt64.to_int64 v)

let test_write_null_terminated_string () =
  let buf = Buffer.create 16 in
  Dwarf_write.write_null_terminated_string buf "hello";
  let obj_buf = object_buffer_of_buffer buf in
  let cur = Object.Buffer.cursor obj_buf ~at:0 in
  let s = Object.Buffer.Read.zero_string cur () in
  check (option string) "null terminated string roundtrip" (Some "hello") s

(* Stage 2: Abbreviation table tests *)

let parse_abbrev_from_cursor cur =
  let table = Hashtbl.create 16 in
  let rec loop () =
    let code = Object.Buffer.Read.uleb128 cur in
    if code = 0 then ()
    else
      let tag_raw = Object.Buffer.Read.uleb128 cur in
      let has_children =
        Unsigned.UInt8.to_int (Object.Buffer.Read.u8 cur) <> 0
      in
      let rec parse_specs acc =
        let attr_raw = Object.Buffer.Read.uleb128 cur in
        let form_raw = Object.Buffer.Read.uleb128 cur in
        if attr_raw = 0 && form_raw = 0 then List.rev acc
        else
          let form =
            Dwarf.attribute_form_encoding (Unsigned.UInt64.of_int form_raw)
          in
          let implicit_const =
            match form with
            | Dwarf.DW_FORM_implicit_const ->
                Some (Int64.of_int (Object.Buffer.Read.sleb128 cur))
            | _ -> None
          in
          parse_specs
            (Dwarf.
               {
                 attr =
                   Dwarf.attribute_encoding (Unsigned.UInt64.of_int attr_raw);
                 form;
                 implicit_const;
               }
            :: acc)
      in
      let attr_specs = parse_specs [] in
      let abbrev =
        Dwarf.
          {
            code = Unsigned.UInt64.of_int code;
            tag = Dwarf.abbreviation_tag_of_int (Unsigned.UInt64.of_int tag_raw);
            has_children;
            attr_specs;
          }
      in
      Hashtbl.add table (Unsigned.UInt64.of_int code) abbrev;
      loop ()
  in
  loop ();
  table

let u64 n = Unsigned.UInt64.of_int n
let spec attr form : Dwarf.attr_spec = { attr; form; implicit_const = None }

let test_write_abbrev_table_single () =
  let abbrev =
    Dwarf.
      {
        code = u64 1;
        tag = DW_TAG_compile_unit;
        has_children = true;
        attr_specs =
          [
            spec DW_AT_producer DW_FORM_strp; spec DW_AT_language DW_FORM_udata;
          ];
      }
  in
  let buf = Buffer.create 64 in
  Dwarf_write.write_abbrev_table buf [| abbrev |];
  let obj_buf = object_buffer_of_buffer buf in
  let cur = Object.Buffer.cursor obj_buf ~at:0 in
  let table = parse_abbrev_from_cursor cur in
  check int "table has 1 entry" 1 (Hashtbl.length table);
  let parsed = Hashtbl.find table (u64 1) in
  check bool "tag matches" true (parsed.tag = Dwarf.DW_TAG_compile_unit);
  check bool "has_children matches" true parsed.has_children;
  check int "attr_specs length" 2 (List.length parsed.attr_specs);
  let spec0 = List.nth parsed.attr_specs 0 in
  check bool "first attr" true (spec0.attr = Dwarf.DW_AT_producer);
  check bool "first form" true (spec0.form = Dwarf.DW_FORM_strp)

let test_write_abbrev_table_multiple () =
  let abbrevs =
    [|
      Dwarf.
        {
          code = u64 1;
          tag = DW_TAG_compile_unit;
          has_children = true;
          attr_specs = [ spec DW_AT_name DW_FORM_string ];
        };
      Dwarf.
        {
          code = u64 2;
          tag = DW_TAG_base_type;
          has_children = false;
          attr_specs =
            [
              spec DW_AT_name DW_FORM_string; spec DW_AT_byte_size DW_FORM_udata;
            ];
        };
    |]
  in
  let buf = Buffer.create 64 in
  Dwarf_write.write_abbrev_table buf abbrevs;
  let obj_buf = object_buffer_of_buffer buf in
  let cur = Object.Buffer.cursor obj_buf ~at:0 in
  let table = parse_abbrev_from_cursor cur in
  check int "table has 2 entries" 2 (Hashtbl.length table);
  let a1 = Hashtbl.find table (u64 1) in
  let a2 = Hashtbl.find table (u64 2) in
  check bool "a1 has_children" true a1.has_children;
  check bool "a2 no children" false a2.has_children;
  check int "a2 attr_specs length" 2 (List.length a2.attr_specs)

let test_write_abbrev_table_implicit_const () =
  let abbrev =
    Dwarf.
      {
        code = u64 1;
        tag = DW_TAG_subprogram;
        has_children = false;
        attr_specs =
          [
            {
              attr = DW_AT_decl_line;
              form = DW_FORM_implicit_const;
              implicit_const = Some 42L;
            };
          ];
      }
  in
  let buf = Buffer.create 64 in
  Dwarf_write.write_abbrev_table buf [| abbrev |];
  let obj_buf = object_buffer_of_buffer buf in
  let cur = Object.Buffer.cursor obj_buf ~at:0 in
  let table = parse_abbrev_from_cursor cur in
  let parsed = Hashtbl.find table (u64 1) in
  let s = List.hd parsed.attr_specs in
  check bool "form is implicit_const" true
    (s.form = Dwarf.DW_FORM_implicit_const);
  check (option int64) "implicit_const value" (Some 42L) s.implicit_const

let test_abbrev_table_size () =
  let abbrev =
    Dwarf.
      {
        code = u64 1;
        tag = DW_TAG_compile_unit;
        has_children = true;
        attr_specs = [ spec DW_AT_name DW_FORM_string ];
      }
  in
  let buf = Buffer.create 64 in
  Dwarf_write.write_abbrev_table buf [| abbrev |];
  let written_size = Buffer.length buf in
  let computed_size = Dwarf_write.abbrev_table_size [| abbrev |] in
  check int "size matches written bytes" written_size computed_size

let test_assign_abbreviations () =
  let die1 : Dwarf.DIE.t =
    {
      tag = DW_TAG_compile_unit;
      attributes =
        [
          { attr = DW_AT_name; value = String "test.c" };
          { attr = DW_AT_language; value = UData (u64 0x0c) };
        ];
      children = Seq.empty;
      offset = 0;
    }
  in
  let die2 : Dwarf.DIE.t =
    {
      tag = DW_TAG_compile_unit;
      attributes =
        [
          { attr = DW_AT_name; value = String "other.c" };
          { attr = DW_AT_language; value = UData (u64 0x0c) };
        ];
      children = Seq.empty;
      offset = 100;
    }
  in
  let abbrevs, lookup = Dwarf_write.assign_abbreviations [ die1; die2 ] in
  check int "deduplication: 1 abbrev for 2 identical shapes" 1
    (Array.length abbrevs);
  let code1 = lookup 0 in
  let code2 = lookup 100 in
  check int64 "same shape gets same code"
    (Unsigned.UInt64.to_int64 code1)
    (Unsigned.UInt64.to_int64 code2)

let test_assign_abbreviations_different_shapes () =
  let die1 : Dwarf.DIE.t =
    {
      tag = DW_TAG_compile_unit;
      attributes = [ { attr = DW_AT_name; value = String "test.c" } ];
      children = Seq.empty;
      offset = 0;
    }
  in
  let die2 : Dwarf.DIE.t =
    {
      tag = DW_TAG_base_type;
      attributes =
        [
          { attr = DW_AT_name; value = String "int" };
          { attr = DW_AT_byte_size; value = UData (u64 4) };
        ];
      children = Seq.empty;
      offset = 50;
    }
  in
  let abbrevs, lookup = Dwarf_write.assign_abbreviations [ die1; die2 ] in
  check int "2 different shapes" 2 (Array.length abbrevs);
  let code1 = lookup 0 in
  let code2 = lookup 50 in
  check bool "different codes"
    (Unsigned.UInt64.to_int64 code1 <> Unsigned.UInt64.to_int64 code2)
    true

let test_assign_abbreviations_with_children () =
  let child : Dwarf.DIE.t =
    {
      tag = DW_TAG_base_type;
      attributes = [ { attr = DW_AT_name; value = String "int" } ];
      children = Seq.empty;
      offset = 20;
    }
  in
  let parent : Dwarf.DIE.t =
    {
      tag = DW_TAG_compile_unit;
      attributes = [ { attr = DW_AT_name; value = String "test.c" } ];
      children = List.to_seq [ child ];
      offset = 0;
    }
  in
  let abbrevs, lookup = Dwarf_write.assign_abbreviations [ parent ] in
  check int "2 shapes (parent with children, child without)" 2
    (Array.length abbrevs);
  let parent_code = lookup 0 in
  let child_code = lookup 20 in
  check bool "parent and child have different codes"
    (Unsigned.UInt64.to_int64 parent_code <> Unsigned.UInt64.to_int64 child_code)
    true;
  let parent_abbrev = abbrevs.(0) in
  check bool "parent has_children" true parent_abbrev.has_children

let test_assign_then_write_roundtrip () =
  let die : Dwarf.DIE.t =
    {
      tag = DW_TAG_compile_unit;
      attributes =
        [
          { attr = DW_AT_name; value = String "hello.c" };
          { attr = DW_AT_producer; value = String "durin" };
        ];
      children = Seq.empty;
      offset = 0;
    }
  in
  let abbrevs, _ = Dwarf_write.assign_abbreviations [ die ] in
  let buf = Buffer.create 64 in
  Dwarf_write.write_abbrev_table buf abbrevs;
  let obj_buf = object_buffer_of_buffer buf in
  let cur = Object.Buffer.cursor obj_buf ~at:0 in
  let table = parse_abbrev_from_cursor cur in
  check int "roundtrip: 1 abbrev" 1 (Hashtbl.length table);
  let parsed = Hashtbl.find table (u64 1) in
  check bool "roundtrip: tag is compile_unit" true
    (parsed.tag = Dwarf.DW_TAG_compile_unit);
  check int "roundtrip: 2 attr_specs" 2 (List.length parsed.attr_specs)

(* Stage 3: Attribute value tests *)

let default_encoding : Dwarf.encoding =
  {
    format = DWARF32;
    address_size = Unsigned.UInt8.of_int 8;
    version = Unsigned.UInt16.of_int 5;
  }

let roundtrip_attr_value value form enc =
  let buf = Buffer.create 64 in
  Dwarf_write.write_attribute_value buf value form enc;
  let obj_buf = object_buffer_of_buffer buf in
  let cur = Object.Buffer.cursor obj_buf ~at:0 in
  Dwarf.DIE.parse_attribute_value cur form enc obj_buf ()

let test_write_attr_string () =
  let result =
    roundtrip_attr_value (String "hello") Dwarf.DW_FORM_string default_encoding
  in
  match result with
  | String s -> check string "string roundtrip" "hello" s
  | _ -> fail "expected String"

let test_write_attr_udata () =
  let result =
    roundtrip_attr_value
      (UData (u64 12345))
      Dwarf.DW_FORM_udata default_encoding
  in
  match result with
  | UData v -> check int "udata roundtrip" 12345 (Unsigned.UInt64.to_int v)
  | _ -> fail "expected UData"

let test_write_attr_sdata () =
  let result =
    roundtrip_attr_value
      (SData (Signed.Int64.of_int (-42)))
      Dwarf.DW_FORM_sdata default_encoding
  in
  match result with
  | SData v -> check int "sdata roundtrip" (-42) (Signed.Int64.to_int v)
  | _ -> fail "expected SData"

let test_write_attr_addr () =
  let addr = Unsigned.UInt64.of_int 0x400000 in
  let result =
    roundtrip_attr_value (Address addr) Dwarf.DW_FORM_addr default_encoding
  in
  match result with
  | Address v ->
      check int64 "addr roundtrip"
        (Unsigned.UInt64.to_int64 addr)
        (Unsigned.UInt64.to_int64 v)
  | _ -> fail "expected Address"

let test_write_attr_flag_present () =
  let result =
    roundtrip_attr_value (Flag true) Dwarf.DW_FORM_flag_present default_encoding
  in
  match result with
  | Flag b -> check bool "flag_present roundtrip" true b
  | _ -> fail "expected Flag"

let test_write_attr_flag () =
  let enc = { default_encoding with format = DWARF32 } in
  let result = roundtrip_attr_value (Flag true) Dwarf.DW_FORM_flag enc in
  (match result with
  | Flag b -> check bool "flag true roundtrip" true b
  | _ -> fail "expected Flag");
  let result = roundtrip_attr_value (Flag false) Dwarf.DW_FORM_flag enc in
  match result with
  | Flag b -> check bool "flag false roundtrip" false b
  | _ -> fail "expected Flag"

let test_write_attr_ref4 () =
  let result =
    roundtrip_attr_value
      (Reference (u64 0x1234))
      Dwarf.DW_FORM_ref4 default_encoding
  in
  match result with
  | Reference v -> check int "ref4 roundtrip" 0x1234 (Unsigned.UInt64.to_int v)
  | _ -> fail "expected Reference"

let test_write_attr_block () =
  let data = "\x01\x02\x03\x04\x05" in
  let result =
    roundtrip_attr_value (Block data) Dwarf.DW_FORM_block default_encoding
  in
  match result with
  | Block b -> check string "block roundtrip" data b
  | _ -> fail "expected Block"

let test_write_attr_language () =
  let result =
    roundtrip_attr_value (Language Dwarf.DW_LANG_OCaml) Dwarf.DW_FORM_udata
      default_encoding
  in
  match result with
  | UData v -> check int "language roundtrip" 0x1b (Unsigned.UInt64.to_int v)
  | _ -> fail "expected UData"

let test_write_attr_encoding () =
  let result =
    roundtrip_attr_value (Encoding Dwarf.DW_ATE_signed) Dwarf.DW_FORM_udata
      default_encoding
  in
  match result with
  | UData v -> check int "encoding roundtrip" 0x05 (Unsigned.UInt64.to_int v)
  | _ -> fail "expected UData"

let test_write_attr_indexed_string () =
  let buf = Buffer.create 64 in
  Dwarf_write.write_attribute_value buf
    (IndexedString (7, "ignored"))
    Dwarf.DW_FORM_strx default_encoding;
  let obj_buf = object_buffer_of_buffer buf in
  let cur = Object.Buffer.cursor obj_buf ~at:0 in
  let idx = Object.Buffer.Read.uleb128 cur in
  check int "strx index roundtrip" 7 idx

let test_write_attr_indexed_address () =
  let result =
    roundtrip_attr_value
      (IndexedAddress (3, u64 0))
      Dwarf.DW_FORM_addrx default_encoding
  in
  match result with
  | IndexedAddress (idx, _) -> check int "addrx index roundtrip" 3 idx
  | _ -> fail "expected IndexedAddress"

let test_attribute_value_size () =
  let enc = default_encoding in
  let check_size msg value form expected =
    let computed = Dwarf_write.attribute_value_size value form enc in
    let buf = Buffer.create 64 in
    Dwarf_write.write_attribute_value buf value form enc;
    let written = Buffer.length buf in
    check int (msg ^ " computed") expected computed;
    check int (msg ^ " matches written") written computed
  in
  check_size "string" (String "hi") Dwarf.DW_FORM_string 3;
  check_size "udata" (UData (u64 128)) Dwarf.DW_FORM_udata 2;
  check_size "sdata" (SData (Signed.Int64.of_int (-1))) Dwarf.DW_FORM_sdata 1;
  check_size "addr" (Address (u64 0)) Dwarf.DW_FORM_addr 8;
  check_size "flag_present" (Flag true) Dwarf.DW_FORM_flag_present 0;
  check_size "flag" (Flag true) Dwarf.DW_FORM_flag 1;
  check_size "ref4" (Reference (u64 0)) Dwarf.DW_FORM_ref4 4;
  check_size "block" (Block "\x01\x02") Dwarf.DW_FORM_block 3

(* GNU extension form parsing tests *)

let parse_form_from_bytes bytes form enc =
  let buf = Buffer.create (String.length bytes) in
  Buffer.add_string buf bytes;
  let obj_buf = object_buffer_of_buffer buf in
  let cur = Object.Buffer.cursor obj_buf ~at:0 in
  Dwarf.DIE.parse_attribute_value cur form enc obj_buf ()

let test_parse_gnu_addr_index () =
  let buf = Buffer.create 8 in
  Dwarf_write.write_uleb128 buf (Unsigned.UInt64.of_int 5);
  let result =
    parse_form_from_bytes (Buffer.contents buf) Dwarf.DW_FORM_GNU_addr_index
      default_encoding
  in
  match result with
  | IndexedAddress (idx, _) -> check int "GNU_addr_index" 5 idx
  | _ -> fail "expected IndexedAddress"

let test_parse_gnu_str_index () =
  let buf = Buffer.create 8 in
  Dwarf_write.write_uleb128 buf (Unsigned.UInt64.of_int 3);
  let result =
    parse_form_from_bytes (Buffer.contents buf) Dwarf.DW_FORM_GNU_str_index
      default_encoding
  in
  match result with
  | IndexedString (idx, _) -> check int "GNU_str_index" 3 idx
  | _ -> fail "expected IndexedString"

let test_parse_gnu_ref_alt () =
  let buf = Buffer.create 8 in
  Dwarf_write.write_offset buf Dwarf.DWARF32 (Unsigned.UInt64.of_int 0x1234);
  let result =
    parse_form_from_bytes (Buffer.contents buf) Dwarf.DW_FORM_GNU_ref_alt
      default_encoding
  in
  match result with
  | Reference v -> check int "GNU_ref_alt" 0x1234 (Unsigned.UInt64.to_int v)
  | _ -> fail "expected Reference"

let test_parse_gnu_strp_alt () =
  let buf = Buffer.create 8 in
  Dwarf_write.write_offset buf Dwarf.DWARF32 (Unsigned.UInt64.of_int 0x5678);
  let result =
    parse_form_from_bytes (Buffer.contents buf) Dwarf.DW_FORM_GNU_strp_alt
      default_encoding
  in
  match result with
  | String s ->
      check bool "GNU_strp_alt returns string" true (String.length s > 0)
  | _ -> fail "expected String"

(* Stage 4: DIE tree tests *)

let abbrev_hashtable_of_array (abbrevs : Dwarf.abbrev array) =
  let table = Hashtbl.create (Array.length abbrevs) in
  Array.iter (fun (a : Dwarf.abbrev) -> Hashtbl.add table a.code a) abbrevs;
  table

let test_write_die_simple () =
  let die : Dwarf.DIE.t =
    {
      tag = DW_TAG_compile_unit;
      attributes =
        [
          { attr = DW_AT_name; value = String "test.c" };
          { attr = DW_AT_producer; value = String "durin" };
        ];
      children = Seq.empty;
      offset = 0;
    }
  in
  let abbrevs, lookup = Dwarf_write.assign_abbreviations [ die ] in
  let table = abbrev_hashtable_of_array abbrevs in
  let buf = Buffer.create 64 in
  Dwarf_write.write_die buf die default_encoding lookup;
  let obj_buf = object_buffer_of_buffer buf in
  let cur = Object.Buffer.cursor obj_buf ~at:0 in
  match Dwarf.DIE.parse_die cur table default_encoding obj_buf with
  | None -> fail "expected Some die"
  | Some parsed -> (
      check int "tag" 0x11
        (Unsigned.UInt64.to_int (Dwarf.uint64_of_abbreviation_tag parsed.tag));
      check int "attr count" 2 (List.length parsed.attributes);
      let name_attr = List.hd parsed.attributes in
      (match name_attr.value with
      | String s -> check string "name" "test.c" s
      | _ -> fail "expected String for name");
      let producer_attr = List.nth parsed.attributes 1 in
      match producer_attr.value with
      | String s -> check string "producer" "durin" s
      | _ -> fail "expected String for producer")

let test_write_die_with_children () =
  let child : Dwarf.DIE.t =
    {
      tag = DW_TAG_base_type;
      attributes =
        [
          { attr = DW_AT_name; value = String "int" };
          { attr = DW_AT_byte_size; value = UData (u64 4) };
        ];
      children = Seq.empty;
      offset = 20;
    }
  in
  let parent : Dwarf.DIE.t =
    {
      tag = DW_TAG_compile_unit;
      attributes = [ { attr = DW_AT_name; value = String "test.c" } ];
      children = List.to_seq [ child ];
      offset = 0;
    }
  in
  let abbrevs, lookup = Dwarf_write.assign_abbreviations [ parent ] in
  let table = abbrev_hashtable_of_array abbrevs in
  let buf = Buffer.create 64 in
  Dwarf_write.write_die buf parent default_encoding lookup;
  let obj_buf = object_buffer_of_buffer buf in
  let cur = Object.Buffer.cursor obj_buf ~at:0 in
  match Dwarf.DIE.parse_die cur table default_encoding obj_buf with
  | None -> fail "expected Some die"
  | Some parsed -> (
      check int "parent tag" 0x11
        (Unsigned.UInt64.to_int (Dwarf.uint64_of_abbreviation_tag parsed.tag));
      match parsed.children () with
      | Seq.Nil -> fail "expected children"
      | Seq.Cons (child_parsed, _) -> (
          check int "child tag" 0x24
            (Unsigned.UInt64.to_int
               (Dwarf.uint64_of_abbreviation_tag child_parsed.tag));
          check int "child attr count" 2 (List.length child_parsed.attributes);
          let name = List.hd child_parsed.attributes in
          match name.value with
          | String s -> check string "child name" "int" s
          | _ -> fail "expected String"))

let test_write_die_language_roundtrip () =
  let die : Dwarf.DIE.t =
    {
      tag = DW_TAG_compile_unit;
      attributes =
        [
          { attr = DW_AT_name; value = String "test.ml" };
          { attr = DW_AT_language; value = Language Dwarf.DW_LANG_OCaml };
        ];
      children = Seq.empty;
      offset = 0;
    }
  in
  let abbrevs, lookup = Dwarf_write.assign_abbreviations [ die ] in
  let table = abbrev_hashtable_of_array abbrevs in
  let buf = Buffer.create 64 in
  Dwarf_write.write_die buf die default_encoding lookup;
  let obj_buf = object_buffer_of_buffer buf in
  let cur = Object.Buffer.cursor obj_buf ~at:0 in
  match Dwarf.DIE.parse_die cur table default_encoding obj_buf with
  | None -> fail "expected Some die"
  | Some parsed -> (
      let lang_attr = List.nth parsed.attributes 1 in
      match lang_attr.value with
      | Language l ->
          check string "language roundtrip" "DW_LANG_OCaml"
            (Dwarf.string_of_dwarf_language l)
      | _ -> fail "expected Language")

let test_die_size () =
  let die : Dwarf.DIE.t =
    {
      tag = DW_TAG_compile_unit;
      attributes =
        [
          { attr = DW_AT_name; value = String "test.c" };
          { attr = DW_AT_producer; value = String "durin" };
        ];
      children = Seq.empty;
      offset = 0;
    }
  in
  let _, lookup = Dwarf_write.assign_abbreviations [ die ] in
  let buf = Buffer.create 64 in
  Dwarf_write.write_die buf die default_encoding lookup;
  let written = Buffer.length buf in
  let computed = Dwarf_write.die_size die default_encoding lookup in
  check int "die_size matches written" written computed

let test_die_size_with_children () =
  let child : Dwarf.DIE.t =
    {
      tag = DW_TAG_base_type;
      attributes = [ { attr = DW_AT_name; value = String "int" } ];
      children = Seq.empty;
      offset = 20;
    }
  in
  let parent : Dwarf.DIE.t =
    {
      tag = DW_TAG_compile_unit;
      attributes = [ { attr = DW_AT_name; value = String "test.c" } ];
      children = List.to_seq [ child ];
      offset = 0;
    }
  in
  let _, lookup = Dwarf_write.assign_abbreviations [ parent ] in
  let buf = Buffer.create 64 in
  Dwarf_write.write_die buf parent default_encoding lookup;
  let written = Buffer.length buf in
  let computed = Dwarf_write.die_size parent default_encoding lookup in
  check int "die_size with children matches written" written computed

let test_write_die_forest () =
  let die1 : Dwarf.DIE.t =
    {
      tag = DW_TAG_compile_unit;
      attributes = [ { attr = DW_AT_name; value = String "a.c" } ];
      children = Seq.empty;
      offset = 0;
    }
  in
  let die2 : Dwarf.DIE.t =
    {
      tag = DW_TAG_compile_unit;
      attributes = [ { attr = DW_AT_name; value = String "b.c" } ];
      children = Seq.empty;
      offset = 50;
    }
  in
  let abbrevs, lookup = Dwarf_write.assign_abbreviations [ die1; die2 ] in
  let table = abbrev_hashtable_of_array abbrevs in
  let buf = Buffer.create 64 in
  Dwarf_write.write_die_forest buf [ die1; die2 ] default_encoding lookup;
  let obj_buf = object_buffer_of_buffer buf in
  let cur = Object.Buffer.cursor obj_buf ~at:0 in
  (match Dwarf.DIE.parse_die cur table default_encoding obj_buf with
  | None -> fail "expected first die"
  | Some d1 -> (
      match (List.hd d1.attributes).value with
      | String s -> check string "first die name" "a.c" s
      | _ -> fail "expected String"));
  match Dwarf.DIE.parse_die cur table default_encoding obj_buf with
  | None -> fail "expected second die"
  | Some d2 -> (
      match (List.hd d2.attributes).value with
      | String s -> check string "second die name" "b.c" s
      | _ -> fail "expected String")

(* Stage 5: Compilation unit & top-level tests *)

let object_buffer_of_string s =
  let filename = Filename.temp_file "dwarf_write_test_" ".bin" in
  let oc = open_out_bin filename in
  output_string oc s;
  close_out oc;
  let obj_buf = Object.Buffer.parse filename in
  Sys.remove filename;
  obj_buf

let test_write_compile_unit () =
  let die : Dwarf.DIE.t =
    {
      tag = DW_TAG_compile_unit;
      attributes =
        [
          { attr = DW_AT_name; value = String "test.c" };
          { attr = DW_AT_producer; value = String "durin" };
        ];
      children = Seq.empty;
      offset = 0;
    }
  in
  let enc = default_encoding in
  let abbrevs, lookup = Dwarf_write.assign_abbreviations [ die ] in
  let table = abbrev_hashtable_of_array abbrevs in
  let buf = Buffer.create 64 in
  Dwarf_write.write_compile_unit buf enc die lookup Unsigned.UInt64.zero;
  let obj_buf = object_buffer_of_buffer buf in
  let cur = Object.Buffer.cursor obj_buf ~at:0 in
  let _span, header = Dwarf.parse_compile_unit_header cur in
  check int "version" 5 (Unsigned.UInt16.to_int header.version);
  check string "unit_type" "DW_UT_compile"
    (Dwarf.string_of_unit_type header.unit_type);
  check int "address_size" 8 (Unsigned.UInt8.to_int header.address_size);
  check int64 "debug_abbrev_offset" 0L
    (Unsigned.UInt64.to_int64 header.debug_abbrev_offset);
  match Dwarf.DIE.parse_die cur table enc obj_buf with
  | None -> fail "expected die"
  | Some parsed ->
      check int "tag" 0x11
        (Unsigned.UInt64.to_int (Dwarf.uint64_of_abbreviation_tag parsed.tag));
      check int "attr count" 2 (List.length parsed.attributes)

let test_write_debug_info_simple () =
  let die : Dwarf.DIE.t =
    {
      tag = DW_TAG_compile_unit;
      attributes =
        [
          { attr = DW_AT_name; value = String "hello.c" };
          { attr = DW_AT_language; value = Language Dwarf.DW_LANG_C99 };
        ];
      children = Seq.empty;
      offset = 0;
    }
  in
  let enc = default_encoding in
  let info_str, abbrev_str = Dwarf_write.write_debug_info enc [ die ] in
  let abbrev_buf = object_buffer_of_string abbrev_str in
  let abbrev_cur = Object.Buffer.cursor abbrev_buf ~at:0 in
  let abbrev_table = parse_abbrev_from_cursor abbrev_cur in
  let info_buf = object_buffer_of_string info_str in
  let info_cur = Object.Buffer.cursor info_buf ~at:0 in
  let _span, header = Dwarf.parse_compile_unit_header info_cur in
  check int "version" 5 (Unsigned.UInt16.to_int header.version);
  check int "address_size" 8 (Unsigned.UInt8.to_int header.address_size);
  match Dwarf.DIE.parse_die info_cur abbrev_table enc info_buf with
  | None -> fail "expected die"
  | Some parsed -> (
      check int "tag" 0x11
        (Unsigned.UInt64.to_int (Dwarf.uint64_of_abbreviation_tag parsed.tag));
      let name = List.hd parsed.attributes in
      (match name.value with
      | String s -> check string "name" "hello.c" s
      | _ -> fail "expected String for name");
      let lang = List.nth parsed.attributes 1 in
      match lang.value with
      | Language l ->
          check string "language" "DW_LANG_C99"
            (Dwarf.string_of_dwarf_language l)
      | _ -> fail "expected Language")

let test_write_debug_info_with_children () =
  let child : Dwarf.DIE.t =
    {
      tag = DW_TAG_base_type;
      attributes =
        [
          { attr = DW_AT_name; value = String "int" };
          { attr = DW_AT_byte_size; value = UData (u64 4) };
          { attr = DW_AT_encoding; value = Encoding Dwarf.DW_ATE_signed };
        ];
      children = Seq.empty;
      offset = 20;
    }
  in
  let die : Dwarf.DIE.t =
    {
      tag = DW_TAG_compile_unit;
      attributes =
        [
          { attr = DW_AT_name; value = String "types.c" };
          { attr = DW_AT_language; value = Language Dwarf.DW_LANG_C11 };
        ];
      children = List.to_seq [ child ];
      offset = 0;
    }
  in
  let enc = default_encoding in
  let info_str, abbrev_str = Dwarf_write.write_debug_info enc [ die ] in
  let abbrev_buf = object_buffer_of_string abbrev_str in
  let abbrev_cur = Object.Buffer.cursor abbrev_buf ~at:0 in
  let abbrev_table = parse_abbrev_from_cursor abbrev_cur in
  let info_buf = object_buffer_of_string info_str in
  let info_cur = Object.Buffer.cursor info_buf ~at:0 in
  let _span, _header = Dwarf.parse_compile_unit_header info_cur in
  match Dwarf.DIE.parse_die info_cur abbrev_table enc info_buf with
  | None -> fail "expected die"
  | Some parsed -> (
      match parsed.children () with
      | Seq.Nil -> fail "expected children"
      | Seq.Cons (child_parsed, _) -> (
          check int "child tag" 0x24
            (Unsigned.UInt64.to_int
               (Dwarf.uint64_of_abbreviation_tag child_parsed.tag));
          check int "child attrs" 3 (List.length child_parsed.attributes);
          let enc_attr = List.nth child_parsed.attributes 2 in
          match enc_attr.value with
          | Encoding e ->
              check string "encoding" "DW_ATE_signed"
                (Dwarf.string_of_base_type e)
          | _ -> fail "expected Encoding"))

let test_write_debug_info_unit_length () =
  let die : Dwarf.DIE.t =
    {
      tag = DW_TAG_compile_unit;
      attributes = [ { attr = DW_AT_name; value = String "test.c" } ];
      children = Seq.empty;
      offset = 0;
    }
  in
  let enc = default_encoding in
  let info_str, _ = Dwarf_write.write_debug_info enc [ die ] in
  let info_buf = object_buffer_of_string info_str in
  let info_cur = Object.Buffer.cursor info_buf ~at:0 in
  let _span, header = Dwarf.parse_compile_unit_header info_cur in
  let expected_total = String.length info_str in
  let unit_length = Unsigned.UInt64.to_int header.unit_length in
  check int "unit_length + 4 = total size" expected_total (unit_length + 4)

(* Stage 6: String table tests *)

let test_string_table_offsets () =
  let table = Dwarf_write.create_string_table () in
  let off1 = Dwarf_write.add_string table "hello" in
  let off2 = Dwarf_write.add_string table "world" in
  check int "first string offset" 0 off1;
  check int "second string offset" 6 off2

let test_string_table_dedup () =
  let table = Dwarf_write.create_string_table () in
  let off1 = Dwarf_write.add_string table "hello" in
  let off2 = Dwarf_write.add_string table "hello" in
  check int "same offset" off1 off2

let test_string_table_roundtrip () =
  let table = Dwarf_write.create_string_table () in
  let _off1 = Dwarf_write.add_string table "hello" in
  let _off2 = Dwarf_write.add_string table "world" in
  let buf = Buffer.create 64 in
  Dwarf_write.write_string_table buf table;
  let obj_buf = object_buffer_of_buffer buf in
  let cur0 = Object.Buffer.cursor obj_buf ~at:0 in
  let s1 = Object.Buffer.Read.zero_string cur0 () in
  check (option string) "first string" (Some "hello") s1;
  let cur6 = Object.Buffer.cursor obj_buf ~at:6 in
  let s2 = Object.Buffer.Read.zero_string cur6 () in
  check (option string) "second string" (Some "world") s2

let test_string_table_size () =
  let table = Dwarf_write.create_string_table () in
  let _ = Dwarf_write.add_string table "abc" in
  let _ = Dwarf_write.add_string table "de" in
  let buf = Buffer.create 64 in
  Dwarf_write.write_string_table buf table;
  check int "size matches written" (Buffer.length buf)
    (Dwarf_write.string_table_size table);
  check int "expected size" 7 (Dwarf_write.string_table_size table)

(* Stage 7: Expression encoding tests *)

let roundtrip_expression ops =
  let buf = Buffer.create 64 in
  Dwarf_write.write_expression buf ops default_encoding;
  let expr_bytes = Buffer.contents buf in
  Dwarf.parse_dwarf_expression ~encoding:default_encoding expr_bytes

let op opcode operands : Dwarf.dwarf_expression_operation =
  { opcode; operands; operand_string = None }

let test_write_expr_no_operands () =
  let ops =
    [ op Dwarf.DW_OP_dup []; op Dwarf.DW_OP_drop []; op Dwarf.DW_OP_nop [] ]
  in
  let parsed = roundtrip_expression ops in
  check int "3 ops" 3 (List.length parsed);
  check string "op0" "DW_OP_dup"
    (Dwarf.string_of_operation_encoding (List.nth parsed 0).opcode);
  check string "op1" "DW_OP_drop"
    (Dwarf.string_of_operation_encoding (List.nth parsed 1).opcode);
  check string "op2" "DW_OP_nop"
    (Dwarf.string_of_operation_encoding (List.nth parsed 2).opcode)

let test_write_expr_literals () =
  let ops = [ op Dwarf.DW_OP_lit0 []; op Dwarf.DW_OP_lit31 [] ] in
  let parsed = roundtrip_expression ops in
  check int "2 ops" 2 (List.length parsed);
  check string "lit0" "DW_OP_lit0"
    (Dwarf.string_of_operation_encoding (List.nth parsed 0).opcode);
  check string "lit31" "DW_OP_lit31"
    (Dwarf.string_of_operation_encoding (List.nth parsed 1).opcode)

let test_write_expr_const1u () =
  let ops = [ op Dwarf.DW_OP_const1u [ 42 ] ] in
  let parsed = roundtrip_expression ops in
  check int "1 op" 1 (List.length parsed);
  check string "opcode" "DW_OP_const1u"
    (Dwarf.string_of_operation_encoding (List.hd parsed).opcode);
  check int "operand" 42 (List.hd (List.hd parsed).operands)

let test_write_expr_const2u () =
  let ops = [ op Dwarf.DW_OP_const2u [ 0x1234 ] ] in
  let parsed = roundtrip_expression ops in
  check int "operand" 0x1234 (List.hd (List.hd parsed).operands)

let test_write_expr_const4u () =
  let ops = [ op Dwarf.DW_OP_const4u [ 0x12345678 ] ] in
  let parsed = roundtrip_expression ops in
  check int "operand" 0x12345678 (List.hd (List.hd parsed).operands)

let test_write_expr_constu () =
  let ops = [ op Dwarf.DW_OP_constu [ 300 ] ] in
  let parsed = roundtrip_expression ops in
  check int "operand" 300 (List.hd (List.hd parsed).operands)

let test_write_expr_consts () =
  let ops = [ op Dwarf.DW_OP_consts [ -42 ] ] in
  let parsed = roundtrip_expression ops in
  check int "operand" (-42) (List.hd (List.hd parsed).operands)

let test_write_expr_fbreg () =
  let ops = [ op Dwarf.DW_OP_fbreg [ -16 ] ] in
  let parsed = roundtrip_expression ops in
  check string "opcode" "DW_OP_fbreg"
    (Dwarf.string_of_operation_encoding (List.hd parsed).opcode);
  check int "operand" (-16) (List.hd (List.hd parsed).operands)

let test_write_expr_breg () =
  let ops = [ op Dwarf.DW_OP_breg7 [ 8 ] ] in
  let parsed = roundtrip_expression ops in
  check string "opcode" "DW_OP_breg7"
    (Dwarf.string_of_operation_encoding (List.hd parsed).opcode);
  check int "operand" 8 (List.hd (List.hd parsed).operands)

let test_write_expr_bregx () =
  let ops = [ op Dwarf.DW_OP_bregx [ 33; -4 ] ] in
  let parsed = roundtrip_expression ops in
  check string "opcode" "DW_OP_bregx"
    (Dwarf.string_of_operation_encoding (List.hd parsed).opcode);
  check int "reg" 33 (List.nth (List.hd parsed).operands 0);
  check int "offset" (-4) (List.nth (List.hd parsed).operands 1)

let test_write_expr_piece () =
  let ops = [ op Dwarf.DW_OP_reg0 []; op Dwarf.DW_OP_piece [ 4 ] ] in
  let parsed = roundtrip_expression ops in
  check int "2 ops" 2 (List.length parsed);
  check int "piece size" 4 (List.hd (List.nth parsed 1).operands)

let test_write_expr_plus_uconst () =
  let ops =
    [ op Dwarf.DW_OP_fbreg [ -32 ]; op Dwarf.DW_OP_plus_uconst [ 8 ] ]
  in
  let parsed = roundtrip_expression ops in
  check int "2 ops" 2 (List.length parsed);
  check int "uconst" 8 (List.hd (List.nth parsed 1).operands)

let test_write_expr_stack_value () =
  let ops = [ op Dwarf.DW_OP_lit5 []; op Dwarf.DW_OP_stack_value [] ] in
  let parsed = roundtrip_expression ops in
  check int "2 ops" 2 (List.length parsed);
  check string "stack_value" "DW_OP_stack_value"
    (Dwarf.string_of_operation_encoding (List.nth parsed 1).opcode)

(* Stage 9: Location/Range list tests *)

let addr_size = 8

let roundtrip_location_list entries =
  let list : Dwarf.DebugLoclists.location_list = { entries } in
  let buf = Buffer.create 64 in
  Dwarf_write.write_location_list buf list addr_size;
  let obj_buf = object_buffer_of_buffer buf in
  let cur = Object.Buffer.cursor obj_buf ~at:0 in
  Dwarf.DebugLoclists.parse_location_list cur (Unsigned.UInt8.of_int addr_size)

let roundtrip_range_list entries =
  let list : Dwarf.DebugRnglists.range_list = { entries } in
  let buf = Buffer.create 64 in
  Dwarf_write.write_range_list buf list addr_size;
  let obj_buf = object_buffer_of_buffer buf in
  let cur = Object.Buffer.cursor obj_buf ~at:0 in
  Dwarf.DebugRnglists.parse_range_list cur (Unsigned.UInt8.of_int addr_size)

let test_write_lle_offset_pair () =
  let entries =
    Dwarf.DebugLoclists.
      [
        LLE_offset_pair
          { start_offset = u64 0x10; end_offset = u64 0x20; expr = "\x50" };
        LLE_end_of_list;
      ]
  in
  let parsed = roundtrip_location_list entries in
  check int "2 entries" 2 (List.length parsed.entries);
  match List.hd parsed.entries with
  | LLE_offset_pair { start_offset; end_offset; expr } ->
      check int "start" 0x10 (Unsigned.UInt64.to_int start_offset);
      check int "end" 0x20 (Unsigned.UInt64.to_int end_offset);
      check int "expr len" 1 (String.length expr)
  | _ -> fail "expected LLE_offset_pair"

let test_write_lle_base_address () =
  let entries =
    Dwarf.DebugLoclists.
      [ LLE_base_address { address = u64 0x400000 }; LLE_end_of_list ]
  in
  let parsed = roundtrip_location_list entries in
  match List.hd parsed.entries with
  | LLE_base_address { address } ->
      check int "addr" 0x400000 (Unsigned.UInt64.to_int address)
  | _ -> fail "expected LLE_base_address"

let test_write_lle_start_end () =
  let entries =
    Dwarf.DebugLoclists.
      [
        LLE_start_end
          { start_addr = u64 0x1000; end_addr = u64 0x2000; expr = "\x50\x51" };
        LLE_end_of_list;
      ]
  in
  let parsed = roundtrip_location_list entries in
  match List.hd parsed.entries with
  | LLE_start_end { start_addr; end_addr; expr } ->
      check int "start" 0x1000 (Unsigned.UInt64.to_int start_addr);
      check int "end" 0x2000 (Unsigned.UInt64.to_int end_addr);
      check int "expr" 2 (String.length expr)
  | _ -> fail "expected LLE_start_end"

let test_write_lle_start_length () =
  let entries =
    Dwarf.DebugLoclists.
      [
        LLE_start_length
          { start_addr = u64 0x3000; length = u64 0x100; expr = "\x50" };
        LLE_end_of_list;
      ]
  in
  let parsed = roundtrip_location_list entries in
  match List.hd parsed.entries with
  | LLE_start_length { start_addr; length; _ } ->
      check int "start" 0x3000 (Unsigned.UInt64.to_int start_addr);
      check int "length" 0x100 (Unsigned.UInt64.to_int length)
  | _ -> fail "expected LLE_start_length"

let test_write_lle_base_addressx () =
  let entries =
    Dwarf.DebugLoclists.[ LLE_base_addressx { index = 3 }; LLE_end_of_list ]
  in
  let parsed = roundtrip_location_list entries in
  match List.hd parsed.entries with
  | LLE_base_addressx { index } -> check int "index" 3 index
  | _ -> fail "expected LLE_base_addressx"

let test_write_lle_default_location () =
  let entries =
    Dwarf.DebugLoclists.
      [ LLE_default_location { expr = "\x50" }; LLE_end_of_list ]
  in
  let parsed = roundtrip_location_list entries in
  match List.hd parsed.entries with
  | LLE_default_location { expr } -> check int "expr len" 1 (String.length expr)
  | _ -> fail "expected LLE_default_location"

let test_write_rle_offset_pair () =
  let entries =
    Dwarf.DebugRnglists.
      [
        RLE_offset_pair { start_offset = u64 0x10; end_offset = u64 0x30 };
        RLE_end_of_list;
      ]
  in
  let parsed = roundtrip_range_list entries in
  check int "2 entries" 2 (List.length parsed.entries);
  match List.hd parsed.entries with
  | RLE_offset_pair { start_offset; end_offset } ->
      check int "start" 0x10 (Unsigned.UInt64.to_int start_offset);
      check int "end" 0x30 (Unsigned.UInt64.to_int end_offset)
  | _ -> fail "expected RLE_offset_pair"

let test_write_rle_base_address () =
  let entries =
    Dwarf.DebugRnglists.
      [ RLE_base_address { address = u64 0x500000 }; RLE_end_of_list ]
  in
  let parsed = roundtrip_range_list entries in
  match List.hd parsed.entries with
  | RLE_base_address { address } ->
      check int "addr" 0x500000 (Unsigned.UInt64.to_int address)
  | _ -> fail "expected RLE_base_address"

let test_write_rle_start_end () =
  let entries =
    Dwarf.DebugRnglists.
      [
        RLE_start_end { start_addr = u64 0x1000; end_addr = u64 0x2000 };
        RLE_end_of_list;
      ]
  in
  let parsed = roundtrip_range_list entries in
  match List.hd parsed.entries with
  | RLE_start_end { start_addr; end_addr } ->
      check int "start" 0x1000 (Unsigned.UInt64.to_int start_addr);
      check int "end" 0x2000 (Unsigned.UInt64.to_int end_addr)
  | _ -> fail "expected RLE_start_end"

let test_write_rle_start_length () =
  let entries =
    Dwarf.DebugRnglists.
      [
        RLE_start_length { start_addr = u64 0x4000; length = u64 0x200 };
        RLE_end_of_list;
      ]
  in
  let parsed = roundtrip_range_list entries in
  match List.hd parsed.entries with
  | RLE_start_length { start_addr; length } ->
      check int "start" 0x4000 (Unsigned.UInt64.to_int start_addr);
      check int "length" 0x200 (Unsigned.UInt64.to_int length)
  | _ -> fail "expected RLE_start_length"

let test_write_debug_loc_roundtrip () =
  let entries =
    Dwarf.DebugLoc.
      [
        Location
          { begin_addr = u64 0x1000; end_addr = u64 0x1010; expr = "\x50" };
        EndOfList;
      ]
  in
  let buf = Buffer.create 64 in
  Dwarf_write.write_debug_loc buf entries addr_size;
  let obj_buf = object_buffer_of_buffer buf in
  let cur = Object.Buffer.cursor obj_buf ~at:0 in
  let parsed = Dwarf.DebugLoc.parse_list cur addr_size in
  check int "2 entries" 2 (List.length parsed);
  match List.hd parsed with
  | Location { begin_addr; end_addr; expr } ->
      check int "begin" 0x1000 (Unsigned.UInt64.to_int begin_addr);
      check int "end" 0x1010 (Unsigned.UInt64.to_int end_addr);
      check int "expr" 1 (String.length expr)
  | _ -> fail "expected Location"

let test_write_debug_ranges_roundtrip () =
  let entries =
    Dwarf.DebugRanges.
      [ Range { begin_addr = u64 0x2000; end_addr = u64 0x3000 }; EndOfList ]
  in
  let buf = Buffer.create 64 in
  Dwarf_write.write_debug_ranges buf entries addr_size;
  let obj_buf = object_buffer_of_buffer buf in
  let cur = Object.Buffer.cursor obj_buf ~at:0 in
  let parsed = Dwarf.DebugRanges.parse_list cur addr_size in
  check int "2 entries" 2 (List.length parsed);
  match List.hd parsed with
  | Range { begin_addr; end_addr } ->
      check int "begin" 0x2000 (Unsigned.UInt64.to_int begin_addr);
      check int "end" 0x3000 (Unsigned.UInt64.to_int end_addr)
  | _ -> fail "expected Range"

(* Stage 10: Line Program Writer tests *)

let default_line_header () : Dwarf.DebugLine.line_program_header =
  {
    format = DWARF32;
    unit_length = Unsigned.UInt64.zero;
    version = Unsigned.UInt16.of_int 5;
    address_size = Unsigned.UInt8.of_int 8;
    segment_selector_size = Unsigned.UInt8.of_int 0;
    header_length = Unsigned.UInt64.zero;
    minimum_instruction_length = Unsigned.UInt8.of_int 1;
    maximum_operations_per_instruction = Unsigned.UInt8.of_int 1;
    default_is_stmt = true;
    line_base = -5;
    line_range = Unsigned.UInt8.of_int 14;
    opcode_base = Unsigned.UInt8.of_int 13;
    standard_opcode_lengths =
      Array.map Unsigned.UInt8.of_int [| 0; 1; 1; 1; 1; 0; 0; 0; 1; 0; 0; 1 |];
    directory_entry_format_count = Unsigned.UInt8.of_int 1;
    directory_entry_formats = [| (Dwarf.DW_LNCT_path, Dwarf.DW_FORM_string) |];
    directories_count = Unsigned.UInt32.of_int 1;
    directories = [| "/src" |];
    file_name_entry_format_count = Unsigned.UInt8.of_int 2;
    file_name_entry_formats =
      [|
        (Dwarf.DW_LNCT_path, Dwarf.DW_FORM_string);
        (Dwarf.DW_LNCT_directory_index, Dwarf.DW_FORM_udata);
      |];
    file_names_count = Unsigned.UInt32.of_int 1;
    file_names =
      [|
        {
          name = "main.c";
          timestamp = Unsigned.UInt64.zero;
          size = Unsigned.UInt64.zero;
          directory = "/src";
          md5_checksum = None;
        };
      |];
  }

let line_entry ?(file = 0) ?(col = 0) ?(is_stmt = true) ?(bb = false)
    ?(pe = false) ?(eb = false) ?(disc = 0) ?(isa = 0) ?(es = false) ~addr ~ln
    () : Dwarf.DebugLine.line_table_entry =
  {
    address = Unsigned.UInt64.of_int addr;
    line = Unsigned.UInt32.of_int ln;
    column = Unsigned.UInt32.of_int col;
    file_index = Unsigned.UInt32.of_int file;
    isa = Unsigned.UInt32.of_int isa;
    discriminator = Unsigned.UInt32.of_int disc;
    op_index = Unsigned.UInt32.of_int 0;
    is_stmt;
    basic_block = bb;
    end_sequence = es;
    prologue_end = pe;
    epilogue_begin = eb;
  }

let test_write_debug_line_header () =
  let header = default_line_header () in
  let buf = Buffer.create 256 in
  Dwarf_write.write_debug_line buf header [];
  let obj_buf = object_buffer_of_buffer buf in
  let cur = Object.Buffer.cursor obj_buf ~at:0 in
  let p = Dwarf.DebugLine.parse_line_program_header cur obj_buf in
  check int "version" 5 (Unsigned.UInt16.to_int p.version);
  check int "address_size" 8 (Unsigned.UInt8.to_int p.address_size);
  check int "line_base" (-5) p.line_base;
  check int "line_range" 14 (Unsigned.UInt8.to_int p.line_range);
  check int "opcode_base" 13 (Unsigned.UInt8.to_int p.opcode_base);
  check int "min_inst_len" 1
    (Unsigned.UInt8.to_int p.minimum_instruction_length);
  check bool "default_is_stmt" true p.default_is_stmt;
  check int "dirs" 1 (Array.length p.directories);
  check string "dir0" "/src" p.directories.(0);
  check int "files" 1 (Array.length p.file_names);
  check string "file0" "main.c" p.file_names.(0).name;
  check string "file0_dir" "/src" p.file_names.(0).directory

let test_write_debug_line_simple () =
  let header = default_line_header () in
  let entries =
    [
      line_entry ~addr:0x1000 ~ln:1 ();
      line_entry ~addr:0x1004 ~ln:2 ();
      line_entry ~addr:0x1010 ~ln:5 ~es:true ();
    ]
  in
  let buf = Buffer.create 256 in
  Dwarf_write.write_debug_line buf header entries;
  let obj_buf = object_buffer_of_buffer buf in
  let cur = Object.Buffer.cursor obj_buf ~at:0 in
  let ph = Dwarf.DebugLine.parse_line_program_header cur obj_buf in
  let parsed = Dwarf.DebugLine.parse_line_program cur ph in
  check int "3 entries" 3 (List.length parsed);
  let e0 = List.nth parsed 0 in
  check int "e0.addr" 0x1000 (Unsigned.UInt64.to_int e0.address);
  check int "e0.line" 1 (Unsigned.UInt32.to_int e0.line);
  let e1 = List.nth parsed 1 in
  check int "e1.addr" 0x1004 (Unsigned.UInt64.to_int e1.address);
  check int "e1.line" 2 (Unsigned.UInt32.to_int e1.line);
  let e2 = List.nth parsed 2 in
  check int "e2.addr" 0x1010 (Unsigned.UInt64.to_int e2.address);
  check bool "e2.end_seq" true e2.end_sequence

let test_write_debug_line_columns () =
  let header = default_line_header () in
  let entries =
    [
      line_entry ~addr:0x2000 ~ln:10 ~col:5 ();
      line_entry ~addr:0x2008 ~ln:10 ~col:20 ();
      line_entry ~addr:0x2010 ~ln:11 ~col:1 ~pe:true ();
      line_entry ~addr:0x2020 ~ln:0 ~es:true ();
    ]
  in
  let buf = Buffer.create 256 in
  Dwarf_write.write_debug_line buf header entries;
  let obj_buf = object_buffer_of_buffer buf in
  let cur = Object.Buffer.cursor obj_buf ~at:0 in
  let ph = Dwarf.DebugLine.parse_line_program_header cur obj_buf in
  let parsed = Dwarf.DebugLine.parse_line_program cur ph in
  check int "4 entries" 4 (List.length parsed);
  let e0 = List.nth parsed 0 in
  check int "e0.col" 5 (Unsigned.UInt32.to_int e0.column);
  let e1 = List.nth parsed 1 in
  check int "e1.col" 20 (Unsigned.UInt32.to_int e1.column);
  check int "e1.line" 10 (Unsigned.UInt32.to_int e1.line);
  let e2 = List.nth parsed 2 in
  check int "e2.col" 1 (Unsigned.UInt32.to_int e2.column);
  check int "e2.line" 11 (Unsigned.UInt32.to_int e2.line);
  check bool "e2.pe" true e2.prologue_end

let test_write_debug_line_stmt_toggle () =
  let header = default_line_header () in
  let entries =
    [
      line_entry ~addr:0x3000 ~ln:1 ();
      line_entry ~addr:0x3004 ~ln:2 ~is_stmt:false ();
      line_entry ~addr:0x3008 ~ln:3 ();
      line_entry ~addr:0x3010 ~ln:0 ~es:true ();
    ]
  in
  let buf = Buffer.create 256 in
  Dwarf_write.write_debug_line buf header entries;
  let obj_buf = object_buffer_of_buffer buf in
  let cur = Object.Buffer.cursor obj_buf ~at:0 in
  let ph = Dwarf.DebugLine.parse_line_program_header cur obj_buf in
  let parsed = Dwarf.DebugLine.parse_line_program cur ph in
  check int "4 entries" 4 (List.length parsed);
  check bool "e0.is_stmt" true (List.nth parsed 0).is_stmt;
  check bool "e1.is_stmt" false (List.nth parsed 1).is_stmt;
  check bool "e2.is_stmt" true (List.nth parsed 2).is_stmt

let test_write_debug_line_multi_seq () =
  let header = default_line_header () in
  let entries =
    [
      line_entry ~addr:0x1000 ~ln:1 ();
      line_entry ~addr:0x1010 ~ln:0 ~es:true ();
      line_entry ~addr:0x2000 ~ln:10 ();
      line_entry ~addr:0x2020 ~ln:0 ~es:true ();
    ]
  in
  let buf = Buffer.create 256 in
  Dwarf_write.write_debug_line buf header entries;
  let obj_buf = object_buffer_of_buffer buf in
  let cur = Object.Buffer.cursor obj_buf ~at:0 in
  let ph = Dwarf.DebugLine.parse_line_program_header cur obj_buf in
  let parsed = Dwarf.DebugLine.parse_line_program cur ph in
  check int "4 entries" 4 (List.length parsed);
  let e0 = List.nth parsed 0 in
  check int "e0.addr" 0x1000 (Unsigned.UInt64.to_int e0.address);
  let e2 = List.nth parsed 2 in
  check int "e2.addr" 0x2000 (Unsigned.UInt64.to_int e2.address);
  check int "e2.line" 10 (Unsigned.UInt32.to_int e2.line)

let test_write_debug_line_discriminator () =
  let header = default_line_header () in
  let entries =
    [
      line_entry ~addr:0x4000 ~ln:1 ~disc:1 ();
      line_entry ~addr:0x4004 ~ln:1 ~disc:2 ();
      line_entry ~addr:0x4010 ~ln:0 ~es:true ();
    ]
  in
  let buf = Buffer.create 256 in
  Dwarf_write.write_debug_line buf header entries;
  let obj_buf = object_buffer_of_buffer buf in
  let cur = Object.Buffer.cursor obj_buf ~at:0 in
  let ph = Dwarf.DebugLine.parse_line_program_header cur obj_buf in
  let parsed = Dwarf.DebugLine.parse_line_program cur ph in
  check int "3 entries" 3 (List.length parsed);
  check int "e0.disc" 1
    (Unsigned.UInt32.to_int (List.nth parsed 0).discriminator);
  check int "e1.disc" 2
    (Unsigned.UInt32.to_int (List.nth parsed 1).discriminator)

let line_strp_line_header () : Dwarf.DebugLine.line_program_header =
  {
    format = DWARF32;
    unit_length = Unsigned.UInt64.zero;
    version = Unsigned.UInt16.of_int 5;
    address_size = Unsigned.UInt8.of_int 8;
    segment_selector_size = Unsigned.UInt8.of_int 0;
    header_length = Unsigned.UInt64.zero;
    minimum_instruction_length = Unsigned.UInt8.of_int 1;
    maximum_operations_per_instruction = Unsigned.UInt8.of_int 1;
    default_is_stmt = true;
    line_base = -5;
    line_range = Unsigned.UInt8.of_int 14;
    opcode_base = Unsigned.UInt8.of_int 13;
    standard_opcode_lengths =
      Array.map Unsigned.UInt8.of_int [| 0; 1; 1; 1; 1; 0; 0; 0; 1; 0; 0; 1 |];
    directory_entry_format_count = Unsigned.UInt8.of_int 1;
    directory_entry_formats =
      [| (Dwarf.DW_LNCT_path, Dwarf.DW_FORM_line_strp) |];
    directories_count = Unsigned.UInt32.of_int 1;
    directories = [| "/src" |];
    file_name_entry_format_count = Unsigned.UInt8.of_int 2;
    file_name_entry_formats =
      [|
        (Dwarf.DW_LNCT_path, Dwarf.DW_FORM_line_strp);
        (Dwarf.DW_LNCT_directory_index, Dwarf.DW_FORM_udata);
      |];
    file_names_count = Unsigned.UInt32.of_int 1;
    file_names =
      [|
        {
          name = "main.c";
          timestamp = Unsigned.UInt64.zero;
          size = Unsigned.UInt64.zero;
          directory = "/src";
          md5_checksum = None;
        };
      |];
  }

let test_write_debug_line_line_strp () =
  let header = line_strp_line_header () in
  let line_str_table = Dwarf_write.create_string_table () in
  let entries =
    [
      line_entry ~addr:0x1000 ~ln:1 ();
      line_entry ~addr:0x1010 ~ln:5 ~es:true ();
    ]
  in
  let buf = Buffer.create 256 in
  Dwarf_write.write_debug_line buf ~line_str_table header entries;
  let obj_buf = object_buffer_of_buffer buf in
  (* Verify strings were added to line_str_table *)
  check bool "table non-empty"
    (Dwarf_write.string_table_size line_str_table > 0)
    true;
  (* Verify we can read the strings from the table *)
  let str_buf = Buffer.create 64 in
  Dwarf_write.write_debug_line_str str_buf line_str_table;
  let str_obj = object_buffer_of_buffer str_buf in
  let str_cur = Object.Buffer.cursor str_obj ~at:0 in
  let s1 = Object.Buffer.Read.zero_string str_cur () in
  check (option string) "first string in line_str" (Some "/src") s1;
  (* Parse the line program header back *)
  let cur = Object.Buffer.cursor obj_buf ~at:0 in
  let ph = Dwarf.DebugLine.parse_line_program_header cur obj_buf in
  check int "version" 5 (Unsigned.UInt16.to_int ph.version);
  (* Verify the directory format uses line_strp *)
  let _dir_ct, dir_form = ph.directory_entry_formats.(0) in
  check string "dir form" "DW_FORM_line_strp"
    (Dwarf.string_of_attribute_form_encoding_variant dir_form)

let test_write_debug_line_missing_table () =
  let header = line_strp_line_header () in
  let raised =
    try
      let buf = Buffer.create 256 in
      Dwarf_write.write_debug_line buf header [];
      false
    with Invalid_argument _ -> true
  in
  check bool "raises Invalid_argument" true raised

let test_write_debug_line_str_dedup () =
  let table = Dwarf_write.create_string_table () in
  let off1 = Dwarf_write.add_string table "hello" in
  let off2 = Dwarf_write.add_string table "hello" in
  check int "dedup same offset" off1 off2;
  let buf = Buffer.create 64 in
  Dwarf_write.write_debug_line_str buf table;
  check int "size = 6 (hello + null)" 6 (Buffer.length buf)

let test_write_debug_str_alias () =
  let table = Dwarf_write.create_string_table () in
  let _ = Dwarf_write.add_string table "abc" in
  let buf1 = Buffer.create 16 in
  Dwarf_write.write_debug_str buf1 table;
  let buf2 = Buffer.create 16 in
  Dwarf_write.write_string_table buf2 table;
  check string "write_debug_str = write_string_table" (Buffer.contents buf1)
    (Buffer.contents buf2)

let test_write_cie_roundtrip () =
  let cie = Dwarf.CallFrame.create_default_cie () in
  let buf = Buffer.create 64 in
  Dwarf_write.write_cie buf cie;
  let obj_buf = object_buffer_of_buffer buf in
  let cur = Object.Buffer.cursor obj_buf ~at:0 in
  let p = Dwarf.CallFrame.parse_common_information_entry cur in
  check int "version"
    (Unsigned.UInt8.to_int cie.version)
    (Unsigned.UInt8.to_int p.version);
  check int "address_size"
    (Unsigned.UInt8.to_int cie.address_size)
    (Unsigned.UInt8.to_int p.address_size);
  check int "code_alignment"
    (Unsigned.UInt64.to_int cie.code_alignment_factor)
    (Unsigned.UInt64.to_int p.code_alignment_factor);
  check int "data_alignment"
    (Signed.Int64.to_int cie.data_alignment_factor)
    (Signed.Int64.to_int p.data_alignment_factor);
  check int "return_addr_reg"
    (Unsigned.UInt64.to_int cie.return_address_register)
    (Unsigned.UInt64.to_int p.return_address_register);
  check string "augmentation" cie.augmentation p.augmentation

let test_write_cfi_initial_state () =
  let ibuf = Buffer.create 16 in
  Dwarf_write.write_cfi_instructions ibuf
    [ CFA_def_cfa (7, 8); CFA_offset (16, 1) ];
  let instructions = Buffer.contents ibuf in
  let cie =
    {
      (Dwarf.CallFrame.create_default_cie ()) with
      initial_instructions = instructions;
    }
  in
  let state = Dwarf.CallFrame.parse_initial_state cie in
  check int "cfa_register" 7 state.cfa_register;
  check int "cfa_offset" 8 (Int64.to_int state.cfa_offset);
  match Hashtbl.find_opt state.register_rules 16 with
  | Some (Dwarf.CallFrame.Rule_offset off) ->
      check int "r16 offset" (-8) (Int64.to_int off)
  | _ -> fail "r16 should have Rule_offset"

let test_write_debug_frame_section () =
  let cie = Dwarf.CallFrame.create_default_cie () in
  let fde : Dwarf.CallFrame.frame_description_entry =
    {
      format = Dwarf.DWARF32;
      length = Unsigned.UInt64.of_int 0;
      cie_pointer = Unsigned.UInt64.of_int 0;
      initial_location = Unsigned.UInt64.of_int 0x401000;
      address_range = Unsigned.UInt64.of_int 0x80;
      augmentation_length = None;
      augmentation_data = None;
      instructions = "";
      offset = Unsigned.UInt64.of_int 0;
    }
  in
  let buf = Buffer.create 128 in
  Dwarf_write.write_debug_frame buf
    [ Dwarf.CallFrame.CIE cie; Dwarf.CallFrame.FDE fde ];
  let obj_buf = object_buffer_of_buffer buf in
  let cur = Object.Buffer.cursor obj_buf ~at:0 in
  let section =
    Dwarf.CallFrame.parse_debug_frame_section cur (Buffer.length buf)
  in
  check int "entry_count" 2 section.entry_count;
  (match List.nth section.entries 0 with
  | Dwarf.CallFrame.CIE p ->
      check int "cie version"
        (Unsigned.UInt8.to_int cie.version)
        (Unsigned.UInt8.to_int p.version)
  | _ -> fail "first entry should be CIE");
  match List.nth section.entries 1 with
  | Dwarf.CallFrame.FDE p ->
      check int64 "fde location"
        (Unsigned.UInt64.to_int64 fde.initial_location)
        (Unsigned.UInt64.to_int64 p.initial_location)
  | _ -> fail "second entry should be FDE"

let test_write_cfi_advance_loc () =
  let ibuf = Buffer.create 16 in
  Dwarf_write.write_cfi_instructions ibuf
    [ CFA_def_cfa (7, 8); CFA_advance_loc 4; CFA_def_cfa_offset 16 ];
  let instructions = Buffer.contents ibuf in
  let results = Dwarf.CallFrame.parse_cfi_instructions instructions 1L 1L in
  check int "2 results" 2 (List.length results);
  let pc0, _ = List.nth results 0 in
  check int "first at pc 0" 0 pc0;
  let pc1, _ = List.nth results 1 in
  check int "second at pc 4" 4 pc1

let test_write_loclists_header_roundtrip () =
  let enc = default_encoding in
  let buf = Buffer.create 64 in
  Dwarf_write.write_loclists_header buf enc 0 10;
  let obj_buf = object_buffer_of_buffer buf in
  let cur = Object.Buffer.cursor obj_buf ~at:0 in
  let _fmt, unit_length = Dwarf.parse_initial_length cur in
  let version = Object.Buffer.Read.u16 cur in
  let address_size = Object.Buffer.Read.u8 cur in
  let segment_size = Object.Buffer.Read.u8 cur in
  let offset_entry_count = Object.Buffer.Read.u32 cur in
  check int "version" 5 (Unsigned.UInt16.to_int version);
  check int "address_size" 8 (Unsigned.UInt8.to_int address_size);
  check int "segment_size" 0 (Unsigned.UInt8.to_int segment_size);
  check int "offset_entry_count" 0 (Unsigned.UInt32.to_int offset_entry_count);
  check int "unit_length"
    (2 + 1 + 1 + 4 + 10)
    (Unsigned.UInt64.to_int unit_length)

(* Stage 12: .eh_frame tests *)

let test_write_eh_cie_roundtrip () =
  let cie : Dwarf.CallFrame.common_information_entry =
    {
      format = Dwarf.DWARF32;
      length = Unsigned.UInt64.of_int 0;
      cie_id = Unsigned.UInt64.of_int 0;
      version = Unsigned.UInt8.of_int 1;
      augmentation = "";
      address_size = Unsigned.UInt8.of_int 8;
      segment_selector_size = Unsigned.UInt8.of_int 0;
      code_alignment_factor = Unsigned.UInt64.of_int 1;
      data_alignment_factor = Signed.Int64.of_int (-8);
      return_address_register = Unsigned.UInt64.of_int 16;
      augmentation_length = None;
      augmentation_data = None;
      initial_instructions = "";
      header_span =
        { start = Unsigned.UInt64.of_int 0; size = Unsigned.UInt64.of_int 0 };
      offset = Unsigned.UInt64.of_int 0;
    }
  in
  let buf = Buffer.create 64 in
  Dwarf_write.write_eh_cie buf cie;
  (* Padding: the parser always attempts an FDE parse
     after each CIE in the same iteration *)
  for _ = 1 to 16 do
    Buffer.add_char buf '\x00'
  done;
  let obj_buf = object_buffer_of_buffer buf in
  let cur = Object.Buffer.cursor obj_buf ~at:0 in
  let section = Dwarf.EHFrame.parse_section cur (Buffer.length buf) in
  let cie_entry =
    List.find
      (function Dwarf.EHFrame.EH_CIE _ -> true | _ -> false)
      section.entries
  in
  match cie_entry with
  | Dwarf.EHFrame.EH_CIE p ->
      check int "version" 1 (Unsigned.UInt8.to_int p.version);
      check string "augmentation" "" p.augmentation;
      check int "code_align" 1 (Unsigned.UInt64.to_int p.code_alignment_factor);
      check int "data_align" (-8) (Signed.Int64.to_int p.data_alignment_factor);
      check int "ret_addr_reg" 16
        (Unsigned.UInt64.to_int p.return_address_register)
  | _ -> fail "expected EH_CIE"

let test_write_eh_frame_cie_fde () =
  let cie : Dwarf.CallFrame.common_information_entry =
    {
      format = Dwarf.DWARF32;
      length = Unsigned.UInt64.of_int 0;
      cie_id = Unsigned.UInt64.of_int 0;
      version = Unsigned.UInt8.of_int 1;
      augmentation = "";
      address_size = Unsigned.UInt8.of_int 8;
      segment_selector_size = Unsigned.UInt8.of_int 0;
      code_alignment_factor = Unsigned.UInt64.of_int 1;
      data_alignment_factor = Signed.Int64.of_int (-8);
      return_address_register = Unsigned.UInt64.of_int 16;
      augmentation_length = None;
      augmentation_data = None;
      initial_instructions = "";
      header_span =
        { start = Unsigned.UInt64.of_int 0; size = Unsigned.UInt64.of_int 0 };
      offset = Unsigned.UInt64.of_int 0;
    }
  in
  let fde : Dwarf.CallFrame.frame_description_entry =
    {
      format = Dwarf.DWARF32;
      length = Unsigned.UInt64.of_int 0;
      cie_pointer = Unsigned.UInt64.of_int 0;
      initial_location = Unsigned.UInt64.of_int 0x401000;
      address_range = Unsigned.UInt64.of_int 0x80;
      augmentation_length = None;
      augmentation_data = None;
      instructions = "";
      offset = Unsigned.UInt64.of_int 0;
    }
  in
  let buf = Buffer.create 128 in
  Dwarf_write.write_eh_frame buf
    [ Dwarf.EHFrame.EH_CIE cie; Dwarf.EHFrame.EH_FDE fde ];
  let obj_buf = object_buffer_of_buffer buf in
  let cur = Object.Buffer.cursor obj_buf ~at:0 in
  let section = Dwarf.EHFrame.parse_section cur (Buffer.length buf) in
  check int "2 entries" 2 (List.length section.entries);
  (match List.nth section.entries 0 with
  | Dwarf.EHFrame.EH_CIE p ->
      check int "cie version" 1 (Unsigned.UInt8.to_int p.version)
  | _ -> fail "first should be CIE");
  match List.nth section.entries 1 with
  | Dwarf.EHFrame.EH_FDE p ->
      check int "fde addr_range" 0x80 (Unsigned.UInt64.to_int p.address_range)
  | _ -> fail "second should be FDE"

let test_write_eh_cie_augmented () =
  let aug_data = "\x1b" in
  let cie : Dwarf.CallFrame.common_information_entry =
    {
      format = Dwarf.DWARF32;
      length = Unsigned.UInt64.of_int 0;
      cie_id = Unsigned.UInt64.of_int 0;
      version = Unsigned.UInt8.of_int 1;
      augmentation = "zR";
      address_size = Unsigned.UInt8.of_int 8;
      segment_selector_size = Unsigned.UInt8.of_int 0;
      code_alignment_factor = Unsigned.UInt64.of_int 1;
      data_alignment_factor = Signed.Int64.of_int (-8);
      return_address_register = Unsigned.UInt64.of_int 16;
      augmentation_length =
        Some (Unsigned.UInt64.of_int (String.length aug_data));
      augmentation_data = Some aug_data;
      initial_instructions = "";
      header_span =
        { start = Unsigned.UInt64.of_int 0; size = Unsigned.UInt64.of_int 0 };
      offset = Unsigned.UInt64.of_int 0;
    }
  in
  let buf = Buffer.create 64 in
  Dwarf_write.write_eh_cie buf cie;
  for _ = 1 to 16 do
    Buffer.add_char buf '\x00'
  done;
  let obj_buf = object_buffer_of_buffer buf in
  let cur = Object.Buffer.cursor obj_buf ~at:0 in
  let section = Dwarf.EHFrame.parse_section cur (Buffer.length buf) in
  let cie_entry =
    List.find
      (function Dwarf.EHFrame.EH_CIE _ -> true | _ -> false)
      section.entries
  in
  match cie_entry with
  | Dwarf.EHFrame.EH_CIE p -> (
      check string "augmentation" "zR" p.augmentation;
      match p.augmentation_data with
      | Some d -> check int "aug data len" 1 (String.length d)
      | None -> fail "expected augmentation data")
  | _ -> fail "expected EH_CIE"

(* Stage 13: .debug_aranges tests *)

let make_aranges_header ?(addr_sz = 8) ?(debug_info_off = 0) () :
    Dwarf.DebugAranges.header =
  {
    format = Dwarf.DWARF32;
    unit_length = Unsigned.UInt64.zero;
    version = Unsigned.UInt16.of_int 2;
    debug_info_offset = Unsigned.UInt64.of_int debug_info_off;
    address_size = Unsigned.UInt8.of_int addr_sz;
    segment_size = Unsigned.UInt8.of_int 0;
    header_span = { start = Unsigned.UInt64.zero; size = Unsigned.UInt64.zero };
  }

let test_write_aranges_set () =
  let header = make_aranges_header () in
  let ranges =
    Dwarf.DebugAranges.
      [
        { start_address = u64 0x401000; length = u64 0x200 };
        { start_address = u64 0x402000; length = u64 0x100 };
      ]
  in
  let aset = Dwarf.DebugAranges.{ header; ranges } in
  let buf = Buffer.create 128 in
  Dwarf_write.write_aranges_set buf aset;
  let obj_buf = object_buffer_of_buffer buf in
  let cur = Object.Buffer.cursor obj_buf ~at:0 in
  let _fmt, _unit_length = Dwarf.parse_initial_length cur in
  let version = Object.Buffer.Read.u16 cur in
  check int "version" 2 (Unsigned.UInt16.to_int version);
  let debug_info_off = Dwarf.read_offset_for_format Dwarf.DWARF32 cur in
  check int "debug_info_offset" 0 (Unsigned.UInt64.to_int debug_info_off);
  let addr_sz = Object.Buffer.Read.u8 cur in
  check int "address_size" 8 (Unsigned.UInt8.to_int addr_sz);
  let seg_sz = Object.Buffer.Read.u8 cur in
  check int "segment_size" 0 (Unsigned.UInt8.to_int seg_sz);
  (* Skip 4 bytes padding *)
  for _ = 1 to 4 do
    ignore (Object.Buffer.Read.u8 cur)
  done;
  (* Read first range *)
  let a1 = Object.Buffer.Read.u64 cur in
  let l1 = Object.Buffer.Read.u64 cur in
  check int "range1 start" 0x401000 (Unsigned.UInt64.to_int a1);
  check int "range1 length" 0x200 (Unsigned.UInt64.to_int l1);
  (* Read second range *)
  let a2 = Object.Buffer.Read.u64 cur in
  let l2 = Object.Buffer.Read.u64 cur in
  check int "range2 start" 0x402000 (Unsigned.UInt64.to_int a2);
  check int "range2 length" 0x100 (Unsigned.UInt64.to_int l2);
  (* Read terminator *)
  let t1 = Object.Buffer.Read.u64 cur in
  let t2 = Object.Buffer.Read.u64 cur in
  check int "term start" 0 (Unsigned.UInt64.to_int t1);
  check int "term length" 0 (Unsigned.UInt64.to_int t2)

let test_write_aranges_empty () =
  let header = make_aranges_header () in
  let aset = Dwarf.DebugAranges.{ header; ranges = [] } in
  let buf = Buffer.create 64 in
  Dwarf_write.write_aranges_set buf aset;
  let obj_buf = object_buffer_of_buffer buf in
  let cur = Object.Buffer.cursor obj_buf ~at:0 in
  let _fmt, unit_length = Dwarf.parse_initial_length cur in
  let expected_body = 2 + 4 + 1 + 1 + 4 + (1 * 8 * 2) in
  check int "unit_length" expected_body (Unsigned.UInt64.to_int unit_length);
  let _version = Object.Buffer.Read.u16 cur in
  let _off = Dwarf.read_offset_for_format Dwarf.DWARF32 cur in
  let _addr_sz = Object.Buffer.Read.u8 cur in
  let _seg_sz = Object.Buffer.Read.u8 cur in
  for _ = 1 to 4 do
    ignore (Object.Buffer.Read.u8 cur)
  done;
  let t1 = Object.Buffer.Read.u64 cur in
  let t2 = Object.Buffer.Read.u64 cur in
  check int "term start" 0 (Unsigned.UInt64.to_int t1);
  check int "term length" 0 (Unsigned.UInt64.to_int t2)

(* Stage 14: .debug_addr and .debug_str_offsets tests *)

let test_write_debug_addr () =
  let header : Dwarf.DebugAddr.header =
    {
      format = Dwarf.DWARF32;
      unit_length = Unsigned.UInt64.zero;
      version = Unsigned.UInt16.of_int 5;
      address_size = Unsigned.UInt8.of_int 8;
      segment_selector_size = Unsigned.UInt8.of_int 0;
      span = { start = Unsigned.UInt64.zero; size = Unsigned.UInt64.zero };
    }
  in
  let entries =
    Dwarf.DebugAddr.
      [|
        { segment = None; address = u64 0x401000 };
        { segment = None; address = u64 0x402000 };
        { segment = None; address = u64 0x403000 };
      |]
  in
  let t = Dwarf.DebugAddr.{ header; entries } in
  let buf = Buffer.create 64 in
  Dwarf_write.write_debug_addr buf t;
  let obj_buf = object_buffer_of_buffer buf in
  let cur = Object.Buffer.cursor obj_buf ~at:0 in
  let parsed_header = Dwarf.DebugAddr.parse_header cur in
  check int "version" 5 (Unsigned.UInt16.to_int parsed_header.version);
  check int "address_size" 8 (Unsigned.UInt8.to_int parsed_header.address_size);
  check int "segment_selector_size" 0
    (Unsigned.UInt8.to_int parsed_header.segment_selector_size);
  let parsed_entries = Dwarf.DebugAddr.parse_entries cur parsed_header in
  check int "entry count" 3 (Array.length parsed_entries);
  check int "addr 0" 0x401000
    (Unsigned.UInt64.to_int parsed_entries.(0).address);
  check int "addr 1" 0x402000
    (Unsigned.UInt64.to_int parsed_entries.(1).address);
  check int "addr 2" 0x403000
    (Unsigned.UInt64.to_int parsed_entries.(2).address)

let test_write_debug_str_offsets () =
  let header : Dwarf.DebugStrOffsets.header =
    {
      format = Dwarf.DWARF32;
      unit_length = Unsigned.UInt64.zero;
      version = Unsigned.UInt16.of_int 5;
      padding = Unsigned.UInt16.of_int 0;
      header_span =
        { start = Unsigned.UInt64.zero; size = Unsigned.UInt64.zero };
    }
  in
  let offsets =
    Dwarf.DebugStrOffsets.
      [|
        { offset = u64 0; resolved_string = None };
        { offset = u64 10; resolved_string = None };
        { offset = u64 25; resolved_string = None };
        { offset = u64 42; resolved_string = None };
      |]
  in
  let t = Dwarf.DebugStrOffsets.{ header; offsets } in
  let buf = Buffer.create 64 in
  Dwarf_write.write_debug_str_offsets buf t;
  let obj_buf = object_buffer_of_buffer buf in
  let cur = Object.Buffer.cursor obj_buf ~at:0 in
  let parsed_header = Dwarf.DebugStrOffsets.parse_header cur in
  check int "version" 5 (Unsigned.UInt16.to_int parsed_header.version);
  check int "padding" 0 (Unsigned.UInt16.to_int parsed_header.padding);
  (* Read the 4 offsets manually *)
  let o0 = Dwarf.read_offset_for_format Dwarf.DWARF32 cur in
  check int "offset 0" 0 (Unsigned.UInt64.to_int o0);
  let o1 = Dwarf.read_offset_for_format Dwarf.DWARF32 cur in
  check int "offset 1" 10 (Unsigned.UInt64.to_int o1);
  let o2 = Dwarf.read_offset_for_format Dwarf.DWARF32 cur in
  check int "offset 2" 25 (Unsigned.UInt64.to_int o2);
  let o3 = Dwarf.read_offset_for_format Dwarf.DWARF32 cur in
  check int "offset 3" 42 (Unsigned.UInt64.to_int o3)

let test_write_debug_addr_segments () =
  let header : Dwarf.DebugAddr.header =
    {
      format = Dwarf.DWARF32;
      unit_length = Unsigned.UInt64.zero;
      version = Unsigned.UInt16.of_int 5;
      address_size = Unsigned.UInt8.of_int 4;
      segment_selector_size = Unsigned.UInt8.of_int 4;
      span = { start = Unsigned.UInt64.zero; size = Unsigned.UInt64.zero };
    }
  in
  let entries =
    Dwarf.DebugAddr.
      [|
        { segment = Some (u64 1); address = u64 0x1000 };
        { segment = Some (u64 2); address = u64 0x2000 };
      |]
  in
  let t = Dwarf.DebugAddr.{ header; entries } in
  let buf = Buffer.create 64 in
  Dwarf_write.write_debug_addr buf t;
  let obj_buf = object_buffer_of_buffer buf in
  let cur = Object.Buffer.cursor obj_buf ~at:0 in
  let parsed_header = Dwarf.DebugAddr.parse_header cur in
  check int "segment_selector_size" 4
    (Unsigned.UInt8.to_int parsed_header.segment_selector_size);
  let parsed_entries = Dwarf.DebugAddr.parse_entries cur parsed_header in
  check int "entry count" 2 (Array.length parsed_entries);
  (match parsed_entries.(0).segment with
  | Some s -> check int "seg 0" 1 (Unsigned.UInt64.to_int s)
  | None -> fail "expected segment");
  check int "addr 0" 0x1000 (Unsigned.UInt64.to_int parsed_entries.(0).address);
  (match parsed_entries.(1).segment with
  | Some s -> check int "seg 1" 2 (Unsigned.UInt64.to_int s)
  | None -> fail "expected segment");
  check int "addr 1" 0x2000 (Unsigned.UInt64.to_int parsed_entries.(1).address)

(* Stage 15: .debug_names tests *)

let test_write_debug_names_simple () =
  let abbrevs =
    Dwarf.DebugNames.
      [
        {
          code = u64 1;
          tag = DW_TAG_subprogram;
          attributes =
            [
              (DW_IDX_compile_unit, Dwarf.DW_FORM_udata);
              (DW_IDX_die_offset, Dwarf.DW_FORM_ref4);
            ];
        };
      ]
  in
  let entries =
    Dwarf.DebugNames.
      [|
        {
          name_offset = Unsigned.UInt32.of_int 0;
          die_offset = Unsigned.UInt32.of_int 0x100;
          attributes = [ (DW_IDX_compile_unit, u64 0) ];
        };
        {
          name_offset = Unsigned.UInt32.of_int 5;
          die_offset = Unsigned.UInt32.of_int 0x200;
          attributes = [ (DW_IDX_compile_unit, u64 0) ];
        };
      |]
  in
  let name_table =
    Dwarf.DebugNames.
      [|
        { offset = Unsigned.UInt32.of_int 0; value = "main" };
        { offset = Unsigned.UInt32.of_int 5; value = "foo" };
      |]
  in
  let hash0 = Dwarf.DebugNames.djb2_hash "main" in
  let hash1 = Dwarf.DebugNames.djb2_hash "foo" in
  let sec =
    Dwarf.DebugNames.
      {
        header =
          {
            format = Dwarf.DWARF32;
            unit_length = Unsigned.UInt64.zero;
            version = Unsigned.UInt16.of_int 5;
            padding = Unsigned.UInt16.of_int 0;
            comp_unit_count = Unsigned.UInt32.of_int 1;
            local_type_unit_count = Unsigned.UInt32.of_int 0;
            foreign_type_unit_count = Unsigned.UInt32.of_int 0;
            bucket_count = Unsigned.UInt32.of_int 2;
            name_count = Unsigned.UInt32.of_int 2;
            abbrev_table_size = Unsigned.UInt32.zero;
            augmentation_string_size = Unsigned.UInt32.of_int 0;
            augmentation_string = "";
            span = 0;
          };
        comp_unit_offsets = [| Unsigned.UInt32.of_int 0 |];
        local_type_unit_offsets = [||];
        foreign_type_unit_signatures = [||];
        buckets = [| Unsigned.UInt32.of_int 1; Unsigned.UInt32.of_int 2 |];
        hash_table = [| hash0; hash1 |];
        name_table;
        entry_offsets = [||];
        abbreviation_table = abbrevs;
        entry_pool = entries;
      }
  in
  let buf = Buffer.create 256 in
  Dwarf_write.write_debug_names buf sec;
  let obj_buf = object_buffer_of_buffer buf in
  let cur = Object.Buffer.cursor obj_buf ~at:0 in
  let _fmt, _unit_length = Dwarf.parse_initial_length cur in
  let version = Object.Buffer.Read.u16 cur in
  check int "version" 5 (Unsigned.UInt16.to_int version);
  let _padding = Object.Buffer.Read.u16 cur in
  let cu_count = Object.Buffer.Read.u32 cur in
  check int "cu_count" 1 (Unsigned.UInt32.to_int cu_count);
  let ltu_count = Object.Buffer.Read.u32 cur in
  check int "ltu_count" 0 (Unsigned.UInt32.to_int ltu_count);
  let ftu_count = Object.Buffer.Read.u32 cur in
  check int "ftu_count" 0 (Unsigned.UInt32.to_int ftu_count);
  let bc = Object.Buffer.Read.u32 cur in
  check int "bucket_count" 2 (Unsigned.UInt32.to_int bc);
  let nc = Object.Buffer.Read.u32 cur in
  check int "name_count" 2 (Unsigned.UInt32.to_int nc);
  let _abbrev_sz = Object.Buffer.Read.u32 cur in
  let aug_sz = Object.Buffer.Read.u32 cur in
  check int "aug_sz" 0 (Unsigned.UInt32.to_int aug_sz);
  let cu0 = Object.Buffer.Read.u32 cur in
  check int "cu0" 0 (Unsigned.UInt32.to_int cu0);
  let _b0 = Object.Buffer.Read.u32 cur in
  let _b1 = Object.Buffer.Read.u32 cur in
  let h0 = Object.Buffer.Read.u32 cur in
  let h1 = Object.Buffer.Read.u32 cur in
  check int "hash0" (Unsigned.UInt32.to_int hash0) (Unsigned.UInt32.to_int h0);
  check int "hash1" (Unsigned.UInt32.to_int hash1) (Unsigned.UInt32.to_int h1);
  let n0 = Object.Buffer.Read.u32 cur in
  let n1 = Object.Buffer.Read.u32 cur in
  check int "name0_offset" 0 (Unsigned.UInt32.to_int n0);
  check int "name1_offset" 5 (Unsigned.UInt32.to_int n1);
  let _eo0 = Object.Buffer.Read.u32 cur in
  let _eo1 = Object.Buffer.Read.u32 cur in
  (* Abbreviation table *)
  let abbrev_code = Object.Buffer.Read.uleb128 cur in
  check int "abbrev code" 1 abbrev_code;
  let abbrev_tag = Object.Buffer.Read.uleb128 cur in
  check int "abbrev tag (subprogram)" 0x2e abbrev_tag;
  let idx_attr = Object.Buffer.Read.uleb128 cur in
  check int "idx attr (compile_unit)" 1 idx_attr;
  let idx_form = Object.Buffer.Read.uleb128 cur in
  check int "idx form (udata)" 0x0f idx_form;
  let die_attr = Object.Buffer.Read.uleb128 cur in
  check int "die attr (die_offset)" 3 die_attr;
  let die_form = Object.Buffer.Read.uleb128 cur in
  check int "die form (ref4)" 0x13 die_form;
  let term1 = Object.Buffer.Read.uleb128 cur in
  check int "attr term" 0 term1;
  let term2 = Object.Buffer.Read.uleb128 cur in
  check int "form term" 0 term2;
  let table_term = Object.Buffer.Read.uleb128 cur in
  check int "table term" 0 table_term;
  (* Entry pool: first entry *)
  let e0_code = Object.Buffer.Read.uleb128 cur in
  check int "entry0 abbrev" 1 e0_code;
  let e0_cu = Object.Buffer.Read.uleb128 cur in
  check int "entry0 cu_idx" 0 e0_cu;
  let e0_die = Object.Buffer.Read.u32 cur in
  check int "entry0 die_offset" 0x100 (Unsigned.UInt32.to_int e0_die);
  let e0_term = Object.Buffer.Read.u8 cur in
  check int "entry0 term" 0 (Unsigned.UInt8.to_int e0_term);
  (* Entry pool: second entry *)
  let e1_code = Object.Buffer.Read.uleb128 cur in
  check int "entry1 abbrev" 1 e1_code;
  let e1_cu = Object.Buffer.Read.uleb128 cur in
  check int "entry1 cu_idx" 0 e1_cu;
  let e1_die = Object.Buffer.Read.u32 cur in
  check int "entry1 die_offset" 0x200 (Unsigned.UInt32.to_int e1_die)

let test_write_debug_names_abbrev_table () =
  let abbrevs =
    Dwarf.DebugNames.
      [
        {
          code = u64 1;
          tag = DW_TAG_variable;
          attributes =
            [
              (DW_IDX_compile_unit, Dwarf.DW_FORM_udata);
              (DW_IDX_die_offset, Dwarf.DW_FORM_ref4);
            ];
        };
        {
          code = u64 2;
          tag = DW_TAG_subprogram;
          attributes = [ (DW_IDX_die_offset, Dwarf.DW_FORM_ref4) ];
        };
      ]
  in
  let buf = Buffer.create 64 in
  Dwarf_write.write_debug_names_abbrev_table buf abbrevs;
  let obj_buf = object_buffer_of_buffer buf in
  let cur = Object.Buffer.cursor obj_buf ~at:0 in
  (* First abbrev: code=1, tag=variable(0x34) *)
  let c1 = Object.Buffer.Read.uleb128 cur in
  check int "code1" 1 c1;
  let t1 = Object.Buffer.Read.uleb128 cur in
  check int "tag1 (variable)" 0x34 t1;
  (* attr pair: DW_IDX_compile_unit=1, DW_FORM_udata=0x0f *)
  let a1 = Object.Buffer.Read.uleb128 cur in
  check int "attr1" 1 a1;
  let f1 = Object.Buffer.Read.uleb128 cur in
  check int "form1" 0x0f f1;
  (* attr pair: DW_IDX_die_offset=3, DW_FORM_ref4=0x13 *)
  let a2 = Object.Buffer.Read.uleb128 cur in
  check int "attr2" 3 a2;
  let f2 = Object.Buffer.Read.uleb128 cur in
  check int "form2" 0x13 f2;
  (* terminator pair *)
  let z1 = Object.Buffer.Read.uleb128 cur in
  check int "term_attr" 0 z1;
  let z2 = Object.Buffer.Read.uleb128 cur in
  check int "term_form" 0 z2;
  (* Second abbrev: code=2, tag=subprogram(0x2e) *)
  let c2 = Object.Buffer.Read.uleb128 cur in
  check int "code2" 2 c2;
  let t2 = Object.Buffer.Read.uleb128 cur in
  check int "tag2 (subprogram)" 0x2e t2;
  (* attr pair: DW_IDX_die_offset=3, DW_FORM_ref4=0x13 *)
  let a3 = Object.Buffer.Read.uleb128 cur in
  check int "attr3" 3 a3;
  let f3 = Object.Buffer.Read.uleb128 cur in
  check int "form3" 0x13 f3;
  (* terminator pair *)
  let z3 = Object.Buffer.Read.uleb128 cur in
  check int "term2_attr" 0 z3;
  let z4 = Object.Buffer.Read.uleb128 cur in
  check int "term2_form" 0 z4;
  (* table terminator *)
  let zt = Object.Buffer.Read.uleb128 cur in
  check int "table_term" 0 zt

(* Stage 16: Split DWARF index tests *)

let test_write_unit_index_cu () =
  let idx =
    Dwarf.SplitDwarf.
      {
        version = 5;
        unit_count = 2;
        entries =
          [|
            {
              dwo_id = u64 0xAABBCCDD;
              contributions =
                [ (DW_SECT_INFO, 0, 100); (DW_SECT_ABBREV, 100, 50) ];
            };
            {
              dwo_id = u64 0x11223344;
              contributions =
                [ (DW_SECT_INFO, 200, 80); (DW_SECT_ABBREV, 300, 40) ];
            };
          |];
      }
  in
  let buf = Buffer.create 256 in
  Dwarf_write.write_unit_index buf idx;
  let obj_buf = object_buffer_of_buffer buf in
  let cur = Object.Buffer.cursor obj_buf ~at:0 in
  (* Header *)
  let version = Object.Buffer.Read.u32 cur in
  check int "version" 5 (Unsigned.UInt32.to_int version);
  let _padding = Object.Buffer.Read.u32 cur in
  let section_count = Object.Buffer.Read.u32 cur in
  check int "section_count" 2 (Unsigned.UInt32.to_int section_count);
  let unit_count = Object.Buffer.Read.u32 cur in
  check int "unit_count" 2 (Unsigned.UInt32.to_int unit_count);
  let slot_count = Object.Buffer.Read.u32 cur in
  let sc = Unsigned.UInt32.to_int slot_count in
  check bool "slot_count >= 4" true (sc >= 4);
  (* Hash table: slot_count u64s *)
  let hashes = Array.init sc (fun _ -> Object.Buffer.Read.u64 cur) in
  (* Index table: slot_count u32s *)
  let indices =
    Array.init sc (fun _ -> Unsigned.UInt32.to_int (Object.Buffer.Read.u32 cur))
  in
  (* Check both IDs are present *)
  let has_id id =
    Array.exists (fun h -> Unsigned.UInt64.to_int h = id) hashes
  in
  check bool "has id1" true (has_id 0xAABBCCDD);
  check bool "has id2" true (has_id 0x11223344);
  (* Check indices are 1-based row references *)
  let used_indices =
    Array.to_list indices |> List.filter (fun i -> i > 0) |> List.sort compare
  in
  check (list int) "used indices" [ 1; 2 ] used_indices;
  (* Column headers *)
  let col0 = Object.Buffer.Read.u32 cur in
  let col1 = Object.Buffer.Read.u32 cur in
  check int "col0 (INFO)" 1 (Unsigned.UInt32.to_int col0);
  check int "col1 (ABBREV)" 3 (Unsigned.UInt32.to_int col1);
  (* Offsets: 2 rows × 2 cols *)
  let off00 = Object.Buffer.Read.u32 cur in
  let off01 = Object.Buffer.Read.u32 cur in
  let off10 = Object.Buffer.Read.u32 cur in
  let off11 = Object.Buffer.Read.u32 cur in
  check int "off[0][INFO]" 0 (Unsigned.UInt32.to_int off00);
  check int "off[0][ABBREV]" 100 (Unsigned.UInt32.to_int off01);
  check int "off[1][INFO]" 200 (Unsigned.UInt32.to_int off10);
  check int "off[1][ABBREV]" 300 (Unsigned.UInt32.to_int off11);
  (* Sizes: 2 rows × 2 cols *)
  let sz00 = Object.Buffer.Read.u32 cur in
  let sz01 = Object.Buffer.Read.u32 cur in
  let sz10 = Object.Buffer.Read.u32 cur in
  let sz11 = Object.Buffer.Read.u32 cur in
  check int "sz[0][INFO]" 100 (Unsigned.UInt32.to_int sz00);
  check int "sz[0][ABBREV]" 50 (Unsigned.UInt32.to_int sz01);
  check int "sz[1][INFO]" 80 (Unsigned.UInt32.to_int sz10);
  check int "sz[1][ABBREV]" 40 (Unsigned.UInt32.to_int sz11)

let test_write_unit_index_tu () =
  let idx =
    Dwarf.SplitDwarf.
      {
        version = 5;
        unit_count = 1;
        entries =
          [|
            {
              dwo_id = u64 0xDEADBEEF;
              contributions =
                [ (DW_SECT_INFO, 0, 64); (DW_SECT_STR_OFFSETS, 64, 32) ];
            };
          |];
      }
  in
  let buf = Buffer.create 128 in
  Dwarf_write.write_unit_index buf idx;
  let obj_buf = object_buffer_of_buffer buf in
  let cur = Object.Buffer.cursor obj_buf ~at:0 in
  let version = Object.Buffer.Read.u32 cur in
  check int "version" 5 (Unsigned.UInt32.to_int version);
  let _padding = Object.Buffer.Read.u32 cur in
  let sc = Object.Buffer.Read.u32 cur in
  check int "section_count" 2 (Unsigned.UInt32.to_int sc);
  let uc = Object.Buffer.Read.u32 cur in
  check int "unit_count" 1 (Unsigned.UInt32.to_int uc);
  let slot_count = Object.Buffer.Read.u32 cur in
  let slots = Unsigned.UInt32.to_int slot_count in
  check bool "slot_count >= 2" true (slots >= 2);
  (* Skip hash + index tables *)
  for _ = 1 to slots do
    ignore (Object.Buffer.Read.u64 cur)
  done;
  for _ = 1 to slots do
    ignore (Object.Buffer.Read.u32 cur)
  done;
  (* Column headers *)
  let c0 = Object.Buffer.Read.u32 cur in
  let c1 = Object.Buffer.Read.u32 cur in
  check int "col0 (INFO)" 1 (Unsigned.UInt32.to_int c0);
  check int "col1 (STR_OFFSETS)" 6 (Unsigned.UInt32.to_int c1);
  (* Offsets *)
  let o0 = Object.Buffer.Read.u32 cur in
  let o1 = Object.Buffer.Read.u32 cur in
  check int "off[0][INFO]" 0 (Unsigned.UInt32.to_int o0);
  check int "off[0][STR_OFFSETS]" 64 (Unsigned.UInt32.to_int o1);
  (* Sizes *)
  let s0 = Object.Buffer.Read.u32 cur in
  let s1 = Object.Buffer.Read.u32 cur in
  check int "sz[0][INFO]" 64 (Unsigned.UInt32.to_int s0);
  check int "sz[0][STR_OFFSETS]" 32 (Unsigned.UInt32.to_int s1)

(* Stage 17: .debug_macro tests *)

let make_macro_header ?(flags = 0) () : Dwarf.debug_macro_header =
  {
    format = Dwarf.DWARF32;
    version = Unsigned.UInt16.of_int 5;
    (* DWARF 5 *)
    flags = Unsigned.UInt8.of_int flags;
    debug_line_offset = None;
    debug_str_offsets_offset = None;
  }

let test_write_debug_macro_define_undef () =
  let header = make_macro_header () in
  let entries =
    Dwarf.
      [
        {
          entry_type = DW_MACRO_define;
          line_number = Some (Unsigned.UInt32.of_int 10);
          string_offset = None;
          string_value = Some "FOO=1";
          file_index = None;
        };
        {
          entry_type = DW_MACRO_undef;
          line_number = Some (Unsigned.UInt32.of_int 20);
          string_offset = None;
          string_value = Some "BAR";
          file_index = None;
        };
      ]
  in
  let unit = Dwarf.{ header; entries } in
  let buf = Buffer.create 64 in
  Dwarf_write.write_debug_macro_unit buf unit;
  let obj_buf = object_buffer_of_buffer buf in
  let cur = Object.Buffer.cursor obj_buf ~at:0 in
  let parsed = Dwarf.parse_debug_macro_unit cur in
  check int "entry count" 2 (List.length parsed.entries);
  let e0 = List.nth parsed.entries 0 in
  check string "e0 type" "DW_MACRO_define"
    (Dwarf.string_of_macro_info_entry_type e0.entry_type);
  check int "e0 line" 10 (Unsigned.UInt32.to_int (Option.get e0.line_number));
  check string "e0 string" "FOO=1" (Option.get e0.string_value);
  let e1 = List.nth parsed.entries 1 in
  check string "e1 type" "DW_MACRO_undef"
    (Dwarf.string_of_macro_info_entry_type e1.entry_type);
  check int "e1 line" 20 (Unsigned.UInt32.to_int (Option.get e1.line_number));
  check string "e1 string" "BAR" (Option.get e1.string_value)

let test_write_debug_macro_start_end_file () =
  let header = make_macro_header () in
  let entries =
    Dwarf.
      [
        {
          entry_type = DW_MACRO_start_file;
          line_number = Some (Unsigned.UInt32.of_int 1);
          string_offset = None;
          string_value = None;
          file_index = Some (Unsigned.UInt32.of_int 3);
        };
        {
          entry_type = DW_MACRO_end_file;
          line_number = None;
          string_offset = None;
          string_value = None;
          file_index = None;
        };
      ]
  in
  let unit = Dwarf.{ header; entries } in
  let buf = Buffer.create 64 in
  Dwarf_write.write_debug_macro_unit buf unit;
  let obj_buf = object_buffer_of_buffer buf in
  let cur = Object.Buffer.cursor obj_buf ~at:0 in
  let parsed = Dwarf.parse_debug_macro_unit cur in
  check int "entry count" 2 (List.length parsed.entries);
  let e0 = List.nth parsed.entries 0 in
  check string "e0 type" "DW_MACRO_start_file"
    (Dwarf.string_of_macro_info_entry_type e0.entry_type);
  check int "e0 line" 1 (Unsigned.UInt32.to_int (Option.get e0.line_number));
  check int "e0 file_index" 3
    (Unsigned.UInt32.to_int (Option.get e0.file_index));
  let e1 = List.nth parsed.entries 1 in
  check string "e1 type" "DW_MACRO_end_file"
    (Dwarf.string_of_macro_info_entry_type e1.entry_type)

let test_write_debug_macro_strp () =
  let header = make_macro_header () in
  let entries =
    Dwarf.
      [
        {
          entry_type = DW_MACRO_define_strp;
          line_number = Some (Unsigned.UInt32.of_int 5);
          string_offset = Some (u64 0x100);
          string_value = None;
          file_index = None;
        };
        {
          entry_type = DW_MACRO_import;
          line_number = None;
          string_offset = Some (u64 0x200);
          string_value = None;
          file_index = None;
        };
      ]
  in
  let unit = Dwarf.{ header; entries } in
  let buf = Buffer.create 64 in
  Dwarf_write.write_debug_macro_unit buf unit;
  let obj_buf = object_buffer_of_buffer buf in
  let cur = Object.Buffer.cursor obj_buf ~at:0 in
  let parsed = Dwarf.parse_debug_macro_unit cur in
  check int "entry count" 2 (List.length parsed.entries);
  let e0 = List.nth parsed.entries 0 in
  check string "e0 type" "DW_MACRO_define_strp"
    (Dwarf.string_of_macro_info_entry_type e0.entry_type);
  check int "e0 line" 5 (Unsigned.UInt32.to_int (Option.get e0.line_number));
  check int "e0 str_offset" 0x100
    (Unsigned.UInt64.to_int (Option.get e0.string_offset));
  let e1 = List.nth parsed.entries 1 in
  check string "e1 type" "DW_MACRO_import"
    (Dwarf.string_of_macro_info_entry_type e1.entry_type);
  check int "e1 offset" 0x200
    (Unsigned.UInt64.to_int (Option.get e1.string_offset))

(* Stage 18: .debug_pubnames/.debug_pubtypes tests *)

let test_write_pubnames_set () =
  let header : Dwarf.DebugPubnames.header =
    {
      format = Dwarf.DWARF32;
      unit_length = Unsigned.UInt64.zero;
      version = Unsigned.UInt16.of_int 2;
      debug_info_offset = u64 0;
      debug_info_length = u64 0x200;
    }
  in
  let entries =
    Dwarf.DebugPubnames.
      [
        { offset = u64 0x2a; name = "main" };
        { offset = u64 0x50; name = "helper" };
        { offset = u64 0x80; name = "init" };
      ]
  in
  let buf = Buffer.create 64 in
  Dwarf_write.write_pubnames_set buf header entries;
  let obj_buf = object_buffer_of_buffer buf in
  let cur = Object.Buffer.cursor obj_buf ~at:0 in
  let parsed_h, parsed_e = Dwarf.DebugPubnames.parse_set cur in
  check int "version" 2 (Unsigned.UInt16.to_int parsed_h.version);
  check int "debug_info_offset" 0
    (Unsigned.UInt64.to_int parsed_h.debug_info_offset);
  check int "debug_info_length" 0x200
    (Unsigned.UInt64.to_int parsed_h.debug_info_length);
  check int "entry count" 3 (List.length parsed_e);
  let e0 = List.nth parsed_e 0 in
  check int "e0 offset" 0x2a (Unsigned.UInt64.to_int e0.offset);
  check string "e0 name" "main" e0.name;
  let e1 = List.nth parsed_e 1 in
  check int "e1 offset" 0x50 (Unsigned.UInt64.to_int e1.offset);
  check string "e1 name" "helper" e1.name;
  let e2 = List.nth parsed_e 2 in
  check int "e2 offset" 0x80 (Unsigned.UInt64.to_int e2.offset);
  check string "e2 name" "init" e2.name

let test_write_pubtypes_set () =
  let header : Dwarf.DebugPubtypes.header =
    {
      format = Dwarf.DWARF32;
      unit_length = Unsigned.UInt64.zero;
      version = Unsigned.UInt16.of_int 2;
      debug_info_offset = u64 0;
      debug_info_length = u64 0x100;
    }
  in
  let entries =
    Dwarf.DebugPubtypes.
      [
        { offset = u64 0x30; name = "int" };
        { offset = u64 0x40; name = "char" };
      ]
  in
  let buf = Buffer.create 64 in
  Dwarf_write.write_pubtypes_set buf header entries;
  let obj_buf = object_buffer_of_buffer buf in
  let cur = Object.Buffer.cursor obj_buf ~at:0 in
  let parsed_h, parsed_e = Dwarf.DebugPubtypes.parse_set cur in
  check int "version" 2 (Unsigned.UInt16.to_int parsed_h.version);
  check int "debug_info_length" 0x100
    (Unsigned.UInt64.to_int parsed_h.debug_info_length);
  check int "entry count" 2 (List.length parsed_e);
  let e0 = List.nth parsed_e 0 in
  check int "e0 offset" 0x30 (Unsigned.UInt64.to_int e0.offset);
  check string "e0 name" "int" e0.name;
  let e1 = List.nth parsed_e 1 in
  check int "e1 offset" 0x40 (Unsigned.UInt64.to_int e1.offset);
  check string "e1 name" "char" e1.name

(* Stage 19: .debug_types tests *)

let test_write_type_unit_header () =
  let die : Dwarf.DIE.t =
    {
      tag = DW_TAG_structure_type;
      attributes = [ { attr = DW_AT_name; value = String "my_struct" } ];
      children = Seq.empty;
      offset = 0;
    }
  in
  let enc : Dwarf.encoding =
    {
      format = DWARF32;
      address_size = Unsigned.UInt8.of_int 8;
      version = Unsigned.UInt16.of_int 4;
    }
  in
  let _, lookup = Dwarf_write.assign_abbreviations [ die ] in
  let type_sig = Unsigned.UInt64.of_string "0xDEADBEEFCAFEBABE" in
  let type_off = u64 0x20 in
  let abbrev_off = u64 0 in
  let buf = Buffer.create 128 in
  Dwarf_write.write_type_unit buf enc die lookup type_sig type_off abbrev_off;
  let obj_buf = object_buffer_of_buffer buf in
  let cur = Object.Buffer.cursor obj_buf ~at:0 in
  let _span, hdr = Dwarf.DebugTypes.parse_type_unit_header cur in
  check int "version" 4 (Unsigned.UInt16.to_int hdr.version);
  check int "address_size" 8 (Unsigned.UInt8.to_int hdr.address_size);
  check int "type_signature"
    (Unsigned.UInt64.to_int type_sig)
    (Unsigned.UInt64.to_int hdr.type_signature);
  check int "type_offset"
    (Unsigned.UInt64.to_int type_off)
    (Unsigned.UInt64.to_int hdr.type_offset);
  check int "debug_abbrev_offset" 0
    (Unsigned.UInt64.to_int hdr.debug_abbrev_offset)

let test_write_type_unit_with_die () =
  let child : Dwarf.DIE.t =
    {
      tag = DW_TAG_member;
      attributes = [ { attr = DW_AT_name; value = String "x" } ];
      children = Seq.empty;
      offset = 0;
    }
  in
  let die : Dwarf.DIE.t =
    {
      tag = DW_TAG_structure_type;
      attributes = [ { attr = DW_AT_name; value = String "point" } ];
      children = List.to_seq [ child ];
      offset = 0;
    }
  in
  let enc : Dwarf.encoding =
    {
      format = DWARF32;
      address_size = Unsigned.UInt8.of_int 8;
      version = Unsigned.UInt16.of_int 4;
    }
  in
  let _, lookup = Dwarf_write.assign_abbreviations [ die ] in
  let type_sig = Unsigned.UInt64.of_string "0x1234567890ABCDEF" in
  let type_off = u64 0x17 in
  let buf = Buffer.create 256 in
  Dwarf_write.write_type_unit buf enc die lookup type_sig type_off (u64 0);
  let total = Buffer.length buf in
  let obj_buf = object_buffer_of_buffer buf in
  let cur = Object.Buffer.cursor obj_buf ~at:0 in
  let _span, hdr = Dwarf.DebugTypes.parse_type_unit_header cur in
  let unit_length = Unsigned.UInt64.to_int hdr.unit_length in
  check int "unit_length + 4 = total" total (unit_length + 4);
  check int "type_signature"
    (Unsigned.UInt64.to_int type_sig)
    (Unsigned.UInt64.to_int hdr.type_signature);
  check int "type_offset" 0x17 (Unsigned.UInt64.to_int hdr.type_offset)

(* DWARF64 format roundtrip tests *)

let dwarf64_encoding : Dwarf.encoding =
  {
    format = DWARF64;
    address_size = Unsigned.UInt8.of_int 8;
    version = Unsigned.UInt16.of_int 5;
  }

let test_compile_unit_dwarf64 () =
  let die : Dwarf.DIE.t =
    {
      tag = DW_TAG_compile_unit;
      attributes =
        [
          { attr = DW_AT_name; value = String "test64.c" };
          { attr = DW_AT_producer; value = String "durin64" };
        ];
      children = Seq.empty;
      offset = 0;
    }
  in
  let enc = dwarf64_encoding in
  let abbrevs, lookup = Dwarf_write.assign_abbreviations [ die ] in
  let table = abbrev_hashtable_of_array abbrevs in
  let buf = Buffer.create 128 in
  Dwarf_write.write_compile_unit buf enc die lookup Unsigned.UInt64.zero;
  let total = Buffer.length buf in
  let obj_buf = object_buffer_of_buffer buf in
  let cur = Object.Buffer.cursor obj_buf ~at:0 in
  let _span, header = Dwarf.parse_compile_unit_header cur in
  check
    (module struct
      type t = Dwarf.dwarf_format

      let equal a b = a = b
      let pp fmt v = Format.fprintf fmt "%s" (Dwarf.string_of_dwarf_format v)
    end)
    "format is DWARF64" Dwarf.DWARF64 header.format;
  check int "version" 5 (Unsigned.UInt16.to_int header.version);
  check int "address_size" 8 (Unsigned.UInt8.to_int header.address_size);
  let unit_length = Unsigned.UInt64.to_int header.unit_length in
  check int "unit_length + 12 = total" total (unit_length + 12);
  match Dwarf.DIE.parse_die cur table enc obj_buf with
  | None -> fail "expected die"
  | Some parsed -> check int "attr count" 2 (List.length parsed.attributes)

let test_debug_info_dwarf64 () =
  let child : Dwarf.DIE.t =
    {
      tag = DW_TAG_base_type;
      attributes =
        [
          { attr = DW_AT_name; value = String "int" };
          { attr = DW_AT_byte_size; value = UData (u64 4) };
        ];
      children = Seq.empty;
      offset = 20;
    }
  in
  let die : Dwarf.DIE.t =
    {
      tag = DW_TAG_compile_unit;
      attributes = [ { attr = DW_AT_name; value = String "d64.c" } ];
      children = List.to_seq [ child ];
      offset = 0;
    }
  in
  let enc = dwarf64_encoding in
  let info_str, abbrev_str = Dwarf_write.write_debug_info enc [ die ] in
  let abbrev_buf = object_buffer_of_string abbrev_str in
  let abbrev_cur = Object.Buffer.cursor abbrev_buf ~at:0 in
  let abbrev_table = parse_abbrev_from_cursor abbrev_cur in
  let info_buf = object_buffer_of_string info_str in
  let info_cur = Object.Buffer.cursor info_buf ~at:0 in
  let _span, header = Dwarf.parse_compile_unit_header info_cur in
  check
    (module struct
      type t = Dwarf.dwarf_format

      let equal a b = a = b
      let pp fmt v = Format.fprintf fmt "%s" (Dwarf.string_of_dwarf_format v)
    end)
    "format is DWARF64" Dwarf.DWARF64 header.format;
  let unit_length = Unsigned.UInt64.to_int header.unit_length in
  check int "unit_length + 12 = total" (String.length info_str)
    (unit_length + 12);
  match Dwarf.DIE.parse_die info_cur abbrev_table enc info_buf with
  | None -> fail "expected die"
  | Some parsed -> (
      match parsed.children () with
      | Seq.Nil -> fail "expected children"
      | Seq.Cons (child_parsed, _) ->
          check int "child attrs" 2 (List.length child_parsed.attributes))

let test_cie_dwarf64 () =
  let cie =
    { (Dwarf.CallFrame.create_default_cie ()) with format = Dwarf.DWARF64 }
  in
  let buf = Buffer.create 128 in
  Dwarf_write.write_cie buf cie;
  let obj_buf = object_buffer_of_buffer buf in
  let cur = Object.Buffer.cursor obj_buf ~at:0 in
  let p = Dwarf.CallFrame.parse_common_information_entry cur in
  check
    (module struct
      type t = Dwarf.dwarf_format

      let equal a b = a = b
      let pp fmt v = Format.fprintf fmt "%s" (Dwarf.string_of_dwarf_format v)
    end)
    "format is DWARF64" Dwarf.DWARF64 p.format;
  check int64 "cie_id is 0xFFFFFFFFFFFFFFFF"
    (Int64.of_string "0xFFFFFFFFFFFFFFFF")
    (Unsigned.UInt64.to_int64 p.cie_id);
  check int "version"
    (Unsigned.UInt8.to_int cie.version)
    (Unsigned.UInt8.to_int p.version);
  check int "code_alignment"
    (Unsigned.UInt64.to_int cie.code_alignment_factor)
    (Unsigned.UInt64.to_int p.code_alignment_factor);
  check string "augmentation" cie.augmentation p.augmentation

let test_debug_frame_dwarf64 () =
  let cie =
    { (Dwarf.CallFrame.create_default_cie ()) with format = Dwarf.DWARF64 }
  in
  let fde : Dwarf.CallFrame.frame_description_entry =
    {
      format = Dwarf.DWARF64;
      length = Unsigned.UInt64.of_int 0;
      cie_pointer = Unsigned.UInt64.of_int 0;
      initial_location = Unsigned.UInt64.of_int 0x401000;
      address_range = Unsigned.UInt64.of_int 0x80;
      augmentation_length = None;
      augmentation_data = None;
      instructions = "";
      offset = Unsigned.UInt64.of_int 0;
    }
  in
  let buf = Buffer.create 256 in
  Dwarf_write.write_debug_frame buf
    [ Dwarf.CallFrame.CIE cie; Dwarf.CallFrame.FDE fde ];
  let obj_buf = object_buffer_of_buffer buf in
  let cur = Object.Buffer.cursor obj_buf ~at:0 in
  let section =
    Dwarf.CallFrame.parse_debug_frame_section cur (Buffer.length buf)
  in
  check int "entry_count" 2 section.entry_count;
  (match List.nth section.entries 0 with
  | Dwarf.CallFrame.CIE p ->
      check
        (module struct
          type t = Dwarf.dwarf_format

          let equal a b = a = b

          let pp fmt v =
            Format.fprintf fmt "%s" (Dwarf.string_of_dwarf_format v)
        end)
        "cie format" Dwarf.DWARF64 p.format
  | _ -> fail "first entry should be CIE");
  match List.nth section.entries 1 with
  | Dwarf.CallFrame.FDE p ->
      check int64 "fde location"
        (Unsigned.UInt64.to_int64 fde.initial_location)
        (Unsigned.UInt64.to_int64 p.initial_location)
  | _ -> fail "second entry should be FDE"

let test_aranges_dwarf64 () =
  let header : Dwarf.DebugAranges.header =
    {
      format = Dwarf.DWARF64;
      unit_length = Unsigned.UInt64.zero;
      version = Unsigned.UInt16.of_int 2;
      debug_info_offset = Unsigned.UInt64.of_int 0;
      address_size = Unsigned.UInt8.of_int 8;
      segment_size = Unsigned.UInt8.of_int 0;
      header_span =
        { start = Unsigned.UInt64.zero; size = Unsigned.UInt64.zero };
    }
  in
  let ranges =
    Dwarf.DebugAranges.
      [
        { start_address = u64 0x401000; length = u64 0x200 };
        { start_address = u64 0x402000; length = u64 0x100 };
      ]
  in
  let aset = Dwarf.DebugAranges.{ header; ranges } in
  let buf = Buffer.create 128 in
  Dwarf_write.write_aranges_set buf aset;
  let obj_buf = object_buffer_of_buffer buf in
  let cur = Object.Buffer.cursor obj_buf ~at:0 in
  let fmt, _unit_length = Dwarf.parse_initial_length cur in
  check
    (module struct
      type t = Dwarf.dwarf_format

      let equal a b = a = b
      let pp fmt v = Format.fprintf fmt "%s" (Dwarf.string_of_dwarf_format v)
    end)
    "format is DWARF64" Dwarf.DWARF64 fmt;
  let version = Object.Buffer.Read.u16 cur in
  check int "version" 2 (Unsigned.UInt16.to_int version);
  let debug_info_off = Dwarf.read_offset_for_format Dwarf.DWARF64 cur in
  check int "debug_info_offset" 0 (Unsigned.UInt64.to_int debug_info_off);
  let addr_sz = Object.Buffer.Read.u8 cur in
  check int "address_size" 8 (Unsigned.UInt8.to_int addr_sz);
  let _seg_sz = Object.Buffer.Read.u8 cur in
  (* Skip padding to 2*address_size boundary *)
  for _ = 1 to 4 do
    ignore (Object.Buffer.Read.u8 cur)
  done;
  let a1 = Object.Buffer.Read.u64 cur in
  check int "range1 start" 0x401000 (Unsigned.UInt64.to_int a1);
  let l1 = Object.Buffer.Read.u64 cur in
  check int "range1 length" 0x200 (Unsigned.UInt64.to_int l1)

let test_debug_addr_dwarf64 () =
  let header : Dwarf.DebugAddr.header =
    {
      format = Dwarf.DWARF64;
      unit_length = Unsigned.UInt64.zero;
      version = Unsigned.UInt16.of_int 5;
      address_size = Unsigned.UInt8.of_int 8;
      segment_selector_size = Unsigned.UInt8.of_int 0;
      span = { start = Unsigned.UInt64.zero; size = Unsigned.UInt64.zero };
    }
  in
  let entries =
    Dwarf.DebugAddr.
      [|
        { segment = None; address = u64 0x501000 };
        { segment = None; address = u64 0x502000 };
      |]
  in
  let t = Dwarf.DebugAddr.{ header; entries } in
  let buf = Buffer.create 128 in
  Dwarf_write.write_debug_addr buf t;
  let obj_buf = object_buffer_of_buffer buf in
  let cur = Object.Buffer.cursor obj_buf ~at:0 in
  let parsed_header = Dwarf.DebugAddr.parse_header cur in
  check
    (module struct
      type t = Dwarf.dwarf_format

      let equal a b = a = b
      let pp fmt v = Format.fprintf fmt "%s" (Dwarf.string_of_dwarf_format v)
    end)
    "format is DWARF64" Dwarf.DWARF64 parsed_header.format;
  check int "version" 5 (Unsigned.UInt16.to_int parsed_header.version);
  let parsed_entries = Dwarf.DebugAddr.parse_entries cur parsed_header in
  check int "entry count" 2 (Array.length parsed_entries);
  check int "addr 0" 0x501000
    (Unsigned.UInt64.to_int parsed_entries.(0).address);
  check int "addr 1" 0x502000
    (Unsigned.UInt64.to_int parsed_entries.(1).address)

let test_debug_str_offsets_dwarf64 () =
  let header : Dwarf.DebugStrOffsets.header =
    {
      format = Dwarf.DWARF64;
      unit_length = Unsigned.UInt64.zero;
      version = Unsigned.UInt16.of_int 5;
      padding = Unsigned.UInt16.of_int 0;
      header_span =
        { start = Unsigned.UInt64.zero; size = Unsigned.UInt64.zero };
    }
  in
  let offsets =
    Dwarf.DebugStrOffsets.
      [|
        { offset = u64 0; resolved_string = None };
        { offset = u64 10; resolved_string = None };
        { offset = u64 25; resolved_string = None };
      |]
  in
  let t = Dwarf.DebugStrOffsets.{ header; offsets } in
  let buf = Buffer.create 128 in
  Dwarf_write.write_debug_str_offsets buf t;
  let obj_buf = object_buffer_of_buffer buf in
  let cur = Object.Buffer.cursor obj_buf ~at:0 in
  let parsed_header = Dwarf.DebugStrOffsets.parse_header cur in
  check
    (module struct
      type t = Dwarf.dwarf_format

      let equal a b = a = b
      let pp fmt v = Format.fprintf fmt "%s" (Dwarf.string_of_dwarf_format v)
    end)
    "format is DWARF64" Dwarf.DWARF64 parsed_header.format;
  let o0 = Dwarf.read_offset_for_format Dwarf.DWARF64 cur in
  check int "offset 0" 0 (Unsigned.UInt64.to_int o0);
  let o1 = Dwarf.read_offset_for_format Dwarf.DWARF64 cur in
  check int "offset 1" 10 (Unsigned.UInt64.to_int o1);
  let o2 = Dwarf.read_offset_for_format Dwarf.DWARF64 cur in
  check int "offset 2" 25 (Unsigned.UInt64.to_int o2)

let test_debug_macro_dwarf64 () =
  let header : Dwarf.debug_macro_header =
    {
      format = Dwarf.DWARF64;
      version = Unsigned.UInt16.of_int 5;
      flags = Unsigned.UInt8.of_int 0x01;
      debug_line_offset = None;
      debug_str_offsets_offset = None;
    }
  in
  let entries =
    Dwarf.
      [
        {
          entry_type = DW_MACRO_define_strp;
          line_number = Some (Unsigned.UInt32.of_int 5);
          string_offset = Some (u64 0x100);
          string_value = None;
          file_index = None;
        };
      ]
  in
  let unit = Dwarf.{ header; entries } in
  let buf = Buffer.create 128 in
  Dwarf_write.write_debug_macro_unit buf unit;
  let obj_buf = object_buffer_of_buffer buf in
  let cur = Object.Buffer.cursor obj_buf ~at:0 in
  let parsed = Dwarf.parse_debug_macro_unit cur in
  check
    (module struct
      type t = Dwarf.dwarf_format

      let equal a b = a = b
      let pp fmt v = Format.fprintf fmt "%s" (Dwarf.string_of_dwarf_format v)
    end)
    "format is DWARF64" Dwarf.DWARF64 parsed.header.format;
  check int "entry count" 1 (List.length parsed.entries);
  let e0 = List.nth parsed.entries 0 in
  check int "e0 str_offset" 0x100
    (Unsigned.UInt64.to_int (Option.get e0.string_offset))

let test_pubnames_dwarf64 () =
  let header : Dwarf.DebugPubnames.header =
    {
      format = Dwarf.DWARF64;
      unit_length = Unsigned.UInt64.zero;
      version = Unsigned.UInt16.of_int 2;
      debug_info_offset = u64 0;
      debug_info_length = u64 0x200;
    }
  in
  let entries =
    Dwarf.DebugPubnames.
      [
        { offset = u64 0x2a; name = "main" };
        { offset = u64 0x50; name = "helper" };
      ]
  in
  let buf = Buffer.create 128 in
  Dwarf_write.write_pubnames_set buf header entries;
  let obj_buf = object_buffer_of_buffer buf in
  let cur = Object.Buffer.cursor obj_buf ~at:0 in
  let parsed_h, parsed_e = Dwarf.DebugPubnames.parse_set cur in
  check
    (module struct
      type t = Dwarf.dwarf_format

      let equal a b = a = b
      let pp fmt v = Format.fprintf fmt "%s" (Dwarf.string_of_dwarf_format v)
    end)
    "format is DWARF64" Dwarf.DWARF64 parsed_h.format;
  check int "version" 2 (Unsigned.UInt16.to_int parsed_h.version);
  check int "debug_info_length" 0x200
    (Unsigned.UInt64.to_int parsed_h.debug_info_length);
  check int "entry count" 2 (List.length parsed_e);
  let e0 = List.nth parsed_e 0 in
  check int "e0 offset" 0x2a (Unsigned.UInt64.to_int e0.offset);
  check string "e0 name" "main" e0.name;
  let e1 = List.nth parsed_e 1 in
  check int "e1 offset" 0x50 (Unsigned.UInt64.to_int e1.offset);
  check string "e1 name" "helper" e1.name

let test_pubtypes_dwarf64 () =
  let header : Dwarf.DebugPubtypes.header =
    {
      format = Dwarf.DWARF64;
      unit_length = Unsigned.UInt64.zero;
      version = Unsigned.UInt16.of_int 2;
      debug_info_offset = u64 0;
      debug_info_length = u64 0x100;
    }
  in
  let entries =
    Dwarf.DebugPubtypes.
      [
        { offset = u64 0x30; name = "int" };
        { offset = u64 0x40; name = "char" };
      ]
  in
  let buf = Buffer.create 128 in
  Dwarf_write.write_pubtypes_set buf header entries;
  let obj_buf = object_buffer_of_buffer buf in
  let cur = Object.Buffer.cursor obj_buf ~at:0 in
  let parsed_h, parsed_e = Dwarf.DebugPubtypes.parse_set cur in
  check
    (module struct
      type t = Dwarf.dwarf_format

      let equal a b = a = b
      let pp fmt v = Format.fprintf fmt "%s" (Dwarf.string_of_dwarf_format v)
    end)
    "format is DWARF64" Dwarf.DWARF64 parsed_h.format;
  check int "version" 2 (Unsigned.UInt16.to_int parsed_h.version);
  check int "debug_info_length" 0x100
    (Unsigned.UInt64.to_int parsed_h.debug_info_length);
  check int "entry count" 2 (List.length parsed_e);
  let e0 = List.nth parsed_e 0 in
  check int "e0 offset" 0x30 (Unsigned.UInt64.to_int e0.offset);
  check string "e0 name" "int" e0.name

let () =
  run "Dwarf_write"
    [
      ( "fixed-width",
        [
          test_case "write_u8" `Quick test_write_u8;
          test_case "write_u16_le" `Quick test_write_u16_le;
          test_case "write_u32_le" `Quick test_write_u32_le;
          test_case "write_u64_le" `Quick test_write_u64_le;
          test_case "write_i64_le" `Quick test_write_i64_le;
        ] );
      ( "variable-length",
        [
          test_case "write_uleb128" `Quick test_write_uleb128;
          test_case "write_sleb128" `Quick test_write_sleb128;
        ] );
      ( "dwarf-compound",
        [
          test_case "initial_length DWARF32" `Quick
            test_write_initial_length_dwarf32;
          test_case "initial_length DWARF64" `Quick
            test_write_initial_length_dwarf64;
          test_case "offset DWARF32" `Quick test_write_offset_dwarf32;
          test_case "offset DWARF64" `Quick test_write_offset_dwarf64;
          test_case "address 4-byte" `Quick test_write_address_4;
          test_case "address 8-byte" `Quick test_write_address_8;
          test_case "null_terminated_string" `Quick
            test_write_null_terminated_string;
        ] );
      ( "abbrev-table",
        [
          test_case "write single abbrev" `Quick test_write_abbrev_table_single;
          test_case "write multiple abbrevs" `Quick
            test_write_abbrev_table_multiple;
          test_case "write implicit_const" `Quick
            test_write_abbrev_table_implicit_const;
          test_case "abbrev_table_size" `Quick test_abbrev_table_size;
        ] );
      ( "assign-abbreviations",
        [
          test_case "deduplication" `Quick test_assign_abbreviations;
          test_case "different shapes" `Quick
            test_assign_abbreviations_different_shapes;
          test_case "with children" `Quick
            test_assign_abbreviations_with_children;
          test_case "assign then write roundtrip" `Quick
            test_assign_then_write_roundtrip;
        ] );
      ( "attribute-values",
        [
          test_case "string" `Quick test_write_attr_string;
          test_case "udata" `Quick test_write_attr_udata;
          test_case "sdata" `Quick test_write_attr_sdata;
          test_case "addr" `Quick test_write_attr_addr;
          test_case "flag_present" `Quick test_write_attr_flag_present;
          test_case "flag" `Quick test_write_attr_flag;
          test_case "ref4" `Quick test_write_attr_ref4;
          test_case "block" `Quick test_write_attr_block;
          test_case "language" `Quick test_write_attr_language;
          test_case "encoding" `Quick test_write_attr_encoding;
          test_case "indexed_string" `Quick test_write_attr_indexed_string;
          test_case "indexed_address" `Quick test_write_attr_indexed_address;
          test_case "attribute_value_size" `Quick test_attribute_value_size;
        ] );
      ( "die-tree",
        [
          test_case "simple die" `Quick test_write_die_simple;
          test_case "die with children" `Quick test_write_die_with_children;
          test_case "language roundtrip" `Quick
            test_write_die_language_roundtrip;
          test_case "die_size" `Quick test_die_size;
          test_case "die_size with children" `Quick test_die_size_with_children;
          test_case "die forest" `Quick test_write_die_forest;
        ] );
      ( "compile-unit",
        [
          test_case "write compile unit" `Quick test_write_compile_unit;
          test_case "debug_info simple" `Quick test_write_debug_info_simple;
          test_case "debug_info with children" `Quick
            test_write_debug_info_with_children;
          test_case "unit_length correct" `Quick
            test_write_debug_info_unit_length;
        ] );
      ( "string-table",
        [
          test_case "offsets" `Quick test_string_table_offsets;
          test_case "dedup" `Quick test_string_table_dedup;
          test_case "roundtrip" `Quick test_string_table_roundtrip;
          test_case "size" `Quick test_string_table_size;
        ] );
      ( "expression",
        [
          test_case "no operands" `Quick test_write_expr_no_operands;
          test_case "literals" `Quick test_write_expr_literals;
          test_case "const1u" `Quick test_write_expr_const1u;
          test_case "const2u" `Quick test_write_expr_const2u;
          test_case "const4u" `Quick test_write_expr_const4u;
          test_case "constu" `Quick test_write_expr_constu;
          test_case "consts" `Quick test_write_expr_consts;
          test_case "fbreg" `Quick test_write_expr_fbreg;
          test_case "breg" `Quick test_write_expr_breg;
          test_case "bregx" `Quick test_write_expr_bregx;
          test_case "piece" `Quick test_write_expr_piece;
          test_case "plus_uconst" `Quick test_write_expr_plus_uconst;
          test_case "stack_value" `Quick test_write_expr_stack_value;
        ] );
      ( "location-lists",
        [
          test_case "offset_pair" `Quick test_write_lle_offset_pair;
          test_case "base_address" `Quick test_write_lle_base_address;
          test_case "start_end" `Quick test_write_lle_start_end;
          test_case "start_length" `Quick test_write_lle_start_length;
          test_case "base_addressx" `Quick test_write_lle_base_addressx;
          test_case "default_location" `Quick test_write_lle_default_location;
          test_case "loclists header" `Quick
            test_write_loclists_header_roundtrip;
        ] );
      ( "range-lists",
        [
          test_case "offset_pair" `Quick test_write_rle_offset_pair;
          test_case "base_address" `Quick test_write_rle_base_address;
          test_case "start_end" `Quick test_write_rle_start_end;
          test_case "start_length" `Quick test_write_rle_start_length;
        ] );
      ( "dwarf4-loc-ranges",
        [
          test_case "debug_loc" `Quick test_write_debug_loc_roundtrip;
          test_case "debug_ranges" `Quick test_write_debug_ranges_roundtrip;
        ] );
      ( "line-program",
        [
          test_case "header" `Quick test_write_debug_line_header;
          test_case "simple entries" `Quick test_write_debug_line_simple;
          test_case "columns and flags" `Quick test_write_debug_line_columns;
          test_case "stmt toggle" `Quick test_write_debug_line_stmt_toggle;
          test_case "multi sequence" `Quick test_write_debug_line_multi_seq;
          test_case "discriminator" `Quick test_write_debug_line_discriminator;
          test_case "line_strp roundtrip" `Quick test_write_debug_line_line_strp;
          test_case "missing table raises" `Quick
            test_write_debug_line_missing_table;
          test_case "line_str dedup" `Quick test_write_debug_line_str_dedup;
          test_case "debug_str alias" `Quick test_write_debug_str_alias;
        ] );
      ( "cfi",
        [
          test_case "cie roundtrip" `Quick test_write_cie_roundtrip;
          test_case "cfi initial state" `Quick test_write_cfi_initial_state;
          test_case "debug_frame section" `Quick test_write_debug_frame_section;
          test_case "advance_loc" `Quick test_write_cfi_advance_loc;
        ] );
      ( "eh-frame",
        [
          test_case "eh cie roundtrip" `Quick test_write_eh_cie_roundtrip;
          test_case "eh frame cie+fde" `Quick test_write_eh_frame_cie_fde;
          test_case "eh cie augmented" `Quick test_write_eh_cie_augmented;
        ] );
      ( "aranges",
        [
          test_case "aranges set" `Quick test_write_aranges_set;
          test_case "aranges empty" `Quick test_write_aranges_empty;
        ] );
      ( "debug-addr",
        [
          test_case "addr roundtrip" `Quick test_write_debug_addr;
          test_case "addr with segments" `Quick test_write_debug_addr_segments;
        ] );
      ( "debug-str-offsets",
        [
          test_case "str_offsets roundtrip" `Quick test_write_debug_str_offsets;
        ] );
      ( "debug-names",
        [
          test_case "simple section" `Quick test_write_debug_names_simple;
          test_case "abbrev table" `Quick test_write_debug_names_abbrev_table;
        ] );
      ( "split-dwarf",
        [
          test_case "cu index" `Quick test_write_unit_index_cu;
          test_case "tu index" `Quick test_write_unit_index_tu;
        ] );
      ( "debug-macro",
        [
          test_case "define+undef" `Quick test_write_debug_macro_define_undef;
          test_case "start/end file" `Quick
            test_write_debug_macro_start_end_file;
          test_case "strp+import" `Quick test_write_debug_macro_strp;
        ] );
      ( "debug-pubnames",
        [ test_case "pubnames set" `Quick test_write_pubnames_set ] );
      ( "debug-pubtypes",
        [ test_case "pubtypes set" `Quick test_write_pubtypes_set ] );
      ( "debug-types",
        [
          test_case "type unit header" `Quick test_write_type_unit_header;
          test_case "type unit with die" `Quick test_write_type_unit_with_die;
        ] );
      ( "dwarf64",
        [
          test_case "compile_unit" `Quick test_compile_unit_dwarf64;
          test_case "debug_info" `Quick test_debug_info_dwarf64;
          test_case "cie" `Quick test_cie_dwarf64;
          test_case "debug_frame" `Quick test_debug_frame_dwarf64;
          test_case "aranges" `Quick test_aranges_dwarf64;
          test_case "debug_addr" `Quick test_debug_addr_dwarf64;
          test_case "debug_str_offsets" `Quick test_debug_str_offsets_dwarf64;
          test_case "debug_macro" `Quick test_debug_macro_dwarf64;
          test_case "pubnames" `Quick test_pubnames_dwarf64;
          test_case "pubtypes" `Quick test_pubtypes_dwarf64;
        ] );
      ( "gnu-forms",
        [
          test_case "GNU_addr_index" `Quick test_parse_gnu_addr_index;
          test_case "GNU_str_index" `Quick test_parse_gnu_str_index;
          test_case "GNU_ref_alt" `Quick test_parse_gnu_ref_alt;
          test_case "GNU_strp_alt" `Quick test_parse_gnu_strp_alt;
        ] );
    ]
