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
      let tag = Object.Buffer.Read.uleb128 cur in
      let has_children =
        Unsigned.UInt8.to_int (Object.Buffer.Read.u8 cur) <> 0
      in
      let rec parse_specs acc =
        let attr = Object.Buffer.Read.uleb128 cur in
        let form = Object.Buffer.Read.uleb128 cur in
        if attr = 0 && form = 0 then List.rev acc
        else
          let implicit_const =
            if form = 0x21 then
              Some (Int64.of_int (Object.Buffer.Read.sleb128 cur))
            else None
          in
          parse_specs
            (Dwarf.
               {
                 attr = Unsigned.UInt64.of_int attr;
                 form = Unsigned.UInt64.of_int form;
                 implicit_const;
               }
            :: acc)
      in
      let attr_specs = parse_specs [] in
      let abbrev =
        Dwarf.
          {
            code = Unsigned.UInt64.of_int code;
            tag = Unsigned.UInt64.of_int tag;
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

let test_write_abbrev_table_single () =
  let abbrev =
    Dwarf.
      {
        code = u64 1;
        tag = u64 0x11;
        has_children = true;
        attr_specs =
          [
            { attr = u64 0x25; form = u64 0x0e; implicit_const = None };
            { attr = u64 0x13; form = u64 0x0f; implicit_const = None };
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
  check int "tag matches" 0x11 (Unsigned.UInt64.to_int parsed.tag);
  check bool "has_children matches" true parsed.has_children;
  check int "attr_specs length" 2 (List.length parsed.attr_specs);
  let spec0 = List.nth parsed.attr_specs 0 in
  check int "first attr" 0x25 (Unsigned.UInt64.to_int spec0.attr);
  check int "first form" 0x0e (Unsigned.UInt64.to_int spec0.form)

let test_write_abbrev_table_multiple () =
  let abbrevs =
    [|
      Dwarf.
        {
          code = u64 1;
          tag = u64 0x11;
          has_children = true;
          attr_specs =
            [ { attr = u64 0x03; form = u64 0x08; implicit_const = None } ];
        };
      Dwarf.
        {
          code = u64 2;
          tag = u64 0x24;
          has_children = false;
          attr_specs =
            [
              { attr = u64 0x03; form = u64 0x08; implicit_const = None };
              { attr = u64 0x0b; form = u64 0x0f; implicit_const = None };
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
        tag = u64 0x2e;
        has_children = false;
        attr_specs =
          [ { attr = u64 0x3b; form = u64 0x21; implicit_const = Some 42L } ];
      }
  in
  let buf = Buffer.create 64 in
  Dwarf_write.write_abbrev_table buf [| abbrev |];
  let obj_buf = object_buffer_of_buffer buf in
  let cur = Object.Buffer.cursor obj_buf ~at:0 in
  let table = parse_abbrev_from_cursor cur in
  let parsed = Hashtbl.find table (u64 1) in
  let spec = List.hd parsed.attr_specs in
  check int "form is implicit_const" 0x21 (Unsigned.UInt64.to_int spec.form);
  check (option int64) "implicit_const value" (Some 42L) spec.implicit_const

let test_abbrev_table_size () =
  let abbrev =
    Dwarf.
      {
        code = u64 1;
        tag = u64 0x11;
        has_children = true;
        attr_specs =
          [ { attr = u64 0x03; form = u64 0x08; implicit_const = None } ];
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
  check int "roundtrip: tag is compile_unit" 0x11
    (Unsigned.UInt64.to_int parsed.tag);
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
    ]
