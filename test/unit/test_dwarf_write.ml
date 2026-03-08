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
  check int "unit_type" 0x01 (Unsigned.UInt8.to_int header.unit_type);
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
  (* Pad for FDE parser bug *)
  for _ = 1 to 4 do
    Buffer.add_char buf '\x00'
  done;
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
    ]
