open Types
module Buffer = Stdlib.Buffer

let write_u8 buf (v : u8) =
  Buffer.add_char buf (Char.chr (Unsigned.UInt8.to_int v))

let write_u16_le buf (v : u16) =
  let i = Unsigned.UInt16.to_int v in
  Buffer.add_char buf (Char.chr (i land 0xff));
  Buffer.add_char buf (Char.chr ((i lsr 8) land 0xff))

let write_u32_le buf (v : u32) =
  let open Unsigned.UInt32 in
  let byte n = to_int (logand (shift_right v n) (of_int 0xff)) in
  Buffer.add_char buf (Char.chr (byte 0));
  Buffer.add_char buf (Char.chr (byte 8));
  Buffer.add_char buf (Char.chr (byte 16));
  Buffer.add_char buf (Char.chr (byte 24))

let write_u64_le buf (v : u64) =
  let open Unsigned.UInt64 in
  let byte n = to_int (logand (shift_right v n) (of_int 0xff)) in
  for i = 0 to 7 do
    Buffer.add_char buf (Char.chr (byte (i * 8)))
  done

let write_i64_le buf (v : i64) =
  let u = Unsigned.UInt64.of_int64 (Signed.Int64.to_int64 v) in
  write_u64_le buf u

let write_uleb128 buf (v : u64) =
  let open Unsigned.UInt64 in
  let mask = of_int 0x7f in
  let rec loop v =
    let byte = to_int (logand v mask) in
    let rest = shift_right v 7 in
    if compare rest zero = 0 then Buffer.add_char buf (Char.chr byte)
    else (
      Buffer.add_char buf (Char.chr (byte lor 0x80));
      loop rest)
  in
  loop v

let write_sleb128 buf (v : i64) =
  let open Signed.Int64 in
  let mask = of_int 0x7f in
  let neg_one = of_int (-1) in
  let rec loop v =
    let byte = to_int (logand v mask) in
    let rest = shift_right v 7 in
    let sign_bit = byte land 0x40 in
    if
      (compare rest zero = 0 && sign_bit = 0)
      || (compare rest neg_one = 0 && sign_bit <> 0)
    then Buffer.add_char buf (Char.chr byte)
    else (
      Buffer.add_char buf (Char.chr (byte lor 0x80));
      loop rest)
  in
  loop v

let write_initial_length buf (format : Dwarf.dwarf_format) length =
  match format with
  | DWARF32 -> write_u32_le buf (Unsigned.UInt32.of_int length)
  | DWARF64 ->
      write_u32_le buf (Unsigned.UInt32.of_int 0xffffffff);
      write_u64_le buf (Unsigned.UInt64.of_int length)

let write_offset buf (format : Dwarf.dwarf_format) (v : u64) =
  match format with
  | DWARF32 ->
      let v32 = Unsigned.UInt32.of_int64 (Unsigned.UInt64.to_int64 v) in
      write_u32_le buf v32
  | DWARF64 -> write_u64_le buf v

let write_address buf address_size (v : u64) =
  match address_size with
  | 4 ->
      let v32 = Unsigned.UInt32.of_int64 (Unsigned.UInt64.to_int64 v) in
      write_u32_le buf v32
  | 8 -> write_u64_le buf v
  | n -> failwith (Printf.sprintf "Unsupported address size: %d" n)

let write_null_terminated_string buf s =
  Buffer.add_string buf s;
  Buffer.add_char buf '\x00'

(* Stage 2: Abbreviation Table *)

let form_for_attribute_value :
    Dwarf.DIE.attribute_value -> Dwarf.attribute_form_encoding = function
  | String _ -> DW_FORM_string
  | IndexedString _ -> DW_FORM_strx
  | UData _ -> DW_FORM_udata
  | SData _ -> DW_FORM_sdata
  | Address _ -> DW_FORM_addr
  | IndexedAddress _ -> DW_FORM_addrx
  | Flag _ -> DW_FORM_flag_present
  | Reference _ -> DW_FORM_ref4
  | Block _ -> DW_FORM_block
  | Language _ -> DW_FORM_udata
  | Encoding _ -> DW_FORM_udata

type die_shape = {
  tag : Dwarf.abbreviation_tag;
  has_children : bool;
  attr_forms : (Dwarf.attribute_encoding * Dwarf.attribute_form_encoding) list;
}

module DieShapeMap = Map.Make (struct
  type t = die_shape

  let compare = Stdlib.compare
end)

let shape_of_die (die : Dwarf.DIE.t) =
  let has_children =
    match die.children () with Seq.Nil -> false | Seq.Cons _ -> true
  in
  let attr_forms =
    List.map
      (fun (a : Dwarf.DIE.attribute) ->
        (a.attr, form_for_attribute_value a.value))
      die.attributes
  in
  { tag = die.tag; has_children; attr_forms }

let assign_abbreviations (dies : Dwarf.DIE.t list) =
  let shapes = ref DieShapeMap.empty in
  let next_code = ref 1 in
  let offset_to_code = Hashtbl.create 16 in
  let abbrevs = ref [] in
  let rec walk die =
    let shape = shape_of_die die in
    let code =
      match DieShapeMap.find_opt shape !shapes with
      | Some code -> code
      | None ->
          let code = Unsigned.UInt64.of_int !next_code in
          incr next_code;
          shapes := DieShapeMap.add shape code !shapes;
          let attr_specs =
            List.map
              (fun (attr_enc, form_enc) ->
                Dwarf.
                  {
                    attr = u64_of_attribute_encoding attr_enc;
                    form = u64_of_attribute_form_encoding form_enc;
                    implicit_const = None;
                  })
              shape.attr_forms
          in
          let abbrev =
            Dwarf.
              {
                code;
                tag = uint64_of_abbreviation_tag shape.tag;
                has_children = shape.has_children;
                attr_specs;
              }
          in
          abbrevs := abbrev :: !abbrevs;
          code
    in
    Hashtbl.replace offset_to_code die.offset code;
    Seq.iter walk die.children
  in
  List.iter walk dies;
  let abbrev_array = Array.of_list (List.rev !abbrevs) in
  let lookup offset =
    match Hashtbl.find_opt offset_to_code offset with
    | Some code -> code
    | None ->
        failwith
          (Printf.sprintf "No abbreviation code for DIE offset %d" offset)
  in
  (abbrev_array, lookup)

let write_abbrev_table buf (abbrevs : Dwarf.abbrev array) =
  Array.iter
    (fun (a : Dwarf.abbrev) ->
      write_uleb128 buf a.code;
      write_uleb128 buf a.tag;
      write_u8 buf (Unsigned.UInt8.of_int (if a.has_children then 1 else 0));
      List.iter
        (fun (spec : Dwarf.attr_spec) ->
          write_uleb128 buf spec.attr;
          write_uleb128 buf spec.form;
          match spec.implicit_const with
          | Some v -> write_sleb128 buf (Signed.Int64.of_int64 v)
          | None -> ())
        a.attr_specs;
      write_uleb128 buf Unsigned.UInt64.zero;
      write_uleb128 buf Unsigned.UInt64.zero)
    abbrevs;
  write_uleb128 buf Unsigned.UInt64.zero

let uleb128_size (v : u64) =
  let open Unsigned.UInt64 in
  let rec count v n =
    let rest = shift_right v 7 in
    if compare rest zero = 0 then n else count rest (n + 1)
  in
  count v 1

let sleb128_size (v : i64) =
  let open Signed.Int64 in
  let mask = of_int 0x7f in
  let neg_one = of_int (-1) in
  let rec count v n =
    let byte = to_int (logand v mask) in
    let rest = shift_right v 7 in
    if
      (compare rest zero = 0 && byte land 0x40 = 0)
      || (compare rest neg_one = 0 && byte land 0x40 <> 0)
    then n
    else count rest (n + 1)
  in
  count v 1

let abbrev_table_size (abbrevs : Dwarf.abbrev array) =
  let size = ref 0 in
  Array.iter
    (fun (a : Dwarf.abbrev) ->
      size := !size + uleb128_size a.code;
      size := !size + uleb128_size a.tag;
      size := !size + 1;
      List.iter
        (fun (spec : Dwarf.attr_spec) ->
          size := !size + uleb128_size spec.attr;
          size := !size + uleb128_size spec.form;
          match spec.implicit_const with
          | Some v -> size := !size + sleb128_size (Signed.Int64.of_int64 v)
          | None -> ())
        a.attr_specs;
      size := !size + 2)
    abbrevs;
  !size + 1

(* Stage 3: Attribute Value Serialisation *)

let write_attribute_value buf (value : Dwarf.DIE.attribute_value)
    (form : Dwarf.attribute_form_encoding) (enc : Dwarf.encoding) =
  match (form, value) with
  | DW_FORM_string, String s -> write_null_terminated_string buf s
  | DW_FORM_strx, IndexedString (idx, _) ->
      write_uleb128 buf (Unsigned.UInt64.of_int idx)
  | DW_FORM_udata, UData v -> write_uleb128 buf v
  | DW_FORM_udata, Language l ->
      write_uleb128 buf (Unsigned.UInt64.of_int (Dwarf.int_of_dwarf_language l))
  | DW_FORM_udata, Encoding e ->
      write_uleb128 buf (Unsigned.UInt64.of_int (Dwarf.int_of_base_type e))
  | DW_FORM_sdata, SData v -> write_sleb128 buf v
  | DW_FORM_addr, Address a ->
      write_address buf (Unsigned.UInt8.to_int enc.address_size) a
  | DW_FORM_addrx, IndexedAddress (idx, _) ->
      write_uleb128 buf (Unsigned.UInt64.of_int idx)
  | DW_FORM_flag_present, Flag _ -> ()
  | DW_FORM_flag, Flag b ->
      write_u8 buf (Unsigned.UInt8.of_int (if b then 1 else 0))
  | DW_FORM_ref4, Reference r ->
      write_u32_le buf (Unsigned.UInt32.of_int64 (Unsigned.UInt64.to_int64 r))
  | DW_FORM_block, Block b ->
      write_uleb128 buf (Unsigned.UInt64.of_int (String.length b));
      Buffer.add_string buf b
  | _ -> failwith "Unsupported form/value combination"

let attribute_value_size (value : Dwarf.DIE.attribute_value)
    (form : Dwarf.attribute_form_encoding) (enc : Dwarf.encoding) =
  match (form, value) with
  | DW_FORM_string, String s -> String.length s + 1
  | DW_FORM_strx, IndexedString (idx, _) ->
      uleb128_size (Unsigned.UInt64.of_int idx)
  | DW_FORM_udata, UData v -> uleb128_size v
  | DW_FORM_udata, Language l ->
      uleb128_size (Unsigned.UInt64.of_int (Dwarf.int_of_dwarf_language l))
  | DW_FORM_udata, Encoding e ->
      uleb128_size (Unsigned.UInt64.of_int (Dwarf.int_of_base_type e))
  | DW_FORM_sdata, SData v -> sleb128_size v
  | DW_FORM_addr, Address _ -> Unsigned.UInt8.to_int enc.address_size
  | DW_FORM_addrx, IndexedAddress (idx, _) ->
      uleb128_size (Unsigned.UInt64.of_int idx)
  | DW_FORM_flag_present, Flag _ -> 0
  | DW_FORM_flag, Flag _ -> 1
  | DW_FORM_ref4, Reference _ -> 4
  | DW_FORM_block, Block b ->
      let len = String.length b in
      uleb128_size (Unsigned.UInt64.of_int len) + len
  | _ -> failwith "Unsupported form/value combination for size"

(* Stage 4: DIE Tree Serialisation *)

let rec write_die buf (die : Dwarf.DIE.t) (enc : Dwarf.encoding)
    (lookup : int -> u64) =
  let code = lookup die.offset in
  write_uleb128 buf code;
  List.iter
    (fun (attr : Dwarf.DIE.attribute) ->
      let form = form_for_attribute_value attr.value in
      write_attribute_value buf attr.value form enc)
    die.attributes;
  let has_children =
    match die.children () with Seq.Nil -> false | Seq.Cons _ -> true
  in
  if has_children then (
    Seq.iter (fun child -> write_die buf child enc lookup) die.children;
    write_u8 buf (Unsigned.UInt8.of_int 0))

let rec die_size (die : Dwarf.DIE.t) (enc : Dwarf.encoding)
    (lookup : int -> u64) =
  let code = lookup die.offset in
  let size = ref (uleb128_size code) in
  List.iter
    (fun (attr : Dwarf.DIE.attribute) ->
      let form = form_for_attribute_value attr.value in
      size := !size + attribute_value_size attr.value form enc)
    die.attributes;
  let has_children =
    match die.children () with Seq.Nil -> false | Seq.Cons _ -> true
  in
  if has_children then (
    Seq.iter
      (fun child -> size := !size + die_size child enc lookup)
      die.children;
    size := !size + 1);
  !size

let write_die_forest buf (dies : Dwarf.DIE.t list) (enc : Dwarf.encoding)
    (lookup : int -> u64) =
  List.iter (fun die -> write_die buf die enc lookup) dies

(* Stage 5: Compilation Unit & Top-Level *)

let write_compile_unit buf (enc : Dwarf.encoding) (die : Dwarf.DIE.t)
    (lookup : int -> u64) (debug_abbrev_offset : u64) =
  let die_bytes = die_size die enc lookup in
  let header_content_size =
    2 + 1 + 1 + Dwarf.offset_size_for_format enc.format
  in
  let unit_length = header_content_size + die_bytes in
  write_initial_length buf enc.format unit_length;
  write_u16_le buf enc.version;
  write_u8 buf (Unsigned.UInt8.of_int 0x01);
  write_u8 buf enc.address_size;
  write_offset buf enc.format debug_abbrev_offset;
  write_die buf die enc lookup

let write_debug_info (enc : Dwarf.encoding) (dies : Dwarf.DIE.t list) =
  let abbrevs, lookup = assign_abbreviations dies in
  let abbrev_buf = Buffer.create 256 in
  write_abbrev_table abbrev_buf abbrevs;
  let info_buf = Buffer.create 1024 in
  List.iter
    (fun die -> write_compile_unit info_buf enc die lookup Unsigned.UInt64.zero)
    dies;
  (Buffer.contents info_buf, Buffer.contents abbrev_buf)
