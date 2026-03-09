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

(* Stage 6: String Table *)

type string_table = { offsets : (string, int) Hashtbl.t; buf : Buffer.t }

let create_string_table () =
  { offsets = Hashtbl.create 64; buf = Buffer.create 256 }

let add_string table s =
  match Hashtbl.find_opt table.offsets s with
  | Some offset -> offset
  | None ->
      let offset = Buffer.length table.buf in
      Buffer.add_string table.buf s;
      Buffer.add_char table.buf '\x00';
      Hashtbl.replace table.offsets s offset;
      offset

let write_string_table buf table =
  Buffer.add_string buf (Buffer.contents table.buf)

let string_table_size table = Buffer.length table.buf

(* Stage 7: Expression Encoding *)

let write_op_byte buf (opcode : Dwarf.operation_encoding) =
  Buffer.add_char buf (Char.chr (Dwarf.int_of_operation_encoding opcode))

let write_2byte_le buf v =
  Buffer.add_char buf (Char.chr (v land 0xff));
  Buffer.add_char buf (Char.chr ((v lsr 8) land 0xff))

let write_4byte_le buf v =
  Buffer.add_char buf (Char.chr (v land 0xff));
  Buffer.add_char buf (Char.chr ((v lsr 8) land 0xff));
  Buffer.add_char buf (Char.chr ((v lsr 16) land 0xff));
  Buffer.add_char buf (Char.chr ((v lsr 24) land 0xff))

let write_expression buf (ops : Dwarf.dwarf_expression_operation list)
    (_enc : Dwarf.encoding) =
  List.iter
    (fun (op : Dwarf.dwarf_expression_operation) ->
      write_op_byte buf op.opcode;
      match op.opcode with
      | DW_OP_deref | DW_OP_dup | DW_OP_drop | DW_OP_over | DW_OP_swap
      | DW_OP_rot | DW_OP_xderef | DW_OP_abs | DW_OP_and | DW_OP_div
      | DW_OP_minus | DW_OP_mod | DW_OP_mul | DW_OP_neg | DW_OP_not | DW_OP_or
      | DW_OP_plus | DW_OP_shl | DW_OP_shr | DW_OP_shra | DW_OP_xor | DW_OP_eq
      | DW_OP_ge | DW_OP_gt | DW_OP_le | DW_OP_lt | DW_OP_ne | DW_OP_nop
      | DW_OP_push_object_address | DW_OP_form_tls_address
      | DW_OP_call_frame_cfa | DW_OP_stack_value | DW_OP_hi_user ->
          ()
      | DW_OP_lit0 | DW_OP_lit1 | DW_OP_lit2 | DW_OP_lit3 | DW_OP_lit4
      | DW_OP_lit5 | DW_OP_lit6 | DW_OP_lit7 | DW_OP_lit8 | DW_OP_lit9
      | DW_OP_lit10 | DW_OP_lit11 | DW_OP_lit12 | DW_OP_lit13 | DW_OP_lit14
      | DW_OP_lit15 | DW_OP_lit16 | DW_OP_lit17 | DW_OP_lit18 | DW_OP_lit19
      | DW_OP_lit20 | DW_OP_lit21 | DW_OP_lit22 | DW_OP_lit23 | DW_OP_lit24
      | DW_OP_lit25 | DW_OP_lit26 | DW_OP_lit27 | DW_OP_lit28 | DW_OP_lit29
      | DW_OP_lit30 | DW_OP_lit31 ->
          ()
      | DW_OP_reg0 | DW_OP_reg1 | DW_OP_reg2 | DW_OP_reg3 | DW_OP_reg4
      | DW_OP_reg5 | DW_OP_reg6 | DW_OP_reg7 | DW_OP_reg8 | DW_OP_reg9
      | DW_OP_reg10 | DW_OP_reg11 | DW_OP_reg12 | DW_OP_reg13 | DW_OP_reg14
      | DW_OP_reg15 | DW_OP_reg16 | DW_OP_reg17 | DW_OP_reg18 | DW_OP_reg19
      | DW_OP_reg20 | DW_OP_reg21 | DW_OP_reg22 | DW_OP_reg23 | DW_OP_reg24
      | DW_OP_reg25 | DW_OP_reg26 | DW_OP_reg27 | DW_OP_reg28 | DW_OP_reg29
      | DW_OP_reg30 | DW_OP_reg31 ->
          ()
      | DW_OP_const1u | DW_OP_const1s | DW_OP_pick | DW_OP_deref_size
      | DW_OP_xderef_size ->
          Buffer.add_char buf (Char.chr (List.hd op.operands land 0xff))
      | DW_OP_const2u | DW_OP_const2s | DW_OP_bra | DW_OP_skip | DW_OP_call2 ->
          write_2byte_le buf (List.hd op.operands)
      | DW_OP_const4u | DW_OP_const4s | DW_OP_call4 | DW_OP_GNU_parameter_ref ->
          write_4byte_le buf (List.hd op.operands)
      | DW_OP_constu | DW_OP_plus_uconst | DW_OP_regx | DW_OP_piece
      | DW_OP_addrx | DW_OP_constx | DW_OP_convert | DW_OP_reinterpret ->
          write_uleb128 buf (Unsigned.UInt64.of_int (List.hd op.operands))
      | DW_OP_consts | DW_OP_fbreg ->
          write_sleb128 buf (Signed.Int64.of_int (List.hd op.operands))
      | DW_OP_breg0 | DW_OP_breg1 | DW_OP_breg2 | DW_OP_breg3 | DW_OP_breg4
      | DW_OP_breg5 | DW_OP_breg6 | DW_OP_breg7 | DW_OP_breg8 | DW_OP_breg9
      | DW_OP_breg10 | DW_OP_breg11 | DW_OP_breg12 | DW_OP_breg13 | DW_OP_breg14
      | DW_OP_breg15 | DW_OP_breg16 | DW_OP_breg17 | DW_OP_breg18 | DW_OP_breg19
      | DW_OP_breg20 | DW_OP_breg21 | DW_OP_breg22 | DW_OP_breg23 | DW_OP_breg24
      | DW_OP_breg25 | DW_OP_breg26 | DW_OP_breg27 | DW_OP_breg28 | DW_OP_breg29
      | DW_OP_breg30 | DW_OP_breg31 ->
          write_sleb128 buf (Signed.Int64.of_int (List.hd op.operands))
      | DW_OP_bregx ->
          write_uleb128 buf (Unsigned.UInt64.of_int (List.nth op.operands 0));
          write_sleb128 buf (Signed.Int64.of_int (List.nth op.operands 1))
      | DW_OP_bit_piece ->
          write_uleb128 buf (Unsigned.UInt64.of_int (List.nth op.operands 0));
          write_uleb128 buf (Unsigned.UInt64.of_int (List.nth op.operands 1))
      | _ -> failwith "Unsupported operation for write")
    ops

(* Stage 9: Location/Range Lists *)

let write_lle_byte buf kind =
  write_u8 buf (Unsigned.UInt8.of_int (Dwarf.int_of_location_list_entry kind))

let write_expr_block buf (expr : string) =
  write_uleb128 buf (Unsigned.UInt64.of_int (String.length expr));
  Buffer.add_string buf expr

let write_location_entry buf (entry : Dwarf.DebugLoclists.location_entry)
    (address_size : int) =
  match entry with
  | LLE_end_of_list -> write_lle_byte buf Dwarf.DW_LLE_end_of_list
  | LLE_base_addressx { index } ->
      write_lle_byte buf Dwarf.DW_LLE_base_addressx;
      write_uleb128 buf (Unsigned.UInt64.of_int index)
  | LLE_startx_endx { start_index; end_index; expr } ->
      write_lle_byte buf Dwarf.DW_LLE_startx_endx;
      write_uleb128 buf (Unsigned.UInt64.of_int start_index);
      write_uleb128 buf (Unsigned.UInt64.of_int end_index);
      write_expr_block buf expr
  | LLE_startx_length { start_index; length; expr } ->
      write_lle_byte buf Dwarf.DW_LLE_startx_length;
      write_uleb128 buf (Unsigned.UInt64.of_int start_index);
      write_uleb128 buf length;
      write_expr_block buf expr
  | LLE_offset_pair { start_offset; end_offset; expr } ->
      write_lle_byte buf Dwarf.DW_LLE_offset_pair;
      write_uleb128 buf start_offset;
      write_uleb128 buf end_offset;
      write_expr_block buf expr
  | LLE_default_location { expr } ->
      write_lle_byte buf Dwarf.DW_LLE_default_location;
      write_expr_block buf expr
  | LLE_base_address { address } ->
      write_lle_byte buf Dwarf.DW_LLE_base_address;
      write_address buf address_size address
  | LLE_start_end { start_addr; end_addr; expr } ->
      write_lle_byte buf Dwarf.DW_LLE_start_end;
      write_address buf address_size start_addr;
      write_address buf address_size end_addr;
      write_expr_block buf expr
  | LLE_start_length { start_addr; length; expr } ->
      write_lle_byte buf Dwarf.DW_LLE_start_length;
      write_address buf address_size start_addr;
      write_uleb128 buf length;
      write_expr_block buf expr

let write_location_list buf (list : Dwarf.DebugLoclists.location_list)
    (address_size : int) =
  List.iter (fun e -> write_location_entry buf e address_size) list.entries

let write_rle_byte buf kind =
  write_u8 buf (Unsigned.UInt8.of_int (Dwarf.int_of_range_list_entry kind))

let write_range_entry buf (entry : Dwarf.DebugRnglists.range_entry)
    (address_size : int) =
  match entry with
  | RLE_end_of_list -> write_rle_byte buf Dwarf.DW_RLE_end_of_list
  | RLE_base_addressx { index } ->
      write_rle_byte buf Dwarf.DW_RLE_base_addressx;
      write_uleb128 buf (Unsigned.UInt64.of_int index)
  | RLE_startx_endx { start_index; end_index } ->
      write_rle_byte buf Dwarf.DW_RLE_startx_endx;
      write_uleb128 buf (Unsigned.UInt64.of_int start_index);
      write_uleb128 buf (Unsigned.UInt64.of_int end_index)
  | RLE_startx_length { start_index; length } ->
      write_rle_byte buf Dwarf.DW_RLE_startx_length;
      write_uleb128 buf (Unsigned.UInt64.of_int start_index);
      write_uleb128 buf length
  | RLE_offset_pair { start_offset; end_offset } ->
      write_rle_byte buf Dwarf.DW_RLE_offset_pair;
      write_uleb128 buf start_offset;
      write_uleb128 buf end_offset
  | RLE_base_address { address } ->
      write_rle_byte buf Dwarf.DW_RLE_base_address;
      write_address buf address_size address
  | RLE_start_end { start_addr; end_addr } ->
      write_rle_byte buf Dwarf.DW_RLE_start_end;
      write_address buf address_size start_addr;
      write_address buf address_size end_addr
  | RLE_start_length { start_addr; length } ->
      write_rle_byte buf Dwarf.DW_RLE_start_length;
      write_address buf address_size start_addr;
      write_uleb128 buf length

let write_range_list buf (list : Dwarf.DebugRnglists.range_list)
    (address_size : int) =
  List.iter (fun e -> write_range_entry buf e address_size) list.entries

let write_loclists_header buf (enc : Dwarf.encoding) (offset_entry_count : int)
    (body_size : int) =
  let header_content_size = 2 + 1 + 1 + 4 in
  let offset_table_size =
    offset_entry_count * Dwarf.offset_size_for_format enc.format
  in
  let unit_length = header_content_size + offset_table_size + body_size in
  write_initial_length buf enc.format unit_length;
  write_u16_le buf (Unsigned.UInt16.of_int 5);
  write_u8 buf enc.address_size;
  write_u8 buf (Unsigned.UInt8.of_int 0);
  write_u32_le buf (Unsigned.UInt32.of_int offset_entry_count)

let write_rnglists_header buf (enc : Dwarf.encoding) (offset_entry_count : int)
    (body_size : int) =
  write_loclists_header buf enc offset_entry_count body_size

let write_debug_loc_entry buf (entry : Dwarf.DebugLoc.entry)
    (address_size : int) =
  match entry with
  | EndOfList ->
      write_address buf address_size Unsigned.UInt64.zero;
      write_address buf address_size Unsigned.UInt64.zero
  | BaseAddress addr ->
      let max_addr =
        if address_size = 4 then Unsigned.UInt64.of_int 0xFFFFFFFF
        else Unsigned.UInt64.max_int
      in
      write_address buf address_size max_addr;
      write_address buf address_size addr
  | Location { begin_addr; end_addr; expr } ->
      write_address buf address_size begin_addr;
      write_address buf address_size end_addr;
      write_u16_le buf (Unsigned.UInt16.of_int (String.length expr));
      Buffer.add_string buf expr

let write_debug_loc buf (entries : Dwarf.DebugLoc.entry list)
    (address_size : int) =
  List.iter (fun e -> write_debug_loc_entry buf e address_size) entries

let write_debug_ranges_entry buf (entry : Dwarf.DebugRanges.entry)
    (address_size : int) =
  match entry with
  | EndOfList ->
      write_address buf address_size Unsigned.UInt64.zero;
      write_address buf address_size Unsigned.UInt64.zero
  | BaseAddress addr ->
      let max_addr =
        if address_size = 4 then Unsigned.UInt64.of_int 0xFFFFFFFF
        else Unsigned.UInt64.max_int
      in
      write_address buf address_size max_addr;
      write_address buf address_size addr
  | Range { begin_addr; end_addr } ->
      write_address buf address_size begin_addr;
      write_address buf address_size end_addr

let write_debug_ranges buf (entries : Dwarf.DebugRanges.entry list)
    (address_size : int) =
  List.iter (fun e -> write_debug_ranges_entry buf e address_size) entries

(* Stage 10: Line Program Writer *)

let int_of_lnct = function
  | Dwarf.DW_LNCT_path -> 0x1
  | DW_LNCT_directory_index -> 0x2
  | DW_LNCT_timestamp -> 0x3
  | DW_LNCT_size -> 0x4
  | DW_LNCT_MD5 -> 0x5
  | DW_LNCT_lo_user -> 0x2000
  | DW_LNCT_hi_user -> 0x3fff

let find_dir_index dirs dir =
  let rec find i =
    if i >= Array.length dirs then 0
    else if dirs.(i) = dir then i
    else find (i + 1)
  in
  find 0

let write_format_descs buf descs =
  Array.iter
    (fun (ct, form) ->
      write_uleb128 buf (Unsigned.UInt64.of_int (int_of_lnct ct));
      write_uleb128 buf (Dwarf.u64_of_attribute_form_encoding form))
    descs

let write_line_dir_entry buf (h : Dwarf.DebugLine.line_program_header) dir =
  Array.iter
    (fun (ct, form) ->
      match (ct, form) with
      | Dwarf.DW_LNCT_path, Dwarf.DW_FORM_string ->
          write_null_terminated_string buf dir
      | _ -> failwith "Unsupported directory format")
    h.directory_entry_formats

let write_line_file_entry buf (h : Dwarf.DebugLine.line_program_header)
    (file : Dwarf.DebugLine.file_entry) =
  Array.iter
    (fun (ct, form) ->
      match (ct, form) with
      | Dwarf.DW_LNCT_path, Dwarf.DW_FORM_string ->
          write_null_terminated_string buf file.name
      | Dwarf.DW_LNCT_directory_index, Dwarf.DW_FORM_udata ->
          let idx = find_dir_index h.directories file.directory in
          write_uleb128 buf (Unsigned.UInt64.of_int idx)
      | Dwarf.DW_LNCT_timestamp, Dwarf.DW_FORM_udata ->
          write_uleb128 buf file.timestamp
      | Dwarf.DW_LNCT_size, Dwarf.DW_FORM_udata -> write_uleb128 buf file.size
      | Dwarf.DW_LNCT_MD5, Dwarf.DW_FORM_data16 -> (
          match file.md5_checksum with
          | Some hex ->
              for i = 0 to 15 do
                let b = int_of_string ("0x" ^ String.sub hex (i * 2) 2) in
                write_u8 buf (Unsigned.UInt8.of_int b)
              done
          | None ->
              for _ = 0 to 15 do
                write_u8 buf (Unsigned.UInt8.of_int 0)
              done)
      | _ -> failwith "Unsupported file entry format")
    h.file_name_entry_formats

let write_line_header_body buf (h : Dwarf.DebugLine.line_program_header) =
  write_u8 buf h.minimum_instruction_length;
  write_u8 buf h.maximum_operations_per_instruction;
  write_u8 buf (Unsigned.UInt8.of_int (if h.default_is_stmt then 1 else 0));
  let lb = if h.line_base < 0 then h.line_base + 256 else h.line_base in
  write_u8 buf (Unsigned.UInt8.of_int lb);
  write_u8 buf h.line_range;
  write_u8 buf h.opcode_base;
  Array.iter (fun l -> write_u8 buf l) h.standard_opcode_lengths;
  write_u8 buf h.directory_entry_format_count;
  write_format_descs buf h.directory_entry_formats;
  write_uleb128 buf
    (Unsigned.UInt64.of_int (Unsigned.UInt32.to_int h.directories_count));
  Array.iter (fun dir -> write_line_dir_entry buf h dir) h.directories;
  write_u8 buf h.file_name_entry_format_count;
  write_format_descs buf h.file_name_entry_formats;
  write_uleb128 buf
    (Unsigned.UInt64.of_int (Unsigned.UInt32.to_int h.file_names_count));
  Array.iter (fun file -> write_line_file_entry buf h file) h.file_names

let write_lne buf opcode_byte operands_writer =
  let op_buf = Buffer.create 16 in
  operands_writer op_buf;
  let len = 1 + Buffer.length op_buf in
  write_u8 buf (Unsigned.UInt8.of_int 0);
  write_uleb128 buf (Unsigned.UInt64.of_int len);
  write_u8 buf (Unsigned.UInt8.of_int opcode_byte);
  Buffer.add_string buf (Buffer.contents op_buf)

let encode_line_entries buf (h : Dwarf.DebugLine.line_program_header)
    (entries : Dwarf.DebugLine.line_table_entry list) =
  let addr_sz = Unsigned.UInt8.to_int h.address_size in
  let min_inst = max 1 (Unsigned.UInt8.to_int h.minimum_instruction_length) in
  let addr = ref Unsigned.UInt64.zero in
  let file = ref (Unsigned.UInt32.of_int 0) in
  let ln = ref (Unsigned.UInt32.of_int 1) in
  let col = ref (Unsigned.UInt32.of_int 0) in
  let stmt = ref h.default_is_stmt in
  let cur_isa = ref (Unsigned.UInt32.of_int 0) in
  let first = ref true in
  let set_addr buf a =
    write_lne buf 0x02 (fun b -> write_address b addr_sz a);
    addr := a
  in
  let reset () =
    addr := Unsigned.UInt64.zero;
    file := Unsigned.UInt32.of_int 0;
    ln := Unsigned.UInt32.of_int 1;
    col := Unsigned.UInt32.of_int 0;
    stmt := h.default_is_stmt;
    cur_isa := Unsigned.UInt32.of_int 0;
    first := true
  in
  List.iter
    (fun (e : Dwarf.DebugLine.line_table_entry) ->
      if e.end_sequence then (
        if Unsigned.UInt64.compare e.address !addr <> 0 then
          set_addr buf e.address;
        write_lne buf 0x01 (fun _ -> ());
        reset ())
      else (
        if !first then (
          set_addr buf e.address;
          first := false)
        else if Unsigned.UInt64.compare e.address !addr <> 0 then (
          let d =
            Unsigned.UInt64.to_int (Unsigned.UInt64.sub e.address !addr)
          in
          let adv = d / min_inst in
          (* DW_LNS_advance_pc *)
          write_u8 buf (Unsigned.UInt8.of_int 0x02);
          write_uleb128 buf (Unsigned.UInt64.of_int adv);
          addr := e.address);
        if Unsigned.UInt32.compare e.file_index !file <> 0 then (
          (* DW_LNS_set_file *)
          write_u8 buf (Unsigned.UInt8.of_int 0x04);
          write_uleb128 buf
            (Unsigned.UInt64.of_int (Unsigned.UInt32.to_int e.file_index));
          file := e.file_index);
        if Unsigned.UInt32.compare e.column !col <> 0 then (
          (* DW_LNS_set_column *)
          write_u8 buf (Unsigned.UInt8.of_int 0x05);
          write_uleb128 buf
            (Unsigned.UInt64.of_int (Unsigned.UInt32.to_int e.column));
          col := e.column);
        if e.is_stmt <> !stmt then (
          (* DW_LNS_negate_stmt *)
          write_u8 buf (Unsigned.UInt8.of_int 0x06);
          stmt := e.is_stmt);
        if e.basic_block then
          (* DW_LNS_set_basic_block *)
          write_u8 buf (Unsigned.UInt8.of_int 0x07);
        if e.prologue_end then
          (* DW_LNS_set_prologue_end *)
          write_u8 buf (Unsigned.UInt8.of_int 0x0a);
        if e.epilogue_begin then
          (* DW_LNS_set_epilogue_begin *)
          write_u8 buf (Unsigned.UInt8.of_int 0x0b);
        (if Unsigned.UInt32.to_int e.discriminator <> 0 then
           let du64 =
             Unsigned.UInt64.of_int (Unsigned.UInt32.to_int e.discriminator)
           in
           write_lne buf 0x04 (fun b -> write_uleb128 b du64));
        if Unsigned.UInt32.compare e.isa !cur_isa <> 0 then (
          (* DW_LNS_set_isa *)
          write_u8 buf (Unsigned.UInt8.of_int 0x0c);
          write_uleb128 buf
            (Unsigned.UInt64.of_int (Unsigned.UInt32.to_int e.isa));
          cur_isa := e.isa);
        let ld = Unsigned.UInt32.to_int e.line - Unsigned.UInt32.to_int !ln in
        if ld <> 0 then (
          (* DW_LNS_advance_line *)
          write_u8 buf (Unsigned.UInt8.of_int 0x03);
          write_sleb128 buf (Signed.Int64.of_int ld);
          ln := e.line);
        (* DW_LNS_copy *)
        write_u8 buf (Unsigned.UInt8.of_int 0x01)))
    entries

let write_debug_line buf (header : Dwarf.DebugLine.line_program_header)
    (entries : Dwarf.DebugLine.line_table_entry list) =
  let fmt = header.format in
  let ver = Unsigned.UInt16.to_int header.version in
  let hdr_buf = Buffer.create 256 in
  write_line_header_body hdr_buf header;
  let hdr_len = Buffer.length hdr_buf in
  let prog_buf = Buffer.create 256 in
  encode_line_entries prog_buf header entries;
  let prog_len = Buffer.length prog_buf in
  let off_sz = if fmt = Dwarf.DWARF32 then 4 else 8 in
  let unit_len =
    2 + (if ver >= 5 then 2 else 0) + off_sz + hdr_len + prog_len
  in
  write_initial_length buf fmt unit_len;
  write_u16_le buf header.version;
  if ver >= 5 then (
    write_u8 buf header.address_size;
    write_u8 buf header.segment_selector_size);
  write_offset buf fmt (Unsigned.UInt64.of_int hdr_len);
  Buffer.add_string buf (Buffer.contents hdr_buf);
  Buffer.add_string buf (Buffer.contents prog_buf)

(* Stage 11: CFI Writer *)

type cfi_op =
  | CFA_advance_loc of int
  | CFA_offset of int * int
  | CFA_restore of int
  | CFA_nop
  | CFA_set_loc of int
  | CFA_advance_loc1 of int
  | CFA_advance_loc2 of int
  | CFA_advance_loc4 of int
  | CFA_offset_extended of int * int
  | CFA_restore_extended of int
  | CFA_undefined of int
  | CFA_same_value of int
  | CFA_register of int * int
  | CFA_remember_state
  | CFA_restore_state
  | CFA_def_cfa of int * int
  | CFA_def_cfa_register of int
  | CFA_def_cfa_offset of int
  | CFA_def_cfa_expression of string
  | CFA_expression of int * string
  | CFA_offset_extended_sf of int * int
  | CFA_def_cfa_sf of int * int
  | CFA_def_cfa_offset_sf of int
  | CFA_val_offset of int * int
  | CFA_val_offset_sf of int * int
  | CFA_val_expression of int * string

let write_cfi_instruction buf = function
  | CFA_advance_loc delta ->
      let b = 0x40 lor (delta land 0x3f) in
      write_u8 buf (Unsigned.UInt8.of_int b)
  | CFA_offset (reg, off) ->
      let b = 0x80 lor (reg land 0x3f) in
      write_u8 buf (Unsigned.UInt8.of_int b);
      write_uleb128 buf (Unsigned.UInt64.of_int off)
  | CFA_restore reg ->
      let b = 0xc0 lor (reg land 0x3f) in
      write_u8 buf (Unsigned.UInt8.of_int b)
  | CFA_nop -> write_u8 buf (Unsigned.UInt8.of_int 0x00)
  | CFA_set_loc addr ->
      write_u8 buf (Unsigned.UInt8.of_int 0x01);
      write_u32_le buf (Unsigned.UInt32.of_int addr)
  | CFA_advance_loc1 delta ->
      write_u8 buf (Unsigned.UInt8.of_int 0x02);
      write_u8 buf (Unsigned.UInt8.of_int delta)
  | CFA_advance_loc2 delta ->
      write_u8 buf (Unsigned.UInt8.of_int 0x03);
      write_u16_le buf (Unsigned.UInt16.of_int delta)
  | CFA_advance_loc4 delta ->
      write_u8 buf (Unsigned.UInt8.of_int 0x04);
      write_u32_le buf (Unsigned.UInt32.of_int delta)
  | CFA_offset_extended (reg, off) ->
      write_u8 buf (Unsigned.UInt8.of_int 0x05);
      write_uleb128 buf (Unsigned.UInt64.of_int reg);
      write_uleb128 buf (Unsigned.UInt64.of_int off)
  | CFA_restore_extended reg ->
      write_u8 buf (Unsigned.UInt8.of_int 0x06);
      write_uleb128 buf (Unsigned.UInt64.of_int reg)
  | CFA_undefined reg ->
      write_u8 buf (Unsigned.UInt8.of_int 0x07);
      write_uleb128 buf (Unsigned.UInt64.of_int reg)
  | CFA_same_value reg ->
      write_u8 buf (Unsigned.UInt8.of_int 0x08);
      write_uleb128 buf (Unsigned.UInt64.of_int reg)
  | CFA_register (reg, target) ->
      write_u8 buf (Unsigned.UInt8.of_int 0x09);
      write_uleb128 buf (Unsigned.UInt64.of_int reg);
      write_uleb128 buf (Unsigned.UInt64.of_int target)
  | CFA_remember_state -> write_u8 buf (Unsigned.UInt8.of_int 0x0a)
  | CFA_restore_state -> write_u8 buf (Unsigned.UInt8.of_int 0x0b)
  | CFA_def_cfa (reg, off) ->
      write_u8 buf (Unsigned.UInt8.of_int 0x0c);
      write_uleb128 buf (Unsigned.UInt64.of_int reg);
      write_uleb128 buf (Unsigned.UInt64.of_int off)
  | CFA_def_cfa_register reg ->
      write_u8 buf (Unsigned.UInt8.of_int 0x0d);
      write_uleb128 buf (Unsigned.UInt64.of_int reg)
  | CFA_def_cfa_offset off ->
      write_u8 buf (Unsigned.UInt8.of_int 0x0e);
      write_uleb128 buf (Unsigned.UInt64.of_int off)
  | CFA_def_cfa_expression expr ->
      write_u8 buf (Unsigned.UInt8.of_int 0x0f);
      write_uleb128 buf (Unsigned.UInt64.of_int (String.length expr));
      Buffer.add_string buf expr
  | CFA_expression (reg, expr) ->
      write_u8 buf (Unsigned.UInt8.of_int 0x10);
      write_uleb128 buf (Unsigned.UInt64.of_int reg);
      write_uleb128 buf (Unsigned.UInt64.of_int (String.length expr));
      Buffer.add_string buf expr
  | CFA_offset_extended_sf (reg, off) ->
      write_u8 buf (Unsigned.UInt8.of_int 0x11);
      write_uleb128 buf (Unsigned.UInt64.of_int reg);
      write_sleb128 buf (Signed.Int64.of_int off)
  | CFA_def_cfa_sf (reg, off) ->
      write_u8 buf (Unsigned.UInt8.of_int 0x12);
      write_uleb128 buf (Unsigned.UInt64.of_int reg);
      write_sleb128 buf (Signed.Int64.of_int off)
  | CFA_def_cfa_offset_sf off ->
      write_u8 buf (Unsigned.UInt8.of_int 0x13);
      write_sleb128 buf (Signed.Int64.of_int off)
  | CFA_val_offset (reg, off) ->
      write_u8 buf (Unsigned.UInt8.of_int 0x14);
      write_uleb128 buf (Unsigned.UInt64.of_int reg);
      write_uleb128 buf (Unsigned.UInt64.of_int off)
  | CFA_val_offset_sf (reg, off) ->
      write_u8 buf (Unsigned.UInt8.of_int 0x15);
      write_uleb128 buf (Unsigned.UInt64.of_int reg);
      write_sleb128 buf (Signed.Int64.of_int off)
  | CFA_val_expression (reg, expr) ->
      write_u8 buf (Unsigned.UInt8.of_int 0x16);
      write_uleb128 buf (Unsigned.UInt64.of_int reg);
      write_uleb128 buf (Unsigned.UInt64.of_int (String.length expr));
      Buffer.add_string buf expr

let write_cfi_instructions buf ops = List.iter (write_cfi_instruction buf) ops

let write_cie buf (cie : Dwarf.CallFrame.common_information_entry) =
  let fmt = cie.format in
  let off_sz = match fmt with Dwarf.DWARF32 -> 4 | Dwarf.DWARF64 -> 8 in
  let aug_str_len = String.length cie.augmentation + 1 in
  let caf_sz = uleb128_size cie.code_alignment_factor in
  let daf_sz = sleb128_size cie.data_alignment_factor in
  let rar_sz = uleb128_size cie.return_address_register in
  let aug_data_sz =
    match (cie.augmentation_length, cie.augmentation_data) with
    | Some len, Some data -> uleb128_size len + String.length data
    | _ -> 0
  in
  let body_len =
    off_sz + 1 + aug_str_len + 1 + 1 + caf_sz + daf_sz + rar_sz + aug_data_sz
    + String.length cie.initial_instructions
  in
  write_initial_length buf fmt body_len;
  (match fmt with
  | Dwarf.DWARF32 -> write_u32_le buf (Unsigned.UInt32.of_int32 0xffffffffl)
  | Dwarf.DWARF64 ->
      write_u64_le buf (Unsigned.UInt64.of_int64 0xffffffffffffffffL));
  write_u8 buf cie.version;
  write_null_terminated_string buf cie.augmentation;
  write_u8 buf cie.address_size;
  write_u8 buf cie.segment_selector_size;
  write_uleb128 buf cie.code_alignment_factor;
  write_sleb128 buf cie.data_alignment_factor;
  write_uleb128 buf cie.return_address_register;
  (match (cie.augmentation_length, cie.augmentation_data) with
  | Some len, Some data ->
      write_uleb128 buf len;
      Buffer.add_string buf data
  | _ -> ());
  Buffer.add_string buf cie.initial_instructions

let write_fde buf (fde : Dwarf.CallFrame.frame_description_entry) =
  let fmt = fde.format in
  let off_sz = match fmt with Dwarf.DWARF32 -> 4 | Dwarf.DWARF64 -> 8 in
  let aug_data_sz =
    match (fde.augmentation_length, fde.augmentation_data) with
    | Some len, Some data -> uleb128_size len + String.length data
    | _ -> 0
  in
  let body_len =
    off_sz + 8 + 8 + aug_data_sz + String.length fde.instructions
  in
  write_initial_length buf fmt body_len;
  write_offset buf fmt fde.cie_pointer;
  write_u64_le buf fde.initial_location;
  write_u64_le buf fde.address_range;
  (match (fde.augmentation_length, fde.augmentation_data) with
  | Some len, Some data ->
      write_uleb128 buf len;
      Buffer.add_string buf data
  | _ -> ());
  Buffer.add_string buf fde.instructions

let write_debug_frame buf entries =
  List.iter
    (function
      | Dwarf.CallFrame.CIE cie -> write_cie buf cie
      | Dwarf.CallFrame.FDE fde -> write_fde buf fde
      | Dwarf.CallFrame.Zero_terminator _ ->
          write_u32_le buf (Unsigned.UInt32.of_int 0))
    entries

(* Stage 12: .eh_frame Writer *)

let write_eh_cie buf (cie : Dwarf.CallFrame.common_information_entry) =
  let ver = Unsigned.UInt8.to_int cie.version in
  let aug_str_len = String.length cie.augmentation + 1 in
  let addr_seg_sz = if ver >= 4 then 2 else 0 in
  let caf_sz = uleb128_size cie.code_alignment_factor in
  let daf_sz = sleb128_size cie.data_alignment_factor in
  let rar_sz = uleb128_size cie.return_address_register in
  let aug_data_sz =
    match (cie.augmentation_length, cie.augmentation_data) with
    | Some len, Some data -> uleb128_size len + String.length data
    | _ -> 0
  in
  let body_len =
    4 + 1 + aug_str_len + addr_seg_sz + caf_sz + daf_sz + rar_sz + aug_data_sz
    + String.length cie.initial_instructions
  in
  write_u32_le buf (Unsigned.UInt32.of_int body_len);
  write_u32_le buf (Unsigned.UInt32.of_int 0);
  write_u8 buf cie.version;
  write_null_terminated_string buf cie.augmentation;
  if ver >= 4 then (
    write_u8 buf cie.address_size;
    write_u8 buf cie.segment_selector_size);
  write_uleb128 buf cie.code_alignment_factor;
  write_sleb128 buf cie.data_alignment_factor;
  write_uleb128 buf cie.return_address_register;
  (match (cie.augmentation_length, cie.augmentation_data) with
  | Some len, Some data ->
      write_uleb128 buf len;
      Buffer.add_string buf data
  | _ -> ());
  Buffer.add_string buf cie.initial_instructions

let write_eh_fde buf (fde : Dwarf.CallFrame.frame_description_entry)
    (cie_offset : int) =
  let fde_start = Buffer.length buf in
  let aug_data_sz =
    match (fde.augmentation_length, fde.augmentation_data) with
    | Some len, Some data -> uleb128_size len + String.length data
    | _ -> 0
  in
  let body_len = 4 + 4 + 4 + aug_data_sz + String.length fde.instructions in
  write_u32_le buf (Unsigned.UInt32.of_int body_len);
  let cie_ptr = fde_start + 4 + 4 - cie_offset in
  write_u32_le buf (Unsigned.UInt32.of_int cie_ptr);
  let il_field_pos = fde_start + 4 + 4 in
  let il_raw = Unsigned.UInt64.to_int fde.initial_location - il_field_pos in
  write_u32_le buf (Unsigned.UInt32.of_int il_raw);
  write_u32_le buf
    (Unsigned.UInt32.of_int64 (Unsigned.UInt64.to_int64 fde.address_range));
  (match (fde.augmentation_length, fde.augmentation_data) with
  | Some len, Some data ->
      write_uleb128 buf len;
      Buffer.add_string buf data
  | _ -> ());
  Buffer.add_string buf fde.instructions

let write_eh_frame buf entries =
  List.iter
    (function
      | Dwarf.EHFrame.EH_CIE cie -> write_eh_cie buf cie
      | Dwarf.EHFrame.EH_FDE fde -> write_eh_fde buf fde 0)
    entries

(* Stage 13: .debug_aranges Writer *)

let write_aranges_set buf (aset : Dwarf.DebugAranges.aranges_set) =
  let h = aset.header in
  let addr_sz = Unsigned.UInt8.to_int h.address_size in
  let fmt = h.format in
  let off_sz = Dwarf.offset_size_for_format fmt in
  let header_content_sz = 2 + off_sz + 1 + 1 in
  let pad = 4 in
  let range_count = List.length aset.ranges + 1 in
  let body_sz = header_content_sz + pad + (range_count * addr_sz * 2) in
  write_initial_length buf fmt body_sz;
  write_u16_le buf h.version;
  write_offset buf fmt h.debug_info_offset;
  write_u8 buf h.address_size;
  write_u8 buf h.segment_size;
  for _ = 1 to pad do
    write_u8 buf (Unsigned.UInt8.of_int 0)
  done;
  List.iter
    (fun (r : Dwarf.DebugAranges.address_range) ->
      write_address buf addr_sz r.start_address;
      write_address buf addr_sz r.length)
    aset.ranges;
  write_address buf addr_sz Unsigned.UInt64.zero;
  write_address buf addr_sz Unsigned.UInt64.zero

(* Stage 14: .debug_addr and .debug_str_offsets Writers *)

let write_debug_addr buf (t : Dwarf.DebugAddr.t) =
  let h = t.header in
  let addr_sz = Unsigned.UInt8.to_int h.address_size in
  let seg_sz = Unsigned.UInt8.to_int h.segment_selector_size in
  let entry_sz = seg_sz + addr_sz in
  let n = Array.length t.entries in
  let header_content_sz = 2 + 1 + 1 in
  let body_sz = header_content_sz + (n * entry_sz) in
  write_initial_length buf h.format body_sz;
  write_u16_le buf h.version;
  write_u8 buf h.address_size;
  write_u8 buf h.segment_selector_size;
  Array.iter
    (fun (e : Dwarf.DebugAddr.entry) ->
      (match e.segment with
      | Some s when seg_sz > 0 -> write_address buf seg_sz s
      | _ -> ());
      write_address buf addr_sz e.address)
    t.entries

(* Stage 15: .debug_names Writer *)

let write_debug_names_abbrev_table buf
    (abbrevs : Dwarf.DebugNames.debug_names_abbrev list) =
  List.iter
    (fun (a : Dwarf.DebugNames.debug_names_abbrev) ->
      write_uleb128 buf a.code;
      write_uleb128 buf (Dwarf.uint64_of_abbreviation_tag a.tag);
      List.iter
        (fun (attr, form) ->
          write_uleb128 buf
            (Unsigned.UInt64.of_int (Dwarf.int_of_name_index_attribute attr));
          write_uleb128 buf (Dwarf.u64_of_attribute_form_encoding form))
        a.attributes;
      write_uleb128 buf Unsigned.UInt64.zero;
      write_uleb128 buf Unsigned.UInt64.zero)
    abbrevs;
  write_uleb128 buf Unsigned.UInt64.zero

let debug_names_abbrev_table_size
    (abbrevs : Dwarf.DebugNames.debug_names_abbrev list) =
  let sz = ref 0 in
  List.iter
    (fun (a : Dwarf.DebugNames.debug_names_abbrev) ->
      sz := !sz + uleb128_size a.code;
      sz := !sz + uleb128_size (Dwarf.uint64_of_abbreviation_tag a.tag);
      List.iter
        (fun (attr, form) ->
          sz :=
            !sz
            + uleb128_size
                (Unsigned.UInt64.of_int
                   (Dwarf.int_of_name_index_attribute attr));
          sz := !sz + uleb128_size (Dwarf.u64_of_attribute_form_encoding form))
        a.attributes;
      sz := !sz + 1 + 1)
    abbrevs;
  sz := !sz + 1;
  !sz

let write_debug_names_entry buf
    (abbrevs : Dwarf.DebugNames.debug_names_abbrev list)
    (entry : Dwarf.DebugNames.name_index_entry) =
  let abbrev =
    List.find
      (fun (a : Dwarf.DebugNames.debug_names_abbrev) ->
        List.length a.attributes = List.length entry.attributes + 1
        && List.exists
             (fun (attr, _) -> attr = Dwarf.DW_IDX_die_offset)
             a.attributes)
      abbrevs
  in
  write_uleb128 buf abbrev.code;
  List.iter
    (fun (attr, form) ->
      let value =
        if attr = Dwarf.DW_IDX_die_offset then
          Unsigned.UInt64.of_uint32 entry.die_offset
        else
          match List.assoc_opt attr entry.attributes with
          | Some v -> v
          | None -> Unsigned.UInt64.zero
      in
      match form with
      | Dwarf.DW_FORM_ref4 ->
          write_u32_le buf
            (Unsigned.UInt32.of_int (Unsigned.UInt64.to_int value))
      | Dwarf.DW_FORM_udata -> write_uleb128 buf value
      | Dwarf.DW_FORM_flag_present -> ()
      | _ ->
          write_u32_le buf
            (Unsigned.UInt32.of_int (Unsigned.UInt64.to_int value)))
    abbrev.attributes

let debug_names_entry_size (abbrevs : Dwarf.DebugNames.debug_names_abbrev list)
    (entry : Dwarf.DebugNames.name_index_entry) =
  let abbrev =
    List.find
      (fun (a : Dwarf.DebugNames.debug_names_abbrev) ->
        List.length a.attributes = List.length entry.attributes + 1
        && List.exists
             (fun (attr, _) -> attr = Dwarf.DW_IDX_die_offset)
             a.attributes)
      abbrevs
  in
  let sz = ref (uleb128_size abbrev.code) in
  List.iter
    (fun (attr, form) ->
      let value =
        if attr = Dwarf.DW_IDX_die_offset then
          Unsigned.UInt64.of_uint32 entry.die_offset
        else
          match List.assoc_opt attr entry.attributes with
          | Some v -> v
          | None -> Unsigned.UInt64.zero
      in
      match form with
      | Dwarf.DW_FORM_ref4 -> sz := !sz + 4
      | Dwarf.DW_FORM_udata -> sz := !sz + uleb128_size value
      | Dwarf.DW_FORM_flag_present -> ()
      | _ -> sz := !sz + 4)
    abbrev.attributes;
  !sz

let write_debug_names buf (sec : Dwarf.DebugNames.debug_names_section) =
  let h = sec.header in
  let nc = Unsigned.UInt32.to_int h.name_count in
  let cu_count = Unsigned.UInt32.to_int h.comp_unit_count in
  let ltu_count = Unsigned.UInt32.to_int h.local_type_unit_count in
  let ftu_count = Unsigned.UInt32.to_int h.foreign_type_unit_count in
  let bc = Unsigned.UInt32.to_int h.bucket_count in
  let aug_sz = Unsigned.UInt32.to_int h.augmentation_string_size in
  let abbrev_sz = debug_names_abbrev_table_size sec.abbreviation_table in
  (* Compute entry pool size *)
  let entry_pool_sz = ref 0 in
  for i = 0 to nc - 1 do
    entry_pool_sz :=
      !entry_pool_sz
      + debug_names_entry_size sec.abbreviation_table sec.entry_pool.(i)
      + 1
  done;
  (* header content after initial_length:
     version(2) + padding(2) + comp_unit_count(4) +
     local_type_unit_count(4) + foreign_type_unit_count(4) +
     bucket_count(4) + name_count(4) + abbrev_table_size(4) +
     augmentation_string_size(4) + augmentation_string *)
  let header_content_sz = 2 + 2 + 4 + 4 + 4 + 4 + 4 + 4 + 4 + aug_sz in
  let arrays_sz =
    (cu_count * 4) + (ltu_count * 4) + (ftu_count * 8) + (bc * 4) + (nc * 4)
    + (nc * 4) + (nc * 4)
  in
  let body_sz = header_content_sz + arrays_sz + abbrev_sz + !entry_pool_sz in
  (* Header *)
  write_initial_length buf h.format body_sz;
  write_u16_le buf h.version;
  write_u16_le buf h.padding;
  write_u32_le buf h.comp_unit_count;
  write_u32_le buf h.local_type_unit_count;
  write_u32_le buf h.foreign_type_unit_count;
  write_u32_le buf h.bucket_count;
  write_u32_le buf h.name_count;
  write_u32_le buf (Unsigned.UInt32.of_int abbrev_sz);
  write_u32_le buf h.augmentation_string_size;
  if aug_sz > 0 then Buffer.add_string buf h.augmentation_string;
  (* CU offsets *)
  Array.iter (fun o -> write_u32_le buf o) sec.comp_unit_offsets;
  (* Local TU offsets *)
  Array.iter (fun o -> write_u32_le buf o) sec.local_type_unit_offsets;
  (* Foreign TU signatures *)
  Array.iter (fun s -> write_u64_le buf s) sec.foreign_type_unit_signatures;
  (* Hash buckets *)
  Array.iter (fun b -> write_u32_le buf b) sec.buckets;
  (* Hash values *)
  Array.iter (fun h -> write_u32_le buf h) sec.hash_table;
  (* Name table: string offsets *)
  Array.iter
    (fun (e : Dwarf.DebugNames.debug_str_entry) -> write_u32_le buf e.offset)
    sec.name_table;
  (* Entry offsets *)
  let offsets = Array.make nc Unsigned.UInt32.zero in
  let cur_off = ref 0 in
  for i = 0 to nc - 1 do
    offsets.(i) <- Unsigned.UInt32.of_int !cur_off;
    cur_off :=
      !cur_off
      + debug_names_entry_size sec.abbreviation_table sec.entry_pool.(i)
      + 1
  done;
  Array.iter (fun o -> write_u32_le buf o) offsets;
  (* Abbreviation table *)
  write_debug_names_abbrev_table buf sec.abbreviation_table;
  (* Entry pool *)
  for i = 0 to nc - 1 do
    write_debug_names_entry buf sec.abbreviation_table sec.entry_pool.(i);
    write_u8 buf (Unsigned.UInt8.of_int 0)
  done

let write_debug_str_offsets buf (t : Dwarf.DebugStrOffsets.t) =
  let h = t.header in
  let off_sz = Dwarf.offset_size_for_format h.format in
  let n = Array.length t.offsets in
  let header_content_sz = 2 + 2 in
  let body_sz = header_content_sz + (n * off_sz) in
  write_initial_length buf h.format body_sz;
  write_u16_le buf h.version;
  write_u16_le buf h.padding;
  Array.iter
    (fun (e : Dwarf.DebugStrOffsets.offset_entry) ->
      write_offset buf h.format e.offset)
    t.offsets

(* Stage 16: Split DWARF index writers *)

let write_unit_index buf (idx : Dwarf.SplitDwarf.unit_index) =
  let uc = idx.unit_count in
  let slot_count =
    if uc = 0 then 1
    else
      let n = ref 1 in
      while !n < uc * 2 do
        n := !n * 2
      done;
      !n
  in
  (* Collect all distinct dw_sect columns *)
  let columns =
    let tbl = Hashtbl.create 8 in
    Array.iter
      (fun (e : Dwarf.SplitDwarf.index_entry) ->
        List.iter (fun (s, _, _) -> Hashtbl.replace tbl s ()) e.contributions)
      idx.entries;
    Hashtbl.fold (fun k () acc -> k :: acc) tbl []
    |> List.sort (fun a b ->
           compare
             (Dwarf.SplitDwarf.int_of_dw_sect a)
             (Dwarf.SplitDwarf.int_of_dw_sect b))
  in
  let sc = List.length columns in
  (* Build hash table *)
  let hash_slots = Array.make slot_count Unsigned.UInt64.zero in
  let index_slots = Array.make slot_count 0 in
  Array.iteri
    (fun row (e : Dwarf.SplitDwarf.index_entry) ->
      let h = Unsigned.UInt64.to_int64 e.dwo_id in
      let mask = slot_count - 1 in
      let start = Int64.to_int h land mask in
      let rec find_slot s =
        if index_slots.(s) = 0 then s else find_slot ((s + 1) land mask)
      in
      let slot = find_slot start in
      hash_slots.(slot) <- e.dwo_id;
      index_slots.(slot) <- row + 1)
    idx.entries;
  (* Header *)
  write_u32_le buf (Unsigned.UInt32.of_int idx.version);
  write_u32_le buf (Unsigned.UInt32.of_int 0);
  write_u32_le buf (Unsigned.UInt32.of_int sc);
  write_u32_le buf (Unsigned.UInt32.of_int uc);
  write_u32_le buf (Unsigned.UInt32.of_int slot_count);
  (* Hash table *)
  Array.iter (fun h -> write_u64_le buf h) hash_slots;
  (* Index table *)
  Array.iter (fun i -> write_u32_le buf (Unsigned.UInt32.of_int i)) index_slots;
  (* Column headers *)
  List.iter
    (fun s ->
      write_u32_le buf
        (Unsigned.UInt32.of_int (Dwarf.SplitDwarf.int_of_dw_sect s)))
    columns;
  (* Offsets *)
  Array.iter
    (fun (e : Dwarf.SplitDwarf.index_entry) ->
      List.iter
        (fun col ->
          let off =
            match List.find_opt (fun (s, _, _) -> s = col) e.contributions with
            | Some (_, o, _) -> o
            | None -> 0
          in
          write_u32_le buf (Unsigned.UInt32.of_int off))
        columns)
    idx.entries;
  (* Sizes *)
  Array.iter
    (fun (e : Dwarf.SplitDwarf.index_entry) ->
      List.iter
        (fun col ->
          let sz =
            match List.find_opt (fun (s, _, _) -> s = col) e.contributions with
            | Some (_, _, s) -> s
            | None -> 0
          in
          write_u32_le buf (Unsigned.UInt32.of_int sz))
        columns)
    idx.entries

(* Stage 17: .debug_macro Writer *)

let write_debug_macro_entry buf (fmt : Dwarf.dwarf_format)
    (e : Dwarf.debug_macro_entry) =
  write_u8 buf
    (Unsigned.UInt8.of_int (Dwarf.int_of_macro_info_entry_type e.entry_type));
  match e.entry_type with
  | DW_MACRO_define | DW_MACRO_undef ->
      let line = Option.value e.line_number ~default:Unsigned.UInt32.zero in
      write_uleb128 buf (Unsigned.UInt64.of_uint32 line);
      let s = Option.value e.string_value ~default:"" in
      write_null_terminated_string buf s
  | DW_MACRO_define_strp | DW_MACRO_undef_strp | DW_MACRO_define_sup
  | DW_MACRO_undef_sup ->
      let line = Option.value e.line_number ~default:Unsigned.UInt32.zero in
      write_uleb128 buf (Unsigned.UInt64.of_uint32 line);
      let off = Option.value e.string_offset ~default:Unsigned.UInt64.zero in
      write_offset buf fmt off
  | DW_MACRO_define_strx | DW_MACRO_undef_strx ->
      let line = Option.value e.line_number ~default:Unsigned.UInt32.zero in
      write_uleb128 buf (Unsigned.UInt64.of_uint32 line);
      let off = Option.value e.string_offset ~default:Unsigned.UInt64.zero in
      write_uleb128 buf off
  | DW_MACRO_start_file ->
      let line = Option.value e.line_number ~default:Unsigned.UInt32.zero in
      write_uleb128 buf (Unsigned.UInt64.of_uint32 line);
      let fi = Option.value e.file_index ~default:Unsigned.UInt32.zero in
      write_uleb128 buf (Unsigned.UInt64.of_uint32 fi)
  | DW_MACRO_end_file -> ()
  | DW_MACRO_import | DW_MACRO_import_sup ->
      let off = Option.value e.string_offset ~default:Unsigned.UInt64.zero in
      write_offset buf fmt off
  | _ -> ()

let write_debug_macro_unit buf (u : Dwarf.debug_macro_unit) =
  let h = u.header in
  write_u16_le buf h.version;
  write_u8 buf h.flags;
  (match h.debug_line_offset with
  | Some off -> write_offset buf h.format off
  | None -> ());
  (match h.debug_str_offsets_offset with
  | Some off -> write_offset buf h.format off
  | None -> ());
  List.iter (fun e -> write_debug_macro_entry buf h.format e) u.entries;
  write_u8 buf (Unsigned.UInt8.of_int 0)

let write_debug_macro buf (sec : Dwarf.debug_macro_section) =
  List.iter (fun u -> write_debug_macro_unit buf u) sec.units

(* Stage 18: .debug_pubnames/.debug_pubtypes Writers *)

let write_pubnames_set buf (h : Dwarf.DebugPubnames.header)
    (entries : Dwarf.DebugPubnames.entry list) =
  let fmt = h.format in
  let off_sz = Dwarf.offset_size_for_format fmt in
  let entries_sz =
    List.fold_left
      (fun acc (e : Dwarf.DebugPubnames.entry) ->
        acc + off_sz + String.length e.name + 1)
      0 entries
  in
  let body_sz = 2 + off_sz + off_sz + entries_sz + off_sz in
  write_initial_length buf fmt body_sz;
  write_u16_le buf h.version;
  write_offset buf fmt h.debug_info_offset;
  write_offset buf fmt h.debug_info_length;
  List.iter
    (fun (e : Dwarf.DebugPubnames.entry) ->
      write_offset buf fmt e.offset;
      write_null_terminated_string buf e.name)
    entries;
  write_offset buf fmt Unsigned.UInt64.zero

let write_pubtypes_set buf (h : Dwarf.DebugPubtypes.header)
    (entries : Dwarf.DebugPubtypes.entry list) =
  let fmt = h.format in
  let off_sz = Dwarf.offset_size_for_format fmt in
  let entries_sz =
    List.fold_left
      (fun acc (e : Dwarf.DebugPubtypes.entry) ->
        acc + off_sz + String.length e.name + 1)
      0 entries
  in
  let body_sz = 2 + off_sz + off_sz + entries_sz + off_sz in
  write_initial_length buf fmt body_sz;
  write_u16_le buf h.version;
  write_offset buf fmt h.debug_info_offset;
  write_offset buf fmt h.debug_info_length;
  List.iter
    (fun (e : Dwarf.DebugPubtypes.entry) ->
      write_offset buf fmt e.offset;
      write_null_terminated_string buf e.name)
    entries;
  write_offset buf fmt Unsigned.UInt64.zero

(* Stage 19: .debug_types Writer *)

let write_type_unit buf (enc : Dwarf.encoding) (die : Dwarf.DIE.t)
    (lookup : int -> u64) (type_signature : u64) (type_offset : u64)
    (debug_abbrev_offset : u64) =
  let die_bytes = die_size die enc lookup in
  let off_sz = Dwarf.offset_size_for_format enc.format in
  let header_content_size = 2 + off_sz + 1 + 8 + off_sz in
  let unit_length = header_content_size + die_bytes in
  write_initial_length buf enc.format unit_length;
  write_u16_le buf (Unsigned.UInt16.of_int 4);
  write_offset buf enc.format debug_abbrev_offset;
  write_u8 buf enc.address_size;
  write_u64_le buf type_signature;
  write_offset buf enc.format type_offset;
  write_die buf die enc lookup
