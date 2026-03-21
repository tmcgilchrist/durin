open Types
module Buffer = Stdlib.Buffer

let emit_byte fmt v = Format.fprintf fmt "\t.byte\t0x%02x@\n" v
let emit_2byte fmt v = Format.fprintf fmt "\t.2byte\t0x%04x@\n" v
let emit_4byte fmt v = Format.fprintf fmt "\t.4byte\t0x%08x@\n" v
let emit_8byte fmt v = Format.fprintf fmt "\t.8byte\t0x%016Lx@\n" v

let emit_uleb128 fmt v =
  Format.fprintf fmt "\t.uleb128\t%s@\n" (Unsigned.UInt64.to_string v)

let emit_sleb128 fmt v =
  Format.fprintf fmt "\t.sleb128\t%s@\n" (Signed.Int64.to_string v)

let emit_asciz fmt s = Format.fprintf fmt "\t.asciz\t\"%s\"@\n" s
let emit_label fmt name = Format.fprintf fmt "%s:@\n" name
let emit_comment fmt s = Format.fprintf fmt "\t# %s@\n" s

let emit_section fmt name =
  Format.fprintf fmt "\t.section\t%s,\"\",@@progbits@\n" name

let emit_initial_length fmt (format : Dwarf.dwarf_format) ~start_label
    ~end_label =
  match format with
  | DWARF32 -> Format.fprintf fmt "\t.4byte\t%s - %s@\n" end_label start_label
  | DWARF64 ->
      emit_4byte fmt 0xffffffff;
      Format.fprintf fmt "\t.8byte\t%s - %s@\n" end_label start_label

let emit_offset fmt (format : Dwarf.dwarf_format) label =
  match format with
  | DWARF32 -> Format.fprintf fmt "\t.4byte\t%s@\n" label
  | DWARF64 -> Format.fprintf fmt "\t.8byte\t%s@\n" label

let emit_attribute_value fmt (value : Dwarf.DIE.attribute_value)
    (form : Dwarf.attribute_form_encoding) (enc : Dwarf.encoding) =
  match (form, value) with
  | DW_FORM_string, String s -> emit_asciz fmt s
  | DW_FORM_strx, IndexedString (idx, _) ->
      emit_uleb128 fmt (Unsigned.UInt64.of_int idx)
  | DW_FORM_udata, UData v -> emit_uleb128 fmt v
  | DW_FORM_udata, Language l ->
      emit_uleb128 fmt (Unsigned.UInt64.of_int (Dwarf.int_of_dwarf_language l))
  | DW_FORM_udata, Encoding e ->
      emit_uleb128 fmt (Unsigned.UInt64.of_int (Dwarf.int_of_base_type e))
  | DW_FORM_udata, Ordering v ->
      emit_uleb128 fmt (Unsigned.UInt64.of_int (Dwarf.int_of_array_ordering v))
  | DW_FORM_udata, DecimalSign v ->
      emit_uleb128 fmt (Unsigned.UInt64.of_int (Dwarf.int_of_decimal_sign v))
  | DW_FORM_udata, Endianity v ->
      emit_uleb128 fmt (Unsigned.UInt64.of_int (Dwarf.int_of_endianity v))
  | DW_FORM_udata, Accessibility v ->
      emit_uleb128 fmt (Unsigned.UInt64.of_int (Dwarf.int_of_accessibility v))
  | DW_FORM_udata, Visibility v ->
      emit_uleb128 fmt (Unsigned.UInt64.of_int (Dwarf.int_of_visibility v))
  | DW_FORM_udata, Virtuality v ->
      emit_uleb128 fmt (Unsigned.UInt64.of_int (Dwarf.int_of_virtuality v))
  | DW_FORM_udata, IdentifierCase v ->
      emit_uleb128 fmt (Unsigned.UInt64.of_int (Dwarf.int_of_identifier v))
  | DW_FORM_udata, CallingConvention v ->
      emit_uleb128 fmt
        (Unsigned.UInt64.of_int (Dwarf.int_of_calling_convention v))
  | DW_FORM_udata, Inline v ->
      emit_uleb128 fmt (Unsigned.UInt64.of_int (Dwarf.int_of_inlined v))
  | DW_FORM_udata, Defaulted v ->
      emit_uleb128 fmt
        (Unsigned.UInt64.of_int (Dwarf.int_of_defaulted_attribute v))
  | DW_FORM_sdata, SData v -> emit_sleb128 fmt v
  | DW_FORM_addr, Address a -> (
      match Unsigned.UInt8.to_int enc.address_size with
      | 4 -> emit_4byte fmt (Int64.to_int (Unsigned.UInt64.to_int64 a))
      | 8 -> emit_8byte fmt (Unsigned.UInt64.to_int64 a)
      | n -> failwith (Printf.sprintf "Unsupported address size: %d" n))
  | DW_FORM_addrx, IndexedAddress (idx, _) ->
      emit_uleb128 fmt (Unsigned.UInt64.of_int idx)
  | DW_FORM_flag_present, Flag _ -> ()
  | DW_FORM_flag, Flag b -> emit_byte fmt (if b then 1 else 0)
  | DW_FORM_ref4, Reference r ->
      emit_4byte fmt (Int64.to_int (Unsigned.UInt64.to_int64 r))
  | DW_FORM_block, Block b ->
      emit_uleb128 fmt (Unsigned.UInt64.of_int (String.length b));
      String.iter (fun c -> emit_byte fmt (Char.code c)) b
  | _ -> failwith "Unsupported form/value for asm"

let rec emit_die fmt (die : Dwarf.DIE.t) (enc : Dwarf.encoding)
    (lookup : int -> u64) =
  let code = lookup die.offset in
  emit_uleb128 fmt code;
  List.iter
    (fun (attr : Dwarf.DIE.attribute) ->
      let form = Dwarf_write.form_for_attribute_value attr.value in
      emit_attribute_value fmt attr.value form enc)
    die.attributes;
  let has_children =
    match die.children () with Seq.Nil -> false | Seq.Cons _ -> true
  in
  if has_children then (
    Seq.iter (fun child -> emit_die fmt child enc lookup) die.children;
    emit_byte fmt 0)

let emit_abbrev_table fmt (abbrevs : Dwarf.abbrev array) =
  Array.iter
    (fun (a : Dwarf.abbrev) ->
      emit_uleb128 fmt a.code;
      emit_uleb128 fmt (Dwarf.uint64_of_abbreviation_tag a.tag);
      emit_byte fmt (if a.has_children then 1 else 0);
      List.iter
        (fun (spec : Dwarf.attr_spec) ->
          emit_uleb128 fmt (Dwarf.u64_of_attribute_encoding spec.attr);
          emit_uleb128 fmt (Dwarf.u64_of_attribute_form_encoding spec.form);
          match spec.implicit_const with
          | Some v -> emit_sleb128 fmt (Signed.Int64.of_int64 v)
          | None -> ())
        a.attr_specs;
      emit_uleb128 fmt Unsigned.UInt64.zero;
      emit_uleb128 fmt Unsigned.UInt64.zero)
    abbrevs;
  emit_uleb128 fmt Unsigned.UInt64.zero

let emit_compile_unit fmt (enc : Dwarf.encoding) (die : Dwarf.DIE.t)
    (lookup : int -> u64) ~abbrev_label ~unit_id =
  let start_label = Printf.sprintf ".Ldebug_info%d_start" unit_id in
  let end_label = Printf.sprintf ".Ldebug_info%d_end" unit_id in
  emit_initial_length fmt enc.format ~start_label ~end_label;
  emit_label fmt start_label;
  emit_2byte fmt (Unsigned.UInt16.to_int enc.version);
  emit_byte fmt 0x01;
  emit_byte fmt (Unsigned.UInt8.to_int enc.address_size);
  emit_offset fmt enc.format abbrev_label;
  emit_die fmt die enc lookup;
  emit_label fmt end_label

let emit_debug_abbrev fmt (abbrevs : Dwarf.abbrev array) =
  emit_section fmt ".debug_abbrev";
  emit_label fmt ".Ldebug_abbrev0";
  emit_abbrev_table fmt abbrevs

let emit_debug_info fmt (enc : Dwarf.encoding) (dies : Dwarf.DIE.t list)
    (lookup : int -> u64) =
  emit_section fmt ".debug_info";
  List.iteri
    (fun i die ->
      emit_compile_unit fmt enc die lookup ~abbrev_label:".Ldebug_abbrev0"
        ~unit_id:i)
    dies

let emit_string_table_section fmt section_name
    (table : Dwarf_write.string_table) =
  emit_section fmt section_name;
  let buf = Buffer.create 64 in
  Dwarf_write.write_string_table buf table;
  let contents = Buffer.contents buf in
  let i = ref 0 in
  let len = String.length contents in
  while !i < len do
    let start = !i in
    while !i < len && contents.[!i] <> '\x00' do
      incr i
    done;
    let s = String.sub contents start (!i - start) in
    emit_asciz fmt s;
    if !i < len then incr i
  done

let emit_debug_str fmt table = emit_string_table_section fmt ".debug_str" table

let emit_debug_line_str fmt table =
  emit_string_table_section fmt ".debug_line_str" table

let emit_all fmt (enc : Dwarf.encoding) (dies : Dwarf.DIE.t list) =
  let abbrevs, lookup = Dwarf_write.assign_abbreviations dies in
  emit_debug_abbrev fmt abbrevs;
  emit_debug_info fmt enc dies lookup
