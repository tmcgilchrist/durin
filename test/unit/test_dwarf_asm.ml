open Alcotest
open Durin
module Buffer = Stdlib.Buffer

let u64 = Unsigned.UInt64.of_int

let default_encoding : Dwarf.encoding =
  {
    format = DWARF32;
    address_size = Unsigned.UInt8.of_int 8;
    version = Unsigned.UInt16.of_int 5;
  }

let emit_to_string f =
  let buf = Buffer.create 256 in
  let fmt = Format.formatter_of_buffer buf in
  f fmt;
  Format.pp_print_flush fmt ();
  Buffer.contents buf

let assemble_and_read asm_text =
  let asm_file = Filename.temp_file "dwarf_asm_test_" ".s" in
  let obj_file = Filename.temp_file "dwarf_asm_test_" ".o" in
  let oc = open_out asm_file in
  output_string oc asm_text;
  close_out oc;
  let rc =
    Sys.command (Printf.sprintf "as -o %s %s 2>/dev/null" obj_file asm_file)
  in
  Sys.remove asm_file;
  if rc <> 0 then (
    (try Sys.remove obj_file with _ -> ());
    failwith "assembler failed");
  let obj_buf = Object.Buffer.parse obj_file in
  Sys.remove obj_file;
  obj_buf

let find_elf_section obj_buf name =
  let _header, sections = Object.Elf.read_elf obj_buf in
  match
    Array.find_opt
      (fun (s : Object.Elf.section) -> s.sh_name_str = name)
      sections
  with
  | Some s -> s
  | None -> failwith (Printf.sprintf "section %s not found" name)

let contains s sub =
  let r = Str.regexp_string sub in
  try
    ignore (Str.search_forward r s 0);
    true
  with Not_found -> false

(* Test primitive directives produce valid assembly text *)

let test_emit_byte () =
  let s = emit_to_string (fun fmt -> Dwarf_asm.emit_byte fmt 0x42) in
  check bool "contains .byte" true (contains s ".byte");
  check bool "has 0x42" true (contains s "0x42")

let test_emit_uleb128 () =
  let s = emit_to_string (fun fmt -> Dwarf_asm.emit_uleb128 fmt (u64 300)) in
  check bool "contains .uleb128" true (contains s ".uleb128")

let test_emit_sleb128 () =
  let s =
    emit_to_string (fun fmt ->
        Dwarf_asm.emit_sleb128 fmt (Signed.Int64.of_int (-42)))
  in
  check bool "contains .sleb128" true (contains s ".sleb128")

let test_emit_asciz () =
  let s = emit_to_string (fun fmt -> Dwarf_asm.emit_asciz fmt "hello") in
  check bool "contains .asciz" true (contains s ".asciz");
  check bool "contains hello" true (contains s "hello")

let test_emit_section () =
  let s =
    emit_to_string (fun fmt -> Dwarf_asm.emit_section fmt ".debug_info")
  in
  check bool "contains .section" true (contains s ".section");
  check bool "contains .debug_info" true (contains s ".debug_info")

let test_emit_label () =
  let s = emit_to_string (fun fmt -> Dwarf_asm.emit_label fmt ".Ltest_label") in
  check bool "contains label" true (contains s ".Ltest_label:")

(* Assemble-and-read roundtrip tests *)

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

let parse_abbrev_from_obj obj_buf =
  let sec = find_elf_section obj_buf ".debug_abbrev" in
  let offset = Unsigned.UInt64.to_int sec.Object.Elf.sh_offset in
  let cur = Object.Buffer.cursor obj_buf ~at:offset in
  parse_abbrev_from_cursor cur

let test_assemble_abbrev_roundtrip () =
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
    |]
  in
  let asm =
    emit_to_string (fun fmt -> Dwarf_asm.emit_debug_abbrev fmt abbrevs)
  in
  let obj_buf = assemble_and_read asm in
  let table = parse_abbrev_from_obj obj_buf in
  let abbrev = Hashtbl.find table (u64 1) in
  check int64 "tag" 0x11L (Unsigned.UInt64.to_int64 abbrev.Dwarf.tag);
  check bool "has_children" true abbrev.has_children;
  check int "attr_specs" 1 (List.length abbrev.attr_specs);
  let spec = List.hd abbrev.attr_specs in
  check int64 "attr" 0x03L (Unsigned.UInt64.to_int64 spec.attr);
  check int64 "form" 0x08L (Unsigned.UInt64.to_int64 spec.form)

let test_assemble_debug_info_roundtrip () =
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
  let asm =
    emit_to_string (fun fmt ->
        Dwarf_asm.emit_debug_abbrev fmt abbrevs;
        Dwarf_asm.emit_debug_info fmt enc [ die ] lookup)
  in
  let obj_buf = assemble_and_read asm in
  let table = parse_abbrev_from_obj obj_buf in
  let info_sec = find_elf_section obj_buf ".debug_info" in
  let offset = Unsigned.UInt64.to_int info_sec.Object.Elf.sh_offset in
  let cur = Object.Buffer.cursor obj_buf ~at:offset in
  let _span, header = Dwarf.parse_compile_unit_header cur in
  check int "version" 5 (Unsigned.UInt16.to_int header.version);
  check int "address_size" 8 (Unsigned.UInt8.to_int header.address_size);
  match Dwarf.DIE.parse_die cur table enc obj_buf with
  | None -> fail "expected die"
  | Some parsed -> (
      check int "tag" 0x11
        (Unsigned.UInt64.to_int (Dwarf.uint64_of_abbreviation_tag parsed.tag));
      check int "attr count" 2 (List.length parsed.attributes);
      let name = List.hd parsed.attributes in
      match name.value with
      | String s -> check string "name" "test.c" s
      | _ -> fail "expected String")

let test_assemble_debug_info_with_children () =
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
      attributes = [ { attr = DW_AT_name; value = String "types.c" } ];
      children = List.to_seq [ child ];
      offset = 0;
    }
  in
  let enc = default_encoding in
  let abbrevs, lookup = Dwarf_write.assign_abbreviations [ die ] in
  let asm =
    emit_to_string (fun fmt ->
        Dwarf_asm.emit_debug_abbrev fmt abbrevs;
        Dwarf_asm.emit_debug_info fmt enc [ die ] lookup)
  in
  let obj_buf = assemble_and_read asm in
  let table = parse_abbrev_from_obj obj_buf in
  let info_sec = find_elf_section obj_buf ".debug_info" in
  let offset = Unsigned.UInt64.to_int info_sec.Object.Elf.sh_offset in
  let cur = Object.Buffer.cursor obj_buf ~at:offset in
  let _span, _header = Dwarf.parse_compile_unit_header cur in
  match Dwarf.DIE.parse_die cur table enc obj_buf with
  | None -> fail "expected die"
  | Some parsed -> (
      match parsed.children () with
      | Seq.Nil -> fail "expected children"
      | Seq.Cons (child_parsed, _) ->
          check int "child tag" 0x24
            (Unsigned.UInt64.to_int
               (Dwarf.uint64_of_abbreviation_tag child_parsed.tag));
          check int "child attrs" 2 (List.length child_parsed.attributes))

let test_assemble_emit_all () =
  let die : Dwarf.DIE.t =
    {
      tag = DW_TAG_compile_unit;
      attributes =
        [
          { attr = DW_AT_name; value = String "main.c" };
          { attr = DW_AT_language; value = Language Dwarf.DW_LANG_C99 };
        ];
      children = Seq.empty;
      offset = 0;
    }
  in
  let enc = default_encoding in
  let asm = emit_to_string (fun fmt -> Dwarf_asm.emit_all fmt enc [ die ]) in
  let obj_buf = assemble_and_read asm in
  let table = parse_abbrev_from_obj obj_buf in
  let info_sec = find_elf_section obj_buf ".debug_info" in
  let offset = Unsigned.UInt64.to_int info_sec.Object.Elf.sh_offset in
  let cur = Object.Buffer.cursor obj_buf ~at:offset in
  let _span, header = Dwarf.parse_compile_unit_header cur in
  check int "version" 5 (Unsigned.UInt16.to_int header.version);
  match Dwarf.DIE.parse_die cur table enc obj_buf with
  | None -> fail "expected die"
  | Some parsed -> (
      check int "attr count" 2 (List.length parsed.attributes);
      let lang = List.nth parsed.attributes 1 in
      match lang.value with
      | Language l ->
          check string "language" "DW_LANG_C99"
            (Dwarf.string_of_dwarf_language l)
      | _ -> fail "expected Language")

let test_assemble_debug_str () =
  let table = Dwarf_write.create_string_table () in
  let _off1 = Dwarf_write.add_string table "hello" in
  let _off2 = Dwarf_write.add_string table "world" in
  let asm = emit_to_string (fun fmt -> Dwarf_asm.emit_debug_str fmt table) in
  let obj_buf = assemble_and_read asm in
  let str_sec = find_elf_section obj_buf ".debug_str" in
  let base = Unsigned.UInt64.to_int str_sec.Object.Elf.sh_offset in
  let cur0 = Object.Buffer.cursor obj_buf ~at:base in
  let s1 = Object.Buffer.Read.zero_string cur0 () in
  check (option string) "first string" (Some "hello") s1;
  let cur6 = Object.Buffer.cursor obj_buf ~at:(base + 6) in
  let s2 = Object.Buffer.Read.zero_string cur6 () in
  check (option string) "second string" (Some "world") s2

let () =
  run "Dwarf_asm"
    [
      ( "directives",
        [
          test_case "emit_byte" `Quick test_emit_byte;
          test_case "emit_uleb128" `Quick test_emit_uleb128;
          test_case "emit_sleb128" `Quick test_emit_sleb128;
          test_case "emit_asciz" `Quick test_emit_asciz;
          test_case "emit_section" `Quick test_emit_section;
          test_case "emit_label" `Quick test_emit_label;
        ] );
      ( "assemble-roundtrip",
        [
          test_case "abbrev roundtrip" `Quick test_assemble_abbrev_roundtrip;
          test_case "debug_info roundtrip" `Quick
            test_assemble_debug_info_roundtrip;
          test_case "debug_info with children" `Quick
            test_assemble_debug_info_with_children;
          test_case "emit_all roundtrip" `Quick test_assemble_emit_all;
          test_case "debug_str roundtrip" `Quick test_assemble_debug_str;
        ] );
    ]
