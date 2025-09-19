(* An implementation of the "dwarfdump" utility *)
open Durin

let find_debug_section buffer section_name =
  (* Try to find the specified DWARF section in MachO file *)
  try
    let open Object.Macho in
    let _header, commands = read buffer in

    (* Look for __DWARF segment *)
    let dwarf_segment_opt =
      List.find_map
        (function
          | LC_SEGMENT_64 (lazy seg) when seg.seg_segname = "__DWARF" ->
              Some seg
          | _ -> None)
        commands
    in

    match dwarf_segment_opt with
    | None -> None
    | Some dwarf_segment ->
        (* Find the specified section within __DWARF segment *)
        Array.find_map
          (fun section ->
            if section.sec_sectname = section_name then
              Some (section.sec_offset, section.sec_size)
            else None)
          dwarf_segment.seg_sections
  with _ -> None

let find_debug_line_section buffer object_format =
  let debug_line_section_name =
    Dwarf.object_format_to_section_name object_format Dwarf.Debug_line
  in
  find_debug_section buffer debug_line_section_name

let resolve_binary_path filename =
  (* Determine if we're dealing with a regular binary or dSYM *)
  if Sys.file_exists filename then (filename, false)
  else
    (* Try to find dSYM file *)
    let dsym_path =
      filename ^ ".dSYM/Contents/Resources/DWARF/" ^ Filename.basename filename
    in
    if Sys.file_exists dsym_path then (dsym_path, true) else (filename, false)
(* Use original filename, will fail later if not found *)

let suggest_dsym_if_needed filename is_dsym section_name =
  if not is_dsym then (
    Printf.printf "Note: For MachO binaries, %s is typically in .dSYM bundles\n"
      section_name;
    let dsym_path =
      filename ^ ".dSYM/Contents/Resources/DWARF/" ^ Filename.basename filename
    in
    if Sys.file_exists dsym_path then Printf.printf "Try: %s\n" dsym_path)

let handle_section_not_found section_name filename is_dsym =
  Printf.printf "No %s section found in file\n" section_name;
  suggest_dsym_if_needed filename is_dsym section_name

let create_section_cursor buffer section_offset =
  Object.Buffer.cursor buffer ~at:(Unsigned.UInt32.to_int section_offset)

let init_dwarf_context filename =
  let actual_filename, is_dsym = resolve_binary_path filename in
  (* Check if the file is a directory before trying to parse it *)
  if Sys.is_directory actual_filename then
    failwith (Printf.sprintf "'%s' is a directory" actual_filename)
  else
    let buffer = Object.Buffer.parse actual_filename in
    let format_str = Dwarf.detect_format_and_arch buffer in
    let object_format = Dwarf.detect_format buffer in
    (actual_filename, is_dsym, buffer, format_str, object_format)

let handle_dwarf_errors f =
  try f () with
  | Sys_error msg ->
      Printf.eprintf "Error: %s\n" msg;
      exit 1
  | Failure msg ->
      Printf.eprintf "Error: %s\n" msg;
      exit 1
  | Unix.Unix_error (Unix.EISDIR, _, filename) ->
      Printf.eprintf "Error: '%s' is a directory\n" filename;
      exit 1
  | exn ->
      Printf.eprintf "Error parsing DWARF information: %s\n"
        (Printexc.to_string exn);
      exit 1

let read_null_terminated_string cursor =
  let str_buffer = Stdlib.Buffer.create 256 in
  let rec read_string () =
    let byte = Object.Buffer.Read.u8 cursor in
    if Unsigned.UInt8.to_int byte = 0 then ()
    else (
      Stdlib.Buffer.add_char str_buffer
        (char_of_int (Unsigned.UInt8.to_int byte));
      read_string ())
  in
  read_string ();
  Stdlib.Buffer.contents str_buffer

let dump_line_program_header header =
  Printf.printf "Line table prologue:\n";
  Printf.printf "    total_length: 0x%08lx\n"
    (Unsigned.UInt32.to_int32 header.Dwarf.LineTable.unit_length);
  (* TODO derive this from the header, currently we only handle DWARF32 *)
  Printf.printf "          format: DWARF32\n";
  Printf.printf "         version: %d\n" (Unsigned.UInt16.to_int header.version);
  Printf.printf "    address_size: %d\n"
    (Unsigned.UInt8.to_int header.address_size);
  Printf.printf " seg_select_size: %d\n"
    (Unsigned.UInt8.to_int header.segment_selector_size);
  Printf.printf " prologue_length: 0x%08lx\n"
    (Unsigned.UInt32.to_int32 header.header_length);
  Printf.printf " min_inst_length: %d\n"
    (Unsigned.UInt8.to_int header.minimum_instruction_length);
  Printf.printf "max_ops_per_inst: %d\n"
    (Unsigned.UInt8.to_int header.maximum_operations_per_instruction);
  Printf.printf " default_is_stmt: %d\n"
    (if header.default_is_stmt then 1 else 0);
  Printf.printf "       line_base: %d\n" header.line_base;
  Printf.printf "      line_range: %d\n"
    (Unsigned.UInt8.to_int header.line_range);
  Printf.printf "     opcode_base: %d\n"
    (Unsigned.UInt8.to_int header.opcode_base);

  (* Print standard opcode lengths *)
  for i = 0 to Array.length header.standard_opcode_lengths - 1 do
    let opcode_name =
      try
        let opcode_value = i + 1 in
        (* Standard opcodes are 1-based *)
        let opcode = Dwarf.line_number_opcode opcode_value in
        Dwarf.string_of_line_number_opcode opcode
      with Failure _ -> "unknown"
    in
    let length = Unsigned.UInt8.to_int header.standard_opcode_lengths.(i) in
    Printf.printf "standard_opcode_lengths[%s] = %d\n" opcode_name length
  done;

  (* Print directories *)
  for i = 0 to Array.length header.directories - 1 do
    Printf.printf "include_directories[%3d] = \"%s\"\n" i header.directories.(i)
  done;

  (* Print file names *)
  for i = 0 to Array.length header.file_names - 1 do
    let file_entry = header.file_names.(i) in
    Printf.printf "file_names[%3d]:\n" i;
    Printf.printf "           name: \"%s\"\n" file_entry.name;

    (* Find the directory index by searching through the directories array *)
    let dir_index = ref (-1) in
    for j = 0 to Array.length header.directories - 1 do
      if header.directories.(j) = file_entry.directory then dir_index := j
    done;
    Printf.printf "      dir_index: %d\n" !dir_index;

    if Unsigned.UInt64.to_int file_entry.timestamp <> 0 then
      Printf.printf "   mod_time: %Ld\n"
        (Unsigned.UInt64.to_int64 file_entry.timestamp);
    if Unsigned.UInt64.to_int file_entry.size <> 0 then
      Printf.printf "     length: %Ld\n"
        (Unsigned.UInt64.to_int64 file_entry.size);

    (* Show MD5 checksum if available *)
    match file_entry.md5_checksum with
    | Some md5_hash -> Printf.printf "   md5_checksum: %s\n" md5_hash
    | None -> ()
  done

let dump_debug_line filename =
  handle_dwarf_errors (fun () ->
      let actual_filename, is_dsym, buffer, format_str, object_format =
        init_dwarf_context filename
      in
      Printf.printf "%s:\tfile format %s\n\n" actual_filename format_str;
      Printf.printf ".debug_line contents:\n";

      (* Try to find and parse the debug_line section *)
      match find_debug_line_section buffer object_format with
      | None -> handle_section_not_found "__debug_line section" filename is_dsym
      | Some (offset, _size) ->
          Printf.printf "debug_line[0x%08x]\n" 0;

          (* Create cursor at the debug_line section offset *)
          let cursor = create_section_cursor buffer offset in

          (* Parse the line program header using our implementation *)
          let header =
            Dwarf.LineTable.parse_line_program_header cursor buffer
          in

          (* Dump the header information *)
          dump_line_program_header header;

          (* Parse the line program and display entries *)
          let entries = Dwarf.LineTable.parse_line_program cursor header in
          Printf.printf "\n";

          (* Display line table header *)
          Printf.printf
            "Address            Line   Column File   ISA Discriminator OpIndex \
             Flags\n";
          Printf.printf
            "------------------ ------ ------ ------ --- ------------- ------- \
             -------------\n";

          (* Display each entry *)
          List.iter
            (fun entry ->
              let flags =
                let flags_list = [] in
                let flags_list =
                  if entry.Dwarf.LineTable.is_stmt then "is_stmt" :: flags_list
                  else flags_list
                in
                let flags_list =
                  if entry.Dwarf.LineTable.basic_block then
                    "basic_block" :: flags_list
                  else flags_list
                in
                let flags_list =
                  if entry.Dwarf.LineTable.end_sequence then
                    "end_sequence" :: flags_list
                  else flags_list
                in
                let flags_list =
                  if entry.Dwarf.LineTable.prologue_end then
                    "prologue_end" :: flags_list
                  else flags_list
                in
                let flags_list =
                  if entry.Dwarf.LineTable.epilogue_begin then
                    "epilogue_begin" :: flags_list
                  else flags_list
                in
                " " ^ String.concat " " (List.rev flags_list)
              in
              Printf.printf "0x%016Lx %6ld %6ld %6ld %3ld %13ld %7ld %s\n"
                (Unsigned.UInt64.to_int64 entry.Dwarf.LineTable.address)
                (Unsigned.UInt32.to_int32 entry.Dwarf.LineTable.line)
                (Unsigned.UInt32.to_int32 entry.Dwarf.LineTable.column)
                (Unsigned.UInt32.to_int32 entry.Dwarf.LineTable.file_index)
                (Unsigned.UInt32.to_int32 entry.Dwarf.LineTable.isa)
                (Unsigned.UInt32.to_int32 entry.Dwarf.LineTable.discriminator)
                (Unsigned.UInt32.to_int32 entry.Dwarf.LineTable.op_index)
                flags)
            entries;
          Printf.printf "\n")

let string_of_abbreviation_tag tag =
  Dwarf.(uint64_of_abbreviation_tag tag |> string_of_abbreviation_tag)

(* TODO Move into main dwarf.ml library when implementing CFI parsing. *)
let decode_simple_dwarf_expression block_data =
  (* Simple decoder for common DWARF expressions, especially register references *)
  if String.length block_data = 0 then None
  else
    let opcode = Char.code block_data.[0] in
    match opcode with
    (* DW_OP_reg0 - DW_OP_reg31: 0x50-0x6f *)
    | n when n >= 0x50 && n <= 0x6f ->
        let reg_num = n - 0x50 in
        (* ARM64 register names *)
        let reg_name =
          match reg_num with
          | 29 -> "W29" (* Frame pointer *)
          | 30 -> "W30" (* Link register *)
          | n when n <= 30 -> Printf.sprintf "x%d" n
          | _ -> Printf.sprintf "reg%d" n
        in
        Some (Printf.sprintf "DW_OP_reg%d %s" reg_num reg_name)
    (* Add more opcodes as needed *)
    | _ -> None

let format_dwarf_expression_block block_data =
  match decode_simple_dwarf_expression block_data with
  | Some decoded -> Printf.sprintf "(%s)" decoded
  | None -> Printf.sprintf "(<%d bytes>)" (String.length block_data)

let resolve_file_index buffer object_format stmt_list_offset file_index =
  (* Try to find the debug_line section and resolve file index to filename *)
  try
    match find_debug_line_section buffer object_format with
    | None -> None
    | Some (debug_line_offset, _size) ->
        (* Calculate absolute offset in debug_line section *)
        let absolute_offset =
          Unsigned.UInt32.to_int debug_line_offset
          + Unsigned.UInt64.to_int stmt_list_offset
        in
        let cursor = Object.Buffer.cursor buffer ~at:absolute_offset in
        let header = Dwarf.LineTable.parse_line_program_header cursor buffer in
        let file_index_int = Unsigned.UInt64.to_int file_index in
        if file_index_int < Array.length header.file_names then
          let file_entry = header.file_names.(file_index_int) in
          let full_path =
            if file_entry.directory = "" then file_entry.name
            else file_entry.directory ^ "/" ^ file_entry.name
          in
          Some full_path
        else None
  with _ -> None

let resolve_address_attribute buffer die attr_name addr_value cu_addr_base =
  (* Check if this is an address attribute that might need resolution *)
  match attr_name with
  | Dwarf.DW_AT_low_pc | Dwarf.DW_AT_entry_pc -> (
      (* Use compilation unit's addr_base for address resolution *)
      match cu_addr_base with
      | Some addr_base ->
          let index = Unsigned.UInt64.to_int addr_value in
          Dwarf.resolve_address_index buffer index addr_base
      | None -> addr_value)
  | Dwarf.DW_AT_high_pc -> (
      (* DW_AT_high_pc can be either absolute address or offset from DW_AT_low_pc *)
      (* If it came from DW_FORM_addrx, resolve it using addr_base *)
      (* If it came from DW_FORM_data*, it's an offset from low_pc *)
      match Dwarf.DIE.find_attribute die Dwarf.DW_AT_low_pc with
      | Some (Dwarf.DIE.Address low_pc) ->
          (* For data forms, addr_value is an offset from low_pc *)
          (* Resolve low_pc first, then add the offset *)
          let resolved_low_pc =
            match cu_addr_base with
            | Some addr_base ->
                let index = Unsigned.UInt64.to_int low_pc in
                Dwarf.resolve_address_index buffer index addr_base
            | None -> low_pc
          in
          Unsigned.UInt64.add resolved_low_pc addr_value
      | _ -> (
          (* If no DW_AT_low_pc found, try to resolve as direct address *)
          match cu_addr_base with
          | Some addr_base ->
              let index = Unsigned.UInt64.to_int addr_value in
              Dwarf.resolve_address_index buffer index addr_base
          | None -> addr_value))
  | _ -> addr_value

let resolve_type_reference buffer abbrev_table debug_info_offset die_offset =
  (* Try to parse DIE at the given offset and extract its name *)
  try
    (* The die_offset is relative to debug_info section start *)
    let absolute_offset =
      debug_info_offset + Unsigned.UInt64.to_int die_offset
    in
    let cursor = Object.Buffer.cursor buffer ~at:absolute_offset in
    match Dwarf.DIE.parse_die cursor abbrev_table buffer with
    | Some die -> (
        (* Look for DW_AT_name attribute in the referenced DIE *)
        match Dwarf.DIE.find_attribute die Dwarf.DW_AT_name with
        | Some (Dwarf.DIE.String name) -> Some name
        | _ -> None)
    | None -> None
  with _ -> None

let rec print_die die depth buffer object_format stmt_list_offset cu_addr_base
    debug_info_offset abbrev_table =
  (* Indentation pattern from test expectations:
     - All DIEs: no leading spaces before offset
     - Root DIE: 1 space after colon, 14 spaces for attributes
     - Child DIEs: 3 spaces after colon, 16 spaces for attributes *)
  let colon_spaces = if depth = 0 then " " else "   " in
  let attr_spaces =
    if depth = 0 then "              " else "                "
  in

  Printf.printf "\n0x%08x:%s%s\n" die.Dwarf.DIE.offset colon_spaces
    (string_of_abbreviation_tag die.Dwarf.DIE.tag);

  (* Print attributes *)
  List.iter
    (fun attr ->
      let attr_name = Dwarf.string_of_attribute_encoding attr.Dwarf.DIE.attr in
      let attr_value =
        match attr.Dwarf.DIE.value with
        | Dwarf.DIE.String s -> Printf.sprintf "(\"%s\")" s
        | Dwarf.DIE.UData u ->
            (* Special handling for DW_AT_high_pc which might be an offset from DW_AT_low_pc *)
            if attr.Dwarf.DIE.attr = Dwarf.DW_AT_high_pc then
              let resolved_addr =
                resolve_address_attribute buffer die attr.Dwarf.DIE.attr u
                  cu_addr_base
              in
              Printf.sprintf "(0x%016Lx)"
                (Unsigned.UInt64.to_int64 resolved_addr)
            else if attr.Dwarf.DIE.attr = Dwarf.DW_AT_decl_file then
              (* Resolve file index to filename *)
              match
                resolve_file_index buffer object_format stmt_list_offset u
              with
              | Some filename -> Printf.sprintf "(\"%s\")" filename
              | None ->
                  Printf.sprintf "(0x%08x)"
                    (Unsigned.UInt64.to_int64 u |> Int64.to_int)
            else if attr.Dwarf.DIE.attr = Dwarf.DW_AT_decl_line then
              (* Format byte_size as simple value *)
              Printf.sprintf "(%i)" (Unsigned.UInt64.to_int64 u |> Int64.to_int)
            else if attr.Dwarf.DIE.attr = Dwarf.DW_AT_byte_size then
              (* Format byte_size as simple hex byte value *)
              Printf.sprintf "(0x%02x)"
                (Unsigned.UInt64.to_int64 u |> Int64.to_int)
            else
              Printf.sprintf "(0x%08x)"
                (Unsigned.UInt64.to_int64 u |> Int64.to_int)
        | Dwarf.DIE.SData i -> Printf.sprintf "(%Ld)" i
        | Dwarf.DIE.Address a ->
            let resolved_addr =
              resolve_address_attribute buffer die attr.Dwarf.DIE.attr a
                cu_addr_base
            in
            Printf.sprintf "(0x%016Lx)" (Unsigned.UInt64.to_int64 resolved_addr)
        | Dwarf.DIE.Flag b -> if b then "(true)" else "(false)"
        | Dwarf.DIE.Reference r ->
            let offset_hex =
              Printf.sprintf "0x%08x"
                (Unsigned.UInt64.to_int64 r |> Int64.to_int)
            in
            if attr.Dwarf.DIE.attr = Dwarf.DW_AT_type then
              (* Resolve type reference and get name *)
              match
                resolve_type_reference buffer abbrev_table debug_info_offset r
              with
              | Some type_name ->
                  Printf.sprintf "(%s \"%s\")" offset_hex type_name
              | None -> Printf.sprintf "(%s)" offset_hex
            else Printf.sprintf "(%s)" offset_hex
        | Dwarf.DIE.Block b ->
            (* Special handling for DW_AT_frame_base - decode DWARF expression *)
            if attr.Dwarf.DIE.attr = Dwarf.DW_AT_frame_base then
              format_dwarf_expression_block b
            else Printf.sprintf "(<%d bytes>)" (String.length b)
        | Dwarf.DIE.Language lang ->
            Printf.sprintf "(%s)" (Dwarf.string_of_dwarf_language lang)
        | Dwarf.DIE.Encoding enc ->
            Printf.sprintf "(%s)" (Dwarf.string_of_base_type enc)
      in
      Printf.printf "%s%s\t%s\n" attr_spaces attr_name attr_value)
    die.Dwarf.DIE.attributes;

  (* Print children *)
  Seq.iter
    (fun child ->
      print_die child (depth + 1) buffer object_format stmt_list_offset
        cu_addr_base debug_info_offset abbrev_table)
    die.Dwarf.DIE.children

let dump_debug_info filename =
  handle_dwarf_errors (fun () ->
      let actual_filename, is_dsym, buffer, format_str, object_format =
        init_dwarf_context filename
      in
      Printf.printf "%s:\tfile format %s\n\n" actual_filename format_str;
      Printf.printf ".debug_info contents:\n";

      (* Try to find the debug_info section *)
      let debug_info_section_name =
        Dwarf.object_format_to_section_name object_format Dwarf.Debug_info
      in
      match find_debug_section buffer debug_info_section_name with
      | None -> handle_section_not_found "__debug_info section" filename is_dsym
      | Some (debug_info_offset, _size) ->
          (* Create DWARF context and parse compile units *)
          let dwarf = Dwarf.create buffer in
          let debug_info_offset_int =
            Unsigned.UInt32.to_int debug_info_offset
          in

          let compile_units = Dwarf.parse_compile_units dwarf in

          (* Process each compile unit *)
          Seq.iter
            (fun unit ->
              let span = Dwarf.CompileUnit.data unit in
              let header = Dwarf.CompileUnit.header unit in

              (* Calculate the absolute offset of this compile unit within the debug_info section *)
              let unit_offset_in_section = Unsigned.UInt64.to_int span.start in

              (* TODO We should already return the size/length in 'span' *)
              (* Extract values from already-parsed header data *)
              let unit_length = Unsigned.UInt32.to_int header.unit_length in
              let unit_type = Dwarf.unit_type_of_u8 header.unit_type in
              let next_unit_offset = unit_offset_in_section + unit_length + 4 in
              (* +4 for length field *)

              Printf.printf
                "0x%08x: Compile Unit: length = 0x%08x, format = DWARF32, \
                 version = 0x%04x, unit_type = %s, abbr_offset = 0x%04x, \
                 addr_size = 0x%02x (next unit at 0x%08x)\n"
                unit_offset_in_section unit_length
                (Unsigned.UInt16.to_int header.version)
                (Dwarf.string_of_unit_type unit_type)
                (Unsigned.UInt32.to_int header.debug_abbrev_offset)
                (Unsigned.UInt8.to_int header.address_size)
                next_unit_offset;

              (* Get the abbreviation table for this compilation unit *)
              let abbrev_offset =
                Unsigned.UInt64.of_uint32 header.debug_abbrev_offset
              in
              let _, abbrev_table =
                Dwarf.get_abbrev_table dwarf abbrev_offset
              in

              (* Get the root DIE for this compilation unit *)
              match Dwarf.CompileUnit.root_die unit abbrev_table buffer with
              | None ->
                  Printf.printf
                    "  No root DIE found for this compilation unit\n"
              | Some root_die ->
                  (* Extract DW_AT_stmt_list offset for file index resolution *)
                  let stmt_list_offset =
                    match
                      Dwarf.DIE.find_attribute root_die Dwarf.DW_AT_stmt_list
                    with
                    | Some (Dwarf.DIE.UData offset) -> offset
                    | _ -> Unsigned.UInt64.zero
                  in
                  (* Extract DW_AT_addr_base for address resolution *)
                  let cu_addr_base =
                    match
                      Dwarf.DIE.find_attribute root_die Dwarf.DW_AT_addr_base
                    with
                    | Some (Dwarf.DIE.UData addr_base) -> Some addr_base
                    | _ -> None
                  in
                  print_die root_die 0 buffer object_format stmt_list_offset
                    cu_addr_base debug_info_offset_int abbrev_table;
                  (* Add NULL entry at the end of the compilation unit *)
                  (* TODO Consider exposing the NULL entry in the Sequence *)
                  let null_offset = next_unit_offset - 1 in
                  Printf.printf "\n0x%08x:   NULL\n" null_offset)
              (* +12 for DWARF 5 CU header size *)
            compile_units)

let dump_all filename =
  Printf.printf "Dumping all debug information from: %s\n" filename;
  dump_debug_line filename

let dump_debug_names filename =
  handle_dwarf_errors (fun () ->
      let actual_filename, is_dsym, buffer, format_str, object_format =
        init_dwarf_context filename
      in
      Printf.printf "%s:\tfile format %s\n\n" actual_filename format_str;
      Printf.printf ".debug_names contents:\n";

      (* Try to find the debug_names section *)
      let debug_names_section_name =
        Dwarf.object_format_to_section_name object_format Dwarf.Debug_names
      in
      match find_debug_section buffer debug_names_section_name with
      | None ->
          handle_section_not_found debug_names_section_name filename is_dsym
      | Some (section_offset, _section_size) ->
          (* Create cursor at the debug_names section offset *)
          let cursor = create_section_cursor buffer section_offset in

          (* Parse the debug_names section *)
          let debug_names =
            Dwarf.DebugNames.parse_debug_names_section cursor buffer
          in

          (* Format output to match system dwarfdump *)
          Printf.printf "Name Index @ 0x0 {\n";
          Printf.printf "  Header {\n";
          Printf.printf "    Length: 0x%X\n"
            (Unsigned.UInt32.to_int debug_names.header.unit_length);
          Printf.printf "    Format: DWARF32\n";
          Printf.printf "    Version: %d\n"
            (Unsigned.UInt16.to_int debug_names.header.version);
          Printf.printf "    CU count: %d\n"
            (Unsigned.UInt32.to_int debug_names.header.comp_unit_count);
          Printf.printf "    Local TU count: %d\n"
            (Unsigned.UInt32.to_int debug_names.header.local_type_unit_count);
          Printf.printf "    Foreign TU count: %d\n"
            (Unsigned.UInt32.to_int debug_names.header.foreign_type_unit_count);
          Printf.printf "    Bucket count: %d\n"
            (Unsigned.UInt32.to_int debug_names.header.bucket_count);
          Printf.printf "    Name count: %d\n"
            (Unsigned.UInt32.to_int debug_names.header.name_count);
          Printf.printf "    Abbreviations table size: 0x%X\n"
            (Unsigned.UInt32.to_int debug_names.header.abbrev_table_size);
          Printf.printf "    Augmentation: '%s'\n"
            debug_names.header.augmentation_string;
          Printf.printf "  }\n";

          (* Print compilation unit offsets *)
          Printf.printf "  Compilation Unit offsets [\n";
          Array.iteri
            (fun i offset ->
              Printf.printf "    CU[%d]: 0x%08X\n" i
                (Unsigned.UInt32.to_int offset))
            debug_names.comp_unit_offsets;
          Printf.printf "  ]\n";

          (* Print abbreviations table using parsed data *)
          Printf.printf "  Abbreviations [\n";
          List.iter
            (fun abbrev ->
              let code = Unsigned.UInt64.to_int abbrev.Dwarf.DebugNames.code in
              let tag_str =
                Dwarf.string_of_abbreviation_tag
                  (Dwarf.uint64_of_abbreviation_tag abbrev.tag)
              in
              Printf.printf "    Abbreviation 0x%x {\n" code;
              Printf.printf "      Tag: %s\n" tag_str;
              List.iter
                (fun (idx_attr, form) ->
                  let idx_str = Dwarf.string_of_name_index_attribute idx_attr in
                  let form_str =
                    Dwarf.string_of_attribute_form_encoding_variant form
                  in
                  Printf.printf "      %s: %s\n" idx_str form_str)
                abbrev.attributes;
              Printf.printf "    }\n")
            debug_names.abbreviation_table;
          Printf.printf "  ]\n";

          (* Create bucket to names mapping according to DWARF 5 spec *)
          let bucket_to_names =
            Array.mapi
              (fun bucket_idx bucket_entry ->
                let bucket_start = Unsigned.UInt32.to_int bucket_entry in
                if bucket_start = 0 then [] (* Empty bucket *)
                else
                  (* Collect all names that hash to this bucket *)
                  let rec collect_names acc name_idx =
                    if name_idx >= Array.length debug_names.hash_table then acc
                    else
                      let hash = debug_names.hash_table.(name_idx) in
                      let computed_bucket =
                        Unsigned.UInt32.to_int hash
                        mod Array.length debug_names.buckets
                      in
                      if computed_bucket = bucket_idx then
                        collect_names (name_idx :: acc) (name_idx + 1)
                      else collect_names acc (name_idx + 1)
                  in
                  List.rev (collect_names [] 0)
                (* Check all names, bucket entries are 1-based but name indices are 0-based *))
              debug_names.buckets
            |> Array.to_list
          in

          (* Print buckets and entries *)
          List.iteri
            (fun bucket_idx name_indices ->
              Printf.printf "  Bucket %d [\n" bucket_idx;
              if name_indices = [] then Printf.printf "    EMPTY\n"
              else
                List.iter
                  (fun name_idx ->
                    if
                      name_idx < Array.length debug_names.name_table
                      && name_idx < Array.length debug_names.hash_table
                    then (
                      let name_entry = debug_names.name_table.(name_idx) in
                      let hash =
                        Unsigned.UInt32.to_int debug_names.hash_table.(name_idx)
                      in
                      let str_offset =
                        Unsigned.UInt32.to_int name_entry.offset
                      in
                      (* Try to resolve the name from debug_str section *)
                      let name =
                        let debug_str_section_name =
                          Dwarf.object_format_to_section_name object_format
                            Dwarf.Debug_str
                        in
                        match
                          find_debug_section buffer debug_str_section_name
                        with
                        | Some (debug_str_offset, _) -> (
                            try
                              let cursor =
                                Object.Buffer.cursor buffer
                                  ~at:
                                    (Unsigned.UInt32.to_int debug_str_offset
                                    + str_offset)
                              in
                              read_null_terminated_string cursor
                            with _ -> name_entry.value)
                        | None -> name_entry.value
                      in

                      (* Parse entries for this name *)
                      let entries =
                        Dwarf.DebugNames.parse_all_entries_for_name buffer
                          debug_names
                          (Unsigned.UInt32.to_int section_offset)
                          name_idx
                      in

                      (* Helper function to format parent information *)
                      let format_parent_info parent_offset_opt _has_parent_flag
                          =
                        match parent_offset_opt with
                        | Some parent_offset ->
                            (* Calculate parent entry address using entry pool offset calculation *)
                            let entry_pool_relative_offset =
                              Dwarf.DebugNames.calculate_entry_pool_offset
                                debug_names.header
                            in
                            let parent_entry_addr =
                              entry_pool_relative_offset + parent_offset
                            in
                            Printf.sprintf "Entry @ 0x%x" parent_entry_addr
                        | None -> "<parent not indexed>"
                      in

                      (* Print name header *)
                      Printf.printf "    Name %d {\n" (name_idx + 1);
                      Printf.printf "      Hash: 0x%X\n" hash;
                      Printf.printf "      String: 0x%08x \"%s\"\n" str_offset
                        name;

                      (* Print all entries for this name *)
                      List.iter
                        (fun ( entry_addr,
                               die_offset,
                               tag_str,
                               abbrev_id,
                               parent_offset_opt,
                               has_parent_flag ) ->
                          let parent_info_str =
                            format_parent_info parent_offset_opt has_parent_flag
                          in

                          Printf.printf "      Entry @ 0x%x {\n" entry_addr;
                          Printf.printf "        Abbrev: %s\n" abbrev_id;
                          Printf.printf "        Tag: %s\n" tag_str;
                          Printf.printf "        %s: 0x%08x\n"
                            (Dwarf.string_of_name_index_attribute
                               Dwarf.DW_IDX_die_offset)
                            die_offset;
                          Printf.printf "        %s: %s\n"
                            (Dwarf.string_of_name_index_attribute
                               Dwarf.DW_IDX_parent)
                            parent_info_str;
                          Printf.printf "      }\n")
                        entries;

                      Printf.printf "    }\n"))
                  name_indices;
              Printf.printf "  ]\n")
            bucket_to_names;

          Printf.printf "}\n")

let dump_debug_abbrev filename =
  handle_dwarf_errors (fun () ->
      let actual_filename, is_dsym, buffer, format_str, object_format =
        init_dwarf_context filename
      in
      Printf.printf "%s:\tfile format %s\n\n" actual_filename format_str;
      Printf.printf ".debug_abbrev contents:\n";

      (* Try to find the debug_abbrev section *)
      let debug_abbrev_section_name =
        Dwarf.object_format_to_section_name object_format Dwarf.Debug_abbrev
      in
      match find_debug_section buffer debug_abbrev_section_name with
      | None ->
          handle_section_not_found "__debug_abbrev section" filename is_dsym
      | Some (_offset, _size) ->
          (* System dwarfdump shows offset 0x00000000 for the start of the section *)
          Printf.printf "Abbrev table for offset: 0x%08x\n" 0;

          (* Create DWARF context and parse abbreviation table *)
          let dwarf = Dwarf.create buffer in
          let _dwarf, abbrev_table =
            Dwarf.get_abbrev_table dwarf (Unsigned.UInt64.of_int 0)
          in

          (* Convert abbreviation table to sorted list for consistent output *)
          let abbrevs =
            Hashtbl.fold
              (fun code abbrev acc -> (code, abbrev) :: acc)
              abbrev_table []
          in
          let sorted_abbrevs =
            List.sort
              (fun (c1, _) (c2, _) -> Unsigned.UInt64.compare c1 c2)
              abbrevs
          in

          (* Output each abbreviation *)
          List.iter
            (fun (code, abbrev) ->
              Printf.printf "[%d] %s\t%s\n"
                (Unsigned.UInt64.to_int code)
                (Dwarf.string_of_abbreviation_tag abbrev.Dwarf.tag)
                (if abbrev.Dwarf.has_children then "DW_CHILDREN_yes"
                 else "DW_CHILDREN_no");

              (* Print attributes *)
              List.iter
                (fun attr_spec ->
                  Printf.printf "\t%s\t%s\n"
                    (Dwarf.string_of_attribute_code attr_spec.Dwarf.attr)
                    (Dwarf.string_of_attribute_form_encoding
                       attr_spec.Dwarf.form))
                abbrev.Dwarf.attr_specs;

              Printf.printf "\n")
            sorted_abbrevs)

let dump_debug_str_offsets filename =
  handle_dwarf_errors (fun () ->
      let actual_filename, is_dsym, buffer, format_str, object_format =
        init_dwarf_context filename
      in
      Printf.printf "%s:\tfile format %s\n\n" actual_filename format_str;
      Printf.printf ".debug_str_offsets contents:\n";

      (* Try to find the debug_str_offs section *)
      let debug_str_offsets_section_name =
        Dwarf.object_format_to_section_name object_format Dwarf.Debug_str_offs
      in
      match find_debug_section buffer debug_str_offsets_section_name with
      | None ->
          handle_section_not_found "__debug_str_offs section" filename is_dsym
      | Some (section_offset, _section_size) ->
          (* Use the new parsing functions from DWARF library *)
          let parsed_str_offsets =
            Dwarf.DebugStrOffsets.parse buffer section_offset
          in

          (* Print header information *)
          let header = parsed_str_offsets.header in
          Printf.printf
            "0x%08x: Contribution size = %d, Format = %s, Version = %d\n" 0
            (Unsigned.UInt32.to_int header.unit_length)
            "DWARF32"
            (Unsigned.UInt16.to_int header.version);

          (* Print each offset with its resolved string *)
          let header_size = 8 in
          let offset_size = 4 in
          Array.iteri
            (fun i offset_entry ->
              let relative_pos = header_size + (i * offset_size) in
              let offset_value =
                Unsigned.UInt32.to_int offset_entry.Dwarf.DebugStrOffsets.offset
              in
              let string_part =
                match offset_entry.Dwarf.DebugStrOffsets.resolved_string with
                | Some s -> Printf.sprintf " \"%s\"" s
                | None -> ""
              in
              Printf.printf "0x%08x: %08x%s\n" relative_pos offset_value
                string_part)
            parsed_str_offsets.offsets)

let dump_debug_str filename =
  handle_dwarf_errors (fun () ->
      let actual_filename, is_dsym, buffer, format_str, _object_format =
        init_dwarf_context filename
      in
      Printf.printf "%s:\tfile format %s\n\n" actual_filename format_str;
      Printf.printf ".debug_str contents:\n";

      (* Use DebugStr.parse to get string table *)
      match Dwarf.DebugStr.parse buffer with
      | None -> handle_section_not_found "__debug_str section" filename is_dsym
      | Some str_table ->
          (* Output each string entry in dwarfdump format *)
          Array.iter (fun (entry : Dwarf.DebugStr.string_entry) ->
            if entry.length > 0 then
              Printf.printf "0x%08x: \"%s\"\n" entry.offset entry.content
            else
              Printf.printf "0x%08x: \"\"\n" entry.offset
          ) str_table.entries)

let dump_debug_line_str filename =
  handle_dwarf_errors (fun () ->
      let actual_filename, is_dsym, buffer, format_str, _object_format =
        init_dwarf_context filename
      in
      Printf.printf "%s:\tfile format %s\n\n" actual_filename format_str;
      Printf.printf ".debug_line_str contents:\n";

      (* Use DebugLineStr.parse to get line string table *)
      match Dwarf.DebugLineStr.parse buffer with
      | None ->
          handle_section_not_found "__debug_line_str section" filename is_dsym
      | Some line_str_table ->
          (* Output each string entry in dwarfdump format *)
          Array.iter (fun (entry : Dwarf.DebugLineStr.string_entry) ->
            if entry.length > 0 then
              Printf.printf "0x%08x: \"%s\"\n" entry.offset entry.content
            else
              Printf.printf "0x%08x: \"\"\n" entry.offset
          ) line_str_table.entries)

let dump_debug_addr filename =
  handle_dwarf_errors (fun () ->
      let actual_filename, is_dsym, buffer, format_str, object_format =
        init_dwarf_context filename
      in
      Printf.printf "%s:\tfile format %s\n\n" actual_filename format_str;
      Printf.printf ".debug_addr contents:\n";

      let debug_addr_section_name =
        Dwarf.object_format_to_section_name object_format Dwarf.Debug_addr
      in
      match find_debug_section buffer debug_addr_section_name with
      | None ->
          handle_section_not_found debug_addr_section_name filename is_dsym
      | Some (section_offset, _section_size) ->
          (* Parse the debug_addr section *)
          let parsed_addr = Dwarf.DebugAddr.parse buffer section_offset in

          (* Print header information *)
          let header = parsed_addr.header in
          Printf.printf
            "Address table header: length = 0x%08lx, format = DWARF32, version \
             = 0x%04x, addr_size = 0x%02x, seg_size = 0x%02x\n"
            (Unsigned.UInt32.to_int32 header.unit_length)
            (Unsigned.UInt16.to_int header.version)
            (Unsigned.UInt8.to_int header.address_size)
            (Unsigned.UInt8.to_int header.segment_selector_size);

          (* Print entries *)
          Printf.printf "Addrs: [\n";
          Array.iter
            (fun entry ->
              Printf.printf "0x%016Lx\n"
                (Unsigned.UInt64.to_int64 entry.Dwarf.DebugAddr.address))
            parsed_addr.entries;
          Printf.printf "]\n")

let dump_debug_aranges filename =
  handle_dwarf_errors (fun () ->
      let actual_filename, is_dsym, buffer, format_str, object_format =
        init_dwarf_context filename
      in
      Printf.printf "%s:\tfile format %s\n\n" actual_filename format_str;
      Printf.printf ".debug_aranges contents:\n";

      (* Try to find the debug_aranges section *)
      let debug_aranges_section_name =
        Dwarf.object_format_to_section_name object_format Dwarf.Debug_aranges
      in
      match find_debug_section buffer debug_aranges_section_name with
      | None ->
          handle_section_not_found debug_aranges_section_name filename is_dsym
      | Some (section_offset, _section_size) ->
          (* Parse the debug_aranges section *)
          let aranges_set = Dwarf.DebugAranges.parse buffer section_offset in
          let header = aranges_set.Dwarf.DebugAranges.header in

          (* Print header information matching system dwarfdump format *)
          Printf.printf
            "Address Range Header: length = 0x%08lx, format = DWARF32, version \
             = 0x%04x, cu_offset = 0x%08lx, addr_size = 0x%02x, seg_size = \
             0x%02x\n"
            (Unsigned.UInt32.to_int32 header.Dwarf.DebugAranges.unit_length)
            (Unsigned.UInt16.to_int header.Dwarf.DebugAranges.version)
            (Unsigned.UInt32.to_int32
               header.Dwarf.DebugAranges.debug_info_offset)
            (Unsigned.UInt8.to_int header.Dwarf.DebugAranges.address_size)
            (Unsigned.UInt8.to_int header.Dwarf.DebugAranges.segment_size);

          (* Print address ranges *)
          List.iter
            (fun range ->
              let start_addr =
                Unsigned.UInt64.to_int64 range.Dwarf.DebugAranges.start_address
              in
              let end_addr =
                Unsigned.UInt64.to_int64
                  (Unsigned.UInt64.add range.Dwarf.DebugAranges.start_address
                     range.Dwarf.DebugAranges.length)
              in
              Printf.printf "[0x%016Lx, 0x%016Lx)\n" start_addr end_addr)
            aranges_set.Dwarf.DebugAranges.ranges)

let dump_debug_macro filename =
  handle_dwarf_errors (fun () ->
      let actual_filename, is_dsym, buffer, format_str, object_format =
        init_dwarf_context filename
      in
      Printf.printf "%s:\tfile format %s\n\n" actual_filename format_str;
      Printf.printf ".debug_macro contents:\n";

      (* Try to find the debug_macro section *)
      let debug_macro_section_name =
        Dwarf.object_format_to_section_name object_format Dwarf.Debug_macro
      in
      match find_debug_section buffer debug_macro_section_name with
      | None ->
          handle_section_not_found debug_macro_section_name filename is_dsym
      | Some (section_offset, section_size) ->
          (* Create cursor at the debug_macro section offset *)
          let cursor = create_section_cursor buffer section_offset in

          (* Parse the debug_macro section *)
          let section_size_int = Unsigned.UInt64.to_int section_size in
          let macro_section =
            Dwarf.parse_debug_macro_section cursor section_size_int
          in

          Printf.printf
            "Debug macro section parsed successfully with %d units\n"
            (List.length macro_section.units))

let dump_debug_loclists filename =
  handle_dwarf_errors (fun () ->
      let actual_filename, _is_dsym, buffer, format_str, object_format =
        init_dwarf_context filename
      in
      Printf.printf "%s:\tfile format %s\n\n" actual_filename format_str;
      Printf.printf ".debug_loclists contents:\n";

      (* Try to find the debug_loclists section *)
      let debug_loclists_section_name =
        Dwarf.object_format_to_section_name object_format Dwarf.Debug_loclists
      in
      match find_debug_section buffer debug_loclists_section_name with
      | None ->
          (* No debug_loclists section found - this is normal for simple programs.
             Show empty section output to match system dwarfdump behavior *)
          ()
      | Some (section_offset, _section_size) ->
          (* Parse the debug_loclists section *)
          let loclists_section =
            Dwarf.DebugLoclists.parse buffer section_offset
          in

          (* Check if section is empty (indicated by zero unit_length) *)
          if
            Unsigned.UInt32.equal loclists_section.header.unit_length
              Unsigned.UInt32.zero
          then
            (* Empty section - no output needed, this matches system dwarfdump *)
            ()
          else
            (* Format output similar to other debug sections *)
            Printf.printf
              "Location lists header: length = 0x%08lx, format = DWARF32, \
               version = 0x%04x, addr_size = 0x%02x, seg_size = 0x%02x, \
               offset_entry_count = 0x%08lx\n"
              (Unsigned.UInt32.to_int32 loclists_section.header.unit_length)
              (Unsigned.UInt16.to_int loclists_section.header.version)
              (Unsigned.UInt8.to_int loclists_section.header.address_size)
              (Unsigned.UInt8.to_int loclists_section.header.segment_size)
              (Unsigned.UInt32.to_int32
                 loclists_section.header.offset_entry_count))

(* Command line interface *)
(* TODO Use platform agnostic names for sections. *)
let filename =
  let doc = "Binary file to analyze for DWARF debug information" in
  Cmdliner.Arg.(required & pos 0 (some file) None & info [] ~docv:"FILE" ~doc)

let debug_line_flag =
  let doc = "Dump debug line information (.debug_line section)" in
  Cmdliner.Arg.(value & flag & info [ "debug-line" ] ~doc)

let debug_info_flag =
  let doc = "Dump debug info information (.debug_info section)" in
  Cmdliner.Arg.(value & flag & info [ "debug-info" ] ~doc)

let debug_names_flag =
  let doc = "Dump debug names information (.debug_names section)" in
  Cmdliner.Arg.(value & flag & info [ "debug-names" ] ~doc)

let debug_abbrev_flag =
  let doc = "Dump debug abbreviation information (.debug_abbrev section)" in
  Cmdliner.Arg.(value & flag & info [ "debug-abbrev" ] ~doc)

let debug_str_offsets_flag =
  let doc = "Dump debug string offsets information (.debug_str_offs section)" in
  Cmdliner.Arg.(value & flag & info [ "debug-str-offsets" ] ~doc)

let debug_str_flag =
  let doc = "Dump debug string information (.debug_str section)" in
  Cmdliner.Arg.(value & flag & info [ "debug-str" ] ~doc)

let debug_line_str_flag =
  let doc = "Dump debug line string information (.debug_line_str section)" in
  Cmdliner.Arg.(value & flag & info [ "debug-line-str" ] ~doc)

let debug_addr_flag =
  let doc = "Dump debug address information (.debug_addr section)" in
  Cmdliner.Arg.(value & flag & info [ "debug-addr" ] ~doc)

let debug_aranges_flag =
  let doc = "Dump debug address ranges information (.debug_aranges section)" in
  Cmdliner.Arg.(value & flag & info [ "debug-aranges" ] ~doc)

let debug_macro_flag =
  let doc = "Dump debug macro information (.debug_macro section)" in
  Cmdliner.Arg.(value & flag & info [ "debug-macro" ] ~doc)

let debug_loclists_flag =
  let doc = "Dump debug location lists information (.debug_loclists section)" in
  Cmdliner.Arg.(value & flag & info [ "debug-loclists" ] ~doc)

let all_flag =
  let doc = "Dump all available debug information" in
  Cmdliner.Arg.(value & flag & info [ "all"; "a" ] ~doc)

(* TODO handle .debug_frame .debug_rnglists

   dwarfdump --show-section-sizes  - Show the sizes of all debug sections, expressed in bytes.
 *)
let dwarfdump_cmd debug_line debug_info debug_names debug_abbrev
    debug_str_offsets debug_str debug_line_str debug_addr debug_aranges
    debug_macro debug_loclists all filename =
  match
    ( debug_line,
      debug_info,
      debug_names,
      debug_abbrev,
      debug_str_offsets,
      debug_str,
      debug_line_str,
      debug_addr,
      debug_aranges,
      debug_macro,
      debug_loclists,
      all )
  with
  | true, _, _, _, _, _, _, _, _, _, _, _ -> dump_debug_line filename
  | _, true, _, _, _, _, _, _, _, _, _, _ -> dump_debug_info filename
  | _, _, true, _, _, _, _, _, _, _, _, _ -> dump_debug_names filename
  | _, _, _, true, _, _, _, _, _, _, _, _ -> dump_debug_abbrev filename
  | _, _, _, _, true, _, _, _, _, _, _, _ -> dump_debug_str_offsets filename
  | _, _, _, _, _, true, _, _, _, _, _, _ -> dump_debug_str filename
  | _, _, _, _, _, _, true, _, _, _, _, _ -> dump_debug_line_str filename
  | _, _, _, _, _, _, _, true, _, _, _, _ -> dump_debug_addr filename
  | _, _, _, _, _, _, _, _, true, _, _, _ -> dump_debug_aranges filename
  | _, _, _, _, _, _, _, _, _, true, _, _ -> dump_debug_macro filename
  | _, _, _, _, _, _, _, _, _, _, true, _ -> dump_debug_loclists filename
  | _, _, _, _, _, _, _, _, _, _, _, true -> dump_all filename
  | ( false,
      false,
      false,
      false,
      false,
      false,
      false,
      false,
      false,
      false,
      false,
      false ) ->
      (* Default behavior - dump debug line information *)
      dump_debug_line filename

let cmd =
  let doc = "A DWARF debugging information dumper" in
  let info = Cmdliner.Cmd.info "dwarfdump" ~doc in
  Cmdliner.Cmd.v info
    Cmdliner.Term.(
      const dwarfdump_cmd $ debug_line_flag $ debug_info_flag $ debug_names_flag
      $ debug_abbrev_flag $ debug_str_offsets_flag $ debug_str_flag
      $ debug_line_str_flag $ debug_addr_flag $ debug_aranges_flag
      $ debug_macro_flag $ debug_loclists_flag $ all_flag $ filename)

let () = exit (Cmdliner.Cmd.eval cmd)
