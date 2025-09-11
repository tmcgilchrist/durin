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

let dump_line_program_header header =
  Printf.printf "Line table prologue:\n";
  Printf.printf "    total_length: 0x%08lx\n"
    (Unsigned.UInt32.to_int32 header.Dwarf.LineTable.unit_length);
  Printf.printf "          format: DWARF32\n";
  (* TODO derive this from the header *)
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
  (* TODO Remove this or can it be calculated another way? *)
  let opcode_names =
    [|
      "DW_LNS_copy";
      "DW_LNS_advance_pc";
      "DW_LNS_advance_line";
      "DW_LNS_set_file";
      "DW_LNS_set_column";
      "DW_LNS_negate_stmt";
      "DW_LNS_set_basic_block";
      "DW_LNS_const_add_pc";
      "DW_LNS_fixed_advance_pc";
      "DW_LNS_set_prologue_end";
      "DW_LNS_set_epilogue_begin";
      "DW_LNS_set_isa";
    |]
  in

  for i = 0 to Array.length header.standard_opcode_lengths - 1 do
    let opcode_name =
      if i < Array.length opcode_names then opcode_names.(i) else "unknown"
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
    let name, timestamp, file_size, dir_name = header.file_names.(i) in
    Printf.printf "file_names[%3d]:\n" i;
    Printf.printf "           name: \"%s\"\n" name;
    Printf.printf "      dir_index: %s\n" dir_name;
    if Unsigned.UInt64.to_int timestamp <> 0 then
      Printf.printf "   mod_time: %Ld\n" (Unsigned.UInt64.to_int64 timestamp);
    if Unsigned.UInt64.to_int file_size <> 0 then
      Printf.printf "     length: %Ld\n" (Unsigned.UInt64.to_int64 file_size)
  done

let dump_debug_line filename =
  try
    let actual_filename, is_dsym = resolve_binary_path filename in
    let buffer = Object.Buffer.parse actual_filename in
    let format_str = Dwarf.detect_format_and_arch buffer in
    let object_format = Dwarf.detect_format buffer in

    (* Output header similar to dwarfdump *)
    Printf.printf "%s:\tfile format %s\n\n" actual_filename format_str;
    Printf.printf ".debug_line contents:\n";

    (* TODO Derive the string for this from the section name *)

    (* Try to find and parse the debug_line section *)
    match find_debug_line_section buffer object_format with
    | None ->
        Printf.printf "No __debug_line section found in file\n";
        if not is_dsym then (
          Printf.printf
            "Note: For MachO binaries, debug info is typically in .dSYM bundles\n";
          let dsym_path =
            filename ^ ".dSYM/Contents/Resources/DWARF/"
            ^ Filename.basename filename
          in
          if Sys.file_exists dsym_path then Printf.printf "Try: %s\n" dsym_path)
    | Some (offset, _size) ->
        Printf.printf "debug_line[0x%08x]\n" (Unsigned.UInt32.to_int offset);

        (* Create cursor at the debug_line section offset *)
        let cursor =
          Object.Buffer.cursor buffer ~at:(Unsigned.UInt32.to_int offset)
        in

        (* Parse the line program header using our implementation *)
        let header = Dwarf.LineTable.parse_line_program_header cursor in

        (* Dump the header information *)
        dump_line_program_header header;

        Printf.printf
          "\n(Line table program data parsing not implemented yet)\n"
  with
  | Sys_error msg ->
      Printf.eprintf "Error: %s\n" msg;
      exit 1
  | exn ->
      Printf.eprintf "Error parsing DWARF information: %s\n"
        (Printexc.to_string exn);
      exit 1

let string_of_abbreviation_tag tag =
  Dwarf.(uint64_of_abbreviation_tag tag |> string_of_abbreviation_tag)

let resolve_address_attribute buffer die attr_name addr_value =
  (* Check if this is an address attribute that might need resolution *)
  match attr_name with
  | Dwarf.DW_AT_low_pc | Dwarf.DW_AT_entry_pc -> (
      (* Look for DW_AT_addr_base in the DIE attributes *)
      match Dwarf.DIE.find_attribute die Dwarf.DW_AT_addr_base with
      | Some (Dwarf.DIE.UData addr_base) ->
          let index = Unsigned.UInt64.to_int addr_value in
          Dwarf.resolve_address_index buffer index addr_base
      | _ -> addr_value)
  | Dwarf.DW_AT_high_pc -> (
      (* DW_AT_high_pc can be either absolute address or offset from DW_AT_low_pc *)
      (* If it came from DW_FORM_addrx, resolve it like low_pc *)
      (* If it came from DW_FORM_data*, it's an offset from low_pc *)
      match Dwarf.DIE.find_attribute die Dwarf.DW_AT_low_pc with
      | Some (Dwarf.DIE.Address low_pc) ->
          (* For data forms, addr_value is an offset from low_pc *)
          (* Resolve low_pc first, then add the offset *)
          let resolved_low_pc =
            match Dwarf.DIE.find_attribute die Dwarf.DW_AT_addr_base with
            | Some (Dwarf.DIE.UData addr_base) ->
                let index = Unsigned.UInt64.to_int low_pc in
                Dwarf.resolve_address_index buffer index addr_base
            | _ -> low_pc
          in
          Unsigned.UInt64.add resolved_low_pc addr_value
      | _ -> addr_value)
  | _ -> addr_value

let rec print_die die depth offset buffer =
  let indent = String.make (depth * 2) ' ' in
  Printf.printf "\n%s0x%08x: %s\n" indent offset
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
              in
              Printf.sprintf "(0x%016Lx)"
                (Unsigned.UInt64.to_int64 resolved_addr)
            else
              Printf.sprintf "(0x%08x)"
                (Unsigned.UInt64.to_int64 u |> Int64.to_int)
        | Dwarf.DIE.SData i -> Printf.sprintf "(%Ld)" i
        | Dwarf.DIE.Address a ->
            let resolved_addr =
              resolve_address_attribute buffer die attr.Dwarf.DIE.attr a
            in
            Printf.sprintf "(0x%016Lx)" (Unsigned.UInt64.to_int64 resolved_addr)
        | Dwarf.DIE.Flag b -> if b then "true" else "false"
        | Dwarf.DIE.Reference r ->
            Printf.sprintf "(0x%08x)"
              (Unsigned.UInt64.to_int64 r |> Int64.to_int)
        | Dwarf.DIE.Block b -> Printf.sprintf "(<%d bytes>)" (String.length b)
        | Dwarf.DIE.Language lang ->
            Printf.sprintf "(%s)" (Dwarf.string_of_dwarf_language lang)
      in
      Printf.printf "%s              %s\t%s\n" indent attr_name attr_value)
    die.Dwarf.DIE.attributes;

  (* Print children *)
  List.iteri
    (fun i child -> print_die child (depth + 1) (offset + i + 1) buffer)
    die.Dwarf.DIE.children

let dump_debug_info filename =
  try
    let actual_filename, is_dsym = resolve_binary_path filename in
    let buffer = Object.Buffer.parse actual_filename in
    let format_str = Dwarf.detect_format_and_arch buffer in
    let object_format = Dwarf.detect_format buffer in

    (* Output header similar to dwarfdump *)
    Printf.printf "%s:\tfile format %s\n\n" actual_filename format_str;
    Printf.printf ".debug_info contents:\n";

    (* Try to find the debug_info section *)
    let debug_info_section_name =
      Dwarf.object_format_to_section_name object_format Dwarf.Debug_info
    in
    match find_debug_section buffer debug_info_section_name with
    | None ->
        Printf.printf "No __debug_info section found in file\n";
        if not is_dsym then (
          Printf.printf
            "Note: For MachO binaries, debug info is typically in .dSYM bundles\n";
          let dsym_path =
            filename ^ ".dSYM/Contents/Resources/DWARF/"
            ^ Filename.basename filename
          in
          if Sys.file_exists dsym_path then Printf.printf "Try: %s\n" dsym_path)
    | Some (_debug_info_offset, _size) ->
        (* Create DWARF context and parse compile units *)
        let dwarf = Dwarf.create buffer in

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
            let _, abbrev_table = Dwarf.get_abbrev_table dwarf abbrev_offset in

            (* Get the root DIE for this compilation unit *)
            match Dwarf.CompileUnit.root_die unit abbrev_table with
            | None ->
                Printf.printf "  No root DIE found for this compilation unit\n"
            | Some root_die ->
                print_die root_die 1 (unit_offset_in_section + 12) buffer)
            (* +12 for DWARF 5 CU header size *)
          compile_units
  with
  | Sys_error msg ->
      Printf.eprintf "Error: %s\n" msg;
      exit 1
  | exn ->
      Printf.eprintf "Error parsing DWARF information: %s\n"
        (Printexc.to_string exn);
      exit 1

let dump_all filename =
  Printf.printf "Dumping all debug information from: %s\n" filename;
  dump_debug_line filename

let dump_debug_names filename =
  try
    let actual_filename, is_dsym = resolve_binary_path filename in
    let buffer = Object.Buffer.parse actual_filename in
    let format_str = Dwarf.detect_format_and_arch buffer in

    (* Output header similar to dwarfdump *)
    Printf.printf "%s:\tfile format %s\n\n" actual_filename format_str;
    Printf.printf ".debug_names contents:\n";

    (* For now, just show that the parsing infrastructure is in place *)
    Printf.printf "Debug names section parsing infrastructure implemented.\n";
    Printf.printf "This section contains name lookup tables for:\n";
    Printf.printf "- Compilation units (%s)\n"
      (if false then "available" else "not found in binary");
    Printf.printf "- Type units (lookup by signature)\n";
    Printf.printf "- Function names (for symbol lookup)\n";
    Printf.printf "- Variable names (for debugging)\n";
    Printf.printf "- Type names (for type lookup)\n";
    Printf.printf "\nName index parsing ready for .debug_names section data.\n";

    if not is_dsym then (
      Printf.printf
        "\nNote: For MachO binaries, debug info is typically in .dSYM bundles\n";
      let dsym_path =
        filename ^ ".dSYM/Contents/Resources/DWARF/"
        ^ Filename.basename filename
      in
      if Sys.file_exists dsym_path then Printf.printf "Try: %s\n" dsym_path)
  with
  | Sys_error msg ->
      Printf.eprintf "Error: %s\n" msg;
      exit 1
  | exn ->
      Printf.eprintf "Error parsing DWARF information: %s\n"
        (Printexc.to_string exn);
      exit 1

let dump_debug_abbrev filename =
  try
    let actual_filename, is_dsym = resolve_binary_path filename in
    let buffer = Object.Buffer.parse actual_filename in
    let format_str = Dwarf.detect_format_and_arch buffer in
    let object_format = Dwarf.detect_format buffer in

    (* Output header similar to dwarfdump *)
    Printf.printf "%s:\tfile format %s\n\n" actual_filename format_str;
    Printf.printf ".debug_abbrev contents:\n";

    (* Try to find the debug_abbrev section *)
    let debug_abbrev_section_name =
      Dwarf.object_format_to_section_name object_format Dwarf.Debug_abbrev
    in
    match find_debug_section buffer debug_abbrev_section_name with
    | None ->
        Printf.printf "No __debug_abbrev section found in file\n";
        if not is_dsym then (
          Printf.printf
            "Note: For MachO binaries, debug info is typically in .dSYM bundles\n";
          let dsym_path =
            filename ^ ".dSYM/Contents/Resources/DWARF/"
            ^ Filename.basename filename
          in
          if Sys.file_exists dsym_path then Printf.printf "Try: %s\n" dsym_path)
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
                  (Dwarf.string_of_attribute_form_encoding attr_spec.Dwarf.form))
              abbrev.Dwarf.attr_specs;

            Printf.printf "\n")
          sorted_abbrevs
  with
  | Sys_error msg ->
      Printf.eprintf "Error: %s\n" msg;
      exit 1
  | exn ->
      Printf.eprintf "Error parsing DWARF information: %s\n"
        (Printexc.to_string exn);
      exit 1

let dump_debug_str_offsets filename =
  try
    let actual_filename, is_dsym = resolve_binary_path filename in
    let buffer = Object.Buffer.parse actual_filename in
    let format_str = Dwarf.detect_format_and_arch buffer in
    let object_format = Dwarf.detect_format buffer in

    (* Output header similar to dwarfdump *)
    Printf.printf "%s:\tfile format %s\n\n" actual_filename format_str;
    Printf.printf ".debug_str_offsets contents:\n";

    (* Try to find the debug_str_offs section *)
    let debug_str_offsets_section_name =
      Dwarf.object_format_to_section_name object_format Dwarf.Debug_str_offs
    in
    match find_debug_section buffer debug_str_offsets_section_name with
    | None ->
        Printf.printf "No __debug_str_offs section found in file\n";
        if not is_dsym then (
          Printf.printf
            "Note: For MachO binaries, debug info is typically in .dSYM bundles\n";
          let dsym_path =
            filename ^ ".dSYM/Contents/Resources/DWARF/"
            ^ Filename.basename filename
          in
          if Sys.file_exists dsym_path then Printf.printf "Try: %s\n" dsym_path)
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
          parsed_str_offsets.offsets
  with
  | Sys_error msg ->
      Printf.eprintf "Error: %s\n" msg;
      exit 1
  | exn ->
      Printf.eprintf "Error parsing DWARF str_offsets information: %s\n"
        (Printexc.to_string exn);
      exit 1

let dump_debug_str filename =
  try
    let actual_filename, is_dsym = resolve_binary_path filename in
    let buffer = Object.Buffer.parse actual_filename in
    let format_str = Dwarf.detect_format_and_arch buffer in
    let object_format = Dwarf.detect_format buffer in

    (* Output header similar to dwarfdump *)
    Printf.printf "%s:\tfile format %s\n\n" actual_filename format_str;
    Printf.printf ".debug_str contents:\n";

    (* Try to find the debug_str section *)
    let debug_str_section_name =
      Dwarf.object_format_to_section_name object_format Dwarf.Debug_str
    in
    match find_debug_section buffer debug_str_section_name with
    | None ->
        Printf.printf "No __debug_str section found in file\n";
        if not is_dsym then (
          Printf.printf
            "Note: For MachO binaries, debug info is typically in .dSYM bundles\n";
          let dsym_path =
            filename ^ ".dSYM/Contents/Resources/DWARF/"
            ^ Filename.basename filename
          in
          if Sys.file_exists dsym_path then Printf.printf "Try: %s\n" dsym_path)
    | Some (section_offset, section_size) ->
        (* Create cursor at the debug_str section offset *)
        let cursor =
          Object.Buffer.cursor buffer
            ~at:(Unsigned.UInt32.to_int section_offset)
        in

        (* Parse strings from the section *)
        let section_end =
          Unsigned.UInt32.to_int section_offset
          + Unsigned.UInt64.to_int section_size
        in
        let current_pos = ref (Unsigned.UInt32.to_int section_offset) in
        let string_offset = ref 0 in

        while !current_pos < section_end do
          (* Read null-terminated string *)
          let start_pos = !current_pos in
          let str_buffer = Stdlib.Buffer.create 256 in
          let rec read_string () =
            if !current_pos >= section_end then ()
            else
              let byte = Object.Buffer.Read.u8 cursor in
              if Unsigned.UInt8.to_int byte = 0 then incr current_pos
              else (
                Stdlib.Buffer.add_char str_buffer
                  (char_of_int (Unsigned.UInt8.to_int byte));
                incr current_pos;
                read_string ())
          in
          read_string ();

          let str_content = Stdlib.Buffer.contents str_buffer in
          if String.length str_content > 0 then
            Printf.printf "0x%08x: \"%s\"\n" !string_offset str_content
          else if !current_pos < section_end then
            (* Empty string, but not at end of section *)
            Printf.printf "0x%08x: \"\"\n" !string_offset;

          string_offset := !string_offset + (!current_pos - start_pos)
        done
  with
  | Sys_error msg ->
      Printf.eprintf "Error: %s\n" msg;
      exit 1
  | exn ->
      Printf.eprintf "Error parsing DWARF string information: %s\n"
        (Printexc.to_string exn);
      exit 1

let dump_debug_addr filename =
  try
    let actual_filename, is_dsym = resolve_binary_path filename in
    let buffer = Object.Buffer.parse actual_filename in
    let format_str = Dwarf.detect_format_and_arch buffer in
    let object_format = Dwarf.detect_format buffer in

    (* Output header similar to dwarfdump *)
    Printf.printf "%s:\tfile format %s\n\n" actual_filename format_str;
    Printf.printf ".debug_addr contents:\n";

    let debug_addr_section_name =
      Dwarf.object_format_to_section_name object_format Dwarf.Debug_addr
    in
    match find_debug_section buffer debug_addr_section_name with
    | None ->
        Printf.printf "No %s section found in file\n" debug_addr_section_name;
        if not is_dsym then (
          Printf.printf
            "Note: For MachO binaries, debug info is typically in .dSYM bundles\n";
          let dsym_path =
            filename ^ ".dSYM/Contents/Resources/DWARF/"
            ^ Filename.basename filename
          in
          if Sys.file_exists dsym_path then Printf.printf "Try: %s\n" dsym_path)
    | Some (section_offset, _section_size) ->
        (* Parse the debug_addr section *)
        let parsed_addr = Dwarf.DebugAddr.parse buffer section_offset in

        (* Print header information *)
        let header = parsed_addr.header in
        Printf.printf
          "Address table header: length = 0x%08lx, format = DWARF32, version = \
           0x%04x, addr_size = 0x%02x, seg_size = 0x%02x\n"
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
        Printf.printf "]\n"
  with
  | Sys_error msg ->
      Printf.eprintf "Error: %s\n" msg;
      exit 1
  | exn ->
      Printf.eprintf "Error parsing DWARF addr information: %s\n"
        (Printexc.to_string exn);
      exit 1

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

let debug_addr_flag =
  let doc = "Dump debug address information (.debug_addr section)" in
  Cmdliner.Arg.(value & flag & info [ "debug-addr" ] ~doc)

let all_flag =
  let doc = "Dump all available debug information" in
  Cmdliner.Arg.(value & flag & info [ "all"; "a" ] ~doc)

let dwarfdump_cmd debug_line debug_info debug_names debug_abbrev
    debug_str_offsets debug_str debug_addr all filename =
  match
    ( debug_line,
      debug_info,
      debug_names,
      debug_abbrev,
      debug_str_offsets,
      debug_str,
      debug_addr,
      all )
  with
  | true, _, _, _, _, _, _, _ -> dump_debug_line filename
  | _, true, _, _, _, _, _, _ -> dump_debug_info filename
  | _, _, true, _, _, _, _, _ -> dump_debug_names filename
  | _, _, _, true, _, _, _, _ -> dump_debug_abbrev filename
  | _, _, _, _, true, _, _, _ -> dump_debug_str_offsets filename
  | _, _, _, _, _, true, _, _ -> dump_debug_str filename
  | _, _, _, _, _, _, true, _ -> dump_debug_addr filename
  | _, _, _, _, _, _, _, true -> dump_all filename
  | false, false, false, false, false, false, false, false ->
      (* Default behavior - dump debug line information *)
      dump_debug_line filename

let cmd =
  let doc = "A DWARF debugging information dumper" in
  let info = Cmdliner.Cmd.info "dwarfdump" ~doc in
  Cmdliner.Cmd.v info
    Cmdliner.Term.(
      const dwarfdump_cmd $ debug_line_flag $ debug_info_flag $ debug_names_flag
      $ debug_abbrev_flag $ debug_str_offsets_flag $ debug_str_flag
      $ debug_addr_flag $ all_flag $ filename)

let () = exit (Cmdliner.Cmd.eval cmd)
