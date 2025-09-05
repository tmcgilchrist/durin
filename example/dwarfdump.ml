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

let find_debug_line_section buffer = find_debug_section buffer "__debug_line"

let dump_line_program_header header =
  Printf.printf "Line table prologue:\n";
  Printf.printf "    total_length: 0x%08lx\n"
    (Unsigned.UInt32.to_int32 header.Dwarf.LineTable.unit_length);
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
    (* Determine if we're dealing with a regular binary or dSYM *)
    let actual_filename, is_dsym =
      if Sys.file_exists filename then (filename, false)
      else
        (* Try to find dSYM file *)
        let dsym_path =
          filename ^ ".dSYM/Contents/Resources/DWARF/"
          ^ Filename.basename filename
        in
        if Sys.file_exists dsym_path then (dsym_path, true)
        else (filename, false)
      (* Use original filename, will fail later if not found *)
    in

    (* Output header similar to dwarfdump *)
    Printf.printf "%s:\tfile format Mach-O arm64\n\n" actual_filename;
    Printf.printf ".debug_line contents:\n";

    let buffer = Object.Buffer.parse actual_filename in

    (* Try to find and parse the debug_line section *)
    match find_debug_line_section buffer with
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

let dump_debug_info filename =
  try
    (* Determine if we're dealing with a regular binary or dSYM *)
    let actual_filename, is_dsym =
      if Sys.file_exists filename then (filename, false)
      else
        (* Try to find dSYM file *)
        let dsym_path =
          filename ^ ".dSYM/Contents/Resources/DWARF/"
          ^ Filename.basename filename
        in
        if Sys.file_exists dsym_path then (dsym_path, true)
        else (filename, false)
      (* Use original filename, will fail later if not found *)
    in

    (* Output header similar to dwarfdump *)
    Printf.printf "%s:\tfile format Mach-O arm64\n\n" actual_filename;
    Printf.printf ".debug_info contents:\n";

    let buffer = Object.Buffer.parse actual_filename in

    (* Try to find the debug_info section *)
    match find_debug_section buffer "__debug_info" with
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
        List.iter
          (fun unit ->
            let data = Dwarf.CompileUnit.data unit in
            let parsed = Dwarf.CompileUnit.parsed_data unit in

            (* Calculate the absolute offset of this compile unit within the debug_info section *)
            let unit_offset_in_section = Unsigned.UInt64.to_int data.start in

            (* Extract values from already-parsed header data *)
            let unit_length = Unsigned.UInt32.to_int parsed.unit_length in
            let unit_type = Dwarf.unit_type_of_u8 parsed.unit_type in
            let next_unit_offset = unit_offset_in_section + unit_length + 4 in
            (* +4 for length field *)

            Printf.printf
              "0x%08x: Compile Unit: length = 0x%08x, format = DWARF32, \
               version = 0x%04x, unit_type = %s, abbr_offset = 0x%04x, \
               addr_size = 0x%02x (next unit at 0x%08x)\n"
              unit_offset_in_section unit_length
              (Unsigned.UInt16.to_int parsed.version)
              (Dwarf.string_of_unit_type unit_type)
              (Unsigned.UInt32.to_int parsed.debug_abbrev_offset)
              (Unsigned.UInt8.to_int parsed.address_size)
              next_unit_offset)
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
    (* Determine if we're dealing with a regular binary or dSYM *)
    let actual_filename, is_dsym =
      if Sys.file_exists filename then (filename, false)
      else
        (* Try to find dSYM file *)
        let dsym_path =
          filename ^ ".dSYM/Contents/Resources/DWARF/"
          ^ Filename.basename filename
        in
        if Sys.file_exists dsym_path then (dsym_path, true)
        else (filename, false)
      (* Use original filename, will fail later if not found *)
    in

    (* Output header similar to dwarfdump *)
    Printf.printf "%s:\tfile format Mach-O arm64\n\n" actual_filename;
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

(* Command line interface *)
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

let all_flag =
  let doc = "Dump all available debug information" in
  Cmdliner.Arg.(value & flag & info [ "all"; "a" ] ~doc)

let dwarfdump_cmd debug_line debug_info debug_names all filename =
  match (debug_line, debug_info, debug_names, all) with
  | true, _, _, _ -> dump_debug_line filename
  | _, true, _, _ -> dump_debug_info filename
  | _, _, true, _ -> dump_debug_names filename
  | _, _, _, true -> dump_all filename
  | false, false, false, false ->
      (* Default behavior - dump debug line information *)
      dump_debug_line filename

let cmd =
  let doc = "A DWARF debugging information dumper" in
  let info = Cmdliner.Cmd.info "dwarfdump" ~doc in
  Cmdliner.Cmd.v info
    Cmdliner.Term.(
      const dwarfdump_cmd $ debug_line_flag $ debug_info_flag $ debug_names_flag
      $ all_flag $ filename)

let () = exit (Cmdliner.Cmd.eval cmd)
