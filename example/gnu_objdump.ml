(* An implementation of the "objdump" utility for DWARF debugging information on Linux ELF files *)
open Durin

(* Helper function to get section offset and size for ELF files *)
let get_section_offset buffer section_type =
  let object_format = Dwarf.detect_format buffer in
  let section_name = Dwarf.object_format_to_section_name object_format section_type in
  try
    let open Object.Elf in
    let _header, section_array = read_elf buffer in
    (* Find the section by name and return its offset and size *)
    let section_opt = Array.find_opt (fun section ->
      section.sh_name_str = section_name
    ) section_array in
    match section_opt with
    | Some section -> Some (section.sh_offset, section.sh_size)
    | None -> None
  with _ -> None

let resolve_binary_path filename =
  (* For ELF files, debug info is typically in the same file or separate .debug file *)
  if Sys.file_exists filename then (filename, false)
  else
    (* Try to find separate debug file *)
    let debug_path = filename ^ ".debug" in
    if Sys.file_exists debug_path then (debug_path, true) else (filename, false)

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
  try
    let actual_filename, is_debug = resolve_binary_path filename in
    let buffer = Object.Buffer.parse actual_filename in
    let format_str = Dwarf.detect_format_and_arch buffer in

    (* Output header similar to objdump --dwarf=line *)
    Printf.printf "%s:\tfile format %s\n\n" actual_filename format_str;
    Printf.printf "Contents of the .debug_line section:\n\n";

    (* Try to get the debug_line section offset and size *)
    match get_section_offset buffer Dwarf.Debug_line with
    | None ->
        Printf.printf "No .debug_line section found in file\n";
        if not is_debug then (
          Printf.printf
            "Note: For ELF binaries, debug info might be in separate .debug files\n";
          let debug_path = filename ^ ".debug" in
          if Sys.file_exists debug_path then Printf.printf "Try: %s\n" debug_path)
    | Some (section_offset, _section_size) ->
        Printf.printf "debug_line[0x%08x]\n" 0;

        (* Create cursor at the debug_line section offset *)
        let cursor = Object.Buffer.cursor buffer ~at:(Unsigned.UInt64.to_int section_offset) in

        (* Parse the line program header using our implementation *)
        let header = Dwarf.LineTable.parse_line_program_header cursor buffer in

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
        Printf.printf "\n"
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
    let actual_filename, is_debug = resolve_binary_path filename in
    let buffer = Object.Buffer.parse actual_filename in
    let format_str = Dwarf.detect_format_and_arch buffer in

    (* Output header similar to objdump --dwarf=info *)
    Printf.printf "%s:\tfile format %s\n\n" actual_filename format_str;
    Printf.printf "Contents of the .debug_info section:\n\n";

    (* Try to get the debug_info section offset and size *)
    match get_section_offset buffer Dwarf.Debug_info with
    | None ->
        Printf.printf "No .debug_info section found in file\n";
        if not is_debug then (
          Printf.printf
            "Note: For ELF binaries, debug info might be in separate .debug files\n";
          let debug_path = filename ^ ".debug" in
          if Sys.file_exists debug_path then Printf.printf "Try: %s\n" debug_path)
    | Some (debug_info_offset, section_size) ->
        Printf.printf "  Note: Simplified debug_info parsing (section at offset 0x%x, size 0x%x)\n"
          (Unsigned.UInt64.to_int debug_info_offset) (Unsigned.UInt64.to_int section_size);
        Printf.printf "  Use 'objdump --dwarf=info' or dwarfdump for complete output\n"
  with
  | Sys_error msg ->
      Printf.eprintf "Error: %s\n" msg;
      exit 1
  | exn ->
      Printf.eprintf "Error parsing DWARF information: %s\n"
        (Printexc.to_string exn);
      exit 1

(* Command line interface matching objdump's --dwarf options *)
let filename =
  let doc = "ELF binary file to analyze for DWARF debug information" in
  Cmdliner.Arg.(required & pos 0 (some file) None & info [] ~docv:"FILE" ~doc)

let dwarf_flag =
  let doc = "Dump DWARF debug information. Supported sections: line, info, abbrev, str, str-offsets, addr" in
  Cmdliner.Arg.(value & opt (some string) None & info [ "dwarf" ] ~docv:"SECTION" ~doc)

let objdump_cmd dwarf filename =
  match dwarf with
  | Some "line" -> dump_debug_line filename
  | Some "info" -> dump_debug_info filename
  | Some _ ->
      Printf.eprintf "Error: Unsupported DWARF section. Supported: line, info, abbrev, str, str-offsets, addr\n";
      exit 1
  | None ->
      Printf.eprintf "Error: --dwarf option requires a section name\n";
      exit 1

let cmd =
  let doc = "Display information from ELF object files with DWARF debugging information" in
  let info = Cmdliner.Cmd.info "gnu-objdump" ~doc in
  Cmdliner.Cmd.v info
    Cmdliner.Term.(const objdump_cmd $ dwarf_flag $ filename)

let () = exit (Cmdliner.Cmd.eval cmd)