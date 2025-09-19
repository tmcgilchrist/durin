(* An implementation of the "dwarfdump" utility for DWARF debugging information on Linux ELF files *)
open Durin

(* Helper function to get section offset and size for ELF files *)
let get_section_offset buffer section_type =
  let object_format = Dwarf.detect_format buffer in
  let section_name =
    Dwarf.object_format_to_section_name object_format section_type
  in
  try
    let open Object.Elf in
    let _header, section_array = read_elf buffer in
    (* Find the section by name and return its offset and size *)
    let section_opt =
      Array.find_opt
        (fun section -> section.sh_name_str = section_name)
        section_array
    in
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

(* Helper function to get section display name using object format detection *)
let get_section_display_name buffer section_type =
  let object_format = Dwarf.detect_format buffer in
  Dwarf.object_format_to_section_name object_format section_type

(* Helper function to format section headers consistently *)
let format_section_header buffer section_type additional_info =
  let section_name = get_section_display_name buffer section_type in
  match additional_info with
  | None -> Printf.sprintf "\n%s\n" section_name
  | Some info -> Printf.sprintf "\n%s: %s\n" section_name info

(* Helper function to get section name for CLI documentation (uses ELF as default) *)
let get_section_doc_name section_type =
  Dwarf.object_format_to_section_name Dwarf.ELF section_type

(* Helper function to generate CLI flag documentation *)
let make_section_flag_doc section_type =
  Printf.sprintf "Dump the %s section" (get_section_doc_name section_type)

let dump_debug_line filename =
  try
    let actual_filename, _is_debug = resolve_binary_path filename in
    let buffer = Object.Buffer.parse actual_filename in

    (* Try to get the debug_line section offset and size *)
    match get_section_offset buffer Dwarf.Debug_line with
    | None -> Printf.printf "\n"
    | Some (section_offset, _section_size) ->
        Printf.printf "%s"
          (format_section_header buffer Dwarf.Debug_line
             (Some "line number info for a single cu"));
        Printf.printf "Source lines (from CU-DIE at %s offset 0x0000000c):\n\n"
          (get_section_display_name buffer Dwarf.Debug_info);
        Printf.printf
          "            NS new statement, BB new basic block, ET end of text \
           sequence\n";
        Printf.printf "            PE prologue end, EB epilogue begin\n";
        Printf.printf
          "            IS=val ISA number, DI=val discriminator value\n";
        Printf.printf
          "<pc>        [lno,col] NS BB ET PE EB IS= DI= uri: \"filepath\"\n";

        (* Create cursor at the debug_line section offset *)
        let cursor =
          Object.Buffer.cursor buffer
            ~at:(Unsigned.UInt64.to_int section_offset)
        in

        (* Parse the line program header using our implementation *)
        let header = Dwarf.LineTable.parse_line_program_header cursor buffer in

        (* Parse the line program and display entries *)
        let entries = Dwarf.LineTable.parse_line_program cursor header in

        (* Helper function to resolve line_strp offsets using object format detection *)
        let resolve_line_strp_offset_proper buffer offset =
          match get_section_offset buffer Dwarf.Debug_line_str with
          | None ->
              Printf.sprintf "<line_strp:0x%08lx>"
                (Unsigned.UInt32.to_int32 offset)
          | Some (section_offset, _) -> (
              try
                let string_cursor =
                  Object.Buffer.cursor buffer
                    ~at:
                      (Unsigned.UInt64.to_int section_offset
                      + Unsigned.UInt32.to_int offset)
                in
                let str_buffer = Stdlib.Buffer.create 256 in
                let rec read_string () =
                  let byte = Object.Buffer.Read.u8 string_cursor in
                  if Unsigned.UInt8.to_int byte = 0 then ()
                  else (
                    Stdlib.Buffer.add_char str_buffer
                      (char_of_int (Unsigned.UInt8.to_int byte));
                    read_string ())
                in
                read_string ();
                Stdlib.Buffer.contents str_buffer
              with _ ->
                Printf.sprintf "<line_strp:0x%08lx>"
                  (Unsigned.UInt32.to_int32 offset))
        in

        (* Get the resolved filename from header for first file if available *)
        let get_filename () =
          if Array.length header.file_names > 0 then
            let file_entry = header.file_names.(0) in
            (* Resolve line_strp references if they exist *)
            let resolved_name =
              if String.contains file_entry.name '<' then
                (* Parse <line_strp:0x...> format *)
                let name_str = file_entry.name in
                if String.sub name_str 0 11 = "<line_strp:" then
                  let hex_part =
                    String.sub name_str 11 (String.length name_str - 12)
                  in
                  let offset =
                    Scanf.sscanf hex_part "0x%08lx" (fun x ->
                        Unsigned.UInt32.of_int32 x)
                  in
                  resolve_line_strp_offset_proper buffer offset
                else file_entry.name
              else file_entry.name
            in
            let resolved_dir =
              if String.contains file_entry.directory '<' then
                (* Parse <line_strp:0x...> format *)
                let dir_str = file_entry.directory in
                if String.sub dir_str 0 11 = "<line_strp:" then
                  let hex_part =
                    String.sub dir_str 11 (String.length dir_str - 12)
                  in
                  let offset =
                    Scanf.sscanf hex_part "0x%08lx" (fun x ->
                        Unsigned.UInt32.of_int32 x)
                  in
                  resolve_line_strp_offset_proper buffer offset
                else file_entry.directory
              else file_entry.directory
            in
            let full_path =
              if resolved_dir = "" then resolved_name
              else resolved_dir ^ "/" ^ resolved_name
            in
            Some full_path
          else None
        in

        (* Display each entry in system dwarfdump format *)
        List.iteri
          (fun i entry ->
            let flags = [] in
            let flags =
              if entry.Dwarf.LineTable.is_stmt then "NS" :: flags else flags
            in
            let flags =
              if entry.Dwarf.LineTable.basic_block then "BB" :: flags else flags
            in
            let flags =
              if entry.Dwarf.LineTable.end_sequence then "ET" :: flags
              else flags
            in
            let flags =
              if entry.Dwarf.LineTable.prologue_end then "PE" :: flags
              else flags
            in
            let flags =
              if entry.Dwarf.LineTable.epilogue_begin then "EB" :: flags
              else flags
            in
            let flags_str = String.concat " " (List.rev flags) in

            let isa_str =
              if Unsigned.UInt32.to_int entry.Dwarf.LineTable.isa <> 0 then
                Printf.sprintf " IS=%ld"
                  (Unsigned.UInt32.to_int32 entry.Dwarf.LineTable.isa)
              else ""
            in

            let discriminator_str =
              if Unsigned.UInt32.to_int entry.Dwarf.LineTable.discriminator <> 0
              then
                Printf.sprintf " DI=%ld"
                  (Unsigned.UInt32.to_int32 entry.Dwarf.LineTable.discriminator)
              else ""
            in

            let uri_str =
              if i = 0 then
                match get_filename () with
                | Some filename -> Printf.sprintf " uri: \"%s\"" filename
                | None -> ""
              else ""
            in

            Printf.printf "0x%08x  [%4ld,%2ld] %s%s%s%s\n"
              (Unsigned.UInt64.to_int entry.Dwarf.LineTable.address)
              (Unsigned.UInt32.to_int32 entry.Dwarf.LineTable.line)
              (Unsigned.UInt32.to_int32 entry.Dwarf.LineTable.column)
              flags_str isa_str discriminator_str uri_str)
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

    (* Output header similar to dwarfdump --debug-info *)
    Printf.printf "%s:\tfile format %s\n\n" actual_filename format_str;
    Printf.printf "Contents of the %s section:\n\n"
      (get_section_display_name buffer Dwarf.Debug_info);

    (* Try to get the debug_info section offset and size *)
    match get_section_offset buffer Dwarf.Debug_info with
    | None ->
        Printf.printf "No %s section found in file\n"
          (get_section_display_name buffer Dwarf.Debug_info);
        if not is_debug then (
          Printf.printf
            "Note: For ELF binaries, debug info might be in separate .debug \
             files\n";
          let debug_path = filename ^ ".debug" in
          if Sys.file_exists debug_path then
            Printf.printf "Try: %s\n" debug_path)
    | Some (debug_info_offset, section_size) ->
        Printf.printf
          "  Note: Simplified debug_info parsing (section at offset 0x%x, size \
           0x%x)\n"
          (Unsigned.UInt64.to_int debug_info_offset)
          (Unsigned.UInt64.to_int section_size);
        Printf.printf "  Use 'dwarfdump --debug-info' for complete output\n"
  with
  | Sys_error msg ->
      Printf.eprintf "Error: %s\n" msg;
      exit 1
  | exn ->
      Printf.eprintf "Error parsing DWARF information: %s\n"
        (Printexc.to_string exn);
      exit 1

let dump_debug_str filename =
  try
    let actual_filename, is_debug = resolve_binary_path filename in
    let buffer = Object.Buffer.parse actual_filename in

    (* Output header similar to dwarfdump --debug-str *)
    Printf.printf "%s" (format_section_header buffer Dwarf.Debug_str None);

    (* Try to get the debug_str section offset and size *)
    match get_section_offset buffer Dwarf.Debug_str with
    | None ->
        Printf.printf "No %s section found in file\n"
          (get_section_display_name buffer Dwarf.Debug_str);
        if not is_debug then (
          Printf.printf
            "Note: For ELF binaries, debug info might be in separate .debug \
             files\n";
          let debug_path = filename ^ ".debug" in
          if Sys.file_exists debug_path then
            Printf.printf "Try: %s\n" debug_path)
    | Some (section_offset, section_size) ->
        (* Create cursor at the debug_str section offset *)
        let cursor =
          Object.Buffer.cursor buffer
            ~at:(Unsigned.UInt64.to_int section_offset)
        in

        (* Parse strings from the section *)
        let section_end =
          Unsigned.UInt64.to_int section_offset
          + Unsigned.UInt64.to_int section_size
        in
        let current_pos = ref (Unsigned.UInt64.to_int section_offset) in
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
          let str_length = String.length str_content in
          if str_length > 0 then
            Printf.printf "name at offset 0x%08x, length %4d is '%s'\n"
              !string_offset str_length str_content
          else if !current_pos < section_end then
            (* Empty string, but not at end of section *)
            Printf.printf "name at offset 0x%08x, length %4d is ''\n"
              !string_offset 0;

          string_offset := !string_offset + (!current_pos - start_pos)
        done;
        Printf.printf "\n"
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
    let actual_filename, _is_debug = resolve_binary_path filename in
    let buffer = Object.Buffer.parse actual_filename in

    (* Try to get the debug_str_offsets section offset and size using conversion function *)
    match get_section_offset buffer Dwarf.Debug_str_offs with
    | None ->
        (* Match system dwarfdump behavior: output empty line when section missing *)
        Printf.printf "\n"
    | Some (section_offset, _section_size) ->
        (* Output header when section exists *)
        Printf.printf "%s"
          (format_section_header buffer Dwarf.Debug_str_offs None);

        (* Create cursor at the debug_str_offsets section offset *)
        let cursor =
          Object.Buffer.cursor buffer
            ~at:(Unsigned.UInt64.to_int section_offset)
        in

        (* Parse header manually for ELF format *)
        let unit_length = Object.Buffer.Read.u32 cursor in
        let version = Object.Buffer.Read.u16 cursor in
        let padding = Object.Buffer.Read.u16 cursor in

        (* Print table header information *)
        Printf.printf " table 0\n";
        Printf.printf " tableheader 0x%08x\n" 0;
        Printf.printf " arrayoffset 0x%08x\n" 8;
        Printf.printf " unit length 0x%08x\n"
          (Unsigned.UInt32.to_int unit_length);
        Printf.printf " entry size  4\n";
        Printf.printf " version     %d\n" (Unsigned.UInt16.to_int version);
        Printf.printf " padding     0x%x\n" (Unsigned.UInt16.to_int padding);

        (* Calculate number of offsets *)
        let data_size = Unsigned.UInt32.to_int unit_length - 4 in
        (* unit_length excludes itself *)
        let offset_size = 4 in
        let num_offsets = data_size / offset_size in
        Printf.printf " arraysize   %d\n" num_offsets;

        (* Parse and print offsets in the format expected by system dwarfdump *)
        let rec print_offsets i =
          if i < num_offsets then (
            let offset1 = Object.Buffer.Read.u32 cursor in
            let offset2 =
              if i + 1 < num_offsets then Object.Buffer.Read.u32 cursor
              else Unsigned.UInt32.zero
            in
            let offset3 =
              if i + 2 < num_offsets then Object.Buffer.Read.u32 cursor
              else Unsigned.UInt32.zero
            in
            let offset4 =
              if i + 3 < num_offsets then Object.Buffer.Read.u32 cursor
              else Unsigned.UInt32.zero
            in

            let print_4_offsets start_idx o1 o2 o3 o4 count =
              Printf.printf " Entry [%4d]: " start_idx;
              for j = 0 to count - 1 do
                let offset =
                  match j with
                  | 0 -> o1
                  | 1 -> o2
                  | 2 -> o3
                  | 3 -> o4
                  | _ -> Unsigned.UInt32.zero
                in
                Printf.printf " 0x%08x" (Unsigned.UInt32.to_int offset)
              done;
              Printf.printf "\n"
            in

            let remaining = num_offsets - i in
            let count = min 4 remaining in
            print_4_offsets i offset1 offset2 offset3 offset4 count;
            print_offsets (i + 4))
        in
        print_offsets 0;
        Printf.printf " wasted      0 bytes\n\n"
  with
  | Sys_error msg ->
      Printf.eprintf "Error: %s\n" msg;
      exit 1
  | exn ->
      Printf.eprintf "Error parsing DWARF information: %s\n"
        (Printexc.to_string exn);
      exit 1

(* Command line interface matching dwarfdump's --debug-* options *)
let filename =
  let doc = "ELF binary file to analyze for DWARF debug information" in
  Cmdliner.Arg.(required & pos 0 (some file) None & info [] ~docv:"FILE" ~doc)

let debug_line_flag =
  let doc = make_section_flag_doc Dwarf.Debug_line in
  Cmdliner.Arg.(value & flag & info [ "debug-line" ] ~doc)

let debug_info_flag =
  let doc = make_section_flag_doc Dwarf.Debug_info in
  Cmdliner.Arg.(value & flag & info [ "debug-info" ] ~doc)

let debug_str_flag =
  let doc = make_section_flag_doc Dwarf.Debug_str in
  Cmdliner.Arg.(value & flag & info [ "debug-str" ] ~doc)

let debug_str_offsets_flag =
  let doc = make_section_flag_doc Dwarf.Debug_str_offs in
  Cmdliner.Arg.(value & flag & info [ "debug-str-offsets" ] ~doc)

let debug_abbrev_flag =
  let doc = make_section_flag_doc Dwarf.Debug_abbrev in
  Cmdliner.Arg.(value & flag & info [ "debug-abbrev" ] ~doc)

let debug_addr_flag =
  let doc = make_section_flag_doc Dwarf.Debug_addr in
  Cmdliner.Arg.(value & flag & info [ "debug-addr" ] ~doc)

let debug_names_flag =
  let doc = make_section_flag_doc Dwarf.Debug_names in
  Cmdliner.Arg.(value & flag & info [ "debug-names" ] ~doc)

let debug_macro_flag =
  let doc = make_section_flag_doc Dwarf.Debug_macro in
  Cmdliner.Arg.(value & flag & info [ "debug-macro" ] ~doc)

let debug_line_str_flag =
  let doc = make_section_flag_doc Dwarf.Debug_line_str in
  Cmdliner.Arg.(value & flag & info [ "debug-line-str" ] ~doc)

let debug_aranges_flag =
  let doc = make_section_flag_doc Dwarf.Debug_aranges in
  Cmdliner.Arg.(value & flag & info [ "debug-aranges" ] ~doc)

let debug_loclists_flag =
  let doc = make_section_flag_doc Dwarf.Debug_loclists in
  Cmdliner.Arg.(value & flag & info [ "debug-loclists" ] ~doc)

let dwarfdump_cmd debug_line debug_info debug_str debug_str_offsets debug_abbrev
    debug_addr debug_names debug_macro debug_line_str debug_aranges
    debug_loclists filename =
  let count =
    [
      debug_line;
      debug_info;
      debug_str;
      debug_str_offsets;
      debug_abbrev;
      debug_addr;
      debug_names;
      debug_macro;
      debug_line_str;
      debug_aranges;
      debug_loclists;
    ]
    |> List.filter (fun x -> x)
    |> List.length
  in
  if count = 0 then (
    Printf.eprintf
      "Error: No debug section specified. Use --debug-line, --debug-info, etc.\n";
    exit 1)
  else if count > 1 then (
    Printf.eprintf "Error: Only one debug section can be specified at a time\n";
    exit 1)
  else if debug_line then dump_debug_line filename
  else if debug_info then dump_debug_info filename
  else if debug_str then dump_debug_str filename
  else if debug_str_offsets then dump_debug_str_offsets filename
  else if debug_abbrev then (
    Printf.eprintf "Unimplemented\n";
    exit 1)
  else if debug_addr then (
    Printf.eprintf "Unimplemented\n";
    exit 1)
  else if debug_names then (
    Printf.eprintf "Unimplemented\n";
    exit 1)
  else if debug_macro then (
    Printf.eprintf "Unimplemented\n";
    exit 1)
  else if debug_line_str then (
    Printf.eprintf "Unimplemented\n";
    exit 1)
  else if debug_aranges then (
    Printf.eprintf "Unimplemented\n";
    exit 1)
  else if debug_loclists then (
    Printf.eprintf "Unimplemented\n";
    exit 1)
  else (
    Printf.eprintf "Error: Unknown debug section\n";
    exit 1)

let cmd =
  let doc = "Display DWARF debugging information from ELF object files" in
  let info = Cmdliner.Cmd.info "gnu-dwarfdump" ~doc in
  Cmdliner.Cmd.v info
    Cmdliner.Term.(
      const dwarfdump_cmd $ debug_line_flag $ debug_info_flag $ debug_str_flag
      $ debug_str_offsets_flag $ debug_abbrev_flag $ debug_addr_flag
      $ debug_names_flag $ debug_macro_flag $ debug_line_str_flag
      $ debug_aranges_flag $ debug_loclists_flag $ filename)

let () = exit (Cmdliner.Cmd.eval cmd)
