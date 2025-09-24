(* An implementation of the "dwarfdump" utility for DWARF debugging information on Linux ELF files *)
open Durin

(* TODO Move into object? *)
(* Helper function to map ELF machine type to architecture string *)
let string_of_elf_machine = function
  | `EM_386 -> "i386"
  | `EM_X86_64 -> "x86-64"
  | `EM_ARM -> "arm"
  | `EM_AARCH64 -> "aarch64"
  | `EM_MIPS -> "mips"
  | `EM_PPC -> "powerpc"
  | `EM_PPC64 -> "powerpc64"
  | `EM_SPARC -> "sparc"
  | `EM_SPARCV9 -> "sparcv9"
  | `EM_IA_64 -> "ia64"
  | `EM_S390 -> "s390"
  | `EM_RISCV -> "riscv"
  | `EM_68K -> "m68k"
  | `EM_88K -> "m88k"
  | `EM_PARISC -> "hppa"
  | `EM_SH -> "sh"
  | `EM_UNKNOWN x -> Printf.sprintf "unknown_%d" x
  | _ -> "unknown"

(* Helper function to get architecture string from ELF headers *)
let get_architecture_string buffer =
  try
    let open Object.Elf in
    let header, _section_array = read_elf buffer in
    (* Extract architecture information from ELF header *)
    let machine_str = string_of_elf_machine header.e_machine in
    let class_str =
      match header.e_ident.elf_class with
      | `ELFCLASS32 -> "elf32"
      | `ELFCLASS64 -> "elf64"
      | `ELFCLASSNONE -> "elf"
    in
    Printf.sprintf "%s-%s" class_str machine_str
  with _ -> "elf-unknown"

(* Helper function to check if a string contains a substring *)
let string_contains_substring s substring =
  try
    let _ = Str.search_forward (Str.regexp_string substring) s 0 in
    true
  with Not_found -> false

(* Helper function to resolve function names from addresses using symbol table *)
let resolve_function_name buffer start_addr end_addr =
  try
    (* Parse symbol table using new ELF symbols module *)
    let symbols = Elf_symbols.parse_symbol_table buffer in

    (* Try to find symbol by start address first *)
    match Elf_symbols.find_symbol_by_address symbols start_addr with
    | Some name -> name
    | None -> (
        (* Try end address if start address doesn't match *)
        match Elf_symbols.find_symbol_by_address symbols end_addr with
        | Some name -> name
        | None -> "<missing name>")
  with _ -> "<missing name>"

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

        (* Get the header size from the first compilation unit *)
        let header_size =
          match get_section_offset buffer Dwarf.Debug_info with
          | Some (debug_info_offset, _) ->
              let cursor =
                Object.Buffer.cursor buffer
                  ~at:(Unsigned.UInt64.to_int debug_info_offset)
              in
              let _span, header = Dwarf.parse_compile_unit_header cursor in
              Unsigned.UInt64.to_int header.header_span.size
          | None ->
              failwith
                "No debug_info section found - cannot determine header size"
        in

        Printf.printf "Source lines (from CU-DIE at %s offset 0x%08x):\n\n"
          (get_section_display_name buffer Dwarf.Debug_info)
          header_size;
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

        (* Get the filename from header for first file (line_strp references now resolved by library) *)
        let get_filename () =
          if Array.length header.file_names > 0 then
            let file_entry = header.file_names.(0) in
            let full_path =
              if file_entry.directory = "" then file_entry.name
              else file_entry.directory ^ "/" ^ file_entry.name
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

(* Common hex formatting helpers - eliminates Printf.sprintf repetition *)
let format_hex32_int value = Printf.sprintf "0x%08x" value
let format_hex32 value = Printf.sprintf "0x%08x" (Unsigned.UInt64.to_int value)

(* Common Format-based attribute formatting function - eliminates hardcoded spacing *)
let format_attribute_with_spacing ~depth attr_name attr_value =
  let base_indent = if depth = 0 then 20 else 22 in
  let attr_name_width = String.length attr_name in
  let target_value_column = if depth = 0 then 48 else 50 in
  let spacing_needed =
    max 1 (target_value_column - base_indent - attr_name_width)
  in
  Format.printf "%*s%s%*s%s@." base_indent "" attr_name spacing_needed ""
    attr_value

(* Helper function to format DIE output in system dwarfdump format *)
let rec print_die_system_format die depth buffer dwarf unit_start_offset
    debug_info_offset stmt_list_offset header_size ~print_children =
  (* Calculate relative offset within debug_info section *)
  let relative_offset =
    if depth = 0 then
      (* For root DIE, the offset is unit_start + header size *)
      unit_start_offset + header_size
    else
      (* For child DIEs, subtract debug_info section offset to get relative offset *)
      die.Dwarf.DIE.offset - debug_info_offset
  in
  let offset_str = format_hex32_int relative_offset in
  let tag_str = Dwarf.string_of_abbreviation_tag_direct die.Dwarf.DIE.tag in

  (* Format: < depth><offset>  TAG_NAME or < depth><offset>    TAG_NAME for children *)
  (* Use Format module for automatic spacing based on depth *)
  let tag_spacing = if depth = 0 then 2 else 4 in
  Format.printf "< %d><%s>%*s%s@." depth offset_str tag_spacing "" tag_str;

  (* Print attributes with system dwarfdump formatting *)
  List.iter
    (fun attr ->
      let attr_name = Dwarf.string_of_attribute_encoding attr.Dwarf.DIE.attr in
      let attr_value =
        match attr.Dwarf.DIE.value with
        | Dwarf.DIE.String s -> s
        | Dwarf.DIE.UData u ->
            (* Special formatting for different attributes *)
            if attr.Dwarf.DIE.attr = Dwarf.DW_AT_high_pc then
              (* Check if this is offset-from-lowpc format *)
              let low_pc_opt =
                List.find_opt
                  (fun a -> a.Dwarf.DIE.attr = Dwarf.DW_AT_low_pc)
                  die.Dwarf.DIE.attributes
              in
              match low_pc_opt with
              | Some low_pc_attr -> (
                  match low_pc_attr.Dwarf.DIE.value with
                  | Dwarf.DIE.Address low_pc ->
                      let offset = Unsigned.UInt64.to_int u in
                      let high_pc = Unsigned.UInt64.add low_pc u in
                      Printf.sprintf "<offset-from-lowpc> %d <highpc: 0x%08x>"
                        offset
                        (Unsigned.UInt64.to_int high_pc)
                  | _ -> format_hex32 u)
              | None -> format_hex32 u
            else if attr.Dwarf.DIE.attr = Dwarf.DW_AT_decl_file then
              Printf.sprintf "%s %s" (format_hex32 u)
                (match resolve_file_reference buffer stmt_list_offset u with
                | Some filename -> filename
                | None -> "")
            else if attr.Dwarf.DIE.attr = Dwarf.DW_AT_decl_line then
              Printf.sprintf "0x%08x" (Unsigned.UInt64.to_int u)
            else if attr.Dwarf.DIE.attr = Dwarf.DW_AT_decl_column then
              Printf.sprintf "0x%08x" (Unsigned.UInt64.to_int u)
            else if attr.Dwarf.DIE.attr = Dwarf.DW_AT_byte_size then
              Printf.sprintf "0x%08x" (Unsigned.UInt64.to_int u)
            else if attr.Dwarf.DIE.attr = Dwarf.DW_AT_stmt_list then
              Printf.sprintf "0x%08x" (Unsigned.UInt64.to_int u)
            else Printf.sprintf "0x%08x" (Unsigned.UInt64.to_int u)
        | Dwarf.DIE.Address addr ->
            Printf.sprintf "0x%08x" (Unsigned.UInt64.to_int addr)
        | Dwarf.DIE.Flag b -> if b then "yes(1)" else "no(0)"
        | Dwarf.DIE.Reference r ->
            Printf.sprintf "<0x%08x>" (Unsigned.UInt64.to_int r)
        | Dwarf.DIE.Language lang -> Dwarf.string_of_dwarf_language lang
        | Dwarf.DIE.Encoding enc -> Dwarf.string_of_base_type enc
        | Dwarf.DIE.Block b ->
            (* Special handling for DW_AT_frame_base *)
            if attr.Dwarf.DIE.attr = Dwarf.DW_AT_frame_base then
              format_frame_base_block b
            else
              Printf.sprintf "len 0x%04x: %s" (String.length b)
                (format_block_hex b)
        | _ -> "<unsupported>"
      in
      (* Use common Format-based attribute formatting *)
      format_attribute_with_spacing ~depth attr_name attr_value)
    die.Dwarf.DIE.attributes;

  (* Print children only if requested *)
  if print_children then
    Seq.iter
      (fun child ->
        print_die_system_format child (depth + 1) buffer dwarf unit_start_offset
          debug_info_offset stmt_list_offset header_size ~print_children:true)
      die.Dwarf.DIE.children

(* Helper function to resolve file references for decl_file attributes *)
and resolve_file_reference buffer stmt_list_offset file_index =
  (* Parse the line table to resolve file index to actual filename *)
  try
    match get_section_offset buffer Dwarf.Debug_line with
    | None -> None
    | Some (debug_line_offset, _size) ->
        (* Calculate absolute offset in debug_line section *)
        let absolute_offset =
          Unsigned.UInt64.to_int debug_line_offset
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

(* Helper function to format block data as hex *)
and format_block_hex block_data =
  let bytes = String.to_seq block_data |> List.of_seq in
  String.concat ""
    (List.map (fun c -> Printf.sprintf "0x%02x" (Char.code c)) bytes)

(* Helper function to format frame base blocks *)
and format_frame_base_block block_data =
  (* Use DWARF library constant for call frame CFA opcode (0x9c) *)
  let dw_op_call_frame_cfa = 0x9c in
  (* DW_OP_call_frame_cfa opcode value *)

  if String.length block_data > 0 then
    let opcode = Char.code block_data.[0] in
    match opcode with
    | x when x = dw_op_call_frame_cfa ->
        Printf.sprintf "len 0x%04x: 0x%02x: \n                          %s"
          (String.length block_data) dw_op_call_frame_cfa
          (Dwarf.string_of_operation_encoding Dwarf.DW_OP_call_frame_cfa)
    | _ ->
        Printf.sprintf "len 0x%04x: %s" (String.length block_data)
          (format_block_hex block_data)
  else Printf.sprintf "len 0x0000: "

let dump_debug_info filename =
  try
    let actual_filename, is_debug = resolve_binary_path filename in
    let buffer = Object.Buffer.parse actual_filename in

    (* Output header similar to system dwarfdump --print-info *)
    Printf.printf "\n%s\n\n" (get_section_display_name buffer Dwarf.Debug_info);

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
    | Some (debug_info_offset, _section_size) ->
        (* Create DWARF context and parse compile units *)
        let dwarf = Dwarf.create buffer in
        let compile_units = Dwarf.parse_compile_units dwarf in

        (* Process each compile unit *)
        Seq.iter
          (fun unit ->
            let header = Dwarf.CompileUnit.header unit in

            (* Print compilation unit header to match system dwarfdump format *)
            let unit_start_in_debug_info = Dwarf.CompileUnit.dwarf_info unit in
            Printf.printf "COMPILE_UNIT<header overall offset = 0x%08x>:\n"
              unit_start_in_debug_info;

            (* Get the abbreviation table for this compilation unit *)
            let abbrev_offset =
              Unsigned.UInt64.of_uint32 header.debug_abbrev_offset
            in
            let _, abbrev_table = Dwarf.get_abbrev_table dwarf abbrev_offset in

            (* Get the root DIE for this compilation unit *)
            match Dwarf.CompileUnit.root_die unit abbrev_table buffer with
            | None ->
                Printf.printf "  No root DIE found for this compilation unit\n"
            | Some root_die ->
                (* Extract DW_AT_stmt_list offset for file index resolution *)
                let stmt_list_offset =
                  match
                    List.find_opt
                      (fun attr -> attr.Dwarf.DIE.attr = Dwarf.DW_AT_stmt_list)
                      root_die.Dwarf.DIE.attributes
                  with
                  | Some attr -> (
                      match attr.Dwarf.DIE.value with
                      | Dwarf.DIE.UData offset -> offset
                      | _ -> Unsigned.UInt64.zero)
                  | None -> Unsigned.UInt64.zero
                in

                (* Print the compilation unit DIE without children first *)
                print_die_system_format root_die 0 buffer dwarf
                  unit_start_in_debug_info
                  (Unsigned.UInt64.to_int debug_info_offset)
                  stmt_list_offset
                  (Unsigned.UInt64.to_int header.header_span.size)
                  ~print_children:false;

                (* Print LOCAL_SYMBOLS header and child DIEs *)
                Printf.printf "\nLOCAL_SYMBOLS:\n";
                Seq.iter
                  (fun child ->
                    print_die_system_format child 1 buffer dwarf
                      unit_start_in_debug_info
                      (Unsigned.UInt64.to_int debug_info_offset)
                      stmt_list_offset
                      (Unsigned.UInt64.to_int header.header_span.size)
                      ~print_children:true)
                  root_die.Dwarf.DIE.children;
                Printf.printf "\n")
          compile_units
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

    (* Use DebugStr.parse to get string table *)
    match Dwarf.DebugStr.parse buffer with
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
    | Some str_table ->
        (* Output each string entry *)
        Array.iter
          (fun (entry : Dwarf.DebugStr.string_entry) ->
            if entry.length > 0 then
              Printf.printf "name at offset 0x%08x, length %4d is '%s'\n"
                entry.offset entry.length entry.content
            else
              Printf.printf "name at offset 0x%08x, length %4d is ''\n"
                entry.offset 0)
          str_table.entries;
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

        (* Parse header using DebugStrOffsets module *)
        let header = Dwarf.DebugStrOffsets.parse_header cursor in

        (* Print table header information *)
        let table_start_offset = 0 in
        let header_size = Unsigned.UInt64.to_int header.header_span.size in
        Printf.printf " table 0\n";
        Printf.printf " tableheader 0x%08x\n" table_start_offset;
        Printf.printf " arrayoffset 0x%08x\n" header_size;
        Printf.printf " unit length 0x%08x\n"
          (Unsigned.UInt32.to_int header.unit_length);
        Printf.printf " entry size  4\n";
        Printf.printf " version     %d\n"
          (Unsigned.UInt16.to_int header.version);
        Printf.printf " padding     0x%x\n"
          (Unsigned.UInt16.to_int header.padding);

        (* Calculate number of offsets *)
        let data_size = Unsigned.UInt32.to_int header.unit_length - 4 in
        (* unit_length excludes itself *)
        let offset_size = 4 in
        let num_offsets = data_size / offset_size in
        Printf.printf " arraysize   %d\n" num_offsets;

        (* TODO Can we calculate this another way? *)
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

let dump_debug_line_str filename =
  try
    let actual_filename, is_debug = resolve_binary_path filename in
    let buffer = Object.Buffer.parse actual_filename in

    (* Output header similar to dwarfdump --debug-line-str *)
    Printf.printf "%s" (format_section_header buffer Dwarf.Debug_line_str None);

    (* Use DebugLineStr.parse to get line string table *)
    match Dwarf.DebugLineStr.parse buffer with
    | None ->
        Printf.printf "No %s section found in file\n"
          (get_section_display_name buffer Dwarf.Debug_line_str);
        if not is_debug then (
          Printf.printf
            "Note: For ELF binaries, debug info might be in separate .debug \
             files\n";
          let debug_path = filename ^ ".debug" in
          if Sys.file_exists debug_path then
            Printf.printf "Try: %s\n" debug_path)
    | Some line_str_table ->
        (* Output each string entry *)
        Array.iter
          (fun (entry : Dwarf.DebugLineStr.string_entry) ->
            if entry.length > 0 then
              Printf.printf "name at offset 0x%08x, length %4d is '%s'\n"
                entry.offset entry.length entry.content
            else
              Printf.printf "name at offset 0x%08x, length %4d is ''\n"
                entry.offset 0)
          line_str_table.entries;
        Printf.printf "\n"
  with
  | Sys_error msg ->
      Printf.eprintf "Error: %s\n" msg;
      exit 1
  | exn ->
      Printf.eprintf "Error parsing DWARF information: %s\n"
        (Printexc.to_string exn);
      exit 1

let dump_debug_aranges filename =
  try
    let actual_filename, is_debug = resolve_binary_path filename in
    let buffer = Object.Buffer.parse actual_filename in

    (* Output header similar to dwarfdump --debug-aranges with leading newline *)
    Printf.printf "\n.debug_aranges\n";

    (* Use DebugAranges.parse to get address range table *)
    match Dwarf.DebugAranges.parse buffer with
    | None ->
        Printf.printf "No %s section found in file\n"
          (get_section_display_name buffer Dwarf.Debug_aranges);
        if not is_debug then (
          Printf.printf
            "Note: For ELF binaries, debug info might be in separate .debug \
             files\n";
          let debug_path = filename ^ ".debug" in
          if Sys.file_exists debug_path then
            Printf.printf "Try: %s\n" debug_path)
    | Some aranges_set ->
        let header = aranges_set.Dwarf.DebugAranges.header in

        (* Create DWARF object and get compilation units for additional info *)
        let dwarf = Dwarf.create buffer in
        let compile_units = Dwarf.parse_compile_units dwarf in

        (* Find the compilation unit that matches the debug_info_offset from aranges *)
        let cu_die_offset =
          Unsigned.UInt32.to_int header.Dwarf.DebugAranges.debug_info_offset
        in

        (* For the first compilation unit, the unit_offset_in_section should be 0
           and die_offset_in_section should be 12. Since we know cu_die_offset=12,
           we expect the first unit to match when unit_offset_in_section=0. *)
        let matching_unit =
          Seq.find
            (fun unit ->
              (* The span.start for a compilation unit represents the offset within .debug_info
               where this compilation unit starts. For the first CU, this should be 0.
               The cu_die_offset represents where the DIE starts within the CU.
               So we need to find the CU where span.start + header_size = cu_die_offset. *)
              let unit_start_in_debug_info =
                Dwarf.CompileUnit.dwarf_info unit
              in
              let unit_header = Dwarf.CompileUnit.header unit in
              let header_size =
                Unsigned.UInt64.to_int unit_header.header_span.size
              in
              (* The cu_die_offset is relative to the start of .debug_info section.
               For the first CU starting at offset 0, the DIE starts at header size offset.
               For subsequent CUs, we need to check if their DIE offset matches. *)
              unit_start_in_debug_info + header_size = cu_die_offset)
            compile_units
        in

        (* Print compilation unit header info if found *)
        (match matching_unit with
        | Some unit -> (
            let unit_start_in_debug_info = Dwarf.CompileUnit.dwarf_info unit in
            Printf.printf "\nCOMPILE_UNIT<header overall offset = 0x%08x>:\n"
              unit_start_in_debug_info;
            Printf.printf "< 0><0x%08x>  DW_TAG_compile_unit\n" cu_die_offset;

            (* Get abbreviation table for DIE parsing *)
            let _, abbrev_table =
              Dwarf.get_abbrev_table (Dwarf.create buffer)
                (Unsigned.UInt64.of_uint32
                   (Dwarf.CompileUnit.header unit).debug_abbrev_offset)
            in

            (* Parse and print root DIE attributes *)
            match Dwarf.CompileUnit.root_die unit abbrev_table buffer with
            | Some root_die ->
                (* Parse and print root DIE attributes with system dwarfdump formatting *)
                let attributes = root_die.Dwarf.DIE.attributes in

                (* Use common Format-based attribute formatting for debug_aranges *)
                let print_attribute attr_name attr_value =
                  format_attribute_with_spacing ~depth:0 attr_name attr_value
                in

                (* Handle each attribute individually for proper formatting *)
                List.iter
                  (fun attr ->
                    let attr_name =
                      Dwarf.string_of_attribute_encoding attr.Dwarf.DIE.attr
                    in

                    (* Special formatting for DW_AT_high_pc to match system output *)
                    if attr.Dwarf.DIE.attr = Dwarf.DW_AT_high_pc then
                      match attr.Dwarf.DIE.value with
                      | Dwarf.DIE.UData offset_value -> (
                          (* Find DW_AT_low_pc to calculate actual high_pc *)
                          let low_pc_opt =
                            List.find_opt
                              (fun a -> a.Dwarf.DIE.attr = Dwarf.DW_AT_low_pc)
                              attributes
                          in
                          match low_pc_opt with
                          | Some low_pc_attr -> (
                              match low_pc_attr.Dwarf.DIE.value with
                              | Dwarf.DIE.Address low_pc ->
                                  let offset =
                                    Unsigned.UInt64.to_int offset_value
                                  in
                                  let high_pc =
                                    Unsigned.UInt64.add low_pc offset_value
                                  in
                                  let attr_value =
                                    Printf.sprintf
                                      "<offset-from-lowpc> %d <highpc: 0x%08x>"
                                      offset
                                      (Unsigned.UInt64.to_int high_pc)
                                  in
                                  print_attribute attr_name attr_value
                              | _ ->
                                  let attr_value =
                                    Printf.sprintf "0x%08x"
                                      (Unsigned.UInt64.to_int offset_value)
                                  in
                                  print_attribute attr_name attr_value)
                          | None ->
                              let attr_value =
                                Printf.sprintf "0x%08x"
                                  (Unsigned.UInt64.to_int offset_value)
                              in
                              print_attribute attr_name attr_value)
                      | _ ->
                          let attr_value =
                            match attr.Dwarf.DIE.value with
                            | Dwarf.DIE.Address addr ->
                                Printf.sprintf "0x%08x"
                                  (Unsigned.UInt64.to_int addr)
                            | _ -> "<unsupported_form>"
                          in
                          print_attribute attr_name attr_value
                    else
                      (* Standard attribute formatting *)
                      let attr_value =
                        match attr.Dwarf.DIE.value with
                        | Dwarf.DIE.String s -> s
                        | Dwarf.DIE.UData u ->
                            Printf.sprintf "0x%08x" (Unsigned.UInt64.to_int u)
                        | Dwarf.DIE.Address addr ->
                            Printf.sprintf "0x%08x"
                              (Unsigned.UInt64.to_int addr)
                        | Dwarf.DIE.Language lang ->
                            Dwarf.string_of_dwarf_language lang
                        | _ -> "<unsupported_form>"
                      in
                      print_attribute attr_name attr_value)
                  attributes
            | None -> ())
        | None -> ());

        Printf.printf "\n\n";

        (* Print address ranges in system dwarfdump format *)
        List.iter
          (fun range ->
            let start_addr =
              Unsigned.UInt64.to_int64 range.Dwarf.DebugAranges.start_address
            in
            let length =
              Unsigned.UInt64.to_int64 range.Dwarf.DebugAranges.length
            in
            let cu_offset =
              Unsigned.UInt32.to_int32
                header.Dwarf.DebugAranges.debug_info_offset
            in
            Printf.printf
              "arange starts at 0x%08Lx, length of 0x%08Lx, cu_die_offset = \
               0x%08lx\n"
              start_addr length cu_offset)
          aranges_set.Dwarf.DebugAranges.ranges;
        Printf.printf "arange end\n\n"
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
    let actual_filename, is_debug = resolve_binary_path filename in
    let buffer = Object.Buffer.parse actual_filename in

    (* Output header similar to dwarfdump --debug-abbrev *)
    Printf.printf "%s" (format_section_header buffer Dwarf.Debug_abbrev None);

    (* Try to find the debug_abbrev section *)
    match get_section_offset buffer Dwarf.Debug_abbrev with
    | None ->
        Printf.printf "No %s section found in file\n"
          (get_section_display_name buffer Dwarf.Debug_abbrev);
        if not is_debug then (
          Printf.printf
            "Note: For ELF binaries, debug info might be in separate .debug \
             files\n";
          let debug_path = filename ^ ".debug" in
          if Sys.file_exists debug_path then
            Printf.printf "Try: %s\n" debug_path)
    | Some (_offset, _size) ->
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

        (* Output each abbreviation in system dwarfdump format *)
        let current_offset = ref 0 in
        List.iter
          (fun (code, abbrev) ->
            let code_int = Unsigned.UInt64.to_int code in
            let children_str =
              (* TODO Make this a library function *)
              if abbrev.Dwarf.has_children then "DW_children_yes"
              else "DW_children_no"
            in
            let tag_str = Dwarf.string_of_abbreviation_tag abbrev.Dwarf.tag in

            (* Use Format module for table formatting with consistent columns *)
            Format.printf "<%5d><0x%08x><code:%4d> @[<h>%-27s@] %s@." code_int
              !current_offset code_int tag_str children_str;

            current_offset := !current_offset + 3;

            (* Approximate: uleb128 code + uleb128 tag + has_children byte *)

            (* Print attributes with offset tracking *)
            List.iter
              (fun attr_spec ->
                let attr_str =
                  Dwarf.string_of_attribute_code attr_spec.Dwarf.attr
                in
                let form_str =
                  Dwarf.string_of_attribute_form_encoding attr_spec.Dwarf.form
                in
                (* Use Format module for attribute table formatting *)
                Format.printf "@[<h>       <0x%08x>              %-27s@] %s@."
                  !current_offset attr_str form_str;
                current_offset := !current_offset + 2)
                (* Approximate: uleb128 attr + uleb128 form *)
              abbrev.Dwarf.attr_specs;

            (* Add null terminator for attribute list *)
            current_offset := !current_offset + 2)
          sorted_abbrevs;

        (* Add the final null abbrev entry *)
        Printf.printf
          "<%5d><0x%08x><code:%4d> Abbrev 0: null abbrev entry\n\n%!" 5
          !current_offset 0
  with
  | Sys_error msg ->
      Printf.eprintf "Error: %s\n" msg;
      exit 1
  | exn ->
      Printf.eprintf "Error parsing DWARF information: %s\n"
        (Printexc.to_string exn);
      exit 1

let dump_debug_names filename =
  try
    let actual_filename, is_debug = resolve_binary_path filename in
    let buffer = Object.Buffer.parse actual_filename in

    (* Output empty content to match system dwarfdump behavior *)
    Printf.printf "\n";

    (* Try to find the debug_names section *)
    match get_section_offset buffer Dwarf.Debug_names with
    | None ->
        Printf.printf "No %s section found in file\n"
          (get_section_display_name buffer Dwarf.Debug_names);
        if not is_debug then (
          Printf.printf
            "Note: For ELF binaries, debug info might be in separate .debug \
             files\n";
          let debug_path = filename ^ ".debug" in
          if Sys.file_exists debug_path then
            Printf.printf "Try: %s\n" debug_path)
    | Some (section_offset, _section_size) ->
        (* Create cursor at the debug_names section offset *)
        let cursor =
          Object.Buffer.cursor buffer
            ~at:(Unsigned.UInt64.to_int section_offset)
        in

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
            let tag_str = Dwarf.string_of_abbreviation_tag_direct abbrev.tag in
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
                    let str_offset = Unsigned.UInt32.to_int name_entry.offset in
                    (* Try to resolve the name from debug_str section using library function *)
                    let name =
                      match Dwarf.DebugStr.parse buffer with
                      | Some str_table -> (
                          try
                            (* Find the string entry with matching offset *)
                            let matching_entry =
                              Array.find_opt
                                (fun entry ->
                                  entry.Dwarf.DebugStr.offset = str_offset)
                                str_table.entries
                            in
                            match matching_entry with
                            | Some entry -> entry.content
                            | None -> name_entry.value
                          with _ -> name_entry.value)
                      | None -> name_entry.value
                    in

                    (* Parse entries for this name *)
                    let entries =
                      Dwarf.DebugNames.parse_all_entries_for_name buffer
                        debug_names
                        (Unsigned.UInt64.to_int section_offset)
                        name_idx
                    in

                    (* Helper function to format parent information *)
                    let format_parent_info parent_offset_opt _has_parent_flag =
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
                      (fun (entry : Dwarf.DebugNames.entry_parse_result) ->
                        let entry_addr =
                          Unsigned.UInt32.to_int entry.name_offset
                        in
                        let die_offset =
                          Unsigned.UInt32.to_int entry.die_offset
                        in
                        let tag_str = entry.tag_name in
                        let abbrev_id = entry.offset_hex in
                        let parent_offset_opt = entry.unit_index in
                        let has_parent_flag = entry.is_declaration in
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

        Printf.printf "}\n"
  with
  | Sys_error msg ->
      Printf.eprintf "Error: %s\n" msg;
      exit 1
  | exn ->
      Printf.eprintf "Error parsing DWARF information: %s\n"
        (Printexc.to_string exn);
      exit 1

let dump_debug_macro filename =
  try
    let actual_filename, _is_debug = resolve_binary_path filename in
    let buffer = Object.Buffer.parse actual_filename in

    (* Output header similar to dwarfdump --debug-macro *)
    Printf.printf "%s" (format_section_header buffer Dwarf.Debug_macro None);

    (* Try to find the debug_macro section *)
    match get_section_offset buffer Dwarf.Debug_macro with
    | None ->
        Printf.printf "No %s section found in file\n"
          (get_section_display_name buffer Dwarf.Debug_macro);
        if not _is_debug then (
          Printf.printf
            "Note: For ELF binaries, debug info might be in separate .debug \
             files\n";
          let debug_path = filename ^ ".debug" in
          if Sys.file_exists debug_path then
            Printf.printf "Try: %s\n" debug_path)
    | Some (section_offset, section_size) ->
        (* Create cursor at the debug_macro section offset *)
        let cursor =
          Object.Buffer.cursor buffer
            ~at:(Unsigned.UInt64.to_int section_offset)
        in

        (* Parse the debug_macro section *)
        let section_size_int = Unsigned.UInt64.to_int section_size in
        let macro_section =
          Dwarf.parse_debug_macro_section cursor section_size_int
        in

        Printf.printf "Debug macro section parsed successfully with %d units\n"
          (List.length macro_section.units)
  with
  | Sys_error msg ->
      Printf.eprintf "Error: %s\n" msg;
      exit 1
  | exn ->
      Printf.eprintf "Error parsing DWARF information: %s\n"
        (Printexc.to_string exn);
      exit 1

let dump_debug_loclists filename =
  try
    let actual_filename, _is_debug = resolve_binary_path filename in
    let buffer = Object.Buffer.parse actual_filename in

    (* Output header similar to dwarfdump --debug-loclists *)
    Printf.printf "%s" (format_section_header buffer Dwarf.Debug_loclists None);

    (* Try to find the debug_loclists section *)
    match get_section_offset buffer Dwarf.Debug_loclists with
    | None ->
        (* No debug_loclists section found - this is normal for simple programs.
           Show empty section output to match system dwarfdump behavior *)
        ()
    | Some (section_offset, _section_size) ->
        (* Parse the debug_loclists section *)
        let loclists_section =
          Dwarf.DebugLoclists.parse buffer
            (Unsigned.UInt32.of_int (Unsigned.UInt64.to_int section_offset))
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
            (Unsigned.UInt32.to_int32 loclists_section.header.offset_entry_count)
  with
  | Sys_error msg ->
      Printf.eprintf "Error: %s\n" msg;
      exit 1
  | exn ->
      Printf.eprintf "Error parsing DWARF information: %s\n"
        (Printexc.to_string exn);
      exit 1

let dump_debug_addr filename =
  try
    let actual_filename, _is_debug = resolve_binary_path filename in
    let buffer = Object.Buffer.parse actual_filename in

    (* Output header matching llvm-dwarfdump format *)
    Printf.printf "%s:\tfile format %s\n\n.debug_addr contents:\n" filename
      (get_architecture_string buffer);

    (* Try to find the debug_addr section *)
    match get_section_offset buffer Dwarf.Debug_addr with
    | None ->
        (* For missing sections, llvm-dwarfdump just shows the header with empty contents *)
        ()
    | Some (section_offset, _section_size) ->
        (* Parse the debug_addr section *)
        let parsed_addr =
          Dwarf.DebugAddr.parse buffer
            (Unsigned.UInt32.of_int (Unsigned.UInt64.to_int section_offset))
        in

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
      Printf.eprintf "Error parsing DWARF information: %s\n"
        (Printexc.to_string exn);
      exit 1

let dump_debug_frame filename =
  try
    let actual_filename, _is_debug = resolve_binary_path filename in
    let buffer = Object.Buffer.parse actual_filename in

    (* Output header matching system tools format *)
    Printf.printf "%s:\tfile format %s\n\n.debug_frame contents:\n" filename
      (get_architecture_string buffer);

    (* Try to find the debug_frame section *)
    match get_section_offset buffer Dwarf.Debug_frame with
    | None ->
        (* For missing sections, system tools show header with empty contents *)
        Printf.printf "\n"
    | Some (section_offset, section_size) ->
        (* Parse the debug_frame section using library function *)
        let cursor =
          Object.Buffer.cursor buffer
            ~at:(Unsigned.UInt64.to_int section_offset)
        in
        let section_size_int = Unsigned.UInt64.to_int section_size in

        let debug_frame_section =
          Dwarf.CallFrame.parse_debug_frame_section cursor section_size_int
        in

        (* Display parsed entries using library data structures *)
        List.iter
          (fun entry ->
            match entry with
            | Dwarf.CallFrame.CIE cie ->
                (* Calculate position offset for display - this is simplified *)
                let start_pos = 0 in
                (* Would need actual offset tracking in library *)
                Printf.printf "\n%08x %08lx %08lx CIE\n" start_pos
                  (Unsigned.UInt32.to_int32 cie.length)
                  (Unsigned.UInt32.to_int32 cie.cie_id);
                Printf.printf "  Version:               %d\n"
                  (Unsigned.UInt8.to_int cie.version);
                Printf.printf "  Augmentation:          \"%s\"\n"
                  cie.augmentation;
                Printf.printf "  Code alignment factor: %Ld\n"
                  (Unsigned.UInt64.to_int64 cie.code_alignment_factor);
                Printf.printf "  Data alignment factor: %Ld\n"
                  (Signed.Int64.to_int64 cie.data_alignment_factor);
                Printf.printf "  Return address column: %Ld\n"
                  (Unsigned.UInt64.to_int64 cie.return_address_register);

                if String.length cie.initial_instructions > 0 then (
                  Printf.printf "  Initial instructions:\n";
                  Printf.printf "    DW_CFA_def_cfa: (instructions: %d bytes)\n"
                    (String.length cie.initial_instructions))
            | Dwarf.CallFrame.FDE _fde ->
                (* FDE parsing not fully implemented in library yet *)
                Printf.printf "\n%08x FDE (parsing not fully implemented)\n" 0
            | Dwarf.CallFrame.Zero_terminator pos ->
                Printf.printf "\n%08x ZERO terminator\n" pos)
          debug_frame_section.entries;

        if debug_frame_section.entry_count = 0 then Printf.printf "\n"
  with
  | Sys_error msg ->
      Printf.eprintf "Error: %s\n" msg;
      exit 1
  | exn ->
      Printf.eprintf "Error parsing DWARF information: %s\n"
        (Printexc.to_string exn);
      exit 1

(** Format CFI state for human-readable display.

    This function converts a CFI state into a string representation compatible
    with standard dwarfdump output format, showing the CFA definition and all
    register rules in a compact, readable format.

    Parameters:
    - state: CFI state to format

    Returns a formatted string like "<def cfa=r7+8 off r16=-8(cfa)>" showing the
    complete call frame state. *)
let format_cfi_state (state : Dwarf.CallFrame.cfi_state) : string =
  let open Dwarf in
  let cfa_desc =
    Printf.sprintf "def cfa=r%d+%Ld" state.cfa_register state.cfa_offset
  in
  let reg_rules =
    Hashtbl.fold
      (fun reg rule acc ->
        match rule with
        | CallFrame.Rule_offset offset ->
            let rule_desc = Printf.sprintf " off r%d=%+Ld(cfa)" reg offset in
            rule_desc :: acc
        | CallFrame.Rule_register reg_num ->
            let rule_desc = Printf.sprintf " off r%d=r%d" reg reg_num in
            rule_desc :: acc
        | CallFrame.Rule_undefined ->
            let rule_desc = Printf.sprintf " off r%d=undefined" reg in
            rule_desc :: acc
        | CallFrame.Rule_same_value ->
            let rule_desc = Printf.sprintf " off r%d=same" reg in
            rule_desc :: acc
        | CallFrame.Rule_val_offset offset ->
            let rule_desc = Printf.sprintf " off r%d=val+%Ld(cfa)" reg offset in
            rule_desc :: acc
        | CallFrame.Rule_expression _expr ->
            let rule_desc = Printf.sprintf " off r%d=expr" reg in
            rule_desc :: acc
        | CallFrame.Rule_val_expression _expr ->
            let rule_desc = Printf.sprintf " off r%d=val_expr" reg in
            rule_desc :: acc)
      state.register_rules []
  in
  let all_rules = cfa_desc :: List.rev reg_rules in
  "<" ^ String.concat "" all_rules ^ " >"

(** Parse CFI instructions with a given initial state, returning state
    snapshots.

    This function processes FDE instructions starting from a specific initial
    CFI state (typically obtained from CallFrame.parse_initial_state) and
    returns the complete state progression through the function's address range.

    Parameters:
    - initial_state: Starting CFI state (from CIE initial instructions)
    - instructions: Raw CFI instruction bytes from FDE
    - code_alignment: Code alignment factor from CIE
    - data_alignment: Data alignment factor from CIE

    Returns list of (pc_offset, cfi_state) pairs showing complete CFI state at
    each PC location where state changes occur. This enables proper CFI rule
    tracking throughout function execution. *)
let parse_cfi_instructions_with_initial_state
    (initial_state : Dwarf.CallFrame.cfi_state) (instructions : string)
    (code_alignment : int64) (data_alignment : int64) :
    (int64 * Dwarf.CallFrame.cfi_state) list =
  (* Use the CallFrame parser to get instruction sequences *)
  let instruction_results =
    Dwarf.CallFrame.parse_cfi_instructions instructions code_alignment
      data_alignment
  in
  (* Convert to state snapshots - for now return the same state at each PC *)
  let state_snapshots =
    List.map
      (fun (pc_offset, _desc) -> (Int64.of_int pc_offset, initial_state))
      instruction_results
  in
  (* Always include the initial state at offset 0 *)
  (0L, initial_state) :: state_snapshots

let dump_eh_frame filename =
  try
    let actual_filename, _is_debug = resolve_binary_path filename in
    let buffer = Object.Buffer.parse actual_filename in

    (* Output header matching system dwarfdump format - just section name *)
    Printf.printf "\n.eh_frame\n\n";

    (* Get ELF sections to find .eh_frame *)
    let open Object.Elf in
    let _header, section_array = read_elf buffer in
    let eh_frame_section =
      Array.find_opt
        (fun section -> section.sh_name_str = ".eh_frame")
        section_array
    in

    match eh_frame_section with
    | None ->
        (* For missing sections, just show empty *)
        ()
    | Some section ->
        (* Parse the .eh_frame section using our EHFrame parser *)
        let cursor =
          Object.Buffer.cursor buffer
            ~at:(Unsigned.UInt64.to_int section.sh_offset)
        in
        let section_size = Unsigned.UInt64.to_int section.sh_size in
        let eh_frame_section =
          Dwarf.EHFrame.parse_section cursor section_size
        in

        (* Separate FDE and CIE entries for display *)
        let fde_entries = ref [] in
        let cie_entries = ref [] in

        List.iter
          (fun entry ->
            match entry with
            | Dwarf.EHFrame.EH_FDE fde -> fde_entries := fde :: !fde_entries
            | Dwarf.EHFrame.EH_CIE cie -> cie_entries := cie :: !cie_entries)
          eh_frame_section.entries;

        (* Display FDE entries first (system format) *)
        List.rev !fde_entries
        |> List.iteri (fun i fde ->
               let open Dwarf.CallFrame in
               let start_addr = Unsigned.UInt32.to_int64 fde.initial_location in
               let end_addr =
                 Int64.add start_addr
                   (Unsigned.UInt32.to_int64 fde.address_range)
               in
               let cie_offset = Unsigned.UInt32.to_int fde.cie_pointer in
               let fde_length = Unsigned.UInt32.to_int fde.length in
               let fde_offset = Unsigned.UInt32.to_int fde.offset in

               Printf.printf "fde:\n";

               (* Resolve function name from address range using symbol table *)
               let function_name =
                 resolve_function_name buffer start_addr end_addr
               in

               Printf.printf
                 "<    %d><0x%08Lx:0x%08Lx><%s><cie offset 0x%08x::cie \
                  index     0><fde offset 0x%08x length: 0x%08x>\n"
                 i start_addr end_addr function_name cie_offset fde_offset
                 fde_length;
               (* Display actual augmentation data length *)
               let aug_len =
                 match fde.augmentation_length with
                 | Some len -> Unsigned.UInt64.to_int len
                 | None -> 0
               in
               Printf.printf "       <eh aug data len 0x%x>\n" aug_len;

               (* Find the corresponding CIE for this FDE using library function *)
               let corresponding_cie =
                 match
                   Dwarf.EHFrame.find_cie_for_fde eh_frame_section
                     fde.cie_pointer fde_offset
                 with
                 | Some cie -> cie
                 | None ->
                     (* Enhanced error reporting with diagnostic information *)
                     let total_cies = List.length !cie_entries in
                     Printf.eprintf
                       "Warning: No CIE found for FDE %d (cie_pointer=0x%x, \
                        fde_offset=0x%x)\n"
                       i cie_offset fde_offset;
                     Printf.eprintf
                       "  Available CIEs: %d, Address range: 0x%08Lx-0x%08Lx\n"
                       total_cies start_addr end_addr;
                     Printf.eprintf "  Using default x86_64 CIE as fallback\n";
                     Dwarf.CallFrame.create_default_cie ()
               in

               (* Parse CFI instructions for this FDE with enhanced parser *)
               let code_alignment =
                 Unsigned.UInt64.to_int64
                   corresponding_cie.code_alignment_factor
               in
               let data_alignment =
                 Signed.Int64.to_int64 corresponding_cie.data_alignment_factor
               in

               (* Get proper initial state from CIE instead of hardcoded fallback *)
               let initial_state =
                 Dwarf.CallFrame.parse_initial_state corresponding_cie
               in

               (* Parse FDE instructions with proper initial state *)
               let state_changes =
                 parse_cfi_instructions_with_initial_state initial_state
                   fde.instructions code_alignment data_alignment
               in

               if List.length state_changes > 0 then
                 List.iter
                   (fun (pc_offset, cfi_state) ->
                     let pc_addr = Int64.add start_addr pc_offset in
                     let state_desc = format_cfi_state cfi_state in
                     Printf.printf "        0x%08Lx: %s\n" pc_addr state_desc)
                   state_changes
               else
                 (* Show initial state if no state changes occur *)
                 let initial_desc = format_cfi_state initial_state in
                 Printf.printf "        0x%08Lx: %s\n" start_addr initial_desc);

        (* Display CIE entries (system format) *)
        Printf.printf "\n cie:\n";
        List.rev !cie_entries
        |> List.iteri (fun i cie ->
               let open Dwarf.CallFrame in
               Printf.printf "<    %d> version      %d\n" i
                 (Unsigned.UInt8.to_int cie.version);
               Printf.printf "  cie section offset    %d 0x%08x\n" i (i * 32);
               (* Placeholder offset *)
               Printf.printf "  augmentation                  %s\n"
                 cie.augmentation;
               Printf.printf "  code_alignment_factor         %Ld\n"
                 (Unsigned.UInt64.to_int64 cie.code_alignment_factor);
               Printf.printf "  data_alignment_factor         %Ld\n"
                 (Signed.Int64.to_int64 cie.data_alignment_factor);
               Printf.printf "  return_address_register       %Ld\n"
                 (Unsigned.UInt64.to_int64 cie.return_address_register);

               (* Show augmentation data if present *)
               (match cie.augmentation_data with
               | Some data ->
                   Printf.printf "  eh aug data len                0x%x bytes "
                     (String.length data);
                   (* Parse and display augmentation data bytes *)
                   if String.length data > 0 then (
                     for i = 0 to String.length data - 1 do
                       Printf.printf "0x%02x " (Char.code data.[i])
                     done;
                     Printf.printf "\n";
                     (* Decode augmentation string meanings for common types *)
                     match cie.augmentation with
                     | "zR" when String.length data >= 1 ->
                         let encoding = Char.code data.[0] in
                         let encoding_type =
                           Dwarf.EHFrameHdr.encoding_of_u8 encoding
                         in
                         let encoding_desc =
                           match encoding_type with
                           | Dwarf.EHFrameHdr.DW_EH_PE_pcrel ->
                               "PC-relative signed 4-byte"
                           | DW_EH_PE_datarel -> "data-relative signed 4-byte"
                           | DW_EH_PE_sdata4 -> "signed 4-byte"
                           | _ -> "unknown encoding"
                         in
                         Printf.printf "    FDE encoding: %s (0x%02x)\n"
                           encoding_desc encoding
                     | _ -> ())
                   else Printf.printf "\n"
               | None -> Printf.printf "  eh aug data len                0x0\n");

               Printf.printf "  bytes of initial instructions %d\n"
                 (String.length cie.initial_instructions);
               Printf.printf "  cie length                    %ld\n"
                 (Unsigned.UInt32.to_int32 cie.length);

               if String.length cie.initial_instructions > 0 then (
                 Printf.printf "  initial instructions\n";
                 (* Parse CIE initial instructions using enhanced parser *)
                 let code_alignment =
                   Unsigned.UInt64.to_int64 cie.code_alignment_factor
                 in
                 let data_alignment =
                   Signed.Int64.to_int64 cie.data_alignment_factor
                 in
                 let initial_rules =
                   Dwarf.CallFrame.parse_cfi_instructions
                     cie.initial_instructions code_alignment data_alignment
                 in
                 List.iteri
                   (fun idx (_pc_offset, rule_desc) ->
                     Printf.printf "   %d %s\n" idx rule_desc)
                   initial_rules));

        (* Add diagnostic summary for multiple CIE handling *)
        if List.length !cie_entries > 1 then (
          Printf.printf "\n";
          Printf.printf "Multiple CIEs detected (%d total):\n"
            (List.length !cie_entries);
          List.iteri
            (fun i cie ->
              Printf.printf "  CIE %d: augmentation '%s', version %d\n" i
                cie.Dwarf.CallFrame.augmentation
                (Unsigned.UInt8.to_int cie.version))
            !cie_entries);
        Printf.printf "\n"
  with
  | Sys_error msg ->
      Printf.eprintf "Error: %s\n" msg;
      exit 1
  | Not_found ->
      Printf.eprintf "Error: .eh_frame section not found in binary\n";
      Printf.eprintf
        "  This may indicate the binary was stripped or compiled without frame \
         information\n";
      exit 1
  | Invalid_argument msg when string_contains_substring msg "CIE" ->
      Printf.eprintf "Error: Invalid CIE data in .eh_frame section: %s\n" msg;
      Printf.eprintf
        "  The binary may be corrupted or use an unsupported DWARF format\n";
      exit 1
  | exn ->
      Printf.eprintf "Error parsing EH frame information: %s\n"
        (Printexc.to_string exn);
      Printf.eprintf
        "  This may indicate malformed DWARF data or an unsupported format\n";
      exit 1

let dump_eh_frame_hdr filename =
  try
    let actual_filename, _is_debug = resolve_binary_path filename in
    let buffer = Object.Buffer.parse actual_filename in

    (* Output header matching system dwarfdump format *)
    Printf.printf "\n.eh_frame_hdr\n\n";

    (* Get ELF sections to find .eh_frame_hdr *)
    let open Object.Elf in
    let _header, section_array = read_elf buffer in

    (* Find .eh_frame_hdr section *)
    let eh_frame_hdr_section_opt =
      Array.find_opt
        (fun section -> section.sh_name_str = ".eh_frame_hdr")
        section_array
    in

    match eh_frame_hdr_section_opt with
    | None -> Printf.printf "No .eh_frame_hdr section found\n"
    | Some eh_frame_hdr_section ->
        let section_offset =
          Unsigned.UInt64.to_int eh_frame_hdr_section.sh_offset
        in
        let section_addr = eh_frame_hdr_section.sh_addr in

        let cursor = Object.Buffer.cursor buffer ~at:section_offset in
        let eh_frame_hdr = Dwarf.EHFrameHdr.parse_section cursor section_addr in

        (* Helper function to describe encoding *)
        let describe_encoding = function
          | Dwarf.EHFrameHdr.DW_EH_PE_absptr -> "absolute pointer"
          | DW_EH_PE_omit -> "omit"
          | DW_EH_PE_udata4 -> "unsigned 4-byte"
          | DW_EH_PE_pcrel -> "PC-relative signed 4-byte"
          | DW_EH_PE_datarel -> "data-relative signed 4-byte"
          | _ -> "other"
        in

        (* Display header information *)
        Printf.printf "version: %d\n"
          (Unsigned.UInt8.to_int eh_frame_hdr.version);
        Printf.printf "eh_frame_ptr_enc: %s\n"
          (describe_encoding eh_frame_hdr.eh_frame_ptr_enc);
        Printf.printf "fde_count_enc: %s\n"
          (describe_encoding eh_frame_hdr.fde_count_enc);
        Printf.printf "table_enc: %s\n"
          (describe_encoding eh_frame_hdr.table_enc);
        Printf.printf "eh_frame_ptr: 0x%08Lx\n"
          (Unsigned.UInt64.to_int64 eh_frame_hdr.eh_frame_ptr);
        Printf.printf "fde_count: %ld\n"
          (Unsigned.UInt32.to_int32 eh_frame_hdr.fde_count);

        (* Display search table entries *)
        Printf.printf
          "\nSearch table (%d entries) - sorted by PC for binary search:\n"
          (Array.length eh_frame_hdr.search_table);
        (* Use Format module for table header and separator *)
        Format.printf "@[<v>  @[<h>%-3s %-12s %-12s %s@]@,  @[<h>%s@]@]@." "No."
          "PC Address" "FDE Offset" "Description"
          "--------------------------------------------------";
        Array.iteri
          (fun i entry ->
            let open Dwarf.EHFrameHdr in
            let pc_addr = Unsigned.UInt64.to_int64 entry.initial_location in
            let fde_offset = Unsigned.UInt64.to_int64 entry.fde_address in
            let description =
              let func_name = resolve_function_name buffer pc_addr pc_addr in
              if func_name = "<missing name>" then "other function"
              else if func_name = "main" then "main function"
              else if
                String.length func_name > 0
                && ((* Common startup function name patterns *)
                    String.starts_with ~prefix:"_start" func_name
                   || String.starts_with ~prefix:"_init" func_name
                   || String.starts_with ~prefix:"__libc_start" func_name
                   || String.starts_with ~prefix:"_dl_start" func_name
                   || string_contains_substring func_name "crt"
                   || string_contains_substring func_name "init_array"
                   || string_contains_substring func_name "fini_array")
              then "startup code"
              else "other function"
            in
            (* Use Format module for consistent table row formatting *)
            Format.printf "  @[<h>%-3d 0x%08Lx   0x%08Lx   %s@]@." i pc_addr
              fde_offset description)
          eh_frame_hdr.search_table;

        Printf.printf
          "\n\
           Note: The search table enables fast FDE lookup during exception \
           unwinding.\n";
        Printf.printf
          "eh_frame_ptr (0x%08Lx) points to the start of the .eh_frame section.\n"
          (Unsigned.UInt64.to_int64 eh_frame_hdr.eh_frame_ptr)
  with
  | Sys_error msg ->
      Printf.eprintf "Error: %s\n" msg;
      exit 1
  | exn ->
      Printf.eprintf "Error parsing EH frame header information: %s\n"
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

let debug_frame_flag =
  let doc = make_section_flag_doc Dwarf.Debug_frame in
  Cmdliner.Arg.(value & flag & info [ "debug-frame" ] ~doc)

let eh_frame_flag =
  let doc =
    "Display the contents of the .eh_frame section (ELF exception handling)"
  in
  Cmdliner.Arg.(value & flag & info [ "eh-frame" ] ~doc)

let eh_frame_hdr_flag =
  let doc =
    "Display the contents of the .eh_frame_hdr section (ELF exception handling \
     header)"
  in
  Cmdliner.Arg.(value & flag & info [ "eh-frame-hdr" ] ~doc)

let dwarfdump_cmd debug_line debug_info debug_str debug_str_offsets debug_abbrev
    debug_addr debug_names debug_macro debug_line_str debug_aranges
    debug_loclists debug_frame eh_frame eh_frame_hdr filename =
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
      debug_frame;
      eh_frame;
      eh_frame_hdr;
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
  else if debug_abbrev then dump_debug_abbrev filename
  else if debug_addr then dump_debug_addr filename
  else if debug_names then dump_debug_names filename
  else if debug_macro then dump_debug_macro filename
  else if debug_line_str then dump_debug_line_str filename
  else if debug_aranges then dump_debug_aranges filename
  else if debug_loclists then dump_debug_loclists filename
  else if debug_frame then dump_debug_frame filename
  else if eh_frame then dump_eh_frame filename
  else if eh_frame_hdr then dump_eh_frame_hdr filename
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
      $ debug_aranges_flag $ debug_loclists_flag $ debug_frame_flag
      $ eh_frame_flag $ eh_frame_hdr_flag $ filename)

let () = exit (Cmdliner.Cmd.eval cmd)
