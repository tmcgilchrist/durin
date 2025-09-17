open Durin

let resolve_binary_path filename =
  if Sys.file_exists filename then (filename, false)
  else
    let dsym_path = filename ^ ".dSYM/Contents/Resources/DWARF/" in
    let binary_name = Filename.basename filename in
    let full_dsym_path = dsym_path ^ binary_name in
    if Sys.file_exists full_dsym_path then (full_dsym_path, true)
    else (filename, false)

let init_context filename =
  let actual_filename, is_dsym = resolve_binary_path filename in
  let buffer = Object.Buffer.parse actual_filename in
  let format_str = Dwarf.detect_format_and_arch buffer in
  (actual_filename, is_dsym, buffer, format_str)

let handle_errors f =
  try f () with
  | Sys_error msg ->
      Printf.eprintf "Error: %s\n" msg;
      exit 1
  | exn ->
      Printf.eprintf "Error parsing object file: %s\n" (Printexc.to_string exn);
      exit 1

(* Helper functions for printing unwind info sections *)
let print_header (header : Dwarf.CompactUnwind.unwind_info_header) =
  Printf.printf "  Version:                                   0x%lx\n"
    (Unsigned.UInt32.to_int32 header.Dwarf.CompactUnwind.version);
  Printf.printf "  Common encodings array section offset:     0x%lx\n"
    (Unsigned.UInt32.to_int32
       header.Dwarf.CompactUnwind.common_encodings_array_section_offset);
  Printf.printf "  Number of common encodings in array:       0x%lx\n"
    (Unsigned.UInt32.to_int32
       header.Dwarf.CompactUnwind.common_encodings_array_count);
  Printf.printf "  Personality function array section offset: 0x%lx\n"
    (Unsigned.UInt32.to_int32
       header.Dwarf.CompactUnwind.personality_array_section_offset);
  Printf.printf "  Number of personality functions in array:  0x%lx\n"
    (Unsigned.UInt32.to_int32 header.Dwarf.CompactUnwind.personality_array_count);
  Printf.printf "  Index array section offset:                0x%lx\n"
    (Unsigned.UInt32.to_int32 header.Dwarf.CompactUnwind.index_section_offset);
  Printf.printf "  Number of indices in array:                0x%lx\n"
    (Unsigned.UInt32.to_int32 header.Dwarf.CompactUnwind.index_count)

let print_common_encodings (header : Dwarf.CompactUnwind.unwind_info_header)
    (encodings : Dwarf.CompactUnwind.compact_unwind_encoding array) =
  Printf.printf "  Common encodings: (count = %ld)\n"
    (Unsigned.UInt32.to_int32
       header.Dwarf.CompactUnwind.common_encodings_array_count);
  Array.iteri
    (fun i encoding ->
      Printf.printf "    encoding[%d]: 0x%08lx\n" i
        (Unsigned.UInt32.to_int32 encoding))
    encodings

let print_personalities (header : Dwarf.CompactUnwind.unwind_info_header)
    (personalities : Types.u32 array) =
  Printf.printf "  Personality functions: (count = %ld)\n"
    (Unsigned.UInt32.to_int32 header.Dwarf.CompactUnwind.personality_array_count);
  Array.iteri
    (fun i personality ->
      Printf.printf "    personality[%d]: 0x%08lx\n" (i + 1)
        (Unsigned.UInt32.to_int32 personality))
    personalities

let print_index_entries (header : Dwarf.CompactUnwind.unwind_info_header)
    (index_entries :
      Dwarf.CompactUnwind.unwind_info_section_header_index_entry array) =
  Printf.printf "  Top level indices: (count = %ld)\n"
    (Unsigned.UInt32.to_int32 header.Dwarf.CompactUnwind.index_count);
  Array.iteri
    (fun i (entry : Dwarf.CompactUnwind.unwind_info_section_header_index_entry)
       ->
      Printf.printf
        "    [%d]: function offset=0x%08lx, 2nd level page offset=0x%08lx, \
         LSDA offset=0x%08lx\n"
        i
        (Unsigned.UInt32.to_int32 entry.Dwarf.CompactUnwind.function_offset)
        (Unsigned.UInt32.to_int32
           entry.Dwarf.CompactUnwind.second_level_page_section_offset)
        (Unsigned.UInt32.to_int32
           entry.Dwarf.CompactUnwind.lsda_index_array_section_offset))
    index_entries

let print_lsda_descriptors
    (lsda_descriptors : Dwarf.CompactUnwind.lsda_descriptor array) =
  Printf.printf "  LSDA descriptors:\n";
  Array.iteri
    (fun i descriptor ->
      Printf.printf "    [%d]: function offset=0x%08lx, LSDA offset=0x%08lx\n" i
        (Unsigned.UInt32.to_int32 descriptor.Dwarf.CompactUnwind.function_offset)
        (Unsigned.UInt32.to_int32 descriptor.Dwarf.CompactUnwind.lsda_offset))
    lsda_descriptors

let get_encoding_ref
    (entry : Dwarf.CompactUnwind.unwind_info_compressed_second_level_entry)
    (common_encodings : Dwarf.CompactUnwind.compact_unwind_encoding array)
    (page_encodings : Dwarf.CompactUnwind.compact_unwind_encoding array) =
  if Unsigned.UInt16.to_int entry.encoding_index < Array.length common_encodings
  then common_encodings.(Unsigned.UInt16.to_int entry.encoding_index)
  else if
    Unsigned.UInt16.to_int entry.encoding_index - Array.length common_encodings
    < Array.length page_encodings
  then
    page_encodings.(Unsigned.UInt16.to_int entry.encoding_index
                    - Array.length common_encodings)
  else Unsigned.UInt32.zero

let print_regular_page (page_idx : int)
    (corresponding_entry :
      Dwarf.CompactUnwind.unwind_info_section_header_index_entry option)
    (entries : Dwarf.CompactUnwind.unwind_info_regular_second_level_entry array)
    =
  let page_offset, base_func_offset =
    match corresponding_entry with
    | Some entry ->
        ( Unsigned.UInt32.to_int32
            entry.Dwarf.CompactUnwind.second_level_page_section_offset,
          Unsigned.UInt32.to_int32 entry.Dwarf.CompactUnwind.function_offset )
    | None -> (0l, 0l)
  in
  Printf.printf
    "    Second level index[%d]: offset in section=0x%08lx, base function \
     offset=0x%08lx\n"
    page_idx page_offset base_func_offset;
  Array.iteri
    (fun i (entry : Dwarf.CompactUnwind.unwind_info_regular_second_level_entry)
       ->
      Printf.printf "      [%d]: function offset=0x%08lx, encoding=0x%08lx\n" i
        (Unsigned.UInt32.to_int32 entry.function_offset)
        (Unsigned.UInt32.to_int32 entry.encoding))
    entries

let print_compressed_page (page_idx : int)
    (corresponding_entry :
      Dwarf.CompactUnwind.unwind_info_section_header_index_entry option)
    (encoding_array : Dwarf.CompactUnwind.compact_unwind_encoding array)
    (entries :
      Dwarf.CompactUnwind.unwind_info_compressed_second_level_entry array)
    (common_encodings : Dwarf.CompactUnwind.compact_unwind_encoding array) =
  let page_offset, base_func_offset =
    match corresponding_entry with
    | Some entry ->
        ( Unsigned.UInt32.to_int32
            entry.Dwarf.CompactUnwind.second_level_page_section_offset,
          Unsigned.UInt32.to_int32 entry.Dwarf.CompactUnwind.function_offset )
    | None -> (0l, 0l)
  in
  Printf.printf
    "    Second level index[%d]: offset in section=0x%08lx, base function \
     offset=0x%08lx\n"
    page_idx page_offset base_func_offset;
  Printf.printf "      Page encodings: (count = %d)\n"
    (Array.length encoding_array);
  Array.iteri
    (fun i encoding ->
      let global_index = i + Array.length common_encodings in
      Printf.printf "        encoding[%d]: 0x%08lx\n" global_index
        (Unsigned.UInt32.to_int32 encoding))
    encoding_array;
  Array.iteri
    (fun i
         (entry : Dwarf.CompactUnwind.unwind_info_compressed_second_level_entry)
       ->
      let encoding_ref =
        get_encoding_ref entry common_encodings encoding_array
      in
      let absolute_function_offset =
        Int32.add base_func_offset
          (Unsigned.UInt32.to_int32 entry.function_offset)
      in
      Printf.printf
        "      [%d]: function offset=0x%08lx, encoding[%d]=0x%08lx\n" i
        absolute_function_offset
        (Unsigned.UInt16.to_int entry.encoding_index)
        (Unsigned.UInt32.to_int32 encoding_ref))
    entries

let print_second_level_pages
    (pages : Dwarf.CompactUnwind.second_level_page array)
    (index_entries :
      Dwarf.CompactUnwind.unwind_info_section_header_index_entry array)
    (common_encodings : Dwarf.CompactUnwind.compact_unwind_encoding array) =
  Printf.printf "  Second level indices:\n";

  (* Create a function to get the nth valid entry without array conversion *)
  let get_valid_entry n =
    let rec find_nth_valid idx count =
      if idx >= Array.length index_entries then None
      else if
        not
          Unsigned.UInt32.(
            equal
              index_entries.(idx)
                .Dwarf.CompactUnwind.second_level_page_section_offset zero)
      then
        if count = n then Some index_entries.(idx)
        else find_nth_valid (idx + 1) (count + 1)
      else find_nth_valid (idx + 1) count
    in
    find_nth_valid 0 0
  in
  Array.iteri
    (fun page_idx page ->
      let corresponding_entry = get_valid_entry page_idx in
      match page with
      | Dwarf.CompactUnwind.Regular { entries; _ } ->
          print_regular_page page_idx corresponding_entry entries
      | Dwarf.CompactUnwind.Compressed { encoding_array; entries; _ } ->
          print_compressed_page page_idx corresponding_entry encoding_array
            entries common_encodings)
    pages

let dump_unwind_info filename =
  handle_errors (fun () ->
      let actual_filename, _is_dsym, buffer, format_str =
        init_context filename
      in
      print_newline ();
      Printf.printf "%s:\tfile format %s\n" actual_filename
        (String.lowercase_ascii format_str);
      Printf.printf "Unwind info:\n\n";

      match Dwarf.CompactUnwind.parse_from_buffer buffer with
      | None -> Printf.printf "No unwind information found\n"
      | Some (unwind_info, _arch) ->
          Printf.printf "Contents of __unwind_info section:\n";

          print_header unwind_info.header;

          print_common_encodings unwind_info.header unwind_info.common_encodings;

          print_personalities unwind_info.header unwind_info.personalities;

          print_index_entries unwind_info.header unwind_info.index_entries;

          print_lsda_descriptors unwind_info.lsda_descriptors;

          print_second_level_pages unwind_info.pages unwind_info.index_entries
            unwind_info.common_encodings)

(* Command line interface *)
let filename =
  let doc = "Binary file to analyze for object information" in
  Cmdliner.Arg.(required & pos 0 (some file) None & info [] ~docv:"FILE" ~doc)

let unwind_info_flag =
  let doc = "Display unwind information" in
  Cmdliner.Arg.(value & flag & info [ "unwind-info"; "u" ] ~doc)

let objdump_cmd unwind_info filename =
  if unwind_info then dump_unwind_info filename
  else
    Printf.printf
      "No option specified. Use --unwind-info to display unwind information.\n"

let cmd =
  let doc = "Display information from object files" in
  let info = Cmdliner.Cmd.info "objdump" ~doc in
  Cmdliner.Cmd.v info
    Cmdliner.Term.(const objdump_cmd $ unwind_info_flag $ filename)

let () = exit (Cmdliner.Cmd.eval cmd)
