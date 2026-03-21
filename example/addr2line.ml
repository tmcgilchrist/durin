(* addr2line provides a cross-platform library for retrieving per-address
   debug information from files with DWARF debug information. Given an
   address, it can return the file name, line number, and function name
   associated with that address, as well as the inline call stack leading
   to that address.
 *)
open Durin

(* Helper function to resolve dSYM paths similar to other examples *)
let resolve_binary_path filename =
  if Sys.file_exists filename then (filename, false)
  else
    let dsym_path =
      filename ^ ".dSYM/Contents/Resources/DWARF/" ^ Filename.basename filename
    in
    if Sys.file_exists dsym_path then (dsym_path, true) else (filename, false)

(* Initialize DWARF context from file *)
let init_context filename =
  let actual_filename, _ = resolve_binary_path filename in
  if Sys.is_directory actual_filename then
    failwith (Printf.sprintf "'%s' is a directory" actual_filename)
  else
    let buffer = Object.Buffer.parse actual_filename in
    (buffer, actual_filename)

(* Find line table entry for a given address using binary search *)
let find_line_entry entries target_addr =
  let len = Array.length entries in
  let rec binary_search low high =
    if low > high then None
    else
      let mid = (low + high) / 2 in
      let entry = entries.(mid) in
      let addr = entry.Dwarf.DebugLine.address in
      if Unsigned.UInt64.equal addr target_addr then Some entry
      else if Unsigned.UInt64.compare target_addr addr < 0 then
        binary_search low (mid - 1)
      else if mid < len - 1 then
        let next_addr = entries.(mid + 1).Dwarf.DebugLine.address in
        if Unsigned.UInt64.compare target_addr next_addr < 0 then Some entry
        else binary_search (mid + 1) high
      else Some entry
  in
  if len = 0 then None else binary_search 0 (len - 1)

(* Get section offset helper *)
let get_section_offset buffer section_type =
  let object_format = Object_format.detect_format buffer in
  let section_name =
    Dwarf.object_format_to_section_name object_format section_type
  in
  try
    let open Macho in
    let _header, commands = read buffer in
    let sections = ref [] in
    List.iter
      (fun cmd ->
        match cmd with
        | LC_SEGMENT_64 (lazy segment) ->
            Array.iter
              (fun section ->
                if
                  String.equal section.sec_segname "__DWARF"
                  && String.equal section.sec_sectname section_name
                then
                  sections :=
                    ( Unsigned.UInt32.to_int section.sec_offset,
                      Unsigned.UInt64.to_int section.sec_size )
                    :: !sections)
              segment.seg_sections
        | LC_SEGMENT_32 (lazy segment) ->
            Array.iter
              (fun section ->
                if
                  String.equal section.sec_segname "__DWARF"
                  && String.equal section.sec_sectname section_name
                then
                  sections :=
                    ( Unsigned.UInt32.to_int section.sec_offset,
                      Unsigned.UInt64.to_int section.sec_size )
                    :: !sections)
              segment.seg_sections
        | _ -> ())
      commands;
    match !sections with
    | (offset, size) :: _ ->
        Some (Unsigned.UInt64.of_int offset, Unsigned.UInt64.of_int size)
    | [] -> None
  with _ -> None

(* Parse line table from debug_line section *)
let parse_line_table buffer =
  match get_section_offset buffer Dwarf.Debug_line with
  | None -> None
  | Some (offset, _size) ->
      let cursor =
        Object.Buffer.cursor buffer ~at:(Unsigned.UInt64.to_int offset)
      in
      let header = Dwarf.DebugLine.parse_line_program_header cursor buffer in
      let entries =
        Dwarf.DebugLine.parse_line_program cursor header |> Array.of_seq
      in
      Some (header, entries)

(* Resolve address to source location *)
let addr_to_location _buffer header entries addr =
  match find_line_entry entries addr with
  | None -> ("??", 0)
  | Some entry ->
      let file_index =
        Unsigned.UInt32.to_int entry.Dwarf.DebugLine.file_index
      in
      if file_index < Array.length header.Dwarf.DebugLine.file_names then
        let file_entry = header.Dwarf.DebugLine.file_names.(file_index) in
        let filename =
          if file_entry.directory = "" then file_entry.name
          else file_entry.directory ^ "/" ^ file_entry.name
        in
        let line = Unsigned.UInt32.to_int entry.Dwarf.DebugLine.line in
        (filename, line)
      else ("??", 0)

(* Resolve DIE address attribute considering addr_base *)
let resolve_die_address buffer addr_base addr_value =
  match addr_base with
  | Some base ->
      let index = Unsigned.UInt64.to_int addr_value in
      Dwarf.resolve_address_index buffer index base
  | None -> addr_value

(* Find function name for address by searching debug_info DIEs *)
let find_function_name buffer addr =
  try
    let dwarf = Dwarf.create buffer in
    let compile_units = Dwarf.parse_compile_units dwarf in
    let rec search_cu cu_seq =
      match cu_seq () with
      | Seq.Nil -> None
      | Seq.Cons (unit, rest) -> (
          let header = Dwarf.CompileUnit.header unit in
          let abbrev_offset = header.debug_abbrev_offset in
          let _, abbrev_table = Dwarf.get_abbrev_table dwarf abbrev_offset in
          match Dwarf.CompileUnit.root_die unit abbrev_table buffer with
          | None -> search_cu rest
          | Some root_die -> (
              (* Get addr_base from root DIE if present *)
              let addr_base =
                match
                  Dwarf.DIE.find_attribute root_die Dwarf.DW_AT_addr_base
                with
                | Some (Dwarf.DIE.UData base) -> Some base
                | _ -> None
              in
              let resolve_attr_address = function
                | Dwarf.DIE.Address a -> Some a
                | Dwarf.DIE.IndexedAddress (_, idx) ->
                    Some (resolve_die_address buffer addr_base idx)
                | _ -> None
              in
              let get_die_name die =
                match Dwarf.DIE.find_attribute die Dwarf.DW_AT_name with
                | Some (Dwarf.DIE.String name) -> Some name
                | Some (Dwarf.DIE.IndexedString (_, name)) -> Some name
                | _ -> None
              in
              let rec search_die die =
                (* Check if this DIE is a subprogram containing the address *)
                (match die.Dwarf.DIE.tag with
                | Dwarf.DW_TAG_subprogram | Dwarf.DW_TAG_inlined_subroutine -> (
                    let low_pc_opt =
                      Dwarf.DIE.find_attribute die Dwarf.DW_AT_low_pc
                      |> Option.map (fun v -> resolve_attr_address v)
                      |> Option.join
                    in
                    let high_pc_opt =
                      match
                        ( low_pc_opt,
                          Dwarf.DIE.find_attribute die Dwarf.DW_AT_high_pc )
                      with
                      | Some lpc, Some (Dwarf.DIE.UData offset) ->
                          Some (Unsigned.UInt64.add lpc offset)
                      | _, Some v -> resolve_attr_address v
                      | _ -> None
                    in
                    match (low_pc_opt, high_pc_opt) with
                    | Some low_pc, Some high_pc ->
                        if
                          Unsigned.UInt64.compare addr low_pc >= 0
                          && Unsigned.UInt64.compare addr high_pc < 0
                        then get_die_name die
                        else None
                    | _ -> None)
                | _ -> None)
                |> function
                | Some name -> Some name
                | None ->
                    (* Search children *)
                    let rec search_children children_seq =
                      match children_seq () with
                      | Seq.Nil -> None
                      | Seq.Cons (child, rest) -> (
                          match search_die child with
                          | Some name -> Some name
                          | None -> search_children rest)
                    in
                    search_children die.Dwarf.DIE.children
              in
              match search_die root_die with
              | Some name -> Some name
              | None -> search_cu rest))
    in
    search_cu compile_units
  with _ -> None

(* Main addr2line lookup function *)
let lookup_address buffer addr_str show_functions =
  try
    let addr = Unsigned.UInt64.of_string addr_str in
    match parse_line_table buffer with
    | None ->
        if show_functions then Printf.printf "??\n??:0\n"
        else Printf.printf "??:0\n"
    | Some (header, entries) ->
        let filename, line = addr_to_location buffer header entries addr in
        if show_functions then
          let func_name =
            match find_function_name buffer addr with
            | Some name -> name
            | None -> "??"
          in
          Printf.printf "%s\n%s:%d\n" func_name filename line
        else Printf.printf "%s:%d\n" filename line
  with _ ->
    if show_functions then Printf.printf "??\n??:0\n"
    else Printf.printf "??:0\n"

(* Command-line interface *)
let executable_file =
  let doc = "Executable file to analyze" in
  Cmdliner.Arg.(
    value & opt (some string) None & info [ "e"; "exe" ] ~docv:"FILE" ~doc)

let show_functions =
  let doc = "Show function names" in
  Cmdliner.Arg.(value & flag & info [ "f"; "functions" ] ~doc)

let show_inlines =
  let doc = "Unwind inlined functions" in
  Cmdliner.Arg.(value & flag & info [ "i"; "inlines" ] ~doc)

let pretty_print =
  let doc = "Make the output easier to read for humans" in
  Cmdliner.Arg.(value & flag & info [ "p"; "pretty-print" ] ~doc)

let basenames =
  let doc = "Strip directory names" in
  Cmdliner.Arg.(value & flag & info [ "s"; "basenames" ] ~doc)

let addresses =
  let doc = "Show addresses" in
  Cmdliner.Arg.(value & flag & info [ "a"; "addresses" ] ~doc)

let demangle =
  let doc = "Demangle function names" in
  Cmdliner.Arg.(value & flag & info [ "C"; "demangle" ] ~doc)

let addr_list =
  let doc = "Addresses to look up" in
  Cmdliner.Arg.(value & pos_all string [] & info [] ~docv:"ADDRESS" ~doc)

let addr2line_cmd exec_file show_funcs _inlines _pretty _base _addrs _dem addrs
    =
  let filename = match exec_file with Some f -> f | None -> "a.out" in
  try
    let buffer, _ = init_context filename in
    if List.length addrs = 0 then
      (* Read from stdin *)
      try
        while true do
          let line = input_line stdin in
          let addr = String.trim line in
          if addr <> "" then lookup_address buffer addr show_funcs
        done
      with End_of_file -> ()
    else
      (* Process command-line addresses *)
      List.iter (fun addr -> lookup_address buffer addr show_funcs) addrs
  with
  | Sys_error msg ->
      Printf.eprintf "Error: %s\n" msg;
      exit 1
  | Failure msg ->
      Printf.eprintf "Error: %s\n" msg;
      exit 1
  | exn ->
      Printf.eprintf "Error: %s\n" (Printexc.to_string exn);
      exit 1

let cmd =
  let doc = "Convert addresses to line number/file name pairs" in
  let info = Cmdliner.Cmd.info "addr2line" ~doc in
  Cmdliner.Cmd.v info
    Cmdliner.Term.(
      const addr2line_cmd $ executable_file $ show_functions $ show_inlines
      $ pretty_print $ basenames $ addresses $ demangle $ addr_list)

let () = exit (Cmdliner.Cmd.eval cmd)
