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

(* Resolve an address to its (file, line) using the context's address layer. *)
let addr_to_location dwarf addr =
  match Dwarf.line_info_for_address dwarf addr with
  | Some { Dwarf.file; line; _ } -> (file, line)
  | None -> ("??", 0)

(* Find the name of the function covering an address via its subprogram DIE. *)
let find_function_name dwarf addr =
  match
    (Dwarf.unit_for_address dwarf addr, Dwarf.subprogram_for_address dwarf addr)
  with
  | Some cu, Some die ->
      Dwarf.attr_string (Dwarf.unit dwarf cu) die Dwarf.DW_AT_name
  | _ -> None

(* Main addr2line lookup function *)
let lookup_address dwarf addr_str show_functions =
  try
    let addr = Unsigned.UInt64.of_string addr_str in
    let filename, line = addr_to_location dwarf addr in
    if show_functions then
      let func_name =
        match find_function_name dwarf addr with
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
  let doc = "Executable file to analyse" in
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
    let dwarf = Dwarf.create buffer in
    if List.length addrs = 0 then
      (* Read from stdin *)
      try
        while true do
          let line = input_line stdin in
          let addr = String.trim line in
          if addr <> "" then lookup_address dwarf addr show_funcs
        done
      with End_of_file -> ()
    else
      (* Process command-line addresses *)
      List.iter (fun addr -> lookup_address dwarf addr show_funcs) addrs
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
