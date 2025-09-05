(* dwprod is a simple utility to find the DW_AT_producer for all compilation
   units within a shared library or executable.

   The DW_AT_producer is an attribute within DWARF debug info that says what
   compiler was used to create each compilation unit that ended up within a
   given shared library or executable.
*)

open Durin

let process_file filename =
  try
    (* Parse the object file *)
    let buffer = Object.Buffer.parse filename in
    let dwarf = Dwarf.create buffer in

    (* Get compilation units *)
    let units = Dwarf.parse_compile_units dwarf in

    (* Process each compilation unit *)
    List.iteri
      (fun i unit ->
        Printf.printf "\nCompilation Unit %d:\n" (i + 1);

        (* Get the abbreviation table for this unit *)
        let parsed_data = Dwarf.CompileUnit.parsed_data unit in
        let _dwarf_with_abbrevs, abbrev_table =
          Dwarf.get_abbrev_table dwarf
            (Unsigned.UInt64.of_uint32 parsed_data.debug_abbrev_offset)
        in

        (* Try to get the producer *)
        match Dwarf.CompileUnit.get_producer unit abbrev_table with
        | Some producer -> Printf.printf "  Producer: %s\n" producer
        | None -> Printf.printf "  Producer: <not found>\n")
      units
  with exn ->
    Printf.printf "Error processing %s: %s\n" filename (Printexc.to_string exn)

(* Command line interface *)
let filename =
  let doc = "Binary file to analyze for DWARF debug information" in
  Cmdliner.Arg.(required & pos 0 (some file) None & info [] ~docv:"FILE" ~doc)

let cmd =
  let doc = "A DWARF debugging information dumper" in
  let info = Cmdliner.Cmd.info "dwarfdump" ~doc in
  Cmdliner.Cmd.v info Cmdliner.Term.(const process_file $ filename)

let () = exit (Cmdliner.Cmd.eval cmd)
