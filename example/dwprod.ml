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
    Seq.iteri
      (fun i unit ->
        Printf.printf "\nCompilation Unit %d:\n" (i + 1);

        (* TODO: Implement producer extraction using DIE.get_producer on root DIE *)
        (* For now, just get some basic info to show the unit exists *)
        let parsed_data = Dwarf.CompileUnit.header unit in
        Printf.printf "  Unit at offset: 0x%x\n"
          (Unsigned.UInt32.to_int parsed_data.debug_abbrev_offset);
        Printf.printf "  Producer: <TODO: not implemented>\n")
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
