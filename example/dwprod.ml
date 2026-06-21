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

    (* Print the DW_AT_producer of each compilation unit's root DIE. *)
    Seq.iteri
      (fun i unit ->
        let header = Dwarf.CompileUnit.header unit in
        let _, abbrev_table =
          Dwarf.get_abbrev_table dwarf header.debug_abbrev_offset
        in
        let producer =
          match Dwarf.CompileUnit.root_die unit abbrev_table buffer with
          | None -> "<no root DIE>"
          | Some root_die -> (
              match Dwarf.DIE.find_attribute root_die Dwarf.DW_AT_producer with
              | Some (Dwarf.DIE.String s)
              | Some (Dwarf.DIE.IndexedString (_, s)) ->
                  s
              | _ -> "<no DW_AT_producer>")
        in
        Printf.printf "Compilation Unit %d: %s\n" (i + 1) producer)
      units
  with exn ->
    Printf.printf "Error processing %s: %s\n" filename (Printexc.to_string exn)

(* Command line interface *)
let filename =
  let doc = "Binary file to analyze for DWARF debug information" in
  Cmdliner.Arg.(required & pos 0 (some file) None & info [] ~docv:"FILE" ~doc)

let cmd =
  let doc = "List the DW_AT_producer of each compilation unit in a binary" in
  let info = Cmdliner.Cmd.info "dwprod" ~doc in
  Cmdliner.Cmd.v info Cmdliner.Term.(const process_file $ filename)

let () = exit (Cmdliner.Cmd.eval cmd)
