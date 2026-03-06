let find_section binary_path name =
  let buffer = Object.Buffer.parse binary_path in
  let _header, sections = Object.Elf.read_elf buffer in
  match
    Array.find_opt
      (fun (s : Object.Elf.section) -> s.sh_name_str = name)
      sections
  with
  | None -> None
  | Some section -> Some (buffer, section)

let binary_path ~doc =
  Cmdliner.Arg.(
    required & opt (some file) None & info [ "binary"; "b" ] ~doc ~docv:"BINARY")
