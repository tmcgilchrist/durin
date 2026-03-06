open Alcotest
open Durin

let find_debug_frame binary_path =
  let buffer = Object.Buffer.parse binary_path in
  let _header, sections = Object.Elf.read_elf buffer in
  let section_opt =
    Array.find_opt
      (fun (s : Object.Elf.section) -> s.sh_name_str = ".debug_frame")
      sections
  in
  match section_opt with
  | None -> None
  | Some section ->
      let offset = Unsigned.UInt64.to_int section.sh_offset in
      let size = Unsigned.UInt64.to_int section.sh_size in
      Some (buffer, offset, size)

let test_parse_succeeds binary_path =
  match find_debug_frame binary_path with
  | None -> fail "expected .debug_frame section"
  | Some (buffer, offset, size) ->
      let cur = Object.Buffer.cursor ~at:offset buffer in
      let section = Dwarf.CallFrame.parse_debug_frame_section cur size in
      check bool "entry_count > 0" true (section.entry_count > 0)

let test_has_cie binary_path =
  match find_debug_frame binary_path with
  | None -> fail "expected .debug_frame section"
  | Some (buffer, offset, size) ->
      let cur = Object.Buffer.cursor ~at:offset buffer in
      let section = Dwarf.CallFrame.parse_debug_frame_section cur size in
      let has_cie =
        List.exists
          (fun e -> match e with Dwarf.CallFrame.CIE _ -> true | _ -> false)
          section.entries
      in
      check bool "has at least one CIE" true has_cie

let test_has_fde binary_path =
  match find_debug_frame binary_path with
  | None -> fail "expected .debug_frame section"
  | Some (buffer, offset, size) ->
      let cur = Object.Buffer.cursor ~at:offset buffer in
      let section = Dwarf.CallFrame.parse_debug_frame_section cur size in
      let has_fde =
        List.exists
          (fun e -> match e with Dwarf.CallFrame.FDE _ -> true | _ -> false)
          section.entries
      in
      check bool "has at least one FDE" true has_fde

let test_cie_fields_valid binary_path =
  match find_debug_frame binary_path with
  | None -> fail "expected .debug_frame section"
  | Some (buffer, offset, size) ->
      let cur = Object.Buffer.cursor ~at:offset buffer in
      let section = Dwarf.CallFrame.parse_debug_frame_section cur size in
      List.iter
        (fun e ->
          match e with
          | Dwarf.CallFrame.CIE cie ->
              check int "version is 4" 4 (Unsigned.UInt8.to_int cie.version);
              check int "address_size is 8" 8
                (Unsigned.UInt8.to_int cie.address_size);
              check bool "code_alignment_factor > 0" true
                (Unsigned.UInt64.to_int64 cie.code_alignment_factor > 0L)
          | _ -> ())
        section.entries

let test_fde_fields_valid binary_path =
  match find_debug_frame binary_path with
  | None -> fail "expected .debug_frame section"
  | Some (buffer, offset, size) ->
      let cur = Object.Buffer.cursor ~at:offset buffer in
      let section = Dwarf.CallFrame.parse_debug_frame_section cur size in
      List.iter
        (fun e ->
          match e with
          | Dwarf.CallFrame.FDE fde ->
              check bool "initial_location > 0" true
                (Unsigned.UInt64.to_int64 fde.initial_location > 0L);
              check bool "address_range > 0" true
                (Unsigned.UInt64.to_int64 fde.address_range > 0L)
          | _ -> ())
        section.entries

let binary_path =
  let doc = "Path to DWARF 5 test binary" in
  Cmdliner.Arg.(
    required & opt (some file) None & info [ "binary"; "b" ] ~doc ~docv:"BINARY")

let () =
  run_with_args "debug_frame integration" binary_path
    [
      ("parse", [ ("parse succeeds", `Quick, test_parse_succeeds) ]);
      ( "entries",
        [
          ("has CIE", `Quick, test_has_cie);
          ("has FDE", `Quick, test_has_fde);
          ("CIE fields valid", `Quick, test_cie_fields_valid);
          ("FDE fields valid", `Quick, test_fde_fields_valid);
        ] );
    ]
