open Alcotest
open Durin

let find_section binary_path name =
  let buffer = Object.Buffer.parse binary_path in
  let _header, sections = Object.Elf.read_elf buffer in
  let section_opt =
    Array.find_opt
      (fun (s : Object.Elf.section) -> s.sh_name_str = name)
      sections
  in
  match section_opt with
  | None -> None
  | Some section ->
      let offset = Unsigned.UInt64.to_int section.sh_offset in
      let size = Unsigned.UInt64.to_int section.sh_size in
      let addr = Unsigned.UInt64.to_int64 section.sh_addr in
      Some (buffer, offset, size, addr)

let test_eh_frame_parse_succeeds binary_path =
  match find_section binary_path ".eh_frame" with
  | None -> fail "expected .eh_frame section"
  | Some (buffer, offset, size, _) ->
      let cur = Object.Buffer.cursor ~at:offset buffer in
      let section = Dwarf.EHFrame.parse_section cur size in
      check bool "has entries" true (List.length section.entries > 0)

let test_eh_frame_has_cie binary_path =
  match find_section binary_path ".eh_frame" with
  | None -> fail "expected .eh_frame section"
  | Some (buffer, offset, size, _) ->
      let cur = Object.Buffer.cursor ~at:offset buffer in
      let section = Dwarf.EHFrame.parse_section cur size in
      let has_cie =
        List.exists
          (fun e -> match e with Dwarf.EHFrame.EH_CIE _ -> true | _ -> false)
          section.entries
      in
      check bool "has at least one CIE" true has_cie

let test_eh_frame_has_fde binary_path =
  match find_section binary_path ".eh_frame" with
  | None -> fail "expected .eh_frame section"
  | Some (buffer, offset, size, _) ->
      let cur = Object.Buffer.cursor ~at:offset buffer in
      let section = Dwarf.EHFrame.parse_section cur size in
      let has_fde =
        List.exists
          (fun e -> match e with Dwarf.EHFrame.EH_FDE _ -> true | _ -> false)
          section.entries
      in
      check bool "has at least one FDE" true has_fde

let test_eh_frame_fde_fields_valid binary_path =
  match find_section binary_path ".eh_frame" with
  | None -> fail "expected .eh_frame section"
  | Some (buffer, offset, size, _) ->
      let cur = Object.Buffer.cursor ~at:offset buffer in
      let section = Dwarf.EHFrame.parse_section cur size in
      List.iter
        (fun e ->
          match e with
          | Dwarf.EHFrame.EH_FDE fde ->
              check bool "address_range > 0" true
                (Unsigned.UInt64.to_int64 fde.address_range > 0L)
          | _ -> ())
        section.entries

let test_eh_frame_hdr_parse_succeeds binary_path =
  match find_section binary_path ".eh_frame_hdr" with
  | None -> fail "expected .eh_frame_hdr section"
  | Some (buffer, offset, _size, addr) ->
      let cur = Object.Buffer.cursor ~at:offset buffer in
      let hdr =
        Dwarf.EHFrameHdr.parse_section cur (Unsigned.UInt64.of_int64 addr)
      in
      check int "version is 1" 1 (Unsigned.UInt8.to_int hdr.version)

let test_eh_frame_hdr_has_entries binary_path =
  match find_section binary_path ".eh_frame_hdr" with
  | None -> fail "expected .eh_frame_hdr section"
  | Some (buffer, offset, _size, addr) ->
      let cur = Object.Buffer.cursor ~at:offset buffer in
      let hdr =
        Dwarf.EHFrameHdr.parse_section cur (Unsigned.UInt64.of_int64 addr)
      in
      check bool "fde_count > 0" true (Unsigned.UInt32.to_int hdr.fde_count > 0);
      check bool "search_table non-empty" true
        (Array.length hdr.search_table > 0)

let binary_path =
  let doc = "Path to DWARF 5 test binary" in
  Cmdliner.Arg.(
    required & opt (some file) None & info [ "binary"; "b" ] ~doc ~docv:"BINARY")

let () =
  run_with_args "eh_frame integration" binary_path
    [
      ( "eh_frame",
        [
          ("parse succeeds", `Quick, test_eh_frame_parse_succeeds);
          ("has CIE", `Quick, test_eh_frame_has_cie);
          ("has FDE", `Quick, test_eh_frame_has_fde);
          ("FDE fields valid", `Quick, test_eh_frame_fde_fields_valid);
        ] );
      ( "eh_frame_hdr",
        [
          ("parse succeeds", `Quick, test_eh_frame_hdr_parse_succeeds);
          ("has search table entries", `Quick, test_eh_frame_hdr_has_entries);
        ] );
    ]
