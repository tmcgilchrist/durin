open Alcotest
open Durin

let test_eh_frame_parse_succeeds binary_path =
  match Test_helpers.find_section binary_path ".eh_frame" with
  | None -> fail "expected .eh_frame section"
  | Some (buffer, section) ->
      let offset = Unsigned.UInt64.to_int section.sh_offset in
      let size = Unsigned.UInt64.to_int section.sh_size in
      let cur = Object.Buffer.cursor ~at:offset buffer in
      let parsed = Dwarf.EHFrame.parse_section cur size in
      check bool "has entries" true (List.length parsed.entries > 0)

let test_eh_frame_has_cie binary_path =
  match Test_helpers.find_section binary_path ".eh_frame" with
  | None -> fail "expected .eh_frame section"
  | Some (buffer, section) ->
      let offset = Unsigned.UInt64.to_int section.sh_offset in
      let size = Unsigned.UInt64.to_int section.sh_size in
      let cur = Object.Buffer.cursor ~at:offset buffer in
      let parsed = Dwarf.EHFrame.parse_section cur size in
      let has_cie =
        List.exists
          (fun e -> match e with Dwarf.EHFrame.EH_CIE _ -> true | _ -> false)
          parsed.entries
      in
      check bool "has at least one CIE" true has_cie

let test_eh_frame_has_fde binary_path =
  match Test_helpers.find_section binary_path ".eh_frame" with
  | None -> fail "expected .eh_frame section"
  | Some (buffer, section) ->
      let offset = Unsigned.UInt64.to_int section.sh_offset in
      let size = Unsigned.UInt64.to_int section.sh_size in
      let cur = Object.Buffer.cursor ~at:offset buffer in
      let parsed = Dwarf.EHFrame.parse_section cur size in
      let has_fde =
        List.exists
          (fun e -> match e with Dwarf.EHFrame.EH_FDE _ -> true | _ -> false)
          parsed.entries
      in
      check bool "has at least one FDE" true has_fde

let test_eh_frame_fde_fields_valid binary_path =
  match Test_helpers.find_section binary_path ".eh_frame" with
  | None -> fail "expected .eh_frame section"
  | Some (buffer, section) ->
      let offset = Unsigned.UInt64.to_int section.sh_offset in
      let size = Unsigned.UInt64.to_int section.sh_size in
      let cur = Object.Buffer.cursor ~at:offset buffer in
      let parsed = Dwarf.EHFrame.parse_section cur size in
      List.iter
        (fun e ->
          match e with
          | Dwarf.EHFrame.EH_FDE fde ->
              check bool "address_range > 0" true
                (Unsigned.UInt64.to_int64 fde.address_range > 0L)
          | _ -> ())
        parsed.entries

let test_eh_frame_hdr_parse_succeeds binary_path =
  match Test_helpers.find_section binary_path ".eh_frame_hdr" with
  | None -> fail "expected .eh_frame_hdr section"
  | Some (buffer, section) ->
      let offset = Unsigned.UInt64.to_int section.sh_offset in
      let cur = Object.Buffer.cursor ~at:offset buffer in
      let hdr = Dwarf.EHFrameHdr.parse_section cur section.sh_addr in
      check int "version is 1" 1 (Unsigned.UInt8.to_int hdr.version)

let test_eh_frame_hdr_has_entries binary_path =
  match Test_helpers.find_section binary_path ".eh_frame_hdr" with
  | None -> fail "expected .eh_frame_hdr section"
  | Some (buffer, section) ->
      let offset = Unsigned.UInt64.to_int section.sh_offset in
      let cur = Object.Buffer.cursor ~at:offset buffer in
      let hdr = Dwarf.EHFrameHdr.parse_section cur section.sh_addr in
      check bool "fde_count > 0" true (Unsigned.UInt32.to_int hdr.fde_count > 0);
      check bool "search_table non-empty" true
        (Array.length hdr.search_table > 0)

let binary_path = Test_helpers.binary_path ~doc:"Path to DWARF 5 test binary"

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
