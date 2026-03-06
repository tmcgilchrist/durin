open Alcotest
open Durin

let find_str_offsets_section binary_path =
  let buffer = Object.Buffer.parse binary_path in
  let _header, sections = Object.Elf.read_elf buffer in
  let section_opt =
    Array.find_opt
      (fun (s : Object.Elf.section) -> s.sh_name_str = ".debug_str_offsets")
      sections
  in
  match section_opt with
  | None -> None
  | Some section ->
      let offset = Unsigned.UInt64.to_int section.sh_offset in
      Some (buffer, offset)

let test_parse_succeeds binary_path =
  match find_str_offsets_section binary_path with
  | None -> fail "expected .debug_str_offsets section"
  | Some (buffer, offset) ->
      let t =
        Dwarf.DebugStrOffsets.parse buffer (Unsigned.UInt32.of_int offset)
      in
      check bool "has offsets" true (Array.length t.offsets > 0)

let test_header_valid binary_path =
  match find_str_offsets_section binary_path with
  | None -> fail "expected .debug_str_offsets section"
  | Some (buffer, offset) ->
      let t =
        Dwarf.DebugStrOffsets.parse buffer (Unsigned.UInt32.of_int offset)
      in
      let h = t.header in
      check int "version is 5" 5 (Unsigned.UInt16.to_int h.version);
      check int "padding is 0" 0 (Unsigned.UInt16.to_int h.padding);
      check bool "format is DWARF32" true (h.format = Dwarf.DWARF32);
      check bool "unit_length > 0" true
        (Unsigned.UInt64.to_int64 h.unit_length > 0L)

let test_offsets_non_negative binary_path =
  match find_str_offsets_section binary_path with
  | None -> fail "expected .debug_str_offsets section"
  | Some (buffer, offset) ->
      let t =
        Dwarf.DebugStrOffsets.parse buffer (Unsigned.UInt32.of_int offset)
      in
      Array.iteri
        (fun i (e : Dwarf.DebugStrOffsets.offset_entry) ->
          check bool
            (Printf.sprintf "offset[%d] >= 0" i)
            true
            (Unsigned.UInt64.to_int64 e.offset >= 0L))
        t.offsets

let test_strings_resolved binary_path =
  match find_str_offsets_section binary_path with
  | None -> fail "expected .debug_str_offsets section"
  | Some (buffer, offset) ->
      let t =
        Dwarf.DebugStrOffsets.parse buffer (Unsigned.UInt32.of_int offset)
      in
      let resolved_count =
        Array.fold_left
          (fun acc (e : Dwarf.DebugStrOffsets.offset_entry) ->
            if Option.is_some e.resolved_string then acc + 1 else acc)
          0 t.offsets
      in
      check bool "all strings resolved" true
        (resolved_count = Array.length t.offsets)

let test_known_string_present binary_path =
  match find_str_offsets_section binary_path with
  | None -> fail "expected .debug_str_offsets section"
  | Some (buffer, offset) ->
      let t =
        Dwarf.DebugStrOffsets.parse buffer (Unsigned.UInt32.of_int offset)
      in
      let has_main =
        Array.exists
          (fun (e : Dwarf.DebugStrOffsets.offset_entry) ->
            match e.resolved_string with
            | Some s -> String.equal s "main"
            | None -> false)
          t.offsets
      in
      check bool "contains resolved \"main\"" true has_main

let binary_path =
  let doc = "Path to DWARF 5 test binary" in
  Cmdliner.Arg.(
    required & opt (some file) None & info [ "binary"; "b" ] ~doc ~docv:"BINARY")

let () =
  run_with_args "debug_str_offsets integration" binary_path
    [
      ("parse", [ ("parse succeeds", `Quick, test_parse_succeeds) ]);
      ("header", [ ("header fields valid", `Quick, test_header_valid) ]);
      ( "offsets",
        [
          ("offsets non-negative", `Quick, test_offsets_non_negative);
          ("strings resolved", `Quick, test_strings_resolved);
          ("known string present", `Quick, test_known_string_present);
        ] );
    ]
