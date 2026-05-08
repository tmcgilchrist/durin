open Alcotest
open Durin

let parse_sframe binary_path =
  match Test_helpers.find_section binary_path ".sframe" with
  | None -> fail "expected .sframe section"
  | Some (buffer, section) ->
      let offset = Unsigned.UInt64.to_int section.sh_offset in
      let size = Unsigned.UInt64.to_int section.sh_size in
      let cur = Object.Buffer.cursor ~at:offset buffer in
      Dwarf.SFrame.parse cur size

let test_section_present binary_path =
  match Test_helpers.find_section binary_path ".sframe" with
  | None -> fail ".sframe section missing — toolchain may not support SFrame"
  | Some (_, section) ->
      check bool "non-empty section" true
        (Unsigned.UInt64.to_int section.sh_size > 0)

let test_header_basic binary_path =
  let t = parse_sframe binary_path in
  check int "magic" 0xdee2 (Unsigned.UInt16.to_int t.header.preamble.magic);
  check int "version 2" 2 (Unsigned.UInt8.to_int t.header.preamble.version);
  check bool "AMD64-LE ABI on x86_64" true
    (t.header.abi_arch = Dwarf.SFrame.Amd64_le)

let test_header_amd64_ra_offset binary_path =
  (* On AMD64 the return address sits at CFA-8. *)
  let t = parse_sframe binary_path in
  check int "CFA fixed RA offset is -8" (-8) t.header.cfa_fixed_ra_offset

let test_header_fde_sorted binary_path =
  let t = parse_sframe binary_path in
  check bool "FDE_SORTED flag set" true t.header.preamble.flags.fde_sorted

let test_header_func_start_pcrel binary_path =
  (* binutils 2.42+ enables FUNC_START_PCREL by default when emitting .sframe.
     Anything we build with -Wa,--gsframe on a modern toolchain has it set. *)
  let t = parse_sframe binary_path in
  check bool "FUNC_START_PCREL flag set" true
    t.header.preamble.flags.func_start_pcrel

let test_fdes_present binary_path =
  let t = parse_sframe binary_path in
  let n = Unsigned.UInt32.to_int t.header.num_fdes in
  check bool "at least one FDE" true (n > 0);
  check int "fdes array matches header count" n (Array.length t.fdes)

let test_fre_total_matches_header binary_path =
  let t = parse_sframe binary_path in
  let total =
    Array.fold_left
      (fun acc (fde : Dwarf.SFrame.fde) ->
        acc + Unsigned.UInt32.to_int fde.func_num_fres)
      0 t.fdes
  in
  let header_total = Unsigned.UInt32.to_int t.header.num_fres in
  check int "sum of FDE fre counts matches header" header_total total

let test_decode_all_fres binary_path =
  (* Every FDE's FREs should decode without raising, and yield exactly
     func_num_fres entries each. *)
  let t = parse_sframe binary_path in
  Array.iter
    (fun (fde : Dwarf.SFrame.fde) ->
      let fres = Dwarf.SFrame.fres_of_fde t fde in
      check int "fre count matches func_num_fres"
        (Unsigned.UInt32.to_int fde.func_num_fres)
        (Array.length fres);
      Array.iter
        (fun (fre : Dwarf.SFrame.fre) ->
          check bool "offset_count in 1..3" true
            (fre.info.offset_count >= 1 && fre.info.offset_count <= 3);
          check int "offsets array length matches" fre.info.offset_count
            (Array.length fre.offsets))
        fres)
    t.fdes

let test_parse_from_buffer binary_path =
  let buffer = Object.Buffer.parse binary_path in
  match Dwarf.SFrame.parse_from_buffer buffer with
  | None -> fail "parse_from_buffer returned None"
  | Some (t, section_addr) ->
      check bool "non-empty FDEs" true (Array.length t.fdes > 0);
      check bool "section_addr is positive" true (section_addr > 0)

let test_roundtrip_fixture binary_path =
  match Test_helpers.find_section binary_path ".sframe" with
  | None -> fail "expected .sframe section"
  | Some (buffer, section) ->
      let offset = Unsigned.UInt64.to_int section.sh_offset in
      let size = Unsigned.UInt64.to_int section.sh_size in
      let cur = Object.Buffer.cursor ~at:offset buffer in
      let t = Dwarf.SFrame.parse cur size in
      let written = Dwarf.SFrame.Write.to_bytes t in
      check int "written size matches section size" size (Bytes.length written);
      let original = Bytes.create size in
      for i = 0 to size - 1 do
        Bytes.set original i
          (Char.chr (Bigarray.Array1.unsafe_get buffer (offset + i)))
      done;
      check bool "byte-for-byte roundtrip" true (Bytes.equal original written)

let binary_path = Test_helpers.binary_path ~doc:"Path to SFrame test binary"

let () =
  run_with_args "sframe integration" binary_path
    [
      ("section", [ ("present and non-empty", `Quick, test_section_present) ]);
      ( "header",
        [
          ("magic + version + ABI", `Quick, test_header_basic);
          ("CFA fixed RA offset", `Quick, test_header_amd64_ra_offset);
          ("FDE_SORTED flag", `Quick, test_header_fde_sorted);
          ("FUNC_START_PCREL flag", `Quick, test_header_func_start_pcrel);
        ] );
      ( "fdes",
        [
          ("FDE count > 0", `Quick, test_fdes_present);
          ("FRE total matches header", `Quick, test_fre_total_matches_header);
          ("decode every FDE's FREs", `Quick, test_decode_all_fres);
        ] );
      ( "convenience",
        [ ("parse_from_buffer works", `Quick, test_parse_from_buffer) ] );
      ( "roundtrip",
        [ ("byte-for-byte fixture roundtrip", `Quick, test_roundtrip_fixture) ]
      );
    ]
