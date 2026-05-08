open Alcotest
open Durin

let buffer_of_bytes (bytes : int list) : Object.Buffer.t =
  let filename = Filename.temp_file "sframe_test_" ".bin" in
  let oc = open_out_bin filename in
  List.iter (fun b -> output_char oc (Char.chr (b land 0xff))) bytes;
  close_out oc;
  let buffer = Object.Buffer.parse filename in
  Sys.remove filename;
  buffer

(* Little-endian byte builders. *)
let u8 (n : int) : int list = [ n land 0xff ]
let s8 (n : int) : int list = [ (if n < 0 then n + 0x100 else n) land 0xff ]
let u16 (n : int) : int list = [ n land 0xff; (n lsr 8) land 0xff ]

let u32 (n : int) : int list =
  [
    n land 0xff; (n lsr 8) land 0xff; (n lsr 16) land 0xff; (n lsr 24) land 0xff;
  ]

let s32 (n : int) : int list =
  let v = if n < 0 then n + 0x100000000 else n in
  u32 v

(* ---- Header / preamble construction helpers ---- *)

let make_header ~version ~flags_byte ~abi ~cfa_fp_off ~cfa_ra_off ~auxhdr_len
    ~num_fdes ~num_fres ~fre_len ~fde_off ~fre_off () : int list =
  u16 0xdee2 @ u8 version @ u8 flags_byte @ u8 abi @ s8 cfa_fp_off
  @ s8 cfa_ra_off @ u8 auxhdr_len @ u32 num_fdes @ u32 num_fres @ u32 fre_len
  @ u32 fde_off @ u32 fre_off

let make_fde_v2 ~addr ~size ~fre_off ~num_fres ~info_byte ~rep_size () :
    int list =
  s32 addr @ u32 size @ u32 fre_off @ u32 num_fres @ u8 info_byte @ u8 rep_size
  @ u16 0 (* padding *)

let make_fde_v1 ~addr ~size ~fre_off ~num_fres ~info_byte () : int list =
  s32 addr @ u32 size @ u32 fre_off @ u32 num_fres @ u8 info_byte

(* FDE info byte construction. fretype: 0=Addr1, 1=Addr2, 2=Addr4.
   fdetype: 0=Pcinc, 1=Pcmask. pauth_key: 0=A, 1=B. *)
let fde_info_byte ~fretype ~fdetype ~pauth_key : int =
  fretype lor (fdetype lsl 4) lor (pauth_key lsl 5)

(* FRE info byte. cfa_base: 1=Sp, 0=Fp (binutils convention; docs-2.41
   has the mapping inverted but real-world data uses SP=1, FP=0).
   offset_count: 1..3. offset_size: 0=1B, 1=2B, 2=4B. mangled_ra: bool. *)
let fre_info_byte ~cfa_base ~offset_count ~offset_size ~mangled_ra : int =
  let mr = if mangled_ra then 1 else 0 in
  cfa_base lor (offset_count lsl 1) lor (offset_size lsl 5) lor (mr lsl 7)

(* ---- Preamble tests ---- *)

let test_magic_correct () =
  let bytes =
    make_header ~version:2 ~flags_byte:0 ~abi:3 ~cfa_fp_off:0 ~cfa_ra_off:(-8)
      ~auxhdr_len:0 ~num_fdes:0 ~num_fres:0 ~fre_len:0 ~fde_off:0 ~fre_off:0 ()
  in
  let buf = buffer_of_bytes bytes in
  let cur = Object.Buffer.cursor buf ~at:0 in
  let t = Dwarf.SFrame.parse cur (List.length bytes) in
  check int "magic" 0xdee2 (Unsigned.UInt16.to_int t.header.preamble.magic);
  check int "version" 2 (Unsigned.UInt8.to_int t.header.preamble.version)

let test_magic_swapped_rejected () =
  (* Bytes 0xde 0xe2 read as LE u16 give 0xe2de — the swapped magic that
     indicates the section uses opposite endianness. *)
  let bytes =
    [ 0xde; 0xe2; 0x02; 0x00 ] (* preamble *) @ List.init 24 (fun _ -> 0x00)
    (* rest of header *)
  in
  let buf = buffer_of_bytes bytes in
  let cur = Object.Buffer.cursor buf ~at:0 in
  check_raises "swapped magic should raise"
    (Dwarf.SFrame.Invalid_sframe_format
       "SFrame section is in opposite endianness from host; big-endian support \
        is not yet implemented in Durin") (fun () ->
      ignore (Dwarf.SFrame.parse cur (List.length bytes)))

let test_magic_garbage_rejected () =
  let bytes = [ 0xab; 0xcd; 0x02; 0x00 ] @ List.init 24 (fun _ -> 0x00) in
  let buf = buffer_of_bytes bytes in
  let cur = Object.Buffer.cursor buf ~at:0 in
  match Dwarf.SFrame.parse cur (List.length bytes) with
  | exception Dwarf.SFrame.Invalid_sframe_format msg ->
      check bool "magic error mentions invalid" true
        (String.length msg > 0 && Astring.String.is_infix ~affix:"magic" msg)
  | _ -> fail "expected Invalid_sframe_format"

let test_version_unsupported_rejected () =
  let bytes =
    make_header ~version:3 ~flags_byte:0 ~abi:3 ~cfa_fp_off:0 ~cfa_ra_off:0
      ~auxhdr_len:0 ~num_fdes:0 ~num_fres:0 ~fre_len:0 ~fde_off:0 ~fre_off:0 ()
  in
  let buf = buffer_of_bytes bytes in
  let cur = Object.Buffer.cursor buf ~at:0 in
  match Dwarf.SFrame.parse cur (List.length bytes) with
  | exception Dwarf.SFrame.Invalid_sframe_format _ -> ()
  | _ -> fail "expected version rejection"

(* ---- Flag decoding ---- *)

let parse_minimal_v2 ?(flags_byte = 0) ?(abi = 3) ?(cfa_fp_off = 0)
    ?(cfa_ra_off = -8) () =
  let bytes =
    make_header ~version:2 ~flags_byte ~abi ~cfa_fp_off ~cfa_ra_off
      ~auxhdr_len:0 ~num_fdes:0 ~num_fres:0 ~fre_len:0 ~fde_off:0 ~fre_off:0 ()
  in
  let buf = buffer_of_bytes bytes in
  let cur = Object.Buffer.cursor buf ~at:0 in
  Dwarf.SFrame.parse cur (List.length bytes)

let test_flag_fde_sorted () =
  let t = parse_minimal_v2 ~flags_byte:0x1 () in
  check bool "fde_sorted set" true t.header.preamble.flags.fde_sorted;
  check bool "frame_pointer unset" false t.header.preamble.flags.frame_pointer

let test_flag_frame_pointer () =
  let t = parse_minimal_v2 ~flags_byte:0x2 () in
  check bool "frame_pointer set" true t.header.preamble.flags.frame_pointer;
  check bool "fde_sorted unset" false t.header.preamble.flags.fde_sorted

let test_flag_both () =
  let t = parse_minimal_v2 ~flags_byte:0x3 () in
  check bool "fde_sorted set" true t.header.preamble.flags.fde_sorted;
  check bool "frame_pointer set" true t.header.preamble.flags.frame_pointer

let test_flag_func_start_pcrel () =
  let t = parse_minimal_v2 ~flags_byte:0x4 () in
  check bool "func_start_pcrel set" true
    t.header.preamble.flags.func_start_pcrel;
  check bool "fde_sorted unset" false t.header.preamble.flags.fde_sorted

let test_flag_all_three () =
  let t = parse_minimal_v2 ~flags_byte:0x7 () in
  check bool "fde_sorted set" true t.header.preamble.flags.fde_sorted;
  check bool "frame_pointer set" true t.header.preamble.flags.frame_pointer;
  check bool "func_start_pcrel set" true
    t.header.preamble.flags.func_start_pcrel

(* ---- ABI decoding ---- *)

let test_abi_amd64_le () =
  let t = parse_minimal_v2 ~abi:3 () in
  check bool "abi is Amd64_le" true (t.header.abi_arch = Dwarf.SFrame.Amd64_le)

let test_abi_aarch64_le () =
  let t = parse_minimal_v2 ~abi:2 () in
  check bool "abi is Aarch64_le" true
    (t.header.abi_arch = Dwarf.SFrame.Aarch64_le)

let test_abi_aarch64_be () =
  let t = parse_minimal_v2 ~abi:1 () in
  check bool "abi is Aarch64_be" true
    (t.header.abi_arch = Dwarf.SFrame.Aarch64_be)

let test_abi_unknown_rejected () =
  match parse_minimal_v2 ~abi:42 () with
  | exception Dwarf.SFrame.Invalid_sframe_format _ -> ()
  | _ -> fail "expected ABI rejection"

(* ---- Signed cfa_fixed_*_offset ---- *)

let test_cfa_fixed_offsets_signed () =
  let t = parse_minimal_v2 ~cfa_fp_off:(-16) ~cfa_ra_off:(-8) () in
  check int "fp offset" (-16) t.header.cfa_fixed_fp_offset;
  check int "ra offset" (-8) t.header.cfa_fixed_ra_offset

(* ---- FDE V1 vs V2 layouts ---- *)

let test_fde_v1_layout () =
  (* V1 FDEs are 17 bytes — no rep_size or padding. *)
  let info = fde_info_byte ~fretype:0 ~fdetype:0 ~pauth_key:0 in
  let fde_bytes =
    make_fde_v1 ~addr:0x1000 ~size:64 ~fre_off:0 ~num_fres:0 ~info_byte:info ()
  in
  check int "v1 fde size" 17 (List.length fde_bytes);
  let header_bytes =
    make_header ~version:1 ~flags_byte:0 ~abi:3 ~cfa_fp_off:0 ~cfa_ra_off:(-8)
      ~auxhdr_len:0 ~num_fdes:1 ~num_fres:0 ~fre_len:0 ~fde_off:0 ~fre_off:17 ()
  in
  let bytes = header_bytes @ fde_bytes in
  let buf = buffer_of_bytes bytes in
  let cur = Object.Buffer.cursor buf ~at:0 in
  let t = Dwarf.SFrame.parse cur (List.length bytes) in
  check int "fde count" 1 (Array.length t.fdes);
  let fde = t.fdes.(0) in
  check int "fde addr" 0x1000 fde.func_start_address;
  check int "fde size" 64 (Unsigned.UInt32.to_int fde.func_size);
  check int "v1 rep_size is zero" 0 (Unsigned.UInt8.to_int fde.rep_size)

let test_fde_v2_layout () =
  let info = fde_info_byte ~fretype:0 ~fdetype:1 ~pauth_key:0 in
  let fde_bytes =
    make_fde_v2 ~addr:0x2000 ~size:128 ~fre_off:0 ~num_fres:0 ~info_byte:info
      ~rep_size:8 ()
  in
  check int "v2 fde size" 20 (List.length fde_bytes);
  let header_bytes =
    make_header ~version:2 ~flags_byte:0 ~abi:3 ~cfa_fp_off:0 ~cfa_ra_off:(-8)
      ~auxhdr_len:0 ~num_fdes:1 ~num_fres:0 ~fre_len:0 ~fde_off:0 ~fre_off:20 ()
  in
  let bytes = header_bytes @ fde_bytes in
  let buf = buffer_of_bytes bytes in
  let cur = Object.Buffer.cursor buf ~at:0 in
  let t = Dwarf.SFrame.parse cur (List.length bytes) in
  let fde = t.fdes.(0) in
  check int "fde addr" 0x2000 fde.func_start_address;
  check int "v2 rep_size" 8 (Unsigned.UInt8.to_int fde.rep_size);
  check bool "fde_type Pcmask" true (fde.fde_type = Dwarf.SFrame.Pcmask)

let test_fde_signed_address () =
  (* func_start_address is int32_t; check sign-extension works. *)
  let info = fde_info_byte ~fretype:0 ~fdetype:0 ~pauth_key:0 in
  let fde_bytes =
    make_fde_v2 ~addr:(-0x100) ~size:32 ~fre_off:0 ~num_fres:0 ~info_byte:info
      ~rep_size:0 ()
  in
  let header_bytes =
    make_header ~version:2 ~flags_byte:0 ~abi:3 ~cfa_fp_off:0 ~cfa_ra_off:(-8)
      ~auxhdr_len:0 ~num_fdes:1 ~num_fres:0 ~fre_len:0 ~fde_off:0 ~fre_off:20 ()
  in
  let bytes = header_bytes @ fde_bytes in
  let buf = buffer_of_bytes bytes in
  let cur = Object.Buffer.cursor buf ~at:0 in
  let t = Dwarf.SFrame.parse cur (List.length bytes) in
  check int "negative addr round-trips" (-0x100) t.fdes.(0).func_start_address

(* ---- FDE info byte decoding ---- *)

let test_fde_info_fre_types () =
  List.iter
    (fun (raw, expected) ->
      let info = fde_info_byte ~fretype:raw ~fdetype:0 ~pauth_key:0 in
      let fde_bytes =
        make_fde_v2 ~addr:0 ~size:0 ~fre_off:0 ~num_fres:0 ~info_byte:info
          ~rep_size:0 ()
      in
      let header_bytes =
        make_header ~version:2 ~flags_byte:0 ~abi:3 ~cfa_fp_off:0 ~cfa_ra_off:0
          ~auxhdr_len:0 ~num_fdes:1 ~num_fres:0 ~fre_len:0 ~fde_off:0
          ~fre_off:20 ()
      in
      let bytes = header_bytes @ fde_bytes in
      let buf = buffer_of_bytes bytes in
      let cur = Object.Buffer.cursor buf ~at:0 in
      let t = Dwarf.SFrame.parse cur (List.length bytes) in
      check bool
        (Printf.sprintf "fre_type raw=%d" raw)
        true
        (t.fdes.(0).fre_type = expected))
    [
      (0, Dwarf.SFrame.Addr1); (1, Dwarf.SFrame.Addr2); (2, Dwarf.SFrame.Addr4);
    ]

let test_fde_info_pauth_key () =
  let info = fde_info_byte ~fretype:0 ~fdetype:0 ~pauth_key:1 in
  let fde_bytes =
    make_fde_v2 ~addr:0 ~size:0 ~fre_off:0 ~num_fres:0 ~info_byte:info
      ~rep_size:0 ()
  in
  let header_bytes =
    make_header ~version:2 ~flags_byte:0 ~abi:3 ~cfa_fp_off:0 ~cfa_ra_off:0
      ~auxhdr_len:0 ~num_fdes:1 ~num_fres:0 ~fre_len:0 ~fde_off:0 ~fre_off:20 ()
  in
  let bytes = header_bytes @ fde_bytes in
  let buf = buffer_of_bytes bytes in
  let cur = Object.Buffer.cursor buf ~at:0 in
  let t = Dwarf.SFrame.parse cur (List.length bytes) in
  check bool "pauth_key B" true (t.fdes.(0).pauth_key = Dwarf.SFrame.Key_b)

let test_fde_info_unknown_fre_type_rejected () =
  let info = fde_info_byte ~fretype:5 ~fdetype:0 ~pauth_key:0 in
  let fde_bytes =
    make_fde_v2 ~addr:0 ~size:0 ~fre_off:0 ~num_fres:0 ~info_byte:info
      ~rep_size:0 ()
  in
  let header_bytes =
    make_header ~version:2 ~flags_byte:0 ~abi:3 ~cfa_fp_off:0 ~cfa_ra_off:0
      ~auxhdr_len:0 ~num_fdes:1 ~num_fres:0 ~fre_len:0 ~fde_off:0 ~fre_off:20 ()
  in
  let bytes = header_bytes @ fde_bytes in
  let buf = buffer_of_bytes bytes in
  let cur = Object.Buffer.cursor buf ~at:0 in
  match Dwarf.SFrame.parse cur (List.length bytes) with
  | exception Dwarf.SFrame.Invalid_sframe_format _ -> ()
  | _ -> fail "expected FRE type rejection"

(* ---- FRE decoding ---- *)

let make_section_with_one_fde_one_fre ~fre_type_int ~addr_bytes ~info_byte
    ~offset_bytes : int list * int (* total fre size in bytes *) =
  let fde_info = fde_info_byte ~fretype:fre_type_int ~fdetype:0 ~pauth_key:0 in
  let fde_bytes =
    make_fde_v2 ~addr:0x1000 ~size:0x40 ~fre_off:0 ~num_fres:1
      ~info_byte:fde_info ~rep_size:0 ()
  in
  let fre_bytes = addr_bytes @ u8 info_byte @ offset_bytes in
  let fre_len = List.length fre_bytes in
  let header_bytes =
    make_header ~version:2 ~flags_byte:0 ~abi:3 ~cfa_fp_off:0 ~cfa_ra_off:(-8)
      ~auxhdr_len:0 ~num_fdes:1 ~num_fres:1 ~fre_len ~fde_off:0 ~fre_off:20 ()
  in
  (header_bytes @ fde_bytes @ fre_bytes, fre_len)

let test_fre_addr1_one_offset () =
  let info =
    fre_info_byte ~cfa_base:1 ~offset_count:1 ~offset_size:0 ~mangled_ra:false
  in
  let bytes, _ =
    make_section_with_one_fde_one_fre ~fre_type_int:0 ~addr_bytes:(u8 0x10)
      ~info_byte:info ~offset_bytes:(s8 16)
  in
  let buf = buffer_of_bytes bytes in
  let cur = Object.Buffer.cursor buf ~at:0 in
  let t = Dwarf.SFrame.parse cur (List.length bytes) in
  let fres = Dwarf.SFrame.fres_of_fde t t.fdes.(0) in
  check int "fre count" 1 (Array.length fres);
  let fre = fres.(0) in
  check int "fre addr" 0x10 (Unsigned.UInt32.to_int fre.start_address);
  check int "offset count" 1 fre.info.offset_count;
  check bool "cfa from sp" true (fre.info.cfa_base_reg = `Sp);
  check int "offset value" 16 fre.offsets.(0)

let test_fre_addr2_two_offsets_2byte () =
  let info =
    fre_info_byte ~cfa_base:0 ~offset_count:2 ~offset_size:1 ~mangled_ra:false
  in
  let bytes, _ =
    make_section_with_one_fde_one_fre ~fre_type_int:1 ~addr_bytes:(u16 0x1234)
      ~info_byte:info
      ~offset_bytes:(u16 32 @ u16 (-8 + 0x10000))
  in
  let buf = buffer_of_bytes bytes in
  let cur = Object.Buffer.cursor buf ~at:0 in
  let t = Dwarf.SFrame.parse cur (List.length bytes) in
  let fres = Dwarf.SFrame.fres_of_fde t t.fdes.(0) in
  let fre = fres.(0) in
  check int "fre addr" 0x1234 (Unsigned.UInt32.to_int fre.start_address);
  check int "offset count" 2 fre.info.offset_count;
  check bool "cfa from fp" true (fre.info.cfa_base_reg = `Fp);
  check int "offset 0" 32 fre.offsets.(0);
  check int "offset 1 (sign-ext)" (-8) fre.offsets.(1)

let test_fre_addr4_three_offsets_4byte () =
  let info =
    fre_info_byte ~cfa_base:0 ~offset_count:3 ~offset_size:2 ~mangled_ra:true
  in
  let bytes, _ =
    make_section_with_one_fde_one_fre ~fre_type_int:2
      ~addr_bytes:(u32 0xdeadbeef) ~info_byte:info
      ~offset_bytes:(s32 (-128) @ s32 256 @ s32 (-1))
  in
  let buf = buffer_of_bytes bytes in
  let cur = Object.Buffer.cursor buf ~at:0 in
  let t = Dwarf.SFrame.parse cur (List.length bytes) in
  let fres = Dwarf.SFrame.fres_of_fde t t.fdes.(0) in
  let fre = fres.(0) in
  check int "fre addr" 0xdeadbeef (Unsigned.UInt32.to_int fre.start_address);
  check bool "mangled_ra set" true fre.info.mangled_ra;
  check int "offset 0" (-128) fre.offsets.(0);
  check int "offset 1" 256 fre.offsets.(1);
  check int "offset 2" (-1) fre.offsets.(2)

let test_fre_offset_count_zero_rejected () =
  let info =
    fre_info_byte ~cfa_base:0 ~offset_count:0 ~offset_size:0 ~mangled_ra:false
  in
  let bytes, _ =
    make_section_with_one_fde_one_fre ~fre_type_int:0 ~addr_bytes:(u8 0)
      ~info_byte:info ~offset_bytes:[]
  in
  let buf = buffer_of_bytes bytes in
  let cur = Object.Buffer.cursor buf ~at:0 in
  let t = Dwarf.SFrame.parse cur (List.length bytes) in
  match Dwarf.SFrame.fres_of_fde t t.fdes.(0) with
  | exception Dwarf.SFrame.Invalid_sframe_format _ -> ()
  | _ -> fail "expected offset_count=0 rejection"

(* ---- Auxiliary header ---- *)

let test_aux_header_shifts_offsets () =
  let aux = [ 0xaa; 0xbb; 0xcc; 0xdd ] in
  let info = fde_info_byte ~fretype:0 ~fdetype:0 ~pauth_key:0 in
  let fde_bytes =
    make_fde_v2 ~addr:0x500 ~size:16 ~fre_off:0 ~num_fres:0 ~info_byte:info
      ~rep_size:0 ()
  in
  let header_bytes =
    make_header ~version:2 ~flags_byte:0 ~abi:3 ~cfa_fp_off:0 ~cfa_ra_off:0
      ~auxhdr_len:(List.length aux) ~num_fdes:1 ~num_fres:0 ~fre_len:0
      ~fde_off:0 ~fre_off:20 ()
  in
  let bytes = header_bytes @ aux @ fde_bytes in
  let buf = buffer_of_bytes bytes in
  let cur = Object.Buffer.cursor buf ~at:0 in
  let t = Dwarf.SFrame.parse cur (List.length bytes) in
  check int "auxhdr_len" 4 (Unsigned.UInt8.to_int t.header.auxhdr_len);
  check int "fde addr after aux" 0x500 t.fdes.(0).func_start_address

(* ---- find_fde_for_pc ---- *)

let make_multi_fde_section ~sorted () =
  (* Three FDEs at 0x1000(size 0x40), 0x2000(size 0x80), 0x3000(size 0x20). *)
  let info = fde_info_byte ~fretype:0 ~fdetype:0 ~pauth_key:0 in
  let fdes =
    [
      make_fde_v2 ~addr:0x1000 ~size:0x40 ~fre_off:0 ~num_fres:0 ~info_byte:info
        ~rep_size:0 ();
      make_fde_v2 ~addr:0x2000 ~size:0x80 ~fre_off:0 ~num_fres:0 ~info_byte:info
        ~rep_size:0 ();
      make_fde_v2 ~addr:0x3000 ~size:0x20 ~fre_off:0 ~num_fres:0 ~info_byte:info
        ~rep_size:0 ();
    ]
  in
  let fde_bytes = List.concat fdes in
  let flags = if sorted then 0x1 else 0x0 in
  let header_bytes =
    make_header ~version:2 ~flags_byte:flags ~abi:3 ~cfa_fp_off:0 ~cfa_ra_off:0
      ~auxhdr_len:0 ~num_fdes:3 ~num_fres:0 ~fre_len:0 ~fde_off:0 ~fre_off:60 ()
  in
  let bytes = header_bytes @ fde_bytes in
  let buf = buffer_of_bytes bytes in
  let cur = Object.Buffer.cursor buf ~at:0 in
  Dwarf.SFrame.parse cur (List.length bytes)

let test_find_fde_hit_sorted () =
  let t = make_multi_fde_section ~sorted:true () in
  let fde = Dwarf.SFrame.find_fde_for_pc t 0x2010 in
  match fde with
  | Some f -> check int "matched FDE addr" 0x2000 f.func_start_address
  | None -> fail "expected hit at 0x2010"

let test_find_fde_hit_unsorted () =
  let t = make_multi_fde_section ~sorted:false () in
  let fde = Dwarf.SFrame.find_fde_for_pc t 0x301f in
  match fde with
  | Some f -> check int "matched FDE addr" 0x3000 f.func_start_address
  | None -> fail "expected hit at 0x301f"

let test_find_fde_miss_before () =
  let t = make_multi_fde_section ~sorted:true () in
  check bool "miss before first" true
    (Dwarf.SFrame.find_fde_for_pc t 0x500 = None)

let test_find_fde_miss_after () =
  let t = make_multi_fde_section ~sorted:true () in
  check bool "miss after last" true
    (Dwarf.SFrame.find_fde_for_pc t 0x4000 = None)

let test_find_fde_miss_in_gap () =
  let t = make_multi_fde_section ~sorted:true () in
  (* Gap between FDE 0 (ends at 0x1040) and FDE 1 (starts at 0x2000). *)
  check bool "miss in gap" true (Dwarf.SFrame.find_fde_for_pc t 0x1500 = None)

let test_find_fde_boundaries () =
  let t = make_multi_fde_section ~sorted:true () in
  (* Inclusive at start, exclusive at end. *)
  check bool "hit at start" true (Dwarf.SFrame.find_fde_for_pc t 0x1000 <> None);
  check bool "hit at last byte" true
    (Dwarf.SFrame.find_fde_for_pc t 0x103f <> None);
  check bool "miss at end" true (Dwarf.SFrame.find_fde_for_pc t 0x1040 = None)

(* ---- Writer roundtrip ---- *)

let bytes_of_int_list (lst : int list) : bytes =
  Bytes.of_seq (List.to_seq lst |> Seq.map (fun n -> Char.chr (n land 0xff)))

let roundtrip_check label (input_bytes : int list) =
  let buf = buffer_of_bytes input_bytes in
  let cur = Object.Buffer.cursor buf ~at:0 in
  let t = Dwarf.SFrame.parse cur (List.length input_bytes) in
  let written = Dwarf.SFrame.Write.to_bytes t in
  let expected = bytes_of_int_list input_bytes in
  check int
    (label ^ ": output length")
    (List.length input_bytes) (Bytes.length written);
  check bool (label ^ ": bytes equal") true (Bytes.equal expected written)

let test_roundtrip_minimal_v2 () =
  let bytes =
    make_header ~version:2 ~flags_byte:0x1 ~abi:3 ~cfa_fp_off:0 ~cfa_ra_off:(-8)
      ~auxhdr_len:0 ~num_fdes:0 ~num_fres:0 ~fre_len:0 ~fde_off:0 ~fre_off:0 ()
  in
  roundtrip_check "minimal V2" bytes

let test_roundtrip_minimal_v1 () =
  let bytes =
    make_header ~version:1 ~flags_byte:0x1 ~abi:3 ~cfa_fp_off:0 ~cfa_ra_off:(-8)
      ~auxhdr_len:0 ~num_fdes:0 ~num_fres:0 ~fre_len:0 ~fde_off:0 ~fre_off:0 ()
  in
  roundtrip_check "minimal V1" bytes

let test_roundtrip_v2_with_pcrel_flag () =
  (* All three flags set — exercises func_start_pcrel encoding. *)
  let bytes =
    make_header ~version:2 ~flags_byte:0x7 ~abi:3 ~cfa_fp_off:0 ~cfa_ra_off:(-8)
      ~auxhdr_len:0 ~num_fdes:0 ~num_fres:0 ~fre_len:0 ~fde_off:0 ~fre_off:0 ()
  in
  roundtrip_check "V2 + all flags" bytes

let test_roundtrip_with_aux_header () =
  let aux = [ 0xaa; 0xbb; 0xcc; 0xdd; 0x11; 0x22; 0x33; 0x44 ] in
  let info = fde_info_byte ~fretype:0 ~fdetype:0 ~pauth_key:0 in
  let fde_bytes =
    make_fde_v2 ~addr:0x1234 ~size:64 ~fre_off:0 ~num_fres:0 ~info_byte:info
      ~rep_size:0 ()
  in
  let header_bytes =
    make_header ~version:2 ~flags_byte:0x1 ~abi:3 ~cfa_fp_off:0 ~cfa_ra_off:(-8)
      ~auxhdr_len:(List.length aux) ~num_fdes:1 ~num_fres:0 ~fre_len:0
      ~fde_off:0 ~fre_off:20 ()
  in
  roundtrip_check "with aux header" (header_bytes @ aux @ fde_bytes)

let test_roundtrip_v1_fde () =
  (* V1 FDE is 17 bytes (no rep_size/padding). *)
  let info = fde_info_byte ~fretype:1 ~fdetype:0 ~pauth_key:0 in
  let fde_bytes =
    make_fde_v1 ~addr:(-0x1000) ~size:0x100 ~fre_off:0 ~num_fres:0
      ~info_byte:info ()
  in
  let header_bytes =
    make_header ~version:1 ~flags_byte:0x1 ~abi:2 ~cfa_fp_off:0 ~cfa_ra_off:(-8)
      ~auxhdr_len:0 ~num_fdes:1 ~num_fres:0 ~fre_len:0 ~fde_off:0 ~fre_off:17 ()
  in
  roundtrip_check "V1 with FDE" (header_bytes @ fde_bytes)

let test_roundtrip_pcmask_fde_with_pauth_b () =
  (* Exercises every nontrivial bit of the FDE info byte. *)
  let info = fde_info_byte ~fretype:2 ~fdetype:1 ~pauth_key:1 in
  let fde_bytes =
    make_fde_v2 ~addr:0x4000 ~size:0x80 ~fre_off:0 ~num_fres:0 ~info_byte:info
      ~rep_size:16 ()
  in
  let header_bytes =
    make_header ~version:2 ~flags_byte:0x1 ~abi:2 ~cfa_fp_off:0 ~cfa_ra_off:(-8)
      ~auxhdr_len:0 ~num_fdes:1 ~num_fres:0 ~fre_len:0 ~fde_off:0 ~fre_off:20 ()
  in
  roundtrip_check "PCMASK + KEY_B + Addr4" (header_bytes @ fde_bytes)

let test_roundtrip_multi_fde_with_fres () =
  (* Two FDEs with FREs of differing widths in the same section. *)
  let fde_info_a = fde_info_byte ~fretype:0 ~fdetype:0 ~pauth_key:0 in
  let fde_info_b = fde_info_byte ~fretype:2 ~fdetype:0 ~pauth_key:0 in
  let fre_info_a =
    fre_info_byte ~cfa_base:0 ~offset_count:1 ~offset_size:0 ~mangled_ra:false
  in
  let fre_info_b =
    fre_info_byte ~cfa_base:1 ~offset_count:3 ~offset_size:2 ~mangled_ra:true
  in
  let fre_a = u8 0x10 @ u8 fre_info_a @ s8 (-16) in
  let fre_b = u32 0x100 @ u8 fre_info_b @ s32 (-32) @ s32 64 @ s32 (-1) in
  let fre_a_len = List.length fre_a in
  let fre_pool = fre_a @ fre_b in
  let fre_len = List.length fre_pool in
  let fde_a =
    make_fde_v2 ~addr:0x1000 ~size:0x40 ~fre_off:0 ~num_fres:1
      ~info_byte:fde_info_a ~rep_size:0 ()
  in
  let fde_b =
    make_fde_v2 ~addr:0x2000 ~size:0x80 ~fre_off:fre_a_len ~num_fres:1
      ~info_byte:fde_info_b ~rep_size:0 ()
  in
  let header_bytes =
    make_header ~version:2 ~flags_byte:0x1 ~abi:3 ~cfa_fp_off:0 ~cfa_ra_off:(-8)
      ~auxhdr_len:0 ~num_fdes:2 ~num_fres:2 ~fre_len ~fde_off:0 ~fre_off:40 ()
  in
  roundtrip_check "multi-FDE with mixed FREs"
    (header_bytes @ fde_a @ fde_b @ fre_pool)

(* ---- Builder (emit_from_parts) ---- *)

let parse_bytes (b : bytes) : Dwarf.SFrame.t =
  let filename = Filename.temp_file "sframe_build_" ".bin" in
  let oc = open_out_bin filename in
  output_bytes oc b;
  close_out oc;
  let buffer = Object.Buffer.parse filename in
  Sys.remove filename;
  let cur = Object.Buffer.cursor buffer ~at:0 in
  Dwarf.SFrame.parse cur (Bytes.length b)

let no_flags : Dwarf.SFrame.flags =
  { fde_sorted = false; frame_pointer = false; func_start_pcrel = false }

let sorted_flag : Dwarf.SFrame.flags = { no_flags with fde_sorted = true }

let test_builder_empty () =
  let parts : Dwarf.SFrame.Write.section_parts =
    {
      version = 2;
      flags = sorted_flag;
      abi_arch = Dwarf.SFrame.Amd64_le;
      cfa_fixed_fp_offset = 0;
      cfa_fixed_ra_offset = -8;
      aux_header = Bytes.empty;
      fdes = [||];
    }
  in
  let bytes = Dwarf.SFrame.Write.to_bytes_from_parts parts in
  let t = parse_bytes bytes in
  check int "no FDEs" 0 (Array.length t.fdes);
  check int "no FREs" 0 (Unsigned.UInt32.to_int t.header.num_fres);
  check bool "abi" true (t.header.abi_arch = Dwarf.SFrame.Amd64_le);
  check int "ra offset" (-8) t.header.cfa_fixed_ra_offset

let make_fre ~start ~cfa_base ~offsets ~size ~mangled_ra : Dwarf.SFrame.fre =
  {
    start_address = Unsigned.UInt32.of_int start;
    info =
      {
        cfa_base_reg = cfa_base;
        offset_count = List.length offsets;
        offset_size = size;
        mangled_ra;
      };
    offsets = Array.of_list offsets;
  }

let test_builder_single_fde_single_fre () =
  let fre =
    make_fre ~start:0 ~cfa_base:`Sp ~offsets:[ 16 ] ~size:Dwarf.SFrame.Off_1b
      ~mangled_ra:false
  in
  let spec : Dwarf.SFrame.Write.fde_spec =
    {
      func_start_address = 0x1000;
      func_size = Unsigned.UInt32.of_int 0x40;
      fre_type = Dwarf.SFrame.Addr1;
      fde_type = Dwarf.SFrame.Pcinc;
      pauth_key = Dwarf.SFrame.Key_a;
      rep_size = Unsigned.UInt8.zero;
      fres = [| fre |];
    }
  in
  let parts : Dwarf.SFrame.Write.section_parts =
    {
      version = 2;
      flags = sorted_flag;
      abi_arch = Dwarf.SFrame.Amd64_le;
      cfa_fixed_fp_offset = 0;
      cfa_fixed_ra_offset = -8;
      aux_header = Bytes.empty;
      fdes = [| spec |];
    }
  in
  let bytes = Dwarf.SFrame.Write.to_bytes_from_parts parts in
  let t = parse_bytes bytes in
  check int "fde count" 1 (Array.length t.fdes);
  check int "fre count" 1 (Unsigned.UInt32.to_int t.header.num_fres);
  let fde = t.fdes.(0) in
  check int "fde start address" 0x1000 fde.func_start_address;
  check int "fde func size" 0x40 (Unsigned.UInt32.to_int fde.func_size);
  let fres = Dwarf.SFrame.fres_of_fde t fde in
  check int "fres for fde" 1 (Array.length fres);
  let fre = fres.(0) in
  check int "fre start" 0 (Unsigned.UInt32.to_int fre.start_address);
  check int "fre offset" 16 fre.offsets.(0)

let test_builder_multi_fde_mixed_widths () =
  (* Two FDEs: first uses Addr1+Off_1b, second uses Addr4+Off_4b. *)
  let fre_a =
    make_fre ~start:0 ~cfa_base:`Sp ~offsets:[ 8 ] ~size:Dwarf.SFrame.Off_1b
      ~mangled_ra:false
  in
  let fre_b1 =
    make_fre ~start:0 ~cfa_base:`Fp ~offsets:[ -16; 32; -1 ]
      ~size:Dwarf.SFrame.Off_4b ~mangled_ra:true
  in
  let fre_b2 =
    make_fre ~start:0x100 ~cfa_base:`Sp ~offsets:[ 0; 0; 0 ]
      ~size:Dwarf.SFrame.Off_4b ~mangled_ra:false
  in
  let spec_a : Dwarf.SFrame.Write.fde_spec =
    {
      func_start_address = 0x1000;
      func_size = Unsigned.UInt32.of_int 0x40;
      fre_type = Dwarf.SFrame.Addr1;
      fde_type = Dwarf.SFrame.Pcinc;
      pauth_key = Dwarf.SFrame.Key_a;
      rep_size = Unsigned.UInt8.zero;
      fres = [| fre_a |];
    }
  in
  let spec_b : Dwarf.SFrame.Write.fde_spec =
    {
      func_start_address = 0x2000;
      func_size = Unsigned.UInt32.of_int 0x200;
      fre_type = Dwarf.SFrame.Addr4;
      fde_type = Dwarf.SFrame.Pcinc;
      pauth_key = Dwarf.SFrame.Key_a;
      rep_size = Unsigned.UInt8.zero;
      fres = [| fre_b1; fre_b2 |];
    }
  in
  let parts : Dwarf.SFrame.Write.section_parts =
    {
      version = 2;
      flags = sorted_flag;
      abi_arch = Dwarf.SFrame.Amd64_le;
      cfa_fixed_fp_offset = 0;
      cfa_fixed_ra_offset = -8;
      aux_header = Bytes.empty;
      fdes = [| spec_a; spec_b |];
    }
  in
  let bytes = Dwarf.SFrame.Write.to_bytes_from_parts parts in
  let t = parse_bytes bytes in
  check int "fde count" 2 (Array.length t.fdes);
  check int "fre total" 3 (Unsigned.UInt32.to_int t.header.num_fres);
  (* FDE 0: 1 FRE at SP, single byte offset. *)
  let fres_a = Dwarf.SFrame.fres_of_fde t t.fdes.(0) in
  check int "fde0 fre count" 1 (Array.length fres_a);
  check int "fde0 fre offset" 8 fres_a.(0).offsets.(0);
  check bool "fde0 cfa from sp" true (fres_a.(0).info.cfa_base_reg = `Sp);
  (* FDE 1: 2 FREs at Addr4, three 4B offsets each. *)
  let fres_b = Dwarf.SFrame.fres_of_fde t t.fdes.(1) in
  check int "fde1 fre count" 2 (Array.length fres_b);
  check int "fde1 fre0 offset 0" (-16) fres_b.(0).offsets.(0);
  check int "fde1 fre0 offset 1" 32 fres_b.(0).offsets.(1);
  check int "fde1 fre0 offset 2" (-1) fres_b.(0).offsets.(2);
  check bool "fde1 fre0 mangled_ra" true fres_b.(0).info.mangled_ra;
  check int "fde1 fre1 start" 0x100
    (Unsigned.UInt32.to_int fres_b.(1).start_address)

let test_builder_v1 () =
  let spec : Dwarf.SFrame.Write.fde_spec =
    {
      func_start_address = 0x500;
      func_size = Unsigned.UInt32.of_int 0x10;
      fre_type = Dwarf.SFrame.Addr2;
      fde_type = Dwarf.SFrame.Pcinc;
      pauth_key = Dwarf.SFrame.Key_a;
      rep_size = Unsigned.UInt8.zero;
      fres = [||];
    }
  in
  let parts : Dwarf.SFrame.Write.section_parts =
    {
      version = 1;
      flags = no_flags;
      abi_arch = Dwarf.SFrame.Aarch64_le;
      cfa_fixed_fp_offset = 0;
      cfa_fixed_ra_offset = 0;
      aux_header = Bytes.empty;
      fdes = [| spec |];
    }
  in
  let bytes = Dwarf.SFrame.Write.to_bytes_from_parts parts in
  let t = parse_bytes bytes in
  check int "v1 version" 1 (Unsigned.UInt8.to_int t.header.preamble.version);
  check int "v1 fde count" 1 (Array.length t.fdes);
  (* V1 FDE is 17 bytes; with no FREs, fre_off should equal num_fdes * 17. *)
  check int "v1 fre_off" 17 (Unsigned.UInt32.to_int t.header.fre_off);
  check int "v1 fre_len" 0 (Unsigned.UInt32.to_int t.header.fre_len)

let test_builder_with_aux () =
  let aux = Bytes.of_string "AUX!" in
  let fre =
    make_fre ~start:0 ~cfa_base:`Sp ~offsets:[ 16 ] ~size:Dwarf.SFrame.Off_1b
      ~mangled_ra:false
  in
  let spec : Dwarf.SFrame.Write.fde_spec =
    {
      func_start_address = 0x100;
      func_size = Unsigned.UInt32.of_int 0x10;
      fre_type = Dwarf.SFrame.Addr1;
      fde_type = Dwarf.SFrame.Pcinc;
      pauth_key = Dwarf.SFrame.Key_a;
      rep_size = Unsigned.UInt8.zero;
      fres = [| fre |];
    }
  in
  let parts : Dwarf.SFrame.Write.section_parts =
    {
      version = 2;
      flags = sorted_flag;
      abi_arch = Dwarf.SFrame.Amd64_le;
      cfa_fixed_fp_offset = 0;
      cfa_fixed_ra_offset = -8;
      aux_header = aux;
      fdes = [| spec |];
    }
  in
  let bytes = Dwarf.SFrame.Write.to_bytes_from_parts parts in
  let t = parse_bytes bytes in
  check int "auxhdr_len" 4 (Unsigned.UInt8.to_int t.header.auxhdr_len);
  check int "fde present" 0x100 t.fdes.(0).func_start_address;
  let fres = Dwarf.SFrame.fres_of_fde t t.fdes.(0) in
  check int "fre offset survives aux" 16 fres.(0).offsets.(0)

let test_builder_pcmask_with_rep_size () =
  let fre =
    make_fre ~start:0 ~cfa_base:`Sp ~offsets:[ 16 ] ~size:Dwarf.SFrame.Off_1b
      ~mangled_ra:false
  in
  let spec : Dwarf.SFrame.Write.fde_spec =
    {
      func_start_address = 0x3000;
      func_size = Unsigned.UInt32.of_int 0x80;
      fre_type = Dwarf.SFrame.Addr1;
      fde_type = Dwarf.SFrame.Pcmask;
      pauth_key = Dwarf.SFrame.Key_b;
      rep_size = Unsigned.UInt8.of_int 16;
      fres = [| fre |];
    }
  in
  let parts : Dwarf.SFrame.Write.section_parts =
    {
      version = 2;
      flags = sorted_flag;
      abi_arch = Dwarf.SFrame.Aarch64_le;
      cfa_fixed_fp_offset = 0;
      cfa_fixed_ra_offset = 0;
      aux_header = Bytes.empty;
      fdes = [| spec |];
    }
  in
  let bytes = Dwarf.SFrame.Write.to_bytes_from_parts parts in
  let t = parse_bytes bytes in
  let fde = t.fdes.(0) in
  check bool "fde_type Pcmask" true (fde.fde_type = Dwarf.SFrame.Pcmask);
  check bool "pauth_key B" true (fde.pauth_key = Dwarf.SFrame.Key_b);
  check int "rep_size" 16 (Unsigned.UInt8.to_int fde.rep_size)

let test_builder_unsupported_version () =
  let parts : Dwarf.SFrame.Write.section_parts =
    {
      version = 3;
      flags = no_flags;
      abi_arch = Dwarf.SFrame.Amd64_le;
      cfa_fixed_fp_offset = 0;
      cfa_fixed_ra_offset = 0;
      aux_header = Bytes.empty;
      fdes = [||];
    }
  in
  match Dwarf.SFrame.Write.to_bytes_from_parts parts with
  | exception Invalid_argument _ -> ()
  | _ -> fail "expected Invalid_argument for version 3"

(* ---- Section name mapping ---- *)

let test_section_name () =
  check string "ELF .sframe" ".sframe"
    (Dwarf.object_format_to_section_name Object_format.ELF Dwarf.Sframe);
  check_raises "MachO sframe raises"
    (Failure ".sframe is an ELF-only section, not supported on MachO")
    (fun () ->
      ignore
        (Dwarf.object_format_to_section_name Object_format.MACHO Dwarf.Sframe))

let () =
  run "sframe"
    [
      ( "preamble",
        [
          test_case "correct magic + version" `Quick test_magic_correct;
          test_case "swapped magic rejected" `Quick test_magic_swapped_rejected;
          test_case "garbage magic rejected" `Quick test_magic_garbage_rejected;
          test_case "unsupported version rejected" `Quick
            test_version_unsupported_rejected;
        ] );
      ( "flags",
        [
          test_case "FDE_SORTED" `Quick test_flag_fde_sorted;
          test_case "FRAME_POINTER" `Quick test_flag_frame_pointer;
          test_case "both flags" `Quick test_flag_both;
          test_case "FDE_FUNC_START_PCREL" `Quick test_flag_func_start_pcrel;
          test_case "all three flags" `Quick test_flag_all_three;
        ] );
      ( "abi",
        [
          test_case "AMD64 LE" `Quick test_abi_amd64_le;
          test_case "AArch64 LE" `Quick test_abi_aarch64_le;
          test_case "AArch64 BE" `Quick test_abi_aarch64_be;
          test_case "unknown ABI rejected" `Quick test_abi_unknown_rejected;
        ] );
      ( "header",
        [
          test_case "signed cfa_fixed offsets" `Quick
            test_cfa_fixed_offsets_signed;
        ] );
      ( "fde",
        [
          test_case "V1 layout (17B)" `Quick test_fde_v1_layout;
          test_case "V2 layout (20B)" `Quick test_fde_v2_layout;
          test_case "signed func_start_address" `Quick test_fde_signed_address;
          test_case "info byte fre_types" `Quick test_fde_info_fre_types;
          test_case "info byte pauth_key" `Quick test_fde_info_pauth_key;
          test_case "unknown fre_type rejected" `Quick
            test_fde_info_unknown_fre_type_rejected;
        ] );
      ( "fre",
        [
          test_case "Addr1 + 1 offset" `Quick test_fre_addr1_one_offset;
          test_case "Addr2 + 2 offsets (2B)" `Quick
            test_fre_addr2_two_offsets_2byte;
          test_case "Addr4 + 3 offsets (4B, mangled_ra)" `Quick
            test_fre_addr4_three_offsets_4byte;
          test_case "offset_count=0 rejected" `Quick
            test_fre_offset_count_zero_rejected;
        ] );
      ( "aux_header",
        [
          test_case "auxhdr shifts FDE offset" `Quick
            test_aux_header_shifts_offsets;
        ] );
      ( "find_fde_for_pc",
        [
          test_case "hit (sorted)" `Quick test_find_fde_hit_sorted;
          test_case "hit (unsorted)" `Quick test_find_fde_hit_unsorted;
          test_case "miss before first" `Quick test_find_fde_miss_before;
          test_case "miss after last" `Quick test_find_fde_miss_after;
          test_case "miss in gap" `Quick test_find_fde_miss_in_gap;
          test_case "range boundaries" `Quick test_find_fde_boundaries;
        ] );
      ( "section_name",
        [ test_case "ELF/MachO mapping" `Quick test_section_name ] );
      ( "writer_roundtrip",
        [
          test_case "minimal V2" `Quick test_roundtrip_minimal_v2;
          test_case "minimal V1" `Quick test_roundtrip_minimal_v1;
          test_case "V2 + all flags (PCREL etc.)" `Quick
            test_roundtrip_v2_with_pcrel_flag;
          test_case "with aux header" `Quick test_roundtrip_with_aux_header;
          test_case "V1 with FDE" `Quick test_roundtrip_v1_fde;
          test_case "PCMASK FDE + KEY_B" `Quick
            test_roundtrip_pcmask_fde_with_pauth_b;
          test_case "multi-FDE with mixed FREs" `Quick
            test_roundtrip_multi_fde_with_fres;
        ] );
      ( "builder",
        [
          test_case "empty section" `Quick test_builder_empty;
          test_case "single FDE single FRE" `Quick
            test_builder_single_fde_single_fre;
          test_case "multi-FDE mixed FRE widths" `Quick
            test_builder_multi_fde_mixed_widths;
          test_case "V1 with empty-FRE FDE" `Quick test_builder_v1;
          test_case "with aux header" `Quick test_builder_with_aux;
          test_case "PCMASK FDE with rep_size" `Quick
            test_builder_pcmask_with_rep_size;
          test_case "unsupported version rejected" `Quick
            test_builder_unsupported_version;
        ] );
    ]
