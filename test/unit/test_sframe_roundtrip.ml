(* Property-based roundtrip test for the SFrame builder.

   Generates arbitrary [section_parts], emits bytes via
   [Dwarf.SFrame.Write.to_bytes_from_parts], parses them back via
   [Dwarf.SFrame.parse], and asserts every observable field is preserved.

   Generators respect the inter-field constraints: each FRE's
   [start_address] must fit in its enclosing FDE's [fre_type] width,
   and each offset must fit (signed) in the FRE's [offset_size] width. *)

open QCheck.Gen
open Durin

(* --- Atomic generators --- *)

let gen_version = oneof_list [ 1; 2 ]

let gen_flags : Dwarf.SFrame.flags QCheck.Gen.t =
  let* fde_sorted = bool in
  let* frame_pointer = bool in
  let* func_start_pcrel = bool in
  return Dwarf.SFrame.{ fde_sorted; frame_pointer; func_start_pcrel }

let gen_abi_arch : Dwarf.SFrame.abi_arch QCheck.Gen.t =
  oneof_list [ Dwarf.SFrame.Aarch64_be; Aarch64_le; Amd64_le ]

let gen_fre_type : Dwarf.SFrame.fre_type QCheck.Gen.t =
  oneof_list [ Dwarf.SFrame.Addr1; Addr2; Addr4 ]

let gen_fde_type : Dwarf.SFrame.fde_type QCheck.Gen.t =
  oneof_list [ Dwarf.SFrame.Pcinc; Pcmask ]

let gen_pauth_key : Dwarf.SFrame.pauth_key QCheck.Gen.t =
  oneof_list [ Dwarf.SFrame.Key_a; Key_b ]

let gen_offset_size : Dwarf.SFrame.fre_offset_size QCheck.Gen.t =
  oneof_list [ Dwarf.SFrame.Off_1b; Off_2b; Off_4b ]

let gen_cfa_base : [ `Sp | `Fp ] QCheck.Gen.t = oneof_list [ `Sp; `Fp ]

(* Signed integer of [bytes] bytes width. *)
let gen_signed_n bytes =
  let bits = bytes * 8 in
  let max_v = 1 lsl (bits - 1) in
  int_range (-max_v) (max_v - 1)

(* start_address fits unsigned in the FDE's fre_type width. *)
let gen_start_addr_for (ft : Dwarf.SFrame.fre_type) :
    Unsigned.UInt32.t QCheck.Gen.t =
  let max_v =
    match ft with
    | Dwarf.SFrame.Addr1 -> 0xff
    | Addr2 -> 0xffff
    | Addr4 -> 0xffffffff
  in
  let* n = int_range 0 max_v in
  return (Unsigned.UInt32.of_int n)

let offset_bytes = function
  | Dwarf.SFrame.Off_1b -> 1
  | Off_2b -> 2
  | Off_4b -> 4

(* --- FRE / FDE / section generators --- *)

let gen_fre (fre_type : Dwarf.SFrame.fre_type) : Dwarf.SFrame.fre QCheck.Gen.t =
  let* offset_count = int_range 1 3 in
  let* offset_size = gen_offset_size in
  let* mangled_ra = bool in
  let* cfa_base_reg = gen_cfa_base in
  let* start_address = gen_start_addr_for fre_type in
  let bytes = offset_bytes offset_size in
  let* offsets = list_size (return offset_count) (gen_signed_n bytes) in
  return
    Dwarf.SFrame.
      {
        start_address;
        info = { cfa_base_reg; offset_count; offset_size; mangled_ra };
        offsets = Array.of_list offsets;
      }

let gen_fde_spec : Dwarf.SFrame.Write.fde_spec QCheck.Gen.t =
  let* fre_type = gen_fre_type in
  let* fde_type = gen_fde_type in
  let* pauth_key = gen_pauth_key in
  let* func_start_address = gen_signed_n 4 in
  let* func_size_n = int_range 0 0xffff in
  let* rep_size_n = int_range 0 0xff in
  let* num_fres = int_range 0 5 in
  let* fres = list_size (return num_fres) (gen_fre fre_type) in
  return
    Dwarf.SFrame.Write.
      {
        func_start_address;
        func_size = Unsigned.UInt32.of_int func_size_n;
        fre_type;
        fde_type;
        pauth_key;
        rep_size = Unsigned.UInt8.of_int rep_size_n;
        fres = Array.of_list fres;
      }

let gen_aux_header : bytes QCheck.Gen.t =
  let* len = int_range 0 8 in
  let* byte_list = list_size (return len) (int_range 0 0xff) in
  return (Bytes.init len (fun i -> Char.chr (List.nth byte_list i)))

let gen_section_parts : Dwarf.SFrame.Write.section_parts QCheck.Gen.t =
  let* version = gen_version in
  let* flags = gen_flags in
  let* abi_arch = gen_abi_arch in
  let* cfa_fixed_fp_offset = int_range (-128) 127 in
  let* cfa_fixed_ra_offset = int_range (-128) 127 in
  let* aux_header = gen_aux_header in
  let* num_fdes = int_range 0 4 in
  let* fdes = list_size (return num_fdes) gen_fde_spec in
  (* V1 has no rep_size on disk (added in V2). Force it to 0 in V1
     specs so the roundtrip property holds. *)
  let fdes =
    if version = 1 then
      List.map
        (fun (s : Dwarf.SFrame.Write.fde_spec) ->
          { s with rep_size = Unsigned.UInt8.zero })
        fdes
    else fdes
  in
  return
    Dwarf.SFrame.Write.
      {
        version;
        flags;
        abi_arch;
        cfa_fixed_fp_offset;
        cfa_fixed_ra_offset;
        aux_header;
        fdes = Array.of_list fdes;
      }

(* --- Roundtrip property --- *)

let parse_bytes (b : bytes) : Dwarf.SFrame.t =
  let filename = Filename.temp_file "sframe_qc_" ".bin" in
  let oc = open_out_bin filename in
  output_bytes oc b;
  close_out oc;
  let buffer = Object.Buffer.parse filename in
  Sys.remove filename;
  let cur = Object.Buffer.cursor buffer ~at:0 in
  Dwarf.SFrame.parse cur (Bytes.length b)

let check b msg = if not b then failwith ("MISMATCH: " ^ msg) else ()

let fre_equal (i : int) (j : int) (a : Dwarf.SFrame.fre) (b : Dwarf.SFrame.fre)
    : bool =
  let prefix = Printf.sprintf "fde[%d].fre[%d]" i j in
  check
    (Unsigned.UInt32.to_int a.start_address
    = Unsigned.UInt32.to_int b.start_address)
    (prefix ^ ".start_address: parsed "
    ^ string_of_int (Unsigned.UInt32.to_int a.start_address)
    ^ " vs spec "
    ^ string_of_int (Unsigned.UInt32.to_int b.start_address));
  check
    (a.info.cfa_base_reg = b.info.cfa_base_reg)
    (prefix ^ ".info.cfa_base_reg");
  check
    (a.info.offset_count = b.info.offset_count)
    (prefix ^ ".info.offset_count: parsed "
    ^ string_of_int a.info.offset_count
    ^ " vs spec "
    ^ string_of_int b.info.offset_count);
  check (a.info.offset_size = b.info.offset_size) (prefix ^ ".info.offset_size");
  check (a.info.mangled_ra = b.info.mangled_ra) (prefix ^ ".info.mangled_ra");
  check
    (Array.length a.offsets = Array.length b.offsets)
    (prefix ^ ".offsets length");
  Array.iteri
    (fun k v ->
      check
        (a.offsets.(k) = v)
        (Printf.sprintf "%s.offsets[%d]: parsed %d vs spec %d" prefix k
           a.offsets.(k) v))
    b.offsets;
  true

let fde_matches_spec (t : Dwarf.SFrame.t) (i : int)
    (spec : Dwarf.SFrame.Write.fde_spec) : bool =
  let fde = t.fdes.(i) in
  let prefix = Printf.sprintf "fde[%d]" i in
  check
    (fde.func_start_address = spec.func_start_address)
    (prefix ^ ".func_start_address");
  check
    (Unsigned.UInt32.to_int fde.func_size
    = Unsigned.UInt32.to_int spec.func_size)
    (prefix ^ ".func_size");
  check (fde.fre_type = spec.fre_type) (prefix ^ ".fre_type");
  check (fde.fde_type = spec.fde_type) (prefix ^ ".fde_type");
  check (fde.pauth_key = spec.pauth_key) (prefix ^ ".pauth_key");
  check
    (Unsigned.UInt8.to_int fde.rep_size = Unsigned.UInt8.to_int spec.rep_size)
    (prefix ^ ".rep_size");
  let fres = Dwarf.SFrame.fres_of_fde t fde in
  check (Array.length fres = Array.length spec.fres) (prefix ^ ".fres length");
  Array.iteri (fun j fre -> ignore (fre_equal i j fre spec.fres.(j))) fres;
  true

let prop_roundtrip (parts : Dwarf.SFrame.Write.section_parts) : bool =
  let bytes = Dwarf.SFrame.Write.to_bytes_from_parts parts in
  let t = parse_bytes bytes in
  let h = t.header in
  Unsigned.UInt8.to_int h.preamble.version = parts.version
  && h.preamble.flags = parts.flags
  && h.abi_arch = parts.abi_arch
  && h.cfa_fixed_fp_offset = parts.cfa_fixed_fp_offset
  && h.cfa_fixed_ra_offset = parts.cfa_fixed_ra_offset
  && Unsigned.UInt8.to_int h.auxhdr_len = Bytes.length parts.aux_header
  && Unsigned.UInt32.to_int h.num_fdes = Array.length parts.fdes
  && Array.length t.fdes = Array.length parts.fdes
  &&
  let n = Array.length parts.fdes in
  let rec all i =
    i = n || (fde_matches_spec t i parts.fdes.(i) && all (i + 1))
  in
  all 0

let test_roundtrip =
  QCheck.Test.make ~count:1000 ~name:"section_parts roundtrip"
    (QCheck.make gen_section_parts)
    prop_roundtrip

let () =
  let open Alcotest in
  run "SFrame roundtrip"
    [ ("property", [ QCheck_alcotest.to_alcotest test_roundtrip ]) ]
