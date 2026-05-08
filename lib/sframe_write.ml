module Buffer = Stdlib.Buffer

(* Primitive writers. Mirror Object.Buffer.Read for symmetry. *)

let write_u8 buf (v : Unsigned.UInt8.t) : unit =
  Buffer.add_char buf (Char.chr (Unsigned.UInt8.to_int v))

let write_s8 buf (n : int) : unit =
  let b = if n < 0 then n + 0x100 else n in
  Buffer.add_char buf (Char.chr (b land 0xff))

let write_u16_le buf (v : Unsigned.UInt16.t) : unit =
  let n = Unsigned.UInt16.to_int v in
  Buffer.add_char buf (Char.chr (n land 0xff));
  Buffer.add_char buf (Char.chr ((n lsr 8) land 0xff))

let write_u32_le buf (v : Unsigned.UInt32.t) : unit =
  let n = Unsigned.UInt32.to_int v in
  Buffer.add_char buf (Char.chr (n land 0xff));
  Buffer.add_char buf (Char.chr ((n lsr 8) land 0xff));
  Buffer.add_char buf (Char.chr ((n lsr 16) land 0xff));
  Buffer.add_char buf (Char.chr ((n lsr 24) land 0xff))

let write_s32_le buf (n : int) : unit =
  let v = if n < 0 then n + 0x100000000 else n in
  Buffer.add_char buf (Char.chr (v land 0xff));
  Buffer.add_char buf (Char.chr ((v lsr 8) land 0xff));
  Buffer.add_char buf (Char.chr ((v lsr 16) land 0xff));
  Buffer.add_char buf (Char.chr ((v lsr 24) land 0xff))

let copy_object_buffer buf (b : Object.Buffer.t) : unit =
  let n = Object.Buffer.size b in
  for i = 0 to n - 1 do
    Buffer.add_char buf (Char.chr (Bigarray.Array1.unsafe_get b i))
  done

(* Bitfield encoders — inverses of the decoders in sframe.ml. *)

let flags_byte (f : Sframe.flags) : int =
  (if f.fde_sorted then 0x1 else 0)
  lor (if f.frame_pointer then 0x2 else 0)
  lor if f.func_start_pcrel then 0x4 else 0

let abi_arch_byte = function
  | Sframe.Aarch64_be -> 1
  | Sframe.Aarch64_le -> 2
  | Sframe.Amd64_le -> 3

let fre_type_int = function
  | Sframe.Addr1 -> 0
  | Sframe.Addr2 -> 1
  | Sframe.Addr4 -> 2

let fde_type_int = function Sframe.Pcinc -> 0 | Sframe.Pcmask -> 1
let pauth_key_int = function Sframe.Key_a -> 0 | Sframe.Key_b -> 1

let fde_info_byte (fde : Sframe.fde) : int =
  fre_type_int fde.fre_type
  lor (fde_type_int fde.fde_type lsl 4)
  lor (pauth_key_int fde.pauth_key lsl 5)

(* Section emission. *)

let write_preamble buf (p : Sframe.preamble) : unit =
  write_u16_le buf p.magic;
  write_u8 buf p.version;
  Buffer.add_char buf (Char.chr (flags_byte p.flags))

let write_header buf (h : Sframe.header) : unit =
  write_preamble buf h.preamble;
  Buffer.add_char buf (Char.chr (abi_arch_byte h.abi_arch));
  write_s8 buf h.cfa_fixed_fp_offset;
  write_s8 buf h.cfa_fixed_ra_offset;
  write_u8 buf h.auxhdr_len;
  write_u32_le buf h.num_fdes;
  write_u32_le buf h.num_fres;
  write_u32_le buf h.fre_len;
  write_u32_le buf h.fde_off;
  write_u32_le buf h.fre_off

let write_fde buf ~version (fde : Sframe.fde) : unit =
  write_s32_le buf fde.func_start_address;
  write_u32_le buf fde.func_size;
  write_u32_le buf fde.func_start_fre_off;
  write_u32_le buf fde.func_num_fres;
  Buffer.add_char buf (Char.chr (fde_info_byte fde));
  if version = 2 then (
    write_u8 buf fde.rep_size;
    write_u16_le buf Unsigned.UInt16.zero (* padding *))

let emit_section buf (t : Sframe.t) : unit =
  let version = Unsigned.UInt8.to_int t.header.preamble.version in
  write_header buf t.header;
  copy_object_buffer buf t.aux_header;
  Array.iter (write_fde buf ~version) t.fdes;
  copy_object_buffer buf t.fres_raw

let to_bytes (t : Sframe.t) : bytes =
  let buf = Buffer.create 256 in
  emit_section buf t;
  Buffer.to_bytes buf

(* ---- Higher-level builder ---- *)

type fde_spec = {
  func_start_address : int;
  func_size : Unsigned.UInt32.t;
  fre_type : Sframe.fre_type;
  fde_type : Sframe.fde_type;
  pauth_key : Sframe.pauth_key;
  rep_size : Unsigned.UInt8.t;
  fres : Sframe.fre array;
}

type section_parts = {
  version : int;
  flags : Sframe.flags;
  abi_arch : Sframe.abi_arch;
  cfa_fixed_fp_offset : int;
  cfa_fixed_ra_offset : int;
  aux_header : bytes;
  fdes : fde_spec array;
}

let fre_addr_bytes = function
  | Sframe.Addr1 -> 1
  | Sframe.Addr2 -> 2
  | Sframe.Addr4 -> 4

let offset_size_bytes = function
  | Sframe.Off_1b -> 1
  | Sframe.Off_2b -> 2
  | Sframe.Off_4b -> 4

let fre_size ~fre_type (fre : Sframe.fre) : int =
  fre_addr_bytes fre_type + 1
  + (fre.info.offset_count * offset_size_bytes fre.info.offset_size)

let fre_info_byte (info : Sframe.fre_info) : int =
  let cfa_bit = match info.cfa_base_reg with `Sp -> 0 | `Fp -> 1 in
  let size_bits =
    match info.offset_size with
    | Sframe.Off_1b -> 0
    | Sframe.Off_2b -> 1
    | Sframe.Off_4b -> 2
  in
  cfa_bit
  lor ((info.offset_count land 0xf) lsl 1)
  lor ((size_bits land 0x3) lsl 5)
  lor if info.mangled_ra then 0x80 else 0

let write_fre_start_address buf ~fre_type (addr : Unsigned.UInt32.t) : unit =
  let n = Unsigned.UInt32.to_int addr in
  match fre_type with
  | Sframe.Addr1 -> Buffer.add_char buf (Char.chr (n land 0xff))
  | Sframe.Addr2 ->
      Buffer.add_char buf (Char.chr (n land 0xff));
      Buffer.add_char buf (Char.chr ((n lsr 8) land 0xff))
  | Sframe.Addr4 -> write_u32_le buf addr

let write_signed_offset buf ~size n =
  match size with
  | Sframe.Off_1b -> write_s8 buf n
  | Sframe.Off_2b ->
      let v = if n < 0 then n + 0x10000 else n in
      Buffer.add_char buf (Char.chr (v land 0xff));
      Buffer.add_char buf (Char.chr ((v lsr 8) land 0xff))
  | Sframe.Off_4b -> write_s32_le buf n

let write_fre buf ~fre_type (fre : Sframe.fre) : unit =
  write_fre_start_address buf ~fre_type fre.start_address;
  Buffer.add_char buf (Char.chr (fre_info_byte fre.info));
  Array.iter
    (fun n -> write_signed_offset buf ~size:fre.info.offset_size n)
    fre.offsets

let emit_from_parts buf (parts : section_parts) : unit =
  if parts.version <> 1 && parts.version <> 2 then
    invalid_arg
      (Printf.sprintf "SFrame builder: unsupported version %d (need 1 or 2)"
         parts.version);
  let fde_size_bytes = if parts.version = 2 then 20 else 17 in
  let aux_len = Bytes.length parts.aux_header in
  let n_fdes = Array.length parts.fdes in

  (* Encode FRE pool, recording each FDE's start offset within it. *)
  let fre_pool = Buffer.create 256 in
  let fde_records =
    Array.map
      (fun (spec : fde_spec) ->
        let start_off = Buffer.length fre_pool in
        Array.iter
          (fun fre -> write_fre fre_pool ~fre_type:spec.fre_type fre)
          spec.fres;
        ({
           Sframe.func_start_address = spec.func_start_address;
           func_size = spec.func_size;
           func_start_fre_off = Unsigned.UInt32.of_int start_off;
           func_num_fres = Unsigned.UInt32.of_int (Array.length spec.fres);
           fre_type = spec.fre_type;
           fde_type = spec.fde_type;
           pauth_key = spec.pauth_key;
           rep_size = spec.rep_size;
         }
          : Sframe.fde))
      parts.fdes
  in
  let fre_len = Buffer.length fre_pool in
  let total_num_fres =
    Array.fold_left
      (fun acc (s : fde_spec) -> acc + Array.length s.fres)
      0 parts.fdes
  in

  let header : Sframe.header =
    {
      preamble =
        {
          magic = Unsigned.UInt16.of_int 0xdee2;
          version = Unsigned.UInt8.of_int parts.version;
          flags = parts.flags;
        };
      abi_arch = parts.abi_arch;
      cfa_fixed_fp_offset = parts.cfa_fixed_fp_offset;
      cfa_fixed_ra_offset = parts.cfa_fixed_ra_offset;
      auxhdr_len = Unsigned.UInt8.of_int aux_len;
      num_fdes = Unsigned.UInt32.of_int n_fdes;
      num_fres = Unsigned.UInt32.of_int total_num_fres;
      fre_len = Unsigned.UInt32.of_int fre_len;
      fde_off = Unsigned.UInt32.zero;
      fre_off = Unsigned.UInt32.of_int (n_fdes * fde_size_bytes);
    }
  in
  write_header buf header;
  Buffer.add_bytes buf parts.aux_header;
  Array.iter (write_fde buf ~version:parts.version) fde_records;
  Buffer.add_buffer buf fre_pool

let to_bytes_from_parts (parts : section_parts) : bytes =
  let buf = Buffer.create 256 in
  emit_from_parts buf parts;
  Buffer.to_bytes buf
