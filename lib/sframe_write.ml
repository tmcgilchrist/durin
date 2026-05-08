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
  lor (if f.func_start_pcrel then 0x4 else 0)

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
