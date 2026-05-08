open Dwarf_types

exception Invalid_sframe_format of string

let invalid_format msg = raise (Invalid_sframe_format msg)

(* Spec constants — see https://sourceware.org/binutils/docs-2.41/sframe-spec.html *)
let sframe_magic = 0xdee2
let sframe_magic_swapped = 0xe2de
let sframe_version_1 = 1
let sframe_version_2 = 2
let fde_v1_size = 17
let fde_v2_size = 20

(* Flag bits in sframe_preamble.sfp_flags *)
let flag_fde_sorted = 0x1
let flag_frame_pointer = 0x2
let flag_func_start_pcrel = 0x4

(* ABI/arch identifiers *)
let abi_aarch64_be_id = 1
let abi_aarch64_le_id = 2
let abi_amd64_le_id = 3

(* sfde_func_info bit layout: [pad:2][pauth_key:1][fdetype:1][fretype:4] *)
let fde_info_fretype_mask = 0x0f
let fde_info_fdetype_mask = 0x10
let fde_info_pauth_key_mask = 0x20

(* sfre_info bit layout: [mangled_ra:1][offset_size:2][offset_count:4][cfa_base:1] *)
let fre_info_cfa_base_reg_mask = 0x01
let fre_info_offset_count_mask = 0x1e
let fre_info_offset_count_shift = 1
let fre_info_offset_size_mask = 0x60
let fre_info_offset_size_shift = 5
let fre_info_mangled_ra_mask = 0x80

type abi_arch = Aarch64_be | Aarch64_le | Amd64_le

type flags = {
  fde_sorted : bool;
  frame_pointer : bool;
  func_start_pcrel : bool;
}

type preamble = { magic : u16; version : u8; flags : flags }

type header = {
  preamble : preamble;
  abi_arch : abi_arch;
  cfa_fixed_fp_offset : int;
  cfa_fixed_ra_offset : int;
  auxhdr_len : u8;
  num_fdes : u32;
  num_fres : u32;
  fre_len : u32;
  fde_off : u32;
  fre_off : u32;
}

type fde_type = Pcinc | Pcmask
type fre_type = Addr1 | Addr2 | Addr4
type pauth_key = Key_a | Key_b

type fde = {
  func_start_address : int;
  func_size : u32;
  func_start_fre_off : u32;
  func_num_fres : u32;
  fre_type : fre_type;
  fde_type : fde_type;
  pauth_key : pauth_key;
  rep_size : u8;
}

type fre_offset_size = Off_1b | Off_2b | Off_4b

type fre_info = {
  cfa_base_reg : [ `Sp | `Fp ];
  offset_count : int;
  offset_size : fre_offset_size;
  mangled_ra : bool;
}

type fre = { start_address : u32; info : fre_info; offsets : int array }

type t = {
  header : header;
  aux_header : Object.Buffer.t;
  fdes : fde array;
  fres_raw : Object.Buffer.t;
}

let abi_arch_of_int = function
  | n when n = abi_aarch64_be_id -> Aarch64_be
  | n when n = abi_aarch64_le_id -> Aarch64_le
  | n when n = abi_amd64_le_id -> Amd64_le
  | n -> invalid_format (Printf.sprintf "Unknown SFrame ABI/arch: %d" n)

let string_of_abi_arch = function
  | Aarch64_be -> "AArch64 (big-endian)"
  | Aarch64_le -> "AArch64 (little-endian)"
  | Amd64_le -> "AMD64 (little-endian)"

let string_of_fde_type = function Pcinc -> "PCINC" | Pcmask -> "PCMASK"

let string_of_fre_type = function
  | Addr1 -> "ADDR1"
  | Addr2 -> "ADDR2"
  | Addr4 -> "ADDR4"

let string_of_pauth_key = function Key_a -> "KEY_A" | Key_b -> "KEY_B"

let string_of_fre_offset_size = function
  | Off_1b -> "1B"
  | Off_2b -> "2B"
  | Off_4b -> "4B"

let string_of_version (v : u8) =
  Printf.sprintf "SFRAME_VERSION_%d" (Unsigned.UInt8.to_int v)

let flag_names (f : flags) : string list =
  let bits = ref [] in
  if f.func_start_pcrel then bits := "SFRAME_F_FDE_FUNC_START_PCREL" :: !bits;
  if f.frame_pointer then bits := "SFRAME_F_FRAME_POINTER" :: !bits;
  if f.fde_sorted then bits := "SFRAME_F_FDE_SORTED" :: !bits;
  !bits

let string_of_cfa_base_reg = function `Sp -> "sp" | `Fp -> "fp"

let format_cfa (fre : fre) : string =
  let reg = string_of_cfa_base_reg fre.info.cfa_base_reg in
  let off = fre.offsets.(0) in
  if off >= 0 then Printf.sprintf "%s+%d" reg off
  else Printf.sprintf "%s%d" reg off

let resolve_func_start_pc (t : t) ~section_addr ~fde_index =
  let h = t.header in
  let fde = t.fdes.(fde_index) in
  if h.preamble.flags.func_start_pcrel then
    let version = Unsigned.UInt8.to_int h.preamble.version in
    let fde_size =
      if version = sframe_version_2 then fde_v2_size else fde_v1_size
    in
    let auxhdr_len = Unsigned.UInt8.to_int h.auxhdr_len in
    let fde_off = Unsigned.UInt32.to_int h.fde_off in
    let fde_addr =
      section_addr + 28 (* preamble + main header *) + auxhdr_len
      + fde_off + (fde_index * fde_size)
    in
    fde_addr + fde.func_start_address
  else fde.func_start_address

(* Sign-extend a u32 read as if it were int32_t. On 64-bit OCaml, int is
   63-bit, so the full 32-bit unsigned range fits without overflow. *)
let sign_extend_u32 (v : u32) : int =
  let n = Unsigned.UInt32.to_int v in
  if n >= 0x80000000 then n - 0x100000000 else n

let sign_extend_byte (b : int) : int = if b >= 0x80 then b - 0x100 else b
let sign_extend_u16_int (n : int) : int = if n >= 0x8000 then n - 0x10000 else n

let parse_preamble (cur : Object.Buffer.cursor) : preamble =
  let magic = Object.Buffer.Read.u16 cur in
  let magic_int = Unsigned.UInt16.to_int magic in
  if magic_int = sframe_magic_swapped then
    invalid_format
      "SFrame section is in opposite endianness from host; big-endian support \
       is not yet implemented in Durin"
  else if magic_int <> sframe_magic then
    invalid_format
      (Printf.sprintf "Invalid SFrame magic: 0x%04x (expected 0x%04x)" magic_int
         sframe_magic);
  let version = Object.Buffer.Read.u8 cur in
  let version_int = Unsigned.UInt8.to_int version in
  if version_int <> sframe_version_1 && version_int <> sframe_version_2 then
    invalid_format
      (Printf.sprintf "Unsupported SFrame version: %d (expected 1 or 2)"
         version_int);
  let flags_byte = Object.Buffer.Read.u8 cur in
  let flags_int = Unsigned.UInt8.to_int flags_byte in
  let flags =
    {
      fde_sorted = flags_int land flag_fde_sorted <> 0;
      frame_pointer = flags_int land flag_frame_pointer <> 0;
      func_start_pcrel = flags_int land flag_func_start_pcrel <> 0;
    }
  in
  { magic; version; flags }

let parse_header (cur : Object.Buffer.cursor) : header =
  let preamble = parse_preamble cur in
  let abi_byte = Object.Buffer.Read.u8 cur in
  let abi_arch = abi_arch_of_int (Unsigned.UInt8.to_int abi_byte) in
  let cfa_fixed_fp_offset = Signed.Int8.to_int (Object.Buffer.Read.s8 cur) in
  let cfa_fixed_ra_offset = Signed.Int8.to_int (Object.Buffer.Read.s8 cur) in
  let auxhdr_len = Object.Buffer.Read.u8 cur in
  let num_fdes = Object.Buffer.Read.u32 cur in
  let num_fres = Object.Buffer.Read.u32 cur in
  let fre_len = Object.Buffer.Read.u32 cur in
  let fde_off = Object.Buffer.Read.u32 cur in
  let fre_off = Object.Buffer.Read.u32 cur in
  {
    preamble;
    abi_arch;
    cfa_fixed_fp_offset;
    cfa_fixed_ra_offset;
    auxhdr_len;
    num_fdes;
    num_fres;
    fre_len;
    fde_off;
    fre_off;
  }

let parse_fde_info_byte (b : int) : fre_type * fde_type * pauth_key =
  let fre_type =
    match b land fde_info_fretype_mask with
    | 0 -> Addr1
    | 1 -> Addr2
    | 2 -> Addr4
    | n -> invalid_format (Printf.sprintf "Unknown FRE type: %d" n)
  in
  let fde_type = if b land fde_info_fdetype_mask <> 0 then Pcmask else Pcinc in
  let pauth_key =
    if b land fde_info_pauth_key_mask <> 0 then Key_b else Key_a
  in
  (fre_type, fde_type, pauth_key)

let parse_fde version_int (cur : Object.Buffer.cursor) : fde =
  let raw_addr = Object.Buffer.Read.u32 cur in
  let func_start_address = sign_extend_u32 raw_addr in
  let func_size = Object.Buffer.Read.u32 cur in
  let func_start_fre_off = Object.Buffer.Read.u32 cur in
  let func_num_fres = Object.Buffer.Read.u32 cur in
  let info_byte = Unsigned.UInt8.to_int (Object.Buffer.Read.u8 cur) in
  let fre_type, fde_type, pauth_key = parse_fde_info_byte info_byte in
  let rep_size =
    if version_int = sframe_version_2 then
      let rs = Object.Buffer.Read.u8 cur in
      let _padding = Object.Buffer.Read.u16 cur in
      rs
    else Unsigned.UInt8.zero
  in
  {
    func_start_address;
    func_size;
    func_start_fre_off;
    func_num_fres;
    fre_type;
    fde_type;
    pauth_key;
    rep_size;
  }

let parse_fre_info_byte (b : int) : fre_info =
  (* binutils encodes SP=1, FP=0 (opposite of what docs-2.41 says). *)
  let cfa_base_reg =
    if b land fre_info_cfa_base_reg_mask <> 0 then `Sp else `Fp
  in
  let offset_count =
    (b land fre_info_offset_count_mask) lsr fre_info_offset_count_shift
  in
  if offset_count < 1 || offset_count > 3 then
    invalid_format
      (Printf.sprintf "FRE offset_count out of range: %d (expected 1..3)"
         offset_count);
  let offset_size =
    match (b land fre_info_offset_size_mask) lsr fre_info_offset_size_shift with
    | 0 -> Off_1b
    | 1 -> Off_2b
    | 2 -> Off_4b
    | n -> invalid_format (Printf.sprintf "FRE offset_size out of range: %d" n)
  in
  let mangled_ra = b land fre_info_mangled_ra_mask <> 0 in
  { cfa_base_reg; offset_count; offset_size; mangled_ra }

let read_fre_start_address (fre_type : fre_type) (cur : Object.Buffer.cursor) :
    u32 =
  match fre_type with
  | Addr1 ->
      Object.Buffer.Read.u8 cur |> Unsigned.UInt8.to_int
      |> Unsigned.UInt32.of_int
  | Addr2 ->
      Object.Buffer.Read.u16 cur |> Unsigned.UInt16.to_int
      |> Unsigned.UInt32.of_int
  | Addr4 -> Object.Buffer.Read.u32 cur

let read_fre_offset (size : fre_offset_size) (cur : Object.Buffer.cursor) : int
    =
  match size with
  | Off_1b ->
      let b = Unsigned.UInt8.to_int (Object.Buffer.Read.u8 cur) in
      sign_extend_byte b
  | Off_2b ->
      let n = Unsigned.UInt16.to_int (Object.Buffer.Read.u16 cur) in
      sign_extend_u16_int n
  | Off_4b -> sign_extend_u32 (Object.Buffer.Read.u32 cur)

let parse_fre (fre_type : fre_type) (cur : Object.Buffer.cursor) : fre =
  let start_address = read_fre_start_address fre_type cur in
  let info_byte = Unsigned.UInt8.to_int (Object.Buffer.Read.u8 cur) in
  let info = parse_fre_info_byte info_byte in
  let offsets =
    Array.init info.offset_count (fun _ -> read_fre_offset info.offset_size cur)
  in
  { start_address; info; offsets }

let fres_of_fde (t : t) (fde : fde) : fre array =
  let start = Unsigned.UInt32.to_int fde.func_start_fre_off in
  let cur = Object.Buffer.cursor t.fres_raw ~at:start in
  let n = Unsigned.UInt32.to_int fde.func_num_fres in
  Array.init n (fun _ -> parse_fre fde.fre_type cur)

let fde_contains_pc (fde : fde) (pc : int) : bool =
  let start = fde.func_start_address in
  let size = Unsigned.UInt32.to_int fde.func_size in
  pc >= start && pc < start + size

let find_fde_for_pc (t : t) (pc : int) : fde option =
  let n = Array.length t.fdes in
  if n = 0 then None
  else if t.header.preamble.flags.fde_sorted then (
    (* Binary search for the largest start <= pc, then check range. *)
    let lo = ref 0
    and hi = ref (n - 1)
    and best = ref (-1) in
    while !lo <= !hi do
      let mid = (!lo + !hi) / 2 in
      if t.fdes.(mid).func_start_address <= pc then (
        best := mid;
        lo := mid + 1)
      else hi := mid - 1
    done;
    if !best < 0 then None
    else
      let candidate = t.fdes.(!best) in
      if fde_contains_pc candidate pc then Some candidate else None)
  else
    let rec loop i =
      if i >= n then None
      else if fde_contains_pc t.fdes.(i) pc then Some t.fdes.(i)
      else loop (i + 1)
    in
    loop 0

let parse (cur : Object.Buffer.cursor) (size : int) : t =
  let section_start = cur.position in
  let header = parse_header cur in
  let version_int = Unsigned.UInt8.to_int header.preamble.version in
  let auxhdr_len = Unsigned.UInt8.to_int header.auxhdr_len in
  let aux_header = Object.Buffer.Read.buffer cur auxhdr_len in
  (* The end of header (including auxhdr) is the reference point for
     fde_off / fre_off. *)
  let header_end = cur.position in
  let fde_sub_start = header_end + Unsigned.UInt32.to_int header.fde_off in
  let fre_sub_start = header_end + Unsigned.UInt32.to_int header.fre_off in
  let num_fdes_int = Unsigned.UInt32.to_int header.num_fdes in
  let fde_size =
    if version_int = sframe_version_2 then fde_v2_size else fde_v1_size
  in
  (* Sanity-check that the FDE sub-section fits within the section. *)
  let fde_sub_end = fde_sub_start + (num_fdes_int * fde_size) in
  if fde_sub_end > section_start + size then
    invalid_format
      (Printf.sprintf
         "FDE sub-section extends past end of SFrame section (%d > %d)"
         fde_sub_end (section_start + size));
  Object.Buffer.seek cur fde_sub_start;
  let fdes = Array.init num_fdes_int (fun _ -> parse_fde version_int cur) in
  let fre_len_int = Unsigned.UInt32.to_int header.fre_len in
  if fre_sub_start + fre_len_int > section_start + size then
    invalid_format
      (Printf.sprintf
         "FRE sub-section extends past end of SFrame section (%d > %d)"
         (fre_sub_start + fre_len_int)
         (section_start + size));
  Object.Buffer.seek cur fre_sub_start;
  let fres_raw = Object.Buffer.Read.buffer cur fre_len_int in
  { header; aux_header; fdes; fres_raw }
