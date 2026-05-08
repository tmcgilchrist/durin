(** SFrame stack-trace format support for ELF binaries.

    SFrame ("Simple Frame format") is a compact stack-unwinding format
    introduced by GNU binutils. It is stored in the [.sframe] ELF section
    (segment type [PT_GNU_SFRAME]) and tracks the minimum information needed to
    unwind a stack: the Canonical Frame Address (CFA), Frame Pointer (FP), and
    Return Address (RA).

    SFrame is ELF-only and currently defined only for AMD64 (little-endian) and
    AArch64 (both endians). This implementation supports versions
    [SFRAME_VERSION_1] and [SFRAME_VERSION_2].

    References:
    - https://sourceware.org/binutils/docs-2.41/sframe-spec.html
    - https://github.com/tmcgilchrist/durin/issues/11 *)

open Dwarf_types

exception Invalid_sframe_format of string
(** Raised when the section bytes do not match a supported SFrame layout. *)

(** ABI/architecture identifier (header field [sfh_abi_arch]). *)
type abi_arch =
  | Aarch64_be  (** [SFRAME_ABI_AARCH64_ENDIAN_BIG] = 1 *)
  | Aarch64_le  (** [SFRAME_ABI_AARCH64_ENDIAN_LITTLE] = 2 *)
  | Amd64_le  (** [SFRAME_ABI_AMD64_ENDIAN_LITTLE] = 3 *)

type flags = {
  fde_sorted : bool;  (** [SFRAME_F_FDE_SORTED] (0x1): FDEs sorted by PC. *)
  frame_pointer : bool;
      (** [SFRAME_F_FRAME_POINTER] (0x2): functions preserve FP. *)
  func_start_pcrel : bool;
      (** [SFRAME_F_FDE_FUNC_START_PCREL] (0x4): when set,
          [fde.func_start_address] is PC-relative to the FDE's location in the
          section rather than an absolute VMA. Added in binutils 2.42; enabled
          by default by gcc/gas when emitting [.sframe]. *)
}
(** Section-wide flag bits stored in the preamble. *)

type preamble = {
  magic : u16;  (** [SFRAME_MAGIC] = [0xdee2] in target endianness. *)
  version : u8;  (** [SFRAME_VERSION_1] = 1 (obsolete) or 2 (current). *)
  flags : flags;
}
(** SFrame preamble — first 4 bytes of the section. *)

type header = {
  preamble : preamble;
  abi_arch : abi_arch;
  cfa_fixed_fp_offset : int;
      (** Signed 8-bit offset from CFA to FP, when fixed for the ABI. *)
  cfa_fixed_ra_offset : int;
      (** Signed 8-bit offset from CFA to RA, when fixed for the ABI. *)
  auxhdr_len : u8;
      (** Length in bytes of the auxiliary header following the main header. *)
  num_fdes : u32;  (** Number of FDE records in the section. *)
  num_fres : u32;  (** Total number of FRE records across all FDEs. *)
  fre_len : u32;  (** Length in bytes of the FRE sub-section. *)
  fde_off : u32;
      (** Offset to the FDE sub-section, relative to the end of the SFrame
          header (including the auxiliary header if present). *)
  fre_off : u32;
      (** Offset to the FRE sub-section, relative to the end of the SFrame
          header (including the auxiliary header if present). *)
}
(** SFrame header — 28 bytes including the preamble. *)

(** FDE type, encoded in bit 4 of [sfde_func_info]. *)
type fde_type =
  | Pcinc  (** [SFRAME_FDE_TYPE_PCINC] = 0: FRE addrs are PC offsets. *)
  | Pcmask
      (** [SFRAME_FDE_TYPE_PCMASK] = 1: FRE addrs are PC masks (used with
          [rep_size] for repeating code blocks). *)

(** FRE address-width type, encoded in bits 0–3 of [sfde_func_info]. All FREs
    within a single FDE use the same type. *)
type fre_type =
  | Addr1  (** [SFRAME_FRE_TYPE_ADDR1] = 0: 1-byte function-relative addr. *)
  | Addr2  (** [SFRAME_FRE_TYPE_ADDR2] = 1: 2-byte function-relative addr. *)
  | Addr4  (** [SFRAME_FRE_TYPE_ADDR4] = 2: 4-byte function-relative addr. *)

(** AArch64 PAUTH key selector, encoded in bit 5 of [sfde_func_info]. Only
    meaningful on AArch64; ignored on other architectures. *)
type pauth_key =
  | Key_a  (** [SFRAME_AARCH64_PAUTH_KEY_A] = 0 *)
  | Key_b  (** [SFRAME_AARCH64_PAUTH_KEY_B] = 1 *)

type fde = {
  func_start_address : int;
      (** Signed 32-bit virtual memory address of the function start. *)
  func_size : u32;  (** Function size in bytes. *)
  func_start_fre_off : u32;
      (** Offset (in bytes) to this function's first FRE, relative to the end of
          the FDE sub-section. *)
  func_num_fres : u32;  (** Number of FREs for this function. *)
  fre_type : fre_type;
  fde_type : fde_type;
  pauth_key : pauth_key;
  rep_size : u8;
      (** Size of the repeating code block when [fde_type = Pcmask]. Always 0 in
          V1 (field did not exist) and meaningless when [fde_type = Pcinc]. *)
}
(** SFrame Function Descriptor Entry. 17 bytes in V1, 20 bytes in V2 (V2 added
    [rep_size] plus 2 bytes of padding). *)

(** FRE stack-offset width, encoded in bits 5–6 of [sfre_info]. *)
type fre_offset_size =
  | Off_1b  (** [SFRAME_FRE_OFFSET_1B] = 0 *)
  | Off_2b  (** [SFRAME_FRE_OFFSET_2B] = 1 *)
  | Off_4b  (** [SFRAME_FRE_OFFSET_4B] = 2 *)

type fre_info = {
  cfa_base_reg : [ `Sp | `Fp ];
      (** Bit 0: [1] = CFA derived from SP, [0] = CFA derived from FP. The
          docs-2.41 spec writes this the opposite way around, but the binutils
          implementation (and therefore real-world [.sframe] data) uses SP=1,
          FP=0. *)
  offset_count : int;
      (** Bits 1–4: number of stack offsets following the FRE header (1–3).
          Offset 1: CFA = base_reg + offset1. Offset 2: RA (or FP if RA not
          tracked). Offset 3: FP (when RA also tracked). *)
  offset_size : fre_offset_size;  (** Bits 5–6: byte width of each offset. *)
  mangled_ra : bool;  (** Bit 7: RA is signed/mangled (AArch64 PAUTH). *)
}
(** SFrame FRE info word — one byte ([sfre_info]) packed bitfield. *)

type fre = {
  start_address : u32;
      (** Function-relative byte offset where this FRE becomes active. The
          on-disk width depends on [fde.fre_type]; we widen it to [u32] in the
          OCaml representation. *)
  info : fre_info;
  offsets : int array;
      (** Signed stack offsets, length = [info.offset_count]. *)
}
(** SFrame Frame Row Entry — variable length: a 1/2/4-byte address, a 1-byte
    info word, then [info.offset_count] signed offsets each [info.offset_size]
    bytes wide. *)

type t = {
  header : header;
  aux_header : Object.Buffer.t;
      (** Opaque blob of length [header.auxhdr_len]. *)
  fdes : fde array;
  fres_raw : Object.Buffer.t;
      (** Raw FRE pool. Decode per-FDE on demand via [fres_of_fde] rather than
          eagerly materialising every FRE in memory. *)
}
(** Parsed SFrame section. *)

val parse : Object.Buffer.cursor -> int -> t
(** [parse cursor size] parses an SFrame section starting at [cursor] and
    spanning [size] bytes.

    @raise Invalid_sframe_format
      if the magic, version, ABI identifier, or layout is unrecognised. *)

val fres_of_fde : t -> fde -> fre array
(** [fres_of_fde t fde] decodes the FREs belonging to [fde] from the raw FRE
    pool. *)

val find_fde_for_pc : t -> int -> fde option
(** [find_fde_for_pc t pc] returns the FDE whose function range contains the
    given program counter, or [None] if no FDE matches. Uses binary search when
    [t.header.preamble.flags.fde_sorted] is set. *)

val string_of_abi_arch : abi_arch -> string
val string_of_fde_type : fde_type -> string
val string_of_fre_type : fre_type -> string
val string_of_pauth_key : pauth_key -> string
val string_of_fre_offset_size : fre_offset_size -> string

val string_of_version : u8 -> string
(** Format as ["SFRAME_VERSION_N"], matching the [SFRAME_VERSION_*] tokens used
    by [readelf --sframe]. *)

val flag_names : flags -> string list
(** Returns the [SFRAME_F_*] token of each flag set in [f], in spec order:
    [FDE_SORTED], [FRAME_POINTER], [FDE_FUNC_START_PCREL]. *)

val string_of_cfa_base_reg : [ `Sp | `Fp ] -> string
(** ["sp"] or ["fp"]. *)

val format_cfa : fre -> string
(** Format the CFA specification of a parsed FRE — e.g. ["sp+16"], ["fp-8"].
    Uses [info.cfa_base_reg] and [offsets.(0)]; valid regardless of ABI. *)

val resolve_func_start_pc : t -> section_addr:int -> fde_index:int -> int
(** Resolve the absolute virtual memory address of the function described by FDE
    [fde_index]. When [SFRAME_F_FDE_FUNC_START_PCREL] is set in the section's
    flags, the stored [func_start_address] is PC-relative to the FDE's location
    in memory, so [section_addr] (typically the ELF section header's [sh_addr])
    is needed to recover the absolute PC. When the flag is unset,
    [func_start_address] is returned unchanged. *)
