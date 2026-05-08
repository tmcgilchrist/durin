(** Binary serialisation of SFrame sections.

    See {!Sframe} for the parsed-side types. Writing is straightforward because
    all SFrame structures are fixed-layout: the only variable-length region is
    the FRE pool, which is held verbatim as bytes inside [Sframe.t.fres_raw] and
    copied through unchanged. *)

module Buffer = Stdlib.Buffer

val emit_section : Buffer.t -> Sframe.t -> unit
(** [emit_section buf t] appends the on-disk byte representation of [t] to
    [buf]. The header's count/offset fields are honoured as-is — callers
    constructing an [Sframe.t] by hand must ensure they are consistent with the
    FDE array, FRE pool, and aux header. *)

val to_bytes : Sframe.t -> bytes
(** [to_bytes t] renders [t] to a fresh [bytes]. *)

(** {1 Higher-level builder}

    {!emit_from_parts} / {!to_bytes_from_parts} take FDE descriptors plus their
    FREs and compute the offsets, counts, and FRE pool layout that
    {!emit_section} otherwise expects callers to populate by hand. *)

type fde_spec = {
  func_start_address : int;  (** int32_t signed VMA of function start. *)
  func_size : Unsigned.UInt32.t;
  fre_type : Sframe.fre_type;
  fde_type : Sframe.fde_type;
  pauth_key : Sframe.pauth_key;
  rep_size : Unsigned.UInt8.t;
      (** Significant only when [fde_type = Pcmask]; pass [zero] otherwise. *)
  fres : Sframe.fre array;
}
(** User-controlled inputs for one FDE. The [func_start_fre_off] and
    [func_num_fres] fields of [Sframe.fde] are derived from the [fres] array, so
    they aren't part of [fde_spec]. *)

type section_parts = {
  version : int;  (** 1 or 2. *)
  flags : Sframe.flags;
  abi_arch : Sframe.abi_arch;
  cfa_fixed_fp_offset : int;
  cfa_fixed_ra_offset : int;
  aux_header : bytes;
      (** Opaque blob written verbatim after the main header. Pass [Bytes.empty]
          when no auxiliary header is needed. *)
  fdes : fde_spec array;
}
(** All inputs needed to build a complete SFrame section. *)

val to_bytes_from_parts : section_parts -> bytes
(** Serialise a section described by [parts] to a fresh [bytes].

    Computes [num_fdes], [num_fres], [fre_len], [fde_off], [fre_off],
    [auxhdr_len], and per-FDE [func_start_fre_off] / [func_num_fres].

    Each FRE's start address is encoded at the width specified by its enclosing
    FDE's [fre_type]; offsets are encoded at the width specified by their FRE's
    [info.offset_size].

    @raise Invalid_argument if [version] is not 1 or 2. *)

val emit_from_parts : Buffer.t -> section_parts -> unit
(** Like {!to_bytes_from_parts} but appends to an existing [Buffer.t]. *)

val fre_size : fre_type:Sframe.fre_type -> Sframe.fre -> int
(** Byte length of an FRE on disk: address bytes (per [fre_type]) + 1 info byte
    \+ [offset_count × offset_size_bytes]. *)
