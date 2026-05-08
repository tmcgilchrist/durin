(** Binary serialisation of SFrame sections.

    See {!Sframe} for the parsed-side types. Writing is straightforward
    because all SFrame structures are fixed-layout: the only variable-length
    region is the FRE pool, which is held verbatim as bytes inside
    [Sframe.t.fres_raw] and copied through unchanged. *)

module Buffer = Stdlib.Buffer

val emit_section : Buffer.t -> Sframe.t -> unit
(** [emit_section buf t] appends the on-disk byte representation of [t] to
    [buf]. The header's count/offset fields are honoured as-is — callers
    constructing an [Sframe.t] by hand must ensure they are consistent with
    the FDE array, FRE pool, and aux header. *)

val to_bytes : Sframe.t -> bytes
(** [to_bytes t] renders [t] to a fresh [bytes]. *)
