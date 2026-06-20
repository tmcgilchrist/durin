(** EH Frame Header parsing for .eh_frame_hdr section.

    The .eh_frame_hdr section provides a sorted table for fast lookup of Frame
    Description Entries in the .eh_frame section. This is used by the runtime
    exception handling mechanism for efficient stack unwinding.

    Reference: Linux Standards Base Core Specification, section 10.6 ("Exception
    Frames") and the System V ABI AMD64 Architecture Supplement, section 4.2.4
*)

open Dwarf_types

(** The value format of an [.eh_frame] pointer encoding (the low nibble of the
    encoding byte). Constructor names follow the DWARF/LSB spec. *)
type pe_format =
  | DW_EH_PE_absptr
  | DW_EH_PE_uleb128
  | DW_EH_PE_udata2
  | DW_EH_PE_udata4
  | DW_EH_PE_udata8
  | DW_EH_PE_sleb128
  | DW_EH_PE_sdata2
  | DW_EH_PE_sdata4
  | DW_EH_PE_sdata8

(** How an [.eh_frame] pointer value is applied (bits 4-6 of the encoding byte).
    The spec has no name for the absolute case (0x00), it is represented as
    [None] in {!encoding}. *)
type pe_application =
  | DW_EH_PE_pcrel  (** relative to the value's own address. *)
  | DW_EH_PE_textrel  (** relative to the text section. *)
  | DW_EH_PE_datarel  (** relative to the section base. *)
  | DW_EH_PE_funcrel  (** relative to the enclosing function. *)
  | DW_EH_PE_aligned  (** aligned to the address size. *)

(** A decoded [.eh_frame] pointer encoding byte (DWARF/LSB). The encoding is
    composed of a {!pe_format}, an optional {!pe_application}, and an [indirect]
    flag that combine freely; [DW_EH_PE_omit] (byte 0xff) means no value is
    present. *)
type encoding =
  | DW_EH_PE_omit
  | DW_EH_PE_encoding of {
      format : pe_format;
      application : pe_application option;
          (** [None] is the absolute application (0x00). *)
      indirect : bool;
          (** [DW_EH_PE_indirect] (0x80): the stored value is the address of the
              real value. *)
    }

type search_table_entry = { initial_location : u64; fde_address : u64 }

type header = {
  version : u8;  (** Header version (typically 1) *)
  eh_frame_ptr_enc : encoding;  (** Encoding used for the eh_frame_ptr field *)
  fde_count_enc : encoding;  (** Encoding used for the fde_count field *)
  table_enc : encoding;  (** Encoding used for search table entries *)
  eh_frame_ptr : u64;  (** Pointer to the start of .eh_frame *)
  fde_count : u32;  (** Number of FDEs in the search table *)
  search_table : search_table_entry array;
      (** Sorted table mapping addresses to FDEs *)
}
(** Parsed .eh_frame_hdr header with its search table. *)

val parse_header : Object.Buffer.cursor -> u64 -> header
(** Parse the .eh_frame_hdr header and search table.

    The second parameter is the base address of the section for relative address
    calculations.

    @raise Dwarf.Parse_error if the header uses an unknown pointer encoding. *)

val parse_section : Object.Buffer.cursor -> u64 -> header
(** Parse complete .eh_frame_hdr section - alias for parse_header.

    @raise Dwarf.Parse_error if the section uses an unknown pointer encoding. *)

val encoding_of_u8 : int -> encoding
(** Decode an [.eh_frame] pointer-encoding byte into its {!pe_format},
    {!pe_application} and [indirect] components.

    @raise Dwarf.Parse_error
      if the format or application nibble is not recognized. *)

val string_of_encoding : encoding -> string
(** Human-readable description of an encoding, e.g.
    ["PC-relative signed 4-byte"]. *)
