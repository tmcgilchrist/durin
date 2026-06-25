(** DW_EH_PE pointer encodings for [.eh_frame] / [.eh_frame_hdr].

    A pointer in the exception-handling sections is stored as a value preceded
    by an encoding byte that selects its {!pe_format} (size/signedness) and
    {!pe_application} (how it is resolved to an address). This module decodes
    those encodings and reads values described by them; it is shared by
    {!Eh_frame_header} (the [.eh_frame_hdr] search table) and {!Eh_frame} (FDE
    [initial_location]).

    Reference: Linux Standards Base Core Specification, section 10.6 ("Exception
    Frames"). *)

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

val encoding_of_u8 : int -> encoding
(** Decode an [.eh_frame] pointer-encoding byte into its {!pe_format},
    {!pe_application} and [indirect] components.

    @raise Dwarf.Parse_error
      if the format or application nibble is not recognized. *)

val string_of_encoding : encoding -> string
(** Human-readable description of an encoding, e.g.
    ["PC-relative signed 4-byte"]. *)

val read_encoded_value : Object.Buffer.cursor -> encoding -> u64 -> u64
(** [read_encoded_value cursor encoding base_addr] reads one value described by
    [encoding] from [cursor], advancing it. PC-relative encodings are resolved
    against the value's own position in the buffer; data-relative encodings
    against [base_addr] (the section base).

    @raise Dwarf.Parse_error
      on [DW_EH_PE_omit], indirect encodings, or text-, function-relative and
      aligned applications, none of which are supported. *)
