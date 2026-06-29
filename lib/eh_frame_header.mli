(** EH Frame Header parsing for .eh_frame_hdr section.

    The .eh_frame_hdr section provides a sorted table for fast lookup of Frame
    Description Entries in the .eh_frame section. This is used by the runtime
    exception handling mechanism for efficient stack unwinding.

    Reference: Linux Standards Base Core Specification, section 10.6 ("Exception
    Frames") and the System V ABI AMD64 Architecture Supplement, section 4.2.4
*)

open Dwarf_types

(** Pointer encodings (the DW_EH_PE_ constants) used by the header are defined
    in {!Eh_encoding}. *)

type search_table_entry = { initial_location : u64; fde_address : u64 }

type header = {
  version : u8;  (** Header version (typically 1) *)
  eh_frame_ptr_enc : Eh_encoding.encoding;
      (** Encoding used for the eh_frame_ptr field *)
  fde_count_enc : Eh_encoding.encoding;
      (** Encoding used for the fde_count field *)
  table_enc : Eh_encoding.encoding;
      (** Encoding used for search table entries *)
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
