(** Common aliases to make more explicit the nature of values being read. *)

type s8 = Signed.Int8.t
type u8 = Unsigned.UInt8.t
type u16 = Unsigned.UInt16.t
type s32 = Signed.Int32.t
type u32 = Unsigned.UInt32.t
type u64 = Unsigned.UInt64.t
type i64 = Signed.Int64.t
type s128 = int (* Ahem, we don't expect 128 bits to really consume 128 bits *)
type u128 = int
type size_t = u64

(** Sections that hold DWARF information. *)
type dwarf_section =
  | Debug_info
  | Debug_abbrev
  | Debug_aranges
  | Debug_line
  | Debug_line_str
  | Debug_loclists
  | Debug_rnglists
  | Debug_str
  | Debug_str_offs
  | Debug_names
  | Debug_addr
  | Debug_macro
  | Debug_frame
  | Debug_loc
  | Debug_ranges
  | Debug_pubnames
  | Debug_pubtypes
  | Debug_types
  | Debug_info_dwo
  | Debug_abbrev_dwo
  | Debug_line_dwo
  | Debug_loclists_dwo
  | Debug_rnglists_dwo
  | Debug_str_dwo
  | Debug_str_offs_dwo
  | Debug_macro_dwo
  | Debug_macinfo
  | Debug_cu_index
  | Debug_tu_index

type parse_error = {
  section : dwarf_section option;
  offset : int option;
  message : string;
}
(** Structured description of a DWARF parsing failure. Defined here so every
    module in the library (readers, writers and helpers) can raise the same
    exception. *)

exception Parse_error of parse_error

(* Raise a [Parse_error]. [section] and [offset] add context where known. *)
let fail ?section ?offset message =
  raise (Parse_error { section; offset; message })
