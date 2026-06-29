(* EH Frame Header (.eh_frame_hdr section) - ELF exception handling support.
   Pointer encodings (the DW_EH_PE_ constants) live in Eh_encoding. *)

open Dwarf_types
open Eh_encoding

type search_table_entry = {
  initial_location : u64; (* PC value *)
  fde_address : u64; (* Address of FDE *)
}

type header = {
  version : u8;
  eh_frame_ptr_enc : encoding;
  fde_count_enc : encoding;
  table_enc : encoding;
  eh_frame_ptr : u64;
  fde_count : u32;
  search_table : search_table_entry array;
}

let parse_header cursor section_base_addr =
  let version = Object.Buffer.Read.u8 cursor in
  let eh_frame_ptr_enc =
    encoding_of_u8 (Unsigned.UInt8.to_int (Object.Buffer.Read.u8 cursor))
  in
  let fde_count_enc =
    encoding_of_u8 (Unsigned.UInt8.to_int (Object.Buffer.Read.u8 cursor))
  in
  let table_enc =
    encoding_of_u8 (Unsigned.UInt8.to_int (Object.Buffer.Read.u8 cursor))
  in

  let eh_frame_ptr =
    read_encoded_value cursor eh_frame_ptr_enc section_base_addr
  in
  let fde_count_raw =
    read_encoded_value cursor fde_count_enc section_base_addr
  in
  let fde_count =
    Unsigned.UInt32.of_int64 (Unsigned.UInt64.to_int64 fde_count_raw)
  in

  (* Parse search table *)
  let search_table =
    Array.init (Unsigned.UInt32.to_int fde_count) (fun _i ->
        let initial_location =
          read_encoded_value cursor table_enc section_base_addr
        in
        let fde_address =
          read_encoded_value cursor table_enc section_base_addr
        in
        { initial_location; fde_address })
  in

  {
    version;
    eh_frame_ptr_enc;
    fde_count_enc;
    table_enc;
    eh_frame_ptr;
    fde_count;
    search_table;
  }

(** Parse complete .eh_frame_hdr section *)
let parse_section cursor section_base_addr =
  parse_header cursor section_base_addr
