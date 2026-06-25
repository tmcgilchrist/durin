(* DW_EH_PE pointer encodings shared by .eh_frame_hdr (Eh_frame_header) and
   .eh_frame (Eh_frame) parsing.

   An .eh_frame pointer encoding is a single byte made of two independent
   nibbles plus a flag (DWARF/LSB):
   - the low nibble (0x0f) selects the value [format];
   - bits 4-6 (0x70) select how the value is [applied];
   - bit 7 (0x80) is the [indirect] flag: the stored value is the address of
     the real value;
   - the whole byte 0xff (omit) means no value is present.
   The two nibbles combine freely, so they must be decoded separately rather
   than enumerated as whole-byte cases. *)

open Dwarf_types

type pe_format =
  | DW_EH_PE_absptr (* 0x00: target-address-sized *)
  | DW_EH_PE_uleb128 (* 0x01 *)
  | DW_EH_PE_udata2 (* 0x02 *)
  | DW_EH_PE_udata4 (* 0x03 *)
  | DW_EH_PE_udata8 (* 0x04 *)
  | DW_EH_PE_sleb128 (* 0x09 *)
  | DW_EH_PE_sdata2 (* 0x0a *)
  | DW_EH_PE_sdata4 (* 0x0b *)
  | DW_EH_PE_sdata8 (* 0x0c *)

type pe_application =
  | DW_EH_PE_pcrel (* 0x10: relative to the value's own address *)
  | DW_EH_PE_textrel (* 0x20: relative to the text section *)
  | DW_EH_PE_datarel (* 0x30: relative to the section base *)
  | DW_EH_PE_funcrel (* 0x40: relative to the enclosing function *)
  | DW_EH_PE_aligned (* 0x50: aligned to the address size *)

type encoding =
  | DW_EH_PE_omit
  | DW_EH_PE_encoding of {
      format : pe_format;
      application : pe_application option; (* None = absolute (0x00) *)
      indirect : bool; (* DW_EH_PE_indirect, 0x80 *)
    }

let encoding_of_u8 byte =
  if byte = 0xff then DW_EH_PE_omit
  else
    let format =
      match byte land 0x0f with
      | 0x00 -> DW_EH_PE_absptr
      | 0x01 -> DW_EH_PE_uleb128
      | 0x02 -> DW_EH_PE_udata2
      | 0x03 -> DW_EH_PE_udata4
      | 0x04 -> DW_EH_PE_udata8
      | 0x09 -> DW_EH_PE_sleb128
      | 0x0a -> DW_EH_PE_sdata2
      | 0x0b -> DW_EH_PE_sdata4
      | 0x0c -> DW_EH_PE_sdata8
      | n -> fail (Printf.sprintf "Unknown EH pointer encoding format: 0x%x" n)
    in
    let application =
      match byte land 0x70 with
      | 0x00 -> None
      | 0x10 -> Some DW_EH_PE_pcrel
      | 0x20 -> Some DW_EH_PE_textrel
      | 0x30 -> Some DW_EH_PE_datarel
      | 0x40 -> Some DW_EH_PE_funcrel
      | 0x50 -> Some DW_EH_PE_aligned
      | n ->
          fail
            (Printf.sprintf "Unknown EH pointer encoding application: 0x%02x" n)
    in
    DW_EH_PE_encoding { format; application; indirect = byte land 0x80 <> 0 }

let string_of_pe_format = function
  | DW_EH_PE_absptr -> "absolute pointer"
  | DW_EH_PE_uleb128 -> "unsigned LEB128"
  | DW_EH_PE_udata2 -> "unsigned 2-byte"
  | DW_EH_PE_udata4 -> "unsigned 4-byte"
  | DW_EH_PE_udata8 -> "unsigned 8-byte"
  | DW_EH_PE_sleb128 -> "signed LEB128"
  | DW_EH_PE_sdata2 -> "signed 2-byte"
  | DW_EH_PE_sdata4 -> "signed 4-byte"
  | DW_EH_PE_sdata8 -> "signed 8-byte"

let string_of_pe_application = function
  | DW_EH_PE_pcrel -> "PC-relative "
  | DW_EH_PE_textrel -> "text-relative "
  | DW_EH_PE_datarel -> "data-relative "
  | DW_EH_PE_funcrel -> "function-relative "
  | DW_EH_PE_aligned -> "aligned "

let string_of_encoding = function
  | DW_EH_PE_omit -> "omit"
  | DW_EH_PE_encoding { format; application; indirect } ->
      Printf.sprintf "%s%s%s"
        (if indirect then "indirect " else "")
        (match application with
        | None -> ""
        | Some a -> string_of_pe_application a)
        (string_of_pe_format format)

(* Read one value described by [encoding]. PC-relative encodings are resolved
   against the value's own position in the buffer; data-relative encodings
   against [base_addr] (the section base). *)
let read_encoded_value cursor encoding base_addr =
  match encoding with
  | DW_EH_PE_omit -> fail "Cannot read a DW_EH_PE_omit-encoded value"
  | DW_EH_PE_encoding { indirect = true; _ } ->
      fail "Indirect EH pointer encodings are not supported"
  | DW_EH_PE_encoding { format; application; indirect = false } -> (
      let value_pos = cursor.Object.Buffer.position in
      let raw =
        match format with
        | DW_EH_PE_absptr -> Object.Buffer.Read.u64 cursor
        | DW_EH_PE_uleb128 ->
            Unsigned.UInt64.of_int (Object.Buffer.Read.uleb128 cursor)
        | DW_EH_PE_udata2 ->
            Unsigned.UInt64.of_int
              (Unsigned.UInt16.to_int (Object.Buffer.Read.u16 cursor))
        | DW_EH_PE_udata4 ->
            Unsigned.UInt64.of_uint32 (Object.Buffer.Read.u32 cursor)
        | DW_EH_PE_udata8 -> Object.Buffer.Read.u64 cursor
        | DW_EH_PE_sleb128 ->
            Unsigned.UInt64.of_int64
              (Int64.of_int (Object.Buffer.Read.sleb128 cursor))
        | DW_EH_PE_sdata2 ->
            let v = Unsigned.UInt16.to_int (Object.Buffer.Read.u16 cursor) in
            let signed = if v >= 0x8000 then v - 0x10000 else v in
            Unsigned.UInt64.of_int64 (Int64.of_int signed)
        | DW_EH_PE_sdata4 ->
            let v = Object.Buffer.Read.u32 cursor in
            Unsigned.UInt64.of_int64
              (Int64.of_int32 (Unsigned.UInt32.to_int32 v))
        | DW_EH_PE_sdata8 -> Object.Buffer.Read.u64 cursor
      in
      match application with
      | None -> raw
      | Some DW_EH_PE_pcrel ->
          Unsigned.UInt64.add (Unsigned.UInt64.of_int value_pos) raw
      | Some DW_EH_PE_datarel -> Unsigned.UInt64.add base_addr raw
      | Some (DW_EH_PE_textrel | DW_EH_PE_funcrel) ->
          fail "Text- and function-relative EH encodings are not supported"
      | Some DW_EH_PE_aligned ->
          fail "Aligned EH pointer encodings are not supported")
