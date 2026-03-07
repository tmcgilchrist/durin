open Types
module Buffer = Stdlib.Buffer

let write_u8 buf (v : u8) =
  Buffer.add_char buf (Char.chr (Unsigned.UInt8.to_int v))

let write_u16_le buf (v : u16) =
  let i = Unsigned.UInt16.to_int v in
  Buffer.add_char buf (Char.chr (i land 0xff));
  Buffer.add_char buf (Char.chr ((i lsr 8) land 0xff))

let write_u32_le buf (v : u32) =
  let open Unsigned.UInt32 in
  let byte n = to_int (logand (shift_right v n) (of_int 0xff)) in
  Buffer.add_char buf (Char.chr (byte 0));
  Buffer.add_char buf (Char.chr (byte 8));
  Buffer.add_char buf (Char.chr (byte 16));
  Buffer.add_char buf (Char.chr (byte 24))

let write_u64_le buf (v : u64) =
  let open Unsigned.UInt64 in
  let byte n = to_int (logand (shift_right v n) (of_int 0xff)) in
  for i = 0 to 7 do
    Buffer.add_char buf (Char.chr (byte (i * 8)))
  done

let write_i64_le buf (v : i64) =
  let u = Unsigned.UInt64.of_int64 (Signed.Int64.to_int64 v) in
  write_u64_le buf u

let write_uleb128 buf (v : u64) =
  let open Unsigned.UInt64 in
  let mask = of_int 0x7f in
  let rec loop v =
    let byte = to_int (logand v mask) in
    let rest = shift_right v 7 in
    if compare rest zero = 0 then Buffer.add_char buf (Char.chr byte)
    else (
      Buffer.add_char buf (Char.chr (byte lor 0x80));
      loop rest)
  in
  loop v

let write_sleb128 buf (v : i64) =
  let open Signed.Int64 in
  let mask = of_int 0x7f in
  let neg_one = of_int (-1) in
  let rec loop v =
    let byte = to_int (logand v mask) in
    let rest = shift_right v 7 in
    let sign_bit = byte land 0x40 in
    if
      (compare rest zero = 0 && sign_bit = 0)
      || (compare rest neg_one = 0 && sign_bit <> 0)
    then Buffer.add_char buf (Char.chr byte)
    else (
      Buffer.add_char buf (Char.chr (byte lor 0x80));
      loop rest)
  in
  loop v

let write_initial_length buf (format : Dwarf.dwarf_format) length =
  match format with
  | DWARF32 -> write_u32_le buf (Unsigned.UInt32.of_int length)
  | DWARF64 ->
      write_u32_le buf (Unsigned.UInt32.of_int 0xffffffff);
      write_u64_le buf (Unsigned.UInt64.of_int length)

let write_offset buf (format : Dwarf.dwarf_format) (v : u64) =
  match format with
  | DWARF32 ->
      let v32 = Unsigned.UInt32.of_int64 (Unsigned.UInt64.to_int64 v) in
      write_u32_le buf v32
  | DWARF64 -> write_u64_le buf v

let write_address buf address_size (v : u64) =
  match address_size with
  | 4 ->
      let v32 = Unsigned.UInt32.of_int64 (Unsigned.UInt64.to_int64 v) in
      write_u32_le buf v32
  | 8 -> write_u64_le buf v
  | n -> failwith (Printf.sprintf "Unsupported address size: %d" n)

let write_null_terminated_string buf s =
  Buffer.add_string buf s;
  Buffer.add_char buf '\x00'
