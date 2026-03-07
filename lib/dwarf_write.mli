open Types
module Buffer = Stdlib.Buffer

val write_u8 : Buffer.t -> u8 -> unit
val write_u16_le : Buffer.t -> u16 -> unit
val write_u32_le : Buffer.t -> u32 -> unit
val write_u64_le : Buffer.t -> u64 -> unit
val write_i64_le : Buffer.t -> i64 -> unit
val write_uleb128 : Buffer.t -> u64 -> unit
val write_sleb128 : Buffer.t -> i64 -> unit
val write_initial_length : Buffer.t -> Dwarf.dwarf_format -> int -> unit
val write_offset : Buffer.t -> Dwarf.dwarf_format -> u64 -> unit
val write_address : Buffer.t -> int -> u64 -> unit
val write_null_terminated_string : Buffer.t -> string -> unit
