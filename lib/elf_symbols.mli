(* ELF Symbol Table Parser Module Interface *)

open Types

type symbol_entry = {
  name : string;  (** Symbol name *)
  value : u64;  (** Address/value of symbol *)
  size : u64;  (** Size of symbol *)
  info : u8;  (** Symbol type and binding info *)
  other : u8;  (** Symbol visibility *)
  shndx : u16;  (** Section header index *)
}
(** Symbol table entry record *)

(** Symbol type extracted from st_info field *)
type symbol_type =
  | STT_NOTYPE  (** No type *)
  | STT_OBJECT  (** Data object *)
  | STT_FUNC  (** Function *)
  | STT_SECTION  (** Section *)
  | STT_FILE  (** File name *)
  | STT_COMMON  (** Common data object *)
  | STT_TLS  (** Thread-local storage *)
  | STT_OTHER of int  (** Other/unknown type *)

(** Symbol binding extracted from st_info field *)
type symbol_binding =
  | STB_LOCAL  (** Local symbol *)
  | STB_GLOBAL  (** Global symbol *)
  | STB_WEAK  (** Weak symbol *)
  | STB_OTHER of int  (** Other/unknown binding *)

val symbol_type_of_info : u8 -> symbol_type
(** Extract symbol type from st_info field *)

val symbol_binding_of_info : u8 -> symbol_binding
(** Extract symbol binding from st_info field *)

val symbol_contains_address : symbol_entry -> u64 -> bool
(** Check if address falls within symbol's range *)

val find_symbol_by_address : symbol_entry array -> int64 -> string option
(** Find symbol by address - returns first matching function symbol name *)

val parse_symbol_table : Object.Buffer.t -> symbol_entry array
(** Parse symbol table from ELF buffer *)
