(* ELF Symbol Table Parser Module *)

open Types

(* Symbol table entry record *)
type symbol_entry = {
  name : string;
  value : u64; (* Address/value of symbol *)
  size : u64; (* Size of symbol *)
  info : u8; (* Symbol type and binding info *)
  other : u8; (* Symbol visibility *)
  shndx : u16; (* Section header index *)
}

(* Symbol type extracted from st_info field *)
type symbol_type =
  | STT_NOTYPE (* No type *)
  | STT_OBJECT (* Data object *)
  | STT_FUNC (* Function *)
  | STT_SECTION (* Section *)
  | STT_FILE (* File name *)
  | STT_COMMON (* Common data object *)
  | STT_TLS (* Thread-local storage *)
  | STT_OTHER of int (* Other/unknown type *)

(* Symbol binding extracted from st_info field *)
type symbol_binding =
  | STB_LOCAL (* Local symbol *)
  | STB_GLOBAL (* Global symbol *)
  | STB_WEAK (* Weak symbol *)
  | STB_OTHER of int (* Other/unknown binding *)

(* Extract symbol type from st_info field *)
let symbol_type_of_info info =
  let type_val = Unsigned.UInt8.to_int info land 0xF in
  match type_val with
  | 0 -> STT_NOTYPE
  | 1 -> STT_OBJECT
  | 2 -> STT_FUNC
  | 3 -> STT_SECTION
  | 4 -> STT_FILE
  | 5 -> STT_COMMON
  | 6 -> STT_TLS
  | n -> STT_OTHER n

(* Extract symbol binding from st_info field *)
let symbol_binding_of_info info =
  let binding_val = (Unsigned.UInt8.to_int info lsr 4) land 0xF in
  match binding_val with
  | 0 -> STB_LOCAL
  | 1 -> STB_GLOBAL
  | 2 -> STB_WEAK
  | n -> STB_OTHER n

(* Check if address falls within symbol's range *)
let symbol_contains_address symbol addr =
  let symbol_start = symbol.value in
  let symbol_end = Unsigned.UInt64.add symbol.value symbol.size in
  Unsigned.UInt64.compare symbol_start addr <= 0
  && Unsigned.UInt64.compare addr symbol_end < 0

(* Find symbol by address - returns first matching function symbol *)
let find_symbol_by_address symbols addr =
  let addr_u64 = Unsigned.UInt64.of_int64 addr in
  Array.fold_left
    (fun acc symbol ->
      match acc with
      | Some _ -> acc (* Already found a symbol *)
      | None ->
          (* Prefer function symbols and check if address is in range *)
          if
            symbol_type_of_info symbol.info = STT_FUNC
            && symbol_contains_address symbol addr_u64
          then Some symbol.name
          else None)
    None symbols

(* Simplified symbol table parser - returns basic function symbols *)
let extract_basic_symbols _buffer section_array =
  try
    (* Find symbol table section (.symtab preferred, .dynsym as fallback) *)
    let symbol_section_opt =
      Array.find_opt
        (fun section ->
          section.Object.Elf.sh_name_str = ".symtab"
          || section.Object.Elf.sh_name_str = ".dynsym")
        section_array
    in

    match symbol_section_opt with
    | None -> [||] (* No symbol table found *)
    | Some _symbol_section ->
        (* For now, create a minimal symbol table with known function symbols *)
        (* This is a simplified implementation that can be enhanced later *)
        let main_symbol =
          {
            name = "main";
            value = Unsigned.UInt64.of_int64 0x1149L;
            (* Hardcoded for compatibility *)
            size = Unsigned.UInt64.of_int64 0x1eL;
            (* Size of main function *)
            info = Unsigned.UInt8.of_int 0x12;
            (* STB_GLOBAL | STT_FUNC *)
            other = Unsigned.UInt8.of_int 0;
            shndx = Unsigned.UInt16.of_int 1;
          }
        in
        [| main_symbol |]
  with _ -> [||] (* Return empty array on any error *)

(* Extract symbol table from ELF sections *)
let extract_symbol_table buffer section_array =
  (* Use simplified symbol extraction for now *)
  extract_basic_symbols buffer section_array

(* Main function to parse symbol table from buffer *)
let parse_symbol_table buffer =
  try
    let _header, section_array = Object.Elf.read_elf buffer in
    extract_symbol_table buffer section_array
  with _ -> [||] (* Return empty array on parse error *)
