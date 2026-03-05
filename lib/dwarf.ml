open Types

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

(** Convert Mach-O cpu_type to architecture string *)
let string_of_cpu_type = function
  | `X86 -> "i386"
  | `X86_64 -> "x86_64"
  | `ARM -> "arm"
  | `ARM64 -> "arm64"
  | `ARM64_32 -> "arm64_32"
  | `POWERPC -> "ppc"
  | `POWERPC64 -> "ppc64"
  | `Unknown x -> Printf.sprintf "unknown_%d" x

(** Convert ELF machine type to architecture string *)
let string_of_elf_machine = function
  | `EM_386 -> "i386"
  | `EM_X86_64 -> "x86_64"
  | `EM_ARM -> "arm"
  | `EM_AARCH64 -> "aarch64"
  | `EM_MIPS -> "mips"
  | `EM_PPC -> "powerpc"
  | `EM_PPC64 -> "powerpc64"
  | `EM_SPARC -> "sparc"
  | `EM_SPARCV9 -> "sparcv9"
  | `EM_IA_64 -> "ia64"
  | `EM_S390 -> "s390"
  | `EM_RISCV -> "riscv"
  | `EM_68K -> "m68k"
  | `EM_88K -> "m88k"
  | `EM_PARISC -> "hppa"
  | `EM_SH -> "sh"
  | `EM_UNKNOWN x -> Printf.sprintf "unknown_%d" x
  | _ -> "unknown"

(** Detect file format and architecture from buffer *)
let detect_format_and_arch (buf : Object.Buffer.t) : string =
  let format = Object_format.detect_format buf in
  match format with
  | ELF -> (
      (* Read ELF header to get architecture information *)
      try
        let open Object.Elf in
        let header, _section_array = read_elf buf in
        let arch_str = string_of_elf_machine header.e_machine in
        let class_str =
          match header.e_ident.elf_class with
          | `ELFCLASS32 -> "elf32"
          | `ELFCLASS64 -> "elf64"
          | `ELFCLASSNONE -> "elf"
        in
        Printf.sprintf "%s-%s" class_str arch_str
      with _ -> "ELF")
  | MACHO ->
      let header, _commands = Object.Macho.read buf in
      let arch_str = string_of_cpu_type header.cpu_type in
      Printf.sprintf "Mach-O %s" arch_str

type unit_type =
  | DW_UT_compile
  | DW_UT_type
  | DW_UT_partial
  | DW_UT_skeleton
  | DW_UT_split_compile
  | DW_UT_split_type
  | DW_UT_lo_user
  | DW_UT_hi_user

let unit_type_of_u8 u =
  match Unsigned.UInt8.to_int u with
  | 0x01 -> DW_UT_compile
  | 0x02 -> DW_UT_type
  | 0x03 -> DW_UT_partial
  | 0x04 -> DW_UT_skeleton
  | 0x05 -> DW_UT_split_compile
  | 0x06 -> DW_UT_split_type
  | 0x80 -> DW_UT_lo_user
  | 0xff -> DW_UT_hi_user
  | n -> failwith (Printf.sprintf "Unknown unit type: 0x%02x" n)

let string_of_unit_type = function
  | DW_UT_compile -> "DW_UT_compile"
  | DW_UT_type -> "DW_UT_type"
  | DW_UT_partial -> "DW_UT_partial"
  | DW_UT_skeleton -> "DW_UT_skeleton"
  | DW_UT_split_compile -> "DW_UT_split_compile"
  | DW_UT_split_type -> "DW_UT_split_type"
  | DW_UT_lo_user -> "DW_UT_lo_user"
  | DW_UT_hi_user -> "DW_UT_hi_user"

type dwarf_format = DWARF32 | DWARF64

let string_of_dwarf_format = function
  | DWARF32 -> "DWARF32"
  | DWARF64 -> "DWARF64"

type encoding = {
  format : dwarf_format;  (** DWARF32 or DWARF64 format *)
  address_size : u8;  (** Size of addresses in bytes *)
  version : u16;  (** DWARF version *)
}
(** Encoding parameters that affect how DWARF data is parsed. Similar to Gimli's
    Encoding struct, this bundles format with related context needed for parsing
    DIE attributes. *)

let parse_initial_length (cur : Object.Buffer.cursor) : dwarf_format * u64 =
  let first_four = Object.Buffer.Read.u32 cur in
  let first_val = Unsigned.UInt32.to_int first_four in
  if first_val = 0xffffffff then
    let actual_length = Object.Buffer.Read.u64 cur in
    (DWARF64, actual_length)
  else if first_val >= 0xfffffff0 then
    failwith
      (Printf.sprintf
         "Reserved initial_length value 0x%08x in range 0xfffffff0-0xfffffffe"
         first_val)
  else (DWARF32, Unsigned.UInt64.of_uint32 first_four)

let offset_size_for_format = function DWARF32 -> 4 | DWARF64 -> 8

let read_offset_for_format (format : dwarf_format) (cur : Object.Buffer.cursor)
    : u64 =
  match format with
  | DWARF32 -> Object.Buffer.Read.u32 cur |> Unsigned.UInt64.of_uint32
  | DWARF64 -> Object.Buffer.Read.u64 cur

type object_format = Object_format.format

let object_format_to_section_name format section =
  match format with
  | Object_format.MACHO -> (
      match section with
      | Debug_info -> "__debug_info"
      | Debug_abbrev -> "__debug_abbrev"
      | Debug_aranges -> "__debug_aranges"
      | Debug_line -> "__debug_line"
      | Debug_line_str -> "__debug_line_str"
      | Debug_loclists -> "__debug_loclists"
      | Debug_rnglists -> "__debug_rnglists"
      | Debug_str -> "__debug_str"
      | Debug_str_offs -> "__debug_str_offs"
      | Debug_names -> "__debug_names"
      | Debug_addr -> "__debug_addr"
      | Debug_macro -> "__debug_macro"
      | Debug_macinfo -> "__debug_macinfo"
      | Debug_frame -> "__debug_frame"
      | Debug_loc -> "__debug_loc"
      | Debug_ranges -> "__debug_ranges"
      | Debug_pubnames -> "__debug_pubnames"
      | Debug_pubtypes -> "__debug_pubtypes"
      | Debug_types -> "__debug_types"
      | Debug_info_dwo -> ".debug_info.dwo"
      | Debug_abbrev_dwo -> ".debug_abbrev.dwo"
      | Debug_line_dwo -> ".debug_line.dwo"
      | Debug_loclists_dwo -> ".debug_loclists.dwo"
      | Debug_rnglists_dwo -> ".debug_rnglists.dwo"
      | Debug_str_dwo -> ".debug_str.dwo"
      | Debug_str_offs_dwo -> ".debug_str_offsets.dwo"
      | Debug_macro_dwo -> ".debug_macro.dwo"
      | Debug_cu_index -> ".debug_cu_index"
      | Debug_tu_index -> ".debug_tu_index")
  | Object_format.ELF -> (
      match section with
      | Debug_info -> ".debug_info"
      | Debug_abbrev -> ".debug_abbrev"
      | Debug_aranges -> ".debug_aranges"
      | Debug_line -> ".debug_line"
      | Debug_line_str -> ".debug_line_str"
      | Debug_loclists -> ".debug_loclists"
      | Debug_rnglists -> ".debug_rnglists"
      | Debug_str -> ".debug_str"
      | Debug_str_offs -> ".debug_str_offsets"
      | Debug_names -> ".debug_names"
      | Debug_addr -> ".debug_addr"
      | Debug_macro -> ".debug_macro"
      | Debug_macinfo -> ".debug_macinfo"
      | Debug_frame -> ".debug_frame"
      | Debug_loc -> ".debug_loc"
      | Debug_ranges -> ".debug_ranges"
      | Debug_pubnames -> ".debug_pubnames"
      | Debug_pubtypes -> ".debug_pubtypes"
      | Debug_types -> ".debug_types"
      | Debug_info_dwo -> ".debug_info.dwo"
      | Debug_abbrev_dwo -> ".debug_abbrev.dwo"
      | Debug_line_dwo -> ".debug_line.dwo"
      | Debug_loclists_dwo -> ".debug_loclists.dwo"
      | Debug_rnglists_dwo -> ".debug_rnglists.dwo"
      | Debug_str_dwo -> ".debug_str.dwo"
      | Debug_str_offs_dwo -> ".debug_str_offsets.dwo"
      | Debug_macro_dwo -> ".debug_macro.dwo"
      | Debug_cu_index -> ".debug_cu_index"
      | Debug_tu_index -> ".debug_tu_index")

include Dwarf_abbreviation_tag
include Dwarf_attribute
include Dwarf_form
include Dwarf_operation
include Dwarf_constants

type line_number_opcode =
  | DW_LNS_copy
  | DW_LNS_advance_pc
  | DW_LNS_advance_line
  | DW_LNS_set_file
  | DW_LNS_set_column
  | DW_LNS_negate_stmt
  | DW_LNS_set_basic_block
  | DW_LNS_const_add_pc
  | DW_LNS_fixed_advance_pc
  | DW_LNS_set_prologue_end
  | DW_LNS_set_epilogue_begin
  | DW_LNS_set_isa

let line_number_opcode = function
  | 0x01 -> DW_LNS_copy
  | 0x02 -> DW_LNS_advance_pc
  | 0x03 -> DW_LNS_advance_line
  | 0x04 -> DW_LNS_set_file
  | 0x05 -> DW_LNS_set_column
  | 0x06 -> DW_LNS_negate_stmt
  | 0x07 -> DW_LNS_set_basic_block
  | 0x08 -> DW_LNS_const_add_pc
  | 0x09 -> DW_LNS_fixed_advance_pc
  | 0x0a -> DW_LNS_set_prologue_end
  | 0x0b -> DW_LNS_set_epilogue_begin
  | 0x0c -> DW_LNS_set_isa
  | n -> failwith (Printf.sprintf "Unknown line_number_opcode: 0x%02x" n)

let string_of_line_number_opcode = function
  | DW_LNS_copy -> "DW_LNS_copy"
  | DW_LNS_advance_pc -> "DW_LNS_advance_pc"
  | DW_LNS_advance_line -> "DW_LNS_advance_line"
  | DW_LNS_set_file -> "DW_LNS_set_file"
  | DW_LNS_set_column -> "DW_LNS_set_column"
  | DW_LNS_negate_stmt -> "DW_LNS_negate_stmt"
  | DW_LNS_set_basic_block -> "DW_LNS_set_basic_block"
  | DW_LNS_const_add_pc -> "DW_LNS_const_add_pc"
  | DW_LNS_fixed_advance_pc -> "DW_LNS_fixed_advance_pc"
  | DW_LNS_set_prologue_end -> "DW_LNS_set_prologue_end"
  | DW_LNS_set_epilogue_begin -> "DW_LNS_set_epilogue_begin"
  | DW_LNS_set_isa -> "DW_LNS_set_isa"

type line_number_extended_opcode =
  | DW_LNE_end_sequence
  | DW_LNE_set_address
  | DW_LNE_set_discriminator
  | DW_LNE_lo_user
  | DW_LNE_hi_user

let line_number_extended_opcode = function
  | 0x01 -> DW_LNE_end_sequence
  | 0x02 -> DW_LNE_set_address
  | 0x04 -> DW_LNE_set_discriminator
  | 0x80 -> DW_LNE_lo_user
  | 0xff -> DW_LNE_hi_user
  | n ->
      failwith (Printf.sprintf "Unknown line_number_extended_opcode: 0x%02x" n)

let string_of_line_number_extended_opcode = function
  | DW_LNE_end_sequence -> "DW_LNE_end_sequence"
  | DW_LNE_set_address -> "DW_LNE_set_address"
  | DW_LNE_set_discriminator -> "DW_LNE_set_discriminator"
  | DW_LNE_lo_user -> "DW_LNE_lo_user"
  | DW_LNE_hi_user -> "DW_LNE_hi_user"

type line_number_header_entry =
  | DW_LNCT_path
  | DW_LNCT_directory_index
  | DW_LNCT_timestamp
  | DW_LNCT_size
  | DW_LNCT_MD5
  | DW_LNCT_lo_user
  | DW_LNCT_hi_user

let line_number_header_entry = function
  | 0x1 -> DW_LNCT_path
  | 0x2 -> DW_LNCT_directory_index
  | 0x3 -> DW_LNCT_timestamp
  | 0x4 -> DW_LNCT_size
  | 0x5 -> DW_LNCT_MD5
  | 0x2000 -> DW_LNCT_lo_user
  | 0x3fff -> DW_LNCT_hi_user
  | n -> failwith (Printf.sprintf "Unknown line_number_header_entry: 0x%04x" n)

let string_of_line_number_header_entry = function
  | DW_LNCT_path -> "DW_LNCT_path"
  | DW_LNCT_directory_index -> "DW_LNCT_directory_index"
  | DW_LNCT_timestamp -> "DW_LNCT_timestamp"
  | DW_LNCT_size -> "DW_LNCT_size"
  | DW_LNCT_MD5 -> "DW_LNCT_MD5"
  | DW_LNCT_lo_user -> "DW_LNCT_lo_user"
  | DW_LNCT_hi_user -> "DW_LNCT_hi_user"

type macro_info_entry_type =
  | DW_MACRO_define
  | DW_MACRO_undef
  | DW_MACRO_start_file
  | DW_MACRO_end_file
  | DW_MACRO_define_strp
  | DW_MACRO_undef_strp
  | DW_MACRO_import
  | DW_MACRO_define_sup
  | DW_MACRO_undef_sup
  | DW_MACRO_import_sup
  | DW_MACRO_define_strx
  | DW_MACRO_undef_strx
  | DW_MACRO_lo_user
  | DW_MACRO_hi_user

let macro_info_entry_type = function
  | 0x01 -> DW_MACRO_define
  | 0x02 -> DW_MACRO_undef
  | 0x03 -> DW_MACRO_start_file
  | 0x04 -> DW_MACRO_end_file
  | 0x05 -> DW_MACRO_define_strp
  | 0x06 -> DW_MACRO_undef_strp
  | 0x07 -> DW_MACRO_import
  | 0x08 -> DW_MACRO_define_sup
  | 0x09 -> DW_MACRO_undef_sup
  | 0x0a -> DW_MACRO_import_sup
  | 0x0b -> DW_MACRO_define_strx
  | 0x0c -> DW_MACRO_undef_strx
  | 0xe0 -> DW_MACRO_lo_user
  | 0xff -> DW_MACRO_hi_user
  | n -> failwith (Printf.sprintf "Unknown macro_info_entry_type: 0x%02x" n)

let macro_info_entry_type_of_u8 u =
  macro_info_entry_type (Unsigned.UInt8.to_int u)

let string_of_macro_info_entry_type = function
  | DW_MACRO_define -> "DW_MACRO_define"
  | DW_MACRO_undef -> "DW_MACRO_undef"
  | DW_MACRO_start_file -> "DW_MACRO_start_file"
  | DW_MACRO_end_file -> "DW_MACRO_end_file"
  | DW_MACRO_define_strp -> "DW_MACRO_define_strp"
  | DW_MACRO_undef_strp -> "DW_MACRO_undef_strp"
  | DW_MACRO_import -> "DW_MACRO_import"
  | DW_MACRO_define_sup -> "DW_MACRO_define_sup"
  | DW_MACRO_undef_sup -> "DW_MACRO_undef_sup"
  | DW_MACRO_import_sup -> "DW_MACRO_import_sup"
  | DW_MACRO_define_strx -> "DW_MACRO_define_strx"
  | DW_MACRO_undef_strx -> "DW_MACRO_undef_strx"
  | DW_MACRO_lo_user -> "DW_MACRO_lo_user"
  | DW_MACRO_hi_user -> "DW_MACRO_hi_user"

(* Debug Macro Section - DWARF 5 Section 6.3 *)

type debug_macro_header = {
  format : dwarf_format;  (** DWARF32 or DWARF64, from flags bit 0 *)
  version : u16;  (** Version number *)
  flags : u8;  (** Flags *)
  debug_line_offset : u64 option;
  debug_str_offsets_offset : u64 option;
}

type debug_macro_entry = {
  entry_type : macro_info_entry_type;  (** Type of macro entry *)
  line_number : u32 option;  (** Line number for certain types *)
  string_offset : u64 option;  (** Offset into string table *)
  string_value : string option;  (** Direct string value *)
  file_index : u32 option;  (** File index for start_file entries *)
}

type debug_macro_unit = {
  header : debug_macro_header;  (** Unit header *)
  entries : debug_macro_entry list;  (** List of macro entries *)
}

type debug_macro_section = {
  units : debug_macro_unit list;  (** List of macro units *)
}

(* Debug Macro Parsing Functions *)

let parse_debug_macro_header (cur : Object.Buffer.cursor) : debug_macro_header =
  let version = Object.Buffer.Read.u16 cur in
  let flags = Object.Buffer.Read.u8 cur in
  let flags_int = Unsigned.UInt8.to_int flags in
  let format = if flags_int land 0x01 <> 0 then DWARF64 else DWARF32 in
  let debug_line_offset =
    if flags_int land 0x02 <> 0 then Some (read_offset_for_format format cur)
    else None
  in
  let debug_str_offsets_offset =
    if flags_int land 0x04 <> 0 then Some (read_offset_for_format format cur)
    else None
  in
  { format; version; flags; debug_line_offset; debug_str_offsets_offset }

let parse_debug_macro_entry (cur : Object.Buffer.cursor) (format : dwarf_format)
    : debug_macro_entry option =
  let entry_type_code = Object.Buffer.Read.u8 cur in
  if Unsigned.UInt8.to_int entry_type_code = 0 then None
    (* End of entries marker *)
  else
    let entry_type = macro_info_entry_type_of_u8 entry_type_code in
    let line_number, string_offset, string_value, file_index =
      match entry_type with
      | DW_MACRO_define | DW_MACRO_undef ->
          let line = Object.Buffer.Read.uleb128 cur |> Unsigned.UInt32.of_int in
          let str =
            Object.Buffer.Read.zero_string cur () |> Option.value ~default:""
          in
          (Some line, None, Some str, None)
      | DW_MACRO_define_strp | DW_MACRO_undef_strp ->
          let line = Object.Buffer.Read.uleb128 cur |> Unsigned.UInt32.of_int in
          let str_offset = read_offset_for_format format cur in
          (Some line, Some str_offset, None, None)
      | DW_MACRO_define_sup | DW_MACRO_undef_sup ->
          let line = Object.Buffer.Read.uleb128 cur |> Unsigned.UInt32.of_int in
          let str_offset = read_offset_for_format format cur in
          (Some line, Some str_offset, None, None)
      | DW_MACRO_define_strx | DW_MACRO_undef_strx ->
          let line = Object.Buffer.Read.uleb128 cur |> Unsigned.UInt32.of_int in
          let str_offset =
            Object.Buffer.Read.uleb128 cur |> Unsigned.UInt64.of_int
          in
          (Some line, Some str_offset, None, None)
      | DW_MACRO_start_file ->
          let line = Object.Buffer.Read.uleb128 cur |> Unsigned.UInt32.of_int in
          let file_idx =
            Object.Buffer.Read.uleb128 cur |> Unsigned.UInt32.of_int
          in
          (Some line, None, None, Some file_idx)
      | DW_MACRO_end_file -> (None, None, None, None)
      | DW_MACRO_import | DW_MACRO_import_sup ->
          let offset = read_offset_for_format format cur in
          (None, Some offset, None, None)
      | _ ->
          (* For unknown or user-defined entry types, skip *)
          (None, None, None, None)
    in
    Some { entry_type; line_number; string_offset; string_value; file_index }

let parse_debug_macro_unit (cur : Object.Buffer.cursor) : debug_macro_unit =
  let header = parse_debug_macro_header cur in
  let entries = ref [] in

  let rec parse_entries () =
    match parse_debug_macro_entry cur header.format with
    | None -> List.rev !entries
    | Some entry ->
        entries := entry :: !entries;
        parse_entries ()
  in

  let entries_list = parse_entries () in
  { header; entries = entries_list }

let parse_debug_macro_section (cur : Object.Buffer.cursor) (section_size : int)
    : debug_macro_section =
  let start_pos = cur.position in
  let end_pos = start_pos + section_size in
  let units = ref [] in
  let rec parse_units () =
    if cur.position >= end_pos then List.rev !units
    else
      let unit = parse_debug_macro_unit cur in
      units := unit :: !units;
      parse_units ()
  in
  let units_list = parse_units () in
  { units = units_list }

(* Debug Macinfo Section - DWARF 4 Section 6.3 *)

type macinfo_type =
  | DW_MACINFO_define
  | DW_MACINFO_undef
  | DW_MACINFO_start_file
  | DW_MACINFO_end_file
  | DW_MACINFO_vendor_ext

let macinfo_type_of_int = function
  | 0x01 -> DW_MACINFO_define
  | 0x02 -> DW_MACINFO_undef
  | 0x03 -> DW_MACINFO_start_file
  | 0x04 -> DW_MACINFO_end_file
  | 0xff -> DW_MACINFO_vendor_ext
  | n -> failwith (Printf.sprintf "Unknown macinfo_type: 0x%02x" n)

let string_of_macinfo_type = function
  | DW_MACINFO_define -> "DW_MACINFO_define"
  | DW_MACINFO_undef -> "DW_MACINFO_undef"
  | DW_MACINFO_start_file -> "DW_MACINFO_start_file"
  | DW_MACINFO_end_file -> "DW_MACINFO_end_file"
  | DW_MACINFO_vendor_ext -> "DW_MACINFO_vendor_ext"

type debug_macinfo_entry = {
  macinfo_type : macinfo_type;
  line_number : u32 option;
  string_value : string option;
  file_index : u32 option;
  constant : u64 option;
}

type debug_macinfo_section = { entries : debug_macinfo_entry list }

let parse_debug_macinfo_entry (cur : Object.Buffer.cursor) :
    debug_macinfo_entry option =
  let type_code = Object.Buffer.Read.u8 cur |> Unsigned.UInt8.to_int in
  if type_code = 0 then None
  else
    let entry_type = macinfo_type_of_int type_code in
    let line_number, string_value, file_index, constant =
      match entry_type with
      | DW_MACINFO_define | DW_MACINFO_undef ->
          let line = Object.Buffer.Read.uleb128 cur |> Unsigned.UInt32.of_int in
          let str = Object.Buffer.Read.zero_string cur () in
          let str_val = match str with Some s -> s | None -> "" in
          (Some line, Some str_val, None, None)
      | DW_MACINFO_start_file ->
          let line = Object.Buffer.Read.uleb128 cur |> Unsigned.UInt32.of_int in
          let file_idx =
            Object.Buffer.Read.uleb128 cur |> Unsigned.UInt32.of_int
          in
          (Some line, None, Some file_idx, None)
      | DW_MACINFO_end_file -> (None, None, None, None)
      | DW_MACINFO_vendor_ext ->
          let const =
            Object.Buffer.Read.uleb128 cur |> Unsigned.UInt64.of_int
          in
          let str = Object.Buffer.Read.zero_string cur () in
          let str_val = match str with Some s -> s | None -> "" in
          (None, Some str_val, None, Some const)
    in
    Some
      {
        macinfo_type = entry_type;
        line_number;
        string_value;
        file_index;
        constant;
      }

let parse_debug_macinfo_section (cur : Object.Buffer.cursor)
    (section_size : int) : debug_macinfo_section =
  let end_pos = cur.position + section_size in
  let entries = ref [] in
  let rec parse_entries () =
    if cur.position >= end_pos then List.rev !entries
    else
      match parse_debug_macinfo_entry cur with
      | None -> List.rev !entries
      | Some entry ->
          entries := entry :: !entries;
          parse_entries ()
  in
  let entries_list = parse_entries () in
  { entries = entries_list }

type call_frame_instruction =
  | DW_CFA_advance_loc
  | DW_CFA_offset
  | DW_CFA_restore
  | DW_CFA_nop
  | DW_CFA_set_loc
  | DW_CFA_advance_loc1
  | DW_CFA_advance_loc2
  | DW_CFA_advance_loc4
  | DW_CFA_offset_extended
  | DW_CFA_restore_extended
  | DW_CFA_undefined
  | DW_CFA_same_value
  | DW_CFA_register
  | DW_CFA_remember_state
  | DW_CFA_restore_state
  | DW_CFA_def_cfa
  | DW_CFA_def_cfa_register
  | DW_CFA_def_cfa_offset
  | DW_CFA_def_cfa_expression
  | DW_CFA_expression
  | DW_CFA_offset_extended_sf
  | DW_CFA_def_cfa_sf
  | DW_CFA_def_cfa_offset_sf
  | DW_CFA_val_offset
  | DW_CFA_val_offset_sf
  | DW_CFA_val_expression
  | DW_CFA_lo_user
  | DW_CFA_hi_user

(* CFI instruction decoder *)
let decode_cfa_opcode = function
  | 0x00 -> DW_CFA_nop
  | 0x01 -> DW_CFA_set_loc
  | 0x02 -> DW_CFA_advance_loc1
  | 0x03 -> DW_CFA_advance_loc2
  | 0x04 -> DW_CFA_advance_loc4
  | 0x05 -> DW_CFA_offset_extended
  | 0x06 -> DW_CFA_restore_extended
  | 0x07 -> DW_CFA_undefined
  | 0x08 -> DW_CFA_same_value
  | 0x09 -> DW_CFA_register
  | 0x0a -> DW_CFA_remember_state
  | 0x0b -> DW_CFA_restore_state
  | 0x0c -> DW_CFA_def_cfa
  | 0x0d -> DW_CFA_def_cfa_register
  | 0x0e -> DW_CFA_def_cfa_offset
  | 0x0f -> DW_CFA_def_cfa_expression
  | 0x10 -> DW_CFA_expression
  | 0x11 -> DW_CFA_offset_extended_sf
  | 0x12 -> DW_CFA_def_cfa_sf
  | 0x13 -> DW_CFA_def_cfa_offset_sf
  | 0x14 -> DW_CFA_val_offset
  | 0x15 -> DW_CFA_val_offset_sf
  | 0x16 -> DW_CFA_val_expression
  | 0x1c -> DW_CFA_lo_user
  | 0x3f -> DW_CFA_hi_user
  | n when n >= 0x40 && n <= 0x7f -> DW_CFA_advance_loc (* + delta *)
  | n when n >= 0x80 && n <= 0xbf -> DW_CFA_offset (* + register *)
  | n when n >= 0xc0 && n <= 0xff -> DW_CFA_restore (* + register *)
  | _ -> DW_CFA_nop (* Unknown instruction, treat as nop *)

(* Helper functions for parsing ULEB128/SLEB128 values *)
(* Read ULEB128 from string at position *)
let read_uleb128_from_string str pos =
  let rec read_uleb acc shift pos =
    if pos >= String.length str then (acc, pos)
    else
      let byte = Char.code str.[pos] in
      let value = byte land 0x7f in
      let acc' = acc lor (value lsl shift) in
      if byte land 0x80 = 0 then (acc', pos + 1)
      else read_uleb acc' (shift + 7) (pos + 1)
  in
  read_uleb 0 0 pos

(* Read signed LEB128 from string at position *)
let read_sleb128_from_string str pos =
  let rec read_sleb acc shift pos =
    if pos >= String.length str then (acc, pos)
    else
      let byte = Char.code str.[pos] in
      let value = byte land 0x7f in
      let acc' = acc lor (value lsl shift) in
      let pos' = pos + 1 in
      if byte land 0x80 = 0 then
        (* Sign extend if necessary *)
        let result =
          if shift < 32 && value land 0x40 <> 0 then
            acc' lor (-1 lsl (shift + 7))
          else acc'
        in
        (result, pos')
      else read_sleb acc' (shift + 7) pos'
  in
  read_sleb 0 0 pos

(* DWARF Expression Parser *)
type dwarf_expression_operation = {
  opcode : operation_encoding;
  operands : int list;
  operand_string : string option;
}

let parse_dwarf_expression ?(encoding : encoding option) (expr_bytes : string) :
    dwarf_expression_operation list =
  let rec parse_ops pos acc =
    if pos >= String.length expr_bytes then List.rev acc
    else
      let opcode_byte = Char.code expr_bytes.[pos] in
      try
        let opcode = operation_encoding opcode_byte in
        let operands, operand_string, next_pos =
          match opcode with
          (* No operands *)
          | DW_OP_deref | DW_OP_dup | DW_OP_drop | DW_OP_over | DW_OP_swap
          | DW_OP_rot | DW_OP_xderef | DW_OP_abs | DW_OP_and | DW_OP_div
          | DW_OP_minus | DW_OP_mod | DW_OP_mul | DW_OP_neg | DW_OP_not
          | DW_OP_or | DW_OP_plus | DW_OP_shl | DW_OP_shr | DW_OP_shra
          | DW_OP_xor | DW_OP_eq | DW_OP_ge | DW_OP_gt | DW_OP_le | DW_OP_lt
          | DW_OP_ne | DW_OP_nop | DW_OP_push_object_address
          | DW_OP_form_tls_address | DW_OP_call_frame_cfa | DW_OP_stack_value
          | DW_OP_hi_user ->
              ([], None, pos + 1)
          (* Literal values 0-31 *)
          | DW_OP_lit0 | DW_OP_lit1 | DW_OP_lit2 | DW_OP_lit3 | DW_OP_lit4
          | DW_OP_lit5 | DW_OP_lit6 | DW_OP_lit7 | DW_OP_lit8 | DW_OP_lit9
          | DW_OP_lit10 | DW_OP_lit11 | DW_OP_lit12 | DW_OP_lit13 | DW_OP_lit14
          | DW_OP_lit15 | DW_OP_lit16 | DW_OP_lit17 | DW_OP_lit18 | DW_OP_lit19
          | DW_OP_lit20 | DW_OP_lit21 | DW_OP_lit22 | DW_OP_lit23 | DW_OP_lit24
          | DW_OP_lit25 | DW_OP_lit26 | DW_OP_lit27 | DW_OP_lit28 | DW_OP_lit29
          | DW_OP_lit30 | DW_OP_lit31 ->
              ([], None, pos + 1)
          (* Register values 0-31 *)
          | DW_OP_reg0 | DW_OP_reg1 | DW_OP_reg2 | DW_OP_reg3 | DW_OP_reg4
          | DW_OP_reg5 | DW_OP_reg6 | DW_OP_reg7 | DW_OP_reg8 | DW_OP_reg9
          | DW_OP_reg10 | DW_OP_reg11 | DW_OP_reg12 | DW_OP_reg13 | DW_OP_reg14
          | DW_OP_reg15 | DW_OP_reg16 | DW_OP_reg17 | DW_OP_reg18 | DW_OP_reg19
          | DW_OP_reg20 | DW_OP_reg21 | DW_OP_reg22 | DW_OP_reg23 | DW_OP_reg24
          | DW_OP_reg25 | DW_OP_reg26 | DW_OP_reg27 | DW_OP_reg28 | DW_OP_reg29
          | DW_OP_reg30 | DW_OP_reg31 ->
              ([], None, pos + 1)
          (* Base register + offset (breg0-breg31) all take SLEB128 offset *)
          | DW_OP_breg0 | DW_OP_breg1 | DW_OP_breg2 | DW_OP_breg3 | DW_OP_breg4
          | DW_OP_breg5 | DW_OP_breg6 | DW_OP_breg7 | DW_OP_breg8 | DW_OP_breg9
          | DW_OP_breg10 | DW_OP_breg11 | DW_OP_breg12 | DW_OP_breg13
          | DW_OP_breg14 | DW_OP_breg15 | DW_OP_breg16 | DW_OP_breg17
          | DW_OP_breg18 | DW_OP_breg19 | DW_OP_breg20 | DW_OP_breg21
          | DW_OP_breg22 | DW_OP_breg23 | DW_OP_breg24 | DW_OP_breg25
          | DW_OP_breg26 | DW_OP_breg27 | DW_OP_breg28 | DW_OP_breg29
          | DW_OP_breg30 | DW_OP_breg31 ->
              let offset, next_pos =
                read_sleb128_from_string expr_bytes (pos + 1)
              in
              ([ offset ], None, next_pos)
          (* 1-byte operands *)
          | DW_OP_const1u | DW_OP_const1s | DW_OP_pick | DW_OP_deref_size
          | DW_OP_xderef_size ->
              if pos + 1 < String.length expr_bytes then
                let operand = Char.code expr_bytes.[pos + 1] in
                ([ operand ], None, pos + 2)
              else ([], None, pos + 1)
          (* 2-byte operands *)
          | DW_OP_const2u | DW_OP_const2s | DW_OP_bra | DW_OP_skip | DW_OP_call2
            ->
              if pos + 2 < String.length expr_bytes then
                let b1 = Char.code expr_bytes.[pos + 1] in
                let b2 = Char.code expr_bytes.[pos + 2] in
                let operand = b1 lor (b2 lsl 8) in
                ([ operand ], None, pos + 3)
              else ([], None, pos + 1)
          (* 4-byte operands *)
          | DW_OP_const4u | DW_OP_const4s | DW_OP_call4 ->
              if pos + 4 < String.length expr_bytes then
                let b1 = Char.code expr_bytes.[pos + 1] in
                let b2 = Char.code expr_bytes.[pos + 2] in
                let b3 = Char.code expr_bytes.[pos + 3] in
                let b4 = Char.code expr_bytes.[pos + 4] in
                let operand =
                  b1 lor (b2 lsl 8) lor (b3 lsl 16) lor (b4 lsl 24)
                in
                ([ operand ], None, pos + 5)
              else ([], None, pos + 1)
          (* Address operand (architecture dependent size) *)
          | DW_OP_addr ->
              (* For now, assume 8-byte addresses *)
              if pos + 8 < String.length expr_bytes then
                let addr_bytes = String.sub expr_bytes (pos + 1) 8 in
                ( [],
                  Some (Printf.sprintf "0x%s" (String.escaped addr_bytes)),
                  pos + 9 )
              else ([], None, pos + 1)
          (* ULEB128 operands *)
          | DW_OP_constu | DW_OP_plus_uconst | DW_OP_regx | DW_OP_piece
          | DW_OP_addrx | DW_OP_constx ->
              let operand, next_pos =
                read_uleb128_from_string expr_bytes (pos + 1)
              in
              ([ operand ], None, next_pos)
          (* SLEB128 operands *)
          | DW_OP_consts | DW_OP_fbreg ->
              let operand, next_pos =
                read_sleb128_from_string expr_bytes (pos + 1)
              in
              ([ operand ], None, next_pos)
          (* ULEB128 register + SLEB128 offset *)
          | DW_OP_bregx ->
              let reg, pos1 = read_uleb128_from_string expr_bytes (pos + 1) in
              let offset, next_pos = read_sleb128_from_string expr_bytes pos1 in
              ([ reg; offset ], None, next_pos)
          (* ULEB128 size + ULEB128 offset *)
          | DW_OP_bit_piece ->
              let size, pos1 = read_uleb128_from_string expr_bytes (pos + 1) in
              let offset, next_pos = read_uleb128_from_string expr_bytes pos1 in
              ([ size; offset ], None, next_pos)
          (* ULEB128 size + block of that size *)
          | DW_OP_implicit_value | DW_OP_entry_value ->
              let size, pos1 = read_uleb128_from_string expr_bytes (pos + 1) in
              if pos1 + size <= String.length expr_bytes then
                ([ size ], Some (Printf.sprintf "[%d bytes]" size), pos1 + size)
              else ([], None, pos + 1)
          (* Parse 8-byte constant operands *)
          | DW_OP_const8u ->
              if pos + 8 < String.length expr_bytes then
                let b1 = Int64.of_int (Char.code expr_bytes.[pos + 1]) in
                let b2 = Int64.of_int (Char.code expr_bytes.[pos + 2]) in
                let b3 = Int64.of_int (Char.code expr_bytes.[pos + 3]) in
                let b4 = Int64.of_int (Char.code expr_bytes.[pos + 4]) in
                let b5 = Int64.of_int (Char.code expr_bytes.[pos + 5]) in
                let b6 = Int64.of_int (Char.code expr_bytes.[pos + 6]) in
                let b7 = Int64.of_int (Char.code expr_bytes.[pos + 7]) in
                let b8 = Int64.of_int (Char.code expr_bytes.[pos + 8]) in
                let operand =
                  Int64.(
                    logor b1
                      (logor (shift_left b2 8)
                         (logor (shift_left b3 16)
                            (logor (shift_left b4 24)
                               (logor (shift_left b5 32)
                                  (logor (shift_left b6 40)
                                     (logor (shift_left b7 48)
                                        (shift_left b8 56))))))))
                in
                let value_str = Int64.to_string operand in
                ([], Some value_str, pos + 9)
              else ([], Some "[truncated]", pos + 1)
          | DW_OP_const8s ->
              if pos + 8 < String.length expr_bytes then
                let b1 = Int64.of_int (Char.code expr_bytes.[pos + 1]) in
                let b2 = Int64.of_int (Char.code expr_bytes.[pos + 2]) in
                let b3 = Int64.of_int (Char.code expr_bytes.[pos + 3]) in
                let b4 = Int64.of_int (Char.code expr_bytes.[pos + 4]) in
                let b5 = Int64.of_int (Char.code expr_bytes.[pos + 5]) in
                let b6 = Int64.of_int (Char.code expr_bytes.[pos + 6]) in
                let b7 = Int64.of_int (Char.code expr_bytes.[pos + 7]) in
                let b8 = Int64.of_int (Char.code expr_bytes.[pos + 8]) in
                let operand =
                  Int64.(
                    logor b1
                      (logor (shift_left b2 8)
                         (logor (shift_left b3 16)
                            (logor (shift_left b4 24)
                               (logor (shift_left b5 32)
                                  (logor (shift_left b6 40)
                                     (logor (shift_left b7 48)
                                        (shift_left b8 56))))))))
                in
                let value_str = Int64.to_string operand in
                ([], Some value_str, pos + 9)
              else ([], Some "[truncated]", pos + 1)
          (* DW_OP_call_ref - calls a DIE reference (address-sized) *)
          | DW_OP_call_ref ->
              (* For now, assume 8-byte addresses - should be architecture dependent *)
              if pos + 8 < String.length expr_bytes then
                let b1 = Int64.of_int (Char.code expr_bytes.[pos + 1]) in
                let b2 = Int64.of_int (Char.code expr_bytes.[pos + 2]) in
                let b3 = Int64.of_int (Char.code expr_bytes.[pos + 3]) in
                let b4 = Int64.of_int (Char.code expr_bytes.[pos + 4]) in
                let b5 = Int64.of_int (Char.code expr_bytes.[pos + 5]) in
                let b6 = Int64.of_int (Char.code expr_bytes.[pos + 6]) in
                let b7 = Int64.of_int (Char.code expr_bytes.[pos + 7]) in
                let b8 = Int64.of_int (Char.code expr_bytes.[pos + 8]) in
                let die_ref =
                  Int64.(
                    logor b1
                      (logor (shift_left b2 8)
                         (logor (shift_left b3 16)
                            (logor (shift_left b4 24)
                               (logor (shift_left b5 32)
                                  (logor (shift_left b6 40)
                                     (logor (shift_left b7 48)
                                        (shift_left b8 56))))))))
                in
                let ref_str = Printf.sprintf "0x%Lx" die_ref in
                ([], Some ref_str, pos + 9)
              else ([], Some "[truncated]", pos + 1)
          (* DW_OP_implicit_pointer - DIE reference + signed offset *)
          | DW_OP_implicit_pointer ->
              let ref_size =
                match encoding with
                | Some enc when Unsigned.UInt16.to_int enc.version <= 4 -> 4
                | Some enc -> (
                    match enc.format with DWARF32 -> 4 | DWARF64 -> 8)
                | None -> 4
              in
              if pos + ref_size < String.length expr_bytes then
                let die_ref =
                  let rec read_le acc shift i =
                    if i >= ref_size then acc
                    else
                      let b =
                        Int64.of_int (Char.code expr_bytes.[pos + 1 + i])
                      in
                      read_le
                        (Int64.logor acc (Int64.shift_left b shift))
                        (shift + 8) (i + 1)
                  in
                  read_le 0L 0 0
                in
                let offset, next_pos =
                  read_sleb128_from_string expr_bytes (pos + 1 + ref_size)
                in
                let desc = Printf.sprintf "0x%Lx+%d" die_ref offset in
                ([], Some desc, next_pos)
              else ([], Some "[truncated]", pos + 1)
          (* DW_OP_const_type - ULEB128 type offset + variable length constant *)
          | DW_OP_const_type ->
              let type_offset, pos1 =
                read_uleb128_from_string expr_bytes (pos + 1)
              in
              let const_size, pos2 = read_uleb128_from_string expr_bytes pos1 in
              if pos2 + const_size <= String.length expr_bytes then
                let desc =
                  Printf.sprintf "type:0x%x const:[%d bytes]" type_offset
                    const_size
                in
                ([], Some desc, pos2 + const_size)
              else ([], Some "[truncated]", pos + 1)
          (* DW_OP_regval_type - ULEB128 register + ULEB128 type offset *)
          | DW_OP_regval_type ->
              let register, pos1 =
                read_uleb128_from_string expr_bytes (pos + 1)
              in
              let type_offset, next_pos =
                read_uleb128_from_string expr_bytes pos1
              in
              let desc =
                Printf.sprintf "reg:%d type:0x%x" register type_offset
              in
              ([], Some desc, next_pos)
          (* DW_OP_deref_type - 1-byte size + ULEB128 type offset *)
          | DW_OP_deref_type ->
              if pos + 1 < String.length expr_bytes then
                let size = Char.code expr_bytes.[pos + 1] in
                let type_offset, next_pos =
                  read_uleb128_from_string expr_bytes (pos + 2)
                in
                let desc =
                  Printf.sprintf "size:%d type:0x%x" size type_offset
                in
                ([], Some desc, next_pos)
              else ([], Some "[truncated]", pos + 1)
          (* DW_OP_xderef_type - 1-byte size + ULEB128 type offset *)
          | DW_OP_xderef_type ->
              if pos + 1 < String.length expr_bytes then
                let size = Char.code expr_bytes.[pos + 1] in
                let type_offset, next_pos =
                  read_uleb128_from_string expr_bytes (pos + 2)
                in
                let desc =
                  Printf.sprintf "size:%d type:0x%x" size type_offset
                in
                ([], Some desc, next_pos)
              else ([], Some "[truncated]", pos + 1)
          (* DW_OP_convert - ULEB128 type offset *)
          | DW_OP_convert ->
              let type_offset, next_pos =
                read_uleb128_from_string expr_bytes (pos + 1)
              in
              let desc = Printf.sprintf "type:0x%x" type_offset in
              ([], Some desc, next_pos)
          (* DW_OP_reinterpret - ULEB128 type offset *)
          | DW_OP_reinterpret ->
              let type_offset, next_pos =
                read_uleb128_from_string expr_bytes (pos + 1)
              in
              let desc = Printf.sprintf "type:0x%x" type_offset in
              ([], Some desc, next_pos)
          (* DW_OP_GNU_parameter_ref - 4-byte DIE offset *)
          | DW_OP_GNU_parameter_ref ->
              if pos + 4 < String.length expr_bytes then
                let b1 = Char.code expr_bytes.[pos + 1] in
                let b2 = Char.code expr_bytes.[pos + 2] in
                let b3 = Char.code expr_bytes.[pos + 3] in
                let b4 = Char.code expr_bytes.[pos + 4] in
                let offset =
                  b1 lor (b2 lsl 8) lor (b3 lsl 16) lor (b4 lsl 24)
                in
                ([ offset ], None, pos + 5)
              else ([], None, pos + 1)
          (* DW_OP_GNU_variable_value - address-sized ref *)
          | DW_OP_GNU_variable_value ->
              let addr_size =
                match encoding with
                | Some enc -> Unsigned.UInt8.to_int enc.address_size
                | None -> 8
              in
              if pos + addr_size < String.length expr_bytes then
                let die_ref =
                  let rec read_le acc shift i =
                    if i >= addr_size then acc
                    else
                      let b =
                        Int64.of_int (Char.code expr_bytes.[pos + 1 + i])
                      in
                      read_le
                        (Int64.logor acc (Int64.shift_left b shift))
                        (shift + 8) (i + 1)
                  in
                  read_le 0L 0 0
                in
                let desc = Printf.sprintf "0x%Lx" die_ref in
                ([], Some desc, pos + 1 + addr_size)
              else ([], Some "[truncated]", pos + 1)
        in
        let operation = { opcode; operands; operand_string } in
        parse_ops next_pos (operation :: acc)
      with Failure _ ->
        (* Unknown opcode, skip *)
        parse_ops (pos + 1) acc
  in
  parse_ops 0 []

let string_of_dwarf_operation (op : dwarf_expression_operation) : string =
  let opcode_name = string_of_operation_encoding op.opcode in
  match (op.operands, op.operand_string) with
  | [], None -> opcode_name
  | [], Some s -> Printf.sprintf "%s(%s)" opcode_name s
  | operands, None ->
      let operand_strs = List.map string_of_int operands in
      Printf.sprintf "%s(%s)" opcode_name (String.concat "," operand_strs)
  | operands, Some s ->
      let operand_strs = List.map string_of_int operands in
      Printf.sprintf "%s(%s,%s)" opcode_name (String.concat "," operand_strs) s

let string_of_dwarf_expression (ops : dwarf_expression_operation list) : string
    =
  String.concat " " (List.map string_of_dwarf_operation ops)

(** DWARF Expression Evaluator *)
module Expression = struct
  type register = Dwarf_arch.register = Register of int
  type value = Generic of int64 | Address of int64 | ImplicitValue of string

  type piece = {
    size_in_bits : int option;  (** Size in bits, None means rest of value *)
    bit_offset : int option;  (** Bit offset within the piece *)
    location : value option;  (** Location value, or None for empty piece *)
  }

  (** Evaluation result - used for callback pattern *)
  type evaluation_result =
    | Complete  (** Evaluation finished successfully *)
    | RequiresRegister of {
        register : register;
        offset : int64 option;
            (** Evaluator needs the caller to provide a register value. If
                offset is Some, caller should add it to the register value. *)
      }
    | RequiresMemory of {
        address : int64;
        size : int;
            (** Evaluator needs the caller to read memory at the given address
            *)
      }
    | RequiresFrameBase of { offset : int64 }
        (** Evaluator needs the caller to provide the frame base address. The
            offset will be added to the frame base. *)
    | RequiresCFA  (** Evaluator needs the Call Frame Address *)
    | RequiresTLSAddress of { address : int64 }
        (** Evaluator needs the TLS address for the given offset *)
    | RequiresObjectAddress  (** Evaluator needs the object address *)
    | RequiresIndexedAddress of { index : int; is_constant : bool }
        (** Evaluator needs an address from the address table. is_constant
            distinguishes DW_OP_constx from DW_OP_addrx *)
    | RequiresSubExpression of { offset : int64 }
        (** Evaluator needs the result of a sub-expression *)
    | RequiresEntryValue of { bytecode : string }
        (** Evaluator needs the entry value of the given expression *)

  type evaluation_state = {
    bytecode : string;  (** The expression bytecode *)
    mutable pc : int;  (** Program counter (position in bytecode) *)
    mutable stack : value list;  (** Evaluation stack *)
    mutable pieces : piece list;  (** Accumulated location pieces *)
    encoding : encoding;  (** DWARF encoding (address size, format, version) *)
    addr_mask : int64;  (** Mask for address size (computed from encoding) *)
    mutable waiting_for : evaluation_result option;
        (** What external data we're waiting for *)
  }

  (** Create address mask from address size *)
  let make_addr_mask address_size =
    let open Int64 in
    if address_size = 8 then minus_one
    else sub (shift_left one (8 * address_size)) one

  (** Start a new expression evaluation *)
  let start_evaluation ~(bytecode : string) ~(encoding : encoding) :
      evaluation_state =
    let address_size = Unsigned.UInt8.to_int encoding.address_size in
    {
      bytecode;
      pc = 0;
      stack = [];
      pieces = [];
      encoding;
      addr_mask = make_addr_mask address_size;
      waiting_for = None;
    }

  (** Push a value onto the stack *)
  let push state value = state.stack <- value :: state.stack

  (** Pop a value from the stack *)
  let pop state =
    match state.stack with
    | [] -> failwith "Stack underflow in DWARF expression evaluation"
    | v :: rest ->
        state.stack <- rest;
        v

  (** Convert value to int64 *)
  let to_int64 = function
    | Generic v | Address v -> v
    | ImplicitValue _ -> failwith "Cannot convert ImplicitValue to int64"

  (** Apply address mask to a value *)
  let mask_address state value = Int64.logand value state.addr_mask

  (** Sign-extend a value according to address size *)
  let sign_extend state value =
    let sign_bit = Int64.add (Int64.shift_right_logical state.addr_mask 1) 1L in
    if Int64.logand value sign_bit <> 0L then
      Int64.logor value (Int64.lognot state.addr_mask)
    else Int64.logand value state.addr_mask

  (** Read ULEB128 from bytecode at current PC *)
  let read_uleb128 state =
    let value, next_pos = read_uleb128_from_string state.bytecode state.pc in
    state.pc <- next_pos;
    value

  (** Read SLEB128 from bytecode at current PC *)
  let read_sleb128 state =
    let value, next_pos = read_sleb128_from_string state.bytecode state.pc in
    state.pc <- next_pos;
    Int64.of_int value

  (** Read a single byte from bytecode *)
  let read_u8 state =
    if state.pc < String.length state.bytecode then (
      let b = Char.code state.bytecode.[state.pc] in
      state.pc <- state.pc + 1;
      b)
    else failwith "Unexpected end of expression bytecode"

  (** Read 2 bytes (little-endian) from bytecode *)
  let read_u16 state =
    let b1 = read_u8 state in
    let b2 = read_u8 state in
    b1 lor (b2 lsl 8)

  (** Read 4 bytes (little-endian) from bytecode *)
  let read_u32 state =
    let b1 = read_u8 state in
    let b2 = read_u8 state in
    let b3 = read_u8 state in
    let b4 = read_u8 state in
    b1 lor (b2 lsl 8) lor (b3 lsl 16) lor (b4 lsl 24)

  (** Read [len] little-endian bytes from [s] at offset [ofs] as int64 *)
  let int64_of_le_bytes s ofs len =
    let rec go acc shift i =
      if i >= len then acc
      else
        let b = Int64.of_int (Char.code s.[ofs + i]) in
        go (Int64.logor acc (Int64.shift_left b shift)) (shift + 8) (i + 1)
    in
    go 0L 0 0

  (** Read 8 bytes (little-endian) from bytecode as int64 *)
  let read_u64 state =
    let ofs = state.pc in
    if ofs + 8 > String.length state.bytecode then
      failwith "Unexpected end of expression bytecode";
    state.pc <- ofs + 8;
    int64_of_le_bytes state.bytecode ofs 8

  (** Read signed 16-bit little-endian value from bytecode *)
  let read_s16 state =
    let v = read_u16 state in
    if v > 32767 then v - 65536 else v

  (** Evaluate one step of the expression *)
  let rec evaluate_step state =
    if state.pc >= String.length state.bytecode then Complete
    else
      let opcode_byte = read_u8 state in
      try
        (* DW_OP_lit0..DW_OP_lit31: 0x30..0x4f *)
        if opcode_byte >= 0x30 && opcode_byte <= 0x4f then (
          push state (Generic (Int64.of_int (opcode_byte - 0x30)));
          evaluate_step state (* DW_OP_reg0..DW_OP_reg31: 0x50..0x6f *))
        else if opcode_byte >= 0x50 && opcode_byte <= 0x6f then
          RequiresRegister
            { register = Register (opcode_byte - 0x50); offset = None }
          (* DW_OP_breg0..DW_OP_breg31: 0x70..0x8f *)
        else if opcode_byte >= 0x70 && opcode_byte <= 0x8f then
          let offset = read_sleb128 state in
          RequiresRegister
            { register = Register (opcode_byte - 0x70); offset = Some offset }
        else
          let opcode = operation_encoding opcode_byte in
          match opcode with
          (* Constants with operands *)
          | DW_OP_addr ->
              let addr_size =
                Unsigned.UInt8.to_int state.encoding.address_size
              in
              let ofs = state.pc in
              if ofs + addr_size > String.length state.bytecode then
                failwith "Unexpected end of expression bytecode";
              state.pc <- ofs + addr_size;
              let v = int64_of_le_bytes state.bytecode ofs addr_size in
              push state (Address v);
              evaluate_step state
          | DW_OP_const1u ->
              let v = read_u8 state in
              push state (Generic (Int64.of_int v));
              evaluate_step state
          | DW_OP_const1s ->
              let v = read_u8 state in
              let signed = if v > 127 then v - 256 else v in
              push state (Generic (Int64.of_int signed));
              evaluate_step state
          | DW_OP_const2u ->
              let v = read_u16 state in
              push state (Generic (Int64.of_int v));
              evaluate_step state
          | DW_OP_const2s ->
              let v = read_u16 state in
              let signed = if v > 32767 then v - 65536 else v in
              push state (Generic (Int64.of_int signed));
              evaluate_step state
          | DW_OP_const4u ->
              let v = read_u32 state in
              let unsigned_val = Int64.logand (Int64.of_int v) 0xFFFFFFFFL in
              push state (Generic unsigned_val);
              evaluate_step state
          | DW_OP_const4s ->
              let v = read_u32 state in
              push state (Generic (Int64.of_int32 (Int32.of_int v)));
              evaluate_step state
          | DW_OP_const8u ->
              let v = read_u64 state in
              push state (Generic v);
              evaluate_step state
          | DW_OP_const8s ->
              let v = read_u64 state in
              push state (Generic v);
              evaluate_step state
          | DW_OP_constu ->
              let v = read_uleb128 state in
              push state (Generic (Int64.of_int v));
              evaluate_step state
          | DW_OP_consts ->
              let v = read_sleb128 state in
              push state (Generic v);
              evaluate_step state
          (* Stack operations *)
          | DW_OP_dup -> (
              match state.stack with
              | v :: _ ->
                  push state v;
                  evaluate_step state
              | [] -> failwith "DW_OP_dup on empty stack")
          | DW_OP_drop ->
              let _ = pop state in
              evaluate_step state
          | DW_OP_pick ->
              let idx = read_u8 state in
              let rec nth l n =
                match l with
                | [] -> failwith "DW_OP_pick index out of range"
                | x :: _ when n = 0 -> x
                | _ :: rest -> nth rest (n - 1)
              in
              let v = nth state.stack idx in
              push state v;
              evaluate_step state
          | DW_OP_over -> (
              match state.stack with
              | _ :: v :: _ ->
                  push state v;
                  evaluate_step state
              | _ -> failwith "DW_OP_over requires at least 2 stack items")
          | DW_OP_swap -> (
              match state.stack with
              | a :: b :: rest ->
                  state.stack <- b :: a :: rest;
                  evaluate_step state
              | _ -> failwith "DW_OP_swap requires at least 2 stack items")
          | DW_OP_rot -> (
              match state.stack with
              | a :: b :: c :: rest ->
                  state.stack <- c :: a :: b :: rest;
                  evaluate_step state
              | _ -> failwith "DW_OP_rot requires at least 3 stack items")
          (* Arithmetic operations *)
          | DW_OP_abs ->
              let v = pop state |> to_int64 |> sign_extend state in
              push state (Generic (Int64.abs v));
              evaluate_step state
          | DW_OP_and ->
              let b = pop state |> to_int64 |> mask_address state in
              let a = pop state |> to_int64 |> mask_address state in
              push state (Generic (Int64.logand a b));
              evaluate_step state
          | DW_OP_div ->
              let b = pop state |> to_int64 |> sign_extend state in
              let a = pop state |> to_int64 |> sign_extend state in
              if b = 0L then failwith "Division by zero in DWARF expression"
              else (
                push state (Generic (Int64.div a b));
                evaluate_step state)
          | DW_OP_minus ->
              let b = pop state |> to_int64 in
              let a = pop state |> to_int64 in
              let result = mask_address state (Int64.sub a b) in
              push state (Generic result);
              evaluate_step state
          | DW_OP_mod ->
              let b = pop state |> to_int64 in
              let a = pop state |> to_int64 in
              if b = 0L then failwith "Modulo by zero in DWARF expression"
              else (
                push state (Generic (Int64.unsigned_rem a b));
                evaluate_step state)
          | DW_OP_mul ->
              let b = pop state |> to_int64 in
              let a = pop state |> to_int64 in
              let result = mask_address state (Int64.mul a b) in
              push state (Generic result);
              evaluate_step state
          | DW_OP_neg ->
              let v = pop state |> to_int64 |> sign_extend state in
              push state (Generic (Int64.neg v));
              evaluate_step state
          | DW_OP_not ->
              let v = pop state |> to_int64 in
              push state (Generic (Int64.lognot v));
              evaluate_step state
          | DW_OP_or ->
              let b = pop state |> to_int64 |> mask_address state in
              let a = pop state |> to_int64 |> mask_address state in
              push state (Generic (Int64.logor a b));
              evaluate_step state
          | DW_OP_plus ->
              let b = pop state |> to_int64 in
              let a = pop state |> to_int64 in
              let result = mask_address state (Int64.add a b) in
              push state (Generic result);
              evaluate_step state
          | DW_OP_plus_uconst ->
              let addend = read_uleb128 state in
              let v = pop state |> to_int64 in
              let result =
                mask_address state (Int64.add v (Int64.of_int addend))
              in
              push state (Generic result);
              evaluate_step state
          | DW_OP_shl ->
              let b = pop state |> to_int64 in
              let a = pop state |> to_int64 |> mask_address state in
              push state (Generic (Int64.shift_left a (Int64.to_int b)));
              evaluate_step state
          | DW_OP_shr ->
              let b = pop state |> to_int64 in
              let a = pop state |> to_int64 |> mask_address state in
              push state
                (Generic (Int64.shift_right_logical a (Int64.to_int b)));
              evaluate_step state
          | DW_OP_shra ->
              let b = pop state |> to_int64 in
              let a = pop state |> to_int64 |> sign_extend state in
              push state (Generic (Int64.shift_right a (Int64.to_int b)));
              evaluate_step state
          | DW_OP_xor ->
              let b = pop state |> to_int64 |> mask_address state in
              let a = pop state |> to_int64 |> mask_address state in
              push state (Generic (Int64.logxor a b));
              evaluate_step state
          (* Comparison operations *)
          | DW_OP_eq ->
              let b = pop state |> to_int64 |> sign_extend state in
              let a = pop state |> to_int64 |> sign_extend state in
              push state (Generic (if a = b then 1L else 0L));
              evaluate_step state
          | DW_OP_ge ->
              let b = pop state |> to_int64 |> sign_extend state in
              let a = pop state |> to_int64 |> sign_extend state in
              push state (Generic (if a >= b then 1L else 0L));
              evaluate_step state
          | DW_OP_gt ->
              let b = pop state |> to_int64 |> sign_extend state in
              let a = pop state |> to_int64 |> sign_extend state in
              push state (Generic (if a > b then 1L else 0L));
              evaluate_step state
          | DW_OP_le ->
              let b = pop state |> to_int64 |> sign_extend state in
              let a = pop state |> to_int64 |> sign_extend state in
              push state (Generic (if a <= b then 1L else 0L));
              evaluate_step state
          | DW_OP_lt ->
              let b = pop state |> to_int64 |> sign_extend state in
              let a = pop state |> to_int64 |> sign_extend state in
              push state (Generic (if a < b then 1L else 0L));
              evaluate_step state
          | DW_OP_ne ->
              let b = pop state |> to_int64 |> sign_extend state in
              let a = pop state |> to_int64 |> sign_extend state in
              push state (Generic (if a <> b then 1L else 0L));
              evaluate_step state
          (* Control flow *)
          | DW_OP_skip ->
              let offset = read_s16 state in
              let new_pc = state.pc + offset in
              if new_pc < 0 || new_pc > String.length state.bytecode then
                failwith "DW_OP_skip: target out of bounds";
              state.pc <- new_pc;
              evaluate_step state
          | DW_OP_bra ->
              let offset = read_s16 state in
              let v = pop state |> to_int64 in
              if v <> 0L then (
                let new_pc = state.pc + offset in
                if new_pc < 0 || new_pc > String.length state.bytecode then
                  failwith "DW_OP_bra: target out of bounds";
                state.pc <- new_pc);
              evaluate_step state
          (* Memory access *)
          | DW_OP_deref ->
              let addr = pop state |> to_int64 in
              let size = Unsigned.UInt8.to_int state.encoding.address_size in
              state.waiting_for <-
                Some (RequiresMemory { address = addr; size });
              RequiresMemory { address = addr; size }
          | DW_OP_deref_size ->
              let size = read_u8 state in
              let addr_size =
                Unsigned.UInt8.to_int state.encoding.address_size
              in
              if size > addr_size then
                failwith
                  (Printf.sprintf
                     "DW_OP_deref_size: size %d exceeds address size %d" size
                     addr_size);
              let addr = pop state |> to_int64 in
              state.waiting_for <-
                Some (RequiresMemory { address = addr; size });
              RequiresMemory { address = addr; size }
          (* Register operations *)
          | DW_OP_regx ->
              let reg_num = read_uleb128 state in
              RequiresRegister { register = Register reg_num; offset = None }
          | DW_OP_bregx ->
              let reg_num = read_uleb128 state in
              let offset = read_sleb128 state in
              RequiresRegister
                { register = Register reg_num; offset = Some offset }
          | DW_OP_fbreg ->
              let offset = read_sleb128 state in
              RequiresFrameBase { offset }
          | DW_OP_call_frame_cfa -> RequiresCFA
          (* Location description *)
          | DW_OP_piece ->
              let size = read_uleb128 state in
              let location =
                match state.stack with [] -> None | _ -> Some (pop state)
              in
              state.pieces <-
                state.pieces
                @ [
                    {
                      size_in_bits = Some (size * 8);
                      bit_offset = None;
                      location;
                    };
                  ];
              evaluate_step state
          | DW_OP_bit_piece ->
              let size_in_bits = read_uleb128 state in
              let bit_offset = read_uleb128 state in
              let location =
                match state.stack with [] -> None | _ -> Some (pop state)
              in
              state.pieces <-
                state.pieces
                @ [
                    {
                      size_in_bits = Some size_in_bits;
                      bit_offset = Some bit_offset;
                      location;
                    };
                  ];
              evaluate_step state
          | DW_OP_implicit_value ->
              let len = read_uleb128 state in
              if state.pc + len > String.length state.bytecode then
                failwith "DW_OP_implicit_value: block extends past bytecode";
              let bytes = String.sub state.bytecode state.pc len in
              state.pc <- state.pc + len;
              push state (ImplicitValue bytes);
              evaluate_step state
          | DW_OP_xderef ->
              let _addr_space = pop state |> to_int64 in
              let addr = pop state |> to_int64 in
              let size = Unsigned.UInt8.to_int state.encoding.address_size in
              state.waiting_for <-
                Some (RequiresMemory { address = addr; size });
              RequiresMemory { address = addr; size }
          | DW_OP_xderef_size ->
              let size = read_u8 state in
              let _addr_space = pop state |> to_int64 in
              let addr = pop state |> to_int64 in
              state.waiting_for <-
                Some (RequiresMemory { address = addr; size });
              RequiresMemory { address = addr; size }
          | DW_OP_nop -> evaluate_step state
          | DW_OP_stack_value -> (
              match pop state with
              | Generic v ->
                  push state (Address v);
                  evaluate_step state
              | Address _ as addr ->
                  push state addr;
                  evaluate_step state)
          | DW_OP_form_tls_address ->
              let addr = pop state |> to_int64 in
              state.waiting_for <- Some (RequiresTLSAddress { address = addr });
              RequiresTLSAddress { address = addr }
          | DW_OP_push_object_address ->
              state.waiting_for <- Some RequiresObjectAddress;
              RequiresObjectAddress
          | DW_OP_addrx ->
              let index = read_uleb128 state in
              state.waiting_for <-
                Some (RequiresIndexedAddress { index; is_constant = false });
              RequiresIndexedAddress { index; is_constant = false }
          | DW_OP_constx ->
              let index = read_uleb128 state in
              state.waiting_for <-
                Some (RequiresIndexedAddress { index; is_constant = true });
              RequiresIndexedAddress { index; is_constant = true }
          | DW_OP_call2 ->
              let offset = read_u16 state in
              let off64 = Int64.of_int offset in
              state.waiting_for <-
                Some (RequiresSubExpression { offset = off64 });
              RequiresSubExpression { offset = off64 }
          | DW_OP_call4 ->
              let offset = read_u32 state in
              let off64 = Int64.of_int offset in
              state.waiting_for <-
                Some (RequiresSubExpression { offset = off64 });
              RequiresSubExpression { offset = off64 }
          | DW_OP_call_ref ->
              let offset =
                match state.encoding.format with
                | DWARF32 -> Int64.of_int (read_u32 state)
                | DWARF64 -> read_u64 state
              in
              state.waiting_for <- Some (RequiresSubExpression { offset });
              RequiresSubExpression { offset }
          | DW_OP_entry_value ->
              let len = read_uleb128 state in
              if state.pc + len > String.length state.bytecode then
                failwith "DW_OP_entry_value: block extends past bytecode";
              let bytecode = String.sub state.bytecode state.pc len in
              state.pc <- state.pc + len;
              state.waiting_for <- Some (RequiresEntryValue { bytecode });
              RequiresEntryValue { bytecode }
          | DW_OP_implicit_pointer ->
              let ref_size =
                match state.encoding.format with DWARF32 -> 4 | DWARF64 -> 8
              in
              let ofs = state.pc in
              if ofs + ref_size > String.length state.bytecode then
                failwith "DW_OP_implicit_pointer: truncated";
              state.pc <- ofs + ref_size;
              let die_offset = int64_of_le_bytes state.bytecode ofs ref_size in
              let byte_offset = read_sleb128 state in
              state.waiting_for <-
                Some (RequiresSubExpression { offset = die_offset });
              ignore byte_offset;
              RequiresSubExpression { offset = die_offset }
          | DW_OP_GNU_parameter_ref ->
              let offset = Int64.of_int (read_u32 state) in
              state.waiting_for <- Some (RequiresSubExpression { offset });
              RequiresSubExpression { offset }
          | DW_OP_GNU_variable_value ->
              let addr_size =
                Unsigned.UInt8.to_int state.encoding.address_size
              in
              let ofs = state.pc in
              if ofs + addr_size > String.length state.bytecode then
                failwith "DW_OP_GNU_variable_value: truncated";
              state.pc <- ofs + addr_size;
              let offset = int64_of_le_bytes state.bytecode ofs addr_size in
              state.waiting_for <- Some (RequiresSubExpression { offset });
              RequiresSubExpression { offset }
          | _ ->
              failwith
                (Printf.sprintf "Unimplemented operation: %s"
                   (string_of_operation_encoding opcode))
      with Failure msg ->
        failwith (Printf.sprintf "Expression evaluation error: %s" msg)

  (** Start evaluation and run until we need external data or complete *)
  let evaluate state =
    match state.waiting_for with
    | Some result -> result
    | None ->
        let result = evaluate_step state in
        state.waiting_for <- Some result;
        result

  (** Resume evaluation after providing a register value *)
  let resume_with_register state value =
    match state.waiting_for with
    | Some (RequiresRegister { offset; _ }) ->
        let final_value =
          match offset with
          | None -> value
          | Some off -> Generic (Int64.add (to_int64 value) off)
        in
        push state final_value;
        state.waiting_for <- None;
        evaluate state
    | _ ->
        failwith
          "resume_with_register called but not waiting for register value"

  (** Resume evaluation after providing memory contents *)
  let resume_with_memory state bytes =
    match state.waiting_for with
    | Some (RequiresMemory _) ->
        let len = String.length bytes in
        (match len with
        | 1 | 2 | 4 | 8 -> ()
        | _ -> failwith "Unsupported memory read size");
        let value = int64_of_le_bytes bytes 0 len in
        push state (Generic value);
        state.waiting_for <- None;
        evaluate state
    | _ -> failwith "resume_with_memory called but not waiting for memory read"

  (** Resume evaluation after providing frame base *)
  let resume_with_frame_base state frame_base =
    match state.waiting_for with
    | Some (RequiresFrameBase { offset }) ->
        (* Add the offset to the frame base *)
        let result = Int64.add frame_base offset in
        push state (Generic result);
        state.waiting_for <- None;
        evaluate state
    | _ ->
        failwith "resume_with_frame_base called but not waiting for frame base"

  (** Resume evaluation after providing CFA *)
  let resume_with_cfa state cfa =
    match state.waiting_for with
    | Some RequiresCFA ->
        push state (Address cfa);
        state.waiting_for <- None;
        evaluate state
    | _ -> failwith "resume_with_cfa called but not waiting for CFA"

  (** Resume evaluation after providing a TLS address *)
  let resume_with_tls_address state addr =
    match state.waiting_for with
    | Some (RequiresTLSAddress _) ->
        push state (Address addr);
        state.waiting_for <- None;
        evaluate state
    | _ ->
        failwith
          "resume_with_tls_address called but not waiting for TLS address"

  (** Resume evaluation after providing the object address *)
  let resume_with_object_address state addr =
    match state.waiting_for with
    | Some RequiresObjectAddress ->
        push state (Address addr);
        state.waiting_for <- None;
        evaluate state
    | _ ->
        failwith
          "resume_with_object_address called but not waiting for object address"

  (** Resume evaluation after providing an indexed address *)
  let resume_with_indexed_address state value =
    match state.waiting_for with
    | Some (RequiresIndexedAddress { is_constant; _ }) ->
        let v = if is_constant then Generic value else Address value in
        push state v;
        state.waiting_for <- None;
        evaluate state
    | _ ->
        failwith
          "resume_with_indexed_address called but not waiting for indexed \
           address"

  (** Resume evaluation after providing sub-expression results *)
  let resume_with_sub_expression state values =
    match state.waiting_for with
    | Some (RequiresSubExpression _) ->
        List.iter (fun v -> push state v) (List.rev values);
        state.waiting_for <- None;
        evaluate state
    | _ ->
        failwith
          "resume_with_sub_expression called but not waiting for sub-expression"

  (** Resume evaluation after providing an entry value *)
  let resume_with_entry_value state value =
    match state.waiting_for with
    | Some (RequiresEntryValue _) ->
        push state value;
        state.waiting_for <- None;
        evaluate state
    | _ ->
        failwith
          "resume_with_entry_value called but not waiting for entry value"

  (** Get the final result stack *)
  let result state = state.stack

  (** Get location pieces for composite locations *)
  let pieces state = state.pieces
end

module Object_file = struct
  type t = { buffer : Buffer.t; format : Object_format.format }
end

type attr_spec = { attr : u64; form : u64; implicit_const : int64 option }

type abbrev = {
  code : u64;
  tag : u64;
  has_children : bool;
  attr_specs : attr_spec list;
}

(* Convert int to bool in C-style. *)
let bool_of_int = function 0 -> false | _ -> true

(* Object-format-aware section finder that works with both ELF and MachO *)
let find_debug_section_by_type buffer section_type =
  let object_format = Object_format.detect_format buffer in
  let section_name = object_format_to_section_name object_format section_type in
  try
    match object_format with
    | ELF -> (
        let open Object.Elf in
        let _header, section_array = read_elf buffer in
        let section_opt =
          Array.find_opt
            (fun section -> section.sh_name_str = section_name)
            section_array
        in
        match section_opt with
        | Some section -> Some (section.sh_offset, section.sh_size)
        | None -> None)
    | MACHO -> (
        let open Object.Macho in
        let _header, commands = read buffer in
        let dwarf_segment_opt =
          List.find_map
            (function
              | LC_SEGMENT_64 (lazy seg) when seg.seg_segname = "__DWARF" ->
                  Some seg
              | _ -> None)
            commands
        in
        match dwarf_segment_opt with
        | None -> None
        | Some dwarf_segment ->
            Array.find_map
              (fun section ->
                if section.sec_sectname = section_name then
                  Some
                    ( Unsigned.UInt64.of_uint32 section.sec_offset,
                      section.sec_size )
                else None)
              dwarf_segment.seg_sections)
  with _ -> None

open Object.Buffer

let parse_abbrev_table (elf : Object_file.t) (offset : u32) :
    (u64, abbrev) Hashtbl.t =
  let section_offset =
    match find_debug_section_by_type elf.buffer Debug_abbrev with
    | Some (section_offset, _) -> Unsigned.UInt64.to_int section_offset
    | None -> failwith "Could not find debug_abbrev section"
  in
  let cur =
    cursor elf.buffer ~at:(section_offset + Unsigned.UInt32.to_int offset)
  in
  let table = Hashtbl.create 100 in

  let rec parse_abbrevs () =
    let code = Read.uleb128 cur in
    if code = 0 then () (* End of abbreviations *)
    else
      let tag = Read.uleb128 cur in
      let has_children = Read.u8 cur |> Unsigned.UInt8.to_int |> bool_of_int in

      let rec parse_attr_specs acc =
        let attr = Read.uleb128 cur in
        let form = Read.uleb128 cur in
        if attr = 0 && form = 0 then List.rev acc (* End of attributes *)
        else
          let implicit_const =
            if form = 0x21 (* DW_FORM_implicit_const *) then
              Some (Int64.of_int (Read.sleb128 cur))
            else None
          in
          let attr_spec =
            {
              attr = Unsigned.UInt64.of_int attr;
              form = Unsigned.UInt64.of_int form;
              implicit_const;
            }
          in
          parse_attr_specs (attr_spec :: acc)
      in

      let attr_specs = parse_attr_specs [] in
      let abbrev =
        {
          code = Unsigned.UInt64.of_int code;
          tag = Unsigned.UInt64.of_int tag;
          has_children;
          attr_specs;
        }
      in
      Hashtbl.add table (Unsigned.UInt64.of_int code) abbrev;
      parse_abbrevs ()
  in

  parse_abbrevs ();
  table

(* String table helper functions *)

let read_string_from_section buffer offset section_offset : string option =
  try
    let actual_offset = section_offset + offset in
    let cursor = Object.Buffer.cursor buffer ~at:actual_offset in
    Object.Buffer.Read.zero_string cursor ()
  with _ -> None

let resolve_string_index (buffer : Object.Buffer.t) (format : dwarf_format)
    (index : int) : string =
  (* Try to resolve string index using debug_str_offs and debug_str sections *)
  match
    ( find_debug_section_by_type buffer Debug_str_offs,
      find_debug_section_by_type buffer Debug_str )
  with
  | Some (str_offs_offset, _), Some (str_offset, _) -> (
      try
        (* Read offset from string offsets table
         * Debug String Offsets Header Layout (DWARF 5 Section 7.26):
         *
         * For DWARF32:
         * +0     | 4    | unit_length  | Length of this unit (excluding this field)
         * +4     | 2    | version      | DWARF version number (5)
         * +6     | 2    | padding      | Reserved/padding bytes
         * +8     | ...  | offsets[]    | Array of 4-byte offsets into .debug_str
         *
         * For DWARF64:
         * +0     | 4    | 0xffffffff   | DWARF64 marker
         * +4     | 8    | unit_length  | Length of this unit (excluding marker+length)
         * +12    | 2    | version      | DWARF version number (5)
         * +14    | 2    | padding      | Reserved/padding bytes
         * +16    | ...  | offsets[]    | Array of 8-byte offsets into .debug_str
         *)
        let header_size, offset_size =
          match format with DWARF32 -> (8, 4) | DWARF64 -> (16, 8)
        in
        let str_offs_cursor =
          Object.Buffer.cursor buffer
            ~at:
              (Unsigned.UInt64.to_int str_offs_offset
              + header_size + (index * offset_size))
        in
        let string_offset =
          match format with
          | DWARF32 ->
              Object.Buffer.Read.u32 str_offs_cursor |> Unsigned.UInt32.to_int
          | DWARF64 ->
              Object.Buffer.Read.u64 str_offs_cursor |> Unsigned.UInt64.to_int
        in

        (* Read actual string from debug_str section *)
        match
          read_string_from_section buffer string_offset
            (Unsigned.UInt64.to_int str_offset)
        with
        | Some s -> s
        | None -> Printf.sprintf "<strx_error:%d>" index
      with _ -> Printf.sprintf "<strx_error:%d>" index)
  | Some (_, _), None -> Printf.sprintf "<strx_no_str:%d>" index
  | None, Some (_, _) -> Printf.sprintf "<strx_no_offs:%d>" index
  | None, None -> Printf.sprintf "<strx_no_sections:%d>" index

module DIE = struct
  type attribute_value =
    | String of string
    | IndexedString of int * string (* index, resolved_string *)
    | UData of u64
    | SData of i64
    | Address of u64
    | IndexedAddress of int * u64 (* index, resolved_address *)
    | Flag of bool
    | Reference of u64
    | Block of string
    | Language of dwarf_language
    | Encoding of base_type

  type attribute = { attr : attribute_encoding; value : attribute_value }

  type t = {
    tag : abbreviation_tag;
    attributes : attribute list;
    children : t Seq.t;
    offset : int;
  }

  let find_attribute die attr_name =
    List.find_map
      (fun attr -> if attr.attr = attr_name then Some attr.value else None)
      die.attributes

  let rec parse_attribute_value (cur : Object.Buffer.cursor)
      (form : attribute_form_encoding) (encoding : encoding)
      (full_buffer : Object.Buffer.t) ?(implicit_const : int64 option) () :
      attribute_value =
    match form with
    | DW_FORM_implicit_const -> (
        match implicit_const with
        | Some v -> SData (Signed.Int64.of_int64 v)
        | None -> SData (Signed.Int64.of_int64 0L))
    | DW_FORM_string ->
        let str = Object.Buffer.Read.zero_string cur () in
        String (match str with Some s -> s | None -> "")
    | DW_FORM_strp -> (
        (* String pointer to Debug_str section - size depends on format *)
        let offset = read_offset_for_format encoding.format cur in
        let offset_int = Unsigned.UInt64.to_int offset in
        match find_debug_section_by_type full_buffer Debug_str with
        | Some (str_section_offset, _) -> (
            match
              read_string_from_section full_buffer offset_int
                (Unsigned.UInt64.to_int str_section_offset)
            with
            | Some s -> String s
            | None -> String (Printf.sprintf "<strp_offset:%d>" offset_int))
        | None -> String (Printf.sprintf "<strp_offset:%d>" offset_int))
    | DW_FORM_udata ->
        let value = Object.Buffer.Read.uleb128 cur in
        UData (Unsigned.UInt64.of_int value)
    | DW_FORM_sdata ->
        let value = Object.Buffer.Read.sleb128 cur in
        SData (Signed.Int64.of_int value)
    | DW_FORM_addr ->
        let addr =
          let size = Unsigned.UInt8.to_int encoding.address_size in
          if size = 4 then
            Object.Buffer.Read.u32 cur |> Unsigned.UInt64.of_uint32
          else Object.Buffer.Read.u64 cur
        in
        Address addr
    | DW_FORM_flag ->
        let flag = Object.Buffer.Read.u8 cur |> Unsigned.UInt8.to_int in
        Flag (flag != 0)
    | DW_FORM_flag_present ->
        (* DW_FORM_flag_present is a zero-length form per DWARF 5 spec.
           The mere presence of this attribute indicates a true value.
           No data is read from the cursor. *)
        Flag true
    | DW_FORM_data1 ->
        let value = Object.Buffer.Read.u8 cur in
        UData (Unsigned.UInt64.of_int (Unsigned.UInt8.to_int value))
    | DW_FORM_data2 ->
        let value = Object.Buffer.Read.u16 cur in
        UData (Unsigned.UInt64.of_int (Unsigned.UInt16.to_int value))
    | DW_FORM_data4 ->
        let value = Object.Buffer.Read.u32 cur in
        UData (Unsigned.UInt64.of_uint32 value)
    | DW_FORM_data8 ->
        let value = Object.Buffer.Read.u64 cur in
        UData value
    | DW_FORM_strx ->
        (* String index form - reads ULEB128 index into string offsets table *)
        let index = Object.Buffer.Read.uleb128 cur in
        let resolved_string =
          resolve_string_index full_buffer encoding.format index
        in
        IndexedString (index, resolved_string)
    | DW_FORM_sec_offset ->
        (* Section offset - 4 or 8 bytes depending on DWARF format *)
        let offset = read_offset_for_format encoding.format cur in
        UData offset
    | DW_FORM_addrx ->
        (* Address index - ULEB128 index into address table *)
        let index = Object.Buffer.Read.uleb128 cur in
        IndexedAddress (index, Unsigned.UInt64.of_int index)
    | DW_FORM_ref4 ->
        (* 4-byte offset reference within same compilation unit *)
        let offset = Object.Buffer.Read.u32 cur in
        Reference (Unsigned.UInt64.of_uint32 offset)
    | DW_FORM_ref8 ->
        (* 8-byte offset reference within same compilation unit *)
        let offset = Object.Buffer.Read.u64 cur in
        Reference offset
    | DW_FORM_ref1 ->
        (* 1-byte offset reference within same compilation unit *)
        let offset = Object.Buffer.Read.u8 cur in
        Reference (Unsigned.UInt64.of_int (Unsigned.UInt8.to_int offset))
    | DW_FORM_ref2 ->
        (* 2-byte offset reference within same compilation unit *)
        let offset = Object.Buffer.Read.u16 cur in
        Reference (Unsigned.UInt64.of_int (Unsigned.UInt16.to_int offset))
    | DW_FORM_exprloc ->
        (* DWARF expression/location - ULEB128 length followed by expression *)
        let length = Object.Buffer.Read.uleb128 cur in
        let expr_data = Object.Buffer.Read.fixed_string cur length in
        Block expr_data
    | DW_FORM_block ->
        (* Variable length block - ULEB128 length followed by data *)
        let length = Object.Buffer.Read.uleb128 cur in
        let block_data = Object.Buffer.Read.fixed_string cur length in
        Block block_data
    | DW_FORM_block1 ->
        (* 1-byte length followed by data *)
        let length = Object.Buffer.Read.u8 cur |> Unsigned.UInt8.to_int in
        let block_data = Object.Buffer.Read.fixed_string cur length in
        Block block_data
    | DW_FORM_block2 ->
        (* 2-byte length followed by data *)
        let length = Object.Buffer.Read.u16 cur |> Unsigned.UInt16.to_int in
        let block_data = Object.Buffer.Read.fixed_string cur length in
        Block block_data
    | DW_FORM_block4 ->
        (* 4-byte length followed by data *)
        let length = Object.Buffer.Read.u32 cur |> Unsigned.UInt32.to_int in
        let block_data = Object.Buffer.Read.fixed_string cur length in
        Block block_data
    | DW_FORM_strx1 ->
        (* String index form - reads 1-byte index into string offsets table *)
        let index = Object.Buffer.Read.u8 cur |> Unsigned.UInt8.to_int in
        let resolved_string =
          resolve_string_index full_buffer encoding.format index
        in
        IndexedString (index, resolved_string)
    | DW_FORM_strx2 ->
        (* String index form - reads 2-byte index into string offsets table *)
        let index = Object.Buffer.Read.u16 cur |> Unsigned.UInt16.to_int in
        let resolved_string =
          resolve_string_index full_buffer encoding.format index
        in
        IndexedString (index, resolved_string)
    | DW_FORM_strx3 ->
        (* String index form - reads 3-byte index into string offsets table *)
        let byte1 = Object.Buffer.Read.u8 cur |> Unsigned.UInt8.to_int in
        let byte2 = Object.Buffer.Read.u8 cur |> Unsigned.UInt8.to_int in
        let byte3 = Object.Buffer.Read.u8 cur |> Unsigned.UInt8.to_int in
        let index = byte1 lor (byte2 lsl 8) lor (byte3 lsl 16) in
        let resolved_string =
          resolve_string_index full_buffer encoding.format index
        in
        IndexedString (index, resolved_string)
    | DW_FORM_strx4 ->
        (* String index form - reads 4-byte index into string offsets table *)
        let index = Object.Buffer.Read.u32 cur |> Unsigned.UInt32.to_int in
        let resolved_string =
          resolve_string_index full_buffer encoding.format index
        in
        IndexedString (index, resolved_string)
    | DW_FORM_addrx1 ->
        (* Address index form - reads 1-byte index into address table *)
        let index = Object.Buffer.Read.u8 cur |> Unsigned.UInt8.to_int in
        (* Return IndexedAddress - resolution requires addr_base from CU context *)
        IndexedAddress (index, Unsigned.UInt64.of_int index)
    | DW_FORM_addrx2 ->
        (* Address index form - reads 2-byte index into address table *)
        let index = Object.Buffer.Read.u16 cur |> Unsigned.UInt16.to_int in
        (* Return IndexedAddress - resolution requires addr_base from CU context *)
        IndexedAddress (index, Unsigned.UInt64.of_int index)
    | DW_FORM_addrx3 ->
        (* Address index form - reads 3-byte index into address table *)
        let byte1 = Object.Buffer.Read.u8 cur |> Unsigned.UInt8.to_int in
        let byte2 = Object.Buffer.Read.u8 cur |> Unsigned.UInt8.to_int in
        let byte3 = Object.Buffer.Read.u8 cur |> Unsigned.UInt8.to_int in
        let index = byte1 lor (byte2 lsl 8) lor (byte3 lsl 16) in
        (* Return IndexedAddress - resolution requires addr_base from CU context *)
        IndexedAddress (index, Unsigned.UInt64.of_int index)
    | DW_FORM_addrx4 ->
        (* Address index form - reads 4-byte index into address table *)
        let index = Object.Buffer.Read.u32 cur |> Unsigned.UInt32.to_int in
        (* Return IndexedAddress - resolution requires addr_base from CU context *)
        IndexedAddress (index, Unsigned.UInt64.of_int index)
    | DW_FORM_line_strp -> (
        (* String pointer to Debug_line_str section - size depends on format *)
        let offset = read_offset_for_format encoding.format cur in
        let offset_int = Unsigned.UInt64.to_int offset in
        match find_debug_section_by_type full_buffer Debug_line_str with
        | Some (str_section_offset, _) -> (
            match
              read_string_from_section full_buffer offset_int
                (Unsigned.UInt64.to_int str_section_offset)
            with
            | Some s -> String s
            | None -> String (Printf.sprintf "<line_strp_offset:%d>" offset_int)
            )
        | None -> String (Printf.sprintf "<line_strp_offset:%d>" offset_int))
    | DW_FORM_ref_addr ->
        let offset = read_offset_for_format encoding.format cur in
        Reference offset
    | DW_FORM_loclistx ->
        let index = Object.Buffer.Read.uleb128 cur in
        UData (Unsigned.UInt64.of_int index)
    | DW_FORM_rnglistx ->
        let index = Object.Buffer.Read.uleb128 cur in
        UData (Unsigned.UInt64.of_int index)
    | DW_FORM_ref_udata ->
        let value = Object.Buffer.Read.uleb128 cur in
        Reference (Unsigned.UInt64.of_int value)
    | DW_FORM_ref_sig8 ->
        let sig8 = Object.Buffer.Read.u64 cur in
        Reference sig8
    | DW_FORM_data16 ->
        let lo = Object.Buffer.Read.u64 cur in
        let _hi = Object.Buffer.Read.u64 cur in
        UData lo
    | DW_FORM_indirect ->
        let actual_form_code = Object.Buffer.Read.uleb128 cur in
        let actual_form =
          attribute_form_encoding (Unsigned.UInt64.of_int actual_form_code)
        in
        parse_attribute_value cur actual_form encoding full_buffer ()
    | _ -> String "<unsupported_form>"

  let process_language_attribute (raw_value : attribute_value) : attribute_value
      =
    match raw_value with
    | UData lang_code -> (
        let lang_int = Unsigned.UInt64.to_int lang_code in
        (* Language codes are parsed correctly by Object.Buffer based on file endianness *)
        try Language (dwarf_language lang_int) with _ -> raw_value)
    | _ -> raw_value

  let process_encoding_attribute (raw_value : attribute_value) : attribute_value
      =
    match raw_value with
    | UData encoding_code -> (
        let encoding_int = Unsigned.UInt64.to_int encoding_code in
        (* Encoding codes are parsed correctly by Object.Buffer based on file endianness *)
        try Encoding (base_type encoding_int) with _ -> raw_value)
    | _ -> raw_value

  let rec parse_children_seq (cur : Object.Buffer.cursor)
      (abbrev_table : (u64, abbrev) Hashtbl.t) (encoding : encoding)
      (full_buffer : Object.Buffer.t) : t Seq.t =
   fun () ->
    match parse_die cur abbrev_table encoding full_buffer with
    | None -> Seq.Nil (* End of children marker or error *)
    | Some die ->
        Seq.Cons (die, parse_children_seq cur abbrev_table encoding full_buffer)

  and parse_die (cur : Object.Buffer.cursor)
      (abbrev_table : (u64, abbrev) Hashtbl.t) (encoding : encoding)
      (full_buffer : Object.Buffer.t) : t option =
    try
      (* Capture the position at the start of this DIE *)
      let die_offset = cur.position in
      let abbrev_code = Object.Buffer.Read.uleb128 cur in
      if abbrev_code = 0 then None (* End of children marker *)
      else
        let abbrev_code_u64 = Unsigned.UInt64.of_int abbrev_code in
        match Hashtbl.find_opt abbrev_table abbrev_code_u64 with
        | None -> None (* Invalid abbreviation code *)
        | Some abbrev ->
            (* Convert tag from u64 to abbreviation_tag *)
            let tag = abbreviation_tag_of_int abbrev.tag in

            (* Parse attributes according to abbreviation specification *)
            let attributes =
              List.map
                (fun (spec : attr_spec) ->
                  let attr_encoding = attribute_encoding spec.attr in
                  let form_encoding = attribute_form_encoding spec.form in
                  let raw_value =
                    parse_attribute_value cur form_encoding encoding full_buffer
                      ?implicit_const:spec.implicit_const ()
                  in
                  let value =
                    (* Process special attributes that need conversion *)
                    if attr_encoding = DW_AT_language then
                      process_language_attribute raw_value
                    else if attr_encoding = DW_AT_encoding then
                      process_encoding_attribute raw_value
                    else raw_value
                  in
                  { attr = attr_encoding; value })
                abbrev.attr_specs
            in
            (* Parse children if the abbreviation indicates this DIE has children *)
            let children =
              if abbrev.has_children then
                parse_children_seq cur abbrev_table encoding full_buffer
              else Seq.empty
            in
            Some { tag; attributes; children; offset = die_offset }
    with _ -> None
end

(* Represents a section of the binary that corresponds to an
   element e.g. [Die.t] *)
type span = { start : size_t; size : size_t }

module CompileUnit = struct
  type header = {
    format : dwarf_format;
    unit_length : u64;
    version : u16;
    unit_type : u8;
    debug_abbrev_offset : u64;
    address_size : u8;
    header_span : span;
    addr_base : u64 option;
    type_signature : u64 option;
    type_offset : u64 option;
    dwo_id : u64 option;
  }

  type t = {
    parent_ : int;
    span : span;
    raw_buffer_ : Object_file.t;
    header : header;
  }

  let make parent_ span raw_buffer_ header =
    { parent_; span; raw_buffer_; header }

  let dwarf_info t = t.parent_
  let data t = t.span
  let header t = t.header

  (** Extract encoding parameters from the unit header. This provides the
      context needed for parsing DIE attributes. *)
  let encoding t : encoding =
    {
      format = t.header.format;
      address_size = t.header.address_size;
      version = t.header.version;
    }

  let root_die t abbrev_table full_buffer =
    (* Create cursor positioned after the compilation unit header *)
    let header_size = Unsigned.UInt64.to_int t.header.header_span.size in
    let cur =
      Object.Buffer.cursor t.raw_buffer_.buffer
        ~at:
          (Unsigned.UInt64.to_int
             (Unsigned.UInt64.add t.span.start
                (Unsigned.UInt64.of_int header_size)))
    in
    let enc = encoding t in
    DIE.parse_die cur abbrev_table enc full_buffer

  let die_cursor t abbrev_table full_buffer =
    let header_size = Unsigned.UInt64.to_int t.header.header_span.size in
    let pos =
      Unsigned.UInt64.to_int
        (Unsigned.UInt64.add t.span.start (Unsigned.UInt64.of_int header_size))
    in
    let enc = encoding t in
    ( Object.Buffer.cursor t.raw_buffer_.buffer ~at:pos,
      abbrev_table,
      enc,
      full_buffer )
end

let rec skip_attribute_value (cur : Object.Buffer.cursor) (form : u64)
    (encoding : encoding) (_full_buffer : Object.Buffer.t) : unit =
  let form_int = Unsigned.UInt64.to_int form in
  match form_int with
  (* DW_FORM_addr *)
  | 0x01 ->
      let sz = Unsigned.UInt8.to_int encoding.address_size in
      cur.position <- cur.position + sz
  (* DW_FORM_block2 *)
  | 0x03 ->
      let len = Object.Buffer.Read.u16 cur |> Unsigned.UInt16.to_int in
      cur.position <- cur.position + len
  (* DW_FORM_block4 *)
  | 0x04 ->
      let len = Object.Buffer.Read.u32 cur |> Unsigned.UInt32.to_int in
      cur.position <- cur.position + len
  (* DW_FORM_data2 *)
  | 0x05 -> cur.position <- cur.position + 2
  (* DW_FORM_data4 *)
  | 0x06 -> cur.position <- cur.position + 4
  (* DW_FORM_data8 *)
  | 0x07 -> cur.position <- cur.position + 8
  (* DW_FORM_string *)
  | 0x08 ->
      let _ = Object.Buffer.Read.zero_string cur () in
      ()
  (* DW_FORM_block *)
  | 0x09 ->
      let len = Object.Buffer.Read.uleb128 cur in
      cur.position <- cur.position + len
  (* DW_FORM_block1 *)
  | 0x0a ->
      let len = Object.Buffer.Read.u8 cur |> Unsigned.UInt8.to_int in
      cur.position <- cur.position + len
  (* DW_FORM_data1 *)
  | 0x0b -> cur.position <- cur.position + 1
  (* DW_FORM_flag *)
  | 0x0c -> cur.position <- cur.position + 1
  (* DW_FORM_sdata *)
  | 0x0d ->
      let _ = Object.Buffer.Read.sleb128 cur in
      ()
  (* DW_FORM_strp *)
  | 0x0e ->
      cur.position <- cur.position + offset_size_for_format encoding.format
  (* DW_FORM_udata *)
  | 0x0f ->
      let _ = Object.Buffer.Read.uleb128 cur in
      ()
  (* DW_FORM_ref_addr *)
  | 0x10 ->
      cur.position <- cur.position + offset_size_for_format encoding.format
  (* DW_FORM_ref1 *)
  | 0x11 -> cur.position <- cur.position + 1
  (* DW_FORM_ref2 *)
  | 0x12 -> cur.position <- cur.position + 2
  (* DW_FORM_ref4 *)
  | 0x13 -> cur.position <- cur.position + 4
  (* DW_FORM_ref8 *)
  | 0x14 -> cur.position <- cur.position + 8
  (* DW_FORM_ref_udata *)
  | 0x15 ->
      let _ = Object.Buffer.Read.uleb128 cur in
      ()
  (* DW_FORM_indirect *)
  | 0x16 ->
      let actual_form = Object.Buffer.Read.uleb128 cur in
      skip_attribute_value cur
        (Unsigned.UInt64.of_int actual_form)
        encoding _full_buffer
  (* DW_FORM_sec_offset *)
  | 0x17 ->
      cur.position <- cur.position + offset_size_for_format encoding.format
  (* DW_FORM_exprloc *)
  | 0x18 ->
      let len = Object.Buffer.Read.uleb128 cur in
      cur.position <- cur.position + len
  (* DW_FORM_flag_present *)
  | 0x19 -> ()
  (* DW_FORM_strx *)
  | 0x1a ->
      let _ = Object.Buffer.Read.uleb128 cur in
      ()
  (* DW_FORM_addrx *)
  | 0x1b ->
      let _ = Object.Buffer.Read.uleb128 cur in
      ()
  (* DW_FORM_ref_sup4 *)
  | 0x1c -> cur.position <- cur.position + 4
  (* DW_FORM_strp_sup *)
  | 0x1d ->
      cur.position <- cur.position + offset_size_for_format encoding.format
  (* DW_FORM_data16 *)
  | 0x1e -> cur.position <- cur.position + 16
  (* DW_FORM_line_strp *)
  | 0x1f ->
      cur.position <- cur.position + offset_size_for_format encoding.format
  (* DW_FORM_ref_sig8 *)
  | 0x20 -> cur.position <- cur.position + 8
  (* DW_FORM_implicit_const *)
  | 0x21 -> ()
  (* DW_FORM_loclistx *)
  | 0x22 ->
      let _ = Object.Buffer.Read.uleb128 cur in
      ()
  (* DW_FORM_rnglistx *)
  | 0x23 ->
      let _ = Object.Buffer.Read.uleb128 cur in
      ()
  (* DW_FORM_ref_sup8 *)
  | 0x24 -> cur.position <- cur.position + 8
  (* DW_FORM_strx1 *)
  | 0x25 -> cur.position <- cur.position + 1
  (* DW_FORM_strx2 *)
  | 0x26 -> cur.position <- cur.position + 2
  (* DW_FORM_strx3 *)
  | 0x27 -> cur.position <- cur.position + 3
  (* DW_FORM_strx4 *)
  | 0x28 -> cur.position <- cur.position + 4
  (* DW_FORM_addrx1 *)
  | 0x29 -> cur.position <- cur.position + 1
  (* DW_FORM_addrx2 *)
  | 0x2a -> cur.position <- cur.position + 2
  (* DW_FORM_addrx3 *)
  | 0x2b -> cur.position <- cur.position + 3
  (* DW_FORM_addrx4 *)
  | 0x2c -> cur.position <- cur.position + 4
  | n -> failwith (Printf.sprintf "skip_attribute_value: unknown form 0x%02x" n)

let rec skip_die (cur : Object.Buffer.cursor)
    (abbrev_table : (u64, abbrev) Hashtbl.t) (encoding : encoding)
    (buffer : Object.Buffer.t) : unit =
  let abbrev_code = Object.Buffer.Read.uleb128 cur in
  if abbrev_code <> 0 then (
    let abbrev =
      Hashtbl.find abbrev_table (Unsigned.UInt64.of_int abbrev_code)
    in
    List.iter
      (fun (spec : attr_spec) ->
        skip_attribute_value cur spec.form encoding buffer)
      abbrev.attr_specs;
    if abbrev.has_children then skip_children cur abbrev_table encoding buffer)

and skip_children (cur : Object.Buffer.cursor)
    (abbrev_table : (u64, abbrev) Hashtbl.t) (encoding : encoding)
    (buffer : Object.Buffer.t) : unit =
  let rec loop () =
    let code = Object.Buffer.Read.uleb128 cur in
    if code <> 0 then (
      let abbrev = Hashtbl.find abbrev_table (Unsigned.UInt64.of_int code) in
      List.iter
        (fun (spec : attr_spec) ->
          skip_attribute_value cur spec.form encoding buffer)
        abbrev.attr_specs;
      if abbrev.has_children then skip_children cur abbrev_table encoding buffer;
      loop ())
  in
  loop ()

module DieCursor = struct
  type t = {
    cursor : Object.Buffer.cursor;
    abbrev_table : (u64, abbrev) Hashtbl.t;
    encoding : encoding;
    buffer : Object.Buffer.t;
  }

  let create buffer abbrev_table encoding offset =
    let cursor = Object.Buffer.cursor buffer ~at:offset in
    { cursor; abbrev_table; encoding; buffer }

  let next t =
    try
      let die_offset = t.cursor.position in
      let abbrev_code = Object.Buffer.Read.uleb128 t.cursor in
      if abbrev_code = 0 then None
      else
        let abbrev_code_u64 = Unsigned.UInt64.of_int abbrev_code in
        match Hashtbl.find_opt t.abbrev_table abbrev_code_u64 with
        | None -> None
        | Some abbrev ->
            let tag = abbreviation_tag_of_int abbrev.tag in
            let attributes =
              List.map
                (fun (spec : attr_spec) ->
                  let attr_encoding = attribute_encoding spec.attr in
                  let form_encoding = attribute_form_encoding spec.form in
                  let raw_value =
                    DIE.parse_attribute_value t.cursor form_encoding t.encoding
                      t.buffer ?implicit_const:spec.implicit_const ()
                  in
                  DIE.{ attr = attr_encoding; value = raw_value })
                abbrev.attr_specs
            in
            Some
              ( DIE.
                  { tag; attributes; children = Seq.empty; offset = die_offset },
                abbrev.has_children )
    with _ -> None

  let skip_children t =
    skip_children t.cursor t.abbrev_table t.encoding t.buffer

  let position t = t.cursor.position
end

module DieZipper = struct
  type crumb = {
    parent_die : DIE.t;
    parent_has_children : bool;
    parent_children_pos : int;
    siblings_resume_pos : int;
  }

  type t = {
    die : DIE.t;
    has_children : bool;
    children_pos : int;
    crumbs : crumb list;
    abbrev_table : (u64, abbrev) Hashtbl.t;
    encoding : encoding;
    buffer : Object.Buffer.t;
  }

  let of_die_cursor (dc : DieCursor.t) =
    match DieCursor.next dc with
    | None -> None
    | Some (die, has_children) ->
        Some
          {
            die;
            has_children;
            children_pos = DieCursor.position dc;
            crumbs = [];
            abbrev_table = dc.abbrev_table;
            encoding = dc.encoding;
            buffer = dc.buffer;
          }

  let current t = t.die
  let tag t = t.die.tag

  let down t =
    if not t.has_children then None
    else
      let dc =
        DieCursor.create t.buffer t.abbrev_table t.encoding t.children_pos
      in
      match DieCursor.next dc with
      | None -> None
      | Some (child_die, child_has_children) ->
          let crumb =
            {
              parent_die = t.die;
              parent_has_children = t.has_children;
              parent_children_pos = t.children_pos;
              siblings_resume_pos = 0;
            }
          in
          Some
            {
              die = child_die;
              has_children = child_has_children;
              children_pos = DieCursor.position dc;
              crumbs = crumb :: t.crumbs;
              abbrev_table = t.abbrev_table;
              encoding = t.encoding;
              buffer = t.buffer;
            }

  let right t =
    let resume_pos =
      if t.has_children then (
        let cur = Object.Buffer.cursor t.buffer ~at:t.children_pos in
        skip_children cur t.abbrev_table t.encoding t.buffer;
        cur.position)
      else t.children_pos
    in
    let dc = DieCursor.create t.buffer t.abbrev_table t.encoding resume_pos in
    match DieCursor.next dc with
    | None -> None
    | Some (sib_die, sib_has_children) ->
        Some
          {
            die = sib_die;
            has_children = sib_has_children;
            children_pos = DieCursor.position dc;
            crumbs = t.crumbs;
            abbrev_table = t.abbrev_table;
            encoding = t.encoding;
            buffer = t.buffer;
          }

  let up t =
    match t.crumbs with
    | [] -> None
    | crumb :: rest ->
        Some
          {
            die = crumb.parent_die;
            has_children = crumb.parent_has_children;
            children_pos = crumb.parent_children_pos;
            crumbs = rest;
            abbrev_table = t.abbrev_table;
            encoding = t.encoding;
            buffer = t.buffer;
          }

  let children t =
    match down t with
    | None -> Seq.empty
    | Some first ->
        let rec go z () =
          Seq.Cons
            ( z,
              fun () ->
                match right z with None -> Seq.Nil | Some next -> go next () )
        in
        go first

  let siblings t =
    let rec go z () =
      match right z with
      | None -> Seq.Nil
      | Some next -> Seq.Cons (next, fun () -> go next ())
    in
    go t

  let fold_children f init t = Seq.fold_left f init (children t)
  let find_child pred t = Seq.find pred (children t)
  let depth t = List.length t.crumbs
end

(* TODO Record to keep the different parsed areas of an object file together.
   Perhaps this belongs in a consumer? *)
type t = {
  abbrev_tables_ : (size_t, (u64, abbrev) Hashtbl.t) Hashtbl.t;
  compile_units_ : CompileUnit.t Array.t;
  object_ : Object_file.t;
}

let parse_compile_unit_header (cur : Object.Buffer.cursor) :
    span * CompileUnit.header =
  let start = cur.position in
  let format, unit_length = parse_initial_length cur in
  let version = Object.Buffer.Read.u16 cur in
  let version_int = Unsigned.UInt16.to_int version in
  let unit_type, address_size, debug_abbrev_offset =
    if version_int = 5 then
      (* DWARF 5: unit_type, address_size, abbrev_offset *)
      let unit_type = Object.Buffer.Read.u8 cur in
      let address_size = Object.Buffer.Read.u8 cur in
      let debug_abbrev_offset = read_offset_for_format format cur in
      (unit_type, address_size, debug_abbrev_offset)
    else if version_int = 4 then
      (* DWARF 4: abbrev_offset, address_size (no unit_type) *)
      let debug_abbrev_offset = read_offset_for_format format cur in
      let address_size = Object.Buffer.Read.u8 cur in
      let unit_type = Unsigned.UInt8.of_int 0x01 in
      (unit_type, address_size, debug_abbrev_offset)
    else
      failwith
        (Printf.sprintf "Unsupported DWARF version: %d (only 4 and 5 supported)"
           version_int)
  in

  let parsed_unit_type = unit_type_of_u8 unit_type in

  (* Validate address size *)
  let addr_int = Unsigned.UInt8.to_int address_size in
  if addr_int <> 4 && addr_int <> 8 then
    failwith (Printf.sprintf "Invalid address size: %d" addr_int);

  (* Parse extra fields based on unit type (DWARF 5 only) *)
  let type_signature, type_offset, dwo_id =
    match parsed_unit_type with
    | DW_UT_type | DW_UT_split_type ->
        let sig8 = Object.Buffer.Read.u64 cur in
        let toff = read_offset_for_format format cur in
        (Some sig8, Some toff, None)
    | DW_UT_skeleton | DW_UT_split_compile ->
        let id = Object.Buffer.Read.u64 cur in
        (None, None, Some id)
    | _ -> (None, None, None)
  in

  let header_end = cur.position in
  let header_size = header_end - start in
  let header_span =
    {
      start = Unsigned.UInt64.of_int start;
      size = Unsigned.UInt64.of_int header_size;
    }
  in

  (* Total size includes the length field itself.
     For DWARF32 there is a 4-byte value, while for
     DWARF64 there is a 4-byte marker (0xffffffff) + 8-byte length value. *)
  let length_field_size = match format with DWARF32 -> 4 | DWARF64 -> 12 in
  let total_size = start + length_field_size in
  let span =
    {
      start = Unsigned.UInt64.of_int start;
      size = Unsigned.UInt64.of_int total_size;
    }
  in
  ( span,
    {
      format;
      unit_length;
      version;
      unit_type;
      debug_abbrev_offset;
      address_size;
      header_span;
      addr_base = None;
      type_signature;
      type_offset;
      dwo_id;
    } )

let parse_compile_unit (object_file : Object_file.t)
    (cur : Object.Buffer.cursor) : CompileUnit.t =
  (* Start by parsing just the header to get size *)
  let start = cur.position in

  (* Reset cursor to start for consistent parsing *)
  Object.Buffer.seek cur start;

  let data, parsed = parse_compile_unit_header cur in
  CompileUnit.make 0 data object_file parsed

let parse_compile_units (dwarf : t) : CompileUnit.t Seq.t =
  match find_debug_section_by_type dwarf.object_.buffer Debug_info with
  | None -> Seq.empty
  | Some (section_offset, section_size) ->
      let section_end = Unsigned.UInt64.to_int section_size in

      (* Create a lazy sequence generator *)
      let rec parse_units cursor_pos () =
        if cursor_pos >= section_end then Seq.Nil
        else
          try
            let absolute_pos =
              Unsigned.UInt64.to_int section_offset + cursor_pos
            in
            let cur =
              Object.Buffer.cursor dwarf.object_.buffer ~at:absolute_pos
            in
            let span, parsed_header = parse_compile_unit_header cur in
            let unit =
              CompileUnit.make cursor_pos span dwarf.object_ parsed_header
            in

            (* Calculate next position: current + unit_length + length_field_size *)
            let unit_length =
              Unsigned.UInt64.to_int parsed_header.unit_length
            in
            let length_field_size =
              match parsed_header.format with DWARF32 -> 4 | DWARF64 -> 12
            in
            let next_pos = cursor_pos + unit_length + length_field_size in

            Seq.Cons (unit, parse_units next_pos)
          with exn ->
            Printf.eprintf "Error parsing compile unit at offset %d: %s\n"
              cursor_pos (Printexc.to_string exn);
            Seq.Nil
      in
      parse_units 0

module LineTable = struct
  type t = { cu : CompileUnit.t }

  module File = struct
    type t = { path : string; modification_time : u64; file_length : u64 }
  end

  type file_entry = {
    name : string;
    timestamp : u64;
    size : u64;
    directory : string;
    md5_checksum : string option;
  }

  type line_program_header = {
    format : dwarf_format;
    unit_length : u64;
    version : u16;
    address_size : u8;
    segment_selector_size : u8;
    header_length : u64;
    minimum_instruction_length : u8;
    maximum_operations_per_instruction : u8;
    default_is_stmt : bool;
    line_base : int;
    line_range : u8;
    opcode_base : u8;
    standard_opcode_lengths : u8 array;
    directory_entry_format_count : u8;
    directory_entry_formats :
      (line_number_header_entry * attribute_form_encoding) array;
    directories_count : u32;
    directories : string array;
    file_name_entry_format_count : u8;
    file_name_entry_formats :
      (line_number_header_entry * attribute_form_encoding) array;
    file_names_count : u32;
    file_names : file_entry array;
  }

  type line_table_entry = {
    address : u64;
    line : u32;
    column : u32;
    file_index : u32;
    isa : u32;
    discriminator : u32;
    op_index : u32;
    is_stmt : bool;
    basic_block : bool;
    end_sequence : bool;
    prologue_end : bool;
    epilogue_begin : bool;
  }

  (** Resolve a DW_FORM_line_strp offset to its string value.

      In DWARF 5, strings in line tables can be stored indirectly in the
      .debug_line_str section to reduce duplication. This function takes an
      offset into that section and returns the null-terminated string found at
      that offset.

      @param buffer Complete buffer containing debug sections
      @param offset Offset into .debug_line_str section
      @return
        String at the given offset, or a placeholder if section/offset not found

      Reference: DWARF 5 specification, section 7.26 "String Encodings" *)
  let resolve_line_strp_offset buffer offset =
    match find_debug_section_by_type buffer Debug_line_str with
    | None ->
        Printf.sprintf "<line_strp:0x%Lx>" (Unsigned.UInt64.to_int64 offset)
    | Some (section_offset, _size) -> (
        try
          let string_cursor =
            Object.Buffer.cursor buffer
              ~at:
                (Unsigned.UInt64.to_int section_offset
                + Unsigned.UInt64.to_int offset)
          in
          match Object.Buffer.Read.zero_string string_cursor () with
          | Some s -> s
          | None ->
              Printf.sprintf "<line_strp:0x%Lx>"
                (Unsigned.UInt64.to_int64 offset)
        with _ ->
          Printf.sprintf "<line_strp:0x%Lx>" (Unsigned.UInt64.to_int64 offset))

  (** Parse a DWARF 5 line program header from a buffer cursor.

      This implementation handles the complex DWARF 5 format with flexible
      directory and file entry encodings based on format descriptors. It parses
      all header fields, builds the directory and file tables, and resolves any
      string references to the .debug_line_str section.

      The parsing process: 1. Fixed header fields (lengths, versions, basic
      parameters) 2. Standard opcode definitions array 3. Directory entries
      using directory_entry_formats descriptors 4. File entries using
      file_name_entry_formats descriptors 5. Directory index resolution in file
      entries 6. MD5 checksum conversion from binary to hex string

      Supports all standard DWARF 5 content types and forms including
      DW_FORM_line_strp indirect string references and DW_FORM_data16 MD5
      checksums. *)
  let parse_line_program_header (cur : Object.Buffer.cursor) buffer :
      line_program_header =
    let format, unit_length = parse_initial_length cur in
    let version = Object.Buffer.Read.u16 cur in
    let version_int = Unsigned.UInt16.to_int version in
    if version_int <> 4 && version_int <> 5 then
      failwith
        (Printf.sprintf
           "Unsupported line table version: %d (only 4 and 5 supported)"
           version_int);
    let address_size, segment_selector_size =
      if version_int = 5 then
        let a = Object.Buffer.Read.u8 cur in
        let s = Object.Buffer.Read.u8 cur in
        (a, s)
      else (Unsigned.UInt8.of_int 0, Unsigned.UInt8.of_int 0)
    in
    let header_length = read_offset_for_format format cur in
    let minimum_instruction_length = Object.Buffer.Read.u8 cur in
    let maximum_operations_per_instruction = Object.Buffer.Read.u8 cur in
    let default_is_stmt =
      Object.Buffer.Read.u8 cur |> Unsigned.UInt8.to_int |> ( <> ) 0
    in
    let line_base_u8 = Object.Buffer.Read.u8 cur in
    let line_base =
      if Unsigned.UInt8.to_int line_base_u8 > 127 then
        Unsigned.UInt8.to_int line_base_u8 - 256
      else Unsigned.UInt8.to_int line_base_u8
    in
    let line_range = Object.Buffer.Read.u8 cur in
    let opcode_base = Object.Buffer.Read.u8 cur in

    let standard_opcode_lengths =
      Array.make
        (Unsigned.UInt8.to_int opcode_base - 1)
        (Unsigned.UInt8.of_int 0)
    in
    for i = 0 to Array.length standard_opcode_lengths - 1 do
      standard_opcode_lengths.(i) <- Object.Buffer.Read.u8 cur
    done;

    if version_int = 5 then (
      let directory_entry_format_count = Object.Buffer.Read.u8 cur in
      let directory_entry_formats =
        Array.make
          (Unsigned.UInt8.to_int directory_entry_format_count)
          (DW_LNCT_path, DW_FORM_string)
      in
      for i = 0 to Array.length directory_entry_formats - 1 do
        let content_type_code = Object.Buffer.Read.uleb128 cur in
        let form_code = Object.Buffer.Read.uleb128 cur in
        let content_type = line_number_header_entry content_type_code in
        let form = attribute_form_encoding (Unsigned.UInt64.of_int form_code) in
        directory_entry_formats.(i) <- (content_type, form)
      done;

      let directories_count =
        Object.Buffer.Read.uleb128 cur |> Unsigned.UInt32.of_int
      in
      let directories =
        Array.make (Unsigned.UInt32.to_int directories_count) ""
      in
      for i = 0 to Array.length directories - 1 do
        let path_ref = ref "" in
        for j = 0 to Array.length directory_entry_formats - 1 do
          let content_type, form = directory_entry_formats.(j) in
          match (content_type, form) with
          | DW_LNCT_path, DW_FORM_string -> (
              path_ref :=
                match Object.Buffer.Read.zero_string cur () with
                | Some s -> s
                | None -> "")
          | DW_LNCT_path, DW_FORM_line_strp ->
              let offset = read_offset_for_format format cur in
              path_ref := resolve_line_strp_offset buffer offset
          | DW_LNCT_directory_index, _ ->
              failwith
                "DW_LNCT_directory_index is not valid for directory entries \
                 (DWARF 5 spec)"
          | DW_LNCT_timestamp, _ ->
              failwith
                "DW_LNCT_timestamp is not valid for directory entries (DWARF 5 \
                 spec)"
          | DW_LNCT_size, _ ->
              failwith
                "DW_LNCT_size is not valid for directory entries (DWARF 5 spec)"
          | DW_LNCT_MD5, _ ->
              failwith
                "DW_LNCT_MD5 is not valid for directory entries (DWARF 5 spec)"
          | DW_LNCT_lo_user, _ | DW_LNCT_hi_user, _ -> (
              match form with
              | DW_FORM_string -> ignore (Object.Buffer.Read.zero_string cur ())
              | DW_FORM_data1 -> ignore (Object.Buffer.Read.u8 cur)
              | DW_FORM_data2 -> ignore (Object.Buffer.Read.u16 cur)
              | DW_FORM_data4 -> ignore (Object.Buffer.Read.u32 cur)
              | DW_FORM_data8 -> ignore (Object.Buffer.Read.u64 cur)
              | DW_FORM_udata -> ignore (Object.Buffer.Read.uleb128 cur)
              | _ ->
                  failwith
                    "Unsupported form for vendor-defined directory entry \
                     content type")
          | DW_LNCT_path, _form ->
              failwith
                "Unsupported form for DW_LNCT_path in directory entry \
                 (expected DW_FORM_string or DW_FORM_line_strp)"
        done;
        directories.(i) <- !path_ref
      done;

      let file_name_entry_format_count = Object.Buffer.Read.u8 cur in
      let file_name_entry_formats =
        Array.make
          (Unsigned.UInt8.to_int file_name_entry_format_count)
          (DW_LNCT_path, DW_FORM_string)
      in
      for i = 0 to Array.length file_name_entry_formats - 1 do
        let content_type_code = Object.Buffer.Read.uleb128 cur in
        let form_code = Object.Buffer.Read.uleb128 cur in
        let content_type = line_number_header_entry content_type_code in
        let form = attribute_form_encoding (Unsigned.UInt64.of_int form_code) in
        file_name_entry_formats.(i) <- (content_type, form)
      done;

      let file_names_count =
        Object.Buffer.Read.uleb128 cur |> Unsigned.UInt32.of_int
      in
      let file_names =
        Array.make
          (Unsigned.UInt32.to_int file_names_count)
          {
            name = "";
            timestamp = Unsigned.UInt64.of_int 0;
            size = Unsigned.UInt64.of_int 0;
            directory = "";
            md5_checksum = None;
          }
      in
      for i = 0 to Array.length file_names - 1 do
        let path_ref = ref "" in
        let dir_index_ref = ref 0 in
        let timestamp_ref = ref (Unsigned.UInt64.of_int 0) in
        let file_size_ref = ref (Unsigned.UInt64.of_int 0) in
        let md5_ref = ref None in

        for j = 0 to Array.length file_name_entry_formats - 1 do
          let content_type, form = file_name_entry_formats.(j) in
          match (content_type, form) with
          | DW_LNCT_path, DW_FORM_string -> (
              path_ref :=
                match Object.Buffer.Read.zero_string cur () with
                | Some s -> s
                | None -> "")
          | DW_LNCT_path, DW_FORM_line_strp ->
              let offset = read_offset_for_format format cur in
              path_ref := resolve_line_strp_offset buffer offset
          | DW_LNCT_directory_index, DW_FORM_udata ->
              dir_index_ref := Object.Buffer.Read.uleb128 cur
          | DW_LNCT_timestamp, DW_FORM_udata ->
              timestamp_ref :=
                Object.Buffer.Read.uleb128 cur |> Unsigned.UInt64.of_int
          | DW_LNCT_size, DW_FORM_udata ->
              file_size_ref :=
                Object.Buffer.Read.uleb128 cur |> Unsigned.UInt64.of_int
          | DW_LNCT_MD5, DW_FORM_data16 ->
              let md5_bytes = Array.make 16 (Unsigned.UInt8.of_int 0) in
              for k = 0 to 15 do
                md5_bytes.(k) <- Object.Buffer.Read.u8 cur
              done;
              let md5_hex =
                String.concat ""
                  (Array.to_list
                     (Array.map
                        (fun b ->
                          Printf.sprintf "%02x" (Unsigned.UInt8.to_int b))
                        md5_bytes))
              in
              md5_ref := Some md5_hex
          | DW_LNCT_directory_index, DW_FORM_data1 ->
              dir_index_ref :=
                Object.Buffer.Read.u8 cur |> Unsigned.UInt8.to_int
          | DW_LNCT_directory_index, DW_FORM_data2 ->
              dir_index_ref :=
                Object.Buffer.Read.u16 cur |> Unsigned.UInt16.to_int
          | DW_LNCT_timestamp, DW_FORM_data4 ->
              timestamp_ref :=
                Object.Buffer.Read.u32 cur |> Unsigned.UInt32.to_int
                |> Unsigned.UInt64.of_int
          | DW_LNCT_timestamp, DW_FORM_data8 ->
              timestamp_ref := Object.Buffer.Read.u64 cur
          | DW_LNCT_size, DW_FORM_data1 ->
              file_size_ref :=
                Object.Buffer.Read.u8 cur |> Unsigned.UInt8.to_int
                |> Unsigned.UInt64.of_int
          | DW_LNCT_size, DW_FORM_data2 ->
              file_size_ref :=
                Object.Buffer.Read.u16 cur |> Unsigned.UInt16.to_int
                |> Unsigned.UInt64.of_int
          | DW_LNCT_size, DW_FORM_data4 ->
              file_size_ref :=
                Object.Buffer.Read.u32 cur |> Unsigned.UInt32.to_int
                |> Unsigned.UInt64.of_int
          | DW_LNCT_size, DW_FORM_data8 ->
              file_size_ref := Object.Buffer.Read.u64 cur
          | DW_LNCT_lo_user, _ | DW_LNCT_hi_user, _ -> (
              match form with
              | DW_FORM_string -> ignore (Object.Buffer.Read.zero_string cur ())
              | DW_FORM_data1 -> ignore (Object.Buffer.Read.u8 cur)
              | DW_FORM_data2 -> ignore (Object.Buffer.Read.u16 cur)
              | DW_FORM_data4 -> ignore (Object.Buffer.Read.u32 cur)
              | DW_FORM_data8 -> ignore (Object.Buffer.Read.u64 cur)
              | DW_FORM_data16 ->
                  for _k = 0 to 15 do
                    ignore (Object.Buffer.Read.u8 cur)
                  done
              | DW_FORM_udata -> ignore (Object.Buffer.Read.uleb128 cur)
              | _ ->
                  failwith
                    "Unsupported form for vendor-defined file entry content \
                     type")
          | _content_type, _form ->
              failwith
                "Invalid content_type/form combination for file entry (DWARF 5 \
                 spec)"
        done;

        let dir_name =
          if !dir_index_ref < Array.length directories then
            directories.(!dir_index_ref)
          else ""
        in
        file_names.(i) <-
          {
            name = !path_ref;
            timestamp = !timestamp_ref;
            size = !file_size_ref;
            directory = dir_name;
            md5_checksum = !md5_ref;
          }
      done;

      {
        format;
        unit_length;
        version;
        address_size;
        segment_selector_size;
        header_length;
        minimum_instruction_length;
        maximum_operations_per_instruction;
        default_is_stmt;
        line_base;
        line_range;
        opcode_base;
        standard_opcode_lengths;
        directory_entry_format_count;
        directory_entry_formats;
        directories_count;
        directories;
        file_name_entry_format_count;
        file_name_entry_formats;
        file_names_count;
        file_names;
      })
    else
      (* DWARF 4: null-terminated directory and file lists *)
      let dirs = ref [] in
      let rec read_dirs () =
        match Object.Buffer.Read.zero_string cur () with
        | Some s when String.length s > 0 ->
            dirs := s :: !dirs;
            read_dirs ()
        | _ -> ()
      in
      read_dirs ();
      let directories = Array.of_list (List.rev !dirs) in

      let files = ref [] in
      let rec read_files () =
        match Object.Buffer.Read.zero_string cur () with
        | Some name when String.length name > 0 ->
            let dir_idx = Object.Buffer.Read.uleb128 cur in
            let mtime = Object.Buffer.Read.uleb128 cur in
            let flen = Object.Buffer.Read.uleb128 cur in
            let dir_name =
              if dir_idx >= 1 && dir_idx <= Array.length directories then
                directories.(dir_idx - 1)
              else ""
            in
            files :=
              {
                name;
                timestamp = Unsigned.UInt64.of_int mtime;
                size = Unsigned.UInt64.of_int flen;
                directory = dir_name;
                md5_checksum = None;
              }
              :: !files;
            read_files ()
        | _ -> ()
      in
      read_files ();
      let file_names = Array.of_list (List.rev !files) in

      {
        format;
        unit_length;
        version;
        address_size;
        segment_selector_size;
        header_length;
        minimum_instruction_length;
        maximum_operations_per_instruction;
        default_is_stmt;
        line_base;
        line_range;
        opcode_base;
        standard_opcode_lengths;
        directory_entry_format_count = Unsigned.UInt8.of_int 0;
        directory_entry_formats = [||];
        directories_count = Unsigned.UInt32.of_int (Array.length directories);
        directories;
        file_name_entry_format_count = Unsigned.UInt8.of_int 0;
        file_name_entry_formats = [||];
        file_names_count = Unsigned.UInt32.of_int (Array.length file_names);
        file_names;
      }

  (** Parse the line number program following the header.

      This implements the DWARF 5 line number state machine as specified in
      section 6.2.2. The state machine processes opcodes and generates line
      table entries that map program addresses to source locations. *)
  let parse_line_program (cur : Object.Buffer.cursor)
      (header : line_program_header) : line_table_entry list =
    (* Calculate program length from header.
       unit_length includes everything after the initial_length field.
       DWARF 5: version(2) + address_size(1) + segment_selector_size(1)
                + header_length_field(offset_size) = 4 + offset_size
       DWARF 4: version(2) + header_length_field(offset_size)
                = 2 + offset_size *)
    let offset_size = offset_size_for_format header.format in
    let version_int = Unsigned.UInt16.to_int header.version in
    let header_overhead =
      if version_int >= 5 then 4 + offset_size else 2 + offset_size
    in
    let total_program_length =
      Unsigned.UInt64.to_int header.unit_length
      - header_overhead
      - Unsigned.UInt64.to_int header.header_length
    in
    let bytes_read = ref 0 in

    (* Initialize the line number state machine.
       DWARF 5: file register starts at 0 (0-based indices).
       DWARF 4: file register starts at 1 (1-based indices). *)
    let is_dwarf4 = version_int < 5 in
    let init_file = if is_dwarf4 then 1 else 0 in
    let address = ref (Unsigned.UInt64.of_int 0) in
    let op_index = ref (Unsigned.UInt32.of_int 0) in
    let file_index = ref (Unsigned.UInt32.of_int init_file) in
    let line = ref (Unsigned.UInt32.of_int 1) in
    let column = ref (Unsigned.UInt32.of_int 0) in
    let is_stmt = ref header.default_is_stmt in
    let basic_block = ref false in
    let end_sequence = ref false in
    let prologue_end = ref false in
    let epilogue_begin = ref false in
    let isa = ref (Unsigned.UInt32.of_int 0) in
    let discriminator = ref (Unsigned.UInt32.of_int 0) in

    let entries = ref [] in
    let program_done = ref false in

    (* Helper to reset state machine after end_sequence *)
    let reset_state () =
      address := Unsigned.UInt64.of_int 0;
      op_index := Unsigned.UInt32.of_int 0;
      file_index := Unsigned.UInt32.of_int init_file;
      line := Unsigned.UInt32.of_int 1;
      column := Unsigned.UInt32.of_int 0;
      is_stmt := header.default_is_stmt;
      basic_block := false;
      end_sequence := false;
      prologue_end := false;
      epilogue_begin := false;
      isa := Unsigned.UInt32.of_int 0;
      discriminator := Unsigned.UInt32.of_int 0
    in

    (* Helper function to create a line table entry.
       Normalizes file_index to 0-based for both DWARF 4 and 5. *)
    let make_entry () =
      let normalized_file_index =
        if is_dwarf4 then
          let v = Unsigned.UInt32.to_int !file_index in
          Unsigned.UInt32.of_int (max 0 (v - 1))
        else !file_index
      in
      {
        address = !address;
        line = !line;
        column = !column;
        file_index = normalized_file_index;
        isa = !isa;
        discriminator = !discriminator;
        op_index = !op_index;
        is_stmt = !is_stmt;
        basic_block = !basic_block;
        end_sequence = !end_sequence;
        prologue_end = !prologue_end;
        epilogue_begin = !epilogue_begin;
      }
    in

    (* Process opcodes until end of program *)
    while (not !program_done) && !bytes_read < total_program_length do
      try
        let opcode = Object.Buffer.Read.u8 cur in
        bytes_read := !bytes_read + 1;
        let opcode_val = Unsigned.UInt8.to_int opcode in

        if opcode_val = 0 then (
          (* Extended opcode *)
          let length = Object.Buffer.Read.uleb128 cur in
          (* LEB128 encoding: conservatively estimate max 5 bytes for typical values *)
          bytes_read := !bytes_read + if length < 128 then 1 else 2;
          let extended_opcode = Object.Buffer.Read.u8 cur in
          bytes_read := !bytes_read + 1;
          let extended_val = Unsigned.UInt8.to_int extended_opcode in

          (* Extended opcode handling: DWARF 5 extended opcode support.
           * We implement the standard extended opcodes:
           * - DW_LNE_end_sequence (0x01): End of line number sequence
           * - DW_LNE_set_address (0x02): Set absolute address
           * - DW_LNE_set_discriminator (0x04): Set discriminator for profiling
           *
           * Note: DW_LNE_define_file (0x03) is deprecated in DWARF 5 and not supported.
           *
           * For unknown extended opcodes (vendor extensions 0x80-0xFF or future opcodes),
           * we safely skip them by reading exactly 'length - 1' bytes to maintain buffer
           * integrity while ensuring parser robustness.
           *
           * This implementation follows DWARF 5 spec section 6.2.5.3. *)
          match extended_val with
          | 0x01 ->
              (* DW_LNE_end_sequence *)
              (* Set end_sequence, emit row, then reset state and continue *)
              end_sequence := true;
              entries := make_entry () :: !entries;
              reset_state ()
          | 0x02 ->
              (* DW_LNE_set_address *)
              let addr_bytes = length - 1 in
              if addr_bytes = 4 then (
                address :=
                  Object.Buffer.Read.u32 cur |> Unsigned.UInt64.of_uint32;
                bytes_read := !bytes_read + 4)
              else if addr_bytes = 8 then (
                address := Object.Buffer.Read.u64 cur;
                bytes_read := !bytes_read + 8)
              else failwith "Unsupported address size in DW_LNE_set_address"
          | 0x04 ->
              (* DW_LNE_set_discriminator *)
              let disc_val = Object.Buffer.Read.uleb128 cur in
              discriminator := Unsigned.UInt32.of_int disc_val;
              bytes_read := !bytes_read + if disc_val < 128 then 1 else 2
          | _ ->
              (* Skip unknown extended opcodes *)
              (* The DW_LNE_define_file operation defined in earlier versions of DWARF is deprecated
                in DWARF Version 5. *)
              for _i = 1 to length - 1 do
                ignore (Object.Buffer.Read.u8 cur);
                bytes_read := !bytes_read + 1
              done)
        else if opcode_val >= Unsigned.UInt8.to_int header.opcode_base then (
          (* Special opcode *)
          let adjusted_opcode =
            opcode_val - Unsigned.UInt8.to_int header.opcode_base
          in
          let line_increment =
            header.line_base
            + (adjusted_opcode mod Unsigned.UInt8.to_int header.line_range)
          in
          let address_increment =
            adjusted_opcode
            / Unsigned.UInt8.to_int header.line_range
            * Unsigned.UInt8.to_int header.minimum_instruction_length
          in

          address :=
            Unsigned.UInt64.add !address
              (Unsigned.UInt64.of_int address_increment);
          line :=
            Unsigned.UInt32.add !line (Unsigned.UInt32.of_int line_increment);

          entries := make_entry () :: !entries;
          basic_block := false;
          prologue_end := false;
          epilogue_begin := false;
          discriminator := Unsigned.UInt32.of_int 0)
        else
          (* Standard opcode *)
          match opcode_val with
          | 0x01 ->
              (* DW_LNS_copy *)
              entries := make_entry () :: !entries;
              basic_block := false;
              prologue_end := false;
              epilogue_begin := false;
              discriminator := Unsigned.UInt32.of_int 0
          | 0x02 ->
              (* DW_LNS_advance_pc *)
              let advance = Object.Buffer.Read.uleb128 cur in
              bytes_read := !bytes_read + if advance < 128 then 1 else 2;
              let address_increment =
                advance
                * Unsigned.UInt8.to_int header.minimum_instruction_length
              in
              address :=
                Unsigned.UInt64.add !address
                  (Unsigned.UInt64.of_int address_increment)
          | 0x03 ->
              (* DW_LNS_advance_line *)
              let advance = Object.Buffer.Read.sleb128 cur in
              bytes_read :=
                !bytes_read + if advance >= -64 && advance < 64 then 1 else 2;
              line := Unsigned.UInt32.add !line (Unsigned.UInt32.of_int advance)
          | 0x04 ->
              (* DW_LNS_set_file *)
              let file_val = Object.Buffer.Read.uleb128 cur in
              bytes_read := !bytes_read + if file_val < 128 then 1 else 2;
              file_index := Unsigned.UInt32.of_int file_val
          | 0x05 ->
              (* DW_LNS_set_column *)
              let col_val = Object.Buffer.Read.uleb128 cur in
              bytes_read := !bytes_read + if col_val < 128 then 1 else 2;
              column := Unsigned.UInt32.of_int col_val
          | 0x06 ->
              (* DW_LNS_negate_stmt *)
              is_stmt := not !is_stmt
          | 0x07 ->
              (* DW_LNS_set_basic_block *)
              basic_block := true
          | 0x08 ->
              (* DW_LNS_const_add_pc *)
              let adjusted_opcode =
                255 - Unsigned.UInt8.to_int header.opcode_base
              in
              let address_increment =
                adjusted_opcode
                / Unsigned.UInt8.to_int header.line_range
                * Unsigned.UInt8.to_int header.minimum_instruction_length
              in
              address :=
                Unsigned.UInt64.add !address
                  (Unsigned.UInt64.of_int address_increment)
          | 0x09 ->
              (* DW_LNS_fixed_advance_pc *)
              let advance = Object.Buffer.Read.u16 cur in
              bytes_read := !bytes_read + 2;
              address :=
                Unsigned.UInt64.add !address
                  (Unsigned.UInt64.of_int (Unsigned.UInt16.to_int advance))
          | 0x0a ->
              (* DW_LNS_set_prologue_end *)
              prologue_end := true
          | 0x0b ->
              (* DW_LNS_set_epilogue_begin *)
              epilogue_begin := true
          | 0x0c ->
              (* DW_LNS_set_isa *)
              let isa_val = Object.Buffer.Read.uleb128 cur in
              bytes_read := !bytes_read + if isa_val < 128 then 1 else 2;
              isa := Unsigned.UInt32.of_int isa_val
          | _ ->
              (* Skip unknown standard opcodes using operand count from header *)
              let operand_count =
                if opcode_val < Array.length header.standard_opcode_lengths then
                  Unsigned.UInt8.to_int
                    header.standard_opcode_lengths.(opcode_val - 1)
                else 0
              in
              for _i = 1 to operand_count do
                let val_read = Object.Buffer.Read.uleb128 cur in
                bytes_read := !bytes_read + if val_read < 128 then 1 else 2
              done
      with End_of_file | _ -> program_done := true
    done;

    List.rev !entries
end

module DebugLoc = struct
  type entry =
    | EndOfList
    | BaseAddress of u64
    | Location of { begin_addr : u64; end_addr : u64; expr : string }

  let parse_list (cur : Object.Buffer.cursor) (address_size : int) : entry list
      =
    let max_addr =
      if address_size = 4 then Unsigned.UInt64.of_int 0xFFFFFFFF
      else Unsigned.UInt64.max_int
    in
    let read_addr () =
      if address_size = 4 then
        Unsigned.UInt64.of_uint32 (Object.Buffer.Read.u32 cur)
      else Object.Buffer.Read.u64 cur
    in
    let entries = ref [] in
    let done_ = ref false in
    while not !done_ do
      let addr1 = read_addr () in
      let addr2 = read_addr () in
      if
        Unsigned.UInt64.compare addr1 Unsigned.UInt64.zero = 0
        && Unsigned.UInt64.compare addr2 Unsigned.UInt64.zero = 0
      then (
        entries := EndOfList :: !entries;
        done_ := true)
      else if Unsigned.UInt64.compare addr1 max_addr = 0 then
        entries := BaseAddress addr2 :: !entries
      else
        let len = Object.Buffer.Read.u16 cur |> Unsigned.UInt16.to_int in
        let expr = Object.Buffer.Read.fixed_string cur len in
        entries :=
          Location { begin_addr = addr1; end_addr = addr2; expr } :: !entries
    done;
    List.rev !entries
end

module DebugRanges = struct
  type entry =
    | EndOfList
    | BaseAddress of u64
    | Range of { begin_addr : u64; end_addr : u64 }

  let parse_list (cur : Object.Buffer.cursor) (address_size : int) : entry list
      =
    let max_addr =
      if address_size = 4 then Unsigned.UInt64.of_int 0xFFFFFFFF
      else Unsigned.UInt64.max_int
    in
    let read_addr () =
      if address_size = 4 then
        Unsigned.UInt64.of_uint32 (Object.Buffer.Read.u32 cur)
      else Object.Buffer.Read.u64 cur
    in
    let entries = ref [] in
    let done_ = ref false in
    while not !done_ do
      let addr1 = read_addr () in
      let addr2 = read_addr () in
      if
        Unsigned.UInt64.compare addr1 Unsigned.UInt64.zero = 0
        && Unsigned.UInt64.compare addr2 Unsigned.UInt64.zero = 0
      then (
        entries := EndOfList :: !entries;
        done_ := true)
      else if Unsigned.UInt64.compare addr1 max_addr = 0 then
        entries := BaseAddress addr2 :: !entries
      else entries := Range { begin_addr = addr1; end_addr = addr2 } :: !entries
    done;
    List.rev !entries
end

let resolve_location_list (buffer : Object.Buffer.t) (offset : u64)
    (address_size : int) : DebugLoc.entry list option =
  match find_debug_section_by_type buffer Debug_loc with
  | None -> None
  | Some (section_offset, _section_size) ->
      let absolute_pos =
        Unsigned.UInt64.to_int section_offset + Unsigned.UInt64.to_int offset
      in
      let cur = Object.Buffer.cursor buffer ~at:absolute_pos in
      Some (DebugLoc.parse_list cur address_size)

let resolve_range_list (buffer : Object.Buffer.t) (offset : u64)
    (address_size : int) : DebugRanges.entry list option =
  match find_debug_section_by_type buffer Debug_ranges with
  | None -> None
  | Some (section_offset, _section_size) ->
      let absolute_pos =
        Unsigned.UInt64.to_int section_offset + Unsigned.UInt64.to_int offset
      in
      let cur = Object.Buffer.cursor buffer ~at:absolute_pos in
      Some (DebugRanges.parse_list cur address_size)

module DebugTypes = struct
  type type_unit_header = {
    format : dwarf_format;
    unit_length : u64;
    version : u16;
    debug_abbrev_offset : u64;
    address_size : u8;
    type_signature : u64;
    type_offset : u64;
    header_span : span;
  }

  let parse_type_unit_header (cur : Object.Buffer.cursor) :
      span * type_unit_header =
    let start = cur.position in
    let format, unit_length = parse_initial_length cur in
    let version = Object.Buffer.Read.u16 cur in
    let version_int = Unsigned.UInt16.to_int version in
    if version_int <> 4 then
      failwith
        (Printf.sprintf
           "Unsupported .debug_types version: %d (only 4 supported)" version_int);
    let debug_abbrev_offset = read_offset_for_format format cur in
    let address_size = Object.Buffer.Read.u8 cur in
    let type_signature = Object.Buffer.Read.u64 cur in
    let type_offset = read_offset_for_format format cur in
    let header_end = cur.position in
    let header_size = header_end - start in
    let header_span =
      {
        start = Unsigned.UInt64.of_int start;
        size = Unsigned.UInt64.of_int header_size;
      }
    in
    let length_field_size = match format with DWARF32 -> 4 | DWARF64 -> 12 in
    let total_size = start + length_field_size in
    let span =
      {
        start = Unsigned.UInt64.of_int start;
        size = Unsigned.UInt64.of_int total_size;
      }
    in
    ( span,
      {
        format;
        unit_length;
        version;
        debug_abbrev_offset;
        address_size;
        type_signature;
        type_offset;
        header_span;
      } )

  let parse_type_units (buffer : Object.Buffer.t) :
      (span * type_unit_header) Seq.t =
    match find_debug_section_by_type buffer Debug_types with
    | None -> Seq.empty
    | Some (section_offset, section_size) ->
        let section_end = Unsigned.UInt64.to_int section_size in
        let rec parse_units cursor_pos () =
          if cursor_pos >= section_end then Seq.Nil
          else
            try
              let absolute_pos =
                Unsigned.UInt64.to_int section_offset + cursor_pos
              in
              let cur = Object.Buffer.cursor buffer ~at:absolute_pos in
              let span, header = parse_type_unit_header cur in
              let unit_length = Unsigned.UInt64.to_int header.unit_length in
              let length_field_size =
                match header.format with DWARF32 -> 4 | DWARF64 -> 12
              in
              let next_pos = cursor_pos + unit_length + length_field_size in
              Seq.Cons ((span, header), parse_units next_pos)
            with exn ->
              Printf.eprintf "Error parsing type unit at offset %d: %s\n"
                cursor_pos (Printexc.to_string exn);
              Seq.Nil
        in
        parse_units 0
end

module DebugPubnames = struct
  type header = {
    format : dwarf_format;
    unit_length : u64;
    version : u16;
    debug_info_offset : u64;
    debug_info_length : u64;
  }

  type entry = { offset : u64; name : string }

  let parse_set (cur : Object.Buffer.cursor) : header * entry list =
    let format, unit_length = parse_initial_length cur in
    let version = Object.Buffer.Read.u16 cur in
    let debug_info_offset = read_offset_for_format format cur in
    let debug_info_length = read_offset_for_format format cur in
    let header =
      { format; unit_length; version; debug_info_offset; debug_info_length }
    in
    let entries = ref [] in
    let done_ = ref false in
    while not !done_ do
      let offset = read_offset_for_format format cur in
      if Unsigned.UInt64.compare offset Unsigned.UInt64.zero = 0 then
        done_ := true
      else
        match Object.Buffer.Read.zero_string cur () with
        | Some name -> entries := { offset; name } :: !entries
        | None -> done_ := true
    done;
    (header, List.rev !entries)
end

module DebugPubtypes = struct
  type header = {
    format : dwarf_format;
    unit_length : u64;
    version : u16;
    debug_info_offset : u64;
    debug_info_length : u64;
  }

  type entry = { offset : u64; name : string }

  let parse_set (cur : Object.Buffer.cursor) : header * entry list =
    let format, unit_length = parse_initial_length cur in
    let version = Object.Buffer.Read.u16 cur in
    let debug_info_offset = read_offset_for_format format cur in
    let debug_info_length = read_offset_for_format format cur in
    let header =
      { format; unit_length; version; debug_info_offset; debug_info_length }
    in
    let entries = ref [] in
    let done_ = ref false in
    while not !done_ do
      let offset = read_offset_for_format format cur in
      if Unsigned.UInt64.compare offset Unsigned.UInt64.zero = 0 then
        done_ := true
      else
        match Object.Buffer.Read.zero_string cur () with
        | Some name -> entries := { offset; name } :: !entries
        | None -> done_ := true
    done;
    (header, List.rev !entries)
end

(** Call frame information parsing for Debug_frame section *)
module CallFrame = struct
  let debug_frame_cie_id = Unsigned.UInt32.of_int32 0xffffffffl

  type common_information_entry = {
    format : dwarf_format;
    length : u64;
    cie_id : u64;
    version : u8;
    augmentation : string;
    address_size : u8;
    segment_selector_size : u8;
    code_alignment_factor : u64;
    data_alignment_factor : i64;
    return_address_register : u64;
    augmentation_length : u64 option;
    augmentation_data : string option;
    initial_instructions : string;
    header_span : span; (* Tracks exact header size *)
    offset : u64; (* Section-relative offset where CIE starts *)
  }

  type frame_description_entry = {
    format : dwarf_format;
    length : u64;
    cie_pointer : u64;
    initial_location : u64;
    address_range : u64;
    augmentation_length : u64 option;
    augmentation_data : string option;
    instructions : string;
    offset : u64; (* File offset where this FDE starts *)
  }

  (* CFI rule types for state machine *)
  type cfi_rule =
    | Rule_undefined
    | Rule_same_value
    | Rule_offset of int64 (* offset from CFA *)
    | Rule_val_offset of int64 (* value = CFA + offset *)
    | Rule_register of int (* register number *)
    | Rule_expression of string (* DWARF expression *)
    | Rule_val_expression of string (* DWARF expression for value *)

  (* CFI state for tracking register rules *)
  type cfi_state = {
    cfa_register : int;
    cfa_offset : int64;
    register_rules : (int, cfi_rule) Hashtbl.t;
    pc_offset : int;
    state_stack : cfi_state list; (* Stack for remember_state/restore_state *)
  }

  let create_default_cie () =
    {
      format = DWARF32;
      length = Unsigned.UInt64.of_int 0;
      cie_id = Unsigned.UInt64.of_int 0;
      version = Unsigned.UInt8.of_int 1;
      augmentation = "";
      address_size = Unsigned.UInt8.of_int 8;
      segment_selector_size = Unsigned.UInt8.of_int 0;
      code_alignment_factor = Unsigned.UInt64.of_int 1;
      data_alignment_factor = Signed.Int64.of_int (-8);
      return_address_register = Unsigned.UInt64.of_int 16;
      augmentation_length = None;
      augmentation_data = None;
      initial_instructions = "";
      header_span =
        { start = Unsigned.UInt64.of_int 0; size = Unsigned.UInt64.of_int 0 };
      offset = Unsigned.UInt64.of_int 0;
    }

  (** Parse a null-terminated augmentation string from a cursor *)
  let parse_augmentation_string (cur : Object.Buffer.cursor) : string =
    match Object.Buffer.Read.zero_string cur () with Some s -> s | None -> ""

  (** Parse augmentation data if present based on augmentation string *)
  let parse_augmentation_data (cur : Object.Buffer.cursor)
      (augmentation : string) : (u64 * string) option =
    if String.length augmentation > 0 && augmentation.[0] = 'z' then (
      let length_int = Object.Buffer.Read.uleb128 cur in
      let length = Unsigned.UInt64.of_int length_int in
      let data = Bytes.create length_int in
      for i = 0 to Bytes.length data - 1 do
        Bytes.set data i
          (Char.chr (Unsigned.UInt8.to_int (Object.Buffer.Read.u8 cur)))
      done;
      Some (length, Bytes.to_string data))
    else None

  (** Parse call frame instructions as raw bytes *)
  let parse_instructions (cur : Object.Buffer.cursor) (length : int) : string =
    let data = Bytes.create length in
    for i = 0 to length - 1 do
      Bytes.set data i
        (Char.chr (Unsigned.UInt8.to_int (Object.Buffer.Read.u8 cur)))
    done;
    Bytes.to_string data

  (** Parse a Common Information Entry from the Debug_frame section *)
  let parse_common_information_entry (cur : Object.Buffer.cursor) :
      common_information_entry =
    let start_pos = cur.position in
    let format, length = parse_initial_length cur in
    let cie_id = read_offset_for_format format cur in

    (* Verify this is actually a CIE (cie_id should be 0xffffffff for DWARF32,
       0xffffffffffffffff for DWARF64) *)
    let expected_cie_id =
      match format with
      | DWARF32 ->
          Unsigned.UInt64.of_uint32 (Unsigned.UInt32.of_int32 0xffffffffl)
      | DWARF64 -> Unsigned.UInt64.of_int64 0xffffffffffffffffL
    in
    if Unsigned.UInt64.compare cie_id expected_cie_id <> 0 then
      failwith "Invalid CIE: cie_id is not the debug_frame CIE identifier";

    let version = Object.Buffer.Read.u8 cur in
    let augmentation = parse_augmentation_string cur in
    let address_size = Object.Buffer.Read.u8 cur in
    let segment_selector_size = Object.Buffer.Read.u8 cur in
    let code_alignment_factor_int = Object.Buffer.Read.uleb128 cur in
    let code_alignment_factor =
      Unsigned.UInt64.of_int code_alignment_factor_int
    in
    let data_alignment_factor_int = Object.Buffer.Read.sleb128 cur in
    let data_alignment_factor = Signed.Int64.of_int data_alignment_factor_int in
    let return_address_register_int = Object.Buffer.Read.uleb128 cur in
    let return_address_register =
      Unsigned.UInt64.of_int return_address_register_int
    in

    (* Parse augmentation data if present *)
    let augmentation_length, augmentation_data =
      match parse_augmentation_data cur augmentation with
      | Some (len, data) -> (Some len, Some data)
      | None -> (None, None)
    in

    (* Calculate exact header size using cursor position tracking *)
    let header_end_pos = cur.position in
    let header_span =
      {
        start = Unsigned.UInt64.of_int start_pos;
        size = Unsigned.UInt64.of_int (header_end_pos - start_pos);
      }
    in

    (* Calculate exact instructions length using total entry size *)
    let length_field_size = match format with DWARF32 -> 4 | DWARF64 -> 12 in
    let total_entry_size = Unsigned.UInt64.to_int length + length_field_size in
    let instructions_length = total_entry_size - (header_end_pos - start_pos) in
    let initial_instructions =
      if instructions_length > 0 then parse_instructions cur instructions_length
      else ""
    in

    {
      format;
      length;
      cie_id;
      version;
      augmentation;
      address_size;
      segment_selector_size;
      code_alignment_factor;
      data_alignment_factor;
      return_address_register;
      augmentation_length;
      augmentation_data;
      initial_instructions;
      header_span;
      offset = Unsigned.UInt64.of_int 0;
      (* Debug frame CIEs don't have meaningful offsets for eh_frame lookup *)
    }

  (** Parse a Frame Description Entry from the Debug_frame section *)
  let parse_frame_description_entry (cur : Object.Buffer.cursor)
      (start_pos : int) : frame_description_entry =
    let format, length = parse_initial_length cur in
    let cie_pointer = read_offset_for_format format cur in
    let initial_location = Object.Buffer.Read.u64 cur in
    let address_range = Object.Buffer.Read.u64 cur in

    (* Calculate remaining bytes for instructions *)
    let offset_size = offset_size_for_format format in
    let header_size = offset_size + 16 in
    (* cie_pointer + initial_location + address_range *)
    let total_length = Unsigned.UInt64.to_int length in
    let length_field_size = match format with DWARF32 -> 4 | DWARF64 -> 12 in
    let instructions_length =
      max 0 (total_length - header_size + length_field_size)
    in

    (* For now, assume no augmentation data in FDEs for simplicity *)
    let augmentation_length = None in
    let augmentation_data = None in

    (* Parse the FDE instructions *)
    let instructions =
      if instructions_length > 0 then parse_instructions cur instructions_length
      else ""
    in

    {
      format;
      length;
      cie_pointer;
      initial_location;
      address_range;
      augmentation_length;
      augmentation_data;
      instructions;
      offset = Unsigned.UInt64.of_int start_pos;
    }

  (** Debug Frame section entry type *)
  type debug_frame_entry =
    | CIE of common_information_entry
    | FDE of frame_description_entry
    | Zero_terminator of int (* Position of zero terminator *)

  type debug_frame_section = {
    entries : debug_frame_entry list;
    entry_count : int;
  }
  (** Debug Frame section *)

  (** Parse debug_frame section from cursor *)
  let parse_debug_frame_section cursor section_size =
    let section_end = cursor.position + section_size in
    let entries = ref [] in
    let entry_count = ref 0 in

    try
      while cursor.position < section_end do
        let start_pos = cursor.position in
        (* Peek at first 4 bytes to check for zero terminator or DWARF64 marker *)
        let first_word = Object.Buffer.Read.u32 cursor in
        let first_word_int = Unsigned.UInt32.to_int first_word in

        if first_word_int = 0 then (
          (* Zero length indicates end of section *)
          entries := Zero_terminator start_pos :: !entries;
          cursor.position <- section_end (* End parsing *))
        else
          (* Determine format and read ID field *)
          cursor.position <- start_pos;
        let _format, _length = parse_initial_length cursor in
        let _id_pos = cursor.position in
        let id = Object.Buffer.Read.u32 cursor in

        (* Reset cursor to parse the full entry *)
        cursor.position <- start_pos;

        if Unsigned.UInt32.compare id debug_frame_cie_id = 0 then (
          (* This is a Common Information Entry (CIE) *)
          let cie = parse_common_information_entry cursor in
          entries := CIE cie :: !entries;
          incr entry_count)
        else
          (* This is a Frame Description Entry (FDE) *)
          let fde = parse_frame_description_entry cursor start_pos in
          entries := FDE fde :: !entries;
          incr entry_count
      done;
      { entries = List.rev !entries; entry_count = !entry_count }
    with End_of_file | _ ->
      { entries = List.rev !entries; entry_count = !entry_count }

  (* TODO This is x86_64 specific, we want to support ARM64 as well *)
  (* Create initial CFI state *)
  let initial_cfi_state () =
    {
      cfa_register = 7;
      (* Default RSP for x86_64 *)
      cfa_offset = 8L;
      (* Default stack pointer offset *)
      register_rules = Hashtbl.create 32;
      pc_offset = 0;
      state_stack = [];
    }

  (* Parse CIE initial instructions to establish proper initial CFI state *)
  let parse_initial_state (cie : common_information_entry) : cfi_state =
    if String.length cie.initial_instructions = 0 then
      (* No initial instructions, use architecture-aware defaults *)
      initial_cfi_state ()
    else
      (* Parse CIE initial instructions to establish baseline state *)
      let data_alignment = Signed.Int64.to_int64 cie.data_alignment_factor in
      let initial_state = initial_cfi_state () in

      (* Apply CIE initial instructions to the default state *)
      let rec apply_initial_instructions state pos =
        if pos >= String.length cie.initial_instructions then state
        else
          let opcode = Char.code cie.initial_instructions.[pos] in
          let instruction = decode_cfa_opcode opcode in
          match instruction with
          | DW_CFA_def_cfa ->
              (* DW_CFA_def_cfa register offset *)
              let reg, pos1 =
                read_uleb128_from_string cie.initial_instructions (pos + 1)
              in
              let offset, pos2 =
                read_uleb128_from_string cie.initial_instructions pos1
              in
              let new_state =
                {
                  state with
                  cfa_register = reg;
                  cfa_offset = Int64.of_int offset;
                }
              in
              apply_initial_instructions new_state pos2
          | DW_CFA_def_cfa_register ->
              (* DW_CFA_def_cfa_register register *)
              let reg, next_pos =
                read_uleb128_from_string cie.initial_instructions (pos + 1)
              in
              let new_state = { state with cfa_register = reg } in
              apply_initial_instructions new_state next_pos
          | DW_CFA_def_cfa_offset ->
              (* DW_CFA_def_cfa_offset offset *)
              let offset, next_pos =
                read_uleb128_from_string cie.initial_instructions (pos + 1)
              in
              let new_state = { state with cfa_offset = Int64.of_int offset } in
              apply_initial_instructions new_state next_pos
          | DW_CFA_offset ->
              (* DW_CFA_offset register (embedded) offset *)
              let reg = opcode land 0x3f in
              let offset_uleb, next_pos =
                read_uleb128_from_string cie.initial_instructions (pos + 1)
              in
              let offset =
                Int64.mul (Int64.of_int offset_uleb) data_alignment
              in
              Hashtbl.replace state.register_rules reg (Rule_offset offset);
              apply_initial_instructions state next_pos
          | DW_CFA_nop -> apply_initial_instructions state (pos + 1)
          | _ ->
              (* Skip unknown or unsupported instructions for CIE initial state *)
              apply_initial_instructions state (pos + 1)
      in
      apply_initial_instructions initial_state 0

  (* Read multi-byte fixed-size values from string at position *)
  let read_u8_from_string str pos =
    if pos < String.length str then (Char.code str.[pos], pos + 1) else (0, pos)

  let read_u16_from_string str pos =
    if pos + 1 < String.length str then
      let b0 = Char.code str.[pos] in
      let b1 = Char.code str.[pos + 1] in
      (* Little-endian byte order *)
      (b0 lor (b1 lsl 8), pos + 2)
    else (0, pos)

  let read_u32_from_string str pos =
    if pos + 3 < String.length str then
      let b0 = Char.code str.[pos] in
      let b1 = Char.code str.[pos + 1] in
      let b2 = Char.code str.[pos + 2] in
      let b3 = Char.code str.[pos + 3] in
      (* Little-endian byte order *)
      let result = b0 lor (b1 lsl 8) lor (b2 lsl 16) lor (b3 lsl 24) in
      (result, pos + 4)
    else (0, pos)

  (* Parse DWARF expression block for CFI instructions *)
  let parse_cfi_expression_block bytes pos =
    if pos < String.length bytes then
      let block_length, next_pos = read_uleb128_from_string bytes pos in
      if next_pos + block_length <= String.length bytes then
        let expression = String.sub bytes next_pos block_length in
        (expression, next_pos + block_length)
      else ("", pos)
    else ("", pos)

  (* Basic CFI instruction parser - extracts info from instruction bytes *)
  let parse_cfi_instructions (instructions : string) (code_alignment : int64)
      (_data_alignment : int64) : (int * string) list =
    let rec parse_byte_stream bytes pos pc_offset acc =
      if pos >= String.length bytes then List.rev acc
      else
        let opcode = Char.code bytes.[pos] in
        let instruction = decode_cfa_opcode opcode in
        match instruction with
        | DW_CFA_def_cfa ->
            (* DW_CFA_def_cfa takes register and offset *)
            if pos + 2 < String.length bytes then
              let reg = Char.code bytes.[pos + 1] in
              let offset = Char.code bytes.[pos + 2] in
              let desc = Printf.sprintf "<off cfa=%02d(r%d) >" offset reg in
              parse_byte_stream bytes (pos + 3) pc_offset
                ((pc_offset, desc) :: acc)
            else List.rev acc
        | DW_CFA_offset ->
            (* DW_CFA_offset with embedded register number *)
            let reg = opcode land 0x3f in
            if pos + 1 < String.length bytes then
              let offset = Char.code bytes.[pos + 1] in
              let desc =
                Printf.sprintf "<off r%d=-%d(cfa) >" reg (offset * 8)
              in
              parse_byte_stream bytes (pos + 2) pc_offset
                ((pc_offset, desc) :: acc)
            else List.rev acc
        | DW_CFA_advance_loc ->
            (* DW_CFA_advance_loc with embedded delta *)
            let delta = opcode land 0x3f in
            parse_byte_stream bytes (pos + 1) (pc_offset + delta) acc
        | DW_CFA_nop -> parse_byte_stream bytes (pos + 1) pc_offset acc
        | DW_CFA_restore ->
            (* DW_CFA_restore with embedded register number *)
            let reg = opcode land 0x3f in
            let desc = Printf.sprintf "<restore r%d >" reg in
            parse_byte_stream bytes (pos + 1) pc_offset
              ((pc_offset, desc) :: acc)
        | DW_CFA_undefined ->
            (* DW_CFA_undefined takes register as ULEB128 *)
            if pos + 1 < String.length bytes then
              let reg, next_pos = read_uleb128_from_string bytes (pos + 1) in
              let desc = Printf.sprintf "<undefined r%d >" reg in
              parse_byte_stream bytes next_pos pc_offset
                ((pc_offset, desc) :: acc)
            else List.rev acc
        | DW_CFA_same_value ->
            (* DW_CFA_same_value takes register as ULEB128 *)
            if pos + 1 < String.length bytes then
              let reg, next_pos = read_uleb128_from_string bytes (pos + 1) in
              let desc = Printf.sprintf "<same_value r%d >" reg in
              parse_byte_stream bytes next_pos pc_offset
                ((pc_offset, desc) :: acc)
            else List.rev acc
        | DW_CFA_register ->
            (* DW_CFA_register takes register and target register as ULEB128 *)
            if pos + 1 < String.length bytes then
              let reg, pos1 = read_uleb128_from_string bytes (pos + 1) in
              if pos1 < String.length bytes then
                let target_reg, next_pos =
                  read_uleb128_from_string bytes pos1
                in
                let desc =
                  Printf.sprintf "<register r%d=r%d >" reg target_reg
                in
                parse_byte_stream bytes next_pos pc_offset
                  ((pc_offset, desc) :: acc)
              else List.rev acc
            else List.rev acc
        | DW_CFA_def_cfa_register ->
            (* DW_CFA_def_cfa_register takes register as ULEB128 *)
            if pos + 1 < String.length bytes then
              let reg, next_pos = read_uleb128_from_string bytes (pos + 1) in
              let desc = Printf.sprintf "<def_cfa_register r%d >" reg in
              parse_byte_stream bytes next_pos pc_offset
                ((pc_offset, desc) :: acc)
            else List.rev acc
        | DW_CFA_def_cfa_offset ->
            (* DW_CFA_def_cfa_offset takes offset as ULEB128 *)
            if pos + 1 < String.length bytes then
              let offset, next_pos = read_uleb128_from_string bytes (pos + 1) in
              let desc = Printf.sprintf "<def_cfa_offset %d >" offset in
              parse_byte_stream bytes next_pos pc_offset
                ((pc_offset, desc) :: acc)
            else List.rev acc
        | DW_CFA_advance_loc1 ->
            (* DW_CFA_advance_loc1 takes 1-byte delta *)
            if pos + 1 < String.length bytes then
              let delta, next_pos = read_u8_from_string bytes (pos + 1) in
              parse_byte_stream bytes next_pos (pc_offset + delta) acc
            else List.rev acc
        | DW_CFA_advance_loc2 ->
            (* DW_CFA_advance_loc2 takes 2-byte delta *)
            if pos + 2 < String.length bytes then
              let delta, next_pos = read_u16_from_string bytes (pos + 1) in
              parse_byte_stream bytes next_pos (pc_offset + delta) acc
            else List.rev acc
        | DW_CFA_advance_loc4 ->
            (* DW_CFA_advance_loc4 takes 4-byte delta *)
            if pos + 4 < String.length bytes then
              let delta, next_pos = read_u32_from_string bytes (pos + 1) in
              parse_byte_stream bytes next_pos (pc_offset + delta) acc
            else List.rev acc
        | DW_CFA_set_loc ->
            (* DW_CFA_set_loc takes address (4-byte for 32-bit targets) *)
            if pos + 4 < String.length bytes then
              let new_loc, next_pos = read_u32_from_string bytes (pos + 1) in
              parse_byte_stream bytes next_pos new_loc acc
            else List.rev acc
        | DW_CFA_offset_extended ->
            (* DW_CFA_offset_extended takes ULEB128 register + ULEB128 offset *)
            if pos + 1 < String.length bytes then
              let reg, pos1 = read_uleb128_from_string bytes (pos + 1) in
              if pos1 < String.length bytes then
                let offset, next_pos = read_uleb128_from_string bytes pos1 in
                let desc =
                  Printf.sprintf "<off r%d=-%d(cfa) >" reg (offset * 8)
                in
                parse_byte_stream bytes next_pos pc_offset
                  ((pc_offset, desc) :: acc)
              else List.rev acc
            else List.rev acc
        | DW_CFA_restore_extended ->
            (* DW_CFA_restore_extended takes ULEB128 register *)
            if pos + 1 < String.length bytes then
              let reg, next_pos = read_uleb128_from_string bytes (pos + 1) in
              let desc = Printf.sprintf "<restore r%d >" reg in
              parse_byte_stream bytes next_pos pc_offset
                ((pc_offset, desc) :: acc)
            else List.rev acc
        | DW_CFA_remember_state ->
            (* DW_CFA_remember_state - push current state onto stack *)
            let desc = Printf.sprintf "<remember_state >" in
            parse_byte_stream bytes (pos + 1) pc_offset
              ((pc_offset, desc) :: acc)
        | DW_CFA_restore_state ->
            (* DW_CFA_restore_state - pop state from stack *)
            let desc = Printf.sprintf "<restore_state >" in
            parse_byte_stream bytes (pos + 1) pc_offset
              ((pc_offset, desc) :: acc)
        | DW_CFA_def_cfa_expression ->
            (* DW_CFA_def_cfa_expression takes ULEB128 length + expression block *)
            if pos + 1 < String.length bytes then
              let expression, next_pos =
                parse_cfi_expression_block bytes (pos + 1)
              in
              let desc =
                Printf.sprintf "<def_cfa_expression len=%d >"
                  (String.length expression)
              in
              parse_byte_stream bytes next_pos pc_offset
                ((pc_offset, desc) :: acc)
            else List.rev acc
        | DW_CFA_expression ->
            (* DW_CFA_expression takes ULEB128 register + ULEB128 length + expression block *)
            if pos + 1 < String.length bytes then
              let reg, pos1 = read_uleb128_from_string bytes (pos + 1) in
              if pos1 < String.length bytes then
                let expression, next_pos =
                  parse_cfi_expression_block bytes pos1
                in
                let desc =
                  Printf.sprintf "<expression r%d len=%d >" reg
                    (String.length expression)
                in
                parse_byte_stream bytes next_pos pc_offset
                  ((pc_offset, desc) :: acc)
              else List.rev acc
            else List.rev acc
        | DW_CFA_val_expression ->
            (* DW_CFA_val_expression takes ULEB128 register + ULEB128 length + expression block *)
            if pos + 1 < String.length bytes then
              let reg, pos1 = read_uleb128_from_string bytes (pos + 1) in
              if pos1 < String.length bytes then
                let expression, next_pos =
                  parse_cfi_expression_block bytes pos1
                in
                let desc =
                  Printf.sprintf "<val_expression r%d len=%d >" reg
                    (String.length expression)
                in
                parse_byte_stream bytes next_pos pc_offset
                  ((pc_offset, desc) :: acc)
              else List.rev acc
            else List.rev acc
        | DW_CFA_offset_extended_sf ->
            (* DW_CFA_offset_extended_sf takes ULEB128 register + SLEB128 signed factored offset *)
            if pos + 1 < String.length bytes then
              let reg, pos1 = read_uleb128_from_string bytes (pos + 1) in
              if pos1 < String.length bytes then
                let signed_offset, next_pos =
                  read_sleb128_from_string bytes pos1
                in
                let desc =
                  Printf.sprintf "<off_sf r%d=%+d*data_align(cfa) >" reg
                    signed_offset
                in
                parse_byte_stream bytes next_pos pc_offset
                  ((pc_offset, desc) :: acc)
              else List.rev acc
            else List.rev acc
        | DW_CFA_def_cfa_sf ->
            (* DW_CFA_def_cfa_sf takes ULEB128 register + SLEB128 signed factored offset *)
            if pos + 1 < String.length bytes then
              let reg, pos1 = read_uleb128_from_string bytes (pos + 1) in
              if pos1 < String.length bytes then
                let signed_offset, next_pos =
                  read_sleb128_from_string bytes pos1
                in
                let desc =
                  Printf.sprintf "<def_cfa_sf r%d %+d*data_align >" reg
                    signed_offset
                in
                parse_byte_stream bytes next_pos pc_offset
                  ((pc_offset, desc) :: acc)
              else List.rev acc
            else List.rev acc
        | DW_CFA_def_cfa_offset_sf ->
            (* DW_CFA_def_cfa_offset_sf takes SLEB128 signed factored offset *)
            if pos + 1 < String.length bytes then
              let signed_offset, next_pos =
                read_sleb128_from_string bytes (pos + 1)
              in
              let desc =
                Printf.sprintf "<def_cfa_offset_sf %+d*data_align >"
                  signed_offset
              in
              parse_byte_stream bytes next_pos pc_offset
                ((pc_offset, desc) :: acc)
            else List.rev acc
        | DW_CFA_val_offset ->
            (* DW_CFA_val_offset takes ULEB128 register + ULEB128 offset *)
            if pos + 1 < String.length bytes then
              let reg, pos1 = read_uleb128_from_string bytes (pos + 1) in
              if pos1 < String.length bytes then
                let offset, next_pos = read_uleb128_from_string bytes pos1 in
                let desc =
                  Printf.sprintf "<val_offset r%d=cfa+%d*data_align >" reg
                    offset
                in
                parse_byte_stream bytes next_pos pc_offset
                  ((pc_offset, desc) :: acc)
              else List.rev acc
            else List.rev acc
        | DW_CFA_val_offset_sf ->
            (* DW_CFA_val_offset_sf takes ULEB128 register + SLEB128 signed factored offset *)
            if pos + 1 < String.length bytes then
              let reg, pos1 = read_uleb128_from_string bytes (pos + 1) in
              if pos1 < String.length bytes then
                let signed_offset, next_pos =
                  read_sleb128_from_string bytes pos1
                in
                let desc =
                  Printf.sprintf "<val_offset_sf r%d=cfa%+d*data_align >" reg
                    signed_offset
                in
                parse_byte_stream bytes next_pos pc_offset
                  ((pc_offset, desc) :: acc)
              else List.rev acc
            else List.rev acc
        | _ ->
            (* Skip unknown instructions for now *)
            parse_byte_stream bytes (pos + 1) pc_offset acc
    in
    let basic_results = parse_byte_stream instructions 0 0 [] in
    (* Apply scaling based on code alignment factor *)
    List.map
      (fun (pc_offset, desc) ->
        let scaled_pc =
          Int64.to_int (Int64.mul (Int64.of_int pc_offset) code_alignment)
        in
        (scaled_pc, desc))
      basic_results
end

(** EH Frame Header (.eh_frame_hdr section) - ELF exception handling support *)
module EHFrameHdr = struct
  type encoding =
    | DW_EH_PE_absptr (* 0x00 *)
    | DW_EH_PE_omit (* 0xff *)
    | DW_EH_PE_uleb128 (* 0x01 *)
    | DW_EH_PE_udata2 (* 0x02 *)
    | DW_EH_PE_udata4 (* 0x03 *)
    | DW_EH_PE_udata8 (* 0x04 *)
    | DW_EH_PE_sleb128 (* 0x09 *)
    | DW_EH_PE_sdata2 (* 0x0a *)
    | DW_EH_PE_sdata4 (* 0x0b *)
    | DW_EH_PE_sdata8 (* 0x0c *)
    | DW_EH_PE_pcrel (* 0x10 - PC relative *)
    | DW_EH_PE_datarel (* 0x30 - data relative *)
    | DW_EH_PE_funcrel (* 0x40 - function relative *)
    | DW_EH_PE_aligned (* 0x50 - aligned *)
    | DW_EH_PE_indirect (* 0x80 - indirect *)

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

  let encoding_of_u8 = function
    | 0x00 -> DW_EH_PE_absptr
    | 0xff -> DW_EH_PE_omit
    | 0x01 -> DW_EH_PE_uleb128
    | 0x02 -> DW_EH_PE_udata2
    | 0x03 -> DW_EH_PE_udata4
    | 0x04 -> DW_EH_PE_udata8
    | 0x09 -> DW_EH_PE_sleb128
    | 0x0a -> DW_EH_PE_sdata2
    | 0x0b -> DW_EH_PE_sdata4
    | 0x0c -> DW_EH_PE_sdata8
    | 0x10 -> DW_EH_PE_pcrel
    | 0x30 -> DW_EH_PE_datarel
    | 0x40 -> DW_EH_PE_funcrel
    | 0x50 -> DW_EH_PE_aligned
    | 0x80 ->
        DW_EH_PE_indirect
        (* Handle common combined encodings *)
        (* TODO Other common encodings to handle? *)
    | 0x1b -> DW_EH_PE_pcrel (* 0x10 | 0x0b = PC-relative signed 4-byte *)
    | 0x3b -> DW_EH_PE_datarel (* 0x30 | 0x0b = Data-relative signed 4-byte *)
    | n -> failwith (Printf.sprintf "Unknown EH encoding: 0x%02x" n)

  let read_encoded_value cursor encoding _base_addr =
    match encoding with
    | DW_EH_PE_absptr -> Object.Buffer.Read.u64 cursor
    | DW_EH_PE_udata4 ->
        Unsigned.UInt64.of_uint32 (Object.Buffer.Read.u32 cursor)
    | DW_EH_PE_sdata4 ->
        let value = Object.Buffer.Read.u32 cursor in
        let signed_value = Unsigned.UInt32.to_int32 value in
        Unsigned.UInt64.of_int64 (Int64.of_int32 signed_value)
    | DW_EH_PE_pcrel ->
        (* PC-relative: value is relative to current position *)
        let current_pos = Unsigned.UInt64.of_int cursor.position in
        let offset = Object.Buffer.Read.u32 cursor in
        let signed_offset = Unsigned.UInt32.to_int32 offset in
        Unsigned.UInt64.add current_pos
          (Unsigned.UInt64.of_int64 (Int64.of_int32 signed_offset))
    | DW_EH_PE_datarel ->
        (* Data-relative: value is relative to section base *)
        let offset = Object.Buffer.Read.u32 cursor in
        let signed_offset = Unsigned.UInt32.to_int32 offset in
        Unsigned.UInt64.add _base_addr
          (Unsigned.UInt64.of_int64 (Int64.of_int32 signed_offset))
    | _ -> failwith "Unsupported encoding in read_encoded_value"

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
end

(** EH Frame (.eh_frame section) - ELF exception handling call frame information
*)
module EHFrame = struct
  (* Reuse the CallFrame types but add EH-specific handling *)
  type eh_frame_entry =
    | EH_CIE of CallFrame.common_information_entry
    | EH_FDE of CallFrame.frame_description_entry

  type section = { entries : eh_frame_entry list }

  (* Parse CIE adapted for .eh_frame format *)
  let parse_eh_cie cursor _expected_length cie_offset =
    let length = Object.Buffer.Read.u32 cursor in
    let cie_id_start = cursor.position in
    (* Track where CIE content starts *)
    let cie_id = Object.Buffer.Read.u32 cursor in

    (* In .eh_frame, CIE ID is 0 (not 0xffffffff like in .debug_frame) *)
    if Unsigned.UInt32.to_int32 cie_id <> 0x00000000l then
      failwith "Invalid EH CIE: cie_id is not 0";

    let version = Object.Buffer.Read.u8 cursor in
    let augmentation = CallFrame.parse_augmentation_string cursor in

    (* EH frame may not have address_size and segment_selector_size for older versions *)
    let address_size, segment_selector_size =
      if Unsigned.UInt8.to_int version >= 4 then
        (Object.Buffer.Read.u8 cursor, Object.Buffer.Read.u8 cursor)
      else (Unsigned.UInt8.of_int 8, Unsigned.UInt8.of_int 0)
      (* Default values *)
    in

    let code_alignment_factor_int = Object.Buffer.Read.uleb128 cursor in
    let code_alignment_factor =
      Unsigned.UInt64.of_int code_alignment_factor_int
    in
    let data_alignment_factor_int = Object.Buffer.Read.sleb128 cursor in
    let data_alignment_factor = Signed.Int64.of_int data_alignment_factor_int in
    let return_address_register_int = Object.Buffer.Read.uleb128 cursor in
    let return_address_register =
      Unsigned.UInt64.of_int return_address_register_int
    in

    (* Parse augmentation data if present *)
    let augmentation_length, augmentation_data =
      match CallFrame.parse_augmentation_data cursor augmentation with
      | Some (len, data) -> (Some len, Some data)
      | None -> (None, None)
    in

    (* Calculate initial instructions length correctly *)
    let current_pos = cursor.position in
    let bytes_read_from_cie_id = current_pos - cie_id_start in
    let total_cie_size = Unsigned.UInt32.to_int length in
    (* Length field covers cie_id onwards *)

    let instructions_length = max 0 (total_cie_size - bytes_read_from_cie_id) in
    let initial_instructions =
      if instructions_length > 0 then
        CallFrame.parse_instructions cursor instructions_length
      else ""
    in

    {
      CallFrame.format = DWARF32;
      length = Unsigned.UInt64.of_uint32 length;
      cie_id = Unsigned.UInt64.of_uint32 cie_id;
      version;
      augmentation;
      address_size;
      segment_selector_size;
      code_alignment_factor;
      data_alignment_factor;
      return_address_register;
      augmentation_length;
      augmentation_data;
      initial_instructions;
      header_span =
        {
          start = Unsigned.UInt64.of_int cie_id_start;
          size = Unsigned.UInt64.of_int (current_pos - cie_id_start);
        };
      offset = Unsigned.UInt64.of_int cie_offset;
    }

  (* Parse FDE adapted for .eh_frame format *)
  let parse_eh_fde cursor _expected_length fde_offset =
    let length = Object.Buffer.Read.u32 cursor in
    let fde_start = cursor.position in
    (* Track where FDE content starts *)
    let cie_pointer = Object.Buffer.Read.u32 cursor in

    (* In .eh_frame, cie_pointer is a relative offset backwards to the CIE *)
    (* For now, we'll store it as-is and calculate the actual CIE reference later *)

    (* Read initial_location (PC start address) - in .eh_frame usually 32-bit PC-relative *)
    let initial_location_field_pos = cursor.position in
    let initial_location_raw = Object.Buffer.Read.u32 cursor in

    (* In .eh_frame, initial_location is PC-relative to the field position.
       Convert signed 32-bit to int32 first, then add field position to get absolute address *)
    let initial_location_offset =
      Int32.of_int (Unsigned.UInt32.to_int initial_location_raw)
    in
    let initial_location =
      Unsigned.UInt32.of_int
        Int32.(
          to_int
            (add initial_location_offset (of_int initial_location_field_pos)))
    in

    (* Read address_range (length of code covered) - in .eh_frame usually 32-bit *)
    let address_range = Object.Buffer.Read.u32 cursor in

    (* Parse augmentation data if present - check if remaining bytes suggest augmentation *)
    let current_pos = cursor.position in
    let bytes_read_from_fde = current_pos - fde_start in
    let total_fde_size = Unsigned.UInt32.to_int length in
    let remaining_bytes = total_fde_size - bytes_read_from_fde in

    (* Try to parse augmentation data if there are enough bytes and first byte looks like length *)
    let augmentation_length, augmentation_data, instructions_start =
      if remaining_bytes > 0 then (
        let saved_pos = cursor.position in
        try
          (* Try to read augmentation length as ULEB128 *)
          let aug_len = Object.Buffer.Read.uleb128 cursor in
          if aug_len >= 0 && aug_len < remaining_bytes then (
            (* Read augmentation data *)
            let data = Bytes.create aug_len in
            for i = 0 to aug_len - 1 do
              Bytes.set data i
                (Char.chr
                   (Unsigned.UInt8.to_int (Object.Buffer.Read.u8 cursor)))
            done;
            ( Some (Unsigned.UInt64.of_int aug_len),
              Some (Bytes.to_string data),
              cursor.position ))
          else (
            (* Not valid augmentation data, reset position *)
            cursor.position <- saved_pos;
            (None, None, saved_pos))
        with _ ->
          (* Error reading, reset position *)
          cursor.position <- saved_pos;
          (None, None, saved_pos))
      else (None, None, current_pos)
    in

    (* Parse remaining bytes as instructions *)
    let instructions_length =
      max 0 (total_fde_size - (instructions_start - fde_start))
    in
    let instructions =
      if instructions_length > 0 then
        CallFrame.parse_instructions cursor instructions_length
      else ""
    in

    {
      CallFrame.format = DWARF32;
      length = Unsigned.UInt64.of_uint32 length;
      cie_pointer = Unsigned.UInt64.of_uint32 cie_pointer;
      initial_location = Unsigned.UInt64.of_uint32 initial_location;
      address_range = Unsigned.UInt64.of_uint32 address_range;
      augmentation_length;
      augmentation_data;
      instructions;
      offset = Unsigned.UInt64.of_int fde_offset;
    }

  let parse_section cursor section_size =
    let section_start = cursor.position in
    (* Track section start for relative offsets *)
    let section_end = cursor.position + section_size in
    let entries = ref [] in

    while cursor.position < section_end do
      let start_pos = cursor.position in
      let length = Object.Buffer.Read.u32 cursor in
      let length_int = Unsigned.UInt32.to_int length in

      if length_int = 0 then
        (* Zero terminator *)
        cursor.position <- section_end
      else
        let id = Object.Buffer.Read.u32 cursor in
        (* Reset cursor to parse full entry *)
        cursor.position <- start_pos;

        if Unsigned.UInt32.to_int32 id = 0x00000000l then
          (* This is a CIE in .eh_frame format (id = 0 instead of 0xffffffff) *)
          let cie_section_offset = start_pos - section_start in
          let cie = parse_eh_cie cursor length cie_section_offset in
          entries := EH_CIE cie :: !entries
        else
          (* This is an FDE - parse it to get address ranges *)
          cursor.position <- start_pos;
        (* Reset to start of entry *)
        let fde_section_offset = start_pos - section_start in
        (* Calculate section-relative offset *)
        let fde = parse_eh_fde cursor length fde_section_offset in
        entries := EH_FDE fde :: !entries
    done;

    { entries = List.rev !entries }

  (* Find the CIE corresponding to an FDE using the cie_pointer field *)
  let find_cie_for_fde (section : section) (cie_pointer : u32)
      (fde_file_offset : int) : CallFrame.common_information_entry option =
    (* In .eh_frame format, cie_pointer is a relative offset backwards from the
       cie_pointer field's END position to the CIE start.

       The CIE offset is calculated as:
       cie_offset = (fde_offset + 8) - cie_pointer

       Where fde_offset is the start of the FDE and +8 accounts for the
       length field (4 bytes) plus the cie_pointer field (4 bytes).
       This gives us the position right after reading cie_pointer, from which
       we subtract cie_pointer to get the CIE location. *)
    let cie_pointer_position = fde_file_offset + 8 in
    let cie_offset =
      cie_pointer_position - Unsigned.UInt32.to_int cie_pointer
    in

    (* Search for CIE with matching offset *)
    let rec find_cie_by_offset = function
      | [] -> None
      | EH_CIE cie :: rest ->
          if Unsigned.UInt64.to_int cie.offset = cie_offset then Some cie
          else find_cie_by_offset rest
      | EH_FDE _ :: rest -> find_cie_by_offset rest
    in

    (* Try exact offset match first, but if not found, fall back to the first CIE
       This maintains compatibility while providing the correct implementation *)
    match find_cie_by_offset section.entries with
    | Some cie -> Some cie
    | None ->
        (* TODO Unclear if this fall-back is reasonable, it does appear to
        work in practice on sample programs. *)
        (* Fallback: find the first CIE in the section *)
        let rec find_first_cie = function
          | [] -> None
          | EH_CIE cie :: _ -> Some cie
          | EH_FDE _ :: rest -> find_first_cie rest
        in
        find_first_cie section.entries
end

(** Accelerated Name Lookup (.debug_names section) - DWARF 5 Section 6.1 *)
module DebugNames = struct
  type name_index_header = {
    format : dwarf_format;
    unit_length : u64;
    version : u16;
    padding : u16;
    comp_unit_count : u32;
    local_type_unit_count : u32;
    foreign_type_unit_count : u32;
    bucket_count : u32;
    name_count : u32;
    abbrev_table_size : u32;
    augmentation_string_size : u32;
    augmentation_string : string;
    span : int;  (** Total size of header in bytes *)
  }

  type debug_str_entry = {
    offset : u32;  (** Original offset in debug_str section *)
    value : string;  (** Resolved string value *)
  }
  (** String with original offset preserved for debug_names *)

  type entry_parse_result = {
    name_offset : u32;  (** Offset of name in debug_str section *)
    die_offset : u32;  (** Offset of DIE in debug_info section *)
    tag_name : string;  (** Human-readable tag name *)
    offset_hex : string;  (** Hexadecimal representation of offset *)
    unit_index : int option;  (** Index of compilation unit *)
    is_declaration : bool;  (** Whether this is a declaration *)
    compile_unit_index : u32 option;  (** DWARF 5 DW_IDX_compile_unit *)
    type_unit_index : u32 option;  (** DWARF 5 DW_IDX_type_unit *)
    type_hash : u64 option;  (** DWARF 5 DW_IDX_type_hash *)
  }
  (** Result of parsing a single entry from the entry pool *)

  type name_index_entry = {
    name_offset : u32;
    die_offset : u32;
    attributes : (name_index_attribute * u64) list;
  }

  type debug_names_abbrev = {
    code : u64;  (** Abbreviation code *)
    tag : abbreviation_tag;  (** DWARF tag *)
    attributes : (name_index_attribute * attribute_form_encoding) list;
        (** List of attributes and their forms *)
  }
  (** Abbreviation entry for debug_names *)

  type debug_names_section = {
    header : name_index_header;
    comp_unit_offsets : u32 array;
    local_type_unit_offsets : u32 array;
    foreign_type_unit_signatures : u64 array;
    buckets : u32 array;  (** Hash bucket organization *)
    hash_table : u32 array;
    name_table : debug_str_entry array;
    entry_offsets : u32 array;
        (** Entry offsets into entry pool for each name *)
    abbreviation_table : debug_names_abbrev list;
        (** Parsed abbreviation table *)
    entry_pool : name_index_entry array;
  }

  (** Parse augmentation string from debug_names header *)
  let parse_augmentation_string (cur : Object.Buffer.cursor) (size : u32) :
      string =
    if Unsigned.UInt32.to_int size = 0 then ""
    else
      let data = Bytes.create (Unsigned.UInt32.to_int size) in
      for i = 0 to Bytes.length data - 1 do
        Bytes.set data i
          (Char.chr (Unsigned.UInt8.to_int (Object.Buffer.Read.u8 cur)))
      done;
      Bytes.to_string data

  (** Parse name index header *)
  let parse_name_index_header (cur : Object.Buffer.cursor) : name_index_header =
    let start_pos = cur.position in
    let format, unit_length = parse_initial_length cur in
    let version = Object.Buffer.Read.u16 cur in
    let padding = Object.Buffer.Read.u16 cur in
    let comp_unit_count = Object.Buffer.Read.u32 cur in
    let local_type_unit_count = Object.Buffer.Read.u32 cur in
    let foreign_type_unit_count = Object.Buffer.Read.u32 cur in
    let bucket_count = Object.Buffer.Read.u32 cur in
    let name_count = Object.Buffer.Read.u32 cur in
    let abbrev_table_size = Object.Buffer.Read.u32 cur in
    let augmentation_string_size = Object.Buffer.Read.u32 cur in
    let augmentation_string =
      parse_augmentation_string cur augmentation_string_size
    in
    let end_pos = cur.position in
    let span = end_pos - start_pos in

    {
      format;
      unit_length;
      version;
      padding;
      comp_unit_count;
      local_type_unit_count;
      foreign_type_unit_count;
      bucket_count;
      name_count;
      abbrev_table_size;
      augmentation_string_size;
      augmentation_string;
      span;
    }

  (** Parse array of 32-bit offsets *)
  let parse_u32_array (cur : Object.Buffer.cursor) (count : u32) : u32 array =
    let arr =
      Array.make (Unsigned.UInt32.to_int count) (Unsigned.UInt32.of_int 0)
    in
    for i = 0 to Array.length arr - 1 do
      arr.(i) <- Object.Buffer.Read.u32 cur
    done;
    arr

  (** Parse array of 64-bit signatures *)
  let parse_u64_array (cur : Object.Buffer.cursor) (count : u32) : u64 array =
    let arr =
      Array.make (Unsigned.UInt32.to_int count) (Unsigned.UInt64.of_int 0)
    in
    for i = 0 to Array.length arr - 1 do
      arr.(i) <- Object.Buffer.Read.u64 cur
    done;
    arr

  (** Parse name table as array of debug_str_entry with offset tracking *)
  let parse_name_table_with_offsets (cur : Object.Buffer.cursor) (count : u32) :
      debug_str_entry array =
    let arr =
      Array.make
        (Unsigned.UInt32.to_int count)
        { offset = Unsigned.UInt32.zero; value = "" }
    in
    for i = 0 to Array.length arr - 1 do
      let start_pos = cur.position in
      let value =
        match Object.Buffer.Read.zero_string cur () with
        | Some s -> s
        | None -> ""
      in
      arr.(i) <- { offset = Unsigned.UInt32.of_int start_pos; value }
    done;
    arr

  (** Parse name table as array of null-terminated strings *)
  let parse_name_table (cur : Object.Buffer.cursor) (count : u32) : string array
      =
    let arr = Array.make (Unsigned.UInt32.to_int count) "" in
    for i = 0 to Array.length arr - 1 do
      arr.(i) <-
        (match Object.Buffer.Read.zero_string cur () with
        | Some s -> s
        | None -> "")
    done;
    arr

  (** DJB2 hash function used by DWARF 5 debug_names sections *)
  let djb2_hash (s : string) : u32 =
    let hash = ref 5381 in
    String.iter
      (fun c ->
        let char_code = Char.code c in
        hash := (!hash lsl 5) + !hash + char_code)
      s;
    Unsigned.UInt32.of_int !hash

  (** Resolve debug_str offset to debug_str_entry with both offset and value *)
  let resolve_debug_str_offset (buffer : Object.Buffer.t) (offset : u32) :
      debug_str_entry =
    match find_debug_section_by_type buffer Debug_str with
    | Some (str_section_offset, _) -> (
        match
          read_string_from_section buffer
            (Unsigned.UInt32.to_int offset)
            (Unsigned.UInt64.to_int str_section_offset)
        with
        | Some resolved_string -> { offset; value = resolved_string }
        | None ->
            {
              offset;
              value =
                Printf.sprintf "<str_error:0x%lx>"
                  (Unsigned.UInt32.to_int32 offset);
            })
    | None ->
        {
          offset;
          value =
            Printf.sprintf "<no_debug_str:0x%lx>"
              (Unsigned.UInt32.to_int32 offset);
        }

  (** Calculate the absolute byte address of an entry in the debug_names section
  *)
  let calculate_entry_address (section_base_offset : u32)
      (relative_offset : int) : u32 =
    Unsigned.UInt32.add section_base_offset
      (Unsigned.UInt32.of_int relative_offset)

  (** Calculate addresses for all components of a debug_names section *)
  let calculate_section_addresses (section_base_offset : u32)
      (header : name_index_header) : (string * u32) list =
    let addresses = ref [] in
    let current_offset = ref 0 in

    (* Header components *)
    addresses := ("header", section_base_offset) :: !addresses;
    current_offset := !current_offset + 40;

    (* Standard header size *)

    (* Compilation unit offsets *)
    if Unsigned.UInt32.to_int header.comp_unit_count > 0 then (
      addresses :=
        ( "comp_unit_offsets",
          calculate_entry_address section_base_offset !current_offset )
        :: !addresses;
      current_offset :=
        !current_offset + (Unsigned.UInt32.to_int header.comp_unit_count * 4));

    (* Local type unit offsets *)
    if Unsigned.UInt32.to_int header.local_type_unit_count > 0 then (
      addresses :=
        ( "local_type_unit_offsets",
          calculate_entry_address section_base_offset !current_offset )
        :: !addresses;
      current_offset :=
        !current_offset
        + (Unsigned.UInt32.to_int header.local_type_unit_count * 4));

    (* Foreign type unit signatures *)
    if Unsigned.UInt32.to_int header.foreign_type_unit_count > 0 then (
      addresses :=
        ( "foreign_type_unit_signatures",
          calculate_entry_address section_base_offset !current_offset )
        :: !addresses;
      current_offset :=
        !current_offset
        + (Unsigned.UInt32.to_int header.foreign_type_unit_count * 8));

    (* Hash table (buckets + hashes) *)
    addresses :=
      ( "hash_buckets",
        calculate_entry_address section_base_offset !current_offset )
      :: !addresses;
    current_offset :=
      !current_offset + (Unsigned.UInt32.to_int header.bucket_count * 4);

    addresses :=
      ( "hash_values",
        calculate_entry_address section_base_offset !current_offset )
      :: !addresses;
    current_offset :=
      !current_offset + (Unsigned.UInt32.to_int header.name_count * 4);

    (* Name table *)
    addresses :=
      ("name_table", calculate_entry_address section_base_offset !current_offset)
      :: !addresses;

    (* Abbreviation table and entry pool positions are dynamic based on name table size *)
    List.rev !addresses

  (** Parse debug_names abbreviation table *)
  let parse_debug_names_abbreviation_table (cur : Object.Buffer.cursor)
      (abbrev_table_size : u32) : debug_names_abbrev list =
    let start_position = cur.position in
    let end_position =
      start_position + Unsigned.UInt32.to_int abbrev_table_size
    in
    let abbreviations = ref ([] : debug_names_abbrev list) in

    let rec parse_abbreviations () =
      if cur.position >= end_position then ()
      else
        (* Read abbreviation code *)
        let code = Object.Buffer.Read.uleb128 cur |> Unsigned.UInt64.of_int in

        if Unsigned.UInt64.to_int code = 0 then ()
          (* End of abbreviations table *)
        else
          (* Read tag *)
          let tag_code =
            Object.Buffer.Read.uleb128 cur |> Unsigned.UInt64.of_int
          in
          let tag = abbreviation_tag_of_u64 tag_code in

          (* Parse attributes *)
          let attributes = ref [] in
          let rec parse_attrs () =
            let attr_code =
              Object.Buffer.Read.uleb128 cur |> Unsigned.UInt64.of_int
            in
            let form_code =
              Object.Buffer.Read.uleb128 cur |> Unsigned.UInt64.of_int
            in

            if
              Unsigned.UInt64.to_int attr_code = 0
              && Unsigned.UInt64.to_int form_code = 0
            then () (* End of attributes for this abbreviation *)
            else
              let attr = name_index_attribute_of_u64 attr_code in
              let form = attribute_form_encoding form_code in
              attributes := (attr, form) :: !attributes;
              parse_attrs ()
          in
          parse_attrs ();

          let abbrev_entry = { code; tag; attributes = List.rev !attributes } in
          abbreviations := abbrev_entry :: !abbreviations;
          parse_abbreviations ()
    in
    parse_abbreviations ();
    List.rev !abbreviations

  (** Parse a single entry from entry pool at a specific offset *)
  let parse_single_entry (cur : Object.Buffer.cursor)
      (abbrev_table : (u64, debug_names_abbrev) Hashtbl.t) : name_index_entry =
    let abbrev_code_int = Object.Buffer.Read.uleb128 cur in
    let abbrev_code = Unsigned.UInt64.of_int abbrev_code_int in
    match Hashtbl.find_opt abbrev_table abbrev_code with
    | Some abbrev ->
        let name_offset_ref = ref (Unsigned.UInt32.of_int 0) in
        let die_offset_ref = ref (Unsigned.UInt32.of_int 0) in
        let attributes_ref = ref [] in

        (* Parse attributes according to abbreviation specification *)
        List.iter
          (fun (attr, form) ->
            let value =
              match form with
              | DW_FORM_ref4 ->
                  let v = Object.Buffer.Read.u32 cur in
                  Unsigned.UInt64.of_uint32 v
              | DW_FORM_flag_present -> Unsigned.UInt64.of_int 1
              | DW_FORM_udata ->
                  Object.Buffer.Read.uleb128 cur |> Unsigned.UInt64.of_int
              | DW_FORM_unknown _ ->
                  (* Skip unknown forms by reading a single byte for now *)
                  Object.Buffer.Read.u8 cur |> Unsigned.UInt8.to_int
                  |> Unsigned.UInt64.of_int
              | _ ->
                  failwith
                    ("Unsupported form in debug_names entry pool: "
                    ^ string_of_attribute_form_encoding
                        (u64_of_attribute_form_encoding form))
            in

            match attr with
            | DW_IDX_die_offset ->
                die_offset_ref :=
                  Unsigned.UInt32.of_int64 (Unsigned.UInt64.to_int64 value)
            | _ -> attributes_ref := (attr, value) :: !attributes_ref)
          abbrev.attributes;

        {
          name_offset = !name_offset_ref;
          die_offset = !die_offset_ref;
          attributes = List.rev !attributes_ref;
        }
    | None ->
        let available_codes =
          Hashtbl.fold
            (fun k _ acc -> Unsigned.UInt64.to_int k :: acc)
            abbrev_table []
        in
        failwith
          (Printf.sprintf
             "Unknown abbreviation code in debug_names entry pool: %d \
              (available codes: [%s])"
             (Unsigned.UInt64.to_int abbrev_code)
             (String.concat "; " (List.map string_of_int available_codes)))

  (** Parse entry pool using entry_offsets to locate individual entries *)
  let parse_entry_pool_with_offsets (buffer : Object.Buffer.t)
      (entry_pool_start : int) (entry_offsets : u32 array)
      (abbrev_table : (u64, debug_names_abbrev) Hashtbl.t) :
      name_index_entry array =
    Array.mapi
      (fun i offset ->
        let absolute_offset =
          entry_pool_start + Unsigned.UInt32.to_int offset
        in

        if i < 4 then (
          (* Only try to parse first few entries for debugging *)
          (* Scan forward from the offset to find the first non-null byte *)
          let rec find_entry_start pos max_scan =
            if max_scan <= 0 then pos
            else
              let test_cur = Object.Buffer.cursor buffer ~at:pos in
              let byte = Object.Buffer.Read.u8 test_cur in
              if Unsigned.UInt8.to_int byte = 0 then
                find_entry_start (pos + 1) (max_scan - 1)
              else pos
          in

          let actual_entry_start = find_entry_start absolute_offset 20 in
          if actual_entry_start <> absolute_offset then
            ignore (absolute_offset, actual_entry_start);

          let cur = Object.Buffer.cursor buffer ~at:actual_entry_start in
          parse_single_entry cur abbrev_table)
        else
          {
            (* For now, return dummy entry for other entries *)
            name_offset = Unsigned.UInt32.of_int 0;
            die_offset = Unsigned.UInt32.of_int 0;
            attributes = [];
          })
      entry_offsets

  (** Parse a complete debug_names section *)
  let parse_debug_names_section (cur : Object.Buffer.cursor)
      (buffer : Object.Buffer.t) : debug_names_section =
    let header = parse_name_index_header cur in

    let comp_unit_offsets = parse_u32_array cur header.comp_unit_count in
    let local_type_unit_offsets =
      parse_u32_array cur header.local_type_unit_count
    in
    let foreign_type_unit_signatures =
      parse_u64_array cur header.foreign_type_unit_count
    in
    (* Hash lookup table: bucket_count buckets + name_count hashes *)
    let buckets = parse_u32_array cur header.bucket_count in
    let hash_table = parse_u32_array cur header.name_count in
    (* Name table is just an array of offsets into debug_str *)
    let name_offsets = parse_u32_array cur header.name_count in
    (* Resolve string offsets to actual strings using debug_str section *)
    let resolve_string_offset offset =
      match find_debug_section_by_type buffer Debug_str with
      | Some (str_section_offset, _) -> (
          try
            let str_cursor =
              Object.Buffer.cursor buffer
                ~at:
                  (Unsigned.UInt64.to_int str_section_offset
                  + Unsigned.UInt32.to_int offset)
            in
            match Object.Buffer.Read.zero_string str_cursor () with
            | Some resolved_string -> resolved_string
            | None ->
                Printf.sprintf "<string@0x%08lx>"
                  (Unsigned.UInt32.to_int32 offset)
          with _ ->
            Printf.sprintf "<string@0x%08lx>" (Unsigned.UInt32.to_int32 offset))
      | None ->
          Printf.sprintf "<string@0x%08lx>" (Unsigned.UInt32.to_int32 offset)
    in

    let name_table =
      Array.map
        (fun offset -> { offset; value = resolve_string_offset offset })
        name_offsets
    in

    (* Entry offsets array - points to where each name's entry starts in the entry pool *)
    let entry_offsets = parse_u32_array cur header.name_count in

    (* Parse debug_names abbreviation table *)
    let abbreviation_table =
      parse_debug_names_abbreviation_table cur header.abbrev_table_size
    in

    (* Convert abbreviation_entry list to hashtbl for compatibility with entry pool parsing *)
    let abbrev_table = Hashtbl.create 16 in
    List.iter
      (fun abbrev ->
        let debug_names_abbrev =
          {
            code = abbrev.code;
            tag = abbrev.tag;
            attributes = abbrev.attributes;
          }
        in
        Hashtbl.add abbrev_table abbrev.code debug_names_abbrev)
      abbreviation_table;

    (* At this point, the cursor is positioned right at the start of the entry pool *)
    (* Since we just finished parsing the abbreviation table, cur points to entry pool start *)
    let entry_pool_start = cur.position in

    (* Debug: dump the entire entry pool to see its structure *)
    let entry_pool =
      (* DWARF 5 Figure 6.1: Each entry_offset points to a SERIES of entries for that name *)
      (* Each series is terminated by abbreviation code 0 *)
      let parse_entry_series (start_offset : int) =
        (* Parse a series of entries starting at start_offset until we hit terminator (abbrev code 0) *)
        let rec parse_series_rec (cur_offset : int)
            (acc : name_index_entry list) =
          try
            let cur = Object.Buffer.cursor buffer ~at:cur_offset in
            let abbrev_code_int = Object.Buffer.Read.uleb128 cur in

            if abbrev_code_int = 0 then
              (* Terminator found, return accumulated entries *)
              List.rev acc
            else
              (* Parse this entry and continue *)
              let reset_cur = Object.Buffer.cursor buffer ~at:cur_offset in
              let entry = parse_single_entry reset_cur abbrev_table in
              let next_offset = cur_offset + 6 in
              (* Estimate entry size *)
              parse_series_rec next_offset (entry :: acc)
          with _ ->
            (* Failed to parse, return what we have *)
            List.rev acc
        in
        parse_series_rec start_offset []
      in

      let absolute_series = ref 0 in
      let relative_series = ref 0 in
      let failures = ref 0 in

      let results =
        Array.map
          (fun offset ->
            let entry_offset = Unsigned.UInt32.to_int offset in

            (* Try absolute offset first *)
            let abs_series = parse_entry_series entry_offset in
            if List.length abs_series > 0 then (
              incr absolute_series;
              List.hd abs_series (* Return first entry in series for now *))
            else
              (* Try relative to entry pool start *)
              let pool_relative_offset = entry_pool_start + entry_offset in
              let rel_series = parse_entry_series pool_relative_offset in
              if List.length rel_series > 0 then (
                incr relative_series;
                List.hd rel_series (* Return first entry in series for now *))
              else (
                incr failures;
                {
                  name_offset = Unsigned.UInt32.of_int 0;
                  die_offset = Unsigned.UInt32.of_int 0;
                  attributes = [];
                }))
          entry_offsets
      in

      results
    in

    {
      header;
      comp_unit_offsets;
      local_type_unit_offsets;
      foreign_type_unit_signatures;
      buckets;
      hash_table;
      name_table;
      entry_offsets;
      abbreviation_table;
      entry_pool;
    }

  (** Parse a single entry from the entry pool at the given buffer position.
      Returns None if terminator (abbrev code 0) is found, or Some with entry
      details. *)
  let parse_single_entry_at_cursor (cursor : Object.Buffer.cursor)
      (abbrev_table : debug_names_abbrev list) (current_offset_ref : int ref)
      (entry_pool_relative_offset : int) (entry_offset : int)
      (absolute_entry_offset : int) : entry_parse_result option =
    let entry_start_relative = !current_offset_ref - absolute_entry_offset in

    (* Read abbreviation code *)
    let abbrev_code_int = Object.Buffer.Read.uleb128 cursor in

    (* Check for terminator *)
    if abbrev_code_int = 0 then None (* End of entry series *)
    else
      let abbrev_code = Unsigned.UInt64.of_int abbrev_code_int in

      (* Update current offset (ULEB128 minimum 1 byte) *)
      current_offset_ref := !current_offset_ref + 1;

      (* Find the abbreviation in the table *)
      let matching_abbrev_opt =
        List.find_opt (fun abbrev -> abbrev.code = abbrev_code) abbrev_table
      in

      match matching_abbrev_opt with
      | Some abbrev ->
          (* Parse the attributes according to the abbreviation *)
          let die_offset_ref = ref 0 in
          let parent_offset_ref = ref None in
          let has_parent_flag = ref false in
          let compile_unit_index_ref = ref None in
          let type_unit_index_ref = ref None in
          let type_hash_ref = ref None in
          List.iter
            (fun (attr, form) ->
              match attr with
              | DW_IDX_die_offset -> (
                  match form with
                  | DW_FORM_ref4 ->
                      let value = Object.Buffer.Read.u32 cursor in
                      die_offset_ref := Unsigned.UInt32.to_int value;
                      current_offset_ref := !current_offset_ref + 4
                  | _ -> ())
              | DW_IDX_parent -> (
                  match form with
                  | DW_FORM_flag_present -> has_parent_flag := true
                  | DW_FORM_ref4 ->
                      let value = Object.Buffer.Read.u32 cursor in
                      parent_offset_ref := Some (Unsigned.UInt32.to_int value);
                      current_offset_ref := !current_offset_ref + 4
                  | _ -> ())
              | DW_IDX_compile_unit -> (
                  match form with
                  | DW_FORM_ref4 ->
                      let value = Object.Buffer.Read.u32 cursor in
                      compile_unit_index_ref := Some value;
                      current_offset_ref := !current_offset_ref + 4
                  | _ -> ())
              | DW_IDX_type_unit -> (
                  match form with
                  | DW_FORM_ref4 ->
                      let value = Object.Buffer.Read.u32 cursor in
                      type_unit_index_ref := Some value;
                      current_offset_ref := !current_offset_ref + 4
                  | _ -> ())
              | DW_IDX_type_hash -> (
                  match form with
                  | DW_FORM_data8 ->
                      let value = Object.Buffer.Read.u64 cursor in
                      type_hash_ref := Some value;
                      current_offset_ref := !current_offset_ref + 8
                  | _ -> ())
              | DW_IDX_null -> ()
              | DW_IDX_lo_user | DW_IDX_hi_user -> (
                  (* Skip user-defined attributes *)
                  match form with
                  | DW_FORM_ref4 ->
                      let _ = Object.Buffer.Read.u32 cursor in
                      current_offset_ref := !current_offset_ref + 4
                  | DW_FORM_flag_present -> ()
                  | DW_FORM_data8 ->
                      let _ = Object.Buffer.Read.u64 cursor in
                      current_offset_ref := !current_offset_ref + 8
                  | _ -> ()))
            abbrev.attributes;

          let tag_code = uint64_of_abbreviation_tag abbrev.tag in
          let tag_str = string_of_abbreviation_tag tag_code in
          let abbrev_id =
            Printf.sprintf "0x%x" (Unsigned.UInt64.to_int abbrev.code)
          in

          let entry_addr =
            entry_pool_relative_offset + entry_offset + entry_start_relative
          in
          Some
            {
              name_offset = Unsigned.UInt32.of_int entry_addr;
              die_offset = Unsigned.UInt32.of_int !die_offset_ref;
              tag_name = tag_str;
              offset_hex = abbrev_id;
              unit_index = !parent_offset_ref;
              is_declaration = !has_parent_flag;
              compile_unit_index = !compile_unit_index_ref;
              type_unit_index = !type_unit_index_ref;
              type_hash = !type_hash_ref;
            }
      | None ->
          (* Abbreviation not found *)
          let entry_addr =
            entry_pool_relative_offset + entry_offset + entry_start_relative
          in
          Some
            {
              name_offset = Unsigned.UInt32.of_int entry_addr;
              die_offset = Unsigned.UInt32.of_int 0;
              tag_name = "DW_TAG_<unknown>";
              offset_hex = "0x0";
              unit_index = None;
              is_declaration = false;
              compile_unit_index = None;
              type_unit_index = None;
              type_hash = None;
            }

  (** Parse all entries in a series until terminator (abbrev code 0) is found.
      Returns list of parsed entries. *)
  let parse_entry_series (buffer : Object.Buffer.t)
      (absolute_entry_offset : int) (abbrev_table : debug_names_abbrev list)
      (entry_pool_relative_offset : int) (entry_offset : int) :
      entry_parse_result list =
    let current_offset_ref = ref absolute_entry_offset in

    let rec parse_series_rec acc =
      let cursor = Object.Buffer.cursor buffer ~at:!current_offset_ref in
      match
        parse_single_entry_at_cursor cursor abbrev_table current_offset_ref
          entry_pool_relative_offset entry_offset absolute_entry_offset
      with
      | None -> List.rev acc (* Terminator found *)
      | Some entry -> parse_series_rec (entry :: acc)
    in

    try parse_series_rec []
    with _ ->
      [
        {
          name_offset = Unsigned.UInt32.of_int 0;
          die_offset = Unsigned.UInt32.of_int 0;
          tag_name = "DW_TAG_<parse error>";
          offset_hex = "0x0";
          unit_index = None;
          is_declaration = false;
          compile_unit_index = None;
          type_unit_index = None;
          type_hash = None;
        };
      ]

  (** Calculate entry pool offset based on header information *)
  let calculate_entry_pool_offset (header : name_index_header) : int =
    let header_size = header.span in
    let cu_offsets_size = Unsigned.UInt32.to_int header.comp_unit_count * 4 in
    let tu_offsets_size =
      Unsigned.UInt32.to_int header.local_type_unit_count * 4
    in
    let foreign_tu_size =
      Unsigned.UInt32.to_int header.foreign_type_unit_count * 8
    in
    let bucket_size = Unsigned.UInt32.to_int header.bucket_count * 4 in
    let hash_table_size = Unsigned.UInt32.to_int header.name_count * 4 in
    let name_table_size = Unsigned.UInt32.to_int header.name_count * 4 in
    let entry_offsets_size = Unsigned.UInt32.to_int header.name_count * 4 in
    let abbrev_table_size = Unsigned.UInt32.to_int header.abbrev_table_size in

    header_size + cu_offsets_size + tu_offsets_size + foreign_tu_size
    + bucket_size + hash_table_size + name_table_size + entry_offsets_size
    + abbrev_table_size

  (** Parse all entries for a given name index according to DWARF 5
      specification *)
  let parse_all_entries_for_name (buffer : Object.Buffer.t)
      (debug_names : debug_names_section) (section_offset : int)
      (name_idx : int) : entry_parse_result list =
    try
      if name_idx < Array.length debug_names.entry_offsets then
        let entry_offset =
          Unsigned.UInt32.to_int debug_names.entry_offsets.(name_idx)
        in
        let entry_pool_relative_offset =
          calculate_entry_pool_offset debug_names.header
        in
        let absolute_entry_offset =
          section_offset + entry_pool_relative_offset + entry_offset
        in

        parse_entry_series buffer absolute_entry_offset
          debug_names.abbreviation_table entry_pool_relative_offset entry_offset
      else
        [
          {
            name_offset = Unsigned.UInt32.of_int 0;
            die_offset = Unsigned.UInt32.of_int 0;
            tag_name = "DW_TAG_<no entry>";
            offset_hex = "0x0";
            unit_index = None;
            is_declaration = false;
            compile_unit_index = None;
            type_unit_index = None;
            type_hash = None;
          };
        ]
    with _ ->
      [
        {
          name_offset = Unsigned.UInt32.of_int 0;
          die_offset = Unsigned.UInt32.of_int 0;
          tag_name = "DW_TAG_<parse error>";
          offset_hex = "0x0";
          unit_index = None;
          is_declaration = false;
          compile_unit_index = None;
          type_unit_index = None;
          type_hash = None;
        };
      ]

  (** Find bucket index for a given name using DJB2 hash algorithm *)
  let find_bucket_index (name : string) (bucket_count : int) : int =
    let hash = djb2_hash name in
    Unsigned.UInt32.to_int hash mod bucket_count

  (** Get all name indices for a given bucket *)
  let get_name_indices_for_bucket (debug_names : debug_names_section)
      (bucket_index : int) : int list =
    if bucket_index >= Array.length debug_names.buckets then []
    else
      let bucket_entry = debug_names.buckets.(bucket_index) in
      let bucket_start = Unsigned.UInt32.to_int bucket_entry in
      if bucket_start = 0 then [] (* Empty bucket *)
      else
        (* Collect all names that hash to this bucket *)
        let rec collect_names acc name_idx =
          if name_idx >= Array.length debug_names.hash_table then acc
          else
            let hash = debug_names.hash_table.(name_idx) in
            let computed_bucket =
              Unsigned.UInt32.to_int hash mod Array.length debug_names.buckets
            in
            if computed_bucket = bucket_index then
              collect_names (name_idx :: acc) (name_idx + 1)
            else collect_names acc (name_idx + 1)
        in
        List.rev (collect_names [] 0)

  (** Find all name indices that match a given name exactly *)
  let find_name_indices (debug_names : debug_names_section) (name : string) :
      int list =
    let bucket_index =
      find_bucket_index name (Array.length debug_names.buckets)
    in
    let name_indices = get_name_indices_for_bucket debug_names bucket_index in
    List.filter
      (fun name_idx ->
        if name_idx < Array.length debug_names.name_table then
          let name_entry = debug_names.name_table.(name_idx) in
          String.equal name_entry.value name
        else false)
      name_indices

  (** Find all entries (DIEs) that match a given name *)
  let find_entries_by_name (buffer : Object.Buffer.t)
      (debug_names : debug_names_section) (section_offset : int) (name : string)
      : entry_parse_result list =
    let name_indices = find_name_indices debug_names name in
    List.fold_left
      (fun acc name_idx ->
        let entries =
          parse_all_entries_for_name buffer debug_names section_offset name_idx
        in
        entries @ acc)
      [] name_indices

  (** Find all symbols (any kind of DIE) matching a name *)
  let lookup_symbols_by_name (buffer : Object.Buffer.t)
      (debug_names : debug_names_section) (section_offset : int) (name : string)
      : entry_parse_result list =
    find_entries_by_name buffer debug_names section_offset name

  (** Check if a string contains a substring *)
  let string_contains_substring str substring =
    try
      let _ = Str.search_forward (Str.regexp_string substring) str 0 in
      true
    with Not_found -> false

  (** Find specifically function DIEs by name *)
  let find_functions_by_name (buffer : Object.Buffer.t)
      (debug_names : debug_names_section) (section_offset : int) (name : string)
      : entry_parse_result list =
    let all_entries =
      find_entries_by_name buffer debug_names section_offset name
    in
    List.filter
      (fun entry ->
        string_contains_substring entry.tag_name "subprogram"
        || string_contains_substring entry.tag_name "function")
      all_entries

  (** Find specifically type DIEs by name *)
  let find_types_by_name (buffer : Object.Buffer.t)
      (debug_names : debug_names_section) (section_offset : int) (name : string)
      : entry_parse_result list =
    let all_entries =
      find_entries_by_name buffer debug_names section_offset name
    in
    List.filter
      (fun entry ->
        string_contains_substring entry.tag_name "structure_type"
        || string_contains_substring entry.tag_name "class_type"
        || string_contains_substring entry.tag_name "union_type"
        || string_contains_substring entry.tag_name "enumeration_type"
        || string_contains_substring entry.tag_name "typedef"
        || string_contains_substring entry.tag_name "base_type")
      all_entries

  (** Get all symbol names available in the debug_names section *)
  let get_all_symbol_names (debug_names : debug_names_section) : string list =
    Array.to_list debug_names.name_table |> List.map (fun entry -> entry.value)

  (** Find names matching a prefix *)
  let search_names_with_prefix (debug_names : debug_names_section)
      (prefix : string) : string list =
    Array.to_list debug_names.name_table
    |> List.map (fun entry -> entry.value)
    |> List.filter (fun name ->
           String.length name >= String.length prefix
           && String.sub name 0 (String.length prefix) = prefix)

  (** Filter entries by abbreviation tag *)
  let filter_entries_by_tag (tag : abbreviation_tag)
      (entries : entry_parse_result list) : entry_parse_result list =
    let target_tag_str = string_of_abbreviation_tag_direct tag in
    List.filter (fun entry -> entry.tag_name = target_tag_str) entries

  (** Filter entries by multiple tags *)
  let filter_entries_by_tags (tags : abbreviation_tag list)
      (entries : entry_parse_result list) : entry_parse_result list =
    let target_tag_strs =
      List.map string_of_abbreviation_tag_direct tags
      |> List.fold_left (fun acc tag -> tag :: acc) []
    in
    List.filter (fun entry -> List.mem entry.tag_name target_tag_strs) entries

  (** Find variables by name (excludes functions and types) *)
  let find_variables_by_name (buffer : Object.Buffer.t)
      (debug_names : debug_names_section) (section_offset : int) (name : string)
      : entry_parse_result list =
    let all_entries =
      find_entries_by_name buffer debug_names section_offset name
    in
    List.filter
      (fun entry ->
        string_contains_substring entry.tag_name "variable"
        || string_contains_substring entry.tag_name "formal_parameter"
        || string_contains_substring entry.tag_name "constant")
      all_entries

  (** Find namespaces or modules by name *)
  let find_namespaces_by_name (buffer : Object.Buffer.t)
      (debug_names : debug_names_section) (section_offset : int) (name : string)
      : entry_parse_result list =
    let all_entries =
      find_entries_by_name buffer debug_names section_offset name
    in
    List.filter
      (fun entry ->
        string_contains_substring entry.tag_name "namespace"
        || string_contains_substring entry.tag_name "module")
      all_entries

  (** Search entries with regex pattern matching on names *)
  let search_entries_with_pattern (buffer : Object.Buffer.t)
      (debug_names : debug_names_section) (section_offset : int)
      (pattern : string) : entry_parse_result list =
    let all_names = get_all_symbol_names debug_names in
    let regex = Str.regexp pattern in
    let matching_names =
      List.filter
        (fun name ->
          try
            let _ = Str.search_forward regex name 0 in
            true
          with Not_found -> false)
        all_names
    in
    List.fold_left
      (fun acc name ->
        let entries =
          find_entries_by_name buffer debug_names section_offset name
        in
        entries @ acc)
      [] matching_names

  (** Find entries within a specific compilation unit *)
  let find_entries_in_compilation_unit (entries : entry_parse_result list)
      (cu_index : u32) : entry_parse_result list =
    List.filter
      (fun entry ->
        match entry.compile_unit_index with
        | Some index -> Unsigned.UInt32.equal index cu_index
        | None -> false)
      entries

  (** Find entries within a specific type unit *)
  let find_entries_in_type_unit (entries : entry_parse_result list)
      (tu_index : u32) : entry_parse_result list =
    List.filter
      (fun entry ->
        match entry.type_unit_index with
        | Some index -> Unsigned.UInt32.equal index tu_index
        | None -> false)
      entries

  (** Find entries with a specific type hash *)
  let find_entries_with_type_hash (entries : entry_parse_result list)
      (type_hash : u64) : entry_parse_result list =
    List.filter
      (fun entry ->
        match entry.type_hash with
        | Some hash -> Unsigned.UInt64.equal hash type_hash
        | None -> false)
      entries

  (** Find children of a given entry using parent offset relationships *)
  let find_children_entries (buffer : Object.Buffer.t)
      (debug_names : debug_names_section) (section_offset : int)
      (parent_entry : entry_parse_result) : entry_parse_result list =
    let all_names = get_all_symbol_names debug_names in
    let all_entries =
      List.fold_left
        (fun acc name ->
          let entries =
            find_entries_by_name buffer debug_names section_offset name
          in
          entries @ acc)
        [] all_names
    in
    let parent_offset = Unsigned.UInt32.to_int parent_entry.name_offset in
    List.filter
      (fun entry ->
        match entry.unit_index with
        | Some offset -> offset = parent_offset
        | None -> false)
      all_entries

  (** Find all entries that belong to the same compilation unit as a given entry
  *)
  let find_sibling_entries (entries : entry_parse_result list)
      (target_entry : entry_parse_result) : entry_parse_result list =
    match target_entry.compile_unit_index with
    | Some cu_index ->
        List.filter
          (fun entry ->
            match entry.compile_unit_index with
            | Some index ->
                Unsigned.UInt32.equal index cu_index && entry != target_entry
            | None -> false)
          entries
    | None -> []

  (** Group entries by their compilation unit *)
  let group_entries_by_compilation_unit (entries : entry_parse_result list) :
      (u32 * entry_parse_result list) list =
    let cu_groups = Hashtbl.create 16 in
    List.iter
      (fun entry ->
        match entry.compile_unit_index with
        | Some cu_index ->
            let existing =
              try Hashtbl.find cu_groups cu_index with Not_found -> []
            in
            Hashtbl.replace cu_groups cu_index (entry :: existing)
        | None -> ())
      entries;
    Hashtbl.fold
      (fun cu_index entries acc -> (cu_index, List.rev entries) :: acc)
      cu_groups []

  (** Group entries by their type unit *)
  let group_entries_by_type_unit (entries : entry_parse_result list) :
      (u32 * entry_parse_result list) list =
    let tu_groups = Hashtbl.create 16 in
    List.iter
      (fun entry ->
        match entry.type_unit_index with
        | Some tu_index ->
            let existing =
              try Hashtbl.find tu_groups tu_index with Not_found -> []
            in
            Hashtbl.replace tu_groups tu_index (entry :: existing)
        | None -> ())
      entries;
    Hashtbl.fold
      (fun tu_index entries acc -> (tu_index, List.rev entries) :: acc)
      tu_groups []

  (** Find entries with the same type hash (for type deduplication) *)
  let group_entries_by_type_hash (entries : entry_parse_result list) :
      (u64 * entry_parse_result list) list =
    let hash_groups = Hashtbl.create 16 in
    List.iter
      (fun entry ->
        match entry.type_hash with
        | Some type_hash ->
            let existing =
              try Hashtbl.find hash_groups type_hash with Not_found -> []
            in
            Hashtbl.replace hash_groups type_hash (entry :: existing)
        | None -> ())
      entries;
    Hashtbl.fold
      (fun type_hash entries acc -> (type_hash, List.rev entries) :: acc)
      hash_groups []

  type entry_tree = { entry : entry_parse_result; children : entry_tree list }
  (** Build a hierarchical tree structure from entries using parent
      relationships *)

  let build_entry_hierarchy (buffer : Object.Buffer.t)
      (debug_names : debug_names_section) (section_offset : int)
      (root_entries : entry_parse_result list) : entry_tree list =
    let rec build_tree entry =
      let children_entries =
        find_children_entries buffer debug_names section_offset entry
      in
      let children_trees = List.map build_tree children_entries in
      { entry; children = children_trees }
    in
    List.map build_tree root_entries

  (** Find root entries (entries with no parent) in a compilation unit *)
  let find_root_entries_in_compilation_unit (entries : entry_parse_result list)
      (cu_index : u32) : entry_parse_result list =
    List.filter
      (fun entry ->
        match entry.compile_unit_index with
        | Some index when Unsigned.UInt32.equal index cu_index ->
            entry.unit_index = None (* No parent offset means it's a root *)
        | _ -> false)
      entries

  (** Find the compilation unit index that contains a specific DIE offset *)
  let find_compilation_unit_for_die (debug_names : debug_names_section)
      (die_offset : u32) : u32 option =
    let die_offset_int = Unsigned.UInt32.to_int die_offset in
    let rec search_cu_array index =
      if index >= Array.length debug_names.comp_unit_offsets then None
      else
        let cu_offset =
          Unsigned.UInt32.to_int debug_names.comp_unit_offsets.(index)
        in
        if die_offset_int >= cu_offset then
          (* Check if this is the last CU or if die_offset is before next CU *)
          if index + 1 >= Array.length debug_names.comp_unit_offsets then
            Some (Unsigned.UInt32.of_int index)
          else
            let next_cu_offset =
              Unsigned.UInt32.to_int debug_names.comp_unit_offsets.(index + 1)
            in
            if die_offset_int < next_cu_offset then
              Some (Unsigned.UInt32.of_int index)
            else search_cu_array (index + 1)
        else search_cu_array (index + 1)
    in
    search_cu_array 0

  (** Get the compilation unit offset for a given index *)
  let get_compilation_unit_offset (debug_names : debug_names_section)
      (cu_index : u32) : u32 option =
    let index = Unsigned.UInt32.to_int cu_index in
    if index >= 0 && index < Array.length debug_names.comp_unit_offsets then
      Some debug_names.comp_unit_offsets.(index)
    else None

  (** Get all compilation unit offsets from debug_names section *)
  let get_all_compilation_unit_offsets (debug_names : debug_names_section) :
      u32 array =
    debug_names.comp_unit_offsets
end

let get_abbrev_table t (offset : size_t) =
  let a = Hashtbl.find_opt t.abbrev_tables_ offset in
  match a with
  | Some a -> (t, a)
  | None ->
      let a =
        parse_abbrev_table t.object_
          (Unsigned.UInt32.of_int64 (Unsigned.UInt64.to_int64 offset))
      in
      Hashtbl.add t.abbrev_tables_ offset a;
      (t, a)

let create buffer =
  let format = Object_format.detect_format buffer in
  let object_ = Object_file.{ buffer; format } in
  { abbrev_tables_ = Hashtbl.create 10; compile_units_ = [||]; object_ }

(* TODO Change the type of t.compile_units_ *)
let get_compile_units t =
  let compile_units = parse_compile_units t |> List.of_seq |> Array.of_list in
  { t with compile_units_ = compile_units }

(** String Offset Tables (.debug_str_offsets section) - DWARF 5 Section 7.26 *)
module DebugStrOffsets = struct
  type header = {
    format : dwarf_format;
    unit_length : u64;
    version : u16;
    padding : u16;
    header_span : span;
  }

  type offset_entry = { offset : u64; resolved_string : string option }
  type t = { header : header; offsets : offset_entry array }

  let parse_header (cursor : Object.Buffer.cursor) : header =
    let start_pos = cursor.position in
    let format, unit_length = parse_initial_length cursor in
    let version = Object.Buffer.Read.u16 cursor in
    let padding = Object.Buffer.Read.u16 cursor in
    let end_pos = cursor.position in

    (* Calculate header span *)
    let header_span =
      {
        start = Unsigned.UInt64.of_int start_pos;
        size = Unsigned.UInt64.of_int (end_pos - start_pos);
      }
    in

    (* Validate DWARF version 5 *)
    if Unsigned.UInt16.to_int version != 5 then
      failwith
        (Printf.sprintf "Expected DWARF version 5, got %d"
           (Unsigned.UInt16.to_int version));

    { format; unit_length; version; padding; header_span }

  let parse_offsets (cursor : Object.Buffer.cursor) (header : header)
      (debug_str_section : (u32 * u64) option) (buffer : Object.Buffer.t) :
      offset_entry array =
    let _header_size = Unsigned.UInt64.to_int header.header_span.size in
    let offset_size = offset_size_for_format header.format in
    let data_size = Unsigned.UInt64.to_int header.unit_length - 4 in
    (* unit_length excludes itself (version+padding) *)
    let num_offsets = data_size / offset_size in

    Array.init num_offsets (fun _i ->
        let offset = read_offset_for_format header.format cursor in
        let resolved_string =
          match debug_str_section with
          | Some (str_section_offset, _) -> (
              try
                let str_cursor =
                  Object.Buffer.cursor buffer
                    ~at:
                      (Unsigned.UInt32.to_int str_section_offset
                      + Unsigned.UInt64.to_int offset)
                in
                Object.Buffer.Read.zero_string str_cursor ()
              with _ -> None)
          | None -> None
        in
        { offset; resolved_string })

  let parse (buffer : Object.Buffer.t) (section_offset : u32) : t =
    let cursor =
      Object.Buffer.cursor buffer ~at:(Unsigned.UInt32.to_int section_offset)
    in
    let header = parse_header cursor in

    (* Find debug_str section for string resolution *)
    let debug_str_section =
      match find_debug_section_by_type buffer Debug_str with
      | Some (offset, size) ->
          Some (Unsigned.UInt32.of_int (Unsigned.UInt64.to_int offset), size)
      | None -> None
    in

    let offsets = parse_offsets cursor header debug_str_section buffer in
    { header; offsets }
end

(** Debug String Tables (.debug_str section) - DWARF 5 Section 7.26 *)
module DebugStr = struct
  type string_entry = {
    offset : int;  (** Offset from start of .debug_str section *)
    length : int;  (** Length of the string in bytes *)
    content : string;  (** The actual string content *)
  }

  type t = {
    entries : string_entry array;  (** All strings in the section *)
    total_size : int;  (** Total size of the section in bytes *)
  }

  let parse buffer : t option =
    match find_debug_section_by_type buffer Debug_str with
    | None -> None
    | Some (section_offset, section_size) -> (
        try
          let section_start = Unsigned.UInt64.to_int section_offset in
          let section_end =
            section_start + Unsigned.UInt64.to_int section_size
          in
          let cursor = Object.Buffer.cursor buffer ~at:section_start in

          (* Single pass: collect strings using list accumulation *)
          let rec collect_strings acc current_pos string_offset =
            if current_pos >= section_end then List.rev acc
            else
              match Object.Buffer.Read.zero_string cursor () with
              | Some str ->
                  let str_len = String.length str in
                  let entry =
                    { offset = string_offset; length = str_len; content = str }
                  in
                  let next_pos = current_pos + str_len + 1 in
                  (* +1 for null terminator *)
                  let next_offset = string_offset + str_len + 1 in
                  collect_strings (entry :: acc) next_pos next_offset
              | None -> List.rev acc (* Break on read error *)
          in

          let string_list = collect_strings [] section_start 0 in
          let entries = Array.of_list string_list in

          Some { entries; total_size = Unsigned.UInt64.to_int section_size }
        with _ -> None)
end

(** Debug Line String Tables (.debug_line_str section) - DWARF 5 Section 7.26 *)
module DebugLineStr = struct
  type string_entry = {
    offset : int;  (** Offset from start of .debug_line_str section *)
    length : int;  (** Length of the string in bytes *)
    content : string;  (** The actual string content *)
  }

  type t = {
    entries : string_entry array;  (** All strings in the section *)
    total_size : int;  (** Total size of the section in bytes *)
  }

  let parse buffer : t option =
    match find_debug_section_by_type buffer Debug_line_str with
    | None -> None
    | Some (section_offset, section_size) -> (
        try
          let section_start = Unsigned.UInt64.to_int section_offset in
          let section_end =
            section_start + Unsigned.UInt64.to_int section_size
          in
          let cursor = Object.Buffer.cursor buffer ~at:section_start in

          (* Single pass: collect strings using list accumulation *)
          let rec collect_strings acc current_pos string_offset =
            if current_pos >= section_end then List.rev acc
            else
              match Object.Buffer.Read.zero_string cursor () with
              | Some str ->
                  let str_len = String.length str in
                  let entry =
                    { offset = string_offset; length = str_len; content = str }
                  in
                  let next_pos = current_pos + str_len + 1 in
                  (* +1 for null terminator *)
                  let next_offset = string_offset + str_len + 1 in
                  collect_strings (entry :: acc) next_pos next_offset
              | None -> List.rev acc (* Break on read error *)
          in

          let string_list = collect_strings [] section_start 0 in
          let entries = Array.of_list string_list in

          Some { entries; total_size = Unsigned.UInt64.to_int section_size }
        with exn ->
          Printf.eprintf "Error parsing debug_line_str section: %s\n"
            (Printexc.to_string exn);
          None)

  let iter f debug_line_str = Array.iter f debug_line_str.entries

  let find_string_at_offset debug_line_str offset =
    Array.find_map
      (fun entry -> if entry.offset = offset then Some entry.content else None)
      debug_line_str.entries
end

(** Address Tables (.debug_addr section) - DWARF 5 Section 7.27 *)
module DebugAddr = struct
  type header = {
    format : dwarf_format;
    unit_length : u64;
    version : u16;
    address_size : u8;
    segment_selector_size : u8;
    span : span;  (** Header location and size information *)
  }

  type entry = {
    segment : u64 option; (* Present only if segment_selector_size > 0 *)
    address : u64;
  }

  type t = { header : header; entries : entry array }

  let parse_header cursor =
    let start_pos = cursor.position in
    let format, unit_length = parse_initial_length cursor in
    let version = Object.Buffer.Read.u16 cursor in
    let address_size = Object.Buffer.Read.u8 cursor in
    let segment_selector_size = Object.Buffer.Read.u8 cursor in
    let end_pos = cursor.position in
    let span =
      {
        start = Unsigned.UInt64.of_int start_pos;
        size = Unsigned.UInt64.of_int (end_pos - start_pos);
      }
    in
    { format; unit_length; version; address_size; segment_selector_size; span }

  let parse_entries cursor header =
    (* Calculate number of entries from unit_length *)
    (* unit_length includes everything after the length field itself *)
    (* The header size excludes the unit_length field (4 or 12 bytes) *)
    let length_field_size =
      match header.format with DWARF32 -> 4 | DWARF64 -> 12
    in
    let header_size_excluding_length =
      Unsigned.UInt64.to_int header.span.size - length_field_size
    in
    let remaining_length =
      Unsigned.UInt64.to_int header.unit_length - header_size_excluding_length
    in
    let entry_size =
      (if Unsigned.UInt8.to_int header.segment_selector_size > 0 then
         Unsigned.UInt8.to_int header.segment_selector_size
       else 0)
      + Unsigned.UInt8.to_int header.address_size
    in
    let num_entries = remaining_length / entry_size in

    Array.init num_entries (fun _i ->
        let segment =
          if Unsigned.UInt8.to_int header.segment_selector_size > 0 then
            (* Read segment selector based on its size *)
            match Unsigned.UInt8.to_int header.segment_selector_size with
            | 1 ->
                Some
                  (Unsigned.UInt64.of_int
                     (Unsigned.UInt8.to_int (Object.Buffer.Read.u8 cursor)))
            | 2 ->
                Some
                  (Unsigned.UInt64.of_int
                     (Unsigned.UInt16.to_int (Object.Buffer.Read.u16 cursor)))
            | 4 ->
                Some (Unsigned.UInt64.of_uint32 (Object.Buffer.Read.u32 cursor))
            | 8 -> Some (Object.Buffer.Read.u64 cursor)
            | _ -> failwith "Invalid segment_selector_size"
          else None
        in
        let address =
          (* Read address based on address_size *)
          match Unsigned.UInt8.to_int header.address_size with
          | 1 ->
              Unsigned.UInt64.of_int
                (Unsigned.UInt8.to_int (Object.Buffer.Read.u8 cursor))
          | 2 ->
              Unsigned.UInt64.of_int
                (Unsigned.UInt16.to_int (Object.Buffer.Read.u16 cursor))
          | 4 -> Unsigned.UInt64.of_uint32 (Object.Buffer.Read.u32 cursor)
          | 8 -> Object.Buffer.Read.u64 cursor
          | _ -> failwith "Invalid address_size"
        in
        { segment; address })

  let parse buffer section_offset =
    let cursor =
      Object.Buffer.cursor buffer ~at:(Unsigned.UInt64.to_int section_offset)
    in
    let header = parse_header cursor in
    let entries = parse_entries cursor header in
    { header; entries }
end

(** Address Range Tables (.debug_aranges section) - DWARF 5 Section 6.1.2 *)
module DebugAranges = struct
  type header = {
    format : dwarf_format;
    unit_length : u64;
    version : u16;
    debug_info_offset : u64;
    address_size : u8;
    segment_size : u8;
    header_span : span;
  }

  type address_range = { start_address : u64; length : u64 }
  type aranges_set = { header : header; ranges : address_range list }

  let parse_header cursor =
    let start_pos = cursor.position in
    let format, unit_length = parse_initial_length cursor in
    let version = Object.Buffer.Read.u16 cursor in
    let debug_info_offset = read_offset_for_format format cursor in
    let address_size = Object.Buffer.Read.u8 cursor in
    let segment_size = Object.Buffer.Read.u8 cursor in
    let end_pos = cursor.position in

    (* Calculate header span *)
    let header_span =
      {
        start = Unsigned.UInt64.of_int start_pos;
        size = Unsigned.UInt64.of_int (end_pos - start_pos);
      }
    in

    {
      format;
      unit_length;
      version;
      debug_info_offset;
      address_size;
      segment_size;
      header_span;
    }

  let parse_ranges cursor header =
    let address_size = Unsigned.UInt8.to_int header.address_size in
    let segment_size = Unsigned.UInt8.to_int header.segment_size in

    (* DWARF spec requires alignment to 2*address_size after the header.
       The header size is tracked in header_span. For proper alignment with 8-byte addresses (16-byte alignment),
       we observe that 4 bytes of padding are needed in practice. *)
    for _i = 0 to 3 do
      ignore (Object.Buffer.Read.u8 cursor)
    done;

    let rec read_ranges acc =
      (* Read address and length based on address_size *)
      let start_address =
        match address_size with
        | 4 -> Unsigned.UInt64.of_uint32 (Object.Buffer.Read.u32 cursor)
        | 8 -> Object.Buffer.Read.u64 cursor
        | _ ->
            failwith ("Unsupported address size: " ^ string_of_int address_size)
      in

      let length =
        match address_size with
        | 4 -> Unsigned.UInt64.of_uint32 (Object.Buffer.Read.u32 cursor)
        | 8 -> Object.Buffer.Read.u64 cursor
        | _ ->
            failwith ("Unsupported address size: " ^ string_of_int address_size)
      in

      (* Skip segment selector if present *)
      if segment_size > 0 then
        for _i = 0 to segment_size - 1 do
          ignore (Object.Buffer.Read.u8 cursor)
        done;

      (* Check for terminating null entry *)
      if
        Unsigned.UInt64.equal start_address Unsigned.UInt64.zero
        && Unsigned.UInt64.equal length Unsigned.UInt64.zero
      then List.rev acc
      else
        let range = { start_address; length } in
        read_ranges (range :: acc)
    in
    read_ranges []

  let parse buffer : aranges_set option =
    match find_debug_section_by_type buffer Debug_aranges with
    | None -> None
    | Some (section_offset, _section_size) -> (
        try
          let cursor =
            Object.Buffer.cursor buffer
              ~at:(Unsigned.UInt64.to_int section_offset)
          in
          let header = parse_header cursor in
          (* For ELF files, debug_info_offset points to the compilation unit header start.
             The cu_die_offset displayed should be the offset where the actual DIE starts.
             Calculate this as: base offset + header size *)
          let cu_die_offset =
            if
              Unsigned.UInt64.equal header.debug_info_offset
                (Unsigned.UInt64.of_int 0)
            then
              (* If debug_info_offset is 0 (start of .debug_info), calculate DIE offset using proper header parsing *)
              match find_debug_section_by_type buffer Debug_info with
              | None -> header.debug_info_offset
              | Some (debug_info_section_offset, _) -> (
                  try
                    let cursor =
                      Object.Buffer.cursor buffer
                        ~at:(Unsigned.UInt64.to_int debug_info_section_offset)
                    in
                    let _span, cu_header = parse_compile_unit_header cursor in
                    (* DIE starts after the header, use the header_span from the CompileUnit.header *)
                    (* This excludes the unit_length field which is what we want *)
                    let die_offset_from_section_start =
                      Unsigned.UInt64.to_int cu_header.header_span.size
                    in
                    Unsigned.UInt64.of_int die_offset_from_section_start
                  with _ -> header.debug_info_offset)
            else
              (* For non-zero offsets, assume it already points to the DIE *)
              header.debug_info_offset
          in
          let display_header =
            { header with debug_info_offset = cu_die_offset }
          in
          let ranges = parse_ranges cursor header in
          Some { header = display_header; ranges }
        with exn ->
          Printf.eprintf "Error parsing debug_aranges section: %s\n"
            (Printexc.to_string exn);
          None)
end

(** Location Lists (.debug_loclists section) - DWARF 5 Section 7.7.3 *)
module DebugLoclists = struct
  type header = {
    format : dwarf_format;
    unit_length : u64;
    version : u16;
    address_size : u8;
    segment_size : u8;
    offset_entry_count : u32;
  }

  type location_entry =
    | LLE_end_of_list
    | LLE_base_addressx of { index : int }
    | LLE_startx_endx of { start_index : int; end_index : int; expr : string }
    | LLE_startx_length of { start_index : int; length : u64; expr : string }
    | LLE_offset_pair of { start_offset : u64; end_offset : u64; expr : string }
    | LLE_default_location of { expr : string }
    | LLE_base_address of { address : u64 }
    | LLE_start_end of { start_addr : u64; end_addr : u64; expr : string }
    | LLE_start_length of { start_addr : u64; length : u64; expr : string }

  type location_list = { entries : location_entry list }
  type loclists_section = { header : header; offset_table : u64 array }

  let parse_header cursor =
    let format, unit_length = parse_initial_length cursor in
    let version = Object.Buffer.Read.u16 cursor in
    let address_size = Object.Buffer.Read.u8 cursor in
    let segment_size = Object.Buffer.Read.u8 cursor in
    let offset_entry_count = Object.Buffer.Read.u32 cursor in
    {
      format;
      unit_length;
      version;
      address_size;
      segment_size;
      offset_entry_count;
    }

  let parse_offset_table cursor format offset_entry_count =
    let count = Unsigned.UInt32.to_int offset_entry_count in
    Array.init count (fun _i -> read_offset_for_format format cursor)

  let read_addr cursor address_size =
    let addr_sz = Unsigned.UInt8.to_int address_size in
    if addr_sz = 4 then
      Unsigned.UInt64.of_int
        (Unsigned.UInt32.to_int (Object.Buffer.Read.u32 cursor))
    else Object.Buffer.Read.u64 cursor

  let read_expr cursor =
    let len = Object.Buffer.Read.uleb128 cursor in
    Object.Buffer.Read.fixed_string cursor len

  let parse_location_list cursor address_size =
    let rec read_entries acc =
      let kind = Unsigned.UInt8.to_int (Object.Buffer.Read.u8 cursor) in
      match kind with
      | 0x00 -> List.rev (LLE_end_of_list :: acc)
      | 0x01 ->
          let index = Object.Buffer.Read.uleb128 cursor in
          read_entries (LLE_base_addressx { index } :: acc)
      | 0x02 ->
          let start_index = Object.Buffer.Read.uleb128 cursor in
          let end_index = Object.Buffer.Read.uleb128 cursor in
          let expr = read_expr cursor in
          read_entries (LLE_startx_endx { start_index; end_index; expr } :: acc)
      | 0x03 ->
          let start_index = Object.Buffer.Read.uleb128 cursor in
          let length =
            Unsigned.UInt64.of_int (Object.Buffer.Read.uleb128 cursor)
          in
          let expr = read_expr cursor in
          read_entries (LLE_startx_length { start_index; length; expr } :: acc)
      | 0x04 ->
          let start_offset =
            Unsigned.UInt64.of_int (Object.Buffer.Read.uleb128 cursor)
          in
          let end_offset =
            Unsigned.UInt64.of_int (Object.Buffer.Read.uleb128 cursor)
          in
          let expr = read_expr cursor in
          read_entries
            (LLE_offset_pair { start_offset; end_offset; expr } :: acc)
      | 0x05 ->
          let expr = read_expr cursor in
          read_entries (LLE_default_location { expr } :: acc)
      | 0x06 ->
          let address = read_addr cursor address_size in
          read_entries (LLE_base_address { address } :: acc)
      | 0x07 ->
          let start_addr = read_addr cursor address_size in
          let end_addr = read_addr cursor address_size in
          let expr = read_expr cursor in
          read_entries (LLE_start_end { start_addr; end_addr; expr } :: acc)
      | 0x08 ->
          let start_addr = read_addr cursor address_size in
          let length =
            Unsigned.UInt64.of_int (Object.Buffer.Read.uleb128 cursor)
          in
          let expr = read_expr cursor in
          read_entries (LLE_start_length { start_addr; length; expr } :: acc)
      | n -> failwith (Printf.sprintf "Unknown DW_LLE entry kind: 0x%02x" n)
    in
    { entries = read_entries [] }

  let parse buffer =
    match find_debug_section_by_type buffer Debug_loclists with
    | None -> None
    | Some (section_offset, _section_size) -> (
        let cursor =
          Object.Buffer.cursor buffer
            ~at:(Unsigned.UInt64.to_int section_offset)
        in
        try
          let header = parse_header cursor in
          let offset_table =
            parse_offset_table cursor header.format header.offset_entry_count
          in
          Some { header; offset_table }
        with _ -> None)

  let resolve_location_list buffer (offset : u64) (address_size : u8) =
    match find_debug_section_by_type buffer Debug_loclists with
    | None -> None
    | Some (section_offset, _section_size) ->
        let section_start = Unsigned.UInt64.to_int section_offset in
        let absolute_pos = section_start + Unsigned.UInt64.to_int offset in
        let cursor = Object.Buffer.cursor buffer ~at:absolute_pos in
        Some (parse_location_list cursor address_size)
end

module DebugRnglists = struct
  type header = {
    format : dwarf_format;
    unit_length : u64;
    version : u16;
    address_size : u8;
    segment_size : u8;
    offset_entry_count : u32;
  }

  type range_entry =
    | RLE_end_of_list
    | RLE_base_addressx of { index : int }
    | RLE_startx_endx of { start_index : int; end_index : int }
    | RLE_startx_length of { start_index : int; length : u64 }
    | RLE_offset_pair of { start_offset : u64; end_offset : u64 }
    | RLE_base_address of { address : u64 }
    | RLE_start_end of { start_addr : u64; end_addr : u64 }
    | RLE_start_length of { start_addr : u64; length : u64 }

  type range_list = { entries : range_entry list }
  type rnglists_section = { header : header; offset_table : u64 array }

  let parse_header cursor =
    let format, unit_length = parse_initial_length cursor in
    let version = Object.Buffer.Read.u16 cursor in
    let address_size = Object.Buffer.Read.u8 cursor in
    let segment_size = Object.Buffer.Read.u8 cursor in
    let offset_entry_count = Object.Buffer.Read.u32 cursor in
    {
      format;
      unit_length;
      version;
      address_size;
      segment_size;
      offset_entry_count;
    }

  let parse_offset_table cursor format offset_entry_count =
    let count = Unsigned.UInt32.to_int offset_entry_count in
    Array.init count (fun _i -> read_offset_for_format format cursor)

  let read_addr cursor address_size =
    let addr_sz = Unsigned.UInt8.to_int address_size in
    if addr_sz = 4 then
      Unsigned.UInt64.of_int
        (Unsigned.UInt32.to_int (Object.Buffer.Read.u32 cursor))
    else Object.Buffer.Read.u64 cursor

  let parse_range_list cursor address_size =
    let rec read_entries acc =
      let kind = Unsigned.UInt8.to_int (Object.Buffer.Read.u8 cursor) in
      match kind with
      | 0x00 -> List.rev (RLE_end_of_list :: acc)
      | 0x01 ->
          let index = Object.Buffer.Read.uleb128 cursor in
          read_entries (RLE_base_addressx { index } :: acc)
      | 0x02 ->
          let start_index = Object.Buffer.Read.uleb128 cursor in
          let end_index = Object.Buffer.Read.uleb128 cursor in
          read_entries (RLE_startx_endx { start_index; end_index } :: acc)
      | 0x03 ->
          let start_index = Object.Buffer.Read.uleb128 cursor in
          let length =
            Unsigned.UInt64.of_int (Object.Buffer.Read.uleb128 cursor)
          in
          read_entries (RLE_startx_length { start_index; length } :: acc)
      | 0x04 ->
          let start_offset =
            Unsigned.UInt64.of_int (Object.Buffer.Read.uleb128 cursor)
          in
          let end_offset =
            Unsigned.UInt64.of_int (Object.Buffer.Read.uleb128 cursor)
          in
          read_entries (RLE_offset_pair { start_offset; end_offset } :: acc)
      | 0x05 ->
          let address = read_addr cursor address_size in
          read_entries (RLE_base_address { address } :: acc)
      | 0x06 ->
          let start_addr = read_addr cursor address_size in
          let end_addr = read_addr cursor address_size in
          read_entries (RLE_start_end { start_addr; end_addr } :: acc)
      | 0x07 ->
          let start_addr = read_addr cursor address_size in
          let length =
            Unsigned.UInt64.of_int (Object.Buffer.Read.uleb128 cursor)
          in
          read_entries (RLE_start_length { start_addr; length } :: acc)
      | n -> failwith (Printf.sprintf "Unknown DW_RLE entry kind: 0x%02x" n)
    in
    { entries = read_entries [] }

  let parse buffer =
    match find_debug_section_by_type buffer Debug_rnglists with
    | None -> None
    | Some (section_offset, _section_size) -> (
        let cursor =
          Object.Buffer.cursor buffer
            ~at:(Unsigned.UInt64.to_int section_offset)
        in
        try
          let header = parse_header cursor in
          let offset_table =
            parse_offset_table cursor header.format header.offset_entry_count
          in
          Some { header; offset_table }
        with _ -> None)

  let resolve_range_list buffer (offset : u64) (address_size : u8) =
    match find_debug_section_by_type buffer Debug_rnglists with
    | None -> None
    | Some (section_offset, _section_size) ->
        let section_start = Unsigned.UInt64.to_int section_offset in
        let absolute_pos = section_start + Unsigned.UInt64.to_int offset in
        let cursor = Object.Buffer.cursor buffer ~at:absolute_pos in
        Some (parse_range_list cursor address_size)
end

let lookup_address_in_debug_addr (buffer : Object.Buffer.t) (_addr_base : u64)
    (index : int) : u64 option =
  (* Find the debug_addr section *)
  match find_debug_section_by_type buffer Debug_addr with
  | None -> None
  | Some (section_offset, _) -> (
      try
        (* addr_base is an offset from the beginning of the debug_addr section to the address data *)
        (* We need to parse the entire section, not start at addr_base *)
        let parsed_addr = DebugAddr.parse buffer section_offset in

        (* Check if index is within bounds *)
        if index >= 0 && index < Array.length parsed_addr.entries then
          Some parsed_addr.entries.(index).address
        else None
      with _ -> None)

let resolve_address_index (buffer : Object.Buffer.t) (index : int)
    (addr_base : u64) : u64 =
  (* Try to resolve address index using debug_addr section *)
  match lookup_address_in_debug_addr buffer addr_base index with
  | Some address -> address
  | None ->
      (* Fall back to returning the index value if resolution fails *)
      Unsigned.UInt64.of_int index

module SplitDwarf = struct
  type dwo_context = {
    dwo_buffer : Object.Buffer.t;
    parent_buffer : Object.Buffer.t;
    dwo_id : u64;
    contributions : (dwarf_section * int * int) list;
  }

  let dwo_section_of section_type =
    match section_type with
    | Debug_info -> Debug_info_dwo
    | Debug_abbrev -> Debug_abbrev_dwo
    | Debug_str -> Debug_str_dwo
    | Debug_str_offs -> Debug_str_offs_dwo
    | Debug_line -> Debug_line_dwo
    | Debug_loclists -> Debug_loclists_dwo
    | Debug_rnglists -> Debug_rnglists_dwo
    | Debug_macro -> Debug_macro_dwo
    | other -> other

  let find_section ctx section_type =
    match section_type with
    | Debug_addr -> find_debug_section_by_type ctx.parent_buffer Debug_addr
    | s -> (
        match ctx.contributions with
        | _ :: _ -> (
            let dwo_s = dwo_section_of s in
            match
              List.find_opt (fun (sec, _, _) -> sec = dwo_s) ctx.contributions
            with
            | Some (_, off, sz) ->
                Some (Unsigned.UInt64.of_int off, Unsigned.UInt64.of_int sz)
            | None -> (
                match find_debug_section_by_type ctx.dwo_buffer dwo_s with
                | Some _ as r -> r
                | None -> find_debug_section_by_type ctx.dwo_buffer s))
        | [] -> (
            let dwo_s = dwo_section_of s in
            match find_debug_section_by_type ctx.dwo_buffer dwo_s with
            | Some _ as r -> r
            | None -> find_debug_section_by_type ctx.dwo_buffer s))

  let resolve_string_index_dwo (dwo_buffer : Object.Buffer.t)
      (format : dwarf_format) (index : int) : string =
    match
      ( find_debug_section_by_type dwo_buffer Debug_str_offs_dwo,
        find_debug_section_by_type dwo_buffer Debug_str_dwo )
    with
    | Some (str_offs_offset, _), Some (str_offset, _) -> (
        try
          let header_size, offset_size =
            match format with DWARF32 -> (8, 4) | DWARF64 -> (16, 8)
          in
          let str_offs_cursor =
            cursor dwo_buffer
              ~at:
                (Unsigned.UInt64.to_int str_offs_offset
                + header_size + (index * offset_size))
          in
          let string_offset =
            match format with
            | DWARF32 -> Read.u32 str_offs_cursor |> Unsigned.UInt32.to_int
            | DWARF64 -> Read.u64 str_offs_cursor |> Unsigned.UInt64.to_int
          in
          match
            read_string_from_section dwo_buffer string_offset
              (Unsigned.UInt64.to_int str_offset)
          with
          | Some s -> s
          | None -> Printf.sprintf "<dwo_strx_error:%d>" index
        with _ -> Printf.sprintf "<dwo_strx_error:%d>" index)
    | _ -> Printf.sprintf "<dwo_strx_no_sections:%d>" index

  let parse_abbrev_table_from_buffer (buffer : Object.Buffer.t)
      (section_type : dwarf_section) (offset : u64) : (u64, abbrev) Hashtbl.t =
    let section_offset =
      match find_debug_section_by_type buffer section_type with
      | Some (section_offset, _) -> Unsigned.UInt64.to_int section_offset
      | None -> failwith "Could not find debug_abbrev section"
    in
    let cur =
      cursor buffer ~at:(section_offset + Unsigned.UInt64.to_int offset)
    in
    let table = Hashtbl.create 100 in
    let rec parse_abbrevs () =
      let code = Read.uleb128 cur in
      if code = 0 then ()
      else
        let tag = Read.uleb128 cur in
        let has_children =
          Read.u8 cur |> Unsigned.UInt8.to_int |> bool_of_int
        in
        let rec parse_attr_specs acc =
          let attr = Read.uleb128 cur in
          let form = Read.uleb128 cur in
          if attr = 0 && form = 0 then List.rev acc
          else
            let implicit_const =
              if form = 0x21 then Some (Int64.of_int (Read.sleb128 cur))
              else None
            in
            let attr_spec =
              {
                attr = Unsigned.UInt64.of_int attr;
                form = Unsigned.UInt64.of_int form;
                implicit_const;
              }
            in
            parse_attr_specs (attr_spec :: acc)
        in
        let attr_specs = parse_attr_specs [] in
        let abbrev =
          {
            code = Unsigned.UInt64.of_int code;
            tag = Unsigned.UInt64.of_int tag;
            has_children;
            attr_specs;
          }
        in
        Hashtbl.add table (Unsigned.UInt64.of_int code) abbrev;
        parse_abbrevs ()
    in
    parse_abbrevs ();
    table

  let dwo_abbrev_table ctx offset =
    parse_abbrev_table_from_buffer ctx.dwo_buffer Debug_abbrev_dwo offset

  let dwo_compile_units ctx =
    match find_debug_section_by_type ctx.dwo_buffer Debug_info_dwo with
    | None -> Seq.empty
    | Some (section_offset, section_size) ->
        let section_end = Unsigned.UInt64.to_int section_size in
        let rec parse_units cursor_pos () =
          if cursor_pos >= section_end then Seq.Nil
          else
            try
              let absolute_pos =
                Unsigned.UInt64.to_int section_offset + cursor_pos
              in
              let cur = cursor ctx.dwo_buffer ~at:absolute_pos in
              let span, parsed_header = parse_compile_unit_header cur in
              let obj =
                Object_file.
                  {
                    buffer = ctx.dwo_buffer;
                    format = Object_format.detect_format ctx.dwo_buffer;
                  }
              in
              let unit = CompileUnit.make cursor_pos span obj parsed_header in
              let unit_length =
                Unsigned.UInt64.to_int parsed_header.unit_length
              in
              let length_field_size =
                match parsed_header.format with DWARF32 -> 4 | DWARF64 -> 12
              in
              let next_pos = cursor_pos + unit_length + length_field_size in
              Seq.Cons (unit, parse_units next_pos)
            with exn ->
              Printf.eprintf "Error parsing DWO compile unit at offset %d: %s\n"
                cursor_pos (Printexc.to_string exn);
              Seq.Nil
        in
        parse_units 0

  let load_dwo ~parent_buffer ~dwo_path ~dwo_id =
    try
      let dwo_buffer = Object.Buffer.parse dwo_path in
      let ctx = { dwo_buffer; parent_buffer; dwo_id; contributions = [] } in
      match find_debug_section_by_type dwo_buffer Debug_info_dwo with
      | None -> None
      | Some (section_offset, _) -> (
          let cur =
            cursor dwo_buffer ~at:(Unsigned.UInt64.to_int section_offset)
          in
          let _, header = parse_compile_unit_header cur in
          let parsed_ut = unit_type_of_u8 header.unit_type in
          match parsed_ut with
          | DW_UT_split_compile -> (
              match header.dwo_id with
              | Some id when Unsigned.UInt64.equal id dwo_id -> Some ctx
              | _ -> None)
          | _ -> None)
    with _ -> None

  let fixup_dwo_attribute ctx format addr_base (attr : DIE.attribute) =
    match attr.value with
    | DIE.IndexedString (index, _) ->
        let s = resolve_string_index_dwo ctx.dwo_buffer format index in
        { attr with value = DIE.IndexedString (index, s) }
    | DIE.IndexedAddress (index, _) ->
        let addr = resolve_address_index ctx.parent_buffer index addr_base in
        { attr with value = DIE.IndexedAddress (index, addr) }
    | _ -> attr

  let rec fixup_dwo_die ctx format addr_base die =
    DIE.
      {
        die with
        attributes =
          List.map (fixup_dwo_attribute ctx format addr_base) die.attributes;
        children = Seq.map (fixup_dwo_die ctx format addr_base) die.children;
      }

  let dwo_root_die ctx cu addr_base =
    let abbrev =
      dwo_abbrev_table ctx cu.CompileUnit.header.debug_abbrev_offset
    in
    let enc = CompileUnit.encoding cu in
    let header_size = Unsigned.UInt64.to_int cu.header.header_span.size in
    let die_start =
      Unsigned.UInt64.to_int
        (Unsigned.UInt64.add cu.span.start (Unsigned.UInt64.of_int header_size))
    in
    let cur = cursor ctx.dwo_buffer ~at:die_start in
    match DIE.parse_die cur abbrev enc ctx.dwo_buffer with
    | None -> None
    | Some die -> Some (fixup_dwo_die ctx enc.format addr_base die)

  type dw_sect =
    | DW_SECT_INFO
    | DW_SECT_ABBREV
    | DW_SECT_LINE
    | DW_SECT_LOCLISTS
    | DW_SECT_STR_OFFSETS
    | DW_SECT_MACRO
    | DW_SECT_RNGLISTS

  let dw_sect_of_int = function
    | 1 -> Some DW_SECT_INFO
    | 3 -> Some DW_SECT_ABBREV
    | 4 -> Some DW_SECT_LINE
    | 5 -> Some DW_SECT_LOCLISTS
    | 6 -> Some DW_SECT_STR_OFFSETS
    | 7 -> Some DW_SECT_MACRO
    | 8 -> Some DW_SECT_RNGLISTS
    | _ -> None

  let dwarf_section_of_dw_sect = function
    | DW_SECT_INFO -> Debug_info_dwo
    | DW_SECT_ABBREV -> Debug_abbrev_dwo
    | DW_SECT_LINE -> Debug_line_dwo
    | DW_SECT_LOCLISTS -> Debug_loclists_dwo
    | DW_SECT_STR_OFFSETS -> Debug_str_offs_dwo
    | DW_SECT_MACRO -> Debug_macro_dwo
    | DW_SECT_RNGLISTS -> Debug_rnglists_dwo

  type index_entry = {
    dwo_id : u64;
    contributions : (dw_sect * int * int) list;
  }

  type unit_index = {
    version : int;
    unit_count : int;
    entries : index_entry array;
  }

  type dwp_context = {
    dwp_buffer : Object.Buffer.t;
    parent_buffer : Object.Buffer.t;
    cu_index : unit_index;
    tu_index : unit_index option;
  }

  let parse_unit_index buffer section_type =
    match find_debug_section_by_type buffer section_type with
    | None -> None
    | Some (section_offset, _) -> (
        try
          let cur = cursor buffer ~at:(Unsigned.UInt64.to_int section_offset) in
          let version = Read.u32 cur |> Unsigned.UInt32.to_int in
          let _padding = Read.u32 cur |> Unsigned.UInt32.to_int in
          let section_count = Read.u32 cur |> Unsigned.UInt32.to_int in
          let unit_count = Read.u32 cur |> Unsigned.UInt32.to_int in
          let slot_count = Read.u32 cur |> Unsigned.UInt32.to_int in
          let hash_table = Array.make slot_count 0L in
          for i = 0 to slot_count - 1 do
            hash_table.(i) <- Read.u64 cur |> Unsigned.UInt64.to_int64
          done;
          let index_table = Array.make slot_count 0 in
          for i = 0 to slot_count - 1 do
            index_table.(i) <- Read.u32 cur |> Unsigned.UInt32.to_int
          done;
          let columns = Array.make section_count None in
          for i = 0 to section_count - 1 do
            let v = Read.u32 cur |> Unsigned.UInt32.to_int in
            columns.(i) <- dw_sect_of_int v
          done;
          let offsets =
            Array.init unit_count (fun _ ->
                Array.init section_count (fun _ ->
                    Read.u32 cur |> Unsigned.UInt32.to_int))
          in
          let sizes =
            Array.init unit_count (fun _ ->
                Array.init section_count (fun _ ->
                    Read.u32 cur |> Unsigned.UInt32.to_int))
          in
          let entries =
            Array.init unit_count (fun row ->
                let id_ref = ref Unsigned.UInt64.zero in
                for slot = 0 to slot_count - 1 do
                  if index_table.(slot) = row + 1 then
                    id_ref := Unsigned.UInt64.of_int64 hash_table.(slot)
                done;
                let contribs =
                  List.filter_map
                    (fun col ->
                      match columns.(col) with
                      | Some sect ->
                          Some (sect, offsets.(row).(col), sizes.(row).(col))
                      | None -> None)
                    (List.init section_count Fun.id)
                in
                { dwo_id = !id_ref; contributions = contribs })
          in
          Some { version; unit_count; entries }
        with _ -> None)

  let find_cu_by_dwo_id dwp_ctx id =
    let idx = dwp_ctx.cu_index in
    Array.find_opt
      (fun entry -> Unsigned.UInt64.equal entry.dwo_id id)
      idx.entries

  let dwp_dwo_context dwp_ctx entry =
    let contributions =
      List.map
        (fun (sect, off, sz) -> (dwarf_section_of_dw_sect sect, off, sz))
        entry.contributions
    in
    {
      dwo_buffer = dwp_ctx.dwp_buffer;
      parent_buffer = dwp_ctx.parent_buffer;
      dwo_id = entry.dwo_id;
      contributions;
    }

  let load_dwp ~parent_buffer ~dwp_path =
    try
      let dwp_buffer = Object.Buffer.parse dwp_path in
      match parse_unit_index dwp_buffer Debug_cu_index with
      | None -> None
      | Some cu_index ->
          let tu_index = parse_unit_index dwp_buffer Debug_tu_index in
          Some { dwp_buffer; parent_buffer; cu_index; tu_index }
    with _ -> None
end

(** CompactUnwind module integrates with MachO compact unwinding format *)
module CompactUnwind = struct
  include Compact_unwind

  let find_unwind_info_section buffer =
    try
      let open Object.Macho in
      let _header, commands = read buffer in

      (* Look for __TEXT segment *)
      let text_segment_opt =
        List.find_map
          (function
            | LC_SEGMENT_64 (lazy seg) when seg.seg_segname = "__TEXT" ->
                Some seg
            | _ -> None)
          commands
      in

      match text_segment_opt with
      | None -> None
      | Some text_segment ->
          (* Find __unwind_info section within __TEXT segment *)
          Array.find_map
            (fun section ->
              if section.sec_sectname = "__unwind_info" then
                Some
                  ( Unsigned.UInt32.to_int section.sec_offset,
                    Unsigned.UInt64.to_int section.sec_size )
              else None)
            text_segment.seg_sections
    with _ -> None

  let parse_from_buffer buffer =
    match find_unwind_info_section buffer with
    | None -> None
    | Some (section_offset, section_size) -> (
        try
          let unwind_info =
            parse_unwind_info buffer section_offset section_size
          in
          let arch = detect_architecture buffer in
          Some (unwind_info, arch)
        with Invalid_compact_unwind_format _ -> None)
end
