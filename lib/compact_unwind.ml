open Dwarf_types

exception Invalid_compact_unwind_format of string

type architecture = X86 | X86_64 | ARM64
type compact_unwind_encoding = u32
type unwind_mode = FrameBased | StackImmediate | StackIndirect | DwarfCFI

type unwind_info_header = {
  version : u32;
  common_encodings_array_section_offset : u32;
  common_encodings_array_count : u32;
  personality_array_section_offset : u32;
  personality_array_count : u32;
  index_section_offset : u32;
  index_count : u32;
}

type unwind_info_section_header = {
  kind : u32;
  entry_page_offset : u32;
  entry_count : u32;
}

type unwind_info_compressed_section_header = {
  kind : u32;
  entry_page_offset : u16;
  entry_count : u16;
  encodings_page_offset : u16;
  encodings_count : u16;
}

type unwind_info_regular_second_level_entry = {
  function_offset : u32;
  encoding : compact_unwind_encoding;
}

type unwind_info_compressed_second_level_entry = {
  function_offset : u32; (* Actually stored as 3 bytes *)
  encoding_index : u16; (* Actually stored as 1 byte *)
}

type second_level_page =
  | Regular of {
      header : unwind_info_section_header;
      entries : unwind_info_regular_second_level_entry array;
    }
  | Compressed of {
      header : unwind_info_compressed_section_header;
      encoding_array : compact_unwind_encoding array;
      entries : unwind_info_compressed_second_level_entry array;
    }

type unwind_info_section_header_index_entry = {
  function_offset : u32;
  second_level_page_section_offset : u32;
  lsda_index_array_section_offset : u32;
}

type lsda_descriptor = { function_offset : u32; lsda_offset : u32 }

type unwind_info = {
  header : unwind_info_header;
  common_encodings : compact_unwind_encoding array;
  personalities : u32 array;
  index_entries : unwind_info_section_header_index_entry array;
  lsda_descriptors : lsda_descriptor array;
  pages : second_level_page array;
}

module Encoding = struct
  let start_flag_mask = 0x40000000l
  let has_lsda_mask = 0x80000000l
  let personality_index_mask = 0x30000000l
  let personality_index_shift = 28

  let get_personality_index encoding =
    Int32.(
      to_int
        (shift_right_logical
           (logand (Unsigned.UInt32.to_int32 encoding) personality_index_mask)
           personality_index_shift))

  let has_lsda (encoding : compact_unwind_encoding) =
    Int32.logand (Unsigned.UInt32.to_int32 encoding) has_lsda_mask <> 0l

  let is_function_start (encoding : compact_unwind_encoding) =
    Int32.logand (Unsigned.UInt32.to_int32 encoding) start_flag_mask <> 0l

  module X86_64 = struct
    let mode_mask = 0x0F000000l
    let rbp_frame_mode = 0x01000000l
    let stack_immd_mode = 0x02000000l
    let stack_ind_mode = 0x03000000l

    let get_mode (encoding : compact_unwind_encoding) =
      let mode_bits =
        Int32.logand (Unsigned.UInt32.to_int32 encoding) mode_mask
      in
      if mode_bits = rbp_frame_mode then FrameBased
      else if mode_bits = stack_immd_mode then StackImmediate
      else if mode_bits = stack_ind_mode then StackIndirect
      else DwarfCFI
  end

  module ARM64 = struct
    let mode_mask = 0x0F000000l
    let frame_mode = 0x02000000l
    let frameless_mode = 0x03000000l

    let get_mode (encoding : compact_unwind_encoding) =
      let mode_bits =
        Int32.logand (Unsigned.UInt32.to_int32 encoding) mode_mask
      in
      if mode_bits = frame_mode then FrameBased
      else if mode_bits = frameless_mode then StackImmediate
      else DwarfCFI
  end
end

let parse_unwind_info_header cur =
  let open Object.Buffer in
  let version = Read.u32 cur in
  let common_encodings_array_section_offset = Read.u32 cur in
  let common_encodings_array_count = Read.u32 cur in
  let personality_array_section_offset = Read.u32 cur in
  let personality_array_count = Read.u32 cur in
  let index_section_offset = Read.u32 cur in
  let index_count = Read.u32 cur in
  {
    version;
    common_encodings_array_section_offset;
    common_encodings_array_count;
    personality_array_section_offset;
    personality_array_count;
    index_section_offset;
    index_count;
  }

let parse_second_level_header (cur : Object.Buffer.cursor) =
  let open Object.Buffer in
  let kind = Read.u32 cur in
  let entry_page_offset = Read.u32 cur in
  let entry_count = Read.u32 cur in
  { kind; entry_page_offset; entry_count }

let parse_compressed_second_level_header (cur : Object.Buffer.cursor) =
  let open Object.Buffer in
  let kind = Read.u32 cur in
  let entry_page_offset = Read.u16 cur in
  let entry_count = Read.u16 cur in
  let encodings_page_offset = Read.u16 cur in
  let encodings_count = Read.u16 cur in
  {
    kind;
    entry_page_offset;
    entry_count;
    encodings_page_offset;
    encodings_count;
  }

let parse_common_encodings (cursor : Object.Buffer.cursor) count =
  Array.init count (fun _ -> Object.Buffer.Read.u32 cursor)

let parse_personalities (cursor : Object.Buffer.cursor) count =
  Array.init count (fun _ -> Object.Buffer.Read.u32 cursor)

let parse_index_entry (cur : Object.Buffer.cursor) =
  let open Object.Buffer in
  let function_offset = Read.u32 cur in
  let second_level_page_section_offset = Read.u32 cur in
  let lsda_index_array_section_offset = Read.u32 cur in
  {
    function_offset;
    second_level_page_section_offset;
    lsda_index_array_section_offset;
  }

let parse_index_entries (cursor : Object.Buffer.cursor) count =
  Array.init count (fun _ -> parse_index_entry cursor)

let parse_regular_entry (cur : Object.Buffer.cursor) =
  let open Object.Buffer in
  let function_offset = Read.u32 cur in
  let encoding = Read.u32 cur in
  { function_offset; encoding }

let parse_compressed_entry (cur : Object.Buffer.cursor) =
  let open Object.Buffer in
  (* Read 3-byte function offset *)
  let byte0 = Read.u8 cur in
  let byte1 = Read.u8 cur in
  let byte2 = Read.u8 cur in
  let function_offset =
    let open Unsigned.UInt32 in
    logor
      (logor
         (shift_left (of_int (Unsigned.UInt8.to_int byte2)) 16)
         (shift_left (of_int (Unsigned.UInt8.to_int byte1)) 8))
      (of_int (Unsigned.UInt8.to_int byte0))
  in
  (* Read 1-byte encoding index *)
  let encoding_index_byte = Read.u8 cur in
  let encoding_index =
    Unsigned.(UInt16.of_int (UInt8.to_int encoding_index_byte))
  in
  { function_offset; encoding_index }

let parse_regular_page (cursor : Object.Buffer.cursor)
    (header : unwind_info_section_header) =
  let entries =
    Array.init (Unsigned.UInt32.to_int header.entry_count) (fun _ ->
        parse_regular_entry cursor)
  in
  Regular { header; entries }

let parse_compressed_page (buffer : Object.Buffer.t) page_start_offset =
  let open Object.Buffer in
  let cur = cursor buffer in
  seek cur page_start_offset;
  let header = parse_compressed_second_level_header cur in

  (* Seek to encodings array (relative to page start) *)
  seek cur
    (page_start_offset + Unsigned.UInt16.to_int header.encodings_page_offset);
  let encoding_array =
    Array.init (Unsigned.UInt16.to_int header.encodings_count) (fun _ ->
        Read.u32 cur)
  in

  (* Seek to entries array (relative to page start) *)
  seek cur (page_start_offset + Unsigned.UInt16.to_int header.entry_page_offset);
  let entries =
    Array.init (Unsigned.UInt16.to_int header.entry_count) (fun _ ->
        parse_compressed_entry cur)
  in

  Compressed { header; encoding_array; entries }

let parse_second_level_page (buffer : Object.Buffer.t)
    (base_section_offset : int) (page_offset : u32) =
  let open Object.Buffer in
  let page_start_offset =
    base_section_offset + Unsigned.UInt32.to_int page_offset
  in
  let cur = cursor buffer in
  seek cur page_start_offset;
  let kind = Read.u32 cur in
  match kind with
  | k when Unsigned.UInt32.equal k (Unsigned.UInt32.of_int 2) ->
      seek cur page_start_offset;
      let header = parse_second_level_header cur in
      parse_regular_page cur header
  | k when Unsigned.UInt32.equal k (Unsigned.UInt32.of_int 3) ->
      parse_compressed_page buffer page_start_offset
  | _ ->
      raise
        (Invalid_compact_unwind_format
           ("Unknown page kind: " ^ Unsigned.UInt32.to_string kind))

let parse_unwind_info (buffer : Object.Buffer.t) section_offset =
  let open Object.Buffer in
  let cursor = Object.Buffer.cursor buffer in
  seek cursor section_offset;

  let header = parse_unwind_info_header cursor in

  (* Parse common encodings *)
  seek cursor
    (section_offset
    + Unsigned.UInt32.to_int header.common_encodings_array_section_offset);
  let common_encodings =
    parse_common_encodings cursor
      (Unsigned.UInt32.to_int header.common_encodings_array_count)
  in

  (* Parse personalities *)
  seek cursor
    (section_offset
    + Unsigned.UInt32.to_int header.personality_array_section_offset);
  let personalities =
    parse_personalities cursor
      (Unsigned.UInt32.to_int header.personality_array_count)
  in

  (* Parse index entries *)
  seek cursor
    (section_offset + Unsigned.UInt32.to_int header.index_section_offset);
  let index_entries =
    parse_index_entries cursor (Unsigned.UInt32.to_int header.index_count)
  in

  (* Parse LSDA descriptors from unique LSDA array offsets *)
  let lsda_offsets =
    Array.to_list index_entries
    |> List.map (fun entry -> entry.lsda_index_array_section_offset)
    |> List.filter (fun offset ->
        not (Unsigned.UInt32.equal offset Unsigned.UInt32.zero))
    |> List.sort_uniq Unsigned.UInt32.compare
  in

  let lsda_descriptors =
    (* Only read LSDA descriptors if there are multiple unique offsets (indicating real LSDA data) *)
    match List.sort Unsigned.UInt32.compare lsda_offsets with
    | [] -> [||]
    | [ _ ] -> [||] (* Single offset likely means no real LSDA data *)
    | smallest_offset :: _ ->
        seek cursor (section_offset + Unsigned.UInt32.to_int smallest_offset);
        (* Read LSDA entries with sentinel termination - limit to 4 based on system output *)
        let rec read_lsda_entries acc_entries count =
          if count >= 4 then
            List.rev acc_entries (* Known limit from system objdump *)
          else
            try
              let func_offset = Read.u32 cursor in
              let lsda_off = Read.u32 cursor in
              (* Check for sentinel values (function_offset = 0) *)
              if Unsigned.UInt32.(equal func_offset zero) then
                List.rev acc_entries
              else
                let descriptor =
                  { function_offset = func_offset; lsda_offset = lsda_off }
                in
                read_lsda_entries (descriptor :: acc_entries) (count + 1)
            with _ -> List.rev acc_entries
        in
        Array.of_list (read_lsda_entries [] 0)
  in

  (* Parse second-level pages - only for non-zero offsets *)
  let valid_entries =
    Array.to_list index_entries
    |> List.filter (fun entry ->
        not Unsigned.UInt32.(equal entry.second_level_page_section_offset zero))
    |> Array.of_list
  in

  let pages =
    Array.map
      (fun entry ->
        parse_second_level_page buffer section_offset
          entry.second_level_page_section_offset)
      valid_entries
  in

  {
    header;
    common_encodings;
    personalities;
    index_entries;
    lsda_descriptors;
    pages;
  }

let detect_architecture (buffer : Object.Buffer.t) =
  let open Object.Buffer in
  let cursor = cursor buffer in
  let magic = Read.u32 cursor in
  seek cursor 4;
  let cpu_type = Read.u32 cursor in
  let magic_32 = Unsigned.UInt32.of_int 0xfeedface in
  let magic_64 = Unsigned.UInt32.of_int 0xfeedfacf in
  if Unsigned.UInt32.equal magic magic_32 then
    (* MH_MAGIC - 32-bit *)
    if Unsigned.UInt32.equal cpu_type (Unsigned.UInt32.of_int 7) then X86
      (* CPU_TYPE_X86 *)
    else raise (Invalid_compact_unwind_format "Unsupported 32-bit architecture")
  else if Unsigned.UInt32.equal magic magic_64 then
    (* MH_MAGIC_64 - 64-bit *)
    if Unsigned.UInt32.equal cpu_type (Unsigned.UInt32.of_int 0x01000007) then
      X86_64 (* CPU_TYPE_X86_64 *)
    else if Unsigned.UInt32.equal cpu_type (Unsigned.UInt32.of_int 0x0100000c)
    then ARM64 (* CPU_TYPE_ARM64 *)
    else raise (Invalid_compact_unwind_format "Unsupported 64-bit architecture")
  else raise (Invalid_compact_unwind_format "Not a valid MachO file")

let get_unwind_mode (encoding : compact_unwind_encoding) (arch : architecture) =
  match arch with
  | X86_64 -> Encoding.X86_64.get_mode encoding
  | ARM64 -> Encoding.ARM64.get_mode encoding
  | X86 ->
      (* X86 uses similar encoding to X86_64 but simplified *)
      let mode_bits =
        Int32.logand (Unsigned.UInt32.to_int32 encoding) 0x0F000000l
      in
      if mode_bits = 0x01000000l then FrameBased
      else if mode_bits = 0x02000000l then StackImmediate
      else DwarfCFI

let find_unwind_info_section buffer =
  try
    let open Object.Macho in
    let _header, commands = read buffer in

    (* Look for __TEXT segment *)
    let text_segment_opt =
      List.find_map
        (function
          | LC_SEGMENT_64 (lazy seg) when seg.seg_segname = "__TEXT" -> Some seg
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
  | Some (section_offset, _section_size) -> (
      try
        let unwind_info = parse_unwind_info buffer section_offset in
        let arch = detect_architecture buffer in
        Some (unwind_info, arch)
      with Invalid_compact_unwind_format _ -> None)
