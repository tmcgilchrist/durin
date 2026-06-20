(* EH Frame (.eh_frame section) — ELF exception-handling call frame information.
   The entry format reuses DWARF Call Frame Information (Dwarf.CallFrame); the
   section framing is defined by the LSB / System V ABI, not DWARF. *)

open Dwarf_types
open Object.Buffer
open Dwarf

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
    fail "Invalid EH CIE: cie_id is not 0";

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
    span =
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
        to_int (add initial_location_offset (of_int initial_location_field_pos)))
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
              (Char.chr (Unsigned.UInt8.to_int (Object.Buffer.Read.u8 cursor)))
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
    span =
      {
        start = Unsigned.UInt64.of_int fde_offset;
        size = Unsigned.UInt64.of_int (cursor.position - fde_offset);
      };
    offset = Unsigned.UInt64.of_int fde_offset;
  }

let parse_section cur section_size =
  let open Object.Buffer in
  let section_start = cur.position in
  (* Track section start for relative offsets *)
  let section_end = cur.position + section_size in
  let entries = ref [] in

  while cur.position < section_end do
    let start_pos = cur.position in
    let length = Object.Buffer.Read.u32 cur in
    let length_int = Unsigned.UInt32.to_int length in

    if length_int = 0 then
      (* Zero terminator *)
      cur.position <- section_end
    else
      let id = Object.Buffer.Read.u32 cur in
      (* Reset cur to parse the full entry from its start. *)
      cur.position <- start_pos;
      let entry_section_offset = start_pos - section_start in

      if Unsigned.UInt32.to_int32 id = 0x00000000l then
        (* A CIE in .eh_frame format (id = 0 instead of 0xffffffff). *)
        let cie = parse_eh_cie cur length entry_section_offset in
        entries := EH_CIE cie :: !entries
      else
        (* An FDE - parse it to get address ranges. *)
        let fde = parse_eh_fde cur length entry_section_offset in
        entries := EH_FDE fde :: !entries
  done;

  { entries = List.rev !entries }

(* Find the CIE corresponding to an FDE using the cie_pointer field. *)
let find_cie_for_fde (section : section) (cie_pointer : u32)
    (fde_file_offset : int) : CallFrame.common_information_entry option =
  (* In .eh_frame the cie_pointer is subtracted from the section offset of the
     cie_pointer field itself to give the CIE's section offset (LSB Core
     Specification, section 10.6). The cie_pointer field sits 4 bytes into the
     FDE, right after the 4-byte length field. *)
  let cie_pointer_field_offset = fde_file_offset + 4 in
  let cie_offset =
    cie_pointer_field_offset - Unsigned.UInt32.to_int cie_pointer
  in

  (* Return the CIE whose section offset matches exactly, or None. An FDE
     references exactly one CIE; a missing match means corrupt input rather
     than a CIE to guess at, so we do not fall back to an arbitrary CIE. *)
  let rec find_cie_by_offset = function
    | [] -> None
    | EH_CIE cie :: rest ->
        if Unsigned.UInt64.to_int cie.offset = cie_offset then Some cie
        else find_cie_by_offset rest
    | EH_FDE _ :: rest -> find_cie_by_offset rest
  in
  find_cie_by_offset section.entries
