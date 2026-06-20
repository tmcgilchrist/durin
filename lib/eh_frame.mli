(** EH Frame parsing for the [.eh_frame] section.

    The [.eh_frame] section contains Call Frame Information used by the runtime
    exception handling mechanism. While similar to [.debug_frame], it has some
    format differences optimized for runtime performance.

    The entry format reuses DWARF Call Frame Information ({!Dwarf.CallFrame};
    DWARF 5 specification, section 6.4 "Call Frame Information"), but the
    [.eh_frame] section itself is defined by the Linux Standard Base Core
    Specification, section 10.6 ("Exception Frames") and the System V ABI AMD64
    Architecture Supplement, section 4.2.4. *)

open Dwarf_types

type eh_frame_entry =
  | EH_CIE of Dwarf.CallFrame.common_information_entry
  | EH_FDE of Dwarf.CallFrame.frame_description_entry

type section = { entries : eh_frame_entry list }

val parse_eh_cie :
  Object.Buffer.cursor -> u32 -> int -> Dwarf.CallFrame.common_information_entry
(** Parse a Common Information Entry adapted for [.eh_frame] format.

    The second parameter is the expected length from the length field.

    @raise Dwarf.Parse_error if the CIE is malformed. *)

val parse_eh_fde :
  Object.Buffer.cursor -> u32 -> int -> Dwarf.CallFrame.frame_description_entry
(** Parse a Frame Description Entry adapted for [.eh_frame] format.

    The second parameter is the expected length from the length field. The third
    parameter is the file offset where this FDE starts. *)

val parse_section : Object.Buffer.cursor -> int -> section
(** Parse the [.eh_frame] section.

    The second parameter is the section size in bytes.

    @raise Dwarf.Parse_error if a CIE is malformed. *)

val find_cie_for_fde :
  section -> u32 -> int -> Dwarf.CallFrame.common_information_entry option
(** Find the CIE corresponding to an FDE using the cie_pointer field.

    In [.eh_frame] format the cie_pointer is a relative offset backwards to the
    CIE. Returns the corresponding CIE if found, [None] otherwise. *)
