(** Compact Unwinding Format support for MachO binaries.

    Apple's Compact Unwinding Format is a non-standard alternative to DWARF CFI
    used in MachO binaries for efficient stack unwinding and exception handling.
    It provides a compressed representation of unwinding information stored in
    the __unwind_info section.

    The format uses a two-level page table structure:
    - Root page contains global opcodes, personalities, and page entries
    - Second-level pages (Regular or Compressed) map addresses to unwinding
      opcodes

    References:
    - https://github.com/getsentry/symbolic/blob/master/symbolic-debuginfo/src/macho/compact.rs
    - https://github.com/llvm/llvm-project/blob/main/libunwind/include/mach-o/compact_unwind_encoding.h
*)

open Types

(** Target architecture for compact unwinding *)
type architecture =
  | X86  (** 32-bit x86 *)
  | X86_64  (** 64-bit x86 *)
  | ARM64  (** 64-bit ARM *)

type compact_unwind_encoding = u32
(** 32-bit compact unwind encoding containing unwinding instructions *)

(** Unwinding modes supported by the compact format *)
type unwind_mode =
  | FrameBased  (** Frame pointer based unwinding (RBP/FP) *)
  | StackImmediate  (** Immediate stack-based unwinding *)
  | StackIndirect  (** Indirect stack-based unwinding *)
  | DwarfCFI  (** Fall back to DWARF CFI *)

type unwind_info_header = {
  version : u32;  (** Format version (typically 1) *)
  common_encodings_array_section_offset : u32;
      (** Offset to common encodings array *)
  common_encodings_array_count : u32;  (** Number of common encodings *)
  personality_array_section_offset : u32;  (** Offset to personality array *)
  personality_array_count : u32;  (** Number of personality functions *)
  index_section_offset : u32;  (** Offset to index entries *)
  index_count : u32;  (** Number of index entries *)
}
(** Header of the __unwind_info section *)

type unwind_info_section_header = {
  kind : u32;  (** Page type: 2=Regular, 3=Compressed *)
  entry_page_offset : u32;  (** Offset to entries within page *)
  entry_count : u32;  (** Number of entries in page *)
}
(** Header for second-level pages *)

type unwind_info_compressed_section_header = {
  kind : u32;  (** Page type: must be 3 for compressed *)
  entry_page_offset : u16;  (** Offset to entries within page *)
  entry_count : u16;  (** Number of entries in page *)
  encodings_page_offset : u16;  (** Offset to encodings array *)
  encodings_count : u16;  (** Number of encodings in array *)
}
(** Header for compressed second-level pages with 16-bit fields *)

type unwind_info_regular_second_level_entry = {
  function_offset : u32;  (** Function start offset *)
  encoding : compact_unwind_encoding;  (** Complete unwinding encoding *)
}
(** Entry in a regular second-level page *)

type unwind_info_compressed_second_level_entry = {
  function_offset : u32;  (** Function start offset *)
  encoding_index : u16;  (** Index into encoding palette *)
}
(** Entry in a compressed second-level page *)

(** Second-level page types *)
type second_level_page =
  | Regular of {
      header : unwind_info_section_header;
      entries : unwind_info_regular_second_level_entry array;
    }
  | Compressed of {
      header : unwind_info_compressed_section_header;
      encoding_array : compact_unwind_encoding array;  (** Encoding palette *)
      entries : unwind_info_compressed_second_level_entry array;
    }

type unwind_info_section_header_index_entry = {
  function_offset : u32;  (** Start of address range *)
  second_level_page_section_offset : u32;  (** Offset to second-level page *)
  lsda_index_array_section_offset : u32;  (** Offset to LSDA array *)
}
(** Index entry mapping address ranges to second-level pages *)

type lsda_descriptor = {
  function_offset : u32;  (** Function start offset *)
  lsda_offset : u32;  (** Offset to LSDA data *)
}
(** LSDA (Language Specific Data Area) descriptor *)

type unwind_info = {
  header : unwind_info_header;  (** Section header *)
  common_encodings : compact_unwind_encoding array;
      (** Common encoding palette *)
  personalities : u32 array;  (** Personality function addresses *)
  index_entries : unwind_info_section_header_index_entry array;
      (** Index for address lookup *)
  lsda_descriptors : lsda_descriptor array;  (** LSDA descriptors *)
  pages : second_level_page array;  (** Second-level pages *)
}
(** Complete unwind info section *)

(** Encoding manipulation utilities *)
module Encoding : sig
  val get_personality_index : compact_unwind_encoding -> int
  (** Extract personality function index from encoding
      @param encoding The compact unwind encoding
      @return Personality function index (0-3) *)

  val has_lsda : compact_unwind_encoding -> bool
  (** Check if encoding has Language Specific Data Area
      @param encoding The compact unwind encoding
      @return True if LSDA is present *)

  val is_function_start : compact_unwind_encoding -> bool
  (** Check if encoding marks function start
      @param encoding The compact unwind encoding
      @return True if this is a function start *)

  (** x86_64 specific encoding utilities *)
  module X86_64 : sig
    val get_mode : compact_unwind_encoding -> unwind_mode
    (** Extract unwinding mode from x86_64 encoding
        @param encoding The compact unwind encoding
        @return Unwinding mode *)
  end

  (** ARM64 specific encoding utilities *)
  module ARM64 : sig
    val get_mode : compact_unwind_encoding -> unwind_mode
    (** Extract unwinding mode from ARM64 encoding
        @param encoding The compact unwind encoding
        @return Unwinding mode *)
  end
end

val parse_unwind_info_header : Object.Buffer.cursor -> unwind_info_header
(** Parse unwind info header from buffer
    @param cursor Buffer cursor positioned at header start
    @return Parsed header structure
    @raise Invalid_compact_unwind_format on parsing errors *)

val parse_second_level_header :
  Object.Buffer.cursor -> unwind_info_section_header
(** Parse second-level page header from buffer
    @param cursor Buffer cursor positioned at page header
    @return Parsed page header
    @raise Invalid_compact_unwind_format on parsing errors *)

val parse_compressed_second_level_header :
  Object.Buffer.cursor -> unwind_info_compressed_section_header
(** Parse compressed second-level page header from buffer
    @param cursor Buffer cursor positioned at compressed page header
    @return Parsed compressed page header
    @raise Invalid_compact_unwind_format on parsing errors *)

val parse_unwind_info : Object.Buffer.t -> int -> int -> unwind_info
(** Parse complete unwind info section from buffer
    @param buffer The object buffer containing the binary
    @param section_offset Offset to __unwind_info section
    @param section_size Size of the section in bytes
    @return Parsed unwind information structure
    @raise Invalid_compact_unwind_format on parsing errors *)

val detect_architecture : Object.Buffer.t -> architecture
(** Detect target architecture from MachO binary header
    @param buffer The object buffer containing the MachO binary
    @return Target architecture
    @raise Invalid_compact_unwind_format if not a supported MachO file *)

val get_unwind_mode : compact_unwind_encoding -> architecture -> unwind_mode
(** Get unwinding mode for an encoding with architecture-specific interpretation
    @param encoding The compact unwind encoding
    @param arch Target architecture
    @return Unwinding mode *)

exception Invalid_compact_unwind_format of string
(** Exception raised when compact unwind format is invalid *)
