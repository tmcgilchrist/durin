(** DWARF architecture support: the per-target DWARF register conventions.

    DWARF identifies registers by number; the meaning of a number is
    architecture-specific. Each module below maps those numbers to and from
    canonical register names (for display and parsing) and supplies the two CFI
    ABI facts {!Dwarf.CallFrame} needs — the default Canonical Frame Address
    rule and the return-address register — per the target's published ABI. *)

type register =
  | Register of int
      (** A DWARF register, identified by its architecture-specific number. *)

(** The DWARF register conventions of one architecture. Each module below
    implements this against the target's published ABI. *)
module type ARCH = sig
  val register_name : register -> string option
  (** The canonical name of a register for this architecture (e.g. [rsp],
      [x30]), or [None] if the number is not recognised. *)

  val name_to_register : string -> register option
  (** The register number for a canonical name, or [None] if unrecognised. The
      inverse of {!register_name}. *)

  val default_cfa : register * int
  (** The default Canonical Frame Address rule at function entry: the register
      holding the stack pointer and the byte offset added to it. Used as the
      baseline CFA when a CIE carries no initial instructions. *)

  val return_address_register : register
  (** The DWARF register that holds the return address per this architecture's
      ABI. *)
end

module X86_64 : ARCH
(** x86-64 / AMD64, per the System V AMD64 ABI. *)

module ARM64 : ARCH
(** AArch64 / ARM64, per the AArch64 DWARF conventions (AAPCS64). *)

module RISCV : ARCH
(** RISC-V, per the RISC-V ELF psABI. *)

module PowerPC64 : ARCH
(** PowerPC 64-bit, per the Power Architecture 64-Bit ELF V2 ABI. *)

module S390X : ARCH
(** s390x, per the z/Architecture ELF ABI. *)
