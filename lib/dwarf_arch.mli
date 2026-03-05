(** DWARF Architecture Support

    This module provides architecture-specific register definitions and naming.
    The core expression evaluator is architecture-independent - it only works
    with register numbers. These modules provide human-readable names for
    displaying and parsing register references. *)

(** Architecture-independent register type - just a number *)
type register = Register of int

(** Architecture module signature *)
module type ARCH = sig
  val register_name : register -> string option
  (** Convert a register number to its canonical name for this architecture.
      Returns None if the register number is not recognized. *)

  val name_to_register : string -> register option
  (** Convert a register name to its register number for this architecture.
      Returns None if the name is not recognized. *)
end

module X86_64 : ARCH
(** x86-64 / AMD64 architecture *)

module ARM64 : ARCH
(** AArch64 / ARM64 architecture *)

module RISCV : ARCH
(** RISC-V architecture *)

module PowerPC64 : ARCH
(** PowerPC 64-bit architecture *)

module S390X : ARCH
(** s390x architecture *)
