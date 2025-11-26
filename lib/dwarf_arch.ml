(* DWARF Architecture Support *)

type register = Register of int

module type ARCH = sig
  val register_name : register -> string option

  val name_to_register : string -> register option
end

(** x86-64 / AMD64 register definitions

    Register numbers from DWARF for x86-64 spec (System V ABI AMD64 supplement)
*)
module X86_64 : ARCH = struct
  let register_name = function
    | Register 0 -> Some "rax"
    | Register 1 -> Some "rdx"
    | Register 2 -> Some "rcx"
    | Register 3 -> Some "rbx"
    | Register 4 -> Some "rsi"
    | Register 5 -> Some "rdi"
    | Register 6 -> Some "rbp"
    | Register 7 -> Some "rsp"
    | Register 8 -> Some "r8"
    | Register 9 -> Some "r9"
    | Register 10 -> Some "r10"
    | Register 11 -> Some "r11"
    | Register 12 -> Some "r12"
    | Register 13 -> Some "r13"
    | Register 14 -> Some "r14"
    | Register 15 -> Some "r15"
    | Register 16 -> Some "rip"
    | Register 17 -> Some "xmm0"
    | Register 18 -> Some "xmm1"
    | Register 19 -> Some "xmm2"
    | Register 20 -> Some "xmm3"
    | Register 21 -> Some "xmm4"
    | Register 22 -> Some "xmm5"
    | Register 23 -> Some "xmm6"
    | Register 24 -> Some "xmm7"
    | Register 25 -> Some "xmm8"
    | Register 26 -> Some "xmm9"
    | Register 27 -> Some "xmm10"
    | Register 28 -> Some "xmm11"
    | Register 29 -> Some "xmm12"
    | Register 30 -> Some "xmm13"
    | Register 31 -> Some "xmm14"
    | Register 32 -> Some "xmm15"
    | Register _ -> None

  let name_to_register = function
    | "rax" -> Some (Register 0)
    | "rdx" -> Some (Register 1)
    | "rcx" -> Some (Register 2)
    | "rbx" -> Some (Register 3)
    | "rsi" -> Some (Register 4)
    | "rdi" -> Some (Register 5)
    | "rbp" -> Some (Register 6)
    | "rsp" -> Some (Register 7)
    | "r8" -> Some (Register 8)
    | "r9" -> Some (Register 9)
    | "r10" -> Some (Register 10)
    | "r11" -> Some (Register 11)
    | "r12" -> Some (Register 12)
    | "r13" -> Some (Register 13)
    | "r14" -> Some (Register 14)
    | "r15" -> Some (Register 15)
    | "rip" -> Some (Register 16)
    | "xmm0" -> Some (Register 17)
    | "xmm1" -> Some (Register 18)
    | "xmm2" -> Some (Register 19)
    | "xmm3" -> Some (Register 20)
    | "xmm4" -> Some (Register 21)
    | "xmm5" -> Some (Register 22)
    | "xmm6" -> Some (Register 23)
    | "xmm7" -> Some (Register 24)
    | "xmm8" -> Some (Register 25)
    | "xmm9" -> Some (Register 26)
    | "xmm10" -> Some (Register 27)
    | "xmm11" -> Some (Register 28)
    | "xmm12" -> Some (Register 29)
    | "xmm13" -> Some (Register 30)
    | "xmm14" -> Some (Register 31)
    | "xmm15" -> Some (Register 32)
    | _ -> None
end

(** AArch64 / ARM64 register definitions

    Register numbers from DWARF for ARM AArch64 spec *)
module ARM64 : ARCH = struct
  let register_name = function
    | Register n when n >= 0 && n <= 30 -> Some (Printf.sprintf "x%d" n)
    | Register 31 -> Some "sp"
    | Register 32 -> None (* Reserved *)
    | Register 33 -> Some "elr"
    | Register 34 -> Some "ra_sign_state"
    | Register n when n >= 64 && n <= 95 -> Some (Printf.sprintf "v%d" (n - 64))
    | Register _ -> None

  let name_to_register s =
    try
      if String.length s >= 2 then
        match s.[0] with
        | 'x' ->
            let n = int_of_string (String.sub s 1 (String.length s - 1)) in
            if n >= 0 && n <= 30 then Some (Register n) else None
        | 'v' ->
            let n = int_of_string (String.sub s 1 (String.length s - 1)) in
            if n >= 0 && n <= 31 then Some (Register (n + 64)) else None
        | _ -> (
            match s with
            | "sp" -> Some (Register 31)
            | "elr" -> Some (Register 33)
            | "ra_sign_state" -> Some (Register 34)
            | _ -> None)
      else None
    with _ -> None
end

(** RISC-V register definitions

    Register numbers from DWARF for RISC-V spec *)
module RISCV : ARCH = struct
  let register_name = function
    | Register 0 -> Some "zero"
    | Register 1 -> Some "ra"
    | Register 2 -> Some "sp"
    | Register 3 -> Some "gp"
    | Register 4 -> Some "tp"
    | Register 5 -> Some "t0"
    | Register 6 -> Some "t1"
    | Register 7 -> Some "t2"
    | Register 8 -> Some "s0" (* fp *)
    | Register 9 -> Some "s1"
    | Register 10 -> Some "a0"
    | Register 11 -> Some "a1"
    | Register 12 -> Some "a2"
    | Register 13 -> Some "a3"
    | Register 14 -> Some "a4"
    | Register 15 -> Some "a5"
    | Register 16 -> Some "a6"
    | Register 17 -> Some "a7"
    | Register 18 -> Some "s2"
    | Register 19 -> Some "s3"
    | Register 20 -> Some "s4"
    | Register 21 -> Some "s5"
    | Register 22 -> Some "s6"
    | Register 23 -> Some "s7"
    | Register 24 -> Some "s8"
    | Register 25 -> Some "s9"
    | Register 26 -> Some "s10"
    | Register 27 -> Some "s11"
    | Register 28 -> Some "t3"
    | Register 29 -> Some "t4"
    | Register 30 -> Some "t5"
    | Register 31 -> Some "t6"
    | Register 32 -> Some "pc"
    | Register n when n >= 33 && n <= 64 -> Some (Printf.sprintf "f%d" (n - 33))
    | Register _ -> None

  let name_to_register = function
    | "zero" -> Some (Register 0)
    | "ra" -> Some (Register 1)
    | "sp" -> Some (Register 2)
    | "gp" -> Some (Register 3)
    | "tp" -> Some (Register 4)
    | "t0" -> Some (Register 5)
    | "t1" -> Some (Register 6)
    | "t2" -> Some (Register 7)
    | "s0" | "fp" -> Some (Register 8)
    | "s1" -> Some (Register 9)
    | "a0" -> Some (Register 10)
    | "a1" -> Some (Register 11)
    | "a2" -> Some (Register 12)
    | "a3" -> Some (Register 13)
    | "a4" -> Some (Register 14)
    | "a5" -> Some (Register 15)
    | "a6" -> Some (Register 16)
    | "a7" -> Some (Register 17)
    | "s2" -> Some (Register 18)
    | "s3" -> Some (Register 19)
    | "s4" -> Some (Register 20)
    | "s5" -> Some (Register 21)
    | "s6" -> Some (Register 22)
    | "s7" -> Some (Register 23)
    | "s8" -> Some (Register 24)
    | "s9" -> Some (Register 25)
    | "s10" -> Some (Register 26)
    | "s11" -> Some (Register 27)
    | "t3" -> Some (Register 28)
    | "t4" -> Some (Register 29)
    | "t5" -> Some (Register 30)
    | "t6" -> Some (Register 31)
    | "pc" -> Some (Register 32)
    | s when String.length s >= 2 && s.[0] = 'f' -> (
        try
          let n = int_of_string (String.sub s 1 (String.length s - 1)) in
          if n >= 0 && n <= 31 then Some (Register (n + 33)) else None
        with _ -> None)
    | _ -> None
end

(** PowerPC 64-bit register definitions *)
module PowerPC64 : ARCH = struct
  let register_name = function
    | Register n when n >= 0 && n <= 31 -> Some (Printf.sprintf "r%d" n)
    | Register n when n >= 32 && n <= 63 -> Some (Printf.sprintf "f%d" (n - 32))
    | Register 64 -> Some "cr"
    | Register 65 -> Some "fpscr"
    | Register 66 -> Some "msr"
    | Register 67 -> Some "vscr"
    | Register 68 -> Some "lr"
    | Register 69 -> Some "ctr"
    | Register 70 -> Some "xer"
    | Register n when n >= 77 && n <= 108 ->
        Some (Printf.sprintf "v%d" (n - 77))
    | Register _ -> None

  let name_to_register s =
    try
      if String.length s >= 2 then
        match s.[0] with
        | 'r' ->
            let n = int_of_string (String.sub s 1 (String.length s - 1)) in
            if n >= 0 && n <= 31 then Some (Register n) else None
        | 'f' ->
            let n = int_of_string (String.sub s 1 (String.length s - 1)) in
            if n >= 0 && n <= 31 then Some (Register (n + 32)) else None
        | 'v' ->
            let n = int_of_string (String.sub s 1 (String.length s - 1)) in
            if n >= 0 && n <= 31 then Some (Register (n + 77)) else None
        | _ -> (
            match s with
            | "cr" -> Some (Register 64)
            | "fpscr" -> Some (Register 65)
            | "msr" -> Some (Register 66)
            | "vscr" -> Some (Register 67)
            | "lr" -> Some (Register 68)
            | "ctr" -> Some (Register 69)
            | "xer" -> Some (Register 70)
            | _ -> None)
      else None
    with _ -> None
end

(** s390x register definitions *)
module S390X : ARCH = struct
  let register_name = function
    | Register n when n >= 0 && n <= 15 -> Some (Printf.sprintf "r%d" n)
    | Register n when n >= 16 && n <= 31 -> Some (Printf.sprintf "f%d" (n - 16))
    | Register n when n >= 32 && n <= 47 -> Some (Printf.sprintf "a%d" (n - 32))
    | Register 64 -> Some "pswm"
    | Register 65 -> Some "pswa"
    | Register _ -> None

  let name_to_register s =
    try
      if String.length s >= 2 then
        match s.[0] with
        | 'r' ->
            let n = int_of_string (String.sub s 1 (String.length s - 1)) in
            if n >= 0 && n <= 15 then Some (Register n) else None
        | 'f' ->
            let n = int_of_string (String.sub s 1 (String.length s - 1)) in
            if n >= 0 && n <= 15 then Some (Register (n + 16)) else None
        | 'a' ->
            let n = int_of_string (String.sub s 1 (String.length s - 1)) in
            if n >= 0 && n <= 15 then Some (Register (n + 32)) else None
        | _ -> (
            match s with
            | "pswm" -> Some (Register 64)
            | "pswa" -> Some (Register 65)
            | _ -> None)
      else None
    with _ -> None
end
