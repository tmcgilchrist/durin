(* DWARF Architecture Support *)

type register = Register of int

module type ARCH = sig
  val register_name : register -> string option
  val name_to_register : string -> register option
  val default_cfa : register * int
  val return_address_register : register
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

  (* System V AMD64 ABI: CFA = rsp + 8 at entry; return address in rip. *)
  let default_cfa = (Register 7, 8)
  let return_address_register = Register 16
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

  (* AAPCS64: CFA = sp + 0 at entry; return address in x30 (LR). *)
  let default_cfa = (Register 31, 0)
  let return_address_register = Register 30
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
    | Register n when n >= 32 && n <= 63 -> Some (Printf.sprintf "f%d" (n - 32))
    (* DWARF 64 is the Alternate Frame Return Column and 65-95 are reserved;
       vector registers v0-v31 are 96-127. *)
    | Register n when n >= 96 && n <= 127 ->
        Some (Printf.sprintf "v%d" (n - 96))
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
    | s when String.length s >= 2 && s.[0] = 'f' -> (
        try
          let n = int_of_string (String.sub s 1 (String.length s - 1)) in
          if n >= 0 && n <= 31 then Some (Register (n + 32)) else None
        with _ -> None)
    | s when String.length s >= 2 && s.[0] = 'v' -> (
        try
          let n = int_of_string (String.sub s 1 (String.length s - 1)) in
          if n >= 0 && n <= 31 then Some (Register (n + 96)) else None
        with _ -> None)
    | _ -> None

  (* RISC-V: CFA = sp (x2) + 0 at entry; return address in ra (x1). *)
  let default_cfa = (Register 2, 0)
  let return_address_register = Register 1
end

(** PowerPC 64-bit register definitions *)
module PowerPC64 : ARCH = struct
  (* DWARF register numbers per the Power Architecture 64-Bit ELF V2 ABI,
     Table 2.26 "Mappings of Common Registers". DWARF 64 and 67 are reserved. *)
  let register_name = function
    | Register n when n >= 0 && n <= 31 -> Some (Printf.sprintf "r%d" n)
    | Register n when n >= 32 && n <= 63 -> Some (Printf.sprintf "f%d" (n - 32))
    | Register 65 -> Some "lr"
    | Register 66 -> Some "ctr"
    | Register n when n >= 68 && n <= 75 ->
        Some (Printf.sprintf "cr%d" (n - 68))
    | Register 76 -> Some "xer"
    | Register n when n >= 77 && n <= 108 ->
        Some (Printf.sprintf "vr%d" (n - 77))
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
        | 'c' when s.[1] = 'r' ->
            let n = int_of_string (String.sub s 2 (String.length s - 2)) in
            if n >= 0 && n <= 7 then Some (Register (n + 68)) else None
        | 'v' when s.[1] = 'r' ->
            let n = int_of_string (String.sub s 2 (String.length s - 2)) in
            if n >= 0 && n <= 31 then Some (Register (n + 77)) else None
        | _ -> (
            match s with
            | "lr" -> Some (Register 65)
            | "ctr" -> Some (Register 66)
            | "xer" -> Some (Register 76)
            | _ -> None)
      else None
    with _ -> None

  (* ELF V2 ABI: CFA = r1 (stack pointer) + 0 at entry; return address in the
     link register (LR), DWARF register 65. *)
  let default_cfa = (Register 1, 0)
  let return_address_register = Register 65
end

(** s390x register definitions *)
module S390X : ARCH = struct
  (* DWARF register numbers per the z/Architecture ELF ABI. The 16 FPRs use an
     interleaved DWARF ordering (a historical even/odd-pair artifact) rather
     than a sequential one: DWARF 16 = f0, 17 = f2, ... 31 = f15. *)
  let fpr_dwarf_order =
    [| 0; 2; 4; 6; 1; 3; 5; 7; 8; 10; 12; 14; 9; 11; 13; 15 |]

  let register_name = function
    | Register n when n >= 0 && n <= 15 -> Some (Printf.sprintf "r%d" n)
    | Register n when n >= 16 && n <= 31 ->
        Some (Printf.sprintf "f%d" fpr_dwarf_order.(n - 16))
    | Register n when n >= 32 && n <= 47 ->
        Some (Printf.sprintf "cr%d" (n - 32))
    | Register n when n >= 48 && n <= 63 -> Some (Printf.sprintf "a%d" (n - 48))
    | Register 64 -> Some "pswm"
    | Register 65 -> Some "pswa"
    (* v16-v31 use the same interleaving as the FPRs, offset by 16. The low
       vector registers v0-v15 share the FPR numbers 16-31 above. *)
    | Register n when n >= 68 && n <= 83 ->
        Some (Printf.sprintf "v%d" (fpr_dwarf_order.(n - 68) + 16))
    | Register _ -> None

  (* Index into [fpr_dwarf_order] whose value is [n], if any. Inverts the
     interleaved ordering shared by the FPRs and the vector registers. *)
  let fpr_order_index n =
    let rec find i =
      if i >= Array.length fpr_dwarf_order then None
      else if fpr_dwarf_order.(i) = n then Some i
      else find (i + 1)
    in
    find 0

  let name_to_register s =
    try
      if String.length s >= 2 then
        match s.[0] with
        | 'r' ->
            let n = int_of_string (String.sub s 1 (String.length s - 1)) in
            if n >= 0 && n <= 15 then Some (Register n) else None
        | 'f' ->
            let n = int_of_string (String.sub s 1 (String.length s - 1)) in
            if n >= 0 && n <= 15 then
              Option.map (fun i -> Register (i + 16)) (fpr_order_index n)
            else None
        | 'v' ->
            let n = int_of_string (String.sub s 1 (String.length s - 1)) in
            if n >= 16 && n <= 31 then
              (* upper vector registers v16-v31 at DWARF 68-83 *)
              Option.map (fun i -> Register (i + 68)) (fpr_order_index (n - 16))
            else if n >= 0 && n <= 15 then
              (* v0-v15 share the FPR DWARF numbers 16-31 *)
              Option.map (fun i -> Register (i + 16)) (fpr_order_index n)
            else None
        | 'c' when String.length s >= 3 && s.[1] = 'r' ->
            let n = int_of_string (String.sub s 2 (String.length s - 2)) in
            if n >= 0 && n <= 15 then Some (Register (n + 32)) else None
        | 'a' ->
            let n = int_of_string (String.sub s 1 (String.length s - 1)) in
            if n >= 0 && n <= 15 then Some (Register (n + 48)) else None
        | _ -> (
            match s with
            | "pswm" -> Some (Register 64)
            | "pswa" -> Some (Register 65)
            | _ -> None)
      else None
    with _ -> None

  (* s390x: CFA = r15 + 160 (the register save area) at entry; return address in
     r14. *)
  let default_cfa = (Register 15, 160)
  let return_address_register = Register 14
end
