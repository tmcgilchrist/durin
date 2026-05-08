(* Dump the contents of an ELF binary's .sframe section in
   readelf --sframe-compatible format. *)

open Durin

let print_flags (f : Dwarf.SFrame.flags) =
  match Dwarf.SFrame.flag_names f with
  | [] -> Printf.printf "    Flags: NONE\n"
  | [ s ] -> Printf.printf "    Flags: %s\n" s
  | first :: rest ->
      Printf.printf "    Flags: %s,\n" first;
      let n = List.length rest in
      List.iteri
        (fun i s ->
          if i = n - 1 then Printf.printf "           %s\n" s
          else Printf.printf "           %s,\n" s)
        rest

let print_header (h : Dwarf.SFrame.header) =
  Printf.printf "    Version: %s\n"
    (Dwarf.SFrame.string_of_version h.preamble.version);
  print_flags h.preamble.flags;
  if h.cfa_fixed_fp_offset <> 0 then
    Printf.printf "    CFA fixed FP offset: %d\n" h.cfa_fixed_fp_offset;
  Printf.printf "    CFA fixed RA offset: %d\n" h.cfa_fixed_ra_offset;
  Printf.printf "    Num FDEs: %d\n" (Unsigned.UInt32.to_int h.num_fdes);
  Printf.printf "    Num FREs: %d\n" (Unsigned.UInt32.to_int h.num_fres)

(* AMD64-style FP rendering. For AMD64, RA sits at the header's fixed CFA
   offset, so FRE offsets[1] (when present) describes FP. AArch64 with
   PAUTH tracks RA at offsets[1], pushing FP to offsets[2] — not handled
   here since we only test on AMD64. *)
let format_amd64_fp (fre : Dwarf.SFrame.fre) =
  if fre.info.offset_count >= 2 then
    let off = fre.offsets.(1) in
    if off >= 0 then Printf.sprintf "c+%d" off else Printf.sprintf "c%d" off
  else "u"

let print_fre_table_header ~is_pcmask =
  if is_pcmask then
    print_string "    STARTPC[m]      CFA       FP        RA           \n"
  else print_string "    STARTPC         CFA       FP        RA           \n"

let print_fre ~fde_pc ~is_pcmask (fre : Dwarf.SFrame.fre) =
  let raw = Unsigned.UInt32.to_int fre.start_address in
  let addr = if is_pcmask then raw else fde_pc + raw in
  let cfa = Dwarf.SFrame.format_cfa fre in
  let fp = format_amd64_fp fre in
  let ra = "f" in
  Printf.printf "    %016x  %-10s%-10s%-13s\n" addr cfa fp ra

let print_fde t ~section_addr fde_index (fde : Dwarf.SFrame.fde) =
  let pc = Dwarf.SFrame.resolve_func_start_pc t ~section_addr ~fde_index in
  Printf.printf "    func idx [%d]: pc = 0x%x, size = %d bytes\n" fde_index pc
    (Unsigned.UInt32.to_int fde.func_size);
  let is_pcmask = fde.fde_type = Dwarf.SFrame.Pcmask in
  print_fre_table_header ~is_pcmask;
  let fres = Dwarf.SFrame.fres_of_fde t fde in
  Array.iter (print_fre ~fde_pc:pc ~is_pcmask) fres

let dump path =
  let buffer = Object.Buffer.parse path in
  match Dwarf.SFrame.find_sframe_section buffer with
  | None ->
      Printf.eprintf "%s: no .sframe section found\n" path;
      exit 1
  | Some (offset, size, section_addr) ->
      let cur = Object.Buffer.cursor buffer ~at:offset in
      let t = Dwarf.SFrame.parse cur size in
      Printf.printf "Contents of the SFrame section .sframe:\n";
      Printf.printf "  Header :\n\n";
      print_header t.header;
      Printf.printf "\n  Function Index :\n\n";
      Array.iteri
        (fun i fde ->
          print_fde t ~section_addr i fde;
          if i < Array.length t.fdes - 1 then print_newline ())
        t.fdes

let () =
  if Array.length Sys.argv <> 2 then (
    Printf.eprintf "Usage: %s BINARY\n" Sys.argv.(0);
    exit 1);
  try dump Sys.argv.(1) with
  | Dwarf.SFrame.Invalid_sframe_format msg ->
      Printf.eprintf "%s: invalid .sframe section: %s\n" Sys.argv.(1) msg;
      exit 1
  | Sys_error msg ->
      Printf.eprintf "%s\n" msg;
      exit 1
