open Alcotest
open Durin

let u64 n = Unsigned.UInt64.of_int n

let default_line_header () : Dwarf.DebugLine.line_program_header =
  {
    format = DWARF32;
    unit_length = Unsigned.UInt64.zero;
    version = Unsigned.UInt16.of_int 5;
    address_size = Unsigned.UInt8.of_int 8;
    segment_selector_size = Unsigned.UInt8.of_int 0;
    header_length = Unsigned.UInt64.zero;
    minimum_instruction_length = Unsigned.UInt8.of_int 1;
    maximum_operations_per_instruction = Unsigned.UInt8.of_int 1;
    default_is_stmt = true;
    line_base = -5;
    line_range = Unsigned.UInt8.of_int 14;
    opcode_base = Unsigned.UInt8.of_int 13;
    standard_opcode_lengths =
      Array.map Unsigned.UInt8.of_int [| 0; 1; 1; 1; 1; 0; 0; 0; 1; 0; 0; 1 |];
    directory_entry_format_count = Unsigned.UInt8.of_int 1;
    directory_entry_formats = [| (Dwarf.DW_LNCT_path, Dwarf.DW_FORM_string) |];
    directories_count = Unsigned.UInt32.of_int 1;
    directories = [| "/src" |];
    file_name_entry_format_count = Unsigned.UInt8.of_int 2;
    file_name_entry_formats =
      [|
        (Dwarf.DW_LNCT_path, Dwarf.DW_FORM_string);
        (Dwarf.DW_LNCT_directory_index, Dwarf.DW_FORM_udata);
      |];
    file_names_count = Unsigned.UInt32.of_int 1;
    file_names =
      [|
        {
          name = "main.c";
          timestamp = Unsigned.UInt64.zero;
          size = Unsigned.UInt64.zero;
          directory = "/src";
          md5_checksum = None;
        };
      |];
  }

let line_entry ?(file = 0) ?(col = 0) ?(is_stmt = true) ?(bb = false)
    ?(pe = false) ?(eb = false) ?(disc = 0) ?(isa = 0) ?(es = false) ~addr ~ln
    () : Dwarf.DebugLine.line_table_entry =
  {
    address = Unsigned.UInt64.of_int addr;
    line = Unsigned.UInt32.of_int ln;
    column = Unsigned.UInt32.of_int col;
    file_index = Unsigned.UInt32.of_int file;
    isa = Unsigned.UInt32.of_int isa;
    discriminator = Unsigned.UInt32.of_int disc;
    op_index = Unsigned.UInt32.of_int 0;
    is_stmt;
    basic_block = bb;
    end_sequence = es;
    prologue_end = pe;
    epilogue_begin = eb;
  }

(* Query API over a table built with [DebugLine.build] from synthetic rows. Two
   sequences supplied out of address order (each terminated by an end_sequence
   row) exercise sequence sorting, containing-row address lookup, and the
   exact/nearest-following line lookups. *)
let test_line_table_queries () =
  let header = default_line_header () in
  let entries =
    [
      line_entry ~addr:0x2000 ~ln:20 ();
      line_entry ~addr:0x2010 ~ln:21 ();
      line_entry ~addr:0x2020 ~ln:0 ~es:true ();
      line_entry ~addr:0x1000 ~ln:10 ();
      line_entry ~addr:0x1010 ~ln:11 ();
      line_entry ~addr:0x1020 ~ln:0 ~es:true ();
    ]
  in
  let lt = Dwarf.DebugLine.build header (List.to_seq entries) in
  (* entries come back in address order *)
  let rows = Dwarf.DebugLine.entries lt |> List.of_seq in
  check int "row count" 6 (List.length rows);
  check int "first row addr" 0x1000
    (Unsigned.UInt64.to_int (List.hd rows).Dwarf.DebugLine.address);
  (* find_by_address returns the containing row's line, or -1 for none *)
  let line_at a =
    match Dwarf.DebugLine.find_by_address lt (u64 a) with
    | Some e -> Unsigned.UInt32.to_int e.Dwarf.DebugLine.line
    | None -> -1
  in
  check int "addr within first row of seq A" 10 (line_at 0x1008);
  check int "addr within second row of seq A" 11 (line_at 0x1015);
  check int "addr within seq B" 20 (line_at 0x2004);
  check int "addr before all rows" (-1) (line_at 0x0500);
  check int "addr in gap between sequences" (-1) (line_at 0x1800);
  check int "addr at end_sequence row" (-1) (line_at 0x1020);
  check int "addr past end" (-1) (line_at 0x3000);
  (* find_by_line: exact preferred, else nearest source line greater than the
     query (breakpoint slides forward); -1 when nothing matches. Lines present
     are 10,11 (seq A) and 20,21 (seq B). *)
  let line_addr q =
    match Dwarf.DebugLine.find_by_line lt ~file:0 ~line:q with
    | Some e -> Unsigned.UInt64.to_int e.Dwarf.DebugLine.address
    | None -> -1
  in
  check int "exact line 21" 0x2010 (line_addr 21);
  check int "exact line 10" 0x1000 (line_addr 10);
  check int "no-code line 15 slides to line 20" 0x2000 (line_addr 15);
  check int "line 5 slides to line 10" 0x1000 (line_addr 5);
  check int "line past end" (-1) (line_addr 999);
  (* find_by_line_exact never slides forward *)
  let exact_addr q =
    match Dwarf.DebugLine.find_by_line_exact lt ~file:0 ~line:q with
    | Some e -> Unsigned.UInt64.to_int e.Dwarf.DebugLine.address
    | None -> -1
  in
  check int "exact-only line 21 hits" 0x2010 (exact_addr 21);
  check int "exact-only line 10 hits" 0x1000 (exact_addr 10);
  check int "exact-only no-code line 15 misses" (-1) (exact_addr 15);
  check int "exact-only line 5 misses" (-1) (exact_addr 5)

let () =
  run "DebugLine"
    [
      ( "line_table_queries",
        [ test_case "build and query" `Quick test_line_table_queries ] );
    ]
