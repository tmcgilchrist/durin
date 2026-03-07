open Alcotest
open Durin

let parse_str binary_path =
  let buffer = Object.Buffer.parse binary_path in
  Dwarf.DebugStr.parse buffer

let test_parse_succeeds binary_path =
  match parse_str binary_path with
  | None -> fail "expected DebugStr.parse to return Some"
  | Some _ -> ()

let test_has_entries binary_path =
  match parse_str binary_path with
  | None -> fail "expected DebugStr.parse to return Some"
  | Some t -> check bool "entries length > 0" true (Array.length t.entries > 0)

let test_total_size_positive binary_path =
  match parse_str binary_path with
  | None -> fail "expected DebugStr.parse to return Some"
  | Some t -> check bool "total_size > 0" true (t.total_size > 0)

let test_offsets_ascending binary_path =
  match parse_str binary_path with
  | None -> fail "expected DebugStr.parse to return Some"
  | Some t ->
      let entries = t.entries in
      let len = Array.length entries in
      for i = 1 to len - 1 do
        let prev = entries.(i - 1).offset in
        let curr = entries.(i).offset in
        check bool
          (Printf.sprintf "offset %d < offset %d" prev curr)
          true (prev < curr)
      done

let test_known_strings_present binary_path =
  match parse_str binary_path with
  | None -> fail "expected DebugStr.parse to return Some"
  | Some t ->
      let has_main =
        Array.exists
          (fun (e : Dwarf.DebugStr.string_entry) ->
            String.equal e.content "main")
          t.entries
      in
      check bool "contains \"main\"" true has_main

let test_entry_fields_consistent binary_path =
  match parse_str binary_path with
  | None -> fail "expected DebugStr.parse to return Some"
  | Some t ->
      Array.iter
        (fun (e : Dwarf.DebugStr.string_entry) ->
          check int "length = String.length content" (String.length e.content)
            e.length;
          check bool "offset < total_size" true (e.offset < t.total_size))
        t.entries

let binary_path =
  let doc = "Path to DWARF 5 test binary" in
  Cmdliner.Arg.(
    required & opt (some file) None & info [ "binary"; "b" ] ~doc ~docv:"BINARY")

let () =
  run_with_args "debug_str integration" binary_path
    [
      ("parse", [ ("parse succeeds", `Quick, test_parse_succeeds) ]);
      ( "entries",
        [
          ("has entries", `Quick, test_has_entries);
          ("total size positive", `Quick, test_total_size_positive);
          ("offsets ascending", `Quick, test_offsets_ascending);
          ("known strings present", `Quick, test_known_strings_present);
          ("entry fields consistent", `Quick, test_entry_fields_consistent);
        ] );
    ]
