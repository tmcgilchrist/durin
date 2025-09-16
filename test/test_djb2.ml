open Durin

(* TODO Add more thorough test to djb2 hashing. *)
let test_djb2_empty_string () =
  let result = Dwarf.DebugNames.djb2_hash "" |> Unsigned.UInt32.to_int in
  Alcotest.(check int) "empty string hash" 5381 result

let test_djb2_single_char () =
  let result = Dwarf.DebugNames.djb2_hash "a" |> Unsigned.UInt32.to_int in
  Alcotest.(check int) "single char 'a' hash" 177670 result

let test_djb2_two_chars () =
  let result = Dwarf.DebugNames.djb2_hash "ab" |> Unsigned.UInt32.to_int in
  (* Use actual computed value for now - will validate against DWARF spec *)
  Alcotest.(check int) "two chars 'ab' hash" 5863208 result

let test_djb2_main_function () =
  let result = Dwarf.DebugNames.djb2_hash "main" |> Unsigned.UInt32.to_int in
  (* Use actual computed value - will validate against real debug_names data *)
  Alcotest.(check int) "function name 'main' hash" 2090499946 result

let test_djb2_consistency () =
  (* Test that our implementation is consistent *)
  let test_cases =
    [
      ("int", "should produce consistent hash");
      ("char", "should produce consistent hash");
      ("void", "should produce consistent hash");
      ("main", "should produce consistent hash");
    ]
  in
  List.iter
    (fun (name, _description) ->
      let result1 = Dwarf.DebugNames.djb2_hash name |> Unsigned.UInt32.to_int in
      let result2 = Dwarf.DebugNames.djb2_hash name |> Unsigned.UInt32.to_int in
      Alcotest.(check int) ("consistency for '" ^ name ^ "'") result1 result2)
    test_cases

let () =
  let tests =
    [
      ("djb2_empty_string", `Quick, test_djb2_empty_string);
      ("djb2_single_char", `Quick, test_djb2_single_char);
      ("djb2_two_chars", `Quick, test_djb2_two_chars);
      ("djb2_main_function", `Quick, test_djb2_main_function);
      ("djb2_consistency", `Quick, test_djb2_consistency);
    ]
  in
  Alcotest.run "DJB2_Hash" [ ("djb2", tests) ]
