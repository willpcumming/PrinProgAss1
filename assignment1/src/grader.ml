open Unix
open Assignment1

let test_assignment_1_0 () =
  assert ((pow 3 0) = 1)

let test_assignment_1_1 () =
  assert ((pow (-3) 3) = -27)

let test_assignment_1_2 () =
  assert ((range 2 5) = [2;3;4;5])

let test_assignment_1_3 () =
  assert ((range 0 0) = [0])

let test_assignment_1_4 () =
  assert ((range 1 0) = [])

let test_assignment_1_5 () =
  assert ((flatten [[1;2];[3;4]]) = [1;2;3;4])

let test_assignment_1_6 () =
  assert ((flatten [[1;2];[];[3;4];[];[];[5]]) = [1;2;3;4;5])

let test_assignment_1_7 () =
  assert ((remove_stutter [1;2;2;3;1;1;1;4;4;2;2]) = [1;2;3;1;4;2])

let test_assignment_1_8 () =
  assert ((remove_stutter [1;1;1;1;1]) = [1])

let test_assignment_1_9 () =
  assert ((remove_stutter [1;1;1;1;1;2]) = [1;2])

let test_assignment_1_10 () =
  assert ((remove_stutter [5;7;7;5;5;5;5;5;2;1;1]) = [5;7;5;2;1])

let test_assignment_1_11 () =
  assert ((rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 2) = ["g"; "h"; "a"; "b"; "c"; "d"; "e"; "f"])

let test_assignment_1_12 () =
  assert ((rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 0) = ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"])

let test_assignment_1_13 () =
  assert ((rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 7) = ["b"; "c"; "d"; "e"; "f"; "g"; "h"; "a"])

let test_assignment_1_14 () =
  assert ((rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 6) = ["c"; "d"; "e"; "f"; "g"; "h"; "a"; "b"])

let test_assignment_1_15 () =
  assert ((jump ["first"; "second"; "third"; "fourth"] ["fifth"; "sixth"; "seventh"; "eighth"]) = ["fifth"; "second"; "seventh"; "fourth"])

let test_assignment_1_16 () =
  assert ((jump [1; 3; 5; 7; 9] [0; 2; 4; 6]) = [0; 3; 4; 7])

let test_assignment_1_17 () =
  assert ((jump ["1"; "2"] ["3"]) = ["3"])

let test_assignment_1_18 () =
  assert ((jump [] ["c"; "d"; "e"]) = [])

let test_assignment_1_19 () =
  assert ((nth [1; 2; 3; 4; 5; 6; 7] 1) = [1; 2; 3; 4; 5; 6; 7])

let test_assignment_1_20 () =
  assert ((nth [1; 2; 3; 4; 5; 6; 7] 2) = [2; 4; 6])

let test_assignment_1_21 () =
  assert ((nth [1; 2; 3; 4; 5; 6; 7; 6; 5; 4; 3; 2; 1] 3) = [3; 6; 5; 2])

let test_assignment_1_22 () =
  assert ((nth [1; 2; 3; 4; 5; 6; 7] 7) = [7])

let test_assignment_1_23 () =
  assert ((digitsOfInt 352663) = [3;5;2;6;6;3])

let test_assignment_1_24 () =
  assert ((digitsOfInt 31243) = [3;1;2;4;3])

let test_assignment_1_25 () =
  assert ((digitsOfInt 23422) = [2;3;4;2;2])

let test_assignment_1_26 () =
  assert ((additivePersistence 12345678910) = 3)

let test_assignment_1_27 () =
  assert ((digitalRoot 12345678910) = 1)

let test_assignment_1_28 () =
  assert ((additivePersistence 352663) = 2)

let test_assignment_1_29 () =
  assert ((digitalRoot 352663) = 7)

let tests = [
  ("test_assignment_1_0", test_assignment_1_0);
  ("test_assignment_1_1", test_assignment_1_1);
  ("test_assignment_1_2", test_assignment_1_2);
  ("test_assignment_1_3", test_assignment_1_3);
  ("test_assignment_1_4", test_assignment_1_4);
  ("test_assignment_1_5", test_assignment_1_5);
  ("test_assignment_1_6", test_assignment_1_6);
  ("test_assignment_1_7", test_assignment_1_7);
  ("test_assignment_1_8", test_assignment_1_8);
  ("test_assignment_1_9", test_assignment_1_9);
  ("test_assignment_1_10", test_assignment_1_10);
  ("test_assignment_1_11", test_assignment_1_11);
  ("test_assignment_1_12", test_assignment_1_12);
  ("test_assignment_1_13", test_assignment_1_13);
  ("test_assignment_1_14", test_assignment_1_14);
  ("test_assignment_1_15", test_assignment_1_15);
  ("test_assignment_1_16", test_assignment_1_16);
  ("test_assignment_1_17", test_assignment_1_17);
  ("test_assignment_1_18", test_assignment_1_18);
  ("test_assignment_1_19", test_assignment_1_19);
  ("test_assignment_1_20", test_assignment_1_20);
  ("test_assignment_1_21", test_assignment_1_21);
  ("test_assignment_1_22", test_assignment_1_22);
  ("test_assignment_1_23", test_assignment_1_23);
  ("test_assignment_1_24", test_assignment_1_24);
  ("test_assignment_1_25", test_assignment_1_25);
  ("test_assignment_1_26", test_assignment_1_26);
  ("test_assignment_1_27", test_assignment_1_27);
  ("test_assignment_1_28", test_assignment_1_28);
  ("test_assignment_1_29", test_assignment_1_29);
]

exception Timeout

let set_timeout seconds =
  let handler _ = raise Timeout in
  ignore (Sys.signal Sys.sigalrm (Sys.Signal_handle handler));
  ignore (alarm seconds)

let run_test (name : string) (test_fn : unit -> unit) =
  try
    set_timeout 15; (* Set timeout for 15 seconds *)
    let _ = test_fn () in
    ignore (alarm 0); (* Cancel timeout alarm *)
    print_endline (name ^ " passed"); true
  with
  | Timeout -> print_endline (name ^ " failed (timeout)"); false
  | _ -> print_endline (name ^ " failed with exception"); false

let run_tests tests =
  let total = List.length tests in
  let passed = List.fold_left (fun acc (name, test_fn) ->
    if run_test name test_fn then acc + 1 else acc
  ) 0 tests in
  Printf.printf "Summary: %d/%d tests passed\n" passed total

let () = run_tests tests
