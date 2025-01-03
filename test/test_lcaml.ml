open OUnit2
open Lcaml.Lambda
open Lcaml.Church

(* reduction test *)
let rtest name input expected = name >:: fun _ -> assert_equal expected (beta_reduce input) ~printer:string_of_term

(* basic test suite . only contains id*)
let basic_reduction_tests =
    "basic_reduction" >::: [
        (* Identity function application *)
        rtest "identity function"
        (App (Abs ("x", TInt, Var "x"), Church.generate 4))
        (Church.generate 4);
    ]


(* arithmetic tests *)
(* logic tests *)
(* basic recursion tests *)
(* advanced reduction tests *)
(* basic typecheck tests *)
(* advancd typecheck tests *)
(* Alpha Conversion - scoping tests*)

let suite =
    "all tests" >::: [
        basic_reduction_tests;
    ]


let () = run_test_tt_main suite
