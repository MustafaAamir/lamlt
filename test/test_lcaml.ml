open OUnit2
open Lcaml.Type_checker.TypeCheck
open Lcaml.Types.Type
open Lcaml.Church

(* reduction test *)
let rtest name input expected =
  name >:: fun _ -> assert_equal expected (beta_reduce input) ~printer:string_of_term
;;

(* basic test suite . only contains id*)
let basic_reduction_tests =
  "basic_reduction"
  >::: [ (* Identity function application *)
         rtest
           "identity function"
           (EApp (EAbs ("x", EVar "x"), Church.generate 4))
           (Church.generate 4)
       ; rtest
           "multi_arg_function"
           (EApp (EApp (EAbs ("x", EAbs ("y", EVar "x")), EInt 1), EInt 2))
           (EInt 1)
       ; (* Nested applications *)
         rtest
           "nested_application"
           (EApp (EAbs ("x", EApp (EAbs ("y", EVar "y"), EVar "x")), EInt 42))
           (EInt 42)
       ]
;;

let church_tests =
  "church_tests"
  >::: [ (* Church numerals:

    0=λf.λx.x
        (Don’t apply 𝑓 f; just return x.)

    1=λf.λx.f(x)
        (Apply 𝑓 once to 𝑥)

    2=λf.λx.f(f(x))
        (Apply 𝑓 twice to 𝑥) 𝑛 = 𝜆𝑓. 𝜆𝑥.𝑓 𝑛 ( 𝑥)

    n=λf.λx.f^n(x)
        (Apply 𝑓, 𝑛 times to 𝑥)

    if it works for 2 it works for n, interpreting churhc numerals has a complexity of
     O(N) so i'm not encoding larger numerals in the final tests
         *)
         rtest
           "church_zero"
           (let church_zero = EAbs ("f", EAbs ("x", EVar "x")) in
            EInt (Church.interpret church_zero))
           (EInt 0)
       ; (* it decomposes down to an identity function .\y.y*)
         rtest
           "chuch_gen_equiv_zero"
           (let church_zero = EAbs ("f", EAbs ("x", EVar "x")) in
            church_zero)
           (Church.generate 0)
       ; rtest
           "church_one"
           (let church_one = EAbs ("f", EAbs ("x", EApp (EVar "f", EVar "x"))) in
            EInt (Church.interpret church_one))
           (EInt 1)
       ; rtest
           "church_gen_equiv_one"
           (let church_one = EAbs ("f", EAbs ("x", EApp (EVar "f", EVar "x"))) in
            church_one)
           (Church.generate 1)
       ; rtest
           "church_two"
           (let church_two =
              EAbs ("f", EAbs ("x", EApp (EVar "f", EApp (EVar "f", EVar "x"))))
            in
            EInt (Church.interpret church_two))
           (EInt 2)
       ; rtest
           "gen_church_equiv_two"
           (let church_one =
              EAbs ("f", EAbs ("x", EApp (EVar "f", EApp (EVar "f", EVar "x"))))
            in
            church_one)
           (Church.generate 2)
       ; rtest "gen_church_100" (EInt (Church.interpret (Church.generate 100))) (EInt 100)
       ; (* Complex expression from previous example *)
         rtest
           "complex_xyz_reduction"
           (let id = EAbs ("x", EVar "x") in
            let self_app = EAbs ("x", EApp (EVar "x", EVar "x")) in
            let xyz =
              EAbs
                ("x", EAbs ("y", EAbs ("z", EApp (EApp (EVar "x", EVar "y"), EVar "z"))))
            in
            EApp (EApp (EApp (xyz, self_app), id), EVar "x"))
           (EVar "x")
       ]
;;

(* arithmetic tests *)
(* logic tests *)
(* basic recursion tests *)
(* advanced reduction tests *)
(* basic typecheck tests *)
(* advancd typecheck tests *)
(* Alpha Conversion - scoping tests*)

let suite = "all tests" >::: [ basic_reduction_tests; church_tests ]
let () = run_test_tt_main suite
