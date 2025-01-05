open OUnit2
open Lcaml.Lambda
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
           (App (Abs ("x", TInt, Var "x"), Church.generate 4))
           (Church.generate 4)
       ; rtest
           "multi_arg_function"
           (App (App (Abs ("x", TInt, Abs ("y", TInt, Var "x")), Int 1), Int 2))
           (Int 1)
       ; (* Nested applications *)
         rtest
           "nested_application"
           (App (Abs ("x", TInt, App (Abs ("y", TInt, Var "y"), Var "x")), Int 42))
           (Int 42)
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
           (let church_zero = Abs ("f", TArrow (TInt, TInt), Abs ("x", TInt, Var "x")) in
            Int (Church.interpret church_zero))
           (Int 0)
       ; (* it decomposes down to an identity function .\y.y*)
         rtest
           "chuch_gen_equiv_zero"
           (let church_zero = Abs ("f", TArrow (TInt, TInt), Abs ("x", TInt, Var "x")) in
            church_zero)
           (Church.generate 0)
       ; rtest
           "church_one"
           (let church_one =
              Abs ("f", TArrow (TInt, TInt), Abs ("x", TInt, App (Var "f", Var "x")))
            in
            Int (Church.interpret church_one))
           (Int 1)
       ; rtest
           "church_gen_equiv_one"
           (let church_one =
              Abs ("f", TArrow (TInt, TInt), Abs ("x", TInt, App (Var "f", Var "x")))
            in
            church_one)
           (Church.generate 1)
       ; rtest
           "church_two"
           (let church_two =
              Abs
                ( "f"
                , TArrow (TInt, TInt)
                , Abs ("x", TInt, App (Var "f", App (Var "f", Var "x"))) )
            in
            Int (Church.interpret church_two))
           (Int 2)
       ; rtest
           "gen_church_equiv_two"
           (let church_one =
              Abs
                ( "f"
                , TArrow (TInt, TInt)
                , Abs ("x", TInt, App (Var "f", App (Var "f", Var "x"))) )
            in
            church_one)
           (Church.generate 2)
       ; rtest "gen_church_100" (Int (Church.interpret (Church.generate 100))) (Int 100)
       ; (* Complex expression from previous example *)
         rtest
           "complex_xyz_reduction"
           (let id = Abs ("x", TArrow (TVar "a", TVar "a"), Var "x") in
            let self_app = Abs ("x", TVar "a", App (Var "x", Var "x")) in
            let xyz =
              Abs
                ( "x"
                , TVar "a"
                , Abs
                    ( "y"
                    , TVar "b"
                    , Abs ("z", TVar "c", App (App (Var "x", Var "y"), Var "z")) ) )
            in
            App (App (App (xyz, self_app), id), Var "x"))
           (Var "x")
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
