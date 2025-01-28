open Lcaml.Types.Type
open Lcaml.Type_checker

(* \lambda x, y, z -> z *)
(*
let y = 
    Abs
      ( "f"
      , App
          ( Abs ("x", App (Var "f", App (Var "x", Var "x")))
          , Abs ("x", App (Var "f", App (Var "x", Var "x"))) ) )
  ;;
let if' =
    Abs
      ( "p"
      , Abs ("a", Abs ("b", App (App (Var "p", Var "a"), Var "b"))) )
  ;;

let fact = 
    App(
        y, 
        Abs("f", Abs("n", App(App(App(if', App(is_zero, Var "n")), Church.generate 1), App(App(mul', Var "n"), App(Var "f", App(pred', Var "n")))))))
    
let y =
  Abs
    ( "f"
    , App
        ( Abs ("x", App (Var "f", App (Var "x", Var "x")))
        , Abs ("x", App (Var "f", App (Var "x", Var "x"))) ) )
;;

    *)
let conditional = If (App (Abs ("x", Var "x"), Bool false), Abs ("x", Var "x"), Bool true)

let factorial =
  Abs
    ( "f"
    , Abs
        ( "n"
        , If
            ( App (Abs ("=", Var "n"), Int 0)
            , Let
                ( "decrement"
                , App (Var "f", App (Abs ("-", Var "n"), Int 1))
                , App (Abs ("*", Var "n"), Var "decrement") )
            , Int 1 ) ) )
;;

let _expr = App (factorial, Int 5)

let () =
  let inf = TypeCheck.infer (Abs ("x", Abs ("y", Abs ("z", Int 1)))) in
  print_endline ("expression has type: " ^ TypeCheck.string_of_type inf);
  let inte =
    TypeCheck.infer (App (Abs ("x", App (Abs ("y", Var "y"), Var "x")), Int (1 + 1)))
  in
  print_endline ("expression has type: " ^ TypeCheck.string_of_type inte);
  let cond = TypeCheck.infer (TypeCheck.beta_reduce conditional) in
  print_endline ("expression has type: " ^ TypeCheck.string_of_type cond);
  (*let res = TypeCheck.beta_reduce expr in*)
  (*let tres = TypeCheck.infer res in*)
  (*print_endline
    ("expression "
     ^ TypeCheck.string_of_term res
     ^ " has type: "
     ^ TypeCheck.string_of_type tres);*)
  let exttt = App (Int 2, App (Int 1, App (Abs ("+", Var "n"), Var "x"))) in
  let res = TypeCheck.beta_reduce exttt in
  print_endline ("expression " ^ TypeCheck.string_of_term res ^ " has type: ")
;;
