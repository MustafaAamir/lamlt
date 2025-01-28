open Lcaml.Types.Type
open Lcaml.Type_checker

(* \lambda x, y, z -> z *)
(*
let y = 
    EAbs
      ( "f"
      , EApp
          ( EAbs ("x", EApp (EVar "f", EApp (EVar "x", EVar "x")))
          , EAbs ("x", EApp (EVar "f", EApp (EVar "x", EVar "x"))) ) )
  ;;
let if' =
    EAbs
      ( "p"
      , EAbs ("a", EAbs ("b", EApp (EApp (Var "p", Var "a"), Var "b"))) )
  ;;

let fact = 
    EApp(
        y, 
        EAbs("f", EAbs("n", EApp(EApp(EApp(if', EApp(is_zero, Var "n")), Church.generate 1), EApp(EApp(mul', Var "n"), EApp(Var "f", EApp(pred', Var "n")))))))
    
let y =
  EAbs
    ( "f"
    , EApp
        ( EAbs ("x", EApp (EVar "f", EApp (EVar "x", EVar "x")))
        , EAbs ("x", EApp (EVar "f", EApp (EVar "x", EVar "x"))) ) )
;;

    *)
let conditional =
  EIf (EApp (EAbs ("x", EVar "x"), EBool false), EAbs ("x", EVar "x"), EBool true)
;;

let factorial =
  EAbs
    ( "f"
    , EAbs
        ( "n"
        , EIf
            ( EApp (EAbs ("=", EVar "n"), EInt 0)
            , ELet
                ( "decrement"
                , EApp (EVar "f", EApp (EAbs ("-", EVar "n"), EInt 1))
                , EApp (EAbs ("*", EVar "n"), EVar "decrement") )
            , EInt 1 ) ) )
;;

let _expr = EApp (factorial, EInt 5)

let () =
  let inf = TypeCheck.infer (EAbs ("x", EAbs ("y", EAbs ("z", EInt 1)))) in
  print_endline ("expression has type: " ^ TypeCheck.string_of_type inf);
  let inte =
    TypeCheck.infer
      (EApp (EAbs ("x", EApp (EAbs ("y", EVar "y"), EVar "x")), EInt (1 + 1)))
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
  let exttt = EApp (EInt 2, EApp (EInt 1, EApp (EAbs ("+", EVar "n"), EVar "x"))) in
  let res = TypeCheck.beta_reduce exttt in
  print_endline ("expression " ^ TypeCheck.string_of_term res ^ " has type: ")
;;
