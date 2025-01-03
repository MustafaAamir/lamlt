open Lambda

let true' = Abs ("x", TVar "a", Abs ("y", TVar "b", Var "x"))
let false' = Abs ("x", TVar "a", Abs ("y", TVar "b", Var "y"))

(* Zero *)
let zero = Abs ("f", TArrow (TInt, TInt), Abs ("x", TInt, Var "x"))

let interpret = function
  | Abs ("f", _, Abs ("x", _, body)) ->
    let rec count_apps t count =
      match t with
      | Var "x" -> count
      | App (Var "f", t') -> count_apps t' (count + 1)
      | _ -> failwith "invalid church numeral"
    in
    count_apps body 0
  | _ -> failwith "Not a church numeral"
;;

let generate n =
  if n == 0
  then zero
  else
    Abs
      ( "f"
      , TArrow (TInt, TInt)
      , Abs
          ( "x"
          , TInt
          , let rec apply_f n x = if n = 0 then x else App (Var "f", apply_f (n - 1) x) in
            apply_f n (Var "x") ) )
;;

(* Successor function *)
let succ' =
  Abs
    ( "n"
    , TArrow (TInt, TInt)
    , Abs
        ( "f"
        , TArrow (TInt, TInt)
        , Abs ("x", TInt, App (Var "f", App (App (Var "n", Var "f"), Var "x"))) ) )
;;

(* Add two Church numerals *)
let add' =
  Abs
    ( "m"
    , TArrow (TInt, TInt)
    , Abs
        ( "n"
        , TArrow (TInt, TInt)
        , Abs
            ( "f"
            , TArrow (TInt, TInt)
            , Abs
                ( "x"
                , TInt
                , App (App (Var "m", Var "f"), App (App (Var "n", Var "f"), Var "x")) ) )
        ) )
;;

(* Multiply two Church numerals *)
let mul' =
  Abs
    ( "m"
    , TArrow (TInt, TInt)
    , Abs
        ( "n"
        , TArrow (TInt, TInt)
        , Abs ("f", TArrow (TInt, TInt), App (Var "m", App (Var "n", Var "f"))) ) )
;;

(* Subtract one Church numeral from another (via predecessor) *)
let pred' =
  Abs
    ( "n"
    , TArrow (TInt, TInt)
    , Abs
        ( "f"
        , TArrow (TInt, TInt)
        , Abs
            ( "x"
            , TInt
            , App
                ( App
                    ( App
                        ( Var "n"
                        , Abs
                            ( "g"
                            , TArrow (TInt, TInt)
                            , Abs
                                ( "h"
                                , TArrow (TInt, TInt)
                                , App (Var "h", App (Var "g", Var "f")) ) ) )
                    , Abs ("u", TInt, Var "x") )
                , Abs ("u", TInt, Var "u") ) ) ) )
;;

(* If-then-else *)
let if' =
  Abs
    ( "p"
    , TVar "a"
    , Abs ("a", TVar "b", Abs ("b", TVar "c", App (App (Var "p", Var "a"), Var "b"))) )
;;

(* Is zero *)
let is_zero =
  Abs ("n", TArrow (TInt, TInt), App (App (Var "n", Abs ("x", TVar "a", false')), true'))
;;
