module Church = struct
  open Lambda

  let true' = Abs ("x", TVar "a", Abs ("y", TVar "b", Var "x"))
  let false' = Abs ("x", TVar "a", Abs ("y", TVar "b", Var "y"))

  let not' = Abs("x", TVar "a", App(App(Var "x", false'), true'))
  let and' = Abs("x", TVar "a", Abs("y", TVar "b", App(App(Var "x", Var "y"), false')))
  let or'  = Abs("x", TVar "a", Abs("y", TVar "b", App(App(Var "x", true'), Var "y")))
  
  let xor' = 
    Abs ("x",TVar "a", 
         Abs ("y", TVar "b", 
              App (App (Var "x", App (App (Var "y", false'), true')), 
                   App (App (Var "y", true'), false'))))
  let xnor' = Abs ("x", TVar "a", Abs ("y", TVar "b", App (not',App(App(xor',Var "x"),Var "y"))))

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
            , let rec apply_f n x =
                if n = 0 then x else App (Var "f", apply_f (n - 1) x)
              in
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
                  , App (App (Var "m", Var "f"), App (App (Var "n", Var "f"), Var "x")) )
              ) ) )
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


  (*Conditional operators*)
  let sub' = Abs ("m", TInt, Abs ("n", TInt, App (App (Var "n", pred'), Var "m")))
  let gte' = Abs ("a", TInt, Abs ("b", TInt, App (is_zero, App(App (sub' , Var "b"),Var "a"))))
  let lte' = Abs ("a", TInt, Abs ("b", TInt, App (is_zero, App(App (sub' , Var "a"), Var "b"))))
  let gt' = Abs ("a", TInt, Abs ("b", TInt, App (is_zero, App(App (sub' , App(succ',Var "b")),Var "a"))))
  let lt' = Abs ("a", TInt, Abs ("b", TInt, App (is_zero, App(App (sub' , App(succ',Var "a")),Var "b"))))
  let eq' = Abs ("a", TInt, Abs ("b", TInt,App( App (and', App(App (gte' , Var "a"),Var "b")), App(App (lte' , Var "a"), Var "b"))))



(*Combinators and utility funcs, change file later*)
let i' = Abs ("x", TVar "a", Var "x")

let k' = Abs ("x", TVar "a", Abs ("y", TVar "b", Var "x"))

let s' = Abs ("x", TVar "a", Abs ("y", TVar "b", Abs ("z", TVar "c", App (App (Var "x", Var "z"), App (Var "y", Var "z")))))

let y' = Abs ("f", TVar "a", App (Abs ("x", TVar "a", App (Var "f", App (Var "x", Var "x"))), Abs ("x", TVar "a", App (Var "f", App (Var "x", Var "x")))))

  


end