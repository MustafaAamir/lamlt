module Church = struct
  open Types.Type

  let true' = Abs ("x", Abs ("y", Var "x"))
  let false' = Abs ("x", Abs ("y", Var "y"))
  let not' = Abs ("x", App (App (Var "x", false'), true'))
  let and' = Abs ("x", Abs ("y", App (App (Var "x", Var "y"), false')))
  let or' = Abs ("x", Abs ("y", App (App (Var "x", true'), Var "y")))

  let xor' =
    Abs
      ( "x"
      , Abs
          ( "y"
          , App
              ( App (Var "x", App (App (Var "y", false'), true'))
              , App (App (Var "y", true'), false') ) ) )
  ;;

  let xnor' = Abs ("x", Abs ("y", App (not', App (App (xor', Var "x"), Var "y"))))

  (* Zero *)
  let zero = Abs ("f", Abs ("x", Var "x"))

  let interpret = function
    | Abs ("f", Abs ("x", _body)) ->
      let rec count_apps t count =
        match t with
        | Var "x" -> count
        | App (Var "f", t') -> count_apps t' (count + 1)
        | _ -> failwith "invalid church numeral"
      in
      count_apps _body 0
    | _ -> failwith "Not a church numeral"
  ;;

  let generate n =
    if n == 0
    then zero
    else
      Abs
        ( "f"
        , Abs
            ( "x"
            , let rec apply_f n x =
                if n = 0 then x else App (Var "f", apply_f (n - 1) x)
              in
              apply_f n (Var "x") ) )
  ;;

  (* Successor function *)
  let succ' =
    Abs ("n", Abs ("f", Abs ("x", App (Var "f", App (App (Var "n", Var "f"), Var "x")))))
  ;;

  (* Add two Church numerals *)
  let add' =
    Abs
      ( "m"
      , Abs
          ( "n"
          , Abs
              ( "f"
              , Abs
                  ( "x"
                  , App (App (Var "m", Var "f"), App (App (Var "n", Var "f"), Var "x")) )
              ) ) )
  ;;

  (* Multiply two Church numerals *)
  let mul' = Abs ("m", Abs ("n", Abs ("f", App (Var "m", App (Var "n", Var "f")))))

  (* Subtract one Church numeral from another (via predecessor) *)
  let pred' =
    Abs
      ( "n"
      , Abs
          ( "f"
          , Abs
              ( "x"
              , App
                  ( App
                      ( App
                          ( Var "n"
                          , Abs ("g", Abs ("h", App (Var "h", App (Var "g", Var "f")))) )
                      , Abs ("u", Var "x") )
                  , Abs ("u", Var "u") ) ) ) )
  ;;

  (* If-then-else *)
  let if' = Abs ("p", Abs ("a", Abs ("b", App (App (Var "p", Var "a"), Var "b"))))

  (* Is zero *)
  let is_zero = Abs ("n", App (App (Var "n", Abs ("x", false')), true'))

  (*Conditional operators*)
  let sub' = Abs ("m", Abs ("n", App (App (Var "n", pred'), Var "m")))
  let gte' = Abs ("a", Abs ("b", App (is_zero, App (App (sub', Var "b"), Var "a"))))
  let lte' = Abs ("a", Abs ("b", App (is_zero, App (App (sub', Var "a"), Var "b"))))

  let gt' =
    Abs ("a", Abs ("b", App (is_zero, App (App (sub', App (succ', Var "b")), Var "a"))))
  ;;

  let lt' =
    Abs ("a", Abs ("b", App (is_zero, App (App (sub', App (succ', Var "a")), Var "b"))))
  ;;

  let eq' =
    Abs
      ( "a"
      , Abs
          ( "b"
          , App
              ( App (and', App (App (gte', Var "a"), Var "b"))
              , App (App (lte', Var "a"), Var "b") ) ) )
  ;;

  (*Combinators and utility funcs, change file later*)
  let i' = Abs ("x", Var "x")
  let k' = Abs ("x", Abs ("y", Var "x"))

  let s' =
    Abs ("x", Abs ("y", Abs ("z", App (App (Var "x", Var "z"), App (Var "y", Var "z")))))
  ;;

  let y' =
    Abs
      ( "f"
      , App
          ( Abs ("x", App (Var "f", App (Var "x", Var "x")))
          , Abs ("x", App (Var "f", App (Var "x", Var "x"))) ) )
  ;;
end
