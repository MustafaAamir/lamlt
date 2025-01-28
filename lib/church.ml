module Church = struct
  open Types.Type

  let true' = EAbs ("x", EAbs ("y", EVar "x"))
  let false' = EAbs ("x", EAbs ("y", EVar "y"))
  let not' = EAbs ("x", EApp (EApp (EVar "x", false'), true'))
  let and' = EAbs ("x", EAbs ("y", EApp (EApp (EVar "x", EVar "y"), false')))
  let or' = EAbs ("x", EAbs ("y", EApp (EApp (EVar "x", true'), EVar "y")))

  let xor' =
    EAbs
      ( "x"
      , EAbs
          ( "y"
          , EApp
              ( EApp (EVar "x", EApp (EApp (EVar "y", false'), true'))
              , EApp (EApp (EVar "y", true'), false') ) ) )
  ;;

  let xnor' = EAbs ("x", EAbs ("y", EApp (not', EApp (EApp (xor', EVar "x"), EVar "y"))))

  (* Zero *)
  let zero = EAbs ("f", EAbs ("x", EVar "x"))

  let interpret = function
    | EAbs ("f", EAbs ("x", _body)) ->
      let rec count_apps t count =
        match t with
        | EVar "x" -> count
        | EApp (EVar "f", t') -> count_apps t' (count + 1)
        | _ -> failwith "invalid church numeral"
      in
      count_apps _body 0
    | _ -> failwith "Not a church numeral"
  ;;

  let generate n =
    if n == 0
    then zero
    else
      EAbs
        ( "f"
        , EAbs
            ( "x"
            , let rec apply_f n x =
                if n = 0 then x else EApp (EVar "f", apply_f (n - 1) x)
              in
              apply_f n (EVar "x") ) )
  ;;

  (* Successor function *)
  let succ' =
    EAbs
      ( "n"
      , EAbs ("f", EAbs ("x", EApp (EVar "f", EApp (EApp (EVar "n", EVar "f"), EVar "x"))))
      )
  ;;

  (* Add two Church numerals *)
  let add' =
    EAbs
      ( "m"
      , EAbs
          ( "n"
          , EAbs
              ( "f"
              , EAbs
                  ( "x"
                  , EApp
                      ( EApp (EVar "m", EVar "f")
                      , EApp (EApp (EVar "n", EVar "f"), EVar "x") ) ) ) ) )
  ;;

  (* Multiply two Church numerals *)
  let mul' = EAbs ("m", EAbs ("n", EAbs ("f", EApp (EVar "m", EApp (EVar "n", EVar "f")))))

  (* Subtract one Church numeral from another (via predecessor) *)
  let pred' =
    EAbs
      ( "n"
      , EAbs
          ( "f"
          , EAbs
              ( "x"
              , EApp
                  ( EApp
                      ( EApp
                          ( EVar "n"
                          , EAbs
                              ("g", EAbs ("h", EApp (EVar "h", EApp (EVar "g", EVar "f"))))
                          )
                      , EAbs ("u", EVar "x") )
                  , EAbs ("u", EVar "u") ) ) ) )
  ;;

  (* If-then-else *)
  let if' = EAbs ("p", EAbs ("a", EAbs ("b", EApp (EApp (EVar "p", EVar "a"), EVar "b"))))

  (* Is zero *)
  let is_zero = EAbs ("n", EApp (EApp (EVar "n", EAbs ("x", false')), true'))

  (*Conditional operators*)
  let sub' = EAbs ("m", EAbs ("n", EApp (EApp (EVar "n", pred'), EVar "m")))
  let gte' = EAbs ("a", EAbs ("b", EApp (is_zero, EApp (EApp (sub', EVar "b"), EVar "a"))))
  let lte' = EAbs ("a", EAbs ("b", EApp (is_zero, EApp (EApp (sub', EVar "a"), EVar "b"))))

  let gt' =
    EAbs
      ( "a"
      , EAbs ("b", EApp (is_zero, EApp (EApp (sub', EApp (succ', EVar "b")), EVar "a")))
      )
  ;;

  let lt' =
    EAbs
      ( "a"
      , EAbs ("b", EApp (is_zero, EApp (EApp (sub', EApp (succ', EVar "a")), EVar "b")))
      )
  ;;

  let eq' =
    EAbs
      ( "a"
      , EAbs
          ( "b"
          , EApp
              ( EApp (and', EApp (EApp (gte', EVar "a"), EVar "b"))
              , EApp (EApp (lte', EVar "a"), EVar "b") ) ) )
  ;;

  (*Combinators and utility funcs, change file later*)
  let i' = EAbs ("x", EVar "x")
  let k' = EAbs ("x", EAbs ("y", EVar "x"))

  let s' =
    EAbs
      ( "x"
      , EAbs ("y", EAbs ("z", EApp (EApp (EVar "x", EVar "z"), EApp (EVar "y", EVar "z"))))
      )
  ;;

  let y' =
    EAbs
      ( "f"
      , EApp
          ( EAbs ("x", EApp (EVar "f", EApp (EVar "x", EVar "x")))
          , EAbs ("x", EApp (EVar "f", EApp (EVar "x", EVar "x"))) ) )
  ;;
end
