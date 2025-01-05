module TypeCheck = struct
  open Types
  let gen_var =
    let counter = ref 0 in
    fun base ->
      incr counter;
      base ^ string_of_int !counter (* x becomes x1 . Should I use x' to sound cooler? *)
  ;;

  let rec free_var = function
    | Type.EInt _ -> []
    | Type.EVar x -> [ x ]
    | Type.EAbs (x, t) ->
      List.filter (( <> ) x) (free_var t)
      (* filter all occurences of var x and recursively free subsequent mem
         bers. x -> y -> z *)
    | Type.EApp (t1, t2) -> free_var t1 @ free_var t2
    | Type.ELet (var, expression, body) ->
      let expression' = free_var expression in
      let body' = free_var body in
      List.filter (( <> ) var) (body' @ expression')
  ;;

  let rec alpha x s term =
    (* DEBUG *)
    match term with
    | Type.EVar term' when x = term' -> s (* case of direct match return body *)
    | Type.EVar term' -> Type.EVar term'
    | Type.EInt n -> Type.EInt n
    | Type.EApp (t1, t2) -> Type.EApp (alpha x s t1, alpha x s t2)
    | Type.EAbs (y, t) when x = y -> Type.EAbs (y, t)
    | Type.EAbs (y, t) when not (List.mem y (free_var s)) -> Type.EAbs (y, alpha x s t)
    | Type.EAbs (y, t) ->
      let gen = gen_var y in
      let t' = alpha y (Type.EVar gen) t in
      Type.EAbs (gen, alpha x s t')
    | Type.ELet (x', e, body) -> Type.ELet (x', alpha x s e, alpha x s body)
  ;;

  let beta_reduce term =
    let rec reduce term =
      (* DEBUG *)
      match term with
      | Type.EApp (Type.EAbs (x, t1), t2) -> alpha x t2 t1 |> reduce
      | Type.EApp (t1, t2) ->
        let t1' = reduce t1 in
        let t2' = reduce t2 in
        if t1 = t1' && t2 = t2'
        then Type.EApp (t1, t2)
        else Type.EApp (t1', t2') |> reduce
      | Type.EAbs (x, t) -> Type.EAbs (x, reduce t)
      (* let x = \\f.f in x 10
         should reduce to
         (\\f.f) 10
         id 10 ~> Int 10

         bind e1 to x, reduce e1
         alpha substitute occurrences of Var "x" with e1
         reduce the resulting expression
         return
      *)
      | Type.ELet (x, e1, e2) ->
        let e1' = reduce e1 in
        alpha x e1' e2 |> reduce
      | _ -> term
    in
    try reduce term with
    | Stack_overflow ->
      Printf.printf
        "Stack overflow occurred! possible infinite recursion or the expression is too \
         deeply nested to evaluate.";
      term
  ;;

  type tycon = string * Type.t
  type scheme = tycon list

end
