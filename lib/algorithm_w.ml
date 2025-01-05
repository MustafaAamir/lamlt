module Type = struct
  type exp =
    | EVar of string
    | EInt of int
    | EApp of exp * exp
    | EAbs of string * exp
    | ELet of string * exp * exp

  type t =
    | TVar of string
    | TInt
    | TArrow of t * t

  let gen_var =
    let counter = ref 0 in
    fun base ->
      incr counter;
      base ^ string_of_int !counter (* x becomes x1 . Should I use x' to sound cooler? *)
  ;;

  let rec free_var = function
    | EInt _ -> []
    | EVar x -> [ x ]
    | EAbs (x, t) ->
      List.filter (( <> ) x) (free_var t)
      (* filter all occurences of var x and recursively free subsequent mem
         bers. x -> y -> z *)
    | EApp (t1, t2) -> free_var t1 @ free_var t2
    | ELet (var, exp, body) ->
      let exp' = free_var exp in
      let body' = free_var body in
      List.filter (( <> ) var) (body' @ exp')
  ;;

  let rec alpha x s term =
    (* DEBUG *)
    match term with
    | EVar term' when x = term' -> s (* case of direct match return body *)
    | EVar term' -> EVar term'
    | EInt n -> EInt n
    | EApp (t1, t2) -> EApp (alpha x s t1, alpha x s t2)
    | EAbs (y, t) when x = y -> EAbs (y, t)
    | EAbs (y, t) when not (List.mem y (free_var s)) -> EAbs (y, alpha x s t)
    | EAbs (y, t) ->
      let gen = gen_var y in
      let t' = alpha y (EVar gen) t in
      EAbs (gen, alpha x s t')
    | ELet (x', e, body) -> ELet (x', alpha x s e, alpha x s body)
  ;;

  let beta_reduce term =
    let rec reduce term =
      (* DEBUG *)
      match term with
      | EApp (EAbs (x, t1), t2) -> alpha x t2 t1 |> reduce
      | EApp (t1, t2) ->
        let t1' = reduce t1 in
        let t2' = reduce t2 in
        if t1 = t1' && t2 = t2' then EApp (t1, t2) else EApp (t1', t2') |> reduce
      | EAbs (x, t) -> EAbs (x, reduce t)
      | ELet (x, e, body) -> ELet (x, reduce e, reduce body)
      | _ -> term
    in
    try reduce term with
    | Stack_overflow ->
      Printf.printf
        "Stack overflow occurred! possible infinite recursion or the expression is too \
         deeply nested to evaluate.";
      term
  ;;

  type tycon = string * t
  type scheme = tycon list
end
