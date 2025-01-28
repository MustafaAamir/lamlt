module TypeCheck = struct
  open Types.Type

  let gen_var =
    let counter = ref 0 in
    fun base ->
      incr counter;
      base ^ string_of_int !counter (* x becomes x1 . Should I use x' to sound cooler? *)
  ;;

  let rec free_var = function
    | EInt _ -> []
    | EBool _ -> []
    | EVar x -> [ x ]
    | EAbs (x, t) ->
      List.filter (( <> ) x) (free_var t)
      (* filter all occurences of var x and recursively free subsequent mem
         bers. x -> y -> z *)
    | EApp (t1, t2) -> free_var t1 @ free_var t2
    | ELet (var, expression, body) ->
      let expression' = free_var expression in
      let body' = free_var body in
      List.filter (( <> ) var) (body' @ expression')
    | EIf (cond, e1, e2) ->
      let _ = free_var cond in
      let _ = free_var e1 in
      let _ = free_var e2 in
      []
  ;;

  let rec alpha x s term =
    (* DEBUG *)
    match term with
    | EVar term' when x = term' -> s (* case of direct match return body *)
    | EVar term' -> EVar term'
    | EInt n -> EInt n
    | EBool b -> EBool b
    | EApp (t1, t2) -> EApp (alpha x s t1, alpha x s t2)
    | EAbs (y, t) when x = y -> EAbs (y, t)
    | EAbs (y, t) when not (List.mem y (free_var s)) -> EAbs (y, alpha x s t)
    | EAbs (y, t) ->
      let gen = gen_var y in
      let t' = alpha y (EVar gen) t in
      EAbs (gen, alpha x s t')
    | ELet (x', e, body) -> ELet (x', alpha x s e, alpha x s body)
    | EIf (cond, e1, e2) ->
      let cond' = alpha x s cond in
      let e1' = alpha x s e1 in
      let e2' = alpha x s e2 in
      EIf (cond', e1', e2')
  ;;

  let intfun = function
    | "+" -> ( + )
    | "-" -> ( - )
    | "*" -> ( * )
    | "/" -> ( / )
    | _ -> failwith "function not implemented"
  ;;

  let boolfun = function
    | "=" -> ( = )
    | "<>" -> ( <> )
    | ">=" -> ( >= )
    | "<=" -> ( <= )
    | "<" -> ( < )
    | ">" -> ( > )
    | _ -> failwith "Not impplemented"
  ;;

  let beta_reduce term =
    let rec reduce term =
      (* DEBUG *)
      match term with
      | EApp (EAbs (x, t1), t2) ->
        let t1' = reduce t1 in
        let t2' = reduce t2 in
        (match x with
         | "+" | "-" | "*" ->
           (match t1', t2' with
            | EInt z, EInt y -> EInt ((intfun x) z y)
            | _ -> alpha x t2' t1' |> reduce)
         | "=" | "<>" ->
           (match t1', t2' with
            | EInt z, EInt y -> EBool ((boolfun x) z y)
            | EBool z, EBool y -> EBool ((boolfun x) z y)
            | _ -> alpha x t2' t1' |> reduce)
         | _ -> alpha x t2 t1 |> reduce)
      | EApp (t1, t2) ->
        let t1' = reduce t1 in
        let t2' = reduce t2 in
        if t1 = t1' && t2 = t2' then EApp (t1, t2) else EApp (t1', t2') |> reduce
      | EAbs (x, t) -> EAbs (x, reduce t)
      | ELet (x, e1, e2) ->
        let e1' = reduce e1 in
        alpha x e1' e2 |> reduce
      | EIf (cond, e1, e2) ->
        let cond' = reduce cond in
        (match cond' with
         | EBool true -> reduce e1
         | EBool false -> reduce e2
         | _ -> "Condition doesn't reduce down to type EBool of bool" |> failwith)
      | _ -> term
    in
    try reduce term with
    | Stack_overflow ->
      Printf.printf
        "Stack overflow occurred! possible infinite recursion or the expression is too \
         deeply nested to evaluate.";
      term
  ;;

  let code = ref (Char.code 'a')
  let idx = ref (-1)

  let reset_code () =
    code := Char.code 'a';
    incr idx
  ;;

  let rec string_of_term = function
    | EVar x -> x
    | EInt n -> string_of_int n
    | EBool b -> string_of_bool b
    | EAbs (x, body) -> Printf.sprintf "λ%s.%s" x (string_of_term body)
    | EApp (t1, t2) ->
      (match t2 with
       | EApp (_, _) -> Printf.sprintf "%s(%s)" (string_of_term t1) (string_of_term t2)
       | _ -> Printf.sprintf "%s %s" (string_of_term t1) (string_of_term t2))
    | ELet (x, e1, e2) ->
      Printf.sprintf "let %s = %s in %s" x (string_of_term e1) (string_of_term e2)
    | EIf (cond, e1, e2) ->
      Printf.sprintf
        "if (%s) then (%s) else (%s)"
        (string_of_term cond)
        (string_of_term e1)
        (string_of_term e2)
  ;;

  let gen_type_var () : t =
    let mk_var () = TVar (String.make 1 (Char.chr !code)) in
    let mk_var_ext () = TVar (String.make 1 (Char.chr !code) ^ string_of_int !idx) in
    if !code > Char.code 'z'
    then (
      reset_code ();
      if !idx = 0 then mk_var () else mk_var_ext ())
    else (
      incr code;
      if !idx = 0 then mk_var () else mk_var_ext ())
  ;;

  type scheme =
    | SAbs of string * scheme * Types.Type.t
    | SApp of scheme * scheme * Types.Type.t
    | SVar of string * Types.Type.t
    | SInt
    | SBool

  let type_of (sc : scheme) =
    match sc with
    | SAbs (_, _, t) -> t
    | SApp (_, _, t) -> t
    | SVar (_, t) -> t
    | SInt -> TInt
    | SBool -> TBool
  ;;

  (* annotate subexpr with types
        maintain a sep assoc list for bound and free variables
        since we're dealing with single expressions at a time
        if a variable is encountered as a function arg,
        add it to the bound_variable hashtbl
  *)

  let dummy_types (expr : Types.Type.expression) =
    let (fv : (string, Types.Type.t) Hashtbl.t) = Hashtbl.create 128 in
    let rec annotate e bv =
      match e with
      | EInt _ -> SInt
      | EBool _ -> SBool
      | EVar v ->
        (try
           let a = List.assoc v bv in
           SVar (v, a)
         with
         | Not_found ->
           (try
              let a = Hashtbl.find fv v in
              SVar (v, a)
            with
            | Not_found ->
              let a = gen_type_var () in
              Hashtbl.add fv v a;
              SVar (v, a)))
      | EApp (e1, e2) -> SApp (annotate e1 bv, annotate e2 bv, gen_type_var ())
      | EAbs (x, e) ->
        let a = gen_type_var () in
        let ae = annotate e ((x, a) :: bv) in
        SAbs (x, ae, TArrow (a, type_of ae))
      | ELet (x, e1, e2) ->
        let e1t = annotate e1 bv in
        let e1s = type_of e1t in
        let e2t = annotate e2 ((x, e1s) :: bv) in
        e2t
      | EIf (_, e1, e2) ->
        let e1t = annotate e1 bv in
        let e2t = annotate e2 bv in
        if type_of e1t = type_of e2t
        then e2t
        else "e1 and e2 type aren't the same" |> failwith
    in
    annotate expr []
  ;;

  let rec collect dexpr u =
    match dexpr with
    | [] -> u
    | SInt :: r -> collect r u
    | SBool :: r -> collect r u
    | SVar (_, _) :: r -> collect r u
    | SAbs (_, de, _) :: r -> collect (de :: r) u
    | SApp (e1', e2', t) :: r ->
      let f, b = type_of e1', type_of e2' in
      collect (e1' :: e2' :: r) ((f, TArrow (b, t)) :: u)
  ;;

  type subst = (string * Types.Type.t) list

  let rec occurs id t =
    match t with
    | TInt -> true
    | TBool -> true
    | TVar x -> x = id
    | TArrow (u, v) -> occurs id u || occurs id v
  ;;

  let rec substf t id t' =
    match t' with
    | TVar y -> if id = y then t else t'
    | TArrow (u, v) -> TArrow (substf t id u, substf t id v)
    | TInt -> TInt
    | TBool -> TBool
  ;;

  let apply s t = List.fold_right (fun (x, e) -> substf e x) s t

  let rec unify_one s t =
    match s, t with
    | TInt, _ -> []
    | TBool, _ -> []
    | TVar x, TInt -> [ x, t ]
    | TVar x, TBool -> [ x, t ]
    | _, TInt -> []
    | _, TBool -> []
    | TVar x, TVar y -> if x = y then [] else [ x, t ]
    | TArrow (x, y), TArrow (u, v) -> unify [ x, u; y, v ]
    | TVar x, (TArrow (_, _) as z) | (TArrow (_, _) as z), TVar x ->
      if occurs x z then failwith "not unifiable because of circularity" else [ x, z ]

  and unify lop =
    match lop with
    | [] -> []
    | (x, y) :: t ->
      let t2 = unify t in
      let t1 = unify_one (apply t2 x) (apply t2 y) in
      t1 @ t2
  ;;

  let rec string_of_type e =
    match e with
    | TInt -> "int"
    | TBool -> "bool"
    | TVar x -> x
    | TArrow ((TArrow (_, _) as s), t) ->
      Printf.sprintf "(%s) -> %s" (string_of_type s) (string_of_type t)
    | TArrow (s, t) -> Printf.sprintf "%s -> %s" (string_of_type s) (string_of_type t)
  ;;

  let infer e =
    reset_code ();
    let de = dummy_types e in
    let c1 = collect [ de ] [] in
    let s = unify c1 in
    apply s (type_of de)
  ;;
end
