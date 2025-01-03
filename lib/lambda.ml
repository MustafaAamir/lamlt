type t =
  | TVar of string (* TVar "something" *)
  | TArrow of t * t (*int -> int*)
  | TInt (* int *)

type term =
  | Var of string (* Var "x" *)
  | Abs of string * t * term (*λf:int.x*)
  | App of term * term (* f x *)
  | Int of int (* useit like Int 42 *)

let rec string_of_type = function
  | TVar s -> s
  | TInt -> "int"
  | TArrow (t1, t2) -> (
      match t1 with
      | _ -> string_of_type t1 ^ " -> " ^ string_of_type t2)



(* does it handle nesting porpely? *)
let rec string_of_term = function
  | Var x -> x
  | Int n -> string_of_int n
  | Abs (x, t, body) ->
      "λ" ^ x ^ ": " ^ string_of_type t ^ ". " ^ string_of_term body
  | App (t1, t2) -> (
      match t2 with
      | App (_, _) -> string_of_term t1 ^ " (" ^ string_of_term t2 ^ ")"
      | _ -> string_of_term t1 ^ " " ^ string_of_term t2)

(* maintain a Hashtbl of vars to avoid redundant increments *)
let gen_var =
  let counter = ref 0 in
  fun base ->
    incr counter;
    base
    ^ string_of_int
        !counter (* x becomes x1 . Should I use x' to sound cooler? *)

let rec free_var = function
  | Int _ -> []
  | Var x -> [ x ]
  | Abs (x, _, t) -> List.filter (( <> ) x) (free_var t)
      (* filter all occurences of var x and recursively free subsequent mem
bers. x -> y -> z *)
  | App (t1, t2) -> free_var t1 @ free_var t2
      (* recursively frees a function application. quadratic merge tho, soo
oooo *)


let rec alpha x s term =
 let () = print_endline("Alpha: " ^ string_of_term(term)) in (* DEBUG *)
  match term with
  | Var term' when x = term' -> s (* case of direct match return body *)
  | Var term' -> Var term'
  | Int n -> Int n
  | App (t1, t2) -> App (alpha x s t1, alpha x s t2)
  | Abs (y, ty, t) when x = y -> Abs (y, ty, t)
  | Abs (y, ty, t) when not (List.mem y (free_var s)) -> Abs (y, ty, alpha x s t)
  | Abs (y, ty, t) ->
      let gen = gen_var y in
      let t' = alpha y (Var gen) t in
      Abs (gen, ty, alpha x s t')


let beta_reduce term =
  let rec reduce term = begin
    let () = print_endline("Beta: " ^ string_of_
term(term)) in (* DEBUG *)
    match term with
    | App (Abs (x, _, t1), t2) -> alpha x t2 t1
|> reduce
    | App (t1, t2) ->
        let t1' = reduce t1 in
        let t2' = reduce t2 in
        if t1 = t1' && t2 = t2' then App (t1, t2
) else App (t1', t2') |> reduce
    | Abs (x, ty, t) -> Abs (x, ty, reduce t)
    | _ -> term
  end
  in
  try reduce term
  with Stack_overflow ->
    Printf.printf
      "Stack overflow occurred! possible infinit
e recursion or the expression \
       is too deeply nested to evaluate.";
    term



(* type checking . will modify*)
exception TypeError of string

let rec type_check env term =
  match term with
  | Int _ -> TInt
  | Var x -> (
      match List.assoc_opt x env with
      | Some t -> t
      | None -> raise (TypeError ("Unbound varia
ble: " ^ x)))
  | Abs (x, t1, body) ->
      let body_type = type_check ((x, t1) :: env
) body in
      TArrow (t1, body_type)
  | App (t1, t2) -> (
      match type_check env t1 with
      | TArrow (arg_type, ret_type) ->
          let t2_type = type_check env t2 in
          if t2_type = arg_type then ret_type
          else raise (TypeError "Type mismatch i
n application")
      | _ -> raise (TypeError "Expected function
 type"))
