module Lexer = struct
  type token =
    | Lambda
    | Dot
    | LParen
    | RParen
    | Equal
    | In
    | Let
    | Var of string
    | EOF

  type state =
    { input : string
    ; mutable ch : char
    ; mutable pos : int
    ; len : int
    }

  let init inp =
    if inp = "\n"
    then { input = ""; pos = 0; ch = '\x00'; len = 0 }
    else { input = inp; pos = 0; ch = String.get inp 0; len = String.length inp }
  ;;

  let is_alpha c =
    match c with
    | 'A' .. 'Z' | 'a' .. 'z' -> true
    | _ -> false
  ;;

  let is_alphanum c =
    match c with
    | ('0' .. '9' | _) when is_alpha c -> true
    | _ -> false
  ;;

  let id st i =
    let j = ref (i + 1) in
    while !j < st.len && is_alphanum st.input.[!j] do
      incr j
    done;
    String.sub st.input i (!j - i), j
  ;;

  let lex input =
    let st = init input in
    match input with
    | "" -> [ EOF ]
    | s when st.pos < st.len ->
      let inb i = i < st.len in
      let rec aux i =
        let advance st i =
          if st.pos + i < st.len
          then (
            st.pos <- st.pos + i;
            st.ch <- String.get st.input st.pos;
            aux st.pos)
          else [ EOF ]
        in
        match st.input.[i] with
        | ' ' | '\t' | '\n' -> advance st 1
        | '\\' -> Lambda :: advance st 1
        | '.' -> Dot :: advance st 1
        | '(' -> LParen :: advance st 1
        | ')' -> RParen :: advance st 1
        | '=' -> Equal :: advance st 1
        | 'i' when i + 1 |> inb && Char.equal s.[i + 1] 'n' -> In :: advance st 2
        | 'l' when i + 2 |> inb && Char.equal s.[i + 1] 'e' && Char.equal s.[i + 2] 't' ->
          Let :: advance st 3
        | 'A' .. 'Z' | 'a' .. 'z' ->
          let var, j = id st st.pos in
          Var var :: advance st (!j - st.pos)
        | _ -> "Unexpected character: " ^ String.make 1 s.[i] |> failwith
      in
      aux 0
    | _ -> [ EOF ]
  ;;
end

module Parser = struct
  (* Types for our AST *)
  open Angstrom

  type expression =
    | Var of string
    | App of expression * expression
    | Abs of string * expression
    | Let of string * expression * expression

  (* Basic parsers for whitespace and tokens *)
  let is_space = function
    | ' ' | '\t' | '\n' | '\r' -> true
    | _ -> false
  ;;

  let spaces = take_while is_space
  let token p = p <* spaces

  let identifier =
    let is_first_char = function
      | 'a' .. 'z' | 'A' .. 'Z' | '_' -> true
      | _ -> false
    in
    let is_rest_char = function
      | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true
      | _ -> false
    in
    spaces *> satisfy is_first_char
    >>= fun first ->
    take_while is_rest_char >>= fun rest -> return (String.make 1 first ^ rest) <* spaces
  ;;

  (* Specific token parsers *)
  let backslash = token (char '\\')
  let arrow = token (string "->")
  let equals = token (char '=')
  let let_tok = token (string "let")
  let in_tok = token (string "in")
  let lparen = token (char '(')
  let rparen = token (char ')')

  (* Forward reference for the expression parser *)
  let expr : expression t =
    fix (fun expr ->
      (* Helper to collect function applications *)
      let collect_applications = function
        | [] -> assert false (* Should never happen due to our parser structure *)
        | [ e ] -> e
        | e :: es -> List.fold_left (fun acc e -> App (acc, e)) e es
      in
      (* Variable expression *)
      let var = identifier >>| fun x -> Var x in
      (* Abstraction expression *)
      let abs =
        backslash *> identifier <* arrow >>= fun x -> expr >>| fun e -> Abs (x, e)
      in
      (* Let expression *)
      let let_expr =
        let_tok *> identifier
        <* equals
        >>= fun x -> expr <* in_tok >>= fun e1 -> expr >>| fun e2 -> Let (x, e1, e2)
      in
      (* Parenthesized expression *)
      let paren = lparen *> expr <* rparen in
      (* Main expression parser *)
      let atomic = choice [ var; abs; let_expr; paren ] in
      (* Parse a sequence of expressions and collect them into function applications *)
      many1 atomic >>| collect_applications)
  ;;

  (* Main parse function *)
  let parse input =
    match parse_string ~consume:All (spaces *> expr) input with
    | Ok result -> result
    | Error msg -> failwith ("Parse error: " ^ msg)
  ;;
end
