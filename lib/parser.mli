module Lexer : sig
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

  val init : string -> state
  val is_alpha : char -> bool
  val is_alphanum : char -> bool
  val id : state -> int -> string * int ref
  val lex : string -> token list
end

module Parser : sig
  type expression =
    | Var of string
    | App of expression * expression
    | Abs of string * expression
    | Let of string * expression * expression

  val is_space : char -> bool
  val spaces : string Angstrom.t
  val token : 'a Angstrom.t -> 'a Angstrom.t
  val identifier : string Angstrom.t
  val backslash : char Angstrom.t
  val arrow : string Angstrom.t
  val equals : char Angstrom.t
  val let_tok : string Angstrom.t
  val in_tok : string Angstrom.t
  val lparen : char Angstrom.t
  val rparen : char Angstrom.t
  val expr : expression Angstrom.t
  val parse : string -> expression
end
