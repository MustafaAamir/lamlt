# lcaml
[UNFINISHED] a lambda calculus evaluator in OCaml

# usage

```shell
> opam install dune utop ocamlformat
> dune utop

open TypeCheck;;
infer @@ beta_reduce @@ Abs("x", Var "x");;
- : t = TArrow (TVar "b3", TVar "b3")
```

# todo: 
1. tests for type_checking
2. fix module structure
3. add recursive types - something like System F
4. add tests for other constructors (bool, int, if)
5. lexer and parser
