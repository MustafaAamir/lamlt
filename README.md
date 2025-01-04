# lcaml
a lambda calculus evaluator in OCaml

# usage

navigate to lib/ and run utop

```shell
> opam install dune utop ocamlformat
> dune utop
```
# TODO:

1. ~Add macros for if, is_zero, and other primitive operations~
2. Add OUnit2 tests
3. ~add interface~


┌─────────────────────┬──────────┬─────────┬──────────┬──────────┬─────────┬────────────┐
│ Name                │ Time/Run │ mWd/Run │ mjWd/Run │ Prom/Run │ mGC/Run │ Percentage │
├─────────────────────┼──────────┼─────────┼──────────┼──────────┼─────────┼────────────┤
│ church generate 25  │  92.48ns │  83.00w │          │          │ 0.32e-3 │     11.23% │
│ church generate 50  │ 216.19ns │ 158.00w │          │          │ 0.60e-3 │     26.26% │
│ church generate 100 │ 405.86ns │ 308.00w │    0.18w │    0.18w │ 1.17e-3 │     49.30% │
│ church generate 200 │ 823.30ns │ 608.00w │    0.70w │    0.70w │ 2.32e-3 │    100.00% │
└─────────────────────┴──────────┴─────────┴──────────┴──────────┴─────────┴────────────┘

┌──────────────────────┬──────────┬────────────┐
│ Name                 │ Time/Run │ Percentage │
├──────────────────────┼──────────┼────────────┤
│ church interpret 25  │  70.98ns │     13.34% │
│ church interpret 50  │ 153.42ns │     28.83% │
│ church interpret 100 │ 280.16ns │     52.65% │
│ church interpret 200 │ 532.08ns │    100.00% │
└──────────────────────┴──────────┴────────────┘
