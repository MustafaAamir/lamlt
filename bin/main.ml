open Lambda (* namespacing lambda, not church *)
let () = Church.if' |> beta_reduce |> string_of_term |> print_endline

