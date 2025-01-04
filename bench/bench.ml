open Core_bench
open Core
open Lcaml.Church

let twentyfive = Church.generate 25
let fifty = Church.generate 50
let hundred = Church.generate 100
let twohundred = Church.generate 200

let church_generation () =
  [ Bench.Test.create ~name:"church generate 25" (fun () -> Church.generate 25)
  ; Bench.Test.create ~name:"church generate 50" (fun () -> Church.generate 50)
  ; Bench.Test.create ~name:"church generate 100" (fun () -> Church.generate 100)
  ; Bench.Test.create ~name:"church generate 200" (fun () -> Church.generate 200)
  ]
;;

let church_interpret () =
  [ Bench.Test.create ~name:"church interpret 25" (fun () -> Church.interpret twentyfive)
  ; Bench.Test.create ~name:"church interpret 50" (fun () -> Church.interpret fifty)
  ; Bench.Test.create ~name:"church interpret 100" (fun () -> Church.interpret hundred)
  ; Bench.Test.create ~name:"church interpret 200" (fun () -> Church.interpret twohundred)
  ]
;;

let bench_all () = church_generation () @ church_interpret () |> Bench.bench

let map_bench = function
  | "generate" -> church_generation () |> Bench.bench
  | "interpret" -> church_interpret () |> Bench.bench
  | x -> "Invalid option: " ^ x |> print_endline
;;

let command =
  Command.basic
    ~summary:"Bench components of hte lcaml lib"
    ~readme:(fun () -> "check README.md")
    (let%map_open.Command args = anon (sequence ("function" %: string))
     and all_flag = flag "-all" no_arg ~doc:"Bench all functions" in
     fun () ->
       match args, all_flag with
       | [], false ->
         print_endline "specify modules or all";
         exit 0
       | _, true -> bench_all ()
       | _, false -> List.iter args ~f:map_bench)
;;

let () = Command_unix.run command
