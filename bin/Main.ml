open Core_kernel
open Worldgen

let default_seed = 123

let get_seed () =
  match Sys.argv with
  | [|_cmd; seed|] -> (
    match int_of_string_opt seed with Some seed -> seed | None -> default_seed )
  | _ ->
      default_seed

let () =
  Printexc.record_backtrace true ;
  Stats.init () ;
  Init.init ~seed:(get_seed ()) () ;
  Init.prepare_all () ;
  Stats.flush () ;
  Init.save () ;
  Stats.finalize ()
