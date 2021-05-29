open! Core
open Worldgen

let () =
  Printexc.record_backtrace true ;
  Memtrace.trace_if_requested ~context:"worldgen" () ;
  Stats.init () ;
  Command.run Init.command ;
  Stats.finalize ()
