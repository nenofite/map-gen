open! Core
open Worldgen

let () =
  Printexc.record_backtrace true ;
  Memtrace.trace_if_requested ~context:"worldgen" () ;
  Stats.init () ;
  Command_unix.run Init.command ;
  Stats.finalize ()
