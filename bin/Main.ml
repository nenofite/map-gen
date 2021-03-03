open Core_kernel
open Worldgen

let default_seed = 123

let get_seed () =
  match Sys.argv with
  | [|_cmd; seed|] -> (
    match int_of_string_opt seed with Some seed -> seed | None -> default_seed )
  | _ ->
      default_seed

let () = Printexc.record_backtrace true

let () = Stats.init ()

let side = 4096

let seed = get_seed ()

let () = Tale.logf "Using seed %d" seed

let () =
  let s = "seed-" ^ Int.to_string seed in
  Config.Paths.overlays_base := Filename.concat s "overlays" ;
  Config.Paths.world_level_base :=
    Filename.concat s (Filename.concat "worlds" s) ;
  Config.Paths.create_directories ()

let () =
  Progress_view.init () ;
  Overlay.init seed ;
  Canonical_overlay.init ~side ;
  Dirt_overlay.prepare () ;
  Base_overlay.prepare () ;
  Biome_overlay.prepare () ;
  Ore_overlay.prepare () ;
  Cavern_overlay.prepare () ;
  Cave_overlay.prepare () ;
  Town_overlay.prepare () ;
  Road_overlay.prepare () ;
  Plant_overlay.prepare () ;
  Debug_overlay.prepare ()

let apply_overlays args =
  Dirt_overlay.apply args ;
  Base_overlay.apply args ;
  Biome_overlay.apply args ;
  Ore_overlay.apply args ;
  Cavern_overlay.apply args ;
  Cave_overlay.apply args ;
  Town_overlay.apply args ;
  Road_overlay.apply args ;
  Plant_overlay.apply args ;
  Debug_overlay.apply {glassify= (fun _ -> false); illuminate= false} args ;
  ()

let spawn =
  let canon = Canonical_overlay.require () in
  let spawn_point = Mg_util.shuffle canon.spawn_points |> List.hd_exn in
  spawn_point

let () = Minecraft_converter.save ~side ~spawn ~apply_overlays

let () = Stats.finalize ()
