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

let overlays =
  let open Overlay.Infix in
  let* dirt = Dirt_overlay.overlay side in
  let* base, canon = Base_overlay.overlay side in
  let* biomes, canond = Biome_overlay.overlay canon base dirt in
  let canon = Canonical_overlay.apply_delta canond ~onto:canon in
  let* _ores = Ore_overlay.overlay base in
  let* _cavern = Cavern_overlay.overlay base in
  let* _caves, canond = Cave_overlay.overlay canon in
  let canon = Canonical_overlay.apply_delta canond ~onto:canon in
  let* towns, canond = Town_overlay.overlay canon base in
  let canon = Canonical_overlay.apply_delta canond ~onto:canon in
  let* _roads, canond = Road_overlay.overlay canon towns in
  let canon = Canonical_overlay.apply_delta canond ~onto:canon in
  let* _plants = Plant_overlay.overlay biomes in
  let* _debug =
    Debug_overlay.overlay canon {glassify= (fun _ -> false); illuminate= false}
  in
  let spawn_point = Mg_util.shuffle canon.spawn_points |> List.hd_exn in
  Overlay.return spawn_point

let () = Progress_view.init ()

let seed = get_seed ()

let () = Tale.logf "Using seed %d" seed

let spawn, apply_overlays = Overlay.prepare seed overlays

let () = Minecraft_converter.save ~side ~spawn ~apply_overlays

let () = Stats.finalize ()
