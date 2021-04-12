open Core_kernel

let side = 4096

let init ~seed () =
  Tale.logf "Using seed %d" seed ;
  let s = "seed-" ^ Int.to_string seed in
  Config.Paths.overlays_base := Filename.concat s "overlays" ;
  Config.Paths.world_level_base :=
    Filename.concat s (Filename.concat "worlds" s) ;
  Config.Paths.create_directories () ;
  Progress_view.init () ;
  Overlay.init seed ;
  Overlay.Canon.init ~side ;
  ()

let prepare_all () =
  Dirt_overlay.prepare () ;
  Base_overlay.prepare () ;
  Biome_overlay.prepare () ;
  Ore_overlay.prepare () ;
  Cavern_overlay.prepare () ;
  Cave_overlay.prepare () ;
  Site_overlay.prepare () ;
  Town_overlay.prepare () ;
  Road_overlay.prepare () ;
  Plant_overlay.prepare () ;
  Debug_overlay.prepare () ;
  ()

let apply_all region =
  Dirt_overlay.apply region ;
  Base_overlay.apply region ;
  Biome_overlay.apply region ;
  Ore_overlay.apply region ;
  Cavern_overlay.apply region ;
  Cave_overlay.apply region ;
  Site_overlay.apply region ;
  Town_overlay.apply region ;
  Road_overlay.apply region ;
  Plant_overlay.apply region ;
  Debug_overlay.apply {glassify= (fun _ -> false); illuminate= false} region ;
  ()

let save () =
  let canon = Overlay.Canon.require () in
  let spawn =
    match Mg_util.shuffle canon.spawn_points with
    | s :: _ ->
        s
    | [] ->
        Tale.logf "No spawn points, using default" ;
        (0, 100, 0)
  in
  Minecraft_converter.save ~side ~spawn ~apply_overlays:apply_all
