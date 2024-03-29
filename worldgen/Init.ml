open Core

let side = 4096

let init ~show_progress ~save_images ~seed ~force_overlays ~max_regions
    ~install_world =
  Tale.logf "Using seed %d" seed ;
  let s = "seed-" ^ Int.to_string seed in
  Config.Paths.overlays_base := Filename.concat s "overlays" ;
  Config.Paths.world_level_base :=
    Filename.concat s (Filename.concat "worlds" s) ;
  Config.Paths.create_directories () ;
  Config.Force.set_force_overlays force_overlays ;
  Config.Force.set_max_regions max_regions ;
  Config.Install.set_install_world_at install_world ;
  if show_progress then Progress_view.init ~make_window:true
  else if save_images then Progress_view.init ~make_window:false
  else Progress_view.init_ignore () ;
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
  Meta_overlay.prepare () ;
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
  Meta_overlay.apply region ;
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

let command =
  Command.basic ~summary:"Generate a map"
    Command.Let_syntax.(
      let%map_open seed = anon ("seed" %: int)
      and force_overlays =
        flag "-f" (listed string) ~doc:"overlay force an overlay to re-run"
      and show_progress = flag "-p" no_arg ~doc:" display a progress view"
      and save_images = flag "-m" no_arg ~doc:" save progress images"
      and max_regions =
        flag "-r" (optional int) ~doc:"n only produce the nearest n regions"
      and install_world =
        flag "-i" (optional string)
          ~doc:"path install world after generation succeeds"
      in
      fun () ->
        init ~show_progress ~save_images ~seed ~force_overlays ~max_regions
          ~install_world ;
        prepare_all () ;
        if not show_progress then save ())
