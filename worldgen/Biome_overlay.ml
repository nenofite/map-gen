open! Core_kernel
include Biome_overlay_i

let scale_factor = 8

let to_minecraft_biome = function
  | Mid (Plain _) ->
      Minecraft.Biome.Plains
  | Mid (Forest _) ->
      Minecraft.Biome.Forest
  | Mid (Desert _) ->
      Minecraft.Biome.Desert
  | Mid Savanna ->
      Minecraft.Biome.Savanna
  | High Pine_forest ->
      Minecraft.Biome.Wooded_mountains
  | High Barren ->
      Minecraft.Biome.Mountains
  | High Snow ->
      Minecraft.Biome.Snowy_tundra
  | Shore Sand ->
      Minecraft.Biome.Beach
  | Shore Gravel ->
      Minecraft.Biome.Stone_shore
  | Shore Clay ->
      (* TODO sort of a weird choice *)
      Minecraft.Biome.River

let biome_at ~x ~z biomes = Point_cloud.nearest_int biomes x z

let colorize_biome = function
  | Mid (Plain _) ->
      0x86A34D
  | Mid (Forest _) ->
      0x388824
  | Mid (Desert _) ->
      0xD9D0A1
  | Mid Savanna ->
      0x808000
  | High Pine_forest ->
      0x286519
  | High Barren ->
      0x727272
  | High Snow ->
      0xFDFDFD
  | Shore Sand ->
      0xF5EAB7
  | Shore Gravel ->
      0x828282
  | Shore Clay ->
      0x8E7256

let random_flower () =
  let open Minecraft.Block in
  let block =
    match Random.int 12 with
    | 0 ->
        Dandelion
    | 1 ->
        Poppy
    | 2 ->
        Blue_orchid
    | 3 ->
        Allium
    | 4 ->
        Azure_bluet
    | 5 ->
        Red_tulip
    | 6 ->
        Orange_tulip
    | 7 ->
        White_tulip
    | 8 ->
        Pink_tulip
    | 9 ->
        Oxeye_daisy
    | 10 ->
        Cornflower
    | 11 | _ ->
        Lily_of_the_valley
    (* TODO tall flowers *)
  in
  {block; percentage= Random.int_incl 10 100}

let no_flower = {block= Dandelion; percentage= 0}

let random_cactus () = {percentage= Random.int_incl 10 100}

let random_shore () =
  match Random.int 3 with 0 -> Sand | 1 -> Gravel | _ -> Clay

let random_high () =
  match Random.int 3 with 0 -> Pine_forest | 1 -> Barren | _ -> Snow

let get_obstacle dirt biome =
  match biome with
  | Mid (Desert _) -> (
    match dirt = 0 with true -> Overlay.Canon.Impassable | false -> Clear )
  | _ ->
      Clear

let temperature_at ~x ~z =
  let side = (Overlay.Canon.require ()).side in
  let offset =
    Float.(Perlin.at ~x:(of_int x / 256.) ~y:0. ~z:(of_int z / 256.) * 10.)
    |> Int.of_float
  in
  (* range 0 to 50 degrees Celsius *)
  (z * 50 / side) + offset

let fold_scale_factor ~mx ~mz ~init ~f =
  Range.fold (mz * scale_factor)
    ((mz * scale_factor) + scale_factor - 1)
    init
    (fun init z ->
      Range.fold (mx * scale_factor)
        ((mx * scale_factor) + scale_factor - 1)
        init
        (fun init x -> f ~x ~z init) )

let initial_moisture_at ~mx ~mz base =
  fold_scale_factor ~mx ~mz ~init:0 ~f:(fun ~x ~z m ->
      let here =
        match Grid.get x z base with
        | River.Tile.{ocean= true; river= _; elevation= _} ->
            100
        | {ocean= false; river= true; elevation= _} ->
            55
        | {ocean= false; river= false; elevation= _} ->
            0
      in
      max m here )

let moisture_carry_at ~mx ~mz base =
  fold_scale_factor ~mx ~mz ~init:40 ~f:(fun ~x ~z m ->
      let here =
        match Grid.get x z base with
        | River.Tile.{elevation; _} when elevation > 100 ->
            30
        | _ ->
            39
      in
      min m here )

let east_wind = [(1, 0); (0, 1); (1, -1)]

let west_wind = [(-1, 0); (0, 1); (-1, -1)]

let wind_direction_at ~mx ~mz =
  ignore mx ;
  if mz * scale_factor / 512 mod 2 = 0 then east_wind else west_wind

let mountain_threshold_at ~x ~z dirt = 90 + Grid.get x z dirt

let draw_moisture moisture =
  let draw_dense () x z =
    if Grid.is_within_side ~x ~y:z (Point_cloud.side moisture) then
      let here = Point_cloud.nearest_int moisture x z in
      let g = here * 255 / 100 in
      Some (g, g, g)
    else None
  in
  let l = Progress_view.push_layer () in
  Progress_view.update ~draw_dense ~state:() l

let prepare_flowers () =
  let side = (Overlay.Canon.require ()).side in
  let coarse =
    Point_cloud.init ~side ~spacing:512 (fun _ _ -> random_flower ())
  in
  Point_cloud.init ~side ~spacing:64 (fun x z ->
      Point_cloud.nearest_int coarse x z )

let prepare_cacti () =
  let side = (Overlay.Canon.require ()).side in
  let coarse =
    Point_cloud.init ~side ~spacing:512 (fun _ _ -> random_cactus ())
  in
  Point_cloud.init ~side ~spacing:64 (fun x z ->
      Point_cloud.nearest_int coarse x z )

let lookup_whittman ~mountain_threshold ~flower ~cactus ~moisture ~temperature
    ~elevation =
  let e = elevation in
  let t = temperature in
  let m = moisture in
  let mt = mountain_threshold in
  match () with
  | () when e > mt && m > 50 ->
      High Snow
  | () when e > mt - 10 && t > 20 && m > 50 ->
      High Pine_forest
  | () when e > mt - 10 && m > 50 ->
      High Snow
  | () when e > mt ->
      High Barren
  | () when t > 35 && m > 50 ->
      Mid (Forest flower)
  | () when t > 35 && m > 20 ->
      Mid Savanna
  | () when t > 35 ->
      Mid (Desert cactus)
  | () when m > 50 ->
      Mid (Forest flower)
  | () when m > 10 ->
      Mid (Plain flower)
  | () ->
      Mid (Plain no_flower)

let prepare_moisture () =
  let canon = Overlay.Canon.require () in
  let base, _ = Base_overlay.require () in
  let add_to_pq pq ~x ~z ~moisture =
    Pq.insert pq (-moisture) (x, z, moisture)
  in
  let mside = canon.side / scale_factor in
  let moisture =
    Grid.Mut.init ~side:mside ~alloc_side:canon.side 0 ~f:(fun ~x ~z ->
        let m = initial_moisture_at ~mx:x ~mz:z base in
        m )
  in
  let moisture_carry =
    Grid.Mut.init ~side:mside 0 ~f:(fun ~x ~z ->
        moisture_carry_at ~mx:x ~mz:z base )
  in
  let spread_coord ~x ~z ~m pq =
    let carry = Grid.Mut.get ~x ~z moisture_carry in
    List.fold (wind_direction_at ~mx:x ~mz:z) ~init:pq ~f:(fun pq (dx, dz) ->
        let x = x + dx in
        let z = z + dz in
        let m = m * carry / 40 in
        if Grid.Mut.is_within ~x ~z moisture then
          let old_m = Grid.Mut.get ~x ~z moisture in
          if m > old_m then (
            Grid.Mut.set ~x ~z m moisture ;
            add_to_pq pq ~x ~z ~moisture:m )
          else pq
        else pq )
  in
  let full_spread_moisture pq =
    Range.fold 0 (mside - 1) pq (fun pq z ->
        Range.fold 0 (mside - 1) pq (fun pq x ->
            let m = Grid.Mut.get ~x ~z moisture in
            spread_coord ~x ~z ~m pq ) )
  in
  let rec spread_moisture pq =
    match Pq.extract pq with
    | Some (x, z, m), pq ->
        let pq = spread_coord ~x ~z ~m pq in
        spread_moisture pq
    | None, _pq ->
        ()
  in
  let pq = full_spread_moisture Pq.empty in
  spread_moisture pq ;
  for _ = 1 to 1 do
    Subdivide_mut.overwrite_subdivide_with_fill
      ~fill:(fun a b c d -> (a + b + c + d) / 4)
      moisture
  done ;
  Subdivide_mut.subdivide moisture ;
  Subdivide_mut.magnify moisture ;
  Point_cloud.init ~side:canon.side ~spacing:32 (fun x z ->
      Grid.Mut.get ~x ~z moisture )
  |> Point_cloud.subdivide ~spacing:8

let prepare () =
  let canon = Overlay.Canon.require () in
  let dirt = Dirt_overlay.require () in
  let moisture = prepare_moisture () in
  Tale.block "drawing moisture" ~f:(fun () ->
      draw_moisture moisture ;
      Progress_view.save ~side:canon.side "moisture" ) ;
  let biomes =
    let flowers = prepare_flowers () in
    let cacti = prepare_cacti () in
    Point_cloud.init ~side:canon.side ~spacing:8 (fun x z ->
        let temperature = temperature_at ~x ~z in
        let moisture = Point_cloud.nearest_int moisture x z in
        let elevation = Grid.get x z canon.elevation in
        let mountain_threshold = mountain_threshold_at ~x ~z dirt in
        let flower = Point_cloud.nearest_int flowers x z in
        let cactus = Point_cloud.nearest_int cacti x z in
        lookup_whittman ~mountain_threshold ~moisture ~temperature ~elevation
          ~flower ~cactus )
  in
  let biome_obstacles =
    Overlay.Canon.Obstacles.init ~side:(Grid.side dirt) (fun (x, z) ->
        let here_dirt = Grid.get x z dirt in
        let here_biome = biome_at ~x ~z biomes in
        get_obstacle here_dirt here_biome )
  in
  let canond = Overlay.Canon.make_delta ~obstacles:(`Add biome_obstacles) () in
  (biomes, canond)

let colorize (biome, base) =
  if River.has_water base then River.colorize base
  else
    let elev = base.River.Tile.elevation in
    let frac = float_of_int elev /. 200. in
    let frac =
      let open Float in
      max (min frac 1.) 0.
    in
    let black = 0 in
    let biome_col = colorize_biome biome in
    Color.blend black biome_col frac

let apply_progress_view (state : t) =
  let base, _ = Base_overlay.require () in
  let biome, _canon = state in
  let side = base.Grid.side in
  let layer = Progress_view.push_layer () in
  let draw_dense () x z =
    if Grid.is_within_side ~x ~y:z side then
      let here_biome = biome_at ~x ~z biome in
      let here_base = Grid.get x z base in
      Some (colorize (here_biome, here_base) |> Color.split_rgb)
    else None
  in
  Progress_view.update ~fit:(0, side, 0, side) ~draw_dense ~state:() layer ;
  Progress_view.save ~side ~format:Images.Png "biome" ;
  ()

(** overwrite_stone_air only sets the block if it is Stone or Air, to avoid overwriting rivers etc. *)
let overwrite_stone_air region x y z block =
  match Minecraft.Region.get_block_opt region ~x ~y ~z with
  | Some Air | Some Stone ->
      Minecraft.Region.set_block ~x ~y ~z block region
  | Some _ | None ->
      ()
  [@@ocaml.doc
    " overwrite_stone_air only sets the block if it is Stone or Air, to avoid \
     overwriting rivers etc. "]

let apply (state, _canon) (region : Minecraft.Region.t) =
  let dirt = Dirt_overlay.require () in
  Minecraft_converter.iter_blocks region (fun ~x ~z ->
      let open Minecraft.Region in
      let biome = biome_at ~x ~z state in
      set_biome_column ~x ~z (to_minecraft_biome biome) region ;
      let elev = height_at ~x ~z region in
      let dirt_depth = Grid_compat.at dirt x z in
      match biome with
      | Mid (Plain _ | Forest _ | Savanna) | High Pine_forest ->
          (* Dirt (will become grass in Plant_overlay) *)
          for y = elev - dirt_depth + 1 to elev do
            overwrite_stone_air region x y z Dirt
          done
      | Mid (Desert _) ->
          (* Sand with rocks sticking out *)
          for y = elev - dirt_depth + 1 to elev do
            overwrite_stone_air region x y z Sand
          done ;
          if dirt_depth = 0 then
            (* Add some rock sticking out *)
            for y = elev + 1 to elev + 2 do
              overwrite_stone_air region x y z Stone
            done
      | High Barren ->
          (* Gravel *)
          let gravel_depth = max 0 (dirt_depth - Dirt_overlay.max_depth + 2) in
          for y = elev - gravel_depth + 1 to elev do
            overwrite_stone_air region x y z Gravel
          done
      | High Snow ->
          (* Dirt (will add snow in Plant_overlay) *)
          for y = elev - dirt_depth + 1 to elev do
            overwrite_stone_air region x y z Dirt
          done
      | Shore m ->
          let material =
            match m with
            | Sand ->
                Minecraft.Block.Sand
            | Gravel ->
                Minecraft.Block.Gravel
            | Clay ->
                Minecraft.Block.Clay
          in
          for y = elev - dirt_depth + 1 to elev do
            overwrite_stone_air region x y z material
          done )

let require, prepare, apply =
  Overlay.make_lifecycle ~prepare ~after_prepare:apply_progress_view ~apply
    overlay
