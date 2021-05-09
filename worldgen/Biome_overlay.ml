open! Core_kernel
include Biome_overlay_i

let scale_factor = 8

let to_minecraft_biome = function
  | Ocean ->
      Minecraft.Biome.Ocean
  | Plain _ ->
      Minecraft.Biome.Plains
  | Forest _ ->
      Minecraft.Biome.Forest
  | Desert _ ->
      Minecraft.Biome.Desert
  | Savanna ->
      Minecraft.Biome.Savanna
  | Pine_forest ->
      Minecraft.Biome.Wooded_mountains
  | Barren_mountain ->
      Minecraft.Biome.Mountains
  | Snow_mountain ->
      Minecraft.Biome.Snowy_tundra
  | Shore ->
      Minecraft.Biome.Beach
  | Stone_shore ->
      Minecraft.Biome.Stone_shore
  | River ->
      (* TODO sort of a weird choice *)
      Minecraft.Biome.River

let is_near_river_at ~x ~z base =
  let r = 1 in
  let side = Base_overlay.side base in
  let result = ref false in
  for z = z - r to z + r do
    for x = x - r to x + r do
      if Grid.is_within_side ~x ~y:z side && Base_overlay.river_at ~x ~z base
      then result := true
    done
  done ;
  !result

let is_near_ocean_at ~x ~z base =
  let r = 5 in
  let max_elev = Heightmap.sea_level + 2 in
  let side = Base_overlay.side base in
  if Base_overlay.elevation_at ~x ~z base <= max_elev then (
    let result = ref false in
    for z = z - r to z + r do
      for x = x - r to x + r do
        if Grid.is_within_side ~x ~y:z side && Base_overlay.ocean_at ~x ~z base
        then result := true
      done
    done ;
    !result )
  else false

let biome_at ~x ~z biomes =
  let base, _ = Base_overlay.require () in
  match () with
  | _ when Base_overlay.ocean_at ~x ~z base ->
      Ocean
  | _ when is_near_ocean_at ~x ~z base ->
      Shore (* TODO also Stone_shore *)
  | _ when is_near_river_at ~x ~z base ->
      River
  | _ ->
      Point_cloud.nearest_int biomes x z

let colorize_biome = function
  | Ocean ->
      0
  | Plain _ ->
      0x86A34D
  | Forest _ ->
      0x388824
  | Desert _ ->
      0xD9D0A1
  | Savanna ->
      0x808000
  | Pine_forest ->
      0x286519
  | Barren_mountain ->
      0x727272
  | Snow_mountain ->
      0xFDFDFD
  | Shore ->
      0xF5EAB7
  | Stone_shore ->
      0x828282
  | River ->
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

let get_obstacle dirt biome =
  match biome with
  | Desert _ -> (
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
        match Base_overlay.water_at ~x ~z base with
        | Ocean ->
            100
        | River ->
            55
        | No_water ->
            0
      in
      max m here )

let moisture_carry_at ~mx ~mz base =
  fold_scale_factor ~mx ~mz ~init:40 ~f:(fun ~x ~z m ->
      let here =
        match Base_overlay.elevation_at ~x ~z base with
        | e when e > 100 ->
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
      Snow_mountain
  | () when e > mt - 10 && t > 20 && m > 50 ->
      Pine_forest
  | () when e > mt - 10 && m > 50 ->
      Snow_mountain
  | () when e > mt ->
      Barren_mountain
  | () when t > 35 && m > 50 ->
      Forest flower
  | () when t > 35 && m > 20 ->
      Savanna
  | () when t > 35 ->
      Desert cactus
  | () when m > 50 ->
      Forest flower
  | () when m > 10 ->
      Plain flower
  | () ->
      Plain no_flower

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
  let side = Base_overlay.side base in
  let layer = Progress_view.push_layer () in
  let draw_dense () x z =
    if Grid.is_within_side ~x ~y:z side then
      if Base_overlay.any_water_at ~x ~z base then
        Some (Base_overlay.color_at ~x ~z base)
      else
        let here_biome = biome_at ~x ~z biome in
        let gray = Base_overlay.gray_at ~x ~z base in
        Some (Color.blend 0 (colorize_biome here_biome) gray |> Color.split_rgb)
    else None
  in
  Progress_view.update ~fit:(0, side, 0, side) ~draw_dense ~state:() layer ;
  Progress_view.save ~side ~format:Images.Png "biome" ;
  ()

let improvise_river_floors () =
  let random_floor () =
    match Random.int 100 with
    | i when i < 90 ->
        Minecraft.Block.Dirt
    | i when i < 95 ->
        Minecraft.Block.Sand
    | _ ->
        Minecraft.Block.Clay
  in
  let side = Minecraft.Region.block_per_region_side in
  Point_cloud.init ~avoid_edges:true ~side ~spacing:32 (fun _ _ ->
      random_floor () )
  |> Point_cloud.subdivide ~avoid_edges:true ~spacing:8

let river_floor_at p ~x ~z ~rxo ~rzo =
  Point_cloud.nearest_with_edge p Minecraft.Block.Dirt
    (float (x - rxo))
    (float (z - rzo))

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
  let river_floors = improvise_river_floors () in
  Minecraft_converter.iter_blocks region (fun ~x ~z ->
      let open Minecraft.Region in
      let rxo, rzo = Minecraft.Region.region_offset region in
      let biome = biome_at ~x ~z state in
      set_biome_column ~x ~z (to_minecraft_biome biome) region ;
      let elev = height_at ~x ~z region in
      let dirt_depth = Grid_compat.at dirt x z in
      match biome with
      | Plain _ | Forest _ | Savanna | Pine_forest ->
          (* Dirt (will become grass in Plant_overlay) *)
          for y = elev - dirt_depth + 1 to elev do
            overwrite_stone_air region x y z Dirt
          done
      | Desert _ ->
          (* Sand with rocks sticking out *)
          for y = elev - dirt_depth + 1 to elev do
            overwrite_stone_air region x y z Sand
          done ;
          if dirt_depth = 0 then
            (* Add some rock sticking out *)
            for y = elev + 1 to elev + 2 do
              overwrite_stone_air region x y z Stone
            done
      | Barren_mountain ->
          (* Gravel *)
          let gravel_depth = max 0 (dirt_depth - Dirt_overlay.max_depth + 2) in
          for y = elev - gravel_depth + 1 to elev do
            overwrite_stone_air region x y z Gravel
          done
      | Snow_mountain ->
          (* Dirt (will add snow in Plant_overlay) *)
          for y = elev - dirt_depth + 1 to elev do
            overwrite_stone_air region x y z Dirt
          done
      | Ocean | Shore ->
          let material = Minecraft.Block.Sand in
          for y = elev - dirt_depth + 1 to elev do
            overwrite_stone_air region x y z material
          done
      | Stone_shore ->
          let material = Minecraft.Block.Gravel in
          for y = elev - dirt_depth + 1 to elev do
            overwrite_stone_air region x y z material
          done
      | River ->
          let material = river_floor_at river_floors ~x ~z ~rxo ~rzo in
          for y = elev - 1 to elev do
            overwrite_stone_air region x y z material
          done )

let require, prepare, apply =
  Overlay.make_lifecycle ~prepare ~after_prepare:apply_progress_view ~apply
    overlay
