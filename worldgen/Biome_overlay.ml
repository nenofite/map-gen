open! Core
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
  | Snow_plains ->
      Minecraft.Biome.Snowy_plains
  | Snow_taiga ->
      Minecraft.Biome.Snowy_taiga

let is_near_river_at ~x ~z base =
  let r = 0 in
  let side = Base_overlay.side base in
  let result = ref false in
  for z = z - r to z + r do
    for x = x - r to x + r do
      if Grid.is_within_side ~x ~z side && Base_overlay.river_at ~x ~z base then
        result := true
    done
  done ;
  !result

let is_near_ocean_at ~x ~z canon base =
  let r = 5 in
  let max_elev = Base_overlay.shore_level in
  let side = Base_overlay.side base in
  if Overlay.Canon.elevation_at ~x ~z canon <= max_elev then (
    let result = ref false in
    for z = z - r to z + r do
      for x = x - r to x + r do
        if Grid.is_within_side ~x ~z side && Base_overlay.ocean_at ~x ~z base
        then result := true
      done
    done ;
    !result )
  else false

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
  | Snow_plains ->
      0xEBEBEB
  | Snow_taiga ->
      0xBFD3BA

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
  let open Float in
  let side = (Overlay.Canon.require ()).side in
  let offset =
    Mg_util.Perlin.at_opts ~freq:256. ~intervals:2 ~x:(of_int x) ~y:0.
      ~z:(of_int z) ()
    * 25.
    (* + Mg_util.Perlin.at_opts ~freq:3. ~x:(of_int x) ~y:0. ~z:(of_int z) () * 5. *)
  in
  (* range 0 to 50 degrees Celsius *)
  (Float.of_int z * 50. / Float.of_int side) + offset |> Int.of_float

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
        | River _ ->
            55
        | No_water ->
            0
      in
      max m here )

let moisture_loss_at ~mx ~mz canon =
  let sum =
    fold_scale_factor ~mx ~mz ~init:0 ~f:(fun ~x ~z m ->
        let e = Overlay.Canon.elevation_at ~x ~z canon in
        let here = if e > 100 then 25 else 2 in
        m + here )
  in
  let avg = sum / scale_factor / scale_factor in
  avg |> Int.clamp_exn ~min:0 ~max:100

let east_wind = [(1, 0); (0, 1); (1, -1)]

let west_wind = [(-1, 0); (0, 1); (-1, -1)]

let wind_direction_at ~mx ~mz =
  ignore mx ;
  if mz * scale_factor / 512 mod 2 = 0 then east_wind else west_wind

let mountain_threshold_at ~x ~z dirt = 90 + Grid.Mut.get ~x ~z dirt

let draw_cells moisture =
  let draw_dense x z =
    if Grid.is_within_side ~x ~z (Point_cloud.side moisture) then
      let here = Point_cloud.nearest_int moisture x z in
      let g = here * 255 / 100 in
      Some (g * 0x010101)
    else None
  in
  let l = Progress_view.push_layer () in
  Progress_view.update ~draw_dense l

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

let prepare_precipitation () =
  let canon = Overlay.Canon.require () in
  let base, _ = Base_overlay.require () in
  let add_to_pq pq ~x ~z ~moisture = Pq.insert pq moisture (x, z, moisture) in
  let mside = canon.side / scale_factor in
  let moisture =
    Grid.Mut.init ~side:mside 0 ~f:(fun ~x ~z ->
        let m = initial_moisture_at ~mx:x ~mz:z base in
        m )
  in
  let moisture_loss =
    Grid.Mut.init ~side:mside 0 ~f:(fun ~x ~z ->
        moisture_loss_at ~mx:x ~mz:z canon )
  in
  let spread_coord ~x ~z ~m pq =
    let loss = Grid.Mut.get ~x ~z moisture_loss in
    let send = 100 - loss in
    List.fold (wind_direction_at ~mx:x ~mz:z) ~init:pq ~f:(fun pq (dx, dz) ->
        let x = x + dx in
        let z = z + dz in
        let m = m * send / 100 in
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
  let precipitation =
    Grid.Mut.init ~side:(Grid.Mut.side moisture) ~alloc_side:canon.side
      ~f:(fun ~x ~z ->
        let m = Grid.Mut.get ~x ~z moisture in
        let loss = Grid.Mut.get ~x ~z moisture_loss in
        let stay = (4 * loss) + 100 |> Int.clamp_exn ~min:0 ~max:100 in
        m * stay / 100 |> Int.clamp_exn ~min:0 ~max:100 )
      0
  in
  for _ = 1 to 1 do
    Grid.Subdivide_mut.overwrite_subdivide_with_fill
      ~fill:(fun a b c d -> (a + b + c + d) / 4)
      precipitation
  done ;
  Grid.Subdivide_mut.subdivide precipitation ;
  Grid.Subdivide_mut.magnify precipitation ;
  Point_cloud.init ~side:canon.side ~spacing:32 (fun x z ->
      Grid.Mut.get ~x ~z precipitation )
  |> Point_cloud.subdivide ~spacing:8

let precipitation_at ~x ~z biomes =
  Point_cloud.nearest_int biomes.precipitation x z

let category_at ~category ~x ~z biomes =
  let cat = biomes.categories.(category) in
  match cat with
  | First cloud ->
      Point_cloud.nearest_int cloud x z
  | Second const ->
      const

let lookup_biome_category ~mountain_threshold ~temperature ~precipitation
    ~elevation =
  let is_mt_peak = elevation > mountain_threshold + 20 in
  let is_mt_side = elevation > mountain_threshold in
  let is_arid = precipitation <= 10 in
  let is_mid_moisture = (not is_arid) && precipitation <= 50 in
  let is_cold = temperature <= 15 in
  let is_moderate = (not is_cold) && temperature <= 35 in
  if is_mt_peak then 0
  else if is_mt_side then if is_arid then 1 else 2
  else if is_cold then if is_arid then 5 else if is_mid_moisture then 3 else 9
  else if is_moderate then
    if is_arid then 4 else if is_mid_moisture then 5 else 6
  else if is_arid then 4
  else if is_mid_moisture then 7
  else 8

let biome_at ~x ~z biomes =
  let canon = Overlay.Canon.require () in
  let base, _ = Base_overlay.require () in
  let dirt = Dirt_overlay.require () in
  match () with
  | _ when Base_overlay.ocean_at ~x ~z base ->
      Ocean
  | _ when is_near_ocean_at ~x ~z canon base ->
      Shore (* TODO also Stone_shore *)
  | _ when is_near_river_at ~x ~z base ->
      River
  | _ ->
      let temperature = temperature_at ~x ~z in
      let precipitation = precipitation_at ~x ~z biomes in
      let elevation = Overlay.Canon.elevation_at ~x ~z canon in
      let mountain_threshold = mountain_threshold_at ~x ~z dirt in
      let category =
        lookup_biome_category ~temperature ~precipitation ~elevation
          ~mountain_threshold
      in
      category_at ~category ~x ~z biomes

let prepare () =
  let canon = Overlay.Canon.require () in
  let dirt = Dirt_overlay.require () in
  let precipitation = prepare_precipitation () in
  Tale.block "drawing moisture" ~f:(fun () ->
      draw_cells precipitation ;
      Progress_view.save ~side:canon.side "moisture" ) ;
  let cacti = prepare_cacti () in
  let flowers = prepare_flowers () in
  let b0 = Either.Second Snow_mountain in
  let b1 = Either.Second Barren_mountain in
  let b2 = Either.Second Pine_forest in
  let b3 = Either.Second Snow_plains in
  let b4 = Either.First (Point_cloud.map cacti ~f:(fun c -> Desert c)) in
  let b5 = Either.First (Point_cloud.map flowers ~f:(fun f -> Plain f)) in
  let b6 =
    Either.First (Point_cloud.map flowers ~f:(fun f -> Forest f))
    (* TODO mushroom *)
  in
  let b7 = Either.Second Savanna in
  let b8 = b6 (* TODO rainforest *) in
  let b9 = Either.Second Snow_taiga in
  let categories = [|b0; b1; b2; b3; b4; b5; b6; b7; b8; b9|] in
  let biomes = {precipitation; categories} in
  let cached_biomes =
    Grid.init_exact ~side:canon.side ~f:(fun ~x ~z -> biome_at ~x ~z biomes)
  in
  let elevation = Grid.copy canon.elevation in
  Erosion.erode ~biome_at:(Grid.get cached_biomes) ~elevation ;
  let biome_obstacles =
    Grid.Mut.init_exact ~side:(Grid.Mut.side dirt) ~f:(fun ~x ~z ->
        let here_dirt = Grid.Mut.get ~x ~z dirt in
        let here_biome = biome_at ~x ~z biomes in
        get_obstacle here_dirt here_biome )
  in
  let canond =
    Overlay.Canon.make_delta ~elevation:(`Replace elevation)
      ~obstacles:(`Add biome_obstacles) ()
  in
  (biomes, canond)

let after_prepare (state : t) =
  let biome, canond = state in
  Overlay.Canon.push_delta canond ;
  let canon = Overlay.Canon.require () in
  let base, _ = Base_overlay.require () in
  let side = Base_overlay.side base in
  let layer = Progress_view.push_layer () in
  let draw_dense x z =
    if Grid.is_within_side ~x ~z side then
      if Base_overlay.any_water_at ~x ~z base then
        Some (Base_overlay.color_at ~x ~z canon base)
      else
        let here_biome = biome_at ~x ~z biome in
        let gray =
          Base_overlay.gray_of_elevation
            (Overlay.Canon.elevation_at ~x ~z canon)
        in
        Some (Mg_util.Color.blend 0 (colorize_biome here_biome) gray)
    else None
  in
  Progress_view.update ~fit:(0, side, 0, side) ~draw_dense layer ;
  Progress_view.save ~img_side:2048 ~side ~format:Images.Png "biome" ;
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
  Point_cloud.init ~cover_edges:false ~side ~spacing:32 (fun _ _ ->
      random_floor () )
  |> Point_cloud.subdivide ~cover_edges:false ~spacing:8

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
  let base, _ = Base_overlay.require () in
  let river_floors = improvise_river_floors () in
  Minecraft_converter.iter_blocks region (fun ~x ~z ->
      let open Minecraft.Region in
      let rxo, rzo = Minecraft.Region.region_offset region in
      let biome = biome_at ~x ~z state in
      set_biome_column ~x ~z (to_minecraft_biome biome) region ;
      let elev = height_at ~x ~z region in
      let dirt_depth = Grid.Mut.get ~x ~z dirt in
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
      | Snow_mountain | Snow_plains | Snow_taiga ->
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
          let dirt_depth =
            dirt_depth + Base_overlay.river_depth_at ~x ~z base
          in
          let material = river_floor_at river_floors ~x ~z ~rxo ~rzo in
          for y = elev - dirt_depth + 1 to elev do
            overwrite_stone_air region x y z material
          done )

let require, prepare, apply =
  Overlay.make_lifecycle ~prepare ~after_prepare ~apply overlay
