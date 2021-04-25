open! Core_kernel
include Biome_overlay_i

let colorize_biome = function
  | Mid (Plain _) ->
      0x86A34D
  | Mid (Forest _) ->
      0x388824
  | Mid (Desert _) ->
      0xD9D0A1
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

let temperature_at z =
  let side = (Overlay.Canon.require ()).side in
  (* range 0 to 50 degrees Celsius *)
  z * 50 / side

let initial_moisture_at ~x ~z base =
  match Grid.get x z base with
  | River.Tile.{ocean= true; river= _; elevation= _} ->
      100
  | {ocean= false; river= true; elevation= _} ->
      10
  | {ocean= false; river= false; elevation= _} ->
      0

let draw_moisture_mut moisture =
  let draw_dense () x z =
    if Grid.Mut.is_within ~x ~z moisture then
      let here = Grid.Mut.get ~x ~z moisture in
      let g = here * 255 / 100 in
      Some (g, g, g)
    else None
  in
  let l = Progress_view.push_layer () in
  Progress_view.update ~draw_dense ~state:() l

let draw_moisture moisture =
  let draw_dense () x z =
    if Grid.is_within x z moisture then
      let here = Grid.get x z moisture in
      let g = here * 255 / 100 in
      Some (g, g, g)
    else None
  in
  let l = Progress_view.push_layer () in
  Progress_view.update ~draw_dense ~state:() l

let prepare_biomes () =
  let canon = Overlay.Canon.require () in
  let base, _ = Base_overlay.require () in
  let add_to_pq pq ~x ~z ~moisture =
    Pq.insert pq (-moisture) (x, z, moisture)
  in
  let pq = ref Pq.empty in
  let moisture =
    Grid.Mut.init ~side:canon.side 0 ~f:(fun ~x ~z ->
        let m = initial_moisture_at ~x ~z base in
        if m > 0 then pq := add_to_pq !pq ~x ~z ~moisture:m ;
        m )
  in
  let pq = !pq in
  let rec spread_moisture pq ~dit =
    let dit =
      if dit <= 0 then (draw_moisture_mut moisture ; 100) else dit - 1
    in
    match Pq.extract pq with
    | Some (x, z, m), pq ->
        let pq =
          List.fold Grid.Griddable.four_directions ~init:pq
            ~f:(fun pq (dx, dz) ->
              let x = x + dx in
              let z = z + dz in
              let m = m - 1 in
              if Grid.Mut.is_within ~x ~z moisture then
                let old_m = Grid.Mut.get ~x ~z moisture in
                if m > old_m then (
                  Grid.Mut.set ~x ~z m moisture ;
                  add_to_pq pq ~x ~z ~moisture:m )
                else pq
              else pq )
        in
        spread_moisture pq ~dit
    | None, _pq ->
        ()
  in
  spread_moisture pq ~dit:0 ; Grid.Poly.of_mut moisture

let prepare () =
  let canon = Overlay.Canon.require () in
  let dirt = Dirt_overlay.require () in
  let moisture = prepare_biomes () in
  draw_moisture moisture ;
  let biomes = Grid.make ~side:canon.side (Shore Sand) in
  let biome_obstacles =
    Overlay.Canon.Obstacles.zip_map dirt biomes ~f:get_obstacle
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

let apply_progress_view (_state : t) =
  (* let base, _ = Base_overlay.require () in
     let biome, _canon = state in
     let side = base.Grid.side in
     let layer = Progress_view.push_layer () in
     let g = Grid_compat.zip biome base in
     Progress_view.update ~fit:(0, side, 0, side)
       ~draw_dense:(Progress_view_helper.dense colorize)
       ~state:g layer ;
     Progress_view.save ~side ~format:Images.Png "biome" ; *)
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

let apply_dirt (dirt : int Grid_compat.t) (state, _canon)
    (region : Minecraft.Region.t) =
  let region = region in
  Minecraft_converter.iter_blocks region (fun ~x ~z ->
      let open Minecraft.Region in
      let elev = height_at ~x ~z region in
      let dirt_depth = Grid_compat.at dirt x z in
      match Grid_compat.at state x z with
      | Mid (Plain _ | Forest _) | High Pine_forest ->
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

let apply_region state (region : Minecraft.Region.t) =
  let dirt = Dirt_overlay.require () in
  apply_dirt dirt state region

let require, prepare, apply =
  Overlay.make_lifecycle ~prepare ~after_prepare:apply_progress_view
    ~apply:apply_region overlay
